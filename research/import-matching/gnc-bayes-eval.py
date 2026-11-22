#!/usr/bin/env python3
"""
gnc-bayes-eval.py - how well does GnuCash guess the account of imported transactions,
and would a new algorithm do better?

    python3 gnc-bayes-eval.py BOOK.gnucash --start-from OLD.gnucash [--alphas 0.03,0.1,0.3] [--thresholds 0.8,0.9,0.95]
    python3 gnc-bayes-eval.py BOOK.gnucash --from-ledger [--months 6] [--alphas 0.03,0.1,0.3] [--thresholds 0.8,0.9,0.95]
        further options: [--weekdays Mon,Tue,...] [--diagnose] [--verbose]

The books are opened READ-ONLY (XML, compressed or not, or SQLite). Nothing is written.
Only the Python standard library is used, so that testers can run this without
making the gnucash python API available in their environment.

The output produced by this script is just numbers - safe for posting.
--verbose additionally prints your account names for having a closer look.

What the script does
--------------------
GnuCash remembers, for every bank account you import into, how often each word
("token") of an imported transaction was assigned to which account. This learned
data is stored in your book. The script replays imports: it goes through
transactions day by day, asks six algorithms which account they would
suggest for each of them, and then learns the transaction. Which one is best
is open, that is what the results of different books are for. Abbreviations:

   tok:  words split at blanks and "/", ";" removed from the end of words,
         no weekday (see split_words); the stored learned data is converted
         on the fly, nothing has to be reset;
   corr: learned corrections (see Corrections). They learn every
         transaction, also accepted suggestions (a change to the importer),
         because they need to see the correct cases too; the counts still
         learn like the importer;
   cut:  no suggestion if no word of the transaction is typical for the
         suggested account (see concentration).

The rows:

 - "GnuCash 5.17": the current algorithm.
 - "NB+tok": a multi-class naive Bayes classifier (see score_naive_bayes)
   with the new words. The basis of the others.
 - "NB+tok+corr+cut": naive Bayes with corrections and the cut.
 - "damped NB+tok+cut": naive Bayes damped so that words that always occur
   together do not count as independent evidence (see damped_log_scores),
   and the cut. Learns like the importer today.
 - "kNN+tok": not naive Bayes: the account of the most similar earlier
   transactions of the same import account (see NearestNeighbours). It
   needs no learned data, the transactions are in the book.
 - "damped NB+tok+kNN veto": damped naive Bayes suggests, but not if the
   most similar earlier transactions went mostly to other accounts (see
   NaiveBayesAndNeighbours). Meant for payees whose transactions go to
   several accounts, e.g. a shop where you buy for different purposes:
   there it should give no suggestion rather than a wrong one.

Each suggestion is compared with the account you actually booked, and counted as
correct, wrong, or missing (no account reached the 90 % threshold). Each
algorithm learns from its own results, so they can drift apart over the year.

Only transactions with exactly two splits, one in an account with learned data,
are evaluated (with --start-from: learned data in OLD.gnucash, so accounts you
enter by hand are left out). Opening balances (equity accounts) are skipped.

Why not simply score the book's transactions with the learned data stored in
it? Because that data already contains those transactions, learned with the
account they were finally booked to, also where GnuCash suggested a wrong
account or none and you corrected it. That gives too good numbers.

Two ways to replay:

--start-from OLD.gnucash: start with the learned data stored in OLD.gnucash and
replay the transactions of BOOK that are not in OLD.gnucash. OLD.gnucash is

 - the previous year's book, if you keep one book per year and start each
   year as a copy of the previous one with the transactions removed (so the
   learned data is kept): the whole year is replayed;
 - or a backup of BOOK, if you keep many years in one book: everything booked
   since the backup is replayed. The older the backup, the more is replayed.

As in the GnuCash importer, a transaction is learned only if it did not get
the correct suggestion: the importer learns when you choose or change the
account by hand, not when you accept a suggestion. (In the rows with
corrections, "corr", the corrections learn every transaction: they need the
accepted suggestions too. The counts learn like the importer.) Accounts and
transactions are matched by their GUIDs, which a copy or backup keeps.

--from-ledger: start with no learned data at all, and learn every transaction
of BOOK (this is a proposal to rebuild the learned data from the booked
transactions, so earlier wrong assignments that were corrected no longer
count). Transactions of the last N months (--months, default 6) are scored.
This only knows the history contained in BOOK: if you start a new book every
year, the first months have little to learn from.

--alphas compares values of the smoothing parameter ALPHA of naive Bayes (see
score_naive_bayes), e.g. 0.03, 0.1 (default) and 0.3; 0 is strict naive Bayes.
--thresholds compares values of the threshold a suggestion has to reach,
e.g. 0.8, 0.9 (GnuCash) and 0.95. GnuCash's threshold is a user preference,
and naive Bayes may be more (or less) cautious than GnuCash 5.17, so the best
value need not be the same for both. Given one or both options, the script
replays every combination and prints, per threshold, the rows with alpha
once for each alpha, and "GnuCash 5.17" and "kNN+tok" once (they
have no alpha).

--diagnose adds two tables. By payee history: each transaction is sorted by
what earlier transactions with nearly the same words did: none ("new"), all
booked to one account, or to several ("ambiguous"). Calibration: how often
the best account was the booked one, by its probability; for a well
calibrated algorithm about 90 % of the suggestions with 0.9 are correct.

Every table shows the manual effort, estimated as:

    effort = no suggestion + 10 * wrong

A wrong suggestion is much more work than a missing one: it has to be noticed
and then corrected (about two missing ones), some are overlooked and have to
be found and rebooked later, and as soon as wrong suggestions occur, every
suggestion has to be checked carefully. So it counts ten times (WRONG_COST).
"""

import argparse
import calendar
import datetime
import gzip
import itertools
import locale
import math
import sqlite3
import xml.etree.ElementTree as ET
from collections import Counter, defaultdict, namedtuple

THRESHOLD = 0.90  # GnuCash only suggests an account if its probability is at least 90 %
ALPHA = 0.1       # default smoothing of naive Bayes (see score_naive_bayes)
RATE = 0.3        # learning rate of the corrections (see Corrections)
GAMMA = 0.7       # damping of naive Bayes (see damped_log_scores)
CONCENTRATION_CUT = 0.5  # see concentration_cut
KNN_POWER = 4      # nearest neighbours: weight of a neighbour = similarity^KNN_POWER
KNN_PRIOR = 0.05   # nearest neighbours: votes of an unknown account (see NearestNeighbours)
KNN_MIN = 0.3      # nearest neighbours: lowest similarity that counts
VETO = 0.6         # NB + kNN veto: share of the neighbours' votes NB's account needs
WRONG_COST = 10  # a wrong suggestion counts as much as this many missing ones (see effort)
PREFIX = "import-map-bayes/"

Transaction = namedtuple("Transaction", "date bank description memo amount account")


# ---------------------------------------------------------------------------
# Reading the book
#
# Both readers return:
#   accounts:      {guid: (name, parent_guid, type)}
#   learned:       {bank_guid: {token: {account_guid: count}}}
#   transactions:  [(guid, date, description, [(account_guid, amount, memo), ...])]
#
# Learned data is stored as entries "import-map-bayes/<token>/<account guid>" = count
# on the bank account.
# ---------------------------------------------------------------------------

def add_learned_entry(learned, bank, key, count):
    token, account = key[len(PREFIX):-33], key[-32:]
    learned[bank][token][account] = count


def new_learned_data():
    return defaultdict(lambda: defaultdict(dict))


def read_sqlite(path):
    db = sqlite3.connect(f"file:{path}?mode=ro", uri=True)
    accounts = {guid: (name, parent, type_) for guid, name, parent, type_ in
                db.execute("SELECT guid, name, parent_guid, account_type FROM accounts")}

    learned = new_learned_data()
    for bank, key, count in db.execute(
            "SELECT obj_guid, name, int64_val FROM slots WHERE name LIKE ?", (PREFIX + "%",)):
        add_learned_entry(learned, bank, key, count)

    transactions = {}
    for tx, date, description, account, num, denom, memo in db.execute(
            """SELECT t.guid, t.post_date, t.description, s.account_guid,
                      s.value_num, s.value_denom, s.memo
               FROM transactions t JOIN splits s ON s.tx_guid = t.guid"""):
        if tx not in transactions:
            transactions[tx] = (tx, parse_date(date), description or "", [])
        transactions[tx][3].append((account, num / denom, memo or ""))
    return accounts, learned, list(transactions.values())


def read_xml(path):
    ns = {p: "http://www.gnucash.org/XML/" + p
          for p in ("gnc", "act", "trn", "split", "ts", "slot")}
    opener = gzip.open if open(path, "rb").read(2) == b"\x1f\x8b" else open
    with opener(path, "rb") as f:
        book = ET.parse(f).getroot().find("gnc:book", ns)

    accounts = {}
    learned = new_learned_data()
    for acc in book.findall("gnc:account", ns):
        guid = acc.findtext("act:id", namespaces=ns)
        accounts[guid] = (acc.findtext("act:name", namespaces=ns),
                          acc.findtext("act:parent", namespaces=ns),
                          acc.findtext("act:type", namespaces=ns))
        for slot in acc.findall("act:slots/slot", ns):
            key = slot.findtext("slot:key", namespaces=ns)
            if key.startswith(PREFIX):
                add_learned_entry(learned, guid, key, int(slot.findtext("slot:value", namespaces=ns)))

    transactions = []
    for tx in book.findall("gnc:transaction", ns):
        splits = []
        for split in tx.findall("trn:splits/trn:split", ns):
            num, denom = split.findtext("split:value", namespaces=ns).split("/")
            splits.append((split.findtext("split:account", namespaces=ns),
                           int(num) / int(denom),
                           split.findtext("split:memo", default="", namespaces=ns)))
        transactions.append((tx.findtext("trn:id", namespaces=ns),
                             parse_date(tx.findtext("trn:date-posted/ts:date", namespaces=ns)),
                             tx.findtext("trn:description", default="", namespaces=ns),
                             splits))
    return accounts, learned, transactions


def parse_date(text):
    # Older sqlite books store "YYYYMMDDhhmmss", newer ones and XML "YYYY-MM-DD hh:mm:ss".
    return datetime.date.fromisoformat(text[:10] if text[4] == "-" else text[:8])


def read_book(path):
    if open(path, "rb").read(16) == b"SQLite format 3\x00":
        return read_sqlite(path)
    return read_xml(path)


def full_name(accounts, guid):
    name, parent, _ = accounts[guid]
    if parent in accounts and accounts[parent][2] != "ROOT":
        return full_name(accounts, parent) + ":" + name
    return name


# ---------------------------------------------------------------------------
# Tokens
# ---------------------------------------------------------------------------

def tokens_gnucash_517(tx, weekday_names):
    """GnuCash 5.17: words of description and memo, split at spaces only,
    plus the weekday name of the posting date."""
    words = (tx.description + " " + tx.memo).split(" ")
    return {w for w in words if w} | {weekday_names[tx.date.weekday()]}


def split_words(text):
    """New tokenizer: split at blanks and "/", strip ";" from the end of
    each word, keep the case.

    ";" is not bank text: GnuCash joins purpose and payee name with "; ", so
    it only ever sticks to the last word of a block. "/" separates the fields
    of card payments (shop/street/town/country) and month from year
    ("11/2026"), which would otherwise be a word that never comes again.

        "Salary 11/2026; Miller Ltd" -> Salary, 11, 2026, Miller, Ltd"""
    words = text.replace("/", " ").split(" ")
    return {w.rstrip(";") for w in words} - {""}


def tokens_split(tx):
    """New: the words of description and memo, split with split_words.
    Unlike GnuCash 5.17 without the weekday name: it says nothing about the
    account, and leaving it out gave fewer wrong suggestions."""
    return split_words(tx.description + " " + tx.memo)


def convert_learned_data(learned, weekday_names):
    """Re-tokenize the stored tokens with split_words and drop the stored
    weekday names, e.g. "Miller;" -> "Miller", "11/2026" -> "11" and "2026".
    Counts of tokens that become equal are added up. (A stored word that
    equals a weekday name is dropped too; the two cannot be told apart.)"""
    converted = {}
    for token, counts in learned.items():
        if token in weekday_names:
            continue
        for new_token in split_words(token):
            for account, count in counts.items():
                entry = converted.setdefault(new_token, {})
                entry[account] = entry.get(account, 0) + count
    return converted


# ---------------------------------------------------------------------------
# Scoring
#
# Both functions get the learned data of one bank account and the tokens of
# one imported transaction, and return (best account, its probability).
#
#   learned: {token: {account: count}}
#            count = how often the token occurred in a transaction that was
#            assigned to this account by hand
#
# Example used in the comments below (a made-up donor, Miller, who gives to
# two accounts from the same bank account DE123):
#
#   12 donations to "Band",       text "Miller DE123 rehearsal"
#   23 donations to "Youthwork",  text "Miller DE123 youthworker"
#    1 donation of the first kind, once assigned to "Youthwork" by mistake
#
#   learned = {"miller":      {"Band": 12, "Youthwork": 24},
#              "de123":       {"Band": 12, "Youthwork": 24},
#              "rehearsal":   {"Band": 12, "Youthwork": 1},
#              "youthworker": {"Youthwork": 23}}
#
# The new transaction is "Miller DE123 rehearsal". A human sees at once that it
# belongs to "Band". GnuCash 5.17 says 75 % (below the threshold, no suggestion);
# naive Bayes says 92 % for "Band".
# ---------------------------------------------------------------------------

def score_gnucash_517(learned, tokens):
    """This reimplements the algorithm from GnuCash 5.17 (Account.cpp).

    Step 1: for every token and every account the token was seen with,
            the share of this account among all occurrences of the token:

        p = count(token, account) / count(token, all accounts)

    Step 2: per account, the p values of its tokens are combined like this:

        P = product(p) / (product(p) + product(1 - p))

    This is the formula of a two-class spam filter ("spam" vs. "not spam").
    Here it is used to ask "this account or not this account?", separately for
    every account. P is not a probability over all accounts, the P values of
    the accounts do not add up to 1.

    Example (see above), account "Band":

        token        p for "Band"
        miller        12/36 = 0.33
        de123         12/36 = 0.33
        rehearsal     12/13 = 0.92

        P = 0.33 * 0.33 * 0.92 / (0.33 * 0.33 * 0.92 + 0.67 * 0.67 * 0.08) = 0.75

    "miller" and "de123" carry no information about which of the two accounts
    is meant: they occur in every single donation of Miller. But p only looks at
    how often each account was used, so Miller's more frequent "Youthwork"
    donations make both tokens count against "Band" (0.33 < 0.5).
    In addition, the single wrong assignment caps "rehearsal" at 0.92, for ever.

    Two accounts can both reach exactly 100 % (each has a token seen only with
    it); then the account seen first wins. Tokens are sorted here to make that
    choice reproducible."""
    product = {}                # per account: product(p)
    product_of_complement = {}  # per account: product(1 - p)
    for token in sorted(tokens):
        counts = learned.get(token, {})
        total = sum(counts.values())
        for account, count in counts.items():
            p = count / total
            product[account] = product.get(account, 1.0) * p
            product_of_complement[account] = product_of_complement.get(account, 1.0) * (1 - p)

    # the account with the highest P wins
    best, best_probability = None, 0.0
    for account in product:
        denominator = product[account] + product_of_complement[account]
        probability = product[account] / denominator if denominator > 0 else 0.0
        if probability > best_probability:
            best, best_probability = account, probability
    return best, best_probability


def score_naive_bayes(learned, tokens, alpha=ALPHA):
    """PR #2348: a multi-class naive Bayes classifier (multinomial, with smoothing).

    The question it answers: given the tokens of a transaction, how probable is
    each account? Bayes' rule turns this around:

        P(account | tokens)  is proportional to  P(account) * P(tokens | account)

    P(account), the "prior": how often the account was used at all.

        P(a) = N(a) / N

        N(a): sum of all token counts of account a
        N:    sum of all token counts

    P(tokens | account): how typical the tokens are for transactions of this
    account. The "naive" assumption is that tokens occur independently of each
    other, so this is a product over the tokens t of the transaction:

        P(tokens | a) = product of P(t | a)

        P(t | a) = (n(t, a) + alpha) / (N(a) + alpha * V)

        n(t, a): count of token t for account a
        V:       number of distinct tokens

    P(t | a) is the share of token t among all tokens of account a. Unlike p in
    GnuCash 5.17, it does not depend on how often the account was used.

    alpha (smoothing, default ALPHA = 0.1): with alpha = 0 ("strict" naive
    Bayes), a single token never seen with account a makes P(tokens | a) zero,
    whatever all the other tokens say: account a is ruled out. With
    alpha = 0.1 such a token counts strongly against a, but does not veto it.

    Example (see above), "Band" has N = 36, "Youthwork" N = 72, all: N = 108, V = 4:

        token        P(t | Band)              P(t | Youthwork)
        miller       (12 + 0.1) / 36.4 = 0.33  (24 + 0.1) / 72.4 = 0.33
        de123        0.33                      0.33
        rehearsal    0.33                      ( 1 + 0.1) / 72.4 = 0.015
        P(account)   36 / 108 = 0.33           72 / 108 = 0.67

        Band:      0.33 * 0.33 * 0.33 * 0.33  = 0.012
        Youthwork: 0.67 * 0.33 * 0.33 * 0.015 = 0.0011

    "miller" and "de123" are equally typical for both accounts, so they cancel
    out. "rehearsal" decides, the wrong assignment hardly matters. The prior
    favours "Youthwork" 2 : 1, which is not enough to change the result.

    Finally the values are scaled so they add up to 1 over all accounts. That
    makes them real probabilities, which can be compared with the threshold:

        Band: 0.012 / (0.012 + 0.0011) = 0.92

    Tokens never seen at all (e.g. a new invoice number) are skipped: they say
    nothing about any account."""
    return best_of(naive_bayes_log_scores(learned, tokens, alpha))


def naive_bayes_log_scores(learned, tokens, alpha=ALPHA):
    """log P(a) + sum over the known tokens t of log P(t | a), for every
    account a (see score_naive_bayes). {} if no token is known, or if strict
    naive Bayes (alpha = 0) rules out every account."""

    # N(a) and N
    account_totals = Counter()
    for counts in learned.values():
        account_totals.update(counts)
    grand_total = sum(account_totals.values())

    known_tokens = [t for t in tokens if t in learned]
    if not known_tokens or grand_total == 0:
        return {}

    # Multiplying many small numbers gets too close to zero for floating point,
    # so we add logarithms instead: log(x * y) = log(x) + log(y).
    # The order of the results stays the same.
    log_score = {}
    for account, total in account_totals.items():
        if total <= 0:
            continue
        score = math.log(total / grand_total)                                     # P(a)
        for token in known_tokens:
            count = learned[token].get(account, 0)
            if count + alpha == 0:
                break  # strict naive Bayes: P(t | a) = 0 rules the account out
            score += math.log((count + alpha) / (total + alpha * len(learned)))  # P(t | a)
        else:
            log_score[account] = score
    return log_score


def damped_log_scores(learned, tokens, alpha=ALPHA, gamma=GAMMA):
    """Naive Bayes, but k known tokens count like k^gamma tokens:

        log P(a) + (k^gamma / k) * sum over the known tokens t of log P(t | a)

    P(a) and P(t | a) as in score_naive_bayes. Naive Bayes adds up the
    evidence of every token as if they were independent. Tokens that always
    occur together (name and IBAN of a payee, the parts of a date, a town and
    its state) are then counted several times, and naive Bayes becomes too
    sure. Damping scales the summed evidence down; gamma = 1 is naive Bayes.

    Example, gamma = 0.7, two accounts with the same prior, 4 known tokens
    that are each 3 times more likely for Band: naive Bayes gives odds of
    3^4 = 81, so 81/82 = 0.99 for Band. Damped, the 4 tokens count like
    4^0.7 = 2.6, so the odds are 3^2.6 = 18 and Band gets 18/19 = 0.95."""
    account_totals = Counter()
    for counts in learned.values():
        account_totals.update(counts)
    grand_total = sum(account_totals.values())

    known_tokens = [t for t in tokens if t in learned]
    if not known_tokens or grand_total == 0:
        return {}
    damping = len(known_tokens) ** gamma / len(known_tokens)

    log_score = {}
    for account, total in account_totals.items():
        if total <= 0:
            continue
        evidence = 0.0
        for token in known_tokens:
            count = learned[token].get(account, 0)
            if count + alpha == 0:
                break  # strict naive Bayes: P(t | a) = 0 rules the account out
            evidence += math.log((count + alpha) / (total + alpha * len(learned)))   # log P(t | a)
        else:
            log_score[account] = math.log(total / grand_total) + damping * evidence
    return log_score


def probabilities(log_score):
    """Scale log scores to probabilities that add up to 1:
    P(a) = exp(score(a)) / sum of exp(score(a')) over all a'.
    exp() of the raw scores could become 0.0; subtracting the highest score
    from all scores first gives the same result and avoids that."""
    top = max(log_score.values())
    exp = {account: math.exp(score - top) for account, score in log_score.items()}
    total = sum(exp.values())
    return {account: e / total for account, e in exp.items()}


def best_of(log_score):
    """(best account, its probability), or (None, 0.0) if there is no account."""
    if not log_score:
        return None, 0.0
    p = probabilities(log_score)
    best = max(p, key=p.get)
    return best, p[best]


class Corrections:
    """Naive Bayes plus learned corrections (logistic regression on
    top of naive Bayes).

    Naive Bayes treats tokens as independent. Tokens that always occur
    together (name and IBAN of a payee, the parts of a date) are then counted
    several times, and naive Bayes can be sure of a wrong account. The
    corrections learn from such mistakes. Stored per bank account, besides
    the counts: a correction c(t, a) per token and account and c(a) per
    account, 0 where nothing was corrected.

        z(a) = NB log score(a) + c(a) + sum over the tokens t of c(t, a)
        P(a | tokens) = exp(z(a)) / sum over all accounts a' of exp(z(a'))

    Learning a transaction booked to account y (before its counts are added):

        g(a) = P(a | tokens) - (1 if a == y else 0)
        c(t, a) -= RATE * g(a)   for every token t of the transaction
        c(a)    -= RATE * g(a)

    This is one step of gradient descent on -log P(y | tokens), as in logistic
    regression. If naive Bayes was sure of a wrong account w, g(w) is near 1
    and the corrections of the transaction's tokens move against w; the
    booked account y gets g(y) near -1 and moves up. Where naive Bayes is
    right and sure, g is near 0 and nothing changes. Values of |g| < 0.001
    are skipped, so the corrections stay sparse.

    Unlike training logistic regression on all transactions, this learns one
    transaction at a time, like the counts, so it fits the importer."""

    def __init__(self, rate=RATE):
        self.rate = rate
        self.token_corrections = defaultdict(lambda: defaultdict(dict))  # bank -> token -> account -> c
        self.account_corrections = defaultdict(dict)                      # bank -> account -> c

    def corrected(self, bank, log_score, tokens):
        """z(a) for every account that naive Bayes scored."""
        z = dict(log_score)
        for account, c in self.account_corrections[bank].items():
            if account in z:
                z[account] += c
        for token in tokens:
            for account, c in self.token_corrections[bank].get(token, {}).items():
                if account in z:
                    z[account] += c
        return z

    def learn(self, bank, log_score, tokens, account):
        if not log_score:
            return  # no known token: naive Bayes learns it from the counts alone
        for a, p in probabilities(self.corrected(bank, log_score, tokens)).items():
            g = p - (1.0 if a == account else 0.0)
            if abs(g) < 0.001:
                continue
            self.account_corrections[bank][a] = self.account_corrections[bank].get(a, 0.0) - self.rate * g
            for token in tokens:
                corrections = self.token_corrections[bank][token]
                corrections[a] = corrections.get(a, 0.0) - self.rate * g


# ---------------------------------------------------------------------------
# Rules on the best account
#
# Applied after scoring: each gets the learned data of the bank account, the
# tokens, the best account and its probability, and returns a (lower)
# probability, which is then compared with the threshold.
# ---------------------------------------------------------------------------

def concentration(learned, tokens, best):
    """How typical the most typical token of the transaction is for the best
    account:

        c = max over the known tokens t of  n(t, best) / sum over all a of n(t, a)

    A payee seen before usually has a token (name, IBAN, contract number)
    that was booked almost only to its account: c near 1. A suggestion that
    rests only on tokens shared with many accounts (bank text, a town) has a
    low c, and is often wrong.

    Example: "Miller" booked 12 times to Band and 4 times elsewhere: 0.75;
    "SEPA" 3 times to Band among 400: 0.0075; c = 0.75."""
    shares = [learned[t].get(best, 0) / sum(learned[t].values())
              for t in tokens if t in learned and sum(learned[t].values()) > 0]
    return max(shares, default=0.0)


def concentration_cut(cut=CONCENTRATION_CUT):
    """Rule: no suggestion if concentration < cut (default 0.5)."""
    def rule(learned, tokens, best, probability):
        return probability if concentration(learned, tokens, best) >= cut else 0.0
    return rule


# ---------------------------------------------------------------------------
# Models that learn more than the counts (see replay_model)
#
# Each has suggest(tx) -> (best account, probability) and learn(tx, result),
# result being "correct" / "none" / "wrong", or None for a transaction that
# was not scored (history).
# ---------------------------------------------------------------------------

class NearestNeighbours:
    """Not naive Bayes: suggest the account of the most similar earlier
    transactions of the same import account. Needs no learned data at all:
    the earlier transactions are in the book (here: the old book's and the
    replayed ones; every transaction is learned, with its booked account).

    Similarity of two transactions A, B (cosine, words weighted by rarity):

        s(A, B) = sum over t in A and B of w_t^2 / (|A| * |B|),  |A| = sqrt(sum over t in A of w_t^2)
        w_t = log(1 + M / m(t))

        M: number of earlier transactions, m(t): those containing t

    A common word ("SEPA", "Dauerauftrag") weighs little, a name or IBAN a
    lot. A word never seen weighs as if seen once (a new name makes the
    transaction less similar to everything); a never seen word without a
    letter (date, reference number) weighs 0.

    Probability, over the neighbours j with s_j >= KNN_MIN:

        P(a) = sum over j booked to a of s_j^p / (sum over all j of s_j^p + prior)

    p (KNN_POWER) makes the nearest neighbours count most; prior
    (KNN_PRIOR) stands for "an account not among the neighbours", so a
    single neighbour is never certain.

    Example, p = 4, prior = 0.05: one earlier transaction with s = 1:
    P = 1 / 1.05 = 0.95. A shop seen 10 times with Food, 6 with Household,
    4 with Gifts, all s = 1: P(Food) = 10 / 20.05 = 0.50, no suggestion.
    One neighbour with s = 0.8: 0.41 / 0.46 = 0.89, no suggestion."""

    def __init__(self, history, power=KNN_POWER, prior=KNN_PRIOR, min_similarity=KNN_MIN):
        self.seen = []                       # (bank, tokens, account) of every learned transaction
        self.postings = defaultdict(list)    # (bank, token) -> indices into seen
        self.frequency = defaultdict(Counter)  # bank -> token -> m(t)
        self.size = Counter()                # bank -> M
        self.power, self.prior, self.min_similarity = power, prior, min_similarity
        for tx in history:
            self.learn(tx, None)

    def weight(self, bank, token):
        m = self.frequency[bank][token]
        if m == 0 and not any(c.isalpha() for c in token):
            return 0.0
        return math.log(1 + self.size[bank] / max(m, 1))

    def distribution(self, tx):
        """({account: P(account)}, similarity of the nearest neighbour);
        ({}, 0.0) if no earlier transaction is similar enough."""
        bank = tx.bank
        w = {t: self.weight(bank, t) for t in tokens_split(tx)}
        norm = math.sqrt(sum(x * x for x in w.values()))
        if norm == 0:
            return {}, 0.0
        # dot products with every earlier transaction sharing a word
        dot = Counter()
        for t, x in w.items():
            for j in self.postings[bank, t]:
                dot[j] += x * x
        votes, nearest = Counter(), 0.0
        for j, d in dot.items():
            _, tokens, account = self.seen[j]
            s = d / (norm * math.sqrt(sum(self.weight(bank, u) ** 2 for u in tokens)))
            if s >= self.min_similarity:
                votes[account] += s ** self.power
                nearest = max(nearest, s)
        total = sum(votes.values()) + self.prior
        return {a: v / total for a, v in votes.items()}, nearest

    def suggest(self, tx):
        p, _ = self.distribution(tx)
        if not p:
            return None, 0.0
        best = max(p, key=p.get)
        return best, p[best]

    def learn(self, tx, result):
        tokens = tokens_split(tx)
        for t in tokens:
            self.postings[tx.bank, t].append(len(self.seen))
        self.seen.append((tx.bank, tokens, tx.account))
        self.frequency[tx.bank].update(tokens)
        self.size[tx.bank] += 1


class NaiveBayesAndNeighbours:
    """Damped naive Bayes, with a veto by the nearest neighbours. They fail
    in different places: kNN knows when the same payee went to several
    accounts, but has nothing to say about a payee without a similar earlier
    transaction; naive Bayes generalises from single known words, but is
    overconfident when words repeat each other.

    Damped naive Bayes suggests (see damped_log_scores). No suggestion if
    there are neighbours (see NearestNeighbours) and naive Bayes' account
    has less than VETO of their probability.

    Example: a shop booked 10 x to Food, 6 x to Household, 4 x to Gifts:
    kNN gives Food 0.5 < 0.6, so no suggestion, whatever naive Bayes says.

    The counts learn like the importer (learn_correct), kNN learns every
    transaction."""

    def __init__(self, start, history, learn_correct, alpha=ALPHA, veto=VETO):
        self.learned = new_learned_data()
        for bank, tokens in start.items():
            for token, counts in tokens.items():
                self.learned[bank][token] = dict(counts)
        self.neighbours = NearestNeighbours(history)
        self.learn_correct, self.alpha, self.veto = learn_correct, alpha, veto

    def suggest(self, tx):
        best, probability = best_of(damped_log_scores(self.learned[tx.bank], tokens_split(tx), self.alpha))
        if best is None:
            return None, 0.0
        p_knn, _ = self.neighbours.distribution(tx)
        if p_knn and p_knn.get(best, 0.0) < self.veto:
            return best, 0.0
        return best, probability

    def learn(self, tx, result):
        self.neighbours.learn(tx, result)
        if result is None or self.learn_correct or result != "correct":
            for token in tokens_split(tx):
                counts = self.learned[tx.bank][token]
                counts[tx.account] = counts.get(tx.account, 0) + 1


# ---------------------------------------------------------------------------
# Evaluation
# ---------------------------------------------------------------------------

def judge(tx, suggestion, probability, threshold=THRESHOLD):
    """Compare a suggestion with the account actually booked."""
    if suggestion is None or probability < threshold:
        return "none"
    return "correct" if suggestion == tx.account else "wrong"


def replay(transactions, make_tokens, score, start=None, learn_correct=True, first_date=None,
           threshold=THRESHOLD, corrections=None, adjust=None, observe=None):
    """Replay imports: go through the transactions day by day, score them, then
    learn them.

    transactions: bank transactions, sorted by date.
    start: learned data to begin with, {bank: {token: {account: count}}}, e.g.
        the data stored in an older book. It is copied, not changed.
        Default: empty.
    learn_correct: False learns only the transactions that did not get the
        correct suggestion, as the GnuCash importer does (it learns when you
        choose or change the account by hand, not when you accept a suggestion).
    first_date: only transactions after this date are scored; the earlier ones
        are only learned. Default: all are scored.
    threshold: the probability a suggestion has to reach. It also changes
        what is learned, since correct suggestions are not learned.
    corrections: a Corrections object for "NB + corrections". score must then
        return naive Bayes log scores (naive_bayes_log_scores). The
        corrections learn every transaction; the counts follow learn_correct.
    adjust: a rule on the best account (see "Rules on the best account"):
        (learned data of the bank, tokens, best account, probability) ->
        probability, applied before the threshold.
    observe: for experiments; called for every scored transaction as
        observe(tx, learned data of the bank, tokens, best account,
        probability, result) before the transaction is learned.

    Transactions of the same day are not learned from each other, since they
    typically arrive in the same import. Learning works as in GnuCash: every
    token of the transaction adds 1 to count(token, booked account)."""
    learned = new_learned_data()
    for bank, tokens in (start or {}).items():
        for token, counts in tokens.items():
            learned[bank][token] = dict(counts)
    def suggest(tx):
        tokens = make_tokens(tx)
        if corrections is None:
            best, probability = score(learned[tx.bank], tokens)
        else:
            log_score = score(learned[tx.bank], tokens)
            best, probability = best_of(corrections.corrected(tx.bank, log_score, tokens))
        if adjust is not None and best is not None:
            probability = adjust(learned[tx.bank], tokens, best, probability)
        return best, probability

    results = Counter()
    per_account = defaultdict(Counter)
    for date, day in itertools.groupby(transactions, key=lambda tx: tx.date):
        day = list(day)
        # whether the counts learn each transaction; unscored ones are always learned
        learn_counts = [True] * len(day)
        if first_date is None or date > first_date:
            for i, tx in enumerate(day):
                best, probability = suggest(tx)
                result = judge(tx, best, probability, threshold)
                if observe is not None:
                    observe(tx, learned[tx.bank], make_tokens(tx), best, probability, result)
                results[result] += 1
                per_account[tx.account][result] += 1
                # like the importer: the counts do not learn accepted suggestions
                learn_counts[i] = learn_correct or result != "correct"
        for tx, learn in zip(day, learn_counts):
            if corrections is not None:
                # the corrections learn every transaction, from naive Bayes'
                # error before the counts change
                corrections.learn(tx.bank, score(learned[tx.bank], make_tokens(tx)), make_tokens(tx), tx.account)
            if learn:
                for token in make_tokens(tx):
                    counts = learned[tx.bank][token]
                    counts[tx.account] = counts.get(tx.account, 0) + 1
    return results, per_account


def replay_model(model, transactions, first_date=None, threshold=THRESHOLD, observe=None):
    """replay for a model with suggest(tx) and learn(tx, result) (see
    "Models that learn more than the counts"): score the transactions of a
    day, then learn them. What a model learns is up to the model.
    first_date, threshold, observe and the results as in replay (observe
    gets None for the learned data and the tokens)."""
    results = Counter()
    per_account = defaultdict(Counter)
    for date, day in itertools.groupby(transactions, key=lambda tx: tx.date):
        day = list(day)
        day_results = [None] * len(day)  # None: not scored, only learned
        if first_date is None or date > first_date:
            for i, tx in enumerate(day):
                best, probability = model.suggest(tx)
                result = judge(tx, best, probability, threshold)
                if observe is not None:
                    observe(tx, None, None, best, probability, result)
                results[result] += 1
                per_account[tx.account][result] += 1
                day_results[i] = result
        for tx, result in zip(day, day_results):
            model.learn(tx, result)
    return results, per_account


# ---------------------------------------------------------------------------
# Diagnostics (--diagnose)
# ---------------------------------------------------------------------------

NEAR = 0.8  # Jaccard similarity from which two transactions count as the same payee
PAYEE_KINDS = ("new", "one account", "ambiguous")
P_BINS = (0.5, 0.7, 0.9, 0.95, 0.99)  # lower bounds of the probability bins (see calibration)


def letter_words(tx):
    """The words of a transaction that contain a letter (split as in
    tokens_split). Dates, amounts and reference numbers change every time,
    so they would make the same payee look different."""
    return {w for w in tokens_split(tx) if any(c.isalpha() for c in w)}


def jaccard(a, b):
    return len(a & b) / len(a | b) if a | b else 0.0


def payee_kinds(history, transactions, near=NEAR):
    """Sort each transaction by what the same payee did before: the earlier
    transactions of the same import account whose letter words are
    near-identical (Jaccard similarity >= near):

        new:          there are none
        one account:  they were all booked to one account
        ambiguous:    they were booked to two or more accounts (a shop where
                      you buy for different purposes)

    history: transactions before the replay (the old book), or empty.
    Earlier means an earlier day: transactions of the same day are not
    compared, as the replay does not learn them from each other.
    Returns {id(tx): kind} for the transactions in transactions.

    Speed-up: J(W, V) >= near implies that V contains all but at most
    (1 - near) * |W| of W's words. So V contains at least one of W's
    p = floor((1 - near) * |W|) + 1 rarest words, and only transactions
    with one of those need to be compared."""
    everything = sorted(history + transactions, key=lambda tx: tx.date)
    words = [letter_words(tx) for tx in everything]
    frequency = Counter(w for ws in words for w in ws)
    postings = defaultdict(list)  # (import account, word) -> indices of earlier transactions
    scored = {id(tx) for tx in transactions}
    kinds = {}
    for _, day in itertools.groupby(range(len(everything)), key=lambda i: everything[i].date):
        day = list(day)
        for i in day:
            tx, w = everything[i], words[i]
            if id(tx) not in scored:
                continue
            p = int((1 - near) * len(w) + 1e-9) + 1  # 1e-9: 0.2 * 5 is 0.9999... in floating point
            rare = sorted(w, key=lambda word: (frequency[word], word))[:p]
            candidates = {j for word in rare for j in postings[tx.bank, word]}
            accounts = {everything[j].account for j in candidates if jaccard(w, words[j]) >= near}
            kinds[id(tx)] = PAYEE_KINDS[min(len(accounts), 2)]
        # only now: the same day's transactions are not history for each other
        for i in day:
            for word in words[i]:
                postings[everything[i].bank, word].append(i)
    return kinds


class Diagnosis:
    """Collects, through replay's observe, for one row:

    by_kind:     {payee kind: Counter of correct / none / wrong} (see payee_kinds)
    calibration: {bin: Counter of "correct" / "other"}: how often the best
                 account is the booked one, by its probability, whether or
                 not it reached the threshold. A rule that withdraws a
                 suggestion (concentration cut) sets the probability to 0,
                 so those land in the lowest bin.

    Calibrated means: of the suggestions with probability about 0.9, about
    90 % are correct. Example: 200 suggestions in the bin 0.95-0.99, 160
    correct: 80 %, so the model is overconfident there."""

    def __init__(self, kinds):
        self.kinds = kinds
        self.by_kind = defaultdict(Counter)
        self.calibration = defaultdict(Counter)

    def observe(self, tx, learned, tokens, best, probability, result):
        self.by_kind[self.kinds[id(tx)]][result] += 1
        if best is not None:
            bin_ = sum(probability >= bound for bound in P_BINS)  # 0: below 0.5
            self.calibration[bin_]["correct" if best == tx.account else "other"] += 1


def print_diagnoses(diagnoses):
    """Two tables, one line per row: correct / none / wrong by payee kind,
    and the share correct by probability bin."""
    w = max(len(name) for name in diagnoses)
    any_diagnosis = next(iter(diagnoses.values()))
    sizes = {kind: sum(any_diagnosis.by_kind[kind].values()) for kind in PAYEE_KINDS}
    print(f"\nBy payee history (earlier transactions with near-identical words, Jaccard >= {NEAR:g}; "
          f"letter words only): correct / no suggestion / wrong")
    print(f"{'':{w}} " + " ".join(f"{f'{kind} ({sizes[kind]})':>20}" for kind in PAYEE_KINDS))
    for name, d in diagnoses.items():
        cells = ["/".join(str(d.by_kind[kind][r]) for r in ("correct", "none", "wrong")) for kind in PAYEE_KINDS]
        print(f"{name:{w}} " + " ".join(f"{c:>20}" for c in cells))

    bounds = (0.0,) + P_BINS + (1.0,)
    labels = [f"<{P_BINS[0]:g}"] + [f"{lo:g}-{hi:g}" for lo, hi in zip(bounds[1:-2], bounds[2:-1])] + [f">={P_BINS[-1]:g}"]
    print("\nCalibration: share correct of the best account, by its probability (number in brackets)")
    print(f"{'':{w}} " + " ".join(f"{label:>13}" for label in labels))
    for name, d in diagnoses.items():
        cells = []
        for b in range(len(labels)):
            c = d.calibration[b]
            n = c["correct"] + c["other"]
            cells.append(f"{100 * c['correct'] / n:3.0f}% ({n})" if n else "-")
        print(f"{name:{w}} " + " ".join(f"{c:>13}" for c in cells))


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def default_weekday_names():
    """GnuCash stores the weekday in the language of your system (e.g. "Monday", "Montag")."""
    locale.setlocale(locale.LC_TIME, "")
    return list(calendar.day_name)


def import_accounts(accounts, learned, start, start_from):
    """The accounts to replay, and the ones skipped.

    An import account is one with learned data. With --start-from it must
    have learned data in the old book: an account without any there has no
    history, so every transaction would be scored against an empty map and
    get no suggestion. That happens with accounts whose transactions are
    entered by hand, and it says nothing about the matcher. (It also leaves
    out accounts that were first imported after the old book; they would
    start from nothing in GnuCash too, but would learn.)
    Without --start-from, BOOK's learned data decides."""
    if start_from:
        banks = {bank for bank in start if start[bank] and bank in accounts}
        skipped = {bank for bank in learned if bank in accounts} - banks
    else:
        banks, skipped = {bank for bank in learned if bank in accounts}, set()
    return banks, skipped


def bank_transactions(all_transactions, accounts, banks, skip=()):
    """The transactions with two splits, one in a bank account, sorted by date.
    Opening balances (equity accounts) and transactions in skip are left out."""
    result = []
    for guid, date, description, splits in all_transactions:
        if len(splits) != 2 or guid in skip:
            continue
        for (bank, amount, memo), (other, _, _) in (splits, splits[::-1]):
            if bank in banks and accounts[other][2] != "EQUITY":
                result.append(Transaction(date, bank, description, memo, amount, other))
    return sorted(result, key=lambda tx: tx.date)


def effort(results):
    """Estimated manual work: no suggestion + WRONG_COST * wrong.

    Example: 91 without suggestion, 42 wrong -> 91 + 10 * 42 = 511."""
    return results["none"] + WRONG_COST * results["wrong"]


def print_results(scenarios):
    w = max(len(name) for name in scenarios)  # width of the name column
    print(f"{'':{w}} {'correct':>14} {'no suggestion':>15} {'wrong':>14} {'effort':>7}")
    for name, (results, _) in scenarios.items():
        cells = [f"{results[r]:5} ({100 * results[r] / max(1, sum(results.values())):4.1f}%)"
                 for r in ("correct", "none", "wrong")]
        print(f"{name:{w}} {cells[0]:>14} {cells[1]:>15} {cells[2]:>14} {effort(results):7}")


def print_comparison(thresholds, alphas, runs):
    """Every combination of threshold and smoothing value alpha, with
    effort = no suggestion + 10 * wrong (see effort).
    runs: {row name: (function(threshold, alpha) -> (results, per_account),
                      True if the row uses alpha)}.
    Rows without alpha (GnuCash 5.17, kNN) are
    printed once per threshold. A blank line separates the thresholds."""
    w = max(len(name) for name in runs)  # width of the name column
    print(f"\n{'':{w}} {'threshold':>9} {'alpha':>6} {'correct':>8} {'no suggestion':>14} "
          f"{'wrong':>6} {'effort':>7}")
    for threshold in thresholds:
        print()
        for name, (run, uses_alpha) in runs.items():
            for alpha in (alphas if uses_alpha else [None]):
                results, _ = run(threshold, ALPHA if alpha is None else alpha)
                alpha_cell = "-" if alpha is None else f"{alpha:g}"
                print(f"{name:{w}} {threshold:9g} {alpha_cell:>6} {results['correct']:8} "
                      f"{results['none']:14} {results['wrong']:6} {effort(results):7}")


def main():
    parser = argparse.ArgumentParser(
        description="Compare GnuCash's import account matching with new algorithms "
                    "by replaying imports. Read-only.")
    parser.add_argument("book")
    mode = parser.add_mutually_exclusive_group(required=True)
    mode.add_argument("--start-from", metavar="OLD.gnucash",
                      help="start with the learned data of OLD.gnucash (the previous year's book, or a "
                           "backup) and replay the transactions of BOOK that are not in it")
    mode.add_argument("--from-ledger", action="store_true",
                      help="start with no learned data and learn all transactions of BOOK")
    parser.add_argument("--months", type=int, default=6, help="--from-ledger: months to score (default 6)")
    parser.add_argument("--alphas", help="compare smoothing values for naive Bayes, comma separated, e.g. 0.03,0.1,0.3")
    parser.add_argument("--thresholds", help="compare thresholds, comma separated, e.g. 0.8,0.9,0.95")
    parser.add_argument("--weekdays", help="weekday names as GnuCash stored them, Monday first, "
                                           "comma separated (default: from your system language)")
    parser.add_argument("--diagnose", action="store_true",
                        help="results by payee history (new, one account, ambiguous) and calibration")
    parser.add_argument("--verbose", action="store_true", help="results per account (shows account names)")
    args = parser.parse_args()
    weekday_names = args.weekdays.split(",") if args.weekdays else default_weekday_names()

    accounts, learned, all_transactions = read_book(args.book)
    if args.start_from:
        # start: the old book's learned data; replay: what is in BOOK but not in the old book
        old_accounts, start, old_transactions = read_book(args.start_from)
        skip = {guid for guid, _, _, _ in old_transactions}
        learn_correct, first_date = False, None
    else:
        # start: nothing; replay: all of BOOK, the last N months are scored
        start, skip, old_accounts, old_transactions = new_learned_data(), set(), accounts, []
        learn_correct = True
        last_date = max(date for _, date, _, _ in all_transactions)
        first_date = last_date - datetime.timedelta(days=round(30.44 * args.months))
    banks, skipped = import_accounts(accounts, learned, start, args.start_from)
    if not banks:
        raise SystemExit("No learned import data (Bayesian matching) found.")
    transactions = bank_transactions(all_transactions, accounts, banks, skip)
    history = bank_transactions(old_transactions, old_accounts, banks)  # empty with --from-ledger
    scored = [tx for tx in transactions if first_date is None or tx.date > first_date]
    if not scored:
        raise SystemExit("No transactions to replay.")

    weekday_tokens = {t for bank in banks for t in start.get(bank, {}) if t in weekday_names}
    # Import accounts: the bank (or card) accounts that carry learned data.
    # Target accounts: the accounts the transactions were assigned to.
    entries = sum(len(counts) for bank in banks for counts in start.get(bank, {}).values())
    targets = {account for bank in banks for counts in start.get(bank, {}).values() for account in counts}
    print(f"Start: {entries} learned entries for {len(banks)} import accounts, "
          f"{len(targets)} target accounts")
    if skipped:
        print(f"Skipped: {len(skipped)} accounts with learned data in BOOK but none in the old book")
    print(f"Replayed: {len(transactions)} transactions to {len({tx.account for tx in transactions})} "
          f"target accounts, {transactions[0].date} to {transactions[-1].date}, "
          f"scored: {len(scored)} from {scored[0].date}")
    if args.start_from:
        print(f"Weekday tokens: {len(weekday_tokens)} of 7 found ({', '.join(weekday_names)})")
    print()

    # Each row is a function (threshold, alpha) -> replay results, and whether
    # it uses alpha. GnuCash 5.17 uses GnuCash's tokens and the stored data as
    # it is; the "+tok" rows the new tokens and the stored data converted to
    # them. The corrections start empty in every run, and learn every
    # transaction; the counts learn like the importer (learn_correct).
    tokens_517 = lambda tx: tokens_gnucash_517(tx, weekday_names)
    converted = {bank: convert_learned_data(start[bank], weekday_names) for bank in start}
    nb = lambda alpha: lambda lrn, tok: score_naive_bayes(lrn, tok, alpha)
    nb_log = lambda alpha: lambda lrn, tok: naive_bayes_log_scores(lrn, tok, alpha)

    runs = {
        "GnuCash 5.17": (lambda threshold, alpha, observe=None: replay(
            transactions, tokens_517, score_gnucash_517, start, learn_correct, first_date,
            threshold, observe=observe), False),
        "NB+tok": (lambda threshold, alpha, observe=None: replay(
            transactions, tokens_split, nb(alpha), converted, learn_correct, first_date,
            threshold, observe=observe), True),
        "NB+tok+corr+cut": (lambda threshold, alpha, observe=None: replay(
            transactions, tokens_split, nb_log(alpha), converted, learn_correct, first_date,
            threshold, Corrections(), concentration_cut(), observe), True),
        "damped NB+tok+cut": (lambda threshold, alpha, observe=None: replay(
            transactions, tokens_split, lambda lrn, tok: best_of(damped_log_scores(lrn, tok, alpha)),
            converted, learn_correct, first_date, threshold, None, concentration_cut(), observe), True),
        "kNN+tok": (lambda threshold, alpha, observe=None: replay_model(
            NearestNeighbours(history), transactions, first_date, threshold, observe), False),
        "damped NB+tok+kNN veto": (lambda threshold, alpha, observe=None: replay_model(
            NaiveBayesAndNeighbours(converted, history, learn_correct, alpha), transactions,
            first_date, threshold, observe), True),
    }
    # diagnoses: filled during the replays below, printed after the results
    if args.diagnose:
        kinds = payee_kinds(history, transactions)
        diagnoses = {name: Diagnosis(kinds) for name in runs}
    scenarios = {name: run(THRESHOLD, ALPHA, diagnoses[name].observe if args.diagnose else None)
                 for name, (run, _) in runs.items()}
    print_results(scenarios)
    if args.diagnose:
        print_diagnoses(diagnoses)

    if args.alphas or args.thresholds:
        thresholds = [float(t) for t in args.thresholds.split(",")] if args.thresholds else [THRESHOLD]
        alphas = [float(a) for a in args.alphas.split(",")] if args.alphas else [ALPHA]
        print_comparison(thresholds, alphas, runs)

    if args.verbose:
        print("\nPer account (correct/total, w = wrong):")
        widths = [max(17, len(name)) for name in scenarios]  # one column per row name
        print("  " + " ".join(f"{name:>{w}}" for name, w in zip(scenarios, widths)))
        per_account = [per for _, per in scenarios.values()]
        cell = lambda c: f"{c['correct']}/{sum(c.values())}" + (f" ({c['wrong']}w)" if c["wrong"] else "")
        for account in sorted(per_account[0], key=lambda a: -sum(per_account[0][a].values())):
            print("  " + " ".join(f"{cell(per[account]):>{w}}" for per, w in zip(per_account, widths)),
                  full_name(accounts, account))


if __name__ == "__main__":
    main()
