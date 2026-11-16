#!/usr/bin/env python3
"""
gnc-bayes-eval.py - how well does GnuCash guess the account of imported transactions,
and would a proposed new algorithm do better?

    python3 gnc-bayes-eval.py BOOK.gnucash --start-from OLD.gnucash [--alphas 0.03,0.1,0.3] [--thresholds 0.8,0.9,0.95]
    python3 gnc-bayes-eval.py BOOK.gnucash --from-ledger [--months 6] [--alphas 0.03,0.1,0.3] [--thresholds 0.8,0.9,0.95]
        further options: [--weekdays Mon,Tue,...] [--verbose]

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
transactions day by day, asks five algorithms which account they would
suggest for each of them, and then learns the transaction:

 - "GnuCash 5.17": the current algorithm.
 - "naive Bayes": a multi-class naive Bayes classifier, words as in GnuCash 5.17.
 - "+ tokenizer": the same, with words lower-cased and split at every character
   that is not a letter or digit. Stored learned data is converted on the fly;
   nothing has to be reset.
 - "NB + corrections": naive Bayes plus learned corrections (see Corrections),
   words as in GnuCash 5.17. It learns every transaction, also accepted
   suggestions (a change to the importer), because the corrections need to
   see the correct cases too.
 - "NB + corr. + tok.": the same with the tokenizer.

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
account by hand, not when you accept a suggestion. --learn-accepted learns
accepted suggestions too, to see what that change to the importer would give. Accounts and transactions
are matched by their GUIDs, which a copy or backup keeps.

--from-ledger: start with no learned data at all, and learn every transaction
of BOOK (this is a proposal to rebuild the learned data from the booked
transactions, so earlier wrong assignments that were corrected no longer
count). Transactions of the last N months (--months, default 6) are scored.
This only knows the history contained in BOOK: if you start a new book every
year, the first months have little to learn from.

--alphas compares values of the smoothing parameter ALPHA of naive Bayes (see
score_naive_bayes), e.g. 0.03, 0.1 (proposed) and 0.3; 0 is strict naive Bayes.
--thresholds compares values of the threshold a suggestion has to reach,
e.g. 0.8, 0.9 (GnuCash) and 0.95. GnuCash's threshold is a user preference,
and naive Bayes may be more (or less) cautious than GnuCash 5.17, so the best
value need not be the same for both. Given one or both options, the script
replays every combination and prints, per threshold, the "GnuCash 5.17" row
(it has no alpha) and the "naive Bayes" and "+ tokenizer" rows for each alpha,
plus the manual effort estimated as:

    effort = no suggestion + 3 * wrong

A wrong suggestion is more work than a missing one (it has to be noticed and
corrected instead of chosen), and it may be overlooked and end up in the
books. So it counts three times (WRONG_COST).
"""

import argparse
import calendar
import datetime
import gzip
import itertools
import locale
import math
import re
import sqlite3
import xml.etree.ElementTree as ET
from collections import Counter, defaultdict, namedtuple

THRESHOLD = 0.90  # GnuCash only suggests an account if its probability is at least 90 %
ALPHA = 0.1       # smoothing for the proposed algorithm (see score_naive_bayes)
RATE = 0.3        # learning rate of the corrections (see Corrections)
WRONG_COST = 3    # a wrong suggestion counts as much as this many missing ones (see effort)
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
    return datetime.date.fromisoformat(text[:10])


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
    """Proposed tokenizer: lower case, split at every character that is not a
    letter or a digit (also at "_", like GnuCash's g_unichar_isalnum)."""
    return {w for w in re.split(r"[\W_]+", text.lower()) if w}


def tokens_proposed(tx, weekday_names):
    """Proposed: the same tokens as GnuCash 5.17, but split with split_words.
    The weekday is lower-cased too, so it matches the converted stored data."""
    return split_words(tx.description + " " + tx.memo) | {weekday_names[tx.date.weekday()].lower()}


def convert_learned_data(learned):
    """Re-tokenize the stored tokens with the proposed tokenizer, e.g.
    "PROBERAUM;" -> "proberaum", "Montag" -> "montag". Counts of tokens that
    become equal are added up. This is so the gnucash file will stay compatible
    to older gnucash-versions in case the tokenizer change is implemented."""
    converted = {}
    for token, counts in learned.items():
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
    """Proposed: a multi-class naive Bayes classifier (multinomial, with smoothing).

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
    """Proposed: naive Bayes plus learned corrections (logistic regression on
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
# Evaluation
# ---------------------------------------------------------------------------

def judge(tx, suggestion, probability, threshold=THRESHOLD):
    """Compare a suggestion with the account actually booked."""
    if suggestion is None or probability < threshold:
        return "none"
    return "correct" if suggestion == tx.account else "wrong"


def replay(transactions, make_tokens, score, start=None, learn_correct=True, first_date=None,
           threshold=THRESHOLD, corrections=None):
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
        return naive Bayes log scores (naive_bayes_log_scores), and every
        transaction is learned.

    Transactions of the same day are not learned from each other, since they
    typically arrive in the same import. Learning works as in GnuCash: every
    token of the transaction adds 1 to count(token, booked account)."""
    learned = new_learned_data()
    for bank, tokens in (start or {}).items():
        for token, counts in tokens.items():
            learned[bank][token] = dict(counts)
    def suggest(tx):
        if corrections is None:
            return score(learned[tx.bank], make_tokens(tx))
        log_score = score(learned[tx.bank], make_tokens(tx))
        return best_of(corrections.corrected(tx.bank, log_score, make_tokens(tx)))

    results = Counter()
    per_account = defaultdict(Counter)
    for date, day in itertools.groupby(transactions, key=lambda tx: tx.date):
        to_learn = list(day)
        if first_date is None or date > first_date:
            day, to_learn = to_learn, []
            for tx in day:
                result = judge(tx, *suggest(tx), threshold)
                results[result] += 1
                per_account[tx.account][result] += 1
                if learn_correct or corrections is not None or result != "correct":
                    to_learn.append(tx)
        for tx in to_learn:
            if corrections is not None:
                # the corrections learn from naive Bayes' error before the counts change
                corrections.learn(tx.bank, score(learned[tx.bank], make_tokens(tx)), make_tokens(tx), tx.account)
            for token in make_tokens(tx):
                counts = learned[tx.bank][token]
                counts[tx.account] = counts.get(tx.account, 0) + 1
    return results, per_account


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

    Example: 91 without suggestion, 42 wrong -> 91 + 3 * 42 = 217."""
    return results["none"] + WRONG_COST * results["wrong"]


def print_results(scenarios):
    print(f"{'':17} {'correct':>14} {'no suggestion':>15} {'wrong':>14}")
    for name, (results, _) in scenarios.items():
        cells = [f"{results[r]:5} ({100 * results[r] / max(1, sum(results.values())):4.1f}%)"
                 for r in ("correct", "none", "wrong")]
        print(f"{name:17} {cells[0]:>14} {cells[1]:>15} {cells[2]:>14}")


def print_comparison(thresholds, alphas, runs):
    """Every combination of threshold and smoothing value alpha, with
    effort = no suggestion + 3 * wrong (see effort).
    runs: {row name: (function(score, threshold) -> (results, per_account),
                      function(alpha) -> score)};
    the first row is GnuCash 5.17, which has no alpha.
    A blank line separates the groups, wherever threshold or alpha changes."""
    def print_row(name, threshold, alpha):
        run, make_score = runs[name]
        results, _ = run(make_score(alpha), threshold)
        alpha_cell = "-" if alpha is None else f"{alpha:g}"
        print(f"{name:17} {threshold:9g} {alpha_cell:>6} {results['correct']:8} "
              f"{results['none']:14} {results['wrong']:6} {effort(results):7}")

    baseline, *naive_bayes = runs
    print(f"\n{'':17} {'threshold':>9} {'alpha':>6} {'correct':>8} {'no suggestion':>14} "
          f"{'wrong':>6} {'effort':>7}")
    for threshold in thresholds:
        print()
        print_row(baseline, threshold, None)
        for alpha in alphas:
            print()
            for name in naive_bayes:
                print_row(name, threshold, alpha)


def main():
    parser = argparse.ArgumentParser(
        description="Compare GnuCash's import account matching with a proposed new algorithm "
                    "by replaying imports. Read-only.")
    parser.add_argument("book")
    mode = parser.add_mutually_exclusive_group(required=True)
    mode.add_argument("--start-from", metavar="OLD.gnucash",
                      help="start with the learned data of OLD.gnucash (the previous year's book, or a "
                           "backup) and replay the transactions of BOOK that are not in it")
    mode.add_argument("--from-ledger", action="store_true",
                      help="start with no learned data and learn all transactions of BOOK")
    parser.add_argument("--months", type=int, default=6, help="--from-ledger: months to score (default 6)")
    parser.add_argument("--learn-accepted", action="store_true",
                        help="--start-from: also learn transactions whose suggestion was correct "
                             "(GnuCash does not do this; for comparison)")
    parser.add_argument("--alphas", help="compare smoothing values for naive Bayes, comma separated, e.g. 0.03,0.1,0.3")
    parser.add_argument("--thresholds", help="compare thresholds, comma separated, e.g. 0.8,0.9,0.95")
    parser.add_argument("--weekdays", help="weekday names as GnuCash stored them, Monday first, "
                                           "comma separated (default: from your system language)")
    parser.add_argument("--verbose", action="store_true", help="results per account (shows account names)")
    args = parser.parse_args()
    weekday_names = args.weekdays.split(",") if args.weekdays else default_weekday_names()

    accounts, learned, all_transactions = read_book(args.book)
    if args.start_from:
        # start: the old book's learned data; replay: what is in BOOK but not in the old book
        _, start, old_transactions = read_book(args.start_from)
        skip = {guid for guid, _, _, _ in old_transactions}
        learn_correct, first_date = args.learn_accepted, None
    else:
        # start: nothing; replay: all of BOOK, the last N months are scored
        start, skip = new_learned_data(), set()
        learn_correct = True
        last_date = max(date for _, date, _, _ in all_transactions)
        first_date = last_date - datetime.timedelta(days=round(30.44 * args.months))
    banks, skipped = import_accounts(accounts, learned, start, args.start_from)
    if not banks:
        raise SystemExit("No learned import data (Bayesian matching) found.")
    transactions = bank_transactions(all_transactions, accounts, banks, skip)
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

    # Each run replays with one kind of tokens and learned data; the scoring
    # function (which depends on alpha) is given later. GnuCash 5.17 always
    # uses its own tokens. The corrections start empty in every run.
    tokens_517 = lambda tx: tokens_gnucash_517(tx, weekday_names)
    tokens_new = lambda tx: tokens_proposed(tx, weekday_names)
    converted = {bank: convert_learned_data(start[bank]) for bank in start}
    nb = lambda alpha: lambda lrn, tok: score_naive_bayes(lrn, tok, alpha)
    nb_log = lambda alpha: lambda lrn, tok: naive_bayes_log_scores(lrn, tok, alpha)
    runs = {
        "GnuCash 5.17": (lambda score, threshold=THRESHOLD:
            replay(transactions, tokens_517, score, start, learn_correct, first_date, threshold),
            lambda alpha: score_gnucash_517),
        "naive Bayes": (lambda score, threshold=THRESHOLD:
            replay(transactions, tokens_517, score, start, learn_correct, first_date, threshold), nb),
        "+ tokenizer": (lambda score, threshold=THRESHOLD:
            replay(transactions, tokens_new, score, converted, learn_correct, first_date, threshold), nb),
        "NB + corrections": (lambda score, threshold=THRESHOLD:
            replay(transactions, tokens_517, score, start, True, first_date, threshold, Corrections()),
            nb_log),
        "NB + corr. + tok.": (lambda score, threshold=THRESHOLD:
            replay(transactions, tokens_new, score, converted, True, first_date, threshold, Corrections()),
            nb_log),
    }
    scenarios = {name: run(make_score(ALPHA)) for name, (run, make_score) in runs.items()}
    print_results(scenarios)

    if args.alphas or args.thresholds:
        thresholds = [float(t) for t in args.thresholds.split(",")] if args.thresholds else [THRESHOLD]
        alphas = [float(a) for a in args.alphas.split(",")] if args.alphas else [ALPHA]
        print_comparison(thresholds, alphas, runs)

    if args.verbose:
        print("\nPer account (correct/total, w = wrong):")
        print("  " + " ".join(f"{name:>17}" for name in scenarios))
        per_account = [per for _, per in scenarios.values()]
        cell = lambda c: f"{c['correct']}/{sum(c.values())}" + (f" ({c['wrong']}w)" if c["wrong"] else "")
        for account in sorted(per_account[0], key=lambda a: -sum(per_account[0][a].values())):
            print("  " + " ".join(f"{cell(per[account]):>17}" for per in per_account),
                  full_name(accounts, account))


if __name__ == "__main__":
    main()
