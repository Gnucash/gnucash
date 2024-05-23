/********************************************************************\
 * gnc-lot.h -- AR/AP invoices; inventory lots; stock lots          *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
\********************************************************************/
/** @addtogroup Engine
    @{ */
/** @addtogroup Lot Lots: Core Function for AR/AP, Inventory, Stock Lots, Cap Gains
 * One often needs to know that the item 'bought' in one transaction
 * is the same one as the item 'sold' in a different transaction.
 * Lots are used to make this association.  One Lot holds all of the
 * splits that involve the same item.   A lot is typically formed when
 * the item is bought, and is closed when the item is sold out.
 * A lot need not be a single item, it can be a quantity of the same
 * thing e.g. 500 gallons of paint (sold off a few gallons at a time).
 *
 * Lots are required to correctly implement invoices, inventory,
 * depreciation and stock market investment gains. See the file
 * src/doc/lots.txt for a detailed implementation overview.
 *
 * A lot is "closed" when the number of items in the lot has gone to zero.
 * It is very easy to compute the gains/losses for a closed lot: it is the
 * sum-total of the values of the items put into/taken out of the lot.
 * (Realized) Gains on still-open lots can be computed by pro-rating the
 * purchase prices.
 *
 * Lots are nothing more than a collection or grouping of splits in an
 * account. All of the splits in a lot must belong to the same account;
 * there's no mix-n-match.  Thus, in this sense, a lot belongs to an
 * account as well.
 *
 * Lots have an implicit "opening date": the date of the earliest split in
 * the lot. The "close date" is the date of the split that brought the lot
 * item balance down to zero.
 *
 @{ */

/** @file gnc-lot.h
 *
 * @author Created by Linas Vepstas May 2002
 * @author Copyright (c) 2002,2003 Linas Vepstas <linas@linas.org>
 */

#ifndef GNC_LOT_H
#define GNC_LOT_H

//typedef struct _GncLotClass GNCLotClass;

#include "qof.h"
#include "gnc-engine.h"
/*#include "gnc-lot-p.h"*/
#include "gncInvoice.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct
{
    QofInstanceClass parent_class;
} GncLotClass;
#define GNCLotClass GncLotClass

/* --- type macros --- */
#define GNC_TYPE_LOT            (gnc_lot_get_type ())
#define GNC_LOT(o)              \
     (G_TYPE_CHECK_INSTANCE_CAST ((o), GNC_TYPE_LOT, GNCLot))
#define GNC_LOT_CLASS(k)        \
     (G_TYPE_CHECK_CLASS_CAST((k), GNC_TYPE_LOT, GNCLotClass))
#define GNC_IS_LOT(o)           \
     (G_TYPE_CHECK_INSTANCE_TYPE ((o), GNC_TYPE_LOT))
#define GNC_IS_LOT_CLASS(k)     \
     (G_TYPE_CHECK_CLASS_TYPE ((k), GNC_TYPE_LOT))
#define GNC_LOT_GET_CLASS(o)    \
     (G_TYPE_INSTANCE_GET_CLASS ((o), GNC_TYPE_LOT, GNCLotClass))
GType gnc_lot_get_type(void);


/*@ dependent @*/
GNCLot * gnc_lot_new (QofBook *);
void gnc_lot_destroy (GNCLot *);

/*@ dependent @*/
GNCLot * gnc_lot_lookup (const GncGUID *guid, QofBook *book);
QofBook * gnc_lot_get_book (GNCLot *);

void gnc_lot_begin_edit (GNCLot *lot);
void gnc_lot_commit_edit (GNCLot *lot);

/** Adds a split to this lot.
 *
 *  @note
 *  - *All* splits in a lot must be in the same account.
 *  - Splits are added unconditionally, with
 *    no regard for the accounting policy.  To enforce a particular
 *    accounting policy, use the xaccSplitAssignToLot() routine
 *    instead.
 */
void gnc_lot_add_split (GNCLot *, Split *);

/** Adds a split from this lot.
 */
void gnc_lot_remove_split (GNCLot *, Split *);

/** Returns a list of all the splits in this lot.
 *
 *   @returns GList
 *
 *   This GList is owned and managed by the lot.
 *   - Do *not* free it when done.
 *   - Do *not* modify it directly
 *   - Calls to either gnc_lot_add_split() or gnc_lot_remove_split()
 *     will invalidate the returned pointer
 */
SplitList * gnc_lot_get_split_list (const GNCLot *);
gint gnc_lot_count_splits (const GNCLot *);

/** Returns the account with which this lot is associated.
 */
/*@ dependent @*/
Account * gnc_lot_get_account (const GNCLot *);
void gnc_lot_set_account(GNCLot*, Account*);

/** Returns the invoice with which this lot is associated.
 */
/*@ dependent @*/
GncInvoice * gnc_lot_get_cached_invoice (const GNCLot *lot);
void gnc_lot_set_cached_invoice(GNCLot* lot, GncInvoice *invoice);

/** Returns the lot balance.
 *  This balance will be expressed in the lot account's commodity.
 */
gnc_numeric gnc_lot_get_balance (GNCLot *);

/** Computes both the balance and value in the lot considering only splits
 *  in transactions prior to the one containing the given split or other
 *  splits in the same transaction.
 *  The first return value is the amount and the second is the value. */
void gnc_lot_get_balance_before (const GNCLot *, const Split *,
                                 gnc_numeric *, gnc_numeric *);

/** Returns closed status of the given lot.
 *  A lot is closed if its balance is zero.  This
 *  routine is faster than using gnc_lot_get_balance() because
 *  once the balance goes to zero, this fact is cached.
 *
 *  @returns boolean
 */
gboolean gnc_lot_is_closed (GNCLot *);

/** Convenience routine to identify the earliest date in the lot.
 *  It loops over all of the splits in the lot, and returns the split
 *  with the earliest split->transaction->date_posted.  It may not
 *  necessarily identify the lot opening split.
 */
Split * gnc_lot_get_earliest_split (GNCLot *lot);

/** Convenience routineto identify the date this lot was closed.
 *  It simply loops over all of the splits in the lot, and returns
 *  the split with the latest split->transaction->date_posted.
 */
Split * gnc_lot_get_latest_split (GNCLot *lot);

/** Reset closed flag so that it will be recalculated. */
void gnc_lot_set_closed_unknown(GNCLot*);

/** @name Get/set the account title and notes.
    @{ */
const char * gnc_lot_get_title (const GNCLot *);
const char * gnc_lot_get_notes (const GNCLot *);
void gnc_lot_set_title (GNCLot *, const char *);
void gnc_lot_set_notes (GNCLot *, const char *);
/** @} */

/** @todo Document this function ? */
GNCLot * gnc_lot_make_default (Account * acc);

#define gnc_lot_get_guid(X)  qof_entity_get_guid(QOF_INSTANCE(X))

#define LOT_IS_CLOSED   "is-closed?"
#define LOT_BALANCE     "balance"
#define LOT_TITLE       "lot-title"
#define LOT_NOTES       "notes"

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* GNC_LOT_H */
/** @} */
/** @} */

/** \page lotsoverview Lots Architecture & Implementation Overview

               Linas Vepstas <linas@linas.org>
                  Last Revised May 2004

API \ref Lot

One often needs to know that the item 'bought' in one transaction
is the same one as the item 'sold' in a different transaction.
Lots are used to make this association.  One Lot holds all of the
splits that involve the same item.   A lot is typically formed when
the item is bought, and is closed when the item is sold out.
A lot need not be a single item, it can be a quantity of the same
thing e.g. 500 gallons of paint (sold off a few gallons at a time).
Lots are required to correctly implement invoices, inventory,
depreciation and stock market investment gains.

'Lots' capture a fundamental accounting idea behind AR/AP, billing,
inventory, capital gains, depreciation and the like.  The basic idea
is that a set of items is tracked together as a 'lot'; the date of
creation of the lot helps determine when a bill is due, when depreciation
starts, or the tax rate for capital gains on a stock market investment.

\section lotsdefines Definition

In GnuCash, a 'lot' will consist of a set of splits identified with
a unique lot number.  Any given split can belong to one lot and one
lot only.  All splits in a lot must also belong to the same account.
Lots have a 'balance': the sum of all the splits in the lot.  A lot
is 'opened' when the first split is assigned to it.  The date of that
split is the 'opening date' of the lot, and its quantity is the
'opening balance'.  A lot is 'closed' when its balance is zero;
once closed, a lot cannot be re-opened.   Open lots are always carried
forward into the current open book; closed lots are left behind in the
book in which they were closed.

\section How Lots Are Used

The following sections review how lots are used to implement various
accounting devices, and some of the related issues.

\subsection Billing

Tracking lots is the 'definition' of A/R and A/P.  You want to be able
to say that this bill is 120 days old, and its balance has been partly
paid off; and you want to be able to do this whether or not there are
also other bills, of different ages, to the same company.  In GnuCash,
a 'bill' is tracked as a lot:  The 'billing date' is the opening date
of the lot, and the 'balance due' is the balance of the lot.  When the
bill is 'paid in full', the lot is closed.  The average age of
receivables can be computed by traversing all lots, etc.  Additional
information about bills, such as a due date or payment terms, should
be stored as kvp values associated with the lot.

\subsection Billing Example

Normally, there is a one-to-one correspondence between bills and
lots: there  is one lot per bill, and one bill per lot.  (Note: this
is not how gnucash invoices are currently implemented; this is a
proposed implementation.)

For example:
\verbatim
   invoice #258    customer XXX
   Order placed 20 December 2001
       quant (10)  gallons paint      $10/each   $100
       quant (2)   paintbrushes       $15/each    $30
                   sales tax                       $8.27
                                              ------------
                Total due                        $138.27

   Payment received 24 january  2002  $50      Balance Due: $88.27
   Payment received 13 february 2002  $60      Balance Due: $28.27
   Payment received 18 march    2002  $28.27   PAID IN FULL
\endverbatim

In this example, the lot encompasses four transactions, dated
December, January, February and March.  The December transaction
opens the lot, and gives it a non-zero balance.   To be precise,
the lot actually consists of four splits, belonging to four
different transactions.  All four splits are a part of an imagined
"Accounts Receivable-Billing" account. The first split is for
$138.27, the second split is for $50, the third split is for $60,
and the fourth split is for $28.27.   Note that the sales-tax
split, and th paint/paint-brush splits are *NOT* a part of this
lot. They are only a part of the transaction that opened this lot.

Note also that this example might also encompass two other lots:
the transfer of paint may belong to a lot in the "Paint Inventory"
account, and the split describing the paintbrushes might be a part
of a lot in the "Paintbrush Inventory" account.   These lots should
not be confused with the invoice lot.

\subsection lotsinventory Inventory

The correct way of handling inventory under GnuCash is to have a
separate account for each widget type.  The account balance represents
how many widgets there are in the warehouse.  Lots offer an additional
level of detail that can be useful when, for example, the widgets have
an expiration date (e.g. milk) or vary slightly from batch to batch
(e.g paint), and creating a new account to track these differences
would be too heavy-weight.

In order to track widgets as single units (and prohibit fractional
widgets), set the currency denominator to 1.  This will prevent
fractional widgets from being transferred into/out of an account.

Note that using accounts to track the inventory of a grocery store
causes an explosion of accounts, and this explosion would overwhelm
many of the account GUI's in GnuCash.  The GUI should probably be
modified to treat inventory accounts in a special way, e.g. by
not listing them in account lists, and providing an alternate
management GUI.

\subsection Capital Gains

In the United States, gains on stock investments are taxed at different
rates depending on how long they were held before being sold. By using
lots with an investment account, it becomes easy to track when any given
share was bought or sold, and thus, the length of time that share was
held.

Note, however, that using lots might cause some confusion for naive
users, since some transactions might be split across different lots.
For example, the user may have conceptually made the following
transactions:

\verbatim
>        Date     Desc    Buy     Sell    Price    Value
>        18/1/01  XCORP   500             $10.00   $5000.00
>        21/3/01  XCORP   500             $12.00   $6000.00
>        14/7/02  XCORP           750     $20.00  $15000.00
\endverbatim

However, the two buy transactions create different lots (because
you can't add to a stock-investment lot after its been created, because
the purchases occurred on different dates).  Thus, when the shares are
sold, the sale is split across two lots:

\verbatim
>        Date     Desc    Lot     Buy     Sell    Price    Value
>        18/1/01  XCORP   187     500             $10.00   $5000.00
>        21/3/01  XCORP   188     500             $12.00   $6000.00
>        14/7/02  XCORP   187             500     $20.00  $10000.00
>        14/7/02  XCORP   188             250     $20.00   $5000.00
\endverbatim

In the above, lot 187 was closed, and lot 188 has a balance of 250
shares.  Note that we used a FIFO accounting method in this example:
the oldest shares were sold first.

Note also, that by using lots in this example, we are able to accurately
compute the gains in this transaction: it is 500*($20-$10) + 250*($20-$12)
= $5000+$2000 = $7000.  If we had used LIFO accounting, and sold the
youngest shares first, then the profits would have been 500*($20-$12)
+ 250*($20-$10) = $4000 + 2500 = $6500.  Thus, different accounting
methods do affect income, and thus the tax rate.

Note that the two ledgers, the 'without-lots' and the 'with-lots'
ledgers look different, even though the conceptual transactions are
the same.  If a naive user was expecting a 'without-lots' ledger,
and is shown a 'with lots' ledger, they may get confused.  I don't
know of any simple way of dealing with this. In order to have lots
work (thereby allowing cap gains reports to work correctly), the GnuCash
engine *must* use the 'with-lots' representation of the data.  If the
GUI wants to hide the fact that there are lots under the covers, it
must figure out a way of doing this (i.e. re-combining splits) on
its own.

\section lotsclosing Closing of Books

A few words on lots and the closing of books.  Without lots, there
is really no way of correctly implementing book closing.  That is
because some reports, such as the capital-gains report, need to know
not only the account balance, but also the purchase dates.  Lots
provide the natural mechanism for storing this information.

When a book is closed, any open lots must be moved into/kept with
the open book.  Since the splits in a lot belong to transactions,
and transactions are 'atomic', this means that the associated
transactions must be moved into the open book as well.  A lot
is considered closed when its balance is zero; when a book is closed,
all of the lots that were closed stay with that book.  That is,
closed lots are not propagated forward into the currently open book.

Actually, its slightly more subtle than that. Not only must open
lots stay with the open book, but so must all transactions that
have splits that participate in the open lot, and, by extension,
all closed lots that participate in these 'open transactions',
ad infinitum.

\section lotsdouble The "Double Balance" Requirement

The following is a proposal for how to handle both realized
and unrealized gains/losses by means of "adjusting transactions."
It works for simple cases, but has issues for more complex cases.


Canonical transaction balancing: If all splits in a transaction
are in the same 'currency' as the transaction currency, then the
sum of the splits *must* equal zero.  This is the old, existing
double-entry requirement as implemented in Gnucash, and doesn't
change.

If some splits are in one commodity, and others in another, then
we can't force a zero balance as above.  Instead, we will force
a different requirement, the 'double-balance' requirement:

- All splits that are *not* in the transaction currency C
   must be made a part of a lot.   (Thus, for example,
   the transaction currency C is dollars, whereas the split
   commodity is 'S', shares of RHAT.  If a transaction
   has C in dollars, and 'S' in RHAT, then the S split must
   be made a part of a Lot.)
- The lot will have a designated 'lot currency' L that must
   be the same as the C of every split in the lot.  One
   cannot enter a split into the lot if C != L.  (That is,
   if I'm working with a Lot of RHAT shares, then *all*
   splits in the lot must belong to dollar-denominated
   transactions.)
- When a lot is closed, we must have the 'double-balance'
   condition:  The sum total of all 'S' is zero, and the
   sum total of all 'C' is zero.  Thus, if I buy 100 shares
   of RHAT for $5 and sell 100 shares of RHAT for $10, then
   I *must* also add an 'adjusting transaction' for zero
   shares of RHAT, at $500.  If there is no adjusting transaction,
   then the lot cannot be closed.  If sum 'S' is zero,
   while sum 'C' is not zero, then the lot is declared to
   be 'out-of-balance', and an 'adjusting transaction' must
   be forced.

It is only by 'closing a lot' that one is able to regain
'perfect balance' in the books.   That is, the 'double-balance'
requirement is the generalization of the 'double-entry'
requirement for stock accounts.

Note that because the 'adjusting transaction' has one split
in dollars, and another split in RHAT shares (albeit for zero
RHAT shares), it evades the old double-entry requirement,
and will not be flagged as 'out of balance'.  Note also
that because the 'adjusting transaction' contains a split
holding S (albeit zero S), it *must* be a part of a Lot.


The above seems to work for simple stock-transactions, but
fails in other more complex cases.   Here's an example.

Imagine 'S' is in euros, instead of 'RHAT'.  So I sell
100 dollars, and buy 110 euros.  This causes a lot to open
up for the euros, with the lot currency 'L' in dollars.
Now I try to transfer the euros to other euro accounts.
What happens to the lot?  Do I have to give up on it?
How can I save this bad situation?

A similar problem occurs for more complex stock transactions:
If I buy 100 shares of RHAT with Schwab, and transfer them
to another account with Etrade, then I have the same lot
closing problem.   There's an even worse scenario, where
I move to Brazil, and take my RHAT stock (purchased in dollars)
to my Brazilian broker (who will sell them for cruzeiros).

Is the correct answer to just 'punt' in these cases?
How is the closing of books to be handled in such a case?

GUI Elements:
- The user should be able to specify a default 'realized-gain'
   account that is associated with a stock account.
- The user should be able to specify a default 'unrealized-gain'
   account that is associated with a stock account.

\section lotsfifo FIFO's

What is a FIFO ?  A FIFO is a type of accounting policy where
income from selling inventory is accounted by selling the oldest
inventory first.  Thus, the profit/loss of the sale corresponds
to the difference in price between when it was bought and sold.
FIFO's are also used in depreciation schedules, and in other places.

Currently the only policy that is implemented in the cap-gains
code is the FIFO policy.  I believe that it's been abstracted
properly, so that it should be easy to add other policies,
e.g. LIFO.  See policy.c for what would need to be implemented.

\section lotsimplement Implementation

Every split has a pointer to a lot (which may be null).   A lot has a list
of splits in it (so that the other splits in the lot can be easily found).
A lot does not need to maintain a balance (this is easy enough to calculate
on the fly).  A lot has a kvp tree (for storage of lot-related date, such
as due dates for invoices, etc.  A lot has a GUID.

From the memory-management and data-base management point of view, lots
belong to accounts.  The GnuCash account structure maintains a list of
lots so that all lots belonging to an account can be quickly retrieved.
(In principle, the lots can be found by scanning every split in the
account, but this is a painful process.)

\section lotscapgains Implementing Cap Gains (Is a Pain in the Neck)

Although Lots provide a good conceptual framework for determining
gains or losses when a lot is closed, cap-gains on half-open
lots present additional complexities.   Consider the following
stock purchase and subsequent sale of half the stock:

Account A is a stock account

Account B is a bank account

Account C is an income account
\verbatim
                Acct A       Txn    Acct B    Acct C
Date     Action  Amt    Prc  Value    Amt      Amt
1/1/01   buy     100s   $10  $1000   ($1000)   -
2/2/02   sell    (50)s  $25  $1250    $1250    -
2/2/02   gain     -      -    $750            $750
\endverbatim

The gain, shown in the third line, is computed as a straight
sum of purchase price to sale price.

Should the above be represented as two transactions, or as three?
One could, in principle, combine the second and third lines into
one transaction.  However, this has some drawbacks:  computing
the overall balance of such a transaction is tricky, because
it has so many different splits (including, possibly, splits
for brokerage fees, tax, etc. not shown).   The alternative
is to represent each line as a separate transaction.  This has
other drawbacks:  If the date, amount, price or value is adjusted
for the second transaction, the corresponding values must be
adjusted for the third, and vice-versa.

Both schemes pose trouble for the register display: we want
the stock register to show the gain as if it were a part of
the stock sale; but the third line is a pair of splits, and
we want to show only one of these two splits.   Whichever method
is chosen, the register has to filter out some of the splits
that it shows.

The implementation that seems best is to represent the sale
with two separate transactions: one for the sale itself, and a
separate one for the gains.  This makes computing the balance
easier, although it makes the logic for setting the date
more complex.  Ughh..

\section loanscapnotes Cap Gains Implementation Notes

Cap Gains will be handled by GnuCash as described above, using
two distinct transactions.  These transactions will be marked up
using KVP, pointing to each other, so that the one can be found
from the other.  Implementation in src/engine/cap-gains.c

Quick API Overview:
-  xaccSplitGetCapGains(): Returns the capital gains associated with
      a split.  Split must have been a sale/purchase in a previously
      opened lot.
-  xaccSplitAssignToLot(): If a split is not already in a lot,
      then it places it into a lot, using a FIFO accounting policy.

\section lotscapimplement Cap Gains Actual Implementation

Cap Gains are noted by creating a separate transaction with two
splits in it.  One of the splits is as described above: zero
amount, non-zero value.   There is a GUI requirement that when
the looking at a gains transaction, certain things need to be
kept in sync with the transaction that is the source of the gains.
In order to accomplish this, the engine uses a set of 'dirty'
flags, and a pair of pointers between the gains split and the
source split, so that the one can be quickly found from the other.

Things kept in sync:
- date posted
- value
- void status
- other things ?

Things not kept in sync:
- kvp trees
- description, memo, action.

The posted date is kept in sync using a data-constraint scheme.
If xaccTransactionSetDatePosted() is called, the date change is
accepted, and the split is marked date-dirty.  When the transaction
is committed (using xaccTransCommitEdit()), the date-dirty flag
is evaluated, and, if needed, the date changes are propagated/rolled
back on the appropriate gains splits.  Currently, one can only change
the date on the gains-source transaction; the date on the
gains-recording split cannot be changed.

The value recorded by the gains transaction is updated whenever
the value of the source changes.  The actual update is done by
the xaccSplitComputeCapGains() routine, via xaccScrubLot(), which
is called at the time of xaccTransCommitEdit().  Note that two
different things can affect the gains: a change in the value of
the sale, and a change of the value of the purchase.  A set of
dirty flags are used to track these.

If the amount of a split changes, then the lot that its in becomes
potentially unbalanced.  This requires the lot membership to be
recomputed; this in turn may require the split to be split into
pieces, or to be recombined into one from several pieces.

\section lotsconversion Conversion

As Lots are put into production, old GnuCash datasets
will need to be converted.   Conversion will be done by running
all splits in an account through an accounting policy (currently,
there is only one policy, a FIFO).  The goal of the policy is to
match up purchases and sales so that these can be assigned to a Lot.

The conversion algorithm will work as follows:
\verbatim
for each account {
   loop over splits {
      // perform the 'double-balance' check
      if (split commodity != transaction currency) account needs conversion
   }
   if account needs conversion
   for each split {
      If (split amount > 0)  create new lot, put split in lot.
      If (split amount < 0)  find oldest lot, put split in that lot
   }
}
\endverbatim

See the file Scrub2.h for details of the low-level API, and Scrub3.h
for the high-level API.

There is a bit of a problem with this conversion procedure: If the
user had previously recorded cap gains using a 'handmade' version of
lots, those cap gains will be ignored and will throw off balances.
User will need to hand-edit to recover.


*/
