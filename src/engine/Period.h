/********************************************************************\
 * Period.h -- Implement accounting Periods                         *
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
/** @addtogroup Period Accounting Periods
    The architecture of the Accounting Period design is discussed
    in greater detail in the file "src/doc/books.txt"
    @{ */
/** @file Period.h
 *  @brief Implement accounting periods, as per design in src/doc/books.txt
 *  @author Copyright (c) 2001,2003 Linas Vepstas <linas@linas.org>
 */


#ifndef XACC_PERIOD_H
#define XACC_PERIOD_H

#include "gnc-engine.h"


/** The gnc_book_close_period() routine will 'close' a book at
 *    the indicated date.  It returns a pointer to the closed book,
 *    while the argument remains open.  This routine will move
 *    all of the older transactions from the open book to the
 *    closed book.  The guid's of the old transactions will not
 *    be changed in the move.  Note, however, that the closed
 *    book will have a copy of every account in the open book,
 *    and that these copies will have new GncGUID's issued to them.
 *    Thus, every account has a 'twin' in the other book.
 *
 *    This routine will also create 'equity transactions' in
 *    order to preserve the balances on accounts.  For any
 *    account that is not of income, expense, trading or equity type,
 *    this routine wil find the closing balance of each account
 *    in the closed book.  It will then create an 'equity
 *    transaction' in the open book, creating an opening balance
 *    between an equity account and the twin account to the
 *    closed account.  The 'memo' field will be used to set
 *    the description in the equity transaction.  Typically,
 *    you will want to set this field to _("Opening Balance").
 *
 *    The equity_account argument is supposed to indicate the
 *    equity account in the open book into which the opening
 *    balances will be placed.   This argument may be NULL,
 *    if it is NULL, then a search algorithm will be used to
 *    find a suitable equity account.  If NULL, this routine
 *    searches for the 'nearest' account of GNCAccountType ACCT_TYPE_EQUITY
 *    among its siblings, or the siblings of its parents.  It
 *    does not search downwards.  If it does not find such an
 *    account, it will create one, hanging off the top-most group.
 *
 *    This routine also populates a number of KVP values in
 *    order to make a log of the closing.  In principle, the
 *    stored KVP's should be enough to locate everything needed
 *    to safely re-open and re-close a closed book.  In particular,
 *    if a closed book is re-opened, the 'equity transaction'
 *    would need to be adjusted.
 *
 *    The kvp values that are set are:
 *
 *    Implemented in the closed book:
 *    /book/close-date       Latest date in this book. Must not change.
 *    /book/log-date         Date on which user called this routine.
 *    /book/next-book        GncGUID of next book (the still-open book).
 *
 *    Implemented in still-open book:
 *    /book/open-date        Earliest date in this book.
 *    /book/prev-book        GncGUID of previous book (the closed book).
 *
 *    Implemented in the balancing transaction:
 *    /book/closed-acct      GncGUID of account whose balance was brought forward
 *    /book/closed-book      GncGUID of book whose balance was brought forward
 *
 *    Implemented in the closed account:
 *    /book/balancing-trans  GncGUID of equity-balancing transaction.
 *    /book/next-book        GncGUID of equity-balancing book.
 *    /book/next-acct        GncGUID of twin of this account in the open book.
 *
 *    Implemented in the still-open account:
 *    /book/prev-acct         GncGUID of twin of this account in the closed book.
 *    /book/prev-book         GncGUID of previous book (the closed book)
 *
 */
QofBook * gnc_book_close_period (QofBook *, Timespec,
                                 Account *equity_acct,
                                 const char *memo);

/** The gnc_book_partition_txn() uses the result of the indicated query
 *    to move a set of transactions from the "src" book to the "dest"
 *    book.  Before moving the transactions, it will first place a
 *    copy of all of the accounts in "src" into "dest".  This is done
 *    in order to ensure that all of the moved transactions will have
 *    the corrrect set of accounts to reference.  The transactions
 *    that will be moved are precisely those specified by the query.
 *    Any query that returns a list of transactions will work to
 *    partition a book; however, its expected that this routine will
 *    mostly serve as a utility to break up a book into accounting
 *    periods.
 *
 *    This routine intentionally does not copy scheduled/recurring
 *    transactions.
 *
 *    This routine will also copy closed lots to the destination book.
 *    NOTICE:
 *    It will not copy open lots, nor will it copy lots that have
 *    lead to transactions that contains splits in other open lots.
 *    Leaving behind open lots is exactly what is needed for closing
 *    books, but it means that gnc_book_partition() is not really
 *    a 'general purpose' function.  The way to fix this would be to
 *    weed out open lots by constructing the query correctly.
 *
 *    When an account is copied, the copy is issued a new GncGUID.
 *    The GncGUID of its sibling is placed in the 'gemini' KVP value
 *    (See kvp_doc.txt for more detail).   Transactions and splits
 *    are moved without reassigning them a new GncGUID.  Note they
 *    are removed from one book's entity table and placed into the
 *    other book:  Once moved, they won't be findable in the entity
 *    table of the old book.
 *
 *    Known Bugs:
 *    When this routine copies accounts, it does not check to see
 *    if they already exist in the 'dest' book; it should.
 *    For the current usage, this bug aint important, and I'm too
 *    lazy to fix it.
 */
void gnc_book_partition_txn (QofBook *dest, QofBook *src, QofQuery *);

/** The gnc_book_partition_pricedb() routine uses te result of the
 *   indicated query to move a set of prices from the "src" book
 *   to the "dest" book.  The query passed into it must be set up
 *   to return a list of prices.
 */
void gnc_book_partition_pricedb (QofBook *dest, QofBook *src, QofQuery *);

/** The gnc_book_insert_trans_clobber() routine takes an existing
 *    transaction that is located in one book, and moves it to
 *    another book.  It moves all of the splits as well.  In the
 *    course of the move, the transaction is literally deleted
 *    from the first book as its placed into the second.  The
 *    transaction and split GncGUID's are not changed in the move.
 *    This routine assumes that twin accounts already exist in
 *    both books (and can be located with the standard twining
 *    proceedure).
 *
 *    Note that this routine does *not* move the lots that any
 *    of the splits might belong to.  These must be moved sepearately.
 *    Note that one must take care when moving a transaction, so
 *    that any associated lots don't end up hamstrung across two
 *    different books.
 *
 * The gnc_book_insert_trans() routine does the same as the above,
 *    except that it doesn't actually clobber the transaction: it
 *    merely moves the transaction and split GncGUID's to the new
 *    books' entity tables, and not much else.
 *
 * The gnc_book_insert_lot() routine, as above, but for lots ...
 */

void gnc_book_insert_trans (QofBook *book, Transaction *trans);
void gnc_book_insert_trans_clobber (QofBook *book, Transaction *trans);

void gnc_book_insert_lot (QofBook *book, GNCLot *lot);
void gnc_book_insert_lot_clobber (QofBook *book, GNCLot *lot);

void gnc_book_insert_price (QofBook *book, GNCPrice *prc);
void gnc_book_insert_price_clobber (QofBook *book, GNCPrice *prc);

#endif /* XACC_PERIOD_H */
/** @} */
/** @} */
