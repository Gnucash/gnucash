/*
 * FILE:
 * Period.h
 *
 * FUNCTION:
 * Implement accounting periods.
 *
 * CAUTION: this is currently a semi-functional, untested implementation
 * of the design described in src/doc/book.txt
 *
 * HISTORY:
 * created by Linas Vepstas November 2001
 * Copyright (c) 2001 Linas Vepstas <linas@linas.org>
 */


#ifndef XACC_PERIOD_H
#define XACC_PERIOD_H

#include "gnc-book.h"
#include "gnc-engine.h"
#include "Query.h"


/* The gnc_book_close_period() routine will 'close' a book at
 *    the indicated date.  It returns a pointer to the closed book,
 *    while the argument remains open.  This routine will move
 *    all of the older transactions from the open book to the
 *    closed book.  The guid's of the old transactions will not
 *    be changed in the move.  Note, however, that the closed
 *    book will have a copy of every account in the open book,
 *    and that these copies will have new GUID's issued to them.
 *    Thus, every account has a 'twin' in the other book.
 * 
 *    This routine will also create 'equity transactions' in 
 *    order to preserve the balances on accounts.  For any
 *    account that is not of income, expense or equity type,
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
 *    searches for the 'nearest' account of GNCAccountType EQUITY 
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
 *    /book/next-book        GUID of next book (the still-open book).
 *    
 *    Implemented in still-open book:
 *    /book/open-date        Earliest date in this book.
 *    /book/prev-book        GUID of previous book (the closed book).
 *    
 *    Implemented in the balancing transaction:
 *    /book/closed-acct      GUID of account whose balance was brought forward
 *    /book/closed-book      GUID of book whose balance was brought forward
 *    
 *    Implemented in the closed account:
 *    /book/balancing-trans  GUID of equity-balancing transaction.
 *    /book/next-book        GUID of equity-balancing book.
 *    /book/next-acct        GUID of twin of this account in the open book.
 *    
 *    Implemented in the still-open account:
 *    /book/prev-acct         GUID of twin of this account in the closed book.
 *    /book/prev-book         GUID of previous book (the closed book)
 *    
-- hack alert -- 
   Need to also split up the price db too.  Also need to make copies of SX,
   and the like ... 

-- hack alert -- feature request: 
   have some way of remembering the quickfill text from older books...

-- hack alert -- 
  Not imlemented (yet), these should go into book:
  /book/name=some-user-supplied-name
  /book/notes=user-supplied-descriptive-comments
  /book/accounting-period=enum {none, week, month, quarter, trimester, year}

-- hack alert -- should not allow closed books to have unreconciled
   transactions ???

 */
GNCBook * gnc_book_close_period (GNCBook *, Timespec, 
                                 Account *equity_acct, 
                                 const char *memo);

/* The gnc_book_partition() uses the result of the indicated query
 *    to move a set of transactions from the "src" book to the "dest"
 *    book.  Before moving the transactions, it will first place a 
 *    copy of all of the accounts in "src" into "dest".  This is done 
 *    in order to ensure that all of the moved transactions will have
 *    the corrrect set of accounts to reference.  The transactions
 *    that will be moved are precisely those specified by the query.
 *    Any query will work to partition a book; however, its expected 
 *    that this routine will mostly serve as a utility to break up a 
 *    book into accounting periods. 
 *
 *    This routine intentionally does not copy scheduled/recurring 
 *    transactions.
 *
 *    When an account is copied, the copy is issued a new GUID.
 *    The GUID of its sibling is placed in the 'gemini' KVP value
 *    (See kvp_doc.txt for more detail).   Transactions and splits
 *    are moved without reassigning them a new GUID.  Note they
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
void gnc_book_partition (GNCBook *dest, GNCBook *src, Query *);

/* The gnc_book_insert_trans_clobber() routine takes an existing 
 *    transaction that is located in one book, and moves it to 
 *    another book.  It moves all of the splits as well.  In the 
 *    course of the move, the transaction is literally deleted 
 *    from the first book as its placed into the second.  The 
 *    transaction and split GUID's are not changed in the move.  
 *    This routine assumes that twin accounts already exist in 
 *    both books (and can be located with the standard twining 
 *    proceedure).
 *
 * The gnc_book_insert_trans() routine does the same as the above,
 *    except that it doesn't actually clobber the transaction: it
 *    merely moves the transaction and split GUID's to the new
 *    books' entity tables, and not much else.
 */

void gnc_book_insert_trans (GNCBook *book, Transaction *trans);
void gnc_book_insert_trans_clobber (GNCBook *book, Transaction *trans);

#endif /* XACC_PERIOD_H */

