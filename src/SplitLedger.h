/********************************************************************\
 * SplitLedger.h -- split ledger api                                *
 * Copyright (C) 1998-2000 Linas Vepstas                            *
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

/* 
 * FILE:
 * SplitLedger.h 
 *
 * FUNCTION:
 * Definitions for a number of functions that interface 
 * between the transaction engine, and the register GUI.
 *
 * The xaccSRSetData() method sets the user data and callback
 *    hooks for the register.
 *
 * The xaccSRSetAccountSeparator() method sets the character
 *    used to separate accounts in fully-qualified names.
 *
 * The xaccSRSetReverseBalanceCallback() method sets up
 *    a callback used to determine whether split balances
 *    should be reversed.
 *
 * The xaccSRGetCurrentTrans() method returns the transaction
 *    which is the parent of the current split (see below).
 *
 * The xaccSRGetCurrentSplit() method returns the split at which 
 *    the cursor is currently located.
 *
 * The xaccSRGetBlankSplit() method returns the blank split or
 *    NULL if there is none.
 *
 * The xaccSRGetSplitRowCol() method searches the split register for
 *    the given split. If found, it returns GNC_T and the virt_row
 *    and virt_col arguments are set to the location of the split.
 *    Otherwise, the method returns GNC_F.
 *
 * The xaccSRGetTransSplitRowCol() method works as above, but searches
 *    first for the first split (the transaction split) and then the
 *    next split, and returns the location of the second split.
 *
 * The xaccSRDuplicateCurrent() method duplicates either the current
 *    transaction or the current split depending on the register mode
 *    and cursor position. Returns the split just created, or the
 *    'main' split of the transaction just created, or NULL if
 *    nothing happened.
 *
 * The xaccSRDeleteCurrentSplit() method deletes the split associated
 *    with the current cursor, if both are non-NULL. If successful, all
 *    affected account windows are refreshed. Deleting the blank split
 *    just clears the cursor values.
 *
 * The xaccSRDeleteCurrentTrans() method deletes the transaction
 *    associated with the current cursor, if both are non-NULL.
 *    If successful, all affected account windows are refreshed.
 *
 * The xaccSREmptyCurrentTrans() method deletes the non-transaction
 *    splits associated wih the current cursor, if both are non-NULL.
 *    If successful, all affected account windows are refreshed.
 *
 * The xaccSRCancelCursorSplitChanges() method cancels any changes made
 *    to the current cursor, reloads the cursor from the engine, reloads
 *    the table from the cursor, and updates the GUI. The change flags
 *    are cleared.
 *
 * The xaccSRCancelCursorTransChanges() method cancels any changes made
 *    to the current pending transaction, reloads the table from the engine,
 *    and updates the GUI. The change flags are cleared.
 *
 * The xaccSRLoadRegister() subroutine will copy transaction
 *    information from a list of splits to the rows of the
 *    register GUI.  The third argument, default_source_acc,
 *    will be used to initialize the source account of a new,
 *    blank split appended to the tail end of the register.
 *    This "blank split" is the place where the user can 
 *    edit info to create new tranasactions.
 *
 * The xaccSRSaveRegEntry() method will copy the contents 
 *    from the cursor to a split.  The split/transaction
 *    that is updated is the one associated with the current 
 *    cursor (register entry) position. If the current transaction
 *    is different from newtrans, the current transaction will be
 *    comitted. Pass in NULL for newtrans to force a commit.
 *    The method returns GNC_T if the cursor was really saved,
 *    i.e., it had been changed. Otherwise, it returns GNC_F.
 *
 * The xaccSRRedrawRegEntry() method should be called soon 
 *    after the xaccSRSaveRegEntry() method.  It checks the 
 *    change flag for the current entry/split, and if it
 *    has been modified, it causes a redraw of any register
 *    window that could be affected.  That is, it causes 
 *    a rdraw of any window showing this split, or any other
 *    split that belongs to this same tansaction.
 *
 * The xaccSRLoadXferCells() method loads (or reloads) the transfer
 *    cells with appropriate entries.
 */

#ifndef __XACC_SPLIT_LEDGER_H__
#define __XACC_SPLIT_LEDGER_H__

#include "gnc-common.h"
#include "Group.h"
#include "splitreg.h"
#include "Transaction.h"

/* Callback function type */
typedef gncUIWidget (*SRGetParentCallback) (void *user_data);
typedef void (*SRSetHelpCallback) (void *user_data, const char *help_str);
typedef gncBoolean (*SRReverseBalanceCallback) (Account *account);

void xaccSRSetData(SplitRegister *reg, void *user_data,
                   SRGetParentCallback get_parent,
                   SRSetHelpCallback set_help);

void xaccSRSetAccountSeparator(char separator);

void xaccSRSetReverseBalanceCallback(SRReverseBalanceCallback callback);

Transaction * xaccSRGetCurrentTrans (SplitRegister *reg);
Split * xaccSRGetCurrentSplit (SplitRegister *reg);
Split * xaccSRGetBlankSplit (SplitRegister *reg);

gncBoolean xaccSRGetSplitRowCol (SplitRegister *reg, Split *split,
                                 int *virt_row, int *virt_col);
gncBoolean xaccSRGetTransSplitRowCol (SplitRegister *reg,
                                      Transaction *trans, Split *split,
                                      int *virt_row, int *virt_col);

Split * xaccSRDuplicateCurrent (SplitRegister *reg);

void    xaccSRDeleteCurrentSplit (SplitRegister *reg);
void    xaccSRDeleteCurrentTrans (SplitRegister *reg);
void    xaccSREmptyCurrentTrans  (SplitRegister *reg);

void    xaccSRCancelCursorSplitChanges (SplitRegister *reg);
void    xaccSRCancelCursorTransChanges (SplitRegister *reg);

void    xaccSRLoadRegister (SplitRegister *reg, Split **slist,
                            Account *default_source_acc);

gncBoolean xaccSRSaveRegEntry (SplitRegister *reg, Transaction *newtrans);
void       xaccSRRedrawRegEntry (SplitRegister *reg);

void    xaccSRLoadXferCells (SplitRegister *reg, Account *base_account);

#endif /* __XACC_SPLIT_LEDGER_H__ */
