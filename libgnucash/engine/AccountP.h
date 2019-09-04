/********************************************************************\
 * AccountP.h -- Account engine-private data structure              *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1997-2002, Linas Vepstas <linas@linas.org>         *
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
 *                                                                  *
\********************************************************************/

/** @file AccountP.h
 *
 * This is the *private* header for the account structure.
 * No one outside of the engine should ever include this file.
 *
 * This header includes prototypes for "dangerous" functions.
 * Invoking any of these functions potentially leave the account
 * in an inconsistent state.  If they are not used in the proper
 * setting, they can leave the account structures in an inconsistent
 * state.  Thus, these methods should never be used outside of
 * the engine, which is why they are "hidden" here.
 *
 */

#ifndef XACC_ACCOUNT_P_H
#define XACC_ACCOUNT_P_H

#include "Account.h"

#ifdef __cplusplus
extern "C" {
#endif

#define GNC_ID_ROOT_ACCOUNT        "RootAccount"

/** STRUCTS *********************************************************/

/** This is the data that describes an account.
 *
 * This is the *private* header for the account structure.
 * No one outside of the engine should ever include this file.
*/

/** \struct Account */
typedef struct AccountPrivate
{
    /* The accountName is an arbitrary string assigned by the user.
     * It is intended to a short, 5 to 30 character long string that
     * is displayed by the GUI as the account mnemonic.
     */
    char *accountName;

    /* The accountCode is an arbitrary string assigned by the user.
     * It is intended to be reporting code that is a synonym for the
     * accountName. Typically, it will be a numeric value that follows
     * the numbering assignments commonly used by accountants, such
     * as 100, 200 or 600 for top-level accounts, and 101, 102..  etc.
     * for detail accounts.
     */
    char *accountCode;

    /* The description is an arbitrary string assigned by the user.
     * It is intended to be a longer, 1-5 sentence description of what
     * this account is all about.
     */
    char *description;

    /* The type field is the account type, picked from the enumerated
     * list that includes ACCT_TYPE_BANK, ACCT_TYPE_STOCK,
     * ACCT_TYPE_CREDIT, ACCT_TYPE_INCOME, etc.  Its intended use is to
     * be a hint to the GUI as to how to display and format the
     * transaction data.
     */
    GNCAccountType type;

    /*
     * The commodity field denotes the kind of 'stuff' stored
     * in this account.  The 'amount' field of a split indicates
     * how much of the 'stuff' there is.
     */
    gnc_commodity * commodity;
    int commodity_scu;
    gboolean non_standard_scu;

    /* The parent and children pointers are used to implement an account
     * hierarchy, of accounts that have sub-accounts ("detail accounts").
     */
    Account *parent;    /* back-pointer to parent */
    GList *children;    /* list of sub-accounts */

    /* protected data - should only be set by backends */
    gnc_numeric starting_balance;
    gnc_numeric starting_noclosing_balance;
    gnc_numeric starting_cleared_balance;
    gnc_numeric starting_reconciled_balance;

    /* cached parameters */
    gnc_numeric balance;
    gnc_numeric noclosing_balance;
    gnc_numeric cleared_balance;
    gnc_numeric reconciled_balance;

    gboolean balance_dirty;     /* balances in splits incorrect */

    GList *splits;              /* list of split pointers */
    gboolean sort_dirty;        /* sort order of splits is bad */

    LotList   *lots;		/* list of lot pointers */
    GNCPolicy *policy;		/* Cached pointer to policy method */

    /* The "mark" flag can be used by the user to mark this account
     * in any way desired.  Handy for specialty traversals of the
     * account tree. */
    short mark;
} AccountPrivate;

struct account_s
{
    QofInstance inst;
};

/* Set the account's GncGUID. This should only be done when reading
 * an account from a datafile, or some other external source. Never
 * call this on an existing account! */
void xaccAccountSetGUID (Account *account, const GncGUID *guid);

/* Register Accounts with the engine */
gboolean xaccAccountRegister (void);

/* Structure for accessing static functions for testing */
typedef struct
{
    AccountPrivate *(*get_private) (Account *acc);
    Account *(*coll_get_root_account) (QofCollection *col);
    void (*xaccFreeAccountChildren) (Account *acc);
    void (*xaccFreeAccount) (Account *acc);
    void (*qofAccountSetParent) (Account *acc, QofInstance *parent);
    Account *(*gnc_account_lookup_by_full_name_helper) (const Account *acc,
            gchar **names);
} AccountTestFunctions;

AccountTestFunctions* _utest_account_fill_functions(void);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* XACC_ACCOUNT_P_H */
