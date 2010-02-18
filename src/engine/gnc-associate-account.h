/********************************************************************\
 * gnc-associate-account.h : api for associating income and        *
 * expense accounts with stock/mutual fund accounts, for tracking   *
 * dividends, brokerage, and other stock-related expenses and       *
 * income so that they can be reported                              *
 * Copyright 2000 Gnumatic Incorporated                             *
 * Written by Robert Merkel <rgmerk@mira.net>                       *
 *
 * WARNING WARNING WARNING: THIS CODE IS TOTALLY UNTESTED.          *
 * THE ONLY REASON IT'S IN CVS IS FOR SAFEKEEPING                   *
 *                                                                  *
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

#include <Account.h>
#include <glib.h>

/*
 * account_list is a list of account *'s, all of which much be expense
 * accounts
 */
typedef enum {GNC_TR_INC_MISC,
              GNC_TR_INC_INTEREST,
              GNC_TR_INC__DIVIDEND,
              GNC_TR_INC_LT_CG,
              GNC_TR_INC_ST_CG,
              GNC_TR_INC_N_CATEGORIES
             } GNCTrackingIncomeCategory;

typedef enum {GNC_TR_EXP_MISC,
              GNC_TR_EXP_COMMISSION,
              GNC_TR_EXP_N_CATEGORIES
             } GNCTrackingExpenseCategory;


/*
 * account_list is a list of account *'s, all of which much be expense
 * accounts.  You can clear associations by setting account_list to NULL
 */

void gnc_tracking_associate_income_accounts(Account *stock_account,
        GNCTrackingIncomeCategory category,
        AccountList *account_list);


void gnc_tracking_asssociate_expense_account(Account *stock_account,
        GNCTrackingExpenseCategory category,
        AccountList *account_list);

/*
 * returns a list of account *'s,
 * returns null if no association specified
 */

AccountList *gnc_tracking_find_expense_accounts(Account *stock_account,
        GNCTrackingExpenseCategory category);

AccountList *gnc_tracking_find_income_accounts(Account *stock_account,
        GNCTrackingIncomeCategory category);

/* for ROI purposes we don't care about categories, these are "grab
all" for that purpose */

AccountList *gnc_tracking_find_all_expense_accounts(Account *stock_account);

AccountList *gnc_tracking_find_all_income_accounts(Account *stock_account);


/*
 * reverse lookup - obviously returns a stock account (or NULL if none
 * associated), and argument must be an income or expense account
 */
Account *gnc_tracking_find_stock_account(Account *inc_or_expense_acc);

void gnc_tracking_dissociate_account(Account *inc_or_expense_account);
