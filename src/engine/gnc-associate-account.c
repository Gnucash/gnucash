/********************************************************************\
 * gnc-associate-account.h : api for associating income and         *
 * expense accounts with stock/mutual fund accounts, for tracking   *
 * dividends, brokerage, and other stock-related expenses and       *
 * income so that they can be reported                              *
 * Copyright 2000 Gnumatic Incorporated                             *
 * Written by Robert Merkel <rgmerk@mira.net>                       *
 *                                                                  *
 * WARNING WARNING WARNING: THIS STUFF IS TOTALLY UNTESTED AND      *
 * IS ONLY IN CVS FOR SAFEKEEPING                                   *
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

#include "config.h"

#include "gnc-associate-account.h"
#include "gnc-engine.h"
#include "qof.h"

static QofLogModule log_module = GNC_MOD_ENGINE;

/* Maps GNCTrackingIncomeCategory to string keys.  If this enum
   changes, update */

static char * income_to_key[] = {"income-miscellaneous",
                                 "income-interest",
                                 "income-dividend"
                                 "income-long-term-capital-gain",
                                 "income-short-term-capital-gain"
                                };

/* Maps GNCTrackingExpenseCategory to string keys.  If this enum
   changes, update */

static char * expense_to_key[] = {"expense-miscellaneous",
                                  "expense-commission"
                                 };

static KvpFrame *
get_assoc_acc_frame(KvpFrame *account_frame)
{
    KvpFrame *assoc_acc_frame;
    KvpValue *assoc_acc_frame_kvpvalue =
        kvp_frame_get_slot(account_frame, "associated-accounts");

    assoc_acc_frame = kvp_value_get_frame(assoc_acc_frame_kvpvalue);
    if (!assoc_acc_frame)
    {
        assoc_acc_frame = kvp_frame_new();
        assoc_acc_frame_kvpvalue = kvp_value_new_frame(assoc_acc_frame);
        kvp_frame_set_slot(account_frame, "associated-accounts",
                           assoc_acc_frame_kvpvalue);
    }

    return assoc_acc_frame;
}

static void
back_associate_expense_accounts(Account *stock_account,
                                GList *accounts,
                                GNCTrackingExpenseCategory category)
{
    KvpFrame *acc_frame;
    KvpValue *val, *stock_acc_guid_kvpval, *stock_acc_category_kvpval;
    const GncGUID *stock_acc_guid;
    const GncGUID *existing_acc_guid;

    stock_acc_guid = xaccAccountGetGUID(stock_account);
    stock_acc_guid_kvpval = kvp_value_new_guid(stock_acc_guid);

    stock_acc_category_kvpval = kvp_value_new_string(expense_to_key[category]);

    for (; accounts; accounts = g_list_next(accounts))
    {
        acc_frame = xaccAccountGetSlots(accounts->data);
        g_return_if_fail((val = kvp_frame_get_slot(acc_frame,
                                "associated-stock-account")));
        g_return_if_fail(kvp_value_get_type(val) == KVP_TYPE_GUID);
        existing_acc_guid = kvp_value_get_guid(val);

        kvp_frame_set_slot_nc(acc_frame, "associated-stock-account",
                              stock_acc_guid_kvpval);

        kvp_frame_set_slot_nc(acc_frame, "associated-stock-account-category",
                              stock_acc_category_kvpval);
    }

    return;
}

static void
back_associate_income_accounts(Account *stock_account,
                               GList *accounts,
                               GNCTrackingIncomeCategory category)
{
    KvpFrame *acc_frame;
    KvpValue *val, *stock_acc_guid_kvpval, *stock_acc_category_kvpval;
    const GncGUID *stock_acc_guid;
    const GncGUID *existing_acc_guid;

    stock_acc_guid = xaccAccountGetGUID(stock_account);
    stock_acc_guid_kvpval = kvp_value_new_guid(stock_acc_guid);

    stock_acc_category_kvpval = kvp_value_new_string(income_to_key[category]);

    for (; accounts; accounts = g_list_next(accounts))
    {
        acc_frame = xaccAccountGetSlots(accounts->data);
        g_return_if_fail((val = kvp_frame_get_slot(acc_frame,
                                "associated-stock-account")));
        g_return_if_fail(kvp_value_get_type(val) == KVP_TYPE_GUID);
        existing_acc_guid = kvp_value_get_guid(val);

        kvp_frame_set_slot_nc(acc_frame, "associated-stock-account",
                              stock_acc_guid_kvpval);
        kvp_frame_set_slot_nc(acc_frame, "associated-stock-account-category",
                              stock_acc_category_kvpval);
    }

    return;
}

static KvpValue *
make_kvpd_on_list(GList *account_list)
{
    GList *iter;
    KvpValue *retval;
    KvpValue *guid_kvp;
    GList  *kvp_acc_list = NULL;
    const GncGUID *acc_id;

    for (iter = account_list; iter; iter = g_list_next(iter))
    {
        GNCAccountType type;
        Account *current_account;

        current_account = iter->data;
        type = xaccAccountGetType(current_account);
        g_return_val_if_fail(type == ACCT_TYPE_INCOME || type == ACCT_TYPE_EXPENSE,
                             NULL);

        acc_id = xaccAccountGetGUID(current_account);
        guid_kvp = kvp_value_new_guid(acc_id);
        kvp_acc_list = g_list_prepend(kvp_acc_list, guid_kvp);
    }

    kvp_acc_list = g_list_reverse(kvp_acc_list);

    retval = kvp_value_new_glist_nc(kvp_acc_list);
    return retval;
}

static GList *
de_kvp_account_list(KvpValue *kvpd_list, QofBook *book)
{
    GList *guid_account_list = kvp_value_get_glist(kvpd_list);
    if (guid_account_list)
    {
        GList *expense_acc_list = NULL;
        for (; guid_account_list; guid_account_list = g_list_next(guid_account_list))
        {
            expense_acc_list = g_list_prepend(expense_acc_list,
                                              xaccAccountLookup(guid_account_list->data, book));
        }

        expense_acc_list = g_list_reverse(expense_acc_list);
        return expense_acc_list;
    }
    else
    {
        return NULL;
    }
}

/*********************************************************************\
 * gnc_tracking_associate_income_accounts                            *
 *   associate a list of income accounts with a stock account        *
 *                                                                   *
 *  NOTE: Please disassociate all the accounts in account_list       *
 *  using gnc_tracking_dissociate_accounts if necessary, BEFORE      *
 *  calling this function                                            *
 *                                                                   *
 *  Args: stock_account - the stock account                          *
 *        category - the type of association                         *
 *        account_list - a GList of Account *'s of the accounts      *
 *        to associate with the stock account                        *
 *                                                                   *
 * Returns : void                                                    *
\*********************************************************************/

void
gnc_tracking_associate_income_accounts(Account *stock_account,
                                       GNCTrackingIncomeCategory category,
                                       GList *account_list)
{
    KvpFrame *account_frame, *inc_account_frame;
    KvpValue *kvpd_on_account_list;

    g_return_if_fail(stock_account);
    g_return_if_fail(xaccAccountIsPriced(stock_account));
    account_frame = xaccAccountGetSlots(stock_account);
    g_return_if_fail(account_frame);
    g_return_if_fail(category >= 0);
    g_return_if_fail(category < GNC_TR_INC_N_CATEGORIES);

    inc_account_frame = get_assoc_acc_frame(account_frame);
    kvpd_on_account_list = make_kvpd_on_list(account_list);

    back_associate_income_accounts(stock_account, account_list, category);

    kvp_frame_set_slot_nc(inc_account_frame,
                          income_to_key[category],
                          kvpd_on_account_list);
}

/*********************************************************************\
 * gnc_tracking_associate_expense_accounts                           *
 *   associate a list of expense accounts with a stock account       *
 *                                                                   *
 *  NOTE: Please disassociate all the accounts in account_list       *
 *  using gnc_tracking_dissociate_accounts if necessary, BEFORE      *
 *  calling this function                                            *
 *                                                                   *
 *  Args: stock_account - the stock account                          *
 *        category - the type of association                         *
 *        account_list - a GList of Account *'s of the accounts      *
 *        to associate with the stock account                        *
 *                                                                   *
 * Returns : void                                                    *
\*********************************************************************/
void
gnc_tracking_asssociate_expense_account(Account *stock_account,
                                        GNCTrackingExpenseCategory category,
                                        GList *account_list)
{
    KvpFrame *account_frame, *expense_acc_frame;
    KvpValue *kvpd_on_account_list;

    g_return_if_fail(stock_account);
    g_return_if_fail(xaccAccountIsPriced(stock_account));
    account_frame = xaccAccountGetSlots(stock_account);
    g_return_if_fail(account_frame);
    g_return_if_fail(category >= 0);
    g_return_if_fail(category < GNC_TR_EXP_N_CATEGORIES);

    expense_acc_frame = get_assoc_acc_frame(account_frame);
    kvpd_on_account_list = make_kvpd_on_list(account_list);

    back_associate_expense_accounts(stock_account, account_list, category);

    kvp_frame_set_slot_nc(expense_acc_frame,
                          expense_to_key[category],
                          kvpd_on_account_list);
}

/*********************************************************************\
 * gnc_tracking_find_expense_accounts                                *
 *   find out which accounts are associated with a particular        *
 *   account in a particular way                                     *
 *                                                                   *
 *                                                                   *
 *  Args: stock_account - the stock account                          *
 *        category - the type of association                         *
 *                                                                   *
 * Returns : A GList of Account *'s listing the accounts             *
\*********************************************************************/

GList *
gnc_tracking_find_expense_accounts(Account *stock_account,
                                   GNCTrackingExpenseCategory category)
{
    KvpFrame *account_frame, *expense_acc_frame;
    KvpValue *kvpd_on_account_list;

    g_return_val_if_fail(xaccAccountIsPriced(stock_account), NULL);
    g_return_val_if_fail(category >= 0 && category < GNC_TR_EXP_N_CATEGORIES,
                         NULL);

    account_frame = xaccAccountGetSlots(stock_account);
    g_return_val_if_fail(account_frame, NULL);

    expense_acc_frame = get_assoc_acc_frame(account_frame);
    kvpd_on_account_list = kvp_frame_get_slot(account_frame,
                           expense_to_key[category]);

    return de_kvp_account_list(kvpd_on_account_list,
                               gnc_account_get_book(stock_account));
}

/*********************************************************************\
 * gnc_tracking_find_income_accounts                                 *
 *   find out which accounts are associated with a particular        *
 *   account in a particular way                                     *
 *                                                                   *
 *                                                                   *
 *  Args: stock_account - the stock account                          *
 *        category - the type of association                         *
 *                                                                   *
 * Returns : A GList of Account *'s listing the accounts             *
\*********************************************************************/
GList *
gnc_tracking_find_income_accounts(Account *stock_account,
                                  GNCTrackingIncomeCategory category)
{
    KvpFrame *account_frame, *income_acc_frame;
    KvpValue *kvpd_on_account_list;

    g_return_val_if_fail(xaccAccountIsPriced(stock_account), NULL);
    g_return_val_if_fail(category >= 0 && category < GNC_TR_INC_N_CATEGORIES,
                         NULL);

    account_frame = xaccAccountGetSlots(stock_account);
    g_return_val_if_fail(account_frame, NULL);

    income_acc_frame = get_assoc_acc_frame(account_frame);
    kvpd_on_account_list = kvp_frame_get_slot(income_acc_frame,
                           income_to_key[category]);

    return de_kvp_account_list(kvpd_on_account_list,
                               gnc_account_get_book(stock_account));
}

/*********************************************************************\
 * gnc_tracking_find_all_expense_accounts                            *
 *   find all expense accounts associated with a stock account       *
 *                                                                   *
 *  Args: stock_account - the stock account                          *
 *                                                                   *
 * Returns : A GList of Account *'s listing the accounts             *
\*********************************************************************/

GList *
gnc_tracking_find_all_expense_accounts(Account *stock_account)
{
    GList *complete_list = NULL;
    int i;

    for (i = 0; i < GNC_TR_EXP_N_CATEGORIES; i++)
    {
        complete_list =
            g_list_concat(complete_list,
                          gnc_tracking_find_expense_accounts(stock_account, i));
    }

    return complete_list;
}

/*********************************************************************\
 * gnc_tracking_find_all_income_accounts                             *
 *   find all income accounts associated with a stock account        *
 *                                                                   *
 *  Args: stock_account - the stock account                          *
 *                                                                   *
 * Returns : A GList of Account *'s listing the accounts             *
\*********************************************************************/

GList *
gnc_tracking_find_all_income_accounts(Account *stock_account)
{
    GList *complete_list = NULL;
    int i;

    for (i = 0; i < GNC_TR_EXP_N_CATEGORIES; i++)
    {
        complete_list = g_list_concat(complete_list,
                                      gnc_tracking_find_expense_accounts(stock_account,
                                              i));
    }
    return complete_list;
}

/*********************************************************************\
 * gnc_tracking_find_stock_account                                   *
 *   find the stock account associated with this expense/income      *
 *   account.  If there is no association, return NULL               *
 *                                                                   *
 *  Args: inc_or_expense_acc - the expense/income account            *
 *                                                                   *
 *                                                                   *
 * Returns : The associated stock account                            *
\*********************************************************************/

Account *
gnc_tracking_find_stock_account(Account *inc_or_expense_acc)
{
    return NULL;
}

/*********************************************************************\
 * gnc_tracking_dissociate_account                                   *
 *   remove any association between this income/expense account      *
 *   and any stock account it is presently associated with           *
 *   account.                                                        *
 *                                                                   *
 *  Args: inc_or_expense_acc - the expense/income account            *
 *                                                                   *
 *                                                                   *
 * Returns : void                                                    *
\*********************************************************************/

void
gnc_tracking_dissociate_account(Account *inc_or_expense_account)
{
    GNCAccountType type;
    KvpFrame *stock_account_kvpframe, *assoc_acc_kvpframe;
    KvpFrame *current_account_kvpframe;
    KvpValue *stock_account_kvpval, *acc_list_kvpval, *category_kvpval;
    const GncGUID *stock_account_guid, *inc_or_expense_account_guid, *current_guid;
    Account *stock_account;
    char *category_name;
    GList *assoc_acc_list, *assoc_acc_list_start;

    type = xaccAccountGetType(inc_or_expense_account);

    g_return_if_fail(type == ACCT_TYPE_INCOME || type == ACCT_TYPE_EXPENSE);

    current_account_kvpframe = xaccAccountGetSlots(inc_or_expense_account);

    stock_account_kvpval = kvp_frame_get_slot(current_account_kvpframe,
                           "associated-stock-account");

    stock_account_guid = kvp_value_get_guid(stock_account_kvpval);

    category_kvpval = kvp_frame_get_slot(current_account_kvpframe,
                                         "associated-stock-account-category");
    category_name = kvp_value_get_string(category_kvpval);


    inc_or_expense_account_guid = xaccAccountGetGUID(inc_or_expense_account);
    stock_account = xaccAccountLookup
                    (stock_account_guid, gnc_account_get_book(inc_or_expense_account));

    stock_account_kvpframe = xaccAccountGetSlots(stock_account);

    g_return_if_fail((stock_account_kvpval =
                          kvp_frame_get_slot(stock_account_kvpframe,
                                  "associated-accounts")));

    assoc_acc_kvpframe = kvp_value_get_frame(stock_account_kvpval);

    g_return_if_fail((acc_list_kvpval = kvp_frame_get_slot(assoc_acc_kvpframe,
                                        category_name)));
    g_return_if_fail((assoc_acc_list_start =
                          kvp_value_get_glist(acc_list_kvpval)));

    for (assoc_acc_list = assoc_acc_list_start;
            assoc_acc_list;
            assoc_acc_list = g_list_next(assoc_acc_list))
    {
        g_return_if_fail((current_guid = kvp_value_get_guid(assoc_acc_list->data)));
        if (guid_equal(current_guid, inc_or_expense_account_guid))
        {
            assoc_acc_list_start =
                g_list_remove_link(assoc_acc_list_start, assoc_acc_list);
            g_list_free_1(assoc_acc_list);
            acc_list_kvpval = kvp_value_new_glist_nc(assoc_acc_list);
            kvp_frame_set_slot_nc(assoc_acc_kvpframe,
                                  category_name,
                                  acc_list_kvpval);
            return;
        }
    }

    /* should never happen */
    PERR("Income/Expense account and stock account disagree on association");
}

/* ========================== END OF FILE ===================== */
