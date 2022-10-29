/********************************************************************\
 * ScrubBudget.c -- fix budget amount signs                         *
 * Copyright (c) 2020 Christoher Lam                                *
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

#include <config.h>

#include <glib.h>
#include <glib/gi18n.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>

#include "gnc-prefs.h"
#include "gnc-budget.h"
#include "gnc-features.h"
#include "ScrubBudget.h"

static QofLogModule log_module = "gnc.engine.scrub";

typedef enum
{
    HEURISTICS_INC_EXP,
    HEURISTICS_CREDIT_ACC,
    HEURISTICS_NONE
} SignReversals;

typedef struct
{
    GncBudget* budget;
    SignReversals policy;
} ReversalType;

typedef struct
{
    gint asset,liability,equity,income,expense;
    gint num_periods;
    GncBudget* budget;
} ProcessData;

static void
process_heuristics_acct (Account * account, gpointer user_data)
{
    /* each account- check type. sum budget period amounts. if sum<0,
       decrease type tally by 1. if sum>0, increase type tally by 1. */
    ProcessData *heuristics = (ProcessData*) user_data;
    gnc_numeric total = gnc_numeric_zero(), val;
    gint sign;
    gchar *totalstr;

    for (gint i = 0; i < heuristics->num_periods; ++i)
    {
        if (!gnc_budget_is_account_period_value_set (heuristics->budget, account, i))
            continue;
        val = gnc_budget_get_account_period_value (heuristics->budget, account, i);
        total = gnc_numeric_add_fixed (total, val);
    }

    sign = gnc_numeric_compare (total, gnc_numeric_zero ());
    totalstr = gnc_numeric_to_string (total);
    PINFO ("acct=%s, total=%s, sign=%d",
           xaccAccountGetName (account), totalstr, sign);
    g_free (totalstr);

    switch (xaccAccountTypeGetFundamental (xaccAccountGetType (account)))
    {
    case ACCT_TYPE_ASSET:
        heuristics->asset += sign;
        break;
    case ACCT_TYPE_LIABILITY:
        heuristics->liability += sign;
        break;
    case ACCT_TYPE_EXPENSE:
        heuristics->expense += sign;
        break;
    case ACCT_TYPE_INCOME:
        heuristics->income += sign;
        break;
    case ACCT_TYPE_EQUITY:
        heuristics->equity += sign;
        break;
    default:
        break;
    }
}

static SignReversals
heuristics_on_budget (GncBudget * budget, Account *root)
{
    ProcessData heuristics = {0, 0, 0, 0, 0, gnc_budget_get_num_periods (budget),
                              budget};
    SignReversals result;

    gnc_account_foreach_descendant (root, process_heuristics_acct, &heuristics);

    result =
        heuristics.expense < 0 ? HEURISTICS_INC_EXP :
        heuristics.income < 0 ? HEURISTICS_NONE :
        HEURISTICS_CREDIT_ACC;

    LEAVE ("heuristics_on_budget %s: A(%d) L(%d) Inc(%d) Exp(%d) Eq(%d) = %d",
           gnc_budget_get_name (budget),
           heuristics.asset, heuristics.liability, heuristics.income,
           heuristics.expense, heuristics.equity, result);

    return result;
}

static void
fix_budget_acc_sign (Account *acc, gpointer user_data)
{
    ReversalType* reversal = (ReversalType*) user_data;
    GncBudget* budget = reversal->budget;
    guint numperiods = gnc_budget_get_num_periods (budget);
    int type = xaccAccountTypeGetFundamental (xaccAccountGetType (acc));

    ENTER ("budget account reversal [%s] starting", xaccAccountGetName(acc));

    switch (reversal->policy)
    {
    case HEURISTICS_INC_EXP:
        if ((type != ACCT_TYPE_INCOME) && (type != ACCT_TYPE_EXPENSE))
            return;
        PINFO ("budget account [%s] is inc/exp. reverse!",
               xaccAccountGetName(acc));
        break;
    case HEURISTICS_CREDIT_ACC:
        if ((type != ACCT_TYPE_LIABILITY) &&
            (type != ACCT_TYPE_EQUITY) &&
            (type != ACCT_TYPE_INCOME))
            return;
        PINFO ("budget account [%s] is credit-account. reverse!",
               xaccAccountGetName(acc));
        break;
    default:
        /* shouldn't happen. */
        return;
    }

    for (guint i=0; i < numperiods; ++i)
    {
        gnc_numeric amt;
        if (!gnc_budget_is_account_period_value_set (budget, acc, i))
            continue;

        amt = gnc_budget_get_account_period_value (budget, acc, i);
        amt = gnc_numeric_neg (amt);
        gnc_budget_set_account_period_value (budget, acc, i, amt);
    }

    LEAVE ("budget account reversal [%s] completed!", xaccAccountGetName(acc));
}

static void
maybe_scrub_budget (QofInstance* data, gpointer user_data)
{
    GncBudget* budget = GNC_BUDGET(data);
    Account *root = (Account*) user_data;
    ReversalType reversal;

    reversal.policy = heuristics_on_budget (budget, root);
    if (reversal.policy == HEURISTICS_NONE)
    {
        PWARN ("budget [%s] doesn't need reversing", gnc_budget_get_name (budget));
        return;
    }

    reversal.budget = budget;

    ENTER ("processing budget [%s] for reversal", gnc_budget_get_name (budget));
    gnc_account_foreach_descendant (root, fix_budget_acc_sign, &reversal);
    LEAVE ("completed budget [%s] for reversal", gnc_budget_get_name (budget));
}

gboolean
gnc_maybe_scrub_all_budget_signs (QofBook *book)
{
    QofCollection* collection = qof_book_get_collection (book, GNC_ID_BUDGET);
    gboolean has_no_budgets = (qof_collection_count (collection) == 0);
    gboolean featured = gnc_features_check_used (book, GNC_FEATURE_BUDGET_UNREVERSED);

    /* If there are no budgets, there shouldn't be feature! */
    if (has_no_budgets && featured)
    {
        gnc_features_set_unused (book, GNC_FEATURE_BUDGET_UNREVERSED);
        PWARN ("There are no budgets, removing feature BUDGET_UNREVERSED");
    }

    if (has_no_budgets || featured)
        return FALSE;

    /* There are budgets and feature is not set. Scrub, and set
       feature. Return TRUE to show budget fix warning. */
    qof_collection_foreach (collection, maybe_scrub_budget,
                            gnc_book_get_root_account (book));
    gnc_features_set_used (book, GNC_FEATURE_BUDGET_UNREVERSED);
    return TRUE;
}
/* ==================== END OF FILE ==================== */
