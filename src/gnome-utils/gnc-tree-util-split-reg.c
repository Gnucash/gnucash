/********************************************************************\
 * gnc-tree-util-split-reg.c -- GtkTreeView implementation          *
 *                     to display registers in a GtkTreeView.       *
 *                                                                  *
 * Copyright (C) 2006-2007 Chris Shoemaker <c.shoemaker@cox.net>    *
 * Copyright (C) 2012 Robert Fewell                                 *
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

#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <stdlib.h>
#include <string.h>

#include "gnc-tree-util-split-reg.h"
#include "gnc-tree-model-split-reg.h"
#include "gnc-tree-view-split-reg.h"

#include "gnc-ui.h"
#include "dialog-utils.h"
#include "dialog-transfer.h"
#include "engine-helpers.h"
#include "Transaction.h"


#define SPLIT_TRANS_STR _("-- Split Transaction --")
#define STOCK_SPLIT_STR _("-- Stock Split --")

/** Static Globals *******************************************************/
static QofLogModule log_module = GNC_MOD_LEDGER;

/*****************************************************************************/


/* Is current split a security account */
static gboolean
gtu_sr_use_security (GncTreeViewSplitReg *view)
{
    RowDepth depth;
    Account *account = NULL;
    Split *split;

    split = gnc_tree_view_split_reg_get_current_split (view);

    depth = gnc_tree_view_reg_get_selected_row_depth (view);

    if (!split)
        return TRUE;

    if (depth != SPLIT3)
        return TRUE;

    if (!account)
        account = xaccSplitGetAccount (split);

    if (!account)
        return TRUE;

    if (xaccTransUseTradingAccounts (xaccSplitGetParent (split)))
    {
        if (!gnc_commodity_is_iso (xaccAccountGetCommodity (account)))
            return TRUE;
    }

    return xaccAccountIsPriced (account);
}


/* Get the rate from the price db */
static gnc_numeric
gtu_sr_get_rate_from_db (gnc_commodity *from, gnc_commodity *to)
{
    GNCPrice *prc;
    gnc_numeric rate_split;
    gboolean have_rate = FALSE;
    QofBook *book = gnc_get_current_book ();

    /* Do we have a rate allready */
    prc = gnc_pricedb_lookup_latest (gnc_pricedb_get_db (book), from, to);
    if (prc)
    {
        rate_split = gnc_price_get_value (prc);
        gnc_price_unref (prc);
        have_rate = TRUE;
    }

    /* Lets try reversing the commodities */
    if (!have_rate)
    {
        prc = gnc_pricedb_lookup_latest (gnc_pricedb_get_db (book), to, from);
        if (prc)
        {
            rate_split = gnc_numeric_div (gnc_numeric_create (100, 100), gnc_price_get_value (prc),
                                 GNC_DENOM_AUTO, GNC_HOW_DENOM_REDUCE);

            gnc_price_unref (prc);
            have_rate = TRUE;
        }
    }

    /* No rate, set to 1/1 */
    if (!have_rate)
        rate_split = gnc_numeric_create (100, 100);

    return rate_split;
}


/* Do we need an exchange rate */
static gboolean
gtu_sr_needs_exchange_rate (GncTreeViewSplitReg *view, Transaction *trans, Split *split)
{
    gnc_commodity *split_com, *txn_curr, *reg_com;

    ENTER("gtu_sr_needs_exchange_rate - trans %p and split %p", trans, split);

    txn_curr = xaccTransGetCurrency (trans);
    split_com = xaccAccountGetCommodity (xaccSplitGetAccount (split));
    if (split_com && txn_curr && !gnc_commodity_equiv (split_com, txn_curr))
    {
        LEAVE("gtu_sr_needs_exchange_rate split_com to txn_curr return TRUE");
        return TRUE;
    }

    reg_com = gnc_tree_view_split_reg_get_reg_commodity (view);
    if (split_com && reg_com && !gnc_commodity_equiv (split_com, reg_com))
    {
        LEAVE("gtu_sr_needs_exchange_rate split_com and reg_com return TRUE");
        return TRUE;
    }
    LEAVE("No Exchange rate needed");
    return FALSE;
}


/* Either sets the value and amount for split and returns TRUE, or
   does nothing and returns FALSE. */
static gboolean
gtu_sr_handle_exchange_rate (GncTreeViewSplitReg *view, gnc_numeric amount, Transaction *trans, Split *split, gboolean force)
{
    GncTreeModelSplitReg *model;
    XferDialog *xfer;
    gboolean rate_split_ok, rate_reg_ok;
    gnc_numeric rate_split, rate_reg, value;
    Account *reg_acc;
    gnc_commodity *xfer_comm = xaccAccountGetCommodity (xaccSplitGetAccount (split));
    gnc_commodity *reg_comm = gnc_tree_view_split_reg_get_reg_commodity (view);
    gnc_commodity *trans_curr = xaccTransGetCurrency (trans);
    gboolean expanded;
    gboolean have_rate = TRUE;

    ENTER("handle_exchange_rate amount %s, trans %p and split %p force %d", gnc_numeric_to_string (amount), trans, split, force);


    model = gnc_tree_view_split_reg_get_model_from_view (view);

    reg_acc = gnc_tree_model_split_reg_get_anchor (model);

    /* Rate from trans-curr to split-comm */
    rate_split_ok = xaccTransGetRateForCommodity (trans, xfer_comm, split, &rate_split);
    DEBUG("rate_split_ok %d and xfer_comm %s", rate_split_ok, gnc_commodity_get_fullname (xfer_comm));

    /* Rate from trans-curr to reg-comm */
    rate_reg_ok = xaccTransGetRateForCommodity (trans, reg_comm, split, &rate_reg);
    DEBUG("rate_reg_ok %d and reg_comm %s", rate_reg_ok, gnc_commodity_get_fullname (reg_comm));

    /* Are we expanded */
    expanded = gnc_tree_view_split_reg_trans_expanded (view, trans);

    if (gnc_commodity_equal (trans_curr, xfer_comm) && rate_split_ok)
    {
        xaccSplitSetAmount (split, amount);
        xaccSplitSetValue (split, amount);
        return TRUE;
    }

    if (rate_reg_ok && rate_split_ok && !force)
    {
        value = gnc_numeric_div (amount, rate_reg, gnc_commodity_get_fraction (trans_curr), GNC_HOW_DENOM_REDUCE);
        amount = gnc_numeric_mul (value, rate_split, GNC_DENOM_AUTO, GNC_HOW_RND_ROUND);
    }
    else
    {
        if (!rate_split_ok)
            rate_split = gtu_sr_get_rate_from_db (reg_comm, xfer_comm);

        /* create the exchange-rate dialog */
        xfer = gnc_xfer_dialog (NULL, NULL);

        gnc_xfer_dialog_is_exchange_dialog (xfer, &rate_split);

        /* fill in the dialog entries */
        gnc_xfer_dialog_set_description (xfer, xaccTransGetDescription (trans));
        gnc_xfer_dialog_set_memo (xfer, xaccSplitGetMemo (split));

        /* Get per book option */
        gnc_xfer_dialog_set_num (xfer, gnc_get_num_action (trans, split));
        gnc_xfer_dialog_set_date (xfer, timespecToTime64 (xaccTransRetDatePostedTS (trans)));

        value = amount;
        if (gnc_xfer_dialog_run_exchange_dialog (xfer, &rate_split, value, reg_acc, trans, xfer_comm, expanded))
        {
            if (!rate_split_ok)
                rate_split = gnc_numeric_create (1, 1);
            have_rate = FALSE;
        }
        else
            have_rate = TRUE;

        amount = gnc_numeric_mul (value, rate_split, GNC_DENOM_AUTO, GNC_HOW_RND_ROUND);
    }
    xaccSplitSetAmount (split, amount);
    xaccSplitSetValue (split, value);

    LEAVE("handle_exchange_rate set split %p amt=%s; and val=%s", split, gnc_numeric_to_string (amount), gnc_numeric_to_string (value));
    return have_rate;
}


/* Returns the value denom */
static int
gtu_sr_get_value_denom (Split *split)
{
    gnc_commodity *currency;
    int denom;

    currency = xaccTransGetCurrency (xaccSplitGetParent (split));
    denom = gnc_commodity_get_fraction (currency);
    if (denom == 0)
    {
        gnc_commodity *commodity = gnc_default_currency ();
        denom = gnc_commodity_get_fraction (commodity);
        if (denom == 0)
            denom = 100;
    }
    return denom;
}


/* Returns the amount denom */
static int
gtu_sr_get_amount_denom (Split *split)
{
    int denom;

    denom = xaccAccountGetCommoditySCU (xaccSplitGetAccount (split));
    if (denom == 0)
    {
        gnc_commodity *commodity = gnc_default_currency ();
        denom = gnc_commodity_get_fraction (commodity);
        if (denom == 0)
            denom = 100;
    }
    return denom;
}



/*###########################################################################*/



/* return TRUE if we have a RATE; return FALSE if we do not. */
gboolean
gnc_tree_util_split_reg_has_rate (GncTreeViewSplitReg *view)
{
    GncTreeModelSplitReg *model;

    model = gnc_tree_view_split_reg_get_model_from_view (view);

    switch (model->type)
    {
    case BANK_REGISTER2:
    case CASH_REGISTER2:
    case ASSET_REGISTER2:
    case CREDIT_REGISTER2:
    case LIABILITY_REGISTER2:
    case INCOME_REGISTER2:
    case EXPENSE_REGISTER2:
    case EQUITY_REGISTER2:
    case TRADING_REGISTER2:
    case GENERAL_JOURNAL2:
    case INCOME_LEDGER2:
    case SEARCH_LEDGER2:
        return TRUE;

    case STOCK_REGISTER2:
    case CURRENCY_REGISTER2:
    case PORTFOLIO_LEDGER2:
    case RECEIVABLE_REGISTER2:
    case PAYABLE_REGISTER2:
    default:
        return FALSE;
    }
}


/* Is this split part of a multi split transaction */
gboolean
gnc_tree_util_split_reg_is_multi (Split *split)
{
    gboolean multi = FALSE;
    Split *osplit;

    if (!split)
        return FALSE;

    osplit = xaccSplitGetOtherSplit (split);

    if (osplit)
            multi = FALSE;
    else
    {
        /* For multi-split transactions and stock splits,
         * use a special value. */
        osplit = xaccTransGetSplit (xaccSplitGetParent (split), 1);

        if (osplit)
            multi = TRUE;
        else if (g_strcmp0 ("stock-split", xaccSplitGetType (split)) == 0)
            multi = TRUE;
        else
            multi = FALSE;
    }
    return multi;
}


/* Return the string entry for transfer column and if multi */
const char *
gnc_tree_util_split_reg_get_transfer_entry (Split *split, gboolean *is_multi)
{
    static char *name = NULL;
    gboolean multi = FALSE;

    Split *osplit;

    if (is_multi)
        *is_multi = multi;

    if (!split)
        return NULL;

    osplit = xaccSplitGetOtherSplit (split);

    g_free (name);

    if (osplit)
        name = gnc_get_account_name_for_register (xaccSplitGetAccount (osplit));
    else
    {
        /* For multi-split transactions and stock splits,
         * use a special value. */
        osplit = xaccTransGetSplit (xaccSplitGetParent (split), 1);
        if (osplit)
        {
            name = g_strdup (SPLIT_TRANS_STR);
            multi = TRUE;
        }
        else if (g_strcmp0 ("stock-split", xaccSplitGetType (split)) == 0)
        {
            name = g_strdup (STOCK_SPLIT_STR);
            multi = TRUE;
        }
        else
            name = g_strdup ("");
    }

    if (is_multi)
        *is_multi = multi;

    return name;
}


/* Return the string entry for transfer column when template */
const char *
gnc_tree_util_split_reg_template_get_transfer_entry (Split *split)
{
    static char *name = NULL;
    Account *account;
    GncGUID *guid = NULL;

    /* Callers either g_strdup the return or use it as a temp for comparison,
       so we keep our static ref and free it on every call. */
    g_free (name);

    if (!split)
        return NULL;
    qof_instance_get (QOF_INSTANCE (split),
		      "sx-account", &guid,
		      NULL);
    account = xaccAccountLookup (guid, gnc_get_current_book ());
    name = account ? gnc_get_account_name_for_register (account) : NULL;

    return name;
}


const char *
gnc_tree_util_split_reg_template_get_fdebt_entry (Split *split)
{
    gchar *formula = NULL;

    g_return_val_if_fail (split != NULL, NULL);
    qof_instance_get (QOF_INSTANCE (split),
		      "sx-debit-formula", &formula,
		      NULL);

    return formula;
}

const char *
gnc_tree_util_split_reg_template_get_fcred_entry (Split *split)
{
    gchar *formula = NULL;

    g_return_val_if_fail (split != NULL, NULL);
    qof_instance_get (QOF_INSTANCE (split),
		      "sx-credit-formula", &formula,
		      NULL);

    return formula;
}


gchar *
gnc_tree_util_split_reg_get_date_help (GDate *date)
{
    char string[1024];
    struct tm tm;

    if (g_date_valid (date))
    {
        struct tm tm;
        memset (&tm, 0, sizeof (tm));
        g_date_to_struct_tm (date, &tm);
        qof_strftime (string, sizeof (string), _("%A %d %B %Y"), &tm);
        return g_strdup (string);
    }
    else
        return g_strdup (" ");
}


void
gnc_tree_util_split_reg_parse_date (GDate *parsed, const char *datestr)
{
    int day, month, year;
    gboolean use_autoreadonly = qof_book_uses_autoreadonly (gnc_get_current_book ());

    if (!parsed) return;
    if (!datestr) return;

    if (!qof_scan_date (datestr, &day, &month, &year))
    {
        // Couldn't parse date, use today
        struct tm tm_today;
        gnc_tm_get_today_start (&tm_today);
        day = tm_today.tm_mday;
        month = tm_today.tm_mon + 1;
        year = tm_today.tm_year + 1900;
    }

    // If we have an auto-read-only threshold, do not accept a date that is
    // older than the threshold.
    if (use_autoreadonly)
    {
        GDate *d = g_date_new_dmy (day, month, year);
        GDate *readonly_threshold = qof_book_get_autoreadonly_gdate (gnc_get_current_book());
        if (g_date_compare (d, readonly_threshold) < 0)
        {
            g_warning("Entered date %s is before the \"auto-read-only threshold\"; resetting to the threshold.", datestr);
#if 0
            GtkWidget *dialog = gtk_message_dialog_new (NULL,
                                                       0,
                                                       GTK_MESSAGE_ERROR,
                                                       GTK_BUTTONS_OK,
                                                       "%s", _("Cannot store a transaction at this date"));
            gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog),
                                                     "%s", _("The entered date of the new transaction is older than the \"Read-Only Threshold\" set for this book. "
                                                             "This setting can be changed in File -> Properties -> Accounts."));
            gtk_dialog_run (GTK_DIALOG (dialog));
            gtk_widget_destroy (dialog);
#endif

            // Reset the date to the threshold date
            day = g_date_get_day (readonly_threshold);
            month = g_date_get_month (readonly_threshold);
            year = g_date_get_year (readonly_threshold);
        }
        g_date_free (d);
        g_date_free (readonly_threshold);
    }
    g_date_set_dmy (parsed, day, month, year);
}


gboolean
gnc_tree_util_split_reg_rotate (GncTreeViewSplitReg *view, GtkTreeViewColumn *col, Transaction *trans, Split *split)
{
    GtkCellRenderer *cr0 = NULL;
    GList *renderers;
    ViewCol viewcol;

    // Get the first renderer, it has the view-column value.
    renderers = gtk_cell_layout_get_cells (GTK_CELL_LAYOUT (col));
    cr0 = g_list_nth_data (renderers, 0);
    g_list_free (renderers);

    viewcol = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (cr0), "view_column"));

    if (viewcol == COL_RECN)
    {
        const char recn_flags[] = {NREC, CREC, 0}; // List of reconciled flags
        const gchar *flags;
        const gchar *text;
        gchar *this_flag;
        gint index = 0;
        char rec;

        flags = recn_flags;

        text = g_strdup_printf("%c", xaccSplitGetReconcile (split));

        /* Find the existing text in the list of flags */
        this_flag = strstr (flags, text);

        if (this_flag != NULL && *this_flag != '\0')
        {
            /* In the list, choose the next item in the list
               (wrapping around as necessary). */
            index = this_flag - flags;

            if (flags[index + 1] != '\0')
                index = index + 1;
            else
                index = 0;

            rec = recn_flags[index];
        }
        else
            rec = NREC;

        gnc_tree_view_split_reg_set_dirty_trans (view, trans);
        if (!xaccTransIsOpen (trans))
            xaccTransBeginEdit (trans);

        xaccSplitSetReconcile (split, rec);
        return TRUE;
    }

    if (viewcol == COL_TYPE)
    {
        const char type_flags[] = {TXN_TYPE_INVOICE, TXN_TYPE_PAYMENT, 0}; // list of type flags
        const gchar *flags;
        const gchar *text;
        gchar *this_flag;
        gint index = 0;
        char type;

        flags = type_flags;

        text = g_strdup_printf("%c", xaccTransGetTxnType (trans));

        /* Find the existing text in the list of flags */
        this_flag = strstr (flags, text);

        if (this_flag != NULL && *this_flag != '\0')
        {
            /* In the list, choose the next item in the list
               (wrapping around as necessary). */
            index = this_flag - flags;

            if (flags[index + 1] != '\0')
                index = index + 1;
            else
                index = 0;

            type = type_flags[index];
        }
        else
            type = TXN_TYPE_NONE;

        gnc_tree_view_split_reg_set_dirty_trans (view, trans);
        if (!xaccTransIsOpen (trans))
            xaccTransBeginEdit (trans);

        xaccTransSetTxnType (trans, type);
        return TRUE;
    }
    return FALSE;
}

/*###########################################################################*/

/* returns TRUE if you need to convert the split's value to the local
 * (account) display currency.  Returns FALSE if you can just use the
 * split->value directly.
 */
gboolean
gnc_tree_util_split_reg_needs_conv_rate (GncTreeViewSplitReg *view,
                                    Transaction *trans, Account *acc)
{
    gnc_commodity *trans_cur, *acc_com;

    /* If there is not a RATE_CELL, then don't do anything */
    if (!gnc_tree_util_split_reg_has_rate (view))
        return FALSE;

    /* if txn->currency == acc->commodity, then return FALSE */
    acc_com = xaccAccountGetCommodity (acc);
    trans_cur = xaccTransGetCurrency (trans);
    if (trans_cur && acc_com && gnc_commodity_equal (trans_cur, acc_com))
        return FALSE;

    return TRUE;
}


gboolean
gnc_tree_util_split_reg_needs_amount (GncTreeViewSplitReg *view, Split *split)
{
    Transaction *txn = xaccSplitGetParent (split);
    Account *acc = xaccSplitGetAccount (split);

    return gnc_tree_util_split_reg_needs_conv_rate (view, txn, acc);
}

/*###########################################################################*/

gnc_numeric
gnc_tree_util_split_reg_get_value_for (GncTreeViewSplitReg *view, Transaction *trans, Split *split, gboolean is_blank)
{
    gnc_numeric        ret_num;
    GNCPrintAmountInfo ret_print_info;

    if (gnc_tree_util_split_reg_get_debcred_entry (view, trans, split, is_blank, &ret_num, &ret_print_info))
        return ret_num;
    else
        return gnc_numeric_zero();
}


gboolean
gnc_tree_util_split_reg_get_debcred_entry (GncTreeViewSplitReg *view,
                                      Transaction *trans, Split *split,
                                      gboolean is_blank, gnc_numeric *ret_num,
                                      GNCPrintAmountInfo *ret_print_info)

{
    GncTreeModelSplitReg *model;
    gnc_commodity *currency;

    model = gnc_tree_view_split_reg_get_model_from_view (view);

    currency = xaccTransGetCurrency (trans);
    if (!currency)
        currency = gnc_default_currency ();

    if (is_blank)
    {
        gnc_numeric imbalance;
        Account *acc;

        imbalance = xaccTransGetImbalanceValue (trans);

        if (gnc_numeric_zero_p (imbalance))
            return FALSE;

        if (xaccTransUseTradingAccounts (trans))
        {
            MonetaryList *imbal_list;
            gnc_monetary *imbal_mon;
            imbal_list = xaccTransGetImbalance (trans);

            if (!imbal_list)
            {
                /* No commodity imbalance, there shouldn't be a value imablance. */
                return FALSE;
            }

            if (imbal_list->next)
            {
                /* Multiple currency imbalance. */
                gnc_monetary_list_free (imbal_list);
                return FALSE;
            }

            imbal_mon = imbal_list->data;
            if (!gnc_commodity_equal (gnc_monetary_commodity (*imbal_mon), currency))
            {
                /* Imbalance is in wrong currency */
                gnc_monetary_list_free (imbal_list);
                return FALSE;
            }

            if (!gnc_numeric_equal (gnc_monetary_value (*imbal_mon), imbalance))
            {
                /* Value and commodity imbalances differ */
                gnc_monetary_list_free (imbal_list);
                return FALSE;
            }

            /* Done with the imbalance list */
            gnc_monetary_list_free (imbal_list);
        }

        imbalance = gnc_numeric_neg (imbalance);

        acc = gnc_tree_model_split_reg_get_anchor (model);

        if (gnc_tree_util_split_reg_needs_conv_rate (view, trans, acc))
        {
            imbalance = gnc_numeric_mul (imbalance,
                                         xaccTransGetAccountConvRate (trans, acc),
                                         gnc_commodity_get_fraction (currency),
                                         GNC_HOW_RND_ROUND_HALF_UP);
        }
        else
        {
            imbalance = gnc_numeric_convert (imbalance,
                                             gnc_commodity_get_fraction (currency),
                                             GNC_HOW_RND_ROUND_HALF_UP);
        }

        *ret_num = imbalance;
        *ret_print_info = gnc_account_print_info (acc, FALSE);
         return TRUE;
    }

    {
        gnc_numeric amount;
        gnc_commodity *split_commodity;
        GNCPrintAmountInfo print_info;
        Account *account;
        gnc_commodity *commodity;

        account = gnc_tree_model_split_reg_get_anchor (model);

        commodity = xaccAccountGetCommodity (account);
        split_commodity = xaccAccountGetCommodity (xaccSplitGetAccount (split));

        if (xaccTransUseTradingAccounts (trans))
        {
            gboolean use_symbol, is_current = FALSE;
            Split *current_split = gnc_tree_view_split_reg_get_current_split (view);
            RowDepth depth = gnc_tree_view_reg_get_selected_row_depth (view);

            if ((split == current_split) && (depth == SPLIT3))
                is_current = TRUE;

            if (model->type == STOCK_REGISTER2 ||
                    model->type == CURRENCY_REGISTER2 ||
                    model->type == PORTFOLIO_LEDGER2)
            {
                gnc_commodity *amount_commodity;
                /* security register.  If this split has price and shares columns,
                   use the value, otherwise use the amount.  */
                if (gtu_sr_use_security (view))
                {
                    amount = xaccSplitGetValue (split);
                    amount_commodity = currency;
                }
                else
                {
                    amount = xaccSplitGetAmount (split);
                    amount_commodity = split_commodity;
                }
                /* Show the currency if it is not the default currency */
                if (is_current ||
                        gnc_commodity_equiv (amount_commodity, gnc_default_currency ()))
                    use_symbol = FALSE;
                else
                    use_symbol = TRUE;
                print_info = gnc_commodity_print_info (amount_commodity, use_symbol);
            }
            else
            {
                /* non-security register, always use the split amount. */
                amount = xaccSplitGetAmount (split);
                if (is_current ||
                        gnc_commodity_equiv (split_commodity, commodity))
                    use_symbol = FALSE;
                else
                    use_symbol = TRUE;
                print_info = gnc_commodity_print_info (split_commodity, use_symbol);
            }
        }
        else
        {
            /* If this account is not a stock/mutual/currency account, and
            * currency != the account commodity, then use the SplitAmount
            * instead of the SplitValue.
            */
            switch (model->type)
            {
            case STOCK_REGISTER2:
            case CURRENCY_REGISTER2:
            case PORTFOLIO_LEDGER2:
                amount = xaccSplitGetValue (split);
                print_info = gnc_commodity_print_info (currency, FALSE);
                break;

            default:
                if (commodity && !gnc_commodity_equal (commodity, currency))
                    /* Convert this to the "local" value */
                    amount = xaccSplitConvertAmount (split, account);
                else
                    amount = xaccSplitGetValue (split);
                print_info = gnc_account_print_info (account, FALSE);
                break;
            }
        }

        if (gnc_numeric_zero_p (amount))
            return FALSE;

        *ret_num = amount;
        *ret_print_info = print_info;
         return TRUE;
    }
}

/*###########################################################################*/

void
gnc_tree_util_split_reg_set_value_for (GncTreeViewSplitReg *view, Transaction *trans, Split *split, gnc_numeric input, gboolean force)
{
    GncTreeModelSplitReg *model;
    GtkWidget *window;
    Account *anchor;
    Account *acct = xaccSplitGetAccount (split);
    gnc_commodity *currency;
    gnc_numeric value, amount, rate;

    ENTER("set_value_for trans %p and split %p input %s force %d", trans, split, gnc_numeric_to_string (input), force);

    currency = xaccTransGetCurrency (trans);

    model = gnc_tree_view_split_reg_get_model_from_view (view);

    anchor = gnc_tree_model_split_reg_get_anchor (model);

    if (gnc_numeric_zero_p (input))
    {
        xaccSplitSetValue (split, input);
        xaccSplitSetAmount (split, input);
        LEAVE("input is zero");
        return;
    }

    window = gnc_tree_view_split_reg_get_parent (view);

    if (gtu_sr_needs_exchange_rate (view, trans, split))
    {
        if (gtu_sr_handle_exchange_rate (view, input, trans, split, force))
        {
            ; //FIXME ??????
        }
        else
        {
            gnc_error_dialog (window, "%s",
                         _("Exchange Rate Canceled, using existing rate or default 1 to 1 rate if this is a new transaction."));
        }
        LEAVE("used exchange rate");
        return;
    }

    gnc_tree_util_split_reg_save_amount_values (view, trans, split, input);

    LEAVE(" ");
}

void
gnc_tree_util_split_reg_save_amount_values (GncTreeViewSplitReg *view, Transaction *trans, Split *split, gnc_numeric input)
{
    GncTreeModelSplitReg *model;
    Account *acc;
    gnc_numeric new_amount, convrate, amtconv, value;
    gnc_commodity *curr, *reg_com, *xfer_com;
    Account *xfer_acc;

    ENTER("View is %p, trans is %p, split is %p, input is %s", view, trans, split, gnc_numeric_to_string (input));

    model = gnc_tree_view_split_reg_get_model_from_view (view);

    new_amount = input;

    acc = gnc_tree_model_split_reg_get_anchor (model);

    xfer_acc = xaccSplitGetAccount (split);
    xfer_com = xaccAccountGetCommodity (xfer_acc);
    reg_com = xaccAccountGetCommodity (acc);
    curr = xaccTransGetCurrency (trans);

    if (!xaccTransGetRateForCommodity (trans, reg_com, NULL, &convrate))
        convrate = gnc_numeric_create (100, 100);

    amtconv = convrate;

    if (gnc_tree_util_split_reg_needs_conv_rate (view, trans, acc))
    {

        /* If we are in an expanded register and the xfer_acc->comm !=
        * reg_acc->comm then we need to compute the convrate here.
        * Otherwise, we _can_ use the rate_cell!
        */
        if (gnc_commodity_equal (reg_com, xfer_com))
            amtconv = xaccTransGetAccountConvRate (trans, acc);
    }

    if (xaccTransUseTradingAccounts (trans))
    {
        /* Using currency accounts, the amount is probably really the
           amount and not the value. */
        gboolean is_amount;

        if (model->type == STOCK_REGISTER2 ||
                model->type == CURRENCY_REGISTER2 ||
                model->type == PORTFOLIO_LEDGER2)
        {
            if (xaccAccountIsPriced (xfer_acc) ||
                    !gnc_commodity_is_iso (xaccAccountGetCommodity (xfer_acc)))
                is_amount = FALSE;
            else
                is_amount = TRUE;
        }
        else
        {
            is_amount = TRUE;
        }

        if (is_amount)
        {
            xaccSplitSetAmount (split, new_amount);
            if (gnc_tree_util_split_reg_needs_amount (view, split))
            {
                value = gnc_numeric_div (new_amount, amtconv,
                                        gnc_commodity_get_fraction (curr),
                                        GNC_HOW_RND_ROUND_HALF_UP);
                xaccSplitSetValue (split, value);
            }
            else
                xaccSplitSetValue (split, new_amount);
        }
        else
        {
            xaccSplitSetValue (split, new_amount);
        }
        LEAVE(" ");
        return;
    }

    /* How to interpret new_amount depends on our view of this
     * transaction.  If we're sitting in an account with the same
     * commodity as the transaction, then we can set the Value and then
     * compute the amount.  Otherwise we are setting the "converted
     * value".  This means we need to convert new_amount to the actual
     * 'value' by dividing by the convrate in order to set the value.
     */

    /* Now compute/set the split value.  Amount is in the register
     * currency but we need to convert to the txn currency.
     */
    if (gnc_tree_util_split_reg_needs_conv_rate (view, trans, acc))
    {
        /* convert the amount to the Value ... */
        value = gnc_numeric_div (new_amount, amtconv,
                                 gnc_commodity_get_fraction (curr),
                                 GNC_HOW_RND_ROUND_HALF_UP);
        xaccSplitSetValue (split, value);
    }
    else
    {
        xaccSplitSetValue (split, new_amount);
    }

    /* Now re-compute the Amount from the Value.  We may need to convert
     * from the Value back to the amount here using the convrate from
     * earlier.
     */
    value = xaccSplitGetValue (split);

    if (gnc_tree_util_split_reg_needs_amount (view, split))
    {
        acc = xaccSplitGetAccount (split);
        new_amount = gnc_numeric_mul (value, convrate,
                                      xaccAccountGetCommoditySCU (acc),
                                      GNC_HOW_RND_ROUND_HALF_UP);
        xaccSplitSetAmount (split, new_amount);
    }
    else
    {
        xaccSplitSetAmount (split, value);
    }
    LEAVE(" ");
}

/*###########################################################################*/

/* Takes the input with column and sets the price / amount / value so they are consistent */
void
gnc_tree_util_set_number_for_input (GncTreeViewSplitReg *view, Transaction *trans, Split *split, gnc_numeric input, gint viewcol)
{
    GncTreeModelSplitReg *model;
    gnc_numeric  price;
    gnc_numeric  amount;
    gnc_numeric  value;

    gboolean price_changed = FALSE;   // Price of each share
    gboolean value_changed = FALSE;   // Total value of shares
    gboolean amount_changed = FALSE;  // No of shares

    gboolean recalc_amount = FALSE;
    gboolean recalc_price = FALSE;
    gboolean recalc_value = FALSE;
    gboolean expanded = FALSE;
    int denom;
    Account *account = NULL;

    ENTER("trans %p and split %p and input is %s and viewcol is %d", trans, split, gnc_numeric_to_string (input), viewcol);

    model = gnc_tree_view_split_reg_get_model_from_view (view);

    /* Check for sub account view */
    if (!gnc_tree_model_split_reg_get_sub_account (model))
        account = gnc_tree_model_split_reg_get_anchor (model);

    expanded = gnc_tree_view_split_reg_trans_expanded (view, trans);

    if (!account)
        account = xaccSplitGetAccount (split);

    if (!xaccAccountIsPriced (account))
        return;

    /* If we are using commodity trading accounts then the value may
       not really be the value.  Punt if so. */
    if (xaccTransUseTradingAccounts (xaccSplitGetParent (split)))
    {
        gnc_commodity *acc_commodity;
        acc_commodity = xaccAccountGetCommodity (account);
        if (!(xaccAccountIsPriced (account) || !gnc_commodity_is_iso (acc_commodity)))
            return;
    }

    if (gnc_numeric_zero_p (input))
    {
        xaccSplitSetValue (split, input);
        xaccSplitSetAmount (split, input);
        LEAVE("zero");
        return;
    }

    amount = xaccSplitGetAmount (split);
    value = xaccSplitGetValue (split);

    if (viewcol == COL_AMTVAL && !expanded)
    {
        value_changed = TRUE;
        if (gnc_numeric_zero_p (amount))
        {
            xaccSplitSetValue (split, input);
            xaccSplitSetAmount (split, input);
            LEAVE("");
            return;
        }
    }
    else if (viewcol == COL_AMTVAL && expanded)
    {
        amount_changed = TRUE;
        if (gnc_numeric_zero_p (value))
        {
            xaccSplitSetValue (split, input);
            xaccSplitSetAmount (split, input);
            LEAVE("");
            return;
        }
    }

    if (viewcol == COL_PRICE)
    {
        price_changed = TRUE;
        if (gnc_numeric_zero_p (value))
        {
            amount = gnc_numeric_create (1,1);
            value = gnc_numeric_mul (input, amount, GNC_DENOM_AUTO, GNC_HOW_RND_ROUND);
            xaccSplitSetValue (split, input);
            xaccSplitSetAmount (split, amount);
            LEAVE("");
            return;
        }
    }

    if ((viewcol == COL_CREDIT || viewcol == COL_DEBIT) && !expanded)
    {
        amount_changed = TRUE;
        if (gnc_numeric_zero_p (value))
        {
            xaccSplitSetValue (split, input);
            xaccSplitSetAmount (split, input);
            LEAVE("");
            return;
        }
    }
    else if ((viewcol == COL_CREDIT || viewcol == COL_DEBIT) && expanded)
    {
        value_changed = TRUE;
        if (gnc_numeric_zero_p (value))
        {
            xaccSplitSetValue (split, input);
            xaccSplitSetAmount (split, input);
            LEAVE("");
            return;
        }
    }

    DEBUG("value_changed %d, price_changed %d, amount_changed %d", value_changed, price_changed, amount_changed);

    {
        int choice;
        int default_value;
        GList *node;
        GList *radio_list = NULL;
        const char *title = _("Recalculate Transaction");
        const char *message = _("The values entered for this transaction "
                                "are inconsistent. Which value would you "
                                "like to have recalculated?");

        if (amount_changed)
            radio_list = g_list_append (radio_list,
                                        g_strdup_printf ("%s (%s)",
                                                _("_Shares"), _("Changed")));
        else
            radio_list = g_list_append (radio_list, g_strdup (_("_Shares")));

        if (price_changed)
            radio_list = g_list_append (radio_list,
                                        g_strdup_printf ("%s (%s)",
                                                _("_Price"), _("Changed")));
        else
            radio_list = g_list_append (radio_list, g_strdup (_("_Price")));

        if (value_changed)
            radio_list = g_list_append (radio_list,
                                        g_strdup_printf ("%s (%s)",
                                                _("_Value"), _("Changed")));
        else
            radio_list = g_list_append (radio_list, g_strdup (_("_Value")));

        if(expanded)
        {
            if (price_changed)
                default_value = 2;  /* change the value */
            else
                default_value = 1;  /* change the price */
        }
        else
        {
            if (price_changed)
                default_value = 0;  /* change the amount / shares */
            else
                default_value = 1;  /* change the price */
        }
        choice = gnc_choose_radio_option_dialog
                 (gnc_tree_view_split_reg_get_parent (view),
                  title,
                  message,
                  _("_Recalculate"),
                  default_value,
                  radio_list);

        for (node = radio_list; node; node = node->next)
            g_free (node->data);

        g_list_free (radio_list);

        switch (choice)
        {
        case 0: /* Modify number of shares */
            recalc_amount = TRUE;
            break;
        case 1: /* Modify the share price */
            recalc_price = TRUE;
            break;
        case 2: /* Modify total value */
            recalc_value = TRUE;
            break;
        default: /* Cancel */
            LEAVE(" " );
            return;
        }
    }

    DEBUG("recalc_value %d, recalc_price %d, recalc_amount %d", recalc_value, recalc_price, recalc_amount);

    if (recalc_amount)
    {
        denom = gtu_sr_get_amount_denom (split);

        if (amount_changed)
        {
            LEAVE("");
            return;
        }

        if (price_changed)
            price = input;
        else
            price = gnc_numeric_div (value, amount, GNC_DENOM_AUTO, GNC_HOW_DENOM_EXACT);

        if (value_changed)
        {
            xaccSplitSetValue (split, input);
            amount = gnc_numeric_div (input, price, denom, GNC_HOW_RND_ROUND_HALF_UP);
            xaccSplitSetAmount (split, amount);
        }
        else
        {
            amount = gnc_numeric_div (value, price, denom, GNC_HOW_RND_ROUND_HALF_UP);
            xaccSplitSetAmount (split, amount);
        }
    }

    if (recalc_price)
    {
        if (price_changed)
        {
            LEAVE("");
            return;
        }

        if (amount_changed)
        {
            xaccSplitSetAmount (split, input);
            xaccSplitSetValue (split, value);
        }

        if (value_changed)
        {
            xaccSplitSetValue (split, input);
            xaccSplitSetAmount (split, amount);
        }
    }

    if (recalc_value)
    {
        denom = gtu_sr_get_value_denom (split);

        if (value_changed)
        {
            LEAVE("");
            return;
        }

        if (price_changed)
            price = input;
        else
            price = gnc_numeric_div (value, amount, GNC_DENOM_AUTO, GNC_HOW_DENOM_EXACT);

        if (amount_changed)
        {
            xaccSplitSetAmount (split, input);
            value = gnc_numeric_mul (input, price, denom, GNC_HOW_RND_ROUND_HALF_UP);
            xaccSplitSetValue (split, value);
        }
        else
        {
            value = gnc_numeric_mul (amount, price, denom, GNC_HOW_RND_ROUND_HALF_UP);
            xaccSplitSetValue (split, value);
        }
    }

    /* If the number of splits is two, change other split to balance */
    if (!gnc_tree_util_split_reg_is_multi (split) && expanded)
    {
        Split *osplit;
        gnc_commodity *osplit_com;

        osplit = xaccSplitGetOtherSplit (split);

        value = xaccSplitGetValue (split);

        osplit_com = xaccAccountGetCommodity (xaccSplitGetAccount (osplit));

        if (gnc_commodity_is_currency (osplit_com))
        {
            xaccSplitSetValue (osplit, gnc_numeric_neg (value));
            xaccSplitSetAmount (osplit, gnc_numeric_neg (value));
        }
    }
    LEAVE("");
}


/* Set the value for the given input amount */
void
gnc_tree_util_set_value_for_amount (GncTreeViewSplitReg *view, Transaction *trans, Split *split, gnc_numeric input)
{
    gnc_numeric  split_rate;
    gnc_numeric  amount;
    gnc_numeric  value, new_value;
    int denom;

    ENTER("trans %p and split %p and input is %s", trans, split, gnc_numeric_to_string (input));

    if (gnc_numeric_zero_p (input))
    {
        xaccSplitSetValue (split, input);
        xaccSplitSetAmount (split, input);
        LEAVE("zero");
        return;
    }

    amount = xaccSplitGetAmount (split);
    value = xaccSplitGetValue (split);

    denom = gtu_sr_get_value_denom (split);

    split_rate = gnc_numeric_div (value, amount, GNC_DENOM_AUTO, GNC_HOW_DENOM_EXACT);
    if (gnc_numeric_check (split_rate) != GNC_ERROR_OK)
        split_rate = gnc_numeric_create (100,100);

    new_value = gnc_numeric_mul (input, split_rate, denom, GNC_HOW_RND_ROUND_HALF_UP);

    xaccSplitSetValue (split, new_value);
    xaccSplitSetAmount (split, input);

    LEAVE("");
}


/* Get the rate */
gnc_numeric
gnc_tree_util_get_rate_for (GncTreeViewSplitReg *view, Transaction *trans, Split *split, gboolean is_blank)
{
    gnc_numeric num;

    ENTER("trans %p and split %p is_blank %d", trans, split, is_blank);

    num = gnc_tree_util_split_reg_get_value_for (view, trans, split, is_blank);
//FIXME Not sure about this...
    if (xaccTransUseTradingAccounts (trans))
        num = gnc_numeric_div (num, xaccSplitGetValue (split), GNC_DENOM_AUTO, GNC_HOW_RND_ROUND);
    else
        num = gnc_numeric_div (xaccSplitGetAmount (split), num, GNC_DENOM_AUTO, GNC_HOW_RND_ROUND);

    LEAVE("split %p and return num is %s", split, gnc_numeric_to_string (num));
    return num;
}


/*****************************************************************************/
