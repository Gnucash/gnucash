/********************************************************************\
 * dialog-stock-editor.c -- UI for stock editing                    *
 * Copyright (C) 2020 Christopher Lam                               *
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

#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "dialog-utils.h"
#include "engine-helpers.h"
#include "gnc-account-sel.h"
#include "gnc-amount-edit.h"
#include "gnc-date-edit.h"
#include "gnc-component-manager.h"
#include "gnc-event.h"
#include "gnc-gnome-utils.h"
#include "gnc-helpers.h"
#include "gnc-main-window.h"
#include "gnc-plugin-page-register.h"
#include "gnc-session.h"
#include "gnc-ui.h"
#include "gnc-ui-balances.h"
#include "dialog-stock-editor.h"

static QofLogModule log_module = GNC_MOD_GUI;

enum
{
     ACTION_COL_LABEL = 0,
     ACTION_COL_MASK,
     ACTION_COL_PROCEEDS,
     ACTION_COL_DIVIDEND,
     ACTION_COL_CAPGAINS,
     ACTION_COL_EXPENSES,
     ACTION_COL_NUM_COLUMNS
};

/** STRUCTS *********************************************************/
struct _StockEditorWindow
{
    Account *account;        /* The stock account account */
    gnc_commodity *trans_currency;
    time64 latest_split_date;
    gint component_id;       /* id of component */

    GtkWidget *window;       /* The stock-editor window                 */
    GtkWidget *date_entry;
    GtkWidget *action_combobox;

    GtkLabel *current_balance_label;
    GtkLabel *new_balance_label;

    GtkWidget *warning_icon;
    GtkLabel *warning_text;

    GtkWidget *proceeds_acc;
    GtkWidget *dividend_acc;
    GtkWidget *capgains_acc;
    GtkWidget *fees_acc;
    GtkWidget *fees_capitalize;

    GNCAmountEdit *asset_amount;
    GNCAmountEdit *asset_basis;
    GNCAmountEdit *proceeds_val;
    GNCAmountEdit *dividend_val;
    GNCAmountEdit *capgains_val;
    GNCAmountEdit *fees_val;

    GtkWidget *asset_memo;
    GtkWidget *proceeds_memo;
    GtkWidget *dividend_memo;
    GtkWidget *capgains_memo;
    GtkWidget *fees_memo;
    GtkEntry *description_entry;

    GtkWidget *ok_button;
    GtkWidget *cancel_button;
};

typedef struct _StockEditorWindow StockEditorWindow;

/** Callback prototypes************************************************/
void ok_button_cb     (GtkWidget *widget, StockEditorWindow *data);
void cancel_button_cb (GtkWidget *widget, StockEditorWindow *data);


static void gnc_stockeditor_set_title_name (GtkWidget *window, Account *account)
{
    gchar *fullname = gnc_account_get_full_name (account);
    gchar *title = g_strconcat(fullname, " - ", _("Stock Editor"), NULL);
    gtk_window_set_title (GTK_WINDOW (window), title);
    g_free (fullname);
    g_free (title);
}

void cancel_button_cb (GtkWidget *widget, StockEditorWindow *data)
{
    gtk_widget_destroy (data->window);
    g_free (data);
}

static gboolean gae_unfocus (GtkWidget *widget, const StockEditorWindow *data)
{
    gnc_amount_edit_evaluate (GNC_AMOUNT_EDIT (widget));
    return FALSE;
}


static Account * gas_get_account (GtkWidget *gae)
{
    if (!gtk_widget_get_sensitive (GTK_WIDGET (gae))) return NULL;
    return gnc_account_sel_get_account (GNC_ACCOUNT_SEL (gae));
}

static gnc_numeric gae_get_amount (GNCAmountEdit *gae)
{
    gnc_numeric retval = gnc_numeric_zero ();
    if (!gtk_widget_get_sensitive (GTK_WIDGET (gae))) return retval;
    if (!gnc_amount_edit_expr_is_valid (gae, &retval, FALSE)) return retval;
    return gnc_numeric_zero ();
}

static gboolean acct_missing_or_invalid (GtkWidget *gae)
{
    Account *acct;
    if (!gtk_widget_get_sensitive (gae)) return FALSE;
    acct = gnc_account_sel_get_account (GNC_ACCOUNT_SEL (gae));
    return (!acct || xaccAccountGetPlaceholder (acct));
}

static gchar * append_status (gchar *prev_str, gchar *str)
{
    gchar *newstr;
    if (!prev_str) return str;

    newstr = g_strconcat (prev_str, "\n", str, NULL);
    g_free (str);
    g_free (prev_str);
    return newstr;
}

static void check_signs (GNCAmountEdit *gae, gchar mask, gchar **status,
                         gboolean *passes, gchar *type)
{
    gchar *sign = NULL;
    gnc_numeric num;

    /* mask=7 is permissive. no need to check anything. */
    if (!gtk_widget_get_sensitive (GTK_WIDGET (gae)) || (mask == '7'))
        return;

    num = gae_get_amount (gae);
    if (gnc_numeric_positive_p (num) &&
        (mask == '1' || mask == '3' || mask == '#' || mask == '5'))
        return;
    if ((gnc_numeric_zero_p (num)) &&
        (mask == '2' || mask == '3' || mask == '#' || mask == '6'))
        return;
    if (gnc_numeric_negative_p (num) &&
        (mask == '4' || mask == '5' || mask == '6'))
        return;

    if (mask == '1') sign = _("positive");
    else if (mask == '2') sign = _("zero");
    else if (mask == '3') sign = _("zero or positive");
    else if (mask == '#') sign = _("zero or positive");
    else if (mask == '4') sign = _("negative");
    else if (mask == '5') sign = _("negative or positive");
    else if (mask == '6') sign = _("zero or negative");
    *status = append_status (*status, g_strdup_printf (_("%s must be %s"), type, sign));
    *passes = FALSE;
    return;
}

static void refresh_all (GtkWidget *widget, const StockEditorWindow *data)
{
    gnc_numeric bal, old_bal, new_bal;
    GNCPrintAmountInfo printinfo;
    gboolean passes = TRUE;
    gchar *status = NULL, *mask;
    gnc_numeric asset_amount, asset_basis, proceeds_val, dividend_val, capgains_val,
        fees_val;
    Account *proceeds_acc, *dividend_acc, *capgains_acc, *fees_acc;
    GtkTreeIter action_iter;

    if (!gtk_combo_box_get_active_iter (GTK_COMBO_BOX (data->action_combobox),
                                        &action_iter))
    {
        PERR ("shouldn't happen. action should always select item.");
        return;
    }

    gnc_suspend_gui_refresh ();

    gtk_tree_model_get (gtk_combo_box_get_model
                        (GTK_COMBO_BOX (data->action_combobox)),
                        &action_iter, ACTION_COL_MASK, &mask, -1);

    printinfo = gnc_account_print_info (data->account, TRUE);

    proceeds_acc = gas_get_account (data->proceeds_acc);
    dividend_acc = gas_get_account (data->dividend_acc);
    capgains_acc = gas_get_account (data->capgains_acc);
    fees_acc = gas_get_account (data->fees_acc);

    asset_amount = gae_get_amount (data->asset_amount);
    asset_basis  = gae_get_amount (data->asset_basis);
    proceeds_val = gae_get_amount (data->proceeds_val);
    dividend_val = gae_get_amount (data->dividend_val);
    capgains_val = gae_get_amount (data->capgains_val);
    fees_val = gae_get_amount (data->fees_val);

    /* update current & new balances */
    old_bal = xaccAccountGetBalance (data->account);
    gtk_label_set_text (data->current_balance_label,
                        xaccPrintAmount (old_bal, printinfo));
    new_bal = gnc_numeric_add_fixed (old_bal, asset_amount);
    gtk_label_set_text (data->new_balance_label,
                        xaccPrintAmount (new_bal, printinfo));

    if (gnc_numeric_negative_p
        (gnc_numeric_mul
         (old_bal, new_bal, GNC_DENOM_AUTO, GNC_HOW_RND_ROUND)))
    {
        status = append_status
            (status, g_strdup (_("Cannot sell more units than available.")));
        passes = FALSE;
    }

    bal = asset_basis;
    bal = gnc_numeric_add_fixed (bal, proceeds_val);
    bal = gnc_numeric_add_fixed (bal, dividend_val);
    bal = gnc_numeric_add_fixed (bal, fees_val);

    if (gnc_date_edit_get_date (GNC_DATE_EDIT (data->date_entry)) <
        data->latest_split_date)
    {
        status = append_status
            (status, g_strdup (_("Date is before latest split. Balances may not \
be valid.")));
    }

    if (acct_missing_or_invalid (data->proceeds_acc) ||
        acct_missing_or_invalid (data->fees_acc) ||
        acct_missing_or_invalid (data->capgains_acc) ||
        acct_missing_or_invalid (data->dividend_acc))
    {
        status = append_status
            (status, g_strdup (_("Account missing or placeholder")));
        passes = FALSE;
    }

    printinfo = gnc_commodity_print_info (data->trans_currency, TRUE);
    if (!gnc_numeric_zero_p (bal))
    {
        status = append_status
            (status, g_strdup_printf (_("Imbalance of %s"),
                                      xaccPrintAmount (bal, printinfo)));
        passes = FALSE;
    }

    check_signs (data->asset_amount, mask[0], &status, &passes, _("Units"));
    check_signs (data->asset_basis,  mask[1], &status, &passes, _("Basis"));
    check_signs (data->proceeds_val, mask[2], &status, &passes, _("Proceeds"));
    check_signs (data->dividend_val, mask[3], &status, &passes, _("Dividend"));
    check_signs (data->capgains_val, mask[4], &status, &passes, _("CapGains"));
    check_signs (data->fees_val, mask[5], &status, &passes, _("Fees"));

    gtk_widget_set_visible (data->warning_icon, !passes);
    gtk_widget_set_sensitive (data->ok_button, passes);
    gtk_label_set_text (data->warning_text, status);

    g_free (mask);
    if (status)
        g_free (status);

    gnc_resume_gui_refresh ();

    return;
}

static void capitalize_toggled_cb (GtkWidget *widget, const StockEditorWindow *data)
{
    if (!gtk_widget_get_sensitive (GTK_WIDGET (data->fees_val)))
        return;

    gtk_widget_set_sensitive
        (GTK_WIDGET (data->fees_acc),
         !gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (data->fees_capitalize)));

    refresh_all (widget, data);
}



static void action_changed_cb (GtkWidget *widget, const StockEditorWindow *data)
{
    GtkTreeIter action_iter;
    GtkTreeModel *model;
    gchar *mask, *asset_memo, *proceeds_memo, *dividend_memo,
        *capgains_memo, *fees_memo;

    /* check action combobox and update visibility of fields*/
    if (!gtk_combo_box_get_active_iter (GTK_COMBO_BOX (data->action_combobox),
                                        &action_iter))
    {
        PERR ("shouldn't happen. action should always select item.");
        return;
    }

    model = gtk_combo_box_get_model (GTK_COMBO_BOX (data->action_combobox));
    gtk_tree_model_get (model, &action_iter,
                        ACTION_COL_MASK, &mask,
                        ACTION_COL_PROCEEDS, &proceeds_memo,
                        ACTION_COL_DIVIDEND, &dividend_memo,
                        ACTION_COL_CAPGAINS, &capgains_memo,
                        ACTION_COL_EXPENSES, &fees_memo, -1);
    gtk_widget_set_sensitive (GTK_WIDGET (data->asset_amount), mask[0] != '0');
    gtk_widget_set_sensitive (GTK_WIDGET (data->asset_basis), mask[1] != '0');
    gtk_widget_set_sensitive (GTK_WIDGET (data->proceeds_val), mask[2] != '0');
    gtk_widget_set_sensitive (GTK_WIDGET (data->proceeds_acc), mask[2] != '0');
    gtk_widget_set_sensitive (GTK_WIDGET (data->proceeds_memo), mask[2] != '0');
    gtk_widget_set_sensitive (GTK_WIDGET (data->dividend_val), mask[3] != '0');
    gtk_widget_set_sensitive (GTK_WIDGET (data->dividend_acc), mask[3] != '0');
    gtk_widget_set_sensitive (GTK_WIDGET (data->dividend_memo), mask[3] != '0');
    gtk_widget_set_sensitive (GTK_WIDGET (data->capgains_val), mask[4] != '0');
    gtk_widget_set_sensitive (GTK_WIDGET (data->capgains_acc), mask[4] != '0');
    gtk_widget_set_sensitive (GTK_WIDGET (data->capgains_memo), mask[4] != '0');
    gtk_widget_set_sensitive (GTK_WIDGET (data->fees_val), mask[5] != '0');
    gtk_widget_set_sensitive (GTK_WIDGET (data->fees_acc), mask[5] != '0');
    gtk_widget_set_sensitive (GTK_WIDGET (data->fees_memo), mask[5] != '0');
    gtk_widget_set_sensitive (GTK_WIDGET (data->fees_capitalize), mask[5] != '0');

    gtk_entry_set_text (GTK_ENTRY (data->proceeds_memo), proceeds_memo);
    gtk_entry_set_text (GTK_ENTRY (data->dividend_memo), dividend_memo);
    gtk_entry_set_text (GTK_ENTRY (data->capgains_memo), capgains_memo);
    gtk_entry_set_text (GTK_ENTRY (data->fees_memo), fees_memo);

    if (mask[5] == '#')
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (data->fees_capitalize), TRUE);
    if (mask[5] == '3')
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (data->fees_capitalize), FALSE);

    /* capitalize_toggled_cb (widget, data); */
    /* refresh_all (widget, data); */
    g_free (mask);
}

static void create_split (Transaction * txn, Account *account, GtkWidget *memo,
                          GNCAmountEdit *amount, GNCAmountEdit *value,
                          gboolean reverse)
{
    Split * split;
    gnc_numeric amt, val;
    const gchar *memostr;

    /* if account is NULL it means the account widget is
       disabled. skip creating split. */
    if (!account)
        return;

    memostr = gtk_entry_get_text (GTK_ENTRY (memo));
    amt = amount ? gae_get_amount (amount) : gnc_numeric_zero ();
    val = value  ? gae_get_amount (value)  : gnc_numeric_zero ();

    split = xaccMallocSplit (gnc_get_current_book ());
    xaccAccountBeginEdit (account);
    xaccSplitSetAccount (split, account);
    xaccSplitSetParent (split, txn);
    xaccSplitSetAmount (split, reverse ? gnc_numeric_neg (amt) : amt);
    xaccSplitSetValue (split, reverse ? gnc_numeric_neg (val) : val);
    xaccSplitSetMemo (split, memostr);
    gnc_set_num_action (NULL, split, NULL, memostr);
    xaccAccountCommitEdit (account);
}

void ok_button_cb (GtkWidget *widget, StockEditorWindow *data)
{
    Account
        *proceeds_acc = gas_get_account (data->proceeds_acc),
        *dividend_acc = gas_get_account (data->dividend_acc),
        *capgains_acc = gas_get_account (data->capgains_acc),
        *fees_acc     = gas_get_account (data->fees_acc);
    Transaction *txn = xaccMallocTransaction (gnc_get_current_book ());
    time64 date = gnc_date_edit_get_date (GNC_DATE_EDIT (data->date_entry));
    const gchar *desc = gtk_entry_get_text (GTK_ENTRY (data->description_entry));
    gboolean fees_capitalize = gtk_toggle_button_get_active
        (GTK_TOGGLE_BUTTON (data->fees_capitalize));

    gnc_suspend_gui_refresh ();

    gnc_account_set_linked_account (data->account, "proceeds_account", proceeds_acc);
    gnc_account_set_linked_account (data->account, "dividend_account", dividend_acc);
    gnc_account_set_linked_account (data->account, "capgains_account", capgains_acc);
    gnc_account_set_linked_account (data->account, "fees_account", fees_acc);

    xaccTransBeginEdit (txn);
    xaccTransSetCurrency (txn, data->trans_currency);
    xaccTransSetDatePostedSecsNormalized (txn, date);
    xaccTransSetDescription (txn, desc);

    create_split (txn, data->account, data->asset_memo, data->asset_amount, data->asset_basis, FALSE);
    create_split (txn, proceeds_acc, data->proceeds_memo, data->proceeds_val, data->proceeds_val, FALSE);
    create_split (txn, dividend_acc, data->dividend_memo, data->dividend_val, data->dividend_val, FALSE);

    if (fees_capitalize)
        create_split (txn, data->account, data->fees_memo, NULL, data->fees_val, FALSE);
    else
        create_split (txn, fees_acc, data->fees_memo, data->fees_val, data->fees_val, FALSE);

    if (capgains_acc)
    {
        create_split (txn, capgains_acc, data->capgains_memo, data->capgains_val, data->capgains_val, FALSE);
        create_split (txn, data->account, data->capgains_memo, NULL, data->capgains_val, TRUE);
    }

    xaccTransCommitEdit (txn);

    gnc_resume_gui_refresh ();

    cancel_button_cb (widget, data);
}

static GtkWidget *connect_account (GtkBuilder *builder, const gchar *id,
                                   const gchar *id_box,
                                   StockEditorWindow *data, GList *types)
{
    GtkBox *box = GTK_BOX (gtk_builder_get_object (builder, id_box));
    GtkWidget *retval = gnc_account_sel_new ();
    Account *stored_acc = gnc_account_get_linked_account (data->account, id);
    GList *commodities = g_list_prepend (NULL, data->trans_currency);

    gnc_account_sel_set_acct_filters (GNC_ACCOUNT_SEL (retval), types, commodities);
    if (stored_acc)
    {
        if (!gnc_commodity_equal (xaccAccountGetCommodity (stored_acc),
                                 data->trans_currency))
            PWARN ("stored %s acc %s has incorrect currency\n", id,
                   gnc_commodity_get_mnemonic (xaccAccountGetCommodity (stored_acc)));
        else if (xaccAccountGetPlaceholder (stored_acc))
            PWARN ("stored %s acc %s is placeholder\n", id,
                   gnc_commodity_get_mnemonic (xaccAccountGetCommodity (stored_acc)));
        else
            gnc_account_sel_set_account (GNC_ACCOUNT_SEL (retval), stored_acc, TRUE);
    }

    gtk_box_pack_start (box, retval, TRUE, TRUE, 0);
    g_signal_connect (retval, "account_sel_changed", G_CALLBACK (refresh_all), data);
    g_list_free (commodities);
    return retval;
}

static GNCAmountEdit *connect_amount_edit (GtkBuilder *builder, const gchar *id,
                                           const Account *account,
                                           StockEditorWindow *data)
{
    GtkBox *box = GTK_BOX (gtk_builder_get_object (builder, id));
    GNCAmountEdit *retval = GNC_AMOUNT_EDIT (gnc_amount_edit_new ());

    if (account)
    {
        GNCPrintAmountInfo print_info = gnc_account_print_info (account, FALSE);
        gint scu = xaccAccountGetCommoditySCU (account);
        gnc_amount_edit_set_print_info (GNC_AMOUNT_EDIT (retval), print_info);
        gnc_amount_edit_set_fraction (GNC_AMOUNT_EDIT (retval), scu);
    }
    gtk_box_pack_start (box, GTK_WIDGET (retval), TRUE, TRUE, 0);
    g_signal_connect (retval, "changed", G_CALLBACK (refresh_all), data);
    g_signal_connect (retval, "focus-out-event", G_CALLBACK (gae_unfocus), data);
    return retval;
}


static void add_action (GtkListStore *store, const gchar *mask, const gchar *text,
                        const gchar *proceeds, const gchar *dividend,
                        const gchar *capgains, const gchar *fees)
{
    gtk_list_store_insert_with_values (store, NULL, -1,
                                       ACTION_COL_LABEL, text,
                                       ACTION_COL_MASK, mask,
                                       ACTION_COL_PROCEEDS, proceeds,
                                       ACTION_COL_DIVIDEND, dividend,
                                       ACTION_COL_CAPGAINS, capgains,
                                       ACTION_COL_EXPENSES, fees, -1);
}

/* initializes action list. each action has metadata for fields.
 * 6 fields: asset_amount, asset_basis, proceeds, dividend, capgains, fees
 * mask: 6-char string indicating field sensitivity
 *       0: disable
 *       1: enabled, must be positive
 *       2: enabled, must be zero
 *       3: enabled, may be zero or positive
 *       #: enabled, may be zero or positive, but capitalize
 *       4: enabled, must be negative
 *       5: enabled, may be negative or positive
 *       6: enabled, may be negative or zero
 *       7: enabled, may be negative zero or positive
 * label: label string
 * 4 field memos
 */
static void initialize_action (GtkWidget *combobox, gnc_numeric balance)
{
    GtkListStore *store = gtk_list_store_new (ACTION_COL_NUM_COLUMNS,
                                              G_TYPE_STRING, /* mask */
                                              G_TYPE_STRING, /* label */
                                              G_TYPE_STRING, /* proceeds_memo */
                                              G_TYPE_STRING, /* dividend_memo */
                                              G_TYPE_STRING, /* capgains_memo */
                                              G_TYPE_STRING); /* fees_memo */
    if (gnc_numeric_positive_p (balance))
    {
        add_action (store, "11400#", _("Buy"), _("Source"), "", "", _("Fees"));
        add_action (store, "441073", _("Sell"), _("Proceeds"), "", _("Capgains"), _("Fees"));
        add_action (store, "001403", _("Dividend"), _("Proceeds"), _("Dividend"), "", _("Fees"));
        add_action (store, "010400", _("Notional Distribution"), "", _("Notional Distribution"), "", "");
        add_action (store, "041000", _("Return of Capital"), _("Proceeds"), "", "", "");
        add_action (store, "500000", _("Stock Split"), "", "", "", "");
    }
    else if (gnc_numeric_negative_p (balance))
    {
        add_action (store, "44100#", _("Short Sell"), _("Source"), "", "", _("Fees"));
        add_action (store, "114073", _("Short Buy"), _("Proceeds"), "", _("Capgains"), _("Fees"));
        add_action (store, "004103", _("Compensatory Dividend"), _("Proceeds"), _("Dividend"), "", _("Fees"));
        add_action (store, "040100", _("Compensatory Notional Distribution"), "", _("Notional Distribution"), "", "");
        add_action (store, "014000", _("Compensatory Return of Capital"), _("Proceeds"), "", "", "");
        add_action (store, "500000", _("Stock Split"), "", "", "", "");
    }
    else
    {
        add_action (store, "11400#", _("Open Long"), _("Source"), "", "", _("Fees"));
        add_action (store, "44100#", _("Open Short"), _("Source"), "", "", _("Fees"));
    }
    gtk_combo_box_set_model (GTK_COMBO_BOX (combobox), GTK_TREE_MODEL (store));
    gtk_combo_box_set_active (GTK_COMBO_BOX (combobox), 0);
    g_object_unref (store);
}

static time64 account_get_latest_date (const Account *account)
{
    GList *last = NULL;
    for (GList *n = xaccAccountGetSplitList (account); n; n = n->next)
        last = n->data;
    return last ? xaccTransGetDate (xaccSplitGetParent ((Split*)last)) : -INT64_MAX;
}

/********************************************************************   \
 * stockeditorWindow                                                *
 *   opens up the window to stock-editor                            *
 *                                                                  *
 * Args:   parent  - the parent of this window                      *
 *         account - the account to stock-edit                      *
\********************************************************************/
void gnc_ui_stockeditor_dialog (GtkWidget *parent, Account *account)
{
    GtkBox *box;
    GtkBuilder *builder;
    GList *types;
    StockEditorWindow *data;

    g_return_if_fail (parent);
    g_return_if_fail (GNC_IS_ACCOUNT (account));

    if (!xaccAccountIsPriced (account))
    {
        PWARN ("Stock Editor for Stock accounts only");
        return;
    }

    data = g_new0 (StockEditorWindow, 1);

    data->account = account;
    data->trans_currency = xaccAccountGetCommodity (gnc_get_current_root_account ());
    data->latest_split_date = account_get_latest_date (account);

    /* Create the dialog box */
    builder = gtk_builder_new();

    gnc_builder_add_from_file (builder, "dialog-stock-editor.glade",
                               "stock_transaction_editor");

    /* window */
    data->window = GTK_WIDGET (gtk_builder_get_object
                               (builder, "stock_transaction_editor"));
    gnc_stockeditor_set_title_name (data->window, account);
    gtk_widget_set_name (GTK_WIDGET(data->window), "gnc-id-stock-editor");

    data->ok_button = GTK_WIDGET (gtk_builder_get_object (builder, "okbutton1"));
    data->cancel_button = GTK_WIDGET (gtk_builder_get_object (builder, "cancelbutton1"));

    data->date_entry = gnc_date_edit_new (gnc_time (NULL), FALSE, FALSE);
    g_signal_connect (data->date_entry, "date_changed", G_CALLBACK (refresh_all), data);
    box = GTK_BOX (gtk_builder_get_object (builder, "post_date_box"));
    gtk_box_pack_end (box, data->date_entry, TRUE, TRUE, 0);

    /* action */
    data->action_combobox = GTK_WIDGET (gtk_builder_get_object (builder, "action_combobox"));
    initialize_action (data->action_combobox, xaccAccountGetBalance (account));
    g_signal_connect (data->action_combobox, "changed", G_CALLBACK (action_changed_cb), data);

    /* description */
    data->description_entry = GTK_ENTRY (gtk_builder_get_object (builder, "description_entry"));

    /* current and new balances */
    data->current_balance_label = GTK_LABEL (gtk_builder_get_object
                                             (builder, "current_balance_label"));
    data->new_balance_label = GTK_LABEL (gtk_builder_get_object
                                         (builder, "new_balance_label"));
    gtk_label_set_text (GTK_LABEL (gtk_builder_get_object (builder, "trans_currency_label")),
                        gnc_commodity_get_mnemonic (data->trans_currency));

    /* warning text & icon */
    data->warning_text = GTK_LABEL (gtk_builder_get_object (builder, "warning_text"));
    data->warning_icon = GTK_WIDGET (gtk_builder_get_object (builder, "warning_icon"));

    /* accounts */
    types = g_list_prepend (NULL, GINT_TO_POINTER (ACCT_TYPE_CASH));
    types = g_list_prepend (types, GINT_TO_POINTER (ACCT_TYPE_ASSET));
    data->proceeds_acc = connect_account (builder, "proceeds_account", "proceeds_account_box", data, types);
    g_list_free (types);

    types = g_list_prepend (NULL, GINT_TO_POINTER (ACCT_TYPE_INCOME));
    data->dividend_acc = connect_account (builder, "dividend_account", "dividend_account_box", data, types);
    data->capgains_acc = connect_account (builder, "capgains_account", "capgains_account_box", data, types);
    g_list_free (types);

    types = g_list_prepend (NULL, GINT_TO_POINTER (ACCT_TYPE_EXPENSE));
    data->fees_acc = connect_account (builder, "fees_account", "fees_account_box", data, types);
    g_list_free (types);

    /* Add amount edit box */
    data->asset_amount = connect_amount_edit (builder, "units_box", account, data);
    data->asset_basis  = connect_amount_edit (builder, "value_box", NULL, data);
    data->proceeds_val = connect_amount_edit (builder, "proceeds_box", NULL, data);
    data->dividend_val = connect_amount_edit (builder, "dividend_box", NULL, data);
    data->capgains_val = connect_amount_edit (builder, "capgains_box", NULL, data);
    data->fees_val = connect_amount_edit (builder, "fees_box", NULL, data);

    g_signal_connect (data->ok_button, "clicked", G_CALLBACK (ok_button_cb), data);
    g_signal_connect (data->cancel_button, "clicked",
                      G_CALLBACK (cancel_button_cb), data);

    data->asset_memo    = GTK_WIDGET (gtk_builder_get_object (builder, "asset_memo"));
    data->proceeds_memo = GTK_WIDGET (gtk_builder_get_object (builder, "proceeds_memo"));
    data->dividend_memo = GTK_WIDGET (gtk_builder_get_object (builder, "dividend_memo"));
    data->capgains_memo = GTK_WIDGET (gtk_builder_get_object (builder, "capgains_memo"));
    data->fees_memo     = GTK_WIDGET (gtk_builder_get_object (builder, "fees_memo"));
    data->fees_capitalize = GTK_WIDGET (gtk_builder_get_object (builder, "fees_capitalize"));

    g_signal_connect (data->fees_capitalize, "toggled",
                      G_CALLBACK (capitalize_toggled_cb), data);

    /* Autoconnect signals */
    /* gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, */
    /*                                   data->window); */
    if (parent)
        gtk_window_set_transient_for (GTK_WINDOW (data->window), GTK_WINDOW (parent));

    /* gtk_builder_connect_signals(builder, data); */
    g_object_unref (G_OBJECT (builder));

    gtk_widget_show_all (data->window);

    action_changed_cb (NULL, data);
    refresh_all (NULL, data);

    gtk_widget_grab_focus (GTK_WIDGET (data->action_combobox));

    gtk_window_present (GTK_WINDOW (data->window));
}
