/********************************************************************\
 * dialog-account.c -- window for creating and editing accounts for *
 *                     GnuCash                                      *
 * Copyright (C) 2000 Dave Peticolas <dave@krondo.com>              *
 * Copyright (C) 2003,2005,2006 David Hampton <hampton@employees.org> *
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
#include <math.h>
#ifdef G_OS_WIN32
#include <pow.h>
#endif
#include <string.h>

#include "Transaction.h"
#include "dialog-account.h"
#include "dialog-commodity.h"
#include "dialog-utils.h"
#include "gnc-gtk-utils.h"
#include "gnc-amount-edit.h"
#include "gnc-general-select.h"
#include "gnc-commodity.h"
#include "gnc-commodity-edit.h"
#include "gnc-component-manager.h"
#include "gnc-date-edit.h"
#include "gnc-engine.h"
#include "gnc-gui-query.h"
#include "gnc-session.h"
#include "gnc-tree-model-account-types.h"
#include "gnc-tree-view-account.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include <gnc-locale-tax.h>

#define DIALOG_NEW_ACCOUNT_CM_CLASS "dialog-new-account"
#define DIALOG_EDIT_ACCOUNT_CM_CLASS "dialog-edit-account"
#define GNC_PREFS_GROUP "dialogs.account"
#define DEFAULT_COLOR "rgb(237,236,235)"

enum account_cols
{
    ACCOUNT_COL_FULLNAME = 0,
    ACCOUNT_COL_FIELDNAME,
    ACCOUNT_COL_OLD_VALUE,
    ACCOUNT_COL_NEW_VALUE,
    NUM_ACCOUNT_COLS
};

typedef enum
{
    NEW_ACCOUNT,
    EDIT_ACCOUNT
} AccountDialogType;

typedef struct _AccountWindow
{
    QofBook   *book;
    gboolean   modal;
    GtkWindow *dialog;
    gboolean   closing;

    AccountDialogType dialog_type;

    GncGUID  account;
    Account *created_account;
    GncGUID  created_account_guid;
    GncNewAccountCreatedCB creation_callback;
    gpointer creation_callback_data;
    GncSessionOperationContext *operation_context;

    gchar **subaccount_names;
    gchar **next_name;

    GNCAccountType type;

    GtkWidget *notebook;

    GtkWidget     *name_entry;
    GtkWidget     *description_entry;
    GtkColorDialogButton *color_entry_button;
    GtkWidget     *color_default_button;
    GtkWidget     *code_entry;
    GtkTextBuffer *notes_text_buffer;

    GtkWidget            *commodity_edit;
    dialog_commodity_mode commodity_mode;
    GtkWidget            *account_scu;

    guint32        valid_types;
    guint32        displayed_types;
    GNCAccountType preferred_account_type;
    gboolean       updating_type_dropdown;
    GtkWidget     *type_combo;
    GtkWidget     *parent_tree;
    GtkWidget     *parent_scroll;

    GtkWidget *more_properties_page;

    GtkWidget *balance_grid;
    GtkWidget *higher_balance_limit_edit;
    GtkWidget *lower_balance_limit_edit;
    GtkWidget *include_balance_sub_accts;
    gboolean   balance_is_reversed;

    GtkWidget *opening_balance_button;
    GtkWidget *opening_balance_edit;
    GtkWidget *opening_balance_date_edit;
    GtkWidget *opening_balance_page;

    GtkWidget *opening_equity_radio;
    GtkWidget *transfer_account_scroll;
    GtkWidget *transfer_tree;

    GtkWidget *tax_related_button;
    GtkWidget *placeholder_button;
    GtkWidget *hidden_button;
    GtkWidget *auto_interest_button;

    gint component_id;

    GObject *selection;
    gulong handler_id;
} AccountWindow;

typedef struct _RenumberDialog
{
    GtkWindow *dialog;
    GtkWidget *prefix;
    GtkWidget *interval;
    GtkWidget *digits;
    GtkWidget *example1;
    GtkWidget *example2;

    QofBook   *book;
    GncGUID    book_guid;
    GncGUID    parent_guid;
    gboolean   closing;
    gint       num_children;
} RenumberDialog;

#define RENUMBER_DIALOG_DATA "gnc-account-renumber-dialog"

/** Static Globals *******************************************************/
static QofLogModule log_module = GNC_MOD_GUI;

static GNCAccountType last_used_account_type = ACCT_TYPE_BANK;

static GList *ac_destroy_cb_list = NULL;

/** Declarations *********************************************************/
static void gnc_account_window_set_name (AccountWindow *aw);

void gnc_account_renumber_prefix_changed_cb (GtkEditable *editable, RenumberDialog *data);
void gnc_account_renumber_interval_changed_cb (GtkSpinButton *spinbutton, RenumberDialog *data);
void gnc_account_renumber_digits_changed_cb (GtkSpinButton *spinbutton, RenumberDialog *data);
static void gnc_account_renumber_apply_cb (GtkButton *button, RenumberDialog *data);
static void gnc_account_renumber_cancel_cb (GtkButton *button, RenumberDialog *data);
static gboolean gnc_account_renumber_close_request_cb (GtkWindow *window, RenumberDialog *data);

void gnc_account_window_destroy_cb (GtkWidget *object, gpointer data);
void opening_equity_cb (GtkWidget *w, gpointer data);
static void gnc_account_parent_changed_cb (GObject *selection, gpointer data);
void gnc_account_name_changed_cb (GtkWidget *widget, gpointer data);
void gnc_account_color_default_cb (GtkWidget *widget, gpointer data);
void gnc_account_name_insert_text_cb (GtkWidget   *entry,
                                      const gchar *text,
                                      gint         length,
                                      gint        *position,
                                      gpointer     data);
static void set_auto_interest_box (AccountWindow *aw);
static void gnc_account_type_update (AccountWindow *aw);
static void gnc_finish_ok (AccountWindow *aw);
static void account_window_close (AccountWindow *aw);
static gboolean account_commodity_filter (Account *account, gpointer user_data);
static void account_parent_selection_changed_cb (GtkSelectionModel *selection,
                                                 guint position, guint n_items,
                                                 AccountWindow *aw);

/** Implementation *******************************************************/

static void
aw_call_destroy_callbacks (Account* acc)
{
    GList *node;
    void (*cb)(Account*);

    for (node = ac_destroy_cb_list; node; node = node->next)
    {
        cb = node->data;
        (cb)(acc);
    }
}

static Account *
aw_get_account (AccountWindow *aw)
{
    if (!aw)
        return NULL;

    return xaccAccountLookup (&aw->account, aw->book);
}

static void
aw_clear_selection_handler (AccountWindow *aw)
{
    if (aw->selection && aw->handler_id)
        g_signal_handler_disconnect (aw->selection, aw->handler_id);
    g_clear_object (&aw->selection);
    aw->handler_id = 0;
}

static void
aw_connect_selection_changed (AccountWindow *aw)
{
    aw_clear_selection_handler (aw);
    aw->selection = G_OBJECT (g_object_ref (
        gnc_tree_view_account_get_selection_model (
            GNC_TREE_VIEW_ACCOUNT (aw->parent_tree))));
    aw->handler_id = g_signal_connect (aw->selection, "selection-changed",
                                       G_CALLBACK (account_parent_selection_changed_cb), aw);
}

static void
gnc_account_commodity_from_type (AccountWindow * aw, gboolean update)
{
    dialog_commodity_mode new_mode;

    if (aw->type == ACCT_TYPE_TRADING)
        new_mode = DIAG_COMM_ALL;
    else if ((aw->type == ACCT_TYPE_STOCK) || (aw->type == ACCT_TYPE_MUTUAL))
        new_mode = DIAG_COMM_NON_CURRENCY_SELECT;
    else
        new_mode = DIAG_COMM_CURRENCY;

    if (update && (new_mode != aw->commodity_mode))
    {
        gnc_general_select_set_selected (GNC_GENERAL_SELECT(aw->commodity_edit),
                                         NULL);
    }
    aw->commodity_mode = new_mode;
}

static void
gnc_account_opening_balance_button_update (AccountWindow *aw, gnc_commodity *commodity)
{
    Account *account = aw_get_account (aw);
    Account *ob_account = gnc_account_lookup_by_opening_balance (gnc_book_get_root_account (aw->book), commodity);
    gboolean has_splits = (xaccAccountGetSplitsSize (account) != 0);

    if (aw->type != ACCT_TYPE_EQUITY)
    {
        gtk_widget_set_sensitive (aw->opening_balance_button, FALSE);
        return;
    }

    /* The opening balance flag can be edited, if the associated feature is enabled and
     * there is no opening balance account or we are editing the only opening balance account
     * and it has no splits assigned.
     */
    if (!gnc_using_equity_type_opening_balance_account (gnc_get_current_book()))
        return;

    switch (aw->dialog_type)
    {
    case EDIT_ACCOUNT:
        gtk_widget_set_sensitive (aw->opening_balance_button, (ob_account == NULL ||
                                                               ob_account == account) &&
                                                               has_splits == 0);
        break;
    case NEW_ACCOUNT:
        gtk_widget_set_sensitive (aw->opening_balance_button, ob_account == NULL);
        break;
    }
}

/* Copy the account values to the GUI widgets */
static void
gnc_account_to_ui (AccountWindow *aw)
{
    Account *account;
    gnc_commodity * commodity;
    const char *string;
    GdkRGBA color;
    gboolean flag, nonstd_scu;
    gint index;
    gnc_numeric balance_limit;
    gboolean    balance_limit_valid;

    ENTER("%p", aw);
    account = aw_get_account (aw);
    if (!account)
    {
        LEAVE("no account");
        return;
    }

    string = xaccAccountGetName (account);
    if (string == NULL)
        string = "";
    gnc_entry_set_text (GTK_ENTRY(aw->name_entry), string);

    string = xaccAccountGetDescription (account);
    if (string == NULL)
        string = "";
    gnc_entry_set_text (GTK_ENTRY(aw->description_entry), string);

    string = xaccAccountGetColor (account);

    if (!string)
        string = DEFAULT_COLOR;

    if (!gdk_rgba_parse (&color, string))
        gdk_rgba_parse (&color, DEFAULT_COLOR);

    gtk_color_dialog_button_set_rgba (aw->color_entry_button, &color);

    commodity = xaccAccountGetCommodity (account);
    gnc_general_select_set_selected (GNC_GENERAL_SELECT(aw->commodity_edit),
                                     commodity);
    gnc_account_commodity_from_type (aw, FALSE);

    nonstd_scu = xaccAccountGetNonStdSCU (account);
    if (nonstd_scu)
    {
        index = xaccAccountGetCommoditySCUi (account);
        index = log10 (index) + 1;
    }
    else
    {
        index = 0;
    }
    gtk_drop_down_set_selected (GTK_DROP_DOWN (aw->account_scu), index);

    string = xaccAccountGetCode (account);
    if (string == NULL)
        string = "";
    gnc_entry_set_text (GTK_ENTRY(aw->code_entry), string);

    string = xaccAccountGetNotes (account);
    if (string == NULL)
        string = "";

    gtk_text_buffer_set_text (aw->notes_text_buffer, string, strlen(string));

    gnc_account_opening_balance_button_update (aw, commodity);

    flag = xaccAccountGetIsOpeningBalance (account);
    gtk_check_button_set_active (GTK_CHECK_BUTTON(aw->opening_balance_button),
                                  flag);

    flag = xaccAccountGetTaxRelated (account);
    gtk_check_button_set_active (GTK_CHECK_BUTTON(aw->tax_related_button),
                                  flag);

    flag = xaccAccountGetPlaceholder (account);
    gtk_check_button_set_active (GTK_CHECK_BUTTON(aw->placeholder_button),
                                  flag);

    flag = xaccAccountGetHidden (account);
    gtk_check_button_set_active (GTK_CHECK_BUTTON(aw->hidden_button),
                                  flag);

    aw->balance_is_reversed = gnc_reverse_balance (account);

    flag = xaccAccountGetIncludeSubAccountBalances (account);

    gtk_check_button_set_active (GTK_CHECK_BUTTON(aw->include_balance_sub_accts),
                                  flag);

    balance_limit_valid = xaccAccountGetHigherBalanceLimit (account, &balance_limit);
    if (balance_limit_valid)
    {
        if (aw->balance_is_reversed)
        {
            balance_limit = gnc_numeric_neg (balance_limit);
            gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT(aw->lower_balance_limit_edit),
                                                        balance_limit);
        }
        else
            gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT(aw->higher_balance_limit_edit),
                                                        balance_limit);
    }

    balance_limit_valid = xaccAccountGetLowerBalanceLimit (account, &balance_limit);
    if (balance_limit_valid)
    {
        if (aw->balance_is_reversed)
        {
            balance_limit = gnc_numeric_neg (balance_limit);
            gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT(aw->higher_balance_limit_edit),
                                                        balance_limit);
        }
        else
            gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT(aw->lower_balance_limit_edit),
                                                        balance_limit);
    }

    set_auto_interest_box (aw);
    LEAVE(" ");
}

static gboolean
gnc_account_create_transfer_balance (QofBook *book,
                                     Account *account,
                                     Account *transfer,
                                     gnc_numeric balance,
                                     time64 date)
{
    Transaction *trans;
    Split *split;

    if (gnc_numeric_zero_p (balance))
        return TRUE;

    g_return_val_if_fail (account != NULL, FALSE);
    g_return_val_if_fail (transfer != NULL, FALSE);

    xaccAccountBeginEdit (account);
    xaccAccountBeginEdit (transfer);

    trans = xaccMallocTransaction (book);

    xaccTransBeginEdit (trans);

    xaccTransSetCurrency (trans, gnc_account_or_default_currency (account, NULL));
    xaccTransSetDatePostedSecsNormalized (trans, date);
    xaccTransSetDescription (trans, _("Opening Balance"));

    split = xaccMallocSplit (book);

    xaccTransAppendSplit (trans, split);
    xaccAccountInsertSplit (account, split);

    xaccSplitSetAmount (split, balance);
    xaccSplitSetValue (split, balance);

    balance = gnc_numeric_neg (balance);

    split = xaccMallocSplit (book);

    xaccTransAppendSplit (trans, split);
    xaccAccountInsertSplit (transfer, split);

    xaccSplitSetAmount (split, balance);
    xaccSplitSetValue (split, balance);

    xaccTransCommitEdit (trans);
    xaccAccountCommitEdit (transfer);
    xaccAccountCommitEdit (account);

    return TRUE;
}

/* Record the GUI values into the Account structure */
static void
gnc_ui_to_account (AccountWindow *aw)
{
    Account *account;
    gnc_commodity *commodity;
    Account *parent_account;
    const char *old_string;
    const char *string;
    const GdkRGBA *color;
    gboolean flag;
    gnc_numeric balance;
    gnc_numeric balance_limit;
    gint higher_balance_limit_valid;
    gint lower_balance_limit_valid;
    gboolean use_equity, nonstd;
    time64 date;
    gint index, old_scu, new_scu;
    GtkTextIter start, end;

    account = aw_get_account (aw);
    if (!account)
    {
        LEAVE("no account");
        return;
    }

    if (aw->dialog_type == EDIT_ACCOUNT
            && aw->type != xaccAccountGetType (account))
    {
        /* Just refreshing won't work. */
        aw_call_destroy_callbacks (account);
    }

    xaccAccountBeginEdit (account);

    if (aw->type != xaccAccountGetType (account))
        xaccAccountSetType (account, aw->type);

    last_used_account_type = aw->type;

    string = gnc_entry_get_text (GTK_ENTRY(aw->name_entry));
    old_string = xaccAccountGetName (account);
    if (g_strcmp0 (string, old_string) != 0)
        xaccAccountSetName (account, string);

    string = gnc_entry_get_text (GTK_ENTRY(aw->description_entry));
    old_string = xaccAccountGetDescription (account);
    if (g_strcmp0 (string, old_string) != 0)
        xaccAccountSetDescription (account, string);

    color = gtk_color_dialog_button_get_rgba (aw->color_entry_button);
    char* new_string = gdk_rgba_to_string (color);
    if (!g_strcmp0 (new_string, DEFAULT_COLOR))
    {
        g_free(new_string);
        new_string = NULL;
    }

    old_string = xaccAccountGetColor (account);

    if (!g_strcmp0 (new_string, DEFAULT_COLOR) && old_string)
        xaccAccountSetColor (account, ""); // remove entry
    else
    {
        if (g_strcmp0 (new_string, old_string) != 0)
            xaccAccountSetColor (account, new_string); // update entry
    }
    g_free (new_string);

    commodity = (gnc_commodity *)
                gnc_general_select_get_selected (GNC_GENERAL_SELECT(aw->commodity_edit));
    if (commodity &&
            !gnc_commodity_equiv (commodity, xaccAccountGetCommodity (account)))
    {
        xaccAccountSetCommodity (account, commodity);
        old_scu = 0;
    }
    else
    {
        old_scu = xaccAccountGetCommoditySCU (account);
    }

    index = gtk_drop_down_get_selected (GTK_DROP_DOWN (aw->account_scu));
    nonstd = (index != 0);
    if (nonstd != xaccAccountGetNonStdSCU (account))
        xaccAccountSetNonStdSCU (account, nonstd);
    new_scu = (nonstd ? pow (10, index - 1) : gnc_commodity_get_fraction (commodity));
    if (old_scu != new_scu)
        xaccAccountSetCommoditySCU (account, new_scu);

    string = gnc_entry_get_text (GTK_ENTRY(aw->code_entry));
    old_string = xaccAccountGetCode (account);
    if (g_strcmp0 (string, old_string) != 0)
        xaccAccountSetCode (account, string);

    gtk_text_buffer_get_start_iter (aw->notes_text_buffer, &start);
    gtk_text_buffer_get_end_iter (aw->notes_text_buffer, &end);
    new_string = gtk_text_buffer_get_text (aw->notes_text_buffer, &start, &end,
                                           FALSE);
    old_string = xaccAccountGetNotes (account);
    if (g_strcmp0 (new_string, old_string))
        xaccAccountSetNotes (account, new_string);
    g_free (new_string);

    flag = gtk_check_button_get_active (GTK_CHECK_BUTTON(aw->opening_balance_button));
    if (xaccAccountGetIsOpeningBalance (account) != flag)
        xaccAccountSetIsOpeningBalance (account, flag);

    flag = gtk_check_button_get_active (GTK_CHECK_BUTTON(aw->tax_related_button));
    if (xaccAccountGetTaxRelated (account) != flag)
        xaccAccountSetTaxRelated (account, flag);

    flag = gtk_check_button_get_active (GTK_CHECK_BUTTON(aw->placeholder_button));
    if (xaccAccountGetPlaceholder (account) != flag)
        xaccAccountSetPlaceholder (account, flag);

    flag = gtk_check_button_get_active (GTK_CHECK_BUTTON(aw->hidden_button));
    if (xaccAccountGetHidden (account) != flag)
        xaccAccountSetHidden (account, flag);

    flag = gtk_check_button_get_active (GTK_CHECK_BUTTON(aw->auto_interest_button));
    if (xaccAccountGetAutoInterest (account) != flag)
        xaccAccountSetAutoInterest (account, flag);

    parent_account = gnc_tree_view_account_get_selected_account (GNC_TREE_VIEW_ACCOUNT(aw->parent_tree));

    if (parent_account == NULL)
        parent_account = gnc_book_get_root_account (aw->book);
    if (parent_account != gnc_account_get_parent (account))
        gnc_account_append_child (parent_account, account);

    flag = gtk_check_button_get_active (GTK_CHECK_BUTTON(
                                         aw->include_balance_sub_accts));

    xaccAccountSetIncludeSubAccountBalances (account, flag);

    higher_balance_limit_valid = gnc_amount_edit_expr_is_valid (GNC_AMOUNT_EDIT(
                                                                aw->higher_balance_limit_edit),
                                                                &balance_limit, TRUE, NULL);

    if (higher_balance_limit_valid == 0)
    {
        if (aw->balance_is_reversed)
        {
            balance_limit = gnc_numeric_neg (balance_limit);
            xaccAccountSetLowerBalanceLimit (account, balance_limit);
        }
        else
            xaccAccountSetHigherBalanceLimit (account, balance_limit);
    }

    if (higher_balance_limit_valid == -1)
    {
        if (aw->balance_is_reversed)
            xaccAccountClearLowerBalanceLimit (account);
        else
            xaccAccountClearHigherBalanceLimit (account);
    }

    lower_balance_limit_valid = gnc_amount_edit_expr_is_valid (GNC_AMOUNT_EDIT(
                                                               aw->lower_balance_limit_edit),
                                                               &balance_limit, TRUE, NULL);

    if (lower_balance_limit_valid == 0)
    {
        if (aw->balance_is_reversed)
        {
            balance_limit = gnc_numeric_neg (balance_limit);
            xaccAccountSetHigherBalanceLimit (account, balance_limit);
        }
        else
            xaccAccountSetLowerBalanceLimit (account, balance_limit);
    }

    if (lower_balance_limit_valid == -1)
    {
        if (aw->balance_is_reversed)
            xaccAccountClearHigherBalanceLimit (account);
        else
            xaccAccountClearLowerBalanceLimit (account);
    }

    if ((higher_balance_limit_valid == -1) && (lower_balance_limit_valid == -1))
        xaccAccountSetIncludeSubAccountBalances (account, FALSE);

    xaccAccountCommitEdit (account);

    balance = gnc_amount_edit_get_amount (GNC_AMOUNT_EDIT(aw->opening_balance_edit));

    if (gnc_numeric_zero_p (balance))
    {
        LEAVE("zero balance");
        return;
    }

    if (gnc_reverse_balance (account))
        balance = gnc_numeric_neg (balance);

    date = gnc_date_edit_get_date (GNC_DATE_EDIT(aw->opening_balance_date_edit));

    use_equity = gtk_check_button_get_active (GTK_CHECK_BUTTON(aw->opening_equity_radio));

    if (use_equity)
    {
        if (!gnc_account_create_opening_balance (account, balance, date, aw->book))
        {
            const char *message = _("Could not create opening balance.");
            gnc_error_dialog (GTK_WINDOW(aw->dialog), "%s", message);
        }
    }
    else
    {
        Account *transfer = NULL;

        transfer = gnc_tree_view_account_get_selected_account (GNC_TREE_VIEW_ACCOUNT(aw->transfer_tree));
        if (!transfer)
        {
            LEAVE("no transfer account");
            return;
        }

        gnc_account_create_transfer_balance (aw->book, account, transfer, balance, date);
    }
    LEAVE(" ");
}

static void
set_children_types (Account *account, GNCAccountType type)
{
    GList *children, *iter;

    children = gnc_account_get_children (account);
    if (children == NULL)
        return;

    for (iter = children; iter; iter = iter->next)
    {
        account = iter->data;
        if (type == xaccAccountGetType (account))
            continue;

        /* Just refreshing won't work. */
        aw_call_destroy_callbacks (account);

        xaccAccountBeginEdit (account);
        xaccAccountSetType (account, type);
        xaccAccountCommitEdit (account);

        set_children_types (account, type);
    }
    g_list_free (children);
}

static void
make_children_compatible (AccountWindow *aw)
{
    Account *account;

    g_return_if_fail (aw);

    if (aw->dialog_type == NEW_ACCOUNT)
        return;

    account = aw_get_account (aw);
    g_return_if_fail (account);

    if (xaccAccountTypesCompatible (aw->type, xaccAccountGetType (account)))
        return;

    set_children_types (account, aw->type);
}

static void
gnc_finish_ok (AccountWindow *aw)
{
    gboolean operation_held = FALSE;

    ENTER("aw %p", aw);
    if (aw->operation_context)
    {
        operation_held = gnc_session_operation_context_begin (
            aw->operation_context);
        if (!operation_held)
        {
            account_window_close (aw);
            return;
        }
    }
    gnc_suspend_gui_refresh ();

    /* make the account changes */
    make_children_compatible (aw);
    gnc_ui_to_account (aw);

    gnc_resume_gui_refresh ();

    /* do it all again, if needed */
    if ((aw->dialog_type == NEW_ACCOUNT) && aw->next_name && *aw->next_name)
    {
        gnc_commodity *commodity;
        Account *parent;
        Account *account;

        /* Drop the old parent_tree so we can update it with an up to date one */
        aw_clear_selection_handler (aw);
        gtk_scrolled_window_set_child (GTK_SCROLLED_WINDOW (aw->parent_scroll), NULL);
        aw->parent_tree = gnc_tree_view_account_new (TRUE);
        gtk_scrolled_window_set_child (GTK_SCROLLED_WINDOW (aw->parent_scroll),
                                       GTK_WIDGET (aw->parent_tree));
        gtk_widget_set_visible (GTK_WIDGET(aw->parent_tree), TRUE);

        aw_connect_selection_changed (aw);
        gnc_suspend_gui_refresh ();

        parent = aw_get_account (aw);
        account = xaccMallocAccount (aw->book);
        aw->account = *xaccAccountGetGUID (account);
        aw->type = xaccAccountGetType (parent);

        xaccAccountSetName (account, *aw->next_name);
        aw->next_name++;

        gnc_account_to_ui (aw);

        gnc_account_window_set_name (aw);

        commodity = xaccAccountGetCommodity (parent);
        gnc_general_select_set_selected (GNC_GENERAL_SELECT(aw->commodity_edit),
                                         commodity);
        gnc_account_commodity_from_type (aw, FALSE);

        gnc_tree_view_account_set_selected_account (GNC_TREE_VIEW_ACCOUNT(
                                                    aw->parent_tree),
                                                    parent);

        gnc_resume_gui_refresh ();
        LEAVE("1");
        if (operation_held)
            gnc_session_operation_context_end (aw->operation_context);
        return;
    }

    /* Save the account identity before the asynchronous completion can run.
     * The completion itself looks it up again in the still-current book. */
    aw->created_account = aw_get_account (aw);
    if (aw->created_account)
        aw->created_account_guid = *xaccAccountGetGUID (aw->created_account);

    /* so it doesn't get freed on close */
    aw->account = *guid_null ();

    if (operation_held)
        gnc_session_operation_context_end (aw->operation_context);
    gnc_close_gui_component (aw->component_id);
    LEAVE("2");
}

typedef struct
{
    GWeakRef account_window;
    GtkWindow *dialog;
} AccountTypeConfirmation;

#define ACCOUNT_TYPE_CONFIRMATION_DATA "gnc-account-type-confirmation"

static void
account_type_confirmation_destroy_cb (GtkWidget *object,
                                      AccountTypeConfirmation *confirmation)
{
    if (!confirmation)
        return;
    g_object_set_data (G_OBJECT (object), ACCOUNT_TYPE_CONFIRMATION_DATA, NULL);
    if (confirmation->dialog == GTK_WINDOW (object))
        confirmation->dialog = NULL;
    g_weak_ref_clear (&confirmation->account_window);
    g_free (confirmation);
}

static void
account_type_confirmation_close (AccountTypeConfirmation *confirmation)
{
    GtkWindow *dialog;

    if (!confirmation || !confirmation->dialog)
        return;

    dialog = g_steal_pointer (&confirmation->dialog);
    gtk_window_destroy (dialog);
    g_object_unref (dialog);
}

static void
account_type_confirmation_parent_destroy_cb (GtkWindow *dialog)
{
    account_type_confirmation_close (
        g_object_get_data (G_OBJECT (dialog), ACCOUNT_TYPE_CONFIRMATION_DATA));
}

static void
account_type_confirmation_apply_cb (GtkButton *button,
                                    AccountTypeConfirmation *confirmation)
{
    GtkWindow *window = GTK_WINDOW (g_weak_ref_get (&confirmation->account_window));
    AccountWindow *aw = window ? g_object_get_data (G_OBJECT (window), "dialog_info") : NULL;

    if (aw && !aw->closing && aw->book == gnc_get_current_book () && aw_get_account (aw))
    {
        account_type_confirmation_close (confirmation);
        gnc_finish_ok (aw);
    }
    else
        account_type_confirmation_close (confirmation);
    g_clear_object (&window);
    (void)button;
}

static void
account_type_confirmation_cancel_cb (GtkButton *button,
                                     AccountTypeConfirmation *confirmation)
{
    account_type_confirmation_close (confirmation);
    (void)button;
}

static gboolean
account_type_confirmation_close_request_cb (GtkWindow *window,
                                            AccountTypeConfirmation *confirmation)
{
    account_type_confirmation_close (confirmation);
    (void)window;
    return TRUE;
}

/* Check whether the children need a type adjustment after an incompatible
 * account-type change. The answer is asynchronous so the account window never
 * enters a nested event loop. */
static gboolean
verify_children_compatible (AccountWindow *aw)
{
    Account *account;
    AccountTypeConfirmation *confirmation;
    GtkWidget *content, *row, *text, *image, *expander, *scrolled, *view;
    GtkWidget *actions, *spacer, *cancel, *apply;
    gchar *detail;

    if (!aw || aw->closing || !aw->dialog)
        return FALSE;
    account = aw_get_account (aw);
    if (!account)
        return FALSE;
    if (xaccAccountTypesCompatible (aw->type, xaccAccountGetType (account)) ||
        gnc_account_n_children (account) == 0)
        return TRUE;

    confirmation = g_new0 (AccountTypeConfirmation, 1);
    g_weak_ref_init (&confirmation->account_window, G_OBJECT (aw->dialog));
    confirmation->dialog = GTK_WINDOW (gtk_window_new ());
    gnc_window_bind_to_application (confirmation->dialog);
    g_object_set_data (G_OBJECT (confirmation->dialog), ACCOUNT_TYPE_CONFIRMATION_DATA,
                       confirmation);
    gtk_window_set_title (confirmation->dialog, _("Give the children the same type?"));
    gtk_window_set_modal (confirmation->dialog, TRUE);
    gtk_window_set_transient_for (confirmation->dialog, aw->dialog);
    gtk_window_set_default_size (confirmation->dialog, 500, 360);

    content = gtk_box_new (GTK_ORIENTATION_VERTICAL, 12);
    gnc_box_set_all_margins (GTK_BOX (content), 12);
    gtk_window_set_child (confirmation->dialog, content);
    row = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 12);
    gtk_box_append (GTK_BOX (content), row);
    image = gtk_image_new_from_icon_name ("dialog-information");
    gtk_image_set_icon_size (GTK_IMAGE (image), GTK_ICON_SIZE_LARGE);
    gtk_box_append (GTK_BOX (row), image);
    text = gtk_box_new (GTK_ORIENTATION_VERTICAL, 6);
    gtk_widget_set_hexpand (text, TRUE);
    gtk_box_append (GTK_BOX (row), text);
    view = gtk_label_new (_("Give the children the same type?"));
    gnc_widget_style_context_add_class (view, "gnc-class-title");
    gtk_label_set_wrap (GTK_LABEL (view), TRUE);
    gnc_label_set_alignment (view, 0.0, 0.0);
    gtk_box_append (GTK_BOX (text), view);
    detail = g_strdup_printf (_("The children of the edited account have to be "
                               "changed to type \"%s\" to make them compatible."),
                              xaccAccountGetTypeStr (aw->type));
    view = gtk_label_new (detail);
    gtk_label_set_wrap (GTK_LABEL (view), TRUE);
    gnc_label_set_alignment (view, 0.0, 0.0);
    gtk_box_append (GTK_BOX (text), view);
    g_free (detail);

    expander = gtk_expander_new_with_mnemonic (_("_Show children accounts"));
    scrolled = gtk_scrolled_window_new ();
    gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled),
                                    GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
    gtk_widget_set_vexpand (scrolled, TRUE);
    gtk_widget_set_size_request (scrolled, -1, 180);
    view = gnc_tree_view_account_new_with_root (account, FALSE);
    gtk_scrolled_window_set_child (GTK_SCROLLED_WINDOW (scrolled), view);
    gtk_expander_set_child (GTK_EXPANDER (expander), scrolled);
    gtk_box_append (GTK_BOX (content), expander);

    actions = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 6);
    spacer = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
    gtk_widget_set_hexpand (spacer, TRUE);
    gtk_box_append (GTK_BOX (actions), spacer);
    cancel = gtk_button_new_with_mnemonic (_("_Cancel"));
    apply = gtk_button_new_with_mnemonic (_("_OK"));
    gtk_box_append (GTK_BOX (actions), cancel);
    gtk_box_append (GTK_BOX (actions), apply);
    gtk_box_append (GTK_BOX (content), actions);
    gtk_window_set_default_widget (confirmation->dialog, apply);
    g_signal_connect (apply, "clicked", G_CALLBACK (account_type_confirmation_apply_cb),
                      confirmation);
    g_signal_connect (cancel, "clicked", G_CALLBACK (account_type_confirmation_cancel_cb),
                      confirmation);
    g_signal_connect (confirmation->dialog, "close-request",
                      G_CALLBACK (account_type_confirmation_close_request_cb), confirmation);
    g_signal_connect (confirmation->dialog, "destroy",
                      G_CALLBACK (account_type_confirmation_destroy_cb), confirmation);
    g_signal_connect_object (aw->dialog, "destroy",
                             G_CALLBACK (account_type_confirmation_parent_destroy_cb),
                             confirmation->dialog, G_CONNECT_SWAPPED);
    gtk_window_present (confirmation->dialog);
    return FALSE;
}
static gboolean
gnc_filter_parent_accounts (Account *account, gpointer data)
{
    AccountWindow *aw = data;
    Account *aw_account = aw_get_account (aw);

    if (account == NULL)
        return FALSE;

    if (aw_account == NULL)
        return FALSE;

    if (gnc_account_is_root (account))
        return TRUE;

    if (account == aw_account)
        return FALSE;

    if (xaccAccountHasAncestor (account, aw_account))
        return FALSE;

    return TRUE;
}

static gboolean
gnc_common_ok (AccountWindow *aw)
{
    Account *root, *account, *parent;
    gnc_commodity * commodity;
    gchar *fullname, *fullname_parent;
    const gchar *name, *separator;
    gboolean higher_limit_valid;
    gnc_numeric higher_balance_limit;
    gboolean lower_limit_valid;
    gnc_numeric lower_balance_limit;

    ENTER("aw %p", aw);
    root = gnc_book_get_root_account (aw->book);

    separator = gnc_get_account_separator_string ();

    /* check for valid name */
    name = gnc_entry_get_text (GTK_ENTRY(aw->name_entry));
    if (g_strcmp0 (name, "") == 0)
    {
        const char *message = _("The account must be given a name.");
        gnc_error_dialog (GTK_WINDOW(aw->dialog), "%s", message);
        LEAVE("bad name");
        return FALSE;
    }

    /* check for a duplicate name */
    parent = gnc_tree_view_account_get_selected_account
             (GNC_TREE_VIEW_ACCOUNT(aw->parent_tree));
    if (parent == NULL)
    {
        account = gnc_account_lookup_by_full_name (root, name);
    }
    else
    {
        fullname_parent = gnc_account_get_full_name (parent);
        fullname = g_strconcat (fullname_parent, separator, name, NULL);

        account = gnc_account_lookup_by_full_name (root, fullname);

        g_free (fullname_parent);
        g_free (fullname);
    }
    if ((account != NULL) &&
            !guid_equal (&aw->account, xaccAccountGetGUID (account)))
    {
        const char *message = _("There is already an account with that name.");
        gnc_error_dialog (GTK_WINDOW(aw->dialog), "%s", message);
        LEAVE("duplicate name");
        return FALSE;
    }

    /* Parent check, probably not needed, but be safe */
    if (!gnc_filter_parent_accounts (parent, aw))
    {
        const char *message = _("You must choose a valid parent account.");
        gnc_error_dialog (GTK_WINDOW(aw->dialog), "%s", message);
        LEAVE("invalid parent");
        return FALSE;
    }

    /* check for valid type */
    if (aw->type == ACCT_TYPE_INVALID)
    {
        const char *message = _("You must select an account type.");
        gnc_error_dialog (GTK_WINDOW(aw->dialog), "%s", message);
        LEAVE("invalid type");
        return FALSE;
    }

    /* check whether the types of child and parent are compatible */
    if (!xaccAccountTypesCompatible (xaccAccountGetType (parent), aw->type))
    {
        const char *message = _("The selected account type is incompatible with "
                                "the one of the selected parent.");
        gnc_error_dialog (GTK_WINDOW(aw->dialog), "%s", message);
        LEAVE("incompatible types");
        return FALSE;
    }

    /* check for commodity */
    commodity = (gnc_commodity *)
                gnc_general_select_get_selected (GNC_GENERAL_SELECT(aw->commodity_edit));
    if (!commodity)
    {
        const char *message = _("You must choose a commodity.");
        gnc_error_dialog (GTK_WINDOW(aw->dialog), "%s", message);
        LEAVE("invalid commodity");
        return FALSE;
    }

    /* check for higher balance limit greater than lower */
    higher_limit_valid = gnc_amount_edit_expr_is_valid (GNC_AMOUNT_EDIT(aw->higher_balance_limit_edit),
                                                        &higher_balance_limit, TRUE, NULL);

    lower_limit_valid = gnc_amount_edit_expr_is_valid (GNC_AMOUNT_EDIT(aw->lower_balance_limit_edit),
                                                       &lower_balance_limit, TRUE, NULL);

    if ((lower_limit_valid == 0) && (higher_limit_valid == 0))
    {
        gint compare = gnc_numeric_compare (higher_balance_limit,
                                            lower_balance_limit);

        if ((compare == 0) && (!gnc_numeric_zero_p (higher_balance_limit)))
        {
            const char *message = _("Balance limits must be different unless they are both zero.");
            gnc_error_dialog (GTK_WINDOW(aw->dialog), "%s", message);
            LEAVE("invalid balance limit, both the same but not zero");
            return FALSE;
        }
        else if (compare == -1)
        {
            const char *message = _("The lower balance limit must be less than the higher limit.");
            gnc_error_dialog (GTK_WINDOW(aw->dialog), "%s", message);
            LEAVE("invalid balance limit, lower limit not less than upper");
            return FALSE;
        }
    }

    LEAVE("passed");
    return TRUE;
}

static void
gnc_edit_account_ok (AccountWindow *aw)
{
    Account *account;

    ENTER("aw %p", aw);

    account = aw_get_account (aw);
    if (!account)
    {
        LEAVE(" ");
        return;
    }

    if (!gnc_common_ok (aw))
    {
        LEAVE(" ");
        return;
    }

    if (!verify_children_compatible (aw))
    {
        LEAVE(" ");
        return;
    }

    gnc_finish_ok (aw);
    LEAVE(" ");
}

static void
gnc_new_account_ok (AccountWindow *aw)
{
    gnc_numeric balance;

    ENTER("aw %p", aw);

    if (!gnc_common_ok (aw))
    {
        LEAVE(" ");
        return;
    }

    if (!gnc_amount_edit_evaluate (GNC_AMOUNT_EDIT(aw->opening_balance_edit), NULL))
    {
        const char *message = _("You must enter a valid opening balance "
                                "or leave it blank.");
        gnc_error_dialog (GTK_WINDOW(aw->dialog), "%s", message);
        LEAVE(" ");
        return;
    }

    balance = gnc_amount_edit_get_amount (GNC_AMOUNT_EDIT(aw->opening_balance_edit));

    if (!gnc_numeric_zero_p (balance))
    {
        gboolean use_equity;

        use_equity = gtk_check_button_get_active
                     (GTK_CHECK_BUTTON(aw->opening_equity_radio));

        if (!use_equity)
        {
            Account *transfer = NULL;

            transfer = gnc_tree_view_account_get_selected_account (GNC_TREE_VIEW_ACCOUNT(
                                                                   aw->transfer_tree));
            if (!transfer)
            {
                const char *message = _("You must select a transfer account or choose"
                                        " the opening balances equity account.");
                gnc_error_dialog (GTK_WINDOW(aw->dialog), "%s", message);
                LEAVE(" ");
                return;
            }
        }
    }

    gnc_finish_ok (aw);
    LEAVE(" ");
}

static void
account_window_close (AccountWindow *aw)
{
    if (!aw || aw->closing)
        return;
    aw->closing = TRUE;
    if (aw->component_id)
        gnc_close_gui_component (aw->component_id);
    else if (aw->dialog)
        gtk_window_destroy (aw->dialog);
}

static void
gnc_account_window_ok_cb (GtkButton *button, AccountWindow *aw)
{
    if (!aw || aw->closing)
        return;
    if (aw->operation_context &&
        !gnc_session_operation_context_is_current (aw->operation_context))
    {
        account_window_close (aw);
        return;
    }
    switch (aw->dialog_type)
    {
    case NEW_ACCOUNT:
        gnc_new_account_ok (aw);
        break;
    case EDIT_ACCOUNT:
        gnc_edit_account_ok (aw);
        break;
    default:
        g_assert_not_reached ();
    }
    (void)button;
}

static void
gnc_account_window_cancel_cb (GtkButton *button, AccountWindow *aw)
{
    account_window_close (aw);
    (void)button;
}

static void
gnc_account_window_help_cb (GtkButton *button, AccountWindow *aw)
{
    if (!aw || aw->closing || !aw->dialog)
        return;
    gnc_gnome_help (aw->dialog, DF_MANUAL,
                    aw->dialog_type == NEW_ACCOUNT ? DL_ACC : DL_ACCEDIT);
    (void)button;
}

static gboolean
gnc_account_window_close_request_cb (GtkWindow *window, AccountWindow *aw)
{
    account_window_close (aw);
    (void)window;
    return TRUE;
}
void
gnc_account_window_destroy_cb (GtkWidget *object, gpointer data)
{
    AccountWindow *aw = data;
    Account *account = NULL;
    gboolean operation_held = FALSE;

    ENTER("object %p, aw %p", object, aw);
    aw->closing = TRUE;
    if (aw->operation_context &&
        !guid_equal (&aw->account, guid_null ()))
    {
        operation_held = gnc_session_operation_context_begin (
            aw->operation_context);
        if (!operation_held)
            operation_held = gnc_session_operation_context_begin_cleanup (
                aw->operation_context);
    }
    if (!aw->operation_context || operation_held ||
        guid_equal (&aw->account, guid_null ()))
        account = aw_get_account (aw);

    aw_clear_selection_handler (aw);
    gnc_suspend_gui_refresh ();

    switch (aw->dialog_type)
    {
    case NEW_ACCOUNT:
        if (account != NULL)
        {
            xaccAccountBeginEdit (account);
            xaccAccountDestroy (account);
            aw->account = *guid_null ();
        }

        DEBUG ("account add window destroyed\n");
        break;

    case EDIT_ACCOUNT:
        break;

    default:
        PERR ("unexpected dialog type\n");
        break;
    }

    if (aw->component_id)
        gnc_unregister_gui_component (aw->component_id);

    gnc_resume_gui_refresh ();
    if (operation_held)
        gnc_session_operation_context_end (aw->operation_context);

    if (aw->creation_callback)
    {
        Account *created_account = NULL;

        if ((!aw->operation_context ||
             gnc_session_operation_context_is_current (
                 aw->operation_context)) &&
            aw->book == gnc_get_current_book () &&
            !guid_equal (&aw->created_account_guid, guid_null ()))
        {
            created_account = xaccAccountLookup (&aw->created_account_guid, aw->book);
            if (created_account &&
                qof_instance_get_destroying (QOF_INSTANCE (created_account)))
                created_account = NULL;
        }
        aw->creation_callback (created_account, created_account != NULL,
                               aw->creation_callback_data);
    }

    if (aw->subaccount_names)
    {
        g_strfreev (aw->subaccount_names);
        aw->subaccount_names = NULL;
        aw->next_name = NULL;
    }

    gnc_session_operation_context_unref (aw->operation_context);
    aw->dialog = NULL;
    g_free (aw);
    LEAVE(" ");
}

static gboolean
account_type_mask_contains (guint32 types, GNCAccountType type)
{
    return type > ACCT_TYPE_NONE && type < NUM_ACCOUNT_TYPES &&
           (types & (1u << type)) != 0;
}

static guint
account_type_dropdown_find (GListModel *model, GNCAccountType type)
{
    guint count = g_list_model_get_n_items (model);

    for (guint position = 0; position < count; position++)
    {
        GncAccountTypeItem *item = GNC_ACCOUNT_TYPE_ITEM (
            g_list_model_get_item (model, position));
        gboolean found = gnc_account_type_item_get_account_type (item) == type;

        g_object_unref (item);
        if (found)
            return position;
    }
    return GTK_INVALID_LIST_POSITION;
}

static void
account_type_dropdown_set_model (AccountWindow *aw, guint32 types)
{
    GListModel *model = gnc_account_type_list_new (types);
    guint selected = account_type_dropdown_find (model, aw->type);

    if (selected == GTK_INVALID_LIST_POSITION &&
        g_list_model_get_n_items (model) != 0)
    {
        g_object_unref (model);
        model = gnc_account_type_list_new_with_placeholder (types);
        selected = 0;
    }

    aw->updating_type_dropdown = TRUE;
    gtk_drop_down_set_model (GTK_DROP_DOWN (aw->type_combo), model);
    gtk_drop_down_set_selected (GTK_DROP_DOWN (aw->type_combo), selected);
    aw->updating_type_dropdown = FALSE;
    aw->displayed_types = types;
    g_object_unref (model);
}

static void
gnc_account_parent_changed_cb (GObject *selection, gpointer data)
{
    AccountWindow *aw = data;
    Account *parent_account;
    guint32 types;
    gboolean type_changed = FALSE;

    g_return_if_fail (aw);
    g_return_if_fail (selection == aw->selection);

    parent_account = gnc_tree_view_account_get_selected_account (
                         GNC_TREE_VIEW_ACCOUNT (aw->parent_tree));
    if (!parent_account)
        return;

    if (gnc_account_is_root (parent_account))
        types = aw->valid_types;
    else
        types = aw->valid_types & xaccParentAccountTypesCompatibleWith (
            xaccAccountGetType (parent_account));

    if (aw->type != aw->preferred_account_type &&
        account_type_mask_contains (types, aw->preferred_account_type))
    {
        aw->type = aw->preferred_account_type;
        type_changed = TRUE;
    }
    else if (!account_type_mask_contains (types, aw->type))
    {
        aw->type = ACCT_TYPE_INVALID;
        type_changed = TRUE;
    }

    if (type_changed || aw->displayed_types != types)
        account_type_dropdown_set_model (aw, types);

    if (type_changed)
        gnc_account_type_update (aw);

    gnc_account_window_set_name (aw);
}

static void
account_scu_dropdown_setup (AccountWindow *aw)
{
    const gchar *fractions[] =
    {
        _("Use Commodity Value"),
        "1",
        "1/10",
        "1/100",
        "1/1000",
        "1/10000",
        "1/100000",
        "1/1000000",
        "1/10000000",
        "1/100000000",
        "1/1000000000",
        NULL
    };
    GtkStringList *model = gtk_string_list_new (fractions);

    gtk_drop_down_set_model (GTK_DROP_DOWN (aw->account_scu),
                             G_LIST_MODEL (model));
    gtk_drop_down_set_selected (GTK_DROP_DOWN (aw->account_scu), 0);
    g_object_unref (model);
}
static void
set_auto_interest_box (AccountWindow *aw)
{
    Account* account = aw_get_account (aw);
    gboolean type_ok = account_type_has_auto_interest_xfer (aw->type);
    gboolean pref_set = xaccAccountGetAutoInterest (account);

    gtk_check_button_set_active (GTK_CHECK_BUTTON (aw->auto_interest_button),
                                  type_ok && pref_set);
    gtk_widget_set_sensitive (aw->auto_interest_button, type_ok);
}

static void
gnc_account_type_update (AccountWindow *aw)
{
    gboolean sensitive = FALSE;

    if (aw->type == ACCT_TYPE_NONE || aw->type == ACCT_TYPE_INVALID)
    {
        aw->type = ACCT_TYPE_INVALID;
    }
    else
    {
        aw->preferred_account_type = aw->type;
        gnc_account_commodity_from_type (aw, TRUE);
        sensitive = aw->type != ACCT_TYPE_EQUITY &&
                    aw->type != ACCT_TYPE_CURRENCY &&
                    aw->type != ACCT_TYPE_STOCK &&
                    aw->type != ACCT_TYPE_MUTUAL &&
                    aw->type != ACCT_TYPE_TRADING;
    }

    gtk_widget_set_sensitive (aw->opening_balance_page, sensitive);
    if (!sensitive)
    {
        gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (aw->opening_balance_edit),
                                    gnc_numeric_zero ());
    }
    set_auto_interest_box (aw);
}

static void
gnc_account_type_dropdown_changed_cb (GtkDropDown *dropdown,
                                      GParamSpec *pspec,
                                      gpointer data)
{
    AccountWindow *aw = data;
    GncAccountTypeItem *item;

    g_return_if_fail (aw);
    if (aw->updating_type_dropdown)
        return;

    item = GNC_ACCOUNT_TYPE_ITEM (gtk_drop_down_get_selected_item (dropdown));
    aw->type = item ? gnc_account_type_item_get_account_type (item) :
               ACCT_TYPE_INVALID;
    gnc_account_type_update (aw);

    (void)pspec;
}

static void
gnc_account_type_view_create (AccountWindow *aw, guint32 compat_types)
{
    GtkExpression *expression;

    aw->valid_types &= compat_types;
    if (aw->valid_types == 0)
    {
        /* No type restrictions: keep the account's current type visible. */
        aw->valid_types = compat_types;
        if (aw->type > ACCT_TYPE_NONE && aw->type < NUM_ACCOUNT_TYPES)
            aw->valid_types |= 1u << aw->type;
        aw->preferred_account_type = aw->type;
    }
    else if (account_type_mask_contains (aw->valid_types, aw->type))
    {
        aw->preferred_account_type = aw->type;
    }
    else if (account_type_mask_contains (aw->valid_types,
                                         last_used_account_type))
    {
        aw->type = last_used_account_type;
        aw->preferred_account_type = last_used_account_type;
    }
    else
    {
        aw->preferred_account_type = aw->type;
        aw->type = ACCT_TYPE_INVALID;
        for (gint type = ACCT_TYPE_NONE + 1; type < NUM_ACCOUNT_TYPES; type++)
        {
            if (account_type_mask_contains (aw->valid_types, type))
            {
                aw->type = type;
                break;
            }
        }
    }

    expression = gtk_property_expression_new (GNC_TYPE_ACCOUNT_TYPE_ITEM,
                                              NULL, "name");
    gtk_drop_down_set_expression (GTK_DROP_DOWN (aw->type_combo), expression);
    gtk_expression_unref (expression);
    g_signal_connect (aw->type_combo, "notify::selected",
                      G_CALLBACK (gnc_account_type_dropdown_changed_cb), aw);
    account_type_dropdown_set_model (aw, aw->valid_types);
    gnc_account_type_update (aw);
}
void
gnc_account_name_insert_text_cb (GtkWidget   *entry,
                                 const gchar *text,
                                 gint         length,
                                 gint        *position,
                                 gpointer     data)
{
    GtkEditable *editable = GTK_EDITABLE(entry);
    const gchar *separator = NULL;
    gchar **strsplit;

    separator = gnc_get_account_separator_string ();
    strsplit = g_strsplit (text, separator, 0);
    if (strsplit[1] != NULL)
    {
        gchar *result = g_strjoinv (NULL, strsplit);
        g_signal_handlers_block_by_func (G_OBJECT(editable),
                                         G_CALLBACK(gnc_account_name_insert_text_cb),
                                         data);
        gtk_editable_insert_text (editable, result, g_utf8_strlen (result, -1), position);
        g_signal_handlers_unblock_by_func (G_OBJECT(editable),
                                           G_CALLBACK(gnc_account_name_insert_text_cb),
                                           data);
        g_signal_stop_emission_by_name (G_OBJECT(editable), "insert_text");
        g_free (result);
    }

    g_strfreev (strsplit);
}

void
gnc_account_name_changed_cb (GtkWidget *widget, gpointer data)
{
    AccountWindow *aw = data;

    gnc_account_window_set_name (aw);
}

void
gnc_account_color_default_cb (GtkWidget *widget, gpointer data)
{
    GdkRGBA color;
    AccountWindow *aw = data;

    gdk_rgba_parse (&color, DEFAULT_COLOR);
    gtk_color_dialog_button_set_rgba (aw->color_entry_button, &color);

}

static void
commodity_changed_cb (GNCGeneralSelect *gsl, gpointer data)
{
    AccountWindow *aw = data;
    gnc_commodity *currency;
    GtkSelectionModel *selection;
    Account *account = aw_get_account (aw);

    currency = (gnc_commodity *) gnc_general_select_get_selected (gsl);
    if (!currency)
        return;

    if (xaccAccountGetIsOpeningBalance (account))
    {
        Account *ob_account = gnc_account_lookup_by_opening_balance (gnc_book_get_root_account (aw->book), currency);
        if (ob_account != account)
        {
            gnc_error_dialog (aw->dialog, "%s",
                              _("An account with opening balance already exists for the desired currency."));
            g_signal_handlers_block_by_func (gsl, commodity_changed_cb, data);
            gnc_general_select_set_selected (gsl, xaccAccountGetCommodity (account));
            g_signal_handlers_unblock_by_func (gsl, commodity_changed_cb, data);
            return;
        }
    }

    gnc_amount_edit_set_fraction (GNC_AMOUNT_EDIT(aw->opening_balance_edit),
                                  gnc_commodity_get_fraction (currency));
    gnc_amount_edit_set_print_info (GNC_AMOUNT_EDIT(aw->opening_balance_edit),
                                    gnc_commodity_print_info (currency, FALSE));

    selection = gnc_tree_view_account_get_selection_model (
        GNC_TREE_VIEW_ACCOUNT (aw->transfer_tree));
    gtk_selection_model_unselect_all (selection);
    gnc_account_opening_balance_button_update (aw, currency);
}

static gboolean
account_commodity_filter (Account *account, gpointer user_data)
{
    AccountWindow *aw = user_data;
    gnc_commodity *commodity;

    if (!account)
        return FALSE;
    commodity = gnc_general_select_get_selected (GNC_GENERAL_SELECT (aw->commodity_edit));
    return gnc_commodity_equiv (xaccAccountGetCommodity (account), commodity);
}

static void
account_parent_selection_changed_cb (GtkSelectionModel *selection, guint position,
                                     guint n_items, AccountWindow *aw)
{
    gnc_account_parent_changed_cb (G_OBJECT (selection), aw);
    (void)position;
    (void)n_items;
}

void
opening_equity_cb (GtkWidget *w, gpointer data)
{
    AccountWindow *aw = data;
    gboolean use_equity;

    use_equity = gtk_check_button_get_active (GTK_CHECK_BUTTON(w));

    gtk_widget_set_sensitive (aw->transfer_account_scroll, !use_equity);
}

/********************************************************************\
 * gnc_account_window_create                                        *
 *   creates a window to create a new account.                      *
 *                                                                  *
 * Args:   parent - the parent window dialog                        *
 * Args:   aw - the information structure for this window           *
 * Return: the created window                                       *
 \*******************************************************************/
static void
gnc_account_window_create (GtkWindow *parent, AccountWindow *aw)
{
    GtkWidget *amount;
    GtkWidget *date_edit;
    GObject *awo;
    GtkWidget *box;
    GtkWidget *label;
    GtkBuilder  *builder;
    const gchar *tt = _("This Account contains Transactions.\nChanging this option is not possible.");
    guint32 compat_types = xaccAccountTypesValid ();

    ENTER("aw %p, modal %d", aw, aw->modal);
    builder = gtk_builder_new ();
    gnc_builder_add_from_file (builder, "dialog-account.glade", "account_dialog");

    /* GtkWindow owns the toplevel lifetime. AccountWindow observes it only;
     * retaining it here would prevent the destroy signal from releasing the
     * AccountWindow and create a reference cycle. */
    aw->dialog = GTK_WINDOW (gtk_builder_get_object (builder, "account_dialog"));
    awo = G_OBJECT(aw->dialog);

    if (parent)
        gtk_window_set_transient_for (GTK_WINDOW(aw->dialog), parent);

    // Set the name for this dialog so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(aw->dialog), "gnc-id-account");
    gnc_widget_style_context_add_class (GTK_WIDGET(aw->dialog), "gnc-class-account");


    g_object_set_data (awo, "dialog_info", aw);

    if (aw->modal)
        gtk_window_set_modal (aw->dialog, TRUE);
    g_signal_connect (aw->dialog, "close-request",
                      G_CALLBACK (gnc_account_window_close_request_cb), aw);
    g_signal_connect (gtk_builder_get_object (builder, "ok_button"), "clicked",
                      G_CALLBACK (gnc_account_window_ok_cb), aw);
    g_signal_connect (gtk_builder_get_object (builder, "cancel_button"), "clicked",
                      G_CALLBACK (gnc_account_window_cancel_cb), aw);
    g_signal_connect (gtk_builder_get_object (builder, "help_button"), "clicked",
                      G_CALLBACK (gnc_account_window_help_cb), aw);
    gtk_window_set_default_widget (aw->dialog,
                                   GTK_WIDGET (gtk_builder_get_object (builder, "ok_button")));

    aw->notebook = GTK_WIDGET(gtk_builder_get_object (builder, "account_notebook"));
    aw->name_entry = GTK_WIDGET(gtk_builder_get_object (builder, "name_entry"));
    aw->description_entry = GTK_WIDGET(gtk_builder_get_object (builder, "description_entry"));
    aw->color_entry_button = GTK_COLOR_DIALOG_BUTTON (gtk_builder_get_object (builder, "color_entry_button"));
    aw->color_default_button = GTK_WIDGET(gtk_builder_get_object (builder, "color_default_button"));
    aw->code_entry = GTK_WIDGET(gtk_builder_get_object (builder, "code_entry"));
    aw->notes_text_buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW(GTK_WIDGET(
                                                      gtk_builder_get_object (builder,
                                                      "notes_text"))));

    box = GTK_WIDGET(gtk_builder_get_object (builder, "commodity_hbox"));
    aw->commodity_edit = gnc_general_select_new (GNC_GENERAL_SELECT_TYPE_SELECT,
                                                 gnc_commodity_edit_get_string,
                                                 gnc_commodity_edit_new_select,
                                                 &aw->commodity_mode);

    gtk_box_append (GTK_BOX(box), GTK_WIDGET(aw->commodity_edit));
    gtk_widget_set_visible (GTK_WIDGET(aw->commodity_edit), TRUE);
    // If the account has transactions, prevent changes by displaying a label and tooltip
    if (xaccAccountGetSplitsSize (aw_get_account (aw)) != 0)
    {
        gtk_widget_set_tooltip_text (aw->commodity_edit, tt);
        gtk_widget_set_sensitive (aw->commodity_edit, FALSE);
    }

    label = GTK_WIDGET(gtk_builder_get_object (builder, "security_label"));
    gnc_general_select_make_mnemonic_target (GNC_GENERAL_SELECT(aw->commodity_edit), label);

    g_signal_connect (G_OBJECT(aw->commodity_edit), "changed",
                      G_CALLBACK(commodity_changed_cb), aw);

    aw->account_scu = GTK_WIDGET(gtk_builder_get_object (builder, "account_scu"));
    account_scu_dropdown_setup (aw);

    aw->parent_scroll = GTK_WIDGET(gtk_builder_get_object (builder, "parent_scroll"));

    aw->parent_tree = gnc_tree_view_account_new (TRUE);
    gtk_scrolled_window_set_child (GTK_SCROLLED_WINDOW (aw->parent_scroll),
                                   GTK_WIDGET (aw->parent_tree));
    gtk_widget_set_visible (GTK_WIDGET(aw->parent_tree), TRUE);
    aw_connect_selection_changed (aw);

    aw->balance_grid = GTK_WIDGET(gtk_builder_get_object (builder, "balance_grid"));

    box  = GTK_WIDGET(gtk_builder_get_object (builder, "higher_balance_limit_hbox"));
    aw->higher_balance_limit_edit = gnc_amount_edit_new ();
    gtk_box_append (GTK_BOX(box), GTK_WIDGET(aw->higher_balance_limit_edit));
    gnc_amount_edit_set_evaluate_on_enter (GNC_AMOUNT_EDIT(aw->higher_balance_limit_edit), TRUE);
    gnc_amount_edit_set_validate_on_change (GNC_AMOUNT_EDIT(aw->higher_balance_limit_edit), TRUE);
    gnc_amount_edit_show_warning_symbol (GNC_AMOUNT_EDIT(aw->higher_balance_limit_edit), TRUE);
    gtk_widget_set_visible (GTK_WIDGET(aw->higher_balance_limit_edit), TRUE);

    box  = GTK_WIDGET(gtk_builder_get_object (builder, "lower_balance_limit_hbox"));
    aw->lower_balance_limit_edit = gnc_amount_edit_new ();
    gtk_box_append (GTK_BOX(box), GTK_WIDGET(aw->lower_balance_limit_edit));
    gnc_amount_edit_set_evaluate_on_enter (GNC_AMOUNT_EDIT(aw->lower_balance_limit_edit), TRUE);
    gnc_amount_edit_set_validate_on_change (GNC_AMOUNT_EDIT(aw->lower_balance_limit_edit), TRUE);
    gnc_amount_edit_show_warning_symbol (GNC_AMOUNT_EDIT(aw->lower_balance_limit_edit), TRUE);
    gtk_widget_set_visible (GTK_WIDGET(aw->lower_balance_limit_edit), TRUE);

    aw->include_balance_sub_accts = GTK_WIDGET(gtk_builder_get_object (builder, "include_sub_accts_tb"));

    aw->more_properties_page =
        gtk_notebook_get_nth_page (GTK_NOTEBOOK(aw->notebook), 1);

    aw->opening_balance_button = GTK_WIDGET(gtk_builder_get_object (builder, "opening_balance_button"));
    aw->tax_related_button = GTK_WIDGET(gtk_builder_get_object (builder, "tax_related_button"));
    aw->placeholder_button = GTK_WIDGET(gtk_builder_get_object (builder, "placeholder_button"));
    aw->hidden_button = GTK_WIDGET(gtk_builder_get_object (builder, "hidden_button"));
    aw->auto_interest_button = GTK_WIDGET(gtk_builder_get_object (builder, "auto_interest_button"));
    set_auto_interest_box (aw);


    box = GTK_WIDGET(gtk_builder_get_object (builder, "opening_balance_box"));
    amount = gnc_amount_edit_new ();
    aw->opening_balance_edit = amount;
    gtk_box_append (GTK_BOX(box), GTK_WIDGET(amount));
    gnc_amount_edit_set_evaluate_on_enter (GNC_AMOUNT_EDIT(amount), TRUE);
    gtk_widget_set_visible (GTK_WIDGET(amount), TRUE);

    label = GTK_WIDGET(gtk_builder_get_object (builder, "balance_label"));
    gnc_amount_edit_make_mnemonic_target (GNC_AMOUNT_EDIT(amount), label);

    box = GTK_WIDGET(gtk_builder_get_object (builder, "opening_balance_date_box"));
    label = GTK_WIDGET(gtk_builder_get_object (builder, "date_label"));
    date_edit = gnc_date_edit_new (gnc_time (NULL), 0, 0);
    gnc_date_make_mnemonic_target (GNC_DATE_EDIT(date_edit), label);
    aw->opening_balance_date_edit = date_edit;
    gtk_box_append (GTK_BOX(box), GTK_WIDGET(date_edit));
    gtk_widget_set_visible (GTK_WIDGET(date_edit), TRUE);

    aw->opening_balance_page =
        gtk_notebook_get_nth_page (GTK_NOTEBOOK(aw->notebook), 2);

    aw->opening_equity_radio = GTK_WIDGET(gtk_builder_get_object (builder,
                                          "opening_equity_radio"));

    box = GTK_WIDGET(gtk_builder_get_object (builder, "transfer_account_scroll"));
    aw->transfer_account_scroll = box;

    aw->transfer_tree = GTK_WIDGET(gnc_tree_view_account_new (FALSE));
    gnc_tree_view_account_set_selection_filter (
        GNC_TREE_VIEW_ACCOUNT (aw->transfer_tree), account_commodity_filter, aw, NULL);

    gtk_scrolled_window_set_child (GTK_SCROLLED_WINDOW (box),
                                   GTK_WIDGET (aw->transfer_tree));
    gtk_widget_set_visible (GTK_WIDGET(aw->transfer_tree), TRUE);

    label = GTK_WIDGET(gtk_builder_get_object (builder, "parent_label"));
    gtk_label_set_mnemonic_widget (GTK_LABEL(label), GTK_WIDGET(aw->parent_tree));

    /* This goes at the end so the select callback has good data. */
    aw->type_combo = GTK_WIDGET(gtk_builder_get_object (builder, "account_type_combo"));

    // If the account has transactions, reduce the available account types
    // to change the current account type to based on the following
    // restrictions:
    // - the new account type should not force a change of commodity
    // - the old/new type is not an immutable type. Types are marked as
    //   immutable if gnucash depends on details that would be lost/missing
    //   if changing from/to such a type. At the time of this writing the
    //   immutable types are AR, AP and trading types.
    if (xaccAccountGetSplitsSize (aw_get_account (aw)) != 0)
    {
        GNCAccountType atype = xaccAccountGetType (aw_get_account (aw));
        compat_types = xaccAccountTypesCompatibleWith (atype);
        if (!compat_types)
            compat_types = xaccAccountTypesValid ();
    }
    gnc_account_type_view_create (aw, compat_types);

    gnc_restore_window_size (GNC_PREFS_GROUP, GTK_WINDOW(aw->dialog), parent);

    gtk_widget_grab_focus (GTK_WIDGET(aw->name_entry));

gnc_builder_connect_signals (builder, aw);
    g_object_unref (G_OBJECT(builder));

    LEAVE(" ");
}

static char *
get_ui_fullname (AccountWindow *aw)
{
    Account *parent_account;
    char *fullname;
    const gchar *name;

    name = gnc_entry_get_text (GTK_ENTRY(aw->name_entry));
    if (!name || *name == '\0')
        name = _("<No name>");

    parent_account = gnc_tree_view_account_get_selected_account (GNC_TREE_VIEW_ACCOUNT(aw->parent_tree));

    if (parent_account && !gnc_account_is_root (parent_account))
    {
        char *parent_name = gnc_account_get_full_name (parent_account);
        const gchar *separator = gnc_get_account_separator_string ();

        fullname = g_strconcat (parent_name, separator, name, NULL);
        g_free (parent_name);
    }
    else
        fullname = g_strdup (name);

    return fullname;
}

static void
gnc_account_window_set_name (AccountWindow *aw)
{
    char *fullname;
    char *title;

    if (!aw || !aw->parent_tree)
        return;

    fullname = get_ui_fullname (aw);

    if (aw->dialog_type == EDIT_ACCOUNT)
        title = g_strconcat(_("Edit Account"), " - ", fullname, NULL);
    else if (aw->next_name && (g_strv_length (aw->next_name) > 0))
    {
        const char *format = _("(%d) New Accounts");
        char *prefix = g_strdup_printf (format,
                                        g_strv_length (aw->next_name) + 1);

        title = g_strconcat (prefix, " - ", fullname, " …", NULL);
        g_free (prefix);
    }
    else
        title = g_strconcat (_("New Account"), " - ", fullname, NULL);

    gtk_window_set_title (GTK_WINDOW(aw->dialog), title);

    g_free (fullname);
    g_free (title);
}

static void
close_handler (gpointer user_data)
{
    AccountWindow *aw = user_data;

    ENTER("aw %p, modal %d", aw, aw->modal);
    if (!aw || !aw->dialog)
        return;
    gnc_save_window_size (GNC_PREFS_GROUP, aw->dialog);
    gtk_window_destroy (aw->dialog);
    LEAVE(" ");
}

/********************************************************************\
 * gnc_ui_refresh_account_window                                    *
 *   refreshes the edit window                                      *
 *                                                                  *
 * Args:   aw - the account window to refresh                       *
 * Return: none                                                     *
\********************************************************************/
static void
gnc_ui_refresh_account_window (AccountWindow *aw)
{
    if (aw == NULL)
        return;

    /*  gnc_account_tree_refresh (GNC_ACCOUNT_TREE(aw->parent_tree));*/

    gnc_account_window_set_name (aw);
}

static void
refresh_handler (GHashTable *changes, gpointer user_data)
{
    AccountWindow *aw = user_data;
    Account *account;

    account = aw_get_account (aw);
    if (!account)
    {
        gnc_close_gui_component (aw->component_id);
        return;
    }

    if (changes)
    {
        const EventInfo *info = gnc_gui_get_entity_events (changes, &aw->account);
        if (info && (info->event_mask & QOF_EVENT_DESTROY))
        {
            gnc_close_gui_component (aw->component_id);
            return;
        }
    }
    gnc_ui_refresh_account_window (aw);
}

static AccountWindow *
gnc_ui_new_account_window_internal (GtkWindow *parent,
                                    QofBook *book,
                                    Account *base_account,
                                    gchar **subaccount_names,
                                    GList *valid_types,
                                    const gnc_commodity * default_commodity,
                                    gboolean modal,
                                    GncSessionOperationContext *operation_context)
{
    const gnc_commodity *commodity, *parent_commodity;
    AccountWindow *aw;
    Account *account;
    GList *list;

    g_return_val_if_fail(book, NULL);

    aw = g_new0 (AccountWindow, 1);

    aw->book = book;
    aw->modal = modal;
    aw->operation_context =
        gnc_session_operation_context_ref (operation_context);
    aw->dialog_type = NEW_ACCOUNT;

    aw->valid_types = 0;
    for (list = valid_types; list; list = list->next)
        aw->valid_types |= (1 << GPOINTER_TO_INT (list->data));

    account = xaccMallocAccount (book);
    aw->account = *xaccAccountGetGUID (account);

    if (base_account)
    {
        aw->type = xaccAccountGetType (base_account);
        parent_commodity = xaccAccountGetCommodity (base_account);
    }
    else
    {
        aw->type = last_used_account_type;
        parent_commodity = gnc_default_currency ();
    }

    gnc_suspend_gui_refresh ();

    if (subaccount_names && *subaccount_names)
    {
        xaccAccountSetName (account, subaccount_names[0]);
        aw->subaccount_names = subaccount_names;
        aw->next_name = subaccount_names + 1;
    }

    gnc_account_window_create (parent, aw);
    gnc_account_to_ui (aw);

    gnc_resume_gui_refresh ();

    if (default_commodity != NULL)
    {
        commodity = default_commodity;
        if ((aw->type == ACCT_TYPE_STOCK) || (aw->type == ACCT_TYPE_MUTUAL))
        {
            gnc_entry_set_text (GTK_ENTRY(aw->name_entry),
                                (gpointer) gnc_commodity_get_mnemonic (commodity));
            gnc_entry_set_text (GTK_ENTRY(aw->description_entry),
                                (gpointer) gnc_commodity_get_fullname (commodity));
        }
    }
    else if ((aw->type != ACCT_TYPE_STOCK) && (aw->type != ACCT_TYPE_MUTUAL))
    {
        commodity = parent_commodity;
    }
    else
    {
        commodity = NULL;
    }
    gnc_general_select_set_selected (GNC_GENERAL_SELECT(aw->commodity_edit),
                                     (gpointer) commodity);
    gnc_account_commodity_from_type (aw, FALSE);

    if (base_account == NULL)
    {
        base_account = gnc_book_get_root_account (book);
    }

    gnc_tree_view_account_collapse_all (GNC_TREE_VIEW_ACCOUNT (aw->parent_tree));
    gnc_tree_view_account_set_selected_account (GNC_TREE_VIEW_ACCOUNT(
                                                aw->parent_tree),
                                                base_account);

    gnc_account_window_set_name (aw);

    aw->component_id = gnc_register_gui_component (DIALOG_NEW_ACCOUNT_CM_CLASS,
                                                   refresh_handler,
                                                   close_handler,
                                                   aw);
    gnc_gui_component_set_session (aw->component_id, gnc_get_current_session());
    gnc_gui_component_watch_entity_type (aw->component_id,
                                         GNC_ID_ACCOUNT,
                                         QOF_EVENT_MODIFY | QOF_EVENT_DESTROY);

    gnc_window_adjust_for_screen (aw->dialog);
    gtk_window_present (aw->dialog);
    return aw;
}

static gchar **
gnc_split_account_name (QofBook *book, const char *in_name, Account **base_account)
{
    Account *root, *account;
    gchar **names, **ptr, **out_names;
    GList *list, *node;

    root = gnc_book_get_root_account (book);
    list = gnc_account_get_children (root);
    names = g_strsplit (in_name, gnc_get_account_separator_string (), -1);

    for (ptr = names; *ptr; ptr++)
    {
        /* Stop if there are no children at the current level. */
        if (list == NULL)
            break;

        /* Look for the first name in the children. */
        for (node = list; node; node = g_list_next (node))
        {
            account = node->data;

            if (g_strcmp0 (xaccAccountGetName (account), *ptr) == 0)
            {
                /* We found an account. */
                *base_account = account;
                break;
            }
        }

        /* Was there a match?  If no, stop the traversal. */
        if (node == NULL)
            break;

        g_list_free (list);
        list = gnc_account_get_children (account);
    }

    out_names = g_strdupv (ptr);
    g_strfreev (names);
    if (list)
        g_list_free (list);
    return out_names;
}

/************************************************************
 *              Entry points for a Modal Dialog             *
 ************************************************************/

void
gnc_ui_new_accounts_from_name_with_defaults_async_with_operation_context (
    GtkWindow *parent, const char *name, GList *valid_types,
    const gnc_commodity *default_commodity, Account *parent_acct,
    GncSessionOperationContext *operation_context,
    GncNewAccountCreatedCB callback, gpointer user_data)
{
    if (operation_context &&
        !gnc_session_operation_context_is_current (operation_context))
    {
        if (callback)
            callback (NULL, FALSE, user_data);
        return;
    }

    QofBook *book = gnc_get_current_book ();
    AccountWindow *aw;
    Account *base_account = NULL;
    gchar **subaccount_names;

    if (!book)
    {
        if (callback)
            callback (NULL, FALSE, user_data);
        return;
    }
    if (!name || !*name)
        subaccount_names = NULL;
    else
        subaccount_names = gnc_split_account_name (book, name, &base_account);
    if (parent_acct)
        base_account = parent_acct;

    if (operation_context &&
        !gnc_session_operation_context_begin (operation_context))
    {
        g_strfreev (subaccount_names);
        if (callback)
            callback (NULL, FALSE, user_data);
        return;
    }
    aw = gnc_ui_new_account_window_internal (parent, book, base_account,
                                             subaccount_names, valid_types,
                                             default_commodity, FALSE,
                                             operation_context);
    if (operation_context)
        gnc_session_operation_context_end (operation_context);
    if (!aw)
    {
        g_strfreev (subaccount_names);
        if (callback)
            callback (NULL, FALSE, user_data);
        return;
    }

    aw->creation_callback = callback;
    aw->creation_callback_data = user_data;
    gtk_window_set_modal (GTK_WINDOW (aw->dialog), TRUE);
    if (parent)
        g_signal_connect_object (parent, "destroy",
                                 G_CALLBACK (gtk_window_destroy), aw->dialog,
                                 G_CONNECT_SWAPPED);
}

void
gnc_ui_new_accounts_from_name_with_defaults_async (
    GtkWindow *parent, const char *name, GList *valid_types,
    const gnc_commodity *default_commodity, Account *parent_acct,
    GncNewAccountCreatedCB callback, gpointer user_data)
{
    gnc_ui_new_accounts_from_name_with_defaults_async_with_operation_context (
        parent, name, valid_types, default_commodity, parent_acct, NULL,
        callback, user_data);
}
/************************************************************
 *            Entry points for a non-Modal Dialog           *
 ************************************************************/

static gboolean
find_by_account (gpointer find_data, gpointer user_data)
{
    Account *account = find_data;
    AccountWindow *aw = user_data;

    if (!aw)
        return FALSE;

    return guid_equal (&aw->account, xaccAccountGetGUID (account));
}

/*
 * opens up a window to edit an account
 *
 * Args:   account - the account to edit
 * Return: EditAccountWindow object
 */
void
gnc_ui_edit_account_window (GtkWindow *parent, Account *account)
{
    AccountWindow * aw;
    Account *parent_acct;

    if (account == NULL)
        return;

    aw = gnc_find_first_gui_component (DIALOG_EDIT_ACCOUNT_CM_CLASS,
                                       find_by_account, account);
    if (aw)
    {
        gtk_window_present (GTK_WINDOW(aw->dialog));
        return;
    }

    aw = g_new0 (AccountWindow, 1);

    aw->book = gnc_account_get_book (account);
    aw->modal = FALSE;
    aw->dialog_type = EDIT_ACCOUNT;
    aw->account = *xaccAccountGetGUID (account);
    aw->subaccount_names = NULL;
    aw->type = xaccAccountGetType (account);

    gnc_suspend_gui_refresh ();

    gnc_account_window_create (parent, aw);
    gnc_account_to_ui (aw);

    gnc_resume_gui_refresh ();

    if (xaccAccountGetSplitList (account) != 0)
        gtk_widget_set_visible (GTK_WIDGET(aw->opening_balance_page), FALSE);

    parent_acct = gnc_account_get_parent (account);
    if (parent_acct == NULL)
        parent_acct = account; // must be at the root

    gnc_tree_view_account_collapse_all (GNC_TREE_VIEW_ACCOUNT (aw->parent_tree));
    gnc_tree_view_account_set_selected_account (GNC_TREE_VIEW_ACCOUNT(
                                                aw->parent_tree),
                                                parent_acct);

    gnc_account_window_set_name (aw);

    gnc_window_adjust_for_screen (GTK_WINDOW(aw->dialog));

    aw->component_id = gnc_register_gui_component (DIALOG_EDIT_ACCOUNT_CM_CLASS,
                                                   refresh_handler,
                                                   close_handler, aw);

    gnc_gui_component_set_session (aw->component_id, gnc_get_current_session ());
    gnc_gui_component_watch_entity_type (aw->component_id,
                                         GNC_ID_ACCOUNT,
                                         QOF_EVENT_MODIFY | QOF_EVENT_DESTROY);

    gtk_window_present (GTK_WINDOW(aw->dialog));
}

void
gnc_ui_new_account_with_types_and_commodity (GtkWindow *parent, QofBook *book, GList *valid_types,
                                             gnc_commodity *default_commodity)
{
    gnc_ui_new_account_window_internal (parent, book, NULL, NULL,
                                        valid_types, default_commodity, FALSE, NULL);
}

/*
 * opens up a window to create a new account
 *
 * Args:    book - containing book for the new account
 *   parent_acct - The initial parent for the new account (optional)
 */
void
gnc_ui_new_account_window (GtkWindow *parent, QofBook *book,
                           Account *parent_acct)
{
    g_return_if_fail(book != NULL);
    if (parent_acct && book)
        g_return_if_fail(gnc_account_get_book (parent_acct) == book);

    gnc_ui_new_account_window_internal (parent, book, parent_acct, NULL, NULL,
                                        NULL, FALSE, NULL);
}

/************************************************************
 *             Callbacks for a non-Modal Dialog             *
 ************************************************************/

/*
 * register a callback that gets called when the account has changed
 * so significantly that you need to destroy yourself.  In particular
 * this is used by the ledger display to destroy ledgers when the
 * account type has changed.
 */
void
gnc_ui_register_account_destroy_callback (void (*cb)(Account *))
{
    if (!cb)
        return;

    if (g_list_index (ac_destroy_cb_list, cb) == -1)
        ac_destroy_cb_list = g_list_append (ac_destroy_cb_list, cb);

    return;
}

/**************************************************/

static void
gnc_account_renumber_update_examples (RenumberDialog *data)
{
    gchar *str;
    gint   interval;
    gint   digits;
    unsigned int num_digits = 1;

    g_return_if_fail (data->num_children > 0);

    const gchar *prefix = gnc_entry_get_text (GTK_ENTRY(data->prefix));
    interval = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON(data->interval));
    digits = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON(data->digits));

    if (interval <= 0)
        interval = 10;

    num_digits = (unsigned int)log10((double)(data->num_children * interval)) + 1;

    if (digits <= num_digits)
    {
        g_signal_handlers_block_by_func (GTK_SPIN_BUTTON(data->digits),
                                         (gpointer)gnc_account_renumber_digits_changed_cb,
                                          data);
        gtk_spin_button_set_value (GTK_SPIN_BUTTON(data->digits), num_digits);
        g_signal_handlers_unblock_by_func (GTK_SPIN_BUTTON(data->digits),
                                           (gpointer)gnc_account_renumber_digits_changed_cb,
                                            data);
    }
    else
        num_digits = digits;

    if (prefix && *prefix)
        str = g_strdup_printf ("%s-%0*d", prefix, num_digits, interval);
    else
        str = g_strdup_printf ("%0*d", num_digits, interval);

    gtk_label_set_text (GTK_LABEL(data->example1), str);
    g_free (str);

    if (prefix && *prefix)
        str = g_strdup_printf ("%s-%0*d", prefix, num_digits,
                               interval * data->num_children);
    else
        str = g_strdup_printf ("%0*d", num_digits,
                               interval * data->num_children);

    gtk_label_set_text (GTK_LABEL(data->example2), str);

    g_free (str);
}

void
gnc_account_renumber_prefix_changed_cb (GtkEditable *editable,
                                        RenumberDialog *data)
{
    gnc_account_renumber_update_examples (data);
}

void
gnc_account_renumber_interval_changed_cb (GtkSpinButton *spinbutton,
                                          RenumberDialog *data)
{
    gnc_account_renumber_update_examples (data);
}

void
gnc_account_renumber_digits_changed_cb (GtkSpinButton *spinbutton,
                                        RenumberDialog *data)
{
    gnc_account_renumber_update_examples (data);
}

static Account *
renumber_dialog_get_parent (RenumberDialog *data)
{
    Account *parent;

    if (!data || data->book != gnc_get_current_book () ||
        !guid_equal (qof_instance_get_guid (QOF_INSTANCE (data->book)),
                     &data->book_guid))
        return NULL;
    parent = xaccAccountLookup (&data->parent_guid, data->book);
    return parent && !qof_instance_get_destroying (QOF_INSTANCE (parent)) ? parent : NULL;
}

static void
renumber_dialog_destroy_cb (GtkWidget *object, RenumberDialog *data)
{
    if (!data)
        return;
    data->closing = TRUE;
    g_object_set_data (G_OBJECT (object), RENUMBER_DIALOG_DATA, NULL);
    if (data->dialog == GTK_WINDOW (object))
        data->dialog = NULL;
    g_free (data);
}

static void
renumber_dialog_close (RenumberDialog *data)
{
    GtkWindow *dialog;

    if (!data || data->closing || !data->dialog)
        return;

    data->closing = TRUE;
    dialog = g_steal_pointer (&data->dialog);
    gtk_window_destroy (dialog);
    g_object_unref (dialog);
}

static void
renumber_dialog_parent_destroy_cb (GtkWindow *dialog)
{
    renumber_dialog_close (g_object_get_data (G_OBJECT (dialog), RENUMBER_DIALOG_DATA));
}

static void
gnc_account_renumber_apply_cb (GtkButton *button, RenumberDialog *data)
{
    Account *parent;
    GList *children, *tmp;
    gint interval;
    unsigned int num_digits, i;
    const gchar *prefix;

    if (!data || data->closing || !(parent = renumber_dialog_get_parent (data)))
    {
        renumber_dialog_close (data);
        return;
    }
    children = gnc_account_get_children_sorted (parent);
    if (!children)
    {
        PWARN ("Can't renumber children of an account with no children!");
        renumber_dialog_close (data);
        return;
    }
    prefix = gnc_entry_get_text (GTK_ENTRY (data->prefix));
    interval = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON (data->interval));
    num_digits = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON (data->digits));

    gnc_set_busy_cursor (NULL, TRUE);
    for (tmp = children, i = 1; tmp; tmp = g_list_next (tmp), i++)
    {
        gchar *str = prefix && *prefix
            ? g_strdup_printf ("%s-%0*d", prefix, num_digits, interval * i)
            : g_strdup_printf ("%0*d", num_digits, interval * i);
        xaccAccountSetCode (tmp->data, str);
        g_free (str);
    }
    gnc_unset_busy_cursor (NULL);
    g_list_free (children);
    renumber_dialog_close (data);
    (void)button;
}

static void
gnc_account_renumber_cancel_cb (GtkButton *button, RenumberDialog *data)
{
    renumber_dialog_close (data);
    (void)button;
}

static gboolean
gnc_account_renumber_close_request_cb (GtkWindow *window, RenumberDialog *data)
{
    renumber_dialog_close (data);
    (void)window;
    return TRUE;
}

void
gnc_account_renumber_create_dialog (GtkWidget *window, Account *account)
{
    RenumberDialog *data;
    GtkBuilder *builder;
    GtkWidget *widget;
    gchar *string, *fullname;

    g_return_if_fail (account && gnc_account_n_children (account) > 0);

    data = g_new0 (RenumberDialog, 1);
    data->book = gnc_account_get_book (account);
    data->book_guid = *qof_instance_get_guid (QOF_INSTANCE (data->book));
    data->parent_guid = *xaccAccountGetGUID (account);
    data->num_children = gnc_account_n_children (account);

    builder = gtk_builder_new ();
    gnc_builder_add_from_file (builder, "dialog-account.glade", "interval_adjustment");
    gnc_builder_add_from_file (builder, "dialog-account.glade", "digit_spin_adjustment");
    gnc_builder_add_from_file (builder, "dialog-account.glade", "account_renumber_dialog");
    data->dialog = GTK_WINDOW (gtk_builder_get_object (builder, "account_renumber_dialog"));
    g_object_ref (data->dialog);
    g_object_set_data (G_OBJECT (data->dialog), RENUMBER_DIALOG_DATA, data);
    if (GTK_IS_WINDOW (window))
        gtk_window_set_transient_for (data->dialog, GTK_WINDOW (window));
    gtk_window_set_modal (data->dialog, TRUE);

    widget = GTK_WIDGET (gtk_builder_get_object (builder, "header_label"));
    fullname = gnc_account_get_full_name (account);
    string = g_strdup_printf (_("Renumber the immediate sub-accounts of '%s'?"), fullname);
    gtk_label_set_text (GTK_LABEL (widget), string);
    g_free (string);
    g_free (fullname);

    data->prefix = GTK_WIDGET (gtk_builder_get_object (builder, "prefix_entry"));
    data->interval = GTK_WIDGET (gtk_builder_get_object (builder, "interval_spin"));
    data->digits = GTK_WIDGET (gtk_builder_get_object (builder, "digit_spin"));
    data->example1 = GTK_WIDGET (gtk_builder_get_object (builder, "example1_label"));
    data->example2 = GTK_WIDGET (gtk_builder_get_object (builder, "example2_label"));
    gnc_entry_set_text (GTK_ENTRY (data->prefix), xaccAccountGetCode (account));
    gnc_account_renumber_update_examples (data);

    gnc_builder_connect_signals (builder, data);
    g_signal_connect (gtk_builder_get_object (builder, "okbutton2"), "clicked",
                      G_CALLBACK (gnc_account_renumber_apply_cb), data);
    g_signal_connect (gtk_builder_get_object (builder, "cancelbutton2"), "clicked",
                      G_CALLBACK (gnc_account_renumber_cancel_cb), data);
    g_signal_connect (data->dialog, "close-request",
                      G_CALLBACK (gnc_account_renumber_close_request_cb), data);
    g_signal_connect (data->dialog, "destroy", G_CALLBACK (renumber_dialog_destroy_cb), data);
    gtk_window_set_default_widget (data->dialog,
                                   GTK_WIDGET (gtk_builder_get_object (builder, "okbutton2")));
    g_object_unref (builder);
    if (GTK_IS_WINDOW (window))
        g_signal_connect_object (window, "destroy", G_CALLBACK (renumber_dialog_parent_destroy_cb),
                                 data->dialog, G_CONNECT_SWAPPED);
    gtk_window_present (data->dialog);
}
static void
default_color_button_cb (GtkButton *button, gpointer user_data)
{
    GdkRGBA color;

    if (gdk_rgba_parse (&color, DEFAULT_COLOR))
        gtk_color_dialog_button_set_rgba (GTK_COLOR_DIALOG_BUTTON (user_data), &color);
}

static void
update_account_color (Account *acc, const gchar *old_color, const gchar *new_color, gboolean replace)
{
    PINFO("Account is '%s', old_color is '%s', new_color is '%s', replace is %d",
            xaccAccountGetName (acc), old_color, new_color, replace);

    // have a new color, update if we can
    if (new_color)
    {
        if (!old_color || replace)
        {
            // check to see if the color is different from old one
            if (g_strcmp0 (new_color, old_color) != 0)
                xaccAccountSetColor (acc, new_color);
        }
    }
    else // change from a color to default one, remove color entry if we can
    {
        if (old_color && replace)
            xaccAccountSetColor (acc, ""); // remove entry
    }
}

static void
enable_box_cb (GtkCheckButton *toggle_button, gpointer user_data)
{
    gboolean sensitive = FALSE;

    if (gtk_check_button_get_active (toggle_button))
        sensitive = TRUE;

    gtk_widget_set_sensitive (GTK_WIDGET(user_data), sensitive);
}

typedef struct
{
    GtkWindow *dialog;
    QofBook *book;
    GncGUID book_guid;
    GncGUID account_guid;
    GtkColorDialogButton *color_button;
    GtkWidget *over_write;
    GtkWidget *enable_color;
    GtkWidget *enable_placeholder;
    GtkWidget *enable_hidden;
    GtkWidget *placeholder_button;
    GtkWidget *hidden_button;
    gchar *old_color;
    gboolean closing;
} CascadePropertiesDialog;

#define CASCADE_PROPERTIES_DIALOG_DATA "gnc-account-cascade-properties-dialog"

static Account *
cascade_dialog_get_account (CascadePropertiesDialog *data)
{
    Account *account;

    if (!data || data->book != gnc_get_current_book () ||
        !guid_equal (qof_instance_get_guid (QOF_INSTANCE (data->book)),
                     &data->book_guid))
        return NULL;
    account = xaccAccountLookup (&data->account_guid, data->book);
    return account && !qof_instance_get_destroying (QOF_INSTANCE (account)) ? account : NULL;
}

static void
cascade_dialog_destroy_cb (GtkWidget *object, CascadePropertiesDialog *data)
{
    if (!data)
        return;
    data->closing = TRUE;
    g_object_set_data (G_OBJECT (object), CASCADE_PROPERTIES_DIALOG_DATA, NULL);
    if (data->dialog == GTK_WINDOW (object))
        data->dialog = NULL;
    g_free (data->old_color);
    g_free (data);
}

static void
cascade_dialog_close (CascadePropertiesDialog *data)
{
    GtkWindow *dialog;

    if (!data || data->closing || !data->dialog)
        return;

    data->closing = TRUE;
    dialog = g_steal_pointer (&data->dialog);
    gtk_window_destroy (dialog);
    g_object_unref (dialog);
}

static void
cascade_dialog_parent_destroy_cb (GtkWindow *dialog)
{
    cascade_dialog_close (
        g_object_get_data (G_OBJECT (dialog), CASCADE_PROPERTIES_DIALOG_DATA));
}

static void
cascade_dialog_apply_cb (GtkButton *button, CascadePropertiesDialog *data)
{
    Account *account;
    GList *accounts;
    const GdkRGBA *new_color;
    gchar *new_color_string = NULL;
    gboolean color_active, placeholder_active, hidden_active, replace;
    gboolean placeholder, hidden;

    if (!data || data->closing || !(account = cascade_dialog_get_account (data)))
    {
        cascade_dialog_close (data);
        return;
    }
    color_active = gtk_check_button_get_active (GTK_CHECK_BUTTON (data->enable_color));
    placeholder_active = gtk_check_button_get_active (
        GTK_CHECK_BUTTON (data->enable_placeholder));
    hidden_active = gtk_check_button_get_active (GTK_CHECK_BUTTON (data->enable_hidden));
    replace = gtk_check_button_get_active (GTK_CHECK_BUTTON (data->over_write));
    placeholder = gtk_check_button_get_active (GTK_CHECK_BUTTON (data->placeholder_button));
    hidden = gtk_check_button_get_active (GTK_CHECK_BUTTON (data->hidden_button));

    if (color_active)
    {
        new_color = gtk_color_dialog_button_get_rgba (data->color_button);
        new_color_string = gdk_rgba_to_string (new_color);
        if (g_strcmp0 (new_color_string, DEFAULT_COLOR) == 0)
            g_clear_pointer (&new_color_string, g_free);
        update_account_color (account, data->old_color, new_color_string, replace);
    }
    if (placeholder_active)
        xaccAccountSetPlaceholder (account, placeholder);
    if (hidden_active)
        xaccAccountSetHidden (account, hidden);

    accounts = gnc_account_get_descendants (account);
    for (GList *node = accounts; node; node = g_list_next (node))
    {
        Account *descendant = node->data;
        if (color_active)
            update_account_color (descendant, xaccAccountGetColor (descendant),
                                  new_color_string, replace);
        if (placeholder_active)
            xaccAccountSetPlaceholder (descendant, placeholder);
        if (hidden_active)
            xaccAccountSetHidden (descendant, hidden);
    }
    g_list_free (accounts);
    g_free (new_color_string);
    cascade_dialog_close (data);
    (void)button;
}

static void
cascade_dialog_cancel_cb (GtkButton *button, CascadePropertiesDialog *data)
{
    cascade_dialog_close (data);
    (void)button;
}

static gboolean
cascade_dialog_close_request_cb (GtkWindow *window, CascadePropertiesDialog *data)
{
    cascade_dialog_close (data);
    (void)window;
    return TRUE;
}

void
gnc_account_cascade_properties_dialog (GtkWidget *window, Account *account)
{
    CascadePropertiesDialog *data;
    GtkBuilder *builder;
    GtkWidget *label, *color_box, *placeholder_box, *hidden_box;
    GtkWidget *color_button_default;
    gchar *string, *fullname;
    const char *color_string;
    GdkRGBA color;

    g_return_if_fail (account && gnc_account_n_children (account) > 0);

    data = g_new0 (CascadePropertiesDialog, 1);
    data->book = gnc_account_get_book (account);
    data->book_guid = *qof_instance_get_guid (QOF_INSTANCE (data->book));
    data->account_guid = *xaccAccountGetGUID (account);
    builder = gtk_builder_new ();
    gnc_builder_add_from_file (builder, "dialog-account.glade", "account_cascade_dialog");
    data->dialog = GTK_WINDOW (gtk_builder_get_object (builder, "account_cascade_dialog"));
    g_object_ref (data->dialog);
    g_object_set_data (G_OBJECT (data->dialog), CASCADE_PROPERTIES_DIALOG_DATA, data);
    if (GTK_IS_WINDOW (window))
        gtk_window_set_transient_for (data->dialog, GTK_WINDOW (window));
    gtk_window_set_modal (data->dialog, TRUE);

    data->enable_color = GTK_WIDGET (gtk_builder_get_object (builder, "enable_cascade_color"));
    color_box = GTK_WIDGET (gtk_builder_get_object (builder, "color_box"));
    label = GTK_WIDGET (gtk_builder_get_object (builder, "color_label"));
    data->over_write = GTK_WIDGET (gtk_builder_get_object (builder, "replace_check"));
    data->color_button = GTK_COLOR_DIALOG_BUTTON (gtk_builder_get_object (builder, "color_button"));
    color_button_default = GTK_WIDGET (gtk_builder_get_object (builder, "color_button_default"));
    g_signal_connect (data->enable_color, "toggled", G_CALLBACK (enable_box_cb), color_box);
    g_signal_connect (color_button_default, "clicked", G_CALLBACK (default_color_button_cb),
                      data->color_button);

    fullname = gnc_account_get_full_name (account);
    string = g_strdup_printf (_("Set the account color for account '%s' including all "
                                "sub-accounts to the selected color"), fullname);
    gtk_label_set_text (GTK_LABEL (label), string);
    g_free (string);
    color_string = xaccAccountGetColor (account);
    if (color_string)
        data->old_color = g_strdup (color_string);
    if (!gdk_rgba_parse (&color, color_string ? color_string : DEFAULT_COLOR))
        gdk_rgba_parse (&color, DEFAULT_COLOR);
    gtk_color_dialog_button_set_rgba (data->color_button, &color);

    data->enable_placeholder = GTK_WIDGET (gtk_builder_get_object (
        builder, "enable_cascade_placeholder"));
    placeholder_box = GTK_WIDGET (gtk_builder_get_object (builder, "placeholder_box"));
    label = GTK_WIDGET (gtk_builder_get_object (builder, "placeholder_label"));
    data->placeholder_button = GTK_WIDGET (gtk_builder_get_object (
        builder, "placeholder_check_button"));
    g_signal_connect (data->enable_placeholder, "toggled", G_CALLBACK (enable_box_cb),
                      placeholder_box);
    string = g_strdup_printf (_("Set the account placeholder value for account '%s' "
                                "including all sub-accounts"), fullname);
    gtk_label_set_text (GTK_LABEL (label), string);
    g_free (string);

    data->enable_hidden = GTK_WIDGET (gtk_builder_get_object (builder, "enable_cascade_hidden"));
    hidden_box = GTK_WIDGET (gtk_builder_get_object (builder, "hidden_box"));
    label = GTK_WIDGET (gtk_builder_get_object (builder, "hidden_label"));
    data->hidden_button = GTK_WIDGET (gtk_builder_get_object (builder, "hidden_check_button"));
    g_signal_connect (data->enable_hidden, "toggled", G_CALLBACK (enable_box_cb), hidden_box);
    string = g_strdup_printf (_("Set the account hidden value for account '%s' including all "
                                "sub-accounts"), fullname);
    gtk_label_set_text (GTK_LABEL (label), string);
    g_free (string);
    g_free (fullname);

    g_signal_connect (gtk_builder_get_object (builder, "okbutton3"), "clicked",
                      G_CALLBACK (cascade_dialog_apply_cb), data);
    g_signal_connect (gtk_builder_get_object (builder, "cancelbutton3"), "clicked",
                      G_CALLBACK (cascade_dialog_cancel_cb), data);
    g_signal_connect (data->dialog, "close-request", G_CALLBACK (cascade_dialog_close_request_cb),
                      data);
    g_signal_connect (data->dialog, "destroy", G_CALLBACK (cascade_dialog_destroy_cb), data);
    gtk_window_set_default_widget (data->dialog,
                                   GTK_WIDGET (gtk_builder_get_object (builder, "okbutton3")));
    g_object_unref (builder);
    if (GTK_IS_WINDOW (window))
        g_signal_connect_object (window, "destroy", G_CALLBACK (cascade_dialog_parent_destroy_cb),
                                 data->dialog, G_CONNECT_SWAPPED);
    gtk_window_present (data->dialog);
}
