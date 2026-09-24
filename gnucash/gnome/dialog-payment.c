/*
 * dialog-payment.c -- Dialog for payment entry
 * Copyright (C) 2002,2006 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 * Copyright (c) 2006 David Hampton <hampton@employees.org>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "dialog-utils.h"
#include "gnc-gtk-utils.h"
#include "gnc-component-manager.h"
#include "gnc-ui.h"
#include "gnc-gui-query.h"
#include "gnc-ui-util.h"
#include <gnc-string-utils.h>
#include "qof.h"
#include "gnc-date.h"
#include "gnc-date-edit.h"
#include "gnc-amount-edit.h"
#include "gnc-prefs.h"
#include "gnc-tree-view-account.h"
#include "Transaction.h"
#include "Account.h"
#include "gncOwner.h"
#include "engine-helpers.h"

#include "gncInvoice.h"

#include "dialog-payment.h"
#include "business-gnome-utils.h"

#include "dialog-transfer.h"
#include "dialog-print-check.h"
#include "gnc-general-search.h"
#include <qoflog.h>

static const QofLogModule log_module = G_LOG_DOMAIN;

#define DIALOG_PAYMENT_CM_CLASS "payment-dialog"
#define GNC_PREFS_GROUP         "dialogs.process-payment"

enum
{
    PAYMENT_DOC_DATE,
    PAYMENT_DOC_NUMBER,
    PAYMENT_DOC_TYPE,
    PAYMENT_DOC_DEBIT,
    PAYMENT_DOC_CREDIT,
    PAYMENT_DOC_COLUMN_COUNT,
};

#define PAYMENT_DOC_DATE_DATA   "payment-document-date"
#define PAYMENT_DOC_NUMBER_DATA "payment-document-number"
#define PAYMENT_DOC_TYPE_DATA   "payment-document-type"
#define PAYMENT_DOC_DEBIT_DATA  "payment-document-debit"
#define PAYMENT_DOC_CREDIT_DATA "payment-document-credit"
#define PAYMENT_DOC_LOT_DATA    "payment-document-lot"
#define PAYMENT_ACCOUNT_DATA    "payment-account"

typedef struct
{
    GNCLot      * lot;
    gnc_numeric   amount;
} PreExistLotInfo;

typedef struct
{
    GncOwner      owner;
    Transaction * txn;
    Account     * post_acct;
    GList       * lots;
} InitialPaymentInfo;

struct _payment_window
{
    GtkWidget   * dialog;

    GtkWidget   * payment_warning;
    GtkWidget   * conflict_message;
    GtkWidget   * ok_button;
    GtkWidget   * num_entry;
    GtkWidget   * memo_entry;
    GtkWidget   * post_combo;
    GtkWidget   * post_popover;
    GtkWidget   * owner_box;
    GtkDropDown * owner_type_combo;
    GtkWidget   * owner_choice;
    GtkWidget   * amount_debit_edit;
    GtkWidget   * amount_credit_edit;
    GtkWidget   * amount_payment_box;
    GtkWidget   * amount_refund_box;
    GtkWidget   * date_edit;
    GtkWidget        * acct_tree;
    GtkColumnView    * docs_list_view;
    GListStore       * docs_list_store;
    GtkSortListModel * docs_list_sorted;
    GtkMultiSelection *docs_list_selection;
    GListStore       * post_account_store;
    GtkFilterListModel *post_account_filtered;
    GtkSingleSelection *post_account_selection;
    GtkCustomFilter  * post_account_filter;
    GtkListView      * post_account_view;
    GtkWidget        * commodity_label;
    GtkWidget   * print_check;

    gint          component_id;
    QofBook     * book;
    GncOwner      owner;
    GncOwnerType  owner_type;
    Account     * post_acct;
    Account     * xfer_acct;
    gnc_numeric   amount_tot;
    GList       * acct_types;
    GList       * acct_commodities;

    InitialPaymentInfo *tx_info;
    gboolean      print_check_state;
};

static void payment_post_account_set_text (PaymentWindow *pw, const gchar *text);
static guint payment_document_selection_count (PaymentWindow *pw);
void gnc_payment_dialog_post_to_changed_cb (GtkEditable *widget, gpointer data);

void gnc_ui_payment_window_set_num (PaymentWindow *pw, const char* num)
{
    g_assert(pw);
    gnc_entry_set_text(GTK_ENTRY (pw->num_entry), num);
}
void gnc_ui_payment_window_set_memo (PaymentWindow *pw, const char* memo)
{
    g_assert(pw);
    gnc_entry_set_text(GTK_ENTRY (pw->memo_entry), memo);
}
void gnc_ui_payment_window_set_date (PaymentWindow *pw, const GDate *date)
{
    g_assert(pw);
    g_assert(date);
    gnc_date_edit_set_gdate (GNC_DATE_EDIT (pw->date_edit), date);
}
void gnc_ui_payment_window_set_amount (PaymentWindow *pw, gnc_numeric amount)
{
    g_assert(pw);

    /* Debits are negative, credits are positive */
    if (gnc_numeric_positive_p (amount))
    {
        gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT(pw->amount_credit_edit),
                                    amount);
        gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT(pw->amount_debit_edit),
                                    gnc_numeric_zero ());
    }
    else
    {
        gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT(pw->amount_debit_edit),
                                    gnc_numeric_neg (amount));
        gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT(pw->amount_credit_edit),
                                    gnc_numeric_zero ());
    }

}

static void gnc_ui_payment_window_set_commodity (PaymentWindow *pw, const Account* account)
{
    gchar *comm_string;
    gnc_commodity *comm;

    g_assert(pw);
    g_assert(account);

    comm = xaccAccountGetCommodity (account);
    comm_string = g_strconcat ("(", gnc_commodity_get_nice_symbol (comm), ")", NULL);
    gtk_label_set_text (GTK_LABEL(pw->commodity_label), comm_string);
    g_free (comm_string);
}

void gnc_ui_payment_window_set_postaccount (PaymentWindow *pw, const Account* account)
{
    g_assert(pw);
    g_assert(account);
    {
        gchar *acct_string = gnc_account_get_full_name (account);
        payment_post_account_set_text (pw, acct_string);
        gnc_payment_dialog_post_to_changed_cb (GTK_EDITABLE (pw->post_combo), pw);
        g_free (acct_string);
    }

    gnc_ui_payment_window_set_commodity (pw, account);
}

void gnc_ui_payment_window_set_xferaccount (PaymentWindow *pw, const Account* account)
{
    g_assert(pw);
    g_assert(account);
    gnc_tree_view_account_set_selected_account(GNC_TREE_VIEW_ACCOUNT(pw->acct_tree),
            (Account*)account);
}

static gboolean gnc_payment_dialog_has_pre_existing_txn(const PaymentWindow* pw)
{
    return pw->tx_info->txn != NULL;
}
int  gnc_payment_dialog_owner_changed_cb (G_GNUC_UNUSED GtkWidget *widget, gpointer data);
void gnc_payment_dialog_post_to_changed_cb (GtkEditable *widget, gpointer data);
void gnc_payment_dialog_document_selection_changed_cb (GtkSelectionModel *selection,
                                                        guint position,
                                                        guint n_items,
                                                        gpointer data);
void gnc_payment_dialog_xfer_acct_changed_cb (GtkWidget *widget, gpointer data);
void gnc_payment_ok_cb (GtkWidget *widget, gpointer data);
void gnc_payment_cancel_cb (GtkWidget *widget, gpointer data);
void gnc_payment_window_destroy_cb (GtkWidget *widget, gpointer data);
void gnc_payment_leave_amount_cb (GtkEventControllerFocus *controller,
                                  gpointer user_data);
void gnc_payment_activate_amount_cb (GtkWidget *widget, PaymentWindow *pw);
void gnc_payment_window_fill_docs_list (PaymentWindow *pw);

static Account *payment_post_account_fill (PaymentWindow *pw);
static Account *payment_post_account_get_active (PaymentWindow *pw);
static void payment_post_entry_changed_cb (GtkEditable *editable, gpointer data);
static gboolean payment_account_tree_key_pressed_cb (GtkEventControllerKey *controller,
                                                      guint keyval, guint keycode,
                                                      GdkModifierType state, gpointer data);
static void payment_account_tree_released_cb (GtkGestureClick *gesture,
                                              gint n_press, gdouble x, gdouble y,
                                              gpointer data);


static void
payment_post_account_set_text (PaymentWindow *pw, const gchar *text)
{
    g_signal_handlers_block_by_func (pw->post_combo,
                                     G_CALLBACK (payment_post_entry_changed_cb), pw);
    gtk_editable_set_text (GTK_EDITABLE (pw->post_combo), text ? text : "");
    g_signal_handlers_unblock_by_func (pw->post_combo,
                                       G_CALLBACK (payment_post_entry_changed_cb), pw);
}

static Account *
payment_post_account_get_active (PaymentWindow *pw)
{
    const gchar *text;
    guint n_items;

    if (!pw || !pw->post_combo || !pw->post_account_store)
        return NULL;

    text = gtk_editable_get_text (GTK_EDITABLE (pw->post_combo));
    if (!text || !*text)
        return NULL;

    n_items = g_list_model_get_n_items (G_LIST_MODEL (pw->post_account_store));
    for (guint position = 0; position < n_items; position++)
    {
        GtkStringObject *item = g_list_model_get_item (G_LIST_MODEL (pw->post_account_store),
                                                        position);
        Account *account = NULL;

        if (g_strcmp0 (text, gtk_string_object_get_string (item)) == 0)
            account = g_object_get_data (G_OBJECT (item), PAYMENT_ACCOUNT_DATA);
        g_object_unref (item);
        if (account)
            return account;
    }
    return NULL;
}

static gboolean
payment_post_account_matches (gpointer item, gpointer user_data)
{
    PaymentWindow *pw = user_data;
    const gchar *text;
    const gchar *name;
    gchar *needle;
    gchar *haystack;
    gboolean matches;

    if (!pw || !pw->post_combo || !GTK_IS_STRING_OBJECT (item))
        return FALSE;

    text = gtk_editable_get_text (GTK_EDITABLE (pw->post_combo));
    if (!text || !*text)
        return TRUE;

    name = gtk_string_object_get_string (GTK_STRING_OBJECT (item));
    needle = g_utf8_casefold (text, -1);
    haystack = g_utf8_casefold (name, -1);
    matches = g_strstr_len (haystack, -1, needle) != NULL;
    g_free (needle);
    g_free (haystack);
    return matches;
}

static void
payment_post_account_item_setup_cb (G_GNUC_UNUSED GtkSignalListItemFactory *factory,
                                    GtkListItem *list_item,
                                    G_GNUC_UNUSED gpointer user_data)
{
    GtkWidget *label = gtk_label_new (NULL);
    gtk_label_set_xalign (GTK_LABEL (label), 0.0f);
    gtk_list_item_set_child (list_item, label);
}

static void
payment_post_account_item_bind_cb (G_GNUC_UNUSED GtkSignalListItemFactory *factory,
                                   GtkListItem *list_item,
                                   G_GNUC_UNUSED gpointer user_data)
{
    GtkStringObject *item = GTK_STRING_OBJECT (gtk_list_item_get_item (list_item));
    GtkWidget *label = gtk_list_item_get_child (list_item);

    gtk_label_set_text (GTK_LABEL (label), gtk_string_object_get_string (item));
}

static void
payment_post_account_activated_cb (G_GNUC_UNUSED GtkListView *view,
                                   guint position, gpointer user_data)
{
    PaymentWindow *pw = user_data;
    GtkStringObject *item;

    if (!pw || !pw->post_account_filtered)
        return;

    item = g_list_model_get_item (G_LIST_MODEL (pw->post_account_filtered), position);
    if (!item)
        return;

    payment_post_account_set_text (pw, gtk_string_object_get_string (item));
    gtk_popover_popdown (GTK_POPOVER (pw->post_popover));
    gnc_payment_dialog_post_to_changed_cb (GTK_EDITABLE (pw->post_combo), pw);
    g_object_unref (item);
}

static void
payment_post_entry_changed_cb (G_GNUC_UNUSED GtkEditable *editable, gpointer data)
{
    PaymentWindow *pw = data;

    if (!pw)
        return;

    gtk_filter_changed (GTK_FILTER (pw->post_account_filter), GTK_FILTER_CHANGE_DIFFERENT);
    if (g_list_model_get_n_items (G_LIST_MODEL (pw->post_account_filtered)) > 0)
        gtk_popover_popup (GTK_POPOVER (pw->post_popover));
    gnc_payment_dialog_post_to_changed_cb (GTK_EDITABLE (pw->post_combo), pw);
}

static void
payment_post_account_setup (PaymentWindow *pw, GtkBox *box)
{
    GtkListItemFactory *factory;
    GtkWidget *scroller;

    pw->post_combo = gtk_entry_new ();
    gtk_widget_set_hexpand (pw->post_combo, TRUE);
    gtk_box_append (box, pw->post_combo);

    pw->post_account_store = g_list_store_new (GTK_TYPE_STRING_OBJECT);
    pw->post_account_filter = gtk_custom_filter_new (payment_post_account_matches, pw, NULL);
    pw->post_account_filtered = gtk_filter_list_model_new (
        G_LIST_MODEL (g_object_ref (pw->post_account_store)),
        GTK_FILTER (g_object_ref (pw->post_account_filter)));
    pw->post_account_selection = gtk_single_selection_new (
        G_LIST_MODEL (g_object_ref (pw->post_account_filtered)));

    factory = GTK_LIST_ITEM_FACTORY (gtk_signal_list_item_factory_new ());
    g_signal_connect (factory, "setup", G_CALLBACK (payment_post_account_item_setup_cb), NULL);
    g_signal_connect (factory, "bind", G_CALLBACK (payment_post_account_item_bind_cb), NULL);
    pw->post_account_view = GTK_LIST_VIEW (gtk_list_view_new (
        GTK_SELECTION_MODEL (g_object_ref (pw->post_account_selection)), factory));
    g_signal_connect (pw->post_account_view, "activate",
                      G_CALLBACK (payment_post_account_activated_cb), pw);

    scroller = gtk_scrolled_window_new ();
    gtk_widget_set_size_request (scroller, 360, 240);
    gtk_scrolled_window_set_child (GTK_SCROLLED_WINDOW (scroller),
                                   GTK_WIDGET (pw->post_account_view));

    pw->post_popover = gtk_popover_new ();
    gtk_widget_set_parent (pw->post_popover, pw->post_combo);
    gtk_popover_set_child (GTK_POPOVER (pw->post_popover), scroller);
    g_signal_connect (pw->post_combo, "changed",
                      G_CALLBACK (payment_post_entry_changed_cb), pw);
}

static Account *
payment_post_account_fill (PaymentWindow *pw)
{
    GList *accounts;
    GList *node;
    gchar *old_text;
    gchar *first_name = NULL;
    gboolean old_text_is_allowed = FALSE;

    g_return_val_if_fail (pw && pw->book && pw->post_account_store, NULL);

    old_text = g_strdup (gtk_editable_get_text (GTK_EDITABLE (pw->post_combo)));
    g_list_store_remove_all (pw->post_account_store);
    accounts = gnc_account_get_descendants (gnc_book_get_root_account (pw->book));

    for (node = accounts; node; node = node->next)
    {
        Account *account = node->data;
        GtkStringObject *item;
        gchar *name;

        if (g_list_index (pw->acct_types,
                          GINT_TO_POINTER (xaccAccountGetType (account))) == -1)
            continue;

        if (pw->acct_commodities &&
            !g_list_find_custom (pw->acct_commodities,
                                 GINT_TO_POINTER (xaccAccountGetCommodity (account)),
                                 gnc_commodity_compare_void))
            continue;

        name = gnc_account_get_full_name (account);
        item = gtk_string_object_new (name);
        g_object_set_data (G_OBJECT (item), PAYMENT_ACCOUNT_DATA, account);
        g_list_store_append (pw->post_account_store, item);
        if (!first_name)
            first_name = g_strdup (name);
        if (g_strcmp0 (old_text, name) == 0)
            old_text_is_allowed = TRUE;
        g_object_unref (item);
        g_free (name);
    }

    g_list_free (accounts);
    payment_post_account_set_text (pw,
                                   old_text_is_allowed ? old_text : (first_name ? first_name : ""));
    gtk_filter_changed (GTK_FILTER (pw->post_account_filter), GTK_FILTER_CHANGE_DIFFERENT);
    g_free (first_name);
    g_free (old_text);
    return payment_post_account_get_active (pw);
}
static void
gnc_payment_window_refresh_handler (G_GNUC_UNUSED GHashTable *changes, gpointer data)
{
    PaymentWindow *pw = data;

    gnc_payment_window_fill_docs_list (pw);
    pw->post_acct = payment_post_account_fill (pw);
}

static gboolean
gnc_payment_window_check_payment (PaymentWindow *pw)
{
    const char *conflict_msg = NULL;
    gnc_numeric amount_deb, amount_cred;
    gboolean enable_xfer_acct = TRUE;
    gboolean allow_payment = TRUE;
    gint c_result, d_result;

    if (!pw)
        return FALSE;

    /* Verify the "post" account */
    if (!pw->post_acct)
    {
        conflict_msg = _("You must enter a valid account name for posting.");
        allow_payment = FALSE;
        goto update_cleanup;
    }

    /* Verify the user has selected an owner */
    gnc_owner_get_owner (pw->owner_choice, &(pw->owner));
    if (!gncOwnerIsValid(&pw->owner))
    {
        conflict_msg = _("You must select a company for payment processing.");
        allow_payment = FALSE;
        goto update_cleanup;
    }

    /* Verify the credit / debit amounts are valid */
    d_result = gnc_amount_edit_expr_is_valid (GNC_AMOUNT_EDIT(pw->amount_debit_edit),
                                              &amount_deb, FALSE, NULL);

    c_result = gnc_amount_edit_expr_is_valid (GNC_AMOUNT_EDIT(pw->amount_credit_edit),
                                              &amount_cred, FALSE, NULL);

    if ((d_result == 1) || (c_result == 1))
    {
        conflict_msg = _("There is a problem with the Payment or Refund amount.");
        allow_payment = FALSE;
        goto update_cleanup;
    }

    /* Test the total amount */
    pw->amount_tot = gnc_numeric_sub (amount_cred, amount_deb,
                                      gnc_commodity_get_fraction (
                                      xaccAccountGetCommodity (pw->post_acct)),
                                      GNC_HOW_RND_ROUND_HALF_UP);

    if (gnc_numeric_check (pw->amount_tot) || gnc_numeric_zero_p (pw->amount_tot))
    {
        enable_xfer_acct = FALSE;
    }
    else
    {
        /* Verify the user has selected a transfer account */
        pw->xfer_acct = gnc_tree_view_account_get_selected_account (GNC_TREE_VIEW_ACCOUNT(pw->acct_tree));
        if (!pw->xfer_acct)
        {
            conflict_msg = _("You must select a transfer account from the account tree.");
            allow_payment = FALSE;
            goto update_cleanup;
        }
    }

    /* this last test checks whether documents were selected. if none,
       emit warning but still allow as an unattached payment. */
    if (payment_document_selection_count (pw) == 0)
    {
        conflict_msg = _("No business items were selected to assign this payment to. This may create an unattached payment.");
        allow_payment = TRUE;
    }


update_cleanup:
    gtk_widget_set_sensitive (pw->acct_tree, enable_xfer_acct);

    /* Disable "Print Check" widget if amount is zero but save current
       state to restore when the widget is re-enabled */
    if (gtk_widget_is_sensitive (pw->print_check))
        pw->print_check_state = gtk_check_button_get_active (GTK_CHECK_BUTTON(pw->print_check));
    if (!enable_xfer_acct)
        gtk_check_button_set_active (GTK_CHECK_BUTTON(pw->print_check), FALSE);
    gtk_widget_set_sensitive (pw->print_check, enable_xfer_acct);
    if (gtk_widget_is_sensitive (pw->print_check))
        gtk_check_button_set_active (GTK_CHECK_BUTTON(pw->print_check), pw->print_check_state);

    /* Check if there are issues preventing a successful payment */
    gtk_label_set_text (GTK_LABEL(pw->conflict_message), conflict_msg);
    gtk_widget_set_sensitive (pw->ok_button, allow_payment);

    if (conflict_msg)
        gtk_widget_set_visible (GTK_WIDGET(pw->payment_warning), TRUE);
    else
        gtk_widget_set_visible (GTK_WIDGET(pw->payment_warning), FALSE);

    return allow_payment;
}

static void
gnc_payment_window_close_handler (gpointer data)
{
    PaymentWindow *pw = data;

    if (!pw) return;
    gnc_save_window_size (GNC_PREFS_GROUP, GTK_WINDOW(pw->dialog));

    gtk_window_destroy (GTK_WINDOW (pw->dialog));
}

static time64
payment_document_row_date (GObject *row)
{
    time64 *date = g_object_get_data (row, PAYMENT_DOC_DATE_DATA);
    return date ? *date : 0;
}

static const gchar *
payment_document_row_text (GObject *row, guint column)
{
    switch (column)
    {
        case PAYMENT_DOC_NUMBER:
            return g_object_get_data (row, PAYMENT_DOC_NUMBER_DATA);
        case PAYMENT_DOC_TYPE:
            return g_object_get_data (row, PAYMENT_DOC_TYPE_DATA);
        case PAYMENT_DOC_DEBIT:
            return g_object_get_data (row, PAYMENT_DOC_DEBIT_DATA);
        case PAYMENT_DOC_CREDIT:
            return g_object_get_data (row, PAYMENT_DOC_CREDIT_DATA);
        default:
            return NULL;
    }
}

static GObject *
payment_document_row_new (time64 date, const gchar *number, const gchar *type,
                          const gchar *debit, const gchar *credit, GNCLot *lot)
{
    GObject *row = g_object_new (G_TYPE_OBJECT, NULL);
    time64 *stored_date = g_new (time64, 1);

    *stored_date = date;
    g_object_set_data_full (row, PAYMENT_DOC_DATE_DATA, stored_date, g_free);
    g_object_set_data_full (row, PAYMENT_DOC_NUMBER_DATA, g_strdup (number), g_free);
    g_object_set_data_full (row, PAYMENT_DOC_TYPE_DATA, g_strdup (type), g_free);
    g_object_set_data_full (row, PAYMENT_DOC_DEBIT_DATA, g_strdup (debit), g_free);
    g_object_set_data_full (row, PAYMENT_DOC_CREDIT_DATA, g_strdup (credit), g_free);
    g_object_set_data (row, PAYMENT_DOC_LOT_DATA, lot);
    return row;
}

static void
payment_document_item_setup_cb (G_GNUC_UNUSED GtkSignalListItemFactory *factory,
                                GtkListItem *list_item, gpointer user_data)
{
    guint column = GPOINTER_TO_UINT (user_data);
    GtkWidget *label = gtk_label_new (NULL);

    gtk_label_set_xalign (GTK_LABEL (label),
                          column == PAYMENT_DOC_DEBIT || column == PAYMENT_DOC_CREDIT ? 1.0f : 0.0f);
    gtk_list_item_set_child (list_item, label);
}

static void
payment_document_item_bind_cb (G_GNUC_UNUSED GtkSignalListItemFactory *factory,
                               GtkListItem *list_item, gpointer user_data)
{
    guint column = GPOINTER_TO_UINT (user_data);
    GObject *row = gtk_list_item_get_item (list_item);
    GtkWidget *label = gtk_list_item_get_child (list_item);
    const gchar *text;
    gchar *date_text = NULL;

    if (column == PAYMENT_DOC_DATE)
    {
        date_text = qof_print_date (payment_document_row_date (row));
        text = date_text;
    }
    else
        text = payment_document_row_text (row, column);

    gtk_label_set_text (GTK_LABEL (label), text ? text : "");
    g_free (date_text);
}

static gint
payment_document_sort_cb (gconstpointer first, gconstpointer second, gpointer user_data)
{
    GObject *a = (GObject *)first;
    GObject *b = (GObject *)second;
    guint column = GPOINTER_TO_UINT (user_data);
    gint result;

    if (column == PAYMENT_DOC_DATE)
    {
        time64 a_date = payment_document_row_date (a);
        time64 b_date = payment_document_row_date (b);
        result = (a_date > b_date) - (a_date < b_date);
        if (result)
            return result;
        return g_strcmp0 (payment_document_row_text (a, PAYMENT_DOC_NUMBER),
                          payment_document_row_text (b, PAYMENT_DOC_NUMBER));
    }

    return g_strcmp0 (payment_document_row_text (a, column),
                      payment_document_row_text (b, column));
}

static GtkColumnViewColumn *
payment_document_column_new (const gchar *title, guint column, gint width)
{
    GtkListItemFactory *factory = GTK_LIST_ITEM_FACTORY (gtk_signal_list_item_factory_new ());
    GtkColumnViewColumn *view_column;
    GtkCustomSorter *sorter;

    g_signal_connect (factory, "setup", G_CALLBACK (payment_document_item_setup_cb),
                      GUINT_TO_POINTER (column));
    g_signal_connect (factory, "bind", G_CALLBACK (payment_document_item_bind_cb),
                      GUINT_TO_POINTER (column));
    view_column = gtk_column_view_column_new (title, factory);
    gtk_column_view_column_set_resizable (view_column, TRUE);
    gtk_column_view_column_set_fixed_width (view_column, width);
    sorter = gtk_custom_sorter_new (payment_document_sort_cb, GUINT_TO_POINTER (column), NULL);
    gtk_column_view_column_set_sorter (view_column, GTK_SORTER (sorter));
    g_object_unref (sorter);
    return view_column;
}

static void
payment_document_append_column (GtkColumnView *view,
                                const gchar *title, guint column, gint width)
{
    GtkColumnViewColumn *view_column = payment_document_column_new (title,
                                                                     column,
                                                                     width);

    gtk_column_view_append_column (view, view_column);
    g_object_unref (view_column);
}

static guint
payment_document_selection_count (PaymentWindow *pw)
{
    guint count = 0;
    guint n_items;

    if (!pw || !pw->docs_list_sorted || !pw->docs_list_selection)
        return 0;

    n_items = g_list_model_get_n_items (G_LIST_MODEL (pw->docs_list_sorted));
    for (guint position = 0; position < n_items; position++)
        count += gtk_selection_model_is_selected (GTK_SELECTION_MODEL (pw->docs_list_selection), position);
    return count;
}

static GList *
payment_document_selected_lots (PaymentWindow *pw)
{
    GList *lots = NULL;
    guint n_items;

    if (!pw || !pw->docs_list_sorted || !pw->docs_list_selection)
        return NULL;

    n_items = g_list_model_get_n_items (G_LIST_MODEL (pw->docs_list_sorted));
    for (guint position = 0; position < n_items; position++)
    {
        GObject *row;
        GNCLot *lot;

        if (!gtk_selection_model_is_selected (GTK_SELECTION_MODEL (pw->docs_list_selection), position))
            continue;
        row = g_list_model_get_item (G_LIST_MODEL (pw->docs_list_sorted), position);
        lot = g_object_get_data (row, PAYMENT_DOC_LOT_DATA);
        if (lot)
            lots = g_list_insert_sorted (lots, lot, (GCompareFunc)gncOwnerLotsSortFunc);
        g_object_unref (row);
    }
    return lots;
}

static gnc_numeric
payment_document_selected_total (PaymentWindow *pw)
{
    gnc_numeric total = gnc_numeric_zero ();
    guint n_items;

    if (!pw || !pw->docs_list_sorted || !pw->docs_list_selection)
        return total;

    n_items = g_list_model_get_n_items (G_LIST_MODEL (pw->docs_list_sorted));
    for (guint position = 0; position < n_items; position++)
    {
        GObject *row;
        GNCLot *lot;
        Account *account;

        if (!gtk_selection_model_is_selected (GTK_SELECTION_MODEL (pw->docs_list_selection), position))
            continue;
        row = g_list_model_get_item (G_LIST_MODEL (pw->docs_list_sorted), position);
        lot = g_object_get_data (row, PAYMENT_DOC_LOT_DATA);
        account = lot ? gnc_lot_get_account (lot) : NULL;
        if (account)
            total = gnc_numeric_add (total, gnc_lot_get_balance (lot),
                                     gnc_commodity_get_fraction (xaccAccountGetCommodity (account)),
                                     GNC_HOW_RND_ROUND_HALF_UP);
        g_object_unref (row);
    }
    return total;
}

static void
payment_document_selection_changed (PaymentWindow *pw)
{
    if (gnc_payment_dialog_has_pre_existing_txn (pw))
        return;

    gnc_ui_payment_window_set_amount (pw, payment_document_selected_total (pw));
}

static gint
_gnc_lotinfo_find_by_lot(PreExistLotInfo *lotinfo_inst, GNCLot *lot_to_find)
{
    return lotinfo_inst->lot == lot_to_find ? 0 : -1;
}

static void
payment_document_highlight (PaymentWindow *pw)
{
    gboolean selection_changed = FALSE;
    guint n_items;

    if (!pw || !pw->docs_list_sorted || !pw->docs_list_selection)
        return;

    g_signal_handlers_block_by_func (pw->docs_list_selection,
                                     G_CALLBACK (gnc_payment_dialog_document_selection_changed_cb), pw);
    gtk_selection_model_unselect_all (GTK_SELECTION_MODEL (pw->docs_list_selection));
    n_items = g_list_model_get_n_items (G_LIST_MODEL (pw->docs_list_sorted));
    for (guint position = 0; position < n_items; position++)
    {
        GObject *row = g_list_model_get_item (G_LIST_MODEL (pw->docs_list_sorted), position);
        GNCLot *lot = g_object_get_data (row, PAYMENT_DOC_LOT_DATA);
        GList *found = lot ? g_list_find_custom (pw->tx_info->lots, lot,
                                                  (GCompareFunc)_gnc_lotinfo_find_by_lot) : NULL;

        if (found)
        {
            gtk_selection_model_select_item (GTK_SELECTION_MODEL (pw->docs_list_selection),
                                             position, FALSE);
            selection_changed = TRUE;
        }
        g_object_unref (row);
    }
    g_signal_handlers_unblock_by_func (pw->docs_list_selection,
                                       G_CALLBACK (gnc_payment_dialog_document_selection_changed_cb), pw);

    if (selection_changed)
        payment_document_selection_changed (pw);
}

void
gnc_payment_window_fill_docs_list (PaymentWindow *pw)
{
    GList *list = NULL, *node;

    g_return_if_fail (pw && pw->docs_list_store);

    /* Get a list of open lots for this owner and post account */
    if (pw->owner.owner.undefined && pw->post_acct)
        list = xaccAccountFindOpenLots (pw->post_acct, gncOwnerLotMatchOwnerFunc,
                                        &pw->owner, NULL);

    /* If pre-existing transaction's post account equals the selected post account
     * and we have lots for this transaction then compensate the document list for those.
     * The presence of such lots indicates the pre-existing transaction is an existing payment that
     * we are about to replace. So we should make sure this existing payment info can be reselected
     * by the user (within the practical limits of the payment window*) to redo the same
     * payment again.
     * If the txn's lots are closed they are ignored by default so we should explicitly readd
     * them here.
     * And for all lots in the pre-existing transaction we need to readd the split amount
     * for that lot or the existing payment values would not be taken into account.
     * This will happen further below though.
     *
     * Finally all this is only relevant if the lot belongs to the same owner...
     *
     * * The practical limits are
     * - The payment dialog can handle only one transfer split. If the pre-existing
     *   transaction has more possible candidates, all but the first will be used
     * - The payment dialog can't handle AR/AP splits that aren't linked to a lot
     *   in the current post account. Such splits will be ignored as well.
     * In both cases the user will have been informed before and given the option to abort.
     */
    if (pw->tx_info->post_acct == pw->post_acct)
        for (node = pw->tx_info->lots; node; node = node->next)
        {
            PreExistLotInfo *lot_info = node->data;
            if (gnc_numeric_zero_p (gnc_lot_get_balance (lot_info->lot)))
                /* The not-zero case will be handled below when the lot is processed as part of the open lots */
            {
                GncOwner lotowner;
                gncOwnerInitUndefined(&lotowner, NULL);
                if (!gncOwnerGetOwnerFromLot(lot_info->lot, &lotowner))
                {
                    const GncOwner *owner;
                    const GncInvoice *invoice = gncInvoiceGetInvoiceFromLot(lot_info->lot);
                    if (invoice)
                    {
                        owner = gncOwnerGetEndOwner (gncInvoiceGetOwner (invoice));
                        gncOwnerCopy (owner, &lotowner);
                    }
                }
                if (gncOwnerEqual(&pw->owner, &lotowner))
                    list = g_list_prepend (list, lot_info->lot);
            }
        }

    /* Clear the existing list without treating the refresh as user input. */
    g_signal_handlers_block_by_func (pw->docs_list_selection,
                                     G_CALLBACK (gnc_payment_dialog_document_selection_changed_cb), pw);
    gtk_selection_model_unselect_all (GTK_SELECTION_MODEL (pw->docs_list_selection));
    g_list_store_remove_all (pw->docs_list_store);
    g_signal_handlers_unblock_by_func (pw->docs_list_selection,
                                       G_CALLBACK (gnc_payment_dialog_document_selection_changed_cb), pw);

    /* Add the documents and overpayments to the tree view */
    for (node = list; node; node = node->next)
    {
        GNCLot *lot = node->data;
        GList *li_node;
        time64 doc_date_time = 0;
        const gchar *doc_type_str = NULL;
        const gchar *doc_id_str   = NULL;
        const gchar *doc_deb_str  = NULL;
        const gchar *doc_cred_str = NULL;
        GncInvoice *document;
        gnc_numeric value;
        gnc_numeric debit = gnc_numeric_zero();
        gnc_numeric credit = gnc_numeric_zero();

        /* Find the lot's document if it exists,
         * it could also be a prepayment lot. */
        document = gncInvoiceGetInvoiceFromLot (lot);

        /* Find the document's date or pre-payment date */
        if (document)
            doc_date_time = gncInvoiceGetDatePosted (document);
        else
        {
            /* Calculate the payment date based on the lot splits */
            Transaction *trans = xaccSplitGetParent (gnc_lot_get_latest_split (lot));
            if (trans)
                doc_date_time = xaccTransRetDatePosted (trans);
            else
                continue; /* No valid split in this lot, skip it */
        }

        /* Find the document type. No type means pre-payment in this case */
        if (document)
        {
            doc_type_str = gncInvoiceGetTypeString (document);
        }
        else
            doc_type_str = _("Pre-Payment");

        /* Find the document id. Empty for pre-payments. */
        if (document)
        {
            doc_id_str = gncInvoiceGetID (document);
        }

        /* Find the debit/credit amount.
         * Invoices/vendor credit notes are debit (increasing the balance)
         * Customer credit notes/bills are credit (decreasing the balance)
         * Pre-payments are debit or credit depending on their sign
         */
        value = gnc_lot_get_balance (lot);

        /* If this lot is linked to the pre-existing transaction, compensate
         * its amount so the same pre-existing transaction can be reselected bye the user
         * (within applicable limits)
         */
        li_node = g_list_find_custom (pw->tx_info->lots, lot,
                                      (GCompareFunc)_gnc_lotinfo_find_by_lot);
        if (li_node)
        {
            PreExistLotInfo *lot_info = li_node->data;
            value = gnc_numeric_sub(value, lot_info->amount,
                                    gnc_commodity_get_fraction (xaccAccountGetCommodity (pw->post_acct)),
                                    GNC_HOW_RND_ROUND_HALF_UP);
        }

        if (gnc_numeric_zero_p (value))
        /* If the lot's balance is 0 after the above compensation, skip this lot */
            continue;
        else if (gnc_numeric_positive_p (value))
            debit = value;
        else
            credit = gnc_numeric_neg (value);


        /* Only display non-zero debits/credits */
        if (!gnc_numeric_zero_p (debit))
            doc_deb_str = xaccPrintAmount (debit, gnc_default_print_info (FALSE));
        if (!gnc_numeric_zero_p (credit))
            doc_cred_str = xaccPrintAmount (credit, gnc_default_print_info (FALSE));

        {
            GObject *row = payment_document_row_new (doc_date_time, doc_id_str, doc_type_str,
                                                      doc_deb_str, doc_cred_str, lot);
            g_list_store_append (pw->docs_list_store, row);
            g_object_unref (row);
        }

    }

    g_list_free (list);

    /* Highlight the preset invoice if it's in the new list */
    payment_document_highlight (pw);
    gnc_payment_window_check_payment (pw);
}

static void
gnc_payment_dialog_post_to_changed (PaymentWindow *pw)
{
    gnc_payment_window_fill_docs_list (pw);
}

static void
gnc_payment_dialog_owner_changed (PaymentWindow *pw)
{
    GncOwner *owner = &pw->owner;

    /* refresh the post and acc available accounts, but cleanup first */
    if (pw->acct_types)
    {
        g_list_free(pw->acct_types);
        pw->acct_types = NULL;
    }

    if (pw->acct_commodities)
    {
        g_list_free(pw->acct_commodities);
        pw->acct_commodities = NULL;
    }

    pw->acct_types = gncOwnerGetAccountTypesList(owner);
    if (gncOwnerIsValid(owner))
        pw->acct_commodities = gncOwnerGetCommoditiesList (owner);

    pw->post_acct = payment_post_account_fill (pw);
    if (gncOwnerEqual(&pw->owner, &pw->tx_info->owner) && pw->tx_info->post_acct)
    {
        pw->post_acct = pw->tx_info->post_acct;
        gnc_ui_payment_window_set_postaccount (pw, pw->post_acct);
    }
    gnc_payment_dialog_post_to_changed (pw);

    if (pw->post_acct)
        gnc_ui_payment_window_set_commodity (pw, pw->post_acct);

    /* Set the last-used transfer account, but only if we didn't
     * create this dialog from a pre-existing transaction. */
    if (!gnc_payment_dialog_has_pre_existing_txn(pw))
    {
        GncGUID *guid = NULL;
        Account *last_acct = NULL;

        if (gncOwnerIsValid(owner))
            qof_instance_get (qofOwnerGetOwner (owner),
                            "payment-last-account", &guid,
                            NULL);
        last_acct = xaccAccountLookup(guid, pw->book);
        guid_free (guid);
        if (last_acct)
            gnc_tree_view_account_set_selected_account(GNC_TREE_VIEW_ACCOUNT(pw->acct_tree),
                last_acct);
    }
}


static void
gnc_payment_dialog_owner_type_changed (PaymentWindow *pw)
{
    GtkWidget *debit_box, *credit_box;

    /* Some terminology:
     * Invoices are paid, credit notes are refunded.
     * A customer payment is a credit action, paying a vendor is debit
     *
     * So depending on the owner the payment amount should be considered
     * credit (customer) or debit (vendor/employee) and refunds should be
     * considered debit (customer) or credit (vendor/employee).
     * For visual consistency, the dialog box will always show a payment and
     * a refund field. Internally they are treated as credit or debit depending
     * on the owner type.
     */
    if (pw->owner_type == GNC_OWNER_CUSTOMER)
    {
        debit_box = pw->amount_refund_box;
        credit_box = pw->amount_payment_box;
    }
    else
    {
        debit_box = pw->amount_payment_box;
        credit_box = pw->amount_refund_box;
    }

    g_object_ref (G_OBJECT (pw->amount_debit_edit));
    g_object_ref (G_OBJECT (pw->amount_credit_edit));

    if (gtk_widget_is_ancestor(pw->amount_debit_edit, credit_box))
        gtk_box_remove (GTK_BOX(credit_box), GTK_WIDGET(pw->amount_debit_edit));
    if (gtk_widget_is_ancestor(pw->amount_credit_edit, debit_box))
        gtk_box_remove (GTK_BOX(debit_box), GTK_WIDGET(pw->amount_credit_edit));

    if (!gtk_widget_is_ancestor(pw->amount_debit_edit, debit_box))
        gtk_box_append (GTK_BOX(debit_box), GTK_WIDGET(pw->amount_debit_edit));
    if (!gtk_widget_is_ancestor(pw->amount_credit_edit, credit_box))
        gtk_box_append (GTK_BOX(credit_box), GTK_WIDGET(pw->amount_credit_edit));

    g_object_unref (G_OBJECT (pw->amount_debit_edit));
    g_object_unref (G_OBJECT (pw->amount_credit_edit));

    /* Rebuild the owner selector for the newly selected owner type. */
    if (pw->owner_choice)
    {
        gtk_box_remove (GTK_BOX (pw->owner_box), pw->owner_choice);
        pw->owner_choice = NULL;
    }
    pw->owner_choice = gnc_owner_select_create (NULL, pw->owner_box, pw->book, &pw->owner);
    gtk_widget_set_visible (GTK_WIDGET(pw->owner_choice), TRUE);
    gnc_payment_dialog_owner_changed (pw);

    g_signal_connect (G_OBJECT (pw->owner_choice), "changed",
                      G_CALLBACK (gnc_payment_dialog_owner_changed_cb), pw);
}

static void
gnc_payment_dialog_remember_account (PaymentWindow *pw, Account *acc)
{
     QofInstance *owner = qofOwnerGetOwner (&pw->owner);
    const GncGUID *guid;

    if (!acc) return;

    guid = xaccAccountGetGUID(acc);
    qof_begin_edit (owner);
    qof_instance_set (owner,
		      "payment-last-account", guid,
		      NULL);
    qof_commit_edit (owner);
}


static void
gnc_payment_update_style_classes (PaymentWindow *pw)
{
    GtkWidget *dialog = GTK_WIDGET (pw->dialog);
    const gchar *style_label = NULL;

    gtk_widget_remove_css_class (dialog, "gnc-class-customers");
    gtk_widget_remove_css_class (dialog, "gnc-class-vendors");
    gtk_widget_remove_css_class (dialog, "gnc-class-employees");
    gtk_widget_remove_css_class (dialog, "gnc-class-unknown");

    switch (pw->owner_type)
    {
        case GNC_OWNER_CUSTOMER:
            style_label = "gnc-class-customers";
            break;
        case GNC_OWNER_VENDOR:
            style_label = "gnc-class-vendors";
            break;
        case GNC_OWNER_EMPLOYEE:
            style_label = "gnc-class-employees";
            break;
        default:
            style_label = "gnc-class-unknown";
            break;
    }
    // Set a secondary style context for this page so it can be easily manipulated with css
    gtk_widget_add_css_class (dialog, style_label);
}

static guint
payment_owner_type_position (GncOwnerType owner_type)
{
    switch (owner_type)
    {
        case GNC_OWNER_VENDOR:
            return 1;
        case GNC_OWNER_EMPLOYEE:
            return 2;
        case GNC_OWNER_CUSTOMER:
        default:
            return 0;
    }
}

static GncOwnerType
payment_owner_type_from_position (guint position)
{
    switch (position)
    {
        case 1:
            return GNC_OWNER_VENDOR;
        case 2:
            return GNC_OWNER_EMPLOYEE;
        default:
            return GNC_OWNER_CUSTOMER;
    }
}

static void
payment_owner_type_changed_cb (G_GNUC_UNUSED GObject *object,
                               G_GNUC_UNUSED GParamSpec *property,
                               gpointer data)
{
    PaymentWindow *pw = data;
    GncOwnerType owner_type;

    if (!pw)
        return;

    owner_type = payment_owner_type_from_position (
        gtk_drop_down_get_selected (pw->owner_type_combo));
    if (owner_type == pw->owner_type)
    {
        gnc_payment_window_check_payment (pw);
        return;
    }

    pw->owner_type = owner_type;
    if (gncOwnerGetType (&pw->tx_info->owner) == pw->owner_type)
        gncOwnerCopy (&pw->tx_info->owner, &pw->owner);
    else
    {
        switch (pw->owner_type)
        {
            case GNC_OWNER_VENDOR:
                gncOwnerInitVendor (&pw->owner, NULL);
                break;
            case GNC_OWNER_EMPLOYEE:
                gncOwnerInitEmployee (&pw->owner, NULL);
                break;
            default:
                gncOwnerInitCustomer (&pw->owner, NULL);
                break;
        }
    }

    gnc_payment_dialog_owner_type_changed (pw);
    gnc_payment_window_check_payment (pw);
}

static void
gnc_payment_set_owner_type (PaymentWindow *pw, GncOwnerType owner_type)
{
    switch (owner_type)
    {
        case GNC_OWNER_CUSTOMER:
        case GNC_OWNER_EMPLOYEE:
        case GNC_OWNER_VENDOR:
            pw->owner_type = owner_type;
            break;
        default:
            pw->owner_type = GNC_OWNER_CUSTOMER;
            break;
    }

    g_signal_handlers_block_by_func (pw->owner_type_combo,
                                     G_CALLBACK (payment_owner_type_changed_cb), pw);
    gtk_drop_down_set_selected (pw->owner_type_combo,
                                payment_owner_type_position (pw->owner_type));
    g_signal_handlers_unblock_by_func (pw->owner_type_combo,
                                       G_CALLBACK (payment_owner_type_changed_cb), pw);
    gnc_payment_update_style_classes (pw);
    gnc_payment_dialog_owner_type_changed (pw);
}

int
gnc_payment_dialog_owner_changed_cb (G_GNUC_UNUSED GtkWidget *widget, gpointer data)
{
    PaymentWindow *pw = data;
    GncOwner owner;

    if (!pw) return FALSE;

    gncOwnerCopy (&(pw->owner), &owner);
    gnc_owner_get_owner (pw->owner_choice, &owner);

    /* If this owner really changed, then reset ourselves */
    if (!gncOwnerEqual (&owner, &(pw->owner)))
    {
        gncOwnerCopy (&owner, &(pw->owner));
        gnc_payment_dialog_owner_changed(pw);
    }

    /* Reflect if the payment could complete now */
    gnc_payment_window_check_payment (pw);

    return FALSE;
}

void
gnc_payment_dialog_document_selection_changed_cb (G_GNUC_UNUSED GtkSelectionModel *selection,
                                                   G_GNUC_UNUSED guint position,
                                                   G_GNUC_UNUSED guint n_items,
                                                   gpointer data)
{
    PaymentWindow *pw = data;

    if (!pw)
        return;

    payment_document_selection_changed (pw);
    gnc_payment_window_check_payment (pw);
}

void
gnc_payment_dialog_xfer_acct_changed_cb (G_GNUC_UNUSED GtkWidget *widget, gpointer data)
{
    PaymentWindow *pw = data;

    if (!pw) return;

    /* Reflect if the payment could complete now */
    gnc_payment_window_check_payment (pw);
}

void
gnc_payment_dialog_post_to_changed_cb (G_GNUC_UNUSED GtkEditable *widget, gpointer data)
{
    PaymentWindow *pw = data;
    Account *post_acct;

    if (!pw)
        return;

    post_acct = payment_post_account_get_active (pw);
    if (post_acct != pw->post_acct)
    {
        pw->post_acct = post_acct;
        gnc_payment_dialog_post_to_changed (pw);
    }
    else
        payment_document_highlight (pw);
    gnc_payment_window_check_payment (pw);

    gnc_payment_window_check_payment (pw);
}

typedef struct
{
    GWeakRef window;
    PaymentWindow *pw;
    GList *selected_lots;
    gchar *memo;
    gchar *num;
    time64 date;
    gnc_numeric exch_rate;
} PaymentApplyRequest;

static void
payment_apply_request_free (PaymentApplyRequest *request)
{
    if (!request)
        return;
    g_list_free (request->selected_lots);
    g_free (request->memo);
    g_free (request->num);
    g_weak_ref_clear (&request->window);
    g_free (request);
}

static void
payment_apply_request_finish (PaymentApplyRequest *request)
{
    GtkWidget *window = g_weak_ref_get (&request->window);
    PaymentWindow *pw = request->pw;

    if (!window || !pw || pw->dialog != window)
        goto cleanup;

    /* The dialog remained alive through the modal exchange request, so this
     * is the same confirmed payment snapshot the user accepted. */
    gnc_gui_component_clear_watches (pw->component_id);
    gnc_suspend_gui_refresh ();
    {
        gboolean auto_pay;
        if (gncOwnerGetType (&pw->owner) == GNC_OWNER_CUSTOMER)
            auto_pay = gnc_prefs_get_bool (GNC_PREFS_GROUP_INVOICE, GNC_PREF_AUTO_PAY);
        else
            auto_pay = gnc_prefs_get_bool (GNC_PREFS_GROUP_BILL, GNC_PREF_AUTO_PAY);
        gncOwnerApplyPaymentSecs (&pw->owner, &pw->tx_info->txn, request->selected_lots,
                                  pw->post_acct, pw->xfer_acct, pw->amount_tot,
                                  request->exch_rate, request->date, request->memo,
                                  request->num, auto_pay);
    }
    gnc_resume_gui_refresh ();

    gnc_payment_dialog_remember_account (pw, pw->xfer_acct);
    if (gtk_widget_is_sensitive (pw->print_check) &&
        gtk_check_button_get_active (GTK_CHECK_BUTTON (pw->print_check)))
    {
        Split *split = xaccTransFindSplitByAccount (pw->tx_info->txn, pw->xfer_acct);
        GList *splits = g_list_append (NULL, split);
        gnc_ui_print_check_dialog_create (NULL, splits, NULL);
        g_list_free (splits);
    }
    gnc_ui_payment_window_destroy (pw);

cleanup:
    g_clear_object (&window);
    payment_apply_request_free (request);
}

static void
payment_exchange_finished_cb (gboolean completed, gpointer user_data)
{
    PaymentApplyRequest *request = user_data;
    if (completed)
        payment_apply_request_finish (request);
    else
        payment_apply_request_free (request);
}

void
gnc_payment_ok_cb (G_GNUC_UNUSED GtkWidget *widget, gpointer data)
{
    PaymentWindow *pw = data;
    if (!pw || !gnc_payment_window_check_payment (pw))
        return;

    PaymentApplyRequest *request = g_new0 (PaymentApplyRequest, 1);
    request->pw = pw;
    request->exch_rate = gnc_numeric_create (1, 1);
    request->memo = g_strdup (gnc_entry_get_text (GTK_ENTRY (pw->memo_entry)));
    request->num = g_strdup (gnc_entry_get_text (GTK_ENTRY (pw->num_entry)));
    g_weak_ref_init (&request->window, pw->dialog);
    {
        GDate date;
        g_date_clear (&date, 1);
        gnc_date_edit_get_gdate (GNC_DATE_EDIT (pw->date_edit), &date);
        request->date = gdate_to_time64 (date);
        request->selected_lots = payment_document_selected_lots (pw);
    }

    if (gnc_numeric_zero_p (pw->amount_tot) ||
        gnc_commodity_equal (xaccAccountGetCommodity (pw->xfer_acct),
                              xaccAccountGetCommodity (pw->post_acct)))
    {
        payment_apply_request_finish (request);
        return;
    }

    const char *text = _("The transfer and post accounts are associated with different currencies. Please specify the conversion rate.");
    XferDialog *xfer = gnc_xfer_dialog (pw->dialog, pw->post_acct);
    gnc_info_dialog (GTK_WINDOW (pw->dialog), "%s", text);
    gnc_xfer_dialog_select_to_account (xfer, pw->xfer_acct);
    gnc_xfer_dialog_set_amount (xfer, pw->amount_tot);
    gnc_xfer_dialog_set_date (xfer, request->date);
    gnc_xfer_dialog_set_from_show_button_active (xfer, FALSE);
    gnc_xfer_dialog_set_to_show_button_active (xfer, FALSE);
    gnc_xfer_dialog_hide_from_account_tree (xfer);
    gnc_xfer_dialog_hide_to_account_tree (xfer);
    gnc_xfer_dialog_is_exchange_dialog (xfer, &request->exch_rate);
    gnc_xfer_dialog_run_async (xfer, payment_exchange_finished_cb, request);
}
void
gnc_payment_cancel_cb (G_GNUC_UNUSED GtkWidget *widget, gpointer data)
{
    PaymentWindow *pw = data;
    gnc_ui_payment_window_destroy (pw);
}

void
gnc_payment_window_destroy_cb (G_GNUC_UNUSED GtkWidget *widget, gpointer data)
{
    PaymentWindow *pw = data;

    if (!pw) return;

    gnc_unregister_gui_component (pw->component_id);

    g_list_free (pw->acct_types);
    g_list_free (pw->acct_commodities);
    if (pw->tx_info)
    {
        g_list_free_full (pw->tx_info->lots, g_free);
        g_free (pw->tx_info);
    }
    if (pw->post_combo)
    {
        g_signal_handlers_disconnect_by_data (pw->post_combo, pw);
        pw->post_combo = NULL;
    }
    if (pw->post_account_view)
    {
        g_signal_handlers_disconnect_by_data (pw->post_account_view, pw);
        gtk_list_view_set_model (pw->post_account_view, NULL);
        pw->post_account_view = NULL;
    }
    if (pw->post_account_filter)
        gtk_custom_filter_set_filter_func (pw->post_account_filter, NULL, NULL, NULL);
    if (pw->docs_list_selection)
        g_signal_handlers_disconnect_by_data (pw->docs_list_selection, pw);
    if (pw->docs_list_view)
        gtk_column_view_set_model (pw->docs_list_view, NULL);
    if (pw->post_popover)
    {
        gtk_popover_popdown (GTK_POPOVER (pw->post_popover));
        gtk_widget_unparent (pw->post_popover);
        pw->post_popover = NULL;
    }
    g_clear_object (&pw->docs_list_selection);
    g_clear_object (&pw->docs_list_sorted);
    g_clear_object (&pw->docs_list_store);
    g_clear_object (&pw->post_account_selection);
    g_clear_object (&pw->post_account_filtered);
    g_clear_object (&pw->post_account_filter);
    g_clear_object (&pw->post_account_store);
    g_free (pw);
}

static gboolean
payment_account_tree_key_pressed_cb (G_GNUC_UNUSED GtkEventControllerKey *controller,
                                     guint keyval, G_GNUC_UNUSED guint keycode,
                                     G_GNUC_UNUSED GdkModifierType state, gpointer data)
{
    PaymentWindow *pw = data;

    if (keyval == GDK_KEY_Return || keyval == GDK_KEY_KP_Enter)
    {
        Account *account = gnc_tree_view_account_get_selected_account (
            GNC_TREE_VIEW_ACCOUNT (pw->acct_tree));

        if (account && gnc_account_n_children (account) == 0 &&
            gnc_payment_window_check_payment (pw))
        {
            gnc_payment_ok_cb (pw->acct_tree, pw);
            return TRUE;
        }
    }
    return FALSE;
}

static void
payment_account_tree_released_cb (G_GNUC_UNUSED GtkGestureClick *gesture,
                                  G_GNUC_UNUSED gint n_press,
                                  G_GNUC_UNUSED gdouble x,
                                  G_GNUC_UNUSED gdouble y,
                                  gpointer data)
{
    gnc_payment_window_check_payment (data);
}

void
gnc_payment_leave_amount_cb (GtkEventControllerFocus *controller,
                             gpointer user_data)
{
    PaymentWindow *pw = user_data;
    gboolean d_payment_ok = FALSE;
    gboolean c_payment_ok = FALSE;

    if (! pw->amount_credit_edit || ! pw->amount_debit_edit)
        return;

    c_payment_ok = gnc_amount_edit_evaluate (GNC_AMOUNT_EDIT(pw->amount_credit_edit), NULL);
    d_payment_ok = gnc_amount_edit_evaluate (GNC_AMOUNT_EDIT(pw->amount_debit_edit), NULL);

    if (c_payment_ok && d_payment_ok)
    {
        gnc_numeric amount_deb, amount_cred, amount_tot;

        /* If both credit and debit amount are entered, simplify it to either one */
        amount_deb  = gnc_amount_edit_get_amount (GNC_AMOUNT_EDIT (pw->amount_debit_edit));
        amount_cred = gnc_amount_edit_get_amount (GNC_AMOUNT_EDIT (pw->amount_credit_edit));
        amount_tot = gnc_numeric_sub (amount_cred, amount_deb,
                                      gnc_commodity_get_fraction (
                                      xaccAccountGetCommodity (pw->post_acct)),
                                      GNC_HOW_RND_ROUND_HALF_UP);

        gnc_ui_payment_window_set_amount (pw, amount_tot);
    }
    /* Reflect if the payment could complete now */
    gnc_payment_window_check_payment (pw);
}

void
gnc_payment_activate_amount_cb (G_GNUC_UNUSED GtkWidget *widget,
                                PaymentWindow *pw)
{
    gnc_payment_leave_amount_cb (NULL, pw);
}

/* Select the list of accounts to show in the tree */
static void
gnc_payment_set_account_types (GncTreeViewAccount *tree)
{
    AccountViewInfo avi;
    int i;

    gnc_tree_view_account_get_view_info (tree, &avi);

    for (i = 0; i < NUM_ACCOUNT_TYPES; i++)
        avi.include_type[i] = !xaccAccountIsAPARType (i);

    gnc_tree_view_account_set_view_info (tree, &avi);
}

static gboolean
find_handler (G_GNUC_UNUSED gpointer find_data, gpointer user_data)
{
    PaymentWindow *pw = user_data;

    return (pw != NULL);
}

static void
payment_document_view_setup (PaymentWindow *pw, GtkBox *box)
{
    GtkColumnViewColumn *date_column;

    pw->docs_list_store = g_list_store_new (G_TYPE_OBJECT);
    pw->docs_list_sorted = gtk_sort_list_model_new (
        G_LIST_MODEL (g_object_ref (pw->docs_list_store)), NULL);
    gtk_sort_list_model_set_incremental (pw->docs_list_sorted, FALSE);
    pw->docs_list_selection = gtk_multi_selection_new (
        G_LIST_MODEL (g_object_ref (pw->docs_list_sorted)));
    pw->docs_list_view = GTK_COLUMN_VIEW (
        gtk_column_view_new (GTK_SELECTION_MODEL (
            g_object_ref (pw->docs_list_selection))));
    gtk_column_view_set_show_row_separators (pw->docs_list_view, TRUE);
    gtk_column_view_set_show_column_separators (pw->docs_list_view, TRUE);

    date_column = payment_document_column_new (_("Date"), PAYMENT_DOC_DATE, 95);
    gtk_column_view_append_column (pw->docs_list_view, date_column);
    payment_document_append_column (pw->docs_list_view, _("Number"),
                                    PAYMENT_DOC_NUMBER, 120);
    payment_document_append_column (pw->docs_list_view, _("Type"),
                                    PAYMENT_DOC_TYPE, 125);
    payment_document_append_column (pw->docs_list_view, _("Debit"),
                                    PAYMENT_DOC_DEBIT, 120);
    payment_document_append_column (pw->docs_list_view, _("Credit"),
                                    PAYMENT_DOC_CREDIT, 120);
    gtk_sort_list_model_set_sorter (pw->docs_list_sorted,
                                    gtk_column_view_get_sorter (pw->docs_list_view));
    gtk_column_view_sort_by_column (pw->docs_list_view, date_column, GTK_SORT_ASCENDING);
    g_object_unref (date_column);
    g_signal_connect (pw->docs_list_selection, "selection-changed",
                      G_CALLBACK (gnc_payment_dialog_document_selection_changed_cb), pw);
    gtk_box_append (box, GTK_WIDGET (pw->docs_list_view));
}

static gboolean
payment_dialog_close_request_cb (GtkWindow *window, G_GNUC_UNUSED gpointer user_data)
{
    // This callback allows the window size to be saved on closing with the X.
    gnc_save_window_size (GNC_PREFS_GROUP, window);

    return FALSE;
}

static PaymentWindow *
new_payment_window (GtkWindow *parent, QofBook *book, InitialPaymentInfo *tx_info)
{
    PaymentWindow *pw;
    GtkBuilder *builder;
    GtkWidget *box;
    GtkWidget *cancel_button;
    GtkStringList *owner_types;

    /* Ensure we always have a properly initialized PreExistTxnInfo struct to work with */
    if (!tx_info)
    {
        tx_info = g_new0 (InitialPaymentInfo, 1);
        gncOwnerInitCustomer (&tx_info->owner, NULL);
    }

    /*
     * Find an existing payment window.  If found, bring it to
     * the front.  If we have an actual owner, then set it in
     * the window. And update the PreExistTxnInfo (tx_info) for this window.
     */

    pw = gnc_find_first_gui_component (DIALOG_PAYMENT_CM_CLASS, find_handler, NULL);
    if (pw)
    {

        // Reset the current
        if (pw->tx_info->lots)
            g_list_free_full (pw->tx_info->lots, g_free);
        g_free (pw->tx_info);
        pw->tx_info = tx_info;

        gncOwnerCopy (&pw->tx_info->owner, &(pw->owner));
        gnc_payment_set_owner_type (pw, gncOwnerGetType(&pw->tx_info->owner));

        gtk_window_set_transient_for (GTK_WINDOW(pw->dialog), parent);
        gtk_window_present (GTK_WINDOW(pw->dialog));
        return(pw);
    }

    /* Ok, we need a new window */

    pw = g_new0 (PaymentWindow, 1);
    pw->book = book;
    pw->tx_info = tx_info;

    /* Open and read the Glade File */
    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-payment.glade", "payment_dialog");
    pw->dialog = GTK_WIDGET (gtk_builder_get_object (builder, "payment_dialog"));
    gtk_window_set_transient_for (GTK_WINDOW(pw->dialog), parent);

    // Set the name for this dialog so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(pw->dialog), "gnc-id-payment");

    /* Grab the widgets and build the dialog */
    pw->payment_warning = GTK_WIDGET (gtk_builder_get_object (builder, "payment_warning"));
    pw->conflict_message = GTK_WIDGET (gtk_builder_get_object (builder, "conflict_message"));
    pw->ok_button = GTK_WIDGET (gtk_builder_get_object (builder, "okbutton"));
    cancel_button = GTK_WIDGET (gtk_builder_get_object (builder, "cancelbutton"));
    pw->num_entry = GTK_WIDGET (gtk_builder_get_object (builder, "num_entry"));
    pw->memo_entry = GTK_WIDGET (gtk_builder_get_object (builder, "memo_entry"));
    pw->commodity_label = GTK_WIDGET (gtk_builder_get_object (builder, "commodity_label"));
    box = GTK_WIDGET (gtk_builder_get_object (builder, "post_account_box"));
    payment_post_account_setup (pw, GTK_BOX (box));

    box = GTK_WIDGET (gtk_builder_get_object (builder, "owner_type_box"));
    owner_types = gtk_string_list_new ((const char *[]) { _("Customer"), _("Vendor"), _("Employee"), NULL });
    pw->owner_type_combo = gnc_gtk_drop_down_new (G_LIST_MODEL (owner_types), NULL);
    gtk_box_append (GTK_BOX (box), GTK_WIDGET (pw->owner_type_combo));
    pw->owner_box = GTK_WIDGET (gtk_builder_get_object (builder, "owner_box"));

    pw->amount_refund_box = GTK_WIDGET (gtk_builder_get_object (builder, "amount_refund_box"));
    pw->amount_payment_box = GTK_WIDGET (gtk_builder_get_object (builder, "amount_payment_box"));

    pw->amount_debit_edit = gnc_amount_edit_new ();
    gnc_amount_edit_set_evaluate_on_enter (GNC_AMOUNT_EDIT (pw->amount_debit_edit),
                                           TRUE);
    gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (pw->amount_debit_edit), gnc_numeric_zero());

    GtkEventController *event_controller1 = gtk_event_controller_focus_new ();
    gtk_widget_add_controller (GTK_WIDGET(gnc_amount_edit_gtk_entry(GNC_AMOUNT_EDIT(pw->amount_debit_edit))),
                                          event_controller1);
    g_signal_connect(G_OBJECT(event_controller1),
                     "leave",
                     G_CALLBACK(gnc_payment_leave_amount_cb), pw);

    g_signal_connect(G_OBJECT(pw->amount_debit_edit),
                     "activate",
                     G_CALLBACK(gnc_payment_activate_amount_cb), pw);

    pw->amount_credit_edit = gnc_amount_edit_new ();
    gnc_amount_edit_set_evaluate_on_enter (GNC_AMOUNT_EDIT (pw->amount_credit_edit),
                                           TRUE);
    gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (pw->amount_credit_edit), gnc_numeric_zero());

    GtkEventController *event_controller2 = gtk_event_controller_focus_new ();
    gtk_widget_add_controller (GTK_WIDGET(gnc_amount_edit_gtk_entry(GNC_AMOUNT_EDIT(pw->amount_credit_edit))),
                                          event_controller2);
    g_signal_connect(G_OBJECT(event_controller2),
                     "leave",
                     G_CALLBACK(gnc_payment_leave_amount_cb), pw);

    g_signal_connect(G_OBJECT(pw->amount_credit_edit),
                     "activate",
                     G_CALLBACK(gnc_payment_activate_amount_cb), pw);

    box = GTK_WIDGET (gtk_builder_get_object (builder, "date_box"));
    pw->date_edit = gnc_date_edit_new (time(NULL), FALSE, FALSE);
    gtk_box_append (GTK_BOX(box), GTK_WIDGET(pw->date_edit));
    pw->print_check = GTK_WIDGET (gtk_builder_get_object (builder, "print_check"));

    box = GTK_WIDGET (gtk_builder_get_object (builder, "docs_list_box"));
    payment_document_view_setup (pw, GTK_BOX (box));

    box = GTK_WIDGET (gtk_builder_get_object (builder, "acct_window"));
    pw->acct_tree = GTK_WIDGET (gnc_tree_view_account_new (FALSE));
    gtk_scrolled_window_set_child (GTK_SCROLLED_WINDOW (box), pw->acct_tree);
    gnc_payment_set_account_types (GNC_TREE_VIEW_ACCOUNT (pw->acct_tree));

    /* Set the dialog for the 'new' owner and owner type.
     * Note that this also sets the post account tree. */
    gncOwnerCopy (&pw->tx_info->owner, &(pw->owner));
    gnc_payment_set_owner_type (pw, gncOwnerGetType (&pw->tx_info->owner));

    /* Window actions are explicitly asynchronous callbacks. */
    g_signal_connect (pw->dialog, "destroy", G_CALLBACK (gnc_payment_window_destroy_cb), pw);
    g_signal_connect (cancel_button, "clicked", G_CALLBACK (gnc_payment_cancel_cb), pw);
    g_signal_connect (pw->ok_button, "clicked", G_CALLBACK (gnc_payment_ok_cb), pw);
    g_signal_connect (pw->owner_type_combo, "notify::selected",
                      G_CALLBACK (payment_owner_type_changed_cb), pw);
    {
        GtkEventController *key_controller = gtk_event_controller_key_new ();
        GtkGesture *click_controller = gtk_gesture_click_new ();

        g_signal_connect (key_controller, "key-pressed",
                          G_CALLBACK (payment_account_tree_key_pressed_cb), pw);
        g_signal_connect (click_controller, "released",
                          G_CALLBACK (payment_account_tree_released_cb), pw);
        gtk_widget_add_controller (pw->acct_tree, key_controller);
        gtk_widget_add_controller (pw->acct_tree, GTK_EVENT_CONTROLLER (click_controller));
    }

    g_signal_connect (G_OBJECT(pw->dialog), "close-request",
                      G_CALLBACK(payment_dialog_close_request_cb), pw);

    /* Register with the component manager */
    pw->component_id =
        gnc_register_gui_component (DIALOG_PAYMENT_CM_CLASS,
                                    gnc_payment_window_refresh_handler,
                                    gnc_payment_window_close_handler,
                                    pw);

    /* Watch for any new or changed accounts */
    gnc_gui_component_watch_entity_type (pw->component_id,
                                         GNC_ID_ACCOUNT,
                                         QOF_EVENT_CREATE | QOF_EVENT_MODIFY |
                                         QOF_EVENT_DESTROY);

    gnc_restore_window_size (GNC_PREFS_GROUP, GTK_WINDOW(pw->dialog), GTK_WINDOW(parent));

    /* Show it all */
    gtk_window_present (GTK_WINDOW (pw->dialog));
    g_object_unref(G_OBJECT(builder));

    // The customer choice widget should have keyboard focus
    if (GNC_IS_GENERAL_SEARCH(pw->owner_choice))
    {
        gnc_general_search_grab_focus(GNC_GENERAL_SEARCH(pw->owner_choice));
    }

    /* Reflect if the payment could complete now */
    gnc_payment_window_check_payment (pw);

    /* Warn the user if they have no valid post-to accounts */
    {
        const gchar *text;
        const char *acct_type;

        text = gtk_editable_get_text (GTK_EDITABLE (pw->post_combo));

        if (!text || g_strcmp0 (text, "") == 0)
        {

            /* The code below assumes there will only be one account type.
             * Let's assert this to protect from potential future changes. */
            g_assert (g_list_length (pw->acct_types) == 1);
            acct_type = xaccAccountGetTypeStr(GPOINTER_TO_INT(pw->acct_types->data));
            gnc_warning_dialog(GTK_WINDOW (pw->dialog),
                               _("You have no valid \"Post To\" accounts. "
                                 "Please create an account of type \"%s\" "
                                 "before you continue to process this payment. "
                                 "Perhaps you want to create an Invoice or "
                                 "Bill first?"),
                               acct_type);
        }
    }

    return pw;
}


void
gnc_ui_payment_window_destroy (PaymentWindow *pw)
{
    if (!pw) return;

    gnc_close_gui_component (pw->component_id);
}

PaymentWindow *
gnc_ui_payment_new_with_invoice (GtkWindow *parent, const GncOwner *owner,
                                 QofBook *book, GncInvoice *invoice)
{
    GNCLot *postlot;
    InitialPaymentInfo *tx_info;

    if (!book) return NULL;


    tx_info = g_new0 (InitialPaymentInfo, 1);

    if (owner)
    {
        /* Figure out the company */
        gncOwnerCopy (gncOwnerGetEndOwner (owner), &tx_info->owner);
    }
    else
    {
        gncOwnerInitCustomer (&tx_info->owner, NULL);
    }

    tx_info->post_acct = gncInvoiceGetPostedAcc (invoice);

    postlot = gncInvoiceGetPostedLot (invoice);
    if (postlot)
    {
        PreExistLotInfo *lot_info = g_new0 (PreExistLotInfo, 1);
        lot_info->lot = postlot;
        lot_info->amount = gnc_numeric_zero ();
        tx_info->lots = g_list_prepend (tx_info->lots, lot_info);
    }
    return new_payment_window (parent, book, tx_info);
}

PaymentWindow *
gnc_ui_payment_new (GtkWindow *parent, GncOwner *owner, QofBook *book)
{
    return gnc_ui_payment_new_with_invoice (parent, owner, book, NULL);
}

// ///////////////

gboolean gnc_ui_payment_is_customer_payment(const Transaction *txn)
{
    gboolean result = TRUE;
    Split *assetaccount_split, *aparaccount_split;
    gnc_numeric amount;

    if (!txn)
        return result;

    if (!xaccTransGetSplitList(txn))
        return result;

    /* First test if one split is in an A/R or A/P account.
     * That will give us the best Customer vs Vendor/Employee distinction */
    // Prefer true business split (one that's linked to a lot)
    aparaccount_split = xaccTransGetFirstAPARAcctSplit(txn, TRUE);
    if (!aparaccount_split)
        // No true business split found, try again but this time more relaxed
        aparaccount_split = xaccTransGetFirstAPARAcctSplit(txn, FALSE);
    if (aparaccount_split)
    {
        if (xaccAccountGetType (xaccSplitGetAccount (aparaccount_split)) == ACCT_TYPE_RECEIVABLE)
            return TRUE;  // Type is Customer
        else if (xaccAccountGetType (xaccSplitGetAccount (aparaccount_split)) == ACCT_TYPE_PAYABLE)
            return FALSE; // Type is Vendor/Employee, there's not enough information to refine more
    }

    /* For the lack of an A/R or A/P account we'll assume positive changes to an
     * Asset/Liability or Equity account are Customer payments the others will be
     * considered Vendor payments */
    assetaccount_split = xaccTransGetFirstPaymentAcctSplit(txn);
    if (!assetaccount_split)
    {
        /* Transaction isn't valid for a payment, just return the default
         * Calling code will have to handle this situation properly */
        PINFO("No asset splits in txn \"%s\"; cannot use this for assigning a payment.",
                  xaccTransGetDescription(txn));
        return result;
    }

    assetaccount_split = xaccTransGetFirstPaymentAcctSplit(txn);
    amount = xaccSplitGetValue(assetaccount_split);
    result = gnc_numeric_positive_p(amount); // positive amounts == customer
    //PINFO("Amount=%s", gnc_numeric_to_string(amount));
    return result;
}

// ///////////////
static char *gen_split_desc (Transaction *txn, Split *split)
{
    gnc_numeric value = xaccSplitGetAmount(split);
    Account *xfer_acct = xaccSplitGetAccount(split);
    char *acct_name = gnc_account_get_full_name (xfer_acct);
    const char *action = gnc_get_action_num (txn, split);
    const char *memo = xaccSplitGetMemo (split);
    char rec_state = xaccSplitGetReconcile (split);
    const char *print_amt = xaccPrintAmount(value, gnc_account_print_info (xfer_acct, TRUE));
    char *split_str = NULL;
    char *rec_str = NULL;

    if (rec_state == CREC)
        rec_str = g_strdup_printf("[%s] ", _("Cleared"));
    else if (rec_state == YREC)
        rec_str = g_strdup_printf("[%s] ", _("Reconciled"));
    else
        rec_str = g_strdup("");

    if (action && *action && memo && *memo)
        split_str = g_strdup_printf ("%s%s: %s (%s, %s)", rec_str, acct_name, print_amt,
                                        action, memo);
    else if((action && *action) || (memo && *memo))
        split_str = g_strdup_printf ("%s%s: %s (%s)", rec_str, acct_name, print_amt,
                                        action ? action : memo);
    else
        split_str = g_strdup_printf ("%s%s: %s", rec_str, acct_name, print_amt);

    g_free (acct_name);
    g_free (rec_str);

    return split_str;
}

static void
payment_show_alert (GtkWindow *parent, const gchar *message)
{
    GtkAlertDialog *alert = gtk_alert_dialog_new ("%s", message);
    gtk_alert_dialog_show (alert, parent);
    g_object_unref (alert);
}

static GList *
select_txn_lots (GtkWindow *parent, Transaction *txn, Account **post_acct, gboolean *abort)
{
    SplitList *apar_splits = NULL; /* all splits in txn that are APAR type */
    SplitList *apar_splits_no_lot = NULL; /* APAR splits not tied to a lot */
    SplitList *iter;
    GList *txn_lots = NULL;
    GList *unique_apar_accts = NULL;

    if (!post_acct || !abort)
        return NULL;

    *abort = FALSE;
    *post_acct = NULL;
    apar_splits = xaccTransGetAPARAcctSplitList (txn, FALSE);
    for (iter = apar_splits; iter; iter = iter->next)
    {
        GNCLot *postlot;
        Split *post_split = iter->data;
        Account *apar_acct = xaccSplitGetAccount (post_split);

        if (!g_list_find (unique_apar_accts, apar_acct))
            unique_apar_accts = g_list_prepend (unique_apar_accts, apar_acct);

        postlot = xaccSplitGetLot (post_split);
        if (postlot)
        {
            PreExistLotInfo *lot_info = g_new0 (PreExistLotInfo, 1);
            lot_info->lot = postlot;
            lot_info->amount = xaccSplitGetValue (post_split);
            txn_lots = g_list_prepend (txn_lots, lot_info);
            *post_acct = apar_acct;
        }
        else
            apar_splits_no_lot = g_list_prepend (apar_splits_no_lot, post_split);
    }

    if (!*post_acct && apar_splits_no_lot)
        *post_acct = xaccSplitGetAccount (apar_splits_no_lot->data);

    if (g_list_length (unique_apar_accts) > 1)
    {
        GString *accounts = g_string_new (NULL);
        gchar *message;

        for (iter = unique_apar_accts; iter; iter = iter->next)
        {
            gchar *name = gnc_account_get_full_name (iter->data);
            g_string_append_printf (accounts, "• %s\n", name);
            g_free (name);
        }
        message = g_strdup_printf (
            _("This transaction has splits in multiple business accounts:\n\n%s\n"
              "GnuCash can only handle transactions that post to a single account.\n\n"
              "Please correct this manually by editing the transaction directly and then try again."),
            accounts->str);
        payment_show_alert (parent, message);
        PINFO ("Multiple asset accounts in splits of txn \"%s\"; cannot use this for assigning a payment.",
               xaccTransGetDescription (txn));
        g_free (message);
        g_string_free (accounts, TRUE);

        *abort = TRUE;
        g_list_free_full (txn_lots, g_free);
        txn_lots = NULL;
    }

    g_list_free (apar_splits);
    g_list_free (apar_splits_no_lot);
    g_list_free (unique_apar_accts);
    return txn_lots;
}

static PaymentWindow *
payment_window_from_transaction (GtkWindow *parent, GncOwner *owner,
                                 Transaction *txn, Split *payment_split)
{
    Account *post_acct = NULL;
    InitialPaymentInfo *tx_info;
    GList *txn_lots;
    gboolean abort = FALSE;
    PaymentWindow *pw;
    gnc_numeric amount;

    txn_lots = select_txn_lots (parent, txn, &post_acct, &abort);
    if (abort)
        return NULL;

    tx_info = g_new0 (InitialPaymentInfo, 1);
    tx_info->txn = txn;
    tx_info->post_acct = post_acct;
    tx_info->lots = txn_lots;
    if (owner)
        gncOwnerCopy (owner, &tx_info->owner);
    else
        gncOwnerInitCustomer (&tx_info->owner, NULL);

    pw = new_payment_window (parent, qof_instance_get_book (QOF_INSTANCE (txn)), tx_info);
    gnc_ui_payment_window_set_num (pw, gnc_get_num_action (txn, payment_split));
    gnc_ui_payment_window_set_memo (pw, xaccTransGetDescription (txn));
    {
        GDate txn_date = xaccTransGetDatePostedGDate (txn);
        gnc_ui_payment_window_set_date (pw, &txn_date);
    }

    amount = payment_split ? xaccSplitGetAmount (payment_split) : gnc_numeric_zero ();
    if (payment_split && pw->post_acct)
        amount = xaccSplitConvertAmount (payment_split, pw->post_acct);
    gnc_ui_payment_window_set_amount (pw, amount);
    if (payment_split)
        gnc_ui_payment_window_set_xferaccount (pw, xaccSplitGetAccount (payment_split));
    return pw;
}

typedef struct
{
    GWeakRef parent;
    GncOwner owner;
    Transaction *txn;
    GList *payment_splits;
    GList *buttons;
    GtkWindow *dialog;
    gboolean consumed;
} PaymentTxnRequest;

static void
payment_txn_request_free (PaymentTxnRequest *request)
{
    if (!request)
        return;
    g_list_free (request->payment_splits);
    g_list_free (request->buttons);
    g_weak_ref_clear (&request->parent);
    g_free (request);
}

static Split *
payment_txn_request_selected_split (PaymentTxnRequest *request)
{
    for (GList *node = request->buttons; node; node = node->next)
    {
        GtkCheckButton *button = GTK_CHECK_BUTTON (node->data);
        if (gtk_check_button_get_active (button))
            return g_object_get_data (G_OBJECT (button), "payment-split");
    }
    return NULL;
}

static void
payment_txn_dialog_destroyed_cb (G_GNUC_UNUSED GtkWidget *widget, gpointer user_data)
{
    PaymentTxnRequest *request = user_data;
    request->dialog = NULL;
    if (!request->consumed)
        payment_txn_request_free (request);
}

static void
payment_txn_continue_cb (G_GNUC_UNUSED GtkButton *button, gpointer user_data)
{
    PaymentTxnRequest *request = user_data;
    GtkWindow *parent;
    Split *payment_split;

    if (request->consumed)
        return;

    payment_split = payment_txn_request_selected_split (request);
    if (!payment_split)
        return;

    request->consumed = TRUE;
    parent = g_weak_ref_get (&request->parent);
    gtk_window_destroy (request->dialog);
    payment_window_from_transaction (parent, &request->owner, request->txn, payment_split);
    g_clear_object (&parent);
    payment_txn_request_free (request);
}

static void
payment_txn_cancel_cb (G_GNUC_UNUSED GtkButton *button, gpointer user_data)
{
    PaymentTxnRequest *request = user_data;
    if (request->dialog)
        gtk_window_destroy (request->dialog);
}

static void
payment_txn_request_show_split_picker (PaymentTxnRequest *request)
{
    GtkWindow *parent = g_weak_ref_get (&request->parent);
    GtkWidget *content = gtk_box_new (GTK_ORIENTATION_VERTICAL, 12);
    GtkWidget *message = gtk_label_new (_("While this transaction has multiple splits that can be considered\n"
                                          "as 'the payment split', GnuCash only knows how to handle one.\n"
                                          "Please select one; the others will be discarded."));
    GtkWidget *button_box = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 6);
    GtkCheckButton *first = NULL;
    GtkWidget *cancel;
    GtkWidget *continue_button;

    request->dialog = GTK_WINDOW (gtk_window_new ());
    gnc_window_bind_to_application (request->dialog);
    gtk_window_set_title (request->dialog, _("Select Payment Split"));
    gtk_window_set_modal (request->dialog, TRUE);
    if (parent)
        gtk_window_set_transient_for (request->dialog, parent);
    gtk_widget_set_margin_top (content, 12);
    gtk_widget_set_margin_bottom (content, 12);
    gtk_widget_set_margin_start (content, 12);
    gtk_widget_set_margin_end (content, 12);
    gtk_label_set_xalign (GTK_LABEL (message), 0.0f);
    gtk_box_append (GTK_BOX (content), message);

    for (GList *node = request->payment_splits; node; node = node->next)
    {
        Split *split = node->data;
        gchar *description = gen_split_desc (request->txn, split);
        GtkCheckButton *choice = GTK_CHECK_BUTTON (gtk_check_button_new_with_label (description));

        if (first)
            gtk_check_button_set_group (choice, first);
        else
        {
            first = choice;
            gtk_check_button_set_active (choice, TRUE);
        }
        g_object_set_data (G_OBJECT (choice), "payment-split", split);
        request->buttons = g_list_append (request->buttons, choice);
        gtk_box_append (GTK_BOX (content), GTK_WIDGET (choice));
        g_free (description);
    }

    gtk_widget_set_halign (button_box, GTK_ALIGN_END);
    cancel = gtk_button_new_with_mnemonic (_("_Cancel"));
    continue_button = gtk_button_new_with_mnemonic (_("_Continue"));
    gtk_box_append (GTK_BOX (button_box), cancel);
    gtk_box_append (GTK_BOX (button_box), continue_button);
    gtk_box_append (GTK_BOX (content), button_box);
    gtk_window_set_child (request->dialog, content);
    g_signal_connect (request->dialog, "destroy",
                      G_CALLBACK (payment_txn_dialog_destroyed_cb), request);
    g_signal_connect (cancel, "clicked", G_CALLBACK (payment_txn_cancel_cb), request);
    g_signal_connect (continue_button, "clicked", G_CALLBACK (payment_txn_continue_cb), request);
    gtk_window_present (request->dialog);
    g_clear_object (&parent);
}

PaymentWindow *
gnc_ui_payment_new_with_txn (GtkWindow *parent, GncOwner *owner, Transaction *txn)
{
    GList *payment_splits;

    if (!txn || !xaccTransGetSplitList (txn))
        return NULL;

    payment_splits = xaccTransGetPaymentAcctSplitList (txn);
    if (!payment_splits)
    {
        if (xaccTransGetTxnType (txn) == TXN_TYPE_LINK)
            return payment_window_from_transaction (parent, owner, txn, NULL);

        payment_show_alert (parent,
                            _("The selected transaction doesn't have splits that can be assigned as a payment"));
        PINFO ("No asset splits in txn \"%s\"; cannot use this for assigning a payment.",
               xaccTransGetDescription (txn));
        return NULL;
    }

    if (!payment_splits->next)
    {
        PaymentWindow *pw = payment_window_from_transaction (parent, owner, txn,
                                                              payment_splits->data);
        g_list_free (payment_splits);
        return pw;
    }

    {
        PaymentTxnRequest *request = g_new0 (PaymentTxnRequest, 1);
        g_weak_ref_init (&request->parent, parent);
        if (owner)
            gncOwnerCopy (owner, &request->owner);
        else
            gncOwnerInitCustomer (&request->owner, NULL);
        request->txn = txn;
        request->payment_splits = payment_splits;
        payment_txn_request_show_split_picker (request);
    }
    return NULL;
}
