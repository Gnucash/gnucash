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
#include "gnc-component-manager.h"
#include "gnc-ui.h"
#include "gnc-gui-query.h"
#include "gnc-ui-util.h"
#include "qof.h"
#include "gnc-date.h"
#include "gnc-date-edit.h"
#include "gnc-amount-edit.h"
#include "gnc-gtk-utils.h"
#include "gnc-prefs.h"
#include "gnc-tree-view-account.h"
#include "tree-view-utils.h"
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

#define DIALOG_PAYMENT_CM_CLASS "payment-dialog"

typedef enum
{
    COL_OWNER_TYPE_NAME ,
    COL_OWNER_TYPE_NUM ,
} OwnerTypeCols;

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
    GtkWidget   * owner_box;
    GtkWidget   * owner_type_combo;
    GtkWidget   * owner_choice;
    GtkWidget   * amount_debit_edit;
    GtkWidget   * amount_credit_edit;
    GtkWidget   * amount_payment_box;
    GtkWidget   * amount_refund_box;
    GtkWidget   * date_edit;
    GtkWidget   * acct_tree;
    GtkWidget   * docs_list_tree_view;
    GtkWidget   * commodity_label;
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

void gnc_ui_payment_window_set_num (PaymentWindow *pw, const char* num)
{
    g_assert(pw);
    gtk_entry_set_text(GTK_ENTRY (pw->num_entry), num);
}
void gnc_ui_payment_window_set_memo (PaymentWindow *pw, const char* memo)
{
    g_assert(pw);
    gtk_entry_set_text(GTK_ENTRY (pw->memo_entry), memo);
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
        gnc_cbwe_set_by_string(GTK_COMBO_BOX(pw->post_combo), acct_string);
        g_free(acct_string);
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
int  gnc_payment_dialog_post_to_changed_cb (GtkWidget *widget, gpointer data);
void gnc_payment_dialog_document_selection_changed_cb (GtkWidget *widget, gpointer data);
void gnc_payment_dialog_xfer_acct_changed_cb (GtkWidget *widget, gpointer data);
void gnc_payment_ok_cb (GtkWidget *widget, gpointer data);
void gnc_payment_cancel_cb (GtkWidget *widget, gpointer data);
void gnc_payment_window_destroy_cb (GtkWidget *widget, gpointer data);
void gnc_payment_acct_tree_row_activated_cb (GtkWidget *widget, GtkTreePath *path,
        GtkTreeViewColumn *column, PaymentWindow *pw);
void gnc_payment_leave_amount_cb (GtkWidget *widget, GdkEventFocus *event,
                                  PaymentWindow *pw);
void gnc_payment_window_fill_docs_list (PaymentWindow *pw);


static void
gnc_payment_window_refresh_handler (G_GNUC_UNUSED GHashTable *changes, gpointer data)
{
    PaymentWindow *pw = data;

    gnc_payment_window_fill_docs_list (pw);
    pw->post_acct = gnc_account_select_combo_fill (pw->post_combo, pw->book, pw->acct_types, pw->acct_commodities);
}

static gboolean
gnc_payment_window_check_payment (PaymentWindow *pw)
{
    const char *conflict_msg = NULL;
    gnc_numeric amount_deb, amount_cred;
    gboolean enable_xfer_acct = TRUE;
    gboolean allow_payment = TRUE;
    GtkTreeSelection *selection;

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

    /* Test the total amount */
    amount_deb  = gnc_amount_edit_get_amount (GNC_AMOUNT_EDIT (pw->amount_debit_edit));
    amount_cred = gnc_amount_edit_get_amount (GNC_AMOUNT_EDIT (pw->amount_credit_edit));
    pw->amount_tot = gnc_numeric_sub (amount_cred, amount_deb,
                                      gnc_commodity_get_fraction (xaccAccountGetCommodity (pw->post_acct)),
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
    selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(pw->docs_list_tree_view));
    if (gtk_tree_selection_count_selected_rows (selection) == 0)
    {
        conflict_msg = _("No documents were selected to assign this payment to. This may create an unattached payment.");
        allow_payment = TRUE;
    }


update_cleanup:
    gtk_widget_set_sensitive (pw->acct_tree, enable_xfer_acct);

    /* Disable "Print Check" widget if amount is zero but save current
       state to restore when the widget is re-enabled */
    if (gtk_widget_is_sensitive (pw->print_check))
        pw->print_check_state = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(pw->print_check));
    if (!enable_xfer_acct)
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(pw->print_check), FALSE);
    gtk_widget_set_sensitive (pw->print_check, enable_xfer_acct);
    if (gtk_widget_is_sensitive (pw->print_check))
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(pw->print_check), pw->print_check_state);

    /* Check if there are issues preventing a successful payment */
    gtk_label_set_text (GTK_LABEL(pw->conflict_message), conflict_msg);
    gtk_widget_set_sensitive (pw->ok_button, allow_payment);
    if (conflict_msg)
    {
        gtk_widget_show (pw->payment_warning);
    }
    else
    {
        gtk_widget_hide (pw->payment_warning);
    }

    return allow_payment;
}

static void
gnc_payment_window_close_handler (gpointer data)
{
    PaymentWindow *pw = data;

    if (pw)
        gtk_widget_destroy (pw->dialog);
}

static void
calculate_selected_total_helper (GtkTreeModel *model,
                                 G_GNUC_UNUSED GtkTreePath *path,
                                 GtkTreeIter *iter,
                                 gpointer data)
{
    gnc_numeric *subtotal = (gnc_numeric*) data;
    gnc_numeric cur_val;
    GValue value = { 0 };
    GNCLot *lot;
    Account *acct;
    gnc_commodity *currency;

    gtk_tree_model_get_value (model, iter, 5, &value);
    lot = (GNCLot *) g_value_get_pointer (&value);
    g_value_unset (&value);

    /* Find the amount's currency to determine the required precision */
    acct = gnc_lot_get_account (lot);
    currency = xaccAccountGetCommodity (acct);

    cur_val = gnc_lot_get_balance (lot);
    *subtotal = gnc_numeric_add (*subtotal, cur_val,
                                 gnc_commodity_get_fraction (currency), GNC_HOW_RND_ROUND_HALF_UP);
}

static gnc_numeric
gnc_payment_dialog_calculate_selected_total (PaymentWindow *pw)
{
    GtkTreeSelection *selection;
    gnc_numeric val = gnc_numeric_zero();

    if (!pw->docs_list_tree_view || !GTK_IS_TREE_VIEW(pw->docs_list_tree_view))
        return gnc_numeric_zero();

    /* Figure out if anything is set in the current list */
    selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(pw->docs_list_tree_view));

    gtk_tree_selection_selected_foreach (selection,
                                         calculate_selected_total_helper,
                                         (gpointer) &val);

    return val;
}

static void
gnc_payment_dialog_document_selection_changed (PaymentWindow *pw)
{
    gnc_numeric val;

    /* Don't change the amount based on the selected documents
     * in case this payment is from a pre-existing txn
     */
    if (gnc_payment_dialog_has_pre_existing_txn (pw))
        return;

    /* Set the payment amount in the dialog */
    val = gnc_payment_dialog_calculate_selected_total (pw);
    gnc_ui_payment_window_set_amount(pw, val);
}

static gint
_gnc_lotinfo_find_by_lot(PreExistLotInfo *lotinfo_inst, GNCLot *lot_to_find)
{
    if (lotinfo_inst->lot == lot_to_find)
        return 0;
    return -1;
}

static void
gnc_payment_dialog_highlight_documents (PaymentWindow *pw)
{
    gboolean selection_changed = FALSE;
    GtkTreeIter iter;
    GtkTreeModel *model = gtk_tree_view_get_model(GTK_TREE_VIEW(pw->docs_list_tree_view));
    GtkTreeSelection *selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(pw->docs_list_tree_view));
    gtk_tree_selection_unselect_all (selection);

    if (gtk_tree_model_get_iter_first (model, &iter))
    {
        do
        {
            GValue value = { 0 };
            GNCLot *lot;
            GList *li_node;

            gtk_tree_model_get_value (model, &iter, 5, &value);
            lot = (GNCLot *) g_value_get_pointer (&value);
            g_value_unset (&value);

            if (!lot)
                continue; /* Lot has been deleted behind our back... */

            li_node = g_list_find_custom (pw->tx_info->lots, lot,
                                            (GCompareFunc)_gnc_lotinfo_find_by_lot);
            if (li_node)
            {
                gtk_tree_selection_select_iter (selection, &iter);
                selection_changed = TRUE;
            }
        }
        while (gtk_tree_model_iter_next (model, &iter));
    }

    if (selection_changed)
        gnc_payment_dialog_document_selection_changed (pw);
}


void
gnc_payment_window_fill_docs_list (PaymentWindow *pw)
{
    GtkListStore *store;
    GtkTreeSelection *selection;
    GList *list = NULL, *node;
    GNCLot *tx_lot = NULL;

    g_return_if_fail (pw->docs_list_tree_view && GTK_IS_TREE_VIEW(pw->docs_list_tree_view));

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

    /* Clear the existing list */
    selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(pw->docs_list_tree_view));
    gtk_tree_selection_unselect_all (selection);
    store = GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(pw->docs_list_tree_view)));
    gtk_list_store_clear(store);

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
        GtkTreeIter iter;
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

        gtk_list_store_append (store, &iter);
        gtk_list_store_set (store, &iter,
                            0, doc_date_time,
                            1, doc_id_str,
                            2, doc_type_str,
                            3, doc_deb_str,
                            4, doc_cred_str,
                            5, (gpointer)lot,
                            -1);

    }

    g_list_free (list);

    /* Highlight the preset invoice if it's in the new list */
    gnc_payment_dialog_highlight_documents (pw);
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

    pw->post_acct = gnc_account_select_combo_fill (pw->post_combo, pw->book, pw->acct_types, pw->acct_commodities);
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
        gtk_container_remove (GTK_CONTAINER (credit_box), pw->amount_debit_edit);
    if (gtk_widget_is_ancestor(pw->amount_credit_edit, debit_box))
        gtk_container_remove (GTK_CONTAINER (debit_box), pw->amount_credit_edit);

    if (!gtk_widget_is_ancestor(pw->amount_debit_edit, debit_box))
        gtk_box_pack_start (GTK_BOX (debit_box), pw->amount_debit_edit, TRUE, TRUE, 0);
    if (!gtk_widget_is_ancestor(pw->amount_credit_edit, credit_box))
        gtk_box_pack_start (GTK_BOX (credit_box), pw->amount_credit_edit, TRUE, TRUE, 0);

    g_object_unref (G_OBJECT (pw->amount_debit_edit));
    g_object_unref (G_OBJECT (pw->amount_credit_edit));

    /* Redo the owner_choice widget */
    if (pw->owner_choice)
        gtk_widget_destroy(pw->owner_choice);
    pw->owner_choice = gnc_owner_select_create (NULL, pw->owner_box, pw->book, &pw->owner);
    gtk_widget_show (pw->owner_choice);
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
gnc_payment_set_owner (PaymentWindow *pw, GncOwner *owner)
{
    gnc_owner_set_owner (pw->owner_choice, owner);
    gnc_payment_dialog_owner_changed(pw);
}


static void
gnc_payment_set_owner_type (PaymentWindow *pw, GncOwnerType owner_type)
{
    gboolean valid;
    GtkTreeModel *store;
    GtkTreeIter iter;

    switch (owner_type)
    {
        case GNC_OWNER_CUSTOMER:
        case GNC_OWNER_EMPLOYEE:
        case GNC_OWNER_VENDOR:
            pw->owner_type = owner_type;
            break;
        default:
            pw->owner_type = GNC_OWNER_CUSTOMER;
    }

    store = gtk_combo_box_get_model (GTK_COMBO_BOX(pw->owner_type_combo));
    valid = gtk_tree_model_get_iter_first (store, &iter);
    while (valid)
    {
        GncOwnerType owner_type;
        gtk_tree_model_get (store, &iter, COL_OWNER_TYPE_NUM, &owner_type, -1);
        if (owner_type == pw->owner_type)
        {
            gtk_combo_box_set_active_iter (GTK_COMBO_BOX(pw->owner_type_combo), &iter);
            break;
        }
        valid = gtk_tree_model_iter_next (store, &iter);
    }

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

static int
gnc_payment_dialog_owner_type_changed_cb (G_GNUC_UNUSED GtkWidget *widget, gpointer data)
{
    PaymentWindow *pw = data;
    GtkTreeIter iter;
    GtkTreeModel *model;
    GncOwnerType owner_type;

    if (!pw) return FALSE;

    gtk_combo_box_get_active_iter (GTK_COMBO_BOX(pw->owner_type_combo), &iter);
    model = gtk_combo_box_get_model (GTK_COMBO_BOX(pw->owner_type_combo));
    gtk_tree_model_get (model, &iter, COL_OWNER_TYPE_NUM, &owner_type, -1);

    if (owner_type != pw->owner_type)
    {
        pw->owner_type = owner_type;

        /* If type changed, the currently selected owner can't be valid any more
         * If the initial owner is of the new owner_type, we propose that one
         * otherwise we just reset the owner
         */
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
            }

        }

        gnc_payment_dialog_owner_type_changed (pw);
    }

    /* Reflect if the payment could complete now */
    gnc_payment_window_check_payment (pw);

    return FALSE;
}

void
gnc_payment_dialog_document_selection_changed_cb (G_GNUC_UNUSED GtkWidget *widget, gpointer data)
{
    PaymentWindow *pw = data;

    if (!pw) return;

    gnc_payment_dialog_document_selection_changed (pw);

    /* Reflect if the payment could complete now */
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

int
gnc_payment_dialog_post_to_changed_cb (G_GNUC_UNUSED GtkWidget *widget, gpointer data)
{
    PaymentWindow *pw = data;
    Account *post_acct;

    if (!pw) return FALSE;

    post_acct = gnc_account_select_combo_get_active (pw->post_combo);

    /* If this post account really changed, then reset ourselves */
    if (post_acct != pw->post_acct)
    {
        pw->post_acct = post_acct;
        gnc_payment_dialog_post_to_changed(pw);
    }
    else
        gnc_payment_dialog_highlight_documents (pw);

    /* Reflect if the payment could complete now */
    gnc_payment_window_check_payment (pw);

    return FALSE;
}

/*
 * This helper function is called once for each row in the tree view
 * that is currently selected.  Its task is to add the corresponding
 * lot to the end of a glist.
 */
static void
get_selected_lots (GtkTreeModel *model,
                   G_GNUC_UNUSED GtkTreePath *path,
                   GtkTreeIter *iter,
                   gpointer data)
{
    GList **return_list = data;
    GNCLot *lot;
    GValue value = { 0 };

    gtk_tree_model_get_value (model, iter, 5, &value);
    lot = (GNCLot *) g_value_get_pointer (&value);
    g_value_unset (&value);

    if (lot)
        *return_list = g_list_insert_sorted (*return_list, lot, (GCompareFunc)gncOwnerLotsSortFunc);
}

void
gnc_payment_ok_cb (G_GNUC_UNUSED GtkWidget *widget, gpointer data)
{
    PaymentWindow *pw = data;
    const char *text = NULL;

    if (!pw)
        return;

    /* The gnc_payment_window_check_payment function
     * ensures we have valid owner, post account, transfer account
     * and amount so we can proceed with the payment.
     * Note: make sure it's called before all entry points to this function !
     */

    /* We're on our way out, stop watching for object changes that could
     * trigger a gui refresh. Without this the gui suspend/resume
     * pair could still trigger a gui update on the payment dialog
     * before we close it. This is undesired because the lots may be in
     * an inconsistent state until after all events are handled. So
     * the gui refresh may result in a crash.
     * See https://bugs.gnucash.org/show_bug.cgi?id=740471
     */
    gnc_gui_component_clear_watches (pw->component_id);

    gnc_suspend_gui_refresh ();
    {
        const char *memo, *num;
        GDate date;
        time64 t;
        gnc_numeric exch = gnc_numeric_create(1, 1); //default to "one to one" rate
        GList *selected_lots = NULL;
        GtkTreeSelection *selection;
        gboolean auto_pay;

        /* Obtain all our ancillary information */
        memo = gtk_entry_get_text (GTK_ENTRY (pw->memo_entry));
        num = gtk_entry_get_text (GTK_ENTRY (pw->num_entry));
        g_date_clear (&date, 1);
        gnc_date_edit_get_gdate (GNC_DATE_EDIT (pw->date_edit), &date);
        t = gdate_to_time64 (date);

        /* Obtain the list of selected lots (documents/payments) from the dialog */
        selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(pw->docs_list_tree_view));
        gtk_tree_selection_selected_foreach (selection, get_selected_lots, &selected_lots);

        /* When the payment amount is 0, the selected documents cancel each other out
         * so no money is actually transferred.
         * For non-zero payments money will be transferred between the post account
         * and the transfer account. In that case if these two accounts don't have
         * the same currency the user is asked to enter the exchange rate.
         */
        if (!gnc_numeric_zero_p (pw->amount_tot) &&
            !gnc_commodity_equal(xaccAccountGetCommodity(pw->xfer_acct), xaccAccountGetCommodity(pw->post_acct)))
        {
            XferDialog* xfer;

            text = _("The transfer and post accounts are associated with different currencies. Please specify the conversion rate.");

            xfer = gnc_xfer_dialog(pw->dialog, pw->post_acct);
            gnc_info_dialog (GTK_WINDOW (pw->dialog), "%s", text);

            gnc_xfer_dialog_select_to_account(xfer, pw->xfer_acct);
            gnc_xfer_dialog_set_amount(xfer, pw->amount_tot);

            /* All we want is the exchange rate so prevent the user from thinking
               it makes sense to mess with other stuff */
            gnc_xfer_dialog_set_from_show_button_active(xfer, FALSE);
            gnc_xfer_dialog_set_to_show_button_active(xfer, FALSE);
            gnc_xfer_dialog_hide_from_account_tree(xfer);
            gnc_xfer_dialog_hide_to_account_tree(xfer);
            gnc_xfer_dialog_is_exchange_dialog(xfer, &exch);
            gnc_xfer_dialog_run_until_done(xfer);
        }

        /* Perform the payment */
        if (gncOwnerGetType (&(pw->owner)) == GNC_OWNER_CUSTOMER)
            auto_pay = gnc_prefs_get_bool (GNC_PREFS_GROUP_INVOICE, GNC_PREF_AUTO_PAY);
        else
            auto_pay = gnc_prefs_get_bool (GNC_PREFS_GROUP_BILL, GNC_PREF_AUTO_PAY);

        gncOwnerApplyPaymentSecs (&pw->owner, &(pw->tx_info->txn), selected_lots,
                                  pw->post_acct, pw->xfer_acct, pw->amount_tot,
                                  exch, t, memo, num, auto_pay);
    }
    gnc_resume_gui_refresh ();

    /* Save the transfer account, xfer_acct */
    gnc_payment_dialog_remember_account(pw, pw->xfer_acct);

    if (gtk_widget_is_sensitive (pw->print_check) &&
        gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(pw->print_check)))
    {
        Split *split = xaccTransFindSplitByAccount (pw->tx_info->txn, pw->xfer_acct);
        GList *splits = NULL;
        splits = g_list_append(splits, split);
        gnc_ui_print_check_dialog_create(NULL, splits);
    }

    gnc_ui_payment_window_destroy (pw);
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
    if (pw->tx_info->lots)
        g_list_free_full (pw->tx_info->lots, g_free);
    g_free (pw);
}

void
gnc_payment_acct_tree_row_activated_cb (GtkWidget *widget, GtkTreePath *path,
                                        G_GNUC_UNUSED GtkTreeViewColumn *column, PaymentWindow *pw)
{
    GtkTreeView *view;
    GtkTreeModel *model;
    GtkTreeIter iter;

    g_return_if_fail(widget);
    view = GTK_TREE_VIEW(widget);

    model = gtk_tree_view_get_model(view);
    if (gtk_tree_model_get_iter(model, &iter, path))
    {
        if (gtk_tree_model_iter_has_child(model, &iter))
        {
            /* There are children,
             * just expand or collapse the row. */
            if (gtk_tree_view_row_expanded(view, path))
                gtk_tree_view_collapse_row(view, path);
            else
                gtk_tree_view_expand_row(view, path, FALSE);
        }
        else if (gnc_payment_window_check_payment (pw))
            /* It's an account without any children
             * If all conditions for a valid payment are met click the Ok button. */
            gnc_payment_ok_cb(widget, pw);
    }
}

void
gnc_payment_leave_amount_cb (G_GNUC_UNUSED GtkWidget *widget,
                             G_GNUC_UNUSED GdkEventFocus *event,
                             PaymentWindow *pw)
{
    gnc_numeric amount_deb, amount_cred, amount_tot;

    if (! pw->amount_credit_edit || ! pw->amount_debit_edit)
        return;

    /* If both credit and debit amount are entered, simplify it to either one */
    amount_deb  = gnc_amount_edit_get_amount (GNC_AMOUNT_EDIT (pw->amount_debit_edit));
    amount_cred = gnc_amount_edit_get_amount (GNC_AMOUNT_EDIT (pw->amount_credit_edit));
    amount_tot = gnc_numeric_sub (amount_cred, amount_deb,
                                  gnc_commodity_get_fraction (xaccAccountGetCommodity (pw->post_acct)),
                                  GNC_HOW_RND_ROUND_HALF_UP);

    gnc_ui_payment_window_set_amount (pw, amount_tot);

    /* Reflect if the payment could complete now */
    gnc_payment_window_check_payment (pw);
}

/* Select the list of accounts to show in the tree */
static void
gnc_payment_set_account_types (GncTreeViewAccount *tree)
{
    AccountViewInfo avi;
    int i;

    gnc_tree_view_account_get_view_info (tree, &avi);

    for (i = 0; i < NUM_ACCOUNT_TYPES; i++)
        avi.include_type[i] = gncBusinessIsPaymentAcctType (i);

    gnc_tree_view_account_set_view_info (tree, &avi);
}

static gboolean
find_handler (G_GNUC_UNUSED gpointer find_data, gpointer user_data)
{
    PaymentWindow *pw = user_data;

    return (pw != NULL);
}

static void print_date (G_GNUC_UNUSED GtkTreeViewColumn *tree_column,
                        GtkCellRenderer *cell,
                        GtkTreeModel *tree_model,
                        GtkTreeIter *iter,
                        G_GNUC_UNUSED gpointer data)
{
    GValue value = { 0 };
    time64 doc_date_time;
    gchar *doc_date_str;

    g_return_if_fail (cell && iter && tree_model);


    gtk_tree_model_get_value (tree_model, iter, 0, &value);
    doc_date_time = (time64) g_value_get_int64 (&value);
    g_value_unset (&value);
    doc_date_str = qof_print_date (doc_date_time);
    g_object_set (G_OBJECT (cell), "text", doc_date_str, NULL);
    g_free (doc_date_str);
}

static PaymentWindow *
new_payment_window (GtkWindow *parent, QofBook *book, InitialPaymentInfo *tx_info)
{
    PaymentWindow *pw;
    GtkBuilder *builder;
    GtkWidget *box;
    GtkTreeSelection *selection;
    GtkTreeViewColumn *column;
    GtkCellRenderer *renderer;
    GtkTreeModel *store;
    GtkTreeIter iter;

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
    gnc_builder_add_from_file (builder, "dialog-payment.glade", "docs_list_hor_adj");
    gnc_builder_add_from_file (builder, "dialog-payment.glade", "docs_list_vert_adj");
    gnc_builder_add_from_file (builder, "dialog-payment.glade", "docs_list_model");
    gnc_builder_add_from_file (builder, "dialog-payment.glade", "post_combo_model");
    gnc_builder_add_from_file (builder, "dialog-payment.glade", "owner_type_combo_model");
    gnc_builder_add_from_file (builder, "dialog-payment.glade", "payment_dialog");
    pw->dialog = GTK_WIDGET (gtk_builder_get_object (builder, "payment_dialog"));
    gtk_window_set_transient_for (GTK_WINDOW(pw->dialog), parent);

    // Set the style context for this dialog so it can be easily manipulated with css
    gnc_widget_set_style_context (GTK_WIDGET(pw->dialog), "GncPaymentDialog");

    /* Grab the widgets and build the dialog */
    pw->payment_warning = GTK_WIDGET (gtk_builder_get_object (builder, "payment_warning"));
    pw->conflict_message = GTK_WIDGET (gtk_builder_get_object (builder, "conflict_message"));
    pw->ok_button = GTK_WIDGET (gtk_builder_get_object (builder, "okbutton"));
    pw->num_entry = GTK_WIDGET (gtk_builder_get_object (builder, "num_entry"));
    pw->memo_entry = GTK_WIDGET (gtk_builder_get_object (builder, "memo_entry"));
    pw->commodity_label = GTK_WIDGET (gtk_builder_get_object (builder, "commodity_label"));
    pw->post_combo = GTK_WIDGET (gtk_builder_get_object (builder, "post_combo"));
    gtk_combo_box_set_entry_text_column( GTK_COMBO_BOX( pw->post_combo ), 0 );
    gnc_cbwe_require_list_item(GTK_COMBO_BOX(pw->post_combo));

    pw->owner_type_combo = GTK_WIDGET (gtk_builder_get_object (builder, "owner_type_combo"));
    /* Add the respective GNC_OWNER_TYPEs to the combo box model
     * ATTENTION: the order here should match the order of the
     * store's entries as set in the glade file !
     */
    store = gtk_combo_box_get_model (GTK_COMBO_BOX(pw->owner_type_combo));
    gtk_tree_model_get_iter_first (store, &iter);
    gtk_list_store_set (GTK_LIST_STORE(store), &iter,
                        COL_OWNER_TYPE_NAME, _("Customer"),
                        COL_OWNER_TYPE_NUM, GNC_OWNER_CUSTOMER, -1);
    gtk_tree_model_iter_next (store, &iter);
    gtk_list_store_set (GTK_LIST_STORE(store), &iter,
                        COL_OWNER_TYPE_NAME, _("Vendor"),
                        COL_OWNER_TYPE_NUM, GNC_OWNER_VENDOR, -1);
    gtk_tree_model_iter_next (store, &iter);
    gtk_list_store_set (GTK_LIST_STORE(store), &iter,
                        COL_OWNER_TYPE_NAME, _("Employee"),
                        COL_OWNER_TYPE_NUM, GNC_OWNER_EMPLOYEE, -1);

    pw->owner_box = GTK_WIDGET (gtk_builder_get_object (builder, "owner_box"));

    pw->amount_refund_box = GTK_WIDGET (gtk_builder_get_object (builder, "amount_refund_box"));
    pw->amount_payment_box = GTK_WIDGET (gtk_builder_get_object (builder, "amount_payment_box"));

    pw->amount_debit_edit = gnc_amount_edit_new ();
    gnc_amount_edit_set_evaluate_on_enter (GNC_AMOUNT_EDIT (pw->amount_debit_edit),
                                           TRUE);
    gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (pw->amount_debit_edit), gnc_numeric_zero());
    g_signal_connect(G_OBJECT(gnc_amount_edit_gtk_entry(GNC_AMOUNT_EDIT(pw->amount_debit_edit))),
                     "focus-out-event",
                     G_CALLBACK(gnc_payment_leave_amount_cb), pw);

    pw->amount_credit_edit = gnc_amount_edit_new ();
    gnc_amount_edit_set_evaluate_on_enter (GNC_AMOUNT_EDIT (pw->amount_credit_edit),
                                           TRUE);
    gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (pw->amount_credit_edit), gnc_numeric_zero());
    g_signal_connect(G_OBJECT(gnc_amount_edit_gtk_entry(GNC_AMOUNT_EDIT(pw->amount_credit_edit))),
                     "focus-out-event",
                     G_CALLBACK(gnc_payment_leave_amount_cb), pw);

    box = GTK_WIDGET (gtk_builder_get_object (builder, "date_box"));
    pw->date_edit = gnc_date_edit_new (time(NULL), FALSE, FALSE);
    gtk_box_pack_start (GTK_BOX (box), pw->date_edit, TRUE, TRUE, 0);
    pw->print_check = GTK_WIDGET (gtk_builder_get_object (builder, "print_check"));

    pw->docs_list_tree_view = GTK_WIDGET (gtk_builder_get_object (builder, "docs_list_tree_view"));
    selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(pw->docs_list_tree_view));
    gtk_tree_selection_set_mode (selection, GTK_SELECTION_MULTIPLE);

    // Set grid lines option to preference
    gtk_tree_view_set_grid_lines (GTK_TREE_VIEW(pw->docs_list_tree_view), gnc_tree_view_get_grid_lines_pref ());

    /* Configure date column */
    renderer = gtk_cell_renderer_text_new ();
    column = gtk_tree_view_get_column (GTK_TREE_VIEW (pw->docs_list_tree_view), 0);
    gtk_tree_view_column_pack_start (column, renderer, TRUE);
    tree_view_column_set_default_width (GTK_TREE_VIEW (pw->docs_list_tree_view),
                                        column, "31-12-2013");
    gtk_tree_view_column_set_cell_data_func (column, renderer,
                                             (GtkTreeCellDataFunc) print_date,
                                             NULL, NULL);

    /* Configure document number column */
    column = gtk_tree_view_get_column (GTK_TREE_VIEW (pw->docs_list_tree_view), 1);
    tree_view_column_set_default_width (GTK_TREE_VIEW (pw->docs_list_tree_view),
                                        column, _("Pre-Payment"));

    /* Configure document type column */
    column = gtk_tree_view_get_column (GTK_TREE_VIEW (pw->docs_list_tree_view), 2);
    tree_view_column_set_default_width (GTK_TREE_VIEW (pw->docs_list_tree_view),
                                        column, _("Credit Note"));

    /* Configure debit column */
    column = gtk_tree_view_get_column (GTK_TREE_VIEW (pw->docs_list_tree_view), 3);
    tree_view_column_set_default_width (GTK_TREE_VIEW (pw->docs_list_tree_view),
                                        column, "9,999,999.00");

    /* Configure credit column */
    column = gtk_tree_view_get_column (GTK_TREE_VIEW (pw->docs_list_tree_view), 4);
    tree_view_column_set_default_width (GTK_TREE_VIEW (pw->docs_list_tree_view),
                                        column, "9,999,999.00");

    gtk_tree_sortable_set_sort_column_id (
        GTK_TREE_SORTABLE (gtk_tree_view_get_model (GTK_TREE_VIEW (pw->docs_list_tree_view))),
        0, GTK_SORT_ASCENDING);


    box = GTK_WIDGET (gtk_builder_get_object (builder, "acct_window"));
    pw->acct_tree = GTK_WIDGET(gnc_tree_view_account_new (FALSE));
    gtk_container_add (GTK_CONTAINER (box), pw->acct_tree);
    gtk_tree_view_set_headers_visible (GTK_TREE_VIEW(pw->acct_tree), FALSE);
    gnc_payment_set_account_types (GNC_TREE_VIEW_ACCOUNT (pw->acct_tree));

    /* Set the dialog for the 'new' owner and owner type.
     * Note that this also sets the post account tree. */
    gncOwnerCopy (&pw->tx_info->owner, &(pw->owner));
    gnc_payment_set_owner_type (pw, gncOwnerGetType (&pw->tx_info->owner));

    /* Setup signals */
    gtk_builder_connect_signals_full( builder,
                                      gnc_builder_connect_full_func,
                                      pw);

    g_signal_connect (G_OBJECT (pw->acct_tree), "row-activated",
                      G_CALLBACK (gnc_payment_acct_tree_row_activated_cb), pw);

    g_signal_connect (G_OBJECT (pw->owner_type_combo), "changed",
                      G_CALLBACK (gnc_payment_dialog_owner_type_changed_cb), pw);

    selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(pw->acct_tree));
    g_signal_connect (G_OBJECT (selection), "changed",
                      G_CALLBACK (gnc_payment_dialog_xfer_acct_changed_cb), pw);


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

    /* Show it all */
    gtk_widget_show_all (pw->dialog);
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

        text = gtk_entry_get_text(GTK_ENTRY (gtk_bin_get_child(GTK_BIN (GTK_COMBO_BOX(pw->post_combo)))));

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
        g_message("No asset splits in txn \"%s\"; cannot use this for assigning a payment.",
                  xaccTransGetDescription(txn));
        return result;
    }

    assetaccount_split = xaccTransGetFirstPaymentAcctSplit(txn);
    amount = xaccSplitGetValue(assetaccount_split);
    result = gnc_numeric_positive_p(amount); // positive amounts == customer
    //g_message("Amount=%s", gnc_numeric_to_string(amount));
    return result;
}

// ///////////////
static char *gen_split_desc (Transaction *txn, Split *split)
{
    gnc_numeric value = xaccSplitGetValue(split);
    Account *xfer_acct = xaccSplitGetAccount(split);
    char *acct_name = gnc_account_get_full_name (xfer_acct);
    const char *action = gnc_get_action_num (txn, split);
    const char *memo = xaccSplitGetMemo (split);
    const char *print_amt = xaccPrintAmount(value, gnc_account_print_info (xfer_acct, TRUE));
    char *split_str = NULL;

    if (action && *action && memo && *memo)
        split_str = g_strdup_printf ("%s: %s (%s, %s)", acct_name, print_amt,
                                        action, memo);
    else if((action && *action) || (memo && *memo))
        split_str = g_strdup_printf ("%s: %s (%s)", acct_name, print_amt,
                                        action ? action : memo);
    else
        split_str = g_strdup_printf ("%s: %s", acct_name, print_amt);
    g_free (acct_name);

    return split_str;
}

static Split *select_payment_split (GtkWindow *parent, Transaction *txn)
{
    /* We require the txn to have one split in an Asset account.
     * The only exception would be a lot link transaction
     */
    GList *payment_splits = xaccTransGetPaymentAcctSplitList (txn);
    if (!payment_splits)
    {
        GtkWidget *dialog;

        if (xaccTransGetTxnType(txn) == TXN_TYPE_LINK)
            return NULL;

        dialog = gtk_message_dialog_new (parent,
                                         GTK_DIALOG_DESTROY_WITH_PARENT,
                                         GTK_MESSAGE_INFO,
                                         GTK_BUTTONS_CLOSE,
                                         "%s",
                                         _("The selected transaction doesn't have splits that can be assigned as a payment"));
        gtk_dialog_run (GTK_DIALOG(dialog));
        gtk_widget_destroy (dialog);
        g_message("No asset splits in txn \"%s\"; cannot use this for assigning a payment.",
                  xaccTransGetDescription(txn));
        return NULL;
    }

    if (g_list_length(payment_splits) > 1)
    {
        Split *selected_split = NULL;
        GList *node;
        GtkWidget *first_rb = NULL;
        int answer = GTK_BUTTONS_OK;
        const char *message = _("While this transaction has multiple splits that can be considered\nas 'the payment split', gnucash only knows how to handle one.\n"
                                "Please select one, the others will be ignored.\n\n");
        GtkDialog *dialog = GTK_DIALOG(
                            gtk_dialog_new_with_buttons (_("Warning"),
                                                         parent,
                                                         GTK_DIALOG_DESTROY_WITH_PARENT,
                                                         _("Continue"), GTK_BUTTONS_OK,
                                                         _("Cancel"), GTK_BUTTONS_CANCEL,
                                                         NULL));
        GtkWidget *content = gtk_dialog_get_content_area(dialog);
        GtkWidget *label = gtk_label_new (message);
        gtk_box_pack_start (GTK_BOX(content), label, FALSE, TRUE, 0);

        /* Add splits as selectable options to the dialog */
        for (node = payment_splits; node; node = node->next)
        {
            GtkWidget *rbutton;
            Split *split = node->data;
            char *split_str = gen_split_desc (txn, split);

            if (node == payment_splits)
            {
                first_rb = gtk_radio_button_new_with_label (NULL, split_str);
                rbutton = first_rb;
            }
            else
                rbutton = gtk_radio_button_new_with_label_from_widget(GTK_RADIO_BUTTON(first_rb), split_str);

            g_object_set_data(G_OBJECT(rbutton), "split", split);
            gtk_box_pack_start (GTK_BOX(content), rbutton, FALSE, FALSE, 0);

            g_free (split_str);
        }

        gtk_dialog_set_default_response (dialog, GTK_BUTTONS_CANCEL);
        gtk_widget_show_all (GTK_WIDGET(dialog));
        answer = gtk_dialog_run (dialog);

        if (answer == GTK_BUTTONS_OK)
        {
            GSList *rbgroup = gtk_radio_button_get_group(GTK_RADIO_BUTTON(first_rb));
            GSList *rbnode;
            for (rbnode = rbgroup; rbnode; rbnode = rbnode->next)
            {
                GtkWidget *rbutton = rbnode->data;
                if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(rbutton)))
                {
                    selected_split = g_object_get_data(G_OBJECT(rbutton), "split");
                    break;
                }
            }
        }

        gtk_widget_destroy (GTK_WIDGET(dialog));
        return selected_split;
    }
    else
        return payment_splits->data;
}

static GList *select_txn_lots (GtkWindow *parent, Transaction *txn, Account **post_acct, gboolean *abort)
{
    gboolean has_no_lot_apar_splits = FALSE;
    SplitList *post_splits = NULL, *no_lot_post_splits = NULL;
    SplitList *iter;
    GList *txn_lots = NULL;

    /* There's no use in continuing if I can't set the post_acct or abort variables */
    if (!post_acct || !abort)
        return NULL;

    *abort = FALSE;
    *post_acct = NULL;

    post_splits = xaccTransGetAPARAcctSplitList (txn, FALSE);
    for (iter = post_splits; iter; iter = iter->next)
    {
        GNCLot *postlot = NULL;
        Split *post_split = iter->data;
        postlot = xaccSplitGetLot (post_split);
        if (postlot)
        {
            PreExistLotInfo *lot_info = g_new0 (PreExistLotInfo, 1);
            lot_info->lot = postlot;
            lot_info->amount = xaccSplitGetValue (post_split);
            txn_lots = g_list_prepend (txn_lots, lot_info);
            *post_acct = xaccSplitGetAccount (post_split);
        }
        else
        {
            /* Make sure not to override post_acct if it was set above from a lot split */
            if (!*post_acct)
                *post_acct = xaccSplitGetAccount (post_split);
            no_lot_post_splits = g_list_prepend (no_lot_post_splits, post_split);
            has_no_lot_apar_splits = TRUE;
        }
    }

    /* If the txn has both APAR splits linked to a business lot and
     * splits that are not, issue a warning some will be discarded.
     */
    if (has_no_lot_apar_splits && (g_list_length (txn_lots) > 0))
    {
        GtkWidget *dialog;
        char *split_str = g_strdup ("");
        for (iter = no_lot_post_splits; iter; iter = iter->next)
        {
            Split *post_split = iter->data;
            char *tmp_str = gen_split_desc (txn, post_split);
            char *tmp_str2 = g_strconcat(split_str, "• ", tmp_str, "\n", NULL);
            g_free (tmp_str);
            g_free (split_str);
            split_str = tmp_str2;
        }

        dialog = gtk_message_dialog_new (parent,
                                         GTK_DIALOG_DESTROY_WITH_PARENT,
                                         GTK_MESSAGE_WARNING,
                                         GTK_BUTTONS_CANCEL,
                                         _("The transaction has at least one split in a business account that is not part of a business transaction.\n"
                                         "If you continue these splits will be ignored:\n\n%s\n"
                                         "Do you wish to continue and ignore these splits ?"),
                                         split_str);
        gtk_dialog_add_buttons (GTK_DIALOG(dialog),
                                _("Continue"), GTK_BUTTONS_OK, NULL);
        gtk_dialog_set_default_response (GTK_DIALOG(dialog), GTK_BUTTONS_CANCEL);
        if (gtk_dialog_run (GTK_DIALOG(dialog)) != GTK_BUTTONS_OK)
        {
            *abort = TRUE;
            g_list_free_full (txn_lots, g_free);
            txn_lots = NULL;
        }
        gtk_widget_destroy (dialog);
        g_free (split_str);
    }

    return txn_lots;
}

PaymentWindow * gnc_ui_payment_new_with_txn (GtkWindow* parent, GncOwner *owner, Transaction *txn)
{
    Split *payment_split = NULL;
    Account *post_acct = NULL;
    InitialPaymentInfo *tx_info = NULL;
    GList *txn_lots = NULL;
    gboolean abort = FALSE;
    PaymentWindow *pw;

    if (!txn)
        return NULL;

    if (!xaccTransGetSplitList(txn))
        return NULL;

    /* We require the txn to have one split in an Asset account.
     * The only exception would be a lot link transaction
     */
    payment_split = select_payment_split (parent, txn);
    if (!payment_split && (xaccTransGetTxnType(txn) != TXN_TYPE_LINK))
        return NULL;

    /* Get all APAR related lots. Watch out: there might be none */
    txn_lots = select_txn_lots (parent, txn, &post_acct, &abort);
    if (abort)
        return NULL;

    // Fill in the values from the given txn
    tx_info = g_new0(InitialPaymentInfo, 1);
    tx_info->txn = txn;
    tx_info->post_acct = post_acct;
    tx_info->lots = txn_lots;
    gncOwnerCopy (owner, &tx_info->owner);

    pw = new_payment_window (parent,
                            qof_instance_get_book(QOF_INSTANCE(txn)),
                            tx_info);

    gnc_ui_payment_window_set_num(pw, gnc_get_num_action (txn, payment_split));
    gnc_ui_payment_window_set_memo(pw, xaccTransGetDescription(txn));
    {
        GDate txn_date = xaccTransGetDatePostedGDate (txn);
        gnc_ui_payment_window_set_date(pw, &txn_date);
    }
    gnc_ui_payment_window_set_amount(pw, xaccSplitGetValue(payment_split));
    if (payment_split)
        gnc_ui_payment_window_set_xferaccount(pw, xaccSplitGetAccount(payment_split));
    return pw;
}
