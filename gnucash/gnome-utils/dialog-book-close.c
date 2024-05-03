/********************************************************************\
 * dialog-book-close.c -- dialog for helping the user close the     *
 *                        book at the end of the year by adding     *
 *                        zero-izing splits to all Income and       *
 *                        Expense accounts                          *
 *                                                                  *
 * Copyright (C) 2007-8 Derek Atkins <derek@ihtfp.com>              *
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
#include "gnc-engine.h"
#include "Transaction.h"
#include "Split.h"
#include "Account.h"
#include "gnc-ui.h"
#include "gnc-gui-query.h"
#include "dialog-book-close.h"
#include "gnc-account-sel.h"
#include "gnc-component-manager.h"
#include "gnc-date-edit.h"
#include "gnc-session.h"
#include "gnc-ui-util.h"

#define DIALOG_BOOK_CLOSE_CM_CLASS "dialog-book-close"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

struct CloseBookWindow
{
    /* Passed in by the creator */
    QofBook* book;

    /* Parts of the dialog */
    GtkWidget* window;
    GtkWidget* close_date_widget;
    GtkWidget* income_acct_widget;
    GtkWidget* expense_acct_widget;
    GtkWidget* desc_widget;

    /* The final settings */
    time64 close_date;
    const char* desc;

    /* Component registration */
    gint component_manager_id;
};

struct CloseAccountsCB
{
    struct CloseBookWindow* cbw;
    Account* base_acct;
    GNCAccountType acct_type;
    GHashTable* txns;
    guint hash_size;
};

struct CACBTransactionList
{
    gnc_commodity* cmdty;
    Transaction* txn;
    gnc_numeric total;
};

static struct CACBTransactionList*
find_or_create_txn (struct CloseAccountsCB* cacb, gnc_commodity* cmdty)
{
    struct CACBTransactionList* txn;

    g_return_val_if_fail (cacb, NULL);
    g_return_val_if_fail (cmdty, NULL);

    txn = g_hash_table_lookup (cacb->txns, cmdty);
    if (!txn)
    {
        txn = g_new0(struct CACBTransactionList, 1);
        txn->cmdty = cmdty;
        txn->total = gnc_numeric_zero();
        txn->txn = xaccMallocTransaction (cacb->cbw->book);
        xaccTransBeginEdit (txn->txn);
        xaccTransSetDateEnteredSecs (txn->txn, gnc_time (NULL));
        xaccTransSetDatePostedSecsNormalized (txn->txn, cacb->cbw->close_date);

        xaccTransSetDescription (txn->txn, cacb->cbw->desc);
        xaccTransSetCurrency (txn->txn, cmdty);
        xaccTransSetIsClosingTxn (txn->txn, TRUE);
        g_hash_table_insert (cacb->txns, cmdty, txn);
    }
    return txn;
}

/* Make sure that the account is of the correct type.
 * then make sure the account has a balance as of the closing date.
 * then get the account commodity and find the appropriate
 * balancing transaction for that commodity and add this balance
 * to it.
 */
static void close_accounts_cb (Account *a, gpointer data)
{
    struct CloseAccountsCB* cacb = data;
    struct CACBTransactionList* txn;
    gnc_commodity* acct_commodity;
    Split* split;
    gnc_numeric bal;

    g_return_if_fail (a);
    g_return_if_fail (cacb);
    g_return_if_fail (cacb->cbw);
    g_return_if_fail (cacb->txns);

    if (cacb->acct_type != xaccAccountGetType (a))
        return;

    bal = xaccAccountGetBalanceAsOfDate (a, gnc_time64_get_day_end (cacb->cbw->close_date));
    if (gnc_numeric_zero_p (bal))
        return;

    acct_commodity = gnc_account_or_default_currency (a, NULL);
    g_assert (acct_commodity);

    txn = find_or_create_txn (cacb, acct_commodity);
    g_assert (txn);

    split = xaccMallocSplit (cacb->cbw->book);
    xaccSplitSetParent (split, txn->txn);
    xaccAccountBeginEdit (a);
    xaccAccountInsertSplit (a, split);
    xaccSplitSetBaseValue (split, gnc_numeric_neg(bal), acct_commodity);
    xaccAccountCommitEdit (a);
    txn->total = gnc_numeric_add (txn->total, bal, GNC_DENOM_AUTO,
                                  GNC_HOW_DENOM_FIXED | GNC_HOW_RND_NEVER);
}


static void finish_txn_cb (gnc_commodity* cmdty,
                           struct CACBTransactionList* txn,
                           struct CloseAccountsCB* cacb)
{
    Account* acc;
    Split* split;

    g_return_if_fail (cmdty);
    g_return_if_fail (txn);
    g_return_if_fail (cacb);
    g_return_if_fail (cacb->hash_size);

    /* If we only have one currency and the base account uses
     * that currency, then we can use that account.  Otherwise,
     * create a subaccount for each currency.
     */
    if (cacb->hash_size == 1 &&
            gnc_commodity_equal (cmdty, xaccAccountGetCommodity (cacb->base_acct)))
        acc = cacb->base_acct;
    else
    {
        /* See if we already have an account by that name */
        acc = gnc_account_lookup_by_name (cacb->base_acct,
                                          gnc_commodity_get_mnemonic (cmdty));

        /* If not, then create one */
        if (!acc)
        {
            acc = xaccMallocAccount (cacb->cbw->book);
            xaccAccountBeginEdit (acc);
            xaccAccountSetType (acc, ACCT_TYPE_EQUITY);
            xaccAccountSetName (acc, gnc_commodity_get_mnemonic (cmdty));
            xaccAccountSetDescription (acc, gnc_commodity_get_mnemonic (cmdty));
            xaccAccountSetCommodity (acc, cmdty);
            gnc_account_append_child (cacb->base_acct, acc);
            xaccAccountCommitEdit (acc);
        }
    }
    /* Make sure the account exists and is of the correct commodity */
    g_assert (acc);
    g_assert (gnc_commodity_equal (cmdty, xaccAccountGetCommodity (acc)));

    /* Create the split for the Equity account to balance out
     * all the accounts of this.  Use the "total".
     */
    split = xaccMallocSplit (cacb->cbw->book);
    xaccSplitSetParent (split, txn->txn);
    xaccAccountBeginEdit (acc);
    xaccAccountInsertSplit (acc, split);
    xaccSplitSetBaseValue (split, txn->total, cmdty);
    xaccAccountCommitEdit (acc);
    xaccTransCommitEdit (txn->txn);
}

static void close_accounts_of_type (struct CloseBookWindow* cbw,
                                    Account* acct,
                                    GNCAccountType acct_type)
{
    struct CloseAccountsCB cacb;
    Account* root_acct;

    g_return_if_fail (cbw);
    g_return_if_fail (acct);

    cacb.cbw = cbw;
    cacb.base_acct = acct;
    cacb.acct_type = acct_type;
    cacb.txns = g_hash_table_new_full (g_direct_hash,
                                       (GEqualFunc)gnc_commodity_equal,
                                       NULL, g_free);

    /* Iterate through all accounts and set up the balancing splits */
    root_acct = gnc_book_get_root_account (cbw->book);
    gnc_account_foreach_descendant (root_acct, close_accounts_cb, &cacb);

    /* now iterate through the transactions and handle each currency */
    cacb.hash_size = g_hash_table_size (cacb.txns);
    if (cacb.hash_size)
        g_hash_table_foreach (cacb.txns, (GHFunc)finish_txn_cb, &cacb);

    /* Destroy the table, freeing the used memory */
    g_hash_table_destroy (cacb.txns);
}

static void
close_handler (gpointer user_data)
{
    struct CloseBookWindow *cbw = user_data;

    gtk_window_destroy (GTK_WINDOW(cbw->window));
}

static void
destroy_cb (GObject *object, gpointer user_data)
{
    struct CloseBookWindow *cbw = user_data;

    if (cbw->component_manager_id)
    {
        gnc_unregister_gui_component (cbw->component_manager_id);
        cbw->component_manager_id = 0;
    }
    g_free (cbw);
}

static void
help_button_cb (GtkWidget *widget, gpointer user_data)
{
    struct CloseBookWindow *cbw = user_data;
    gnc_gnome_help (GTK_WINDOW(cbw->window), DF_MANUAL, DL_CLOSE_BOOK);
}

static void
cancel_button_cb (GtkWidget *widget, gpointer user_data)
{
    struct CloseBookWindow *cbw = user_data;
    gtk_window_destroy (GTK_WINDOW(cbw->window));
}

static void
ok_button_cb (GtkWidget *widget, gpointer user_data)
{
    struct CloseBookWindow *cbw = user_data;
    Account* income_acct;
    Account* expense_acct;

    cbw->close_date = gnc_date_edit_get_date (GNC_DATE_EDIT(cbw->close_date_widget));
    cbw->desc = gnc_entry_get_text (GTK_ENTRY(cbw->desc_widget));

    income_acct = gnc_account_sel_get_account (GNC_ACCOUNT_SEL(cbw->income_acct_widget));
    expense_acct = gnc_account_sel_get_account (GNC_ACCOUNT_SEL(cbw->expense_acct_widget));

    if (!income_acct)
    {
        gnc_error_dialog (GTK_WINDOW (cbw->window), "%s",
                          _("Please select an Equity account to hold the total Period Income."));
        return;
    }

    if (!expense_acct)
    {
        gnc_error_dialog (GTK_WINDOW (cbw->window), "%s",
                          _("Please select an Equity account to hold the total Period Expense."));
        return;
    }

    gnc_suspend_gui_refresh();
    close_accounts_of_type (cbw, income_acct, ACCT_TYPE_INCOME);
    close_accounts_of_type (cbw, expense_acct, ACCT_TYPE_EXPENSE);
    gnc_resume_gui_refresh();

    gtk_window_destroy (GTK_WINDOW(cbw->window));
}

static gboolean
dialog_key_press_cb (GtkEventControllerKey *key, guint keyval,
                     guint keycode, GdkModifierType state,
                     gpointer user_data)
{
    struct CloseBookWindow *cbw = user_data;

    if (keyval == GDK_KEY_Escape)
    {
        if (gnc_ok_to_close_window (GTK_WIDGET(cbw->window)))
            close_handler (cbw);

        return TRUE;
    }
    else
        return FALSE;
}

void gnc_ui_close_book (QofBook* book, GtkWindow *parent)
{
    struct CloseBookWindow *cbw;
    GtkBuilder* builder;
    GtkWidget* box;
    GList* equity_list = NULL;
    GtkWidget *button;

    g_return_if_fail(book);

    cbw = g_new0 (struct CloseBookWindow, 1);
    g_return_if_fail(cbw);
    cbw->book = book;

    /* Open the dialog */
    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-book-close.ui", "close_book_window");
    cbw->window = GTK_WIDGET(gtk_builder_get_object (builder,  "close_book_window"));

    // Set the name for this dialog so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(cbw->window), "gnc-id-book-close");

    /* parent */
    if (parent != NULL)
        gtk_window_set_transient_for (GTK_WINDOW(cbw->window), GTK_WINDOW(parent));

    PINFO("Closed Book Window is %p, Dialog is %p", cbw, cbw->window);

    /* close date */
    box = GTK_WIDGET(gtk_builder_get_object (builder,  "date_box"));
    cbw->close_date_widget = gnc_date_edit_new (gnc_time (NULL), FALSE, FALSE);
    gtk_box_append (GTK_BOX(box), GTK_WIDGET(cbw->close_date_widget));

    /* income acct */
    equity_list = g_list_prepend (equity_list, GINT_TO_POINTER(ACCT_TYPE_EQUITY));
    box = GTK_WIDGET(gtk_builder_get_object (builder, "income_acct_box"));
    cbw->income_acct_widget = gnc_account_sel_new();
    gnc_account_sel_set_acct_filters (GNC_ACCOUNT_SEL(cbw->income_acct_widget),
                                      equity_list, NULL);
    gnc_account_sel_set_new_account_ability (GNC_ACCOUNT_SEL(cbw->income_acct_widget), TRUE);
    gtk_box_append (GTK_BOX(box), GTK_WIDGET(cbw->income_acct_widget));

    /* expense acct */
    box = GTK_WIDGET(gtk_builder_get_object (builder, "expense_acct_box"));
    cbw->expense_acct_widget = gnc_account_sel_new();
    gnc_account_sel_set_acct_filters (GNC_ACCOUNT_SEL(cbw->expense_acct_widget),
                                      equity_list, NULL);
    gnc_account_sel_set_new_account_ability (GNC_ACCOUNT_SEL(cbw->expense_acct_widget), TRUE);
    gtk_box_append (GTK_BOX(box), GTK_WIDGET(cbw->expense_acct_widget));

    /* desc */
    cbw->desc_widget = GTK_WIDGET(gtk_builder_get_object (builder, "desc_entry"));

    button = GTK_WIDGET(gtk_builder_get_object (builder, "helpbutton"));
    g_signal_connect (G_OBJECT(button), "clicked",
                      G_CALLBACK(help_button_cb), cbw);
    button = GTK_WIDGET(gtk_builder_get_object (builder, "cancelbutton"));
    g_signal_connect (G_OBJECT(button), "clicked",
                      G_CALLBACK(cancel_button_cb), cbw);

    /* default to 'cancel' button */
    gtk_window_set_default_widget (GTK_WINDOW(cbw->window),
                                   GTK_WIDGET(button)); //FIXME gtk4, may not work

    button = GTK_WIDGET(gtk_builder_get_object (builder, "okbutton"));
    g_signal_connect (G_OBJECT(button), "clicked",
                      G_CALLBACK(ok_button_cb), cbw);

    GtkEventController *event_controller_window = gtk_event_controller_key_new ();
    gtk_widget_add_controller (GTK_WIDGET(cbw->window), event_controller_window);
    g_signal_connect (G_OBJECT(event_controller_window),
                      "key-pressed",
                      G_CALLBACK(dialog_key_press_cb), cbw);

    /* Register dialog with component manager */
    cbw->component_manager_id = gnc_register_gui_component (
                                   DIALOG_BOOK_CLOSE_CM_CLASS,
                                   NULL, close_handler,
                                   cbw->window);
    gnc_gui_component_set_session (cbw->component_manager_id,
                                   gnc_get_current_session());

    g_signal_connect (G_OBJECT(cbw->window), "destroy",
                      G_CALLBACK(destroy_cb), cbw);

    g_object_unref (G_OBJECT(builder));

    gtk_widget_set_visible (GTK_WIDGET(cbw->window), TRUE);

    g_list_free (equity_list);
}

