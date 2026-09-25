/********************************************************************\
 * window-autoclear.c -- the autoclear window                       *
 * Copyright (C) 2010 Cristian KLEIN                                *
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
#include "gnc-amount-edit.h"
#include "gnc-component-manager.h"
#include "gnc-session.h"
#include "gnc-event.h"
#include "gnc-gnome-utils.h"
#include "gnc-main-window.h"
#include "gnc-plugin-page-register.h"
#include "gnc-ui.h"
#include "gnc-autoclear.h"
#include "window-autoclear.h"

#define WINDOW_AUTOCLEAR_CM_CLASS "window-autoclear"

__attribute__((unused)) static QofLogModule log_module = GNC_MOD_GUI;

/** STRUCTS *********************************************************/
struct _AutoClearWindow
{
    QofBook *book;
    GncGUID account_guid;
    gint component_id;

    GtkWindow *window;
    GNCAmountEdit *end_value;
    GtkButton *ok_button;
    GtkButton *cancel_button;
    GtkCheckButton *show_cleared_splits_button;
    GtkLabel *status_label;
};

static void gnc_autoclear_window_ok_cb (GtkWidget *widget,
                                        AutoClearWindow *data);
static void gnc_autoclear_window_cancel_cb (GtkWidget *widget,
                                            AutoClearWindow *data);
static void autoclear_window_destroy (AutoClearWindow *data);

/********************************************************************\
 * gnc_ui_autoclear_window_raise                                    *
 *   shows and raises an auto-clear window                          *
 *                                                                  *
 * Args:   autoClearData - the auto-clear window structure          *
\********************************************************************/
void
gnc_ui_autoclear_window_raise(AutoClearWindow * autoClearData)
{
    if (autoClearData == NULL)
        return;

    if (autoClearData->window == NULL)
        return;

    gtk_window_present(GTK_WINDOW(autoClearData->window));
}

static char *
gnc_autoclear_make_window_name(Account *account)
{
    char *fullname;
    char *title;

    fullname = gnc_account_get_full_name(account);
    title = g_strconcat(fullname, " - ", _("Auto-clear"), NULL);

    g_free(fullname);

    return title;
}

static void
show_cleared_splits (QofBook *book, GList *splits)
{
    GNCLedgerDisplay *ledger;
    GncPluginPage *page;
    Query *book_query, *guid_query;

    book_query = qof_query_create_for (GNC_ID_SPLIT);
    guid_query = qof_query_create_for (GNC_ID_SPLIT);
    qof_query_set_book (book_query, book);

    for (GList *iter = splits; iter; iter = iter->next)
    {
        GncGUID guid = iter->data ? *xaccSplitGetGUID (iter->data) : *guid_null() ;
        xaccQueryAddGUIDMatch (guid_query, &guid, GNC_ID_SPLIT, QOF_QUERY_OR);
    }
    book_query = qof_query_merge (book_query, guid_query, QOF_QUERY_AND);
    ledger = gnc_ledger_display_query (book_query, SEARCH_LEDGER, REG_STYLE_JOURNAL);
    gnc_ledger_display_refresh (ledger);
    page = gnc_plugin_page_register_new_ledger (ledger);
    main_window_update_page_name (page, _("Cleared Transactions"));
    gnc_main_window_open_page (NULL, page);
    qof_query_destroy (book_query);
    qof_query_destroy (guid_query);
}

static Account *
autoclear_window_get_account (AutoClearWindow *data)
{
    if (!data || data->book != gnc_get_current_book ())
        return NULL;

    return xaccAccountLookup (&data->account_guid, data->book);
}


static void
autoclear_window_destroy (AutoClearWindow *data)
{
    if (data && data->window)
        gtk_window_destroy (data->window);
}


static void
autoclear_window_destroyed_cb (GtkWidget *widget, AutoClearWindow *data)
{
    gint component_id;

    (void)widget;
    if (!data)
        return;

    data->window = NULL;
    component_id = data->component_id;
    data->component_id = 0;
    if (component_id)
        gnc_unregister_gui_component (component_id);

    g_free (data);
}


static gboolean
autoclear_window_close_request_cb (GtkWindow *window, AutoClearWindow *data)
{
    (void)window;
    (void)data;
    return FALSE;
}


static gboolean
autoclear_window_key_pressed_cb (GtkEventControllerKey *controller,
                                 guint keyval, guint keycode,
                                 GdkModifierType state,
                                 AutoClearWindow *data)
{
    (void)controller;
    (void)keycode;
    (void)state;

    if (keyval != GDK_KEY_Escape)
        return FALSE;

    autoclear_window_destroy (data);
    return TRUE;
}


static void
autoclear_window_close_handler (gpointer user_data)
{
    autoclear_window_destroy (user_data);
}


static void
autoclear_window_refresh_handler (GHashTable *changes, gpointer user_data)
{
    AutoClearWindow *data = user_data;

    (void)changes;
    if (!autoclear_window_get_account (data))
        autoclear_window_destroy (data);
}


static void
gnc_autoclear_window_ok_cb (GtkWidget *widget, AutoClearWindow *data)
{
    Account *account;
    GList *toclear_list = NULL;
    gnc_numeric toclear_value = gnc_numeric_error (GNC_ERROR_ARG);
    GError *error = NULL;

    (void)widget;
    g_return_if_fail (data);

    account = autoclear_window_get_account (data);
    if (!account)
    {
        autoclear_window_destroy (data);
        return;
    }

    /* Test for a valid value. */
    if (gnc_amount_edit_evaluate (data->end_value, &error))
    {
        toclear_value = gnc_amount_edit_get_amount (data->end_value);

        if (gnc_reverse_balance (account))
            toclear_value = gnc_numeric_neg (toclear_value);

        toclear_value = gnc_numeric_convert (
            toclear_value, xaccAccountGetCommoditySCU (account),
            GNC_HOW_RND_ROUND);

#define MAX_AUTOCLEAR_SECONDS 5
        toclear_list = gnc_account_get_autoclear_splits (
            account, toclear_value, INT64_MAX, &error, MAX_AUTOCLEAR_SECONDS);
    }

    if (error && error->message)
    {
        GtkWidget *entry = gnc_amount_edit_gtk_entry (data->end_value);

        gtk_label_set_text (data->status_label, error->message);
        if (gnc_numeric_check (toclear_value) == 0)
            gnc_amount_edit_set_amount (data->end_value, toclear_value);
        gtk_widget_grab_focus (entry);
        gnc_amount_edit_select_region (data->end_value, 0, -1);
        g_error_free (error);
        return;
    }

    xaccAccountBeginEdit (account);
    for (GList *node = toclear_list; node; node = node->next)
        xaccSplitSetReconcile (node->data, CREC);
    xaccAccountCommitEdit (account);

    if (gtk_check_button_get_active (data->show_cleared_splits_button))
        show_cleared_splits (data->book, toclear_list);

    g_list_free (toclear_list);
    autoclear_window_destroy (data);
}


static void
gnc_autoclear_window_cancel_cb (GtkWidget *widget, AutoClearWindow *data)
{
    (void)widget;
    autoclear_window_destroy (data);
}


static void
clear_status_label_cb (GtkEditable *editable, AutoClearWindow *data)
{
    (void)editable;
    gtk_label_set_text (data->status_label, "");
}


/********************************************************************\
 * autoClearWindow                                                  *
 *   opens up the window to auto-clear an account                   *
 *                                                                  *
 * Args:   parent  - the parent of this window                      *
 *         account - the account to auto-clear                      *
 * Return: autoClearData - the instance of this AutoClearWindow     *
\********************************************************************/
AutoClearWindow *
autoClearWindow (GtkWidget *parent, Account *account)
{
    GtkBox *box;
    GtkWidget *label;
    GtkBuilder *builder;
    GtkEventController *key_controller;
    AutoClearWindow *data;
    char *title;
    gnc_numeric after;
    GNCPrintAmountInfo print_info;
    gnc_commodity *currency;

    g_return_val_if_fail (account, NULL);

    data = g_new0 (AutoClearWindow, 1);
    data->book = gnc_account_get_book (account);
    data->account_guid = *xaccAccountGetGUID (account);

    builder = gtk_builder_new ();
    if (!gnc_builder_add_from_file (builder, "window-autoclear.glade",
                                    "auto_clear_start_dialog"))
    {
        g_object_unref (builder);
        g_free (data);
        return NULL;
    }

    data->window = GTK_WINDOW (gtk_builder_get_object (
        builder, "auto_clear_start_dialog"));
    data->ok_button = GTK_BUTTON (gtk_builder_get_object (builder, "ok_button"));
    data->cancel_button = GTK_BUTTON (gtk_builder_get_object (
        builder, "cancel_button"));
    data->show_cleared_splits_button = GTK_CHECK_BUTTON (
        gtk_builder_get_object (builder, "show_cleared_splits_button"));
    data->status_label = GTK_LABEL (gtk_builder_get_object (builder,
                                                             "status_label"));
    box = GTK_BOX (gtk_builder_get_object (builder, "end_value_box"));
    label = GTK_WIDGET (gtk_builder_get_object (builder, "end_label"));

    if (!data->window || !data->ok_button || !data->cancel_button ||
        !data->show_cleared_splits_button || !data->status_label || !box ||
        !label)
    {
        g_object_unref (builder);
        g_free (data);
        return NULL;
    }

    title = gnc_autoclear_make_window_name (account);
    gtk_window_set_title (data->window, title);
    g_free (title);
    gtk_widget_set_name (GTK_WIDGET (data->window), "gnc-id-auto-clear");

    data->end_value = GNC_AMOUNT_EDIT (gnc_amount_edit_new ());
    currency = xaccAccountGetCommodity (account);
    print_info = gnc_commodity_print_info (currency, FALSE);
    gnc_amount_edit_set_print_info (data->end_value, print_info);
    gnc_amount_edit_set_fraction (data->end_value,
                                  gnc_commodity_get_fraction (currency));
    gtk_box_append (box, GTK_WIDGET (data->end_value));
    gnc_amount_edit_make_mnemonic_target (data->end_value, label);

    after = xaccAccountGetClearedBalance (account);
    if (gnc_reverse_balance (account))
        after = gnc_numeric_neg (after);
    gnc_amount_edit_set_amount (data->end_value, after);
    gtk_widget_grab_focus (GTK_WIDGET (data->end_value));
    gnc_amount_edit_select_region (data->end_value, 0, -1);

    g_signal_connect (data->end_value, "activate",
                      G_CALLBACK (gnc_autoclear_window_ok_cb), data);
    g_signal_connect (data->end_value, "changed",
                      G_CALLBACK (clear_status_label_cb), data);
    g_signal_connect (data->ok_button, "clicked",
                      G_CALLBACK (gnc_autoclear_window_ok_cb), data);
    g_signal_connect (data->cancel_button, "clicked",
                      G_CALLBACK (gnc_autoclear_window_cancel_cb), data);
    g_signal_connect (data->window, "close-request",
                      G_CALLBACK (autoclear_window_close_request_cb), data);
    g_signal_connect (data->window, "destroy",
                      G_CALLBACK (autoclear_window_destroyed_cb), data);

    key_controller = gtk_event_controller_key_new ();
    gtk_event_controller_set_propagation_phase (key_controller, GTK_PHASE_CAPTURE);
    gtk_widget_add_controller (GTK_WIDGET (data->window), key_controller);
    g_signal_connect (key_controller, "key-pressed",
                      G_CALLBACK (autoclear_window_key_pressed_cb), data);
    if (parent && GTK_IS_WINDOW (parent))
        gtk_window_set_transient_for (data->window, GTK_WINDOW (parent));

    data->component_id = gnc_register_gui_component (
        WINDOW_AUTOCLEAR_CM_CLASS, autoclear_window_refresh_handler,
        autoclear_window_close_handler, data);
    gnc_gui_component_set_session (data->component_id, gnc_get_current_session ());
    gnc_gui_component_watch_entity_type (data->component_id, GNC_ID_ACCOUNT,
                                         QOF_EVENT_MODIFY | QOF_EVENT_DESTROY);

    g_object_unref (builder);
    return data;
}
