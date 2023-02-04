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
    Account *account;        /* The account that we are auto-clearing */

    gint component_id;       /* id of component                       */

    GtkWidget *window;       /* The auto-clear window                 */
    GNCAmountEdit *end_value;/* The ending value                      */
    GtkWidget *ok_button;
    GtkWidget *cancel_button;
    GtkWidget *show_cleared_splits_button;
    GtkLabel *status_label;
};

/** Callback prototypes************************************************/
void gnc_autoclear_window_ok_cb     (GtkWidget *widget,
                                     AutoClearWindow *data);
void gnc_autoclear_window_cancel_cb (GtkWidget *widget,
                                     AutoClearWindow *data);

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
show_cleared_splits (GList *splits)
{
    GNCLedgerDisplay *ledger;
    GncPluginPage *page;
    Query *book_query, *guid_query;

    book_query = qof_query_create_for (GNC_ID_SPLIT);
    guid_query = qof_query_create_for (GNC_ID_SPLIT);
    qof_query_set_book (book_query, gnc_get_current_book ());

    for (GList *iter = splits; iter; iter = iter->next)
    {
        GncGUID guid = xaccSplitReturnGUID (iter->data);
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

void
gnc_autoclear_window_ok_cb (GtkWidget *widget,
                            AutoClearWindow *data)
{
    GList *toclear_list = NULL;
    gnc_numeric toclear_value;
    gchar *errmsg = NULL;
    GError* error = NULL;

    g_return_if_fail (widget && data);

    /* test for valid value */
    if (!gnc_amount_edit_evaluate (GNC_AMOUNT_EDIT(data->end_value), &error))
    {
        errmsg = g_strdup (error->message);
        g_error_free (error);
    }
    else
    {
        toclear_value = gnc_amount_edit_get_amount(data->end_value);

        if (gnc_reverse_balance(data->account))
            toclear_value = gnc_numeric_neg (toclear_value);

        toclear_value = gnc_numeric_convert
            (toclear_value, xaccAccountGetCommoditySCU(data->account), GNC_HOW_RND_ROUND);

        toclear_list = gnc_account_get_autoclear_splits
            (data->account, toclear_value, &errmsg);
    }

    if (errmsg)
    {
        GtkWidget *entry = gnc_amount_edit_gtk_entry (GNC_AMOUNT_EDIT(data->end_value));
        gtk_label_set_text (data->status_label, errmsg);
        if (gnc_numeric_check (toclear_value) == 0)
            gnc_amount_edit_set_amount (data->end_value, toclear_value);
        gtk_widget_grab_focus (GTK_WIDGET(entry));
        gnc_amount_edit_select_region (GNC_AMOUNT_EDIT(data->end_value), 0, -1);
        g_free (errmsg);
    }
    else
    {
        xaccAccountBeginEdit (data->account);
        for (GList *node = toclear_list; node; node = node->next)
            xaccSplitSetReconcile (node->data, CREC);
        xaccAccountCommitEdit (data->account);

        if (gtk_toggle_button_get_active
            (GTK_TOGGLE_BUTTON (data->show_cleared_splits_button)))
            show_cleared_splits (toclear_list);

        g_list_free (toclear_list);

        /* Close window */
        gtk_widget_destroy (data->window);
        g_free (data);
    }
}

void
gnc_autoclear_window_cancel_cb (GtkWidget *widget,
                                AutoClearWindow *data)
{
    /* Close window */
    gtk_widget_destroy(data->window);
    g_free(data);
}

static void clear_status_label_cb (GtkEditable *editable, AutoClearWindow *data)
{
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
    AutoClearWindow *data;
    char *title;
    gnc_numeric after;
    GNCPrintAmountInfo print_info;
    gnc_commodity *currency;

    data = g_new0 (AutoClearWindow, 1);
    data->account = account;

    /* Create the dialog box */
    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "window-autoclear.glade", "auto_clear_start_dialog");
    data->window = GTK_WIDGET(gtk_builder_get_object (builder, "auto_clear_start_dialog"));
    title = gnc_autoclear_make_window_name (account);
    gtk_window_set_title(GTK_WINDOW(data->window), title);
    g_free (title);

    // Set the name for this dialog so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(data->window), "gnc-id-auto-clear");

    data->show_cleared_splits_button =
        GTK_WIDGET (gtk_builder_get_object (builder, "show_cleared_splits_button"));

    /* Add amount edit box */
    data->end_value = GNC_AMOUNT_EDIT(gnc_amount_edit_new());

    currency = xaccAccountGetCommodity (account);
    print_info = gnc_commodity_print_info (currency, FALSE);
    gnc_amount_edit_set_print_info (GNC_AMOUNT_EDIT(data->end_value), print_info);
    gnc_amount_edit_set_fraction (GNC_AMOUNT_EDIT(data->end_value),
                                  gnc_commodity_get_fraction (currency));

    g_signal_connect(GTK_WIDGET(data->end_value), "activate",
                     G_CALLBACK(gnc_autoclear_window_ok_cb), data);

    box   = GTK_BOX(gtk_builder_get_object (builder, "end_value_box"));
    gtk_box_pack_start(box, GTK_WIDGET(data->end_value), TRUE, TRUE, 0);

    label = GTK_WIDGET(gtk_builder_get_object (builder, "end_label"));
    gnc_amount_edit_make_mnemonic_target (GNC_AMOUNT_EDIT(data->end_value), label);

    /* pre-fill with current balance */
    after = xaccAccountGetClearedBalance (data->account);
    if (gnc_reverse_balance(data->account))
        after = gnc_numeric_neg(after);
    gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (data->end_value), after);
    gtk_widget_grab_focus(GTK_WIDGET(data->end_value));
    gnc_amount_edit_select_region (GNC_AMOUNT_EDIT (data->end_value), 0, -1);

    data->status_label = GTK_LABEL(gtk_builder_get_object (builder, "status_label"));

    g_signal_connect (GTK_WIDGET(data->end_value), "changed",
                      G_CALLBACK(clear_status_label_cb), data);

    if (parent != NULL)
        gtk_window_set_transient_for (GTK_WINDOW (data->window), GTK_WINDOW (parent));

    gtk_builder_connect_signals(builder, data);
    g_object_unref(G_OBJECT(builder));

    return data;
}

