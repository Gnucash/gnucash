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
#include "gnc-ui-balances.h"
#include "window-autoclear.h"

#define WINDOW_AUTOCLEAR_CM_CLASS "window-autoclear"

static QofLogModule log_module = GNC_MOD_GUI;

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
    GList *toclear_list;
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
    gnc_numeric toclear_value;
    gchar *errmsg = NULL;

    g_return_if_fail (widget && data);

    /* sanity check: if toclear_list is null, bail out. but this
       should not happen because the OK button is disabled if there is
       autoclear error, and toclear_list is null */
    g_return_if_fail (data->toclear_list);

    xaccAccountBeginEdit (data->account);
    for (GList *node = data->toclear_list; node; node = node->next)
        xaccSplitSetReconcile (node->data, CREC);
    xaccAccountCommitEdit (data->account);

    if (gtk_toggle_button_get_active
        (GTK_TOGGLE_BUTTON (data->show_cleared_splits_button)))
        show_cleared_splits (data->toclear_list);

    /* Close window */
    gnc_autoclear_window_cancel_cb (widget, data);
}

static gboolean
gnc_autoclear_window_delete_cb (GtkWidget *widget, GdkEvent  *event,
                                AutoClearWindow *data)
{
    if (data->toclear_list)
        g_list_free (data->toclear_list);
    g_free(data);
    return FALSE;
}

static void
gnc_autoclear_end_value_activate_cb (GtkWidget *widget, AutoClearWindow *data)
{
    if (data->toclear_list)
        gnc_autoclear_window_ok_cb (widget, data);
}

void
gnc_autoclear_window_cancel_cb (GtkWidget *widget,
                                AutoClearWindow *data)
{
    /* Close window */
    gtk_widget_destroy(data->window);
    if (data->toclear_list)
        g_list_free (data->toclear_list);
    g_free(data);
}

#define MAX_LENGTH 50

static void end_value_changed_cb (GtkEditable *editable, AutoClearWindow *data)
{
    gnc_numeric toclear_value;
    gchar *errmsg = NULL;

    if (gnc_amount_edit_expr_is_valid (data->end_value, &toclear_value, TRUE))
    {
        gtk_widget_set_sensitive (data->ok_button, FALSE);
        gtk_label_set_text (data->status_label, "");
        return;
    }

    if (gnc_reverse_balance (data->account))
        toclear_value = gnc_numeric_neg (toclear_value);

    toclear_value = gnc_numeric_convert
        (toclear_value, xaccAccountGetCommoditySCU(data->account), GNC_HOW_RND_ROUND);

    if (data->toclear_list)
        g_list_free (data->toclear_list);

    data->toclear_list = gnc_account_get_autoclear_splits
        (data->account, toclear_value, &errmsg);

    gtk_widget_set_sensitive (data->ok_button, errmsg == NULL);

    if (errmsg)
    {
        gtk_widget_set_sensitive (data->ok_button, FALSE);
        gtk_label_set_text (data->status_label, errmsg);
        g_free (errmsg);
    }
    else
    {
        gchar *status = g_strdup (_("The following splits will be cleared:"));
        GNCPrintAmountInfo p_info = gnc_account_print_info (data->account, TRUE);
        gboolean reverse = gnc_reverse_balance (data->account);
        for (GList *node = data->toclear_list; node; node = node->next)
        {
            Transaction *trans = xaccSplitGetParent (node->data);
            gnc_numeric amount = xaccSplitGetAmount (node->data);
            const gchar *desc = xaccTransGetDescription (trans);
            gchar *datestr = qof_print_date (xaccTransGetDate (trans));
            gchar *newdesc, *newstatus;

            if (g_utf8_strlen (desc, -1) > MAX_LENGTH)
            {
                gchar *trunc = g_utf8_substring (desc, 0, MAX_LENGTH);
                newdesc = g_strdup_printf ("%s...", trunc);
                g_free (trunc);
            }
            else
                newdesc = g_strdup (desc);

            if (reverse)
                amount = gnc_numeric_neg (amount);

            newstatus = g_strdup_printf ("%s\n%s %s %s", status, datestr,
                                         newdesc, xaccPrintAmount (amount, p_info));
            g_free (status);
            g_free (datestr);
            g_free (newdesc);
            status = newstatus;
        }
        gtk_label_set_text (data->status_label, status);
        g_free (status);
    }
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
    GtkLabel *label;
    GtkBuilder *builder;
    AutoClearWindow *data;
    char *title;
    gnc_numeric after;

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

    data->ok_button = GTK_WIDGET (gtk_builder_get_object (builder, "ok_button"));
    gtk_widget_set_sensitive (data->ok_button, FALSE);

    data->cancel_button = GTK_WIDGET (gtk_builder_get_object
                                      (builder, "cancel_button"));

    data->show_cleared_splits_button =
        GTK_WIDGET (gtk_builder_get_object (builder, "show_cleared_splits_button"));

    /* Add amount edit box */
    data->end_value = GNC_AMOUNT_EDIT(gnc_amount_edit_new());
    g_signal_connect(GTK_WIDGET(data->end_value), "activate",
                     G_CALLBACK(gnc_autoclear_end_value_activate_cb), data);
    g_signal_connect(GTK_WIDGET(data->ok_button), "activate",
                     G_CALLBACK(gnc_autoclear_window_ok_cb), data);
    g_signal_connect(GTK_WIDGET(data->cancel_button), "activate",
                     G_CALLBACK(gnc_autoclear_window_cancel_cb), data);

    box   = GTK_BOX(gtk_builder_get_object (builder, "end_value_box"));
    gtk_box_pack_start(box, GTK_WIDGET(data->end_value), TRUE, TRUE, 0);

    label = GTK_LABEL(gtk_builder_get_object (builder, "end_label"));
    gtk_label_set_mnemonic_widget(label, GTK_WIDGET(data->end_value));

    /* pre-fill with current balance */
    after = xaccAccountGetClearedBalance (data->account);
    if (gnc_reverse_balance(data->account))
        after = gnc_numeric_neg(after);
    gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (data->end_value), after);
    gtk_widget_grab_focus(GTK_WIDGET(data->end_value));
    gtk_editable_select_region (GTK_EDITABLE (data->end_value), 0, -1);

    data->status_label = GTK_LABEL(gtk_builder_get_object (builder, "status_label"));

    g_signal_connect (GTK_WIDGET(data->end_value), "changed",
                      G_CALLBACK(end_value_changed_cb), data);

    g_signal_connect (data->window, "delete-event",
                      G_CALLBACK (gnc_autoclear_window_delete_cb), data);

    if (parent != NULL)
        gtk_window_set_transient_for (GTK_WINDOW (data->window), GTK_WINDOW (parent));

    gtk_builder_connect_signals(builder, data);
    g_object_unref(G_OBJECT(builder));

    return data;
}

