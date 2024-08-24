/********************************************************************\
 * dialog-price-editor.c -- price selector dialog                   *
 * Copyright (C) 2001 Gnumatic, Inc.                                *
 * Author: Dave Peticolas <dave@krondo.com>                         *
 * Copyright (C) 2003,2005 David Hampton                            *
 * Copyright (C) 2011 Robert Fewell                                 *
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
#include <time.h>
#include <gnc-quotes.hpp>

#include "dialog-utils.h"
#include "gnc-accounting-period.h"
#include "gnc-amount-edit.h"
#include "gnc-commodity-edit.h"
#include "gnc-general-select.h"
#include "gnc-component-manager.h"
#include "gnc-currency-edit.h"
#include "gnc-date-edit.h"
#include "gnc-engine.h"
#include "gnc-gui-query.h"
#include "gnc-pricedb.h"
#include "gnc-session.h"
#include "gnc-tree-view-price.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "gnc-warnings.h"
#include <gnc-glib-utils.h>


#define DIALOG_PRICE_DB_CM_CLASS "dialog-price-edit-db"
#define STATE_SECTION "dialogs/edit_prices"
#define GNC_PREFS_GROUP "dialogs.pricedb-editor"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;


extern "C" {
void gnc_prices_dialog_destroy_cb (GtkWidget *object, gpointer data);
void gnc_prices_dialog_close_cb (GtkDialog *dialog, gpointer data);
void gnc_prices_dialog_help_cb (GtkDialog *dialog, gpointer data);
void gnc_prices_dialog_edit_clicked (GtkWidget *widget, gpointer data);
void gnc_prices_dialog_remove_clicked (GtkWidget *widget, gpointer data);
void gnc_prices_dialog_remove_old_clicked (GtkWidget *widget, gpointer data);
void gnc_prices_dialog_add_clicked (GtkWidget *widget, gpointer data);
void gnc_prices_dialog_get_quotes_clicked (GtkWidget *widget, gpointer data);
static gboolean gnc_prices_dialog_key_press_cb (GtkWidget *widget,
                                                GdkEventKey *event,
                                                gpointer data);
}


struct PricesDialog
{
    GtkWidget * window;
    QofSession *session;
    QofBook *book;
    GNCPriceDB *price_db;

    GncTreeViewPrice * price_tree;

    GtkWidget * edit_button;
    GtkWidget * remove_button;
    GtkWidget * add_button;

    GtkWidget *remove_dialog;
    GtkTreeView *remove_view;
    int remove_source;
};


void
gnc_prices_dialog_destroy_cb (GtkWidget *object, gpointer data)
{
    auto pdb_dialog = static_cast<PricesDialog *> (data);

    ENTER(" ");
    gnc_unregister_gui_component_by_data (DIALOG_PRICE_DB_CM_CLASS, pdb_dialog);

    if (pdb_dialog->window)
    {
        gtk_widget_destroy (pdb_dialog->window);
        pdb_dialog->window = NULL;
    }

    g_free (pdb_dialog);
    LEAVE(" ");
}


static gboolean
gnc_prices_dialog_delete_event_cb (GtkWidget *widget,
                                   GdkEvent  *event,
                                   gpointer   data)
{
    auto pdb_dialog = static_cast<PricesDialog *> (data);
    // this cb allows the window size to be saved on closing with the X
    gnc_save_window_size (GNC_PREFS_GROUP,
                          GTK_WINDOW(pdb_dialog->window));
    return FALSE;
}


void
gnc_prices_dialog_close_cb (GtkDialog *dialog, gpointer data)
{
    auto pdb_dialog = static_cast<PricesDialog *> (data);

    ENTER(" ");
    gnc_close_gui_component_by_data (DIALOG_PRICE_DB_CM_CLASS, pdb_dialog);
    LEAVE(" ");
}


void
gnc_prices_dialog_help_cb (GtkDialog *dialog, gpointer data)
{
    auto pdb_dialog{static_cast<PricesDialog*>(data)};

    gnc_gnome_help (GTK_WINDOW (pdb_dialog->window), DF_MANUAL, DL_PRICE_DB);
}


void
gnc_prices_dialog_edit_clicked (GtkWidget *widget, gpointer data)
{
    auto pdb_dialog = static_cast<PricesDialog *> (data);

    ENTER(" ");
    auto price_list = gnc_tree_view_price_get_selected_prices (pdb_dialog->price_tree);
    if (!price_list)
    {
        LEAVE("no price selected");
        return;
    }
    if (g_list_next(price_list))
    {
        g_list_free(price_list);
        LEAVE("too many prices selected");
        return;
    }

    auto price = static_cast<GNCPrice *> (price_list->data);
    gnc_price_edit_dialog (pdb_dialog->window, pdb_dialog->session,
                           price, GNC_PRICE_EDIT);
    g_list_free (price_list);
    LEAVE(" ");
}


static void
remove_helper(GNCPrice *price, GNCPriceDB *pdb)
{
    gnc_pricedb_remove_price (pdb, price);
}


void
gnc_prices_dialog_remove_clicked (GtkWidget *widget, gpointer data)
{
    auto pdb_dialog = static_cast<PricesDialog *> (data);

    ENTER(" ");
    auto price_list = gnc_tree_view_price_get_selected_prices (pdb_dialog->price_tree);
    if (!price_list)
    {
        LEAVE("no price selected");
        return;
    }

    gint response;
    auto length = g_list_length(price_list);
    if (length > 0)
    {
        gchar *message;

        message = g_strdup_printf
                  (/* Translators: %d is the number of prices. This is a ngettext(3) message. */
                      ngettext("Are you sure you want to delete the selected price?",
                               "Are you sure you want to delete the %d selected prices?",
                               length),
                      length);
        auto dialog = gtk_message_dialog_new (GTK_WINDOW(pdb_dialog->window),
                                              GTK_DIALOG_DESTROY_WITH_PARENT,
                                              GTK_MESSAGE_QUESTION,
                                              GTK_BUTTONS_NONE,
                                              "%s", _("Delete prices?"));
        gtk_message_dialog_format_secondary_text(GTK_MESSAGE_DIALOG(dialog),
                "%s", message);
        g_free(message);
        gtk_dialog_add_buttons(GTK_DIALOG(dialog),
                               _("_Cancel"), GTK_RESPONSE_CANCEL,
                               _("_Delete"), GTK_RESPONSE_YES,
                               (gchar *)NULL);
        gtk_dialog_set_default_response(GTK_DIALOG(dialog), GTK_RESPONSE_YES);
        response = gnc_dialog_run(GTK_DIALOG(dialog), GNC_PREF_WARN_PRICE_QUOTES_DEL);
        gtk_widget_destroy(dialog);
    }
    else
    {
        response = GTK_RESPONSE_YES;
    }

    if (response == GTK_RESPONSE_YES)
    {
        g_list_foreach(price_list, (GFunc)remove_helper, pdb_dialog->price_db);
    }
    g_list_free(price_list);
    gnc_gui_refresh_all ();
    LEAVE(" ");
}


/** Enumeration for the price delete list-store */
enum GncPriceColumn {PRICED_FULL_NAME, PRICED_COMM, PRICED_DATE, PRICED_COUNT};

static time64
gnc_prices_dialog_load_view (GtkTreeView *view, GNCPriceDB *pdb)
{
    auto oldest = gnc_time (nullptr);
    auto model = gtk_tree_view_get_model (view);
    const auto commodity_table = gnc_get_current_commodities ();
    auto namespace_list = gnc_commodity_table_get_namespaces (commodity_table);

    for (auto node_n = namespace_list; node_n; node_n = g_list_next (node_n))
    {
        auto tmp_namespace = static_cast<char*>(node_n->data);
        DEBUG("Looking at namespace %s", tmp_namespace);
        auto commodity_list = gnc_commodity_table_get_commodities (commodity_table, tmp_namespace);
        for (auto node_c = commodity_list; node_c; node_c = g_list_next (node_c))
        {
            auto tmp_commodity = static_cast<gnc_commodity*>(node_c->data);
            auto num = gnc_pricedb_num_prices (pdb, tmp_commodity);
            DEBUG("Looking at commodity %s, Number of prices %d", gnc_commodity_get_fullname (tmp_commodity), num);

            if (num > 0)
            {
                auto list = gnc_pricedb_get_prices (pdb, tmp_commodity, NULL);
                auto node = g_list_last (list);
                auto price = static_cast<GNCPrice*> (node->data);
                auto price_time = gnc_price_get_time64 (price);
                auto name_str = gnc_commodity_get_printname (tmp_commodity);

                if (oldest > price_time)
                    oldest = price_time;

                auto date_str = qof_print_date (price_time);
                auto num_str = g_strdup_printf ("%d", num);

                GtkTreeIter iter;
                gtk_list_store_append (GTK_LIST_STORE(model), &iter);
                gtk_list_store_set (GTK_LIST_STORE(model), &iter, PRICED_FULL_NAME, name_str,
                                    PRICED_COMM, tmp_commodity, PRICED_DATE, date_str, PRICED_COUNT, num_str, -1);

                g_free (date_str);
                g_free (num_str);
                g_list_free_full (list, (GDestroyNotify)gnc_price_unref);
            }
        }
        g_list_free (commodity_list);
    }
    g_list_free (namespace_list);

    return oldest;
}

static GList *
gnc_prices_dialog_get_commodities (GtkTreeView *view)
{
    auto model = gtk_tree_view_get_model (GTK_TREE_VIEW(view));
    auto selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(view));
    auto list = gtk_tree_selection_get_selected_rows (selection, &model);
    GList *comm_list = nullptr;

    // Walk the list
    for (auto row = g_list_first (list); row; row = g_list_next (row))
    {
        auto path = static_cast<GtkTreePath *> (row->data);
        GtkTreeIter iter;
        if (gtk_tree_model_get_iter (model, &iter, path))
        {
            gnc_commodity *comm;
            gtk_tree_model_get (model, &iter, PRICED_COMM, &comm, -1);
            comm_list = g_list_prepend (comm_list, comm);
        }
    }
    g_list_free_full (list, (GDestroyNotify) gtk_tree_path_free);

    return g_list_reverse (comm_list);
}

static void
change_source_flag (PriceRemoveSourceFlags source, gboolean set, gpointer data)
{
    auto pdb_dialog = static_cast<PricesDialog *> (data);
    GtkWidget *w = gtk_dialog_get_widget_for_response (GTK_DIALOG(pdb_dialog->remove_dialog), GTK_RESPONSE_OK);
    gboolean enable_button;

    if (set)
        pdb_dialog->remove_source = pdb_dialog->remove_source | source;
    else
        pdb_dialog->remove_source = pdb_dialog->remove_source & (~source);

    // Check if we have the required options to enable OK button
    enable_button = (pdb_dialog->remove_source > 8 ? TRUE : FALSE); // commodities flag is 8
    gtk_widget_set_sensitive (w, enable_button);

    DEBUG("Source is: %d, remove_source is %d", source, pdb_dialog->remove_source);
}

static void
check_event_fq_cb (GtkWidget *widget, gpointer data)
{
    auto pdb_dialog = static_cast<PricesDialog *> (data);
    gboolean active = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(widget));

    change_source_flag (PRICE_REMOVE_SOURCE_FQ, active, pdb_dialog);
}

static void
check_event_user_cb (GtkWidget *widget, gpointer data)
{
    auto pdb_dialog = static_cast<PricesDialog *> (data);
    gboolean active = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(widget));

    change_source_flag (PRICE_REMOVE_SOURCE_USER, active, pdb_dialog);
}

static void
check_event_app_cb (GtkWidget *widget, gpointer data)
{
    auto pdb_dialog = static_cast<PricesDialog *> (data);
    gboolean active = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(widget));

    change_source_flag (PRICE_REMOVE_SOURCE_APP, active, pdb_dialog);
}

static void
selection_changed_cb (GtkTreeSelection *selection, gpointer data)
{
    auto pdb_dialog = static_cast<PricesDialog *> (data);
    auto model = gtk_tree_view_get_model (GTK_TREE_VIEW(pdb_dialog->remove_view));
    auto rows = gtk_tree_selection_get_selected_rows (selection, &model);
    gboolean have_rows = (gnc_list_length_cmp (rows, 0));

    change_source_flag (PRICE_REMOVE_SOURCE_COMM, have_rows, pdb_dialog);
    g_list_free_full (rows, (GDestroyNotify) gtk_tree_path_free);
}

static GDate
get_fiscal_end_date (void)
{
    time64 end;
    char datebuff[MAX_DATE_LENGTH + 1];
    memset (datebuff, 0, sizeof(datebuff));
    end = gnc_accounting_period_fiscal_end();
    qof_print_date_buff(datebuff, MAX_DATE_LENGTH,
                        gnc_accounting_period_fiscal_end());
    PINFO("Fiscal end date is %s", datebuff);

    return time64_to_gdate (end);
}

void
gnc_prices_dialog_remove_old_clicked (GtkWidget *widget, gpointer data)
{
    auto pdb_dialog = static_cast<PricesDialog *> (data);

    ENTER(" ");
    auto builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-price.glade", "liststore4");
    gnc_builder_add_from_file (builder, "dialog-price.glade", "deletion_date_dialog");

    pdb_dialog->remove_dialog = GTK_WIDGET(gtk_builder_get_object (builder, "deletion_date_dialog"));

    auto box = GTK_WIDGET(gtk_builder_get_object (builder, "date_hbox"));
    auto date = gnc_date_edit_new (time (NULL), FALSE, FALSE);

    gtk_box_pack_start (GTK_BOX (box), date, FALSE, FALSE, 0);
    gtk_widget_show (date);
    gtk_entry_set_activates_default(GTK_ENTRY(GNC_DATE_EDIT(date)->date_entry), TRUE);
    auto label = GTK_WIDGET(gtk_builder_get_object (builder, "date_label"));
    gnc_date_make_mnemonic_target (GNC_DATE_EDIT(date), label);

    // Setup the commodity view
    pdb_dialog->remove_view = GTK_TREE_VIEW(gtk_builder_get_object (builder, "commodty_treeview"));
    auto selection = gtk_tree_view_get_selection (pdb_dialog->remove_view);
    gtk_tree_selection_set_mode (selection, GTK_SELECTION_MULTIPLE);

    // Add Entries column this way as align does not seem to work from builder
    auto tree_column = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title (tree_column, _("Entries"));
    gtk_tree_view_append_column (GTK_TREE_VIEW(pdb_dialog->remove_view), tree_column);
    gtk_tree_view_column_set_alignment (tree_column, 0.5);
    gtk_tree_view_column_set_expand (tree_column, TRUE);
    auto cr = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start (tree_column, cr, TRUE);
    // set 'xalign' property of the cell renderer
    gtk_tree_view_column_set_attributes (tree_column, cr, "text", PRICED_COUNT, NULL);
    gtk_cell_renderer_set_alignment (cr, 0.5, 0.5);

    // Load the view and get the earliest date
    gnc_prices_dialog_load_view (pdb_dialog->remove_view, pdb_dialog->price_db);
    gtk_tree_selection_select_all (selection);
    g_signal_connect (selection, "changed", G_CALLBACK(selection_changed_cb), pdb_dialog);

    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, pdb_dialog);

    gtk_window_set_transient_for (GTK_WINDOW (pdb_dialog->remove_dialog), GTK_WINDOW (pdb_dialog->window));

    pdb_dialog->remove_source = PRICE_REMOVE_SOURCE_FQ + PRICE_REMOVE_SOURCE_COMM; // FQ and Commodities highlighted
    auto button = GTK_WIDGET(gtk_builder_get_object (builder, "checkbutton_fq"));
    g_signal_connect (button, "toggled", G_CALLBACK (check_event_fq_cb), pdb_dialog);
    button = GTK_WIDGET(gtk_builder_get_object (builder, "checkbutton_user"));
    g_signal_connect (button, "toggled", G_CALLBACK (check_event_user_cb), pdb_dialog);
    button = GTK_WIDGET(gtk_builder_get_object (builder, "checkbutton_app"));
    g_signal_connect (button, "toggled", G_CALLBACK (check_event_app_cb), pdb_dialog);

    auto result = gtk_dialog_run (GTK_DIALOG (pdb_dialog->remove_dialog));
    if (result == GTK_RESPONSE_OK)
    {
        const char *fmt = _("Are you sure you want to delete these prices?");
        auto comm_list = gnc_prices_dialog_get_commodities (pdb_dialog->remove_view);

        // Are you sure you want to delete the entries and we have commodities
        if ((g_list_length (comm_list) != 0) && (gnc_verify_dialog (GTK_WINDOW (pdb_dialog->remove_dialog), FALSE, fmt, NULL)))
        {
            time64 last;
            GDate fiscal_end_date = get_fiscal_end_date ();
            PriceRemoveKeepOptions keep = PRICE_REMOVE_KEEP_NONE;

            // disconnect the model to the price treeview
            auto model = gtk_tree_view_get_model (GTK_TREE_VIEW(pdb_dialog->price_tree));
            g_object_ref (G_OBJECT(model));
            gtk_tree_view_set_model (GTK_TREE_VIEW(pdb_dialog->price_tree), NULL);

            DEBUG("deleting prices");
            last = gnc_date_edit_get_date (GNC_DATE_EDIT (date));

            button = GTK_WIDGET(gtk_builder_get_object (builder, "radiobutton_last_week"));
            if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(button)))
                keep = PRICE_REMOVE_KEEP_LAST_WEEKLY;
            button = GTK_WIDGET(gtk_builder_get_object (builder, "radiobutton_last_month"));
            if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(button)))
                keep = PRICE_REMOVE_KEEP_LAST_MONTHLY;
            button = GTK_WIDGET(gtk_builder_get_object (builder, "radiobutton_last_quarter"));
            if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(button)))
                keep = PRICE_REMOVE_KEEP_LAST_QUARTERLY;
            button = GTK_WIDGET(gtk_builder_get_object (builder, "radiobutton_last_period"));
            if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(button)))
                keep = PRICE_REMOVE_KEEP_LAST_PERIOD;
            button = GTK_WIDGET(gtk_builder_get_object (builder, "radiobutton_scaled"));
            if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(button)))
                keep = PRICE_REMOVE_KEEP_SCALED;

            if (keep != PRICE_REMOVE_KEEP_SCALED)
                gnc_pricedb_remove_old_prices (pdb_dialog->price_db, comm_list,
                                               &fiscal_end_date, last,
                                               static_cast<PriceRemoveSourceFlags> (pdb_dialog->remove_source),
                                               keep);
            else
            {
                auto tmp_date = time64_to_gdate (last);
                g_date_subtract_months (&tmp_date, 6);
                auto tmp = gdate_to_time64 (tmp_date);

                gnc_pricedb_remove_old_prices (pdb_dialog->price_db, comm_list,
                                               &fiscal_end_date, tmp,
                                               static_cast<PriceRemoveSourceFlags> (pdb_dialog->remove_source),
                                               PRICE_REMOVE_KEEP_LAST_WEEKLY);

                g_date_subtract_months (&tmp_date, 6);
                tmp = gdate_to_time64 (tmp_date);

                gnc_pricedb_remove_old_prices (pdb_dialog->price_db, comm_list,
                                               &fiscal_end_date, tmp,
                                               static_cast<PriceRemoveSourceFlags> (pdb_dialog->remove_source),
                                               PRICE_REMOVE_KEEP_LAST_MONTHLY);
            }
            // reconnect the model to the price treeview
            gtk_tree_view_set_model (GTK_TREE_VIEW(pdb_dialog->price_tree), model);
            g_object_unref(G_OBJECT(model));
        }
        g_list_free (comm_list);
    }
    gnc_gui_refresh_all ();
    gtk_widget_destroy (pdb_dialog->remove_dialog);
    g_object_unref (G_OBJECT (builder));
    LEAVE(" ");
}


void
gnc_prices_dialog_add_clicked (GtkWidget *widget, gpointer data)
{
    auto pdb_dialog = static_cast<PricesDialog *> (data);
    GNCPrice *price = nullptr;
    gboolean unref_price = FALSE;

    ENTER(" ");
    auto price_list = gnc_tree_view_price_get_selected_prices (pdb_dialog->price_tree);
    auto comm_list = gnc_tree_view_price_get_selected_commodities (pdb_dialog->price_tree);

    if (price_list) // selected row is on a price
    {
        price = static_cast<GNCPrice *> (price_list->data);
        g_list_free (price_list);
    }
    else if (comm_list) // selection contains price parent rows
    {
        if (!gnc_list_length_cmp (comm_list, 1)) // make sure it is only one parent
        {
            auto comm = GNC_COMMODITY (comm_list->data);
            auto latest_price = gnc_pricedb_lookup_latest_any_currency (pdb_dialog->price_db, comm);

            if (latest_price)
            {
                price = GNC_PRICE (latest_price->data);
                gnc_price_ref (price);

                gnc_price_list_destroy (latest_price);
            }

            if (!price)
            {
                price = gnc_price_create (pdb_dialog->book);
                gnc_price_set_commodity (price, comm);
            }

            unref_price = TRUE;
        }
        g_list_free (comm_list);
    }
    gnc_price_edit_dialog (pdb_dialog->window, pdb_dialog->session,
                           price, GNC_PRICE_NEW);

    if (unref_price)
        gnc_price_unref (price);
    LEAVE(" ");
}


void
gnc_prices_dialog_get_quotes_clicked (GtkWidget *widget, gpointer data)
{
    auto pdb_dialog = static_cast<PricesDialog *> (data);

    ENTER(" ");
    try {
        GncQuotes quotes;
        gnc_set_busy_cursor (NULL, TRUE);
        quotes.fetch (pdb_dialog->book);
        gnc_unset_busy_cursor (NULL);
        if (quotes.had_failures())
            gnc_warning_dialog(GTK_WINDOW(pdb_dialog->window), "%s",
                               quotes.report_failures().c_str());
    }
    catch (const GncQuoteException& err)
    {
        gnc_unset_busy_cursor(nullptr);
        PERR("Price retrieval failed: %s", err.what());
        gnc_error_dialog(GTK_WINDOW(pdb_dialog->window), _("Price retrieval failed: %s"), err.what());
    }

    /* Without this, the summary bar on the accounts tab
     * won't reflect the new prices (bug #522095). */
    gnc_gui_refresh_all ();

    LEAVE(" ");
}


static void
gnc_prices_dialog_selection_changed (GtkTreeSelection *treeselection,
                                     gpointer data)
{
    auto pdb_dialog = static_cast<PricesDialog *> (data);

    ENTER(" ");
    auto price_list = gnc_tree_view_price_get_selected_prices (pdb_dialog->price_tree);
    auto length = g_list_length (price_list);
    g_list_free (price_list);

    auto model = gtk_tree_view_get_model (GTK_TREE_VIEW(pdb_dialog->price_tree));
    auto rows = gtk_tree_selection_get_selected_rows (treeselection, &model);

    // if selected rows greater than length, parents must of been selected also
    if (g_list_length (rows) > length)
        length = 0;

    g_list_free_full (rows, (GDestroyNotify) gtk_tree_path_free);

    gtk_widget_set_sensitive (pdb_dialog->edit_button,
                              length == 1);
    gtk_widget_set_sensitive (pdb_dialog->remove_button,
                              length >= 1);
    gtk_widget_set_sensitive (pdb_dialog->add_button,
                              length <= 1);
    LEAVE("%d prices selected", length);
}


static gboolean
gnc_price_dialog_filter_ns_func (gnc_commodity_namespace *name_space,
                                 gpointer data)
{
    auto pdb_dialog = static_cast<PricesDialog *> (data);

    /* Never show the template list */
    auto name = gnc_commodity_namespace_get_name (name_space);
    if (g_strcmp0 (name, GNC_COMMODITY_NS_TEMPLATE) == 0)
        return FALSE;

    /* See if this namespace has commodities */
    auto cm_list = gnc_commodity_namespace_get_commodity_list (name_space);
    auto rv = false;
    for (auto item = cm_list; !rv && item; item = g_list_next (item))
    {
        /* For each commodity, see if there are prices */
        auto comm = static_cast<gnc_commodity *> (item->data);
        if (gnc_pricedb_has_prices (pdb_dialog->price_db, comm, nullptr))
            rv = true;
    }

    g_list_free (cm_list);
    return rv;
}


static gboolean
gnc_price_dialog_filter_cm_func (gnc_commodity *commodity,
                                 gpointer data)
{
    auto pdb_dialog = static_cast<PricesDialog *> (data);

    /* Show any commodity that has prices */
    return gnc_pricedb_has_prices(pdb_dialog->price_db, commodity, NULL);
}


static void
row_activated_cb (GtkTreeView *view, GtkTreePath *path,
                  GtkTreeViewColumn *column, gpointer data)
{
    GtkTreeModel *model;
    GtkTreeIter iter;

    g_return_if_fail(view);

    model = gtk_tree_view_get_model(view);
    if (gtk_tree_model_get_iter(model, &iter, path))
    {
        if (gtk_tree_model_iter_has_child(model, &iter))
        {
            /* There are children, so it's not a price.
             * Just expand or collapse the row. */
            if (gtk_tree_view_row_expanded(view, path))
                gtk_tree_view_collapse_row(view, path);
            else
                gtk_tree_view_expand_row(view, path, FALSE);
        }
        else
            /* It's a price, so click the Edit button. */
            gnc_prices_dialog_edit_clicked(GTK_WIDGET(view), data);
    }
}


static void
gnc_prices_dialog_create (GtkWidget * parent, PricesDialog *pdb_dialog)
{
    GtkWidget *window, *scrolled_window;
    GtkBuilder *builder;
    GtkTreeView *view;
    GtkTreeSelection *selection;

    ENTER(" ");
    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-price.glade", "prices_window");

    window = GTK_WIDGET(gtk_builder_get_object (builder, "prices_window"));
    pdb_dialog->window = window;

    // Set the name for this dialog so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(window), "gnc-id-price-edit");
    gnc_widget_style_context_add_class (GTK_WIDGET(window), "gnc-class-securities");

    pdb_dialog->session = gnc_get_current_session();
    pdb_dialog->book = qof_session_get_book(pdb_dialog->session);
    pdb_dialog->price_db = gnc_pricedb_get_db(pdb_dialog->book);

    g_signal_connect (pdb_dialog->window, "delete-event",
                      G_CALLBACK(gnc_prices_dialog_delete_event_cb), pdb_dialog);

    g_signal_connect (pdb_dialog->window, "key_press_event",
                      G_CALLBACK (gnc_prices_dialog_key_press_cb), pdb_dialog);

    /* price tree */
    scrolled_window = GTK_WIDGET(gtk_builder_get_object (builder, "price_list_window"));
    view = gnc_tree_view_price_new(pdb_dialog->book,
                                   "state-section", STATE_SECTION,
                                   "show-column-menu", TRUE,
                                   NULL);
    pdb_dialog->price_tree = GNC_TREE_VIEW_PRICE(view);
    gtk_container_add (GTK_CONTAINER (scrolled_window), GTK_WIDGET(view));
    gnc_tree_view_price_set_filter (pdb_dialog->price_tree,
                                    gnc_price_dialog_filter_ns_func,
                                    gnc_price_dialog_filter_cm_func,
                                    NULL,
                                    pdb_dialog, NULL);

    selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (view));
    gtk_tree_selection_set_mode(selection, GTK_SELECTION_MULTIPLE);
    g_signal_connect (G_OBJECT (selection), "changed",
                      G_CALLBACK (gnc_prices_dialog_selection_changed), pdb_dialog);

    g_signal_connect (G_OBJECT (view), "row-activated",
                      G_CALLBACK (row_activated_cb), pdb_dialog);

    /* buttons */
    {
        GtkWidget *button;

        button = GTK_WIDGET(gtk_builder_get_object (builder, "edit_button"));
        pdb_dialog->edit_button = button;

        button = GTK_WIDGET(gtk_builder_get_object (builder, "remove_button"));
        pdb_dialog->remove_button = button;

        button = GTK_WIDGET(gtk_builder_get_object (builder, "add_button"));
        pdb_dialog->add_button = button;

        if (!gnc_quote_source_fq_installed())
        {
            button = GTK_WIDGET(gtk_builder_get_object (builder, "get_quotes_button"));
            gtk_widget_set_sensitive(button, FALSE);
        }
        /* default to 'close' button */
        button = GTK_WIDGET(gtk_builder_get_object (builder, "close_button"));
        gtk_widget_grab_default (button);
        gtk_widget_grab_focus (button);

    }

    g_signal_connect (pdb_dialog->window, "destroy",
                      G_CALLBACK(gnc_prices_dialog_destroy_cb), pdb_dialog);

    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, pdb_dialog);
    g_object_unref(G_OBJECT(builder));

    gnc_restore_window_size (GNC_PREFS_GROUP, GTK_WINDOW(pdb_dialog->window), GTK_WINDOW (parent));
    LEAVE(" ");
}


static void
close_handler (gpointer user_data)
{
    auto pdb_dialog = static_cast<PricesDialog *> (user_data);

    ENTER(" ");
    gnc_save_window_size (GNC_PREFS_GROUP, GTK_WINDOW(pdb_dialog->window));
    gtk_widget_destroy (GTK_WIDGET (pdb_dialog->window));
    LEAVE(" ");
}


static void
refresh_handler (GHashTable *changes, gpointer user_data)
{
    ENTER(" ");
    LEAVE(" ");
}


static gboolean
show_handler (const char *klass, gint component_id,
              gpointer user_data, gpointer iter_data)
{
    auto pdb_dialog = static_cast<PricesDialog *> (user_data);

    ENTER(" ");
    if (!pdb_dialog)
    {
        LEAVE("no data structure");
        return(FALSE);
    }

    gtk_window_present (GTK_WINDOW(pdb_dialog->window));
    LEAVE(" ");
    return(TRUE);
}


gboolean
gnc_prices_dialog_key_press_cb (GtkWidget *widget, GdkEventKey *event,
                                gpointer data)
{
    auto pdb_dialog = static_cast<PricesDialog *> (data);

    if (event->keyval == GDK_KEY_Escape)
    {
        close_handler (pdb_dialog);
        return TRUE;
    }
    else
        return FALSE;
}


/********************************************************************\
 * gnc_prices_dialog                                                *
 *   opens up a window showing all price information                *
 *                                                                  *
 * Args:   parent  - the parent of the window to be created         *
 * Return: nothing                                                  *
\********************************************************************/
void
gnc_prices_dialog (GtkWidget * parent)
{
    PricesDialog *pdb_dialog;
    gint component_id;

    ENTER(" ");
    if (gnc_forall_gui_components (DIALOG_PRICE_DB_CM_CLASS, show_handler, NULL))
    {
        LEAVE("existing dialog raised");
        return;
    }

    pdb_dialog = g_new0 (PricesDialog, 1);

    gnc_prices_dialog_create (parent, pdb_dialog);

    component_id = gnc_register_gui_component (DIALOG_PRICE_DB_CM_CLASS,
                   refresh_handler, close_handler,
                   pdb_dialog);
    gnc_gui_component_set_session (component_id, pdb_dialog->session);

    gtk_widget_grab_focus (GTK_WIDGET(pdb_dialog->price_tree));

    gtk_widget_show (pdb_dialog->window);
    LEAVE(" ");
}
