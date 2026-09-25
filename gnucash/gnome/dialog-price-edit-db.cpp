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
#include "dialog-commodity.h"
#include "gnc-accounting-period.h"
#include "gnc-amount-edit.h"
#include "gnc-commodity-edit.h"
#include "gnc-general-select.h"
#include "gnc-component-manager.h"
#include "gnc-currency-edit.h"
#include "gnc-date-edit.h"
#include "gnc-engine.h"
#include "gnc-gtk-utils.h"
#include "gnc-gui-query.h"
#include "gnc-pricedb.h"
#include "gnc-session.h"
#include "gnc-tree-view-price.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "gnc-warnings.h"
#include <gnc-string-utils.h>


#define DIALOG_PRICE_DB_CM_CLASS "dialog-price-edit-db"
#define STATE_SECTION "dialogs/edit_prices"
#define GNC_PREFS_GROUP "dialogs.pricedb-editor"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;


extern "C" {
void gnc_prices_dialog_destroy_cb (GtkWidget *object, gpointer data);
void gnc_prices_dialog_close_cb (GtkWidget *button, gpointer data);
void gnc_prices_dialog_help_cb (GtkWidget *button, gpointer data);
void gnc_prices_dialog_edit_clicked (GtkWidget *widget, gpointer data);
void gnc_prices_dialog_remove_clicked (GtkWidget *widget, gpointer data);
void gnc_prices_dialog_remove_old_clicked (GtkWidget *widget, gpointer data);
void gnc_prices_dialog_add_clicked (GtkWidget *widget, gpointer data);
void gnc_prices_dialog_get_quotes_clicked (GtkWidget *widget, gpointer data);
static gboolean gnc_prices_dialog_key_pressed_cb (GtkEventControllerKey *key,
                                                  guint keyval, guint keycode,
                                                  GdkModifierType state,
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


};

constexpr const char *PRICE_DIALOG_DATA = "gnc-price-edit-dialog";

struct PriceDeleteRequest
{
    GWeakRef window;
    GPtrArray *price_guids;
};

static void
price_delete_request_free (PriceDeleteRequest *request)
{
    g_weak_ref_clear (&request->window);
    g_ptr_array_unref (request->price_guids);
    g_free (request);
}

static void
price_delete_finished (gint response, gpointer user_data)
{
    auto request = static_cast<PriceDeleteRequest *> (user_data);
    auto window = GTK_WIDGET (g_weak_ref_get (&request->window));

    if (response == GTK_RESPONSE_YES && window)
    {
        auto pdb_dialog = static_cast<PricesDialog *> (
            g_object_get_data (G_OBJECT (window), PRICE_DIALOG_DATA));

        if (pdb_dialog && pdb_dialog->book == gnc_get_current_book () &&
            pdb_dialog->price_db && !qof_book_shutting_down (pdb_dialog->book))
        {
            for (guint index = 0; index < request->price_guids->len; index++)
            {
                auto guid = static_cast<GncGUID *> (
                    g_ptr_array_index (request->price_guids, index));
                auto price = gnc_price_lookup (guid, pdb_dialog->book);

                if (price)
                    gnc_pricedb_remove_price (pdb_dialog->price_db, price);
            }
            gnc_gui_refresh_all ();
        }
    }

    g_clear_object (&window);
    price_delete_request_free (request);
}


void
gnc_prices_dialog_destroy_cb (GtkWidget *object, gpointer data)
{
    auto pdb_dialog = static_cast<PricesDialog *> (data);

    ENTER(" ");
    g_object_set_data (G_OBJECT (object), PRICE_DIALOG_DATA, nullptr);
    gnc_unregister_gui_component_by_data (DIALOG_PRICE_DB_CM_CLASS, pdb_dialog);

    pdb_dialog->window = nullptr;

    g_free (pdb_dialog);
    LEAVE(" ");
}


static gboolean
gnc_prices_dialog_close_request_cb (GtkWindow *window, gpointer data)
{
    // This callback allows the window size to be saved on closing with the X.
    gnc_save_window_size (GNC_PREFS_GROUP, window);
    (void)data;
    return FALSE;
}


void
gnc_prices_dialog_close_cb (GtkWidget *button, gpointer data)
{
    auto pdb_dialog = static_cast<PricesDialog *> (data);

    (void)button;
    ENTER(" ");
    gnc_close_gui_component_by_data (DIALOG_PRICE_DB_CM_CLASS, pdb_dialog);
    LEAVE(" ");
}


void
gnc_prices_dialog_help_cb (GtkWidget *button, gpointer data)
{
    auto pdb_dialog{static_cast<PricesDialog*>(data)};

    (void)button;
    gnc_gnome_help (GTK_WINDOW (pdb_dialog->window), DF_MANUAL, DL_PRICE_DB);
}


void
gnc_prices_dialog_edit_clicked (GtkWidget *widget, gpointer data)
{
    auto pdb_dialog = static_cast<PricesDialog *> (data);

    (void)widget;
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


void
gnc_prices_dialog_remove_clicked (GtkWidget *widget, gpointer data)
{
    auto pdb_dialog = static_cast<PricesDialog *> (data);

    (void)widget;
    ENTER(" ");
    auto price_list = gnc_tree_view_price_get_selected_prices (pdb_dialog->price_tree);
    if (!price_list)
    {
        LEAVE("no price selected");
        return;
    }

    auto request = g_new0 (PriceDeleteRequest, 1);
    auto length = g_list_length (price_list);
    request->price_guids = g_ptr_array_new_with_free_func (g_free);
    g_weak_ref_init (&request->window, pdb_dialog->window);
    for (auto node = price_list; node; node = g_list_next (node))
    {
        auto guid = g_new (GncGUID, 1);
        *guid = *gnc_price_get_guid (static_cast<GNCPrice *> (node->data));
        g_ptr_array_add (request->price_guids, guid);
    }
    g_list_free (price_list);

    auto message = g_strdup_printf
        (/* Translators: %d is the number of prices. This is a ngettext(3) message. */
         ngettext("Are you sure you want to delete the selected price?",
                  "Are you sure you want to delete the %d selected prices?", length),
         length);
    gnc_warning_dialog_async (GTK_WINDOW (pdb_dialog->window),
                              GNC_PREF_WARN_PRICE_QUOTES_DEL,
                              _("Delete prices?"), message, _("_Delete"),
                              GTK_RESPONSE_YES, TRUE, price_delete_finished, request);
    g_free (message);
    LEAVE(" ");
}

namespace
{
constexpr const char *PRICE_REMOVE_MODEL_DATA = "gnc-price-remove-model";
constexpr const char *PRICE_REMOVE_FULL_NAME_DATA = "gnc-price-remove-full-name";
constexpr const char *PRICE_REMOVE_COMMODITY_DATA = "gnc-price-remove-commodity";
constexpr const char *PRICE_REMOVE_DATE_DATA = "gnc-price-remove-date";
constexpr const char *PRICE_REMOVE_COUNT_DATA = "gnc-price-remove-count";

static GListStore *
price_remove_model (GtkWidget *view)
{
    return G_LIST_STORE (g_object_get_data (G_OBJECT (view), PRICE_REMOVE_MODEL_DATA));
}

static GObject *
price_remove_row_new (const char *full_name, gnc_commodity *commodity,
                      const char *date, const char *count)
{
    auto row = G_OBJECT (g_object_new (G_TYPE_OBJECT, nullptr));
    g_object_set_data_full (row, PRICE_REMOVE_FULL_NAME_DATA, g_strdup (full_name), g_free);
    g_object_set_data (row, PRICE_REMOVE_COMMODITY_DATA, commodity);
    g_object_set_data_full (row, PRICE_REMOVE_DATE_DATA, g_strdup (date), g_free);
    g_object_set_data_full (row, PRICE_REMOVE_COUNT_DATA, g_strdup (count), g_free);
    return row;
}

static void
price_remove_item_setup (GtkSignalListItemFactory *, GtkListItem *list_item, gpointer)
{
    auto label = gtk_label_new (nullptr);
    gtk_label_set_xalign (GTK_LABEL (label), 0.0);
    gtk_label_set_ellipsize (GTK_LABEL (label), PANGO_ELLIPSIZE_END);
    gtk_list_item_set_child (list_item, label);
}

static void
price_remove_item_bind (GtkSignalListItemFactory *, GtkListItem *list_item, gpointer user_data)
{
    auto row = G_OBJECT (gtk_list_item_get_item (list_item));
    auto label = GTK_LABEL (gtk_list_item_get_child (list_item));
    auto key = static_cast<const char *> (user_data);
    auto value = static_cast<const char *> (g_object_get_data (row, key));

    gtk_label_set_text (label, value ? value : "");
    gtk_label_set_xalign (label,
                          key == PRICE_REMOVE_COUNT_DATA ? 0.5 : 0.0);
}

static GtkColumnViewColumn *
price_remove_column_new (const char *title, const char *data_key)
{
    auto factory = gtk_signal_list_item_factory_new ();
    g_signal_connect (factory, "setup", G_CALLBACK (price_remove_item_setup), nullptr);
    g_signal_connect (factory, "bind", G_CALLBACK (price_remove_item_bind),
                      const_cast<char *> (data_key));
    return gtk_column_view_column_new (title, GTK_LIST_ITEM_FACTORY (factory));
}

static void
price_remove_append_column (GtkColumnView *view, const char *title, const char *data_key)
{
    auto column = price_remove_column_new (title, data_key);
    gtk_column_view_append_column (view, column);
    g_object_unref (column);
}
}

static bool
continue_namespace_check (const gchar *target_namespace_name, const gchar *namespace_name)
{
   if ((g_strcmp0 (target_namespace_name, GNC_COMMODITY_NS_CURRENCY) == 0) &&
       (g_strcmp0 (namespace_name, GNC_COMMODITY_NS_CURRENCY) != 0))
       return true;

   if ((g_strcmp0 (target_namespace_name, GNC_COMMODITY_NS_NONISO_GUI) == 0) &&
       (g_strcmp0 (namespace_name, GNC_COMMODITY_NS_CURRENCY) == 0))
       return true;

   if ((g_strcmp0 (target_namespace_name, GNC_COMMODITY_NS_NONISO_GUI) != 0) &&
       (g_strcmp0 (target_namespace_name, GNC_COMMODITY_NS_CURRENCY) != 0) &&
       (g_strcmp0 (target_namespace_name, namespace_name) != 0))
       return true;

    return false;
}

static time64
gnc_prices_dialog_load_view (GtkWidget *view, GNCPriceDB *pdb, const gchar *target_namespace_name)
{
    auto oldest = gnc_time (nullptr);
    auto model = price_remove_model (view);
    const auto commodity_table = gnc_get_current_commodities ();
    auto namespace_list = gnc_commodity_table_get_namespaces_list (commodity_table);

    g_list_store_remove_all (model);

    for (auto node_n = namespace_list; node_n; node_n = g_list_next (node_n))
    {
        auto tmp_namespace = static_cast<gnc_commodity_namespace*>(node_n->data);
        auto tmp_namespace_name_str = gnc_commodity_namespace_get_name (tmp_namespace);

        DEBUG("Restricted to %s, looking at namespace %s", target_namespace_name, tmp_namespace_name_str);
        if (continue_namespace_check (target_namespace_name, tmp_namespace_name_str))
            continue;

        auto commodity_list = gnc_commodity_table_get_commodities (commodity_table, tmp_namespace_name_str);
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
                auto row = price_remove_row_new (name_str, tmp_commodity, date_str, num_str);
                g_list_store_append (model, row);
                g_object_unref (row);

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
gnc_prices_dialog_get_commodities (GtkWidget *view)
{
    auto model = price_remove_model (view);
    auto selection_model = gtk_column_view_get_model (GTK_COLUMN_VIEW (view));
    auto selection = gtk_selection_model_get_selection (selection_model);
    GtkBitsetIter iter;
    guint position;
    GList *comm_list = nullptr;

    for (auto valid = gtk_bitset_iter_init_first (&iter, selection, &position);
         valid; valid = gtk_bitset_iter_next (&iter, &position))
    {
        auto row = G_OBJECT (g_list_model_get_item (G_LIST_MODEL (model), position));
        auto commodity = static_cast<gnc_commodity *> (
            g_object_get_data (row, PRICE_REMOVE_COMMODITY_DATA));
        comm_list = g_list_prepend (comm_list, commodity);
        g_object_unref (row);
    }
    gtk_bitset_unref (selection);

    return g_list_reverse (comm_list);
}

struct PriceOldRemoveRequest
{
    gatomicrefcount ref_count;
    GWeakRef price_window;
    GtkWindow *dialog;
    GtkWidget *date;
    GtkWidget *remove_view;
    GtkWidget *namespace_picker;
    GtkWidget *ok_button;
    GtkWidget *keep_none;
    GtkWidget *keep_last_month;
    GtkWidget *keep_last_quarter;
    GtkWidget *keep_last_period;
    GtkWidget *keep_scaled;
    gchar *target_namespace_name;
    gint remove_source;
    gboolean completed;
    gboolean waiting_for_confirmation;
    gulong parent_destroy_handler;
    gulong dialog_destroy_handler;
};

static PriceOldRemoveRequest *
price_old_remove_request_ref (PriceOldRemoveRequest *request)
{
    g_atomic_ref_count_inc (&request->ref_count);
    return request;
}

static void
price_old_remove_request_free (PriceOldRemoveRequest *request)
{
    auto window = GTK_WIDGET (g_weak_ref_get (&request->price_window));

    if (window && request->parent_destroy_handler)
        g_signal_handler_disconnect (window, request->parent_destroy_handler);
    g_clear_object (&window);
    g_clear_object (&request->dialog);
    g_weak_ref_clear (&request->price_window);
    g_free (request->target_namespace_name);
    g_free (request);
}

static void
price_old_remove_request_unref (PriceOldRemoveRequest *request)
{
    if (request && g_atomic_ref_count_dec (&request->ref_count))
        price_old_remove_request_free (request);
}

static PricesDialog *
price_old_remove_get_prices_dialog (PriceOldRemoveRequest *request,
                                    GtkWidget **window_out)
{
    auto window = GTK_WIDGET (g_weak_ref_get (&request->price_window));
    PricesDialog *pdb_dialog = nullptr;

    if (window)
    {
        pdb_dialog = static_cast<PricesDialog *> (
            g_object_get_data (G_OBJECT (window), PRICE_DIALOG_DATA));
        if (!pdb_dialog || pdb_dialog->window != window ||
            pdb_dialog->book != gnc_get_current_book () ||
            qof_book_shutting_down (pdb_dialog->book))
            pdb_dialog = nullptr;
    }

    if (window_out)
        *window_out = window;
    else
        g_clear_object (&window);
    return pdb_dialog;
}

static void
price_old_remove_request_complete (PriceOldRemoveRequest *request,
                                   gboolean refresh_prices)
{
    GtkWidget *window;

    if (!request || request->completed)
        return;

    request->completed = TRUE;
    window = GTK_WIDGET (g_weak_ref_get (&request->price_window));
    if (window && request->parent_destroy_handler)
        g_signal_handler_disconnect (window, request->parent_destroy_handler);
    request->parent_destroy_handler = 0;
    g_clear_object (&window);

    if (request->dialog)
    {
        if (request->dialog_destroy_handler)
            g_signal_handler_disconnect (request->dialog,
                                         request->dialog_destroy_handler);
        request->dialog_destroy_handler = 0;
        gtk_window_destroy (request->dialog);
        g_clear_object (&request->dialog);
    }

    if (refresh_prices)
        gnc_gui_refresh_all ();
    price_old_remove_request_unref (request);
}

static void
price_old_remove_dialog_destroyed_cb (GtkWidget *dialog, gpointer user_data)
{
    auto request = static_cast<PriceOldRemoveRequest *> (user_data);

    (void)dialog;
    request->dialog_destroy_handler = 0;
    g_clear_object (&request->dialog);
    price_old_remove_request_complete (request, FALSE);
}

static void
price_old_remove_parent_destroyed_cb (GtkWidget *window, gpointer user_data)
{
    auto request = static_cast<PriceOldRemoveRequest *> (user_data);

    (void)window;
    request->parent_destroy_handler = 0;
    price_old_remove_request_complete (request, FALSE);
}

static void
price_old_remove_update_actions (PriceOldRemoveRequest *request)
{
    const auto have_commodities =
        (request->remove_source & PRICE_REMOVE_SOURCE_COMM) != 0;
    const auto have_sources =
        (request->remove_source & (PRICE_REMOVE_SOURCE_FQ |
                                   PRICE_REMOVE_SOURCE_USER |
                                   PRICE_REMOVE_SOURCE_APP)) != 0;

    gtk_widget_set_sensitive (request->ok_button,
                              !request->waiting_for_confirmation &&
                              have_commodities && have_sources);
}

static void
price_old_remove_change_source_flag (PriceOldRemoveRequest *request,
                                     PriceRemoveSourceFlags source,
                                     gboolean set)
{
    if (set)
        request->remove_source |= source;
    else
        request->remove_source &= ~source;

    price_old_remove_update_actions (request);
    DEBUG ("Source is: %d, remove_source is %d", source,
           request->remove_source);
}

static void
price_old_remove_check_fq_cb (GtkWidget *widget, gpointer user_data)
{
    auto request = static_cast<PriceOldRemoveRequest *> (user_data);

    price_old_remove_change_source_flag (
        request, PRICE_REMOVE_SOURCE_FQ,
        gtk_check_button_get_active (GTK_CHECK_BUTTON (widget)));
}

static void
price_old_remove_check_user_cb (GtkWidget *widget, gpointer user_data)
{
    auto request = static_cast<PriceOldRemoveRequest *> (user_data);

    price_old_remove_change_source_flag (
        request, PRICE_REMOVE_SOURCE_USER,
        gtk_check_button_get_active (GTK_CHECK_BUTTON (widget)));
}

static void
price_old_remove_check_app_cb (GtkWidget *widget, gpointer user_data)
{
    auto request = static_cast<PriceOldRemoveRequest *> (user_data);

    price_old_remove_change_source_flag (
        request, PRICE_REMOVE_SOURCE_APP,
        gtk_check_button_get_active (GTK_CHECK_BUTTON (widget)));
}

static void
price_old_remove_selection_changed_cb (GtkSelectionModel *selection,
                                       guint, guint, gpointer user_data)
{
    auto request = static_cast<PriceOldRemoveRequest *> (user_data);
    auto selected = gtk_selection_model_get_selection (selection);

    price_old_remove_change_source_flag (
        request, PRICE_REMOVE_SOURCE_COMM,
        gtk_bitset_get_size (selected) != 0);
    gtk_bitset_unref (selected);
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

static void
price_old_remove_namespace_changed_cb (GtkEditable *, gpointer user_data)
{
    auto request = static_cast<PriceOldRemoveRequest *> (user_data);
    GtkWidget *window = nullptr;
    auto pdb_dialog = price_old_remove_get_prices_dialog (request, &window);

    if (!pdb_dialog)
    {
        g_clear_object (&window);
        price_old_remove_request_complete (request, FALSE);
        return;
    }

    g_free (request->target_namespace_name);
    request->target_namespace_name = gnc_ui_namespace_picker_ns (
        request->namespace_picker);
    gnc_prices_dialog_load_view (request->remove_view, pdb_dialog->price_db,
                                 request->target_namespace_name);
    g_clear_object (&window);
}

static PriceRemoveKeepOptions
price_old_remove_get_keep_option (const PriceOldRemoveRequest *request)
{
    if (gtk_check_button_get_active (GTK_CHECK_BUTTON (request->keep_none)))
        return PRICE_REMOVE_KEEP_NONE;
    if (gtk_check_button_get_active (
            GTK_CHECK_BUTTON (request->keep_last_month)))
        return PRICE_REMOVE_KEEP_LAST_MONTHLY;
    if (gtk_check_button_get_active (
            GTK_CHECK_BUTTON (request->keep_last_quarter)))
        return PRICE_REMOVE_KEEP_LAST_QUARTERLY;
    if (gtk_check_button_get_active (
            GTK_CHECK_BUTTON (request->keep_last_period)))
        return PRICE_REMOVE_KEEP_LAST_PERIOD;
    if (gtk_check_button_get_active (GTK_CHECK_BUTTON (request->keep_scaled)))
        return PRICE_REMOVE_KEEP_SCALED;

    return PRICE_REMOVE_KEEP_LAST_WEEKLY;
}

static gboolean
price_old_remove_execute (PriceOldRemoveRequest *request)
{
    GtkWidget *window = nullptr;
    auto pdb_dialog = price_old_remove_get_prices_dialog (request, &window);
    auto comm_list = gnc_prices_dialog_get_commodities (request->remove_view);
    gboolean deleted = FALSE;

    if (!pdb_dialog || !comm_list)
    {
        g_list_free (comm_list);
        g_clear_object (&window);
        return FALSE;
    }

    const auto last = gnc_date_edit_get_date (GNC_DATE_EDIT (request->date));
    auto fiscal_end_date = get_fiscal_end_date ();
    const auto keep = price_old_remove_get_keep_option (request);
    /* Keep the selection model connected while the engine emits one event per deletion. */
    gnc_tree_view_price_suspend_updates (pdb_dialog->price_tree);

    DEBUG ("deleting prices for keep option %d", keep);
    if (keep != PRICE_REMOVE_KEEP_SCALED)
    {
        gnc_pricedb_remove_old_prices (
            pdb_dialog->price_db, comm_list, &fiscal_end_date, last,
            static_cast<PriceRemoveSourceFlags> (request->remove_source), keep);
    }
    else
    {
        auto tmp_date = time64_to_gdate (last);
        g_date_subtract_months (&tmp_date, 6);
        auto tmp = gdate_to_time64 (tmp_date);

        gnc_pricedb_remove_old_prices (
            pdb_dialog->price_db, comm_list, &fiscal_end_date, tmp,
            static_cast<PriceRemoveSourceFlags> (request->remove_source),
            PRICE_REMOVE_KEEP_LAST_WEEKLY);
        g_date_subtract_months (&tmp_date, 6);
        tmp = gdate_to_time64 (tmp_date);
        gnc_pricedb_remove_old_prices (
            pdb_dialog->price_db, comm_list, &fiscal_end_date, tmp,
            static_cast<PriceRemoveSourceFlags> (request->remove_source),
            PRICE_REMOVE_KEEP_LAST_MONTHLY);
    }

    gnc_tree_view_price_resume_updates (pdb_dialog->price_tree);
    g_list_free (comm_list);
    g_clear_object (&window);
    deleted = TRUE;
    return deleted;
}

static void
price_old_remove_dialog_present (PriceOldRemoveRequest *request);

static void
price_old_remove_confirmation_finished (GtkWindow *, gint response,
                                        gpointer user_data)
{
    auto request = static_cast<PriceOldRemoveRequest *> (user_data);
    const auto deleted = !request->completed && response == GTK_RESPONSE_OK &&
                         price_old_remove_execute (request);

    if (deleted)
        price_old_remove_request_complete (request, TRUE);
    else if (!request->completed)
    {
        request->waiting_for_confirmation = FALSE;
        price_old_remove_update_actions (request);
        price_old_remove_dialog_present (request);
    }
    price_old_remove_request_unref (request);
}

static void
price_old_remove_start_confirmation (PriceOldRemoveRequest *request)
{
    auto comm_list = gnc_prices_dialog_get_commodities (request->remove_view);

    if (!comm_list)
        return;
    g_list_free (comm_list);

    request->waiting_for_confirmation = TRUE;
    price_old_remove_update_actions (request);
    price_old_remove_request_ref (request);
    gnc_action_dialog_async (
        request->dialog, _("Delete"), FALSE,
        price_old_remove_confirmation_finished, request,
        "%s", _("Are you sure you want to delete these prices?"));
}

static void
price_old_remove_ok_clicked_cb (GtkButton *button, gpointer user_data)
{
    auto request = static_cast<PriceOldRemoveRequest *> (user_data);

    (void)button;
    if (!request->completed && !request->waiting_for_confirmation)
        price_old_remove_start_confirmation (request);
}

static void
price_old_remove_cancel_clicked_cb (GtkButton *button, gpointer user_data)
{
    (void)button;
    price_old_remove_request_complete (
        static_cast<PriceOldRemoveRequest *> (user_data), FALSE);
}

static gboolean
price_old_remove_close_request_cb (GtkWindow *dialog, gpointer user_data)
{
    (void)dialog;
    price_old_remove_request_complete (
        static_cast<PriceOldRemoveRequest *> (user_data), FALSE);
    return TRUE;
}

static void
price_old_remove_dialog_present (PriceOldRemoveRequest *request)
{
    if (!request->completed && request->dialog)
        gtk_window_present (request->dialog);
}

void
gnc_prices_dialog_remove_old_clicked (GtkWidget *widget, gpointer data)
{
    auto pdb_dialog = static_cast<PricesDialog *> (data);
    GtkBuilder *builder;
    GtkWidget *dialog;
    GtkWidget *box;
    GtkWidget *label;
    GtkSelectionModel *selection;
    PriceOldRemoveRequest *request;

    (void)widget;
    ENTER (" ");
    if (!pdb_dialog || pdb_dialog->book != gnc_get_current_book () ||
        qof_book_shutting_down (pdb_dialog->book))
        return;

    builder = gtk_builder_new ();
    gnc_builder_add_from_file (builder, "dialog-price.ui", "deletion_date_dialog");
    dialog = GTK_WIDGET (gtk_builder_get_object (builder, "deletion_date_dialog"));
    if (!dialog)
    {
        g_object_unref (builder);
        return;
    }

    request = g_new0 (PriceOldRemoveRequest, 1);
    g_atomic_ref_count_init (&request->ref_count);
    g_weak_ref_init (&request->price_window, pdb_dialog->window);
    request->dialog = GTK_WINDOW (g_object_ref (dialog));
    request->date = gnc_date_edit_new (time (nullptr), FALSE, FALSE);
    request->remove_view = GTK_WIDGET (gtk_builder_get_object (builder,
                                                                 "commodity_list"));
    request->namespace_picker = GTK_WIDGET (gtk_builder_get_object (
        builder, "namespace_combo_we"));
    request->ok_button = GTK_WIDGET (gtk_builder_get_object (builder,
                                                               "ok_button"));
    request->keep_none = GTK_WIDGET (gtk_builder_get_object (builder,
                                                               "radiobutton_none"));
    request->keep_last_month = GTK_WIDGET (gtk_builder_get_object (
        builder, "radiobutton_last_month"));
    request->keep_last_quarter = GTK_WIDGET (gtk_builder_get_object (
        builder, "radiobutton_last_quarter"));
    request->keep_last_period = GTK_WIDGET (gtk_builder_get_object (
        builder, "radiobutton_last_period"));
    request->keep_scaled = GTK_WIDGET (gtk_builder_get_object (builder,
                                                                 "radiobutton_scaled"));

    box = GTK_WIDGET (gtk_builder_get_object (builder, "date_hbox"));
    gnc_box_append_full (GTK_BOX (box), request->date, FALSE, FALSE, 0);
    gtk_widget_set_visible (request->date, TRUE);
    gtk_entry_set_activates_default (
        GTK_ENTRY (GNC_DATE_EDIT (request->date)->date_entry), TRUE);
    label = GTK_WIDGET (gtk_builder_get_object (builder, "date_label"));
    gnc_date_make_mnemonic_target (GNC_DATE_EDIT (request->date), label);

    gnc_ui_commodity_picker_setup (request->namespace_picker);
    label = GTK_WIDGET (gtk_builder_get_object (builder,
                                                 "remove_namespace_label"));
    gtk_label_set_mnemonic_widget (
        GTK_LABEL (label), GTK_WIDGET (
            gnc_ui_commodity_picker_get_entry (request->namespace_picker)));
    gnc_ui_update_namespace_picker (request->namespace_picker,
                                    GNC_COMMODITY_NS_NONISO_GUI, DIAG_COMM_ALL);
    request->target_namespace_name = g_strdup (GNC_COMMODITY_NS_NONISO_GUI);
    g_signal_connect (
        gnc_ui_commodity_picker_get_entry (request->namespace_picker), "changed",
        G_CALLBACK (price_old_remove_namespace_changed_cb), request);

    auto model = g_list_store_new (G_TYPE_OBJECT);
    g_object_set_data_full (G_OBJECT (request->remove_view),
                            PRICE_REMOVE_MODEL_DATA, model, g_object_unref);
    selection = GTK_SELECTION_MODEL (gtk_multi_selection_new (
        G_LIST_MODEL (g_object_ref (model))));
    gtk_column_view_set_model (GTK_COLUMN_VIEW (request->remove_view),
                               selection);
    price_remove_append_column (GTK_COLUMN_VIEW (request->remove_view),
                                _("Commodity"), PRICE_REMOVE_FULL_NAME_DATA);
    price_remove_append_column (GTK_COLUMN_VIEW (request->remove_view),
                                _("First Date"), PRICE_REMOVE_DATE_DATA);
    price_remove_append_column (GTK_COLUMN_VIEW (request->remove_view),
                                _("Entries"), PRICE_REMOVE_COUNT_DATA);
    gnc_prices_dialog_load_view (request->remove_view, pdb_dialog->price_db,
                                 request->target_namespace_name);
    g_signal_connect (selection, "selection-changed",
                      G_CALLBACK (price_old_remove_selection_changed_cb), request);
    g_object_unref (selection);

    request->remove_source = PRICE_REMOVE_SOURCE_FQ;
    price_old_remove_change_source_flag (request, PRICE_REMOVE_SOURCE_FQ, TRUE);
    auto button = GTK_WIDGET (gtk_builder_get_object (builder, "checkbutton_fq"));
    g_signal_connect (button, "toggled", G_CALLBACK (price_old_remove_check_fq_cb),
                      request);
    button = GTK_WIDGET (gtk_builder_get_object (builder, "checkbutton_user"));
    g_signal_connect (button, "toggled", G_CALLBACK (price_old_remove_check_user_cb),
                      request);
    button = GTK_WIDGET (gtk_builder_get_object (builder, "checkbutton_app"));
    g_signal_connect (button, "toggled", G_CALLBACK (price_old_remove_check_app_cb),
                      request);
    button = GTK_WIDGET (gtk_builder_get_object (builder, "cancel_button"));
    g_signal_connect (button, "clicked", G_CALLBACK (price_old_remove_cancel_clicked_cb),
                      request);
    g_signal_connect (request->ok_button, "clicked",
                      G_CALLBACK (price_old_remove_ok_clicked_cb), request);
    gtk_window_set_default_widget (request->dialog, request->ok_button);
    gtk_window_set_transient_for (request->dialog, GTK_WINDOW (pdb_dialog->window));
    gtk_window_set_modal (request->dialog, TRUE);
    request->parent_destroy_handler = g_signal_connect (
        pdb_dialog->window, "destroy",
        G_CALLBACK (price_old_remove_parent_destroyed_cb), request);
    request->dialog_destroy_handler = g_signal_connect (
        request->dialog, "destroy", G_CALLBACK (price_old_remove_dialog_destroyed_cb),
        request);
    g_signal_connect (request->dialog, "close-request",
                      G_CALLBACK (price_old_remove_close_request_cb), request);
    g_object_unref (builder);
    price_old_remove_dialog_present (request);
    LEAVE (" ");
}

void
gnc_prices_dialog_add_clicked (GtkWidget *widget, gpointer data)
{
    auto pdb_dialog = static_cast<PricesDialog *> (data);

    (void)widget;
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

    (void)widget;

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
gnc_prices_dialog_selection_changed (GtkSelectionModel *selection,
                                     guint position, guint n_items,
                                     gpointer data)
{
    auto pdb_dialog = static_cast<PricesDialog *> (data);
    auto price_list = gnc_tree_view_price_get_selected_prices (pdb_dialog->price_tree);
    auto selected_prices = g_list_length (price_list);
    auto selected_rows = gtk_selection_model_get_selection (selection);

    g_list_free (price_list);
    /* A selected namespace or commodity is not a mutable price row. */
    if (gtk_bitset_get_size (selected_rows) > selected_prices)
        selected_prices = 0;
    gtk_bitset_unref (selected_rows);

    gtk_widget_set_sensitive (pdb_dialog->edit_button, selected_prices == 1);
    gtk_widget_set_sensitive (pdb_dialog->remove_button, selected_prices >= 1);
    gtk_widget_set_sensitive (pdb_dialog->add_button, selected_prices <= 1);
    (void)position;
    (void)n_items;
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
row_activated_cb (GtkColumnView *column_view, guint position, gpointer data)
{
    auto pdb_dialog = static_cast<PricesDialog *> (data);

    /* The activated position, not an unrelated existing cursor, decides the action. */
    auto selection = gnc_tree_view_price_get_selection_model (pdb_dialog->price_tree);
    gtk_selection_model_select_item (selection, position, TRUE);

    if (gnc_tree_view_price_get_cursor_price (pdb_dialog->price_tree))
        gnc_prices_dialog_edit_clicked (GTK_WIDGET (column_view), data);
    else
        gnc_tree_view_price_toggle_expand (pdb_dialog->price_tree, position);
}

static void
gnc_prices_dialog_create (GtkWidget * parent, PricesDialog *pdb_dialog)
{
    GtkWidget *window, *scrolled_window;
    GtkBuilder *builder;
    GtkWidget *view;
    GtkSelectionModel *selection;

    ENTER(" ");
    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-price.ui", "prices_window");

    window = GTK_WIDGET(gtk_builder_get_object (builder, "prices_window"));
    pdb_dialog->window = window;
    g_object_set_data (G_OBJECT (window), PRICE_DIALOG_DATA, pdb_dialog);

    // Set the name for this dialog so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(window), "gnc-id-price-edit");
    gnc_widget_style_context_add_class (GTK_WIDGET(window), "gnc-class-securities");

    pdb_dialog->session = gnc_get_current_session();
    pdb_dialog->book = qof_session_get_book(pdb_dialog->session);
    pdb_dialog->price_db = gnc_pricedb_get_db(pdb_dialog->book);

    g_signal_connect (pdb_dialog->window, "close-request",
                      G_CALLBACK(gnc_prices_dialog_close_request_cb), pdb_dialog);

    GtkEventController *key_controller = gtk_event_controller_key_new ();
    gtk_widget_add_controller (pdb_dialog->window, key_controller);
    g_signal_connect (key_controller, "key-pressed",
                      G_CALLBACK (gnc_prices_dialog_key_pressed_cb), pdb_dialog);

    /* price tree */
    scrolled_window = GTK_WIDGET(gtk_builder_get_object (builder, "price_list_window"));
    view = gnc_tree_view_price_new(pdb_dialog->book,
                                   "state-section", STATE_SECTION,
                                   "show-column-menu", TRUE,
                                   NULL);
    pdb_dialog->price_tree = GNC_TREE_VIEW_PRICE(view);
    gtk_scrolled_window_set_child (GTK_SCROLLED_WINDOW(scrolled_window),
                                   GTK_WIDGET(view));
    gnc_tree_view_price_set_filter (pdb_dialog->price_tree,
                                    gnc_price_dialog_filter_ns_func,
                                    gnc_price_dialog_filter_cm_func,
                                    NULL,
                                    pdb_dialog, NULL);

    selection = gnc_tree_view_price_get_selection_model (pdb_dialog->price_tree);
    g_signal_connect (selection, "selection-changed",
                      G_CALLBACK (gnc_prices_dialog_selection_changed), pdb_dialog);
    g_signal_connect (gnc_tree_view_price_get_column_view (pdb_dialog->price_tree),
                      "activate", G_CALLBACK (row_activated_cb), pdb_dialog);

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
        gtk_window_set_default_widget (GTK_WINDOW (pdb_dialog->window), button);
        gtk_widget_grab_focus (button);

    }

    g_signal_connect (pdb_dialog->window, "destroy",
                      G_CALLBACK(gnc_prices_dialog_destroy_cb), pdb_dialog);

    gnc_builder_connect_signals_full (builder, gnc_builder_connect_full_func, pdb_dialog);
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
    gtk_window_destroy (GTK_WINDOW(pdb_dialog->window));
    LEAVE(" ");
}


static void
refresh_handler (GHashTable *changes, gpointer user_data)
{
    (void)changes;
    (void)user_data;
    ENTER(" ");
    LEAVE(" ");
}


static gboolean
show_handler (const char *klass, gint component_id,
              gpointer user_data, gpointer iter_data)
{
    auto pdb_dialog = static_cast<PricesDialog *> (user_data);

    (void)klass;
    (void)component_id;
    (void)iter_data;
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


static gboolean
gnc_prices_dialog_key_pressed_cb (GtkEventControllerKey *key,
                                   guint keyval, guint keycode,
                                   GdkModifierType state, gpointer data)
{
    auto pdb_dialog = static_cast<PricesDialog *> (data);

    (void)key;
    (void)keycode;
    (void)state;
    if (keyval == GDK_KEY_Escape)
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

    gtk_window_present (GTK_WINDOW (pdb_dialog->window));
    LEAVE(" ");
}
