/********************************************************************\
 * dialog-commodities.c -- commodities dialog                       *
 * Copyright (C) 2001 Gnumatic, Inc.                                *
 * Author: Dave Peticolas <dave@krondo.com>                         *
 * Copyright (C) 2003,2005 David Hampton                            *
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

#include "dialog-commodity.h"
#include "gnc-commodity.hpp"
#include "dialog-utils.h"
#include "gnc-commodity.h"
#include "gnc-component-manager.h"
#include "qof.h"
#include "gnc-tree-view-commodity.h"
#include "gnc-prefs.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "gnc-gnome-utils.h"
#include "gnc-session.h"
#include "gnc-warnings.h"
#include "Account.hpp"

#include <vector>
#include <string>

#define DIALOG_COMMODITIES_CM_CLASS "dialog-commodities"
#define STATE_SECTION "dialogs/edit_commodities"
#define GNC_PREFS_GROUP   "dialogs.commodities"
#define GNC_PREF_INCL_ISO "include-iso"

/* This static indicates the debugging module that this .o belongs to.  */
/* static short module = MOD_GUI; */

typedef struct
{
    GtkWidget * window;
    QofSession *session;
    QofBook *book;

    GncTreeViewCommodity * commodity_tree;
    GtkWidget * edit_button;
    GtkWidget * remove_button;
    gboolean    show_currencies;
    GtkWidget * rename_namespace_button;

    gboolean is_new;
} CommoditiesDialog;


void gnc_commodities_window_destroy_cb (GtkWidget *object, CommoditiesDialog *cd);

extern "C" {
void gnc_commodities_dialog_add_clicked (GtkWidget *widget, gpointer data);
void gnc_commodities_dialog_edit_clicked (GtkWidget *widget, gpointer data);
void gnc_commodities_dialog_remove_clicked (GtkWidget *widget, gpointer data);
void gnc_commodities_dialog_close_clicked (GtkWidget *widget, gpointer data);

void gnc_commodities_dialog_rename_namespace_clicked (GtkWidget *widget, gpointer data);

void gnc_commodities_show_currencies_toggled (GtkToggleButton *toggle, CommoditiesDialog *cd);
}

struct RenameNamespaceRequest
{
    QofBook *book{};
    GncGUID namespace_guid{};
    gchar *old_name{};
    GWeakRef entry;
    GWeakRef label;
    bool completed{};
    bool responding{};
};

static void
rename_namespace_request_free (gpointer data)
{
    auto request = static_cast<RenameNamespaceRequest*>(data);
    if (request->book)
        g_object_remove_weak_pointer (G_OBJECT (request->book),
                                      reinterpret_cast<gpointer*>(&request->book));
    g_weak_ref_clear (&request->entry);
    g_weak_ref_clear (&request->label);
    g_free (request->old_name);
    delete request;
}

static void
rename_namespace_dialog_destroyed ([[maybe_unused]] GtkWidget *dialog,
                                  gpointer data)
{
    static_cast<RenameNamespaceRequest*>(data)->completed = true;
}

static void
rename_namespace_response (GtkDialog *dialog, gint response, gpointer data)
{
    auto request = static_cast<RenameNamespaceRequest*>(data);
    g_object_ref (dialog);
    if (request->completed || request->responding)
    {
        g_object_unref (dialog);
        return;
    }

    if (response != GTK_RESPONSE_OK)
    {
        request->completed = true;
        gtk_widget_destroy (GTK_WIDGET (dialog));
        g_object_unref (dialog);
        return;
    }
    request->responding = true;

    auto entry = GTK_ENTRY (g_weak_ref_get (&request->entry));
    auto label = GTK_LABEL (g_weak_ref_get (&request->label));
    if (!entry || !label)
    {
        g_clear_object (&entry);
        g_clear_object (&label);
        request->responding = false;
        g_object_unref (dialog);
        return;
    }

    auto new_name = g_strdup (gtk_entry_get_text (entry));
    if (!new_name || !*new_name)
    {
        gtk_label_set_text (label, _("No new name"));
        g_free (new_name);
        g_object_unref (entry);
        g_object_unref (label);
        request->responding = false;
        g_object_unref (dialog);
        return;
    }

    auto book = request->book;
    bool renamed = false;
    if (book && book == gnc_get_current_book () && qof_book_is_open (book) &&
        !qof_book_shutting_down (book) && !qof_book_is_readonly (book))
    {
        auto table = gnc_commodity_table_get_table (book);
        auto current_namespace = gnc_commodity_table_find_namespace (
            table, request->old_name);
        if (current_namespace &&
            guid_equal (&request->namespace_guid,
                        qof_instance_get_guid (current_namespace)))
            renamed = gnc_commodity_table_rename_namespace (
                table, request->old_name, new_name);
    }

    if (renamed)
    {
        // The engine call may emit events that close this dialog or book.
        request->completed = true;
        request->responding = false;
        if (request->book && request->book == gnc_get_current_book () &&
            qof_book_is_open (request->book) &&
            !qof_book_shutting_down (request->book))
            qof_book_mark_session_dirty (request->book);
        if (!gtk_widget_in_destruction (GTK_WIDGET (dialog)))
            gtk_widget_destroy (GTK_WIDGET (dialog));
    }
    else if (!request->completed &&
             !gtk_widget_in_destruction (GTK_WIDGET (dialog)))
    {
        gtk_label_set_text (label, _("Rename failed, possibly new name exists"));
        request->responding = false;
    }
    else
        request->responding = false;

    g_free (new_name);
    g_object_unref (entry);
    g_object_unref (label);
    g_object_unref (dialog);
}

static gboolean gnc_commodities_window_key_press_cb (GtkWidget *widget,
                                                     GdkEventKey *event,
                                                     gpointer data);


void
gnc_commodities_window_destroy_cb (GtkWidget *object,   CommoditiesDialog *cd)
{
    gnc_unregister_gui_component_by_data (DIALOG_COMMODITIES_CM_CLASS, cd);

    if (cd->window)
    {
        gtk_widget_destroy (cd->window);
        cd->window = NULL;
    }
    g_free (cd);
}

static gboolean
gnc_commodities_window_delete_event_cb (GtkWidget *widget,
                                        GdkEvent  *event,
                                        gpointer   data)
{
    auto cd = static_cast<CommoditiesDialog*>(data);
    // this cb allows the window size to be saved on closing with the X
    gnc_save_window_size (GNC_PREFS_GROUP,
                          GTK_WINDOW(cd->window));
    return FALSE;
}

struct CommodityActionRequest
{
    QofBook *book{};
    GWeakRef tree;
    GncGUID original_guid{};
    bool edit{};
};

static void
commodity_action_complete (QofBook *book, gnc_commodity *commodity,
                           gpointer data)
{
    auto request = static_cast<CommodityActionRequest*>(data);
    auto tree = GNC_TREE_VIEW_COMMODITY (g_weak_ref_get (&request->tree));
    auto original_book = request->book;
    bool valid = commodity && book && book == original_book &&
        original_book == gnc_get_current_book () &&
        qof_book_is_open (original_book) &&
        !qof_book_shutting_down (original_book) &&
        !qof_book_is_readonly (original_book);
    if (valid)
    {
        auto name_space = gnc_commodity_get_namespace (commodity);
        auto mnemonic = gnc_commodity_get_mnemonic (commodity);
        auto registered = gnc_commodity_table_lookup (
            gnc_commodity_table_get_table (original_book), name_space, mnemonic);
        valid = registered == commodity &&
            (!request->edit ||
             guid_equal (&request->original_guid,
                         qof_instance_get_guid (commodity)));
    }
    if (tree && valid)
    {
        gnc_tree_view_commodity_select_commodity (tree, commodity);
        gnc_gui_refresh_all ();
    }
    g_clear_object (&tree);
    g_weak_ref_clear (&request->tree);
    if (request->book)
        g_object_remove_weak_pointer (G_OBJECT (request->book),
                                      reinterpret_cast<gpointer*>(&request->book));
    delete request;
}

static CommodityActionRequest *
commodity_action_request_new (CommoditiesDialog *cd, gnc_commodity *original)
{
    auto book = gnc_get_current_book ();
    if (!cd || !cd->commodity_tree || !book || book != cd->book ||
        !qof_book_is_open (book) || qof_book_shutting_down (book) ||
        qof_book_is_readonly (book))
        return nullptr;
    auto request = new CommodityActionRequest;
    request->book = book;
    request->edit = original != nullptr;
    if (original)
        request->original_guid = *qof_instance_get_guid (original);
    g_weak_ref_init (&request->tree, G_OBJECT (cd->commodity_tree));
    g_object_add_weak_pointer (G_OBJECT (book),
                               reinterpret_cast<gpointer*>(&request->book));
    return request;
}

void
gnc_commodities_dialog_edit_clicked (GtkWidget *widget, gpointer data)
{
    auto cd = static_cast<CommoditiesDialog*>(data);
    gnc_commodity *commodity;

    commodity = gnc_tree_view_commodity_get_selected_commodity (cd->commodity_tree);
    if (commodity == NULL)
        return;

    auto request = commodity_action_request_new (cd, commodity);
    if (request)
        gnc_ui_edit_commodity_async (commodity, cd->window,
                                     commodity_action_complete, request);
}

static void
row_activated_cb (GtkTreeView *view, GtkTreePath *path,
                  GtkTreeViewColumn *column, CommoditiesDialog *cd)
{
    GtkTreeModel *model;
    GtkTreeIter iter;

    g_return_if_fail(view);

    model = gtk_tree_view_get_model(view);
    if (gtk_tree_model_get_iter(model, &iter, path))
    {
        if (gtk_tree_model_iter_has_child(model, &iter))
        {
            /* There are children, so it's not a commodity.
             * Just expand or collapse the row. */
            if (gtk_tree_view_row_expanded(view, path))
                gtk_tree_view_collapse_row(view, path);
            else
                gtk_tree_view_expand_row(view, path, FALSE);
        }
        else
            /* It's a commodity, so click the Edit button. */
            gnc_commodities_dialog_edit_clicked (NULL, cd);
    }
}

static void
commodity_delete_decided (GtkWindow *parent, gint response, gpointer user_data)
{
    auto request = static_cast<CommodityActionRequest *> (user_data);
    auto book = request->book;
    if (book) g_object_ref (book);
    if (parent && response == GTK_RESPONSE_OK && book && gnc_current_session_exist () &&
        gnc_get_current_book () == book && !qof_book_is_readonly (book))
    {
        auto commodity = gnc_commodity_find_commodity_by_guid (&request->original_guid, book);
        bool in_use = false;
        if (commodity)
            gnc_account_foreach_descendant (gnc_book_get_root_account (book),
                [commodity, &in_use] (auto account) {
                    if (xaccAccountGetCommodity (account) == commodity) in_use = true;
                });
        if (commodity && !in_use)
        {
            auto database = gnc_pricedb_get_db (book);
            auto prices = gnc_pricedb_get_prices (database, commodity, nullptr);
            gnc_suspend_gui_refresh ();
            for (auto node = prices; node; node = node->next)
                gnc_pricedb_remove_price (database, GNC_PRICE (node->data));
            gnc_price_list_destroy (prices);
            gnc_commodity_table_remove (gnc_commodity_table_get_table (book), commodity);
            gnc_commodity_destroy (commodity);
            auto tree = g_weak_ref_get (&request->tree);
            if (tree)
                gtk_tree_selection_unselect_all (gtk_tree_view_get_selection (GTK_TREE_VIEW (tree)));
            g_clear_object (&tree);
            gnc_resume_gui_refresh ();
        }
    }
    commodity_action_complete (nullptr, nullptr, request);
    g_clear_object (&book);
}

void
gnc_commodities_dialog_remove_clicked (GtkWidget *widget, gpointer data)
{
    auto cd = static_cast<CommoditiesDialog*>(data);
    GNCPriceDB *pdb;
    GList *prices;
    gnc_commodity *commodity;
    GtkWidget *dialog;
    const gchar *message, *warning;

    commodity = gnc_tree_view_commodity_get_selected_commodity (cd->commodity_tree);
    if (commodity == NULL)
        return;

    std::vector<Account*> commodity_accounts;

    gnc_account_foreach_descendant (gnc_book_get_root_account(cd->book),
                                    [commodity, &commodity_accounts](auto acct)
                                    {
                                        if (commodity == xaccAccountGetCommodity (acct))
                                            commodity_accounts.push_back (acct);
                                    });

    /* FIXME check for transaction references */

    if (!commodity_accounts.empty())
    {
        std::string msg{_("This commodity is currently used by the following accounts. You may "
                          "not delete it.\n")};

        for (const auto acct : commodity_accounts)
        {
            auto full_name = gnc_account_get_full_name (acct);
            msg.append ("\n* ").append (full_name);
            g_free (full_name);
        }

        gnc_warning_dialog (GTK_WINDOW (cd->window), "%s", msg.c_str());
        return;
    }

    pdb = gnc_pricedb_get_db (cd->book);
    prices = gnc_pricedb_get_prices (pdb, commodity, NULL);
    if (prices)
    {
        message = _("This commodity has price quotes. Are "
                    "you sure you want to delete the selected "
                    "commodity and its price quotes?");
        warning = GNC_PREF_WARN_PRICE_COMM_DEL_QUOTES;
    }
    else
    {
        message = _("Are you sure you want to delete the "
                    "selected commodity?");
        warning = GNC_PREF_WARN_PRICE_COMM_DEL;
    }

    dialog = gtk_message_dialog_new (GTK_WINDOW(cd->window),
                                     GTK_DIALOG_DESTROY_WITH_PARENT,
                                     GTK_MESSAGE_QUESTION,
                                     GTK_BUTTONS_NONE,
                                     "%s", _("Delete commodity?"));
    gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG(dialog),
                                              "%s", message);
    gtk_dialog_add_buttons (GTK_DIALOG(dialog),
                            _("_Cancel"), GTK_RESPONSE_CANCEL,
                            _("_Delete"), GTK_RESPONSE_OK,
                            (gchar *)NULL);
    gnc_price_list_destroy (prices);
    auto request = commodity_action_request_new (cd, commodity);
    if (!request)
    {
        gtk_widget_destroy (dialog);
        return;
    }
    gnc_dialog_run_async (GTK_DIALOG (dialog), warning, commodity_delete_decided, request);
}

void
gnc_commodities_dialog_add_clicked (GtkWidget *widget, gpointer data)
{
    auto cd = static_cast<CommoditiesDialog*>(data);
    gnc_commodity *commodity;

    commodity = gnc_tree_view_commodity_get_selected_commodity (cd->commodity_tree);
    auto request = commodity_action_request_new (cd, nullptr);
    if (request)
        gnc_ui_new_commodity_async_full (
            commodity ? gnc_commodity_get_namespace (commodity) : nullptr,
            cd->window, nullptr, nullptr, nullptr, nullptr, 10000,
            commodity_action_complete, request);
}

void
gnc_commodities_dialog_close_clicked (GtkWidget *widget, gpointer data)
{
    auto cd = static_cast<CommoditiesDialog*>(data);

    gnc_close_gui_component_by_data (DIALOG_COMMODITIES_CM_CLASS, cd);
}

void
gnc_commodities_dialog_rename_namespace_clicked (GtkWidget *widget, gpointer data)
{
    auto cd = static_cast<CommoditiesDialog*>(data);
    auto ns = gnc_tree_view_commodity_get_selected_namespace (cd->commodity_tree);

    if (!ns)
        return;

    auto book = gnc_get_current_book ();
    if (!book || book != cd->book || !qof_book_is_open (book) ||
        qof_book_shutting_down (book) || qof_book_is_readonly (book))
        return;

    const auto ns_name = g_strdup (gnc_commodity_namespace_get_name (ns));

    GtkBuilder *builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-commodities.glade", "rename_namespace_dialog");

    GtkDialog *dialog = GTK_DIALOG(gtk_builder_get_object (builder, "rename_namespace_dialog"));
    GtkWidget *entry = GTK_WIDGET(gtk_builder_get_object (builder, "rename_entry"));
    GtkWidget *label = GTK_WIDGET(gtk_builder_get_object (builder, "rename_label"));

    // Set the name for this dialog so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(dialog), "gnc-id-rename-namespace");
    gnc_widget_style_context_add_class (GTK_WIDGET(dialog), "gnc-class-securities");

    // Entry
    gtk_entry_set_text (GTK_ENTRY(entry), ns_name);
    gtk_editable_select_region (GTK_EDITABLE(entry), 0, -1);
    gtk_entry_set_activates_default (GTK_ENTRY(entry), true);

    // Set our parent
    auto parent = gtk_widget_get_toplevel (widget);
    if (GTK_IS_WINDOW (parent))
    {
        gtk_window_set_transient_for (GTK_WINDOW(dialog), GTK_WINDOW(parent));
        gtk_window_set_destroy_with_parent (GTK_WINDOW(dialog), TRUE);
    }
    gtk_window_set_modal (GTK_WINDOW (dialog), TRUE);

    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, nullptr);
    gtk_dialog_set_default_response (GTK_DIALOG(dialog), GTK_RESPONSE_OK);
    auto request = new RenameNamespaceRequest;
    request->book = book;
    request->namespace_guid = *qof_instance_get_guid (ns);
    request->old_name = ns_name;
    g_weak_ref_init (&request->entry, entry);
    g_weak_ref_init (&request->label, label);
    g_object_add_weak_pointer (G_OBJECT (book),
                               reinterpret_cast<gpointer*>(&request->book));
    g_object_set_data_full (G_OBJECT (dialog), "gnc-rename-namespace-request",
                            request, rename_namespace_request_free);
    g_signal_connect (dialog, "response", G_CALLBACK (rename_namespace_response),
                      request);
    g_signal_connect (dialog, "destroy",
                      G_CALLBACK (rename_namespace_dialog_destroyed), request);
    g_object_unref (G_OBJECT(builder));
    gtk_widget_show_all (GTK_WIDGET (dialog));
}

static void
gnc_commodities_dialog_selection_changed (GtkTreeSelection *selection,
        CommoditiesDialog *cd)
{
    gboolean remove_ok;
    gnc_commodity *commodity;

    commodity = gnc_tree_view_commodity_get_selected_commodity (cd->commodity_tree);
    remove_ok = commodity && !gnc_commodity_is_iso(commodity);
    gtk_widget_set_sensitive (cd->edit_button, commodity != NULL);
    gtk_widget_set_sensitive (cd->remove_button, remove_ok);

    gtk_widget_set_sensitive (cd->rename_namespace_button, !commodity);

    if (!commodity)
    {
        gnc_commodity_namespace *ns = gnc_tree_view_commodity_get_selected_namespace (cd->commodity_tree);
        const char *ns_name = gnc_commodity_namespace_get_name (ns);

        gtk_widget_set_sensitive (cd->rename_namespace_button,
                                  !(g_strcmp0 (ns_name, GNC_COMMODITY_NS_LEGACY) == 0 ||
                                    g_strcmp0 (ns_name, GNC_COMMODITY_NS_CURRENCY) == 0));
    }
}

void
gnc_commodities_show_currencies_toggled (GtkToggleButton *toggle,
        CommoditiesDialog *cd)
{
    cd->show_currencies = gtk_toggle_button_get_active (toggle);
    gnc_tree_view_commodity_refilter (cd->commodity_tree);
}

static gboolean
gnc_commodities_dialog_filter_ns_func (gnc_commodity_namespace *name_space,
                                       gpointer data)
{
    auto cd = static_cast<CommoditiesDialog*>(data);
    const gchar *name;
    GList *list;

    /* Never show the template list */
    name = gnc_commodity_namespace_get_name (name_space);
    if (g_strcmp0 (name, GNC_COMMODITY_NS_TEMPLATE) == 0)
        return FALSE;

    /* Check whether or not to show commodities */
    if (!cd->show_currencies && gnc_commodity_namespace_is_iso(name))
        return FALSE;

    /* Show any other namespace that has commodities */
    list = gnc_commodity_namespace_get_commodity_list(name_space);
    gboolean rv = (list != NULL);
    g_list_free (list);
    return rv;
}

static gboolean
gnc_commodities_dialog_filter_cm_func (gnc_commodity *commodity,
                                       gpointer data)
{
    auto cd = static_cast<CommoditiesDialog*>(data);

    if (cd->show_currencies)
        return TRUE;
    return !gnc_commodity_is_iso(commodity);
}

static void
gnc_commodities_dialog_create (GtkWidget * parent, CommoditiesDialog *cd)
{
    GtkWidget *button;
    GtkWidget *scrolled_window;
    GtkBuilder *builder;
    GtkTreeView *view;
    GtkTreeSelection *selection;

    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-commodities.glade", "securities_window");

    cd->window = GTK_WIDGET(gtk_builder_get_object (builder, "securities_window"));
    cd->session = gnc_get_current_session();
    cd->book = qof_session_get_book(cd->session);
    cd->show_currencies = gnc_prefs_get_bool(GNC_PREFS_GROUP, GNC_PREF_INCL_ISO);

    // Set the name for this dialog so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(cd->window), "gnc-id-commodity");
    gnc_widget_style_context_add_class (GTK_WIDGET(cd->window), "gnc-class-securities");

    /* buttons */
    cd->remove_button = GTK_WIDGET(gtk_builder_get_object (builder, "remove_button"));
    cd->edit_button = GTK_WIDGET(gtk_builder_get_object (builder, "edit_button"));

    cd->rename_namespace_button = GTK_WIDGET(gtk_builder_get_object (builder, "rename_namespace_button"));
    gtk_widget_set_sensitive (cd->rename_namespace_button, FALSE);

    /* commodity tree */
    scrolled_window = GTK_WIDGET(gtk_builder_get_object (builder, "commodity_list_window"));
    view = gnc_tree_view_commodity_new(cd->book,
                                       "state-section", STATE_SECTION,
                                       "show-column-menu", TRUE,
                                       NULL);
    cd->commodity_tree = GNC_TREE_VIEW_COMMODITY(view);
    gtk_container_add (GTK_CONTAINER (scrolled_window), GTK_WIDGET(view));
    gtk_tree_view_set_headers_visible(GTK_TREE_VIEW(cd->commodity_tree), TRUE);
    gnc_tree_view_commodity_set_filter (cd->commodity_tree,
                                        gnc_commodities_dialog_filter_ns_func,
                                        gnc_commodities_dialog_filter_cm_func,
                                        cd, NULL);
    selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (view));
    g_signal_connect (G_OBJECT (selection), "changed",
                      G_CALLBACK (gnc_commodities_dialog_selection_changed), cd);

    g_signal_connect (G_OBJECT (cd->commodity_tree), "row-activated",
                      G_CALLBACK (row_activated_cb), cd);

    /* Show currency button */
    button = GTK_WIDGET(gtk_builder_get_object (builder, "show_currencies_button"));
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(button), cd->show_currencies);

    /* default to 'close' button */
    button = GTK_WIDGET(gtk_builder_get_object (builder, "close_button"));
    gtk_widget_grab_default (button);
    gtk_widget_grab_focus (button);

    g_signal_connect (cd->window, "destroy",
                      G_CALLBACK(gnc_commodities_window_destroy_cb), cd);

    g_signal_connect (cd->window, "delete-event",
                      G_CALLBACK(gnc_commodities_window_delete_event_cb), cd);

    g_signal_connect (cd->window, "key_press_event",
                      G_CALLBACK (gnc_commodities_window_key_press_cb), cd);

    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, cd);
    g_object_unref (G_OBJECT(builder));

    gnc_restore_window_size (GNC_PREFS_GROUP, GTK_WINDOW(cd->window), GTK_WINDOW(parent));
}

static void
close_handler (gpointer user_data)
{
    auto cd = static_cast<CommoditiesDialog*>(user_data);

    gnc_save_window_size (GNC_PREFS_GROUP, GTK_WINDOW(cd->window));

    gnc_prefs_set_bool (GNC_PREFS_GROUP, GNC_PREF_INCL_ISO, cd->show_currencies);

    gtk_widget_destroy (cd->window);
}

static void
refresh_handler (GHashTable *changes, gpointer user_data)
{
    auto cd = static_cast<CommoditiesDialog*>(user_data);

    g_return_if_fail(cd != NULL);

    gnc_tree_view_commodity_refilter (cd->commodity_tree);
}

static gboolean
show_handler (const char *klass, gint component_id,
              gpointer user_data, gpointer iter_data)
{
    auto cd = static_cast<CommoditiesDialog*>(user_data);

    if (!cd)
        return(FALSE);
    gtk_window_present (GTK_WINDOW(cd->window));
    return(TRUE);
}

static gboolean
gnc_commodities_window_key_press_cb (GtkWidget *widget, GdkEventKey *event,
                                     gpointer data)
{
    auto cd = static_cast<CommoditiesDialog*>(data);

    if (event->keyval == GDK_KEY_Escape)
    {
        close_handler (cd);
        return TRUE;
    }
    else
        return FALSE;
}

/********************************************************************\
 * gnc_commodities_dialog                                           *
 *   opens up a window to edit price information                    *
 *                                                                  *
 * Args:   parent  - the parent of the window to be created         *
 * Return: nothing                                                  *
\********************************************************************/
void
gnc_commodities_dialog (GtkWidget * parent)
{
    gint component_id;

    if (gnc_forall_gui_components (DIALOG_COMMODITIES_CM_CLASS,
                                   show_handler, NULL))
        return;

    auto cd = static_cast<CommoditiesDialog*>(g_new0 (CommoditiesDialog, 1));

    gnc_commodities_dialog_create (parent, cd);

    component_id = gnc_register_gui_component (DIALOG_COMMODITIES_CM_CLASS,
                   refresh_handler, close_handler,
                   cd);
    gnc_gui_component_set_session (component_id, cd->session);

    gtk_widget_grab_focus (GTK_WIDGET(cd->commodity_tree));

    gtk_widget_show (cd->window);
}
