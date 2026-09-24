/********************************************************************\
 * import-account-matcher.cpp - asynchronous GTK4 account picker    *
 *                                                                  *
 * Copyright (C) 2002 Benoit Grégoire <bock@step.polymtl.ca>        *
 * Copyright (C) 2012 Robert Fewell                                 *
 * Author: Copyright (C) 2002 Benoit Grégoire <bock@step.polymtl.ca>*
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

#include <algorithm>
#include <vector>

#include "import-account-matcher.h"
#include "import-selection-model.h"
#include "Account.hpp"
#include "dialog-account.h"
#include "dialog-utils.h"
#include "gnc-engine.h"
#include "gnc-prefs.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"

G_GNUC_UNUSED static QofLogModule log_module = GNC_MOD_IMPORT;

#define GNC_PREFS_GROUP "dialogs.import.generic.account-picker"

typedef struct
{
    Account *partial_match;
    int count;
    const char *online_id;
} AccountOnlineMatch;

struct AccountPicker
{
    /* GTK owns the window. Signal-source child widgets are weakly observed. */
    GtkWindow *window;
    GtkButton *ok_button;
    GtkButton *cancel_button;
    GtkButton *new_button;
    GtkWidget *warning_box;
    GtkLabel *warning;
    GtkScrolledWindow *scroller;
    GListStore *rows;
    GtkSingleSelection *selection;
    GtkColumnView *view;
    gchar *online_id;
    gchar *description;
    const gnc_commodity *commodity;
    GNCAccountType account_type;
    GncImportAccountSelectedCB callback;
    gpointer user_data;
    GncSessionOperationContext *operation_context;
    gboolean assign_online_id;
    gboolean finished;
    gboolean creating_account;
    gboolean destroyed;
    gboolean signals_disconnected;
};

static GQuark account_row_quark = 0;

static gpointer
test_acct_online_id_match (Account *acct, gpointer data)
{
    auto match = static_cast<AccountOnlineMatch*> (data);
    auto account_id = xaccAccountGetOnlineID (acct);
    if (!account_id || !match->online_id)
        return nullptr;
    auto account_length = strlen (account_id);
    auto match_length = strlen (match->online_id);
    if (account_length && account_id[account_length - 1] == ' ')
        --account_length;
    if (match_length && match->online_id[match_length - 1] == ' ')
        --match_length;
    if (strncmp (account_id, match->online_id, account_length))
        return nullptr;
    if (!strncmp (account_id, match->online_id, match_length))
        return acct;
    if (!match->partial_match)
    {
        match->partial_match = acct;
        ++match->count;
        return nullptr;
    }
    auto partial_id = xaccAccountGetOnlineID (match->partial_match);
    auto partial_length = strlen (partial_id);
    if (partial_length && partial_id[partial_length - 1] == ' ')
        --partial_length;
    if (partial_length < account_length)
    {
        match->partial_match = acct;
        match->count = 1;
    }
    else if (partial_length == account_length)
        ++match->count;
    return nullptr;
}

static Account*
find_account (const gchar *online_id, GNCAccountType account_type)
{
    if (!online_id)
        return nullptr;
    AccountOnlineMatch match { nullptr, 0, online_id };
    auto account = static_cast<Account*> (gnc_account_foreach_descendant_until (
        gnc_get_current_root_account (), test_acct_online_id_match, &match));
    if (!account && match.count == 1 && account_type == ACCT_TYPE_NONE)
        account = match.partial_match;
    return account;
}

static GObject*
account_row_new (Account *account)
{
    if (G_UNLIKELY (!account_row_quark))
        account_row_quark = g_quark_from_static_string ("gnc-import-account-picker-row");
    auto row = G_OBJECT (g_object_new (G_TYPE_OBJECT, nullptr));
    g_object_set_qdata (row, account_row_quark, account);
    return row;
}

static Account*
account_row_get (GObject *row)
{
    return row ? static_cast<Account*> (g_object_get_qdata (row, account_row_quark)) : nullptr;
}

static void
account_label_setup (GtkListItemFactory *factory, GtkListItem *item, gpointer user_data)
{
    auto label = gtk_label_new (nullptr);
    gtk_label_set_xalign (GTK_LABEL (label), 0.0);
    gtk_label_set_ellipsize (GTK_LABEL (label), PANGO_ELLIPSIZE_END);
    gtk_list_item_set_child (item, label);
    (void)factory;
    (void)user_data;
}

static void
account_label_bind (GtkListItemFactory *factory, GtkListItem *item, gpointer user_data)
{
    auto account = account_row_get (G_OBJECT (gtk_list_item_get_item (item)));
    auto full_name = GPOINTER_TO_INT (user_data);
    auto text = full_name ? gnc_account_get_full_name (account) : g_strdup (xaccAccountGetOnlineID (account));
    gtk_label_set_text (GTK_LABEL (gtk_list_item_get_child (item)), text ? text : "");
    g_free (text);
    (void)factory;
}

static Account*
picker_selected_account (AccountPicker *picker)
{
    auto item = G_OBJECT (gtk_single_selection_get_selected_item (picker->selection));
    if (!item)
        return nullptr;
    auto account = account_row_get (item);
    return account;
}

static void
picker_disconnect_signals (AccountPicker *picker)
{
    if (!picker || picker->signals_disconnected)
        return;
    picker->signals_disconnected = TRUE;

    if (picker->window)
        g_signal_handlers_disconnect_by_data (picker->window, picker);
    if (picker->selection)
        g_signal_handlers_disconnect_by_data (picker->selection, picker);
    /* During an external window dispose, finalized children have already
     * cleared these weak pointers; externally retained children remain safe
     * to disconnect. */
    if (picker->view)
    {
        g_signal_handlers_disconnect_by_data (picker->view, picker);
        g_object_remove_weak_pointer (G_OBJECT (picker->view),
                                      reinterpret_cast<gpointer *> (&picker->view));
        picker->view = nullptr;
    }
    if (picker->ok_button)
    {
        g_signal_handlers_disconnect_by_data (picker->ok_button, picker);
        g_object_remove_weak_pointer (G_OBJECT (picker->ok_button),
                                      reinterpret_cast<gpointer *> (&picker->ok_button));
        picker->ok_button = nullptr;
    }
    if (picker->cancel_button)
    {
        g_signal_handlers_disconnect_by_data (picker->cancel_button, picker);
        g_object_remove_weak_pointer (G_OBJECT (picker->cancel_button),
                                      reinterpret_cast<gpointer *> (&picker->cancel_button));
        picker->cancel_button = nullptr;
    }
    if (picker->new_button)
    {
        g_signal_handlers_disconnect_by_data (picker->new_button, picker);
        g_object_remove_weak_pointer (G_OBJECT (picker->new_button),
                                      reinterpret_cast<gpointer *> (&picker->new_button));
        picker->new_button = nullptr;
    }
}

static void
picker_selection_changed (GtkSelectionModel *selection, guint position, guint n_items,
                          AccountPicker *picker)
{
    if (picker->operation_context &&
        !gnc_session_operation_context_is_current (picker->operation_context))
    {
        gtk_widget_set_visible (picker->warning_box, FALSE);
        gtk_widget_set_visible (GTK_WIDGET (picker->warning), FALSE);
        gtk_widget_set_sensitive (GTK_WIDGET (picker->ok_button), FALSE);
        return;
    }
    auto account = picker_selected_account (picker);
    auto placeholder = account && xaccAccountGetPlaceholder (account);
    gtk_widget_set_visible (picker->warning_box, placeholder);
    gtk_widget_set_visible (GTK_WIDGET (picker->warning), placeholder);
    if (placeholder)
    {
        auto message = g_strdup_printf (_("The account '%s' is a placeholder account and does not allow transactions. Please choose a different account."), xaccAccountGetName (account));
        gtk_label_set_text (picker->warning, message);
        g_free (message);
    }
    gtk_widget_set_sensitive (GTK_WIDGET (picker->ok_button), account && !placeholder);
    (void)selection;
    (void)position;
    (void)n_items;
}

static void
picker_finish (AccountPicker *picker, gboolean accepted)
{
    if (!picker || picker->finished)
        return;
    if (accepted && picker->operation_context &&
        !gnc_session_operation_context_is_current (picker->operation_context))
        accepted = FALSE;
    picker->finished = TRUE;
    picker_disconnect_signals (picker);
    auto account = accepted ? picker_selected_account (picker) : nullptr;
    if (account && xaccAccountGetPlaceholder (account))
        account = nullptr;
    auto operation_started = TRUE;
    if (account && picker->assign_online_id && picker->online_id)
    {
        operation_started = !picker->operation_context ||
            gnc_session_operation_context_begin (picker->operation_context);
        if (operation_started)
            xaccAccountSetOnlineID (account, picker->online_id);
        else
            account = nullptr;
        if (picker->operation_context && operation_started)
            gnc_session_operation_context_end (picker->operation_context);
    }
    if (picker->window)
    {
        gnc_save_window_size (GNC_PREFS_GROUP, picker->window);
        gtk_window_destroy (picker->window);
        picker->window = nullptr;
    }
    g_clear_object (&picker->selection);
    g_clear_object (&picker->rows);
    auto callback = picker->callback;
    auto user_data = picker->user_data;
    auto operation_context = picker->operation_context;
    g_free (picker->online_id);
    g_free (picker->description);
    g_free (picker);
    gnc_session_operation_context_unref (operation_context);
    if (callback)
        callback (account, account != nullptr, user_data);
}

static gboolean
picker_close_request (GtkWindow *window, AccountPicker *picker)
{
    (void)window;
    if (picker->creating_account)
        return TRUE;
    picker_finish (picker, FALSE);
    return TRUE;
}

static void
picker_destroyed (GtkWidget *window, AccountPicker *picker)
{
    if (!picker || picker->finished)
        return;
    picker->destroyed = TRUE;
    picker_disconnect_signals (picker);
    picker->window = nullptr;
    if (!picker->creating_account)
        picker_finish (picker, FALSE);
    (void)window;
}

static void
picker_ok_clicked (GtkButton *button, AccountPicker *picker)
{
    (void)button;
    picker_finish (picker, TRUE);
}

static void
picker_cancel_clicked (GtkButton *button, AccountPicker *picker)
{
    (void)button;
    picker_finish (picker, FALSE);
}

static void
picker_activated (GtkColumnView *view, guint position, AccountPicker *picker)
{
    gtk_single_selection_set_selected (picker->selection, position);
    picker_finish (picker, TRUE);
    (void)view;
}

static void
picker_account_created (Account *account, gboolean accepted, gpointer user_data)
{
    auto picker = static_cast<AccountPicker *> (user_data);
    if (!picker || picker->finished)
        return;
    picker->creating_account = FALSE;
    if (picker->destroyed)
    {
        picker_finish (picker, FALSE);
        return;
    }
    if (picker->operation_context &&
        !gnc_session_operation_context_is_current (picker->operation_context))
    {
        picker_finish (picker, FALSE);
        return;
    }
    gtk_widget_set_sensitive (GTK_WIDGET (picker->window), TRUE);
    if (!accepted || !account)
        return;
    auto row = account_row_new (account);
    g_list_store_append (picker->rows, row);
    auto position = g_list_model_get_n_items (G_LIST_MODEL (picker->rows)) - 1;
    g_object_unref (row);
    gtk_single_selection_set_selected (picker->selection, position);
}

static void
picker_add_account (GtkButton *button, AccountPicker *picker)
{
    GList *types = nullptr;
    if (picker->creating_account)
        return;
    if (picker->operation_context &&
        !gnc_session_operation_context_is_current (picker->operation_context))
    {
        picker_finish (picker, FALSE);
        return;
    }
    if (picker->account_type != ACCT_TYPE_NONE)
        types = g_list_prepend (types, GINT_TO_POINTER (picker->account_type));
    picker->creating_account = TRUE;
    gtk_widget_set_sensitive (GTK_WIDGET (picker->window), FALSE);
    gnc_ui_new_accounts_from_name_with_defaults_async_with_operation_context (
        picker->window, picker->description, types, picker->commodity,
        picker_selected_account (picker), picker->operation_context,
        picker_account_created, picker);
    g_list_free (types);
    (void)button;
}

static void
picker_add_column (AccountPicker *picker, const gchar *title, gboolean full_name)
{
    auto factory = gtk_signal_list_item_factory_new ();
    g_signal_connect (factory, "setup", G_CALLBACK (account_label_setup), nullptr);
    g_signal_connect (factory, "bind", G_CALLBACK (account_label_bind), GINT_TO_POINTER (full_name));
    auto column = gtk_column_view_column_new (title, factory);
    gtk_column_view_column_set_resizable (column, TRUE);
    gtk_column_view_append_column (picker->view, column);
    g_object_unref (column);
}

static void
picker_populate (AccountPicker *picker, Account *default_selection)
{
    std::vector<Account*> accounts;
    gnc_account_foreach_descendant (gnc_get_current_root_account (), [&accounts] (Account *account)
    {
        if (!gnc_account_is_root (account))
            accounts.emplace_back (account);
    });
    std::sort (accounts.begin (), accounts.end (), [] (Account *left, Account *right)
    {
        auto left_name = gnc_account_get_full_name (left);
        auto right_name = gnc_account_get_full_name (right);
        auto comparison = g_utf8_collate (left_name, right_name);
        g_free (left_name);
        g_free (right_name);
        return comparison < 0;
    });
    for (guint index = 0; index < accounts.size (); ++index)
    {
        auto row = account_row_new (accounts[index]);
        g_list_store_append (picker->rows, row);
        g_object_unref (row);
        if (accounts[index] == default_selection)
            gtk_single_selection_set_selected (picker->selection, index);
    }
}

static void
gnc_import_select_account_async_internal (GtkWidget *parent, const gchar *online_id,
                                          gboolean prompt_on_no_match, const gchar *description,
                                          const gnc_commodity *commodity,
                                          GNCAccountType account_type,
                                          Account *default_selection,
                                          GncSessionOperationContext *operation_context,
                                          GncImportAccountSelectedCB callback,
                                          gpointer user_data,
                                          gboolean assign_online_id)
{
    auto matched = find_account (online_id, account_type);
    if (matched || !prompt_on_no_match)
    {
        if (callback)
            callback (matched, matched != nullptr, user_data);
        return;
    }
    auto picker = g_new0 (AccountPicker, 1);
    picker->online_id = g_strdup (online_id);
    picker->description = g_strdup (description);
    picker->commodity = commodity;
    picker->account_type = account_type;
    picker->callback = callback;
    picker->user_data = user_data;
    picker->operation_context =
        gnc_session_operation_context_ref (operation_context);
    picker->assign_online_id = assign_online_id;
    auto builder = gtk_builder_new ();
    gnc_builder_add_from_file (builder, "dialog-import.glade", "account_picker_dialog");
    picker->window = GTK_WINDOW (gtk_builder_get_object (builder, "account_picker_dialog"));
    picker->ok_button = GTK_BUTTON (gtk_builder_get_object (builder, "okbutton"));
    picker->warning_box = GTK_WIDGET (gtk_builder_get_object (builder, "warning_hbox"));
    picker->warning = GTK_LABEL (gtk_builder_get_object (builder, "warning_label"));
    picker->scroller = GTK_SCROLLED_WINDOW (gtk_builder_get_object (builder, "account_tree_sw"));
    picker->new_button = GTK_BUTTON (gtk_builder_get_object (builder, "newbutton"));
    picker->cancel_button = GTK_BUTTON (gtk_builder_get_object (builder, "cancelbutton"));
    g_return_if_fail (picker->window && picker->ok_button && picker->scroller &&
                      picker->new_button && picker->cancel_button);
    g_object_add_weak_pointer (G_OBJECT (picker->ok_button),
                               reinterpret_cast<gpointer *> (&picker->ok_button));
    g_object_add_weak_pointer (G_OBJECT (picker->cancel_button),
                               reinterpret_cast<gpointer *> (&picker->cancel_button));
    g_object_add_weak_pointer (G_OBJECT (picker->new_button),
                               reinterpret_cast<gpointer *> (&picker->new_button));
    g_object_unref (builder);
    if (parent)
        gtk_window_set_transient_for (picker->window, GTK_WINDOW (parent));
    gtk_window_set_modal (picker->window, TRUE);
    gnc_restore_window_size (GNC_PREFS_GROUP, picker->window, parent ? GTK_WINDOW (parent) : nullptr);
    picker->rows = g_list_store_new (G_TYPE_OBJECT);
    picker->selection = gnc_import_single_selection_new (G_LIST_MODEL (picker->rows));
    picker->view = GTK_COLUMN_VIEW (gtk_column_view_new (GTK_SELECTION_MODEL (g_object_ref (picker->selection))));
    g_object_add_weak_pointer (G_OBJECT (picker->view),
                               reinterpret_cast<gpointer *> (&picker->view));
    picker_add_column (picker, _("Account"), TRUE);
    picker_add_column (picker, _("Account ID"), FALSE);
    gtk_scrolled_window_set_child (picker->scroller, GTK_WIDGET (picker->view));
    picker_populate (picker, default_selection);
    g_signal_connect (picker->selection, "selection-changed", G_CALLBACK (picker_selection_changed), picker);
    g_signal_connect (picker->view, "activate", G_CALLBACK (picker_activated), picker);
    g_signal_connect (picker->window, "close-request", G_CALLBACK (picker_close_request), picker);
    g_signal_connect (picker->window, "destroy", G_CALLBACK (picker_destroyed), picker);
    g_signal_connect (picker->ok_button, "clicked", G_CALLBACK (picker_ok_clicked), picker);
    g_signal_connect (picker->cancel_button, "clicked", G_CALLBACK (picker_cancel_clicked), picker);
    g_signal_connect (picker->new_button, "clicked", G_CALLBACK (picker_add_account), picker);
    picker_selection_changed (GTK_SELECTION_MODEL (picker->selection), 0, 0, picker);
    gtk_window_set_default_widget (picker->window, GTK_WIDGET (picker->ok_button));
    gtk_window_present (picker->window);
}

void
gnc_import_select_account_async (GtkWidget *parent, const gchar *online_id,
                                 gboolean prompt_on_no_match, const gchar *description,
                                 const gnc_commodity *commodity, GNCAccountType account_type,
                                 Account *default_selection, GncImportAccountSelectedCB callback,
                                 gpointer user_data)
{
    gnc_import_select_account_async_internal (parent, online_id, prompt_on_no_match,
                                              description, commodity, account_type,
                                              default_selection, nullptr, callback,
                                              user_data, TRUE);
}

void
gnc_import_select_account_async_no_mutation (GtkWidget *parent, const gchar *online_id,
                                             gboolean prompt_on_no_match,
                                             const gchar *description,
                                             const gnc_commodity *commodity,
                                             GNCAccountType account_type,
                                             Account *default_selection,
                                             GncImportAccountSelectedCB callback,
                                             gpointer user_data)
{
    gnc_import_select_account_async_internal (parent, online_id, prompt_on_no_match,
                                              description, commodity, account_type,
                                              default_selection, nullptr, callback,
                                              user_data, FALSE);
}

void
gnc_import_select_account_async_no_mutation_with_operation_context (
    GtkWidget *parent, const gchar *online_id, gboolean prompt_on_no_match,
    const gchar *description, const gnc_commodity *commodity,
    GNCAccountType account_type, Account *default_selection,
    GncSessionOperationContext *operation_context,
    GncImportAccountSelectedCB callback, gpointer user_data)
{
    if (!operation_context ||
        !gnc_session_operation_context_is_current (operation_context))
    {
        if (callback)
            callback (nullptr, FALSE, user_data);
        return;
    }
    gnc_import_select_account_async_internal (
        parent, online_id, prompt_on_no_match, description, commodity,
        account_type, default_selection, operation_context, callback,
        user_data, FALSE);
}

Account*
gnc_import_select_account (GtkWidget *parent, const gchar *online_id, gboolean prompt_on_no_match,
                           const gchar *description, const gnc_commodity *commodity,
                           GNCAccountType account_type, Account *default_selection,
                           gboolean *ok_pressed)
{
    auto matched = find_account (online_id, account_type);
    if (ok_pressed)
        *ok_pressed = matched != nullptr;
    if (!matched && prompt_on_no_match)
        g_warning ("gnc_import_select_account() is asynchronous in GTK4; use gnc_import_select_account_async()");
    (void)parent;
    (void)description;
    (void)commodity;
    (void)default_selection;
    return matched;
}
