/********************************************************************\
 * dialog-imap-editor.c -- Import Map Editor dialog                 *
 * Copyright (C) 2015 Robert Fewell                                 *
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

#include "dialog-imap-editor.h"

#include "dialog-utils.h"
#include "gnc-component-manager.h"
#include "gnc-prefs.h"
#include "gnc-session.h"

#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include <gnc-string-utils.h>
#include "Account.h"

#define DIALOG_IMAP_CM_CLASS    "dialog-imap-edit"
#define GNC_PREFS_GROUP         "dialogs.imap-editor"
#define IMAP_FRAME_BAYES        "import-map-bayes"
#define IMAP_FRAME              "import-map"
#define IMAP_FRAME_DESC         "desc"
#define IMAP_FRAME_MEMO         "memo"
#define IMAP_FRAME_CSV          "csv-account-map"

typedef enum { BAYES, NBAYES, ONLINE } GncListType;
typedef struct { guint bayes : 1; guint nbayes : 1; guint online : 1; } GncInvFlags;

typedef struct
{
    Account *source_account;
    Account *map_account;
    GListStore *children;
    gchar *source_full_acc;
    gchar *based_on;
    gchar *match_string;
    gchar *map_full_acc;
    gchar *head;
    gchar *category;
    gchar *count;
} ImapRow;

static GQuark imap_row_quark = 0;

static void
imap_row_free (gpointer data)
{
    ImapRow *row = data;
    if (!row) return;
    g_clear_object (&row->children);
    g_free (row->source_full_acc); g_free (row->based_on); g_free (row->match_string);
    g_free (row->map_full_acc); g_free (row->head); g_free (row->category); g_free (row->count);
    g_free (row);
}

static GObject *
imap_row_new (Account *source_account, Account *map_account, const gchar *source_full_acc,
              const gchar *based_on, const gchar *match_string, const gchar *map_full_acc,
              const gchar *head, const gchar *category, const gchar *count, gboolean has_children)
{
    GObject *object;
    ImapRow *row;
    if (G_UNLIKELY (!imap_row_quark))
        imap_row_quark = g_quark_from_static_string ("gnc-imap-editor-row");
    object = G_OBJECT (g_object_new (G_TYPE_OBJECT, NULL));
    row = g_new0 (ImapRow, 1);
    row->source_account = source_account; row->map_account = map_account;
    row->source_full_acc = g_strdup (source_full_acc); row->based_on = g_strdup (based_on);
    row->match_string = g_strdup (match_string); row->map_full_acc = g_strdup (map_full_acc);
    row->head = g_strdup (head); row->category = g_strdup (category); row->count = g_strdup (count);
    if (has_children) row->children = g_list_store_new (G_TYPE_OBJECT);
    g_object_set_qdata_full (object, imap_row_quark, row, imap_row_free);
    return object;
}

static ImapRow *
imap_row_get (gpointer object)
{
    return object ? g_object_get_qdata (G_OBJECT (object), imap_row_quark) : NULL;
}

typedef struct
{
    GtkWindow *window;
    QofSession *session;
    GtkColumnView *view;
    GListStore *rows;
    GtkTreeListModel *tree_model;
    GtkFilterListModel *filter_model;
    GtkMultiSelection *selection;
    GtkCustomFilter *filter;
    gchar *filter_text;
    GncListType type;
    GtkWidget *radio_bayes, *radio_nbayes, *radio_online;
    GtkWidget *filter_button, *filter_text_entry, *filter_label;
    GtkWidget *total_entries_label, *expand_button, *collapse_button, *remove_button;
    GtkColumnViewColumn *based_on_column, *count_column;
    gint tot_entries, tot_invalid_maps;
    GncInvFlags inv_dialog_shown;
} ImapDialog;

static QofLogModule log_module = GNC_MOD_GUI;
static void get_account_info (ImapDialog *dialog);
static void gnc_imap_invalid_maps_dialog (ImapDialog *dialog);

static GListModel *
imap_create_children (gpointer item, gpointer user_data)
{
    ImapRow *row = imap_row_get (item);
    (void)user_data;
    return row && row->children ? G_LIST_MODEL (g_object_ref (row->children)) : NULL;
}

static gboolean
imap_filter_match (gpointer item, gpointer user_data)
{
    ImapDialog *dialog = user_data;
    GtkTreeListRow *tree_row = GTK_TREE_LIST_ROW (item);
    GObject *row_item = gtk_tree_list_row_get_item (tree_row);
    ImapRow *row = imap_row_get (row_item);
    gboolean matches;
    if (!dialog->filter_text || !*dialog->filter_text || !row || !row->match_string)
        matches = TRUE;
    else
        matches = g_strrstr (row->match_string, dialog->filter_text) != NULL ||
            (row->map_full_acc && g_strrstr (row->map_full_acc, dialog->filter_text) != NULL);
    g_object_unref (row_item);
    return matches;
}

static void
imap_collapse_all (ImapDialog *dialog)
{
    guint n_items = g_list_model_get_n_items (G_LIST_MODEL (dialog->tree_model));
    while (n_items > 0)
    {
        GtkTreeListRow *row = g_list_model_get_item (G_LIST_MODEL (dialog->tree_model), --n_items);
        gtk_tree_list_row_set_expanded (row, FALSE);
        g_object_unref (row);
    }
}

static GPtrArray *
imap_root_rows (ImapDialog *dialog)
{
    GPtrArray *roots;
    guint i, n_items;
    imap_collapse_all (dialog);
    n_items = g_list_model_get_n_items (G_LIST_MODEL (dialog->tree_model));
    roots = g_ptr_array_new_with_free_func (g_object_unref);
    for (i = 0; i < n_items; i++)
        g_ptr_array_add (roots, g_list_model_get_item (G_LIST_MODEL (dialog->tree_model), i));
    return roots;
}

static gboolean
imap_root_matches_filter (ImapRow *row, const gchar *filter_text)
{
    guint i, n_items;
    if (!row || !row->children) return FALSE;
    n_items = g_list_model_get_n_items (G_LIST_MODEL (row->children));
    for (i = 0; i < n_items; i++)
    {
        GObject *object = g_list_model_get_item (G_LIST_MODEL (row->children), i);
        ImapRow *child = imap_row_get (object);
        gboolean matches = child && child->match_string &&
            (g_strrstr (child->match_string, filter_text) != NULL ||
             (child->map_full_acc && g_strrstr (child->map_full_acc, filter_text) != NULL));
        g_object_unref (object);
        if (matches) return TRUE;
    }
    return FALSE;
}

static void
imap_clear_selection (ImapDialog *dialog)
{
    gtk_selection_model_unselect_all (GTK_SELECTION_MODEL (dialog->selection));
}

static gboolean
imap_row_is_mapping (ImapRow *row)
{
    return row && row->match_string;
}

static void
imap_apply_filter (ImapDialog *dialog, gboolean expand_matches)
{
    GPtrArray *roots;
    guint i;

    imap_clear_selection (dialog);
    roots = imap_root_rows (dialog);
    if (expand_matches && dialog->filter_text && *dialog->filter_text)
        for (i = 0; i < roots->len; i++)
        {
            GtkTreeListRow *tree_row = g_ptr_array_index (roots, i);
            GObject *row_item = gtk_tree_list_row_get_item (tree_row);
            ImapRow *row = imap_row_get (row_item);

            gtk_tree_list_row_set_expanded (tree_row,
                                            imap_root_matches_filter (row, dialog->filter_text));
            g_object_unref (row_item);
        }
    g_ptr_array_unref (roots);
    gtk_filter_changed (GTK_FILTER (dialog->filter), GTK_FILTER_CHANGE_DIFFERENT);
}
static void
delete_info_bayes (Account *source_account, gchar *head, guint depth)
{
    if (depth != 1) gnc_account_delete_map_entry (source_account, head, NULL, NULL, FALSE);
    else gnc_account_delete_all_bayes_maps (source_account);
}

static void
delete_info_nbayes (Account *source_account, gchar *head, gchar *category,
                    gchar *match_string, guint depth)
{
    if (depth != 1)
    {
        gnc_account_delete_map_entry (source_account, head, category, match_string, FALSE);
        gnc_account_delete_map_entry (source_account, head, category, NULL, TRUE);
    }
    else gnc_account_delete_map_entry (source_account, head, category, NULL, FALSE);
    gnc_account_delete_map_entry (source_account, head, NULL, NULL, TRUE);
}

static void
delete_row_info (ImapDialog *dialog, ImapRow *row, guint depth)
{
    if (!row || !row->source_account) return;
    PINFO ("Account is '%s', Head is '%s', Category is '%s', Match String is '%s'",
           row->source_full_acc, row->head, row->category, row->match_string);
    if (dialog->type == ONLINE)
        gnc_account_delete_map_entry (row->source_account, row->head, NULL, NULL, FALSE);
    else if (dialog->type == BAYES)
        delete_info_bayes (row->source_account, row->head, depth);
    else
        delete_info_nbayes (row->source_account, row->head, row->category, row->match_string, depth);
}

typedef struct { GObject *row_item; guint depth; } ImapSelectedRow;
static void
imap_selected_row_free (gpointer data)
{
    ImapSelectedRow *selected = data;
    g_clear_object (&selected->row_item);
    g_free (selected);
}

static GPtrArray *
imap_selected_rows (ImapDialog *dialog)
{
    GtkBitset *bitset;
    GtkBitsetIter iter;
    GPtrArray *rows;
    guint position;

    bitset = gtk_selection_model_get_selection (GTK_SELECTION_MODEL (dialog->selection));
    rows = g_ptr_array_new_with_free_func (imap_selected_row_free);

    if (gtk_bitset_iter_init_first (&iter, bitset, &position))
        do
        {
            GtkTreeListRow *tree_row;
            GObject *row_item;
            ImapRow *row;
            guint depth;

            tree_row = g_list_model_get_item (G_LIST_MODEL (dialog->filter_model), position);
            if (!tree_row)
                continue;

            row_item = gtk_tree_list_row_get_item (tree_row);
            row = imap_row_get (row_item);
            depth = gtk_tree_list_row_get_depth (tree_row);
            if (imap_row_is_mapping (row))
            {
                ImapSelectedRow *selected = g_new (ImapSelectedRow, 1);

                selected->row_item = row_item;
                selected->depth = depth + 1;
                g_ptr_array_add (rows, selected);
                row_item = NULL;
            }

            g_clear_object (&row_item);
            g_object_unref (tree_row);
        } while (gtk_bitset_iter_next (&iter, &position));

    gtk_bitset_unref (bitset);
    return rows;
}

static void
imap_selection_changed_cb (GtkSelectionModel *selection, guint position,
                           guint n_items, ImapDialog *dialog)
{
    guint i;

    for (i = position; i < position + n_items; i++)
    {
        GtkTreeListRow *tree_row;
        ImapRow *row;

        if (!gtk_selection_model_is_selected (selection, i))
            continue;

        tree_row = g_list_model_get_item (G_LIST_MODEL (dialog->filter_model), i);
        if (!tree_row)
            continue;

        GObject *row_item = gtk_tree_list_row_get_item (tree_row);
        row = imap_row_get (row_item);
        if (!imap_row_is_mapping (row))
            gtk_selection_model_unselect_item (selection, i);
        g_object_unref (row_item);
        g_object_unref (tree_row);
    }
}

static void
imap_delete_selected (ImapDialog *dialog)
{
    GPtrArray *rows = imap_selected_rows (dialog);
    guint i;

    if (rows->len == 0)
    {
        g_ptr_array_unref (rows);
        return;
    }

    /* Preserve the row data for deletion, then clear the selection before
     * the engine change can cause the model to be rebuilt. */
    imap_clear_selection (dialog);
    gnc_suspend_gui_refresh ();
    for (i = 0; i < rows->len; i++)
    {
        ImapSelectedRow *selected = g_ptr_array_index (rows, i);
        delete_row_info (dialog, imap_row_get (selected->row_item), selected->depth);
    }
    gnc_resume_gui_refresh ();
    g_ptr_array_unref (rows);
    get_account_info (dialog);
}

static gboolean
imap_row_is_invalid (ImapRow *row, guint depth)
{
    return row && row->source_account && !row->map_account &&
        ((g_strcmp0 (row->head, "online_id") == 0 && depth == 1) || depth == 2);
}

static void
imap_remove_invalid_maps (ImapDialog *dialog)
{
    GPtrArray *invalid = g_ptr_array_new_with_free_func (imap_selected_row_free);
    guint i, j, n_items = g_list_model_get_n_items (G_LIST_MODEL (dialog->rows));
    for (i = 0; i < n_items; i++)
    {
        GObject *object = g_list_model_get_item (G_LIST_MODEL (dialog->rows), i);
        ImapRow *row = imap_row_get (object);
        if (imap_row_is_invalid (row, 1))
        {
            ImapSelectedRow *selected = g_new (ImapSelectedRow, 1);
            selected->row_item = g_object_ref (object);
            selected->depth = 1;
            g_ptr_array_add (invalid, selected);
        }
        if (row && row->children)
            for (j = 0; j < g_list_model_get_n_items (G_LIST_MODEL (row->children)); j++)
            {
                GObject *child_object = g_list_model_get_item (G_LIST_MODEL (row->children), j);
                ImapRow *child = imap_row_get (child_object);
                if (imap_row_is_invalid (child, 2))
                {
                    ImapSelectedRow *selected = g_new (ImapSelectedRow, 1);
                    selected->row_item = g_object_ref (child_object);
                    selected->depth = 2;
                    g_ptr_array_add (invalid, selected);
                }
                g_object_unref (child_object);
            }
        g_object_unref (object);
    }
    /* Invalid-map removal also changes the backing engine data. Do not keep
     * selections into a tree that is about to be replaced. */
    imap_clear_selection (dialog);
    gnc_suspend_gui_refresh ();
    for (i = 0; i < invalid->len; i++)
    {
        ImapSelectedRow *selected = g_ptr_array_index (invalid, i);
        delete_row_info (dialog, imap_row_get (selected->row_item), selected->depth);
    }
    gnc_resume_gui_refresh (); g_ptr_array_unref (invalid); get_account_info (dialog);
}

typedef struct { GWeakRef window; } ImapInvalidRequest;
static void imap_invalid_request_free (ImapInvalidRequest *request)
{ g_weak_ref_clear (&request->window); g_free (request); }

static void
imap_invalid_maps_response (GObject *source, GAsyncResult *result, gpointer user_data)
{
    ImapInvalidRequest *request = user_data;
    GtkWindow *window = g_weak_ref_get (&request->window);
    GError *error = NULL;
    gint response = gtk_alert_dialog_choose_finish (GTK_ALERT_DIALOG (source), result, &error);
    ImapDialog *dialog = window ? g_object_get_data (G_OBJECT (window), "gnc-imap-dialog") : NULL;
    if (dialog && !error && response == 1) imap_remove_invalid_maps (dialog);
    else if (dialog && !error)
    {
        gtk_widget_set_visible (dialog->remove_button, TRUE);
        if (dialog->type == BAYES) dialog->inv_dialog_shown.bayes = TRUE;
        else if (dialog->type == NBAYES) dialog->inv_dialog_shown.nbayes = TRUE;
        else dialog->inv_dialog_shown.online = TRUE;
    }
    g_clear_error (&error); g_clear_object (&window); imap_invalid_request_free (request);
}

static void
gnc_imap_invalid_maps_dialog (ImapDialog *dialog)
{
    gchar *message, *detail; GtkAlertDialog *alert; ImapInvalidRequest *request;
    const char *buttons[] = { _("Keep"), _("Remove"), NULL };
    gtk_widget_set_visible (dialog->remove_button, FALSE);
    if (dialog->tot_invalid_maps == 0) return;
    message = g_strdup_printf (ngettext ("There is %d invalid mapping.",
                                         "There are %d invalid mappings.", dialog->tot_invalid_maps),
                               dialog->tot_invalid_maps);
    detail = g_strdup_printf (_("To see the invalid mappings, use a filter of '%s'.\n\n%s"),
                              _("Map Account NOT found"), _("(Note, if there is a large number, it may take a while)"));
    alert = gtk_alert_dialog_new ("%s", message);
    gtk_alert_dialog_set_detail (alert, detail); gtk_alert_dialog_set_buttons (alert, buttons);
    gtk_alert_dialog_set_cancel_button (alert, 0); gtk_alert_dialog_set_default_button (alert, 0);
    request = g_new0 (ImapInvalidRequest, 1); g_weak_ref_init (&request->window, dialog->window);
    gtk_alert_dialog_choose (alert, dialog->window, NULL, imap_invalid_maps_response, request);
    g_object_unref (alert); g_free (message); g_free (detail);
}


static const gchar *
imap_text (ImapRow *row, guint column)
{
    if (!row) return "";
    switch (column)
    {
    case 0: return row->source_full_acc;
    case 1: return row->based_on;
    case 2: return row->match_string;
    case 3: return row->map_full_acc;
    case 4: return row->count;
    default: return "";
    }
}

static void
imap_cell_setup (GtkListItemFactory *factory, GtkListItem *item, gpointer user_data)
{
    guint column = GPOINTER_TO_UINT (user_data); GtkWidget *label = gtk_label_new (NULL);
    gtk_label_set_xalign (GTK_LABEL (label), column == 4 ? 0.5 : 0.0);
    gtk_label_set_ellipsize (GTK_LABEL (label), PANGO_ELLIPSIZE_END);
    if (column == 0)
    {
        GtkTreeExpander *expander = GTK_TREE_EXPANDER (gtk_tree_expander_new ());
        gtk_tree_expander_set_child (expander, label); gtk_list_item_set_child (item, GTK_WIDGET (expander));
    }
    else gtk_list_item_set_child (item, label);
    (void)factory;
}

static void
imap_cell_bind (GtkListItemFactory *factory, GtkListItem *item, gpointer user_data)
{
    guint column = GPOINTER_TO_UINT (user_data); GtkTreeListRow *tree_row = GTK_TREE_LIST_ROW (gtk_list_item_get_item (item));
    GObject *row_item = gtk_tree_list_row_get_item (tree_row);
    ImapRow *row = imap_row_get (row_item); GtkWidget *child = gtk_list_item_get_child (item);
    GtkLabel *label = column == 0 ? GTK_LABEL (gtk_tree_expander_get_child (GTK_TREE_EXPANDER (child))) : GTK_LABEL (child);
    gtk_label_set_text (label, imap_text (row, column));
    if (column == 0) gtk_tree_expander_set_list_row (GTK_TREE_EXPANDER (child), tree_row);
    g_object_unref (row_item);
    (void)factory;
}

static GtkColumnViewColumn *
imap_add_column (ImapDialog *dialog, const gchar *title, guint column, gboolean expand)
{
    GtkListItemFactory *factory = gtk_signal_list_item_factory_new ();
    GtkColumnViewColumn *view_column;
    g_signal_connect (factory, "setup", G_CALLBACK (imap_cell_setup), GUINT_TO_POINTER (column));
    g_signal_connect (factory, "bind", G_CALLBACK (imap_cell_bind), GUINT_TO_POINTER (column));
    view_column = gtk_column_view_column_new (title, factory);
    gtk_column_view_column_set_resizable (view_column, TRUE);
    gtk_column_view_column_set_expand (view_column, expand);
    gtk_column_view_append_column (dialog->view, view_column);
    g_object_unref (view_column);
    return view_column;
}

static void
show_count_column (ImapDialog *dialog, gboolean show)
{
    gtk_column_view_column_set_visible (dialog->count_column, show);
    gtk_column_view_column_set_visible (dialog->based_on_column, !show);
}

static void
add_to_store (ImapDialog *dialog, GListStore *store, const gchar *text, GncImapInfo *info)
{
    gchar *source_full_acc = gnc_account_get_full_name (info->source_account);
    gchar *map_full_acc = info->map_account ? gnc_account_get_full_name (info->map_account) : g_strdup (_("Map Account NOT found"));
    GObject *object;
    if (!info->map_account) dialog->tot_invalid_maps++;
    dialog->tot_entries++;
    object = imap_row_new (info->source_account, info->map_account, source_full_acc, text,
                           info->match_string, map_full_acc, info->head, info->category, info->count, FALSE);
    g_list_store_append (store, object); g_object_unref (object); g_free (source_full_acc); g_free (map_full_acc);
}

static void
get_imap_info (ImapDialog *dialog, Account *account, const gchar *category, const gchar *text)
{
    GList *list, *node; gchar *source_full_acc = gnc_account_get_full_name (account);
    const gchar *head = category ? IMAP_FRAME : IMAP_FRAME_BAYES; GObject *parent_object; ImapRow *parent;
    list = category ? gnc_account_imap_get_info (account, category) : gnc_account_imap_get_info_bayes (account);
    if (!list) { g_free (source_full_acc); return; }
    parent_object = imap_row_new (account, NULL, source_full_acc, text, NULL, NULL, head, category, NULL, TRUE);
    parent = imap_row_get (parent_object); g_list_store_append (dialog->rows, parent_object);
    for (node = list; node; node = node->next) add_to_store (dialog, parent->children, text, node->data);
    g_object_unref (parent_object); g_list_free_full (list, (GDestroyNotify)gnc_account_imap_info_destroy); g_free (source_full_acc);
}

static void
add_online_entry (ImapDialog *dialog, const gchar *based_on, GncImapInfo *info)
{ add_to_store (dialog, dialog->rows, based_on, info); info->map_account = NULL; }

static void
get_account_info_online (ImapDialog *dialog, GList *accounts)
{
    GList *node; GncImapInfo info = { 0 };
    info.category = " "; info.count = " ";
    for (node = accounts; node; node = node->next)
    {
        Account *account = node->data; gchar *text;
        text = gnc_account_get_map_entry (account, "online_id", NULL);
        if (text) { info.source_account = account; info.head = "online_id"; info.match_string = text; if (*text) info.map_account = account; add_online_entry (dialog, _("Online Id"), &info); g_free (text); }
        gchar *id = gnc_account_get_map_entry (account, "hbci", "account-id");
        gchar *code = gnc_account_get_map_entry (account, "hbci", "bank-code");
        if (id && code) { text = g_strconcat (code, ",", id, NULL); info.source_account = account; info.head = "hbci"; info.match_string = text; if (*text) info.map_account = account; add_online_entry (dialog, _("Online HBCI"), &info); g_free (text); }
        g_free (id); g_free (code);
        GncGUID *guid = gnc_account_get_map_guid_entry (account, "ofx/associated-income-account", NULL);
        if (guid) { text = guid_to_string (guid); info.source_account = account; info.head = "ofx/associated-income-account"; info.match_string = text; if (*text) info.map_account = xaccAccountLookup (guid, gnc_get_current_book ()); add_online_entry (dialog, _("OFX Income Account"), &info); guid_free (guid); g_free (text); }
    }
}

static void
show_filter_option (ImapDialog *dialog, gboolean show)
{
    gtk_widget_set_visible (dialog->filter_text_entry, show); gtk_widget_set_visible (dialog->filter_button, show);
    gtk_widget_set_visible (dialog->filter_label, show); gtk_widget_set_visible (dialog->expand_button, show);
    gtk_widget_set_visible (dialog->collapse_button, show);
}

static void
imap_scroll_to_first (ImapDialog *dialog)
{
    if (g_list_model_get_n_items (G_LIST_MODEL (dialog->filter_model)) > 0)
        gtk_column_view_scroll_to (dialog->view, 0, NULL, GTK_LIST_SCROLL_NONE, NULL);
}

static void
get_account_info (ImapDialog *dialog)
{
    Account *root;
    GList *accounts;
    gchar *total;

    root = gnc_book_get_root_account (gnc_get_current_book ());
    accounts = gnc_account_get_descendants_sorted (root);

    /* The selection model is backed by the filtered tree. Clear it while
     * its rows still exist so selection-changed never observes stale rows. */
    imap_clear_selection (dialog);
    g_list_store_remove_all (dialog->rows);
    dialog->tot_entries = 0;
    dialog->tot_invalid_maps = 0;

    gtk_editable_set_text (GTK_EDITABLE (dialog->filter_text_entry), "");
    g_clear_pointer (&dialog->filter_text, g_free);
    dialog->filter_text = g_strdup ("");

    show_count_column (dialog, FALSE);
    show_filter_option (dialog, TRUE);

    if (dialog->type == BAYES)
    {
        GList *node;

        for (node = accounts; node; node = node->next)
            get_imap_info (dialog, node->data, NULL, _("Bayesian"));
        show_count_column (dialog, TRUE);
    }
    else if (dialog->type == NBAYES)
    {
        GList *node;

        for (node = accounts; node; node = node->next)
        {
            get_imap_info (dialog, node->data, IMAP_FRAME_DESC, _("Description Field"));
            get_imap_info (dialog, node->data, IMAP_FRAME_MEMO, _("Memo Field"));
            get_imap_info (dialog, node->data, IMAP_FRAME_CSV, _("CSV Account Map"));
        }
    }
    else
    {
        show_filter_option (dialog, FALSE);
        get_account_info_online (dialog, accounts);
    }

    imap_apply_filter (dialog, FALSE);
    imap_scroll_to_first (dialog);
    total = g_strdup_printf ("%s %d", _("Total Entries"), dialog->tot_entries);
    gtk_label_set_text (GTK_LABEL (dialog->total_entries_label), total);
    gtk_widget_set_visible (dialog->total_entries_label, TRUE);
    gtk_widget_set_visible (dialog->remove_button, dialog->tot_invalid_maps > 0);
    g_free (total);
    g_list_free (accounts);
}
static void
filter_button_cb (GtkButton *button, ImapDialog *dialog)
{
    g_free (dialog->filter_text); dialog->filter_text = g_strdup (gtk_editable_get_text (GTK_EDITABLE (dialog->filter_text_entry)));
    imap_apply_filter (dialog, TRUE); gtk_widget_grab_focus (GTK_WIDGET (dialog->view)); (void)button;
}
static void
expand_button_cb (GtkButton *button, ImapDialog *dialog)
{
    GPtrArray *roots; guint i; gtk_editable_set_text (GTK_EDITABLE (dialog->filter_text_entry), ""); g_free (dialog->filter_text); dialog->filter_text = g_strdup (""); roots = imap_root_rows (dialog);
    for (i = 0; i < roots->len; i++) gtk_tree_list_row_set_expanded (g_ptr_array_index (roots, i), TRUE);
    g_ptr_array_unref (roots); gtk_filter_changed (GTK_FILTER (dialog->filter), GTK_FILTER_CHANGE_DIFFERENT); (void)button;
}
static void
collapse_button_cb (GtkButton *button, ImapDialog *dialog)
{ gtk_editable_set_text (GTK_EDITABLE (dialog->filter_text_entry), ""); g_free (dialog->filter_text); dialog->filter_text = g_strdup (""); imap_apply_filter (dialog, FALSE); (void)button; }
static void
list_type_selected_cb (GtkCheckButton *button, ImapDialog *dialog)
{
    GncListType type = gtk_check_button_get_active (GTK_CHECK_BUTTON (dialog->radio_bayes)) ? BAYES : gtk_check_button_get_active (GTK_CHECK_BUTTON (dialog->radio_nbayes)) ? NBAYES : ONLINE;
    if (type == dialog->type)
        return;
    dialog->type = type;
    get_account_info (dialog);
    if (!((type == BAYES && dialog->inv_dialog_shown.bayes) || (type == NBAYES && dialog->inv_dialog_shown.nbayes) || (type == ONLINE && dialog->inv_dialog_shown.online))) gnc_imap_invalid_maps_dialog (dialog);
    (void)button;
}
static void
imap_update_grid_lines (gpointer prefs, gchar *pref, gpointer user_data)
{
    ImapDialog *dialog = user_data;

    gtk_column_view_set_show_row_separators (
        dialog->view,
        gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL, GNC_PREF_GRID_LINES_HORIZONTAL));
    gtk_column_view_set_show_column_separators (
        dialog->view,
        gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL, GNC_PREF_GRID_LINES_VERTICAL));
    (void)prefs;
    (void)pref;
}
static void
close_handler (gpointer user_data)
{ ImapDialog *dialog = user_data; gnc_save_window_size (GNC_PREFS_GROUP, dialog->window); gtk_window_destroy (dialog->window); }
static gboolean
close_request_cb (GtkWindow *window, ImapDialog *dialog)
{ gnc_save_window_size (GNC_PREFS_GROUP, dialog->window); (void)window; return FALSE; }
static void
destroy_cb (GtkWidget *widget, ImapDialog *dialog)
{
    gnc_prefs_remove_cb_by_func (GNC_PREFS_GROUP_GENERAL,
                                 GNC_PREF_GRID_LINES_HORIZONTAL,
                                 imap_update_grid_lines, dialog);
    gnc_prefs_remove_cb_by_func (GNC_PREFS_GROUP_GENERAL,
                                 GNC_PREF_GRID_LINES_VERTICAL,
                                 imap_update_grid_lines, dialog);
    gnc_unregister_gui_component_by_data (DIALOG_IMAP_CM_CLASS, dialog);
    g_object_set_data (G_OBJECT (dialog->window), "gnc-imap-dialog", NULL);
    g_clear_pointer (&dialog->filter_text, g_free);
    g_clear_object (&dialog->selection);
    g_clear_object (&dialog->filter_model);
    g_clear_object (&dialog->filter);
    g_clear_object (&dialog->tree_model);
    g_clear_object (&dialog->rows);
    dialog->window = NULL;
    g_free (dialog);
    (void)widget;
}

static gboolean
show_handler (const char *klass, gint component_id, gpointer user_data, gpointer iter_data)
{ ImapDialog *dialog = user_data; if (dialog) gtk_window_present (dialog->window); (void)klass; (void)component_id; (void)iter_data; return dialog != NULL; }
static void
refresh_handler (GHashTable *changes, gpointer user_data) { (void)changes; (void)user_data; }

static void
close_clicked_cb (GtkButton *button, ImapDialog *dialog)
{
    gnc_close_gui_component_by_data (DIALOG_IMAP_CM_CLASS, dialog);
    (void)button;
}

static void
create_dialog (GtkWidget *parent, ImapDialog *dialog)
{
    GtkBuilder *builder = gtk_builder_new (); GtkWidget *button;
    gnc_builder_add_from_file (builder, "dialog-imap-editor.ui", "import_map_window");
    dialog->window = GTK_WINDOW (gtk_builder_get_object (builder, "import_map_window")); dialog->session = gnc_get_current_session (); dialog->type = BAYES;
    if (parent)
        gtk_window_set_transient_for (dialog->window, GTK_WINDOW (parent));
    gtk_widget_set_name (GTK_WIDGET (dialog->window), "gnc-id-import-map");
    dialog->radio_bayes = GTK_WIDGET (gtk_builder_get_object (builder, "radio-bayes")); dialog->radio_nbayes = GTK_WIDGET (gtk_builder_get_object (builder, "radio-nbayes")); dialog->radio_online = GTK_WIDGET (gtk_builder_get_object (builder, "radio-online"));
    dialog->filter_text_entry = GTK_WIDGET (gtk_builder_get_object (builder, "filter-text-entry")); dialog->filter_label = GTK_WIDGET (gtk_builder_get_object (builder, "filter-label")); dialog->filter_button = GTK_WIDGET (gtk_builder_get_object (builder, "filter-button")); dialog->expand_button = GTK_WIDGET (gtk_builder_get_object (builder, "expand-button")); dialog->collapse_button = GTK_WIDGET (gtk_builder_get_object (builder, "collapse-button")); dialog->total_entries_label = GTK_WIDGET (gtk_builder_get_object (builder, "total_entries_label")); dialog->remove_button = GTK_WIDGET (gtk_builder_get_object (builder, "remove_button"));
    dialog->rows = g_list_store_new (G_TYPE_OBJECT); dialog->tree_model = gtk_tree_list_model_new (G_LIST_MODEL (g_object_ref (dialog->rows)), FALSE, FALSE, imap_create_children, NULL, NULL); dialog->filter = gtk_custom_filter_new (imap_filter_match, dialog, NULL); dialog->filter_model = gtk_filter_list_model_new (G_LIST_MODEL (g_object_ref (dialog->tree_model)), GTK_FILTER (g_object_ref (dialog->filter))); dialog->selection = gtk_multi_selection_new (G_LIST_MODEL (g_object_ref (dialog->filter_model)));
    dialog->view = GTK_COLUMN_VIEW (gtk_builder_get_object (builder, "treeview")); gtk_column_view_set_model (dialog->view, GTK_SELECTION_MODEL (dialog->selection)); gtk_column_view_set_enable_rubberband (dialog->view, TRUE); imap_update_grid_lines (NULL, NULL, dialog);
    imap_add_column (dialog, _("Source Account Name"), 0, TRUE); dialog->based_on_column = imap_add_column (dialog, _("Based On"), 1, FALSE); imap_add_column (dialog, _("Match String"), 2, TRUE); imap_add_column (dialog, _("Mapped to Account Name"), 3, TRUE); dialog->count_column = imap_add_column (dialog, _("Count of Match String Usage"), 4, FALSE);
    gnc_prefs_register_cb (GNC_PREFS_GROUP_GENERAL, GNC_PREF_GRID_LINES_HORIZONTAL, imap_update_grid_lines, dialog);
    gnc_prefs_register_cb (GNC_PREFS_GROUP_GENERAL, GNC_PREF_GRID_LINES_VERTICAL, imap_update_grid_lines, dialog);
    g_signal_connect (dialog->radio_bayes, "toggled", G_CALLBACK (list_type_selected_cb), dialog);
    g_signal_connect (dialog->radio_nbayes, "toggled", G_CALLBACK (list_type_selected_cb), dialog);
    g_signal_connect (dialog->radio_online, "toggled", G_CALLBACK (list_type_selected_cb), dialog);
    g_signal_connect (dialog->filter_button, "clicked", G_CALLBACK (filter_button_cb), dialog);
    g_signal_connect (dialog->expand_button, "clicked", G_CALLBACK (expand_button_cb), dialog);
    g_signal_connect (dialog->collapse_button, "clicked", G_CALLBACK (collapse_button_cb), dialog);
    g_signal_connect (dialog->selection, "selection-changed",
                      G_CALLBACK (imap_selection_changed_cb), dialog);
    button = GTK_WIDGET (gtk_builder_get_object (builder, "delete_button")); g_signal_connect_swapped (button, "clicked", G_CALLBACK (imap_delete_selected), dialog); button = GTK_WIDGET (gtk_builder_get_object (builder, "remove_button")); g_signal_connect_swapped (button, "clicked", G_CALLBACK (imap_remove_invalid_maps), dialog); button = GTK_WIDGET (gtk_builder_get_object (builder, "close_button")); g_signal_connect (button, "clicked", G_CALLBACK (close_clicked_cb), dialog); gtk_window_set_default_widget (dialog->window, button);
    g_signal_connect (dialog->window, "close-request", G_CALLBACK (close_request_cb), dialog); g_signal_connect (dialog->window, "destroy", G_CALLBACK (destroy_cb), dialog); g_object_set_data (G_OBJECT (dialog->window), "gnc-imap-dialog", dialog); g_object_unref (builder);
    gnc_restore_window_size (GNC_PREFS_GROUP, dialog->window, parent ? GTK_WINDOW (parent) : NULL); get_account_info (dialog);
}

void
gnc_imap_dialog (GtkWidget *parent)
{
    ImapDialog *dialog; gint component_id;
    if (gnc_forall_gui_components (DIALOG_IMAP_CM_CLASS, show_handler, NULL)) return;
    dialog = g_new0 (ImapDialog, 1); create_dialog (parent, dialog);
    component_id = gnc_register_gui_component (DIALOG_IMAP_CM_CLASS, refresh_handler, close_handler, dialog); gnc_gui_component_set_session (component_id, dialog->session);
    gtk_window_present (dialog->window); imap_scroll_to_first (dialog); gnc_imap_invalid_maps_dialog (dialog);
}
