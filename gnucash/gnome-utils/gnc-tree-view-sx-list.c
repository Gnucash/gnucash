/**
 * @brief GTK4 ColumnView implementation for Scheduled Transaction List.
 */
/********************************************************************
 * Copyright (C) 2007 Joshua Sled <jsled@asynchronous.org>
 * Author: Joshua Sled <jsled@asynchronous.org>
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of version 2 and/or version 3 of the   *
 * GNU General Public                                               *
 * License as published by the Free Software Foundation.            *
 *                                                                  *
 * As a special exception, permission is granted to link the binary *
 * module resultant from this code with the OpenSSL project's       *
 * "OpenSSL" library (or modified versions of it that use the same  *
 * license as the "OpenSSL" library), and distribute the linked     *
 * executable.  You must obey the GNU General Public License in all *
 * respects for all of the code used other than "OpenSSL". If you   *
 * modify this file, you may extend this exception to your version  *
 * of the file, but you are not obligated to do so. If you do not   *
 * wish to do so, delete this exception statement from your version *
 * of this file.                                                    *
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
 *                                                                  *
 *******************************************************************/
#include <config.h>

#include <glib/gi18n.h>
#include <gtk/gtk.h>

#include "gnc-tree-view-sx-list.h"
#include "gnc-tree-view.h"
#include "gnc-sx-list-tree-model-adapter.h"

typedef enum
{
    SX_LIST_COLUMN_NAME,
    SX_LIST_COLUMN_ENABLED,
    SX_LIST_COLUMN_FREQUENCY,
    SX_LIST_COLUMN_POSTPONED,
    SX_LIST_COLUMN_LAST_OCCUR,
    SX_LIST_COLUMN_NEXT_OCCUR
} SxListColumn;

typedef struct
{
    GncSxListTreeModelAdapter *adapter;
    GtkSortListModel *sorted;
    GtkMultiSelection *selection;
    GtkColumnViewColumn *enabled_column;
} GncSxListViewData;

static GQuark sx_list_view_data_quark (void)
{
    return g_quark_from_static_string ("gnc-sx-list-view-data");
}

static GncSxListViewData*
sx_list_view_data (GtkColumnView *view)
{
    return g_object_get_qdata (G_OBJECT (view), sx_list_view_data_quark ());
}

static void
sx_list_view_data_free (GncSxListViewData *data)
{
    if (!data)
        return;
    g_clear_object (&data->enabled_column);
    g_clear_object (&data->selection);
    g_clear_object (&data->sorted);
    g_clear_object (&data->adapter);
    g_free (data);
}

static gint
safe_invalidable_date_compare (const GDate *a, const GDate *b)
{
    if (!g_date_valid (a) && !g_date_valid (b))
        return 0;
    if (!g_date_valid (a))
        return 1;
    if (!g_date_valid (b))
        return -1;
    return g_date_compare (a, b);
}

static gint
sx_list_row_compare (gconstpointer a, gconstpointer b, gpointer user_data)
{
    GncSxListRow *left = GNC_SX_LIST_ROW ((gpointer)a);
    GncSxListRow *right = GNC_SX_LIST_ROW ((gpointer)b);
    SxListColumn column = GPOINTER_TO_INT (user_data);
    SchedXaction *left_sx = gnc_sx_list_row_get_sx (left);
    SchedXaction *right_sx = gnc_sx_list_row_get_sx (right);
    gint result = 0;

    switch (column)
    {
        case SX_LIST_COLUMN_NAME:
            result = g_utf8_collate (gnc_sx_list_row_get_name (left),
                                     gnc_sx_list_row_get_name (right));
            break;
        case SX_LIST_COLUMN_ENABLED:
            result = (gint)gnc_sx_list_row_get_enabled (left) -
                     (gint)gnc_sx_list_row_get_enabled (right);
            break;
        case SX_LIST_COLUMN_FREQUENCY:
            result = recurrenceListCmp (gnc_sx_get_schedule (left_sx),
                                        gnc_sx_get_schedule (right_sx));
            break;
        case SX_LIST_COLUMN_POSTPONED:
            result = (gint)gnc_sx_list_row_get_num_postponed (left) -
                     (gint)gnc_sx_list_row_get_num_postponed (right);
            break;
        case SX_LIST_COLUMN_LAST_OCCUR:
            result = safe_invalidable_date_compare (xaccSchedXactionGetLastOccurDate (left_sx),
                                                    xaccSchedXactionGetLastOccurDate (right_sx));
            break;
        case SX_LIST_COLUMN_NEXT_OCCUR:
            result = g_utf8_collate (gnc_sx_list_row_get_next_occur (left),
                                     gnc_sx_list_row_get_next_occur (right));
            break;
    }

    if (result != 0 || column == SX_LIST_COLUMN_NAME)
        return result;
    return g_utf8_collate (gnc_sx_list_row_get_name (left),
                           gnc_sx_list_row_get_name (right));
}

static const gchar*
sx_list_row_text (GncSxListRow *row, SxListColumn column, gchar *number, gsize number_size)
{
    switch (column)
    {
        case SX_LIST_COLUMN_NAME: return gnc_sx_list_row_get_name (row);
        case SX_LIST_COLUMN_FREQUENCY: return gnc_sx_list_row_get_frequency (row);
        case SX_LIST_COLUMN_POSTPONED:
            g_snprintf (number, number_size, "%u", gnc_sx_list_row_get_num_postponed (row));
            return number;
        case SX_LIST_COLUMN_LAST_OCCUR: return gnc_sx_list_row_get_last_occur (row);
        case SX_LIST_COLUMN_NEXT_OCCUR: return gnc_sx_list_row_get_next_occur (row);
        case SX_LIST_COLUMN_ENABLED: break;
    }
    return "";
}

static void
sx_list_text_setup (GtkSignalListItemFactory *factory, GtkListItem *item, gpointer user_data)
{
    GtkWidget *label = gtk_label_new (NULL);
    gtk_label_set_xalign (GTK_LABEL (label),
                          GPOINTER_TO_INT (user_data) == SX_LIST_COLUMN_POSTPONED ? 1.0f : 0.0f);
    gtk_label_set_ellipsize (GTK_LABEL (label), PANGO_ELLIPSIZE_END);
    gtk_list_item_set_child (item, label);
}

static void
sx_list_text_bind (GtkSignalListItemFactory *factory, GtkListItem *item, gpointer user_data)
{
    GncSxListRow *row = GNC_SX_LIST_ROW (gtk_list_item_get_item (item));
    gchar number[32];
    const gchar *text = sx_list_row_text (row, GPOINTER_TO_INT (user_data), number, sizeof number);
    gtk_label_set_text (GTK_LABEL (gtk_list_item_get_child (item)), text ? text : "");
}

static gboolean
sx_list_refresh_idle (gpointer user_data)
{
    GncSxListTreeModelAdapter *adapter = GNC_SX_LIST_TREE_MODEL_ADAPTER (user_data);
    gnc_sx_list_tree_model_adapter_refresh (adapter);
    return G_SOURCE_REMOVE;
}

static void
sx_list_enabled_toggled (GtkCheckButton *button, GtkListItem *item)
{
    GncSxListRow *row = GNC_SX_LIST_ROW (gtk_list_item_get_item (item));
    GncSxListTreeModelAdapter *adapter = g_object_get_data (G_OBJECT (item), "sx-list-adapter");

    if (!row || !adapter)
        return;
    if (gtk_check_button_get_active (button) == gnc_sx_list_row_get_enabled (row))
        return;

    xaccSchedXactionSetEnabled (gnc_sx_list_row_get_sx (row),
                                gtk_check_button_get_active (button));
    g_idle_add_full (G_PRIORITY_DEFAULT_IDLE, sx_list_refresh_idle,
                     g_object_ref (adapter), g_object_unref);
}

static void
sx_list_enabled_setup (GtkSignalListItemFactory *factory, GtkListItem *item, gpointer user_data)
{
    GtkWidget *button = gtk_check_button_new ();
    gtk_widget_set_halign (button, GTK_ALIGN_CENTER);
    gtk_widget_set_valign (button, GTK_ALIGN_CENTER);
    g_signal_connect (button, "toggled", G_CALLBACK (sx_list_enabled_toggled), item);
    gtk_list_item_set_child (item, button);
}

static void
sx_list_enabled_bind (GtkSignalListItemFactory *factory, GtkListItem *item, gpointer user_data)
{
    GncSxListRow *row = GNC_SX_LIST_ROW (gtk_list_item_get_item (item));
    GtkCheckButton *button = GTK_CHECK_BUTTON (gtk_list_item_get_child (item));

    g_object_set_data (G_OBJECT (item), "sx-list-adapter", user_data);
    g_signal_handlers_block_by_func (button, sx_list_enabled_toggled, item);
    gtk_check_button_set_active (button, gnc_sx_list_row_get_enabled (row));
    g_signal_handlers_unblock_by_func (button, sx_list_enabled_toggled, item);
}

static GtkColumnViewColumn*
sx_list_append_text_column (GtkColumnView *view, const gchar *title, SxListColumn column,
                            gboolean expand, gboolean visible)
{
    GtkListItemFactory *factory = gtk_signal_list_item_factory_new ();
    GtkColumnViewColumn *view_column = gtk_column_view_column_new (title, factory);
    GtkSorter *sorter = GTK_SORTER (gtk_custom_sorter_new (sx_list_row_compare,
                                                           GINT_TO_POINTER (column), NULL));

    g_signal_connect (factory, "setup", G_CALLBACK (sx_list_text_setup), GINT_TO_POINTER (column));
    g_signal_connect (factory, "bind", G_CALLBACK (sx_list_text_bind), GINT_TO_POINTER (column));
    gtk_column_view_column_set_sorter (view_column, sorter);
    gtk_column_view_column_set_resizable (view_column, TRUE);
    gtk_column_view_column_set_expand (view_column, expand);
    gtk_column_view_column_set_visible (view_column, visible);
    gtk_column_view_append_column (view, view_column);
    g_object_unref (sorter);
    return view_column;
}

GtkColumnView*
gnc_sx_list_view_new (GncSxInstanceModel *sx_instances)
{
    GtkColumnView *view;
    GncSxListViewData *data;
    GtkSorter *sorter;
    GtkColumnViewColumn *name_column;
    GtkColumnViewColumn *column;
    GtkListItemFactory *enabled_factory;

    g_return_val_if_fail (GNC_IS_SX_INSTANCE_MODEL (sx_instances), NULL);
    view = GTK_COLUMN_VIEW (gtk_column_view_new (NULL));
    gtk_widget_set_name (GTK_WIDGET (view), "gnc-id-sx-list");
    gtk_column_view_set_reorderable (view, TRUE);
    gnc_column_view_bind_grid_line_preferences (view);

    data = g_new0 (GncSxListViewData, 1);
    data->adapter = gnc_sx_list_tree_model_adapter_new (sx_instances);
    /* GtkSortListModel consumes both references; the view keeps its sorter. */
    sorter = g_object_ref (gtk_column_view_get_sorter (view));
    data->sorted = gtk_sort_list_model_new
        (g_object_ref (gnc_sx_list_tree_model_adapter_get_model (data->adapter)), sorter);
    data->selection = gtk_multi_selection_new (G_LIST_MODEL (g_object_ref (data->sorted)));
    gtk_column_view_set_model (view, GTK_SELECTION_MODEL (data->selection));

    name_column = sx_list_append_text_column (view, _("Name"), SX_LIST_COLUMN_NAME, TRUE, TRUE);
    enabled_factory = gtk_signal_list_item_factory_new ();
    g_signal_connect (enabled_factory, "setup", G_CALLBACK (sx_list_enabled_setup), NULL);
    g_signal_connect (enabled_factory, "bind", G_CALLBACK (sx_list_enabled_bind), data->adapter);
    data->enabled_column = gtk_column_view_column_new
        (C_("Single-character short column-title form of 'Enabled'", "E"), enabled_factory);
    gtk_column_view_column_set_resizable (data->enabled_column, TRUE);
    gtk_column_view_append_column (view, data->enabled_column);
    column = sx_list_append_text_column (view, _("Frequency"), SX_LIST_COLUMN_FREQUENCY,
                                         TRUE, TRUE);
    g_object_unref (column);
    column = sx_list_append_text_column (view, _("Postponed"), SX_LIST_COLUMN_POSTPONED,
                                         FALSE, FALSE);
    g_object_unref (column);
    column = sx_list_append_text_column (view, _("Last Occur"), SX_LIST_COLUMN_LAST_OCCUR,
                                         FALSE, TRUE);
    g_object_unref (column);
    column = sx_list_append_text_column (view, _("Next Occur"), SX_LIST_COLUMN_NEXT_OCCUR,
                                         FALSE, TRUE);
    g_object_unref (column);
    gtk_column_view_sort_by_column (view, name_column, GTK_SORT_ASCENDING);
    g_object_unref (name_column);

    g_object_set_qdata_full (G_OBJECT (view), sx_list_view_data_quark (), data,
                             (GDestroyNotify)sx_list_view_data_free);
    return view;
}

GtkSelectionModel*
gnc_sx_list_view_get_selection (GtkColumnView *view)
{
    GncSxListViewData *data = sx_list_view_data (view);
    return data ? GTK_SELECTION_MODEL (data->selection) : NULL;
}

GList*
gnc_sx_list_view_get_selected_sxes (GtkColumnView *view)
{
    GtkSelectionModel *selection = gnc_sx_list_view_get_selection (view);
    GListModel *model;
    GtkBitset *selected;
    GtkBitsetIter iter;
    guint position;
    GList *sxs = NULL;

    if (!selection)
        return NULL;
    model = G_LIST_MODEL (selection);
    selected = gtk_selection_model_get_selection (selection);
    if (gtk_bitset_iter_init_first (&iter, selected, &position))
    {
        do
        {
            GncSxListRow *row = GNC_SX_LIST_ROW (g_list_model_get_item (model, position));
            sxs = g_list_prepend (sxs, gnc_sx_list_row_get_sx (row));
            g_object_unref (row);
        }
        while (gtk_bitset_iter_next (&iter, &position));
    }
    gtk_bitset_unref (selected);
    return g_list_reverse (sxs);
}

void
gnc_sx_list_view_select_sxes (GtkColumnView *view, GList *sxs)
{
    GtkSelectionModel *selection = gnc_sx_list_view_get_selection (view);
    GListModel *model;
    guint position, n_items;
    gboolean selected = FALSE;

    if (!selection)
        return;
    gtk_selection_model_unselect_all (selection);
    model = G_LIST_MODEL (selection);
    n_items = g_list_model_get_n_items (model);
    for (position = 0; position < n_items; position++)
    {
        GncSxListRow *row = GNC_SX_LIST_ROW (g_list_model_get_item (model, position));
        if (g_list_find (sxs, gnc_sx_list_row_get_sx (row)))
        {
            gtk_selection_model_select_item (selection, position, FALSE);
            if (!selected)
            {
                gtk_column_view_scroll_to (view, position, NULL, GTK_LIST_SCROLL_FOCUS, NULL);
                selected = TRUE;
            }
        }
        g_object_unref (row);
    }
    if (!selected && n_items > 0)
        gtk_selection_model_select_item (selection, 0, TRUE);
}

void
gnc_sx_list_view_refresh (GtkColumnView *view)
{
    GncSxListViewData *data = sx_list_view_data (view);
    if (data)
        gnc_sx_list_tree_model_adapter_refresh (data->adapter);
}

gboolean
gnc_sx_list_view_enabled_column_visible (GtkColumnView *view)
{
    GncSxListViewData *data = sx_list_view_data (view);
    return data && gtk_column_view_column_get_visible (data->enabled_column);
}
