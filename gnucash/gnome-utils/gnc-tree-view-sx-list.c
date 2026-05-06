/**
 * @brief GncTreeView implementation for Scheduled Transaction List.
 * @author Copyright (C) 2007 Joshua Sled <jsled@asynchronous.org>
 **/
/********************************************************************
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

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <string.h>

#include "gnc-tree-view.h"
#include "gnc-tree-view-sx-list.h"
#include "gnc-sx-list-tree-model-adapter.h"

#define LOG_MOD "gnc.ui.tree-view.sx-list"
static QofLogModule log_module = LOG_MOD;
#undef G_LOG_DOMAIN
#define G_LOG_DOMAIN LOG_MOD

static void gnc_tree_view_sx_list_dispose (GObject *object);
static void gnc_tree_view_sx_list_finalize (GObject *object);

struct _GncTreeViewSxList
{
    GncTreeView gnc_tree_view;

    GtkTreeModel *tree_model;

    SchedXaction *sx;
    GtkAdjustment *adjustment;
    gdouble position;

    gboolean disposed;
};

G_DEFINE_TYPE(GncTreeViewSxList, gnc_tree_view_sx_list, GNC_TYPE_TREE_VIEW)

static void
gnc_tree_view_sx_list_class_init (GncTreeViewSxListClass *klass)
{
    GObjectClass *o_class = G_OBJECT_CLASS(klass);

    o_class->dispose =  gnc_tree_view_sx_list_dispose;
    o_class->finalize = gnc_tree_view_sx_list_finalize;
}

static void
gnc_tree_view_sx_list_init (GncTreeViewSxList *view)
{
    ; /* nop */
}

static void
gnc_tree_view_sx_list_dispose (GObject *object)
{
    GncTreeViewSxList *view;

    gnc_leave_return_if_fail (object != NULL);
    gnc_leave_return_if_fail (GNC_IS_TREE_VIEW_SX_LIST(object));

    view = GNC_TREE_VIEW_SX_LIST(object);

    if (view->disposed)
        return;
    view->disposed = TRUE;

    g_object_unref (G_OBJECT(view->tree_model));
    view->tree_model = NULL;

    G_OBJECT_CLASS(gnc_tree_view_sx_list_parent_class)->dispose (object);
}

static void
gnc_tree_view_sx_list_finalize(GObject *object)
{
    gnc_leave_return_if_fail (object != NULL);
    gnc_leave_return_if_fail (GNC_IS_TREE_VIEW_SX_LIST(object));

    G_OBJECT_CLASS(gnc_tree_view_sx_list_parent_class)->finalize (object);
}

/************************************************************
 *                        Callbacks                         *
 ************************************************************/

static gboolean
gnc_tree_view_sx_list_restore (gpointer user_data)
{
    GncTreeViewSxList *view = user_data;

    if (view->adjustment)
    {
        gtk_adjustment_set_value (view->adjustment, view->position);
        view->adjustment = NULL;
    }
    if (view->sx)
    {
        SchedXaction *sx = view->sx;
        GtkTreePath *path = gtk_tree_path_new_first ();

        while (gnc_tree_view_path_is_valid (GNC_TREE_VIEW(view), path))
        {
            if (sx == gnc_tree_view_sx_list_get_sx_from_path (view, path))
            {
                GtkTreeSelection *selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(view));

                gtk_tree_selection_unselect_all (selection);
                gtk_tree_selection_select_path (selection, path);
                gtk_tree_view_set_cursor (GTK_TREE_VIEW(view), path, NULL, FALSE);
                gtk_widget_grab_focus (GTK_WIDGET(view));
                gtk_tree_view_scroll_to_cell (GTK_TREE_VIEW(view), path, NULL, FALSE, 0.0, 0.0);
                break;
            }
            gtk_tree_path_next (path);
        }
        gtk_tree_path_free (path);

        view->sx = NULL;
    }

    return FALSE;
}

static void
gnc_tree_view_sx_list_enabled_toggled (GtkCellRendererToggle *cell,
                                      const gchar *path_str,
                                      gpointer user_data)
{
    GncTreeViewSxList *view = user_data;
    GtkTreePath *path = gtk_tree_path_new_from_string (path_str);
    SchedXaction *sx = gnc_tree_view_sx_list_get_sx_from_path (view, path);

    if (sx)
    {
        GtkTreeSortable *sortable = GTK_TREE_SORTABLE(view->tree_model);
        gint sort_column_id;
        GtkSortType sort_order;

        if (gtk_tree_sortable_get_sort_column_id (sortable, &sort_column_id, &sort_order) &&
            sort_column_id == SXLTMA_COL_ENABLED)
        {
            view->sx = sx;
        }
        else
        {
            view->adjustment = gtk_scrollable_get_vadjustment (GTK_SCROLLABLE(view));
            view->position = gtk_adjustment_get_value (view->adjustment);
        }

        gboolean enabled = !gtk_cell_renderer_toggle_get_active (cell);

        xaccSchedXactionSetEnabled (sx, enabled);

        g_idle_add((GSourceFunc)gnc_tree_view_sx_list_restore, user_data);
    }

    gtk_tree_path_free (path);
}


GtkTreeView*
gnc_tree_view_sx_list_new (GncSxInstanceModel *sx_instances)
{
    GncTreeViewSxList *view = (GncTreeViewSxList*)g_object_new (GNC_TYPE_TREE_VIEW_SX_LIST, NULL);
    g_object_set (view, "name", "gnc-id-sx-list-tree", NULL);

    view->tree_model = GTK_TREE_MODEL(gnc_sx_list_tree_model_adapter_new (sx_instances));
    gtk_tree_view_set_model (GTK_TREE_VIEW(view), GTK_TREE_MODEL(view->tree_model));

    GtkTreeViewColumn *col = gnc_tree_view_add_text_column (GNC_TREE_VIEW(view), _("Name"), "name", NULL,
                                                            "Semi-Monthly Paycheck",
                                                             SXLTMA_COL_NAME, -1, NULL);
    g_object_set_data (G_OBJECT(col), DEFAULT_VISIBLE, GINT_TO_POINTER(1));

    col = gnc_tree_view_add_toggle_column (GNC_TREE_VIEW(view), _("Enabled"),
                                           C_("Single-character short column-title form of 'Enabled'", "E"),
                                           "enabled", SXLTMA_COL_ENABLED,
                                           GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
                                           NULL, gnc_tree_view_sx_list_enabled_toggled);
    g_object_set_data (G_OBJECT(col), DEFAULT_VISIBLE, GINT_TO_POINTER(1));

    col = gnc_tree_view_add_text_column (GNC_TREE_VIEW(view), _("Frequency"), "frequency", NULL,
                                         "Weekly (x3): -------",
                                         SXLTMA_COL_FREQUENCY, -1, NULL);
    g_object_set_data (G_OBJECT(col), DEFAULT_VISIBLE, GINT_TO_POINTER(1));

    col = gnc_tree_view_add_numeric_column (GNC_TREE_VIEW(view), _("Postponed"),
                                           "postponed", "    Postponed",
                                           SXLTMA_COL_NUM_POSTPONED,
                                           GNC_TREE_VIEW_COLUMN_COLOR_NONE,
                                           GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS, NULL);
    g_object_set_data (G_OBJECT(col), DEFAULT_VISIBLE, GINT_TO_POINTER(0));

    col = gnc_tree_view_add_text_column (GNC_TREE_VIEW(view), _("Last Occur"), "last-occur", NULL,
                                         "2007-01-02",
                                         SXLTMA_COL_LAST_OCCUR, -1, NULL);
    g_object_set_data (G_OBJECT(col), DEFAULT_VISIBLE, GINT_TO_POINTER(1));

    col = gnc_tree_view_add_text_column (GNC_TREE_VIEW(view), _("Next Occur"), "next-occur", NULL,
                                         "2007-01-02",
                                         SXLTMA_COL_NEXT_OCCUR, -1, NULL);
    g_object_set_data (G_OBJECT(col), DEFAULT_VISIBLE, GINT_TO_POINTER(1));

    gnc_tree_view_configure_columns (GNC_TREE_VIEW(view));

    gtk_widget_show (GTK_WIDGET(view));
    return GTK_TREE_VIEW(view);
}

SchedXaction*
gnc_tree_view_sx_list_get_sx_from_path (GncTreeViewSxList *view, GtkTreePath *path)
{
    GtkTreeIter iter;
    gtk_tree_model_get_iter (GTK_TREE_MODEL(view->tree_model), &iter, path);
    return gnc_sx_list_tree_model_adapter_get_sx_instances(
               GNC_SX_LIST_TREE_MODEL_ADAPTER(view->tree_model), &iter)->sx;
}
