/**
 * @brief GncTreeView implementation for Scheduled Transaction List.
 * @author Copyright (C) 2007 Joshua Sled <jsled@asynchronous.org>
 **/
/********************************************************************
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of version 2 of the GNU General Public *
 * License as published by the Free Software Foundation.            *
 *
 * As a special exception, permission is granted to link the binary
 * module resultant from this code with the OpenSSL project's
 * "OpenSSL" library (or modified versions of it that use the same
 * license as the "OpenSSL" library), and distribute the linked
 * executable.  You must obey the GNU General Public License in all
 * respects for all of the code used other than "OpenSSL". If you
 * modify this file, you may extend this exception to your version
 * of the file, but you are not obligated to do so. If you do not
 * wish to do so, delete this exception statement from your version
 * of this file.
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

#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <string.h>

#include "gnc-tree-view.h"
#include "gnc-tree-view-sx-list.h"
#include "gnc-sx-list-tree-model-adapter.h"
#include "gnc-gconf-utils.h"

#define LOG_MOD "gnc.ui.tree-view.sx-list"
static QofLogModule log_module = LOG_MOD;
#undef G_LOG_DOMAIN
#define G_LOG_DOMAIN LOG_MOD

static void gnc_tree_view_sx_list_class_init(GncTreeViewSxListClass *klass);
static void gnc_tree_view_sx_list_init(GncTreeViewSxList *view);
static void gnc_tree_view_sx_list_dispose(GObject *object);
static void gnc_tree_view_sx_list_finalize(GObject *object);

typedef struct GncTreeViewSxListPrivate
{
    GtkTreeModel *tree_model;
    gboolean disposed;
} GncTreeViewSxListPrivate;

#define GNC_TREE_VIEW_SX_LIST_GET_PRIVATE(o)  \
   (G_TYPE_INSTANCE_GET_PRIVATE ((o), GNC_TYPE_TREE_VIEW_SX_LIST, GncTreeViewSxListPrivate))

static GObjectClass *parent_class = NULL;

GType
gnc_tree_view_sx_list_get_type(void)
{
    static GType gnc_tree_view_sx_list_type = 0;

    if (gnc_tree_view_sx_list_type == 0)
    {
        static const GTypeInfo our_info =
        {
            sizeof (GncTreeViewSxListClass),
            NULL,
            NULL,
            (GClassInitFunc) gnc_tree_view_sx_list_class_init,
            NULL,
            NULL,
            sizeof (GncTreeViewSxList),
            0,
            (GInstanceInitFunc) gnc_tree_view_sx_list_init
        };

        gnc_tree_view_sx_list_type = g_type_register_static (GNC_TYPE_TREE_VIEW,
                                     "GncTreeViewSxList",
                                     &our_info, 0);
    }

    return gnc_tree_view_sx_list_type;
}

static void
gnc_tree_view_sx_list_class_init(GncTreeViewSxListClass *klass)
{
    GObjectClass *o_class;

    parent_class = g_type_class_peek_parent (klass);

    o_class = G_OBJECT_CLASS (klass);

    o_class->dispose =  gnc_tree_view_sx_list_dispose;
    o_class->finalize = gnc_tree_view_sx_list_finalize;

    g_type_class_add_private(klass, sizeof(GncTreeViewSxListPrivate));
}

static void
gnc_tree_view_sx_list_init (GncTreeViewSxList *view)
{
    ; /* nop */
}

static void
gnc_tree_view_sx_list_dispose(GObject *object)
{
    GncTreeViewSxList *view;
    GncTreeViewSxListPrivate *priv;

    gnc_leave_return_if_fail (object != NULL);
    gnc_leave_return_if_fail (GNC_IS_TREE_VIEW_SX_LIST (object));

    view = GNC_TREE_VIEW_SX_LIST (object);
    priv = GNC_TREE_VIEW_SX_LIST_GET_PRIVATE(view);

    if (priv->disposed)
        return;
    priv->disposed = TRUE;

    g_object_unref(G_OBJECT(priv->tree_model));
    priv->tree_model = NULL;

    if (G_OBJECT_CLASS (parent_class)->dispose)
        (* G_OBJECT_CLASS (parent_class)->dispose) (object);
}

static void
gnc_tree_view_sx_list_finalize(GObject *object)
{
    GncTreeViewSxList *view;

    gnc_leave_return_if_fail(object != NULL);
    gnc_leave_return_if_fail(GNC_IS_TREE_VIEW_SX_LIST (object));

    view = GNC_TREE_VIEW_SX_LIST(object);

    if (G_OBJECT_CLASS(parent_class)->finalize)
        (* G_OBJECT_CLASS(parent_class)->finalize) (object);
}

GtkTreeView*
gnc_tree_view_sx_list_new(GncSxInstanceModel *sx_instances)
{
    GncTreeView *view;
    GtkTreeViewColumn *col;
    GncTreeViewSxListPrivate *priv;

    view = (GncTreeView*)g_object_new(GNC_TYPE_TREE_VIEW_SX_LIST, NULL);
    g_object_set(view, "name", "sx_list_tree", NULL);

    priv = GNC_TREE_VIEW_SX_LIST_GET_PRIVATE(view);

    priv->tree_model = GTK_TREE_MODEL(gnc_sx_list_tree_model_adapter_new(sx_instances));
    gnc_tree_view_set_model(view, GTK_TREE_MODEL(priv->tree_model));

    col = gnc_tree_view_add_text_column(view, _("Name"), "name", NULL,
                                        "Semi-Monthly Paycheck",
                                        SXLTMA_COL_NAME, -1, NULL);
    g_object_set_data(G_OBJECT(col), DEFAULT_VISIBLE, GINT_TO_POINTER(1));

    col = gnc_tree_view_add_toggle_column(view, _("Enabled"),
                                          /* Translators: This string has a context prefix; the translation
                                             must only contain the part after the | character. */
                                          Q_("Single-character short column-title form of 'Enabled'|E"),
                                          "enabled", SXLTMA_COL_ENABLED,
                                          GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
                                          NULL, NULL);
    g_object_set_data(G_OBJECT(col), DEFAULT_VISIBLE, GINT_TO_POINTER(1));

    col = gnc_tree_view_add_text_column(view, _("Frequency"), "frequency", NULL,
                                        "Weekly (x3): -------",
                                        SXLTMA_COL_FREQUENCY, -1, NULL);
    g_object_set_data(G_OBJECT(col), DEFAULT_VISIBLE, GINT_TO_POINTER(1));

    col = gnc_tree_view_add_text_column(view, _("Last Occur"), "last-occur", NULL,
                                        "2007-01-02",
                                        SXLTMA_COL_LAST_OCCUR, -1, NULL);
    g_object_set_data(G_OBJECT(col), DEFAULT_VISIBLE, GINT_TO_POINTER(1));

    col = gnc_tree_view_add_text_column(view, _("Next Occur"), "next-occur", NULL,
                                        "2007-01-02",
                                        SXLTMA_COL_NEXT_OCCUR, -1, NULL);
    g_object_set_data(G_OBJECT(col), DEFAULT_VISIBLE, GINT_TO_POINTER(1));

    gnc_tree_view_configure_columns(view);

    gtk_widget_show(GTK_WIDGET(view));
    return GTK_TREE_VIEW(view);
}

SchedXaction*
gnc_tree_view_sx_list_get_sx_from_path(GncTreeViewSxList *view, GtkTreePath *path)
{
    GtkTreeIter iter;
    GncTreeViewSxListPrivate *priv = GNC_TREE_VIEW_SX_LIST_GET_PRIVATE(view);
    gtk_tree_model_get_iter(GTK_TREE_MODEL(priv->tree_model), &iter, path);
    return gnc_sx_list_tree_model_adapter_get_sx_instances(
               GNC_SX_LIST_TREE_MODEL_ADAPTER(priv->tree_model), &iter)->sx;
}
