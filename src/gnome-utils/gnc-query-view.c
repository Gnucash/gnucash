/********************************************************************\
 * gnc-query-view.c -- A query display view.                        *
 * Copyright (C) 2003 Derek Atkins <derek@ihtfp.com>                *
 * Copyright (C) 2012 Robert Fewell                                 *
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

#include "config.h"

#include <gtk/gtk.h>

#include "dialog-utils.h"
#include "gnc-ui-util.h"
#include "qof.h"
#include "gnc-component-manager.h"
#include "gnc-query-view.h"
#include "search-param.h"

/* Signal codes */
enum
{
    COLUMN_TOGGLED,
    ROW_SELECTED,
    DOUBLE_CLICK_ENTRY,
    LAST_SIGNAL
};

typedef struct _GNCQueryViewPriv GNCQueryViewPriv;

struct _GNCQueryViewPriv
{
    const QofParam *get_guid;
    gint	    component_id;
};

#define GNC_QUERY_VIEW_GET_PRIVATE(o)  \
   (G_TYPE_INSTANCE_GET_PRIVATE ((o), GNC_TYPE_QUERY_VIEW, GNCQueryViewPriv))

/** Static Globals ****************************************************/
static GtkTreeViewClass *parent_class = NULL;
static guint query_view_signals[LAST_SIGNAL] = {0};

/** Static function declarations **************************************/
static void gnc_query_view_init (GNCQueryView *qview);
static void gnc_query_view_init_view (GNCQueryView *qview);
static void gnc_query_view_class_init (GNCQueryViewClass *klass);
static void gnc_query_view_select_row_cb (GtkTreeSelection *selection, gpointer user_data);
static void gnc_query_view_toggled_cb (GtkCellRendererToggle *cell_renderer,
                                       gchar *path, gpointer user_data);
static void gnc_query_view_double_click_cb (GtkTreeView *tree_view,
                                             GtkTreePath       *path,
                                             GtkTreeViewColumn *column,
                                             gpointer           user_data);

static void gnc_query_view_destroy (GtkObject *object);
static void gnc_query_view_fill (GNCQueryView *qview);
static void gnc_query_view_set_query_sort (GNCQueryView *qview, gboolean new_column);


GType
gnc_query_view_get_type (void)
{
    static GType gnc_query_view_type = 0;

    if (!gnc_query_view_type)
    {
        GTypeInfo type_info =
        {
            sizeof(GNCQueryViewClass),          /* class_size */
            NULL,   				/* base_init */
            NULL,				/* base_finalize */
            (GClassInitFunc)gnc_query_view_class_init,
            NULL,				/* class_finalize */
            NULL,				/* class_data */
            sizeof (GNCQueryView),		/* */
            0,				        /* n_preallocs */
            (GInstanceInitFunc)gnc_query_view_init,
        };

        gnc_query_view_type = g_type_register_static (GTK_TYPE_TREE_VIEW,
                              "GNCQueryView",
                              &type_info, 0);
    }
    return gnc_query_view_type;
}


/********************************************************************\
 * gnc_query_view_new                                               *
 *   creates the query view                                         *
 *                                                                  *
 * Args: param_list - the list of params                            *
 *       query      - the query to use to find entries              *
 * Returns: the query view widget, or NULL if there was a problem.  *
\********************************************************************/
void
gnc_query_view_construct (GNCQueryView *qview, GList *param_list, Query *query)
{
    GNCQueryViewPriv *priv;

    g_return_if_fail (qview);
    g_return_if_fail (param_list);
    g_return_if_fail (query);
    g_return_if_fail (GNC_IS_QUERY_VIEW(qview));

    /* more configuration */
    qview->query = qof_query_copy (query);
    qview->column_params = param_list;

    /* cache the function to get the guid of this query type */
    priv = GNC_QUERY_VIEW_GET_PRIVATE (qview);
    priv->get_guid =
        qof_class_get_parameter (qof_query_get_search_for (query), QOF_PARAM_GUID);

    /* Initialize the Tree View */
    gnc_query_view_init_view (qview);

    /* Set initial sort order */
    gnc_query_view_set_query_sort (qview, TRUE);
}

GtkWidget *
gnc_query_view_new (GList *param_list, Query *query)
{
    GNCQueryView  *qview;
    GtkListStore  *liststore;
    GList         *node;
    gint           columns, i;
    gsize          array_size;
    GType         *types;

    g_return_val_if_fail (param_list, NULL);
    g_return_val_if_fail (query, NULL);

    /* Add 1 to param_list length for extra pointer column */
    columns = g_list_length (param_list) + 1;
    qview = GNC_QUERY_VIEW (g_object_new (gnc_query_view_get_type(), NULL));

    array_size = sizeof( GType ) * columns;
    types = g_slice_alloc ( array_size );

    types[0] = G_TYPE_POINTER;

    /* Get the types for the list store */
    for (i = 0, node = param_list; node; node = node->next, i++)
    {
        GNCSearchParamSimple *param = node->data;
        const char *type;

        g_assert (GNC_IS_SEARCH_PARAM_SIMPLE (param));
        type = gnc_search_param_get_param_type ((GNCSearchParam *) param);

        if (g_strcmp0 (type, QOF_TYPE_BOOLEAN) == 0)
            types[i+1] = G_TYPE_BOOLEAN;
        else
            types[i+1] = G_TYPE_STRING;
    }

    /* Create the list store and add to treeview */
    liststore = gtk_list_store_newv (columns, types );
    gtk_tree_view_set_model (GTK_TREE_VIEW (qview), GTK_TREE_MODEL (liststore));
    g_object_unref (liststore);

    /* Free array */
    g_slice_free1( array_size, types );

    gnc_query_view_construct (qview, param_list, query);

    return GTK_WIDGET (qview);
}


void gnc_query_view_reset_query (GNCQueryView *qview, Query *query)
{
    g_return_if_fail (qview);
    g_return_if_fail (query);
    g_return_if_fail (GNC_IS_QUERY_VIEW (qview));

    qof_query_destroy (qview->query);
    qview->query = qof_query_copy (query);

    gnc_query_view_set_query_sort (qview, TRUE);
}


static void
gnc_query_view_refresh_handler (GHashTable *changes, gpointer user_data)
{
    GNCQueryView *qview = (GNCQueryView *)user_data;
    g_return_if_fail (qview);
    g_return_if_fail (GNC_IS_QUERY_VIEW (qview));

    gnc_query_view_set_query_sort (qview, TRUE);
}


static void
gnc_query_view_init (GNCQueryView *qview)
{
    GNCQueryViewPriv *priv;

    qview->query = NULL;

    qview->num_columns = 0;
    qview->column_params = NULL;

    qview->sort_column = 0;
    qview->increasing = FALSE;

    qview->numeric_abs = FALSE;
    qview->numeric_inv_sort = FALSE;

    priv = GNC_QUERY_VIEW_GET_PRIVATE (qview);
    priv->component_id =
        gnc_register_gui_component ("gnc-query-view-cm-class",
                                    gnc_query_view_refresh_handler,
                                    NULL, qview);
}


static gint
sort_iter_compare_func (GtkTreeModel *model,
                          GtkTreeIter  *a,
                          GtkTreeIter  *b,
                          gpointer      userdata)
{
    /* This is really a dummy sort function, it leaves the list as is. */
    return 0;
}


/********************************************************************\
 * gnc_query_sort_order                                             *
 *   allows the sort order to be specified                          *
 *                                                                  *
 * Args: qview   - the view to sort                                 *
 *       column  - the sort column in the tree view, 1 ->           *
 *       order   - GTK_SORT_ASCENDING or GTK_SORT_DESCENDING        *
\********************************************************************/
void
gnc_query_sort_order ( GNCQueryView *qview, gint column, GtkSortType order)
{
    GtkTreeSortable *sortable;
    gint sortcol;

    g_return_if_fail (qview != NULL);
    g_return_if_fail (GNC_IS_QUERY_VIEW (qview));

    sortable = GTK_TREE_SORTABLE (gtk_tree_view_get_model (GTK_TREE_VIEW (qview)));

    if((column > qview->num_columns) || (column == 0) )
        sortcol = 1;
    else
        sortcol = column;

    gtk_tree_sortable_set_sort_column_id (sortable, sortcol, order);
}


static void
gnc_query_sort_cb (GtkTreeSortable *sortable, gpointer user_data)
{
    GNCQueryView *qview = GNC_QUERY_VIEW (user_data);
    GtkSortType   type;
    gint          sortcol;
    gboolean      new_column = FALSE;

    g_return_if_fail (qview != NULL);
    g_return_if_fail (GNC_IS_QUERY_VIEW (qview));
    g_return_if_fail (qview->query != NULL);

    gtk_tree_sortable_get_sort_column_id (sortable, &sortcol, &type);

    /* We need to subtract 1 for the added pointer column in the liststore
       which is not displayed to align back to params */
    sortcol = sortcol - 1;

    if(type == GTK_SORT_ASCENDING)
        qview->increasing = TRUE;
    else
        qview->increasing = FALSE;

    /* Is this a new column or a re-click on the existing column? */
    new_column = (qview->sort_column != sortcol);

    /* Save the column */
    qview->sort_column = sortcol;

    gnc_query_view_set_query_sort (qview, new_column);
}


static void
gnc_query_view_init_view (GNCQueryView *qview)
{
    GtkTreeView         *view = GTK_TREE_VIEW (qview);
    GtkTreeSortable     *sortable;
    GtkTreeSelection    *selection;
    GtkTreeViewColumn   *col;
    GtkCellRenderer     *renderer;
    GList               *node;
    gint                 i;

    sortable = GTK_TREE_SORTABLE (gtk_tree_view_get_model (GTK_TREE_VIEW (view)));

    /* compute the number of columns and fill in the rest of the view */
    qview->num_columns = g_list_length (qview->column_params);

    for (i = 0, node = qview->column_params; node; node = node->next, i++)
    {
        const char *type;
        gfloat algn = 0;
        GNCSearchParamSimple *param = node->data;

        g_assert (GNC_IS_SEARCH_PARAM_SIMPLE (param));

        col = gtk_tree_view_column_new ();

        /* Set the column title */
        gtk_tree_view_column_set_title (col, (gchar *) ((GNCSearchParam *) param)->title);

        /* pack tree view column into tree view */
        gtk_tree_view_append_column (view, col);

        /* Get justification */
        if (((GNCSearchParam *) param)->justify == GTK_JUSTIFY_CENTER)
            algn = 0.5;
        else if (((GNCSearchParam *) param)->justify == GTK_JUSTIFY_RIGHT)
            algn = 1.0;

        /* Set column resizeable */
        if (((GNCSearchParam *) param)->non_resizeable)
        {
            gtk_tree_view_column_set_resizable (col, FALSE);
            gtk_tree_view_column_set_expand (col, FALSE);
        }
        else
            gtk_tree_view_column_set_resizable (col, TRUE);

        /* Set column clickable */
        if (((GNCSearchParam *) param)->passive)
            gtk_tree_view_column_set_clickable (col, FALSE);
        else
	{
            gtk_tree_view_column_set_clickable (col, TRUE);
            /* Add sortable columns */
            gtk_tree_view_column_set_sort_column_id (col, i+1);
            gtk_tree_sortable_set_sort_func (sortable, i+1, sort_iter_compare_func,
                                    GINT_TO_POINTER (i+1), NULL);
	}

        type = gnc_search_param_get_param_type (((GNCSearchParam *) param));

        if (g_strcmp0 (type, QOF_TYPE_BOOLEAN) == 0)
        {
            renderer = gtk_cell_renderer_toggle_new ();

            /* pack cell renderer toggle into tree view column */
            gtk_tree_view_column_pack_start (col, renderer, TRUE);
            gtk_tree_view_column_add_attribute (col, renderer, "active", i+1);
            g_object_set (renderer, "xalign", algn, NULL );
            g_object_set_data (G_OBJECT (renderer), "column", GINT_TO_POINTER (i+1) );
            g_signal_connect (renderer, "toggled", G_CALLBACK (gnc_query_view_toggled_cb), view);
        }
        else
        {
            renderer = gtk_cell_renderer_text_new ();

            /* pack cell renderer text into tree view column */
            gtk_tree_view_column_pack_start (col, renderer, TRUE);
            gtk_tree_view_column_add_attribute (col, renderer, "text", i+1);
            g_object_set (renderer, "xalign", algn, NULL );
            g_object_set_data (G_OBJECT (renderer), "column", GINT_TO_POINTER (i+1) );
        }
    }

    /* set initial sort order */
    gtk_tree_sortable_set_default_sort_func (sortable, NULL, NULL, NULL);
    gtk_tree_sortable_set_sort_column_id (sortable, 1, GTK_SORT_DESCENDING);

    g_signal_connect (sortable, "sort-column-changed",
                      G_CALLBACK (gnc_query_sort_cb),
                      view);

    selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (view));
    g_signal_connect (selection, "changed",
                      G_CALLBACK (gnc_query_view_select_row_cb),
                      NULL);

    g_signal_connect (view, "row-activated",
                      G_CALLBACK (gnc_query_view_double_click_cb),
                      NULL);
}


static void
gnc_query_view_class_init (GNCQueryViewClass *klass)
{
    GtkObjectClass       *object_class;
    GtkTreeViewClass     *view_class;

    object_class = (GtkObjectClass*) klass;
    view_class =   (GtkTreeViewClass*) klass;

    parent_class = g_type_class_peek (GTK_TYPE_TREE_VIEW);

    g_type_class_add_private (klass, sizeof(GNCQueryViewPriv));

    query_view_signals[COLUMN_TOGGLED] =
        g_signal_new("column_toggled",
                     G_TYPE_FROM_CLASS (object_class),
                     G_SIGNAL_RUN_FIRST,
                     G_STRUCT_OFFSET (GNCQueryViewClass, column_toggled),
                     NULL, NULL,
                     g_cclosure_marshal_VOID__POINTER,
                     G_TYPE_NONE,
                     1,
                     G_TYPE_POINTER);

    query_view_signals[ROW_SELECTED] =
        g_signal_new("row_selected",
                     G_TYPE_FROM_CLASS (object_class),
                     G_SIGNAL_RUN_FIRST,
                     G_STRUCT_OFFSET (GNCQueryViewClass, row_selected),
                     NULL, NULL,
                     g_cclosure_marshal_VOID__POINTER,
                     G_TYPE_NONE,
                     1,
                     G_TYPE_POINTER);

    query_view_signals[DOUBLE_CLICK_ENTRY] =
        g_signal_new("double_click_entry",
                     G_TYPE_FROM_CLASS (object_class),
                     G_SIGNAL_RUN_FIRST,
                     G_STRUCT_OFFSET (GNCQueryViewClass, double_click_entry),
                     NULL, NULL,
                     g_cclosure_marshal_VOID__POINTER,
                     G_TYPE_NONE,
                     1,
                     G_TYPE_POINTER);

    object_class->destroy = gnc_query_view_destroy;

    klass->column_toggled = NULL;
    klass->row_selected = NULL;
    klass->double_click_entry = NULL;
}


static void
gnc_query_view_select_row_cb (GtkTreeSelection *selection, gpointer user_data)
{
    GNCQueryView   *qview = GNC_QUERY_VIEW (gtk_tree_selection_get_tree_view (selection));
    GtkTreeModel   *model;
    GtkTreeIter     iter;
    gint            number_of_rows;
    gpointer        entry = NULL;
    GList          *node;
    GList          *list_of_rows;

    qview->selected_entry_list = NULL;
    qview->selected_entry = NULL;

    model =  gtk_tree_view_get_model (GTK_TREE_VIEW (qview));
    list_of_rows = gtk_tree_selection_get_selected_rows (selection, &model);
    number_of_rows = gtk_tree_selection_count_selected_rows (selection);

    /* We get a list of TreePaths */
    for(node = list_of_rows; node; node = node->next)
    {
        GtkTreeIter iter;
        if(gtk_tree_model_get_iter(model, &iter, node->data))
        {
            /* now iter is a valid row iterator */
            gtk_tree_model_get (model, &iter, 0, &entry, -1);
            if(number_of_rows == 1)
            {
                qview->selected_entry = entry;
                qview->selected_entry_list = g_list_prepend(qview->selected_entry_list, entry);
            }
            else
            {
                qview->selected_entry = NULL;
                qview->selected_entry_list = g_list_prepend(qview->selected_entry_list, entry);
            }
        }
        gtk_tree_path_free(node->data);
    }
    g_list_free(list_of_rows);

    g_signal_emit (qview, query_view_signals[ROW_SELECTED], 0, GINT_TO_POINTER(number_of_rows));
}


static void
gnc_query_view_double_click_cb (GtkTreeView       *view,
                                GtkTreePath       *path,
                                GtkTreeViewColumn *column,
                                gpointer           user_data)
{
    GNCQueryView     *qview = GNC_QUERY_VIEW(view);
    GtkTreeModel     *model;
    GtkTreeIter       iter;
    gpointer          entry = NULL;

    model = gtk_tree_view_get_model (GTK_TREE_VIEW (view));

    if (gtk_tree_model_get_iter (model, &iter, path)) 
        gtk_tree_model_get (model, &iter, 0, &entry, -1);

    qview->selected_entry = entry;
    qview->selected_entry_list = NULL;

    g_signal_emit (qview, query_view_signals[DOUBLE_CLICK_ENTRY], 0, entry);
}


static void
gnc_query_view_toggled_cb (GtkCellRendererToggle *cell_renderer,
                           gchar                 *path,
                           gpointer               user_data)
{
    GNCQueryView     *qview = GNC_QUERY_VIEW (user_data);
    GtkTreeModel     *model;
    GtkTreeIter       iter;
    GtkTreePath      *treepath;
    gint             *indices;
    gpointer          entry = NULL;
    gboolean          toggled;
    gint              column;

    model = gtk_tree_view_get_model (GTK_TREE_VIEW (qview));

    column = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (cell_renderer),"column"));

    toggled = gtk_cell_renderer_toggle_get_active (cell_renderer);

    treepath = gtk_tree_path_new_from_string (path);

    if (gtk_tree_model_get_iter(model, &iter, treepath)) 
    {
        gtk_tree_model_get (model, &iter, 0, &entry, -1);
        indices = gtk_tree_path_get_indices (treepath);
        qview->toggled_row = indices[0];
        qview->toggled_column = column;
        qview->selected_entry = entry;

        if(toggled)
            g_signal_emit (qview, query_view_signals[COLUMN_TOGGLED], 0, GINT_TO_POINTER(0));
        else
            g_signal_emit (qview, query_view_signals[COLUMN_TOGGLED], 0, GINT_TO_POINTER(1));
    }
    qview->selected_entry = entry;
}


static void
gnc_query_view_destroy (GtkObject *object)
{
    GNCQueryView     *qview = GNC_QUERY_VIEW (object);
    GNCQueryViewPriv *priv;

    priv = GNC_QUERY_VIEW_GET_PRIVATE (qview);
    if (priv->component_id > 0)
    {
        gnc_unregister_gui_component (priv->component_id);
        priv->component_id = 0;
    }
    /* Free the selected entry list */
    if (qview->selected_entry_list)
    {
        g_list_free(qview->selected_entry_list);
        qview->selected_entry_list = NULL;
    }
    /* Remove the query */
    if (qview->query)
    {
        qof_query_destroy (qview->query);
        qview->query = NULL;
    }
    if (GTK_OBJECT_CLASS (parent_class)->destroy)
        GTK_OBJECT_CLASS (parent_class)->destroy (object);
}


gint
gnc_query_view_get_num_entries (GNCQueryView *qview)
{
    g_return_val_if_fail (qview != NULL, 0);
    g_return_val_if_fail (GNC_IS_QUERY_VIEW (qview), 0);

    return qview->num_entries;
}


gpointer
gnc_query_view_get_selected_entry (GNCQueryView *qview)
{
    g_return_val_if_fail (qview != NULL, NULL);
    g_return_val_if_fail (GNC_IS_QUERY_VIEW (qview), NULL);

    return qview->selected_entry;
}


GList *
gnc_query_view_get_selected_entry_list (GNCQueryView *qview)
{
    g_return_val_if_fail (qview != NULL, NULL);
    g_return_val_if_fail (GNC_IS_QUERY_VIEW (qview), NULL);

    return qview->selected_entry_list;
}


static void
gnc_query_view_refresh_selected (GNCQueryView *qview, GList *old_entry)
{
    GtkTreeModel     *model;
    GtkTreeIter       iter;
    GtkTreeSelection *selection;
    GList            *node;
    gboolean          valid;

    g_return_if_fail (qview != NULL);
    g_return_if_fail (GNC_IS_QUERY_VIEW (qview));

    model = gtk_tree_view_get_model (GTK_TREE_VIEW (qview));
    selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (qview));

    if(g_list_length (old_entry) > 0)
    {
        /* Walk the list of old entries */
        for(node = old_entry; node; node = node->next)
        {
            gpointer pointer;

            valid = gtk_tree_model_get_iter_first (model, &iter);

            while (valid)
            {
                // Walk through the liststore, reading each row
                gtk_tree_model_get (model, &iter, 0, &pointer, -1);

                if(pointer == node->data)
                {
                    gtk_tree_selection_select_iter (selection, &iter);
                    break;
                }
                valid = gtk_tree_model_iter_next (model, &iter);
            }
        }
    }
}


/********************************************************************\
 * gnc_query_view_refresh                                           *
 *   refreshes the view                                             *
 *                                                                  *
 * Args: qview - view to refresh                                    *
 * Returns: nothing                                                 *
\********************************************************************/
void
gnc_query_view_refresh (GNCQueryView *qview)
{
    GtkTreeModel     *model;
    GtkTreeIter       iter;
    GtkTreeSelection *selection;
    GList            *old_entry;

    g_return_if_fail (qview != NULL);
    g_return_if_fail (GNC_IS_QUERY_VIEW (qview));

    old_entry = qview->selected_entry_list;
    model = gtk_tree_view_get_model (GTK_TREE_VIEW (qview));
    gtk_list_store_clear (GTK_LIST_STORE (model));

    qview->num_entries = 0;
    qview->selected_entry = NULL;
    qview->selected_entry_list = NULL;

    gnc_query_view_fill (qview);

    gnc_query_view_refresh_selected (qview, old_entry);

    g_list_free(old_entry);
}


/********************************************************************\
 * gnc_query_view_set_query_sort                                    *
 *   sets the sorting order of entries in the view                  *
 *                                                                  *
 * Args: qview      - view to change the sort order for             *
 *	 new_column - is this a new column (so should we set the    *
 *                    query sort order or just set the 'increasing' *
 * Returns: nothing                                                 *
\********************************************************************/
static void
gnc_query_view_set_query_sort (GNCQueryView *qview, gboolean new_column)
{
    gboolean        sort_order = qview->increasing;
    GList          *node;
    GNCSearchParamSimple *param;

    /* Find the column parameter definition */
    node = g_list_nth (qview->column_params, qview->sort_column);
    param = node->data;
    g_assert (GNC_IS_SEARCH_PARAM_SIMPLE (param));

    /* If we're asked to invert numerics, and if this is a numeric or
     * debred column, then invert the sort order.
     */
    if (qview->numeric_inv_sort)
    {
        const char *type = gnc_search_param_get_param_type ((GNCSearchParam *) param);
        if (!g_strcmp0(type, QOF_TYPE_NUMERIC) ||
                !g_strcmp0(type, QOF_TYPE_DEBCRED))
            sort_order = !sort_order;
    }

    /* Set the sort order for the engine, if the key changed */
    if (new_column)
    {
        GSList *p1, *p2;

        p1 = gnc_search_param_get_param_path (param);
        p2 = g_slist_prepend (NULL, QUERY_DEFAULT_SORT);
        qof_query_set_sort_order (qview->query, p1, p2, NULL);
    }

    qof_query_set_sort_increasing (qview->query,
                                   sort_order,
                                   sort_order,
                                   sort_order);

    gnc_query_view_refresh (qview);
}


/********************************************************************\
 * gnc_query_view_fill                                              *
 *   Add all items to the list store                                *
 *                                                                  *
 * Args: qview - view to add item to                                *
 * Returns: nothing                                                 *
\********************************************************************/
static void
gnc_query_view_fill (GNCQueryView *qview)
{
    GNCQueryViewPriv *priv;
    GtkTreeModel     *model;
    GtkTreeIter       iter;
    GList            *entries, *item;
    const             GncGUID *guid;
    gint i;

    /* Clear all watches */
    priv = GNC_QUERY_VIEW_GET_PRIVATE (qview);
    gnc_gui_component_clear_watches (priv->component_id);

    entries = qof_query_run (qview->query);

    model = gtk_tree_view_get_model (GTK_TREE_VIEW (qview));

    for (item = entries; item; item = item->next)
    {
        GList *node;
        gint row = 0;
        const QofParam *gup;
        QofParam *qp = NULL;

        /* Add a row to the list store */
        gtk_list_store_append (GTK_LIST_STORE (model), &iter);
        /* Add a pointer to the data in the first column of the list store */
        gtk_list_store_set (GTK_LIST_STORE (model), &iter, 0, item->data, -1);

        for (i = 0, node = qview->column_params; node; node = node->next)
        {
            gboolean result;
            GNCSearchParamSimple *param = node->data;
            GSList *converters = NULL;
            const char *type = gnc_search_param_get_param_type ((GNCSearchParam *) param);
            gpointer res = item->data;
            gchar *qofstring;

            g_assert (GNC_IS_SEARCH_PARAM_SIMPLE (param));
            converters = gnc_search_param_get_converters (param);

            /* Test for boolean type */
            if (g_strcmp0 (type, QOF_TYPE_BOOLEAN) == 0)
            {
                result = (gboolean) GPOINTER_TO_INT (gnc_search_param_compute_value (param, res));
                gtk_list_store_set (GTK_LIST_STORE (model), &iter, i + 1, result, -1);
                i++;
                continue;
            }

            /* Do all the object conversions */
            for (; converters; converters = converters->next)
            {
                qp = converters->data;
                if (converters->next)
                    res = (qp->param_getfcn)(res, qp);
            }

            /* Now convert this to a text value for the row */
            if ( g_strcmp0(type, QOF_TYPE_DEBCRED) == 0 || g_strcmp0(type, QOF_TYPE_NUMERIC) == 0 )
            {

                gnc_numeric (*nfcn)(gpointer, QofParam *) =
                    (gnc_numeric(*)(gpointer, QofParam *))(qp->param_getfcn);
                gnc_numeric value = nfcn(res, qp);

                if (qview->numeric_abs)
                    value = gnc_numeric_abs (value);
                gtk_list_store_set (GTK_LIST_STORE (model), &iter, i + 1, xaccPrintAmount (value, gnc_default_print_info (FALSE)), -1);
            }
            else
            {
                qofstring = qof_query_core_to_string (type, res, qp);
                gtk_list_store_set (GTK_LIST_STORE (model), &iter, i + 1, qofstring , -1);
                g_free(qofstring);
            }
            i++;
        }
        row++;
        /* and set a watcher on this item */
        gup = priv->get_guid;
        guid = (const GncGUID*)((gup->param_getfcn)(item->data, gup));
        gnc_gui_component_watch_entity (priv->component_id, guid,
                                        QOF_EVENT_MODIFY | QOF_EVENT_DESTROY);

        qview->num_entries++;
    }
}


/********************************************************************\
 * gnc_query_view_unselect_all                                      *
 *   unselect all items in the view                                 *
 *                                                                  *
 * Args: qview - view to unselect all                               *
 * Returns: nothing                                                 *
\********************************************************************/
void
gnc_query_view_unselect_all (GNCQueryView *qview)
{
    GtkTreeSelection *selection;

    g_return_if_fail (qview != NULL);
    g_return_if_fail (GNC_IS_QUERY_VIEW (qview));

    selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (qview));
    gtk_tree_selection_unselect_all (selection);

    qview->selected_entry = NULL;
    qview->selected_entry_list = NULL;
}


gboolean gnc_query_view_item_in_view (GNCQueryView *qview, gpointer item)
{
    GtkTreeModel *model;
    GtkTreeIter   iter;
    gboolean      valid;
    gpointer      pointer;

    g_return_val_if_fail (qview, FALSE);
    g_return_val_if_fail (item, FALSE);
    g_return_val_if_fail (GNC_IS_QUERY_VIEW (qview), FALSE);

    model = gtk_tree_view_get_model (GTK_TREE_VIEW (qview));
    valid = gtk_tree_model_get_iter_first (model, &iter);

    while (valid)
    {
        // Walk through the list, reading each row
        gtk_tree_model_get (model, &iter, 0, &pointer, -1);

        if(pointer == item)
            return TRUE;

        valid = gtk_tree_model_iter_next (model, &iter);
    }
    return FALSE;
}


void
gnc_query_view_set_numerics (GNCQueryView *qview, gboolean abs, gboolean inv_sort)
{
    g_return_if_fail (qview);
    g_return_if_fail (GNC_IS_QUERY_VIEW (qview));

    qview->numeric_abs = abs;
    qview->numeric_inv_sort = inv_sort;
}
