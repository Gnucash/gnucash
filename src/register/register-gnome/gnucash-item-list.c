/********************************************************************\
 * gnucash-item-list.c -- A scrollable list box                     *
 *                                                                  *
 * Initial copyright not recorded.                                  *
 * Copyright (c) 2006 David Hampton <hampton@employees.org>         *
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
 *                                                                  *
\********************************************************************/

/*
 *  A scrollable list box.
 */

#include "config.h"

#include <gnome.h>
#include <glib/gi18n.h>

#include "gnc-engine.h"
#include "gnucash-item-list.h"
#include "gnucash-scrolled-window.h"

/* Item list signals */
enum {
  SELECT_ITEM,
  CHANGE_ITEM,
  ACTIVATE_ITEM,
  KEY_PRESS_EVENT,
  LAST_SIGNAL
};

static GnomeCanvasWidgetClass *gnc_item_list_parent_class;
static guint gnc_item_list_signals[LAST_SIGNAL];

gboolean _gnc_item_find_selection(GtkTreeModel *model, GtkTreePath *path, GtkTreeIter *iter, gpointer data);

gint
gnc_item_list_num_entries (GncItemList *item_list)
{
        GtkTreeModel *model;

        g_return_val_if_fail(item_list != NULL, 0);
        g_return_val_if_fail(IS_GNC_ITEM_LIST(item_list), 0);

        model = GTK_TREE_MODEL(item_list->list_store);
        return gtk_tree_model_iter_n_children(model, NULL);
}


void
gnc_item_list_clear (GncItemList *item_list)
{
	GtkTreeSelection* selection;

	g_return_if_fail(IS_GNC_ITEM_LIST(item_list));
	g_return_if_fail(item_list->list_store != NULL);

	selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (item_list->tree_view));

        g_signal_handlers_block_matched (G_OBJECT (selection), G_SIGNAL_MATCH_DATA,
					 0, 0, NULL, NULL, item_list);
	gtk_list_store_clear (item_list->list_store);
        g_signal_handlers_unblock_matched (G_OBJECT (selection), G_SIGNAL_MATCH_DATA,
					 0, 0, NULL, NULL, item_list);
}


void
gnc_item_list_append (GncItemList *item_list, const char *string)
{
	GtkTreeIter iter;

	g_return_if_fail(IS_GNC_ITEM_LIST(item_list));
	g_return_if_fail(item_list->list_store != NULL);
	g_return_if_fail(string != NULL);

	gtk_list_store_append (item_list->list_store, &iter);
	gtk_list_store_set (item_list->list_store, &iter, 0, string, -1);
}


void
gnc_item_list_set_sort_enabled(GncItemList *item_list, gboolean enabled)
{
	if (enabled) {
		gtk_tree_sortable_set_sort_column_id
			(GTK_TREE_SORTABLE (item_list->list_store),
			 0,
			 GTK_SORT_ASCENDING);
	} else {
		gtk_tree_sortable_set_sort_column_id
			(GTK_TREE_SORTABLE (item_list->list_store),
			 GTK_TREE_SORTABLE_UNSORTED_SORT_COLUMN_ID,
			 GTK_SORT_ASCENDING);
	}
}


typedef struct _findSelectionData
{
        GncItemList *item_list;
        const char *string_to_find;
        GtkTreePath *found_path;
} FindSelectionData;

gboolean
_gnc_item_find_selection(GtkTreeModel *model, GtkTreePath *path, GtkTreeIter *iter, gpointer data)
{
        FindSelectionData *to_find = (FindSelectionData*)data;
        gchar *iterStr;
        gboolean found;

        gtk_tree_model_get(model, iter, 0, &iterStr, -1);
        found = safe_strcmp(to_find->string_to_find, iterStr) == 0;
        g_free(iterStr);
        if (found)
        {
                to_find->found_path = gtk_tree_path_copy(path);
                return TRUE;
        }
        return FALSE;
}

gboolean
gnc_item_in_list (GncItemList *item_list, const char *string)
{
        FindSelectionData *to_find_data;
	gboolean result;

        g_return_val_if_fail(item_list != NULL, FALSE);
        g_return_val_if_fail(IS_GNC_ITEM_LIST(item_list), FALSE);

        to_find_data = (FindSelectionData*)g_new0(FindSelectionData, 1);
        to_find_data->item_list = item_list;
        to_find_data->string_to_find = string;

        gtk_tree_model_foreach(GTK_TREE_MODEL(item_list->list_store),
                               _gnc_item_find_selection,
                               to_find_data);

        result = (to_find_data->found_path != NULL);
        g_free(to_find_data);
	return result;
}


void
gnc_item_list_select (GncItemList *item_list, const char *string)
{
        GtkTreeSelection *tree_sel = NULL;
        FindSelectionData *to_find_data;

        g_return_if_fail(item_list != NULL);
        g_return_if_fail(IS_GNC_ITEM_LIST(item_list));

        tree_sel = gtk_tree_view_get_selection(item_list->tree_view);

        if (string == NULL)
        {
                gtk_tree_selection_unselect_all(tree_sel);
                return;
        }

        to_find_data = (FindSelectionData*)g_new0(FindSelectionData, 1);
        to_find_data->item_list = item_list;
        to_find_data->string_to_find = string;

        gtk_tree_model_foreach(GTK_TREE_MODEL(item_list->list_store),
                               _gnc_item_find_selection,
                               to_find_data);

        if (to_find_data->found_path != NULL)
        {
                gtk_tree_view_set_cursor(item_list->tree_view, to_find_data->found_path, NULL, FALSE);
                gtk_tree_path_free(to_find_data->found_path);

                gnc_item_list_show_selected(item_list);
        }

        g_free(to_find_data);
}


void
gnc_item_list_show_selected (GncItemList *item_list)
{
        GtkTreeSelection *selection;
        GtkTreeIter iter;
        GtkTreePath *path;
        GtkTreeModel *model;

        g_return_if_fail(item_list != NULL);
        g_return_if_fail(IS_GNC_ITEM_LIST(item_list));

        selection = gtk_tree_view_get_selection (item_list->tree_view);

        if (gtk_tree_selection_get_selected (selection, &model, &iter))
        {
                path = gtk_tree_model_get_path (model, &iter);

                gtk_tree_view_scroll_to_cell (item_list->tree_view, 
                                              path, NULL, TRUE, 0.5, 0.0); 
        }
}

int
gnc_item_list_autosize (GncItemList *item_list)
{
        g_return_val_if_fail(item_list != NULL, 0);
        g_return_val_if_fail(IS_GNC_ITEM_LIST(item_list), 0);

#if 0
        return gtk_clist_columns_autosize (item_list->clist);
#endif
        return 100;
}


static void
gnc_item_list_init (GncItemList *item_list)
{
        item_list->tree_view = NULL;
        item_list->list_store = NULL;
        item_list->frame = NULL;
}


static gboolean
gnc_item_list_button_event(GtkWidget *widget, GdkEventButton *event,
			   gpointer data)
{
        GncItemList *item_list;
        GtkTreeIter iter;
        GtkTreePath *path;
        GtkTreeModel *model;
        gchar *string;
	gboolean success;

        g_return_val_if_fail(IS_GNC_ITEM_LIST (data), FALSE);

        item_list = GNC_ITEM_LIST (data);

	switch (event->button)
        {
                case 1:
                        if(!gtk_tree_view_get_path_at_pos (item_list->tree_view,
                                                           event->x,
                                                           event->y,
                                                           &path,
                                                           NULL,
                                                           NULL,
                                                           NULL))
                        {
                                return FALSE;
                        }

                        gtk_tree_view_set_cursor (item_list->tree_view, path, NULL, FALSE);

                        model = GTK_TREE_MODEL (item_list->list_store);
                        success = gtk_tree_model_get_iter (model, &iter, path);

                        gtk_tree_path_free (path);

			if (!success)
				return FALSE;

			gtk_tree_model_get (model, &iter, 0, &string, -1);

	                g_signal_emit (G_OBJECT (item_list), 
                                       gnc_item_list_signals[ACTIVATE_ITEM],
                                       0, 
                                       string);
                        g_free(string);
                        return TRUE;
                default:
                        return FALSE;
        }

	return FALSE;
}

static gboolean
gnc_item_list_key_event (GtkWidget *widget, GdkEventKey *event, gpointer data)
{
	GncItemList *item_list = GNC_ITEM_LIST (data);
        GtkTreeSelection *selection = NULL;
        GtkTreeIter iter;
        GtkTreeModel *model;
        gchar *string;

        switch (event->keyval) {
                case GDK_Return:
                        selection = gtk_tree_view_get_selection (item_list->tree_view);
                        if (!gtk_tree_selection_get_selected (selection, &model, &iter))
                                return FALSE;

	                gtk_tree_model_get (model, &iter, 0, &string, -1);

	                g_signal_emit (G_OBJECT (item_list), 
                                       gnc_item_list_signals[ACTIVATE_ITEM], 
                                       0, 
                                       string);
                        g_free(string);
                        return TRUE;

		case GDK_Page_Up:
		case GDK_Page_Down:
		case GDK_Up:
		case GDK_Down:
			/* These go to the clist */
			return FALSE;
	}

	/* These go to the sheet */
	g_signal_stop_emission_by_name (G_OBJECT (widget), "key_press_event");

	g_signal_emit (G_OBJECT (item_list),
		       gnc_item_list_signals[KEY_PRESS_EVENT], 0, event);

	return TRUE;
}


static void
gnc_item_list_class_init (GncItemListClass *item_list_class)
{
        GObjectClass  *object_class = G_OBJECT_CLASS (item_list_class);

        gnc_item_list_parent_class = g_type_class_peek_parent (item_list_class);


	gnc_item_list_signals[SELECT_ITEM] =
		g_signal_new ("select_item",
			      G_OBJECT_CLASS_TYPE (object_class),
			      G_SIGNAL_RUN_LAST,
			      G_STRUCT_OFFSET(GncItemListClass, select_item),
			      NULL, NULL,
			      g_cclosure_marshal_VOID__POINTER,
			      G_TYPE_NONE, 1,
			      G_TYPE_POINTER);

	gnc_item_list_signals[CHANGE_ITEM] =
		g_signal_new ("change_item",
			      G_OBJECT_CLASS_TYPE (object_class),
			      G_SIGNAL_RUN_LAST,
			      G_STRUCT_OFFSET(GncItemListClass, change_item),
			      NULL, NULL,
			      g_cclosure_marshal_VOID__POINTER,
			      G_TYPE_NONE, 1,
			      G_TYPE_POINTER);

	gnc_item_list_signals[ACTIVATE_ITEM] =
		g_signal_new ("activate_item",
			      G_OBJECT_CLASS_TYPE (object_class),
			      G_SIGNAL_RUN_LAST,
			      G_STRUCT_OFFSET(GncItemListClass, activate_item),
			      NULL, NULL,
			      g_cclosure_marshal_VOID__POINTER,
			      G_TYPE_NONE, 1,
			      G_TYPE_POINTER);

	gnc_item_list_signals[KEY_PRESS_EVENT] =
		g_signal_new ("key_press_event",
			      G_OBJECT_CLASS_TYPE (object_class),
			      G_SIGNAL_RUN_LAST,
			      G_STRUCT_OFFSET(GncItemListClass, key_press_event),
			      NULL, NULL,
			      g_cclosure_marshal_VOID__BOXED,
			      G_TYPE_NONE, 1,
			      GDK_TYPE_EVENT);

        item_list_class->select_item = NULL;
        item_list_class->change_item = NULL;
        item_list_class->activate_item = NULL;
        item_list_class->key_press_event = NULL;
}


GType
gnc_item_list_get_type (void)
{
        static GType gnc_item_list_type = 0;

        if (gnc_item_list_type == 0) {
                static const GTypeInfo gnc_item_list_info = {
			sizeof(GncItemListClass),
			NULL,
			NULL,
			(GClassInitFunc)  gnc_item_list_class_init,
			NULL,
			NULL,
			sizeof(GncItemList),
			0,
			(GInstanceInitFunc) gnc_item_list_init
                };

                gnc_item_list_type =
			g_type_register_static (gnome_canvas_widget_get_type(), "GncItemList",
						&gnc_item_list_info, 0);
        }

        return gnc_item_list_type;
}


static void
tree_view_selection_changed (GtkTreeSelection *selection,
			     gpointer data)
{
	GncItemList *item_list = GNC_ITEM_LIST (data);
	GtkTreeModel *model;
	GtkTreeIter iter;
	char *string;

        g_return_if_fail(data);
        g_return_if_fail(selection);

	if (!gtk_tree_selection_get_selected (selection, &model, &iter))
                return;

	gtk_tree_model_get (model, &iter, 0, &string, -1);

	g_signal_emit (G_OBJECT (item_list), gnc_item_list_signals[CHANGE_ITEM], 0, string);

	g_free (string);
}

GnomeCanvasItem *
gnc_item_list_new(GnomeCanvasGroup *parent, GtkListStore *list_store)
{
        GtkWidget *frame;
	GtkWidget *tree_view;
        GtkWidget *scrollwin;
	GtkCellRenderer *renderer;
	GtkTreeViewColumn *column;
        GnomeCanvasItem *item;
        GncItemList *item_list;

        frame = gtk_frame_new (NULL);

        scrollwin = gnc_scrolled_window_new ();
        gtk_container_add (GTK_CONTAINER (frame), scrollwin);

        gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrollwin),
                                        GTK_POLICY_AUTOMATIC,
                                        GTK_POLICY_AUTOMATIC);

	if (NULL == list_store)
		list_store = gtk_list_store_new (1, G_TYPE_STRING);
	else
		g_object_ref(list_store);
	tree_view = gtk_tree_view_new_with_model (GTK_TREE_MODEL (list_store));
	g_object_unref(list_store);
	/* Removed code to enable sorting. Enable it after the list is
	 * fully populated by calling gnc_item_list_finished_loading(). */
 
	gtk_tree_view_set_headers_visible (GTK_TREE_VIEW (tree_view), FALSE);
	gtk_tree_selection_set_mode (gtk_tree_view_get_selection (GTK_TREE_VIEW (tree_view)),
				     GTK_SELECTION_BROWSE);
	gtk_tree_sortable_set_sort_column_id(GTK_TREE_SORTABLE(list_store),
					     0, GTK_SORT_ASCENDING);

	renderer = gtk_cell_renderer_text_new ();
	column = gtk_tree_view_column_new_with_attributes (_("List"),
							  renderer,
							  "text", 0,
							  NULL);
	gtk_tree_view_append_column (GTK_TREE_VIEW (tree_view), column);

	gtk_container_add (GTK_CONTAINER (scrollwin), tree_view);
        gtk_widget_show_all (frame);

        item = gnome_canvas_item_new (parent, gnc_item_list_get_type(),
                                      "widget", frame,
                                      "size_pixels", TRUE,
                                      "x", -10000.0,
                                      "y", -10000.0,
                                      NULL);

        item_list = GNC_ITEM_LIST (item);

	item_list->tree_view = GTK_TREE_VIEW (tree_view);
	item_list->list_store = list_store;
        item_list->frame = frame;

	g_signal_connect (G_OBJECT(tree_view), "button_press_event",
				G_CALLBACK (gnc_item_list_button_event), item_list);

	g_signal_connect (G_OBJECT (tree_view), "key_press_event",
			  G_CALLBACK (gnc_item_list_key_event), item_list);

	g_signal_connect (G_OBJECT (gtk_tree_view_get_selection (GTK_TREE_VIEW (tree_view))), "changed",
			  G_CALLBACK (tree_view_selection_changed), item_list);

        return item;
}


/*
  Local Variables:
  c-basic-offset: 8
  End:
*/
