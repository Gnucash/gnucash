/********************************************************************\
 * gnucash-item-list.c -- A scrollable list box                     *
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

/*
 *  A scrollable list box.
 */

#include "config.h"

#include <gnome.h>

#include "gnc-engine-util.h"
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

void
gnc_item_list_clear (GncItemList *item_list)
{
	g_return_if_fail (IS_GNC_ITEM_LIST (item_list));
	g_return_if_fail (item_list->list_store != NULL);

	gtk_list_store_clear (item_list->list_store);
}


void
gnc_item_list_append (GncItemList *item_list, char *string)
{
	GtkTreeIter iter;

	g_return_if_fail (IS_GNC_ITEM_LIST (item_list));
	g_return_if_fail (item_list->list_store != NULL);
	g_return_if_fail (string != NULL);

	gtk_list_store_append (item_list->list_store, &iter);
	gtk_list_store_set (item_list->list_store, &iter, 0, string, -1);
}


void
gnc_item_list_select (GncItemList *item_list, const char *string)
{
/*	gint row = 0;
	gchar *text;

	g_return_if_fail(item_list != NULL);
	g_return_if_fail(IS_GNC_ITEM_LIST (item_list));

	if (string == NULL) {
		gtk_clist_unselect_all (item_list->clist);
		return;
	}

	while (gtk_clist_get_text (item_list->clist, row, 0, &text)) {

		if (safe_strcmp (string, text) != 0) {
			row++;
			continue;
		}

                gtk_clist_freeze (item_list->clist);
                item_list->clist->focus_row = row;
		gtk_clist_select_row (item_list->clist, row, 0);
                gtk_clist_thaw (item_list->clist);

                gnc_item_list_show_selected (item_list);

		return;
	}*/
}


void
gnc_item_list_show_selected (GncItemList *item_list)
{
/*	GtkVisibility visibility;
        gint row;

	g_return_if_fail (item_list != NULL);
	g_return_if_fail (IS_GNC_ITEM_LIST (item_list));

        row = item_list->clist->focus_row;

        visibility = gtk_clist_row_is_visible (item_list->clist, row);

        if (visibility != GTK_VISIBILITY_FULL)
                gtk_clist_moveto (item_list->clist, row, 0, 0.5, 0.0);*/
}


void
gnc_item_list_sort (GncItemList *item_list)
{
        g_return_if_fail (item_list != NULL);
	g_return_if_fail (IS_GNC_ITEM_LIST (item_list));

/*        gtk_clist_sort (item_list->clist);*/
}


int
gnc_item_list_autosize (GncItemList *item_list)
{
        g_return_val_if_fail (item_list != NULL, 0);
        g_return_val_if_fail (IS_GNC_ITEM_LIST (item_list), 0);

        return 100; /*gtk_clist_columns_autosize (item_list->clist);*/
}


static void
gnc_item_list_init (GncItemList *item_list)
{
}


static gboolean
gnc_item_list_button_event (GtkWidget *widget, GdkEventButton *event,
			    gpointer data)
{
	/* So the sheet doesn't use it. */
	g_signal_stop_emission_by_name (G_OBJECT(widget), "button_press_event");

	return TRUE;
}

static gboolean
gnc_item_list_key_event (GtkWidget *widget, GdkEventKey *event, gpointer data)
{
	GncItemList *item_list = GNC_ITEM_LIST (data);
/*        GtkCList *clist;
        gboolean got_text;
        gchar *string;
        gint row;*/

        switch (event->keyval) {
                case GDK_Return:
/*                        clist = item_list->clist;
                        row = clist->focus_row;
                        if (row < 0)
                                return FALSE;

                        got_text = gtk_clist_get_text (clist, row, 0, &string);

                        if (!got_text)
                                return FALSE;

                        g_signal_emit (G_OBJECT (item_list),
                                       gnc_item_list_signals[ACTIVATE_ITEM], 0, string);
                        return TRUE;*/
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
			      g_cclosure_marshal_VOID__POINTER,
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

	gtk_tree_selection_get_selected (selection, &model, &iter);

	gtk_tree_model_get (model, &iter, 0, &string, -1);

	g_signal_emit (G_OBJECT (item_list), gnc_item_list_signals[SELECT_ITEM], 0, string);

	g_free (string);
}

GnomeCanvasItem *
gnc_item_list_new (GnomeCanvasGroup *parent)
{
        GtkWidget *frame;
	GtkWidget *tree_view;
        GtkWidget *scrollwin;
	GtkListStore *list_store;
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

	list_store = gtk_list_store_new (1, G_TYPE_STRING);
	tree_view = gtk_tree_view_new_with_model (GTK_TREE_MODEL (list_store));
	gtk_tree_view_set_headers_visible (GTK_TREE_VIEW (tree_view), FALSE);
	gtk_tree_selection_set_mode (gtk_tree_view_get_selection (GTK_TREE_VIEW (tree_view)),
				     GTK_SELECTION_BROWSE);

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

	g_signal_connect_after (G_OBJECT(frame), "button_press_event",
				G_CALLBACK (gnc_item_list_button_event), item_list);

	g_signal_connect (G_OBJECT (list_store), "key_press_event",
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
