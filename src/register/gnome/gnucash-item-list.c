/********************************************************************\
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
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
\********************************************************************/

/*
 *  A scrollable list box.
 */

#include <gnome.h>

#include "gnucash-item-list.h"
#include "util.h"


/* Item list signals */
enum
{
  SELECT_ITEM,
  KEY_PRESS_EVENT,
  LAST_SIGNAL
};

/* Item list arguments */
enum
{
        ARG_0
};


static GnomeCanvasWidgetClass *gnc_item_list_parent_class;
static guint gnc_item_list_signals[LAST_SIGNAL];


void
gnc_item_list_clear(GNCItemList *item_list)
{
	g_return_if_fail(IS_GNC_ITEM_LIST(item_list));
	g_return_if_fail(item_list->clist != NULL);

	gtk_clist_clear(item_list->clist);
}


void
gnc_item_list_append(GNCItemList *item_list, char *string)
{
	char *text[2] = {NULL, NULL};

	g_return_if_fail(IS_GNC_ITEM_LIST(item_list));
	g_return_if_fail(item_list->clist != NULL);
	g_return_if_fail(string != NULL);

	text[0] = string;

	gtk_clist_append(item_list->clist, text);
}


void
gnc_item_list_select(GNCItemList *item_list, char *string)
{
	GtkVisibility visibility;
	gint row = 0;
	gchar *text;

	g_return_if_fail(IS_GNC_ITEM_LIST(item_list));

	if (string == NULL) {
		gtk_clist_unselect_all(item_list->clist);
		return;
	}

	while (gtk_clist_get_text(item_list->clist, row, 0, &text)) {

		if (safe_strcmp(string, text) != 0) {
			row++;
			continue;
		}

		gtk_clist_select_row(item_list->clist, row, 0);

		visibility = gtk_clist_row_is_visible(item_list->clist, row);
		if (visibility == GTK_VISIBILITY_NONE)
			gtk_clist_moveto(item_list->clist, row, 0, 0.5, 0.0);

		return;
	}
}


static void
item_edit_set_arg (GtkObject *object, GtkArg *arg, guint arg_id)
{
	/*        GNCItemList *item_list = GNC_ITEM_LIST(object); */

        switch (arg_id)
	{
		default:
			break;
        }
}


static void
gnc_item_list_init(GNCItemList *item_list)
{
        item_list->clist = NULL;
}


static gboolean
gnc_item_list_button_event(GtkWidget *widget, GdkEventButton *event,
			   gpointer data)
{
	/* So the sheet doesn't use it. */
	gtk_signal_emit_stop_by_name(GTK_OBJECT(widget), "button_press_event");

	return TRUE;
}


static gboolean
gnc_item_list_key_event(GtkWidget *widget, GdkEventKey *event, gpointer data)
{
	GNCItemList *item_list = GNC_ITEM_LIST(data);

        switch (event->keyval) {
		case GDK_Page_Up:
		case GDK_Page_Down:
		case GDK_Up:
		case GDK_Down:
			/* These go to the clist */
			return FALSE;
	}

	/* These go to the sheet */
	gtk_signal_emit_stop_by_name(GTK_OBJECT(widget), "key_press_event");

	gtk_signal_emit(GTK_OBJECT(item_list),
			gnc_item_list_signals[KEY_PRESS_EVENT], event);

	return TRUE;
}


static void
gnc_item_list_class_init(GNCItemListClass *item_list_class)
{
        GtkObjectClass  *object_class;

        gnc_item_list_parent_class =
		gtk_type_class(gnome_canvas_widget_get_type());

        object_class = GTK_OBJECT_CLASS(item_list_class);

        object_class->set_arg = item_edit_set_arg;

	gnc_item_list_signals[SELECT_ITEM] =
		gtk_signal_new("select_item",
			       GTK_RUN_LAST,
			       object_class->type,
			       GTK_SIGNAL_OFFSET(GNCItemListClass,
						 select_item),
			       gtk_marshal_NONE__POINTER,
			       GTK_TYPE_NONE, 1,
			       GTK_TYPE_POINTER);

	gnc_item_list_signals[KEY_PRESS_EVENT] =
		gtk_signal_new ("key_press_event",
				GTK_RUN_LAST,
				object_class->type,
				GTK_SIGNAL_OFFSET(GNCItemListClass,
						  key_press_event),
				gtk_marshal_NONE__POINTER,
				GTK_TYPE_NONE, 1,
				GTK_TYPE_GDK_EVENT);

	gtk_object_class_add_signals(object_class,
				     gnc_item_list_signals,
				     LAST_SIGNAL);
}


GtkType
gnc_item_list_get_type(void)
{
        static GtkType gnc_item_list_type = 0;

        if (gnc_item_list_type == 0)
	{
                GtkTypeInfo gnc_item_list_info =
		{
                        "GNCItemList",
                        sizeof(GNCItemList),
                        sizeof(GNCItemListClass),
                        (GtkClassInitFunc)  gnc_item_list_class_init,
                        (GtkObjectInitFunc) gnc_item_list_init,
                        NULL, /* reserved_1 */
                        NULL, /* reserved_2 */
                        (GtkClassInitFunc) NULL
                };

                gnc_item_list_type =
			gtk_type_unique(gnome_canvas_widget_get_type(),
					&gnc_item_list_info);
        }

        return gnc_item_list_type;
}


static void
clist_select_row_cb(GtkCList *clist, gint row, gint column,
		    GdkEventButton *event, gpointer data)
{
	GNCItemList *item_list = GNC_ITEM_LIST(data);
	gboolean got_text;
	char *string;

	got_text = gtk_clist_get_text(clist, row, column, &string);

	if (!got_text)
		return;

	gtk_signal_emit(GTK_OBJECT(item_list),
			gnc_item_list_signals[SELECT_ITEM], string);
}


GnomeCanvasItem *
gnc_item_list_new(GnomeCanvasGroup *parent)
{
	GtkWidget *clist, *hbox, *scrollbar;
        GnomeCanvasItem *item;
        GNCItemList *item_list;

	hbox = gtk_hbox_new(FALSE, 0);

	clist = gtk_clist_new(1);
	gtk_box_pack_start(GTK_BOX(hbox), clist, TRUE, TRUE, 0);
	gtk_clist_set_column_auto_resize(GTK_CLIST(clist), 0, TRUE);

	scrollbar = gtk_vscrollbar_new(NULL);
	gtk_box_pack_start(GTK_BOX(hbox), scrollbar, FALSE, FALSE, 0);
	gtk_clist_set_vadjustment(GTK_CLIST(clist),
				  gtk_range_get_adjustment
				  (GTK_RANGE(scrollbar)));

        item = gnome_canvas_item_new(parent, gnc_item_list_get_type(),
				     "widget", hbox,
				     "size_pixels", TRUE,
				     "x", -10000.0,                                     
				     NULL);

        item_list = GNC_ITEM_LIST(item);

	item_list->clist = GTK_CLIST(clist);

	gtk_signal_connect_after(GTK_OBJECT(hbox), "button_press_event",
				 GTK_SIGNAL_FUNC(gnc_item_list_button_event),
				 NULL);

	gtk_signal_connect(GTK_OBJECT(clist), "key_press_event",
			   GTK_SIGNAL_FUNC(gnc_item_list_key_event),
			   (gpointer) item_list);

	gtk_signal_connect(GTK_OBJECT(clist), "select_row",
			   GTK_SIGNAL_FUNC(clist_select_row_cb),
			   (gpointer) item_list);

        return item;
}


/*
  Local Variables:
  c-basic-offset: 8
  End:
*/
