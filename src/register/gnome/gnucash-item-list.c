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

#include <gnome.h>

#include "gnc-engine-util.h"
#include "gnucash-item-list.h"
#include "gnucash-scrolled-window.h"


/* Item list signals */
enum
{
  SELECT_ITEM,
  CHANGE_ITEM,
  ACTIVATE_ITEM,
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
gnc_item_list_select(GNCItemList *item_list, const char *string)
{
	gint row = 0;
	gchar *text;

	g_return_if_fail(item_list != NULL);
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

                gtk_clist_freeze(item_list->clist);
                item_list->clist->focus_row = row;
		gtk_clist_select_row(item_list->clist, row, 0);
                gtk_clist_thaw(item_list->clist);

                gnc_item_list_show_selected(item_list);

		return;
	}
}


void
gnc_item_list_show_selected(GNCItemList *item_list)
{
	GtkVisibility visibility;
        gint row;

	g_return_if_fail(item_list != NULL);
	g_return_if_fail(IS_GNC_ITEM_LIST(item_list));

        row = item_list->clist->focus_row;

        visibility = gtk_clist_row_is_visible(item_list->clist, row);

        if (visibility != GTK_VISIBILITY_FULL)
                gtk_clist_moveto(item_list->clist, row, 0, 0.5, 0.0);
}


void
gnc_item_list_sort(GNCItemList *item_list)
{
        g_return_if_fail(item_list != NULL);
	g_return_if_fail(IS_GNC_ITEM_LIST(item_list));

        gtk_clist_sort(item_list->clist);
}


int
gnc_item_list_autosize(GNCItemList *item_list)
{
        g_return_val_if_fail(item_list != NULL, 0);
        g_return_val_if_fail(IS_GNC_ITEM_LIST(item_list), 0);

        return gtk_clist_columns_autosize(item_list->clist);
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
gnc_clist_button_event(GtkWidget *widget, GdkEventButton *event, gpointer data)
{
        GtkAdjustment *vadj;
        gfloat multiplier = 1.0;
        gfloat v_value;

        vadj = gtk_clist_get_vadjustment(GTK_CLIST(widget));
        v_value = vadj->value;
        if (event->state & GDK_SHIFT_MASK)
                multiplier = 5.0;

        switch (event->button)
        {
                case 4:
                        v_value -= vadj->step_increment * multiplier;
                        break;
                case 5:
                        v_value += vadj->step_increment * multiplier;
                        break;
                default:
                        return FALSE;
        }

        v_value = CLAMP(v_value, vadj->lower, vadj->upper - vadj->page_size);

        gtk_adjustment_set_value(vadj, v_value);

	return FALSE;
}


static gboolean
gnc_item_list_key_event(GtkWidget *widget, GdkEventKey *event, gpointer data)
{
	GNCItemList *item_list = GNC_ITEM_LIST(data);
        GtkCList *clist;
        gboolean got_text;
        gchar *string;
        gint row;

        switch (event->keyval) {
                case GDK_Return:
                        clist = item_list->clist;
                        row = clist->focus_row;
                        if (row < 0)
                                return FALSE;

                        got_text = gtk_clist_get_text(clist, row, 0, &string);

                        if (!got_text)
                                return FALSE;

                        gtk_signal_emit(GTK_OBJECT(item_list),
                                        gnc_item_list_signals[ACTIVATE_ITEM],
                                        string);

                        return TRUE;
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
		gtk_type_class (gnome_canvas_widget_get_type ());

        object_class = GTK_OBJECT_CLASS(item_list_class);

	gnc_item_list_signals[SELECT_ITEM] =
		gtk_signal_new("select_item",
			       GTK_RUN_LAST,
			       object_class->type,
			       GTK_SIGNAL_OFFSET(GNCItemListClass,
						 select_item),
			       gtk_marshal_NONE__POINTER,
			       GTK_TYPE_NONE, 1,
			       GTK_TYPE_POINTER);

	gnc_item_list_signals[CHANGE_ITEM] =
		gtk_signal_new("change_item",
			       GTK_RUN_LAST,
			       object_class->type,
			       GTK_SIGNAL_OFFSET(GNCItemListClass,
						 change_item),
			       gtk_marshal_NONE__POINTER,
			       GTK_TYPE_NONE, 1,
			       GTK_TYPE_POINTER);

	gnc_item_list_signals[ACTIVATE_ITEM] =
		gtk_signal_new("activate_item",
			       GTK_RUN_LAST,
			       object_class->type,
			       GTK_SIGNAL_OFFSET(GNCItemListClass,
						 activate_item),
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

        item_list_class->select_item = NULL;
        item_list_class->change_item = NULL;
        item_list_class->activate_item = NULL;
        item_list_class->key_press_event = NULL;
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

	got_text = gtk_clist_get_text(clist, row, 0, &string);

	if (!got_text)
		return;

        if (column < 0)
                gtk_signal_emit(GTK_OBJECT(item_list),
                                gnc_item_list_signals[CHANGE_ITEM], string);
        else
                gtk_signal_emit(GTK_OBJECT(item_list),
                                gnc_item_list_signals[SELECT_ITEM], string);
}


GnomeCanvasItem *
gnc_item_list_new(GnomeCanvasGroup *parent)
{
        GtkWidget *frame;
	GtkWidget *clist;
        GtkWidget *scrollwin;
        GnomeCanvasItem *item;
        GNCItemList *item_list;

        frame = gtk_frame_new (NULL);

        scrollwin = gnc_scrolled_window_new ();
        gtk_container_add (GTK_CONTAINER (frame), scrollwin);

        gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrollwin),
                                        GTK_POLICY_AUTOMATIC,
                                        GTK_POLICY_AUTOMATIC);

	clist = gtk_clist_new (1);
        gtk_container_add (GTK_CONTAINER (scrollwin), clist);
        gtk_clist_set_selection_mode(GTK_CLIST(clist), GTK_SELECTION_BROWSE);

        gtk_widget_show_all (frame);

        item = gnome_canvas_item_new (parent, gnc_item_list_get_type(),
                                      "widget", frame,
                                      "size_pixels", TRUE,
                                      "x", -10000.0,
                                      "y", -10000.0,
                                      NULL);

        item_list = GNC_ITEM_LIST (item);

	item_list->clist = GTK_CLIST (clist);
        item_list->frame = frame;

	gtk_signal_connect_after(GTK_OBJECT(frame), "button_press_event",
				 GTK_SIGNAL_FUNC(gnc_item_list_button_event),
				 (gpointer) item_list);

	gtk_signal_connect(GTK_OBJECT(clist), "button_press_event",
                           GTK_SIGNAL_FUNC(gnc_clist_button_event),
                           (gpointer) item_list);

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
