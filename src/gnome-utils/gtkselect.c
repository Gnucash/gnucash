/* gtkselect.c - select widget for gtk+
 * Copyright 1997 Paolo Molaro (from gtkcombo, where this was copied from)
 * Copyright 2000 Gordon Oliver
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

/*
 * Modified by the GTK+ Team and others 1997-1999.  See the AUTHORS
 * file for a list of people on the GTK+ Team.  See the ChangeLog
 * files for a list of changes.  These files are distributed with
 * GTK+ at ftp://ftp.gtk.org/pub/gtk/. 
 */

#include <string.h>

#include "gtk/gtkarrow.h"
#include "gtk/gtklabel.h"
#include "gtk/gtklist.h"
#include "gtk/gtkeventbox.h"
#include "gtk/gtkbutton.h"
#include "gtk/gtklistitem.h"
#include "gtk/gtkscrolledwindow.h"
#include "gtk/gtkmain.h"
#include "gtk/gtksignal.h"
#include "gtk/gtkwindow.h"
#include "gdk/gdkkeysyms.h"
#include "gtkselect.h"
#include "gtk/gtkframe.h"
#include "gtk/gtkbin.h"

#define SELECT_LIST_MAX_HEIGHT	(400)
#define	EMPTY_LIST_HEIGHT	(15)

static void         gtk_select_class_init      (GtkSelectClass *klass);
static void         gtk_select_init            (GtkSelect      *select);
static void         gtk_select_destroy         (GtkObject     *select);
static void         gtk_select_get_pos         (GtkSelect      *select, 
                                               gint          *x, 
                                               gint          *y, 
                                               gint          *height, 
                                               gint          *width);
static void         gtk_select_popup_list      (GtkSelect      *select);
static void         gtk_select_popup_button_press (GtkWidget        *button,
						  GdkEventButton   *event,
						  GtkSelect         *select);
static void         gtk_select_popup_button_leave (GtkWidget        *button,
						  GdkEventCrossing *event,
						  GtkSelect         *select);
static void         gtk_select_update_entry    (GtkList       *list, 
                                               GtkSelect      *select);
static gint         gtk_select_button_press    (GtkWidget     *widget,
				               GdkEvent      *event,
				               GtkSelect      *select);
static gint         gtk_select_button_release  (GtkWidget     *widget,
				               GdkEvent      *event,
				               GtkSelect      *select);
static gint         gtk_select_list_enter      (GtkWidget        *widget,
				               GdkEventCrossing *event,
				               GtkSelect         *select);
static gint         gtk_select_list_key_press  (GtkWidget     *widget, 
                                               GdkEventKey   *event, 
                                               GtkSelect      *select);
#if 0
static gint         gtk_select_entry_key_press (GtkEntry      *widget, 
                                               GdkEventKey   *event, 
                                               GtkSelect      *select);
#endif
static void         gtk_select_size_allocate   (GtkWidget     *widget,
					       GtkAllocation *allocation);

static GtkHBoxClass *parent_class = NULL;

static void
gtk_select_class_init (GtkSelectClass * klass)
{
  GtkObjectClass *oclass;
  GtkWidgetClass *widget_class;

  parent_class = gtk_type_class (gtk_hbox_get_type ());
  oclass = (GtkObjectClass *) klass;
  widget_class = (GtkWidgetClass *) klass;

  oclass->destroy = gtk_select_destroy;
  
  widget_class->size_allocate = gtk_select_size_allocate;
}

static void
gtk_select_destroy (GtkObject * select)
{
  gtk_widget_destroy (GTK_SELECT (select)->popwin);
  gtk_widget_unref (GTK_SELECT (select)->popwin);
  gtk_widget_unref (GTK_SELECT (select)->empty);

  if (GTK_OBJECT_CLASS (parent_class)->destroy)
    (*GTK_OBJECT_CLASS (parent_class)->destroy) (select);
}

#if 0
static int
gtk_select_entry_key_press (GtkEntry * entry, GdkEventKey * event, GtkSelect * select)
{
  GList *li;

  return FALSE;
  if (!select->use_arrows || !GTK_LIST (select->list)->children)
    return FALSE;

  li = NULL;
  //li = g_list_find (GTK_LIST (select->list)->children, gtk_select_find (select));

  if ((event->keyval == GDK_Up)
      || (event->keyval == GDK_KP_Up)
      || ((event->state & GDK_MOD1_MASK) && ((event->keyval == 'p') || (event->keyval == 'P'))))
    {
      if (li)
	li = li->prev;
      if (!li && select->use_arrows_always)
	{
	  li = g_list_last (GTK_LIST (select->list)->children);
	}
      if (li)
	{
	  gtk_list_select_child (GTK_LIST (select->list), GTK_WIDGET (li->data));
	  gtk_signal_emit_stop_by_name (GTK_OBJECT (entry), "key_press_event");
	  return TRUE;
	}
    }
  else if ((event->keyval == GDK_Down)
	   || (event->keyval == GDK_KP_Down)
	   || ((event->state & GDK_MOD1_MASK) && ((event->keyval == 'n') || (event->keyval == 'N'))))
    {
      if (li)
	li = li->next;
      if (!li && select->use_arrows_always)
	{
	  li = GTK_LIST (select->list)->children;
	}
      if (li)
	{
	  gtk_list_select_child (GTK_LIST (select->list), GTK_WIDGET (li->data));
	  gtk_signal_emit_stop_by_name (GTK_OBJECT (entry), "key_press_event");
	  return TRUE;
	}
    }
  return FALSE;
}
#endif

static void
gtk_select_get_pos (GtkSelect * select, gint * x, gint * y, gint * height, gint * width)
{
  GtkBin *popwin;
  GtkWidget *widget;
  GtkScrolledWindow *popup;
  
  gint real_height;
  GtkRequisition list_requisition;
  gboolean show_hscroll = FALSE;
  gboolean show_vscroll = FALSE;
  gint avail_height;
  gint min_height;
  gint alloc_width;
  gint work_height;
  gint old_height;
  gint old_width;
  
  widget = GTK_WIDGET(select);
  popup  = GTK_SCROLLED_WINDOW (select->popup);
  popwin = GTK_BIN (select->popwin);
  
  gdk_window_get_origin (select->entry->window, x, y);
  real_height = MIN (select->entry->requisition.height, 
		     select->entry->allocation.height);
  *y += real_height;
  avail_height = gdk_screen_height () - *y;
  
  gtk_widget_size_request (select->list, &list_requisition);
  min_height = MIN (list_requisition.height, 
		    popup->vscrollbar->requisition.height);
  if (!GTK_LIST (select->list)->children)
    list_requisition.height += EMPTY_LIST_HEIGHT;
  
  alloc_width = (widget->allocation.width -
		 2 * popwin->child->style->klass->xthickness -
		 2 * GTK_CONTAINER (popwin->child)->border_width -
		 2 * GTK_CONTAINER (select->popup)->border_width -
		 2 * GTK_CONTAINER (GTK_BIN (popup)->child)->border_width - 
		 2 * GTK_BIN (popup)->child->style->klass->xthickness);
  
  work_height = (2 * popwin->child->style->klass->ythickness +
		 2 * GTK_CONTAINER (popwin->child)->border_width +
		 2 * GTK_CONTAINER (select->popup)->border_width +
		 2 * GTK_CONTAINER (GTK_BIN (popup)->child)->border_width +
		 2 * GTK_BIN (popup)->child->style->klass->xthickness);
  
  do 
    {
      old_width = alloc_width;
      old_height = work_height;
      
      if (!show_hscroll &&
	  alloc_width < list_requisition.width)
	{
	  work_height += popup->hscrollbar->requisition.height +
	    GTK_SCROLLED_WINDOW_CLASS 
	    (GTK_OBJECT (select->popup)->klass)->scrollbar_spacing;
	  show_hscroll = TRUE;
	}
      if (!show_vscroll && 
	  work_height + list_requisition.height > avail_height)
	{
	  if (work_height + min_height > avail_height && 
	      *y - real_height > avail_height)
	    {
	      *y -= (work_height + list_requisition.height + real_height);
	      break;
	    }
	  alloc_width -= 
	    popup->vscrollbar->requisition.width +
	    GTK_SCROLLED_WINDOW_CLASS 
	    (GTK_OBJECT (select->popup)->klass)->scrollbar_spacing;
	  show_vscroll = TRUE;
	}
    } while (old_width != alloc_width || old_height != work_height);
  
  *width = widget->allocation.width;
  if (show_vscroll)
    *height = avail_height;
  else
    *height = work_height + list_requisition.height;
  
  if (*x < 0)
    *x = 0;
}

static void
gtk_select_popup_list (GtkSelect * select)
{
  gint height, width, x, y;
  gint old_width, old_height;

  old_width = select->popwin->allocation.width;
  old_height  = select->popwin->allocation.height;

  gtk_select_get_pos (select, &x, &y, &height, &width);

  /* workaround for gtk_scrolled_window_size_allocate bug */
  if (old_width != width || old_height != height)
    {
      gtk_widget_hide (GTK_SCROLLED_WINDOW (select->popup)->hscrollbar);
      gtk_widget_hide (GTK_SCROLLED_WINDOW (select->popup)->vscrollbar);
    }

  gtk_widget_set_uposition (select->popwin, x, y);
  gtk_widget_set_usize (select->popwin, width, height);
  gtk_widget_realize (select->popwin);
  gdk_window_resize (select->popwin->window, width, height);
  gtk_widget_show (select->popwin);

  gtk_widget_grab_focus (select->popwin);
}

static void        
gtk_select_popup_button_press (GtkWidget        *button,
			      GdkEventButton   *event,
			      GtkSelect         *select)
{
  if (!GTK_WIDGET_HAS_FOCUS (select->entry))
    gtk_widget_grab_focus (select->entry);
  if (!select->current_button && (event->button == 1))
    gtk_select_popup_list (select);

  select->current_button = event->button;
  
  GTK_LIST (select->list)->drag_selection = TRUE;
  gdk_pointer_grab (select->list->window, TRUE,
		    GDK_POINTER_MOTION_HINT_MASK |
		    GDK_BUTTON1_MOTION_MASK |
		    GDK_BUTTON_RELEASE_MASK,
		    NULL, NULL, event->time);
  gtk_grab_add (select->list);
}

static void         
gtk_select_popup_button_leave (GtkWidget        *button,
			      GdkEventCrossing *event,
			      GtkSelect         *select)
{
  if (select->current_button)
    gtk_signal_emit_stop_by_name (GTK_OBJECT (button), "leave_notify_event");
}

static void
gtk_select_update_button (GtkSelect *select)
{
  if (g_list_length (select->entries) > 1)
    gtk_widget_show (select->button);
  else if (g_list_length (select->entries) == 1 &&
           select->selected == NULL)
    gtk_widget_show (select->button);
  else
    gtk_widget_hide (select->button);
}

static void
gtk_select_update_entry (GtkList * list, GtkSelect * select)
{
  GtkWidget *selected;
  GtkWidget *old_selected;
  GList *items;
  int posn;

  gtk_grab_remove (GTK_WIDGET (select));
  gtk_signal_handler_block (GTK_OBJECT (list), select->list_change_id);
  if (list->selection) {
    selected = list->selection->data;
    old_selected = select->selected;
    if (old_selected) {
      items = g_list_append(NULL, old_selected);
      posn = g_list_index(select->entries, old_selected);
      gtk_widget_ref(old_selected);
      gtk_container_remove(GTK_CONTAINER(select->entry), old_selected);
      if (old_selected != select->empty)
	gtk_list_insert_items(list, items, posn);
      gtk_widget_unref(old_selected);
    } else {
      gtk_container_remove(GTK_CONTAINER(select->entry), select->empty);
    }
    if (selected) {
      select->selected = selected;
      items = g_list_append(NULL, selected);
      gtk_widget_ref(selected);
      gtk_list_remove_items(list, items);
      gtk_container_add(GTK_CONTAINER(select->entry), selected);
      gtk_widget_unref(selected);
    } else {
      gtk_container_add(GTK_CONTAINER(select->entry), select->empty);
      select->selected = NULL;
    }
  }

  gtk_select_update_button (select);

  gtk_signal_handler_unblock (GTK_OBJECT (list), select->list_change_id);
}

static gint
gtk_select_button_press (GtkWidget * widget, GdkEvent * event, GtkSelect * select)
{
  GtkWidget *child;

  child = gtk_get_event_widget (event);

  /* We don't ask for button press events on the grab widget, so
   *  if an event is reported directly to the grab widget, it must
   *  be on a window outside the application (and thus we remove
   *  the popup window). Otherwise, we check if the widget is a child
   *  of the grab widget, and only remove the popup window if it
   *  is not.
   */
  if (child != widget)
    {
      while (child)
	{
	  if (child == widget)
	    return FALSE;
	  child = child->parent;
	}
    }

  gtk_widget_hide (select->popwin);
  gtk_grab_remove (select->popwin);
  gdk_pointer_ungrab (event->button.time);

  return TRUE;
}

static gint
gtk_select_button_release (GtkWidget * widget, GdkEvent * event, GtkSelect * select)
{
  GtkWidget *child;

  if ((select->current_button != 0) && (event->button.button == 1))
    {
      /* This was the initial button press */

      GdkEventCrossing tmp_event;

      select->current_button = 0;

      if (widget != select->button)
	gtk_widget_event (select->button, event);

      /* Un-pre-hightlight */
      
      tmp_event.type = GDK_LEAVE_NOTIFY;
      tmp_event.window = select->button->window;
      tmp_event.send_event = TRUE;
      tmp_event.subwindow = NULL;
      tmp_event.detail = GDK_NOTIFY_ANCESTOR;
      
      gtk_widget_event (select->button, (GdkEvent *)&tmp_event);

      /* Check to see if we released inside the button */
      child = gtk_get_event_widget ((GdkEvent*) event);

      while (child && child != (select->button))
	child = child->parent;

      if (child == select->button)
	{
	  gtk_grab_add (select->popwin);
	  gdk_pointer_grab (select->popwin->window, TRUE,
			    GDK_BUTTON_PRESS_MASK | 
			    GDK_BUTTON_RELEASE_MASK |
			    GDK_POINTER_MOTION_MASK, 
			    NULL, NULL, GDK_CURRENT_TIME);
	  return FALSE;
	}
    }
  else
    {
      /* The user has clicked inside the popwin and released */

      if (GTK_WIDGET_HAS_GRAB (select->popwin))
	{
	  gtk_grab_remove (select->popwin);
	  gdk_pointer_ungrab (event->button.time);
	}
    }
  
  gtk_widget_hide (select->popwin);

  return TRUE;
}

static gint         
gtk_select_list_enter (GtkWidget        *widget,
		      GdkEventCrossing *event,
		      GtkSelect         *select)
{
  GtkWidget *event_widget;

  event_widget = gtk_get_event_widget ((GdkEvent*) event);
  
  if ((event_widget == select->list) &&
      (select->current_button != 0) && 
      (!GTK_WIDGET_HAS_GRAB (select->list)))
    {
      GdkEvent tmp_event;
      gint x, y;
      GdkModifierType mask;

      gtk_grab_remove (select->popwin);

      /* Transfer the grab over to the list by synthesizing
       * a button press event
       */
      gdk_window_get_pointer (select->list->window, &x, &y, &mask);

      tmp_event.button.type = GDK_BUTTON_PRESS;
      tmp_event.button.window = select->list->window;
      tmp_event.button.send_event = TRUE;
      tmp_event.button.time = GDK_CURRENT_TIME; /* bad */
      tmp_event.button.x = x;
      tmp_event.button.y = y;
      /* We leave all the XInput fields unfilled here, in the expectation
       * that GtkList doesn't care.
       */
      tmp_event.button.button = select->current_button;
      tmp_event.button.state = mask;

      gtk_widget_event (select->list, &tmp_event);
    }

  return FALSE;
}

static int
gtk_select_list_key_press (GtkWidget * widget, GdkEventKey * event, GtkSelect * select)
{
  if (event->keyval == GDK_Escape)
    {
      if (GTK_WIDGET_HAS_GRAB (select->popwin))
	{
	  gtk_grab_remove (select->popwin);
	  gdk_pointer_ungrab (GDK_CURRENT_TIME);
	}
      else if (GTK_WIDGET_HAS_GRAB (select->list))
	gtk_list_end_drag_selection (GTK_LIST (select->list));
      gtk_widget_hide (select->popwin);
      if (GTK_WIDGET_HAS_GRAB (select->button))
	{
	  select->current_button = 0;
	  GTK_BUTTON (select->button)->in_button = FALSE;
	  gtk_button_released (GTK_BUTTON (select->button));
	  gtk_grab_remove (select->button);
	}
      return TRUE;
    }
  return FALSE;
}

static void
gtk_select_init (GtkSelect * select)
{
  GtkWidget *arrow;
  GtkWidget *frame;
  GtkWidget *event_box;
  GdkCursor *cursor;

  select->use_arrows = 1;
  select->entry = gtk_event_box_new ();
  select->button = gtk_button_new ();
  select->current_button = 0;
  select->empty = gtk_label_new ("");
  select->entries = NULL;
  select->selected = NULL;
  arrow = gtk_arrow_new (GTK_ARROW_DOWN, GTK_SHADOW_OUT);
  gtk_widget_show (arrow);
  gtk_widget_ref(select->empty);
  gtk_widget_show (select->empty);
  gtk_container_add(GTK_CONTAINER(select->entry), select->empty);
  gtk_container_add (GTK_CONTAINER (select->button), arrow);
  gtk_box_pack_start (GTK_BOX (select), select->button, FALSE, FALSE, 0);
  gtk_box_pack_start (GTK_BOX (select), select->entry, TRUE, TRUE, 0);
  GTK_WIDGET_UNSET_FLAGS (select->button, GTK_CAN_FOCUS);
  gtk_widget_show (select->entry);
//  gtk_signal_connect (GTK_OBJECT (select->entry), "key_press_event",
//		      (GtkSignalFunc) gtk_select_entry_key_press, select);
  gtk_signal_connect_after (GTK_OBJECT (select->button), "button_press_event",
			    (GtkSignalFunc) gtk_select_popup_button_press, select);
  /*gtk_signal_connect_after (GTK_OBJECT (select->button), "button_release_event",
    (GtkSignalFunc) gtk_select_button_release, select);*/
  gtk_signal_connect (GTK_OBJECT (select->button), "leave_notify_event",
		      (GtkSignalFunc) gtk_select_popup_button_leave, select);
  /*gtk_signal_connect(GTK_OBJECT(select->button), "clicked",
     (GtkSignalFunc)prelight_bug, select); */

  select->popwin = gtk_window_new (GTK_WINDOW_POPUP);
  gtk_widget_ref (select->popwin);
  gtk_window_set_policy (GTK_WINDOW (select->popwin), 1, 1, 0);
  
  gtk_widget_set_events (select->popwin, GDK_KEY_PRESS_MASK);

  event_box = gtk_event_box_new ();
  gtk_container_add (GTK_CONTAINER (select->popwin), event_box);
  gtk_widget_show (event_box);

  gtk_widget_realize (event_box);
  cursor = gdk_cursor_new (GDK_TOP_LEFT_ARROW);
  gdk_window_set_cursor (event_box->window, cursor);
  gdk_cursor_destroy (cursor);

  frame = gtk_frame_new (NULL);
  gtk_container_add (GTK_CONTAINER (event_box), frame);
  gtk_frame_set_shadow_type (GTK_FRAME (frame), GTK_SHADOW_OUT);
  gtk_widget_show (frame);

  select->popup = gtk_scrolled_window_new (NULL, NULL);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (select->popup),
				  GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  GTK_WIDGET_UNSET_FLAGS (GTK_SCROLLED_WINDOW (select->popup)->hscrollbar, GTK_CAN_FOCUS);
  GTK_WIDGET_UNSET_FLAGS (GTK_SCROLLED_WINDOW (select->popup)->vscrollbar, GTK_CAN_FOCUS);
  gtk_container_add (GTK_CONTAINER (frame), select->popup);
  gtk_widget_show (select->popup);

  select->list = gtk_list_new ();
  /* We'll use enter notify events to figure out when to transfer
   * the grab to the list
   */
  gtk_widget_set_events (select->list, GDK_ENTER_NOTIFY_MASK);

  gtk_list_set_selection_mode(GTK_LIST(select->list), GTK_SELECTION_SINGLE);
  gtk_scrolled_window_add_with_viewport (GTK_SCROLLED_WINDOW (select->popup), select->list);
  gtk_container_set_focus_vadjustment (GTK_CONTAINER (select->list),
				       gtk_scrolled_window_get_vadjustment (GTK_SCROLLED_WINDOW (select->popup)));
  gtk_container_set_focus_hadjustment (GTK_CONTAINER (select->list),
				       gtk_scrolled_window_get_hadjustment (GTK_SCROLLED_WINDOW (select->popup)));
  gtk_widget_show (select->list);

  select->list_change_id = gtk_signal_connect (GTK_OBJECT (select->list),
     					       "selection_changed",
			     (GtkSignalFunc) gtk_select_update_entry, select);
  gtk_signal_connect (GTK_OBJECT (select->popwin), "key_press_event",
		      (GtkSignalFunc) gtk_select_list_key_press, select);
  gtk_signal_connect (GTK_OBJECT (select->popwin), "button_press_event",
		      GTK_SIGNAL_FUNC (gtk_select_button_press), select);

  gtk_signal_connect_after (GTK_OBJECT (select->list), "button_release_event",
			    GTK_SIGNAL_FUNC (gtk_select_button_release), select);
  /* We connect here on the button, because we'll have a grab on it
   * when the event occurs. But we are actually interested in enters
   * for the select->list.
   */
  gtk_signal_connect (GTK_OBJECT (select->button), "enter_notify_event",
		      GTK_SIGNAL_FUNC (gtk_select_list_enter), select);
}

guint
gtk_select_get_type (void)
{
  static guint select_type = 0;

  if (!select_type)
    {
      static const GtkTypeInfo select_info =
      {
	"GtkSelect",
	sizeof (GtkSelect),
	sizeof (GtkSelectClass),
	(GtkClassInitFunc) gtk_select_class_init,
	(GtkObjectInitFunc) gtk_select_init,
	/* reserved_1 */ NULL,
	/* reserved_2 */ NULL,
        (GtkClassInitFunc) NULL,
      };
      select_type = gtk_type_unique (gtk_hbox_get_type (), &select_info);
    }
  return select_type;
}

GtkWidget *
gtk_select_new (void)
{
  return GTK_WIDGET (gtk_type_new (gtk_select_get_type ()));
}

void
gtk_select_set_use_arrows (GtkSelect * select, gint val)
{
  g_return_if_fail (select != NULL);
  g_return_if_fail (GTK_IS_SELECT (select));

  select->use_arrows = val;
}

static void
gtk_select_size_allocate (GtkWidget     *widget,
			  GtkAllocation *allocation)
{
  GtkSelect *select;

  g_return_if_fail (widget != NULL);
  g_return_if_fail (GTK_IS_SELECT (widget));
  g_return_if_fail (allocation != NULL);

  GTK_WIDGET_CLASS (parent_class)->size_allocate (widget, allocation);
  
  select = GTK_SELECT (widget);

  if (select->entry->allocation.height > select->entry->requisition.height)
    {
      GtkAllocation button_allocation;

      button_allocation = select->button->allocation;
      button_allocation.height = select->entry->requisition.height;
      button_allocation.y = select->entry->allocation.y + 
	(select->entry->allocation.height - select->entry->requisition.height) 
	/ 2;
      gtk_widget_size_allocate (select->button, &button_allocation);
    }
}

void
gtk_select_disable_activate (GtkSelect* select)
{
  g_return_if_fail (select != NULL);
  g_return_if_fail (GTK_IS_SELECT (select));
}

static GList *
g_list_insert_list(GList *list, GList *items, gint position)
{
	GList *at;
	GList *last;
	int ll;

	if (!items) {
		return list;
	}
	ll = g_list_length(list);
	if (ll <= position) {
		return g_list_concat(list, items);
	}
	if (position == 0) {
		return g_list_concat(items, list);
	}
	at = g_list_nth(list, position - 1);
	last = g_list_last(items);
	last->next = at->next;
	if (at->next) {
		at->next->prev = last;
	}
	at->next = items;
	items->prev = at;
	return list;
}

void
gtk_select_insert_items (GtkSelect *select, GList *items, gint position)
{
  GList *copy;

  g_return_if_fail (select != NULL);
  g_return_if_fail (GTK_IS_SELECT (select));
  copy = g_list_copy(items);
  select->entries = g_list_insert_list(select->entries, items, position);
  gtk_list_insert_items(GTK_LIST(select->list), copy, position);
  gtk_select_update_button (select);
}

void
gtk_select_append_items (GtkSelect *select, GList *items)
{
  GList *copy;

  g_return_if_fail (select != NULL);
  g_return_if_fail (GTK_IS_SELECT (select));
  copy = g_list_copy(items);
  select->entries = g_list_concat(select->entries, items);
  gtk_list_append_items(GTK_LIST(select->list), copy);
  gtk_select_update_button (select);
}

void
gtk_select_prepend_items (GtkSelect *select, GList *items)
{
  GList *copy;

  g_return_if_fail (select != NULL);
  g_return_if_fail (GTK_IS_SELECT (select));
  copy = g_list_copy(items);
  select->entries = g_list_concat(items, select->entries);
  gtk_list_prepend_items(GTK_LIST(select->list), copy);
  gtk_select_update_button (select);
}

static void
gtk_select_remove_items_internal (GtkSelect *select, GList *items,
	       			  gboolean unref)
{
  GList *loop;
  int do_free = 0;;

  g_return_if_fail (select != NULL);
  g_return_if_fail (GTK_IS_SELECT (select));
  for (loop = items; loop; loop = loop->next) {
    select->entries = g_list_remove(select->entries, loop->data);
  }
  if (select->selected && g_list_find(items, select->selected)) {
    items = g_list_copy(items);
    items = g_list_remove(items, select->selected);
    do_free = 1;
    if (!unref)
      gtk_widget_ref(select->selected);
    gtk_container_remove(GTK_CONTAINER(select->entry), select->selected);
    gtk_container_add(GTK_CONTAINER(select->entry), select->empty);
    select->selected = NULL;
  }
  if (unref)
    gtk_list_remove_items(GTK_LIST(select->list), items);
  else
    gtk_list_remove_items_no_unref(GTK_LIST(select->list), items);
  if (do_free)
    g_list_free(items);

  gtk_select_update_button (select);
}

void
gtk_select_remove_items (GtkSelect *select, GList *items)
{
  gtk_select_remove_items_internal(select, items, TRUE);
}

void
gtk_select_clear_items (GtkSelect *select, gint start, gint end)
{
  GList *sptr, *eptr;
  int len;

  g_return_if_fail (select != NULL);
  g_return_if_fail (GTK_IS_SELECT (select));
  if (select->entries == NULL) {
    return;
  }
  len = g_list_length(select->entries);
  if (end > len)
    end = len;
  if (start > end)
    return;
  sptr = g_list_nth(select->entries, start);
  eptr = g_list_nth(select->entries, end);
  if (sptr->prev)
    sptr->prev->next = eptr;
  if (eptr && eptr->prev)
    eptr->prev->next = NULL;
  if (eptr)
    eptr->prev = sptr->prev;
  if (sptr == select->entries)
    select->entries = eptr;
  if (select->selected && g_list_find(sptr, select->selected)) {
    gtk_container_remove(GTK_CONTAINER(select->entry), select->selected);
    gtk_container_add(GTK_CONTAINER(select->entry), select->empty);
    select->selected = NULL;
  }
  gtk_list_remove_items(GTK_LIST(select->list), sptr);
  g_list_free(sptr);
}

void
gtk_select_select_item (GtkSelect *select, gint item)
{
  g_return_if_fail (select != NULL);
  g_return_if_fail (GTK_IS_SELECT (select));
  gtk_list_select_item(GTK_LIST(select->list), item);
}

void
gtk_select_select_child (GtkSelect *select, GtkWidget *child)
{
  g_return_if_fail (select != NULL);
  g_return_if_fail (GTK_IS_SELECT (select));
  gtk_list_select_child(GTK_LIST(select->list), child);
}

gint
gtk_select_child_position (GtkSelect *select, GtkWidget *child)
{
  g_return_val_if_fail (select != NULL, -1);
  g_return_val_if_fail (GTK_IS_SELECT (select), -1);
  return g_list_index(select->entries, child);
}
