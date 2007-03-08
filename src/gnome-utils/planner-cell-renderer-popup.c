/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * Copyright (C) 2001-2002 CodeFactory AB
 * Copyright (C) 2001-2002 Richard Hult <richard@imendio.com>
 * Copyright (C) 2001-2002 Mikael Hallendal <micke@imendio.com>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this program; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include <config.h>
#include <gdk/gdkkeysyms.h>
#include <gtk/gtkcellrenderertext.h>
#include <gtk/gtkwindow.h>
#include <gtk/gtkmain.h>
#include <gtk/gtkeditable.h>
#include <gtk/gtksignal.h>
#include <gtk/gtkbutton.h>
#include "planner-marshal.h"
#include "planner-cell-renderer-popup.h"
#include "planner-popup-entry.h"


enum {
	SHOW_POPUP,
	HIDE_POPUP,
	LAST_SIGNAL
};

static void      mcrp_init               (PlannerCellRendererPopup      *popup);
static void      mcrp_class_init         (PlannerCellRendererPopupClass *class);

static GtkCellEditable *
mcrp_start_editing                       (GtkCellRenderer          *cell,
					  GdkEvent                 *event,
					  GtkWidget                *widget,
					  const gchar              *path,
					  GdkRectangle             *background_area,
					  GdkRectangle             *cell_area,
					  GtkCellRendererState      flags);
static void      mcrp_show_popup         (PlannerCellRendererPopup      *cell,
					  const gchar              *path,
					  gint                      x1,
					  gint                      y1,
					  gint                      x2,
					  gint                      y2);
static void      mcrp_hide_popup         (PlannerCellRendererPopup      *cell);
static void      mcrp_get_size           (GtkCellRenderer          *cell,
					  GtkWidget                *widget,
					  GdkRectangle             *cell_area,
					  gint                     *x_offset,
					  gint                     *y_offset,
					  gint                     *width,
					  gint                     *height);
static void      mcrp_style_set           (GtkWidget               *widget,
					   GtkStyle                *old_style,
					   PlannerCellRendererPopup     *popup);
static gboolean  mcrp_key_press_event    (GtkWidget                *popup_window,
					  GdkEventKey              *event,
					  PlannerCellRendererPopup      *cell);
static gboolean  mcrp_button_press_event (GtkWidget                *widget,
					  GdkEventButton           *event,
					  PlannerCellRendererPopup      *popup);


static GtkCellRendererTextClass *parent_class;
static guint signals[LAST_SIGNAL];

#define PLANNER_CELL_RENDERER_POPUP_PATH "planner-cell-renderer-popup-path"

GType
planner_cell_renderer_popup_get_type (void)
{
	static GType cell_text_type = 0;
	
	if (!cell_text_type) {
		static const GTypeInfo cell_text_info = {
			sizeof (PlannerCellRendererPopupClass),
			NULL,		/* base_init */
			NULL,		/* base_finalize */
			(GClassInitFunc) mcrp_class_init,
			NULL,		/* class_finalize */
			NULL,		/* class_data */
			sizeof (PlannerCellRendererPopup),
			0,              /* n_preallocs */
			(GInstanceInitFunc) mcrp_init,
		};
		
		cell_text_type = g_type_register_static (GTK_TYPE_CELL_RENDERER_TEXT,
							 "PlannerCellRendererPopup",
							 &cell_text_info,
							 0);
	}
	
	return cell_text_type;
}

static void
mcrp_init (PlannerCellRendererPopup *popup)
{
	popup->popup_window = gtk_window_new (GTK_WINDOW_POPUP);

	popup->button_width = -1;
	
	g_signal_connect (popup->popup_window,
			  "button-press-event",
			  G_CALLBACK (mcrp_button_press_event),
			  popup);

	g_signal_connect (popup->popup_window,
			  "key-press-event",
			  G_CALLBACK (mcrp_key_press_event),
			  popup);

	g_signal_connect (popup->popup_window,
			  "style-set",
			  G_CALLBACK (mcrp_style_set),
			  popup);
}

static void
mcrp_class_init (PlannerCellRendererPopupClass *class)
{
	GtkCellRendererClass *cell_class = GTK_CELL_RENDERER_CLASS (class);
	
	parent_class = GTK_CELL_RENDERER_TEXT_CLASS (g_type_class_peek_parent (class));
	
	cell_class->start_editing = mcrp_start_editing;
	cell_class->get_size      = mcrp_get_size;

	class->show_popup = mcrp_show_popup;
	class->hide_popup = mcrp_hide_popup;

	signals[SHOW_POPUP] = g_signal_new (
		"show-popup",
		G_TYPE_FROM_CLASS (class),
		G_SIGNAL_RUN_LAST,
		G_STRUCT_OFFSET (PlannerCellRendererPopupClass, show_popup),
		NULL, NULL,
		planner_marshal_VOID__STRING_INT_INT_INT_INT,
		G_TYPE_NONE, 5,
		G_TYPE_STRING,
		G_TYPE_INT,
		G_TYPE_INT,
		G_TYPE_INT,
		G_TYPE_INT);

	signals[HIDE_POPUP] = g_signal_new (
		"hide-popup",
		G_TYPE_FROM_CLASS (class),
		G_SIGNAL_RUN_LAST,
		G_STRUCT_OFFSET (PlannerCellRendererPopupClass, hide_popup),
		NULL, NULL,
		planner_marshal_VOID__VOID,
		G_TYPE_NONE, 0);
}

static void
mcrp_editing_done (GtkCellEditable     *editable,
		   PlannerCellRendererPopup *cell)
{
	gchar       *path;
	const gchar *new_text;

	if (PLANNER_POPUP_ENTRY (editable)->editing_canceled ||
	    cell->editing_canceled) {
		return;
	}
	
	path = g_object_get_data (G_OBJECT (editable),
				  PLANNER_CELL_RENDERER_POPUP_PATH);
	new_text = planner_popup_entry_get_text (PLANNER_POPUP_ENTRY (editable));

	g_signal_emit_by_name (cell,
			       "edited",
			       path,
			       new_text);
}

static void
mcrp_style_set (GtkWidget           *widget,
		GtkStyle            *old_style,
		PlannerCellRendererPopup *popup)
{
	/* Invalidate the cache. */
	popup->button_width = -1;
}
	
static gboolean
mcrp_grab_on_window (GdkWindow *window,
		     guint32    activate_time)
{
	if ((gdk_pointer_grab (window, TRUE,
			       GDK_BUTTON_PRESS_MASK |
			       GDK_BUTTON_RELEASE_MASK |
			       GDK_POINTER_MOTION_MASK,
			       NULL, NULL, activate_time) == 0)) {
		if (gdk_keyboard_grab (window, TRUE,
			       activate_time) == 0)
			return TRUE;
		else {
			gdk_pointer_ungrab (activate_time);
			return FALSE;
		}
	}

	return FALSE;
}

static void
mcrp_show_popup (PlannerCellRendererPopup *cell,
		 const gchar         *path,
		 gint                 x1,
		 gint                 y1,
		 gint                 x2,
		 gint                 y2)
{
	GtkAllocation alloc;
	gint          x, y;
	gint          screen_height, screen_width;
	gint          button_height;

	cell->shown = TRUE;

	gtk_widget_realize (cell->popup_window);

	/* I'm not sure this is ok to do, but we need to show the window to be
	 * able to get the allocation right.
	 */
	gtk_window_move (GTK_WINDOW (cell->popup_window), -500, -500);
	gtk_widget_show (cell->popup_window);

	alloc = cell->popup_window->allocation;

	x = x2;
	y = y2;

	button_height = y2 - y1;
	
	screen_height = gdk_screen_height () - y;
	screen_width = gdk_screen_width ();

	/* Check if it fits in the available height. */
	if (alloc.height > screen_height) {
		/* It doesn't fit, so we see if we have the minimum space needed. */
		if (alloc.height > screen_height && y - button_height > screen_height) {
			/* We don't, so we show the popup above the cell
			   instead of below it. */
			screen_height = y - button_height;
			y -= (alloc.height + button_height);
			if (y < 0) {
				y = 0;
			}
		}
	}

	/* We try to line it up with the right edge of the column, but we don't
	 * want it to go off the edges of the screen.
	 */
	if (x > screen_width) {
		x = screen_width;
	}

	x -= alloc.width;
	if (x < 0) {
		x = 0;
	}

	gtk_grab_add (cell->popup_window);

	gtk_window_move (GTK_WINDOW (cell->popup_window), x, y);
	gtk_widget_show (cell->popup_window);

	gtk_widget_grab_focus (cell->focus_window);

	mcrp_grab_on_window (cell->popup_window->window,
			     gtk_get_current_event_time ());
}

static void
mcrp_hide_popup (PlannerCellRendererPopup *cell)
{
	gtk_grab_remove (cell->popup_window);
	gtk_widget_hide (cell->popup_window);

	if (cell->editable) {
		gtk_cell_editable_editing_done (GTK_CELL_EDITABLE (cell->editable));
	}

	/* This may look weird (the test), but the weak pointer will actually be
	 * nulled out for some cells, like the date cell.
	 */
	if (cell->editable) {
		gtk_cell_editable_remove_widget (GTK_CELL_EDITABLE (cell->editable));
	}
	
	cell->shown = FALSE;
	cell->editing_canceled = FALSE;
}

static void
mcrp_arrow_clicked (GtkCellEditable     *entry,
		    PlannerCellRendererPopup *cell)
{
	GtkAllocation  alloc;
	gint           x, y;
	const gchar   *path;
	
	if (cell->shown) {
		cell->editing_canceled = TRUE;
		planner_cell_renderer_popup_hide (cell);
		return;
	}
	
	path = g_object_get_data (G_OBJECT (entry),
				  PLANNER_CELL_RENDERER_POPUP_PATH);

	/* Temporarily grab pointer and keyboard on a window we know exists; we
	 * do this so that the grab (with owner events == TRUE) affects
	 * events generated when the window is mapped, such as enter
	 * notify events on subwidgets. If the grab fails, bail out.
	 */
	if (!mcrp_grab_on_window (GTK_WIDGET (entry)->window,
				  gtk_get_current_event_time ())) {
		return;
	}
	
	gtk_editable_select_region (GTK_EDITABLE (PLANNER_POPUP_ENTRY (entry)->entry), 0, 0);

	gdk_window_get_origin (GTK_WIDGET (entry)->window, &x, &y);
	
	alloc = GTK_WIDGET (entry)->allocation;

	g_signal_emit (cell, signals[SHOW_POPUP], 0,
		       path, 
		       x,
		       y,
		       x + alloc.width,
		       y + alloc.height);
}

static GtkCellEditable *
mcrp_start_editing (GtkCellRenderer      *cell,
		    GdkEvent             *event,
		    GtkWidget            *widget,
		    const gchar          *path,
		    GdkRectangle         *background_area,
		    GdkRectangle         *cell_area,
		    GtkCellRendererState  flags)
{
	PlannerCellRendererPopup *popup;
	GtkWidget           *editable;
	gchar               *text;
	
	popup = PLANNER_CELL_RENDERER_POPUP (cell);

	/* If the cell isn't editable we return NULL. */
	if (GTK_CELL_RENDERER_TEXT (popup)->editable == FALSE) {
		return NULL;
	}
	
	editable = g_object_new (PLANNER_TYPE_POPUP_ENTRY, NULL);

	text = GTK_CELL_RENDERER_TEXT (cell)->text;
	planner_popup_entry_set_text (PLANNER_POPUP_ENTRY (editable), text ? text : "");
	
	g_object_set_data_full (G_OBJECT (editable),
				PLANNER_CELL_RENDERER_POPUP_PATH,
				g_strdup (path),
				g_free);
	
	gtk_widget_show (editable);

	g_signal_connect (editable,
			  "editing-done",
			  G_CALLBACK (mcrp_editing_done),
			  popup);

	g_signal_connect (editable,
			  "arrow-clicked",
			  G_CALLBACK (mcrp_arrow_clicked),
			  popup);

	popup->editable = editable;

	g_object_add_weak_pointer (G_OBJECT (popup->editable),
				   (gpointer) &popup->editable);
	
	return GTK_CELL_EDITABLE (editable);
}

GtkCellRenderer *
planner_cell_renderer_popup_new (void)
{
	return GTK_CELL_RENDERER (
		g_object_new (planner_cell_renderer_popup_get_type (), NULL));
}

void
planner_cell_renderer_popup_hide (PlannerCellRendererPopup *cell)
{ 
	g_return_if_fail (PLANNER_IS_CELL_RENDERER_POPUP (cell));
	
	g_signal_emit (cell, signals[HIDE_POPUP], 0);
}

static void
mcrp_get_size (GtkCellRenderer *cell,
	       GtkWidget       *widget,
	       GdkRectangle    *cell_area,
	       gint            *x_offset,
	       gint            *y_offset,
	       gint            *width,
	       gint            *height)
{
	PlannerCellRendererPopup *popup;

	popup = PLANNER_CELL_RENDERER_POPUP (cell);
	
	if (GTK_CELL_RENDERER_CLASS (parent_class)->get_size) { 
		(* GTK_CELL_RENDERER_CLASS (parent_class)->get_size) (cell,
								      widget,
								      cell_area, 
								      x_offset,
								      y_offset,
								      width,
								      height);
	}

	/* We cache this because it takes really long to get the width. */
	if (popup->button_width == -1) {
		popup->button_width = planner_popup_get_button_width ();
	}
	
	*width += popup->button_width;
}

static gboolean
mcrp_key_press_event (GtkWidget           *popup_window,
		      GdkEventKey         *event,
		      PlannerCellRendererPopup *cell)
{
	if (event->keyval != GDK_Escape &&
	    event->keyval != GDK_Return &&
	    event->keyval != GDK_KP_Enter &&
	    event->keyval != GDK_ISO_Enter &&
	    event->keyval != GDK_3270_Enter) {
		return FALSE;
	}

	if (event->keyval == GDK_Escape) {
		cell->editing_canceled = TRUE;
	} else {
		cell->editing_canceled = FALSE;
	}		
	
	planner_cell_renderer_popup_hide (cell);

	return TRUE;
}

static gboolean
mcrp_button_press_event (GtkWidget           *widget,
			 GdkEventButton      *event,
			 PlannerCellRendererPopup *popup)
{
	GtkAllocation alloc;
	gdouble       x, y;
	gint          xoffset, yoffset;
	gint          x1, y1;
	gint          x2, y2;

	if (event->button != 1) {
		return FALSE;
	}
	
	/* If the event happened outside the popup, cancel editing.
	 */

	/*gdk_event_get_root_coords ((GdkEvent *) event, &x, &y);*/
	x = event->x_root;
	y = event->y_root;

	gdk_window_get_root_origin (widget->window,
				    &xoffset,
				    &yoffset);

	xoffset += widget->allocation.x;
	yoffset += widget->allocation.y;
	
	alloc = popup->popup_window->allocation;
	x1 = alloc.x + xoffset;
	y1 = alloc.y + yoffset;
	x2 = x1 + alloc.width;
	y2 = y1 + alloc.height;

	if (x > x1 && x < x2 && y > y1 && y < y2) {
		return FALSE;
	}
	
	popup->editing_canceled = TRUE;
	planner_cell_renderer_popup_hide (popup);
	
	return FALSE;
}
