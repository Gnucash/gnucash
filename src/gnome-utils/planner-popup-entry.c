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
#include <string.h>
#include <gtk/gtkwindow.h>
#include <gtk/gtkbutton.h>
#include <gtk/gtkentry.h>
#include <gtk/gtkarrow.h>
#include <gtk/gtkhbox.h>
#include <gtk/gtkeventbox.h>
#include <gtk/gtkcelleditable.h>
#include <gdk/gdkkeysyms.h>
#include "planner-popup-entry.h"

static void     planner_popup_entry_init       (PlannerPopupEntry        *entry);
static void     planner_popup_entry_class_init (PlannerPopupEntryClass   *class);
static void     mpw_cell_editable_init    (GtkCellEditableIface *iface);
static gboolean mpw_key_press_event       (GtkWidget            *box,
					   GdkEventKey          *key_event);

enum {
	ARROW_CLICKED,
	LAST_SIGNAL
};

static GtkEventBoxClass *parent_class;
static guint signals[LAST_SIGNAL];

GtkType
planner_popup_entry_get_type (void)
{
	static GtkType widget_type = 0;
	
	if (!widget_type) {
		static const GTypeInfo widget_info = {
			sizeof (PlannerPopupEntryClass),
			NULL,		/* base_init */
			NULL,		/* base_finalize */
			(GClassInitFunc) planner_popup_entry_class_init,
			NULL,		/* class_finalize */
			NULL,		/* class_data */
			sizeof (PlannerPopupEntry),
			0,              /* n_preallocs */
			(GInstanceInitFunc) planner_popup_entry_init,
		};

		static const GInterfaceInfo cell_editable_info = {
			(GInterfaceInitFunc) mpw_cell_editable_init,    /* interface_init */
			NULL,                                           /* interface_finalize */
			NULL                                            /* interface_data */
		};
      
		widget_type = g_type_register_static (GTK_TYPE_EVENT_BOX,
						      "PlannerPopupEntry",
						      &widget_info,
						      0);
		
		g_type_add_interface_static (widget_type,
					     GTK_TYPE_CELL_EDITABLE,
					     &cell_editable_info);
	}
	
	return widget_type;
}

static void
planner_popup_entry_init (PlannerPopupEntry *widget)
{
	GtkWidget *arrow;

	widget->hbox = gtk_hbox_new (FALSE, 0);
	gtk_widget_show (widget->hbox);

	widget->entry = g_object_new (GTK_TYPE_ENTRY, "has_frame", FALSE, NULL);
	GTK_ENTRY (widget->entry)->is_cell_renderer = TRUE;
	gtk_widget_show (widget->entry);

	widget->button = gtk_button_new ();
	gtk_widget_show (widget->button);

	arrow = gtk_arrow_new (GTK_ARROW_DOWN, GTK_SHADOW_OUT);
	gtk_widget_show (arrow);

	gtk_container_add (GTK_CONTAINER (widget->button), arrow);

	gtk_box_pack_start (GTK_BOX (widget->hbox), widget->entry, TRUE, TRUE, 0);
	gtk_box_pack_start (GTK_BOX (widget->hbox), widget->button, FALSE, TRUE, 0);

	gtk_container_add (GTK_CONTAINER (widget), widget->hbox);

	GTK_WIDGET_SET_FLAGS (widget, GTK_CAN_FOCUS);
	gtk_widget_add_events (GTK_WIDGET (widget), GDK_KEY_PRESS_MASK);
	gtk_widget_add_events (GTK_WIDGET (widget), GDK_KEY_RELEASE_MASK);
}

static void
planner_popup_entry_class_init (PlannerPopupEntryClass *klass)
{
	GtkWidgetClass *widget_class = GTK_WIDGET_CLASS (klass);

	widget_class->key_press_event = mpw_key_press_event;
	
	parent_class = GTK_EVENT_BOX_CLASS (g_type_class_peek_parent (klass));

	signals[ARROW_CLICKED] = g_signal_new
		("arrow-clicked",
		 G_TYPE_FROM_CLASS (klass),
		 G_SIGNAL_RUN_LAST,
		 0,
		 NULL, NULL,
		 g_cclosure_marshal_VOID__VOID,
		 G_TYPE_NONE, 0);
}

static void
mpw_arrow_clicked (GtkWidget *button, PlannerPopupEntry *widget)
{
	g_signal_emit (widget, signals[ARROW_CLICKED], 0);
}
	
/* GtkCellEditable method implementations
 */
static void
gtk_cell_editable_entry_activated (GtkEntry *entry, PlannerPopupEntry *widget)
{
	gtk_cell_editable_editing_done (GTK_CELL_EDITABLE (widget));
	gtk_cell_editable_remove_widget (GTK_CELL_EDITABLE (widget));
}

static gboolean
gtk_cell_editable_key_press_event (GtkEntry      *entry,
				   GdkEventKey   *key_event,
				   PlannerPopupEntry *widget)
{
	if (key_event->keyval == GDK_Escape) {
		widget->editing_canceled = TRUE;
		
		gtk_cell_editable_editing_done (GTK_CELL_EDITABLE (widget));
		gtk_cell_editable_remove_widget (GTK_CELL_EDITABLE (widget));
		
		return TRUE;
	}

	return FALSE;
}

static gboolean
mpw_key_press_event (GtkWidget   *box,
		     GdkEventKey *key_event)
{
	PlannerPopupEntry *widget = PLANNER_POPUP_ENTRY (box);
	GdkEvent       tmp_event;
	
	if (key_event->keyval == GDK_Escape) {
		widget->editing_canceled = TRUE;

		gtk_cell_editable_editing_done (GTK_CELL_EDITABLE (widget));
		gtk_cell_editable_remove_widget (GTK_CELL_EDITABLE (widget));
		
		return TRUE;
	}

	gtk_widget_grab_focus (widget->entry);

	/* Hackish :/ Synthesize a key press event for the entry. */
	memcpy (&tmp_event, key_event, sizeof (GdkEventKey));

	tmp_event.key.window = widget->entry->window;
	tmp_event.key.send_event = TRUE;
	
	gtk_widget_event (widget->entry, &tmp_event);

	return GTK_WIDGET_CLASS (parent_class)->key_press_event (GTK_WIDGET (widget), 
								 key_event);
}

static void
mpw_start_editing (GtkCellEditable *cell_editable,
		   GdkEvent        *event)
{
	PlannerPopupEntry *widget = PLANNER_POPUP_ENTRY (cell_editable);

	gtk_editable_select_region (GTK_EDITABLE (widget->entry), 0, -1);

	g_signal_connect (G_OBJECT (widget->entry),
			  "activate",
			  G_CALLBACK (gtk_cell_editable_entry_activated),
			  widget);
	g_signal_connect (G_OBJECT (widget->entry),
			  "key_press_event",
			  G_CALLBACK (gtk_cell_editable_key_press_event),
			  widget);
	g_signal_connect (G_OBJECT (widget->button),
			  "clicked",
			  (GCallback) mpw_arrow_clicked,
			  widget);
}

static void
mpw_cell_editable_init (GtkCellEditableIface *iface)
{
	iface->start_editing = mpw_start_editing;
}

void
planner_popup_entry_set_text (PlannerPopupEntry *popup, const gchar *text)
{
	g_return_if_fail (PLANNER_IS_POPUP_ENTRY (popup));

	gtk_entry_set_text (GTK_ENTRY (popup->entry), text ? text : "");
}

const gchar *
planner_popup_entry_get_text (PlannerPopupEntry *popup)
{
	g_return_val_if_fail (PLANNER_IS_POPUP_ENTRY (popup), NULL);

	return gtk_entry_get_text (GTK_ENTRY (popup->entry));
}

gint
planner_popup_get_button_width (void)
{
	GtkWidget *window, *button, *arrow;
	gint       width;

	GtkRequisition req;
	
	window = gtk_window_new (GTK_WINDOW_POPUP);
	
	button = gtk_button_new ();
	gtk_widget_show (button);
	gtk_container_add (GTK_CONTAINER (window), button);

	arrow = gtk_arrow_new (GTK_ARROW_DOWN, GTK_SHADOW_OUT);
	gtk_widget_show (arrow);
	gtk_container_add (GTK_CONTAINER (button), arrow);

	gtk_window_move (GTK_WINDOW (window), -500, -500);
	gtk_widget_show (window);

	gtk_widget_size_request (window, &req);
	
	width = req.width;

	gtk_widget_destroy (window);

	return width;
}

