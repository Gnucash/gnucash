/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * Copyright (C) 2005 Imendio AB
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
#include <stdlib.h>
#include <gdk/gdkkeysyms.h>
#include <gtk/gtkmain.h>
#include <gtk/gtkcellrenderertext.h>
#include <gtk/gtkeditable.h>
#include <gtk/gtkhseparator.h>
#include <gtk/gtkbutton.h>
#include <gtk/gtklabel.h>
#include <gtk/gtkoptionmenu.h>
#include <gtk/gtkmenuitem.h>
#include <gtk/gtkarrow.h>
#include <gtk/gtkwindow.h>
#include <gtk/gtkframe.h>
#include <gtk/gtkvbox.h>
#include <gtk/gtkhbox.h>
#include <gtk/gtkhbbox.h>
#include <gtk/gtkstock.h>
#include <gtk/gtkcalendar.h>
#include <glib/gi18n.h>
#include "planner-marshal.h"
#include "planner-cell-renderer-date.h"
#include "planner-popup-entry.h"
/* CAS: 
 This one function came from planner-format.c and was the only reason 
planner-format.h was included.  I've pasted it here with one modification - 
adding the year to the string.

#include "planner-format.h"

*/
gchar * planner_format_date (mrptime date);
gchar * 
planner_format_date (mrptime date)
{
	gchar *svalue;

	if (date == MRP_TIME_INVALID) {
		svalue = g_strdup ("");	
	} else {
		/* i18n: this string is the date nr and month name, displayed
		 * e.g. in the date cells in the task tree. See
		 * libmrproject/docs/DateFormat.
		 */
		svalue = mrp_time_format (_("%b %e %Y"), date);//CAS: added year
	}
	
	return svalue;	
}

enum {
	PROP_0,
	PROP_USE_CONSTRAINT,
};

static void     mcrd_init                    (PlannerCellRendererDate      *date);
static void     mcrd_class_init              (PlannerCellRendererDateClass *class);
static void     mcrd_set_property            (GObject                 *object,
					      guint                    param_id,
					      const GValue            *value,
					      GParamSpec              *pspec);
static void     mcrd_get_property            (GObject                 *object,
					      guint                    param_id,
					      GValue                  *value,
					      GParamSpec              *pspec);
static void     mcrd_today_clicked           (GtkWidget               *button,
					      PlannerCellRendererDate *cell);
static void     mcrd_selected_double_click   (GtkWidget               *calendar,
					      PlannerCellRendererDate *cell);
static void     mcrd_cancel_clicked          (GtkWidget               *popup_window,
					      PlannerCellRendererDate      *cell);
static void     mcrd_ok_clicked              (GtkWidget               *popup_window,
					      PlannerCellRendererDate      *cell);
static void     mcrd_day_selected            (GtkWidget               *popup_window,
					      PlannerCellRendererDate      *cell);
static void     mcrd_constraint_activated_cb (GtkWidget               *widget,
					      PlannerCellRendererDate      *cell);
GtkCellEditable *mcrd_start_editing          (GtkCellRenderer         *cell,
					      GdkEvent                *event,
					      GtkWidget               *widget,
					      const gchar             *path,
					      GdkRectangle            *background_area,
					      GdkRectangle            *cell_area,
					      GtkCellRendererState     flags);
static void     mcrd_show                    (PlannerCellRendererPopup     *cell,
					      const gchar             *path,
					      gint                     x1,
					      gint                     y1,
					      gint                     x2,
					      gint                     y2);
static void     mcrd_hide                    (PlannerCellRendererPopup     *cell);
static void     mcrd_setup_option_menu       (GtkWidget               *option_menu,
					      GtkSignalFunc            func,
					      gpointer                 user_data,
					      gpointer                 str1, ...);


static PlannerCellRendererPopupClass *parent_class;

GType
planner_cell_renderer_date_get_type (void)
{
	static GType cell_text_type = 0;
	
	if (!cell_text_type) {
		static const GTypeInfo cell_text_info = {
			sizeof (PlannerCellRendererDateClass),
			NULL,		/* base_init */
			NULL,		/* base_finalize */
			(GClassInitFunc) mcrd_class_init,
			NULL,		/* class_finalize */
			NULL,		/* class_data */
			sizeof (PlannerCellRendererDate),
			0,              /* n_preallocs */
			(GInstanceInitFunc) mcrd_init,
		};
		
		cell_text_type = g_type_register_static (PLANNER_TYPE_CELL_RENDERER_POPUP,
							 "PlannerCellRendererDate",
							 &cell_text_info,
							 0);
	}
	
	return cell_text_type;
}

static void
mcrd_init (PlannerCellRendererDate *date)
{
	PlannerCellRendererPopup *popup;
	GtkWidget                *frame;
	GtkWidget                *vbox;
	GtkWidget                *hbox;
	GtkWidget                *bbox;
	GtkWidget                *button;

	popup = PLANNER_CELL_RENDERER_POPUP (date);

	frame = gtk_frame_new (NULL);
	gtk_container_add (GTK_CONTAINER (popup->popup_window), frame);
	gtk_frame_set_shadow_type (GTK_FRAME (frame), GTK_SHADOW_OUT);

	vbox = gtk_vbox_new (FALSE, 6);
	gtk_container_add (GTK_CONTAINER (frame), vbox);
	gtk_container_set_border_width (GTK_CONTAINER (vbox), 6);
	
	date->calendar = gtk_calendar_new ();
	popup->focus_window = date->calendar;
	gtk_box_pack_start (GTK_BOX (vbox), date->calendar, TRUE, TRUE, 0);

	date->constraint_vbox = gtk_vbox_new (FALSE, 0);
	gtk_box_pack_start (GTK_BOX (vbox), date->constraint_vbox, FALSE, FALSE, 0);
	
	hbox = gtk_hbox_new (FALSE, 6);
	/* I18n: the verb "schedule" here. */
	gtk_box_pack_start (GTK_BOX (hbox), gtk_label_new (_("Schedule:")),
			    FALSE, FALSE, 0);
	
	date->option_menu = gtk_option_menu_new ();
	mcrd_setup_option_menu (date->option_menu,
				G_CALLBACK (mcrd_constraint_activated_cb),
				date,
				_("As soon as possible"), MRP_CONSTRAINT_ASAP,
				_("No earlier than"), MRP_CONSTRAINT_SNET,
				_("On fixed date"), MRP_CONSTRAINT_MSO,
				NULL);
	gtk_box_pack_end (GTK_BOX (hbox), date->option_menu, TRUE, TRUE, 0);

	gtk_box_pack_start (GTK_BOX (date->constraint_vbox), hbox, TRUE, TRUE, 0);
	
	hbox = gtk_hbox_new (FALSE, 0);
	gtk_box_pack_start (GTK_BOX (date->constraint_vbox), hbox, FALSE, FALSE, 0);
	
	bbox = gtk_hbutton_box_new ();
	gtk_box_set_spacing (GTK_BOX (bbox), 6);
	gtk_box_pack_start (GTK_BOX (vbox), bbox, FALSE, FALSE, 0);

	date->today_button = gtk_button_new_with_label (_("Today"));
	gtk_container_add (GTK_CONTAINER (bbox), date->today_button);
	g_signal_connect (date->today_button, "clicked",
			  G_CALLBACK (mcrd_today_clicked),
			  date);
	
	button = gtk_button_new_with_label (_("Cancel"));
	gtk_container_add (GTK_CONTAINER (bbox), button);
	g_signal_connect (button,
			  "clicked",
			  G_CALLBACK (mcrd_cancel_clicked),
			  date);

	button = gtk_button_new_with_label (_("Select"));
	gtk_container_add (GTK_CONTAINER (bbox), button);
	g_signal_connect (button, "clicked",
			  G_CALLBACK (mcrd_ok_clicked),
			  date);

	g_signal_connect (date->calendar, "day-selected",
			  G_CALLBACK (mcrd_day_selected),
			  date);
	g_signal_connect (date->calendar, "day-selected-double-click", 
			  G_CALLBACK (mcrd_selected_double_click),
			  date);

	//Set calendar to show current date when displayed
	date->time = mrp_time_current_time();

        gtk_widget_show_all (frame);
}

static void
mcrd_class_init (PlannerCellRendererDateClass *class)
{
	PlannerCellRendererPopupClass *popup_class;
	GtkCellRendererClass          *cell_class;
	GObjectClass                  *gobject_class;

	popup_class = PLANNER_CELL_RENDERER_POPUP_CLASS (class);
	cell_class = GTK_CELL_RENDERER_CLASS (class);	
	parent_class = PLANNER_CELL_RENDERER_POPUP_CLASS (g_type_class_peek_parent (class));
	gobject_class = G_OBJECT_CLASS (class);

	gobject_class->set_property = mcrd_set_property;
	gobject_class->get_property = mcrd_get_property;

	cell_class->start_editing = mcrd_start_editing;
		
	popup_class->show_popup = mcrd_show;
	popup_class->hide_popup = mcrd_hide;

	g_object_class_install_property (
		gobject_class,
                 PROP_USE_CONSTRAINT,
                 g_param_spec_boolean ("use-constraint",
				       NULL,
				       NULL,
				       TRUE,
				       G_PARAM_READWRITE |
				       G_PARAM_CONSTRUCT_ONLY));
}

static void
mcrd_set_property (GObject      *object,
		   guint         param_id,
		   const GValue *value,
		   GParamSpec   *pspec)
{
	PlannerCellRendererDate *date;

	date = PLANNER_CELL_RENDERER_DATE (object);
	
	switch (param_id) {
	case PROP_USE_CONSTRAINT:
		date->use_constraint = g_value_get_boolean (value);

		if (date->use_constraint) {
			gtk_widget_show (date->constraint_vbox);
		} else {
			gtk_widget_hide (date->constraint_vbox);
		}

		gtk_widget_set_sensitive (date->calendar, date->use_constraint);
		gtk_widget_set_sensitive (date->today_button, date->use_constraint);

		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
		break;
	}
}

static void
mcrd_get_property (GObject    *object,
		   guint       param_id,
		   GValue     *value,
		   GParamSpec *pspec)
{
	PlannerCellRendererDate *date;

	date = PLANNER_CELL_RENDERER_DATE (object);
	
	switch (param_id) {
	case PROP_USE_CONSTRAINT:
		g_value_set_boolean (value, date->use_constraint);
		break;

	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
		break;
	}
}

GtkCellEditable *
mcrd_start_editing (GtkCellRenderer      *cell,
		    GdkEvent             *event,
		    GtkWidget            *widget,
		    const gchar          *path,
		    GdkRectangle         *background_area,
		    GdkRectangle         *cell_area,
		    GtkCellRendererState  flags)
{
	PLANNER_CELL_RENDERER_POPUP (cell)->editing_canceled = TRUE;
	
	if (GTK_CELL_RENDERER_CLASS (parent_class)->start_editing) {
		return GTK_CELL_RENDERER_CLASS (parent_class)->start_editing (
							cell,
							event,
							widget,
							path,
							background_area,
							cell_area,
							flags);
	}

	return NULL;
}


static void
mcrd_hide (PlannerCellRendererPopup *cell)
{
	if (parent_class->hide_popup) {
		parent_class->hide_popup (cell);
	}
}

static void
mcrd_show (PlannerCellRendererPopup *cell,
	   const gchar              *path,
	   gint                      x1,
	   gint                      y1,
	   gint                      x2,
	   gint                      y2)
{
	PlannerCellRendererDate *date;
	gint                     year;
	gint                     month;
	gint                     day;
	gint                     index;
	gboolean                 sensitive;

	if (parent_class->show_popup) {
		parent_class->show_popup (cell,
					  path,
					  x1, y1,
					  x2, y2);
	}

	date = PLANNER_CELL_RENDERER_DATE (cell);

	mrp_time_decompose (date->time, &year, &month, &day, NULL, NULL, NULL);
	
	index = 0;
	
	switch (date->type) {
	case MRP_CONSTRAINT_ASAP:
		index = 0;
		break;
	case MRP_CONSTRAINT_SNET:
		index = 1;
		break;
	case MRP_CONSTRAINT_MSO:
		index = 2;
		break;
	default:
		g_assert_not_reached ();
	}

	sensitive = (!date->use_constraint ||
		     (date->type != MRP_CONSTRAINT_ASAP &&
		      date->type != MRP_CONSTRAINT_ALAP));
	
	gtk_widget_set_sensitive (date->calendar, sensitive);
	gtk_widget_set_sensitive (date->today_button, sensitive);
	
	gtk_calendar_clear_marks (GTK_CALENDAR (date->calendar));
	gtk_calendar_select_month (GTK_CALENDAR (date->calendar),
				   month - 1, year);
	gtk_calendar_select_day (GTK_CALENDAR (date->calendar), day);
	gtk_calendar_mark_day (GTK_CALENDAR (date->calendar), day);
	
	gtk_option_menu_set_history (GTK_OPTION_MENU (date->option_menu),
				     index);
}

GtkCellRenderer *
planner_cell_renderer_date_new (gboolean use_constraint)
{
	GObject *cell;

	cell = g_object_new (PLANNER_TYPE_CELL_RENDERER_DATE,
			     "use-constraint", use_constraint,
			     NULL);
	
	return GTK_CELL_RENDERER (cell);
}

static void
mcrd_today_clicked (GtkWidget               *button,
		    PlannerCellRendererDate *cell)
{
	mrptime today;
	gint    year, month, day;
	
	today = mrp_time_current_time ();

	mrp_time_decompose (today, &year, &month, &day,
			    NULL, NULL, NULL);
	
	gtk_calendar_clear_marks (GTK_CALENDAR (cell->calendar));
	gtk_calendar_select_month (GTK_CALENDAR (cell->calendar),
				   month - 1, year);
	gtk_calendar_select_day (GTK_CALENDAR (cell->calendar), day);
	gtk_calendar_mark_day (GTK_CALENDAR (cell->calendar), day);
}

static void
mcrd_selected_double_click (GtkWidget               *calendar,
			    PlannerCellRendererDate *cell)
{
	PlannerCellRendererPopup *popup;
	
	popup = PLANNER_CELL_RENDERER_POPUP (cell);

	mcrd_ok_clicked (popup->popup_window, cell);
}

static void
mcrd_cancel_clicked (GtkWidget               *popup_window,
		     PlannerCellRendererDate *cell)
{
	PlannerCellRendererPopup *popup;
	
	popup = PLANNER_CELL_RENDERER_POPUP (cell);

	popup->editing_canceled = TRUE;
	planner_cell_renderer_popup_hide (popup);
}

static void
mcrd_ok_clicked (GtkWidget               *popup_window,
		 PlannerCellRendererDate *cell)
{
	PlannerCellRendererPopup *popup;
	
	popup = PLANNER_CELL_RENDERER_POPUP (cell);

	mcrd_day_selected (popup_window, cell);

	popup->editing_canceled = FALSE;
	planner_cell_renderer_popup_hide (popup);
}

static void
mcrd_day_selected (GtkWidget               *popup_window,
		   PlannerCellRendererDate *cell)
{
	guint    year;
	guint    month;
	guint    day;
	mrptime  t;
	gchar   *str;

	gtk_calendar_get_date (GTK_CALENDAR (cell->calendar),
			       &year,
			       &month,
			       &day);

	t = mrp_time_compose (year, month + 1, day, 0, 0, 0);

	cell->time = t;

	str = planner_format_date (t);
	planner_popup_entry_set_text (
		PLANNER_POPUP_ENTRY (PLANNER_CELL_RENDERER_POPUP (cell)->editable), str);
	g_free (str);
}

static gboolean
mcrd_grab_on_window (GdkWindow *window,
		     guint32    activate_time)
{
	if ((gdk_pointer_grab (window, TRUE,
			       GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK |
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
mcrd_constraint_activated_cb (GtkWidget               *widget,
			      PlannerCellRendererDate *cell)
{
	gpointer data;
	gboolean sensitive;
	
	data = g_object_get_data (G_OBJECT (widget), "data");

	cell->type = GPOINTER_TO_INT (data);

	sensitive = (!cell->use_constraint ||
		     (cell->type != MRP_CONSTRAINT_ASAP &&
		      cell->type != MRP_CONSTRAINT_ALAP));
	
	gtk_widget_set_sensitive (cell->calendar, sensitive);
	gtk_widget_set_sensitive (cell->today_button, sensitive);
	
	/* A bit hackish. Grab focus on the popup window again when the
	 * optionmenu is activated, since focus is transferred to the optionmenu
	 * when it's popped up.
	 */
	mcrd_grab_on_window (PLANNER_CELL_RENDERER_POPUP (cell)->popup_window->window,
			     gtk_get_current_event_time ());
}

/* Utility function to use before optionmenus work with libglade again.
 */

static void
mcrd_setup_option_menu (GtkWidget     *option_menu,
			GtkSignalFunc  func,
			gpointer       user_data,
			gpointer       str1, ...)
{
	GtkWidget *menu, *menu_item;
	gint       i;
	va_list    args;
	gpointer   str;
	gint       type;

       	menu = gtk_option_menu_get_menu (GTK_OPTION_MENU (option_menu));
	if (menu) {
		gtk_widget_destroy (menu);
	}
	
	menu = gtk_menu_new ();

	va_start (args, str1);
	for (str = str1, i = 0; str != NULL; str = va_arg (args, gpointer), i++) {
		menu_item = gtk_menu_item_new_with_label (str);
		gtk_widget_show (menu_item);
		gtk_menu_append (GTK_MENU (menu), menu_item);

		type = va_arg (args, gint);
		
		g_object_set_data (G_OBJECT (menu_item),
				   "data",
				   GINT_TO_POINTER (type));
		gtk_signal_connect (GTK_OBJECT (menu_item),
				    "activate",
				    func,
				    user_data);
	}
	va_end (args);

	gtk_widget_show (menu);
	gtk_option_menu_set_menu (GTK_OPTION_MENU (option_menu), menu);
}

