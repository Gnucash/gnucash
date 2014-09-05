/*************************************************************************
 * The following code has come from Planner. This code implements a
 * GtkCalendar in a custom GtkCellEditable popup from GtkCellRenderer.
 *
 * These files have been renamed and changed to remove code not required
 * and to remove a dependency on libplanner.
 *
 * Copyright (C) 2012 Robert Fewell
 *
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
 **************************************************************************/
#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <gdk/gdkkeysyms.h>
#include <string.h>
#include <time.h>

#include "gnc-cell-renderer-date.h"
#include "gnc-cell-renderer-popup-entry.h"
#include "gnc-date.h"

enum {
	PROP_0,
        PROP_USE_BUTTONS,
};

static void     gcrd_init                    (GncCellRendererDate      *date);

static void     gcrd_class_init              (GncCellRendererDateClass *klass);

static void     gcrd_set_property            (GObject                 *object,
					      guint                    param_id,
					      const GValue            *value,
					      GParamSpec              *pspec);

static void     gcrd_get_property            (GObject                 *object,
					      guint                    param_id,
					      GValue                  *value,
					      GParamSpec              *pspec);

static void     gcrd_today_clicked           (GtkWidget               *button,
					      GncCellRendererDate *cell);

static void     gcrd_selected_double_click   (GtkWidget               *calendar,
					      GncCellRendererDate *cell);

static void     gcrd_cancel_clicked          (GtkWidget               *popup_window,
					      GncCellRendererDate      *cell);

static void     gcrd_ok_clicked              (GtkWidget               *popup_window,
					      GncCellRendererDate      *cell);

static void     gcrd_day_selected            (GtkWidget               *popup_window,
					      GncCellRendererDate      *cell);

GtkCellEditable *gcrd_start_editing          (GtkCellRenderer         *cell,
					      GdkEvent                *event,
					      GtkWidget               *widget,
					      const gchar             *path,
					      GdkRectangle            *background_area,
					      GdkRectangle            *cell_area,
					      GtkCellRendererState     flags);

static void     gcrd_show                    (GncCellRendererPopup     *cell,
					      const gchar             *path,
					      gint                     x1,
					      gint                     y1,
					      gint                     x2,
					      gint                     y2);
static void     gcrd_hide                    (GncCellRendererPopup     *cell);


/* These two functions are used internally */
gboolean gcrd_time2dmy (time64 raw_time, gint *day, gint *month, gint *year);
static time64 gcrd_dmy2time (gint day, gint month, gint year);

/* These two functions convert string to date to string */
static gchar * gcrd_time2dmy_string (time64 raw_time);
static time64 gcrd_string_dmy2time (const gchar *date_string);


static GncCellRendererPopupClass *parent_class;

GType
gnc_cell_renderer_date_get_type (void)
{
	static GType cell_text_type = 0;
	
	if (!cell_text_type) {
		static const GTypeInfo cell_text_info = {
			sizeof (GncCellRendererDateClass),
			NULL,		/* base_init */
			NULL,		/* base_finalize */
			(GClassInitFunc) gcrd_class_init,
			NULL,		/* class_finalize */
			NULL,		/* class_data */
			sizeof (GncCellRendererDate),
			0,              /* n_preallocs */
			(GInstanceInitFunc) gcrd_init,
		};
		
		cell_text_type = g_type_register_static (GNC_TYPE_CELL_RENDERER_POPUP,
							 "GncCellRendererDate",
							 &cell_text_info,
							 0);
	}
	
	return cell_text_type;
}

static void
gcrd_init (GncCellRendererDate *date)
{
	GncCellRendererPopup *popup;
	GtkWidget                *frame;
	GtkWidget                *vbox;
	GtkWidget                *bbox;
	GtkWidget                *button;

	popup = GNC_CELL_RENDERER_POPUP (date);

	frame = gtk_frame_new (NULL);
	gtk_container_add (GTK_CONTAINER (popup->popup_window), frame);
	gtk_frame_set_shadow_type (GTK_FRAME (frame), GTK_SHADOW_OUT);

	vbox = gtk_vbox_new (FALSE, 6);
	gtk_container_add (GTK_CONTAINER (frame), vbox);
	gtk_container_set_border_width (GTK_CONTAINER (vbox), 6);
	
	date->calendar = gtk_calendar_new ();
	popup->focus_window = date->calendar;
	gtk_box_pack_start (GTK_BOX (vbox), date->calendar, TRUE, TRUE, 0);

        date->button_box = gtk_hbutton_box_new ();
	gtk_box_set_spacing (GTK_BOX (date->button_box), 6);
	gtk_box_pack_start (GTK_BOX (vbox), date->button_box, FALSE, FALSE, 0);

	button = gtk_button_new_with_label (_("Cancel"));
	gtk_container_add (GTK_CONTAINER (date->button_box), button);
	g_signal_connect (button, "clicked",
			  G_CALLBACK (gcrd_cancel_clicked),
			  date);

	date->today_button = gtk_button_new_with_label (_("Today"));
	gtk_container_add (GTK_CONTAINER (date->button_box), date->today_button);
	g_signal_connect (date->today_button, "clicked",
			  G_CALLBACK (gcrd_today_clicked),
			  date);

	button = gtk_button_new_with_label (_("Select"));
	gtk_container_add (GTK_CONTAINER (date->button_box), button);
	g_signal_connect (button, "clicked",
			  G_CALLBACK (gcrd_ok_clicked),
			  date);

	g_signal_connect (date->calendar, "day-selected",
			  G_CALLBACK (gcrd_day_selected),
			  date);
	g_signal_connect (date->calendar, "day-selected-double-click", 
			  G_CALLBACK (gcrd_selected_double_click),
			  date);

	//Set calendar to show current date when displayed
	date->time = gnc_time (NULL);

        gtk_widget_show_all (frame);
}

static void
gcrd_class_init (GncCellRendererDateClass *klass)
{
	GncCellRendererPopupClass     *popup_class;
	GtkCellRendererClass          *cell_class;
	GObjectClass                  *gobject_class;

	popup_class = GNC_CELL_RENDERER_POPUP_CLASS (klass);
	cell_class = GTK_CELL_RENDERER_CLASS (klass);	
	parent_class = GNC_CELL_RENDERER_POPUP_CLASS (g_type_class_peek_parent (klass));
	gobject_class = G_OBJECT_CLASS (klass);

	gobject_class->set_property = gcrd_set_property;
	gobject_class->get_property = gcrd_get_property;

	cell_class->start_editing = gcrd_start_editing;
		
	popup_class->show_popup = gcrd_show;
	popup_class->hide_popup = gcrd_hide;

	g_object_class_install_property (
		gobject_class,
                 PROP_USE_BUTTONS,
                 g_param_spec_boolean ("use-buttons",
				       NULL,
				       NULL,
				       TRUE,
				       G_PARAM_READWRITE));

}

static void
gcrd_set_property (GObject      *object,
		   guint         param_id,
		   const GValue *value,
		   GParamSpec   *pspec)
{
	GncCellRendererDate *date;

	date = GNC_CELL_RENDERER_DATE (object);
	
	switch (param_id) {
	case PROP_USE_BUTTONS:
		date->use_buttons = g_value_get_boolean (value);

		if (date->use_buttons)
			gtk_widget_show (date->button_box);
		else
			gtk_widget_hide (date->button_box);
		break;

	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
		break;
	}
}

static void
gcrd_get_property (GObject    *object,
		   guint       param_id,
		   GValue     *value,
		   GParamSpec *pspec)
{
	GncCellRendererDate *date;

	date = GNC_CELL_RENDERER_DATE (object);
	
	switch (param_id) {
	case PROP_USE_BUTTONS:
		g_value_set_boolean (value, date->use_buttons);
		break;

	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
		break;
	}
}

GtkCellEditable *
gcrd_start_editing (GtkCellRenderer      *cell,
		    GdkEvent             *event,
		    GtkWidget            *widget,
		    const gchar          *path,
		    GdkRectangle         *background_area,
		    GdkRectangle         *cell_area,
		    GtkCellRendererState  flags)
{
	GNC_CELL_RENDERER_POPUP (cell)->editing_canceled = FALSE;
	
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
gcrd_hide (GncCellRendererPopup *cell)
{
	if (parent_class->hide_popup) {
		parent_class->hide_popup (cell);
	}
}

static void
gcrd_show (GncCellRendererPopup *cell,
	   const gchar              *path,
	   gint                      x1,
	   gint                      y1,
	   gint                      x2,
	   gint                      y2)
{
	GncCellRendererDate     *date;
	gint                     year;
	gint                     month;
	gint                     day;
	gint                     index;
	const gchar             *text;

	if (parent_class->show_popup) {
		parent_class->show_popup (cell,
					  path,
					  x1, y1,
					  x2, y2);
	}

	date = GNC_CELL_RENDERER_DATE (cell);

	text = gnc_popup_entry_get_text (GNC_POPUP_ENTRY (GNC_CELL_RENDERER_POPUP (cell)->editable));

        if (!(g_strcmp0(text, "")))
        {
	    date->time = gnc_time (NULL);
            gcrd_time2dmy ( date->time, &day, &month, &year);
        }
        else
        {
            date->time = gcrd_string_dmy2time (text);
            gcrd_time2dmy ( date->time, &day, &month, &year);
        }

	gtk_calendar_clear_marks (GTK_CALENDAR (date->calendar));
	gtk_calendar_select_month (GTK_CALENDAR (date->calendar), month - 1, year);

	gtk_calendar_select_day (GTK_CALENDAR (date->calendar), day);
	gtk_calendar_mark_day (GTK_CALENDAR (date->calendar), day);
	
}

GtkCellRenderer *
gnc_cell_renderer_date_new (gboolean use_buttons)
{
	GObject *cell;

	cell = g_object_new (GNC_TYPE_CELL_RENDERER_DATE,
			     "use-buttons", use_buttons,
			     NULL);
	
	return GTK_CELL_RENDERER (cell);
}

static void
gcrd_today_clicked (GtkWidget *button, GncCellRendererDate *cell)
{
	time64  today;
	gint    year, month, day;
	
	today = gnc_time (NULL);

        gcrd_time2dmy ( today, &day, &month, &year);
	
	gtk_calendar_clear_marks (GTK_CALENDAR (cell->calendar));
	gtk_calendar_select_month (GTK_CALENDAR (cell->calendar), month - 1, year);
	gtk_calendar_select_day (GTK_CALENDAR (cell->calendar), day);
	gtk_calendar_mark_day (GTK_CALENDAR (cell->calendar), day);
}

static void
gcrd_selected_double_click (GtkWidget *calendar, GncCellRendererDate *cell)
{
	GncCellRendererPopup *popup;
	
	popup = GNC_CELL_RENDERER_POPUP (cell);

	gcrd_ok_clicked (popup->popup_window, cell);
}

static void
gcrd_cancel_clicked (GtkWidget *popup_window, GncCellRendererDate *cell)
{
	GncCellRendererPopup *popup;
	
	popup = GNC_CELL_RENDERER_POPUP (cell);

	popup->editing_canceled = TRUE;
	gnc_cell_renderer_popup_hide (popup);
}

static void
gcrd_ok_clicked (GtkWidget *popup_window, GncCellRendererDate *cell)
{
	GncCellRendererPopup *popup;
	
	popup = GNC_CELL_RENDERER_POPUP (cell);

	gcrd_day_selected (popup_window, cell);

	popup->editing_canceled = FALSE;
	gnc_cell_renderer_popup_hide (popup);
}

static void
gcrd_day_selected (GtkWidget *popup_window, GncCellRendererDate *cell)
{
	guint    year;
	guint    month;
	guint    day;
	time64   t;
	gchar   *str;

	gtk_calendar_get_date (GTK_CALENDAR (cell->calendar),
			       &year,
			       &month,
			       &day);

        t = gcrd_dmy2time ( day, month + 1, year);

	cell->time = t;

	str = gcrd_time2dmy_string (t);

	gnc_popup_entry_set_text (
		GNC_POPUP_ENTRY (GNC_CELL_RENDERER_POPUP (cell)->editable), str);
	g_free (str);

}

static gboolean
gcrd_grab_on_window (GdkWindow *window,
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


/* This function converts a time64 value date to separate entities */
gboolean
gcrd_time2dmy (time64 raw_time, gint *day, gint *month, gint *year)
{
    struct tm * timeinfo;
  
    timeinfo = gnc_localtime (&raw_time);
 
    *day = timeinfo->tm_mday;
    *month = timeinfo->tm_mon + 1;
    *year = timeinfo->tm_year + 1900;
    gnc_tm_free (timeinfo);
    return TRUE;
}

/* This function converts separate entities to a time64 value */
static time64
gcrd_dmy2time (gint day, gint month, gint year)
{
    struct tm when;

    memset (&when, 0, sizeof (when));
    when.tm_year = year - 1900;
    when.tm_mon = month - 1 ;
    when.tm_mday = day;

    return gnc_mktime (&when);
}


/* This function converts a time64 value date to a string */
static gchar *
gcrd_time2dmy_string (time64 raw_time)
{
    return qof_print_date (raw_time);
}


/* This function converts a string date to a time64 value */
static time64
gcrd_string_dmy2time (const gchar *date_string)
{
    gint year = 0, month = 0, day = 0;

    if(qof_scan_date (date_string, &day, &month, &year))
    {
	struct tm when;
	memset (&when, 0, sizeof (when));
        when.tm_year = year - 1900;
        when.tm_mon = month - 1 ;
        when.tm_mday = day;

        return gnc_mktime (&when);
    }
    else
    {
	return gnc_time (NULL);
    }
}





