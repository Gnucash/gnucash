/*
 * gnc-dateedit.c -- Date editor widget
 *
 * Copyright (C) 1997, 1998, 1999, 2000 Free Software Foundation
 * All rights reserved.
 *
 * Gnucash is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * as published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * Gnucash is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
 * Boston, MA  02111-1307,  USA       gnu@gnu.org
 *
 */
/*
  @NOTATION@
 */

/*
 * Date editor widget
 *
 * Authors: Miguel de Icaza
 *          Dave Peticolas <dave@krondo.com>
 */

#include "config.h"

#include <string.h>
#include <stdlib.h> /* atoi */
#include <ctype.h> /* isdigit */
#include <stdio.h>
#include <time.h>

#include "gnc-dateedit.h"
#include "messages.h"
#include "date.h"


enum {
	DATE_CHANGED,
	TIME_CHANGED,
	LAST_SIGNAL
};

static gint date_edit_signals [LAST_SIGNAL] = { 0 };


static void gnc_date_edit_init         (GNCDateEdit      *gde);
static void gnc_date_edit_class_init   (GNCDateEditClass *class);
static void gnc_date_edit_destroy      (GtkObject          *object);
static void gnc_date_edit_forall       (GtkContainer       *container,
                                        gboolean	    include_internals,
                                        GtkCallback	    callback,
                                        gpointer	    callbabck_data);
static struct tm gnc_date_edit_get_date_internal (GNCDateEdit *gde);
static int date_accel_key_press(GtkWidget *widget,
                                GdkEventKey *event,
                                gpointer data);


static GtkHBoxClass *parent_class;

/**
 * gnc_date_edit_get_type:
 *
 * Returns the GtkType for the GNCDateEdit widget
 */
guint
gnc_date_edit_get_type (void)
{
	static guint date_edit_type = 0;

	if (!date_edit_type){
		GtkTypeInfo date_edit_info = {
			"GNCDateEdit",
			sizeof (GNCDateEdit),
			sizeof (GNCDateEditClass),
			(GtkClassInitFunc) gnc_date_edit_class_init,
			(GtkObjectInitFunc) gnc_date_edit_init,
			NULL,
			NULL,
		};

		date_edit_type = gtk_type_unique (gtk_hbox_get_type (),
                                                  &date_edit_info);
	}
	
	return date_edit_type;
}


static char *
gnc_strtok_r (char *s, const char *delim, char **save_ptr)
{
  char *token;

  if (s == NULL)
    s = *save_ptr;

  /* Scan leading delimiters.  */
  s += strspn (s, delim);
  if (*s == '\0')
    return NULL;

  /* Find the end of the token.  */
  token = s;
  s = strpbrk (token, delim);
  if (s == NULL)
    /* This token finishes the string.  */
    *save_ptr = strchr (token, '\0');
  else
    {
      /* Terminate the token and make *SAVE_PTR point past it.  */
      *s = '\0';
      *save_ptr = s + 1;
    }
  return token;
}

static void
hide_popup (GNCDateEdit *gde)
{
	gtk_widget_hide (gde->cal_popup);
	gtk_grab_remove (gde->cal_popup);
	gdk_pointer_ungrab (GDK_CURRENT_TIME);
}

static void
day_selected (GtkCalendar *calendar, GNCDateEdit *gde)
{
	char buffer [40];
	gint year, month, day;

	gtk_calendar_get_date (calendar, &year, &month, &day);

        printDate (buffer, day, month + 1, year);
	gtk_entry_set_text (GTK_ENTRY (gde->date_entry), buffer);
	gtk_signal_emit (GTK_OBJECT (gde), date_edit_signals [DATE_CHANGED]);
}

static void
day_selected_double_click (GtkCalendar *calendar, GNCDateEdit *gde)
{
	hide_popup (gde);
}

static gint
delete_popup (GtkWidget *widget, gpointer data)
{
	GNCDateEdit *gde;

	gde = data;
	hide_popup (gde);

	return TRUE;
}

static gint
key_press_popup (GtkWidget *widget, GdkEventKey *event, gpointer data)
{
	GNCDateEdit *gde;

	if (event->keyval != GDK_Escape)
		return date_accel_key_press(widget, event, data);

	gde = data;
	gtk_signal_emit_stop_by_name (GTK_OBJECT (widget), "key_press_event");
	hide_popup (gde);

	return TRUE;
}

/* This function is yanked from gtkcombo.c */
static gint
button_press_popup (GtkWidget *widget, GdkEventButton *event, gpointer data)
{
	GNCDateEdit *gde;
	GtkWidget *child;

	gde = data;

	child = gtk_get_event_widget ((GdkEvent *) event);

	/* We don't ask for button press events on the grab widget, so
	 *  if an event is reported directly to the grab widget, it must
	 *  be on a window outside the application (and thus we remove
	 *  the popup window). Otherwise, we check if the widget is a child
	 *  of the grab widget, and only remove the popup window if it
	 *  is not.
	 */
	if (child != widget) {
		while (child) {
			if (child == widget)
				return FALSE;
			child = child->parent;
		}
	}

	hide_popup (gde);

	return TRUE;
}

static void
position_popup (GNCDateEdit *gde)
{
	gint x, y;
	gint bwidth, bheight;

	gtk_widget_size_request (gde->cal_popup, &gde->cal_popup->requisition);

	gdk_window_get_origin (gde->date_button->window, &x, &y);
	gdk_window_get_size (gde->date_button->window, &bwidth, &bheight);

	x += bwidth - gde->cal_popup->requisition.width;
	y += bheight;

	if (x < 0)
		x = 0;

	if (y < 0)
		y = 0;

	gtk_widget_set_uposition (gde->cal_popup, x, y);
}

static void
select_clicked (GtkWidget *widget, GNCDateEdit *gde)
{
	struct tm mtm;
        /*	GdkCursor *cursor; */

        /* This code is pretty much just copied from gtk_date_edit_get_date */
      	scanDate (gtk_entry_get_text (GTK_ENTRY (gde->date_entry)),
                  &mtm.tm_mday, &mtm.tm_mon, &mtm.tm_year);

        mtm.tm_mon--;

	/* Hope the user does not actually mean years early in the A.D. days...
	 * This date widget will obviously not work for a history program :-)
	 */
	if (mtm.tm_year >= 1900)
		mtm.tm_year -= 1900;

        mtm.tm_sec = 0;
        mtm.tm_min = 0;
        mtm.tm_hour = 0;
        mtm.tm_isdst = -1;

        if (mktime (&mtm) == -1)
        {
                time_t secs = time (NULL);

                mtm = *localtime (&secs);
                mtm.tm_sec = 0;
                mtm.tm_min = 0;
                mtm.tm_hour = 0;
                mtm.tm_isdst = -1;

                gnc_date_edit_set_time (gde, mktime (&mtm));
        }

        gtk_calendar_select_day (GTK_CALENDAR (gde->calendar), 1);
	gtk_calendar_select_month (GTK_CALENDAR (gde->calendar), mtm.tm_mon,
                                   1900 + mtm.tm_year);
        gtk_calendar_select_day (GTK_CALENDAR (gde->calendar), mtm.tm_mday);

        position_popup (gde);

	gtk_widget_show (gde->cal_popup);
	gtk_widget_grab_focus (gde->cal_popup);
	gtk_grab_add (gde->cal_popup);

#if 0
	cursor = gnome_stock_cursor_new (GNOME_STOCK_CURSOR_DEFAULT);

	gdk_pointer_grab (gde->cal_popup->window, TRUE,
			  (GDK_BUTTON_PRESS_MASK
			   | GDK_BUTTON_RELEASE_MASK
			   | GDK_POINTER_MOTION_MASK),
			  NULL, cursor, GDK_CURRENT_TIME);

        gdk_cursor_destroy (cursor);
#endif
}

typedef struct {
	char *hour;
	GNCDateEdit *gde;
} hour_info_t;

static void
set_time (GtkWidget *widget, hour_info_t *hit)
{
	gtk_entry_set_text (GTK_ENTRY (hit->gde->time_entry), hit->hour);
	gtk_signal_emit (GTK_OBJECT (hit->gde),
                         date_edit_signals [TIME_CHANGED]);
}

static void
free_resources (GtkWidget *widget, hour_info_t *hit)
{
	g_free (hit->hour);
	g_free (hit);
}

static void
fill_time_popup (GtkWidget *widget, GNCDateEdit *gde)
{
	GtkWidget *menu;
	struct tm *mtm;
	time_t current_time;
	int i, j;

	if (gde->lower_hour > gde->upper_hour)
		return;

	menu = gtk_menu_new ();
	gtk_option_menu_set_menu (GTK_OPTION_MENU (gde->time_popup), menu);

	time (&current_time);
	mtm = localtime (&current_time);
	
	for (i = gde->lower_hour; i <= gde->upper_hour; i++){
		GtkWidget *item, *submenu;
		hour_info_t *hit;
		char buffer [40];

		mtm->tm_hour = i;
		mtm->tm_min  = 0;
		hit = g_new (hour_info_t, 1);

		if (gde->flags & GNC_DATE_EDIT_24_HR)
			strftime (buffer, sizeof (buffer), "%H:00", mtm);
		else
			strftime (buffer, sizeof (buffer), "%I:00 %p ", mtm);
		hit->hour = g_strdup (buffer);
		hit->gde  = gde;

		item = gtk_menu_item_new_with_label (buffer);
		gtk_menu_append (GTK_MENU (menu), item);
#if 0
		gtk_signal_connect (GTK_OBJECT (item), "activate",
				    GTK_SIGNAL_FUNC (set_time), hit);
#endif
		gtk_signal_connect (GTK_OBJECT (item), "destroy",
				    GTK_SIGNAL_FUNC (free_resources), hit);
		gtk_widget_show (item);

		submenu = gtk_menu_new ();
		gtk_menu_item_set_submenu (GTK_MENU_ITEM (item), submenu);
		for (j = 0; j < 60; j += 15){
			GtkWidget *mins;

			mtm->tm_min = j;
			hit = g_new (hour_info_t, 1);
			if (gde->flags & GNC_DATE_EDIT_24_HR)
				strftime (buffer, sizeof (buffer),
                                          "%H:%M", mtm);
			else
				strftime (buffer, sizeof (buffer),
                                          "%I:%M %p", mtm);
			hit->hour = g_strdup (buffer);
			hit->gde  = gde;

			mins = gtk_menu_item_new_with_label (buffer);
			gtk_menu_append (GTK_MENU (submenu), mins);
			gtk_signal_connect (GTK_OBJECT (mins), "activate",
					    GTK_SIGNAL_FUNC (set_time), hit);
			gtk_signal_connect (GTK_OBJECT (item), "destroy",
					    GTK_SIGNAL_FUNC (free_resources),
                                            hit);
			gtk_widget_show (mins);
		}
	}
}

static void
gnc_date_edit_class_init (GNCDateEditClass *class)
{
	GtkObjectClass *object_class = (GtkObjectClass *) class;
	GtkContainerClass *container_class = (GtkContainerClass *) class;

	object_class = (GtkObjectClass*) class;

	parent_class = gtk_type_class (gtk_hbox_get_type ());

	date_edit_signals [TIME_CHANGED] =
		gtk_signal_new ("time_changed",
				GTK_RUN_FIRST, object_class->type, 
				GTK_SIGNAL_OFFSET (GNCDateEditClass,
                                                   time_changed),
				gtk_signal_default_marshaller,
                                GTK_TYPE_NONE, 0);

	date_edit_signals [DATE_CHANGED] =
		gtk_signal_new ("date_changed",
				GTK_RUN_FIRST, object_class->type, 
				GTK_SIGNAL_OFFSET (GNCDateEditClass,
                                                   date_changed),
				gtk_signal_default_marshaller,
                                GTK_TYPE_NONE, 0);
	
	gtk_object_class_add_signals (object_class, date_edit_signals,
                                      LAST_SIGNAL);

	container_class->forall = gnc_date_edit_forall;

	object_class->destroy = gnc_date_edit_destroy;

	class->date_changed = NULL;
	class->time_changed = NULL;
}

static void
gnc_date_edit_init (GNCDateEdit *gde)
{
	gde->lower_hour = 7;
	gde->upper_hour = 19;
	gde->flags = GNC_DATE_EDIT_SHOW_TIME;
}

static void
gnc_date_edit_destroy (GtkObject *object)
{
	GNCDateEdit *gde;

	g_return_if_fail (object != NULL);
	g_return_if_fail (GNC_IS_DATE_EDIT (object));

	gde = GNC_DATE_EDIT (object);

	gtk_widget_destroy (gde->cal_popup);

	if (GTK_OBJECT_CLASS (parent_class)->destroy)
		(* GTK_OBJECT_CLASS (parent_class)->destroy) (object);
}

static void
gnc_date_edit_forall (GtkContainer *container, gboolean include_internals,
                      GtkCallback callback, gpointer callback_data)
{
	g_return_if_fail (container != NULL);
	g_return_if_fail (GNC_IS_DATE_EDIT (container));
	g_return_if_fail (callback != NULL);

	/* Let GtkBox handle things only if the internal widgets need
	 * to be poked.  */
	if (!include_internals)
                return;

        if (!GTK_CONTAINER_CLASS (parent_class)->forall)
                return;

        GTK_CONTAINER_CLASS (parent_class)->forall (container,
                                                    include_internals,
                                                    callback,
                                                    callback_data);
}

/**
 * gnc_date_edit_set_time:
 * @gde: the GNCDateEdit widget
 * @the_time: The time and date that should be set on the widget
 *
 * Changes the displayed date and time in the GNCDateEdit widget
 * to be the one represented by @the_time.
 */
void
gnc_date_edit_set_time (GNCDateEdit *gde, time_t the_time)
{
	struct tm *mytm;
	char buffer [40];

	g_return_if_fail(gde != NULL);

	if (the_time == 0)
		the_time = time (NULL);
	gde->initial_time = the_time;

	mytm = localtime (&the_time);

	/* Set the date */
        printDate (buffer,
                   mytm->tm_mday,
                   mytm->tm_mon + 1,
                   1900 + mytm->tm_year);
	gtk_entry_set_text (GTK_ENTRY (gde->date_entry), buffer);

	/* Set the time */
	if (gde->flags & GNC_DATE_EDIT_24_HR)
		strftime (buffer, sizeof (buffer), "%H:%M", mytm);
	else
		strftime (buffer, sizeof (buffer), "%I:%M %p", mytm);
	gtk_entry_set_text (GTK_ENTRY (gde->time_entry), buffer);
}

/**
 * gnc_date_edit_set_popup_range:
 * @gde: The GNCDateEdit widget
 * @low_hour: low boundary for the time-range display popup.
 * @up_hour:  upper boundary for the time-range display popup.
 *
 * Sets the range of times that will be provide by the time popup
 * selectors.
 */
void
gnc_date_edit_set_popup_range (GNCDateEdit *gde, int low_hour, int up_hour)
{
        g_return_if_fail(gde != NULL);

	gde->lower_hour = low_hour;
	gde->upper_hour = up_hour;

        fill_time_popup(NULL, gde);
}

/* This code should be kept in sync with src/register/datecell.c */
static int
date_accel_key_press(GtkWidget *widget, GdkEventKey *event, gpointer data)
{
	GNCDateEdit *gde = data;
        char *string;
        struct tm tm;
        GDate gdate;

        switch (event->keyval) {
                case GDK_plus:
                case GDK_KP_Add:
                case GDK_equal:
                case GDK_KP_Equal:
                case GDK_underscore:
                case GDK_minus:
                case GDK_KP_Subtract:
                case GDK_bracketright:
                case GDK_braceright:
                case GDK_bracketleft:
                case GDK_braceleft:
                case GDK_M:
                case GDK_m:
                case GDK_H:
                case GDK_h:
                case GDK_Y:
                case GDK_y:
                case GDK_R:
                case GDK_r:
                case GDK_T:
                case GDK_t:
                        break;
                default:
                        return FALSE;
        }

        string = gtk_entry_get_text (GTK_ENTRY (widget));

        gtk_signal_emit_stop_by_name (GTK_OBJECT (widget), "key_press_event");

        tm = gnc_date_edit_get_date_internal (gde);

        g_date_set_dmy (&gdate, tm.tm_mday, tm.tm_mon + 1, tm.tm_year + 1900);

        switch (event->keyval)
        {
                case GDK_KP_Add:
                case GDK_plus:
                case GDK_equal:
                        if (event->state & GDK_SHIFT_MASK)
                                g_date_add_days (&gdate, 7);
                        else if (event->state & GDK_MOD1_MASK)
                                g_date_add_months (&gdate, 1);
                        else if (event->state & GDK_CONTROL_MASK)
                                g_date_add_years (&gdate, 1);
                        else
                                g_date_add_days (&gdate, 1);
                        break;

                case GDK_minus:
                        if ((strlen (string) != 0) && (dateSeparator () == '-'))
                        {
                                int i;
                                int len;
                                int count;

                                len = strlen (string);
                                /* rough check for existing date */
                                for (i = count = 0; i < len; i++)
                                {
                                        if (string[i] == '-')
                                                count++;
                                }

                                if (count < 2)
                                        return FALSE;
                        }

                        /* fall through */
                case GDK_KP_Subtract:
                case GDK_underscore:
                        if (event->state & GDK_SHIFT_MASK)
                                g_date_subtract_days (&gdate, 7);
                        else if (event->state & GDK_MOD1_MASK)
                                g_date_subtract_months (&gdate, 1);
                        else if (event->state & GDK_CONTROL_MASK)
                                g_date_subtract_years (&gdate, 1);
                        else
                                g_date_subtract_days (&gdate, 1);
                        break;

                case GDK_braceright:
                case GDK_bracketright:
                        /* increment month */
                        g_date_add_months (&gdate, 1);
                        break;

                case GDK_braceleft:
                case GDK_bracketleft:
                        /* decrement month */
                        g_date_subtract_months (&gdate, 1);
                        break;

                case GDK_M:
                case GDK_m:
                        /* beginning of month */
                        g_date_set_day (&gdate, 1);
                        break;

                case GDK_H:
                case GDK_h:
                        /* end of month */
                        g_date_set_day (&gdate, 1);
                        g_date_add_months (&gdate, 1);
                        g_date_subtract_days (&gdate, 1);
                        break;

                case GDK_Y:
                case GDK_y:
                        /* beginning of year */
                        g_date_set_day (&gdate, 1);
                        g_date_set_month (&gdate, 1);
                        break;

                case GDK_R:
                case GDK_r:
                        /* end of year */
                        g_date_set_day (&gdate, 1);
                        g_date_set_month (&gdate, 1);
                        g_date_add_years (&gdate, 1);
                        g_date_subtract_days (&gdate, 1);
                        break;

                case GDK_T:
                case GDK_t:
                        {
                                /* today */
                                GTime gtime;

                                gtime = time (NULL);
                                g_date_set_time (&gdate, gtime);
                                break;
                        }

                default:
                        return FALSE;
        }

        g_date_to_struct_tm (&gdate, &tm);

        if (mktime (&tm) == -1)
        {
                time_t secs = time (NULL);

                tm = *localtime (&secs);
                tm.tm_sec = 0;
                tm.tm_min = 0;
                tm.tm_hour = 0;
                tm.tm_isdst = -1;
        }

        gnc_date_edit_set_time (gde, mktime (&tm));

        gtk_calendar_select_day (GTK_CALENDAR (gde->calendar), 1);
	gtk_calendar_select_month (GTK_CALENDAR (gde->calendar), tm.tm_mon,
                                   1900 + tm.tm_year);
        gtk_calendar_select_day (GTK_CALENDAR (gde->calendar), tm.tm_mday);

        return TRUE;
}

static void
create_children (GNCDateEdit *gde)
{
	GtkWidget *frame;
	GtkWidget *hbox;
	GtkWidget *arrow;

	gde->date_entry  = gtk_entry_new ();
	gtk_widget_set_usize (gde->date_entry, 90, 0);
	gtk_box_pack_start (GTK_BOX (gde), gde->date_entry, TRUE, TRUE, 0);
	gtk_widget_show (gde->date_entry);
	gtk_signal_connect (GTK_OBJECT (gde->date_entry), "key_press_event",
			    GTK_SIGNAL_FUNC(date_accel_key_press), gde);

	gde->date_button = gtk_button_new ();
	gtk_signal_connect (GTK_OBJECT (gde->date_button), "clicked",
			    GTK_SIGNAL_FUNC (select_clicked), gde);
	gtk_box_pack_start (GTK_BOX (gde), gde->date_button, FALSE, FALSE, 0);

	hbox = gtk_hbox_new (FALSE, 3);
	gtk_container_add (GTK_CONTAINER (gde->date_button), hbox);
	gtk_widget_show (hbox);

	/* Calendar label, only shown if the date editor has a time field */

	gde->cal_label = gtk_label_new (_("Calendar"));
	gtk_misc_set_alignment (GTK_MISC (gde->cal_label), 0.0, 0.5);
	gtk_box_pack_start (GTK_BOX (hbox), gde->cal_label, TRUE, TRUE, 0);
	if (gde->flags & GNC_DATE_EDIT_SHOW_TIME)
		gtk_widget_show (gde->cal_label);

	arrow = gtk_arrow_new (GTK_ARROW_DOWN, GTK_SHADOW_OUT);
	gtk_box_pack_start (GTK_BOX (hbox), arrow, FALSE, FALSE, 0);
	gtk_widget_show (arrow);

	gtk_widget_show (gde->date_button);

	gde->time_entry = gtk_entry_new_with_max_length (12);
	gtk_widget_set_usize (gde->time_entry, 88, 0);
	gtk_box_pack_start (GTK_BOX (gde), gde->time_entry, TRUE, TRUE, 0);

	gde->time_popup = gtk_option_menu_new ();
	gtk_box_pack_start (GTK_BOX (gde), gde->time_popup, FALSE, FALSE, 0);

	/* We do not create the popup menu with the hour range until we are
	 * realized, so that it uses the values that the user might supply in a
	 * future call to gnc_date_edit_set_popup_range
	 */
	gtk_signal_connect (GTK_OBJECT (gde), "realize",
			    GTK_SIGNAL_FUNC (fill_time_popup), gde);

	if (gde->flags & GNC_DATE_EDIT_SHOW_TIME) {
		gtk_widget_show (gde->time_entry);
		gtk_widget_show (gde->time_popup);
	}

	gde->cal_popup = gtk_window_new (GTK_WINDOW_POPUP);
	gtk_widget_set_events (gde->cal_popup,
			       gtk_widget_get_events (gde->cal_popup) |
                               GDK_KEY_PRESS_MASK);
	gtk_signal_connect (GTK_OBJECT (gde->cal_popup), "delete_event",
			    (GtkSignalFunc) delete_popup,
			    gde);
	gtk_signal_connect (GTK_OBJECT (gde->cal_popup), "key_press_event",
			    (GtkSignalFunc) key_press_popup,
			    gde);
	gtk_signal_connect (GTK_OBJECT (gde->cal_popup), "button_press_event",
			    (GtkSignalFunc) button_press_popup,
			    gde);
	gtk_window_set_policy (GTK_WINDOW (gde->cal_popup),
                               FALSE, FALSE, TRUE);

	frame = gtk_frame_new (NULL);
	gtk_frame_set_shadow_type (GTK_FRAME (frame), GTK_SHADOW_IN);
	gtk_container_add (GTK_CONTAINER (gde->cal_popup), frame);
	gtk_widget_show (frame);

	gde->calendar = gtk_calendar_new ();
	gtk_calendar_display_options
                (GTK_CALENDAR (gde->calendar),
                 (GTK_CALENDAR_SHOW_DAY_NAMES
                  | GTK_CALENDAR_SHOW_HEADING
                  | ((gde->flags & GNC_DATE_EDIT_WEEK_STARTS_ON_MONDAY)
                     ? GTK_CALENDAR_WEEK_START_MONDAY : 0)));
	gtk_signal_connect (GTK_OBJECT (gde->calendar), "day_selected",
			    GTK_SIGNAL_FUNC (day_selected), gde);
	gtk_signal_connect (GTK_OBJECT (gde->calendar),
                            "day_selected_double_click",
			    GTK_SIGNAL_FUNC (day_selected_double_click), gde);
	gtk_container_add (GTK_CONTAINER (frame), gde->calendar);
        gtk_widget_show (gde->calendar);
}

/**
 * gnc_date_edit_new:
 * @the_time: date and time to be displayed on the widget
 * @show_time: whether time should be displayed
 * @use_24_format: whether 24-hour format is desired for the time display.
 *
 * Creates a new GNCDateEdit widget which can be used to provide
 * an easy to use way for entering dates and times.
 * 
 * Returns a GNCDateEdit widget.
 */
GtkWidget *
gnc_date_edit_new (time_t the_time, int show_time, int use_24_format)
{
	return gnc_date_edit_new_flags
                (the_time,
                 ((show_time ? GNC_DATE_EDIT_SHOW_TIME : 0)
                  | (use_24_format ? GNC_DATE_EDIT_24_HR : 0)));
}

/**
 * gnc_date_edit_new_flags:
 * @the_time: The initial time for the date editor.
 * @flags: A bitmask of GNCDateEditFlags values.
 * 
 * Creates a new GNCDateEdit widget with the specified flags.
 * 
 * Return value: the newly-created date editor widget.
 **/
GtkWidget *
gnc_date_edit_new_flags (time_t the_time, GNCDateEditFlags flags)
{
	GNCDateEdit *gde;

	gde = gtk_type_new (gnc_date_edit_get_type ());

	gde->flags = flags;
	create_children (gde);
	gnc_date_edit_set_time (gde, the_time);

	return GTK_WIDGET (gde);
}

static struct tm
gnc_date_edit_get_date_internal (GNCDateEdit *gde)
{
	struct tm tm = {0};
	char *str, *flags = NULL;

	/* Assert, because we're just hosed if it's NULL */
	g_assert(gde != NULL);
	g_assert(GNC_IS_DATE_EDIT(gde));

      	scanDate (gtk_entry_get_text (GTK_ENTRY (gde->date_entry)),
                  &tm.tm_mday, &tm.tm_mon, &tm.tm_year);

	tm.tm_mon--;

	/* Hope the user does not actually mean years early in the A.D. days...
	 * This date widget will obviously not work for a history program :-)
	 */
	if (tm.tm_year >= 1900)
		tm.tm_year -= 1900;

	if (gde->flags & GNC_DATE_EDIT_SHOW_TIME) {
		char *tokp, *temp;

		str = g_strdup (gtk_entry_get_text
                                (GTK_ENTRY (gde->time_entry)));
		temp = gnc_strtok_r (str, ": ", &tokp);
		if (temp) {
			tm.tm_hour = atoi (temp);
			temp = gnc_strtok_r (NULL, ": ", &tokp);
			if (temp) {
				if (isdigit (*temp)) {
					tm.tm_min = atoi (temp);
					flags = gnc_strtok_r (NULL, ": ",
                                                              &tokp);
					if (flags && isdigit (*flags)) {
						tm.tm_sec = atoi (flags);
						flags = gnc_strtok_r (NULL,
                                                                      ": ",
                                                                      &tokp);
					}
				} else
					flags = temp;
			}
		}

		if (flags && (strcasecmp (flags, "PM") == 0)){
			if (tm.tm_hour < 12)
				tm.tm_hour += 12;
		}
		g_free (str);
	}
        else
        {
                tm.tm_hour = 0;
                tm.tm_min  = 0;
                tm.tm_sec  = 0;
        }

	tm.tm_isdst = -1;

        return tm;
}

/**
 * gnc_date_edit_get_date:
 * @gde: The GNCDateEdit widget
 *
 * Returns the time entered in the GNCDateEdit widget
 */
time_t
gnc_date_edit_get_date (GNCDateEdit *gde)
{
 	struct tm tm;

        g_return_val_if_fail (gde != NULL, 0);
        g_return_val_if_fail (GNC_IS_DATE_EDIT (gde), 0);

        tm = gnc_date_edit_get_date_internal (gde);

        if (mktime (&tm) == -1)
        {
                time_t secs = time (NULL);

                tm = *localtime (&secs);
                tm.tm_sec = 0;
                tm.tm_min = 0;
                tm.tm_hour = 0;
                tm.tm_isdst = -1;
        }

	return mktime (&tm);
}

/**
 * gnc_date_edit_get_date_end:
 * @gde: The GNCDateEdit widget
 *
 * Returns the date entered in the GNCDateEdit widget,
 * but with the time adjusted to the end of the day.
 */
time_t
gnc_date_edit_get_date_end (GNCDateEdit *gde)
{
 	struct tm tm;

        g_return_val_if_fail (gde != NULL, 0);
        g_return_val_if_fail (GNC_IS_DATE_EDIT (gde), 0);

        tm = gnc_date_edit_get_date_internal (gde);

        tm.tm_hour = 23;
        tm.tm_min  = 59;
        tm.tm_sec  = 59;

        if (mktime (&tm) == -1)
        {
                time_t secs = time (NULL);

                tm = *localtime (&secs);
                tm.tm_sec = 23;
                tm.tm_min = 59;
                tm.tm_hour = 59;
                tm.tm_isdst = -1;
        }

	return mktime (&tm);
}

/**
 * gnc_date_edit_set_flags:
 * @gde: The date editor widget whose flags should be changed.
 * @flags: The new bitmask of GNCDateEditFlags values.
 * 
 * Changes the display flags on an existing date editor widget.
 **/
void
gnc_date_edit_set_flags (GNCDateEdit *gde, GNCDateEditFlags flags)
{
        GNCDateEditFlags old_flags;
        
	g_return_if_fail (gde != NULL);
	g_return_if_fail (GNC_IS_DATE_EDIT (gde));

        old_flags = gde->flags;
        gde->flags = flags;
        
	if ((flags & GNC_DATE_EDIT_SHOW_TIME) !=
            (old_flags & GNC_DATE_EDIT_SHOW_TIME)) {
		if (flags & GNC_DATE_EDIT_SHOW_TIME) {
			gtk_widget_show (gde->cal_label);
			gtk_widget_show (gde->time_entry);
			gtk_widget_show (gde->time_popup);
		} else {
			gtk_widget_hide (gde->cal_label);
			gtk_widget_hide (gde->time_entry);
			gtk_widget_hide (gde->time_popup);
		}
	}

	if ((flags & GNC_DATE_EDIT_24_HR) != (old_flags & GNC_DATE_EDIT_24_HR))
                /* This will destroy the old menu properly */
		fill_time_popup (GTK_WIDGET (gde), gde);

	if ((flags & GNC_DATE_EDIT_WEEK_STARTS_ON_MONDAY)
	    != (old_flags & GNC_DATE_EDIT_WEEK_STARTS_ON_MONDAY)) {
		if (flags & GNC_DATE_EDIT_WEEK_STARTS_ON_MONDAY)
			gtk_calendar_display_options
                                (GTK_CALENDAR (gde->calendar),
                                 (GTK_CALENDAR (gde->calendar)->display_flags
                                  | GTK_CALENDAR_WEEK_START_MONDAY));
		else
			gtk_calendar_display_options
                                (GTK_CALENDAR (gde->calendar),
                                 (GTK_CALENDAR (gde->calendar)->display_flags
                                  & ~GTK_CALENDAR_WEEK_START_MONDAY));
	}
}

/**
 * gnc_date_edit_get_flags:
 * @gde: The date editor whose flags should be queried.
 * 
 * Queries the display flags on a date editor widget.
 * 
 * Return value: The current display flags for the given date editor widget.
 **/
int
gnc_date_edit_get_flags (GNCDateEdit *gde)
{
	g_return_val_if_fail (gde != NULL, 0);
	g_return_val_if_fail (GNC_IS_DATE_EDIT (gde), 0);

	return gde->flags;
}

/*
  Local Variables:
  c-basic-offset: 8
  End:
*/
