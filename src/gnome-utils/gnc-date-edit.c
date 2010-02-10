/*
 * gnc-date-edit.c -- Date editor widget
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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
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

#include <gnome.h> 
#include <glib/gi18n.h>
#include <string.h>
#include <stdlib.h> /* atoi */
#include <ctype.h> /* isdigit */
#include <stdio.h>
#include <time.h>

#ifndef HAVE_LOCALTIME_R
# include "localtime_r.h"
#endif
#include "gnc-date.h"
#include "gnc-engine.h"
#include "dialog-utils.h"
#include "gnc-date-edit.h"
#include "glib-compat.h"

enum {
	DATE_CHANGED,
	TIME_CHANGED,
	LAST_SIGNAL
};

static QofLogModule log_module = GNC_MOD_GUI;
static guint date_edit_signals [LAST_SIGNAL] = { 0 };


static void gnc_date_edit_init         (GNCDateEdit      *gde);
static void gnc_date_edit_class_init   (GNCDateEditClass *klass);
static void gnc_date_edit_dispose      (GObject          *object);
static void gnc_date_edit_finalize     (GObject          *object);
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
GType
gnc_date_edit_get_type (void)
{
	static GType date_edit_type = 0;

	if (date_edit_type == 0){
		static const GTypeInfo date_edit_info = {
			sizeof (GNCDateEditClass),
			NULL,
			NULL,
			(GClassInitFunc) gnc_date_edit_class_init,
			NULL,
			NULL,
			sizeof (GNCDateEdit),
			0, /* n_preallocs */
			(GInstanceInitFunc) gnc_date_edit_init,
			NULL,
		};

		date_edit_type = g_type_register_static (GTK_TYPE_HBOX,
							"GNCDateEdit",
							 &date_edit_info, 0);
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
gnc_date_edit_popdown(GNCDateEdit *gde)
{
  g_return_if_fail (GNC_IS_DATE_EDIT (gde));

  ENTER("gde %p", gde);

  gtk_grab_remove (gde->cal_popup);
  gtk_widget_hide (gde->cal_popup);
  gdk_pointer_ungrab (GDK_CURRENT_TIME);

  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (gde->date_button),
                                FALSE);

  LEAVE(" ");
}

static void
day_selected (GtkCalendar *calendar, GNCDateEdit *gde)
{
	char buffer [40];
	guint year, month, day;

	gtk_calendar_get_date (calendar, &year, &month, &day);

	qof_print_date_dmy_buff (buffer, 40, day, month + 1, year);
	gtk_entry_set_text (GTK_ENTRY (gde->date_entry), buffer);
	g_signal_emit (G_OBJECT (gde), date_edit_signals [DATE_CHANGED], 0);
}

static void
day_selected_double_click (GtkCalendar *calendar, GNCDateEdit *gde)
{
	gnc_date_edit_popdown (gde);
}

static gint
delete_popup (GtkWidget *widget, gpointer data)
{
	GNCDateEdit *gde;

	gde = data;
	gnc_date_edit_popdown (gde);

	return TRUE;
}

static gint
key_press_popup (GtkWidget *widget, GdkEventKey *event, gpointer data)
{
	GNCDateEdit *gde = data;

	if (event->keyval != GDK_Return &&
	    event->keyval != GDK_KP_Enter &&
	    event->keyval != GDK_Escape)
	  return date_accel_key_press(gde->date_entry, event, data);

	gde = data;
	g_signal_stop_emission_by_name (G_OBJECT (widget), "key-press-event");
	gnc_date_edit_popdown (gde);

	return TRUE;
}

static void
position_popup (GNCDateEdit *gde)
{
	gint x, y;
	gint bwidth, bheight;
	GtkRequisition req;

	gtk_widget_size_request (gde->cal_popup, &req);

	gdk_window_get_origin (gde->date_button->window, &x, &y);

	x += gde->date_button->allocation.x;
	y += gde->date_button->allocation.y;
	bwidth = gde->date_button->allocation.width;
	bheight = gde->date_button->allocation.height;

	x += bwidth - req.width;
	y += bheight;

	if (x < 0)
		x = 0;

	if (y < 0)
		y = 0;

	gtk_window_move (GTK_WINDOW (gde->cal_popup), x, y);
}

/* Pulled from gtkcombobox.c */
static gboolean
popup_grab_on_window (GdkWindow *window,
                      guint32    activate_time,
                      gboolean   grab_keyboard)
{
  if ((gdk_pointer_grab (window, TRUE,
                         GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK |
                         GDK_POINTER_MOTION_MASK,
                         NULL, NULL, activate_time) == 0))
  {
    if (!grab_keyboard ||
        gdk_keyboard_grab (window, TRUE,
                           activate_time) == 0)
      return TRUE;
    else
    {
      gdk_display_pointer_ungrab (gdk_drawable_get_display (window),
                                  activate_time);
      return FALSE;
    }
  }

  return FALSE;
}

static void
gnc_date_edit_popup (GNCDateEdit *gde)
{
  GtkWidget *toplevel;
  struct tm mtm;

  g_return_if_fail (GNC_IS_DATE_EDIT (gde));

  ENTER("gde %p", gde);

  /* This code is pretty much just copied from gtk_date_edit_get_date */
  qof_scan_date (gtk_entry_get_text (GTK_ENTRY (gde->date_entry)),
                 &mtm.tm_mday, &mtm.tm_mon, &mtm.tm_year);

  mtm.tm_mon--;

  /* Hope the user does not actually mean years early in the A.D. days...
   * This date widget will obviously not work for a history program :-)
   */
  if (mtm.tm_year >= 1900)
    mtm.tm_year -= 1900;

  gnc_tm_set_day_start(&mtm);
  if (mktime (&mtm) == (time_t) -1)
  {
    gnc_tm_get_today_start (&mtm);
    gnc_date_edit_set_time (gde, mktime (&mtm));
  }

  /* Set the calendar.  */
  gtk_calendar_select_day (GTK_CALENDAR (gde->calendar), 1);
  gtk_calendar_select_month (GTK_CALENDAR (gde->calendar), mtm.tm_mon,
                             1900 + mtm.tm_year);
  gtk_calendar_select_day (GTK_CALENDAR (gde->calendar), mtm.tm_mday);

  /* Make sure we'll get notified of clicks outside the popup
   * window so we can properly pop down if that happens. */
  toplevel = gtk_widget_get_toplevel (GTK_WIDGET (gde));
  if (GTK_IS_WINDOW (toplevel))
  {
    gtk_window_group_add_window (
#ifdef HAVE_GTK_2_10
      gtk_window_get_group (GTK_WINDOW (toplevel)),
#else
      _gtk_window_get_group (GTK_WINDOW (toplevel)),
#endif
      GTK_WINDOW (gde->cal_popup));
    gtk_window_set_transient_for (GTK_WINDOW (gde->cal_popup),
                                  GTK_WINDOW (toplevel));
  }

  position_popup (gde);

  gtk_widget_show (gde->cal_popup);

  gtk_widget_grab_focus (gde->cal_popup);
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (gde->date_button),
                                TRUE);

  if (!GTK_WIDGET_HAS_FOCUS (gde->calendar))
    gtk_widget_grab_focus (gde->calendar);

  if (!popup_grab_on_window ((GTK_WIDGET(gde->cal_popup))->window,
                             GDK_CURRENT_TIME, TRUE))
  {
    gtk_widget_hide (gde->cal_popup);
    LEAVE("Failed to grab window");
    return;
  }

  gtk_grab_add (gde->cal_popup);

  LEAVE(" ");
}

/* This function is a customized gtk_combo_box_list_button_pressed(). */
static gboolean
gnc_date_edit_button_pressed (GtkWidget      *widget,
                              GdkEventButton *event,
                              gpointer        data)
{
  GNCDateEdit *gde     = GNC_DATE_EDIT(data);
  GtkWidget   *ewidget = gtk_get_event_widget ((GdkEvent *)event);

  ENTER("widget=%p, ewidget=%p, event=%p, gde=%p", widget, ewidget, event, gde);

  /* While popped up, ignore presses outside the popup window. */
  if (ewidget == gde->cal_popup)
  {
    LEAVE("Press on calendar. Ignoring.");
    return TRUE;
  }

  /* If the press isn't to make the popup appear, just propagate it. */
  if (ewidget != gde->date_button ||
      gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (gde->date_button)))
  {
    LEAVE("Press, not on popup button, or while popup is raised.");
    return FALSE;
  }

  if (!GTK_WIDGET_HAS_FOCUS (gde->date_button))
    gtk_widget_grab_focus (gde->date_button);

  gde->popup_in_progress = TRUE;

  gnc_date_edit_popup (gde);

  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (gde->date_button), TRUE);

  LEAVE("Popup in progress.");
  return TRUE;
}

static gboolean
gnc_date_edit_button_released (GtkWidget      *widget,
                               GdkEventButton *event,
                               gpointer        data)
{
  GNCDateEdit *gde     = GNC_DATE_EDIT(data);
  GtkWidget   *ewidget = gtk_get_event_widget ((GdkEvent *)event);
  gboolean popup_in_progress = FALSE;

  ENTER("widget=%p, ewidget=%p, event=%p, gde=%p", widget, ewidget, event, gde);

  if (gde->popup_in_progress)
  {
    popup_in_progress = TRUE;
    gde->popup_in_progress = FALSE;
  }

  /* Propagate releases on the calendar. */
  if (ewidget == gde->calendar)
  {
    LEAVE("Button release on calendar.");
    return FALSE;
  }

  if (ewidget == gde->date_button)
  {
    /* Pop down if we're up and it isn't due to the preceding press. */
    if (!popup_in_progress &&
        gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (gde->date_button)))
    {
      gnc_date_edit_popdown (gde);
      LEAVE("Release on button, not in progress. Popped down.");
      return TRUE;
    }

    LEAVE("Button release on button. Allowing.");
    return FALSE;
  }

  /* Pop down on a release anywhere else. */
  gnc_date_edit_popdown (gde);
  LEAVE("Release not on button or calendar. Popping down.");
  return TRUE;
}

static void
gnc_date_edit_button_toggled (GtkWidget *widget, GNCDateEdit *gde)
{
  ENTER("widget %p, gde %p", widget, gde);

  if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (widget)))
  {
    if (!gde->popup_in_progress)
      gnc_date_edit_popup (gde);
  }
  else
    gnc_date_edit_popdown (gde);

  LEAVE(" ");
}

typedef struct {
	char *hour;
	GNCDateEdit *gde;
} hour_info_t;

static void
set_time (GtkWidget *widget, hour_info_t *hit)
{
	gtk_entry_set_text (GTK_ENTRY (hit->gde->time_entry), hit->hour);
	g_signal_emit (G_OBJECT (hit->gde), date_edit_signals [TIME_CHANGED], 0);
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
	struct tm *tm_returned;
	struct tm mtm;
	time_t current_time;
	int i, j;

	if (gde->lower_hour > gde->upper_hour)
		return;

	menu = gtk_menu_new ();
	gtk_option_menu_set_menu (GTK_OPTION_MENU (gde->time_popup), menu);

	time (&current_time);
	tm_returned = localtime_r (&current_time, &mtm);
	g_return_if_fail(tm_returned != NULL);
	
	for (i = gde->lower_hour; i <= gde->upper_hour; i++){
		GtkWidget *item, *submenu;
		hour_info_t *hit;
		char buffer [40];

		mtm.tm_hour = i;
		mtm.tm_min  = 0;
		hit = g_new (hour_info_t, 1);

		if (gde->flags & GNC_DATE_EDIT_24_HR)
			qof_strftime (buffer, sizeof (buffer), "%H:00", &mtm);
		else
			qof_strftime (buffer, sizeof (buffer), "%I:00 %p ", &mtm);
		hit->hour = g_strdup (buffer);
		hit->gde  = gde;

		item = gtk_menu_item_new_with_label (buffer);
		gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
#if 0
		g_signal_connect (G_OBJECT (item), "activate",
				  G_CALLBACK  (set_time), hit);
#endif
		g_signal_connect (G_OBJECT (item), "destroy",
				  G_CALLBACK  (free_resources), hit);
		gtk_widget_show (item);

		submenu = gtk_menu_new ();
		gtk_menu_item_set_submenu (GTK_MENU_ITEM (item), submenu);
		for (j = 0; j < 60; j += 15){
			GtkWidget *mins;

			mtm.tm_min = j;
			hit = g_new (hour_info_t, 1);
			if (gde->flags & GNC_DATE_EDIT_24_HR)
				qof_strftime (buffer, sizeof (buffer),
					      "%H:%M", &mtm);
			else
				qof_strftime (buffer, sizeof (buffer),
					      "%I:%M %p", &mtm);
			hit->hour = g_strdup (buffer);
			hit->gde  = gde;

			mins = gtk_menu_item_new_with_label (buffer);
			gtk_menu_shell_append (GTK_MENU_SHELL (submenu), mins);
			g_signal_connect (G_OBJECT (mins), "activate",
					  G_CALLBACK  (set_time), hit);
			g_signal_connect (G_OBJECT (item), "destroy",
					  G_CALLBACK  (free_resources),
                                            hit);
			gtk_widget_show (mins);
		}
	}
}

static void
gnc_date_edit_class_init (GNCDateEditClass *klass)
{
	GtkContainerClass *container_class = (GtkContainerClass *) klass;
	GObjectClass *object_class = (GObjectClass *) klass;

	object_class = (GObjectClass*) klass;
	
	parent_class = g_type_class_ref(GTK_TYPE_HBOX);

	date_edit_signals [TIME_CHANGED] =
		g_signal_new ("time_changed",
			      G_TYPE_FROM_CLASS (object_class),
			      G_SIGNAL_RUN_FIRST,
			      G_STRUCT_OFFSET (GnomeDateEditClass, time_changed),
			      NULL, NULL,
			      g_cclosure_marshal_VOID__VOID,
			      G_TYPE_NONE, 0);

	date_edit_signals [DATE_CHANGED] =
		g_signal_new ("date_changed",
			      G_TYPE_FROM_CLASS (object_class),
			      G_SIGNAL_RUN_FIRST,
			      G_STRUCT_OFFSET (GnomeDateEditClass, date_changed),
			      NULL, NULL,
			      g_cclosure_marshal_VOID__VOID,
			      G_TYPE_NONE, 0);

	container_class->forall = gnc_date_edit_forall;

	object_class->dispose = gnc_date_edit_dispose;
	object_class->finalize = gnc_date_edit_finalize;

	klass->date_changed = NULL;
	klass->time_changed = NULL;
}

static void
gnc_date_edit_init (GNCDateEdit *gde)
{
	gde->disposed = FALSE;
	gde->popup_in_progress = FALSE;
	gde->lower_hour = 7;
	gde->upper_hour = 19;
	gde->flags = GNC_DATE_EDIT_SHOW_TIME;
}

static void
gnc_date_edit_finalize (GObject *object)
{
	GNCDateEdit *gde;

	g_return_if_fail (object != NULL);
	g_return_if_fail (GNC_IS_DATE_EDIT (object));

	gde = GNC_DATE_EDIT (object);


	if (G_OBJECT_CLASS (parent_class)->finalize)
		(* G_OBJECT_CLASS (parent_class)->finalize) (object);
}

static void
gnc_date_edit_dispose (GObject *object)
{
	GNCDateEdit *gde;

	g_return_if_fail (object != NULL);
	g_return_if_fail (GNC_IS_DATE_EDIT (object));

	gde = GNC_DATE_EDIT (object);

	if(gde->disposed)
		return;

	gde->disposed = TRUE;

	/* Only explicitly destroy the toplevel elements */

	gtk_widget_destroy (GTK_WIDGET(gde->date_entry));
	gde->date_entry = NULL;

	gtk_widget_destroy (GTK_WIDGET(gde->date_button));
	gde->date_button= NULL;

	gtk_widget_destroy (GTK_WIDGET(gde->time_entry));
	gde->time_entry = NULL;

	gtk_widget_destroy (GTK_WIDGET(gde->time_popup));
	gde->time_popup = NULL;

	if (G_OBJECT_CLASS (parent_class)->dispose)
		(* G_OBJECT_CLASS (parent_class)->dispose) (object);
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

static void
gnc_date_edit_set_time_tm (GNCDateEdit *gde, struct tm *mytm) 
{
        char buffer [40];

        g_return_if_fail(mytm != NULL);

        /* Update the date text. */
        qof_print_date_dmy_buff(buffer, 40,
                                mytm->tm_mday,
                                mytm->tm_mon + 1,
                                1900 + mytm->tm_year);
        gtk_entry_set_text(GTK_ENTRY(gde->date_entry), buffer);

        /* Update the calendar. */
        gtk_calendar_select_day(GTK_CALENDAR (gde->calendar), 1);
        gtk_calendar_select_month(GTK_CALENDAR (gde->calendar),
                                  mytm->tm_mon, 1900 + mytm->tm_year);
        gtk_calendar_select_day(GTK_CALENDAR (gde->calendar), mytm->tm_mday);

        /* Set the time of day. */
        if (gde->flags & GNC_DATE_EDIT_24_HR)
          qof_strftime (buffer, sizeof (buffer), "%H:%M", mytm);
        else
          qof_strftime (buffer, sizeof (buffer), "%I:%M %p", mytm);
        gtk_entry_set_text(GTK_ENTRY(gde->time_entry), buffer);
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
	struct tm *tm_returned;
	struct tm tm_to_set;

	g_return_if_fail (gde != NULL);
        g_return_if_fail (GNC_IS_DATE_EDIT (gde));

        /* If the_time is invalid, use the last valid time
         * seen (or as a last resort, the current date). */
        if (the_time == (time_t) -1)
        {
          if (gde->initial_time == (time_t) -1)
            gde->initial_time = gnc_timet_get_today_start();
          the_time = gde->initial_time;
        }
        else
          gde->initial_time = the_time;

        /* Convert time_t to tm. */
	tm_returned = localtime_r (&the_time, &tm_to_set);
	g_return_if_fail(tm_returned != NULL);

	gnc_date_edit_set_time_tm(gde, &tm_to_set);
}

void
gnc_date_edit_set_gdate (GNCDateEdit *gde, const GDate *date)
{
        struct tm mytm;
        time_t t;
        
        g_return_if_fail(gde && GNC_IS_DATE_EDIT(gde) && 
                         date && g_date_valid(date));
        g_date_to_struct_tm(date, &mytm);
        t = mktime(&mytm);
        if (t != (time_t)(-1))
                gnc_date_edit_set_time(gde, t);
}

void
gnc_date_edit_set_time_ts (GNCDateEdit *gde, Timespec the_time)
{
        gnc_date_edit_set_time (gde, the_time.tv_sec);
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
	g_return_if_fail (gde != NULL);
        g_return_if_fail (GNC_IS_DATE_EDIT (gde));

	gde->lower_hour = low_hour;
	gde->upper_hour = up_hour;

        fill_time_popup(NULL, gde);
}

/* This code should be kept in sync with src/register/datecell.c */
static int
date_accel_key_press(GtkWidget *widget, GdkEventKey *event, gpointer data)
{
	GNCDateEdit *gde = data;
  	G_CONST_RETURN char *string;
        struct tm tm;

        string = gtk_entry_get_text (GTK_ENTRY (widget));

        tm = gnc_date_edit_get_date_internal (gde);

        if (!gnc_handle_date_accelerator (event, &tm, string))
                return FALSE;

        gnc_date_edit_set_time (gde, mktime (&tm));

	g_signal_emit (G_OBJECT (gde), date_edit_signals [TIME_CHANGED], 0);
        return TRUE;
}

static gint
key_press_entry (GtkWidget *widget, GdkEventKey *event, gpointer data)
{
	if (!date_accel_key_press(widget, event, data))
		return FALSE;

	g_signal_stop_emission_by_name (widget, "key-press-event");
	return TRUE;
}

static int
date_focus_out_event(GtkWidget *widget, GdkEventKey *event, gpointer data)
{
	GNCDateEdit *gde = data;
        struct tm tm;

        /* Get the date entered and attempt to use it. */
        tm = gnc_date_edit_get_date_internal (gde);
        gnc_date_edit_set_time (gde, mktime (&tm));

        /* Get the date again in case it was invalid the first time. */
        tm = gnc_date_edit_get_date_internal (gde);

	g_signal_emit (gde, date_edit_signals [DATE_CHANGED], 0);
	g_signal_emit (gde, date_edit_signals [TIME_CHANGED], 0);

        return FALSE;
}

static void
create_children (GNCDateEdit *gde)
{
	GtkWidget *frame;
	GtkWidget *hbox;
	GtkWidget *arrow;

        /* Create the text entry area. */
	gde->date_entry  = gtk_entry_new ();
	gtk_entry_set_width_chars (GTK_ENTRY (gde->date_entry), 11);
	gtk_box_pack_start (GTK_BOX (gde), gde->date_entry, TRUE, TRUE, 0);
	gtk_widget_show (GTK_WIDGET(gde->date_entry));
	g_signal_connect (G_OBJECT (gde->date_entry), "key-press-event",
			  G_CALLBACK (key_press_entry), gde);
	g_signal_connect (G_OBJECT (gde->date_entry), "focus-out-event",
			  G_CALLBACK (date_focus_out_event), gde);

        /* Create the popup button. */
	gde->date_button = gtk_toggle_button_new ();
	g_signal_connect (gde->date_button, "button-press-event",
			  G_CALLBACK(gnc_date_edit_button_pressed), gde);
	g_signal_connect (G_OBJECT (gde->date_button), "toggled",
			  G_CALLBACK (gnc_date_edit_button_toggled), gde);
	gtk_box_pack_start (GTK_BOX (gde), gde->date_button, FALSE, FALSE, 0);

	hbox = gtk_hbox_new (FALSE, 3);
	gtk_container_add (GTK_CONTAINER (gde->date_button), hbox);
	gtk_widget_show (GTK_WIDGET(hbox));

	/* Calendar label, only shown if the date editor has a time field */
	gde->cal_label = gtk_label_new (_("Calendar"));
	gtk_misc_set_alignment (GTK_MISC (gde->cal_label), 0.0, 0.5);
	gtk_box_pack_start (GTK_BOX (hbox), gde->cal_label, TRUE, TRUE, 0);
	if (gde->flags & GNC_DATE_EDIT_SHOW_TIME)
		gtk_widget_show (GTK_WIDGET(gde->cal_label));

        /* Graphic for the popup button. */
	arrow = gtk_arrow_new (GTK_ARROW_DOWN, GTK_SHADOW_NONE);
	gtk_box_pack_start (GTK_BOX (hbox), arrow, TRUE, FALSE, 0);
	gtk_widget_show (GTK_WIDGET(arrow));

	gtk_widget_show (GTK_WIDGET(gde->date_button));

        /* Time entry controls. */
	gde->time_entry = gtk_entry_new ();
	gtk_entry_set_max_length (GTK_ENTRY(gde->time_entry), 12);
	gtk_widget_set_size_request (GTK_WIDGET(gde->time_entry), 88, -1);
	gtk_box_pack_start (GTK_BOX (gde), gde->time_entry, TRUE, TRUE, 0);

	gde->time_popup = gtk_option_menu_new ();
	gtk_box_pack_start (GTK_BOX (gde), gde->time_popup, FALSE, FALSE, 0);

	/* We do not create the popup menu with the hour range until we are
	 * realized, so that it uses the values that the user might supply in a
	 * future call to gnc_date_edit_set_popup_range
	 */
	g_signal_connect (G_OBJECT (gde), "realize",
			  G_CALLBACK  (fill_time_popup), gde);

	if (gde->flags & GNC_DATE_EDIT_SHOW_TIME) {
		gtk_widget_show (GTK_WIDGET(gde->time_entry));
		gtk_widget_show (GTK_WIDGET(gde->time_popup));
	}

	gde->cal_popup = gtk_window_new (GTK_WINDOW_POPUP);
        gtk_widget_set_name (gde->cal_popup, "gnc-date-edit-popup-window");

#ifdef HAVE_GTK_2_10
        gtk_window_set_type_hint (GTK_WINDOW (gde->cal_popup),
                                  GDK_WINDOW_TYPE_HINT_COMBO);
#endif

	gtk_widget_set_events (GTK_WIDGET(gde->cal_popup),
			       gtk_widget_get_events (GTK_WIDGET(gde->cal_popup)) |
                               GDK_KEY_PRESS_MASK);

	g_signal_connect (gde->cal_popup, "delete-event",
			  G_CALLBACK(delete_popup), gde);
	g_signal_connect (gde->cal_popup, "key-press-event",
			  G_CALLBACK(key_press_popup), gde);
	g_signal_connect (gde->cal_popup, "button-press-event",
			  G_CALLBACK(gnc_date_edit_button_pressed), gde);
	g_signal_connect (gde->cal_popup, "button-release-event",
			  G_CALLBACK(gnc_date_edit_button_released), gde);
	gtk_window_set_resizable (GTK_WINDOW (gde->cal_popup), FALSE);
        gtk_window_set_screen (GTK_WINDOW (gde->cal_popup),
                               gtk_widget_get_screen (GTK_WIDGET (gde)));

	frame = gtk_frame_new (NULL);
	gtk_frame_set_shadow_type (GTK_FRAME (frame), GTK_SHADOW_NONE);
	gtk_container_add (GTK_CONTAINER (gde->cal_popup), frame);
	gtk_widget_show (GTK_WIDGET(frame));

	gde->calendar = gtk_calendar_new ();
	gtk_calendar_set_display_options
                (GTK_CALENDAR (gde->calendar),
                 (GTK_CALENDAR_SHOW_DAY_NAMES
                  | GTK_CALENDAR_SHOW_HEADING
                  | ((gde->flags & GNC_DATE_EDIT_WEEK_STARTS_ON_MONDAY)
                     ? GTK_CALENDAR_WEEK_START_MONDAY : 0)));
	g_signal_connect (gde->calendar, "button-release-event",
			  G_CALLBACK(gnc_date_edit_button_released), gde);
	g_signal_connect (G_OBJECT (gde->calendar), "day-selected",
			  G_CALLBACK  (day_selected), gde);
	g_signal_connect (G_OBJECT (gde->calendar),
                            "day-selected-double-click",
			  G_CALLBACK  (day_selected_double_click), gde);
	gtk_container_add (GTK_CONTAINER (frame), gde->calendar);
        gtk_widget_show (GTK_WIDGET(gde->calendar));
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

GtkWidget *
gnc_date_edit_new_ts (Timespec the_time, int show_time, int use_24_format)
{
        return gnc_date_edit_new (the_time.tv_sec, show_time, use_24_format);
}


/*
 * Create a new GncDateEdit widget from a glade file.  The widget
 * generated is set to today's date, and will not show a time as part
 * of the date.  This function does not use any of the arguments
 * passed by glade.
 */
GtkWidget *
gnc_date_edit_new_glade (gchar *widget_name,
			 gchar *string1, gchar *string2,
			 gint int1, gint int2)
{
	GtkWidget *widget;

	/* None of the standard glade arguments are used. */
	widget = gnc_date_edit_new(time(NULL), FALSE, FALSE);
	gtk_widget_show(widget);
	return widget;
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

	gde = g_object_new (GNC_TYPE_DATE_EDIT, NULL, NULL);

	gde->flags = flags;
	gde->initial_time = -1;
	create_children (gde);
	gnc_date_edit_set_time (gde, the_time);

	return GTK_WIDGET (gde);
}

static struct tm
gnc_date_edit_get_date_internal (GNCDateEdit *gde)
{
	struct tm tm = {0};
	char *str;
	gchar *flags = NULL;

	/* Assert, because we're just hosed if it's NULL */
	g_assert(gde != NULL);
	g_assert(GNC_IS_DATE_EDIT(gde));

      qof_scan_date (gtk_entry_get_text (GTK_ENTRY (gde->date_entry)),
                  &tm.tm_mday, &tm.tm_mon, &tm.tm_year);

	tm.tm_mon--;

	/* Hope the user does not actually mean years early in the A.D. days...
	 * This date widget will obviously not work for a history program :-)
	 */
	if (tm.tm_year >= 1900)
		tm.tm_year -= 1900;

	if (gde->flags & GNC_DATE_EDIT_SHOW_TIME) {
		char *tokp = NULL;
		gchar *temp;

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
		gnc_tm_set_day_start(&tm);
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
        time_t retval;

        g_return_val_if_fail (gde != NULL, 0);
        g_return_val_if_fail (GNC_IS_DATE_EDIT (gde), 0);

        tm = gnc_date_edit_get_date_internal (gde);

        retval = mktime (&tm);
        if (retval == (time_t) -1)
        {
          if (gde->initial_time == (time_t) -1)
            return gnc_timet_get_today_start ();
          else
            return gde->initial_time;
        }
        return retval;
}

void
gnc_date_edit_get_gdate (GNCDateEdit *gde, GDate *date)
{
	time_t t;

	g_return_if_fail (gde && date);
	g_return_if_fail (GNC_IS_DATE_EDIT (gde));

	t = gnc_date_edit_get_date(gde);
	g_date_set_time_t(date, t);
}

Timespec
gnc_date_edit_get_date_ts (GNCDateEdit *gde)
{
        Timespec ts = { 0, 0 };

        ts.tv_sec = gnc_date_edit_get_date (gde);

        return ts;
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
	gnc_tm_set_day_end(&tm);

        if (mktime (&tm) == (time_t) -1)
        {
          if (gde->initial_time == (time_t) -1)
            return gnc_timet_get_today_end();
          else
            return gnc_timet_get_day_end(gde->initial_time);
        }
	return mktime (&tm);
}

Timespec
gnc_date_edit_get_date_end_ts (GNCDateEdit *gde)
{
        Timespec ts = { 0, 0 };

        ts.tv_sec = gnc_date_edit_get_date_end (gde);

        return ts;
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

/**
 * gnc_date_set_activates_default:
 * @gde: The date editor to modify
 * @state: The new state for this widget.
 * 
 * Extracts the editable field from a GNCDateEdit widget, and sets it
 * up so that pressing the Enter key in this field as the same as
 * clicking the button that has the default.
 **/
void
gnc_date_activates_default (GNCDateEdit *gde, gboolean state)
{
	if (!gde)
		return;

    gtk_entry_set_activates_default(GTK_ENTRY(gde->date_entry), state);
}

/**
 * gnc_date_grab_focus:
 * @gde: The date editor to modify
 * @state: The new state for this widget.
 * 
 * Sets the focus to the Editable field.
 **/
void
gnc_date_grab_focus (GNCDateEdit *gde)
{
	if (!gde)
		return;

	gtk_widget_grab_focus (gde->date_entry);
}
/** Sets the editable field from a GNCDateEdit widget as the target
 *  for the specified label's access key.
 *
 *  @param gde The date editor to set as the target.
 *
 *  @param label The label whose access key should set focus to this
 *  widget. */
void
gnc_date_make_mnemonic_target (GNCDateEdit *gde, GtkWidget *label)
{
	if (!gde)
		return;

	gtk_label_set_mnemonic_widget (GTK_LABEL(label), gde->date_entry);
}


/*
  Local Variables:
  c-basic-offset: 8
  End:
*/
