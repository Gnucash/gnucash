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

#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <gdk/gdkkeysyms.h>
#include <string.h>
#include <stdlib.h> /* atoi */
#include <ctype.h> /* isdigit */
#include <stdio.h>

#include "gnc-date.h"
#include "gnc-engine.h"
#include "dialog-utils.h"
#include "gnc-date-edit.h"

enum
{
    DATE_CHANGED,
    TIME_CHANGED,
    LAST_SIGNAL
};

enum
{
    PROP_0,
    PROP_TIME,
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


static GtkBoxClass *parent_class;

/**
 * gnc_date_edit_get_type:
 *
 * Returns the GType for the GNCDateEdit widget
 */
GType
gnc_date_edit_get_type (void)
{
    static GType date_edit_type = 0;

    if (date_edit_type == 0)
    {
        static const GTypeInfo date_edit_info =
        {
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

        date_edit_type = g_type_register_static (GTK_TYPE_BOX,
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
    if (!s || *s == '\0')
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
#if GTK_CHECK_VERSION(3,20,0)
    GdkSeat *seat;
#else
    GdkDeviceManager *device_manager;
#endif
    GdkDevice *pointer;

    g_return_if_fail (GNC_IS_DATE_EDIT (gde));

    ENTER("gde %p", gde);

#if GTK_CHECK_VERSION(3,20,0)
    seat = gdk_display_get_default_seat (gdk_display_get_default());
    pointer = gdk_seat_get_pointer (seat);
#else
    device_manager = gdk_display_get_device_manager (gdk_display_get_default());
    pointer = gdk_device_manager_get_client_pointer (device_manager);
#endif

    gtk_grab_remove (gde->cal_popup);
    gtk_widget_hide (gde->cal_popup);
    if (pointer)
#if GTK_CHECK_VERSION(3,20,0)
        gdk_seat_ungrab (seat);
#else
        gdk_device_ungrab (pointer, GDK_CURRENT_TIME);
#endif

    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (gde->date_button),
                                  FALSE);

    LEAVE(" ");
}

static void
day_selected (GtkCalendar *calendar, GNCDateEdit *gde)
{
    time64 t;
    guint year, month, day;
    gde->in_selected_handler = TRUE;
    gtk_calendar_get_date (calendar, &year, &month, &day);
    /* GtkCalendar returns a 0-based month */
    t = gnc_dmy2time64 (day, month + 1, year);
    gnc_date_edit_set_time (gde, t);
    gde->in_selected_handler = FALSE;
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

    if (event->keyval != GDK_KEY_Return &&
            event->keyval != GDK_KEY_KP_Enter &&
            event->keyval != GDK_KEY_Escape)
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
    GtkAllocation alloc;

    gtk_widget_get_preferred_size (gde->cal_popup, &req, NULL);

    gdk_window_get_origin (gtk_widget_get_window (gde->date_button), &x, &y);

    gtk_widget_get_allocation (gde->date_button, &alloc);
    x += alloc.x;
    y += alloc.y;
    bwidth = alloc.width;
    bheight = alloc.height;

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
                      GdkDevice *keyboard,
                      GdkDevice *pointer,
                      guint32    activate_time)
{

#if GTK_CHECK_VERSION(3,20,0)
    GdkDisplay *display = gdk_display_get_default ();
    GdkSeat *seat = gdk_display_get_default_seat (display);
    GdkEvent *event = gtk_get_current_event ();

    if (keyboard && gdk_seat_grab (seat, window, GDK_SEAT_CAPABILITY_KEYBOARD, TRUE, NULL,
                                   event, NULL, NULL) != GDK_GRAB_SUCCESS )
#else
    if (keyboard && gdk_device_grab (keyboard, window,
                                     GDK_OWNERSHIP_WINDOW, TRUE,
                                     GDK_KEY_PRESS_MASK | GDK_KEY_RELEASE_MASK,
                                     NULL, activate_time) != GDK_GRAB_SUCCESS)
#endif
        return FALSE;

#if GTK_CHECK_VERSION(3,20,0)
    if (pointer && gdk_seat_grab (seat, window, GDK_SEAT_CAPABILITY_POINTER, TRUE, NULL,
                                  event, NULL, NULL) != GDK_GRAB_SUCCESS )
#else
    if (pointer && gdk_device_grab (pointer, window,
                                    GDK_OWNERSHIP_WINDOW, TRUE,
                                    GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK |
                                    GDK_POINTER_MOTION_MASK,
                                    NULL, activate_time) != GDK_GRAB_SUCCESS)
#endif
    {
        if (keyboard)
#if GTK_CHECK_VERSION(3,20,0)
            gdk_seat_ungrab (seat);
#else
            gdk_device_ungrab (keyboard, activate_time);
#endif
        return FALSE;
    }
    return TRUE;
}


static void
gnc_date_edit_popup (GNCDateEdit *gde)
{
    GtkWidget *toplevel;
    struct tm mtm;
    gboolean date_was_valid;
    GdkDevice *device, *keyboard, *pointer;

    g_return_if_fail (GNC_IS_DATE_EDIT (gde));

    ENTER("gde %p", gde);

    device = gtk_get_current_event_device ();

    /* This code is pretty much just copied from gtk_date_edit_get_date */
    date_was_valid = qof_scan_date (gtk_entry_get_text (GTK_ENTRY (gde->date_entry)),
                                    &mtm.tm_mday, &mtm.tm_mon, &mtm.tm_year);
    if (!date_was_valid)
    {
        /* No valid date. Hacky workaround: Instead of crashing we randomly choose today's date. */
        gnc_tm_get_today_start(&mtm);
    }

    mtm.tm_mon--;

    /* Hope the user does not actually mean years early in the A.D. days...
     * This date widget will obviously not work for a history program :-)
     */
    if (mtm.tm_year >= 1900)
        mtm.tm_year -= 1900;

    gnc_tm_set_day_start(&mtm);

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
            gtk_window_get_group (GTK_WINDOW (toplevel)),
            GTK_WINDOW (gde->cal_popup));
        gtk_window_set_transient_for (GTK_WINDOW (gde->cal_popup),
                                      GTK_WINDOW (toplevel));
    }

    position_popup (gde);

    gtk_widget_show (gde->cal_popup);

    gtk_widget_grab_focus (gde->cal_popup);
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (gde->date_button),
                                  TRUE);

    if (gdk_device_get_source (device) == GDK_SOURCE_KEYBOARD)
    {
        keyboard = device;
        pointer = gdk_device_get_associated_device (device);
    }
    else
    {
        pointer = device;
        keyboard = gdk_device_get_associated_device (device);
    }

    if (!gtk_widget_has_focus (gde->calendar))
        gtk_widget_grab_focus (gde->calendar);

    if (!popup_grab_on_window (gtk_widget_get_window ((GTK_WIDGET(gde->cal_popup))),
                               keyboard, pointer, GDK_CURRENT_TIME))
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

    if (!gtk_widget_has_focus (gde->date_button))
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

static void
set_time (GtkWidget *widget, GNCDateEdit *gde)
{
    gchar *text;
    GtkTreeModel *model;
    GtkTreeIter iter;

    model = gtk_combo_box_get_model(GTK_COMBO_BOX(gde->time_combo));
    gtk_combo_box_get_active_iter (GTK_COMBO_BOX(gde->time_combo), &iter);
    gtk_tree_model_get( model, &iter, 0, &text, -1 );

    gtk_entry_set_text (GTK_ENTRY (gde->time_entry), text);
    if(text)
        g_free(text);
    g_signal_emit (G_OBJECT (gde), date_edit_signals [TIME_CHANGED], 0);
}

static void
fill_time_combo (GtkWidget *widget, GNCDateEdit *gde)
{
    GtkTreeModel *model;
    GtkTreeIter  hour_iter, min_iter;
    struct tm *tm_returned;
    struct tm mtm;
    time64 current_time;
    int i, j;

    if (gde->lower_hour > gde->upper_hour)
        return;

    model = gtk_combo_box_get_model (GTK_COMBO_BOX(gde->time_combo));

    gnc_time (&current_time);
    tm_returned = gnc_localtime_r (&current_time, &mtm);
    g_return_if_fail(tm_returned != NULL);

    for (i = gde->lower_hour; i <= gde->upper_hour; i++)
    {
        char buffer [40];
        mtm.tm_hour = i;
        mtm.tm_min  = 0;

        if (gde->flags & GNC_DATE_EDIT_24_HR)
            qof_strftime (buffer, sizeof (buffer), "%H:00", &mtm);
        else
            qof_strftime (buffer, sizeof (buffer), "%I:00 %p ", &mtm);

        gtk_tree_store_append (GTK_TREE_STORE(model), &hour_iter, NULL);
        gtk_tree_store_set (GTK_TREE_STORE(model), &hour_iter, 0, buffer, -1);

        for (j = 0; j < 60; j += 15)
        {
            mtm.tm_min = j;

            if (gde->flags & GNC_DATE_EDIT_24_HR)
                qof_strftime (buffer, sizeof (buffer), "%H:%M", &mtm);
            else
                qof_strftime (buffer, sizeof (buffer), "%I:%M %p", &mtm);

            gtk_tree_store_append(GTK_TREE_STORE(model), &min_iter, &hour_iter );
            gtk_tree_store_set (GTK_TREE_STORE(model), &min_iter, 0, buffer, -1);
        }
    }
}

static void
gnc_date_edit_set_time_internal (GNCDateEdit *gde, time64 the_time)
{
    char buffer [MAX_DATE_LENGTH + 1];
    struct tm *mytm = gnc_localtime (&the_time);

    g_return_if_fail(mytm != NULL);

    /* Update the date text. */
    qof_print_date_dmy_buff(buffer, MAX_DATE_LENGTH,
                            mytm->tm_mday,
                            mytm->tm_mon + 1,
                            1900 + mytm->tm_year);
    gtk_entry_set_text(GTK_ENTRY(gde->date_entry), buffer);

    /* Update the calendar. */
    if (!gde->in_selected_handler)
    {
	gtk_calendar_select_day(GTK_CALENDAR (gde->calendar), 1);
	gtk_calendar_select_month(GTK_CALENDAR (gde->calendar),
				  mytm->tm_mon, 1900 + mytm->tm_year);
	gtk_calendar_select_day(GTK_CALENDAR (gde->calendar), mytm->tm_mday);
    }

    /* Set the time of day. */
    if (gde->flags & GNC_DATE_EDIT_24_HR)
        qof_strftime (buffer, sizeof (buffer), "%H:%M", mytm);
    else
        qof_strftime (buffer, sizeof (buffer), "%I:%M %p", mytm);
    gtk_entry_set_text(GTK_ENTRY(gde->time_entry), buffer);

    gnc_tm_free (mytm);

    g_signal_emit (gde, date_edit_signals [DATE_CHANGED], 0);
    g_signal_emit (gde, date_edit_signals [TIME_CHANGED], 0);
}


/** Retrieve a property specific to this GncPeriodSelect object.  This is
 *  nothing more than a dispatch function for routines that can be
 *  called directly.  It has the nice feature of allowing a single
 *  function call to retrieve multiple properties.
 *
 *  @internal
 */
static void
gnc_date_edit_get_property (GObject     *object,
                            guint        prop_id,
                            GValue      *value,
                            GParamSpec  *pspec)
{
    GNCDateEdit *date_edit = GNC_DATE_EDIT (object);

    switch (prop_id)
    {
    case PROP_TIME:
        g_value_set_int64 (value, gnc_date_edit_get_date (date_edit));
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
        break;
    }
}


/** Set a property specific to this GncDateEdit object.  This is
 *  nothing more than a dispatch function for routines that can be
 *  called directly.  It has the nice feature of allowing a new widget
 *  to be created with a varargs list specifying the properties,
 *  instead of having to explicitly call each property function.
 *
 *  @internal
 */
static void
gnc_date_edit_set_property (GObject      *object,
                            guint         prop_id,
                            const GValue *value,
                            GParamSpec   *pspec)
{
    GNCDateEdit *date_edit = GNC_DATE_EDIT (object);

    switch (prop_id)
    {
    case PROP_TIME:
        gnc_date_edit_set_time_internal (date_edit, g_value_get_int64(value));
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
        break;
    }
}

static void
gnc_date_edit_class_init (GNCDateEditClass *klass)
{
    GtkContainerClass *container_class = (GtkContainerClass *) klass;
    GObjectClass *object_class = (GObjectClass *) klass;

    container_class->forall = gnc_date_edit_forall;
    object_class->set_property = gnc_date_edit_set_property;
    object_class->get_property = gnc_date_edit_get_property;
    object_class->dispose = gnc_date_edit_dispose;
    object_class->finalize = gnc_date_edit_finalize;

    parent_class = g_type_class_ref(GTK_TYPE_BOX);

    date_edit_signals [TIME_CHANGED] =
        g_signal_new ("time_changed",
                      G_TYPE_FROM_CLASS (object_class),
                      G_SIGNAL_RUN_FIRST,
                      G_STRUCT_OFFSET (GNCDateEditClass, time_changed),
                      NULL, NULL,
                      g_cclosure_marshal_VOID__VOID,
                      G_TYPE_NONE, 0);

    date_edit_signals [DATE_CHANGED] =
        g_signal_new ("date_changed",
                      G_TYPE_FROM_CLASS (object_class),
                      G_SIGNAL_RUN_FIRST,
                      G_STRUCT_OFFSET (GNCDateEditClass, date_changed),
                      NULL, NULL,
                      g_cclosure_marshal_VOID__VOID,
                      G_TYPE_NONE, 0);

    g_object_class_install_property(object_class,
                                    PROP_TIME,
                                    g_param_spec_int64("time",
                                            "Date/time (seconds)",
                                            "Date/time represented in seconds since Januari 31st, 1970",
                                            G_MININT64,
                                            G_MAXINT64,
                                            0,
                                            G_PARAM_READWRITE));

    klass->date_changed = NULL;
    klass->time_changed = NULL;
}

static void
gnc_date_edit_init (GNCDateEdit *gde)
{
    gtk_orientable_set_orientation (GTK_ORIENTABLE(gde), GTK_ORIENTATION_HORIZONTAL);

    // Set the style context for this widget so it can be easily manipulated with css
    gnc_widget_set_style_context (GTK_WIDGET(gde), "GncDateEdit");

    gde->disposed = FALSE;
    gde->popup_in_progress = FALSE;
    gde->lower_hour = 7;
    gde->upper_hour = 19;
    gde->flags = GNC_DATE_EDIT_SHOW_TIME;
    gde->in_selected_handler = FALSE;
}

static void
gnc_date_edit_finalize (GObject *object)
{

    g_return_if_fail (object != NULL);
    g_return_if_fail (GNC_IS_DATE_EDIT (object));

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

    if (gde->disposed)
        return;

    gde->disposed = TRUE;

    /* Only explicitly destroy the toplevel elements */

    gtk_widget_destroy (GTK_WIDGET(gde->date_entry));
    gde->date_entry = NULL;

    gtk_widget_destroy (GTK_WIDGET(gde->date_button));
    gde->date_button = NULL;

    gtk_widget_destroy (GTK_WIDGET(gde->time_entry));
    gde->time_entry = NULL;

    gtk_widget_destroy (GTK_WIDGET(gde->time_combo));
    gde->time_combo = NULL;

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

/**
 * gnc_date_edit_set_time:
 * @gde: the GNCDateEdit widget
 * @the_time: The time and date that should be set on the widget
 *
 * Changes the displayed date and time in the GNCDateEdit widget
 * to be the one represented by @the_time.
 */
void
gnc_date_edit_set_time (GNCDateEdit *gde, time64 the_time)
{
    g_return_if_fail (gde != NULL);
    g_return_if_fail (GNC_IS_DATE_EDIT (gde));

    /* If the_time is invalid, use the last valid time
     * seen (or as a last resort, the current date). */
    gde->initial_time = the_time;

    g_object_set (G_OBJECT (gde), "time", the_time, NULL);
}

void
gnc_date_edit_set_gdate (GNCDateEdit *gde, const GDate *date)
{
    struct tm mytm;
    time64 t;

    g_return_if_fail(gde && GNC_IS_DATE_EDIT(gde) &&
                     date && g_date_valid(date));
    g_date_to_struct_tm(date, &mytm);
    t = gnc_mktime(&mytm);
    gnc_date_edit_set_time(gde, t);
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

    fill_time_combo(NULL, gde);
}

/* This code should be kept in sync with src/register/datecell.c */
static int
date_accel_key_press(GtkWidget *widget, GdkEventKey *event, gpointer data)
{
    GNCDateEdit *gde = data;
    const char *string;
    struct tm tm;

    string = gtk_entry_get_text (GTK_ENTRY (widget));

    tm = gnc_date_edit_get_date_internal (gde);

    if (!gnc_handle_date_accelerator (event, &tm, string))
        return FALSE;

    gnc_date_edit_set_time (gde, gnc_mktime (&tm));

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
    gnc_date_edit_set_time (gde, gnc_mktime (&tm));

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
    GtkTreeStore *store;
    GtkCellRenderer *cell;

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

    hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 3);
    gtk_box_set_homogeneous (GTK_BOX (hbox), FALSE);
    gtk_container_add (GTK_CONTAINER (gde->date_button), hbox);
    gtk_widget_show (GTK_WIDGET(hbox));

    /* Calendar label, only shown if the date editor has a time field */
    gde->cal_label = gtk_label_new (_("Calendar"));
    gnc_label_set_alignment (gde->cal_label, 0.0, 0.5);
    gtk_box_pack_start (GTK_BOX (hbox), gde->cal_label, TRUE, TRUE, 0);
    if (gde->flags & GNC_DATE_EDIT_SHOW_TIME)
        gtk_widget_show (GTK_WIDGET(gde->cal_label));

    /* Graphic for the popup button. */
    arrow = gtk_image_new_from_icon_name ("go-down", GTK_ICON_SIZE_BUTTON);

    g_signal_connect (G_OBJECT (arrow), "draw",
                      G_CALLBACK (gnc_draw_arrow_cb), GINT_TO_POINTER(1));

    gtk_box_pack_start (GTK_BOX (hbox), arrow, TRUE, FALSE, 0);
    gtk_widget_show (GTK_WIDGET(arrow));

    gtk_widget_show (GTK_WIDGET(gde->date_button));

    /* Time entry controls. */
    gde->time_entry = gtk_entry_new ();
    gtk_entry_set_max_length (GTK_ENTRY(gde->time_entry), 12);
    gtk_widget_set_size_request (GTK_WIDGET(gde->time_entry), 88, -1);
    gtk_box_pack_start (GTK_BOX (gde), gde->time_entry, TRUE, TRUE, 0);

    store = gtk_tree_store_new(1, G_TYPE_STRING);
    gde->time_combo = GTK_WIDGET(gtk_combo_box_new_with_model(GTK_TREE_MODEL(store)));
    g_object_unref(store);
    /* Create cell renderer. */
    cell = gtk_cell_renderer_text_new();
    /* Pack it to the combo box. */
    gtk_cell_layout_pack_start( GTK_CELL_LAYOUT( gde->time_combo ), cell, TRUE );
    /* Connect renderer to data source */
    gtk_cell_layout_set_attributes( GTK_CELL_LAYOUT( gde->time_combo ), cell, "text", 0, NULL );

    g_signal_connect (G_OBJECT (gde->time_combo), "changed",
                      G_CALLBACK  (set_time), gde);

    gtk_box_pack_start (GTK_BOX (gde), gde->time_combo, FALSE, FALSE, 0);

    /* We do not create the popup menu with the hour range until we are
     * realized, so that it uses the values that the user might supply in a
     * future call to gnc_date_edit_set_popup_range
     */
    g_signal_connect (G_OBJECT (gde), "realize",
                      G_CALLBACK  (fill_time_combo), gde);

    if (gde->flags & GNC_DATE_EDIT_SHOW_TIME)
    {
        gtk_widget_show (GTK_WIDGET(gde->time_entry));
        gtk_widget_show (GTK_WIDGET(gde->time_combo));
    }

    gde->cal_popup = gtk_window_new (GTK_WINDOW_POPUP);
    gtk_widget_set_name (gde->cal_popup, "gnc-date-edit-popup-window");

    gtk_window_set_type_hint (GTK_WINDOW (gde->cal_popup),
                              GDK_WINDOW_TYPE_HINT_COMBO);

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
      | GTK_CALENDAR_SHOW_HEADING));
    g_signal_connect (gde->calendar, "button-release-event",
                      G_CALLBACK(gnc_date_edit_button_released), gde);
    g_signal_connect (G_OBJECT (gde->calendar), "day-selected",
		      G_CALLBACK (day_selected), gde);
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
gnc_date_edit_new (time64 the_time, int show_time, int use_24_format)
{
    return gnc_date_edit_new_flags
           (the_time,
            ((show_time ? GNC_DATE_EDIT_SHOW_TIME : 0)
             | (use_24_format ? GNC_DATE_EDIT_24_HR : 0)));
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
gnc_date_edit_new_flags (time64 the_time, GNCDateEditFlags flags)
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
    gboolean date_was_valid;

    /* Assert, because we're just hosed if it's NULL */
    g_assert(gde != NULL);
    g_assert(GNC_IS_DATE_EDIT(gde));

    date_was_valid = qof_scan_date (gtk_entry_get_text (GTK_ENTRY (gde->date_entry)),
                                    &tm.tm_mday, &tm.tm_mon, &tm.tm_year);

    if (!date_was_valid)
    {
        /* Hm... no valid date. What should we do not? As a hacky workaround we
        revert to today's date. Alternatively we can return some value that
        signals that we don't get a valid date, but all callers of this
        function will have to check this. Alas, I'm too lazy to do this here. */
        gnc_tm_get_today_start(&tm);
    }

    tm.tm_mon--;

    tm.tm_year -= 1900;

    if (gde->flags & GNC_DATE_EDIT_SHOW_TIME)
    {
        char *tokp = NULL;
        gchar *temp;

        str = g_strdup (gtk_entry_get_text
                        (GTK_ENTRY (gde->time_entry)));
        temp = gnc_strtok_r (str, ": ", &tokp);
        if (temp)
        {
            tm.tm_hour = atoi (temp);
            temp = gnc_strtok_r (NULL, ": ", &tokp);
            if (temp)
            {
                if (isdigit (*temp))
                {
                    tm.tm_min = atoi (temp);
                    flags = gnc_strtok_r (NULL, ": ",
                                          &tokp);
                    if (flags && isdigit (*flags))
                    {
                        tm.tm_sec = atoi (flags);
                        flags = gnc_strtok_r (NULL,
                                              ": ",
                                              &tokp);
                    }
                }
                else
                    flags = temp;
            }
        }

        if (flags && (strcasecmp (flags, "PM") == 0))
        {
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
time64
gnc_date_edit_get_date (GNCDateEdit *gde)
{
    struct tm tm;

    g_return_val_if_fail (gde != NULL, 0);
    g_return_val_if_fail (GNC_IS_DATE_EDIT (gde), 0);

    tm = gnc_date_edit_get_date_internal (gde);

    return gnc_mktime (&tm);
}

void
gnc_date_edit_get_gdate (GNCDateEdit *gde, GDate *date)
{
    time64 t;

    g_return_if_fail (gde && date);
    g_return_if_fail (GNC_IS_DATE_EDIT (gde));

    t = gnc_date_edit_get_date(gde);
    g_date_clear (date, 1);
    gnc_gdate_set_time64 (date, t);
}

/**
 * gnc_date_edit_get_date_end:
 * @gde: The GNCDateEdit widget
 *
 * Returns the date entered in the GNCDateEdit widget,
 * but with the time adjusted to the end of the day.
 */
time64
gnc_date_edit_get_date_end (GNCDateEdit *gde)
{
    struct tm tm;

    g_return_val_if_fail (gde != NULL, 0);
    g_return_val_if_fail (GNC_IS_DATE_EDIT (gde), 0);

    tm = gnc_date_edit_get_date_internal (gde);
    gnc_tm_set_day_end(&tm);

    return gnc_mktime (&tm);
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
            (old_flags & GNC_DATE_EDIT_SHOW_TIME))
    {
        if (flags & GNC_DATE_EDIT_SHOW_TIME)
        {
            gtk_widget_show (gde->cal_label);
            gtk_widget_show (gde->time_entry);
            gtk_widget_show (gde->time_combo);
        }
        else
        {
            gtk_widget_hide (gde->cal_label);
            gtk_widget_hide (gde->time_entry);
            gtk_widget_hide (gde->time_combo);
        }
    }

    if ((flags & GNC_DATE_EDIT_24_HR) != (old_flags & GNC_DATE_EDIT_24_HR))
        /* This will destroy the old menu properly */
        fill_time_combo (NULL, gde);

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


