/********************************************************************\
 * gnucash-date-picker.c -- A popup date picker using gtk_calendar  *
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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

/*
 *  A popup date picker for the canvas using gtk_calendar.
 */

#include <config.h>
#include <gdk/gdk.h>
#include "gnucash-date-picker.h"
#include "gnc-gtk-utils.h"

/* Item list signals */
enum
{
    DATE_SELECTED,
    DATE_PICKED,
    CANCELLED,
    LAST_SIGNAL
};

static guint gnc_date_picker_signals[LAST_SIGNAL];

G_DEFINE_TYPE (GNCDatePicker, gnc_date_picker, GTK_TYPE_BOX)

void
gnc_date_picker_set_date (GNCDatePicker *date_picker,
                          guint day, guint mon, guint year)
{
    g_return_if_fail (IS_GNC_DATE_PICKER (date_picker));
    g_return_if_fail (date_picker->calendar != NULL);

    gtk_calendar_set_year (date_picker->calendar, year);
    gtk_calendar_set_month (date_picker->calendar, mon);
    gtk_calendar_set_day (date_picker->calendar, day);
}

void
gnc_date_picker_get_date (GNCDatePicker *date_picker,
                          guint *day, guint *mon, guint *year)
{
    GDateTime *date;

    g_return_if_fail (IS_GNC_DATE_PICKER (date_picker));
    g_return_if_fail (date_picker->calendar != NULL);

    date = gtk_calendar_get_date (date_picker->calendar);
    g_return_if_fail (date != NULL);

    *day = g_date_time_get_day_of_month (date);
    *mon = g_date_time_get_month (date) - 1;
    *year = g_date_time_get_year (date);
    g_date_time_unref (date);
}

static void
gnc_date_picker_init (GNCDatePicker *date_picker)
{
    gtk_orientable_set_orientation (GTK_ORIENTABLE(date_picker), GTK_ORIENTATION_HORIZONTAL);
    date_picker->calendar = NULL;
}

static gboolean
gnc_date_picker_key_pressed (G_GNUC_UNUSED GtkEventControllerKey *controller,
                             guint keyval,
                             G_GNUC_UNUSED guint keycode,
                             G_GNUC_UNUSED GdkModifierType state,
                             gpointer data)
{
    GNCDatePicker *date_picker = GNC_DATE_PICKER (data);

    switch (keyval)
    {
    case GDK_KEY_Return:
    case GDK_KEY_KP_Enter:
        g_signal_emit (date_picker, gnc_date_picker_signals[DATE_PICKED], 0);
        return TRUE;

    case GDK_KEY_Escape:
        g_signal_emit (date_picker, gnc_date_picker_signals[CANCELLED], 0);
        return TRUE;
    }

    return GDK_EVENT_PROPAGATE;
}
static void
gnc_date_picker_class_init (GNCDatePickerClass *date_picker_class)
{
    GObjectClass  *object_class = G_OBJECT_CLASS (date_picker_class);

    gtk_widget_class_set_css_name (GTK_WIDGET_CLASS(date_picker_class), "gnc-id-date-picker");

    gnc_date_picker_signals[DATE_SELECTED] =
        g_signal_new("date_selected",
                     G_TYPE_FROM_CLASS(object_class),
                     G_SIGNAL_RUN_LAST,
                     G_STRUCT_OFFSET(GNCDatePickerClass, date_selected),
                     NULL, NULL,
                     g_cclosure_marshal_VOID__VOID,
                     G_TYPE_NONE, 0);

    gnc_date_picker_signals[DATE_PICKED] =
        g_signal_new("date_picked",
                     G_TYPE_FROM_CLASS(object_class),
                     G_SIGNAL_RUN_LAST,
                     G_STRUCT_OFFSET(GNCDatePickerClass, date_picked),
                     NULL, NULL,
                     g_cclosure_marshal_VOID__VOID,
                     G_TYPE_NONE, 0);

    gnc_date_picker_signals[CANCELLED] =
        g_signal_new ("cancelled",
                      G_TYPE_FROM_CLASS (object_class),
                      G_SIGNAL_RUN_LAST,
                      G_STRUCT_OFFSET (GNCDatePickerClass, cancelled),
                      NULL, NULL,
                      g_cclosure_marshal_VOID__VOID,
                      G_TYPE_NONE, 0);

    date_picker_class->date_selected = NULL;
    date_picker_class->date_picked = NULL;
    date_picker_class->cancelled = NULL;
}


static void
day_selected (GtkCalendar *calendar, GNCDatePicker *gdp)
{
    (void)calendar;

    g_signal_emit (gdp, gnc_date_picker_signals [DATE_SELECTED], 0);
}

static void
calendar_released (GtkGestureClick *gesture, gint n_press,
                  gdouble x, gdouble y, GNCDatePicker *gdp)
{
    (void)gesture;

    if (gnc_gtk_calendar_double_clicks_day (gdp->calendar, n_press, x, y))
        g_signal_emit (gdp, gnc_date_picker_signals [DATE_PICKED], 0);
}


GtkWidget *
gnc_date_picker_new (void)
{
    GtkWidget *calendar;
    GNCDatePicker *date_picker;
    GtkEventController *key_controller;
    GtkGesture *click_gesture;

    date_picker = g_object_new (GNC_TYPE_DATE_PICKER,
                                "homogeneous", FALSE,
                                NULL);

    calendar = gtk_calendar_new ();
    date_picker->calendar = GTK_CALENDAR (calendar);

    gtk_box_append (GTK_BOX (date_picker), calendar);

    key_controller = gtk_event_controller_key_new ();
    gtk_event_controller_set_static_name (key_controller, "gnc-date-picker-key");
    g_signal_connect (key_controller, "key-pressed",
                      G_CALLBACK (gnc_date_picker_key_pressed), date_picker);
    gtk_widget_add_controller (calendar, key_controller);
    g_signal_connect (calendar, "day-selected",
                      G_CALLBACK (day_selected),
                      date_picker);

    click_gesture = gtk_gesture_click_new ();
    gtk_event_controller_set_static_name (GTK_EVENT_CONTROLLER (click_gesture),
                                          "gnc-date-picker-calendar-double-click");
    g_signal_connect (click_gesture, "released", G_CALLBACK (calendar_released),
                      date_picker);
    gtk_widget_add_controller (calendar, GTK_EVENT_CONTROLLER (click_gesture));

    return GTK_WIDGET(date_picker);
}
