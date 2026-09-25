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
#include <string.h>
#include <stdlib.h> /* atoi */
#include <ctype.h> /* isdigit */
#include <stdio.h>

#include "gnc-date.h"
#include "gnc-engine.h"
#include "dialog-utils.h"
#include "gnc-gtk-utils.h"
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

static void gnc_date_edit_dispose      (GObject          *object);
static void gnc_date_edit_finalize     (GObject          *object);
static struct tm gnc_date_edit_get_date_internal (GNCDateEdit *gde);
static void gnc_date_edit_button_toggled (GtkWidget *widget, GNCDateEdit *gde);
static gboolean date_accel_key_pressed (GtkEventControllerKey *controller,
                                        guint keyval, guint keycode,
                                        GdkModifierType state, gpointer data);
static gboolean key_pressed_popup (GtkEventControllerKey *controller,
                                    guint keyval, guint keycode,
                                    GdkModifierType state, gpointer data);
static void date_focus_leave (GtkEventControllerFocus *controller,
                              gpointer data);

G_DEFINE_TYPE (GNCDateEdit, gnc_date_edit, GTK_TYPE_BOX)

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
    g_return_if_fail (GNC_IS_DATE_EDIT (gde));

    ENTER("gde %p", gde);

    gtk_popover_popdown (GTK_POPOVER (gde->cal_popup));
    if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (gde->date_button)))
    {
        g_signal_handlers_block_by_func (gde->date_button,
                                         G_CALLBACK (gnc_date_edit_button_toggled),
                                         gde);
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (gde->date_button), FALSE);
        g_signal_handlers_unblock_by_func (gde->date_button,
                                           G_CALLBACK (gnc_date_edit_button_toggled),
                                           gde);
    }

    LEAVE(" ");
}

static void
day_selected (GtkCalendar *calendar, GNCDateEdit *gde)
{
    time64 t;
    GDateTime *date;

    gde->in_selected_handler = TRUE;
    date = gtk_calendar_get_date (calendar);
    t = gnc_dmy2time64 (g_date_time_get_day_of_month (date),
                         g_date_time_get_month (date),
                         g_date_time_get_year (date));
    g_date_time_unref (date);
    gnc_date_edit_set_time (gde, t);
    gde->in_selected_handler = FALSE;
}

static void
calendar_released (GtkGestureClick *gesture, gint n_press,
                  gdouble x, gdouble y, GNCDateEdit *gde)
{
    (void)gesture;

    /* Run after GtkCalendar's own press handler selected the day. The
     * documented day-number CSS class excludes its header and week labels. */
    if (gnc_gtk_calendar_double_clicks_day (GTK_CALENDAR (gde->calendar),
                                            n_press, x, y))
        gnc_date_edit_popdown (gde);
}

static gboolean
key_pressed_popup (GtkEventControllerKey *controller, guint keyval,
                   guint keycode, GdkModifierType state, gpointer data)
{
    GNCDateEdit *gde = data;

    if (keyval != GDK_KEY_Return && keyval != GDK_KEY_KP_Enter &&
        keyval != GDK_KEY_Escape)
        return date_accel_key_pressed (controller, keyval, keycode, state, data);

    gnc_date_edit_popdown (gde);

    return TRUE;
}

static void
gnc_date_edit_popup_closed (GtkPopover *popover, GNCDateEdit *gde)
{
    (void)popover;

    if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (gde->date_button)))
    {
        g_signal_handlers_block_by_func (gde->date_button,
                                         G_CALLBACK (gnc_date_edit_button_toggled),
                                         gde);
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (gde->date_button), FALSE);
        g_signal_handlers_unblock_by_func (gde->date_button,
                                           G_CALLBACK (gnc_date_edit_button_toggled),
                                           gde);
    }
}


static void
gnc_date_edit_popup (GNCDateEdit *gde)
{
    struct tm mtm;
    gboolean date_was_valid;

    g_return_if_fail (GNC_IS_DATE_EDIT (gde));

    ENTER("gde %p", gde);

    /* This code is pretty much just copied from gtk_date_edit_get_date */
    date_was_valid = qof_scan_date (gtk_editable_get_text (GTK_EDITABLE (gde->date_entry)),
                                    &mtm.tm_mday, &mtm.tm_mon, &mtm.tm_year);
    if (!date_was_valid)
    {
        /* No valid date. Hacky workaround: Instead of crashing we randomly choose today's date. */
        gnc_tm_get_today_neutral(&mtm);
    }

    mtm.tm_mon--;

    /* Hope the user does not actually mean years early in the A.D. days...
     * This date widget will obviously not work for a history program :-)
     */
    if (mtm.tm_year >= 1900)
        mtm.tm_year -= 1900;

    gnc_tm_set_day_neutral(&mtm);

    gtk_calendar_set_year (GTK_CALENDAR (gde->calendar), 1900 + mtm.tm_year);
    gtk_calendar_set_month (GTK_CALENDAR (gde->calendar), mtm.tm_mon);
    gtk_calendar_set_day (GTK_CALENDAR (gde->calendar), mtm.tm_mday);

    gtk_popover_popup (GTK_POPOVER (gde->cal_popup));

    if (!gtk_widget_has_focus (gde->calendar))
        gtk_widget_grab_focus (gde->calendar);

    LEAVE(" ");
}

static void
gnc_date_edit_button_toggled (GtkWidget *widget, GNCDateEdit *gde)
{
    ENTER("widget %p, gde %p", widget, gde);

    if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (widget)))
        gnc_date_edit_popup (gde);
    else
        gnc_date_edit_popdown (gde);

    LEAVE(" ");
}

static void
set_time (GObject *object, GParamSpec *pspec, GNCDateEdit *gde)
{
    GtkDropDown *dropdown = GTK_DROP_DOWN (object);
    guint selected = gtk_drop_down_get_selected (dropdown);
    GListModel *model = gtk_drop_down_get_model (dropdown);
    GtkStringObject *item;

    (void)pspec;

    if (selected == GTK_INVALID_LIST_POSITION)
        return;

    item = GTK_STRING_OBJECT (g_list_model_get_item (model, selected));
    gtk_editable_set_text (GTK_EDITABLE (gde->time_entry),
                        gtk_string_object_get_string (item));
    g_object_unref (item);
    g_signal_emit (G_OBJECT (gde), date_edit_signals [TIME_CHANGED], 0);
}

static void
fill_time_combo (GtkWidget *widget, GNCDateEdit *gde)
{
    GtkStringList *model;
    struct tm *tm_returned;
    struct tm mtm;
    time64 current_time;
    int i, j;

    if (gde->lower_hour > gde->upper_hour)
        return;

    model = GTK_STRING_LIST (gtk_drop_down_get_model (GTK_DROP_DOWN (gde->time_combo)));
    gtk_string_list_splice (model, 0,
                            g_list_model_get_n_items (G_LIST_MODEL (model)), NULL);

    gnc_time (&current_time);
    tm_returned = gnc_localtime_r (&current_time, &mtm);
    g_return_if_fail(tm_returned != NULL);

    for (i = gde->lower_hour; i <= gde->upper_hour; i++)
    {
        char buffer [40];
        mtm.tm_hour = i;
        mtm.tm_min  = 0;

        for (j = 0; j < 60; j += 15)
        {
            mtm.tm_min = j;

            if (gde->flags & GNC_DATE_EDIT_24_HR)
                qof_strftime (buffer, sizeof (buffer), "%H:%M", &mtm);
            else
                qof_strftime (buffer, sizeof (buffer), "%I:%M %p", &mtm);

            gtk_string_list_append (model, buffer);
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
    gtk_editable_set_text (GTK_EDITABLE (gde->date_entry), buffer);

    /* Update the calendar. */
    if (!gde->in_selected_handler)
    {
        gtk_calendar_set_year (GTK_CALENDAR (gde->calendar), 1900 + mytm->tm_year);
        gtk_calendar_set_month (GTK_CALENDAR (gde->calendar), mytm->tm_mon);
        gtk_calendar_set_day (GTK_CALENDAR (gde->calendar), mytm->tm_mday);
    }

    /* Set the time of day. */
    if (gde->flags & GNC_DATE_EDIT_24_HR)
        qof_strftime (buffer, sizeof (buffer), "%H:%M", mytm);
    else
        qof_strftime (buffer, sizeof (buffer), "%I:%M %p", mytm);
    gtk_editable_set_text (GTK_EDITABLE (gde->time_entry), buffer);

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
    GObjectClass *object_class = (GObjectClass *) klass;

    object_class->set_property = gnc_date_edit_set_property;
    object_class->get_property = gnc_date_edit_get_property;
    object_class->dispose = gnc_date_edit_dispose;
    object_class->finalize = gnc_date_edit_finalize;

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
                                            "Date/time represented in seconds since midnight UTC, 1 January 1970",
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

    // Set the name for this widget so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(gde), "gnc-id-date-edit");

    gde->disposed = FALSE;
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

    G_OBJECT_CLASS (gnc_date_edit_parent_class)->finalize (object);
}

static void
disconnect_widget_callbacks (GtkWidget *widget, GNCDateEdit *gde)
{
    GListModel *controllers;

    if (!widget)
        return;

    g_signal_handlers_disconnect_by_data (widget, gde);

    controllers = gtk_widget_observe_controllers (widget);
    for (guint index = 0; index < g_list_model_get_n_items (controllers); index++)
    {
        GtkEventController *controller =
            g_list_model_get_item (controllers, index);

        g_signal_handlers_disconnect_by_data (controller, gde);
        g_object_unref (controller);
    }
    g_object_unref (controllers);
}

static void
gnc_date_edit_dispose (GObject *object)
{
    GNCDateEdit *gde;

    g_return_if_fail (object != NULL);
    g_return_if_fail (GNC_IS_DATE_EDIT (object));

    gde = GNC_DATE_EDIT (object);

    if (!gde->disposed)
    {
        gde->disposed = TRUE;

        /* External references can keep child widgets and their controllers alive
         * beyond this editor. Disconnect all callbacks that borrow @gde first. */
        disconnect_widget_callbacks (gde->date_entry, gde);
        disconnect_widget_callbacks (gde->date_button, gde);
        disconnect_widget_callbacks (gde->time_combo, gde);
        disconnect_widget_callbacks (gde->cal_popup, gde);
        disconnect_widget_callbacks (gde->calendar, gde);

        /* The popover was attached manually, so it isn't a GtkBox child for the
         * parent dispose to remove. Detach it before its toggle-button parent is
         * finalized, while held external references remain valid. */
        if (gde->cal_popup)
            gtk_widget_unparent (gde->cal_popup);

        gde->date_entry = NULL;

        gde->date_button = NULL;

        gde->time_entry = NULL;

        gde->time_combo = NULL;

        gde->cal_popup = NULL;

        gde->calendar = NULL;

        gde->cal_label = NULL;
    }

    G_OBJECT_CLASS (gnc_date_edit_parent_class)->dispose (object);
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
static gboolean
date_accel_key_pressed (GtkEventControllerKey *controller, guint keyval,
                        guint keycode, GdkModifierType state, gpointer data)
{
    GNCDateEdit *gde = data;
    const char *string;
    struct tm tm;
    GncRegisterInput input;

    (void)controller;
    (void)keycode;

    string = gtk_editable_get_text (GTK_EDITABLE (gde->date_entry));

    tm = gnc_date_edit_get_date_internal (gde);

    gnc_register_input_from_keyval (&input, keyval, state);

    if (!gnc_handle_date_accelerator_input (&input, &tm, string))
        return FALSE;

    gnc_date_edit_set_time (gde, gnc_mktime (&tm));

    g_signal_emit (G_OBJECT (gde), date_edit_signals [TIME_CHANGED], 0);
    return TRUE;
}

static void
date_focus_leave (GtkEventControllerFocus *controller, gpointer data)
{
    GNCDateEdit *gde = data;
    struct tm tm;

    (void)controller;

    /* Get the date entered and attempt to use it. */
    tm = gnc_date_edit_get_date_internal (gde);
    gnc_date_edit_set_time (gde, gnc_mktime (&tm));

    g_signal_emit (gde, date_edit_signals [DATE_CHANGED], 0);
    g_signal_emit (gde, date_edit_signals [TIME_CHANGED], 0);

}

static void
create_children (GNCDateEdit *gde)
{
    GtkWidget *frame;
    GtkWidget *hbox;
    GtkWidget *arrow;
    GtkStringList *time_model;
    GtkEventController *key_controller;
    GtkEventController *focus_controller;
    GtkGesture *click_gesture;

    /* Create the text entry area. */
    gde->date_entry  = gtk_entry_new ();
    gtk_editable_set_width_chars (GTK_EDITABLE (gde->date_entry), 11);
    gnc_box_append_full (GTK_BOX (gde), gde->date_entry, TRUE, TRUE, 0);
    gtk_widget_set_visible (GTK_WIDGET(gde->date_entry), TRUE);
    key_controller = gtk_event_controller_key_new ();
    gtk_widget_add_controller (gde->date_entry, key_controller);
    g_signal_connect (key_controller, "key-pressed",
                      G_CALLBACK (date_accel_key_pressed), gde);
    focus_controller = gtk_event_controller_focus_new ();
    gtk_widget_add_controller (gde->date_entry, focus_controller);
    g_signal_connect (focus_controller, "leave", G_CALLBACK (date_focus_leave), gde);

    /* Create the popup button. */
    gde->date_button = gtk_toggle_button_new ();
    g_signal_connect (G_OBJECT (gde->date_button), "toggled",
                      G_CALLBACK (gnc_date_edit_button_toggled), gde);
    gnc_box_append_full (GTK_BOX (gde), gde->date_button, FALSE, FALSE, 0);

    hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 3);
    gtk_box_set_homogeneous (GTK_BOX (hbox), FALSE);
    gtk_button_set_child (GTK_BUTTON(gde->date_button), hbox);
    gtk_widget_set_visible (GTK_WIDGET(hbox), TRUE);

    /* Calendar label, only shown if the date editor has a time field */
    gde->cal_label = gtk_label_new (_("Calendar"));
    gnc_label_set_alignment (gde->cal_label, 0.0, 0.5);
    gnc_box_append_full (GTK_BOX (hbox), gde->cal_label, TRUE, TRUE, 0);
    if (gde->flags & GNC_DATE_EDIT_SHOW_TIME)
        gtk_widget_set_visible (GTK_WIDGET(gde->cal_label), TRUE);

    /* Graphic for the popup button. */
    arrow = gtk_image_new_from_icon_name ("pan-down-symbolic");
    gtk_image_set_icon_size (GTK_IMAGE (arrow), GTK_ICON_SIZE_NORMAL);

    gnc_box_append_full (GTK_BOX (hbox), arrow, TRUE, FALSE, 0);
    gtk_widget_set_visible (GTK_WIDGET(arrow), TRUE);

    gtk_widget_set_visible (GTK_WIDGET(gde->date_button), TRUE);

    /* Time entry controls. */
    gde->time_entry = gtk_entry_new ();
    gtk_entry_set_max_length (GTK_ENTRY(gde->time_entry), 12);
    gtk_widget_set_size_request (GTK_WIDGET(gde->time_entry), 88, -1);
    gnc_box_append_full (GTK_BOX (gde), gde->time_entry, TRUE, TRUE, 0);

    time_model = gtk_string_list_new (NULL);
    gde->time_combo = GTK_WIDGET (gnc_gtk_drop_down_new (G_LIST_MODEL (time_model), NULL));

    g_signal_connect (gde->time_combo, "notify::selected",
                      G_CALLBACK (set_time), gde);

    gnc_box_append_full (GTK_BOX (gde), gde->time_combo, FALSE, FALSE, 0);

    fill_time_combo (NULL, gde);

    if (gde->flags & GNC_DATE_EDIT_SHOW_TIME)
    {
        gtk_widget_set_visible (GTK_WIDGET(gde->time_entry), TRUE);
        gtk_widget_set_visible (GTK_WIDGET(gde->time_combo), TRUE);
    }

    gde->cal_popup = gtk_popover_new ();
    gtk_widget_set_name (gde->cal_popup, "gnc-date-edit-popup");
    gtk_popover_set_autohide (GTK_POPOVER (gde->cal_popup), TRUE);
    gtk_popover_set_position (GTK_POPOVER (gde->cal_popup), GTK_POS_BOTTOM);
    gtk_widget_set_parent (gde->cal_popup, gde->date_button);
    g_signal_connect (gde->cal_popup, "closed",
                      G_CALLBACK (gnc_date_edit_popup_closed), gde);

    key_controller = gtk_event_controller_key_new ();
    gtk_event_controller_set_static_name (key_controller,
                                          "gnc-date-edit-popup-key");
    gtk_widget_add_controller (gde->cal_popup, key_controller);
    g_signal_connect (key_controller, "key-pressed",
                      G_CALLBACK (key_pressed_popup), gde);

    frame = gtk_frame_new (NULL);
    gtk_popover_set_child (GTK_POPOVER (gde->cal_popup), frame);
    gtk_widget_set_visible (GTK_WIDGET(frame), TRUE);

    gde->calendar = gtk_calendar_new ();
    gtk_calendar_set_show_day_names (GTK_CALENDAR (gde->calendar), TRUE);
    gtk_calendar_set_show_heading (GTK_CALENDAR (gde->calendar), TRUE);
    g_signal_connect (G_OBJECT (gde->calendar), "day-selected",
		      G_CALLBACK (day_selected), gde);
    click_gesture = gtk_gesture_click_new ();
    gtk_event_controller_set_static_name (GTK_EVENT_CONTROLLER (click_gesture),
                                          "gnc-date-edit-calendar-double-click");
    g_signal_connect (click_gesture, "released", G_CALLBACK (calendar_released), gde);
    gtk_widget_add_controller (gde->calendar, GTK_EVENT_CONTROLLER (click_gesture));
    gtk_frame_set_child (GTK_FRAME(frame), gde->calendar);
    gtk_widget_set_visible (GTK_WIDGET(gde->calendar), TRUE);
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
    gtk_widget_set_visible (widget, TRUE);
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

    date_was_valid = qof_scan_date (gtk_editable_get_text (GTK_EDITABLE (gde->date_entry)),
                                    &tm.tm_mday, &tm.tm_mon, &tm.tm_year);

    if (!date_was_valid)
    {
        /* Hm... no valid date. What should we do now? As a hacky workaround we
        revert to today's date. Alternatively we can return some value that
        signals that we don't get a valid date, but all callers of this
        function will have to check this. Alas, I'm too lazy to do this here. */
        gnc_tm_get_today_neutral(&tm);
    }
    else
    {
        tm.tm_mon--;
        tm.tm_year -= 1900;
    }

    if (gde->flags & GNC_DATE_EDIT_SHOW_TIME)
    {
        char *tokp = NULL;
        gchar *temp;

        str = g_strdup (gtk_editable_get_text
                        (GTK_EDITABLE (gde->time_entry)));
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
        gnc_tm_set_day_neutral(&tm);
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
            gtk_widget_set_visible (gde->cal_label, TRUE);
            gtk_widget_set_visible (gde->time_entry, TRUE);
            gtk_widget_set_visible (gde->time_combo, TRUE);
        }
        else
        {
            gtk_widget_set_visible (gde->cal_label, FALSE);
            gtk_widget_set_visible (gde->time_entry, FALSE);
            gtk_widget_set_visible (gde->time_combo, FALSE);
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
