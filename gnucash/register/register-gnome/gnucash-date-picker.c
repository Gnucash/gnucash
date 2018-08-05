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
#include <gdk/gdkkeysyms.h>
#include "gnucash-date-picker.h"


/* Item list signals */
enum
{
    DATE_SELECTED,
    DATE_PICKED,
    LAST_SIGNAL
};

static GtkHBox *gnc_date_picker_parent_class;
static guint gnc_date_picker_signals[LAST_SIGNAL];


void
gnc_date_picker_set_date (GNCDatePicker *date_picker,
                          guint day, guint mon, guint year)
{
    g_return_if_fail (IS_GNC_DATE_PICKER (date_picker));
    g_return_if_fail (date_picker->calendar != NULL);

    gtk_calendar_select_day (date_picker->calendar, 1);
    gtk_calendar_select_month (date_picker->calendar, mon, year);
    gtk_calendar_select_day (date_picker->calendar, day);
}

void
gnc_date_picker_get_date (GNCDatePicker *date_picker,
                          guint *day, guint *mon, guint *year)
{
    g_return_if_fail (IS_GNC_DATE_PICKER (date_picker));
    g_return_if_fail (date_picker->calendar != NULL);

    gtk_calendar_get_date (date_picker->calendar, year, mon, day);
}

static void
gnc_date_picker_init (GNCDatePicker *date_picker)
{
    gtk_orientable_set_orientation (GTK_ORIENTABLE(date_picker), GTK_ORIENTATION_HORIZONTAL);
    date_picker->calendar = NULL;
}

static gboolean
gnc_date_picker_button_event (GtkWidget *widget, GdkEventButton *event,
                              gpointer data)
{
    /* So the sheet doesn't use it. */
    g_signal_stop_emission_by_name (widget, "button_press_event");

    return TRUE;
}

static gboolean
gnc_date_picker_key_event(GtkWidget *widget, GdkEventKey *event, gpointer data)
{
    GNCDatePicker *date_picker = GNC_DATE_PICKER (data);
    gboolean retval;

    switch (event->keyval)
    {
    case GDK_KEY_Return:
    case GDK_KEY_KP_Enter:
        g_signal_emit (date_picker, gnc_date_picker_signals[DATE_PICKED], 0);
        g_signal_stop_emission_by_name (widget, "key_press_event");

        return TRUE;

    case GDK_KEY_Up:
    case GDK_KEY_Down:
    case GDK_KEY_Left:
    case GDK_KEY_Right:
    case GDK_KEY_space:
        /* these go to the calendar */
        return FALSE;

    default:
        break;
    }

    /* These go to the sheet */
    g_signal_stop_emission_by_name (widget, "key_press_event");

    g_signal_emit_by_name (date_picker, "key_press_event", event, &retval);

    return retval;
}

static void
gnc_date_picker_class_init (GNCDatePickerClass *date_picker_class)
{
    GObjectClass  *object_class;

    gnc_date_picker_parent_class =
        g_type_class_peek_parent (date_picker_class);

    object_class = G_OBJECT_CLASS (date_picker_class);

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

    date_picker_class->date_selected = NULL;
    date_picker_class->date_picked = NULL;
}

GType
gnc_date_picker_get_type (void)
{
    static GType gnc_date_picker_type = 0;

    if (gnc_date_picker_type == 0)
    {
        GTypeInfo type_info =
        {
            sizeof(GNCDatePickerClass),       /* class_size */
            NULL,   				/* base_init */
            NULL,				/* base_finalize */
            (GClassInitFunc)gnc_date_picker_class_init,
            NULL,				/* class_finalize */
            NULL,				/* class_data */
            sizeof(GNCDatePicker),		/* */
            0,				/* n_preallocs */
            (GInstanceInitFunc)gnc_date_picker_init,
        };

        gnc_date_picker_type =
            g_type_register_static (GTK_TYPE_BOX,
                                    "GNCDatePicker",
                                    &type_info, 0);
    }

    return gnc_date_picker_type;
}


static void
day_selected (GtkCalendar *calendar, GNCDatePicker *gdp)
{
    g_signal_emit (gdp, gnc_date_picker_signals [DATE_SELECTED], 0);
}

static void
day_selected_double_click (GtkCalendar *calendar, GNCDatePicker *gdp)
{
    g_signal_emit (gdp, gnc_date_picker_signals [DATE_PICKED], 0);
}


GtkWidget *
gnc_date_picker_new (void)
{
    GtkWidget *calendar;
    GNCDatePicker *date_picker;
    GtkAllocation allocation;
    GtkRequisition requisition;

    date_picker = g_object_new (GNC_TYPE_DATE_PICKER,
                                "homogeneous", FALSE,
                                NULL);

    calendar = gtk_calendar_new ();
    date_picker->calendar = GTK_CALENDAR (calendar);

    gtk_box_pack_start (GTK_BOX(date_picker), calendar, TRUE, TRUE, 0);

    gtk_widget_get_preferred_size (calendar, &requisition, NULL);
    allocation.x = 0;
    allocation.y = 0;
    allocation.width = requisition.width;
    allocation.height = requisition.height;
    gtk_widget_size_allocate (calendar, &allocation);

    g_signal_connect_after (calendar, "button_press_event",
                            G_CALLBACK (gnc_date_picker_button_event),
                            date_picker);

    g_signal_connect (calendar, "key_press_event",
                      G_CALLBACK (gnc_date_picker_key_event),
                      date_picker);

    g_signal_connect (calendar, "day_selected",
                      G_CALLBACK (day_selected),
                      date_picker);

    g_signal_connect (calendar, "day_selected_double_click",
                      G_CALLBACK (day_selected_double_click),
                      date_picker);

    return GTK_WIDGET(date_picker);
}
