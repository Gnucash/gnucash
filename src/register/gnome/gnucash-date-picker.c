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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

/*
 *  A popup date picker for the canvas using gtk_calendar.
 */

#include <gnome.h>

#include "gnucash-date-picker.h"


/* Item list signals */
enum
{
  DATE_SELECTED,
  DATE_PICKED,
  KEY_PRESS_EVENT,
  LAST_SIGNAL
};

static GnomeCanvasWidgetClass *gnc_date_picker_parent_class;
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
  date_picker->calendar = NULL;
}

static gboolean
gnc_date_picker_button_event (GtkWidget *widget, GdkEventButton *event,
                              gpointer data)
{
  /* So the sheet doesn't use it. */
  gtk_signal_emit_stop_by_name (GTK_OBJECT (widget), "button_press_event");

  return TRUE;
}

static gboolean
gnc_date_picker_key_event(GtkWidget *widget, GdkEventKey *event, gpointer data)
{
  GNCDatePicker *date_picker = GNC_DATE_PICKER (data);

  switch (event->keyval)
  {
    case GDK_Return:
      gtk_signal_emit (GTK_OBJECT (date_picker),
                       gnc_date_picker_signals[DATE_PICKED]);

      gtk_signal_emit_stop_by_name (GTK_OBJECT (widget), "key_press_event");

      return TRUE;

    case GDK_Up:
    case GDK_Down:
    case GDK_Left:
    case GDK_Right:
    case GDK_space:
      /* these go to the calendar */
      return FALSE;

    default:
      break;
  }

  /* These go to the sheet */
  gtk_signal_emit_stop_by_name (GTK_OBJECT (widget), "key_press_event");

  gtk_signal_emit (GTK_OBJECT (date_picker),
                   gnc_date_picker_signals[KEY_PRESS_EVENT], event);

  return TRUE;
}


static void
gnc_date_picker_class_init (GNCDatePickerClass *date_picker_class)
{
  GtkObjectClass  *object_class;

  gnc_date_picker_parent_class =
    gtk_type_class (gnome_canvas_widget_get_type());

  object_class = GTK_OBJECT_CLASS (date_picker_class);

  gnc_date_picker_signals[DATE_SELECTED] =
    gtk_signal_new("date_selected",
                   GTK_RUN_LAST,
                   object_class->type,
                   GTK_SIGNAL_OFFSET(GNCDatePickerClass,
                                     date_selected),
                   gtk_signal_default_marshaller,
                   GTK_TYPE_NONE, 0);

  gnc_date_picker_signals[DATE_PICKED] =
    gtk_signal_new("date_picked",
                   GTK_RUN_LAST,
                   object_class->type,
                   GTK_SIGNAL_OFFSET(GNCDatePickerClass,
                                     date_picked),
                   gtk_signal_default_marshaller,
                   GTK_TYPE_NONE, 0);

  gnc_date_picker_signals[KEY_PRESS_EVENT] =
    gtk_signal_new ("key_press_event",
                    GTK_RUN_LAST,
                    object_class->type,
                    GTK_SIGNAL_OFFSET(GNCDatePickerClass,
                                      key_press_event),
                    gtk_marshal_NONE__POINTER,
                    GTK_TYPE_NONE, 1,
                    GTK_TYPE_GDK_EVENT);

  gtk_object_class_add_signals (object_class,
                                gnc_date_picker_signals,
                                LAST_SIGNAL);

  date_picker_class->date_selected = NULL;
  date_picker_class->date_picked = NULL;
  date_picker_class->key_press_event = NULL;
}


GtkType
gnc_date_picker_get_type (void)
{
  static GtkType gnc_date_picker_type = 0;

  if (gnc_date_picker_type == 0)
  {
    GtkTypeInfo gnc_date_picker_info =
    {
      "GNCDatePicker",
      sizeof(GNCDatePicker),
      sizeof(GNCDatePickerClass),
      (GtkClassInitFunc)  gnc_date_picker_class_init,
      (GtkObjectInitFunc) gnc_date_picker_init,
      NULL, /* reserved_1 */
      NULL, /* reserved_2 */
      (GtkClassInitFunc) NULL
    };

    gnc_date_picker_type =
      gtk_type_unique (gnome_canvas_widget_get_type(),
                       &gnc_date_picker_info);
  }

  return gnc_date_picker_type;
}


static void
day_selected (GtkCalendar *calendar, GNCDatePicker *gdp)
{
  gtk_signal_emit (GTK_OBJECT (gdp), gnc_date_picker_signals [DATE_SELECTED]);
}

static void
day_selected_double_click (GtkCalendar *calendar, GNCDatePicker *gdp)
{
  gtk_signal_emit (GTK_OBJECT (gdp), gnc_date_picker_signals [DATE_PICKED]);
}


GnomeCanvasItem *
gnc_date_picker_new (GnomeCanvasGroup *parent)
{
  GtkWidget *calendar;
  GnomeCanvasItem *item;
  GNCDatePicker *date_picker;

  calendar = gtk_calendar_new ();

  {
    GtkWidget *hbox;

    hbox = gtk_hbox_new (FALSE, 0);

    gtk_box_pack_start (GTK_BOX(hbox), calendar, TRUE, TRUE, 0);

    item = gnome_canvas_item_new (parent, gnc_date_picker_get_type (),
                                  "widget", hbox,
                                  "size_pixels", TRUE,
                                  "x", -10000.0,
                                  "y", -10000.0,
                                  NULL);
  }

  date_picker = GNC_DATE_PICKER (item);

  date_picker->calendar = GTK_CALENDAR (calendar);

  gtk_signal_connect_after (GTK_OBJECT (calendar), "button_press_event",
                            GTK_SIGNAL_FUNC (gnc_date_picker_button_event),
                            date_picker);

  gtk_signal_connect (GTK_OBJECT (calendar), "key_press_event",
                      GTK_SIGNAL_FUNC (gnc_date_picker_key_event),
                      date_picker);

  gtk_signal_connect (GTK_OBJECT (calendar), "day_selected",
                      GTK_SIGNAL_FUNC (day_selected),
                      date_picker);

  gtk_signal_connect (GTK_OBJECT (calendar), "day_selected_double_click",
                      GTK_SIGNAL_FUNC (day_selected_double_click),
                      date_picker);

  return item;
}
