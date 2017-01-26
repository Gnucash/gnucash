/********************************************************************\
 * gnucash-calendar.c -- A clone from GtkCalendar that optimized    *
 *                        to support multi calendar logic           *
 * Copyright 2016 Amin Aghabiki <amin[dot]aghabeiki[at]gmail.com>   *
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
\********************************************************************/


/* GTK - The GIMP Toolkit
 * Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball and Josh MacDonald
 *
 * GTK Calendar Widget
 * Copyright (C) 1998 Cesar Miquel, Shawn T. Amundson and Mattias Groenlund
 *
 * lib_date routines
 * Copyright (c) 1995, 1996, 1997, 1998 by Steffen Beyer
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/*
 * Modified by the GTK+ Team and others 1997-2000.  See the AUTHORS
 * file for a list of people on the GTK+ Team.  See the ChangeLog
 * files for a list of changes.  These files are distributed with
 * GTK+ at ftp://ftp.gtk.org/pub/gtk/.
 */

#include "config.h"

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE__NL_TIME_FIRST_WEEKDAY
#include <langinfo.h>
#endif

#include <string.h>
#include <stdlib.h>
#include <time.h>

#include <glib.h>

#ifdef G_OS_WIN32
#include <windows.h>
#endif

#include "gnucash-calendar.h"
#include <gtk/gtkdnd.h>
#include <gtk/gtkmain.h>
#include <gtk/gtktooltip.h>
#include <gtk/gtkprivate.h>
#include <gdk/gdkkeysyms.h>

/* Spacing around day/week headers and main area, inside those windows */
#define CALENDAR_MARGIN         0
/* Spacing around day/week headers and main area, outside those windows */
#define INNER_BORDER         4
/* Separation between day headers and main area */
#define CALENDAR_YSEP         4
/* Separation between week headers and main area */
#define CALENDAR_XSEP         4

#define DAY_XSEP         0 /* not really good for small calendar */
#define DAY_YSEP         0 /* not really good for small calendar */

#define SCROLL_DELAY_FACTOR      5

/* Color usage */
#define HEADER_FG_COLOR(widget)         (& (widget)->style->fg[GTK_WIDGET_STATE (widget)])
#define HEADER_BG_COLOR(widget)         (& (widget)->style->bg[GTK_WIDGET_STATE (widget)])
#define SELECTED_BG_COLOR(widget)     (& (widget)->style->base[GTK_WIDGET_HAS_FOCUS (widget) ? GTK_STATE_SELECTED : GTK_STATE_ACTIVE])
#define SELECTED_FG_COLOR(widget)     (& (widget)->style->text[GTK_WIDGET_HAS_FOCUS (widget) ? GTK_STATE_SELECTED : GTK_STATE_ACTIVE])
#define NORMAL_DAY_COLOR(widget)     (& (widget)->style->text[GTK_WIDGET_STATE (widget)])
#define PREV_MONTH_COLOR(widget)     (& (widget)->style->mid[GTK_WIDGET_STATE (widget)])
#define NEXT_MONTH_COLOR(widget)     (& (widget)->style->mid[GTK_WIDGET_STATE (widget)])
#define MARKED_COLOR(widget)         (& (widget)->style->text[GTK_WIDGET_STATE (widget)])
#define BACKGROUND_COLOR(widget)     (& (widget)->style->base[GTK_WIDGET_STATE (widget)])
#define HIGHLIGHT_BACK_COLOR(widget)     (& (widget)->style->mid[GTK_WIDGET_STATE (widget)])

enum {
    ARROW_YEAR_LEFT,
    ARROW_YEAR_RIGHT,
    ARROW_MONTH_LEFT,
    ARROW_MONTH_RIGHT
};

enum {
    MONTH_CHANGED_SIGNAL,
    DAY_SELECTED_SIGNAL,
    DAY_SELECTED_DOUBLE_CLICK_SIGNAL,
    PREV_MONTH_SIGNAL,
    NEXT_MONTH_SIGNAL,
    PREV_YEAR_SIGNAL,
    NEXT_YEAR_SIGNAL,
    LAST_SIGNAL
};

enum {
    PROP_0,
    PROP_YEAR,
    PROP_MONTH,
    PROP_DAY,
    PROP_SHOW_HEADING,
    PROP_SHOW_DAY_NAMES,
    PROP_NO_MONTH_CHANGE,
    PROP_SHOW_WEEK_NUMBERS,
    PROP_SHOW_DETAILS,
    PROP_DETAIL_WIDTH_CHARS,
    PROP_DETAIL_HEIGHT_ROWS
};

static guint gnc_calendar_signals[LAST_SIGNAL] = {0};

struct _GncCalendarPrivate {
    GdkWindow *header_win;
    GdkWindow *day_name_win;
    GdkWindow *main_win;
    GdkWindow *week_win;
    GdkWindow *arrow_win[4];

    guint header_h;
    guint day_name_h;
    guint main_h;

    guint arrow_state[4];
    guint arrow_width;
    guint max_month_width;
    guint max_year_width;

    guint day_width;
    guint week_width;

    guint min_day_width;
    guint max_day_char_width;
    guint max_day_char_ascent;
    guint max_day_char_descent;
    guint max_label_char_ascent;
    guint max_label_char_descent;
    guint max_week_char_width;

    /* flags */
    guint year_before : 1;

    guint need_timer  : 1;

    guint in_drag : 1;
    guint drag_highlight : 1;

    guint32 timer;
    gint click_child;

    gint week_start;

    gint drag_start_x;
    gint drag_start_y;

    /* Optional callback, used to display extra information for each day. */
    GncCalendarDetailFunc detail_func;
    gpointer detail_func_user_data;
    GDestroyNotify detail_func_destroy;

    /* Size requistion for details provided by the hook. */
    gint detail_height_rows;
    gint detail_width_chars;
    gint detail_overflow[6];
};

#define GNC_CALENDAR_GET_PRIVATE(widget)  (GNC_CALENDAR (widget)->priv)

static void gnc_calendar_finalize (GObject *calendar);

static void gnc_calendar_destroy (GtkObject *calendar);

static void gnc_calendar_set_property (GObject *object,
                                       guint prop_id,
                                       const GValue *value,
                                       GParamSpec *pspec);

static void gnc_calendar_get_property (GObject *object,
                                       guint prop_id,
                                       GValue *value,
                                       GParamSpec *pspec);

static void gnc_calendar_realize (GtkWidget *widget);

static void gnc_calendar_unrealize (GtkWidget *widget);

static void gnc_calendar_size_request (GtkWidget *widget,
                                       GtkRequisition *requisition);

static void gnc_calendar_size_allocate (GtkWidget *widget,
                                        GtkAllocation *allocation);

static gboolean gnc_calendar_expose (GtkWidget *widget,
                                     GdkEventExpose *event);

static gboolean gnc_calendar_button_press (GtkWidget *widget,
                                           GdkEventButton *event);

static gboolean gnc_calendar_button_release (GtkWidget *widget,
                                             GdkEventButton *event);

static gboolean gnc_calendar_motion_notify (GtkWidget *widget,
                                            GdkEventMotion *event);

static gboolean gnc_calendar_enter_notify (GtkWidget *widget,
                                           GdkEventCrossing *event);

static gboolean gnc_calendar_leave_notify (GtkWidget *widget,
                                           GdkEventCrossing *event);

static gboolean gnc_calendar_scroll (GtkWidget *widget,
                                     GdkEventScroll *event);

static gboolean gnc_calendar_key_press (GtkWidget *widget,
                                        GdkEventKey *event);

static gboolean gnc_calendar_focus_out (GtkWidget *widget,
                                        GdkEventFocus *event);

static void gnc_calendar_grab_notify (GtkWidget *widget,
                                      gboolean was_grabbed);

static void gnc_calendar_state_changed (GtkWidget *widget,
                                        GtkStateType previous_state);

static void gnc_calendar_style_set (GtkWidget *widget,
                                    GtkStyle *previous_style);

static gboolean gnc_calendar_query_tooltip (GtkWidget *widget,
                                            gint x,
                                            gint y,
                                            gboolean keyboard_mode,
                                            GtkTooltip *tooltip);

static void gnc_calendar_drag_data_get (GtkWidget *widget,
                                        GdkDragContext *context,
                                        GtkSelectionData *selection_data,
                                        guint info,
                                        guint time);

static void gnc_calendar_drag_data_received (GtkWidget *widget,
                                             GdkDragContext *context,
                                             gint x,
                                             gint y,
                                             GtkSelectionData *selection_data,
                                             guint info,
                                             guint time);

static gboolean gnc_calendar_drag_motion (GtkWidget *widget,
                                          GdkDragContext *context,
                                          gint x,
                                          gint y,
                                          guint time);

static void gnc_calendar_drag_leave (GtkWidget *widget,
                                     GdkDragContext *context,
                                     guint time);

static gboolean gnc_calendar_drag_drop (GtkWidget *widget,
                                        GdkDragContext *context,
                                        gint x,
                                        gint y,
                                        guint time);

static void calendar_start_spinning (GncCalendar *calendar,
                                     gint click_child);

static void calendar_stop_spinning (GncCalendar *calendar);

static void calendar_invalidate_day (GncCalendar *widget,
                                     gint row,
                                     gint col);

static void calendar_invalidate_day_num (GncCalendar *widget,
                                         gint day);

static void calendar_invalidate_arrow (GncCalendar *widget,
                                       guint arrow);
static void calendar_pre_init_select_day (GncCalendar *widget,
                                          guint day);

static void calendar_compute_days (GncCalendar *calendar);

static char *default_abbreviated_dayname[7];
static char *default_monthname[12];

G_DEFINE_TYPE (GncCalendar, gnc_calendar, GTK_TYPE_WIDGET)

static void
gnc_calendar_class_init (GncCalendarClass *class)
{
  GObjectClass *gobject_class;
  GtkObjectClass *object_class;
  GtkWidgetClass *widget_class;

  gobject_class = (GObjectClass *) class;
  object_class = (GtkObjectClass *) class;
  widget_class = (GtkWidgetClass *) class;

  gobject_class->set_property = gnc_calendar_set_property;
  gobject_class->get_property = gnc_calendar_get_property;
  gobject_class->finalize = gnc_calendar_finalize;

  object_class->destroy = gnc_calendar_destroy;

  widget_class->realize = gnc_calendar_realize;
  widget_class->unrealize = gnc_calendar_unrealize;
  widget_class->expose_event = gnc_calendar_expose;
  widget_class->size_request = gnc_calendar_size_request;
  widget_class->size_allocate = gnc_calendar_size_allocate;
  widget_class->button_press_event = gnc_calendar_button_press;
  widget_class->button_release_event = gnc_calendar_button_release;
  widget_class->motion_notify_event = gnc_calendar_motion_notify;
  widget_class->enter_notify_event = gnc_calendar_enter_notify;
  widget_class->leave_notify_event = gnc_calendar_leave_notify;
  widget_class->key_press_event = gnc_calendar_key_press;
  widget_class->scroll_event = gnc_calendar_scroll;
  widget_class->style_set = gnc_calendar_style_set;
  widget_class->state_changed = gnc_calendar_state_changed;
  widget_class->grab_notify = gnc_calendar_grab_notify;
  widget_class->focus_out_event = gnc_calendar_focus_out;
  widget_class->query_tooltip = gnc_calendar_query_tooltip;

  widget_class->drag_data_get = gnc_calendar_drag_data_get;
  widget_class->drag_motion = gnc_calendar_drag_motion;
  widget_class->drag_leave = gnc_calendar_drag_leave;
  widget_class->drag_drop = gnc_calendar_drag_drop;
  widget_class->drag_data_received = gnc_calendar_drag_data_received;

  /**
     * GncCalendar:year:
     *
     * The selected year.
     * This property gets initially set to the current year.
     */
  g_object_class_install_property (gobject_class,
                                   PROP_YEAR,
                                   g_param_spec_int ("year",
                                                     P_("Year"),
                                                     P_("The selected year"),
                                                     0, G_MAXINT >> 9, 0,
                                                     GTK_PARAM_READWRITE));

  /**
     * GncCalendar:month:
     *
     * The selected month (as a number between 0 and 11).
     * This property gets initially set to the current month.
     */
  g_object_class_install_property (gobject_class,
                                   PROP_MONTH,
                                   g_param_spec_int ("month",
                                                     P_("Month"),
                                                     P_("The selected month (as a number between 0 and 11)"),
                                                     0, 11, 0,
                                                     GTK_PARAM_READWRITE));

  /**
     * GncCalendar:day:
     *
     * The selected day (as a number between 1 and 31, or 0
     * to unselect the currently selected day).
     * This property gets initially set to the current day.
     */
  g_object_class_install_property (gobject_class,
                                   PROP_DAY,
                                   g_param_spec_int ("day",
                                                     P_("Day"),
                                                     P_("The selected day (as a number between 1 and 31, or 0 to unselect the currently selected day)"),
                                                     0, 31, 0,
                                                     GTK_PARAM_READWRITE));

/**
 * GncCalendar:show-heading:
 *
 * Determines whether a heading is displayed.
 *
 * Since: 2.4
 */
  g_object_class_install_property (gobject_class,
                                   PROP_SHOW_HEADING,
                                   g_param_spec_boolean ("show-heading",
                                                         P_("Show Heading"),
                                                         P_("If TRUE, a heading is displayed"),
                                                         TRUE,
                                                         GTK_PARAM_READWRITE));

/**
 * GncCalendar:show-day-names:
 *
 * Determines whether day names are displayed.
 *
 * Since: 2.4
 */
  g_object_class_install_property (gobject_class,
                                   PROP_SHOW_DAY_NAMES,
                                   g_param_spec_boolean ("show-day-names",
                                                         P_("Show Day Names"),
                                                         P_("If TRUE, day names are displayed"),
                                                         TRUE,
                                                         GTK_PARAM_READWRITE));
/**
 * GncCalendar:no-month-change:
 *
 * Determines whether the selected month can be changed.
 *
 * Since: 2.4
 */
  g_object_class_install_property (gobject_class,
                                   PROP_NO_MONTH_CHANGE,
                                   g_param_spec_boolean ("no-month-change",
                                                         P_("No Month Change"),
                                                         P_("If TRUE, the selected month cannot be changed"),
                                                         FALSE,
                                                         GTK_PARAM_READWRITE));

/**
 * GncCalendar:show-week-numbers:
 *
 * Determines whether week numbers are displayed.
 *
 * Since: 2.4
 */
  g_object_class_install_property (gobject_class,
                                   PROP_SHOW_WEEK_NUMBERS,
                                   g_param_spec_boolean ("show-week-numbers",
                                                         P_("Show Week Numbers"),
                                                         P_("If TRUE, week numbers are displayed"),
                                                         FALSE,
                                                         GTK_PARAM_READWRITE));

/**
 * GncCalendar:detail-width-chars:
 *
 * Width of a detail cell, in characters.
 * A value of 0 allows any width. See gtk_calendar_set_detail_func().
 *
 * Since: 2.14
 */
  g_object_class_install_property (gobject_class,
                                   PROP_DETAIL_WIDTH_CHARS,
                                   g_param_spec_int ("detail-width-chars",
                                                     P_("Details Width"),
                                                     P_("Details width in characters"),
                                                     0, 127, 0,
                                                     GTK_PARAM_READWRITE));

/**
 * GncCalendar:detail-height-rows:
 *
 * Height of a detail cell, in rows.
 * A value of 0 allows any width. See gtk_calendar_set_detail_func().
 *
 * Since: 2.14
 */
  g_object_class_install_property (gobject_class,
                                   PROP_DETAIL_HEIGHT_ROWS,
                                   g_param_spec_int ("detail-height-rows",
                                                     P_("Details Height"),
                                                     P_("Details height in rows"),
                                                     0, 127, 0,
                                                     GTK_PARAM_READWRITE));

/**
 * GncCalendar:show-details:
 *
 * Determines whether details are shown directly in the widget, or if they are
 * available only as tooltip. When this property is set days with details are
 * marked.
 *
 * Since: 2.14
 */
  g_object_class_install_property (gobject_class,
                                   PROP_SHOW_DETAILS,
                                   g_param_spec_boolean ("show-details",
                                                         P_("Show Details"),
                                                         P_("If TRUE, details are shown"),
                                                         TRUE,
                                                         GTK_PARAM_READWRITE));

  gnc_calendar_signals[MONTH_CHANGED_SIGNAL] =
      g_signal_new (I_("month-changed"),
                    G_OBJECT_CLASS_TYPE (gobject_class),
                    G_SIGNAL_RUN_FIRST,
                    G_STRUCT_OFFSET (GncCalendarClass, month_changed),
                    NULL, NULL,
                    NULL,
                    G_TYPE_NONE, 0);
  gnc_calendar_signals[DAY_SELECTED_SIGNAL] =
      g_signal_new (I_("day-selected"),
                    G_OBJECT_CLASS_TYPE (gobject_class),
                    G_SIGNAL_RUN_FIRST,
                    G_STRUCT_OFFSET (GncCalendarClass, day_selected),
                    NULL, NULL,
                    NULL,
                    G_TYPE_NONE, 0);
  gnc_calendar_signals[DAY_SELECTED_DOUBLE_CLICK_SIGNAL] =
      g_signal_new (I_("day-selected-double-click"),
                    G_OBJECT_CLASS_TYPE (gobject_class),
                    G_SIGNAL_RUN_FIRST,
                    G_STRUCT_OFFSET (GncCalendarClass, day_selected_double_click),
                    NULL, NULL,
                    NULL,
                    G_TYPE_NONE, 0);
  gnc_calendar_signals[PREV_MONTH_SIGNAL] =
      g_signal_new (I_("prev-month"),
                    G_OBJECT_CLASS_TYPE (gobject_class),
                    G_SIGNAL_RUN_FIRST,
                    G_STRUCT_OFFSET (GncCalendarClass, prev_month),
                    NULL, NULL,
                    NULL,
                    G_TYPE_NONE, 0);
  gnc_calendar_signals[NEXT_MONTH_SIGNAL] =
      g_signal_new (I_("next-month"),
                    G_OBJECT_CLASS_TYPE (gobject_class),
                    G_SIGNAL_RUN_FIRST,
                    G_STRUCT_OFFSET (GncCalendarClass, next_month),
                    NULL, NULL,
                    NULL,
                    G_TYPE_NONE, 0);
  gnc_calendar_signals[PREV_YEAR_SIGNAL] =
      g_signal_new (I_("prev-year"),
                    G_OBJECT_CLASS_TYPE (gobject_class),
                    G_SIGNAL_RUN_FIRST,
                    G_STRUCT_OFFSET (GncCalendarClass, prev_year),
                    NULL, NULL,
                    NULL,
                    G_TYPE_NONE, 0);
  gnc_calendar_signals[NEXT_YEAR_SIGNAL] =
      g_signal_new (I_("next-year"),
                    G_OBJECT_CLASS_TYPE (gobject_class),
                    G_SIGNAL_RUN_FIRST,
                    G_STRUCT_OFFSET (GncCalendarClass, next_year),
                    NULL, NULL,
                    NULL,
                    G_TYPE_NONE, 0);

  g_type_class_add_private (gobject_class, sizeof (GncCalendarPrivate));
}

static void
gnc_calendar_init (GncCalendar *calendar)
{
  GtkWidget *widget = GTK_WIDGET (calendar);
  time_t secs;
  struct tm *tm;
  gint i;
#ifdef G_OS_WIN32
  wchar_t wbuffer[100];
#else
  char buffer[255];
  time_t tmp_time;
#endif
  GncCalendarPrivate *priv;
  gchar *year_before;
#ifdef HAVE__NL_TIME_FIRST_WEEKDAY
                                                                                                                          union { unsigned int word; char *string; } langinfo;
  gint week_1stday = 0;
  gint first_weekday = 1;
  guint week_origin;
#else
  gchar *week_start;
#endif

  priv = calendar->priv = G_TYPE_INSTANCE_GET_PRIVATE (calendar,
                                                       GNC_TYPE_CALENDAR,
                                                       GncCalendarPrivate);

  GTK_WIDGET_SET_FLAGS (widget, GTK_CAN_FOCUS);

  /* Set defaults */

  calendar->calendarFunction = gregorian_calendar_inti ();
  calendar->calendarType = GNC_CALENDAR_TYPE_GREGORIAN; // init default to gregorian calendar
  calendar->calendarTypePrev = GNC_CALENDAR_TYPE_GREGORIAN;

  calendar->calendarFunction->init_day_name (default_abbreviated_dayname);
  calendar->calendarFunction->init_month_name (default_monthname);

  secs = time (NULL);
  tm = localtime (&secs);
  calendar->month = tm->tm_mon;
  calendar->year = 1900 + tm->tm_year;

  for (i = 0; i < 31; i++)
    calendar->marked_date[i] = FALSE;
  calendar->num_marked_dates = 0;
  calendar->selected_day = tm->tm_mday;

  calendar->display_flags = (GNC_CALENDAR_SHOW_HEADING |
                             GNC_CALENDAR_SHOW_DAY_NAMES |
                             GNC_CALENDAR_SHOW_DETAILS);

  calendar->highlight_row = -1;
  calendar->highlight_col = -1;

  calendar->focus_row = -1;
  calendar->focus_col = -1;

  priv->max_year_width = 0;
  priv->max_month_width = 0;
  priv->max_day_char_width = 0;
  priv->max_week_char_width = 0;

  priv->max_day_char_ascent = 0;
  priv->max_day_char_descent = 0;
  priv->max_label_char_ascent = 0;
  priv->max_label_char_descent = 0;

  priv->arrow_width = 10;

  priv->need_timer = 0;
  priv->timer = 0;
  priv->click_child = -1;

  priv->in_drag = 0;
  priv->drag_highlight = 0;

  gtk_drag_dest_set (widget, 0, NULL, 0, GDK_ACTION_COPY);
  gtk_drag_dest_add_text_targets (widget);

  priv->year_before = 0;

  /* Translate to calendar:YM if you want years to be displayed
     * before months; otherwise translate to calendar:MY.
     * Do *not* translate it to anything else, if it
     * it isn't calendar:YM or calendar:MY it will not work.
     *
     * Note that this flipping is in top of the text direction flipping,
     * so if you have a default text direction of RTL and YM, then
     * the year will appear on the right.
     */
  year_before = _("calendar:MY");
  if (strcmp (year_before, "calendar:YM") == 0)
    priv->year_before = 1;
  else if (strcmp (year_before, "calendar:MY") != 0)
    g_warning ("Whoever translated calendar:MY did so wrongly.\n");

#ifdef G_OS_WIN32
                                                                                                                          priv->week_start = 0;
  week_start = NULL;

  if (GetLocaleInfoW (GetThreadLocale (), LOCALE_IFIRSTDAYOFWEEK,
		      wbuffer, G_N_ELEMENTS (wbuffer)))
    week_start = g_utf16_to_utf8 (wbuffer, -1, NULL, NULL, NULL);

  if (week_start != NULL)
    {
      priv->week_start = (week_start[0] - '0' + 1) % 7;
      g_free(week_start);
    }
#else
#ifdef HAVE__NL_TIME_FIRST_WEEKDAY
                                                                                                                          langinfo.string = nl_langinfo (_NL_TIME_FIRST_WEEKDAY);
  first_weekday = langinfo.string[0];
  langinfo.string = nl_langinfo (_NL_TIME_WEEK_1STDAY);
  week_origin = langinfo.word;
  if (week_origin == 19971130) /* Sunday */
    week_1stday = 0;
  else if (week_origin == 19971201) /* Monday */
    week_1stday = 1;
  else
    g_warning ("Unknown value of _NL_TIME_WEEK_1STDAY.\n");

  priv->week_start = (week_1stday + first_weekday - 1) % 7;
#else
  /* Translate to calendar:week_start:0 if you want Sunday to be the
     * first day of the week to calendar:week_start:1 if you want Monday
     * to be the first day of the week, and so on.
     */
  week_start = _("calendar:week_start:0");

  if (strncmp (week_start, "calendar:week_start:", 20) == 0)
    priv->week_start = *(week_start + 20) - '0';
  else
    priv->week_start = -1;

  if (priv->week_start < 0 || priv->week_start > 6)
    {
      g_warning ("Whoever translated calendar:week_start:0 did so wrongly.\n");
      priv->week_start = 0;
    }
#endif
#endif
  priv->week_start = 6;
  calendar_compute_days (calendar);
}

/****************************************
 *          Utility Functions           *
 ****************************************/

static void
calendar_queue_refresh (GncCalendar *calendar)
{
  GncCalendarPrivate *priv = GNC_CALENDAR_GET_PRIVATE (calendar);

  if (!(priv->detail_func) ||
      !(calendar->display_flags & GNC_CALENDAR_SHOW_DETAILS) ||
      (priv->detail_width_chars && priv->detail_height_rows))
    gtk_widget_queue_draw (GTK_WIDGET (calendar));
  else
    gtk_widget_queue_resize (GTK_WIDGET (calendar));
}

static void
calendar_set_month_next (GncCalendar *calendar)
{
  gint month_len;
  gint display_month_number;
  gchar *str;
  gint year;
  gint g_y;
  gint g_m;
  gint g_d;

  if (calendar->display_flags & GNC_CALENDAR_NO_MONTH_CHANGE)
    return;

  str = calendar->calendarFunction->paint_header_helper (calendar->year, calendar->month
                                                                         + 1, calendar->selected_day, &display_month_number);
  display_month_number--;// revert back from display mode
  year = g_ascii_strtoll (str, NULL, 10);

  if (display_month_number == 11)
    {
      display_month_number = 0;
      year++;
    }
  else
    display_month_number++;

  calendar->calendarFunction->invalidate_display_day_to_gregorian (year, display_month_number + 1, 1, &g_y, &g_m, &g_d);
  g_m--;// should be back to the calculation month
  calendar->year = g_y;
  calendar->month = g_m;
  calendar->selected_day = g_d;
  calendar_compute_days (calendar);
  g_signal_emit (calendar,
                 gnc_calendar_signals[NEXT_MONTH_SIGNAL],
                 0);
  g_signal_emit (calendar,
                 gnc_calendar_signals[MONTH_CHANGED_SIGNAL],
                 0);

  month_len = calendar->calendarFunction->month_length[calendar->calendarFunction->leap (year)][display_month_number
                                                                                                + 1];

  if (month_len < calendar->selected_day)
    {
      calendar->selected_day = 0;
      gnc_calendar_select_day (calendar, month_len);
    }
  else
    gnc_calendar_select_day (calendar, calendar->selected_day);

  calendar_queue_refresh (calendar);
}

static void
calendar_set_year_prev (GncCalendar *calendar)
{
  gint month_len;

  calendar->year--;
  calendar_compute_days (calendar);
  g_signal_emit (calendar,
                 gnc_calendar_signals[PREV_YEAR_SIGNAL],
                 0);
  g_signal_emit (calendar,
                 gnc_calendar_signals[MONTH_CHANGED_SIGNAL],
                 0);

  month_len = calendar->calendarFunction->month_length[calendar->calendarFunction->leap (calendar->year)][
      calendar->month + 1];

  if (month_len < calendar->selected_day)
    {
      calendar->selected_day = 0;
      gnc_calendar_select_day (calendar, month_len);
    }
  else
    gnc_calendar_select_day (calendar, calendar->selected_day);

  calendar_queue_refresh (calendar);
}

static void
calendar_set_year_next (GncCalendar *calendar)
{
  gint month_len;

  calendar->year++;
  calendar_compute_days (calendar);
  g_signal_emit (calendar,
                 gnc_calendar_signals[NEXT_YEAR_SIGNAL],
                 0);
  g_signal_emit (calendar,
                 gnc_calendar_signals[MONTH_CHANGED_SIGNAL],
                 0);

  month_len = calendar->calendarFunction->month_length[calendar->calendarFunction->leap (calendar->year)][
      calendar->month + 1];

  if (month_len < calendar->selected_day)
    {
      calendar->selected_day = 0;
      gnc_calendar_select_day (calendar, month_len);
    }
  else
    gnc_calendar_select_day (calendar, calendar->selected_day);

  calendar_queue_refresh (calendar);
}

static void
calendar_compute_days (GncCalendar *calendar)
{
  GncCalendarPrivate *priv = GNC_CALENDAR_GET_PRIVATE (GTK_WIDGET (calendar));

  calendar->calendarFunction->compute_paint_days (calendar->year, calendar->month, calendar->selected_day,
                                                  priv->week_start, calendar->day, calendar->day_month);
  /* GncCalendarPrivate *priv = GNC_CALENDAR_GET_PRIVATE (GTK_WIDGET(calendar));
    gint month;
    gint year;
    gint ndays_in_month;
    gint ndays_in_prev_month;
    gint first_day;
    gint row;
    gint col;
    gint day;

    year = calendar->year;
    month = calendar->month + 1;

    ndays_in_month = calendar->calendarFunction->month_length[calendar->calendarFunction->leap(year)][month];

    first_day = calendar->calendarFunction->day_of_week(year, month, 1);
    first_day = (first_day + 7 - priv->week_start) % 7;

    *//* Compute days of previous month *//*
    if (month > 1)
        ndays_in_prev_month = calendar->calendarFunction->month_length[calendar->calendarFunction->leap(year)][month - 1];
    else
        ndays_in_prev_month = calendar->calendarFunction->month_length[calendar->calendarFunction->leap(year)][12];
    day = ndays_in_prev_month - first_day + 1;

    row = 0;
    if (first_day > 0) {
        for (col = 0; col < first_day; col++) {
            calendar->day[row][col] = day;
            calendar->day_month[row][col] = MONTH_PREV;
            day++;
        }
    }

    *//* Compute days of current month *//*
    col = first_day;
    for (day = ndays_in_month; day >= 1; day--) {
        calendar->day[row][col] = day;
        calendar->day_month[row][col] = MONTH_CURRENT;

        col++;
        if (col == 7) {
            row++;
            col = 0;
        }
    }

    *//* Compute days of next month *//*
    day = 1;
    for (; row <= 5; row++) {
        for (; col <= 6; col++) {
            calendar->day[row][col] = day;
            calendar->day_month[row][col] = MONTH_NEXT;
            day++;
        }
        col = 0;
    }*/
}

static void
calendar_select_and_focus_day (GncCalendar *calendar,
                               guint day)
{
  gint old_focus_row = calendar->focus_row;
  gint old_focus_col = calendar->focus_col;
  gint row;
  gint col;

  for (row = 0; row < 6; row++)
    for (col = 0; col < 7; col++)
      {
        if (calendar->day_month[row][col] == MONTH_CURRENT
            && calendar->day[row][col] == day)
          {
            calendar->focus_row = row;
            calendar->focus_col = col;
          }
      }

  if (old_focus_row != -1 && old_focus_col != -1)
    calendar_invalidate_day (calendar, old_focus_row, old_focus_col);

  calendar_pre_init_select_day (calendar, day);

  gnc_calendar_select_day (calendar, calendar->selected_day);
}

/****************************************
 *     Layout computation utilities     *
 ****************************************/

static gint
calendar_row_height (GncCalendar *calendar)
{
  return (GNC_CALENDAR_GET_PRIVATE (calendar)->main_h - CALENDAR_MARGIN
          - ((calendar->display_flags & GNC_CALENDAR_SHOW_DAY_NAMES)
             ? CALENDAR_YSEP : CALENDAR_MARGIN)) / 6;
}

/* calendar_left_x_for_column: returns the x coordinate
 * for the left of the column */
static gint
calendar_left_x_for_column (GncCalendar *calendar,
                            gint column)
{
  gint width;
  gint x_left;

  if (gtk_widget_get_direction (GTK_WIDGET (calendar)) == GTK_TEXT_DIR_RTL)
    column = 6 - column;

  width = GNC_CALENDAR_GET_PRIVATE (calendar)->day_width;
  if (calendar->display_flags & GNC_CALENDAR_SHOW_WEEK_NUMBERS)
    x_left = CALENDAR_XSEP + (width + DAY_XSEP) * column;
  else
    x_left = CALENDAR_MARGIN + (width + DAY_XSEP) * column;

  return x_left;
}

/* column_from_x: returns the column 0-6 that the
 * x pixel of the xwindow is in */
static gint
calendar_column_from_x (GncCalendar *calendar,
                        gint event_x)
{
  gint c, column;
  gint x_left, x_right;

  column = -1;

  for (c = 0; c < 7; c++)
    {
      x_left = calendar_left_x_for_column (calendar, c);
      x_right = x_left + GNC_CALENDAR_GET_PRIVATE (calendar)->day_width;

      if (event_x >= x_left && event_x < x_right)
        {
          column = c;
          break;
        }
    }

  return column;
}

/* calendar_top_y_for_row: returns the y coordinate
 * for the top of the row */
static gint
calendar_top_y_for_row (GncCalendar *calendar,
                        gint row)
{

  return (GNC_CALENDAR_GET_PRIVATE (calendar)->main_h
          - (CALENDAR_MARGIN + (6 - row)
                               * calendar_row_height (calendar)));
}

/* row_from_y: returns the row 0-5 that the
 * y pixel of the xwindow is in */
static gint
calendar_row_from_y (GncCalendar *calendar,
                     gint event_y)
{
  gint r, row;
  gint height;
  gint y_top, y_bottom;

  height = calendar_row_height (calendar);
  row = -1;

  for (r = 0; r < 6; r++)
    {
      y_top = calendar_top_y_for_row (calendar, r);
      y_bottom = y_top + height;

      if (event_y >= y_top && event_y < y_bottom)
        {
          row = r;
          break;
        }
    }

  return row;
}

static void
calendar_arrow_rectangle (GncCalendar *calendar,
                          guint arrow,
                          GdkRectangle *rect)
{
  GtkWidget *widget = GTK_WIDGET (calendar);
  GncCalendarPrivate *priv = GNC_CALENDAR_GET_PRIVATE (calendar);
  gboolean year_left;

  if (gtk_widget_get_direction (widget) == GTK_TEXT_DIR_LTR)
    year_left = priv->year_before;
  else
    year_left = !priv->year_before;

  rect->y = 3;
  rect->width = priv->arrow_width;
  rect->height = priv->header_h - 7;

  switch (arrow)
    {
      case ARROW_MONTH_LEFT:
        if (year_left)
          rect->x = (widget->allocation.width - 2 * widget->style->xthickness
                     - (3 + 2 * priv->arrow_width
                        + priv->max_month_width));
        else
          rect->x = 3;
      break;
      case ARROW_MONTH_RIGHT:
        if (year_left)
          rect->x = (widget->allocation.width - 2 * widget->style->xthickness
                     - 3 - priv->arrow_width);
        else
          rect->x = (priv->arrow_width
                     + priv->max_month_width);
      break;
      case ARROW_YEAR_LEFT:
        if (year_left)
          rect->x = 3;
        else
          rect->x = (widget->allocation.width - 2 * widget->style->xthickness
                     - (3 + 2 * priv->arrow_width
                        + priv->max_year_width));
      break;
      case ARROW_YEAR_RIGHT:
        if (year_left)
          rect->x = (priv->arrow_width
                     + priv->max_year_width);
        else
          rect->x = (widget->allocation.width - 2 * widget->style->xthickness
                     - 3 - priv->arrow_width);
      break;
    }
}

static void
calendar_day_rectangle (GncCalendar *calendar,
                        gint row,
                        gint col,
                        GdkRectangle *rect)
{
  GncCalendarPrivate *priv = GNC_CALENDAR_GET_PRIVATE (calendar);

  rect->x = calendar_left_x_for_column (calendar, col);
  rect->y = calendar_top_y_for_row (calendar, row);
  rect->height = calendar_row_height (calendar);
  rect->width = priv->day_width;
}

static void
calendar_set_month_prev (GncCalendar *calendar)
{
  gint month_len;
  gint display_month_number;
  gchar *str;
  gint year;
  gint g_y;
  gint g_m;
  gint g_d;

  if (calendar->display_flags & GNC_CALENDAR_NO_MONTH_CHANGE)
    return;

  str = calendar->calendarFunction->paint_header_helper (calendar->year, calendar->month
                                                                         + 1, calendar->selected_day, &display_month_number);
  display_month_number--;// revert back from display mode
  year = g_ascii_strtoll (str, NULL, 10);

  if (display_month_number == 0)
    {
      display_month_number = 11;
      year--;
    }
  else
    display_month_number--;

  calendar->calendarFunction->invalidate_display_day_to_gregorian (year,
                                                                   display_month_number + 1, 27, &g_y, &g_m, &g_d);
  g_m--;// should be back to the calculation month
  calendar->year = g_y;
  calendar->month = g_m;
  calendar->selected_day = g_d;

  month_len = calendar->calendarFunction->month_length[calendar->calendarFunction->leap (year)][display_month_number
                                                                                                + 1];

  calendar_compute_days (calendar);

  g_signal_emit (calendar,
                 gnc_calendar_signals[PREV_MONTH_SIGNAL],
                 0);
  g_signal_emit (calendar,
                 gnc_calendar_signals[MONTH_CHANGED_SIGNAL],
                 0);

  if (month_len < calendar->selected_day)
    {
      calendar->selected_day = 0;
      gnc_calendar_select_day (calendar, month_len);
    }
  else
    {
      if (calendar->selected_day < 0)
        calendar->selected_day =
            calendar->selected_day + 1
            + calendar->calendarFunction->month_length[calendar->calendarFunction->leap (calendar->year)][
                calendar->month + 1];
      gnc_calendar_select_day (calendar, calendar->selected_day);
    }

  calendar_queue_refresh (calendar);
}

/****************************************
 *           Basic object methods       *
 ****************************************/

static void
gnc_calendar_finalize (GObject *object)
{
  G_OBJECT_CLASS (gnc_calendar_parent_class)->finalize (object);
}

static void
gnc_calendar_destroy (GtkObject *object)
{
  GncCalendarPrivate *priv = GNC_CALENDAR_GET_PRIVATE (object);
  GncCalendar *calendar = GNC_CALENDAR (object);
  calendar_stop_spinning (calendar);

  if (calendar && calendar->calendarFunction)
    {
      //gregorian_calendar_destroy(calendar->calendarFunction);
      jalali_calendar_destroy (calendar->calendarFunction);
      calendar->calendarFunction = NULL;
    }

  /* Call the destroy function for the extra display callback: */
  if (priv->detail_func_destroy && priv->detail_func_user_data)
    {
      priv->detail_func_destroy (priv->detail_func_user_data);
      priv->detail_func_user_data = NULL;
      priv->detail_func_destroy = NULL;
    }

  GTK_OBJECT_CLASS (gnc_calendar_parent_class)->destroy (object);
}

static void
calendar_set_display_option (GncCalendar *calendar,
                             GncCalendarDisplayOptions flag,
                             gboolean setting)
{
  GncCalendarDisplayOptions flags;
  if (setting)
    flags = calendar->display_flags | flag;
  else
    flags = calendar->display_flags & ~flag;
  gnc_calendar_set_display_options (calendar, flags);
}

static gboolean
calendar_get_display_option (GncCalendar *calendar,
                             GncCalendarDisplayOptions flag)
{
  return (calendar->display_flags & flag) != 0;
}

static void
gnc_calendar_set_property (GObject *object,
                           guint prop_id,
                           const GValue *value,
                           GParamSpec *pspec)
{
  GncCalendar *calendar;

  calendar = GNC_CALENDAR (object);

  switch (prop_id)
    {
      case PROP_YEAR:
        gnc_calendar_select_month (calendar,
                                   calendar->month,
                                   g_value_get_int (value));
      break;
      case PROP_MONTH:
        gnc_calendar_select_month (calendar,
                                   g_value_get_int (value),
                                   calendar->year);
      break;
      case PROP_DAY:
        gnc_calendar_select_day (calendar,
                                 g_value_get_int (value));
      break;
      case PROP_SHOW_HEADING:
        calendar_set_display_option (calendar,
                                     GNC_CALENDAR_SHOW_HEADING,
                                     g_value_get_boolean (value));
      break;
      case PROP_SHOW_DAY_NAMES:
        calendar_set_display_option (calendar,
                                     GNC_CALENDAR_SHOW_DAY_NAMES,
                                     g_value_get_boolean (value));
      break;
      case PROP_NO_MONTH_CHANGE:
        calendar_set_display_option (calendar,
                                     GNC_CALENDAR_NO_MONTH_CHANGE,
                                     g_value_get_boolean (value));
      break;
      case PROP_SHOW_WEEK_NUMBERS:
        calendar_set_display_option (calendar,
                                     GNC_CALENDAR_SHOW_WEEK_NUMBERS,
                                     g_value_get_boolean (value));
      break;
      case PROP_SHOW_DETAILS:
        calendar_set_display_option (calendar,
                                     GNC_CALENDAR_SHOW_DETAILS,
                                     g_value_get_boolean (value));
      break;
      case PROP_DETAIL_WIDTH_CHARS:
        gnc_calendar_set_detail_width_chars (calendar,
                                             g_value_get_int (value));
      break;
      case PROP_DETAIL_HEIGHT_ROWS:
        gnc_calendar_set_detail_height_rows (calendar,
                                             g_value_get_int (value));
      break;
      default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
gnc_calendar_get_property (GObject *object,
                           guint prop_id,
                           GValue *value,
                           GParamSpec *pspec)
{
  GncCalendarPrivate *priv = GNC_CALENDAR_GET_PRIVATE (object);
  GncCalendar *calendar = GNC_CALENDAR (object);

  switch (prop_id)
    {
      case PROP_YEAR:
        g_value_set_int (value, calendar->year);
      break;
      case PROP_MONTH:
        g_value_set_int (value, calendar->month);
      break;
      case PROP_DAY:
        g_value_set_int (value, calendar->selected_day);
      break;
      case PROP_SHOW_HEADING:
        g_value_set_boolean (value, calendar_get_display_option (calendar,
                                                                 GNC_CALENDAR_SHOW_HEADING));
      break;
      case PROP_SHOW_DAY_NAMES:
        g_value_set_boolean (value, calendar_get_display_option (calendar,
                                                                 GNC_CALENDAR_SHOW_DAY_NAMES));
      break;
      case PROP_NO_MONTH_CHANGE:
        g_value_set_boolean (value, calendar_get_display_option (calendar,
                                                                 GNC_CALENDAR_NO_MONTH_CHANGE));
      break;
      case PROP_SHOW_WEEK_NUMBERS:
        g_value_set_boolean (value, calendar_get_display_option (calendar,
                                                                 GNC_CALENDAR_SHOW_WEEK_NUMBERS));
      break;
      case PROP_SHOW_DETAILS:
        g_value_set_boolean (value, calendar_get_display_option (calendar,
                                                                 GNC_CALENDAR_SHOW_DETAILS));
      break;
      case PROP_DETAIL_WIDTH_CHARS:
        g_value_set_int (value, priv->detail_width_chars);
      break;
      case PROP_DETAIL_HEIGHT_ROWS:
        g_value_set_int (value, priv->detail_height_rows);
      break;
      default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

/****************************************
 *             Realization              *
 ****************************************/

static void
calendar_realize_arrows (GncCalendar *calendar)
{
  GtkWidget *widget = GTK_WIDGET (calendar);
  GncCalendarPrivate *priv = GNC_CALENDAR_GET_PRIVATE (calendar);
  GdkWindowAttr attributes;
  gint attributes_mask;
  gint i;

  /* Arrow windows ------------------------------------- */
  if (!(calendar->display_flags & GNC_CALENDAR_NO_MONTH_CHANGE)
      && (calendar->display_flags & GNC_CALENDAR_SHOW_HEADING))
    {
      attributes.wclass = GDK_INPUT_OUTPUT;
      attributes.window_type = GDK_WINDOW_CHILD;
      attributes.visual = gtk_widget_get_visual (widget);
      attributes.colormap = gtk_widget_get_colormap (widget);
      attributes.event_mask = (gtk_widget_get_events (widget) | GDK_EXPOSURE_MASK
                               | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK
                               | GDK_ENTER_NOTIFY_MASK | GDK_LEAVE_NOTIFY_MASK);
      attributes_mask = GDK_WA_X | GDK_WA_Y | GDK_WA_VISUAL | GDK_WA_COLORMAP;
      for (i = 0; i < 4; i++)
        {
          GdkRectangle rect;
          calendar_arrow_rectangle (calendar, i, &rect);

          attributes.x = rect.x;
          attributes.y = rect.y;
          attributes.width = rect.width;
          attributes.height = rect.height;
          priv->arrow_win[i] = gdk_window_new (priv->header_win,
                                               &attributes,
                                               attributes_mask);
          if (GTK_WIDGET_IS_SENSITIVE (widget))
            priv->arrow_state[i] = GTK_STATE_NORMAL;
          else
            priv->arrow_state[i] = GTK_STATE_INSENSITIVE;
          gdk_window_set_background (priv->arrow_win[i],
                                     HEADER_BG_COLOR (GTK_WIDGET (calendar)));
          gdk_window_show (priv->arrow_win[i]);
          gdk_window_set_user_data (priv->arrow_win[i], widget);
        }
    }
  else
    {
      for (i = 0; i < 4; i++)
        priv->arrow_win[i] = NULL;
    }
}

static void
calendar_realize_header (GncCalendar *calendar)
{
  GtkWidget *widget = GTK_WIDGET (calendar);
  GncCalendarPrivate *priv = GNC_CALENDAR_GET_PRIVATE (calendar);
  GdkWindowAttr attributes;
  gint attributes_mask;

  /* Header window ------------------------------------- */
  if (calendar->display_flags & GNC_CALENDAR_SHOW_HEADING)
    {
      attributes.wclass = GDK_INPUT_OUTPUT;
      attributes.window_type = GDK_WINDOW_CHILD;
      attributes.visual = gtk_widget_get_visual (widget);
      attributes.colormap = gtk_widget_get_colormap (widget);
      attributes.event_mask = gtk_widget_get_events (widget) | GDK_EXPOSURE_MASK;
      attributes_mask = GDK_WA_X | GDK_WA_Y | GDK_WA_VISUAL | GDK_WA_COLORMAP;
      attributes.x = widget->style->xthickness;
      attributes.y = widget->style->ythickness;
      attributes.width = widget->allocation.width - 2 * attributes.x;
      attributes.height = priv->header_h;
      priv->header_win = gdk_window_new (widget->window,
                                         &attributes, attributes_mask);

      gdk_window_set_background (priv->header_win,
                                 HEADER_BG_COLOR (GTK_WIDGET (calendar)));
      gdk_window_show (priv->header_win);
      gdk_window_set_user_data (priv->header_win, widget);

    }
  else
    {
      priv->header_win = NULL;
    }
  calendar_realize_arrows (calendar);
}

static void
calendar_realize_day_names (GncCalendar *calendar)
{
  GtkWidget *widget = GTK_WIDGET (calendar);
  GncCalendarPrivate *priv = GNC_CALENDAR_GET_PRIVATE (calendar);
  GdkWindowAttr attributes;
  gint attributes_mask;

  /* Day names	window --------------------------------- */
  if (calendar->display_flags & GNC_CALENDAR_SHOW_DAY_NAMES)
    {
      attributes.wclass = GDK_INPUT_OUTPUT;
      attributes.window_type = GDK_WINDOW_CHILD;
      attributes.visual = gtk_widget_get_visual (widget);
      attributes.colormap = gtk_widget_get_colormap (widget);
      attributes.event_mask = gtk_widget_get_events (widget) | GDK_EXPOSURE_MASK;
      attributes_mask = GDK_WA_X | GDK_WA_Y | GDK_WA_VISUAL | GDK_WA_COLORMAP;
      attributes.x = (widget->style->xthickness + INNER_BORDER);
      attributes.y = priv->header_h + (widget->style->ythickness
                                       + INNER_BORDER);
      attributes.width = (widget->allocation.width
                          - (widget->style->xthickness + INNER_BORDER)
                            * 2);
      attributes.height = priv->day_name_h;
      priv->day_name_win = gdk_window_new (widget->window,
                                           &attributes,
                                           attributes_mask);
      gdk_window_set_background (priv->day_name_win,
                                 BACKGROUND_COLOR (GTK_WIDGET (calendar)));
      gdk_window_show (priv->day_name_win);
      gdk_window_set_user_data (priv->day_name_win, widget);
    }
  else
    {
      priv->day_name_win = NULL;
    }
}

static void
calendar_realize_week_numbers (GncCalendar *calendar)
{
  GtkWidget *widget = GTK_WIDGET (calendar);
  GncCalendarPrivate *priv = GNC_CALENDAR_GET_PRIVATE (calendar);
  GdkWindowAttr attributes;
  gint attributes_mask;

  /* Week number window -------------------------------- */
  if (calendar->display_flags & GNC_CALENDAR_SHOW_WEEK_NUMBERS)
    {
      attributes.wclass = GDK_INPUT_OUTPUT;
      attributes.window_type = GDK_WINDOW_CHILD;
      attributes.visual = gtk_widget_get_visual (widget);
      attributes.colormap = gtk_widget_get_colormap (widget);
      attributes.event_mask = gtk_widget_get_events (widget) | GDK_EXPOSURE_MASK;

      attributes_mask = GDK_WA_X | GDK_WA_Y | GDK_WA_VISUAL | GDK_WA_COLORMAP;
      attributes.x = widget->style->xthickness + INNER_BORDER;
      attributes.y = (priv->header_h + priv->day_name_h
                      + (widget->style->ythickness + INNER_BORDER));
      attributes.width = priv->week_width;
      attributes.height = priv->main_h;
      priv->week_win = gdk_window_new (widget->window,
                                       &attributes, attributes_mask);
      gdk_window_set_background (priv->week_win,
                                 BACKGROUND_COLOR (GTK_WIDGET (calendar)));
      gdk_window_show (priv->week_win);
      gdk_window_set_user_data (priv->week_win, widget);
    }
  else
    {
      priv->week_win = NULL;
    }
}

static void
gnc_calendar_realize (GtkWidget *widget)
{
  GncCalendar *calendar = GNC_CALENDAR (widget);
  GncCalendarPrivate *priv = GNC_CALENDAR_GET_PRIVATE (widget);
  GdkWindowAttr attributes;
  gint attributes_mask;

  GTK_WIDGET_SET_FLAGS (widget, GTK_REALIZED);

  attributes.x = widget->allocation.x;
  attributes.y = widget->allocation.y;
  attributes.width = widget->allocation.width;
  attributes.height = widget->allocation.height;
  attributes.wclass = GDK_INPUT_OUTPUT;
  attributes.window_type = GDK_WINDOW_CHILD;
  attributes.event_mask = (gtk_widget_get_events (widget)
                           | GDK_EXPOSURE_MASK | GDK_KEY_PRESS_MASK | GDK_SCROLL_MASK);
  attributes.visual = gtk_widget_get_visual (widget);
  attributes.colormap = gtk_widget_get_colormap (widget);

  attributes_mask = GDK_WA_X | GDK_WA_Y | GDK_WA_VISUAL | GDK_WA_COLORMAP;
  widget->window = gdk_window_new (widget->parent->window,
                                   &attributes, attributes_mask);

  widget->style = gtk_style_attach (widget->style, widget->window);

  /* Header window ------------------------------------- */
  calendar_realize_header (calendar);
  /* Day names	window --------------------------------- */
  calendar_realize_day_names (calendar);
  /* Week number window -------------------------------- */
  calendar_realize_week_numbers (calendar);
  /* Main Window --------------------------------------	 */
  attributes.event_mask = (gtk_widget_get_events (widget) | GDK_EXPOSURE_MASK
                           | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK
                           | GDK_POINTER_MOTION_MASK | GDK_LEAVE_NOTIFY_MASK);

  attributes.x = priv->week_width + (widget->style->ythickness + INNER_BORDER);
  attributes.y = (priv->header_h + priv->day_name_h
                  + (widget->style->ythickness + INNER_BORDER));
  attributes.width = (widget->allocation.width - attributes.x
                      - (widget->style->xthickness + INNER_BORDER));
  attributes.height = priv->main_h;
  priv->main_win = gdk_window_new (widget->window,
                                   &attributes, attributes_mask);
  gdk_window_set_background (priv->main_win,
                             BACKGROUND_COLOR (GTK_WIDGET (calendar)));
  gdk_window_show (priv->main_win);
  gdk_window_set_user_data (priv->main_win, widget);
  gdk_window_set_background (widget->window, BACKGROUND_COLOR (widget));
  gdk_window_show (widget->window);
  gdk_window_set_user_data (widget->window, widget);
}

static void
gnc_calendar_unrealize (GtkWidget *widget)
{
  GncCalendarPrivate *priv = GNC_CALENDAR_GET_PRIVATE (widget);
  gint i;

  if (priv->header_win)
    {
      for (i = 0; i < 4; i++)
        {
          if (priv->arrow_win[i])
            {
              gdk_window_set_user_data (priv->arrow_win[i], NULL);
              gdk_window_destroy (priv->arrow_win[i]);
              priv->arrow_win[i] = NULL;
            }
        }
      gdk_window_set_user_data (priv->header_win, NULL);
      gdk_window_destroy (priv->header_win);
      priv->header_win = NULL;
    }

  if (priv->week_win)
    {
      gdk_window_set_user_data (priv->week_win, NULL);
      gdk_window_destroy (priv->week_win);
      priv->week_win = NULL;
    }

  if (priv->main_win)
    {
      gdk_window_set_user_data (priv->main_win, NULL);
      gdk_window_destroy (priv->main_win);
      priv->main_win = NULL;
    }
  if (priv->day_name_win)
    {
      gdk_window_set_user_data (priv->day_name_win, NULL);
      gdk_window_destroy (priv->day_name_win);
      priv->day_name_win = NULL;
    }

  GTK_WIDGET_CLASS (gnc_calendar_parent_class)->unrealize (widget);
}

static gchar *
gnc_calendar_get_detail (GncCalendar *calendar,
                         gint row,
                         gint column)
{
  GncCalendarPrivate *priv = GNC_CALENDAR_GET_PRIVATE (calendar);
  gint year, month;

  year = calendar->year;
  month = calendar->month + calendar->day_month[row][column] - MONTH_CURRENT;

  if (month < 0)
    {
      month += 12;
      year -= 1;
    }
  else if (month > 11)
    {
      month -= 12;
      year += 1;
    }

  return priv->detail_func (calendar,
                            year, month,
                            calendar->day[row][column],
                            priv->detail_func_user_data);
}

static gboolean
gnc_calendar_query_tooltip (GtkWidget *widget,
                            gint x,
                            gint y,
                            gboolean keyboard_mode,
                            GtkTooltip *tooltip)
{
  GncCalendarPrivate *priv = GNC_CALENDAR_GET_PRIVATE (widget);
  GncCalendar *calendar = GNC_CALENDAR (widget);
  gchar *detail = NULL;
  GdkRectangle day_rect;

  if (priv->main_win)
    {
      gint x0, y0, row, col;

      gdk_window_get_position (priv->main_win, &x0, &y0);
      col = calendar_column_from_x (calendar, x - x0);
      row = calendar_row_from_y (calendar, y - y0);

      if (col != -1 && row != -1 &&
          (0 != (priv->detail_overflow[row] & (1 << col)) ||
           0 == (calendar->display_flags & GNC_CALENDAR_SHOW_DETAILS)))
        {
          detail = gnc_calendar_get_detail (calendar, row, col);
          calendar_day_rectangle (calendar, row, col, &day_rect);

          day_rect.x += x0;
          day_rect.y += y0;
        }
    }

  if (detail)
    {
      gtk_tooltip_set_tip_area (tooltip, &day_rect);
      gtk_tooltip_set_markup (tooltip, detail);

      g_free (detail);

      return TRUE;
    }

  if (GTK_WIDGET_CLASS (gnc_calendar_parent_class)->query_tooltip)
    return GTK_WIDGET_CLASS (gnc_calendar_parent_class)->query_tooltip (widget, x, y, keyboard_mode, tooltip);

  return FALSE;
}

/****************************************
 *       Size Request and Allocate      *
 ****************************************/

static void
gnc_calendar_size_request (GtkWidget *widget,
                           GtkRequisition *requisition)
{
  GncCalendar *calendar = GNC_CALENDAR (widget);
  GncCalendarPrivate *priv = GNC_CALENDAR_GET_PRIVATE (widget);
  PangoLayout *layout;
  PangoRectangle logical_rect;

  gint height;
  gint i, r, c;
  gint calendar_margin = CALENDAR_MARGIN;
  gint header_width, main_width;
  gint max_header_height = 0;
  gint focus_width;
  gint focus_padding;
  gint max_detail_height;

  gtk_widget_style_get (GTK_WIDGET (widget),
                        "focus-line-width", &focus_width,
                        "focus-padding", &focus_padding,
                        NULL);

  layout = gtk_widget_create_pango_layout (widget, NULL);

  /*
     * Calculate the requisition	width for the widget.
     */

  /* Header width */

  if (calendar->display_flags & GNC_CALENDAR_SHOW_HEADING)
    {
      priv->max_month_width = 0;
      for (i = 0; i < 12; i++)
        {
          pango_layout_set_text (layout, default_monthname[i], -1);
          pango_layout_get_pixel_extents (layout, NULL, &logical_rect);
          priv->max_month_width = MAX (priv->max_month_width,
                                       logical_rect.width + 8);
          max_header_height = MAX (max_header_height, logical_rect.height);
        }

      priv->max_year_width = 0;
      /* Translators:  This is a text measurement template.
         * Translate it to the widest year text.
         *
         * Don't include the prefix "year measurement template|"
         * in the translation.
         *
         * If you don't understand this, leave it as "2000"
         */
      pango_layout_set_text (layout, Q_("year measurement template|2000"), -1);
      pango_layout_get_pixel_extents (layout, NULL, &logical_rect);
      priv->max_year_width = MAX (priv->max_year_width,
                                  logical_rect.width + 8);
      max_header_height = MAX (max_header_height, logical_rect.height);
    }
  else
    {
      priv->max_month_width = 0;
      priv->max_year_width = 0;
    }

  if (calendar->display_flags & GNC_CALENDAR_NO_MONTH_CHANGE)
    header_width = (priv->max_month_width
                    + priv->max_year_width
                    + 3 * 3);
  else
    header_width = (priv->max_month_width
                    + priv->max_year_width
                    + 4 * priv->arrow_width + 3 * 3);

  /* Mainwindow labels width */

  priv->max_day_char_width = 0;
  priv->max_day_char_ascent = 0;
  priv->max_day_char_descent = 0;
  priv->min_day_width = 0;

  for (i = 0; i < 9; i++)
    {
      gchar buffer[32];
      g_snprintf (buffer, sizeof (buffer), Q_("calendar:day:digits|%d"), i * 11);
      pango_layout_set_text (layout, buffer, -1);
      pango_layout_get_pixel_extents (layout, NULL, &logical_rect);
      priv->min_day_width = MAX (priv->min_day_width,
                                 logical_rect.width);

      priv->max_day_char_ascent = MAX (priv->max_day_char_ascent,
                                       PANGO_ASCENT (logical_rect));
      priv->max_day_char_descent = MAX (priv->max_day_char_descent,
                                        PANGO_DESCENT (logical_rect));
    }

  priv->max_label_char_ascent = 0;
  priv->max_label_char_descent = 0;
  if (calendar->display_flags & GNC_CALENDAR_SHOW_DAY_NAMES)
    for (i = 0; i < 7; i++)
      {
        pango_layout_set_text (layout, default_abbreviated_dayname[i], -1);
        pango_layout_line_get_pixel_extents (pango_layout_get_lines_readonly (layout)->data, NULL, &logical_rect);

        priv->min_day_width = MAX (priv->min_day_width, logical_rect.width);
        priv->max_label_char_ascent = MAX (priv->max_label_char_ascent,
                                           PANGO_ASCENT (logical_rect));
        priv->max_label_char_descent = MAX (priv->max_label_char_descent,
                                            PANGO_DESCENT (logical_rect));
      }

  priv->max_week_char_width = 0;
  if (calendar->display_flags & GNC_CALENDAR_SHOW_WEEK_NUMBERS)
    for (i = 0; i < 9; i++)
      {
        gchar buffer[32];
        g_snprintf (buffer, sizeof (buffer), Q_("calendar:week:digits|%d"), i * 11);
        pango_layout_set_text (layout, buffer, -1);
        pango_layout_get_pixel_extents (layout, NULL, &logical_rect);
        priv->max_week_char_width = MAX (priv->max_week_char_width,
                                         logical_rect.width / 2);
      }

  /* Calculate detail extents. Do this as late as possible since
     * pango_layout_set_markup is called which alters font settings. */
  max_detail_height = 0;

  if (priv->detail_func && (calendar->display_flags & GNC_CALENDAR_SHOW_DETAILS))
    {
      gchar *markup, *tail;

      if (priv->detail_width_chars || priv->detail_height_rows)
        {
          gint rows = MAX (1, priv->detail_height_rows) - 1;
          gsize len = priv->detail_width_chars + rows + 16;

          markup = tail = g_alloca (len);

          memcpy (tail, "<small>", 7);
          tail += 7;

          memset (tail, 'm', priv->detail_width_chars);
          tail += priv->detail_width_chars;

          memset (tail, '\n', rows);
          tail += rows;

          memcpy (tail, "</small>", 9);
          tail += 9;

          g_assert (len == (tail - markup));

          pango_layout_set_markup (layout, markup, -1);
          pango_layout_get_pixel_extents (layout, NULL, &logical_rect);

          if (priv->detail_width_chars)
            priv->min_day_width = MAX (priv->min_day_width, logical_rect.width);
          if (priv->detail_height_rows)
            max_detail_height = MAX (max_detail_height, logical_rect.height);
        }

      if (!priv->detail_width_chars || !priv->detail_height_rows)
        for (r = 0; r < 6; r++)
          for (c = 0; c < 7; c++)
            {
              gchar *detail = gnc_calendar_get_detail (calendar, r, c);

              if (detail)
                {
                  markup = g_strconcat ("<small>", detail, "</small>", NULL);
                  pango_layout_set_markup (layout, markup, -1);

                  if (priv->detail_width_chars)
                    {
                      pango_layout_set_wrap (layout, PANGO_WRAP_WORD_CHAR);
                      pango_layout_set_width (layout, PANGO_SCALE * priv->min_day_width);
                    }

                  pango_layout_get_pixel_extents (layout, NULL, &logical_rect);

                  if (!priv->detail_width_chars)
                    priv->min_day_width = MAX (priv->min_day_width, logical_rect.width);
                  if (!priv->detail_height_rows)
                    max_detail_height = MAX (max_detail_height, logical_rect.height);

                  g_free (markup);
                  g_free (detail);
                }
            }
    }

  /* We add one to max_day_char_width to be able to make the marked day "bold" */
  priv->max_day_char_width = priv->min_day_width / 2 + 1;

  main_width = (7 * (priv->min_day_width + (focus_padding + focus_width) * 2) + (DAY_XSEP * 6) + CALENDAR_MARGIN * 2
                + (priv->max_week_char_width
                   ? priv->max_week_char_width * 2 + (focus_padding + focus_width) * 2 + CALENDAR_XSEP * 2
                   : 0));

  requisition->width = MAX (header_width, main_width + INNER_BORDER * 2) + widget->style->xthickness * 2;

  /*
     * Calculate the requisition height for the widget.
     */

  if (calendar->display_flags & GNC_CALENDAR_SHOW_HEADING)
    {
      priv->header_h = (max_header_height + CALENDAR_YSEP * 2);
    }
  else
    {
      priv->header_h = 0;
    }

  if (calendar->display_flags & GNC_CALENDAR_SHOW_DAY_NAMES)
    {
      priv->day_name_h = (priv->max_label_char_ascent
                          + priv->max_label_char_descent
                          + 2 * (focus_padding + focus_width) + calendar_margin);
      calendar_margin = CALENDAR_YSEP;
    }
  else
    {
      priv->day_name_h = 0;
    }

  priv->main_h = (CALENDAR_MARGIN + calendar_margin
                  + 6 * (priv->max_day_char_ascent
                         + priv->max_day_char_descent
                         + max_detail_height
                         + 2 * (focus_padding + focus_width))
                  + DAY_YSEP * 5);

  height = (priv->header_h + priv->day_name_h
            + priv->main_h);

  requisition->height = height + (widget->style->ythickness + INNER_BORDER) * 2;

  g_object_unref (layout);
}

static void
gnc_calendar_size_allocate (GtkWidget *widget,
                            GtkAllocation *allocation)
{
  GncCalendar *calendar = GNC_CALENDAR (widget);
  GncCalendarPrivate *priv = GNC_CALENDAR_GET_PRIVATE (widget);
  gint xthickness = widget->style->xthickness;
  gint ythickness = widget->style->xthickness;
  guint i;

  widget->allocation = *allocation;

  if (calendar->display_flags & GNC_CALENDAR_SHOW_WEEK_NUMBERS)
    {
      priv->day_width = (priv->min_day_width
                         * ((allocation->width - (xthickness + INNER_BORDER) * 2
                             - (CALENDAR_MARGIN * 2) - (DAY_XSEP * 6) - CALENDAR_XSEP * 2))
                         / (7 * priv->min_day_width + priv->max_week_char_width * 2));
      priv->week_width = ((allocation->width - (xthickness + INNER_BORDER) * 2
                           - (CALENDAR_MARGIN * 2) - (DAY_XSEP * 6) - CALENDAR_XSEP * 2)
                          - priv->day_width * 7 + CALENDAR_MARGIN + CALENDAR_XSEP);
    }
  else
    {
      priv->day_width = (allocation->width
                         - (xthickness + INNER_BORDER) * 2
                         - (CALENDAR_MARGIN * 2)
                         - (DAY_XSEP * 6)) / 7;
      priv->week_width = 0;
    }

  if (GTK_WIDGET_REALIZED (widget))
    {
      gdk_window_move_resize (widget->window,
                              allocation->x, allocation->y,
                              allocation->width, allocation->height);
      if (priv->header_win)
        gdk_window_move_resize (priv->header_win,
                                xthickness, ythickness,
                                allocation->width - 2 * xthickness, priv->header_h);

      for (i = 0; i < 4; i++)
        {
          if (priv->arrow_win[i])
            {
              GdkRectangle rect;
              calendar_arrow_rectangle (calendar, i, &rect);

              gdk_window_move_resize (priv->arrow_win[i],
                                      rect.x, rect.y, rect.width, rect.height);
            }
        }

      if (priv->day_name_win)
        gdk_window_move_resize (priv->day_name_win,
                                xthickness + INNER_BORDER,
                                priv->header_h + (widget->style->ythickness + INNER_BORDER),
                                allocation->width - (xthickness + INNER_BORDER) * 2,
                                priv->day_name_h);
      if (gtk_widget_get_direction (widget) == GTK_TEXT_DIR_LTR)
        {
          if (priv->week_win)
            gdk_window_move_resize (priv->week_win,
                                    (xthickness + INNER_BORDER),
                                    priv->header_h + priv->day_name_h
                                    + (widget->style->ythickness + INNER_BORDER),
                                    priv->week_width,
                                    priv->main_h);
          gdk_window_move_resize (priv->main_win,
                                  priv->week_width + (xthickness + INNER_BORDER),
                                  priv->header_h + priv->day_name_h
                                  + (widget->style->ythickness + INNER_BORDER),
                                  allocation->width
                                  - priv->week_width
                                  - (xthickness + INNER_BORDER) * 2,
                                  priv->main_h);
        }
      else
        {
          gdk_window_move_resize (priv->main_win,
                                  (xthickness + INNER_BORDER),
                                  priv->header_h + priv->day_name_h
                                  + (widget->style->ythickness + INNER_BORDER),
                                  allocation->width
                                  - priv->week_width
                                  - (xthickness + INNER_BORDER) * 2,
                                  priv->main_h);
          if (priv->week_win)
            gdk_window_move_resize (priv->week_win,
                                    allocation->width
                                    - priv->week_width
                                    - (xthickness + INNER_BORDER),
                                    priv->header_h + priv->day_name_h
                                    + (widget->style->ythickness + INNER_BORDER),
                                    priv->week_width,
                                    priv->main_h);
        }
    }
}

/****************************************
 *              Repainting              *
 ****************************************/

static void
calendar_paint_header (GncCalendar *calendar)
{
  GtkWidget *widget = GTK_WIDGET (calendar);
  GncCalendarPrivate *priv = GNC_CALENDAR_GET_PRIVATE (calendar);
  cairo_t *cr;
  char buffer[255];
  int x, y;
  gint header_width;
  gint max_month_width;
  gint max_year_width;
  PangoLayout *layout;
  PangoRectangle logical_rect;
  gboolean year_left;

  gchar *str;
  gint display_month_number;

  if (gtk_widget_get_direction (widget) == GTK_TEXT_DIR_LTR)
    year_left = priv->year_before;
  else
    year_left = !priv->year_before;

  cr = gdk_cairo_create (priv->header_win);

  header_width = widget->allocation.width - 2 * widget->style->xthickness;

  max_month_width = priv->max_month_width;
  max_year_width = priv->max_year_width;

  gtk_paint_shadow (widget->style, priv->header_win,
                    GTK_STATE_NORMAL, GTK_SHADOW_OUT,
                    NULL, widget, "calendar",
                    0, 0, header_width, priv->header_h);

  str = calendar->calendarFunction->paint_header_helper (calendar->year, calendar->month
                                                                         + 1, calendar->selected_day, &display_month_number);

  /* Translators: This dictates how the year is displayed in
     * gtkcalendar widget.  See strftime() manual for the format.
     * Use only ASCII in the translation.
     *
     * Also look for the msgid "year measurement template|2000".
     * Translate that entry to a year with the widest output of this
     * msgid.
     *
     * Don't include the prefix "calendar year format|" in the
     * translation. "%Y" is appropriate for most locales.
     */

  layout = gtk_widget_create_pango_layout (widget, str);
  g_free (str);

  pango_layout_get_pixel_extents (layout, NULL, &logical_rect);

  /* Draw title */
  y = (priv->header_h - logical_rect.height) / 2;

  /* Draw year and its arrows */

  if (calendar->display_flags & GNC_CALENDAR_NO_MONTH_CHANGE)
    if (year_left)
      x = 3 + (max_year_width - logical_rect.width) / 2;
    else
      x = header_width - (3 + max_year_width
                          - (max_year_width - logical_rect.width) / 2);
  else if (year_left)
    x = 3 + priv->arrow_width + (max_year_width - logical_rect.width) / 2;
  else
    x = header_width - (3 + priv->arrow_width + max_year_width
                        - (max_year_width - logical_rect.width) / 2);

  gdk_cairo_set_source_color (cr, HEADER_FG_COLOR (GTK_WIDGET (calendar)));
  cairo_move_to (cr, x, y);
  pango_cairo_show_layout (cr, layout);

  /* Draw month */
  g_snprintf (buffer, sizeof (buffer), "%s", default_monthname[display_month_number - 1]);
  pango_layout_set_text (layout, buffer, -1);
  pango_layout_get_pixel_extents (layout, NULL, &logical_rect);

  if (calendar->display_flags & GNC_CALENDAR_NO_MONTH_CHANGE)
    if (year_left)
      x = header_width - (3 + max_month_width
                          - (max_month_width - logical_rect.width) / 2);
    else
      x = 3 + (max_month_width - logical_rect.width) / 2;
  else if (year_left)
    x = header_width - (3 + priv->arrow_width + max_month_width
                        - (max_month_width - logical_rect.width) / 2);
  else
    x = 3 + priv->arrow_width + (max_month_width - logical_rect.width) / 2;

  cairo_move_to (cr, x, y);
  pango_cairo_show_layout (cr, layout);

  g_object_unref (layout);
  cairo_destroy (cr);
}

static void
calendar_paint_day_names (GncCalendar *calendar)
{
  GtkWidget *widget = GTK_WIDGET (calendar);
  GncCalendarPrivate *priv = GNC_CALENDAR_GET_PRIVATE (calendar);
  cairo_t *cr;
  char buffer[255];
  int day, i;
  int day_width, cal_width;
  int day_wid_sep;
  PangoLayout *layout;
  PangoRectangle logical_rect;
  gint focus_padding;
  gint focus_width;

  cr = gdk_cairo_create (priv->day_name_win);

  gtk_widget_style_get (GTK_WIDGET (widget),
                        "focus-line-width", &focus_width,
                        "focus-padding", &focus_padding,
                        NULL);

  day_width = priv->day_width;
  cal_width = widget->allocation.width;
  day_wid_sep = day_width + DAY_XSEP;

  /*
     * Draw rectangles as inverted background for the labels.
     */

  gdk_cairo_set_source_color (cr, SELECTED_BG_COLOR (widget));
  cairo_rectangle (cr,
                   CALENDAR_MARGIN, CALENDAR_MARGIN,
                   cal_width - CALENDAR_MARGIN * 2,
                   priv->day_name_h - CALENDAR_MARGIN);
  cairo_fill (cr);

  if (calendar->display_flags & GNC_CALENDAR_SHOW_WEEK_NUMBERS)
    {
      cairo_rectangle (cr,
                       CALENDAR_MARGIN,
                       priv->day_name_h - CALENDAR_YSEP,
                       priv->week_width - CALENDAR_YSEP - CALENDAR_MARGIN,
                       CALENDAR_YSEP);
      cairo_fill (cr);
    }

  /*
     * Write the labels
     */

  layout = gtk_widget_create_pango_layout (widget, NULL);

  gdk_cairo_set_source_color (cr, SELECTED_FG_COLOR (widget));
  for (i = 0; i < 7; i++)
    {
      if (gtk_widget_get_direction (GTK_WIDGET (calendar)) == GTK_TEXT_DIR_RTL)
        day = 6 - i;
      else
        day = i;
      day = (day + priv->week_start) % 7;
      g_snprintf (buffer, sizeof (buffer), "%s", default_abbreviated_dayname[day]);

      pango_layout_set_text (layout, buffer, -1);
      pango_layout_get_pixel_extents (layout, NULL, &logical_rect);

      cairo_move_to (cr,
                     (CALENDAR_MARGIN +
                      +(gtk_widget_get_direction (widget) == GTK_TEXT_DIR_LTR ?
                        (priv->week_width + (priv->week_width ? CALENDAR_XSEP : 0))
                                                                              : 0)
                      + day_wid_sep * i
                      + (day_width - logical_rect.width) / 2),
                     CALENDAR_MARGIN + focus_width + focus_padding + logical_rect.y);
      pango_cairo_show_layout (cr, layout);
    }

  g_object_unref (layout);
  cairo_destroy (cr);
}

static void
calendar_paint_week_numbers (GncCalendar *calendar)
{
  GtkWidget *widget = GTK_WIDGET (calendar);
  GncCalendarPrivate *priv = GNC_CALENDAR_GET_PRIVATE (calendar);
  cairo_t *cr;

  guint week = 0, year;
  gint row, x_loc, y_loc;
  gint day_height;
  char buffer[32];
  PangoLayout *layout;
  PangoRectangle logical_rect;
  gint focus_padding;
  gint focus_width;

  cr = gdk_cairo_create (priv->week_win);

  gtk_widget_style_get (GTK_WIDGET (widget),
                        "focus-line-width", &focus_width,
                        "focus-padding", &focus_padding,
                        NULL);

  /*
     * Draw a rectangle as inverted background for the labels.
     */

  gdk_cairo_set_source_color (cr, SELECTED_BG_COLOR (widget));
  if (priv->day_name_win)
    cairo_rectangle (cr,
                     CALENDAR_MARGIN,
                     0,
                     priv->week_width - CALENDAR_MARGIN,
                     priv->main_h - CALENDAR_MARGIN);
  else
    cairo_rectangle (cr,
                     CALENDAR_MARGIN,
                     CALENDAR_MARGIN,
                     priv->week_width - CALENDAR_MARGIN,
                     priv->main_h - 2 * CALENDAR_MARGIN);
  cairo_fill (cr);

  /*
     * Write the labels
     */

  layout = gtk_widget_create_pango_layout (widget, NULL);

  gdk_cairo_set_source_color (cr, SELECTED_FG_COLOR (widget));
  day_height = calendar_row_height (calendar);
  for (row = 0; row < 6; row++)
    {
      gboolean result;

      year = calendar->year;
      if (calendar->day[row][6] < 15 && row > 3 && calendar->month == 11)
        year++;

      result = calendar->calendarFunction->week_of_year (&week, &year,
                                                         ((calendar->day[row][6] < 15 && row > 3 ? 1 : 0)
                                                          + calendar->month) % 12 + 1, calendar->day[row][6]);
      g_return_if_fail (result);

      /* Translators: this defines whether the week numbers should use
         * localized digits or the ones used in English (0123...).
         *
         * Translate to "%Id" if you want to use localized digits, or
         * translate to "%d" otherwise.  Don't include the
         * "calendar:week:digits|" part in the translation.
         *
         * Note that translating this doesn't guarantee that you get localized
         * digits.  That needs support from your system and locale definition
         * too.
         */
      g_snprintf (buffer, sizeof (buffer), Q_("calendar:week:digits|%d"), week);
      pango_layout_set_text (layout, buffer, -1);
      pango_layout_get_pixel_extents (layout, NULL, &logical_rect);

      y_loc = calendar_top_y_for_row (calendar, row) + (day_height - logical_rect.height) / 2;

      x_loc = (priv->week_width
               - logical_rect.width
               - CALENDAR_XSEP - focus_padding - focus_width);

      cairo_move_to (cr, x_loc, y_loc);
      pango_cairo_show_layout (cr, layout);
    }

  g_object_unref (layout);
  cairo_destroy (cr);
}

static void
calendar_invalidate_day_num (GncCalendar *calendar,
                             gint day)
{
  gint r, c, row, col;

  row = -1;
  col = -1;
  for (r = 0; r < 6; r++)
    for (c = 0; c < 7; c++)
      if (calendar->day_month[r][c] == MONTH_CURRENT &&
          calendar->day[r][c] == day)
        {
          row = r;
          col = c;
        }

  g_return_if_fail (row != -1);
  g_return_if_fail (col != -1);

  calendar_invalidate_day (calendar, row, col);
}

static void
calendar_invalidate_day (GncCalendar *calendar,
                         gint row,
                         gint col)
{
  GncCalendarPrivate *priv = GNC_CALENDAR_GET_PRIVATE (calendar);

  if (priv->main_win)
    {
      GdkRectangle day_rect;

      calendar_day_rectangle (calendar, row, col, &day_rect);
      gdk_window_invalidate_rect (priv->main_win, &day_rect, FALSE);
    }
}

static gboolean
is_color_attribute (PangoAttribute *attribute,
                    gpointer data)
{
  return (attribute->klass->type == PANGO_ATTR_FOREGROUND ||
          attribute->klass->type == PANGO_ATTR_BACKGROUND);
}

static void
calendar_paint_day (GncCalendar *calendar,
                    gint row,
                    gint col)
{
  GtkWidget *widget = GTK_WIDGET (calendar);
  GncCalendarPrivate *priv = GNC_CALENDAR_GET_PRIVATE (calendar);
  cairo_t *cr;
  GdkColor *text_color;
  gchar *detail;
  gchar buffer[32];
  gint day;
  gint x_loc, y_loc;
  GdkRectangle day_rect;

  PangoLayout *layout;
  PangoRectangle logical_rect;
  gboolean overflow = FALSE;
  gboolean show_details;
  gint display_selected_day ;

  // a selected day may be different in different calendar so I use  a override one for this
  display_selected_day = calendar->calendarFunction->display_selected_day (calendar->year, calendar->month
                                                                                           + 1, calendar->selected_day);

  g_return_if_fail (row < 6);
  g_return_if_fail (col < 7);

  cr = gdk_cairo_create (priv->main_win);

  day = calendar->day[row][col];
  show_details = (calendar->display_flags & GNC_CALENDAR_SHOW_DETAILS);

  calendar_day_rectangle (calendar, row, col, &day_rect);

  if (calendar->day_month[row][col] == MONTH_PREV)
    {
      text_color = PREV_MONTH_COLOR (widget);
    }
  else if (calendar->day_month[row][col] == MONTH_NEXT)
    {
      text_color = NEXT_MONTH_COLOR (widget);
    }
  else
    {
#if 0
                                                                                                                              if (calendar->highlight_row == row && calendar->highlight_col == col)
	{
	  cairo_set_source_color (cr, HIGHLIGHT_BG_COLOR (widget));
	  gdk_cairo_rectangle (cr, &day_rect);
	  cairo_fill (cr);
	}
#endif

      if (display_selected_day == day)
        {
          gdk_cairo_set_source_color (cr, SELECTED_BG_COLOR (widget));
          gdk_cairo_rectangle (cr, &day_rect);
          cairo_fill (cr);
        }
      if (display_selected_day == day)
        text_color = SELECTED_FG_COLOR (widget);
      else if (calendar->marked_date[day - 1])
        text_color = MARKED_COLOR (widget);
      else
        text_color = NORMAL_DAY_COLOR (widget);
    }

/* Translators: this defines whether the day numbers should use
 * localized digits or the ones used in English (0123...).
 *
 * Translate to "%Id" if you want to use localized digits, or
 * translate to "%d" otherwise.  Don't include the "calendar:day:digits|"
 * part in the translation.
 *
 * Note that translating this doesn't guarantee that you get localized
 * digits.  That needs support from your system and locale definition
 * too.
 */
  g_snprintf (buffer, sizeof (buffer), Q_("calendar:day:digits|%d"), day);

/* Get extra information to show, if any: */

  if (priv->detail_func)
    detail = gnc_calendar_get_detail (calendar, row, col);
  else
    detail = NULL;

  layout = gtk_widget_create_pango_layout (widget, buffer);
  pango_layout_set_alignment (layout, PANGO_ALIGN_CENTER);
  pango_layout_get_pixel_extents (layout, NULL, &logical_rect);

  x_loc = day_rect.x + (day_rect.width - logical_rect.width) / 2;
  y_loc = day_rect.y;

  gdk_cairo_set_source_color (cr, text_color);
  cairo_move_to (cr, x_loc, y_loc);
  pango_cairo_show_layout (cr, layout);

  if (calendar->day_month[row][col] == MONTH_CURRENT &&
      (calendar->marked_date[day - 1] || (detail && !show_details)))
    {
      cairo_move_to (cr, x_loc - 1, y_loc);
      pango_cairo_show_layout (cr, layout);
    }

  y_loc += priv->max_day_char_descent;

  if (priv->detail_func && show_details)
    {
      cairo_save (cr);

      if (display_selected_day == day)
        gdk_cairo_set_source_color (cr, &widget->style->text[GTK_STATE_ACTIVE]);
      else if (calendar->day_month[row][col] == MONTH_CURRENT)
        gdk_cairo_set_source_color (cr, &widget->style->base[GTK_STATE_ACTIVE]);
      else
        gdk_cairo_set_source_color (cr, &widget->style->base[GTK_STATE_INSENSITIVE]);

      cairo_set_line_width (cr, 1);
      cairo_move_to (cr, day_rect.x + 2, y_loc + 0.5);
      cairo_line_to (cr, day_rect.x + day_rect.width - 2, y_loc + 0.5);
      cairo_stroke (cr);

      cairo_restore (cr);

      y_loc += 2;
    }

  if (detail && show_details)
    {
      gchar *markup = g_strconcat ("<small>", detail, "</small>", NULL);
      pango_layout_set_markup (layout, markup, -1);
      g_free (markup);

      if (day == display_selected_day)
        {
/* Stripping colors as they conflict with selection marking. */

          PangoAttrList *attrs = pango_layout_get_attributes (layout);
          PangoAttrList *colors = NULL;

          if (attrs)
            colors = pango_attr_list_filter (attrs, is_color_attribute, NULL);
          if (colors)
            pango_attr_list_unref (colors);
        }

      pango_layout_set_wrap (layout, PANGO_WRAP_WORD_CHAR);
      pango_layout_set_width (layout, PANGO_SCALE * day_rect.width);

      if (priv->detail_height_rows)
        {
          gint dy = day_rect.height - (y_loc - day_rect.y);
          pango_layout_set_height (layout, PANGO_SCALE * dy);
          pango_layout_set_ellipsize (layout, PANGO_ELLIPSIZE_END);
        }

      cairo_move_to (cr, day_rect.x, y_loc);
      pango_cairo_show_layout (cr, layout);
    }

  if (GTK_WIDGET_HAS_FOCUS (calendar)
      && calendar->focus_row == row && calendar->focus_col == col)
    {
      GtkStateType state;

      if ( display_selected_day == day )
        state = GTK_WIDGET_HAS_FOCUS (widget) ? GTK_STATE_SELECTED : GTK_STATE_ACTIVE;
      else
        state = GTK_STATE_NORMAL;

      gtk_paint_focus (widget->style,
                       priv->main_win,
                       state,
                       NULL, widget, "calendar-day",
                       day_rect.x, day_rect.y,
                       day_rect.width, day_rect.height);
    }

  if (overflow)
    priv->detail_overflow[row] |= (1 << col);
  else
    priv->detail_overflow[row] &= ~(1 << col);

  g_object_unref (layout);
  cairo_destroy (cr);
  g_free (detail);
}

static void
calendar_paint_main (GncCalendar *calendar)
{
  gint row, col;

  for (col = 0; col < 7; col++)
    for (row = 0; row < 6; row++)
      calendar_paint_day (calendar, row, col);
}

static void
calendar_invalidate_arrow (GncCalendar *calendar,
                           guint arrow)
{
  GncCalendarPrivate *priv = GNC_CALENDAR_GET_PRIVATE (calendar);
  GdkWindow *window;

  window = priv->arrow_win[arrow];
  if (window)
    gdk_window_invalidate_rect (window, NULL, FALSE);
}
static void calendar_pre_init_select_day (GncCalendar *calendar,
                                          guint day)
{

  gint selected_day = calendar->selected_day;
  gint displayed_month;
  gchar *displayed_year;
  gint real_val_displayed_year;
  gint real_day;
  gint real_month;
  gint real_year;
  displayed_year = calendar->calendarFunction->paint_header_helper (calendar->year, calendar->month
                                                                                    + 1, selected_day, &displayed_month);
  real_val_displayed_year = g_ascii_strtoll (displayed_year, NULL, 10);
  // here I think ! May ! should be change back to gregorian!
  calendar->calendarFunction->invalidate_display_day_to_gregorian (real_val_displayed_year, displayed_month, day, &real_year, &real_month, &real_day);
  if (calendar->year != real_year)
    calendar->year = real_year;
  if (calendar->month != (real_month - 1))
    calendar->month = real_month - 1;
  calendar->selected_day = real_day;

}

static void
calendar_paint_arrow (GncCalendar *calendar,
                      guint arrow)
{
  GtkWidget *widget = GTK_WIDGET (calendar);
  GncCalendarPrivate *priv = GNC_CALENDAR_GET_PRIVATE (widget);
  GdkWindow *window;

  window = priv->arrow_win[arrow];
  if (window)
    {
      cairo_t *cr = gdk_cairo_create (window);
      gint width, height;
      gint state;

      state = priv->arrow_state[arrow];

      gdk_cairo_set_source_color (cr, &widget->style->bg[state]);
      cairo_paint (cr);
      cairo_destroy (cr);

      gdk_drawable_get_size (window, &width, &height);
      if (arrow == ARROW_MONTH_LEFT || arrow == ARROW_YEAR_LEFT)
        gtk_paint_arrow (widget->style, window, state,
                         GTK_SHADOW_OUT, NULL, widget, "calendar",
                         GTK_ARROW_LEFT, TRUE,
                         width / 2 - 3, height / 2 - 4, 8, 8);
      else
        gtk_paint_arrow (widget->style, window, state,
                         GTK_SHADOW_OUT, NULL, widget, "calendar",
                         GTK_ARROW_RIGHT, TRUE,
                         width / 2 - 4, height / 2 - 4, 8, 8);
    }
}

static gboolean
gnc_calendar_expose (GtkWidget *widget,
                     GdkEventExpose *event)
{
  GncCalendar *calendar = GNC_CALENDAR (widget);
  GncCalendarPrivate *priv = GNC_CALENDAR_GET_PRIVATE (widget);
  int i;

  if (GTK_WIDGET_DRAWABLE (widget))
    {
      if (event->window == priv->main_win)
        calendar_paint_main (calendar);

      if (event->window == priv->header_win)
        calendar_paint_header (calendar);

      for (i = 0; i < 4; i++)
        if (event->window == priv->arrow_win[i])
          calendar_paint_arrow (calendar, i);

      if (event->window == priv->day_name_win)
        calendar_paint_day_names (calendar);

      if (event->window == priv->week_win)
        calendar_paint_week_numbers (calendar);
      if (event->window == widget->window)
        {
          gtk_paint_shadow (widget->style, widget->window, GTK_WIDGET_STATE (widget),
                            GTK_SHADOW_IN, NULL, widget, "calendar",
                            0, 0, widget->allocation.width, widget->allocation.height);
        }
    }

  return FALSE;
}

/****************************************
 *           Mouse handling             *
 ****************************************/

static void
calendar_arrow_action (GncCalendar *calendar,
                       guint arrow)
{
  switch (arrow)
    {
      case ARROW_YEAR_LEFT:
        calendar_set_year_prev (calendar);
      break;
      case ARROW_YEAR_RIGHT:
        calendar_set_year_next (calendar);
      break;
      case ARROW_MONTH_LEFT:
        calendar_set_month_prev (calendar);
      break;
      case ARROW_MONTH_RIGHT:
        calendar_set_month_next (calendar);
      break;
      default:;
/* do nothing */
    }
}

static gboolean
calendar_timer (gpointer data)
{
  GncCalendar *calendar = data;
  GncCalendarPrivate *priv = GNC_CALENDAR_GET_PRIVATE (calendar);
  gboolean retval = FALSE;

  if (priv->timer)
    {
      calendar_arrow_action (calendar, priv->click_child);

      if (priv->need_timer)
        {
          GtkSettings *settings;
          guint timeout;

          settings = gtk_widget_get_settings (GTK_WIDGET (calendar));
          g_object_get (settings, "gtk-timeout-repeat", &timeout, NULL);

          priv->need_timer = FALSE;
          priv->timer = gdk_threads_add_timeout_full (G_PRIORITY_DEFAULT_IDLE,
                                                      timeout * SCROLL_DELAY_FACTOR,
                                                      (GSourceFunc) calendar_timer,
                                                      (gpointer) calendar, NULL);
        }
      else
        retval = TRUE;
    }

  return retval;
}

static void
calendar_start_spinning (GncCalendar *calendar,
                         gint click_child)
{
  GncCalendarPrivate *priv = GNC_CALENDAR_GET_PRIVATE (calendar);

  priv->click_child = click_child;

  if (!priv->timer)
    {
      GtkSettings *settings;
      guint timeout;

      settings = gtk_widget_get_settings (GTK_WIDGET (calendar));
      g_object_get (settings, "gtk-timeout-initial", &timeout, NULL);

      priv->need_timer = TRUE;
      priv->timer = gdk_threads_add_timeout_full (G_PRIORITY_DEFAULT_IDLE,
                                                  timeout,
                                                  (GSourceFunc) calendar_timer,
                                                  (gpointer) calendar, NULL);
    }
}

static void
calendar_stop_spinning (GncCalendar *calendar)
{
  GncCalendarPrivate *priv = GNC_CALENDAR_GET_PRIVATE (calendar);

  if (priv->timer)
    {
      g_source_remove (priv->timer);
      priv->timer = 0;
      priv->need_timer = FALSE;
    }
}

static void
calendar_main_button_press (GncCalendar *calendar,
                            GdkEventButton *event)
{
  GtkWidget *widget = GTK_WIDGET (calendar);
  GncCalendarPrivate *priv = GNC_CALENDAR_GET_PRIVATE (calendar);
  gint x, y;
  gint row, col;
  gint day_month;
  gint day;

  x = (gint) (event->x);
  y = (gint) (event->y);

  row = calendar_row_from_y (calendar, y);
  col = calendar_column_from_x (calendar, x);

  /* If row or column isn't found, just return. */
  if (row == -1 || col == -1)
    return;

  day_month = calendar->day_month[row][col];

  if (event->type == GDK_BUTTON_PRESS)
    {
      day = calendar->day[row][col];

      if (day_month == MONTH_PREV)
        calendar_set_month_prev (calendar);
      else if (day_month == MONTH_NEXT)
        calendar_set_month_next (calendar);

      if (!GTK_WIDGET_HAS_FOCUS (widget))
        gtk_widget_grab_focus (widget);

      if (event->button == 1)
        {
          priv->in_drag = 1;
          priv->drag_start_x = x;
          priv->drag_start_y = y;
        }

      calendar_select_and_focus_day (calendar, day);
    }
  else if (event->type == GDK_2BUTTON_PRESS)
    {
      priv->in_drag = 0;
      if (day_month == MONTH_CURRENT)
        g_signal_emit (calendar,
                       gnc_calendar_signals[DAY_SELECTED_DOUBLE_CLICK_SIGNAL],
                       0);
    }
}

static gboolean
gnc_calendar_button_press (GtkWidget *widget,
                           GdkEventButton *event)
{
  GncCalendar *calendar = GNC_CALENDAR (widget);
  GncCalendarPrivate *priv = GNC_CALENDAR_GET_PRIVATE (widget);
  gint arrow = -1;

  if (event->window == priv->main_win)
    calendar_main_button_press (calendar, event);

  if (!GTK_WIDGET_HAS_FOCUS (widget))
    gtk_widget_grab_focus (widget);

  for (arrow = ARROW_YEAR_LEFT; arrow <= ARROW_MONTH_RIGHT; arrow++)
    {
      if (event->window == priv->arrow_win[arrow])
        {

          /* only call the action on single click, not double */
          if (event->type == GDK_BUTTON_PRESS)
            {
              if (event->button == 1)
                calendar_start_spinning (calendar, arrow);

              calendar_arrow_action (calendar, arrow);
            }

          return TRUE;
        }
    }

  return FALSE;
}

static gboolean
gnc_calendar_button_release (GtkWidget *widget,
                             GdkEventButton *event)
{
  GncCalendar *calendar = GNC_CALENDAR (widget);
  GncCalendarPrivate *priv = GNC_CALENDAR_GET_PRIVATE (widget);

  if (event->button == 1)
    {
      calendar_stop_spinning (calendar);

      if (priv->in_drag)
        priv->in_drag = 0;
    }

  return TRUE;
}

static gboolean
gnc_calendar_motion_notify (GtkWidget *widget,
                            GdkEventMotion *event)
{
  GncCalendar *calendar = GNC_CALENDAR (widget);
  GncCalendarPrivate *priv = GNC_CALENDAR_GET_PRIVATE (widget);
  gint event_x, event_y;
  gint row, col;
  gint old_row, old_col;

  event_x = (gint) (event->x);
  event_y = (gint) (event->y);

  if (event->window == priv->main_win)
    {

      if (priv->in_drag)
        {
          if (gtk_drag_check_threshold (widget,
                                        priv->drag_start_x, priv->drag_start_y,
                                        event->x, event->y))
            {
              GdkDragContext *context;
              GtkTargetList *target_list = gtk_target_list_new (NULL, 0);
              gtk_target_list_add_text_targets (target_list, 0);
              context = gtk_drag_begin (widget, target_list, GDK_ACTION_COPY,
                                        1, (GdkEvent *) event);

              priv->in_drag = 0;

              gtk_target_list_unref (target_list);
              gtk_drag_set_icon_default (context);
            }
        }
      else
        {
          row = calendar_row_from_y (calendar, event_y);
          col = calendar_column_from_x (calendar, event_x);

          if (row != calendar->highlight_row || calendar->highlight_col != col)
            {
              old_row = calendar->highlight_row;
              old_col = calendar->highlight_col;
              if (old_row > -1 && old_col > -1)
                {
                  calendar->highlight_row = -1;
                  calendar->highlight_col = -1;
                  calendar_invalidate_day (calendar, old_row, old_col);
                }

              calendar->highlight_row = row;
              calendar->highlight_col = col;

              if (row > -1 && col > -1)
                calendar_invalidate_day (calendar, row, col);
            }
        }
    }
  return TRUE;
}

static gboolean
gnc_calendar_enter_notify (GtkWidget *widget,
                           GdkEventCrossing *event)
{
  GncCalendar *calendar = GNC_CALENDAR (widget);
  GncCalendarPrivate *priv = GNC_CALENDAR_GET_PRIVATE (widget);

  if (event->window == priv->arrow_win[ARROW_MONTH_LEFT])
    {
      priv->arrow_state[ARROW_MONTH_LEFT] = GTK_STATE_PRELIGHT;
      calendar_invalidate_arrow (calendar, ARROW_MONTH_LEFT);
    }

  if (event->window == priv->arrow_win[ARROW_MONTH_RIGHT])
    {
      priv->arrow_state[ARROW_MONTH_RIGHT] = GTK_STATE_PRELIGHT;
      calendar_invalidate_arrow (calendar, ARROW_MONTH_RIGHT);
    }

  if (event->window == priv->arrow_win[ARROW_YEAR_LEFT])
    {
      priv->arrow_state[ARROW_YEAR_LEFT] = GTK_STATE_PRELIGHT;
      calendar_invalidate_arrow (calendar, ARROW_YEAR_LEFT);
    }

  if (event->window == priv->arrow_win[ARROW_YEAR_RIGHT])
    {
      priv->arrow_state[ARROW_YEAR_RIGHT] = GTK_STATE_PRELIGHT;
      calendar_invalidate_arrow (calendar, ARROW_YEAR_RIGHT);
    }

  return TRUE;
}

static gboolean
gnc_calendar_leave_notify (GtkWidget *widget,
                           GdkEventCrossing *event)
{
  GncCalendar *calendar = GNC_CALENDAR (widget);
  GncCalendarPrivate *priv = GNC_CALENDAR_GET_PRIVATE (widget);
  gint row;
  gint col;

  if (event->window == priv->main_win)
    {
      row = calendar->highlight_row;
      col = calendar->highlight_col;
      calendar->highlight_row = -1;
      calendar->highlight_col = -1;
      if (row > -1 && col > -1)
        calendar_invalidate_day (calendar, row, col);
    }

  if (event->window == priv->arrow_win[ARROW_MONTH_LEFT])
    {
      priv->arrow_state[ARROW_MONTH_LEFT] = GTK_STATE_NORMAL;
      calendar_invalidate_arrow (calendar, ARROW_MONTH_LEFT);
    }

  if (event->window == priv->arrow_win[ARROW_MONTH_RIGHT])
    {
      priv->arrow_state[ARROW_MONTH_RIGHT] = GTK_STATE_NORMAL;
      calendar_invalidate_arrow (calendar, ARROW_MONTH_RIGHT);
    }

  if (event->window == priv->arrow_win[ARROW_YEAR_LEFT])
    {
      priv->arrow_state[ARROW_YEAR_LEFT] = GTK_STATE_NORMAL;
      calendar_invalidate_arrow (calendar, ARROW_YEAR_LEFT);
    }

  if (event->window == priv->arrow_win[ARROW_YEAR_RIGHT])
    {
      priv->arrow_state[ARROW_YEAR_RIGHT] = GTK_STATE_NORMAL;
      calendar_invalidate_arrow (calendar, ARROW_YEAR_RIGHT);
    }

  return TRUE;
}

static gboolean
gnc_calendar_scroll (GtkWidget *widget,
                     GdkEventScroll *event)
{
  GncCalendar *calendar = GNC_CALENDAR (widget);

  if (event->direction == GDK_SCROLL_UP)
    {
      if (!GTK_WIDGET_HAS_FOCUS (widget))
        gtk_widget_grab_focus (widget);
      calendar_set_month_prev (calendar);
    }
  else if (event->direction == GDK_SCROLL_DOWN)
    {
      if (!GTK_WIDGET_HAS_FOCUS (widget))
        gtk_widget_grab_focus (widget);
      calendar_set_month_next (calendar);
    }
  else
    return FALSE;

  return TRUE;
}

/****************************************
 *             Key handling              *
 ****************************************/

static void
move_focus (GncCalendar *calendar,
            gint direction)
{
  GtkTextDirection text_dir = gtk_widget_get_direction (GTK_WIDGET (calendar));

  if ((text_dir == GTK_TEXT_DIR_LTR && direction == -1) ||
      (text_dir == GTK_TEXT_DIR_RTL && direction == 1))
    {
      if (calendar->focus_col > 0)
        calendar->focus_col--;
      else if (calendar->focus_row > 0)
        {
          calendar->focus_col = 6;
          calendar->focus_row--;
        }

      if (calendar->focus_col < 0)
        calendar->focus_col = 6;
      if (calendar->focus_row < 0)
        calendar->focus_row = 5;
    }
  else
    {
      if (calendar->focus_col < 6)
        calendar->focus_col++;
      else if (calendar->focus_row < 5)
        {
          calendar->focus_col = 0;
          calendar->focus_row++;
        }

      if (calendar->focus_col < 0)
        calendar->focus_col = 0;
      if (calendar->focus_row < 0)
        calendar->focus_row = 0;
    }
}

static gboolean
gnc_calendar_key_press (GtkWidget *widget,
                        GdkEventKey *event)
{
  GncCalendar *calendar;
  gint return_val;
  gint old_focus_row;
  gint old_focus_col;
  gint row, col, day;

  calendar = GNC_CALENDAR (widget);
  return_val = FALSE;

  old_focus_row = calendar->focus_row;
  old_focus_col = calendar->focus_col;

  switch (event->keyval)
    {
      case GDK_KP_Left:
      case GDK_Left:
        return_val = TRUE;
      if (event->state & GDK_CONTROL_MASK)
        calendar_set_month_prev (calendar);
      else
        {
          move_focus (calendar, -1);
          calendar_invalidate_day (calendar, old_focus_row, old_focus_col);
          calendar_invalidate_day (calendar, calendar->focus_row,
                                   calendar->focus_col);
        }
      break;
      case GDK_KP_Right:
      case GDK_Right:
        return_val = TRUE;
      if (event->state & GDK_CONTROL_MASK)
        calendar_set_month_next (calendar);
      else
        {
          move_focus (calendar, 1);
          calendar_invalidate_day (calendar, old_focus_row, old_focus_col);
          calendar_invalidate_day (calendar, calendar->focus_row,
                                   calendar->focus_col);
        }
      break;
      case GDK_KP_Up:
      case GDK_Up:
        return_val = TRUE;
      if (event->state & GDK_CONTROL_MASK)
        calendar_set_year_prev (calendar);
      else
        {
          if (calendar->focus_row > 0)
            calendar->focus_row--;
          if (calendar->focus_row < 0)
            calendar->focus_row = 5;
          if (calendar->focus_col < 0)
            calendar->focus_col = 6;
          calendar_invalidate_day (calendar, old_focus_row, old_focus_col);
          calendar_invalidate_day (calendar, calendar->focus_row,
                                   calendar->focus_col);
        }
      break;
      case GDK_KP_Down:
      case GDK_Down:
        return_val = TRUE;
      if (event->state & GDK_CONTROL_MASK)
        calendar_set_year_next (calendar);
      else
        {
          if (calendar->focus_row < 5)
            calendar->focus_row++;
          if (calendar->focus_col < 0)
            calendar->focus_col = 0;
          calendar_invalidate_day (calendar, old_focus_row, old_focus_col);
          calendar_invalidate_day (calendar, calendar->focus_row,
                                   calendar->focus_col);
        }
      break;
      case GDK_KP_Space:
      case GDK_space:
        row = calendar->focus_row;
      col = calendar->focus_col;

      if (row > -1 && col > -1)
        {
          return_val = TRUE;

          day = calendar->day[row][col];
          if (calendar->day_month[row][col] == MONTH_PREV)
            calendar_set_month_prev (calendar);
          else if (calendar->day_month[row][col] == MONTH_NEXT)
            calendar_set_month_next (calendar);

          calendar_select_and_focus_day (calendar, day);
        }
    }

  return return_val;
}

/****************************************
 *           Misc widget methods        *
 ****************************************/

static void
calendar_set_background (GtkWidget *widget)
{
  GncCalendarPrivate *priv = GNC_CALENDAR_GET_PRIVATE (widget);
  gint i;

  if (GTK_WIDGET_REALIZED (widget))
    {
      for (i = 0; i < 4; i++)
        {
          if (priv->arrow_win[i])
            gdk_window_set_background (priv->arrow_win[i],
                                       HEADER_BG_COLOR (widget));
        }
      if (priv->header_win)
        gdk_window_set_background (priv->header_win,
                                   HEADER_BG_COLOR (widget));
      if (priv->day_name_win)
        gdk_window_set_background (priv->day_name_win,
                                   BACKGROUND_COLOR (widget));
      if (priv->week_win)
        gdk_window_set_background (priv->week_win,
                                   BACKGROUND_COLOR (widget));
      if (priv->main_win)
        gdk_window_set_background (priv->main_win,
                                   BACKGROUND_COLOR (widget));
      if (widget->window)
        gdk_window_set_background (widget->window,
                                   BACKGROUND_COLOR (widget));
    }
}

static void
gnc_calendar_style_set (GtkWidget *widget,
                        GtkStyle *previous_style)
{
  if (previous_style && GTK_WIDGET_REALIZED (widget))
    calendar_set_background (widget);
}

static void
gnc_calendar_state_changed (GtkWidget *widget,
                            GtkStateType previous_state)
{
  GncCalendar *calendar = GNC_CALENDAR (widget);
  GncCalendarPrivate *priv = GNC_CALENDAR_GET_PRIVATE (widget);
  int i;

  if (!GTK_WIDGET_IS_SENSITIVE (widget))
    {
      priv->in_drag = 0;
      calendar_stop_spinning (calendar);
    }

  for (i = 0; i < 4; i++)
    if (GTK_WIDGET_IS_SENSITIVE (widget))
      priv->arrow_state[i] = GTK_STATE_NORMAL;
    else
      priv->arrow_state[i] = GTK_STATE_INSENSITIVE;

  calendar_set_background (widget);
}

static void
gnc_calendar_grab_notify (GtkWidget *widget,
                          gboolean was_grabbed)
{
  if (!was_grabbed)
    calendar_stop_spinning (GNC_CALENDAR (widget));
}

static gboolean
gnc_calendar_focus_out (GtkWidget *widget,
                        GdkEventFocus *event)
{
  GncCalendarPrivate *priv = GNC_CALENDAR_GET_PRIVATE (widget);
  GncCalendar *calendar = GNC_CALENDAR (widget);

  calendar_queue_refresh (calendar);
  calendar_stop_spinning (calendar);

  priv->in_drag = 0;

  return FALSE;
}

/****************************************
 *          Drag and Drop               *
 ****************************************/

static void
gnc_calendar_drag_data_get (GtkWidget *widget,
                            GdkDragContext *context,
                            GtkSelectionData *selection_data,
                            guint info,
                            guint time)
{
  GncCalendar *calendar = GNC_CALENDAR (widget);
  GDate *date;
  gchar str[128];
  gsize len;

  date = g_date_new_dmy (calendar->selected_day, calendar->month + 1, calendar->year);
  len = g_date_strftime (str, 127, "%x", date);
  gtk_selection_data_set_text (selection_data, str, len);

  g_free (date);
}

/* Get/set whether drag_motion requested the drag data and
 * drag_data_received should thus not actually insert the data,
 * since the data doesn't result from a drop.
 */
static void
set_status_pending (GdkDragContext *context,
                    GdkDragAction suggested_action)
{
  g_object_set_data (G_OBJECT (context),
                     I_("gtk-calendar-status-pending"),
                     GINT_TO_POINTER (suggested_action));
}

static GdkDragAction
get_status_pending (GdkDragContext *context)
{
  return GPOINTER_TO_INT (g_object_get_data (G_OBJECT (context),
                                             "gtk-calendar-status-pending"));
}

static void
gnc_calendar_drag_leave (GtkWidget *widget,
                         GdkDragContext *context,
                         guint time)
{
  GncCalendarPrivate *priv = GNC_CALENDAR_GET_PRIVATE (widget);

  priv->drag_highlight = 0;
  gtk_drag_unhighlight (widget);

}

static gboolean
gnc_calendar_drag_motion (GtkWidget *widget,
                          GdkDragContext *context,
                          gint x,
                          gint y,
                          guint time)
{
  GncCalendarPrivate *priv = GNC_CALENDAR_GET_PRIVATE (widget);
  GdkAtom target;

  if (!priv->drag_highlight)
    {
      priv->drag_highlight = 1;
      gtk_drag_highlight (widget);
    }

  target = gtk_drag_dest_find_target (widget, context, NULL);
  if (target == GDK_NONE || context->suggested_action == 0)
    gdk_drag_status (context, 0, time);
  else
    {
      set_status_pending (context, context->suggested_action);
      gtk_drag_get_data (widget, context, target, time);
    }

  return TRUE;
}

static gboolean
gnc_calendar_drag_drop (GtkWidget *widget,
                        GdkDragContext *context,
                        gint x,
                        gint y,
                        guint time)
{
  GdkAtom target;

  target = gtk_drag_dest_find_target (widget, context, NULL);
  if (target != GDK_NONE)
    {
      gtk_drag_get_data (widget, context,
                         target,
                         time);
      return TRUE;
    }

  return FALSE;
}

static void
gnc_calendar_drag_data_received (GtkWidget *widget,
                                 GdkDragContext *context,
                                 gint x,
                                 gint y,
                                 GtkSelectionData *selection_data,
                                 guint info,
                                 guint time)
{
  GncCalendar *calendar = GNC_CALENDAR (widget);
  guint day, month, year;
  gchar *str;
  GDate *date;
  GdkDragAction suggested_action;

  suggested_action = get_status_pending (context);

  if (suggested_action)
    {
      set_status_pending (context, 0);

      /* We are getting this data due to a request in drag_motion,
         * rather than due to a request in drag_drop, so we are just
         * supposed to call drag_status, not actually paste in the
         * data.
         */
      str = (gchar *) gtk_selection_data_get_text (selection_data);

      if (str)
        {
          date = g_date_new ();
          g_date_set_parse (date, str);
          if (!g_date_valid (date))
            suggested_action = 0;
          g_date_free (date);
          g_free (str);
        }
      else
        suggested_action = 0;

      gdk_drag_status (context, suggested_action, time);

      return;
    }

  date = g_date_new ();
  str = (gchar *) gtk_selection_data_get_text (selection_data);
  if (str)
    {
      g_date_set_parse (date, str);
      g_free (str);
    }

  if (!g_date_valid (date))
    {
      g_warning ("Received invalid date data\n");
      g_date_free (date);
      gtk_drag_finish (context, FALSE, FALSE, time);
      return;
    }

  day = g_date_get_day (date);
  month = g_date_get_month (date);
  year = g_date_get_year (date);
  g_date_free (date);

  gtk_drag_finish (context, TRUE, FALSE, time);

  g_object_freeze_notify (G_OBJECT (calendar));
  if (!(calendar->display_flags & GNC_CALENDAR_NO_MONTH_CHANGE)
      && (calendar->display_flags & GNC_CALENDAR_SHOW_HEADING))
    gnc_calendar_select_month (calendar, month - 1, year);
  gnc_calendar_select_day (calendar, day);
  g_object_thaw_notify (G_OBJECT (calendar));
}


/****************************************
 *              Public API              *
 ****************************************/

/**
 * gtk_calendar_new:
 *
 * Creates a new calendar, with the current date being selected.
 *
 * Return value: a newly #GncCalendar widget
 **/
GtkWidget *
gnc_calendar_new (void)
{
  return g_object_new (GNC_TYPE_CALENDAR, NULL);
}

/**
 * gtk_calendar_display_options:
 * @calendar: a #GncCalendar.
 * @flags: the display options to set.
 *
 * Sets display options (whether to display the heading and the month headings).
 *
 * Deprecated: 2.4: Use gtk_calendar_set_display_options() instead
 **/
void
gnc_calendar_display_options (GncCalendar *calendar,
                              GncCalendarDisplayOptions flags)
{
  gnc_calendar_set_display_options (calendar, flags);
}

/**
 * gtk_calendar_get_display_options:
 * @calendar: a #GncCalendar
 *
 * Returns the current display options of @calendar.
 *
 * Return value: the display options.
 *
 * Since: 2.4
 **/
GncCalendarDisplayOptions
gnc_calendar_get_display_options (GncCalendar *calendar)
{
  g_return_val_if_fail (GNC_IS_CALENDAR (calendar), 0);

  return calendar->display_flags;
}

/**
 * gtk_calendar_set_display_options:
 * @calendar: a #GncCalendar
 * @flags: the display options to set
 *
 * Sets display options (whether to display the heading and the month
 * headings).
 *
 * Since: 2.4
 **/
void
gnc_calendar_set_display_options (GncCalendar *calendar,
                                  GncCalendarDisplayOptions flags)
{
  GtkWidget *widget = GTK_WIDGET (calendar);
  GncCalendarPrivate *priv = GNC_CALENDAR_GET_PRIVATE (calendar);
  gint resize = 0;
  gint i;
  GncCalendarDisplayOptions old_flags;

  g_return_if_fail (GNC_IS_CALENDAR (calendar));

  old_flags = calendar->display_flags;

  if (GTK_WIDGET_REALIZED (widget))
    {
      if ((flags ^ calendar->display_flags) & GNC_CALENDAR_NO_MONTH_CHANGE)
        {
          resize++;
          if (!(flags & GNC_CALENDAR_NO_MONTH_CHANGE)
              && (priv->header_win))
            {
              calendar->display_flags &= ~GNC_CALENDAR_NO_MONTH_CHANGE;
              calendar_realize_arrows (calendar);
            }
          else
            {
              for (i = 0; i < 4; i++)
                {
                  if (priv->arrow_win[i])
                    {
                      gdk_window_set_user_data (priv->arrow_win[i],
                                                NULL);
                      gdk_window_destroy (priv->arrow_win[i]);
                      priv->arrow_win[i] = NULL;
                    }
                }
            }
        }

      if ((flags ^ calendar->display_flags) & GNC_CALENDAR_SHOW_HEADING)
        {
          resize++;

          if (flags & GNC_CALENDAR_SHOW_HEADING)
            {
              calendar->display_flags |= GNC_CALENDAR_SHOW_HEADING;
              calendar_realize_header (calendar);
            }
          else
            {
              for (i = 0; i < 4; i++)
                {
                  if (priv->arrow_win[i])
                    {
                      gdk_window_set_user_data (priv->arrow_win[i],
                                                NULL);
                      gdk_window_destroy (priv->arrow_win[i]);
                      priv->arrow_win[i] = NULL;
                    }
                }
              gdk_window_set_user_data (priv->header_win, NULL);
              gdk_window_destroy (priv->header_win);
              priv->header_win = NULL;
            }
        }

      if ((flags ^ calendar->display_flags) & GNC_CALENDAR_SHOW_DAY_NAMES)
        {
          resize++;

          if (flags & GNC_CALENDAR_SHOW_DAY_NAMES)
            {
              calendar->display_flags |= GNC_CALENDAR_SHOW_DAY_NAMES;
              calendar_realize_day_names (calendar);
            }
          else
            {
              gdk_window_set_user_data (priv->day_name_win, NULL);
              gdk_window_destroy (priv->day_name_win);
              priv->day_name_win = NULL;
            }
        }

      if ((flags ^ calendar->display_flags) & GNC_CALENDAR_SHOW_WEEK_NUMBERS)
        {
          resize++;

          if (flags & GNC_CALENDAR_SHOW_WEEK_NUMBERS)
            {
              calendar->display_flags |= GNC_CALENDAR_SHOW_WEEK_NUMBERS;
              calendar_realize_week_numbers (calendar);
            }
          else
            {
              gdk_window_set_user_data (priv->week_win, NULL);
              gdk_window_destroy (priv->week_win);
              priv->week_win = NULL;
            }
        }

      if ((flags ^ calendar->display_flags) & GNC_CALENDAR_WEEK_START_MONDAY)
        g_warning (
            "GNC_CALENDAR_WEEK_START_MONDAY is ignored; the first day of the week is determined from the locale");

      if ((flags ^ calendar->display_flags) & GNC_CALENDAR_SHOW_DETAILS)
        resize++;

      calendar->display_flags = flags;
      if (resize)
        gtk_widget_queue_resize (GTK_WIDGET (calendar));

    }
  else
    calendar->display_flags = flags;

  g_object_freeze_notify (G_OBJECT (calendar));
  if ((old_flags ^ calendar->display_flags) & GNC_CALENDAR_SHOW_HEADING)
    g_object_notify (G_OBJECT (calendar), "show-heading");
  if ((old_flags ^ calendar->display_flags) & GNC_CALENDAR_SHOW_DAY_NAMES)
    g_object_notify (G_OBJECT (calendar), "show-day-names");
  if ((old_flags ^ calendar->display_flags) & GNC_CALENDAR_NO_MONTH_CHANGE)
    g_object_notify (G_OBJECT (calendar), "no-month-change");
  if ((old_flags ^ calendar->display_flags) & GNC_CALENDAR_SHOW_WEEK_NUMBERS)
    g_object_notify (G_OBJECT (calendar), "show-week-numbers");
  g_object_thaw_notify (G_OBJECT (calendar));
}

/**
 * gtk_calendar_select_month:
 * @calendar: a #GncCalendar
 * @month: a month number between 0 and 11.
 * @year: the year the month is in.
 *
 * Shifts the calendar to a different month.
 *
 * Return value: %TRUE, always
 **/
gboolean
gnc_calendar_select_month (GncCalendar *calendar,
                           guint month,
                           guint year)
{
  g_return_val_if_fail (GNC_IS_CALENDAR (calendar), FALSE);
  g_return_val_if_fail (month <= 11, FALSE);

  calendar->month = month;
  calendar->year = year;

  calendar_compute_days (calendar);
  calendar_queue_refresh (calendar);

  g_object_freeze_notify (G_OBJECT (calendar));
  g_object_notify (G_OBJECT (calendar), "month");
  g_object_notify (G_OBJECT (calendar), "year");
  g_object_thaw_notify (G_OBJECT (calendar));

  g_signal_emit (calendar,
                 gnc_calendar_signals[MONTH_CHANGED_SIGNAL],
                 0);
  return TRUE;
}

/**
 * gtk_calendar_select_day:
 * @calendar: a #GncCalendar.
 * @day: the day number between 1 and 31, or 0 to unselect
 *   the currently selected day.
 *
 * Selects a day from the current month.
 **/
void
gnc_calendar_select_day (GncCalendar *calendar,
                         guint day)
{
  g_return_if_fail (GNC_IS_CALENDAR (calendar));
  g_return_if_fail (day <= 31);

/* Deselect the old day */
  if (calendar->selected_day > 0)
    {
      gint selected_day;
      selected_day = calendar->selected_day;
      calendar->selected_day = 0;
      if (GTK_WIDGET_DRAWABLE (GTK_WIDGET (calendar)))
        calendar_invalidate_day_num (calendar, selected_day);
    }

  calendar->selected_day = day;

/* Select the new day */
  if (day != 0)
    {
      if (GTK_WIDGET_DRAWABLE (GTK_WIDGET (calendar)))
        calendar_invalidate_day_num (calendar, day);
    }

  g_object_notify (G_OBJECT (calendar), "day");

  g_signal_emit (calendar,
                 gnc_calendar_signals[DAY_SELECTED_SIGNAL],
                 0);
}

/**
 * gtk_calendar_clear_marks:
 * @calendar: a #GncCalendar
 *
 * Remove all visual markers.
 **/
void
gnc_calendar_clear_marks (GncCalendar *calendar)
{
  guint day;

  g_return_if_fail (GNC_IS_CALENDAR (calendar));

  for (day = 0; day < 31; day++)
    {
      calendar->marked_date[day] = FALSE;
    }

  calendar->num_marked_dates = 0;
  calendar_queue_refresh (calendar);
}

/**
 * gtk_calendar_mark_day:
 * @calendar: a #GncCalendar
 * @day: the day number to mark between 1 and 31.
 *
 * Places a visual marker on a particular day.
 *
 * Return value: %TRUE, always
 **/
gboolean
gnc_calendar_mark_day (GncCalendar *calendar,
                       guint day)
{
  g_return_val_if_fail (GNC_IS_CALENDAR (calendar), FALSE);

  if (day >= 1 && day <= 31 && calendar->marked_date[day - 1] == FALSE)
    {
      calendar->marked_date[day - 1] = TRUE;
      calendar->num_marked_dates++;
      calendar_invalidate_day_num (calendar, day);
    }

  return TRUE;
}

/**
 * gtk_calendar_unmark_day:
 * @calendar: a #GncCalendar.
 * @day: the day number to unmark between 1 and 31.
 *
 * Removes the visual marker from a particular day.
 *
 * Return value: %TRUE, always
 **/
gboolean
gnc_calendar_unmark_day (GncCalendar *calendar,
                         guint day)
{
  g_return_val_if_fail (GNC_IS_CALENDAR (calendar), FALSE);

  if (day >= 1 && day <= 31 && calendar->marked_date[day - 1] == TRUE)
    {
      calendar->marked_date[day - 1] = FALSE;
      calendar->num_marked_dates--;
      calendar_invalidate_day_num (calendar, day);
    }

  return TRUE;
}

/**
 * gtk_calendar_get_date:
 * @calendar: a #GncCalendar
 * @year: location to store the year number, or %NULL
 * @month: location to store the month number (between 0 and 11), or %NULL
 * @day: location to store the day number (between 1 and 31), or %NULL
 *
 * Obtains the selected date from a #GncCalendar.
 **/
void
gnc_calendar_get_date (GncCalendar *calendar,
                       guint *year,
                       guint *month,
                       guint *day)
{
  g_return_if_fail (GNC_IS_CALENDAR (calendar));

  if (year)
    *year = calendar->year;

  if (month)
    *month = calendar->month;

  if (day)
    *day = calendar->selected_day;
}

/**
 * gtk_calendar_set_detail_func:
 * @calendar: a #GncCalendar.
 * @func: a function providing details for each day.
 * @data: data to pass to @func invokations.
 * @destroy: a function for releasing @data.
 *
 * Installs a function which provides Pango markup with detail information
 * for each day. Examples for such details are holidays or appointments. That
 * information is shown below each day when #GncCalendar:show-details is set.
 * A tooltip containing with full detail information is provided, if the entire
 * text should not fit into the details area, or if #GncCalendar:show-details
 * is not set.
 *
 * The size of the details area can be restricted by setting the
 * #GncCalendar:detail-width-chars and #GncCalendar:detail-height-rows
 * properties.
 *
 * Since: 2.14
 */
void
gnc_calendar_set_detail_func (GncCalendar *calendar,
                              GncCalendarDetailFunc func,
                              gpointer data,
                              GDestroyNotify destroy)
{
  GncCalendarPrivate *priv;

  g_return_if_fail (GNC_IS_CALENDAR (calendar));

  priv = GNC_CALENDAR_GET_PRIVATE (calendar);

  if (priv->detail_func_destroy)
    priv->detail_func_destroy (priv->detail_func_user_data);

  priv->detail_func = func;
  priv->detail_func_user_data = data;
  priv->detail_func_destroy = destroy;

  gtk_widget_set_has_tooltip (GTK_WIDGET (calendar),
                              NULL != priv->detail_func);
  gtk_widget_queue_resize (GTK_WIDGET (calendar));
}

/**
 * gtk_calendar_set_detail_width_chars:
 * @calendar: a #GncCalendar.
 * @chars: detail width in characters.
 *
 * Updates the width of detail cells.
 * See #GncCalendar:detail-width-chars.
 *
 * Since: 2.14
 */
void
gnc_calendar_set_detail_width_chars (GncCalendar *calendar,
                                     gint chars)
{
  GncCalendarPrivate *priv;

  g_return_if_fail (GNC_IS_CALENDAR (calendar));

  priv = GNC_CALENDAR_GET_PRIVATE (calendar);

  if (chars != priv->detail_width_chars)
    {
      priv->detail_width_chars = chars;
      g_object_notify (G_OBJECT (calendar), "detail-width-chars");
      gtk_widget_queue_resize_no_redraw (GTK_WIDGET (calendar));
    }
}

/**
 * gtk_calendar_set_detail_height_rows:
 * @calendar: a #GncCalendar.
 * @rows: detail height in rows.
 *
 * Updates the height of detail cells.
 * See #GncCalendar:detail-height-rows.
 *
 * Since: 2.14
 */
void
gnc_calendar_set_detail_height_rows (GncCalendar *calendar,
                                     gint rows)
{
  GncCalendarPrivate *priv;

  g_return_if_fail (GNC_IS_CALENDAR (calendar));

  priv = GNC_CALENDAR_GET_PRIVATE (calendar);

  if (rows != priv->detail_height_rows)
    {
      priv->detail_height_rows = rows;
      g_object_notify (G_OBJECT (calendar), "detail-height-rows");
      gtk_widget_queue_resize_no_redraw (GTK_WIDGET (calendar));
    }
}

/**
 * gtk_calendar_get_detail_width_chars:
 * @calendar: a #GncCalendar.
 *
 * Queries the width of detail cells, in characters.
 * See #GncCalendar:detail-width-chars.
 *
 * Since: 2.14
 *
 * Return value: The width of detail cells, in characters.
 */
gint
gnc_calendar_get_detail_width_chars (GncCalendar *calendar)
{
  g_return_val_if_fail (GNC_IS_CALENDAR (calendar), 0);
  return GNC_CALENDAR_GET_PRIVATE (calendar)->detail_width_chars;
}

/**
 * gtk_calendar_get_detail_height_rows:
 * @calendar: a #GncCalendar.
 *
 * Queries the height of detail cells, in rows.
 * See #GncCalendar:detail-width-chars.
 *
 * Since: 2.14
 *
 * Return value: The height of detail cells, in rows.
 */
gint
gnc_calendar_get_detail_height_rows (GncCalendar *calendar)
{
  g_return_val_if_fail (GNC_IS_CALENDAR (calendar), 0);
  return GNC_CALENDAR_GET_PRIVATE (calendar)->detail_height_rows;
}

/**
 * gtk_calendar_freeze:
 * @calendar: a #GncCalendar
 *
 * Does nothing. Previously locked the display of the calendar until
 * it was thawed with gtk_calendar_thaw().
 *
 * Deprecated: 2.8:
 **/
void
gnc_calendar_freeze (GncCalendar *calendar)
{
  g_return_if_fail (GNC_IS_CALENDAR (calendar));
}

/**
 * gtk_calendar_thaw:
 * @calendar: a #GncCalendar
 *
 * Does nothing. Previously defrosted a calendar; all the changes made
 * since the last gtk_calendar_freeze() were displayed.
 *
 * Deprecated: 2.8:
 **/
void
gnc_calendar_thaw (GncCalendar *calendar)
{
  g_return_if_fail (GNC_IS_CALENDAR (calendar));
}

static
void
gnc_update_calendar_lib (GncCalendar *calendar)
{
  // we can just update lib if user change it
  if (calendar && (calendar->calendarType != calendar->calendarTypePrev) &&
      calendar->calendarType != GNC_UNDEFINED_CALENDAR_TYPE)
    {
      if (calendar->calendarFunction)
        {
          switch (calendar->calendarTypePrev)
            {
              case GNC_CALENDAR_TYPE_GREGORIAN :
                gregorian_calendar_destroy (calendar->calendarFunction);
              break;
              case GNC_CALENDAR_TYPE_JALALI :
                jalali_calendar_destroy (calendar->calendarFunction);
              break;
              default:
                // todo LOG IT
                break;
            }

          calendar->calendarFunction = NULL;
          calendar->calendarTypePrev = calendar->calendarType;

        }

      switch (calendar->calendarType)
        {
          case GNC_CALENDAR_TYPE_GREGORIAN:
            calendar->calendarFunction = gregorian_calendar_inti ();
          break;
          case GNC_CALENDAR_TYPE_JALALI:
            calendar->calendarFunction = jalali_calendar_inti ();
          break;
          default:
            // todo LOG IT
            break;
        }

    }
}

void
gnc_calendar_set_calendar_type (GncCalendar *calendar, const GncCalendarType calendar_type)
{

  if (calendar && (calendar_type != GNC_UNDEFINED_CALENDAR_TYPE))
    {
      calendar->calendarTypePrev = calendar->calendarType;
      calendar->calendarType = calendar_type;
      gnc_update_calendar_lib (calendar);
      calendar->calendarFunction->init_day_name (default_abbreviated_dayname);
      calendar->calendarFunction->init_month_name (default_monthname);
    }

  // re initialize month name and day name




}
GncCalendarType
gnc_calendar_get_calendar_type (GncCalendar *calendar)
{
  if (calendar && calendar->calendarType)
    {
      return calendar->calendarType;

    }
  return GNC_UNDEFINED_CALENDAR_TYPE;

}

#define __GNUCASH_CALENDAR_C__
//#include "gtkaliasdef.c"