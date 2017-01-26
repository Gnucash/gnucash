/********************************************************************\
 * gnucash-calendar.h -- A clone of GtkCalendar that optimized for  *
 * 								multi Calendar logic support        *
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


#ifndef __GNUCASH_CALENDAR_H__
#define __GNUCASH_CALENDAR_H__


#include <gtk/gtkwidget.h>


/* Not needed, retained for compatibility -Yosh */
#include <gtk/gtksignal.h>

#ifndef __GTKINTL_H__
#define __GTKINTL_H__

#include "config.h"

#include <glib/gi18n-lib.h>
#include <libqof/qof/qof.h>

#ifdef ENABLE_NLS
#define P_(String) g_dgettext(GETTEXT_PACKAGE "-properties",String)
#else
#define P_(String) (String)
#endif

/* not really I18N-related, but also a string marker macro */
#define I_(string) g_intern_static_string (string)

#endif


#include "calendar-gregorian-lib.h"
#include "calendar-jalali-lib.h"



#define GNC_TYPE_CALENDAR                  (gnc_calendar_get_type ())
#define GNC_CALENDAR(obj)                  (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_CALENDAR, GncCalendar))
#define GNC_CALENDAR_CLASS(klass)          (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_CALENDAR, GncCalendarClass))
#define GNC_IS_CALENDAR(obj)               (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_CALENDAR))
#define GNC_IS_CALENDAR_CLASS(klass)       (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_CALENDAR))
#define GNC_CALENDAR_GET_CLASS(obj)        (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_CALENDAR, GncCalendarClass))


typedef struct _GncCalendar	       GncCalendar;
typedef struct _GncCalendarClass       GncCalendarClass;

typedef struct _GncCalendarPrivate     GncCalendarPrivate;

/**
 * GtkCalendarDisplayOptions:
 * @GTK_CALENDAR_SHOW_HEADING: Specifies that the month and year should be displayed.
 * @GTK_CALENDAR_SHOW_DAY_NAMES: Specifies that three letter day descriptions should be present.
 * @GTK_CALENDAR_NO_MONTH_CHANGE: Prevents the user from switching months with the calendar.
 * @GTK_CALENDAR_SHOW_WEEK_NUMBERS: Displays each week numbers of the current year, down the
 * left side of the calendar.
 * @GTK_CALENDAR_WEEK_START_MONDAY: Since GTK+ 2.4, this option is deprecated and ignored by GTK+.
 * The information on which day the calendar week starts is derived from the locale.
 * @GTK_CALENDAR_SHOW_DETAILS: Just show an indicator, not the full details
 * text when details are provided. See gtk_calendar_set_detail_func().
 *
 * These options can be used to influence the display and behaviour of a #GtkCalendar.
 */
typedef enum
{
  GNC_CALENDAR_SHOW_HEADING		= 1 << 0,
  GNC_CALENDAR_SHOW_DAY_NAMES		= 1 << 1,
  GNC_CALENDAR_NO_MONTH_CHANGE		= 1 << 2,
  GNC_CALENDAR_SHOW_WEEK_NUMBERS	= 1 << 3,
  GNC_CALENDAR_WEEK_START_MONDAY	= 1 << 4,
  GNC_CALENDAR_SHOW_DETAILS		= 1 << 5
} GncCalendarDisplayOptions;


/*typedef enum
{
    GREGORIAN_CALENDAR,
    JALALIAN_CALENDAR,
    UNDEFINED_CALENDAR
}GncashCalendarType;*/
/**
 * GtkCalendarDetailFunc:
 * @calendar: a #GtkCalendar.
 * @year: the year for which details are needed.
 * @month: the month for which details are needed.
 * @day: the day of @month for which details are needed.
 * @user_data: the data passed with gtk_calendar_set_detail_func().
 *
 * This kind of functions provide Pango markup with detail information for the
 * specified day. Examples for such details are holidays or appointments. The
 * function returns %NULL when no information is available.
 *
 * Since: 2.14
 *
 * Return value: Newly allocated string with Pango markup with details
 * for the specified day, or %NULL.
 */
typedef gchar* (*GncCalendarDetailFunc) (GncCalendar *calendar,
                                         guint        year,
                                         guint        month,
                                         guint        day,
                                         gpointer     user_data);



struct _GncCalendar
{
  GtkWidget widget;

  GtkStyle  *GSEAL (header_style);
  GtkStyle  *GSEAL (label_style);

  gint GSEAL (month);
  gint GSEAL (year);
  gint GSEAL (selected_day);

  gint GSEAL (day_month[6][7]);
  gint GSEAL (day[6][7]);

  gint GSEAL (num_marked_dates);
  gint GSEAL (marked_date[31]);
  GncCalendarDisplayOptions  GSEAL (display_flags);
  GdkColor GSEAL (marked_date_color[31]);

  GdkGC *GSEAL (gc);			/* unused */
  GdkGC *GSEAL (xor_gc);		/* unused */

  gint GSEAL (focus_row);
  gint GSEAL (focus_col);

  gint GSEAL (highlight_row);
  gint GSEAL (highlight_col);

  GncCalendarPrivate *GSEAL (priv);
  gchar GSEAL (grow_space [32]);

  CalendarLib * calendarFunction;

  GncCalendarType calendarType;
  GncCalendarType calendarTypePrev;
  GncCalendarType qof_calendar_type_get;

  /* Padding for future expansion */
  void (*_gtk_reserved1) (void);
  void (*_gtk_reserved2) (void);
  void (*_gtk_reserved3) (void);
  void (*_gtk_reserved4) (void);
};

struct _GncCalendarClass
{
  GtkWidgetClass parent_class;

  /* Signal handlers */
  void (* month_changed)		(GncCalendar *calendar);
  void (* day_selected)			(GncCalendar *calendar);
  void (* day_selected_double_click)	(GncCalendar *calendar);
  void (* prev_month)			(GncCalendar *calendar);
  void (* next_month)			(GncCalendar *calendar);
  void (* prev_year)			(GncCalendar *calendar);
  void (* next_year)			(GncCalendar *calendar);

};


GType	   gnc_calendar_get_type	(void) G_GNUC_CONST;
GtkWidget* gnc_calendar_new		(void );

gboolean   gnc_calendar_select_month	(GncCalendar *calendar,
					 guint	      month,
					 guint	      year);
void	   gnc_calendar_select_day	(GncCalendar *calendar,
					 guint	      day);

gboolean   gnc_calendar_mark_day	(GncCalendar *calendar,
					 guint	      day);
gboolean   gnc_calendar_unmark_day	(GncCalendar *calendar,
					 guint	      day);
void	   gnc_calendar_clear_marks	(GncCalendar *calendar);


void	   gnc_calendar_set_display_options (GncCalendar    	      *calendar,
					     GncCalendarDisplayOptions flags);
GncCalendarDisplayOptions
           gnc_calendar_get_display_options (GncCalendar   	      *calendar);
#ifndef GTK_DISABLE_DEPRECATED
void	   gnc_calendar_display_options (GncCalendar		  *calendar,
					 GncCalendarDisplayOptions flags);
#endif

void	   gnc_calendar_get_date	(GncCalendar *calendar,
					 guint	     *year,
					 guint	     *month,
					 guint	     *day);

void       gnc_calendar_set_detail_func (GncCalendar           *calendar,
                                         GncCalendarDetailFunc  func,
                                         gpointer               data,
                                         GDestroyNotify         destroy);

void       gnc_calendar_set_detail_width_chars (GncCalendar    *calendar,
                                                gint            chars);
void       gnc_calendar_set_detail_height_rows (GncCalendar    *calendar,
                                                gint            rows);

gint       gnc_calendar_get_detail_width_chars (GncCalendar    *calendar);
gint       gnc_calendar_get_detail_height_rows (GncCalendar    *calendar);


static
void gnc_update_calendar_lib(GncCalendar *);

void        gnc_calendar_set_calendar_type(GncCalendar *,const GncCalendarType);
GncCalendarType        gnc_calendar_get_calendar_type(GncCalendar *);



#ifndef GTK_DISABLE_DEPRECATED
void	   gnc_calendar_freeze		(GncCalendar *calendar);
void	   gnc_calendar_thaw		(GncCalendar *calendar);
#endif

G_END_DECLS

#endif /* __GNUCASH_CALENDAR_H__ */