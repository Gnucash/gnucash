/********************************************************************\
 * druid-acct-period.c -- accouting period druid for GnuCash        *
 * Copyright (C) 2001 Gnumatic, Inc.                                *
 * Copyright (C) 2001 Dave Peticolas <dave@krondo.com>              *
 * Copyright (C) 2003 Linas Vepstas <linas@linas.org>               *
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
\********************************************************************/

#include "config.h"

#include <glib.h>
#include <gnome.h>
#include <libgnomeui/gnome-window-icon.h>

#include "FreqSpec.h"
#include "Query.h"
#include "Transaction.h"
#include "dialog-utils.h"
#include "druid-acct-period.h"
#include "druid-utils.h"
#include "gnc-component-manager.h"
#include "gnc-frequency.h"
#include "gnc-ui-util.h"
#include "messages.h"
#include "qofbook.h"


#define DRUID_ACCT_PERIOD_CM_CLASS "druid-acct-period"


/** structures *********************************************************/
typedef struct
{
  GtkWidget * window;
  GtkWidget * druid;
  GNCFrequency *period_menu;
  GtkLabel  * remarks;
  GtkLabel  * book_details;

  time_t earliest;
  char * earliest_str;
  GDate *closing_date;
  FreqSpec *period;

} AcctPeriodInfo;


/** implementations ****************************************************/
static void
ap_window_destroy_cb (GtkObject *object, gpointer data)
{
  AcctPeriodInfo *info = data;

  gnc_unregister_gui_component_by_data (DRUID_ACCT_PERIOD_CM_CLASS, info);

  // gnc_frequency_destory ??
  xaccFreqSpecFree (info->period);
  g_date_free (info->closing_date);
  g_free (info->earliest_str);
  g_free (info);
}

static void
ap_finish (GnomeDruidPage *druidpage,
                    GtkWidget *druid,
                    gpointer user_data)
{
  AcctPeriodInfo *info = user_data;

  gnc_suspend_gui_refresh ();
  printf ("finished with acct periods\n");

  gnc_resume_gui_refresh ();
  gnc_close_gui_component_by_data (DRUID_ACCT_PERIOD_CM_CLASS, info);
}

static void
ap_druid_cancel (GnomeDruid *druid, gpointer user_data)
{
  AcctPeriodInfo *info = user_data;
  gnc_close_gui_component_by_data (DRUID_ACCT_PERIOD_CM_CLASS, info);
}

/* =============================================================== */

static void
prepare_remarks (AcctPeriodInfo *info)
{
  int nperiods;
  GDate *period_begin, *period_end, *date_now;
  const char *remarks_text;
  char * str;

  remarks_text = 
    _("The earliest transaction date found in this book is %s.\n"
      "Based on the selection made above, this book will be split\n"
      "into %d books.  Click on 'Next' to start closing the\n"
      "earliest book.\n");

  /* Pull info from widget, push into freq spec */
  gnc_frequency_save_state (info->period_menu, info->period, NULL);

  /* Count the number of periods that would be generated */
  period_begin = g_date_new();
  g_date_set_time (period_begin, info->earliest);
  nperiods = 0;
  date_now = g_date_new();
  g_date_set_time (date_now, time(0));
  period_end = g_date_new();
  do 
  {
    GDate *tmp;

    xaccFreqSpecGetNextInstance (info->period, period_begin, period_end);
    nperiods ++;
printf ("duude np=%d end=%d/%d/%d\n", nperiods,
g_date_month(period_end),
g_date_day(period_end),
g_date_year(period_end));
    tmp = period_begin;
    period_begin = period_end;
    period_end = tmp;
  } 
  while (0 > g_date_compare(period_begin, date_now ));

  g_date_free (period_begin);
  g_date_free (period_end);
  g_date_free (date_now);

  
  /* Display the results */
  str = g_strdup_printf (remarks_text, info->earliest_str, nperiods);
  gtk_label_set_text (info->remarks, str);
  g_free (str);
}

/* =============================================================== */

static void
show_book_details (AcctPeriodInfo *info)
{
  const char *period_text;
  char *str;

  /* Pull info from widget, push into freq spec */
  gnc_frequency_save_state (info->period_menu, info->period, NULL);
  printf ("duude gwanna show period info=%p\n", info);

  period_text = 
    _("You have asked for a book to be created.  This book\n"
      "will contain all transactions between midnight %s\n"
      "and midnight %s (for a total of\n"
      "%d transactions spread over %d accounts).\n"
      "Click on 'Next' to create this book.\n"
      "Click on 'Back' to adjust the dates.\n");

  /* Display the results */
  str = g_strdup_printf (remarks_text, 
      "asdf", "asdf", 0, 0);

  gtk_label_set_text (info->book_details, str);
  g_free (str);
}

/* =============================================================== */

static void
ap_changed (GtkWidget *widget, gpointer user_data)
{
  AcctPeriodInfo *info = user_data;

  printf ("duude something changed info=%p\n", info);
  prepare_remarks (info);
}

/* =============================================================== */

static void
ap_show_period (GnomeDruidPage *druidpage,
                GtkWidget *druid,
                gpointer user_data)
{
  AcctPeriodInfo *info = user_data;

  printf ("duude gwanna show period info=%p\n", info);
  show_book_details (info);
}

/* =============================================================== */
/* Find the earliest date occuring in the book.  Do this by making
 * a query and sorting by date. Since the truncated sort returns
 * only the *last* search results, sort in decreasing order.
 */
static time_t
get_earliest_in_book (QofBook *book)
{
  QofQuery *q;
  GSList *p1, *p2;
  GList *res;
  time_t earliest;

  q = qof_query_create_for(GNC_ID_SPLIT);
  qof_query_set_max_results(q, 1);
  qof_query_set_book (q, book);

  /* Sort by transaction date */
  p1 = g_slist_prepend (NULL, TRANS_DATE_POSTED);
  p1 = g_slist_prepend (p1, SPLIT_TRANS);
  p2 = g_slist_prepend (NULL, QUERY_DEFAULT_SORT);
  qof_query_set_sort_order (q, p1, p2, NULL);

  /* Reverse the sort order */
  qof_query_set_sort_increasing (q, FALSE, FALSE, FALSE);

  /* Run the query, find the earliest transaction date */
  res = qof_query_run (q);
  earliest = xaccQueryGetEarliestDateFound (q);

  qof_query_destroy (q);
  return earliest;
}

/* =============================================================== */

static void
ap_druid_create (AcctPeriodInfo *info)
{
  GladeXML *xml;
  GtkWidget *w, *page;

  xml = gnc_glade_xml_new ("acctperiod.glade", "Acct Period Druid");

  info->window = glade_xml_get_widget (xml, "Acct Period Druid");

  info->druid = glade_xml_get_widget (xml, "acct_period_druid");

  /* Find the date of the earliest transaction in the book.
   * Add a year minus a day as the first guess for book closing,
   * and use that to set up the freq spec widget. */
  info->earliest = get_earliest_in_book (gnc_get_current_book());
  info->earliest_str = qof_print_date(info->earliest); 
printf ("duude the earliest is %ld %s\n", info->earliest, ctime (&info->earliest));
  info->closing_date = g_date_new();
  g_date_set_time (info->closing_date, info->earliest);
  g_date_add_years (info->closing_date, 1);
  g_date_subtract_days (info->closing_date, 1);

  info->period = xaccFreqSpecMalloc( gnc_get_current_book() );
  xaccFreqSpecSetMonthly (info->period, info->closing_date, 12);
  xaccFreqSpecSetUIType (info->period, UIFREQ_YEARLY);

  info->period_menu = GNC_FREQUENCY (
          gnc_frequency_new (info->period, info->closing_date));

  /* Change the text so that its more mainingful for this druid */
  gnc_frequency_set_frequency_label_text(info->period_menu, _("Period:"));
  gnc_frequency_set_startdate_label_text(info->period_menu, _("Closing Date:"));

  /* Reparent to the correct location */
  w = glade_xml_get_widget (xml, "period box");
  gtk_box_pack_start (GTK_BOX (w), GTK_WIDGET (info->period_menu),
         TRUE, TRUE, 0);

  info->remarks = 
        GTK_LABEL (glade_xml_get_widget (xml, "remarks label"));

  info->book_details = 
        GTK_LABEL (glade_xml_get_widget (xml, "book label"));

  prepare_remarks (info);

  /* generic finished/close/abort signals */
  gtk_signal_connect (GTK_OBJECT (info->window), "destroy",
                      GTK_SIGNAL_FUNC (ap_window_destroy_cb), info);

  gtk_signal_connect (GTK_OBJECT (info->druid), "cancel",
                      GTK_SIGNAL_FUNC (ap_druid_cancel), info);

  page = glade_xml_get_widget (xml, "details_page");
  gtk_signal_connect (GTK_OBJECT (page), "next",
                      GTK_SIGNAL_FUNC (ap_show_period), info);

  page = glade_xml_get_widget (xml, "finish_page");
  gtk_signal_connect (GTK_OBJECT (page), "finish",
                      GTK_SIGNAL_FUNC (ap_finish), info);

  /* User changes the accouting period or date signals */
  gtk_signal_connect (GTK_OBJECT (info->period_menu), "changed",
                      GTK_SIGNAL_FUNC (ap_changed), info);
}


static void
ap_close_handler (gpointer user_data)
{
  AcctPeriodInfo *info = user_data;

  gtk_widget_destroy (info->window);
}

/********************************************************************\
 * gnc_acct_period_dialog                                           *
 *   opens up a druid to configure accounting periods               *
 *                                                                  * 
 * Args:   none                                                     *
 * Return: nothing                                                  *
\********************************************************************/

void
gnc_acct_period_dialog (void)
{
  AcctPeriodInfo *info;
  gint component_id;

  info = g_new0 (AcctPeriodInfo, 1);

  ap_druid_create (info);

  component_id = gnc_register_gui_component (DRUID_ACCT_PERIOD_CM_CLASS,
                                             NULL, ap_close_handler,
                                             info);

  gnome_window_icon_set_from_default(GTK_WINDOW(info->window));
  gtk_widget_show_all (info->window);

  gnc_window_adjust_for_screen (GTK_WINDOW(info->window));
}
