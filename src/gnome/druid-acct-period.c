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
  // GnomeDateEdit *closing_date_edit;
  GtkLabel  * earliest_date;

  time_t earliest;
  GDate *start_date;
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
  g_free (info->start_date);
  g_free (info);
}

static void
ap_finish (GnomeDruidPage *druidpage,
                    gpointer arg1,
                    gpointer user_data)
{
  // AcctPeriodInfo *info = user_data;

  printf ("finished with acct periods\n");
}

static void
ap_druid_cancel (GnomeDruid *druid, gpointer user_data)
{
  AcctPeriodInfo *info = user_data;

  gnc_close_gui_component_by_data (DRUID_ACCT_PERIOD_CM_CLASS, info);
}

static void
ap_changed (GtkWidget *widget,
                    gpointer arg1,
                    gpointer user_data)
{
  // AcctPeriodInfo *info = user_data;
  time_t closing_date = 0;
  GtkMenuItem *item = NULL;

/*
  closing_date = gnome_date_edit_get_date (info->closing_date_edit);
*/
  
  // item = GTK_MENU_ITEM (gtk_menu_get_active (GTK_MENU(info->period_menu->menu)));
  printf ("something changed, time=%ld item=%p\n", closing_date, item);

  
}

/* =============================================================== */
/* Find the earliest date occuring in the book.  Do this by making
 * a query and sorting by date.
 */
static time_t
get_earliest_in_book (QofBook *book)
{
  QofQuery *q;
  GSList *p1, *p2;
  time_t earliest;

  q = qof_query_create_for(GNC_ID_SPLIT);
  qof_query_set_max_results(q, 5);

  p1 = g_slist_prepend (NULL, TRANS_DATE_POSTED);
  p1 = g_slist_prepend (p1, SPLIT_TRANS);
  p2 = g_slist_prepend (NULL, QUERY_DEFAULT_SORT);
  qof_query_set_sort_order (q, p1, p2, NULL);

  qof_query_set_sort_increasing (q, TRUE, TRUE, TRUE);

  earliest = xaccQueryGetEarliestDateFound (q);

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

  /* Set up the freq spec widget */

  info->earliest = get_earliest_in_book (gnc_get_current_book());
printf ("the earliest is %ld %s\n", info->earliest, ctime (&info->earliest));
  info->start_date = g_date_new_dmy(5,5,1988);

  info->period = xaccFreqSpecMalloc( gnc_get_current_book() );
  xaccFreqSpecSetMonthly (info->period, info->start_date, 12);
  xaccFreqSpecSetUIType (info->period, UIFREQ_YEARLY);

  info->period_menu = GNC_FREQUENCY (
          gnc_frequency_new (info->period, info->start_date));
  w = glade_xml_get_widget (xml, "period box");
  gtk_box_pack_start (GTK_BOX (w), GTK_WIDGET (info->period_menu),
         TRUE, TRUE, 0);
printf ("postbox\n");

  // info->closing_date_edit = 
  //      GNOME_DATE_EDIT (glade_xml_get_widget (xml, "closing date edit"));
  info->earliest_date = 
        GTK_LABEL (glade_xml_get_widget (xml, "earliest trans label"));

// "closing date box"

  /* generic finished/close/abort signals */
  gtk_signal_connect (GTK_OBJECT (info->window), "destroy",
                      GTK_SIGNAL_FUNC (ap_window_destroy_cb), info);

  gtk_signal_connect (GTK_OBJECT (info->druid), "cancel",
                      GTK_SIGNAL_FUNC (ap_druid_cancel), info);

  page = glade_xml_get_widget (xml, "finish_page");
  gtk_signal_connect (GTK_OBJECT (page), "finish",
                      GTK_SIGNAL_FUNC (ap_finish), info);

printf ("presig\n");
  /* User changes the accouting period or date signals */
  gtk_signal_connect (GTK_OBJECT (info->period_menu), "changed",
                      GTK_SIGNAL_FUNC (ap_changed), info);
printf ("postsig\n");
/*
  gtk_signal_connect (GTK_OBJECT (info->closing_date_edit), "date_changed",
                      GTK_SIGNAL_FUNC (ap_changed), info);
*/
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
