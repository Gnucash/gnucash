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

#include "dialog-utils.h"
#include "druid-acct-period.h"
#include "druid-utils.h"
#include "global-options.h"
#include "qofbook.h"
#include "gnc-component-manager.h"
#include "messages.h"


#define DRUID_ACCT_PERIOD_CM_CLASS "druid-acct-period"


/** structures *********************************************************/
typedef struct
{
  GtkWidget * window;
  GtkWidget * druid;
  GtkOptionMenu *period_menu;
  GnomeDateEdit *closing_date_edit;
  GtkLabel  * earliest_date;

} AcctPeriodInfo;


/** implementations ****************************************************/
static void
ap_window_destroy_cb (GtkObject *object, gpointer data)
{
  AcctPeriodInfo *info = data;

  gnc_unregister_gui_component_by_data (DRUID_ACCT_PERIOD_CM_CLASS, info);

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
  AcctPeriodInfo *info = user_data;
  time_t closing_date;
  GtkMenuItem *item;

/*
  closing_date = gnome_date_edit_get_date (info->closing_date_edit);
*/
  
  item = GTK_MENU_ITEM (gtk_menu_get_active (GTK_MENU(info->period_menu->menu)));
  printf ("something changed, time=%ld item=%p\n", closing_date, item);

  
}

/* =============================================================== */

static void
ap_druid_create (AcctPeriodInfo *info)
{
  GladeXML *xml;
  GtkWidget *page;

  xml = gnc_glade_xml_new ("acctperiod.glade", "Acct Period Druid");

  info->window = glade_xml_get_widget (xml, "Acct Period Druid");

  info->druid = glade_xml_get_widget (xml, "acct_period_druid");

  info->period_menu = 
        GTK_OPTION_MENU (glade_xml_get_widget (xml, "period menu"));
  info->closing_date_edit = 
        GNOME_DATE_EDIT (glade_xml_get_widget (xml, "closing date edit"));
  info->earliest_date = 
        GTK_LABEL (glade_xml_get_widget (xml, "earliest trans label"));

  /* generic finished/close/abort signals */
  gtk_signal_connect (GTK_OBJECT (info->window), "destroy",
                      GTK_SIGNAL_FUNC (ap_window_destroy_cb), info);

  gtk_signal_connect (GTK_OBJECT (info->druid), "cancel",
                      GTK_SIGNAL_FUNC (ap_druid_cancel), info);

  page = glade_xml_get_widget (xml, "finish_page");
  gtk_signal_connect (GTK_OBJECT (page), "finish",
                      GTK_SIGNAL_FUNC (ap_finish), info);

  /* User changes the accouting period or date signals */
// XXX doesn't work ... 
  gtk_signal_connect (GTK_OBJECT (info->period_menu), "clicked",
                      GTK_SIGNAL_FUNC (ap_changed), info);

  gtk_signal_connect (GTK_OBJECT (info->period_menu->menu), "selection_done",
                      GTK_SIGNAL_FUNC (ap_changed), info);

  gtk_signal_connect (GTK_OBJECT (info->closing_date_edit), "date_changed",
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
