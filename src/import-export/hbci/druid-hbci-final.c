/********************************************************************\
 * druid-hbci-final.c -- hbci creation functionality                *
 * Copyright (C) 2002 Christian Stimming                            *
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
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "druid-hbci-final.h"
/* #include "Group.h" */
#include "dialog-utils.h"
#include "druid-utils.h"
#include <openhbci/api.h>

typedef struct 
{
  GtkWidget *window;
  GtkWidget *druid;

  /* OpenHBCI stuff */
  HBCI_API *api;

} HBCIFinalInfo;


static void
delete_final_druid (HBCIFinalInfo *info)
{
  if (info == NULL) return;
  if (info->window == NULL) return;
  
  gtk_widget_destroy (info->window);
  info->window = NULL;

  if (info->api == NULL) return;
  HBCI_API_delete(info->api);
  info->api = NULL;
}

static void
on_cancel (GnomeDruid *gnomedruid,
	   gpointer user_data)
{
  HBCIFinalInfo *info = user_data;
  
  delete_final_druid(info);
}

static void
on_finish (GnomeDruidPage *gnomedruidpage,
	   gpointer arg1,
	   gpointer user_data)
{
  HBCIFinalInfo *info = user_data;

  delete_final_druid(info);
}




void gnc_hbci_final_druid (void)
{
  HBCIFinalInfo *info;
  GladeXML *xml;
  GtkWidget *page;
  
  info = g_new0 (HBCIFinalInfo, 1);
  
  xml = gnc_glade_xml_new ("hbci.glade", "Finish HBCI Setup Druid");

  info->window = glade_xml_get_widget (xml, "Finish HBCI Setup Druid");

  info->druid = glade_xml_get_widget (xml, "hbci_final_druid");
  gnc_druid_set_colors (GNOME_DRUID (info->druid));
  
  glade_xml_signal_connect_data (xml, "on_finish", 
				 GTK_SIGNAL_FUNC (on_finish), info);
  glade_xml_signal_connect_data (xml, "on_cancel", 
				 GTK_SIGNAL_FUNC (on_cancel), info);
  
  /*{
    info->userid = glade_xml_get_widget(xml, "user_id_entry");
    page = glade_xml_get_widget(xml, "user_page");
    info->userpage = page;
    gtk_signal_connect (GTK_OBJECT (page), "next", 
			GTK_SIGNAL_FUNC (on_userid_next), info);
			}*/


  //gtk_signal_connect (GTK_OBJECT(dialog), "destroy",
  //                   GTK_SIGNAL_FUNC(gnc_hierarchy_destroy_cb), NULL);

  gtk_widget_show_all (info->window);
  
}

SCM  scm_hbci_final_druid (void)
{
  gnc_hbci_final_druid();
  return SCM_EOL;
}
