/********************************************************************\
 * druid-hbci-initial.c -- hbci creation functionality              *
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

#include "druid-hbci-initial.h"
/* #include "Group.h" */
#include "dialog-utils.h"
#include "druid-utils.h"
/* #include "glade/glade-xml.h" */
/* #include "gnc-amount-edit.h" */
/* #include "gnc-commodity-edit.h" */
/* #include "gnc-general-select.h" */
/* #include "gnc-component-manager.h" */
/* #include "../gnome-utils/gnc-dir.h" */
/* #include "gnc-gui-query.h" */
/* #include "gnc-ui-util.h" */
/* #include "io-example-account.h" */
/* #include "top-level.h" */

typedef struct 
{
  GtkWidget *window;
  GtkWidget *druid;

  /* bank info page */
  GtkWidget *bankcode;
  GtkWidget *countrycode;
  GtkWidget *ipaddr;
  GtkWidget *port;
  /* user info page */
  GtkWidget *userid;
  /* medium page */
  GtkWidget *mediumrdh;
  GtkWidget *mediumpath;
  GtkWidget *mediumddv;
  /* iniletter server */
  GtkWidget *server_text;
  /* iniletter user */
  GtkWidget *user_text;
} HBCIInitialInfo;
//static AccountGroup *our_final_group = NULL;


  

static gboolean
on_userid_next (GnomeDruidPage  *gnomedruidpage,
		gpointer         arg1,
		gpointer         user_data)
{
  HBCIInitialInfo *info = user_data;
  const char *bankcode;
  int countrycode;
  const char *ipaddr, *port;
  const char *userid;

  bankcode = gtk_entry_get_text (GTK_ENTRY (info->bankcode));
  countrycode = atoi (gtk_entry_get_text (GTK_ENTRY (info->countrycode)));
  
  ipaddr = gtk_entry_get_text (GTK_ENTRY (info->ipaddr));
  port = gtk_entry_get_text (GTK_ENTRY (info->port));
  
  userid = gtk_entry_get_text (GTK_ENTRY (info->userid));

  return FALSE;
}

static gboolean
on_medium_next (GnomeDruidPage *gnomedruidpage,
		gpointer arg1,
		gpointer user_data)
{
  HBCIInitialInfo *info = user_data;
  gboolean is_rdh;
  const char *mediumpath;
  
  is_rdh = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (info->mediumrdh));
  mediumpath = gtk_entry_get_text (GTK_ENTRY (info->mediumpath));

  return FALSE;
}

static gboolean
on_iniletter_server_next (GnomeDruidPage  *gnomedruidpage,
			  gpointer arg1,
			  gpointer user_data)
{
  HBCIInitialInfo *info = user_data;
  GtkEditable *text = GTK_EDITABLE (info->server_text);
  const char *mytext = "abcdefg";
  gint *pos;
  
  pos = g_new0(gint,1);
  *pos = 0;
    
  gtk_editable_insert_text (text, mytext, strlen(mytext), pos);
  
  g_free(pos);
    
  return FALSE;
}

static gboolean
on_iniletter_user_next (GnomeDruidPage  *gnomedruidpage,
			gpointer arg1,
			gpointer user_data)
{
  HBCIInitialInfo *info = user_data;
  return FALSE;
}


void gnc_hbci_initial_druid (void)
{
  HBCIInitialInfo *info;
  GladeXML *xml;
  GtkWidget *page;
  
  info = g_new0 (HBCIInitialInfo, 1);
  
  xml = gnc_glade_xml_new ("hbci.glade", "HBCI Init Druid");

  info->window = glade_xml_get_widget (xml, "HBCI Init Druid");

  info->druid = glade_xml_get_widget (xml, "hbci_init_druid");
  gnc_druid_set_colors (GNOME_DRUID (info->druid));
  
  {
    info->bankcode = glade_xml_get_widget(xml, "bank_code_entry");
    info->countrycode = glade_xml_get_widget(xml, "country_code_entry");
    info->ipaddr = glade_xml_get_widget(xml, "ip_address_entry");
    info->port = glade_xml_get_widget(xml, "port_nr_entry");
  }
  {
    info->userid = glade_xml_get_widget(xml, "user_id_entry");
    page = glade_xml_get_widget(xml, "user_page");
    gtk_signal_connect (GTK_OBJECT (page), "next", 
			GTK_SIGNAL_FUNC (on_userid_next), info);
  }
  {
    info->mediumrdh = glade_xml_get_widget(xml, "rdh_radiobutton");
    info->mediumpath = glade_xml_get_widget(xml, "path_entry");
    info->mediumddv = glade_xml_get_widget(xml, "ddv_radiobutton");
    page = glade_xml_get_widget(xml, "medium_page");
    gtk_signal_connect (GTK_OBJECT (page), "next", 
			GTK_SIGNAL_FUNC (on_medium_next), info);
  }
  {
    info->server_text = glade_xml_get_widget(xml, "iniletter_server_text");
    page = glade_xml_get_widget(xml, "iniletter_server_page");
    gtk_signal_connect (GTK_OBJECT (page), "next", 
			GTK_SIGNAL_FUNC (on_iniletter_server_next), info);
  }
  {
    info->user_text = glade_xml_get_widget(xml, "iniletter_user_text");
    page = glade_xml_get_widget(xml, "iniletter_user_page");
    gtk_signal_connect (GTK_OBJECT (page), "next", 
			GTK_SIGNAL_FUNC (on_iniletter_user_next), info);
  }
  

  //gtk_signal_connect (GTK_OBJECT(dialog), "destroy",
  //                   GTK_SIGNAL_FUNC(gnc_hierarchy_destroy_cb), NULL);

  gtk_widget_show_all (info->window);
  
}

SCM  scm_hbci_initial_druid (void)
{
  gnc_hbci_initial_druid();
  return SCM_EOL;
}
