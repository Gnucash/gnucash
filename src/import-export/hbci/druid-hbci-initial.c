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
#include <openhbci/api.h>

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
  GtkWidget *userpage;
    
  /* medium page */
  GtkWidget *mediumrdh;
  GtkWidget *mediumpath;
  GtkWidget *mediumddv;

  /* iniletter server */
  GtkWidget *server_text;

  /* iniletter user */
  GtkWidget *user_text;

  /* OpenHBCI stuff */
  HBCI_API *api;

} HBCIInitialInfo;
//static AccountGroup *our_final_group = NULL;


static void
delete_initial_druid (HBCIInitialInfo *info)
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
  HBCIInitialInfo *info = user_data;
  
  delete_initial_druid(info);
}

static void
on_finish (GnomeDruidPage *gnomedruidpage,
	   gpointer arg1,
	   gpointer user_data)
{
  HBCIInitialInfo *info = user_data;

  delete_initial_druid(info);
}


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
  HBCI_API *api;
  HBCI_Error *err;
  const char *cfgfile = HBCI_CFGFILE;
  char *errstring;
  HBCI_Bank *bank;
  HBCI_User *user;
    
  bankcode = gtk_entry_get_text (GTK_ENTRY (info->bankcode));
  countrycode = atoi (gtk_entry_get_text (GTK_ENTRY (info->countrycode)));
  
  ipaddr = gtk_entry_get_text (GTK_ENTRY (info->ipaddr));
  port = gtk_entry_get_text (GTK_ENTRY (info->port));
  
  userid = gtk_entry_get_text (GTK_ENTRY (info->userid));

  printf("Got bankcode %s and userid %s.\n", bankcode, userid);
  api = HBCI_API_new(FALSE, TRUE);
  err = HBCI_API_loadEnvironment(api, cfgfile);
  if (!HBCI_Error_isOk(err)) {
    errstring = HBCI_Error_errorString(err);
    fprintf(stderr,"At on_userid_next, loadEnvironment: ERROR: %s\n",
	    errstring);
    free(errstring);
    HBCI_API_delete(api);
    info->api = NULL;
    return FALSE;
  }
  
  bank = HBCI_API_findBank(api, countrycode, bankcode);
  if (bank == NULL) {
    printf("No bank found.\n");
    HBCI_API_delete(api);
    info->api = NULL;
    return FALSE;
  }
  printf("Found bank, name %s.\n", HBCI_Bank_name(bank));
    
  user = HBCI_Bank_findUser(bank, userid);
  if (user == NULL) {
    printf("No user found.\n");
    HBCI_API_delete(api);
    info->api = NULL;
    return FALSE;
  }
  printf("Found user, name %s.\n", HBCI_User_userName(user));
    
  info->api = api;
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
  
  glade_xml_signal_connect_data (xml, "on_finish", 
				 GTK_SIGNAL_FUNC (on_finish), info);
  glade_xml_signal_connect_data (xml, "on_cancel", 
				 GTK_SIGNAL_FUNC (on_cancel), info);
  
  {
    info->bankcode = glade_xml_get_widget(xml, "bank_code_entry");
    info->countrycode = glade_xml_get_widget(xml, "country_code_entry");
    info->ipaddr = glade_xml_get_widget(xml, "ip_address_entry");
    info->port = glade_xml_get_widget(xml, "port_nr_entry");
  }
  {
    info->userid = glade_xml_get_widget(xml, "user_id_entry");
    page = glade_xml_get_widget(xml, "user_page");
    info->userpage = page;
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
