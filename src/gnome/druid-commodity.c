/********************************************************************
 * druid-commodity.c -- fancy importer for old Gnucash files        *
 *                       (GnuCash)                                  *
 * Copyright (C) 2000 Bill Gribble <grib@billgribble.com>           *
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
 ********************************************************************/

#define _GNU_SOURCE

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <glib.h>
#include <assert.h>

#include "FileDialog.h"
#include "druid-commodity.h"
#include "query-user.h"
#include "gnc-commodity.h"
#include "gnc-engine.h"

struct _commoditydruid {
  GtkWidget  * window; 
  GtkWidget  * druid;

  GtkWidget  * intro_page;  
  GtkWidget  * finish_page;
  
  GHashTable * new_map;
  GHashTable * old_map;
  GList      * pages;
};

struct _commoditydruidpage {
  GtkWidget * page;
  GtkWidget * old_name_entry;
  GtkWidget * new_type_combo;
  GtkWidget * new_type_entry;
  GtkWidget * new_name_entry;
  GtkWidget * new_mnemonic_entry;
};  

typedef struct _commoditydruidpage CommodityDruidPage;
static CommodityDruidPage * make_commodity_druid_page(gnc_commodity * comm);
 
static GdkColor std_bg_color = { 0, 39835, 49087, 40092 };
static GdkColor std_logo_bg_color = { 0, 65535, 65535, 65535 };
static GdkColor std_title_color =  { 0, 65535, 65535, 65535 };

static int gnc_ui_commodity_druid_comm_check_cb(GnomeDruidPage * page, 
                                                gpointer druid, 
                                                gpointer user_data);
/********************************************************************
 * gnc_ui_commodity_druid_create()
 ********************************************************************/

CommodityDruid *
gnc_ui_commodity_druid_create(const char * filename) {

  CommodityDruid * d = g_new0(CommodityDruid, 1);
  GtkObject      * dobj;
  GList          * orphans, * l;
  gnc_commodity  * lost;
  gnc_commodity  * found;
  CommodityDruidPage * new_page;
  GnomeDruidPage * back_page;

  /* call the glade creator */
  d->window = create_New_Commodity_Format_Druid();
  dobj = GTK_OBJECT(d->window);

  d->druid           = gtk_object_get_data(dobj, "commodity_druid");
  d->intro_page      = gtk_object_get_data(dobj, "start_page");
  d->finish_page     = gtk_object_get_data(dobj, "finish_page");
  back_page = d->intro_page;

  gtk_object_set_data(dobj, "commodity_druid_struct", (gpointer)d);
  
  d->new_map = g_hash_table_new(g_str_hash, g_str_equal);
  d->old_map = g_hash_table_new(g_str_hash, g_str_equal);
  orphans = 
    gnc_commodity_table_get_commodities(gnc_engine_commodities(),
                                        GNC_COMMODITY_NS_LEGACY);
  
  gnc_commodity_table_delete_namespace(gnc_engine_commodities(),
                                       GNC_COMMODITY_NS_LEGACY);
  
  /* make a new list with the (saved) old mnemonic and the 
   * new currency. */
  for(l=orphans; l; l=l->next) {
    lost = (gnc_commodity *)l->data;
    
    /* if the mnemonic is an ISO-4217 currency, use that as 
     * the default */
    found = gnc_commodity_table_lookup(gnc_engine_commodities(),
                                       GNC_COMMODITY_NS_ISO,
                                       gnc_commodity_get_mnemonic(lost));

    /* otherwise, guess that it's a NASDAQ security. */
    if(!found) {
      found = gnc_commodity_new(gnc_commodity_get_mnemonic(lost),
                                "share", "mill", 
                                GNC_COMMODITY_NS_NASDAQ,
                                gnc_commodity_get_mnemonic(lost),
                                0, 1000, 1000);
    }
    
    g_hash_table_insert(d->new_map, gnc_commodity_get_mnemonic(lost),
                        (gpointer)found);
    g_hash_table_insert(d->old_map, gnc_commodity_get_mnemonic(lost),
                        (gpointer)lost);
    
    /* create a new page in the wizard for the commodity */
    new_page = make_commodity_druid_page(found);

    /* set up next/back signal handlers */
    gtk_signal_connect(GTK_OBJECT (new_page->page), "next",
                       GTK_SIGNAL_FUNC(gnc_ui_commodity_druid_comm_check_cb),
                       d->window);
    
    gtk_signal_connect(GTK_OBJECT(new_page->page), "cancel",
                       GTK_SIGNAL_FUNC(gnc_ui_commodity_druid_cancel_cb),
                       d->window);
    
    d->pages = g_list_append(d->pages, new_page);

    gnome_druid_insert_page(GNOME_DRUID(d->druid),
                            back_page, 
                            GNOME_DRUID_PAGE(new_page->page));
    back_page = GNOME_DRUID_PAGE(new_page->page);
  }
  gtk_widget_show_all(d->window);
  return d;
}


/********************************************************************
 * make_commodity_druid_page : build a single commodity xlator 
 * page.  This really should be done in glade, but I can't figure out 
 * how to make it work. 
 ********************************************************************/

static int 
g_strcmp(gconstpointer a, gconstpointer b) {
  return strcmp(a, b);
}


static char * 
update_namespace_picker(GtkWidget * combobox, const char * init_string) {
  
  GList * namespaces;
  char  * active;

  /* fetch a list of the namespaces */
  namespaces = gnc_commodity_table_get_namespaces(gnc_engine_commodities());
  namespaces = g_list_sort(namespaces, g_strcmp);

  /* stick them in the combobox */
  gtk_combo_set_popdown_strings(GTK_COMBO(combobox), namespaces);

  /* set the entry text */
  if(init_string) {
    active = g_strdup(init_string);
  }
  else {
    active = g_strdup(namespaces->data);
  }    
  gtk_entry_set_text(GTK_ENTRY(GTK_COMBO(combobox)->entry), active);
  g_list_free(namespaces);

  return active;
}


static CommodityDruidPage *
make_commodity_druid_page(gnc_commodity * comm) {

  CommodityDruidPage * retval = g_new0(CommodityDruidPage, 1);
  GtkWidget * top_hbox;
  GtkWidget * info_label;
  GtkWidget * next_label;
  GtkWidget * temp_label;
  GtkWidget * label_vbox;
  GtkWidget * entry_vbox;
  GtkWidget * twocol_hbox;
  GtkWidget * old_frame;
  GtkWidget * new_frame;

  GnomeDruidPageStandard * page;

  /* make the page widget */
  retval->page = gnome_druid_page_standard_new_with_vals("", NULL);
  gtk_object_set_data(GTK_OBJECT(retval->page),
                      "page_struct", (gpointer)retval);

  page   = GNOME_DRUID_PAGE_STANDARD(retval->page);
  
  gnome_druid_page_standard_set_bg_color(page, & std_bg_color);  
  gnome_druid_page_standard_set_logo_bg_color(page, & std_logo_bg_color);
  gnome_druid_page_standard_set_title_color(page, & std_title_color);
  gnome_druid_page_standard_set_title(page, 
                                      _("Enter information about a currency "
                                        "or stock"));

  top_hbox = gtk_hbox_new(FALSE, 2);
  gtk_box_pack_start(GTK_BOX(page->vbox), top_hbox, FALSE, FALSE, 0);
                     
  info_label = 
    gtk_label_new(_("\"Old currency/stock info\" describes the information stored in \nyour accounts with the old version of Gnucash.  \n\"New currency/stock info\" is what will be saved with your\naccounts in the new version.\n\n -- \"Type\" means the type of the currency or security.  It \nshould be \"ISO-4217 Currencies\" for national currencies, or \n\"NASDAQ\", \"NYSE\", \"EUREX\", etc for stocks. \n -- \"Full name\" is a descriptive name  for the currency or stock, \n such as \"US Dollar\" or  \"Red Hat stock\".  \n -- \"Symbol/abbreviation\" is the ticker symbol or ISO \ncurrency symbol. \"USD\" for US dollars or \"RHAT\" for Red Hat \nshares.  \n\nThe symbol must be unique within the type (i.e. there \ncan only be one currency with the symbol \"USD\"). "));
  gtk_box_pack_start(GTK_BOX(top_hbox), info_label, FALSE, FALSE, 0);
  gtk_label_set_justify (GTK_LABEL(info_label), GTK_JUSTIFY_LEFT);
  
  entry_vbox = gtk_vbox_new(FALSE, 3);
  gtk_box_pack_start(GTK_BOX(top_hbox), entry_vbox, FALSE, FALSE, 0);
  
  old_frame = gtk_frame_new(_("Old currency/stock info"));  
  gtk_box_pack_start(GTK_BOX(entry_vbox), old_frame, FALSE, FALSE, 0);

  new_frame = gtk_frame_new(_("New currency/stock info"));
  gtk_box_pack_start(GTK_BOX(entry_vbox), new_frame, FALSE, FALSE, 0);
  
  next_label = gtk_label_new(_("Click \"Next\" to accept the information\nand move to the next currency or stock.\n"));
  gtk_box_pack_end(GTK_BOX(entry_vbox), next_label, FALSE, FALSE, 0);
  
  /* the old commodity entry area... not editable */
  twocol_hbox = gtk_hbox_new(FALSE, 2);
  gtk_container_add(GTK_CONTAINER(old_frame), twocol_hbox);

  label_vbox = gtk_vbox_new(TRUE, 0);
  gtk_box_pack_start(GTK_BOX(twocol_hbox), label_vbox, FALSE, FALSE, 0);
  
  entry_vbox = gtk_vbox_new(TRUE, 0);
  gtk_box_pack_start(GTK_BOX(twocol_hbox), entry_vbox, FALSE, FALSE, 0);
  
  temp_label = gtk_label_new(_("Currency/stock name:"));
  gtk_box_pack_start (GTK_BOX (label_vbox), temp_label, FALSE, FALSE, 0);
  gtk_misc_set_alignment (GTK_MISC (temp_label), 1, 0.5);

  retval->old_name_entry = gtk_entry_new();
  gtk_box_pack_start(GTK_BOX(entry_vbox),
                     retval->old_name_entry, FALSE, FALSE, 0);

  gtk_entry_set_editable(GTK_ENTRY(retval->old_name_entry),
                         FALSE);
  gtk_entry_set_text(GTK_ENTRY(retval->old_name_entry),
                     gnc_commodity_get_mnemonic(comm));
  
  /* the new commodity entry area... editable */
  twocol_hbox = gtk_hbox_new(FALSE, 2);
  gtk_container_add(GTK_CONTAINER(new_frame), twocol_hbox);

  label_vbox = gtk_vbox_new(TRUE, 0);
  gtk_box_pack_start(GTK_BOX(twocol_hbox), label_vbox, FALSE, FALSE, 0);
  
  entry_vbox = gtk_vbox_new(TRUE, 0);
  gtk_box_pack_start(GTK_BOX(twocol_hbox), entry_vbox, FALSE, FALSE, 0);
  
  temp_label = gtk_label_new(_("Type:"));
  gtk_box_pack_start (GTK_BOX (label_vbox), temp_label, FALSE, FALSE, 0);
  gtk_misc_set_alignment (GTK_MISC (temp_label), 1, 0.5);

  temp_label = gtk_label_new(_("Full name:"));
  gtk_box_pack_start (GTK_BOX (label_vbox), temp_label, FALSE, FALSE, 0);
  gtk_misc_set_alignment (GTK_MISC (temp_label), 1, 0.5);

  temp_label = gtk_label_new(_("Symbol/abbreviation:"));
  gtk_box_pack_start (GTK_BOX (label_vbox), temp_label, FALSE, FALSE, 0);
  gtk_misc_set_alignment (GTK_MISC (temp_label), 1, 0.5);
  
  retval->new_type_combo = gtk_combo_new(); 
  gtk_box_pack_start(GTK_BOX(entry_vbox),
                     retval->new_type_combo, FALSE, FALSE, 0);
  retval->new_type_entry = (GTK_COMBO(retval->new_type_combo))->entry;
  update_namespace_picker(retval->new_type_combo, 
                          gnc_commodity_get_namespace(comm));
  
  retval->new_name_entry = gtk_entry_new();
  gtk_box_pack_start(GTK_BOX(entry_vbox), retval->new_name_entry,
                     FALSE, FALSE, 0);
  gtk_entry_set_text(GTK_ENTRY(retval->new_name_entry),
                     gnc_commodity_get_fullname(comm));
  
  retval->new_mnemonic_entry = gtk_entry_new();
  gtk_box_pack_start(GTK_BOX(entry_vbox), retval->new_mnemonic_entry,
                     FALSE, FALSE, 0);
  gtk_entry_set_text(GTK_ENTRY(retval->new_mnemonic_entry),
                     gnc_commodity_get_mnemonic(comm));

  return retval;
}
    
/********************************************************************
 * gnc_ui_commodity_druid_destroy()
 ********************************************************************/

void
gnc_ui_commodity_druid_destroy(CommodityDruid * cd) {  
  g_list_free(cd->pages);
  g_hash_table_destroy(cd->new_map);
  g_hash_table_destroy(cd->old_map);

  gtk_widget_destroy(GTK_WIDGET(cd->window));
}


/********************************************************************
 * callbacks
 ********************************************************************/

gboolean
gnc_ui_commodity_druid_cancel_cb(GnomeDruidPage * page, gpointer druid,
                                 gpointer user_data) {
  CommodityDruid * cd = 
    (CommodityDruid *)gtk_object_get_data(GTK_OBJECT(user_data),
                                          "commodity_druid_struct");
  /* unload the current file (can't have out-of-date commodities) */
  gncFileQuit();
  gnc_refresh_main_window();

  /* destroy the dialog */
  gnc_ui_commodity_druid_destroy(cd);

  return TRUE;
}


static gboolean
gnc_ui_commodity_druid_comm_check_cb(GnomeDruidPage * page, gpointer druid,
                                     gpointer user_data) {
  CommodityDruid * cd = 
    (CommodityDruid *)gtk_object_get_data(GTK_OBJECT(user_data),
                                          "commodity_druid_struct");
  CommodityDruidPage * dpage = 
    (CommodityDruidPage *)gtk_object_get_data(GTK_OBJECT(page),
                                              "page_struct");
  char * old_name;
  char * new_type;
  char * new_name;
  char * new_mnemonic;
  gnc_commodity * new_comm;

  old_name     = gtk_entry_get_text(GTK_ENTRY(dpage->old_name_entry));
  new_type     = gtk_entry_get_text(GTK_ENTRY(dpage->new_type_entry));
  new_name     = gtk_entry_get_text(GTK_ENTRY(dpage->new_name_entry));
  new_mnemonic = gtk_entry_get_text(GTK_ENTRY(dpage->new_mnemonic_entry));
  if((strlen(new_type) == 0) ||
     (strlen(new_name) == 0) ||
     (strlen(new_mnemonic) == 0)) {
    gnc_warning_dialog(_("You must put values for the type, name,\n"
                         "and abbreviation of the currency/stock."));
    
    return TRUE;
  }
  else {
    new_comm = g_hash_table_lookup(cd->new_map, old_name);
    assert(new_comm);
    
    /* fill in the commodity structure info */
    gnc_commodity_set_fullname(new_comm, new_name);
    gnc_commodity_set_namespace(new_comm, new_type);
    gnc_commodity_set_mnemonic(new_comm, new_mnemonic);
    return FALSE;
  }
}


static void 
finish_helper(gpointer key, gpointer value, gpointer data) {
  CommodityDruid * cd = data;
  gnc_commodity  * comm = value; 
  gnc_commodity  * old_comm = g_hash_table_lookup(cd->old_map, 
                                                  key);
  Account        ** accts;
  Account        ** current;

  /* key is the old mnemonic, value is a pointer to the gnc_commodity 
   * structure. */
  gnc_commodity_table_insert(gnc_engine_commodities(), comm);

  /* now replace all the accounts using old_comm with new_comm */
  accts = xaccGetAccounts(gncGetCurrentGroup());
  for(current = accts; *current; current++) {
    xaccAccountBeginEdit(*current, TRUE);
    if(gnc_commodity_equiv(xaccAccountGetCurrency(*current),
                           old_comm)) {
      xaccAccountSetCurrency(*current, comm);
    }
    
    if(gnc_commodity_equiv(xaccAccountGetSecurity(*current),
                           old_comm)) {
      xaccAccountSetSecurity(*current, comm);
    }
    xaccAccountCommitEdit(*current);    
  } 
}


void
gnc_ui_commodity_druid_finish_cb(GnomeDruidPage * page, gpointer druid,
                                 gpointer user_data) {
  CommodityDruid * cd = 
    (CommodityDruid *)gtk_object_get_data(GTK_OBJECT(user_data),
                                          "commodity_druid_struct");  

  /* add the new commodities to the engine's namespace map and 
   * replace the account commodity pointers */
  g_hash_table_foreach(cd->new_map, &finish_helper, (gpointer)cd);

  /* destroy the dialog */
  gnc_ui_commodity_druid_destroy(cd);
  gnc_refresh_main_window();
}

