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

#include "config.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <glib.h>
#include <assert.h>

#include "FileDialog.h"
#include "druid-commodity.h"
#include "dialog-commodity.h"
#include "query-user.h"
#include "gnc-commodity.h"
#include "gnc-engine.h"
#include "gnc-ui.h"
#include "gnc-pricedb-p.h"

#include "gnc-engine-util.h"

/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_GUI;

struct _commoditydruid {
  GtkWidget  * window; 
  GtkWidget  * druid;

  GtkWidget  * intro_page;  
  GtkWidget  * finish_page;
  
  GHashTable * new_map;
  GHashTable * old_map;
  GList      * pages;
  
  int        is_modal;

};

struct _commoditydruidpage {
  GtkWidget * page;
  char      * old_name;
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


void
gnc_import_legacy_commodities(const char * filename) {
  CommodityDruid * d = gnc_ui_commodity_druid_create(filename);

  d->is_modal = TRUE;
  gtk_window_set_modal(GTK_WINDOW(d->window), TRUE);
  gtk_main();  
}


static gboolean
window_delete_cb(GtkWidget *widget,
                 GdkEvent  *event,
                 gpointer  user_data)
{
  CommodityDruid *cd = user_data;

  /* unload the current file (can't have out-of-date commodities) */
  gncFileQuit();

  gnc_ui_commodity_druid_destroy (cd);

  /* Don't delete the window, we'll handle things ourselves. */
  return TRUE;
}


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

  gtk_signal_connect (dobj, "delete_event",
                      GTK_SIGNAL_FUNC (window_delete_cb), d);

  d->druid           = gtk_object_get_data(dobj, "commodity_druid");
  d->intro_page      = gtk_object_get_data(dobj, "start_page");
  d->finish_page     = gtk_object_get_data(dobj, "finish_page");
  back_page = GNOME_DRUID_PAGE(d->intro_page);

  d->is_modal = FALSE;

  gtk_object_set_data(dobj, "commodity_druid_struct", (gpointer)d);

  d->new_map = g_hash_table_new(g_str_hash, g_str_equal);
  d->old_map = g_hash_table_new(g_str_hash, g_str_equal);
  orphans = 
    gnc_commodity_table_get_commodities(gnc_engine_commodities(),
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
                                GNC_COMMODITY_NS_NASDAQ,
                                gnc_commodity_get_mnemonic(lost),
                                NULL, 100000);
    }

    g_hash_table_insert(d->new_map, (gpointer)gnc_commodity_get_mnemonic(lost),
                        (gpointer)found);
    g_hash_table_insert(d->old_map, (gpointer)gnc_commodity_get_mnemonic(lost),
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

  g_list_free(orphans);

  return d;
}


/********************************************************************
 * make_commodity_druid_page : build a single commodity xlator 
 * page.  This really should be done in glade, but I can't figure out 
 * how to make it work. 
 ********************************************************************/


static CommodityDruidPage *
make_commodity_druid_page(gnc_commodity * comm) {

  CommodityDruidPage * retval = g_new0(CommodityDruidPage, 1);
  GtkWidget * alignment;
  GtkWidget * top_vbox;
  GtkWidget * info_label;
  GtkWidget * next_label;
  GtkWidget * temp;
  char      * title = NULL;
  GnomeDruidPageStandard * page;

  /* make the page widget */
  retval->page = gnome_druid_page_standard_new_with_vals("", NULL);
  gtk_object_set_data(GTK_OBJECT(retval->page),
                      "page_struct", (gpointer)retval);

  page = GNOME_DRUID_PAGE_STANDARD(retval->page);

  /* save the old commodity name */
  retval->old_name = g_strdup(gnc_commodity_get_mnemonic(comm));
  title = g_strdup_printf("Enter information about \"%s\"",
                          retval->old_name);

  gnome_druid_page_standard_set_bg_color(page, & std_bg_color);  
  gnome_druid_page_standard_set_logo_bg_color(page, & std_logo_bg_color);
  gnome_druid_page_standard_set_title_color(page, & std_title_color);
  gnome_druid_page_standard_set_title(page, title);
  g_free(title);

  alignment = gtk_alignment_new (0.5, 0.5, 0.0, 0.0);
  gtk_box_pack_start(GTK_BOX(page->vbox), alignment, FALSE, FALSE, 0);

  top_vbox = gtk_vbox_new(FALSE, 3);
  gtk_container_add(GTK_CONTAINER(alignment), top_vbox);

  info_label = 
    gtk_label_new(_("Pick the type of the currency or security. For "
                    "national currencies, \nuse \"ISO4217\".  "
                    "Enter a new type in the box if the ones in the\n"
                    "pick list are inappropriate."));
  
  gtk_label_set_justify (GTK_LABEL(info_label), GTK_JUSTIFY_LEFT);
  gtk_box_pack_start(GTK_BOX(top_vbox), info_label, TRUE, TRUE, 0);

  temp = gtk_hbox_new(FALSE, 0);
  gtk_box_pack_start(GTK_BOX(top_vbox), temp, FALSE, FALSE, 0);

  retval->new_type_combo = gtk_combo_new(); 
  gtk_box_pack_start(GTK_BOX(temp), retval->new_type_combo, TRUE, TRUE, 0);

  retval->new_type_entry = (GTK_COMBO(retval->new_type_combo))->entry;
  gnc_ui_update_namespace_picker(retval->new_type_combo, 
                                 gnc_commodity_get_namespace(comm));

  temp = gtk_hbox_new(FALSE, 0);
  gtk_box_pack_start(GTK_BOX(top_vbox), temp, FALSE, FALSE, 5);

  info_label = 
    gtk_label_new(_("Enter a descriptive name for the currency or stock, "
                    "such as \n\"US Dollar\" or \"Red Hat Stock\""));

  gtk_label_set_justify (GTK_LABEL(info_label), GTK_JUSTIFY_LEFT);
  gtk_box_pack_start(GTK_BOX(top_vbox), info_label, TRUE, TRUE, 0);

  temp = gtk_hbox_new(FALSE, 0);
  gtk_box_pack_start(GTK_BOX(top_vbox), temp, FALSE, FALSE, 0);

  retval->new_name_entry = gtk_entry_new();
  gtk_box_pack_start(GTK_BOX(temp), retval->new_name_entry,
                     TRUE, TRUE, 0);
  gtk_entry_set_text(GTK_ENTRY(retval->new_name_entry),
                     gnc_commodity_get_fullname(comm));

  temp = gtk_hbox_new(FALSE, 0);
  gtk_box_pack_start(GTK_BOX(top_vbox), temp, FALSE, FALSE, 5);

  info_label = 
    gtk_label_new(_("Enter the ticker symbol (such as \"RHAT\"), "
                    "ISO currency symbol \n(such as \"USD\"), or "
                    "other unique abbreviation for the name."));

  gtk_label_set_justify (GTK_LABEL(info_label), GTK_JUSTIFY_LEFT);
  gtk_box_pack_start(GTK_BOX(top_vbox), info_label, TRUE, TRUE, 0);

  temp = gtk_hbox_new(FALSE, 0);
  gtk_box_pack_start(GTK_BOX(top_vbox), temp, FALSE, FALSE, 0);

  retval->new_mnemonic_entry = gtk_entry_new();
  gtk_box_pack_start(GTK_BOX(temp), retval->new_mnemonic_entry,
                     TRUE, TRUE, 0);
  gtk_entry_set_text(GTK_ENTRY(retval->new_mnemonic_entry),
                     gnc_commodity_get_mnemonic(comm));

  temp = gtk_hbox_new(FALSE, 0);
  gtk_box_pack_start(GTK_BOX(top_vbox), temp, FALSE, FALSE, 5);

  next_label = gtk_label_new(_("Click \"Next\" to accept the information "
                               "and move \nto the next currency or stock."));
  gtk_label_set_justify (GTK_LABEL(next_label), GTK_JUSTIFY_LEFT);
  gtk_box_pack_start(GTK_BOX(top_vbox), next_label, TRUE, TRUE, 0);

  return retval;
}


/********************************************************************
 * gnc_ui_commodity_druid_destroy()
 ********************************************************************/

void
gnc_ui_commodity_druid_destroy(CommodityDruid * cd) {  

  GList * p;
  CommodityDruidPage * cdp;

  /* remove the old commodities no matter how we exit */
  gnc_commodity_table_delete_namespace(gnc_engine_commodities(),
                                       GNC_COMMODITY_NS_LEGACY);

  for(p=cd->pages; p; p=p->next) {
    cdp = (CommodityDruidPage *)p->data;
    g_free(cdp->old_name);
    g_free(cdp);
  }

  g_list_free(cd->pages);
  g_hash_table_destroy(cd->new_map);
  g_hash_table_destroy(cd->old_map);

  gtk_widget_destroy(GTK_WIDGET(cd->window));

  if(cd->is_modal) {
    gtk_main_quit();
  }
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
  char * new_type;
  char * new_name;
  char * new_mnemonic;
  gnc_commodity * new_comm;

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
    new_comm = g_hash_table_lookup(cd->new_map, dpage->old_name);
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
  GList          * accts;
  GList          * node;
  GNCBook        * book = gncGetCurrentBook();

  if(!book) {
    PERR("finish_helper - no current book.");
    return;
  }

  /* key is the old mnemonic, value is a pointer to the gnc_commodity 
   * structure. */
  comm = gnc_commodity_table_insert(gnc_engine_commodities(), comm);

  /* s/old commodity/new commodity/g  in the pricedb */
  gnc_pricedb_substitute_commodity(gnc_book_get_pricedb(book),
				   old_comm,
				   comm);

  /* now replace all the accounts using old_comm with new_comm */
  accts = xaccGroupGetSubAccounts(gncGetCurrentGroup());

  for(node = accts; node; node = node->next) {
    Account *account = node->data;

    xaccAccountBeginEdit(account);
    if(gnc_commodity_equiv(xaccAccountGetCurrency(account),
                           old_comm)) {
      xaccAccountSetCurrency(account, comm);
    }

    if(gnc_commodity_equiv(xaccAccountGetSecurity(account),
                           old_comm)) {
      xaccAccountSetSecurity(account, comm);
    }
    xaccAccountCommitEdit(account);    
  }

  g_list_free (accts);
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
}

