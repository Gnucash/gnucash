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
#include "gnc-hbci-kvp.h"
#include "hbci-account-picker.h"
#include "gnc-hbci-utils.h"

#include "dialog-utils.h"
#include "druid-utils.h"
#include "gnc-ui-util.h"
#include "gnc-ui.h"
/* #include "Group.h" */
/* #include "glade/glade-xml.h" */
/* #include "gnc-amount-edit.h" */
/* #include "gnc-commodity-edit.h" */
/* #include "gnc-general-select.h" */
/* #include "gnc-component-manager.h" */
/* #include "../gnome-utils/gnc-dir.h" */
/* #include "gnc-gui-query.h" */
/* #include "io-example-account.h" */
/* #include "top-level.h" */
#include <openhbci/api.h>

struct _hbciinitialinfo 
{
  GtkWidget *window;
  GtkWidget *druid;

  /* configfile page */
  GtkWidget *filepage;
  GtkWidget *configfileentry;
  
  /* bank info page */
  GtkWidget *bankcode;
  GtkWidget *countrycode;
  GtkWidget *ipaddr;
  //GtkWidget *port;

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

  /* account match page */
  GtkWidget *accountpage;
  GtkWidget *accountlist;
    
  /* OpenHBCI stuff */
  HBCI_API *api;

  /* account match: row_number (int) -> hbci_account */
  GHashTable *hbci_hash;
  /* hbci_account (direct) -> gnucash_account  -- DO NOT DELETE THE KEYS! */
  GHashTable *gnc_hash;
  
};

static gboolean
hash_remove (gpointer key, gpointer value, gpointer user_data) 
{
  free (key);
  return TRUE;
}

static void
delete_hash (GHashTable *hash) 
{
  if (hash != NULL) {
    g_hash_table_foreach_remove (hash, &hash_remove, NULL);
    g_hash_table_destroy (hash);
  }
}

static void
delete_initial_druid (HBCIInitialInfo *info)
{
  if (info == NULL) return;
  if (info->window == NULL) return;
  
  gtk_widget_destroy (info->window);
  info->window = NULL;

  if (info->api != NULL) {
    HBCI_API_delete(info->api);
    info->api = NULL;
  }
  
  delete_hash (info->hbci_hash);
  info->hbci_hash = NULL;
  g_hash_table_destroy (info->gnc_hash);
  info->gnc_hash = NULL;
}


static void
account_GHFunc (gpointer key, gpointer value, gpointer user_data)
{
  HBCI_Account *hbci_acc = key;
  Account *gnc_acc = value;
  g_assert(hbci_acc);
  g_assert(gnc_acc);
  
  gnc_hbci_set_account_accountid 
    (gnc_acc, HBCI_Account_accountId (hbci_acc));
  gnc_hbci_set_account_bankcode
    (gnc_acc, HBCI_Bank_bankCode(HBCI_Account_bank(hbci_acc)));
  gnc_hbci_set_account_countrycode
    (gnc_acc, HBCI_Bank_countryCode(HBCI_Account_bank(hbci_acc)));
}

/* hash is a DIRECT hash from each HBCI account to each gnucash
   account. */
static void
accounts_save_kvp (GHashTable *hash)
{
  g_assert(hash);
  g_hash_table_foreach (hash, &account_GHFunc, NULL);
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

  accounts_save_kvp (info->gnc_hash);
  
  delete_initial_druid(info);
}

static int
count_accounts (const list_HBCI_Bank *banklist) 
{
  list_HBCI_Bank_iter *iter, *iterend;
  int accounts;
  
  iter = list_HBCI_Bank_begin (banklist);
  iterend = list_HBCI_Bank_end (banklist);
  accounts = 0;
  for ( ; ! list_HBCI_Bank_iter_equal(iter, iterend) ; 
	list_HBCI_Bank_iter_next (iter)) {
    accounts += 
      list_HBCI_Account_size
      (HBCI_Bank_accounts (list_HBCI_Bank_iter_get (iter)));
  }
  list_HBCI_Bank_iter_delete (iter);
  list_HBCI_Bank_iter_delete (iterend);
  
  return accounts;
}

static gboolean 
on_configfile_next (GnomeDruidPage *gnomedruidpage,
		    gpointer arg1,
		    gpointer user_data)
{
  HBCIInitialInfo *info = user_data;
  char *filename;
  HBCI_API *api;
  gboolean createnew;
  
  filename = g_strstrip(gnome_file_entry_get_full_path 
			(GNOME_FILE_ENTRY (info->configfileentry), FALSE));

  if (!g_file_test (filename, G_FILE_TEST_ISFILE | G_FILE_TEST_ISLINK)) {
    createnew = gnc_verify_dialog_parented
      (GTK_WIDGET (info->window), 
       TRUE,
       _("The file %s does not exist. \nWould you like to create it now?\n (not yet implemented)"), 
       filename ? filename : _("(null"));
    if (!createnew) {
      free (filename);
      return TRUE;
    }
    // FIXME: create file here
  }

  if (!g_file_test (filename, G_FILE_TEST_ISFILE | G_FILE_TEST_ISLINK)) {
    gnc_error_dialog_parented
      (GTK_WINDOW (info->window), 
       _("The file %s does not exist."), 
       filename ? filename : _("(null"));
    free (filename);
    return TRUE;
  }
  
  if (strcmp(filename, 
	     gnc_hbci_get_book_configfile (gnc_get_current_book ())) != 0) {
    gnc_hbci_set_book_configfile (gnc_get_current_book (), filename);
  }
  
  if (info->api) {
    HBCI_API_delete (info->api);
    info->api = NULL;
  }

  api = gnc_hbci_api_new (filename);
  free (filename);
  info->api = api;
  
  /* Get HBCI bank and account list */
  {
    const list_HBCI_Bank *banklist;
    int accounts;

    banklist = HBCI_API_bankList (api);
    //printf("%d banks found.\n", list_HBCI_Bank_size (banklist));
    if (list_HBCI_Bank_size (banklist) == 0) {
      return FALSE;
    }

    accounts = count_accounts(banklist);
    //printf("%d accounts found.\n", accounts);
    
    if (accounts > 0){
      gnome_druid_set_page (GNOME_DRUID (info->druid), 
			    GNOME_DRUID_PAGE (info->accountpage));
      return TRUE;
    }
  }

  return FALSE;
}


  

static gboolean
on_userid_next (GnomeDruidPage  *gnomedruidpage,
		gpointer         arg1,
		gpointer         user_data)
{
  HBCIInitialInfo *info = user_data;
  const char *bankcode = NULL;
  int countrycode = 0;
  const char *ipaddr = NULL;//, *port;
  const char *userid = NULL;
  HBCI_API *api = info->api;
  HBCI_Bank *bank = NULL;
  const HBCI_User *user = NULL;
    
  bankcode = gtk_entry_get_text (GTK_ENTRY (info->bankcode));
  countrycode = atoi (gtk_entry_get_text (GTK_ENTRY (info->countrycode)));
  
  ipaddr = gtk_entry_get_text (GTK_ENTRY (info->ipaddr));
  //port = gtk_entry_get_text (GTK_ENTRY (info->port));
  
  userid = gtk_entry_get_text (GTK_ENTRY (info->userid));

  //printf("Got bankcode %s and userid %s.\n", bankcode, userid);

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
  mediumpath = gnome_file_entry_get_full_path 
    (GNOME_FILE_ENTRY (info->mediumpath), FALSE);

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
    
  gtk_editable_insert_text (text, mytext, strlen(mytext), pos);
  
  g_free(pos);
    
  return FALSE;
}

static gboolean
on_iniletter_user_next (GnomeDruidPage  *gnomedruidpage,
			gpointer arg1,
			gpointer user_data)
{
  //HBCIInitialInfo *info = user_data;
  return FALSE;
}

/* Update the account list GtkCList widget */
static void
update_accountlist (HBCIInitialInfo *info)
{
  const list_HBCI_Bank *banklist;
  list_HBCI_Bank_iter *iter, *iterend;
  gchar *row_text[3];
  int row;
  gint *row_key;
  const HBCI_Account *hacc;
  Account *gacc;
  const list_HBCI_Account *acclist;
  list_HBCI_Account_iter *aiter, *aiterend;
  int sel_row = 0;

  g_assert(info);
  g_assert(info->api);
  g_assert(info->gnc_hash);

  banklist = HBCI_API_bankList (info->api);
  //printf("%d banks found.\n", list_HBCI_Bank_size (banklist));
  if (list_HBCI_Bank_size (banklist) == 0) 
    return;

  /* Store old selected row here. */
  sel_row = (GTK_CLIST(info->accountlist))->focus_row;

  /* Delete old list */
  gtk_clist_freeze (GTK_CLIST (info->accountlist));
  gtk_clist_clear (GTK_CLIST (info->accountlist));
  row_text[2] = "";

  /* Delete old row_number -> hbci_account hash */
  delete_hash (info->hbci_hash);
  info->hbci_hash = g_hash_table_new (&g_int_hash, &g_int_equal);
  g_hash_table_freeze (info->hbci_hash);
  
  /* Go through all HBCI banks */
  iter = list_HBCI_Bank_begin (banklist);
  iterend = list_HBCI_Bank_end (banklist);
  for ( ; ! list_HBCI_Bank_iter_equal(iter, iterend) ; 
	list_HBCI_Bank_iter_next (iter)) {
    acclist = HBCI_Bank_accounts (list_HBCI_Bank_iter_get (iter));
    if (list_HBCI_Account_size (acclist) > 0) {
      /* Now go through all HBCI accounts of this HBCI bank */
      aiter = list_HBCI_Account_begin (acclist);
      aiterend = list_HBCI_Account_end (acclist);
      for ( ; ! list_HBCI_Account_iter_equal(aiter, aiterend) ; 
	    list_HBCI_Account_iter_next (aiter)) {
	/* Now add this HBCI account to the CList widget */
	hacc = list_HBCI_Account_iter_get(aiter);

	/* Account code, then Bank name, then Bank code in parentheses. */
	row_text[0] = 
	  g_strdup_printf("%s at %s (code %s)",
			  HBCI_Account_accountId (hacc),
			  HBCI_Bank_name (HBCI_Account_bank (hacc)),
			  HBCI_Bank_bankCode (HBCI_Account_bank (hacc)));
		
	/* Get corresponding gnucash account */
	gacc = g_hash_table_lookup (info->gnc_hash, hacc);

	/* Build the text for the gnucash account. */
	if (gacc == NULL)
	  row_text[1] = "";
	else 
	  row_text[1] = 
	    xaccAccountGetFullName (gacc, gnc_get_account_separator ());

	/* Add this row to the list */
	row = gtk_clist_append (GTK_CLIST (info->accountlist), row_text);

	/* Set the "new" checkbox. */
	gnc_clist_set_check (GTK_CLIST (info->accountlist), row, 2,
			     FALSE);

	/* Store the row_number -> hbci_account hash reference. */
	row_key = g_new(gint, 1);
	*row_key = row;
	g_hash_table_insert (info->hbci_hash, row_key, (HBCI_Account*)hacc);
      }
      list_HBCI_Account_iter_delete (aiter);
      list_HBCI_Account_iter_delete (aiterend);
    }
  }
  list_HBCI_Bank_iter_delete (iter);
  list_HBCI_Bank_iter_delete (iterend);

  //printf("HBCI hash has %d entries.\n", g_hash_table_size(info->hbci_hash));
  //printf("GNC hash has %d entries.\n", g_hash_table_size(info->gnc_hash));
  
  g_hash_table_thaw (info->hbci_hash);
  gtk_clist_thaw (GTK_CLIST (info->accountlist));

  /* move to the old selected row */
  (GTK_CLIST(info->accountlist))->focus_row = sel_row;
  gtk_clist_moveto(GTK_CLIST(info->accountlist), sel_row, 0, 0.0, 0.0);
}


struct hbci_acc_cb_data 
{
  HBCI_API *api;
  GHashTable *hash;
};

static gpointer 
hbci_accountcallback (Account *gnc_acc, gpointer user_data)
{
  struct hbci_acc_cb_data *data = user_data;
  HBCI_Account *hbci_acc = NULL;

  hbci_acc = (HBCI_Account *) gnc_hbci_get_hbci_acc (data->api, gnc_acc);
  if (hbci_acc) {
    g_hash_table_insert (data->hash, hbci_acc, gnc_acc);
  }
  return NULL;
}

static GHashTable *
gnc_hbci_new_hash_from_kvp (HBCI_API *api)
{
  GHashTable *hash;

  hash = g_hash_table_new (&g_direct_hash, &g_direct_equal);
  if (api) {
    struct hbci_acc_cb_data data;
    AccountGroup *grp = gnc_book_get_group (gnc_get_current_book ());
    data.api = api;
    data.hash = hash;
    xaccGroupForEachAccount (grp, 
			     &hbci_accountcallback,
			     &data, TRUE);
  }
  return hash;
}


static void
on_accountlist_prepare (GnomeDruidPage *gnomedruidpage,
			gpointer arg1,
			gpointer user_data)
{
  HBCIInitialInfo *info = user_data;

  if (info->gnc_hash == NULL)
    info->gnc_hash = gnc_hbci_new_hash_from_kvp (info->api);
  
  update_accountlist(info);
}

static void
on_accountlist_select_row (GtkCList *clist, gint row,
			   gint column, GdkEvent *event,
			   gpointer user_data)
{
  HBCIInitialInfo *info = user_data;
  HBCI_Account *hbci_acc;
  Account *gnc_acc, *old_value;
  
  hbci_acc = g_hash_table_lookup (info->hbci_hash, &row);
  if (hbci_acc) {
    old_value = g_hash_table_lookup (info->gnc_hash, hbci_acc);

    gnc_acc = hbci_account_picker_dialog(info, old_value);

    if (gnc_acc) {
      if (old_value) 
	g_hash_table_remove (info->gnc_hash, hbci_acc);
      
      g_hash_table_insert (info->gnc_hash, hbci_acc, gnc_acc);
    }
    
    /* update display */
    update_accountlist(info);
  } /* hbci_acc */
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
    /* Page with config file entry widget */
    char *filename;

    info->configfileentry = 
      glade_xml_get_widget(xml, "configfile_fileentry");
    /* Set the saved filename, if that is a valid file */
    filename = 
      g_strdup (gnc_hbci_get_book_configfile (gnc_get_current_book () ));
    if (filename && 
	g_file_test (filename, G_FILE_TEST_ISFILE | G_FILE_TEST_ISLINK)) 
      gtk_entry_set_text 
	(GTK_ENTRY (gnome_file_entry_gtk_entry
		    (GNOME_FILE_ENTRY (info->configfileentry))), 
	 filename);
    free (filename);

    page = glade_xml_get_widget(xml, "configfile_page");
    info->filepage = page;
    gtk_signal_connect (GTK_OBJECT (page), "next", 
			GTK_SIGNAL_FUNC (on_configfile_next), info);
  }
  {
    info->bankcode = glade_xml_get_widget(xml, "bank_code_entry");
    info->countrycode = glade_xml_get_widget(xml, "country_code_entry");
    info->ipaddr = glade_xml_get_widget(xml, "ip_address_entry");
    //info->port = glade_xml_get_widget(xml, "port_nr_entry");
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
    info->mediumpath = glade_xml_get_widget(xml, "keyfile_fileentry");
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
  {
    info->accountlist = glade_xml_get_widget(xml, "account_page_list");
    gtk_signal_connect (GTK_OBJECT (info->accountlist), "select_row",
			GTK_SIGNAL_FUNC (on_accountlist_select_row), info);
    page = glade_xml_get_widget(xml, "account_match_page");
    info->accountpage = page;
    gtk_signal_connect (GTK_OBJECT (page), "prepare", 
			GTK_SIGNAL_FUNC (on_accountlist_prepare), info);
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
