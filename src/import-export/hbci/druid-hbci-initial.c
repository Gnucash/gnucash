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
#include "druid-hbci-utils.h"
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
#include <openhbci/outboxjobs.h>


struct _hbciinitialinfo 
{
  GtkWidget *window;
  GtkWidget *druid;

  /* configfile page */
  GtkWidget *filepage;
  GtkWidget *configfileentry;
  
  /* bank info page */
  GtkWidget *bankpage;
  GtkWidget *bankcode;
  GtkWidget *countrycode;
  GtkWidget *ipaddr;
  //GtkWidget *port;

  /* user info page */
  GtkWidget *userpage;
  GtkWidget *userid;
  GtkWidget *customerid;
  GtkWidget *mediumrdh;
  GtkWidget *mediumpath;
  GtkWidget *mediumddv;

  /* account match page */
  GtkWidget *accountpage;
  GtkWidget *accountlist;
    
  /* iniletter server */
  GtkWidget *serverpage;
  GtkWidget *server_text;

  /* iniletter user */
  GtkWidget *user_text;

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


/*******************************************************************
 * update_accountlist widget
 */
static gpointer
update_accountlist_acc_cb (const HBCI_Account *hacc, gpointer user_data)
{
  HBCIInitialInfo *info = user_data;
  gchar *row_text[3];
  Account *gacc;
  int row;
  gint *row_key;

  g_assert(hacc);
  g_assert(info);
  row_text[2] = "";
  
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

  return NULL;
}
static gpointer
update_accountlist_bankcb (const HBCI_Bank *bank, gpointer user_data)
{
  g_assert(bank);

  return list_HBCI_Account_foreach (HBCI_Bank_accounts (bank),
				    &update_accountlist_acc_cb,
				    user_data);
}

/* Update the account list GtkCList widget */
static void
update_accountlist (HBCIInitialInfo *info)
{
  const list_HBCI_Bank *banklist;
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

  /* Delete old hash with row_number -> hbci_account */
  delete_hash (info->hbci_hash);
  info->hbci_hash = g_hash_table_new (&g_int_hash, &g_int_equal);
  g_hash_table_freeze (info->hbci_hash);
  
  /* Go through all HBCI banks */
  list_HBCI_Bank_foreach (banklist, 
			  &update_accountlist_bankcb,
			  info);

  //printf("HBCI hash has %d entries.\n", g_hash_table_size(info->hbci_hash));
  //printf("GNC hash has %d entries.\n", g_hash_table_size(info->gnc_hash));
  
  g_hash_table_thaw (info->hbci_hash);
  gtk_clist_thaw (GTK_CLIST (info->accountlist));

  /* move to the old selected row */
  (GTK_CLIST(info->accountlist))->focus_row = sel_row;
  gtk_clist_moveto(GTK_CLIST(info->accountlist), sel_row, 0, 0.0, 0.0);
}
/*
 * end update_accountlist 
 *******************************************************************/




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


static gboolean 
on_configfile_next (GnomeDruidPage *gnomedruidpage,
		    gpointer arg1,
		    gpointer user_data)
{
  HBCIInitialInfo *info = user_data;
  char *filename;
  HBCI_API *api;
  
  filename = g_strstrip(gnome_file_entry_get_full_path 
			(GNOME_FILE_ENTRY (info->configfileentry), FALSE));

  if (!gnc_verify_exist_or_new_file (GTK_WIDGET (info->window), filename)) {
    g_free (filename);
    return TRUE;
  }
  // file doesn't need to be created here since OpenHBCI will create
  // it automatically.

  if (!gnc_test_dir_exist_error (GTK_WINDOW (info->window), filename)) {
    g_free (filename);
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

  /* Create new HBCI_API object, loading its data from filename */
  api = gnc_hbci_api_new (filename);
  g_free (filename);
  info->api = api;
  
  /* Get HBCI bank and account list */
  {
    const list_HBCI_Bank *banklist;

    banklist = HBCI_API_bankList (api);
    //printf("%d banks found.\n", list_HBCI_Bank_size (banklist));
    if (list_HBCI_Bank_size (banklist) == 0) {
      // Zero banks? go to next page (create_bank)
      return FALSE;
    }

    if (HBCI_API_totalUsers(api) == 0) {
      // zero users? go to user-creation page
      gnome_druid_set_page (GNOME_DRUID (info->druid), 
			    GNOME_DRUID_PAGE (info->userpage));
      return TRUE;
    }
    
    if (HBCI_API_totalAccounts(api) == 0) {
      // still no accounts? call account_update 
      update_accounts (api);
    }
  }

  // accounts already exist? Then go to account matching page
  gnome_druid_set_page (GNOME_DRUID (info->druid), 
			GNOME_DRUID_PAGE (info->accountpage));
  return TRUE;
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
  const char *customerid = NULL;
  HBCI_API *api = info->api;
  HBCI_Bank *bank = NULL;
  const HBCI_User *user = NULL;
    
  bankcode = gtk_entry_get_text (GTK_ENTRY (info->bankcode));
  countrycode = atoi (gtk_entry_get_text (GTK_ENTRY (info->countrycode)));
  
  ipaddr = gtk_entry_get_text (GTK_ENTRY (info->ipaddr));
  
  userid = gtk_entry_get_text (GTK_ENTRY (info->userid));
  customerid = gtk_entry_get_text (GTK_ENTRY (info->customerid));

  //printf("Got bankcode %s and userid %s.\n", bankcode, userid);

  bank = HBCI_API_findBank(api, countrycode, bankcode);
  if (bank == NULL) {
    printf("on_userid_next: Creating bank with code %s.\n", bankcode);
    bank = HBCI_API_bankFactory (api, countrycode, bankcode, ipaddr);
    HBCI_API_addBank (api, bank, TRUE);
  } 
  else {
    printf("on_userid_next: Found bank, name %s.\n", HBCI_Bank_name(bank));
  };
  
  user = HBCI_Bank_findUser(bank, userid);
  if (user == NULL) {
    gboolean is_rdh, mount_test;
    HBCI_Medium *medium;
    HBCI_User *newuser;
    HBCI_Customer *cust;

    printf("on_userid_next: Didn't find user with userid %s.\n", userid);
    is_rdh = gtk_toggle_button_get_active 
      (GTK_TOGGLE_BUTTON (info->mediumrdh));

    if (is_rdh) {
      /* Create RDH Medium */ 
      char *mediumpath;

      mediumpath = gnome_file_entry_get_full_path 
	(GNOME_FILE_ENTRY (info->mediumpath), FALSE);

      // Some sanity checks on the filename
      if (!gnc_verify_exist_or_new_file 
	  (GTK_WIDGET (info->window), mediumpath)) {
	g_free (mediumpath);
	return TRUE;
      }
      if (!gnc_test_dir_exist_error (GTK_WINDOW (info->window), 
				     mediumpath)) {
	g_free (mediumpath);
	return TRUE;
      }
      printf("on_userid_next: About to Create user with RDH medium and userid %s.\n", userid);
      medium = HBCI_API_createNewMedium (api, 
					 countrycode, bankcode, userid, 
					 mediumpath, HBCI_SECURITY_RDH);
      printf("on_userid_next: Created user with RDH medium and userid %s.\n", userid);
      g_free(mediumpath);
      g_assert(medium);
      newuser = HBCI_API_userFactory (bank, medium, TRUE, userid);
      g_assert(newuser);
      HBCI_Bank_addUser (bank, newuser, TRUE);

      mount_test = HBCI_Medium_mountMedium (medium, newuser);
      HBCI_Medium_unmountMedium (medium);
      if (mount_test) {
	printf("on_userid_next: Mounting RDH medium was successful.\n");
      } 
      else {
	printf("on_userid_next: Mounting RDH medium failed.\n");
	return TRUE;
      }
      
      cust = HBCI_API_customerFactory (newuser, customerid, 
				       "Default Customer");
      g_assert (cust);
      HBCI_User_addCustomer (newuser, cust, TRUE);
      
      gnome_druid_set_page (GNOME_DRUID (info->druid), 
			    GNOME_DRUID_PAGE (info->serverpage));
      return TRUE;
      
    }

    // DDV Medium
    medium = HBCI_API_createNewMedium (api, 
				       countrycode, bankcode, userid, 
				       "", HBCI_SECURITY_DDV);
    g_assert(medium);
    printf("on_userid_next: Creating user with DDV medium and userid %s.\n", userid);
    newuser = HBCI_API_userFactory (bank, medium, TRUE, userid);
    g_assert(newuser);
    HBCI_Bank_addUser (bank, newuser, TRUE);

    mount_test = HBCI_Medium_mountMedium (medium, newuser);
    HBCI_Medium_unmountMedium (medium);
    if (mount_test) {
      printf("on_userid_next: Mounting DDV medium was successful.\n");
    }
    else {
      printf("on_userid_next: Mounting DDV medium failed.\n");
      return TRUE;
    }
    
    cust = HBCI_API_customerFactory (newuser, customerid, 
				     "Default Customer");
    g_assert (cust);
    HBCI_User_addCustomer (newuser, cust, TRUE);
    return FALSE;

  }
  else {
    printf("on_userid_next: Found user, name %s.\n", HBCI_User_userName(user));
  }
  
  return FALSE;
}


static gboolean
on_iniletter_server_back (GnomeDruidPage  *gnomedruidpage,
			  gpointer arg1,
			  gpointer user_data)
{
  HBCIInitialInfo *info = user_data;
  g_assert(info->druid);
  g_assert(info->userpage);
  gnome_druid_set_page (GNOME_DRUID (info->druid), 
			GNOME_DRUID_PAGE (info->userpage));
  return TRUE;
}
static void
on_iniletter_server_prepare (GnomeDruidPage *gnomedruidpage,
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
}
static gboolean
on_iniletter_server_next (GnomeDruidPage  *gnomedruidpage,
			  gpointer arg1,
			  gpointer user_data)
{
  //HBCIInitialInfo *info = user_data;
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

static void
on_button_clicked (GtkButton *button,
		   gpointer user_data)
{
  HBCIInitialInfo *info = user_data;
  char *name;
  g_assert(info->druid);
  g_assert(info->userpage);
  
  name = gtk_widget_get_name (GTK_WIDGET (button));
  if (strcmp (name, "addbank_button") == 0) {
    gnome_druid_set_page (GNOME_DRUID (info->druid), 
			  GNOME_DRUID_PAGE (info->bankpage));
  } else if (strcmp (name, "adduser_button") == 0) {
    gnome_druid_set_page (GNOME_DRUID (info->druid), 
			  GNOME_DRUID_PAGE (info->userpage));
  } else if (strcmp (name, "updatelist_button") == 0) {
    update_accounts (info->api);
    // Nothing else to do.
    //gnome_druid_set_page (GNOME_DRUID (info->druid), 
    //		  GNOME_DRUID_PAGE (info->accountpage));
  } else {
    printf("on_button_clicked: Oops, unknown button: %s\n",
	   name);
  }
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
    page = glade_xml_get_widget(xml, "bank_page");
    info->bankpage = page;
    info->bankcode = glade_xml_get_widget(xml, "bank_code_entry");
    info->countrycode = glade_xml_get_widget(xml, "country_code_entry");
    info->ipaddr = glade_xml_get_widget(xml, "ip_address_entry");
    //info->port = glade_xml_get_widget(xml, "port_nr_entry");
  }
  {
    page = glade_xml_get_widget(xml, "user_page");
    info->userpage = page;
    info->userid = glade_xml_get_widget(xml, "user_id_entry");
    info->customerid = glade_xml_get_widget(xml, "customer_id_entry");
    info->mediumrdh = glade_xml_get_widget(xml, "rdh_radiobutton");
    info->mediumpath = glade_xml_get_widget(xml, "keyfile_fileentry");
    info->mediumddv = glade_xml_get_widget(xml, "ddv_radiobutton");
    gtk_signal_connect (GTK_OBJECT (page), "next", 
			GTK_SIGNAL_FUNC (on_userid_next), info);
  }
  {
    page = glade_xml_get_widget(xml, "account_match_page");
    info->accountpage = page;
    info->accountlist = glade_xml_get_widget(xml, "account_page_list");
    gtk_signal_connect (GTK_OBJECT (info->accountlist), "select_row",
			GTK_SIGNAL_FUNC (on_accountlist_select_row), info);
    gtk_signal_connect (GTK_OBJECT 
			(glade_xml_get_widget (xml, "addbank_button")), 
			"clicked",
			GTK_SIGNAL_FUNC (on_button_clicked), info);
    gtk_signal_connect (GTK_OBJECT 
			(glade_xml_get_widget (xml, "adduser_button")), 
			"clicked",
			GTK_SIGNAL_FUNC (on_button_clicked), info);
    gtk_signal_connect (GTK_OBJECT 
			(glade_xml_get_widget (xml, "updatelist_button")), 
			"clicked",
			GTK_SIGNAL_FUNC (on_button_clicked), info);
    gtk_signal_connect (GTK_OBJECT (page), "prepare", 
			GTK_SIGNAL_FUNC (on_accountlist_prepare), info);
  }
  {
    info->server_text = glade_xml_get_widget(xml, "iniletter_server_text");
    page = glade_xml_get_widget(xml, "iniletter_server_page");
    info->serverpage = page;
    gtk_signal_connect (GTK_OBJECT (page), "prepare", 
			GTK_SIGNAL_FUNC (on_iniletter_server_prepare), info);
    gtk_signal_connect (GTK_OBJECT (page), "back", 
			GTK_SIGNAL_FUNC (on_iniletter_server_back), info);
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
