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
#include "gnc-html.h"

#include <openhbci/api.h>
#include <openhbci/outboxjobs.h>
#include <openhbci/outboxjobkeys.h>
#include <openhbci/mediumrdh.h>

typedef enum _infostate {
  INI_ADD_BANK,
  INI_ADD_USER,
  INI_UPDATE_ACCOUNTS,
  INI_MATCH_ACCOUNTS,
  ADD_BANK,
  ADD_USER,
  UPDATE_ACCOUNTS,
  MATCH_ACCOUNTS
} Infostate;

struct _hbciinitialinfo 
{
  GtkWidget *window;
  GtkWidget *druid;

  /* configfile page */
  GtkWidget *filepage;
  GtkWidget *configfileentry;
  char *configfile;
  
  /* bank info page */
  GtkWidget *bankpage;
  GtkWidget *bankcode;
  GtkWidget *countrycode;
  GtkWidget *ipaddr;
  //GtkWidget *port;

  /* user info page */
  GtkWidget *userpage;
  GtkWidget *user_bankcode;
  GtkWidget *user_bankname;
  GtkWidget *userid;
  GtkWidget *username;
  GtkWidget *customerid;
  GtkWidget *customername;
  GtkWidget *mediumrdh;
  GtkWidget *mediumpath;
  GtkWidget *mediumddv;

  /* account update info page */
  GtkWidget *accountinfopage;
  
  /* account match page */
  GtkWidget *accountpage;
  GtkWidget *accountlist;
    
  /* server iniletter info page */
  GtkWidget *serverinfopage;

  /* iniletter server */
  GtkWidget *serverpage;
  GtkWidget *server_vbox;
  GtkWidget *server_frame;
  gnc_html *server_html;

  /* user iniletter info page */
  GtkWidget *userinfopage;
  
  /* iniletter user */
  GtkWidget *user_vbox;
  GtkWidget *user_frame;
  gnc_html *user_html;

  /* OpenHBCI stuff */
  HBCI_API *api;
  GNCInteractor *interactor;

  /* account match: row_number (int) -> hbci_account */
  GHashTable *hbci_hash;
  /* hbci_account (direct) -> gnucash_account  -- DO NOT DELETE THE KEYS! */
  GHashTable *gnc_hash;

  /* Status of user's movement through the wizard */
  Infostate state;

  /* Newly created customer */
  const HBCI_Customer *newcustomer;
  /* Bank for which a new user is about to be created */
  const HBCI_Bank *newbank;

  /* Customer for which we already got the keys */
  const HBCI_Customer *gotkeysforCustomer;
  
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
reset_initial_info (HBCIInitialInfo *info)
{
  if (info == NULL) return;
  
  if (info->api != NULL) 
    HBCI_API_delete (info->api);
  info->newcustomer = NULL;
  info->newbank = NULL;
  
  if (info->configfile != NULL) 
    g_free (info->configfile);
    
  delete_hash (info->hbci_hash);
  if (info->gnc_hash != NULL)
    g_hash_table_destroy (info->gnc_hash);
}

static void
delete_initial_druid (HBCIInitialInfo *info)
{
  if (info == NULL) return;

  reset_initial_info (info);
  
  if (info->window != NULL) 
    gtk_widget_destroy (info->window);
    
  g_free (info);
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
update_accountlist_bank_cb (const HBCI_Bank *bank, gpointer user_data)
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
			  &update_accountlist_bank_cb,
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

/*******************************************************************
 *
 * Button enabling */
static void 
druid_enable_next_button(HBCIInitialInfo *info)
{
  g_assert(info);
  gnome_druid_set_buttons_sensitive (GNOME_DRUID (info->druid),
				     TRUE, TRUE, TRUE);
}
static void 
druid_disable_next_button(HBCIInitialInfo *info)
{
  g_assert(info);
  gnome_druid_set_buttons_sensitive (GNOME_DRUID (info->druid),
				     TRUE, FALSE, TRUE);
}
/*
 * end button enabling
 *******************************************************************/

/******************************************************************
 * string conversion 
 */
static char *
to_hexstring (const char *str)
{
  int i, bytes = strlen(str)/2;
  char *res = g_strnfill (3*bytes, ' ');
  for (i=0; i < bytes; i++) {
    res[3*i+0] = str[2*i+0];
    res[3*i+1] = str[2*i+1];
    if (i % 16 == 15)
      res[3*i+2] = '\n';
  }
  res [3*i+2] = '\0';
  //printf ("Converted -%s- to -%s-.\n", str, res);
  return res;
}
static char *
to_hexstring_hash (const char *str)
{
  int i, bytes = strlen(str)/2;
  char *res = g_strnfill (3*bytes, ' ');
  for (i=0; i < bytes; i++) {
    res[3*i+0] = str[2*i+0];
    res[3*i+1] = str[2*i+1];
    if (i % 10 == 9)
      res[3*i+2] = '\n';
  }
  res [3*i+2] = '\0';
  //printf ("Converted -%s- to -%s-.\n", str, res);
  return res;
}
/*
 * end string conversion 
 ***************************************************************/


static void *
print_list_int_cb (int value, void *user_data)
{
  printf("%d, ", value);
  return NULL;
}
static void 
print_list_int (const list_int *list)
{
  g_assert(list);
  list_int_foreach (list, &print_list_int_cb, NULL);
  printf ("\n");
}
static void *
get_resultcode_error_cb (int value, void *user_data)
{
  if (value >= 9000)
    return (void*) value;
  else
    return NULL;
}
static int
get_resultcode_error (const list_int *list)
{
  g_assert (list);
  return (int) list_int_foreach (list, &get_resultcode_error_cb, NULL);
}

static void 
debug_outboxjob (HBCI_OutboxJob *job, HBCIInitialInfo *info)
{
  list_int *list;
  const char *msg;
  int cause;
  
  g_assert (info);
  g_assert (job);
/*   if (HBCI_OutboxJob_status (job) != HBCI_JOB_STATUS_DONE) */
/*     return; */
/*   if (HBCI_OutboxJob_result (job) != HBCI_JOB_RESULT_FAILED) */
/*     return; */
  printf("OutboxJob failed. resultcodes were: \n");
  list = HBCI_OutboxJob_resultCodes (job);
  print_list_int (list);
  cause = get_resultcode_error (list);
  switch (cause) {
  case 9310:
    msg = "Schluessel noch nicht hinterlegt";
    break;
  case 9320:
    msg = "Schluessel noch nicht freigeschaltet";
    break;
  case 9330:
    msg = "Schluessel gesperrt";
    break;
  case 9340:
    msg = "Schluessel falsch";
    break;
  default:
    msg = "Unknown";
  }
  printf("Probable cause of error was: code %d, msg: %s\n", cause, msg);
  list_int_delete (list);
}


static const HBCI_Bank *
choose_one_bank (HBCIInitialInfo *info, int *list_size)
{
  const list_HBCI_Bank *banklist;
  g_assert (info);
  g_assert (list_size);

  /* Get HBCI bank and account list */
  banklist = HBCI_API_bankList (info->api);
  //printf("%d banks found.\n", list_HBCI_Bank_size (banklist));
  *list_size = list_HBCI_Bank_size (banklist);
  if (*list_size == 0) 
    return NULL;
  if (*list_size > 1) 
    return NULL;

  /* Get bank. */
  {
    const HBCI_Bank *bank;
    list_HBCI_Bank_iter *iter;

    iter = list_HBCI_Bank_begin (banklist);
    bank = list_HBCI_Bank_iter_get (iter);
    list_HBCI_Bank_iter_delete (iter);
    return bank;
  }
}


static const HBCI_Customer *
choose_customer (HBCIInitialInfo *info)
{
  const HBCI_Bank *bank;
  int banklist_size = 0;
  g_assert (info);

  bank = choose_one_bank (info, &banklist_size);

  if (banklist_size == 0) 
    return NULL;
  
  if (banklist_size > 1) {
    printf("choose_customer: oops, more than one bank found. Not yet implemented.\n");
    return NULL;
  }
  g_assert (bank);
  
  /* Get User list. */
  {
    const list_HBCI_User *userlist;

    userlist = HBCI_Bank_users (bank);
    
    if (list_HBCI_User_size (userlist) == 0) {
      printf("choose_customer: oops, no user found.\n");
      return NULL;
    }
    if (list_HBCI_User_size (userlist) > 1) {
      printf("choose_customer: oops, more than one user found; not yet implemented.\n");
      return NULL;
    }

    /* Get User */
    {
      const HBCI_User *user;
      list_HBCI_User_iter *iter;
      const list_HBCI_Customer *custlist;
      
      iter = list_HBCI_User_begin (userlist);
      user = list_HBCI_User_iter_get (iter);
      list_HBCI_User_iter_delete (iter);
      custlist = HBCI_User_customers (user);
      
      if (list_HBCI_Customer_size (custlist) == 0) {
	printf ("choose_customer: oops, no customer found.\n");
	return NULL;
      }
      if (list_HBCI_Customer_size (custlist) > 1) {
	printf ("choose_customer: oops, more than one customer found; not yet implemented.\n");
	return NULL;
      }

      /* Get customer */
      {
	const HBCI_Customer *cust;
	list_HBCI_Customer_iter *iter;

	iter = list_HBCI_Customer_begin (custlist);
	cust = list_HBCI_Customer_iter_get (iter);
	list_HBCI_Customer_iter_delete (iter);
	
	return cust;
      }
    }
  }
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
  g_assert (info);

  if (info->configfile &&
      (strcmp(info->configfile, 
	      gnc_hbci_get_book_configfile (gnc_get_current_book ())) != 0)) {
    /* Name of configfile has changed */
    gnc_hbci_set_book_configfile (gnc_get_current_book (), info->configfile);
  }
  
  {
    HBCI_Error *err;
    err = gnc_hbci_api_save (info->api);
    if (err != NULL) {
      if (!HBCI_Error_isOk (err)) 
	printf("on_finish: Error at saving OpenHBCI data: %s.\n",
	       HBCI_Error_message (err));
      HBCI_Error_delete (err);
    }
  }
  
  if (info->gnc_hash)
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
  
  {
    if ((info->configfile == NULL) ||
	(strcmp(filename, info->configfile) != 0)) {
      /* Name of configfile has changed, so reset everything */
      reset_initial_info (info);
      info->configfile = g_strdup (filename);
      /* Create new HBCI_API object, loading its data from filename */
      info->api = gnc_hbci_api_new (filename, TRUE, 
				    GTK_WIDGET (info->window), 
				    &(info->interactor));
    }
    else if (info->api == NULL)
      /* Create new HBCI_API object, loading its data from filename */
      info->api = gnc_hbci_api_new (filename, TRUE, 
				    GTK_WIDGET (info->window), 
				    &(info->interactor));

    api = info->api;
    g_free (filename);
    if (api == NULL)
      return TRUE;
  }

  /* Get HBCI bank and account list */
  {
    const list_HBCI_Bank *banklist;

    banklist = HBCI_API_bankList (api);
    //printf("%d banks found.\n", list_HBCI_Bank_size (banklist));
    if (list_HBCI_Bank_size (banklist) == 0) {
      // Zero banks? go to next page (create_bank)
      info->state = INI_ADD_BANK;
      gnome_druid_set_page (GNOME_DRUID (info->druid), 
			    GNOME_DRUID_PAGE (info->bankpage));
      return TRUE;
    }

    if (HBCI_API_totalUsers(api) == 0) {
      int dummy;
      // zero users? go to user-creation page
      info->state = INI_ADD_USER;
      info->newbank = choose_one_bank (info, &dummy);
      gnome_druid_set_page (GNOME_DRUID (info->druid), 
			    GNOME_DRUID_PAGE (info->userpage));
      return TRUE;
    }
    
    if (HBCI_API_totalAccounts(api) == 0) {
      // still no accounts? go to account update page
      info->state = INI_UPDATE_ACCOUNTS;
      info->newcustomer = choose_customer (info);
      gnome_druid_set_page (GNOME_DRUID (info->druid), 
			    GNOME_DRUID_PAGE (info->accountinfopage));
      return TRUE;
    }
  }

  info->state = INI_MATCH_ACCOUNTS;
  // accounts already exist? Then go to account matching page
  gnome_druid_set_page (GNOME_DRUID (info->druid), 
			GNOME_DRUID_PAGE (info->accountpage));
  return TRUE;
}
static void
on_configfile_activate (GtkEditable *editable,
			gpointer user_data)
{
  on_configfile_next (NULL, NULL, user_data);
}


static gboolean 
on_bankpage_back (GnomeDruidPage  *gnomedruidpage,
		  gpointer         arg1,
		  gpointer         user_data)
{
  HBCIInitialInfo *info = user_data;
  g_assert(info);
  
  switch (info->state) {
  case INI_ADD_BANK:
    return FALSE;
  case ADD_BANK:
    gnome_druid_set_page (GNOME_DRUID (info->druid), 
			  GNOME_DRUID_PAGE (info->accountpage));
    return TRUE;
  default:
  }
  return FALSE;
}
static gboolean 
on_bankpage_next (GnomeDruidPage  *gnomedruidpage,
		  gpointer         arg1,
		  gpointer         user_data)
{
  HBCIInitialInfo *info = user_data;
  const char *bankcode = NULL;
  int countrycode = 0;
  const char *ipaddr = NULL;//, *port;
  HBCI_Bank *bank = NULL;
  g_assert (info);
  g_assert (info->api);
  
  bankcode = gtk_entry_get_text (GTK_ENTRY (info->bankcode));
  countrycode = atoi (gtk_entry_get_text (GTK_ENTRY (info->countrycode)));
  ipaddr = gtk_entry_get_text (GTK_ENTRY (info->ipaddr));
  
  bank = HBCI_API_findBank(info->api, countrycode, bankcode);
  if (bank == NULL) {
    //printf("on_bankpage_next: Creating bank with code %s.\n", bankcode);
    bank = HBCI_API_bankFactory (info->api, countrycode, bankcode, ipaddr);
    {
      HBCI_Error *err;
      err = HBCI_API_addBank (info->api, bank, TRUE);
      if (err != NULL) {
	printf("on_bankpage_next-CRITICAL: Error at addBank: %s.\n",
	       HBCI_Error_message (err));
	HBCI_Error_delete (err);
	return TRUE;
      }
    }
  } 
  /*else {
    printf("on_bankpage_next: Found bank, name %s.\n", HBCI_Bank_name(bank));
    };*/
  info->newbank = bank;

  gnome_druid_set_page (GNOME_DRUID (info->druid), 
			GNOME_DRUID_PAGE (info->userpage));
  return TRUE;
}
/*static void
on_ipaddr_activate (GtkEditable *editable,
		    gpointer user_data)
{
  HBCIInitialInfo *info;
  g_assert (info);
  
  if ((strlen(gtk_entry_get_text (GTK_ENTRY (info->bankcode))) > 0) &&
      (strlen(gtk_entry_get_text (GTK_ENTRY (info->countrycode))) > 0) &&
      (strlen(gtk_entry_get_text (GTK_ENTRY (info->ipaddr))) > 0))
    on_bankpage_next (NULL, NULL, info);
    }*/

static void
on_userid_prepare (GnomeDruidPage *gnomedruidpage,
		   gpointer arg1,
		   gpointer user_data)
{
  HBCIInitialInfo *info = user_data;
  const char *bankcode, *bankname;
  g_assert (info);
  g_assert (info->newbank);
    
  bankcode = HBCI_Bank_bankCode (info->newbank);
  bankname = HBCI_Bank_name (info->newbank);
  
  gtk_label_set_text (GTK_LABEL (info->user_bankcode),
		      bankcode);
  if (bankname && (strlen (bankname) > 0))
    gtk_label_set_text (GTK_LABEL (info->user_bankname), bankname);
  else {
    gtk_label_set_text (GTK_LABEL (info->user_bankname), _("Unknown"));
    gtk_widget_set_sensitive (GTK_WIDGET (info->user_bankname), FALSE);
  }
  /* Let the widgets be redrawn */
  while (g_main_iteration (FALSE));
}

static gboolean 
on_userid_back (GnomeDruidPage  *gnomedruidpage,
		gpointer         arg1,
		gpointer         user_data)
{
  HBCIInitialInfo *info = user_data;
  g_assert(info);
  
  switch (info->state) {
  case INI_ADD_BANK:
  case ADD_BANK:
    return FALSE;
  case INI_ADD_USER:
    gnome_druid_set_page (GNOME_DRUID (info->druid), 
			  GNOME_DRUID_PAGE (info->filepage));
    return TRUE;
  case ADD_USER:
    gnome_druid_set_page (GNOME_DRUID (info->druid), 
			  GNOME_DRUID_PAGE (info->accountpage));
    return TRUE;
  default:
  }
  return FALSE;
}
static gboolean
on_userid_focus_out (GtkWidget *widget,
		     GdkEventFocus *event,
		     gpointer user_data)
{
  HBCIInitialInfo *info = user_data;
  const char *userid = NULL;
  const char *customerid = NULL;
  g_assert(info);
  
  userid = gtk_entry_get_text (GTK_ENTRY (info->userid));
  customerid = gtk_entry_get_text (GTK_ENTRY (info->customerid));

  if (strlen(customerid)==0)
    gtk_entry_set_text (GTK_ENTRY (info->customerid), userid);

  return FALSE;
}
static gboolean
on_userid_next (GnomeDruidPage  *gnomedruidpage,
		gpointer         arg1,
		gpointer         user_data)
{
  HBCIInitialInfo *info = user_data;
  const char *userid = NULL, *username = NULL;
  const char *customerid = NULL, *customername = NULL;
  HBCI_API *api = NULL;
  HBCI_Bank *bank = NULL;
  const HBCI_User *user = NULL;

  g_assert (info);
  api = info->api;
  g_assert (api);
  bank = (HBCI_Bank *)info->newbank;
  g_assert (bank);
  on_userid_focus_out (NULL, NULL, info);
      
  userid = gtk_entry_get_text (GTK_ENTRY (info->userid));
  username = gtk_entry_get_text (GTK_ENTRY (info->username));
  customerid = gtk_entry_get_text (GTK_ENTRY (info->customerid));
  customername = gtk_entry_get_text (GTK_ENTRY (info->customername));

  /*user = HBCI_Bank_findUser(bank, userid);
    if (user == NULL)*/ 
  user = NULL;
  {
    gboolean is_rdh;
    HBCI_Medium *medium;
    HBCI_User *newuser;
    HBCI_Error *err;
    char *mediumname;
    int secmode;

    //printf("on_userid_next: Didn't find user with userid %s.\n", userid);
    is_rdh = gtk_toggle_button_get_active 
      (GTK_TOGGLE_BUTTON (info->mediumrdh));

    if (is_rdh) {
      /* Create RDH Medium */ 
      mediumname = gnome_file_entry_get_full_path 
	(GNOME_FILE_ENTRY (info->mediumpath), FALSE);

      // Some sanity checks on the filename
      if (!gnc_verify_exist_or_new_file 
	  (GTK_WIDGET (info->window), mediumname)) {
	g_free (mediumname);
	return TRUE;
      }
      if (!gnc_test_dir_exist_error (GTK_WINDOW (info->window), 
				     mediumname)) {
	g_free (mediumname);
	return TRUE;
      }
      secmode = HBCI_SECURITY_RDH;
    }
    else {
      /* Create DDV Medium */
      mediumname = g_strdup("");
      secmode = HBCI_SECURITY_DDV;
    }

    medium = HBCI_API_createNewMedium (api, 
				       HBCI_Bank_countryCode (bank),
				       HBCI_Bank_bankCode (bank),
				       userid, 
				       mediumname, secmode, &err);
    g_free(mediumname);

    if (medium == NULL) {
      printf("on_userid_next: Couldn't create medium: %s\n", 
	     HBCI_Error_message (err));
      HBCI_Error_delete (err);
      return TRUE;
    }
    
    newuser = HBCI_API_userFactory (bank, medium, TRUE, userid);
    HBCI_User_setUserName (newuser, username);
    //printf("on_userid_next: Created user with userid %s.\n", userid);
    g_assert(newuser);
    err = HBCI_Bank_addUser (bank, newuser, TRUE);
    if (err != NULL) {
      printf("on_userid_next-CRITICAL: Error at addUser: %s.\n",
	     HBCI_Error_message (err));
      HBCI_Error_delete (err);
      return TRUE;
    }
      
    /* Test mounting only for DDV cards. RDH files should work... */
    if (secmode == HBCI_SECURITY_DDV) {
      err = HBCI_Medium_mountMedium (medium, newuser, NULL);
      if (err != NULL) {
	printf("on_userid_next: Mounting medium failed: %s.\n",
	       HBCI_Error_message (err));
	HBCI_Error_delete (err);
	return TRUE;
      } 
      printf("on_userid_next: Mounting medium was successful.\n");
      HBCI_Medium_unmountMedium (medium, NULL);
    }
      

    {
      HBCI_Customer *cust;
      cust = HBCI_API_customerFactory (newuser, customerid, 
				       customername ? customername : 
				       _("Default Customer"));
      g_assert (cust);
      HBCI_User_addCustomer (newuser, cust, TRUE);
      info->newcustomer = cust;
    }

    if (is_rdh) {
      gnome_druid_set_page (GNOME_DRUID (info->druid), 
			    GNOME_DRUID_PAGE (info->serverinfopage));
      return TRUE;
    } 
    else
      return FALSE;

  }/*
  else {
    printf("on_userid_next: Found user, name %s.\n", HBCI_User_userName(user));
    }*/

  return FALSE;
}

static gboolean 
on_accountinfo_back (GnomeDruidPage  *gnomedruidpage,
		     gpointer         arg1,
		     gpointer         user_data)
{
  HBCIInitialInfo *info = user_data;
  g_assert(info);
  
  switch (info->state) {
  case UPDATE_ACCOUNTS:
    gnome_druid_set_page (GNOME_DRUID (info->druid), 
			  GNOME_DRUID_PAGE (info->accountpage));
    return TRUE;
  case INI_UPDATE_ACCOUNTS:
    gnome_druid_set_page (GNOME_DRUID (info->druid), 
			  GNOME_DRUID_PAGE (info->filepage));
    return TRUE;
  default:
  }
  return FALSE;
}
static gboolean 
on_accountinfo_next (GnomeDruidPage  *gnomedruidpage,
		     gpointer         arg1,
		     gpointer         user_data)
{
  HBCIInitialInfo *info = user_data;
  g_assert(info);

  /* of course we need to know for which customer we do this */
  if (info->newcustomer == NULL) 
    info->newcustomer = choose_customer (info);
  
  if (info->newcustomer == NULL) 
    return FALSE;

  {
    /* Execute a GetAccounts job. */
    HBCI_OutboxJob *job;
    HBCI_Error *err;
    
    job = HBCI_OutboxJobGetAccounts_OutboxJob 
      (HBCI_OutboxJobGetAccounts_new ((HBCI_Customer *)info->newcustomer));
    HBCI_API_addJob (info->api, job);

    if (info->interactor)
      GNCInteractor_show (info->interactor);

    HBCI_Hbci_setDebugLevel(3);
    err = HBCI_API_executeQueue (info->api, TRUE);
    g_assert (err);
    if (!HBCI_Error_isOk(err)) {
      char *errstr = g_strdup_printf("on_accountinfo_next: Error at executeQueue: %s",
				     HBCI_Error_message (err));
      printf("%s; status %d, result %d\n", errstr, HBCI_OutboxJob_status(job),
	     HBCI_OutboxJob_result(job));
      HBCI_Interactor_msgStateResponse (HBCI_Hbci_interactor 
					(HBCI_API_Hbci (info->api)), errstr);
      g_free (errstr);
      HBCI_Error_delete (err);
      debug_outboxjob (job, info);
      return FALSE;
    }
    HBCI_API_clearQueueByStatus (info->api, HBCI_JOB_STATUS_DONE);
    HBCI_Error_delete (err);
  }
  //update_accountlist(info->api);
  
  return FALSE;
}




static gboolean 
on_accountlist_back (GnomeDruidPage  *gnomedruidpage,
		     gpointer         arg1,
		     gpointer         user_data)
{
  HBCIInitialInfo *info = user_data;
  g_assert(info);
  
  switch (info->state) {
  case INI_MATCH_ACCOUNTS:
  case MATCH_ACCOUNTS:
    gnome_druid_set_page (GNOME_DRUID (info->druid), 
			  GNOME_DRUID_PAGE (info->filepage));
    return TRUE;
  default:
  }
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
  
  gnome_druid_set_buttons_sensitive (GNOME_DRUID (info->druid),
				     FALSE, TRUE, TRUE);

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





static gboolean
on_iniletter_info_back (GnomeDruidPage  *gnomedruidpage,
			  gpointer arg1,
			  gpointer user_data)
{
  HBCIInitialInfo *info = user_data;
  g_assert(info);

  if (info->interactor)
    GNCInteractor_hide (info->interactor);
  
  gnome_druid_set_page (GNOME_DRUID (info->druid), 
			GNOME_DRUID_PAGE (info->userpage));
  return TRUE;
}




static gboolean
on_iniletter_info_next (GnomeDruidPage  *gnomedruidpage,
			  gpointer arg1,
			  gpointer user_data)
{
  HBCIInitialInfo *info = user_data;
  g_assert(info);

  if (info->newcustomer == NULL) 
    return FALSE;

  if (info->gotkeysforCustomer == NULL) {
    /* Execute a GetKey job. */
    HBCI_OutboxJob *job;
    HBCI_Error *err;
    
    {
      HBCI_Error *err;
      err = HBCI_API_saveEnvironment (info->api, info->configfile);
      if (err != NULL) {
	if (!HBCI_Error_isOk (err)) 
	  printf("on_iniletter_info_next: Error at saving OpenHBCI data: %s.\n",
		 HBCI_Error_message (err));
	HBCI_Error_delete (err);
      }
    }
    {
      HBCI_Error *err;
      err = HBCI_API_loadEnvironment (info->api, info->configfile);
      if (err != NULL) {
	if (!HBCI_Error_isOk (err)) 
	  printf("on_iniletter_info_next: Error at loading OpenHBCI data: %s.\n",
		 HBCI_Error_message (err));
	HBCI_Error_delete (err);
      }
    }

    job = HBCI_OutboxJobGetKeys_OutboxJob 
      (HBCI_OutboxJobGetKeys_new (info->api, info->newcustomer));
    HBCI_API_addJob (info->api, job);

    if (info->interactor)
      GNCInteractor_show (info->interactor);
  
    err = HBCI_API_executeQueue (info->api, TRUE);
    g_assert (err);
    if (!HBCI_Error_isOk(err)) {
      char *errstr = g_strdup_printf("on_iniletter_info_next: Error at executeQueue: %s",
				     HBCI_Error_message (err));
      printf("%s; status %d, result %d\n", errstr, HBCI_OutboxJob_status(job),
	     HBCI_OutboxJob_result(job));
      HBCI_Interactor_msgStateResponse (HBCI_Hbci_interactor (HBCI_API_Hbci (info->api)), errstr);
      g_free (errstr);
      HBCI_Error_delete (err);
      debug_outboxjob (job, info);
      return FALSE;
    }
    HBCI_Error_delete (err);
    HBCI_API_clearQueueByStatus (info->api, HBCI_JOB_STATUS_DONE);
    info->gotkeysforCustomer = info->newcustomer;

  }
  else if (info->gotkeysforCustomer != info->newcustomer) {
    printf("on_iniletter_info_next: Oops, already got keys for another customer. Not yet implemented.\n");
    return TRUE;
  }

  /* Create Ini-Letter */
  {
    char *res;
    const HBCI_Medium *med;
    const HBCI_MediumRDH *medr;
    const HBCI_Bank *bank;
    gboolean use_cryptkey;
    char *tmp, *hash, *exponent, *modulus;
    const char *bankcode, *bankname, *bankip;
    int keynumber, keyversion;
    time_t now = time(NULL);
    char *time_now = ctime(&now);

    bank = HBCI_User_bank (HBCI_Customer_user 
			   ((HBCI_Customer *)info->newcustomer));
    bankcode = HBCI_Bank_bankCode (bank);
    bankname = HBCI_Bank_name (bank);
    bankname = (strlen (bankname) > 0 ? bankname : _("Unknown"));
    bankip = HBCI_Bank_addr (bank);    

    med = HBCI_User_medium (HBCI_Customer_user
			    ((HBCI_Customer *)info->newcustomer));
    medr = HBCI_Medium_MediumRDH ((HBCI_Medium *)med);
    g_assert (medr);

    use_cryptkey = !HBCI_MediumRDH_hasInstSignKey (medr);
    tmp = HBCI_MediumRDH_getInstIniLetterHash(medr, use_cryptkey);
    hash = to_hexstring_hash (tmp);
    free (tmp);
    tmp = HBCI_MediumRDH_getInstIniLetterExponent(medr, use_cryptkey);
    exponent = to_hexstring (tmp);
    free (tmp);
    tmp = HBCI_MediumRDH_getInstIniLetterModulus(medr, use_cryptkey);
    modulus = to_hexstring (tmp);
    free (tmp);
    keynumber = HBCI_MediumRDH_getInstKeyNumber(medr, use_cryptkey);
    keyversion = HBCI_MediumRDH_getInstKeyVersion(medr, use_cryptkey);

    res = g_strdup_printf("<html><body><h1>Ini-Brief der Bank %s</h1>
<h2>Bankdaten</h2><table>
<tr><td>Bankleitzahl</td><td>%s</td></tr>
<tr><td>IP-Adresse</td><td>%s</td></tr>
<tr><td>Datum, Uhrzeit</td><td>%s</td></tr>
<tr><td>Schl&uuml;sselnummer</td><td>%d</td></tr>
<tr><td>Schl&uuml;sselversion</td><td>%d</td></tr>
</table>
<h2>Oeffentlicher Schl&uuml;ssel f&uuml;r die elektronische
Signatur</h2>
<h3>Exponent (768 Bit)</h3>
<pre>%s</pre>
<h3>Modulus (768 Bit)</h3>
<pre>%s</pre>
<h3>Hashwert</h3>
<pre>%s</pre>
</body></html>",
			  bankname, 
			  bankcode,
			  bankip,
			  time_now,
			  keynumber, keyversion,
			  exponent, modulus, hash);

    g_free (exponent);
    g_free (modulus);
    g_free (hash);
    
    gnc_html_show_data (info->server_html, res, strlen(res));
    
    g_free (res);
  }
  
  return FALSE;
}


static void
on_iniletter_server_prepare (GnomeDruidPage *gnomedruidpage,
			     gpointer arg1,
			     gpointer user_data)
{
  HBCIInitialInfo *info = user_data;
  g_assert(info);

  /* Workaround to get let the GtkHTML scrollbars appear. */
  gtk_box_set_spacing (GTK_BOX (info->server_vbox), 1);
  gtk_widget_queue_resize (GTK_WIDGET (info->server_vbox));
  while (g_main_iteration (FALSE));
  gtk_box_set_spacing (GTK_BOX (info->server_vbox), 0);
  gtk_widget_queue_resize (GTK_WIDGET (info->server_vbox));
  
  druid_disable_next_button (info);

  /* Let the widgets be redrawn */
  while (g_main_iteration (FALSE));
}

static gboolean
on_iniletter_userinfo_next (GnomeDruidPage  *gnomedruidpage,
			  gpointer arg1,
			  gpointer user_data)
{
  HBCIInitialInfo *info = user_data;
  char *res;
  g_assert (info);

  if (info->newcustomer == NULL)
    return FALSE;

  if (info->gotkeysforCustomer == info->newcustomer) {
    /* Execute a SendKey job. */
    HBCI_OutboxJob *job;
    HBCI_Error *err;
    
    job = HBCI_OutboxJobSendKeys_OutboxJob 
      (HBCI_OutboxJobSendKeys_new (info->api, info->newcustomer));
    HBCI_API_addJob (info->api, job);

    if (info->interactor)
      GNCInteractor_show (info->interactor);
  
    HBCI_Hbci_setDebugLevel(3);
    err = HBCI_API_executeQueue (info->api, TRUE);
    g_assert (err);
    if (!HBCI_Error_isOk(err)) {
      char *errstr = g_strdup_printf("on_iniletter_userinfo_next: Error at executeQueue: %s",
				     HBCI_Error_message (err));
      printf("%s; status %d, result %d\n", errstr, HBCI_OutboxJob_status(job),
	     HBCI_OutboxJob_result(job));
      HBCI_Interactor_msgStateResponse (HBCI_Hbci_interactor (HBCI_API_Hbci (info->api)), errstr);
      g_free (errstr);
      HBCI_Error_delete (err);
      debug_outboxjob (job, info);
      return FALSE;
    }
    HBCI_Error_delete (err);
    HBCI_API_clearQueueByStatus (info->api, HBCI_JOB_STATUS_DONE);
  }
  else {
    printf("on_iniletter_userinfo_next: Oops, already got keys for another customer. Not yet implemented.\n");
    return TRUE;
  }

  {
    const HBCI_Medium *med;
    const HBCI_MediumRDH *medr;
    int keynumber, keyversion;
    char *tmp, *hash, *exponent, *modulus;
    const HBCI_User *user;
    HBCI_Customer *cust;
    time_t now = time(NULL);
    char *time_now = ctime(&now);

    cust = (HBCI_Customer *)info->newcustomer;
    user = HBCI_Customer_user (cust);
    med = HBCI_User_medium (user);
    medr = HBCI_Medium_MediumRDH ((HBCI_Medium *)med);
    g_assert (medr);

    tmp = HBCI_MediumRDH_getUserIniLetterHash(medr);
    hash = to_hexstring_hash (tmp);
    g_free (tmp);
    tmp = HBCI_MediumRDH_getUserIniLetterExponent(medr);
    exponent = to_hexstring (tmp);
    g_free (tmp);
    tmp = HBCI_MediumRDH_getUserIniLetterModulus(medr);
    modulus = to_hexstring (tmp);
    g_free (tmp);
    keynumber = HBCI_MediumRDH_getUserKeyNumber(medr);
    keyversion = HBCI_MediumRDH_getUserKeyVersion(medr);

    res = g_strdup_printf("<html><body><h1>Ini-Brief</h1>
<h2>Benutzerdaten</h2><table>
<tr><td>Benutzername</td><td>%s</td></tr>
<tr><td>Datum, Uhrzeit</td><td>%s</td></tr>
<tr><td>Benutzerkennung</td><td>%s</td></tr>
<tr><td>Schl&uuml;sselnummer</td><td>%d</td></tr>
<tr><td>Schl&uuml;sselversion</td><td>%d</td></tr>
<tr><td>Kundensystemkennung</td><td>%s</td></tr>
</table>
<h2>Oeffentlicher Schl&uuml;ssel f&uuml;r die elektronische
Signatur</h2>
<h3>Exponent</h3>
<pre>%s</pre>
<h3>Modulus</h3>
<pre>%s</pre>
<h3>Hash</h3>
<pre>%s</pre>
<p>&nbsp;</p>
<hr>
Ort, Datum, Unterschrift</body></html>",
			  HBCI_User_userName (user),
			  time_now,
			  HBCI_User_userId (user),
			  keynumber, keyversion,
			  HBCI_Hbci_systemName (HBCI_API_Hbci (info->api)),
			  exponent, modulus, hash);
    
    g_free (exponent);
    g_free (modulus);
    g_free (hash);
  }
  gnc_html_show_data (info->user_html, res, strlen(res));

  g_free (res);

  return FALSE;
}

static void
on_iniletter_user_prepare (GnomeDruidPage  *gnomedruidpage,
			gpointer arg1,
			gpointer user_data)
{
  HBCIInitialInfo *info = user_data;
  g_assert (info);
  
  /* Workaround to get let the GtkHTML scrollbars appear. */
  gtk_box_set_spacing (GTK_BOX (info->user_vbox), 1);
  gtk_widget_queue_resize (GTK_WIDGET (info->user_vbox));
  while (g_main_iteration (FALSE));
  gtk_box_set_spacing (GTK_BOX (info->user_vbox), 0);
  gtk_widget_queue_resize (GTK_WIDGET (info->user_vbox));

  druid_disable_next_button (info);

  while (g_main_iteration (FALSE));
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
on_button_clicked (GtkButton *button,
		   gpointer user_data)
{
  HBCIInitialInfo *info = user_data;
  char *name;
  g_assert(info->druid);
  g_assert(info->userpage);
  
  name = gtk_widget_get_name (GTK_WIDGET (button));
  if (strcmp (name, "addbank_button") == 0) {
    info->state = ADD_BANK;
    gnome_druid_set_page (GNOME_DRUID (info->druid), 
			  GNOME_DRUID_PAGE (info->bankpage));
  } else if (strcmp (name, "adduser_button") == 0) {
    int dummy;
    info->state = ADD_USER;
    info->newbank = choose_one_bank (info, &dummy);
    gnome_druid_set_page (GNOME_DRUID (info->druid), 
			  GNOME_DRUID_PAGE (info->userpage));
  } else if (strcmp (name, "updatelist_button") == 0) {
    info->state = UPDATE_ACCOUNTS;
    info->newcustomer = choose_customer (info);
    // Nothing else to do.
    gnome_druid_set_page (GNOME_DRUID (info->druid), 
			  GNOME_DRUID_PAGE (info->accountinfopage));
  } else if (strcmp (name, "serveryes_button") == 0) {
    druid_enable_next_button (info);
  } else if (strcmp (name, "serverno_button") == 0) {
    druid_disable_next_button (info);
    gnc_error_dialog_parented(GTK_WINDOW (info->window),
			      _("Since the cryptographic keys of the bank cannot be verified, 
you should stop contacting this Server Internet Address
and contact your bank. To help your bank figure out the 
problem, you should print out this erroneous Ini-Letter 
and show it to your bank. Please abort the HBCI setup
druid now."));
  } else if (strcmp (name, "serverprint_button") == 0) {
    gnc_html_print (info->server_html);
  } else if (strcmp (name, "userprint_button") == 0) {
    druid_enable_next_button (info);
    gnc_html_print (info->user_html);
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
    page = glade_xml_get_widget(xml, "configfile_page");
    info->filepage = page;
    info->configfileentry = 
      glade_xml_get_widget(xml, "configfile_fileentry");
    gtk_signal_connect 
      (GTK_OBJECT (gnome_file_entry_gtk_entry
		  (GNOME_FILE_ENTRY (info->configfileentry))), 
       "activate", 
       GTK_SIGNAL_FUNC (on_configfile_activate), info);
    /* Set the saved filename, if that is a valid file */
    info->configfile =
      g_strdup (gnc_hbci_get_book_configfile (gnc_get_current_book () ));
    if (info->configfile && 
	g_file_test (info->configfile, 
		     G_FILE_TEST_ISFILE | G_FILE_TEST_ISLINK)) 
      gtk_entry_set_text 
	(GTK_ENTRY (gnome_file_entry_gtk_entry
		    (GNOME_FILE_ENTRY (info->configfileentry))), 
	 info->configfile);
    else {
      const char *homedir = g_get_home_dir();
      char *file = NULL;
      g_assert(homedir);
      file = g_strdup_printf("%s/.openhbci", homedir);
      gtk_entry_set_text 
	(GTK_ENTRY (gnome_file_entry_gtk_entry
		    (GNOME_FILE_ENTRY (info->configfileentry))), 
	 file);
      g_free (file);
    }
      
    gtk_signal_connect (GTK_OBJECT (page), "next", 
			GTK_SIGNAL_FUNC (on_configfile_next), info);
  }
  {
    page = glade_xml_get_widget(xml, "bank_page");
    info->bankpage = page;
    info->bankcode = glade_xml_get_widget(xml, "bank_code_entry");
    info->countrycode = glade_xml_get_widget(xml, "country_code_entry");
    info->ipaddr = glade_xml_get_widget(xml, "ip_address_entry");
    /*gtk_signal_connect (GTK_OBJECT (info->ipaddr), "activate", 
			GTK_SIGNAL_FUNC (on_ipaddr_activate), info);*/
    gtk_signal_connect (GTK_OBJECT (page), "back", 
			GTK_SIGNAL_FUNC (on_bankpage_back), info);
    gtk_signal_connect (GTK_OBJECT (page), "next", 
			GTK_SIGNAL_FUNC (on_bankpage_next), info);
  }
  {
    page = glade_xml_get_widget(xml, "user_page");
    info->userpage = page;
    info->user_bankcode = glade_xml_get_widget(xml, "user_bankcode_label");
    info->user_bankname = glade_xml_get_widget(xml, "user_bankname_label");
    info->userid = glade_xml_get_widget(xml, "user_id_entry");
    info->username = glade_xml_get_widget(xml, "user_name_entry");
    gtk_signal_connect (GTK_OBJECT (info->userid), "focus-out-event", 
			GTK_SIGNAL_FUNC (on_userid_focus_out), info);
    info->customerid = glade_xml_get_widget(xml, "customer_id_entry");
    info->customername = glade_xml_get_widget(xml, "customer_name_entry");
    info->mediumrdh = glade_xml_get_widget(xml, "rdh_radiobutton");
    info->mediumpath = glade_xml_get_widget(xml, "keyfile_fileentry");
    info->mediumddv = glade_xml_get_widget(xml, "ddv_radiobutton");
    {
      char *curdir = g_get_current_dir();
      gtk_entry_set_text 
	(GTK_ENTRY (gnome_file_entry_gtk_entry
		    (GNOME_FILE_ENTRY (info->mediumpath))), 
	 curdir);
      g_free (curdir);
    }
    // no libchipcard? Make that button greyed out
    if (! HBCI_Hbci_hasLibchipcard ()) 
      gtk_widget_set_sensitive (GTK_WIDGET (info->mediumddv),
				FALSE);
    gtk_signal_connect (GTK_OBJECT (page), "back", 
			GTK_SIGNAL_FUNC (on_userid_back), info);
    gtk_signal_connect (GTK_OBJECT (page), "prepare", 
			GTK_SIGNAL_FUNC (on_userid_prepare), info);
    gtk_signal_connect (GTK_OBJECT (page), "next", 
			GTK_SIGNAL_FUNC (on_userid_next), info);
  }
  {
    page = glade_xml_get_widget(xml, "account_info_page");
    info->accountinfopage = page;
    gtk_signal_connect (GTK_OBJECT (page), "back", 
			GTK_SIGNAL_FUNC (on_accountinfo_back), info);
    gtk_signal_connect (GTK_OBJECT (page), "next", 
			GTK_SIGNAL_FUNC (on_accountinfo_next), info);
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
    gtk_signal_connect (GTK_OBJECT (page), "back", 
			GTK_SIGNAL_FUNC (on_accountlist_back), info);
  }
  {
    page = glade_xml_get_widget (xml, "iniletter_info_page");
    info->serverinfopage = page;
    gtk_signal_connect (GTK_OBJECT (page), "back", 
			GTK_SIGNAL_FUNC (on_iniletter_info_back), info);
    gtk_signal_connect (GTK_OBJECT (page), "next", 
			GTK_SIGNAL_FUNC (on_iniletter_info_next), info);
  }
  {
    page = glade_xml_get_widget(xml, "iniletter_server_page");
    info->serverpage = page;
    info->server_vbox = glade_xml_get_widget(xml, "iniletter_server_vbox");
    info->server_frame = glade_xml_get_widget(xml, "iniletter_server_frame");
    info->server_html = gnc_html_new();
    gtk_container_add (GTK_CONTAINER (info->server_frame), 
		       gnc_html_get_widget (info->server_html));

    gtk_signal_connect (GTK_OBJECT 
			(glade_xml_get_widget (xml, "serveryes_button")), 
			"clicked",
			GTK_SIGNAL_FUNC (on_button_clicked), info);
    gtk_signal_connect (GTK_OBJECT 
			(glade_xml_get_widget (xml, "serverno_button")), 
			"clicked",
			GTK_SIGNAL_FUNC (on_button_clicked), info);
    gtk_signal_connect (GTK_OBJECT 
			(glade_xml_get_widget (xml, "serverprint_button")), 
			"clicked",
			GTK_SIGNAL_FUNC (on_button_clicked), info);
    gtk_signal_connect (GTK_OBJECT (page), "prepare", 
			GTK_SIGNAL_FUNC (on_iniletter_server_prepare), info);
    /*gtk_signal_connect (GTK_OBJECT (page), "next", 
      GTK_SIGNAL_FUNC (on_iniletter_server_next), info);*/
  }
  {
    page = glade_xml_get_widget (xml, "iniletter_userinfo_page");
    info->userinfopage = page;
    gtk_signal_connect (GTK_OBJECT (page), "next", 
			GTK_SIGNAL_FUNC (on_iniletter_userinfo_next), info);
  }
  {
    page = glade_xml_get_widget(xml, "iniletter_user_page");
    info->user_vbox = glade_xml_get_widget(xml, "iniletter_user_vbox");
    info->user_frame = glade_xml_get_widget(xml, "iniletter_user_frame");
    info->user_html = gnc_html_new();
    gtk_container_add (GTK_CONTAINER (info->user_frame), 
		       gnc_html_get_widget (info->user_html));

    gtk_signal_connect (GTK_OBJECT 
			(glade_xml_get_widget (xml, "userprint_button")), 
			"clicked",
			GTK_SIGNAL_FUNC (on_button_clicked), info);
    gtk_signal_connect (GTK_OBJECT (page), "next", 
			GTK_SIGNAL_FUNC (on_iniletter_user_next), info);
    gtk_signal_connect (GTK_OBJECT (page), "prepare", 
			GTK_SIGNAL_FUNC (on_iniletter_user_prepare), info);
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
