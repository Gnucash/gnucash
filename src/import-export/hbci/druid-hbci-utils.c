/********************************************************************\
 * druid-hbci-utils.c -- hbci creation functionality              *
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

#include "druid-hbci-utils.h"
#include "gnc-hbci-kvp.h"
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



/**
 * Save the reference strings to the HBCI accounts in the kvp's of the
 * gnucash accounts.*/
static void
accounts_save_kvp_cb (gpointer key, gpointer value, gpointer user_data)
{
  HBCI_Account *hbci_acc = key;
  Account *gnc_acc = value;
  g_assert(hbci_acc);
  g_assert(gnc_acc);

  if ((gnc_hbci_get_account_accountid(gnc_acc) == NULL ) ||
      (strcmp (gnc_hbci_get_account_accountid(gnc_acc), 
	       HBCI_Account_accountId (hbci_acc)) != 0))
    gnc_hbci_set_account_accountid 
      (gnc_acc, HBCI_Account_accountId (hbci_acc));

  if ((gnc_hbci_get_account_bankcode(gnc_acc) == NULL) ||
      (strcmp (gnc_hbci_get_account_bankcode(gnc_acc), 
	       HBCI_Bank_bankCode (HBCI_Account_bank (hbci_acc))) != 0))
    gnc_hbci_set_account_bankcode
      (gnc_acc, HBCI_Bank_bankCode (HBCI_Account_bank (hbci_acc)));

  if (gnc_hbci_get_account_countrycode(gnc_acc) !=
      HBCI_Bank_countryCode (HBCI_Account_bank (hbci_acc)))
    gnc_hbci_set_account_countrycode
      (gnc_acc, HBCI_Bank_countryCode (HBCI_Account_bank (hbci_acc)));
}

/* hash is a DIRECT hash from each HBCI account to each gnucash
   account. */
void
accounts_save_kvp (GHashTable *hash)
{
  g_assert(hash);
  g_hash_table_foreach (hash, &accounts_save_kvp_cb, NULL);
}
/*
******************************************************/



static void 
update_accounts_forbank (HBCI_API *api, const HBCI_Bank *bank);
static void 
update_accounts_foruser (HBCI_API *api, const HBCI_User *user);
static HBCI_Error *
update_accounts_forcustomer (HBCI_API *api, const HBCI_Customer *cust);

void
update_accounts (HBCI_API *api) 
{
  const list_HBCI_Bank *banklist;
  list_HBCI_Bank_iter *begin;
  g_assert(api);

  banklist = HBCI_API_bankList (api);
  //printf("%d banks found.\n", list_HBCI_Bank_size (banklist));
  if (list_HBCI_Bank_size (banklist) == 0) {
    // Zero banks? nothing to do.
    return;
  }
  else if (list_HBCI_Bank_size (banklist) == 1) {
    begin = list_HBCI_Bank_begin (banklist);
    update_accounts_forbank (api, list_HBCI_Bank_iter_get (begin));
    list_HBCI_Bank_iter_delete (begin);
  }
  else {
    printf("Sorry, multiple banks not yet supported.\n");
  }
}
static void 
update_accounts_forbank (HBCI_API *api, const HBCI_Bank *bank)
{
  const list_HBCI_User *userlist;
  list_HBCI_User_iter *begin;
  g_assert(bank);

  userlist = HBCI_Bank_users (bank);
  if (list_HBCI_User_size (userlist) == 0) {
    printf("update_accounts_forbank: Oops, zero users found.\n");
    // Zero users? nothing to do.
    return;
  }
  else if (list_HBCI_User_size (userlist) == 1) {
    begin = list_HBCI_User_begin (userlist);
    update_accounts_foruser (api, list_HBCI_User_iter_get (begin));
    list_HBCI_User_iter_delete (begin);
  }
  else {
    printf("update_accounts_forbank: Sorry, multiple users not yet supported.\n");
  }
}
static void 
update_accounts_foruser (HBCI_API *api, const HBCI_User *user)
{
  const list_HBCI_Customer *customerlist;
  list_HBCI_Customer_iter *begin;
  HBCI_Error *err;
  g_assert(user);

  customerlist = HBCI_User_customers (user);
  if (list_HBCI_Customer_size (customerlist) == 0) {
    printf("update_accounts_foruser: Oops, zero customers found.\n");
    // Zero customers? nothing to do.
    return;
  }
  else if (list_HBCI_Customer_size (customerlist) == 1) {
    begin = list_HBCI_Customer_begin (customerlist);
    err = update_accounts_forcustomer (api, 
				       list_HBCI_Customer_iter_get (begin));
    if (!HBCI_Error_isOk(err)) {
      char *errstr = HBCI_Error_errorString(err);
      fprintf (stderr, "update_accounts_foruser: Encountered an error: %s\n",
	       errstr);
      free (errstr);
      HBCI_Error_delete (err);
      return;
    }
    HBCI_Error_delete (err);
    list_HBCI_Customer_iter_delete (begin);
  }
  else {
    printf("update_accounts_foruser: Sorry, multiple customers not yet supported.\n");
  }
}
static HBCI_Error *
update_accounts_forcustomer (HBCI_API *api, const HBCI_Customer *cust)
{
  HBCI_OutboxJobGetAccounts* job; 
  g_assert(cust);
  
  // this const-warning is okay and can be ignored.
  job = HBCI_OutboxJobGetAccounts_new((HBCI_Customer *)cust); 
  HBCI_API_addJob(api, HBCI_OutboxJobGetAccounts_OutboxJob(job));
    
  // execute queue
  return HBCI_API_executeQueue(api, 1);
}





struct hbci_acc_cb_data 
{
  HBCI_API *api;
  GHashTable *hash;
};

static gpointer 
gnc_hbci_new_hash_from_kvp_cb (Account *gnc_acc, gpointer user_data)
{
  struct hbci_acc_cb_data *data = user_data;
  HBCI_Account *hbci_acc = NULL;

  hbci_acc = (HBCI_Account *) gnc_hbci_get_hbci_acc (data->api, gnc_acc);
  if (hbci_acc) {
    g_hash_table_insert (data->hash, hbci_acc, gnc_acc);
  }
  return NULL;
}

GHashTable *
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
			     &gnc_hbci_new_hash_from_kvp_cb,
			     &data, TRUE);
  }
  return hash;
}


gboolean 
gnc_verify_exist_or_new_file (GtkWidget *parent, const char *filename)
{
  g_assert (parent);
  
  if (g_file_test (filename, G_FILE_TEST_ISFILE | G_FILE_TEST_ISLINK)) {
    return TRUE;
  }

  return gnc_verify_dialog_parented
    (parent, TRUE,
     _("The file %s does not exist. \n"
"Would you like to create it now?"), 
     filename ? filename : _("(null"));
}

gboolean
gnc_test_dir_exist_error (GtkWindow *parent, const char *filename) 
{
  char *dirname = g_dirname (filename);
  gboolean dirtest = g_file_test (dirname, G_FILE_TEST_ISDIR);
  g_free (dirname);
  if (!dirtest) {
    gnc_error_dialog_parented
      (parent, 
       _("The directory for file\n"
"%s\n"
"does not exist. \n"
"Please choose another place for this file."), 
       filename ? filename : _("(null"));
    return FALSE;
  }
  return TRUE;
}

