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
#include <openhbci2/api.h>
#include <openhbci2/outboxjob.h>

#include "gnc-hbci-utils.h"

/**
 * Save the reference strings to the HBCI accounts in the kvp's of the
 * gnucash accounts.*/
static void
accounts_save_kvp_cb (gpointer key, gpointer value, gpointer user_data)
{
  gnc_HBCI_Account *hbci_acc = key;
  Account *gnc_acc = value;
  g_assert(hbci_acc);
  g_assert(gnc_acc);

  if ((gnc_hbci_get_account_accountid(gnc_acc) == NULL ) ||
      (strcmp (gnc_hbci_get_account_accountid(gnc_acc), 
	       gnc_HBCI_Account_accountId (hbci_acc)) != 0))
    gnc_hbci_set_account_accountid 
      (gnc_acc, gnc_HBCI_Account_accountId (hbci_acc));

  if ((gnc_hbci_get_account_bankcode(gnc_acc) == NULL) ||
      (strcmp (gnc_hbci_get_account_bankcode(gnc_acc), 
	       HBCI_Bank_bankCode (gnc_HBCI_Account_bank (hbci_acc))) != 0))
    gnc_hbci_set_account_bankcode
      (gnc_acc, HBCI_Bank_bankCode (gnc_HBCI_Account_bank (hbci_acc)));

  if (gnc_hbci_get_account_countrycode(gnc_acc) !=
      HBCI_Bank_country (gnc_HBCI_Account_bank (hbci_acc)))
    gnc_hbci_set_account_countrycode
      (gnc_acc, HBCI_Bank_country (gnc_HBCI_Account_bank (hbci_acc)));
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
update_accounts_forbank (GtkWidget *parent, HBCI_API *api, 
			 const HBCI_Bank *bank, 
			 GNCInteractor *inter);
static void 
update_accounts_foruser (GtkWidget *parent, HBCI_API *api, 
			 const HBCI_User *user, 
			 GNCInteractor *inter);
static gboolean
update_accounts_forcustomer (GtkWidget *parent, HBCI_API *api, 
			     const HBCI_Customer *cust, 
			     GNCInteractor *inter);


void
update_accounts (GtkWidget *parent, HBCI_API *api, GNCInteractor *inter) 
{
  const list_HBCI_Bank *banklist;
  list_HBCI_Bank_iter *begin;
  g_assert(api);

  banklist = HBCI_API_bankList (api);
  /*printf("%d banks found.\n", list_HBCI_Bank_size (banklist)); */
  if (list_HBCI_Bank_size (banklist) == 0) {
    /* Zero banks? nothing to do. */
    return;
  }
  else if (list_HBCI_Bank_size (banklist) == 1) {
    begin = list_HBCI_Bank_begin (banklist);
    update_accounts_forbank (parent, api, 
			     list_HBCI_Bank_iter_get (begin), inter);
    list_HBCI_Bank_iter_delete (begin);
  }
  else {
    printf("Sorry, multiple banks not yet supported.\n");
  }
}
static void 
update_accounts_forbank (GtkWidget *parent, HBCI_API *api, 
			 const HBCI_Bank *bank, 
			 GNCInteractor *inter)
{
  const list_HBCI_User *userlist;
  const HBCI_User *user;
  g_assert(bank);

  userlist = HBCI_Bank_users (bank);
  if (list_HBCI_User_size (userlist) == 0) {
    printf("update_accounts_forbank: Oops, zero users found.\n");
    /* Zero users? nothing to do. */
    return;
  }
  user = choose_one_user (GNCInteractor_parent(inter), userlist);
  update_accounts_foruser (parent, api, user, inter);
}
static void 
update_accounts_foruser (GtkWidget *parent, HBCI_API *api, 
			 const HBCI_User *user, 
			 GNCInteractor *inter)
{
  const list_HBCI_Customer *customerlist;
  const HBCI_Customer *customer;
  g_assert(user);

  customerlist = HBCI_User_customers (user);
  if (list_HBCI_Customer_size (customerlist) == 0) {
    printf("update_accounts_foruser: Oops, zero customers found.\n");
    /* Zero customers? nothing to do. */
    return;
  }
  customer = choose_one_customer (GNCInteractor_parent(inter), customerlist);
  update_accounts_forcustomer (parent, api, customer, inter);
}

static gboolean
update_accounts_forcustomer (GtkWidget *parent, HBCI_API *api, 
			     const HBCI_Customer *cust, GNCInteractor *inter)
{
  HBCI_OutboxJob *job;
  HBCI_Outbox *outbox;
  gboolean result;
  g_assert(cust);
  
  /* this const-warning is okay and can be ignored. */
  job = HBCI_OutboxJob_new("JobGetAccounts", (HBCI_Customer *)cust, ""); 
  outbox = HBCI_Outbox_new();
  
  HBCI_Outbox_addJob(outbox, job);
  
  /* Execute Outbox. */
  result = gnc_hbci_api_execute (parent, api, outbox, job, inter);

  HBCI_Outbox_delete (outbox);
  return result;
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
  gnc_HBCI_Account *hbci_acc = NULL;

  hbci_acc = (gnc_HBCI_Account *) gnc_hbci_get_hbci_acc (data->api, gnc_acc);
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

  return gnc_verify_dialog
    (parent, TRUE,
     _("The file %s does not exist. \n"
"Would you like to create it now?"), 
     filename ? filename : _("(null)"));
}

gboolean
gnc_test_dir_exist_error (GtkWindow *parent, const char *filename) 
{
  char *dirname = g_dirname (filename);
  gboolean dirtest = g_file_test (dirname, G_FILE_TEST_ISDIR);
  g_free (dirname);
  if (!dirtest) {
    gnc_error_dialog
      (GTK_WIDGET (parent), 
       _("The directory for file\n"
"%s\n"
"does not exist. \n"
"Please choose another place for this file."), 
       filename ? filename : _("(null)"));
    return FALSE;
  }
  return TRUE;
}



GList *
gnc_processOutboxResponse(HBCI_API *api, HBCI_Outbox *outbox, 
			  GList *accountlist)
{
  GWEN_DB_NODE *response, *n;

  g_assert(api);
  g_assert(outbox);
  g_assert(accountlist);
  
  response = HBCI_Outbox_response(outbox);
  n=GWEN_DB_GetFirstGroup(response);
  while (n){
    if (strcasecmp(GWEN_DB_GroupName(n), "accountdata")==0) {
      /* found account data, create account */
      const char *accountId;
      const char *accountSubId;
      const char *bankCode;
      int country;
      gnc_HBCI_Account *acc;
      const char *custid;
      /*list_HBCI_Customer *customers;*/
      /*HBCI_User *user;*/
      HBCI_Bank *bank;

      country=GWEN_DB_GetIntValue(n, "country", 0, 280);
      custid=GWEN_DB_GetCharValue(n, "customer", 0, "");
      bankCode=GWEN_DB_GetCharValue(n, "bankcode", 0, "");
      accountId=GWEN_DB_GetCharValue(n, "accountid", 0, "");
      accountSubId=GWEN_DB_GetCharValue(n, "accountsubid", 0, "");
      if (strlen(bankCode)==0 || strlen(accountId)==0 || strlen(custid)==0) {
        fprintf(stderr, "gnc_processOutboxResponse: AccountData without bank code/account id/customer id");
	continue;
      }

      bank = HBCI_API_findBank(api, country, bankCode);
      if (bank) {
        /* bank uses a different bank code for the accounts, so find
         * the matching bank for the given customer */
        /*customers=getCustomers(country, "*", custid);
	  if (customers.empty()) {
          DBG_ERROR(0, "Unknown customer %d/%s/%s",
	  country, bankCode.c_str(), custid.c_str());
          GWEN_DB_Group_free(db);
          return Error("AqMoneyAPI::processOutboxResponse",
	  ERROR_LEVEL_NORMAL,
	  HBCI_ERROR_CODE_UNKNOWN,
	  ERROR_ADVISE_DONTKNOW,
	  "Unknown customer");
	  }
	  if (customers.size()!=1) {
          DBG_ERROR(0, "Ambiguous customer %d/%s/%s",
	  country, bankCode.c_str(), custid.c_str());
          GWEN_DB_Group_free(db);
          return Error("AqMoneyAPI::processOutboxResponse",
	  ERROR_LEVEL_NORMAL,
	  HBCI_ERROR_CODE_UNKNOWN,
	  ERROR_ADVISE_DONTKNOW,
	  "Ambiguous customer");
	  }
	  user=customers.front().ref().user();
	  bank=user.ref().bank();*/
      }

      /* Create new account object */
      /* FIXME: Store more information here. For now, we stop with
	 this essential information. */
      acc = gnc_HBCI_Account_new(bank, bankCode, accountId);
      
      {
	/* Check if such an account already exists */
	gnc_HBCI_Account *found_account = 
	  list_HBCI_Account_foreach(accountlist, hbci_find_acc_cb, acc);
	
	if (found_account) {
	  const char *p;

	  /* Update account information */
	  printf("Account %d/%s/%s already exists, updating",
		 country, bankCode, accountId);

	  p=GWEN_DB_GetCharValue(n, "name", 0, 0);
	  /*if (p)
	    gnc_HBCI_Account_setAccountName(found_account,p);
	    p=GWEN_DB_GetCharValue(n, "customer", 0, 0);
	    if (p)
	    gnc_HBCI_Account_setCustomer(found_account,p);
	    p=GWEN_DB_GetCharValue(n, "currency", 0, 0);
	    if (p)
	    gnc_HBCI_Account_setCurrency(found_account,p);
	    p=GWEN_DB_GetCharValue(n, "name1", 0, 0);
	    if (p)
	    gnc_HBCI_Account_setUserName(found_account,p);*/
	  gnc_HBCI_Account_delete(acc);
	}
	else {
	  /* new account already created */

	  /* Add it to our internal list. */
	  accountlist = g_list_append(accountlist, acc);

	  printf("Added account %d/%s/%s",
		 country, bankCode, accountId);
	}
      }
      

    } /* if "AccountData" */
    else if (strcasecmp(GWEN_DB_GroupName(n), "bankmsg")==0) {
      /* add to existing bank messages */
      /*GWEN_DB_AddGroup(_bankMessages, GWEN_DB_Group_dup(n));
      fprintf(stderr, "------------------------------------\n");
      fprintf(stderr,
              "Message from \"%s\":\n",
              GWEN_DB_GetCharValue(n, "bankCode", 0, "<unknown>"));
      fprintf(stderr, "Subject: %s\n",
              GWEN_DB_GetCharValue(n, "subject", 0, "<empty>"));
      fprintf(stderr, "\n%s\n\n",
      GWEN_DB_GetCharValue(n, "text", 0, "<empty>"));*/
    }
    n=GWEN_DB_GetNextGroup(n);
  } /* while n */

  GWEN_DB_Group_free(response);
  return accountlist;
}
