/********************************************************************\
 * gnc-hbci-account.c -- hbci account functions                     *
 * Copyright (C) 2004 Christian Stimming                            *
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
#include "gnc-hbci-account.h"

#include <gnome.h>
#include <errno.h>
#include <openhbci2.h>
#include <openhbci2/error.h>

#include "hbci-interaction.h"

/* static short module = MOD_IMPORT; */

struct _gnc_HBCI_Account 
{
  const HBCI_Bank *bank;
  char *bankCode;
  char *accountid;
};

gnc_HBCI_Account *gnc_HBCI_Account_new(const HBCI_Bank *bank, 
				       const char *bankCode,
				       const char *accountid)
{
  gnc_HBCI_Account *r = g_new0(gnc_HBCI_Account, 1);
  r->bank = bank;
  r->bankCode = g_strdup (bankCode);
  r->accountid = g_strdup (accountid);
  return r;
}
void gnc_HBCI_Account_delete (gnc_HBCI_Account *h)
{
  if (!h) return;
  g_free(h->bankCode);
  g_free(h->accountid);
  g_free(h);
}

const char *gnc_HBCI_Account_accountId (const gnc_HBCI_Account *hbci_acc)
{
  g_assert(hbci_acc);
  return hbci_acc->accountid;
}
const char *gnc_HBCI_Account_bankCode (const gnc_HBCI_Account *hbci_acc)
{
  g_assert(hbci_acc);
  return hbci_acc->bankCode;
}
const HBCI_Bank *
gnc_HBCI_Account_bank (const gnc_HBCI_Account *hbci_acc)
{
  g_assert(hbci_acc);
  return hbci_acc->bank;
}

void *list_HBCI_Account_foreach(GList *h_list, 
				void*(*func_cb)(gnc_HBCI_Account *acc,
						void *user_data), 
				void *user_data)
{
  GList *iter;
  g_assert(func_cb);
  void *res = NULL;

  if (h_list == NULL) return NULL;

  iter = h_list;
  if (iter->data)
    res = func_cb(iter->data, user_data);
  if (res) 
    return res;

  for (iter = h_list->next; iter != h_list; iter = iter->next)
    {
      if (iter->data)
	func_cb(iter->data, user_data);
      if (res)
	break;
    }
  return res;
}


static void* del_func(gnc_HBCI_Account *acc, void *user_data)
{
    gnc_HBCI_Account_delete(acc);
    return NULL;
}


void list_HBCI_Account_delete(GList *list_HBCI_Account)
{
    list_HBCI_Account_foreach(list_HBCI_Account, del_func, NULL);
    g_free (list_HBCI_Account);
}


/* ------------------------------------------------------------ */

#define HBCI_ACCOUNT_ID "account-id"
#define HBCI_BANK_CODE "bank-code"
#define HBCI_COUNTRY_CODE "country-code"

/** Constructor from a kvp_frame */
gnc_HBCI_Account *gnc_HBCI_Account_from_kvp(kvp_frame *k, HBCI_API *api)
{
  gnc_HBCI_Account *res = NULL;
  HBCI_Bank *bank;
  char *bankcode;
  int countrycode;
  g_assert(k);

  bankcode = kvp_value_get_string (kvp_frame_get_slot(k, HBCI_BANK_CODE));
  countrycode = kvp_value_get_gint64 (kvp_frame_get_slot(k, HBCI_COUNTRY_CODE));

  if (bankcode && (strlen(bankcode)>0) && (countrycode > 0)) {
    /*printf("gnc_acc %s has blz %s and ccode %d\n",
      xaccAccountGetName (gnc_acc), bankcode, countrycode);*/
    bank = HBCI_API_findBank (api, countrycode, bankcode);
    res= gnc_HBCI_Account_new(bank, 
			      kvp_value_get_string
			      (kvp_frame_get_slot(k, HBCI_ACCOUNT_ID)),
			      kvp_value_get_string
			      (kvp_frame_get_slot(k, HBCI_BANK_CODE)));
    if (!bank) 
      printf("gnc_HBCI_Account_from_kvp: oops, no bank found.");
  }
  return res;
}

/** Creates a kvp_frame from this TransTempl */
kvp_frame *gnc_HBCI_Account_to_kvp(const gnc_HBCI_Account *t)
{
  kvp_frame *k = kvp_frame_new();
  g_assert(t);

  kvp_frame_set_slot(k, HBCI_ACCOUNT_ID, 
		     kvp_value_new_string(gnc_HBCI_Account_accountId(t)));
  kvp_frame_set_slot(k, HBCI_BANK_CODE, 
		     kvp_value_new_string(gnc_HBCI_Account_bankCode (t)));
  if (gnc_HBCI_Account_bank(t))
    kvp_frame_set_slot(k, HBCI_COUNTRY_CODE, 
		       kvp_value_new_gint64(HBCI_Bank_country 
					    (gnc_HBCI_Account_bank(t))));
  return k;
}

struct _glistapi
{
  GList *res;
  HBCI_API *api;
};
/** Creates a GList of gnc_HBCI_Account from a GList of kvp_values which
    in turn contain a kvp_frame. */
static void glist_from_kvp_func(gpointer data, gpointer user_data)
{
  struct _glistapi *mydata = user_data;
  kvp_value *k = data;
  mydata->res = g_list_append(mydata->res, 
			      gnc_HBCI_Account_from_kvp(kvp_value_get_frame(k),
							mydata->api));
}

/** Creates a GList of gnc_HBCI_Account from a GList of kvp_values which
    in turn contain a kvp_frame. */
GList *gnc_HBCI_Account_glist_from_kvp_glist(GList *v, HBCI_API *api)
{
  struct _glistapi mydata;
  if (!v) return NULL;

  mydata.res = NULL;
  mydata.api = api;
  
  g_list_foreach (v, glist_from_kvp_func, &mydata);
  return mydata.res;
}


/** Creates a GList of kvp_value (which in turn contain a kvp_frame)
    from a GList of gnc_HBCI_Account. */
static void glist_to_kvp_func(gpointer data, gpointer user_data)
{
  GList **tmp = user_data;
  GList *res = *tmp;
  gnc_HBCI_Account *g = data;
  *tmp = g_list_append(res, 
		       kvp_value_new_frame_nc(gnc_HBCI_Account_to_kvp(g)));
}
/** Creates a GList of kvp_value (which in turn contain a kvp_frame)
    from a GList of gnc_HBCI_Account. */
GList *gnc_HBCI_Account_kvp_glist_from_glist(GList *k)
{
  GList *res = NULL;
  if (!k) return NULL;

  g_list_foreach (k, glist_to_kvp_func, &res);
  return res;
}


