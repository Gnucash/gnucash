/********************************************************************\
 * gnc-hbci-utils.c -- hbci utility functions                       *
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

#include "gnc-hbci-utils.h"

#include <gnome.h>
#include <openhbci/error.h>

#include "gnc-ui.h"
#include "gnc-hbci-kvp.h"

HBCI_API *
gnc_hbci_api_new (const char *filename)
{
  HBCI_API *api = NULL;
  HBCI_Error *err = NULL;
  char *errstring;
  
  if (!filename || 
      !g_file_test (filename, G_FILE_TEST_ISFILE | G_FILE_TEST_ISLINK))
    return NULL;

  api = HBCI_API_new (FALSE, TRUE);
  
  err = HBCI_API_loadEnvironment (api, filename);
  if (!HBCI_Error_isOk (err)) {
    errstring = HBCI_Error_errorString (err);
    HBCI_Error_delete (err);
    gnc_error_dialog
      (_("Error while loading OpenHBCI config file:\n  %s\n"), errstring);
    free (errstring);
    HBCI_API_delete (api);
    return NULL;
  }
  HBCI_Error_delete (err);
  return api;
}

HBCI_API *
gnc_hbci_api_new_currentbook () 
{
  return gnc_hbci_api_new 
    (gnc_hbci_get_book_configfile (gnc_get_current_book ()));
}


const HBCI_Account *
gnc_hbci_get_hbci_acc (const HBCI_API *api, Account *gnc_acc) 
{
  const char *bankcode = NULL, *accountid = NULL;
  int countrycode = 0;
  const HBCI_Bank *bank = NULL;
  const HBCI_Account *hbci_acc = NULL;

  bankcode = gnc_hbci_get_account_bankcode (gnc_acc);
  countrycode = gnc_hbci_get_account_countrycode (gnc_acc);
  if (bankcode && (strlen(bankcode)>0) && (countrycode > 0)) {
    //printf("gnc_acc %s has blz %s and ccode %d\n",
    //   xaccAccountGetName (gnc_acc), bankcode, countrycode);
    bank = HBCI_API_findBank (api, countrycode, bankcode);
    if (bank) {
      accountid = gnc_hbci_get_account_accountid (gnc_acc);
      if (accountid && (strlen(accountid)>0)) {
	hbci_acc = HBCI_Bank_findAccount (bank, accountid);
	if (hbci_acc) {
	  //printf("can connect gnc_acc %s to hbci_acc %s\n",
	  // xaccAccountGetName (gnc_acc), 
	  // HBCI_Account_accountId (hbci_acc));
	  return hbci_acc;
	} /* hbci_acc */
      } /* accountid */
    } /* bank */
  } /* bankcode */
  return NULL;
}

