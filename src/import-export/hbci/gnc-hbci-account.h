/********************************************************************\
 * gnc-hbci-account.h -- hbci account definition                    *
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

#ifndef GNC_HBCI_ACCOUNT_H
#define GNC_HBCI_ACCOUNT_H

#include <glib.h>
#include <gnome.h>
#include <openhbci2/api.h>

#include "gnc-book.h"

typedef struct _gnc_HBCI_Account gnc_HBCI_Account;
/** @name HBCI_Account wrapper class */
/*@{*/
/** Constructor */
gnc_HBCI_Account *gnc_HBCI_Account_new(const HBCI_Bank *bank, 
				       const char *bankCode,
				       const char *accountid);
/** Returns the account id */
const char *gnc_HBCI_Account_accountId (const gnc_HBCI_Account *hbci_acc);
/** Returns the bank code -- this might be different than the actual
 * bank's bank code */
const char *gnc_HBCI_Account_bankCode (const gnc_HBCI_Account *hbci_acc);
/** Returns the bank this account belongs to */
const HBCI_Bank *
gnc_HBCI_Account_bank (const gnc_HBCI_Account *hbci_acc);
/** Destructor for this account object */
void gnc_HBCI_Account_delete (gnc_HBCI_Account *hbci_acc);
/** Foreach function */
void *list_HBCI_Account_foreach(GList *list_HBCI_Account, 
				void*(*func_cb)(gnc_HBCI_Account *acc,
						void *user_data), 
				void *user_data);
/** Delete each element of a GList of HBCI_Accounts and finally the
 * list itself.  */
void list_HBCI_Account_delete(GList *list_HBCI_Account);
/*@}*/

/** @name Serialization -- List of gnc_HBCI_Accounts to kvp_frame and
 * back */
/*@{*/
/** Constructor from a kvp_frame (the kvp_frame is left unchanged) */
gnc_HBCI_Account *gnc_HBCI_Account_from_kvp(kvp_frame *k, HBCI_API *api);
/** Creates a kvp_frame from this TransTempl */
kvp_frame *gnc_HBCI_Account_to_kvp(const gnc_HBCI_Account *t);

/** Creates a GList of gnc_HBCI_Account from a GList of kvp_values which
    in turn contain a kvp_frame. */
GList *gnc_HBCI_Account_glist_from_kvp_glist(GList *v, HBCI_API *api);
/** Creates a GList of kvp_value (which in turn contain a kvp_frame)
    from a GList of gnc_HBCI_Account. */
GList *gnc_HBCI_Account_kvp_glist_from_glist(GList *k);
/*@}*/


#endif /* GNC_HBCI_ACCOUNT_H */
