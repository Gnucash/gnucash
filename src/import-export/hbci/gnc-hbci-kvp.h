/********************************************************************\
 * gnc-hbci-kvp.h -- hbci kvp handling                              *
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

#ifndef GNC_HBCI_KVP_H
#define GNC_HBCI_KVP_H

//#include <libguile.h>
#include <glib.h>
#include "kvp_frame.h"
#include "Account.h"
#include "gnc-book.h"

/* Account */

/** Returns a non-copied pointer to the accountid string in the
 * Account a. The char* is still owned by the kvp_frame, so don't free
 * it until you want to delete the whole kvp_frame. */
char *gnc_hbci_get_account_accountid (Account *a);
/** Set the accountid string in the Account a. A copy of the string
 * will be stored. The Account will be marked as "dirty". */
void gnc_hbci_set_account_accountid (Account *a, const char *id);

/** Returns a non-copied pointer to the bankcode string in the
 * Account a. The char* is still owned by the kvp_frame, so don't free
 * it until you want to delete the whole kvp_frame. */
char *gnc_hbci_get_account_bankcode (Account *a);
/** Set the bankcode string in the Account a. A copy of the string
 * will be stored. The Account will be marked as "dirty". */
void gnc_hbci_set_account_bankcode (Account *a, const char *code);

/** Returns the countrycode integer value from the Account a.  */
gint gnc_hbci_get_account_countrycode (Account *a);
/** Set the countrycode integer value in the Account a.  The Account
 * will be marked as "dirty". */
void gnc_hbci_set_account_countrycode (Account *a, gint code);

/** Returns the time of last online transaction retrieval */
Timespec gnc_hbci_get_account_trans_retrieval (Account *a);
/** Set the time of last online transaction retrieval. The account
 * will be marked as "dirty". */
void gnc_hbci_set_account_trans_retrieval (Account *a, Timespec time);


/* GNCBook */

/** Returns a non-copied pointer to the configfile string in the
 * GNCBook b. The char* is still owned by the kvp_frame, so don't free
 * it until you want to delete the whole kvp_frame. */
char *gnc_hbci_get_book_configfile (GNCBook *b);
/** Set the configfile string in the GNCBook b. A copy of the string
 * will be stored. The Book will be marked as "dirty". */
void gnc_hbci_set_book_configfile (GNCBook *b, const char *filename);

/* lowlevel */

/* internal getter for kvp frame in book */
kvp_frame *gnc_hbci_get_book_kvp (GNCBook *b);

/* internal getter for kvp frame in Account */
kvp_frame *gnc_hbci_get_account_kvp (Account *a);



#endif
