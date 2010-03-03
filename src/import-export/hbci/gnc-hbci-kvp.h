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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
\********************************************************************/

#ifndef GNC_HBCI_KVP_H
#define GNC_HBCI_KVP_H

#include <glib.h>
#include "qof.h"
#include "Account.h"

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

/** Returns the unique id for the AB_BANKING account in the Account
 * a. */
gint gnc_hbci_get_account_uid (Account *a);
/** Set the unique id for the AB_BANKING account in the Account a. The
    Account will be marked as "dirty". */
void gnc_hbci_set_account_uid (Account *a, gint uid);


/** Returns the time of last online transaction retrieval */
Timespec gnc_hbci_get_account_trans_retrieval (Account *a);
/** Set the time of last online transaction retrieval. The account
 * will be marked as "dirty". */
void gnc_hbci_set_account_trans_retrieval (Account *a, Timespec time);


/* QofBook */

/** Returns a non-copied pointer to the configfile string in the
 * QofBook b. The char* is still owned by the kvp_frame, so don't free
 * it until you want to delete the whole kvp_frame. */
char *gnc_hbci_get_book_configfile (QofBook *b);
/** Set the configfile string in the QofBook b. A copy of the string
 * will be stored. The Book will be marked as "dirty". */
void gnc_hbci_set_book_configfile (QofBook *b, const char *filename);

/** Returns a non-copied pointer to the GList of kvp_frames which
 * eventually are the template transactions, stored in the given
 * book. */
GList *gnc_hbci_get_book_template_list (QofBook *b);
void gnc_hbci_set_book_template_list (QofBook *b, GList *template_list);

#if 0
/** Returns a non-copied pointer to the GList of kvp_frames which
 * eventually are the available HBCI accounts, stored in the given
 * book. */
GList *gnc_hbci_get_book_account_list (QofBook *b);
void gnc_hbci_set_book_account_list (QofBook *b, GList *account_list);
#endif

/* lowlevel */

/* internal getter for kvp frame in book. The create argument says
 *  to create the frame if it doesn't already exist. */
kvp_frame *gnc_hbci_get_book_kvp (QofBook *b, gboolean create);

/* internal getter for kvp frame in Account. The create argument says
 *  to create the frame if it doesn't already exist. */
kvp_frame *gnc_hbci_get_account_kvp (Account *a, gboolean create);



#endif
