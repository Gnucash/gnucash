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

//#include <guile/gh.h>
#include <glib.h>
#include "kvp_frame.h"
#include "Account.h"
#include "gnc-book.h"

/* Account */
gchar *gnc_hbci_get_account_accountid (Account *a);
void gnc_hbci_set_account_accountid (Account *a, const char *id);

gchar *gnc_hbci_get_account_bankcode (Account *a);
void gnc_hbci_set_account_bankcode (Account *a, const char *code);

gint gnc_hbci_get_account_countrycode (Account *a);
void gnc_hbci_set_account_countrycode (Account *a, gint code);

/* GNCBook */
GList *gnc_hbci_get_book_bankframelist (GNCBook *b);
void gnc_hbci_set_book_bankframelist_nc (GNCBook *b, GList *banklist);

gchar *gnc_hbci_get_bankframe_bankcode (kvp_frame *f);
gint gnc_hbci_get_bankframe_countrycode (kvp_frame *f);
GList *gnc_hbci_get_bankframe_userids (kvp_frame *f);

kvp_frame *gnc_hbci_bankframe_new (const char *bankcode,
				   gint countrycode,
				   const GList *userids);

/* lowlevel */
/* getter for kvp frame in book */
kvp_frame *gnc_hbci_get_book_kvp (GNCBook *b);

/* kvp frame in Account */
kvp_frame *gnc_hbci_get_account_kvp (Account *a);



#endif
