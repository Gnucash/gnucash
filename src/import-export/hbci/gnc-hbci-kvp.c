/********************************************************************\
 * gnc-hbci-kvp.c -- hbci kvp handling                              *
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

#include "gnc-hbci-kvp.h"

#define HBCI_KEY "hbci"
#define HBCI_ACCOUNT_ID "account-id"
#define HBCI_BANK_CODE "bank-code"
#define HBCI_COUNTRY_CODE "country-code"
#define HBCI_USERID_LIST "user-id-list"
#define HBCI_BANK_LIST "bank-list"

/* Account */
gchar *gnc_hbci_get_account_accountid (Account *a)
{
  kvp_frame *frame = gnc_hbci_get_account_kvp (a);
  kvp_value *value = kvp_frame_get_slot (frame, HBCI_ACCOUNT_ID);
  return kvp_value_get_string (value);
}
gchar *gnc_hbci_get_account_bankcode (Account *a)
{
  kvp_frame *frame = gnc_hbci_get_account_kvp (a);
  kvp_value *value = kvp_frame_get_slot (frame, HBCI_BANK_CODE);
  return kvp_value_get_string (value);
}
gint gnc_hbci_get_account_countrycode (Account *a)
{
  kvp_frame *frame = gnc_hbci_get_account_kvp (a);
  kvp_value *value = kvp_frame_get_slot (frame, HBCI_COUNTRY_CODE);
  return kvp_value_get_gint64 (value);
}
void gnc_hbci_set_account_accountid (Account *a, const char *id)
{
  kvp_frame *frame = gnc_hbci_get_account_kvp (a);
  kvp_value *value = kvp_value_new_string (id);
  kvp_frame_set_slot (frame, HBCI_ACCOUNT_ID, value);
}
void gnc_hbci_set_account_bankcode (Account *a, const char *code)
{
  kvp_frame *frame = gnc_hbci_get_account_kvp (a);
  kvp_value *value = kvp_value_new_string (code);
  kvp_frame_set_slot (frame, HBCI_BANK_CODE, value);
}
void gnc_hbci_set_account_countrycode (Account *a, gint code)
{
  kvp_frame *frame = gnc_hbci_get_account_kvp (a);
  kvp_value *value = kvp_value_new_gint64 (code);
  kvp_frame_set_slot (frame, HBCI_COUNTRY_CODE, value);
}


/* GNCBook */
GList *gnc_hbci_get_book_bankframelist (GNCBook *b)
{
  kvp_frame *frame = gnc_hbci_get_book_kvp (b);
  kvp_value *value = kvp_frame_get_slot (frame, HBCI_BANK_LIST);
  return kvp_value_get_glist (value);
}
void gnc_hbci_set_book_bankframelist_nc (GNCBook *b, GList *banklist)
{
  kvp_frame *frame = gnc_hbci_get_book_kvp (b);
  kvp_value *value = kvp_value_new_glist_nc (banklist);
  kvp_frame_set_slot_nc (frame, HBCI_BANK_LIST, value);
}
gchar *gnc_hbci_get_bankframe_bankcode (kvp_frame *f)
{
  kvp_value *value = kvp_frame_get_slot (f, HBCI_BANK_CODE);
  return kvp_value_get_string (value);
}
gint gnc_hbci_get_bankframe_countrycode (kvp_frame *f)
{
  kvp_value *value = kvp_frame_get_slot (f, HBCI_COUNTRY_CODE);
  return kvp_value_get_gint64 (value);
}
GList *gnc_hbci_get_bankframe_userids (kvp_frame *f)
{
  kvp_value *value = kvp_frame_get_slot (f, HBCI_USERID_LIST);
  return kvp_value_get_glist (value);
}
kvp_frame *gnc_hbci_bankframe_new (const char *bankcode,
				   gint countrycode,
				   const GList *userids)
{
  kvp_frame *newframe = kvp_frame_new();
  kvp_frame_set_slot(newframe, HBCI_BANK_CODE, 
		     kvp_value_new_string (bankcode));
  kvp_frame_set_slot(newframe, HBCI_COUNTRY_CODE, 
		     kvp_value_new_gint64 (countrycode));
  kvp_frame_set_slot(newframe, HBCI_USERID_LIST,
		     kvp_value_new_glist (userids));
  return newframe;
}

  


/* lowlevel */
/* getters  for kvp frame in book */
kvp_frame *gnc_hbci_get_book_kvp (GNCBook *b)
{
  kvp_frame *toplevel = gnc_book_get_slots (b);
  return kvp_frame_get_frame (toplevel, HBCI_KEY, NULL);
}



/* kvp frame in Account */
kvp_frame *gnc_hbci_get_account_kvp (Account *a)
{
  kvp_frame *toplevel = xaccAccountGetSlots (a);
  return kvp_frame_get_frame (toplevel, HBCI_KEY, NULL);
}
