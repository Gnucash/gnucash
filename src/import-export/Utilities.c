/********************************************************************\
 * generic-import.c -- Functions and utilities to help writing      * 
 * import modules.   See file generic-import-design.txt for         *
 * description                                                      *
 *                        (GnuCash)                                 *
 * Copyright (C) 2002 Benoit Grégoire <bock@step.polymtl.ca>        *
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

#define _GNU_SOURCE

#include "config.h"


#include <glib.h>

#include <stdlib.h>
#include "gnc-generic-import.h"
#include "Account.h"
#include "Transaction.h"

#include "gnc-engine-util.h"


static short module = MOD_IMPORT;

/********************************************************************\
 * Setter and getter functions for the online_id kvp frame in
 * Account and Transaction 
\********************************************************************/

gchar * gnc_import_get_acc_online_id(Account * account)
{
  gchar * string = NULL;
  kvp_frame * frame;
  kvp_value * value;
  frame = xaccAccountGetSlots(account);
  value = kvp_frame_get_slot(frame, "online_id");
  string = kvp_value_get_string(value);  
  return string;
}

void gnc_import_set_acc_online_id(Account * account, gchar * string_value)
{
  kvp_frame * frame;
  kvp_value * value;
  frame = xaccAccountGetSlots(account);
  if(frame==NULL)
    {
      DEBUG("The kvp_frame was NULL, allocating new one\n");
      frame = kvp_frame_new();
    }
  value = kvp_frame_get_slot(frame, "online_id");
  //kvp_value_delete(value);
  value = kvp_value_new_string(string_value);
  kvp_frame_set_slot(frame,"online_id",value);  
  xaccAccountSetSlots_nc(account,frame);
  return;
}

gchar * gnc_import_get_trans_online_id(Transaction * transaction)
{
  gchar * string = NULL;
  kvp_frame * frame;
  kvp_value * value;
  frame = xaccTransGetSlots(transaction);
  value = kvp_frame_get_slot(frame, "online_id");
  string = kvp_value_get_string(value);  
  return string;
}

void gnc_import_set_trans_online_id(Transaction * transaction, gchar * string_value)
{
  kvp_frame * frame;
  kvp_value * value;
  TRACE("Begin");
  frame = xaccTransGetSlots(transaction);
  if(frame==NULL)
    {
      DEBUG("The kvp_frame was NULL, allocating new one");
      frame = kvp_frame_new();
    }
  value = kvp_frame_get_slot(frame, "online_id");
  if(value != NULL)
    {
      kvp_value_delete(value);
    }
  value = kvp_value_new_string(string_value);
  kvp_frame_set_slot(frame,"online_id",value);  
  xaccTransSetSlots_nc(transaction,frame);
  return;
}

