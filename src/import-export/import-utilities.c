/********************************************************************\
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
/** @addtogroup Import_Export
    @{ */
/** @internal
    @file import-utilities.c
    @brief Utility functions for writing import modules.
    @author Copyright (C) 2002 Benoit Grégoire <bock@step.polymtl.ca>
*/
#define _GNU_SOURCE

#include "config.h"


#include <glib.h>

#include <stdlib.h>
#include "import-utilities.h"
#include "Account.h"
#include "Transaction.h"

#include "gnc-engine-util.h"


static short module = MOD_IMPORT;

/********************************************************************\
 * Setter and getter functions for the online_id kvp frame in
 * Account and Transaction 
\********************************************************************/

const gchar * gnc_import_get_acc_online_id(Account * account)
{
  kvp_frame * frame;
  frame = xaccAccountGetSlots(account);
  return kvp_frame_get_string(frame, "online_id");
}

void gnc_import_set_acc_online_id(Account * account,
				  const gchar * string_value)
{
  kvp_frame * frame;
  frame = xaccAccountGetSlots(account);
  kvp_frame_set_string(frame, "online_id", string_value);
}

const gchar * gnc_import_get_trans_online_id(Transaction * transaction)
{
  kvp_frame * frame;
  frame = xaccTransGetSlots(transaction);
  return kvp_frame_get_string(frame, "online_id");
}

void gnc_import_set_trans_online_id(Transaction * transaction,
				    const gchar * string_value)
{
  kvp_frame * frame;
  frame = xaccTransGetSlots(transaction);
  kvp_frame_set_string (frame, "online_id", string_value);
}

/* @} */
