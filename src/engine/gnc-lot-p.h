/********************************************************************\
 * gnc-lot-p.h -- AR/AP invoices; inventory lots; stock lots        *
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


/*
 * FILE:
 * gnc-lot-p.h
 * 
 * FUNCTION:
 * Lots implement the fundamental conceptual idea behind invoices,
 * inventory lots, and stock market investment lots.  See the file
 * src/doc/lots.txt for implmentation overview.
 *
 * HISTORY:
 * Created by Linas Vepstas May 2002
 * Copyright (c) 2002 Linas Vepstas <linas@linas.org>
 */

#ifndef GNC_LOT_P_H
#define GNC_LOT_P_H

#include "GNCIdP.h"
#include "gnc-engine.h"
#include "kvp_frame.h"

struct gnc_lot_struct
{
  /* Unique guid for this lot */
  GUID guid;

  /* Book that this lot belongs to */
  GNCBook *book;  

  /* Anchor for generic lot-specific data. */
  kvp_frame *kvp_data;

  /* Account to which this lot applies.  All splits in the lot must
   * belong to this account. 
   */
  Account * account;

  /* List of splits that belong to this lot. */
  SplitList *splits;

  /* handy cached value to indicate if lot is closed */
  /* if value is negative, then the cache is invalid */
  char is_closed;
};

void gnc_lot_set_guid(GNCLot *lot, GUID guid);

/* Register with the Query engine */
void gnc_lot_register (void);

#endif /* GNC_LOT_P_H */

