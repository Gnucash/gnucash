/********************************************************************\
 * gnc-lot.c -- AR/AP invoices; inventory lots; stock lots          *
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
 * gnc-lot.c
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

#include "gnc-book-p.h"
#include "gnc-lot.h"
#include "gnc-lot-p.h"

/* ============================================================= */

static void
gnc_lot_init (GNCLot *lot, GNCBook *book)
{
   lot->kvp_data = NULL;
   lot->account = NULL;
   lot->splits = NULL;
	lot->is_closed = FALSE;
  
   lot->book = book;
   xaccGUIDNew (&lot->guid, book);
   xaccStoreEntity (book->entity_table, lot, &lot->guid, GNC_ID_LOT);
}

GNCLot *
gnc_lot_new (GNCBook *book)
{
   GNCLot *lot;
   g_return_val_if_fail (book, NULL);

   lot = g_new (GNCLot, 1);
   gnc_lot_init (lot, book);
   return lot;
}

/* ========================== END OF FILE ========================= */
