/********************************************************************\
 * gnc-lot.h -- AR/AP invoices; inventory lots; stock lots          *
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
 * gnc-lot.h
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

#ifndef GNC_LOT_H
#define GNC_LOT_H

#include "gnc-engine.h"
#include "gnc-numeric.h"

GNCLot * gnc_lot_new (GNCBook *);
void gnc_lot_destroy (GNCLot *);

const GUID * gnc_lot_get_guid (GNCLot *p);
GNCLot * gnc_lot_lookup (const GUID *guid, GNCBook *book);
GNCBook * gnc_lot_get_book (GNCLot *);		  

/* The gnc_lot_add_split() routine adds a split to this lot.  Note
 *    that *all* splits in a lot must also be in the same account.
 */
void gnc_lot_add_split (GNCLot *, Split *);
void gnc_lot_remove_split (GNCLot *, Split *);

/* The gnc_lot_get_split_list() routine returns a GList of all the
 *    splits in this lot.  Do *not* not free this list when done;
 *    it is a pointer straight into the lots intenal list.  Do
 *    *not* add to or remove from this list directly.  Calling
 *    either gnc_lot_add_split() or gnc_lot_remove_split() will
 *    invalidate the returned pointer.
 */
SplitList * gnc_lot_get_split_list (GNCLot *);
gint gnc_lot_count_splits (GNCLot *);

/* The gnc_lot_get_account() routine returns the account with which 
 *    this lot is associated. */
Account * gnc_lot_get_account (GNCLot *);

/* The gnc_lot_get_balance() routine returns the balance of the lot. 
 *    The commodity in which this balance is expressed is the commodity 
 *    of the account. */
gnc_numeric gnc_lot_get_balance (GNCLot *);

/* The gnc_lot_is_closed() routine returns a boolean flag: is this 
 *    lot closed?  A lot is closed if its balance is zero.  This 
 *    routine is faster than using gnc_lot_get_balance() because
 *    once the balance goes to zero, this fact is cached.  
 */
gboolean gnc_lot_is_closed (GNCLot *);

/* Every lot has a place to hang kvp data.  This routine returns that
 * place. 
 * */
kvp_frame * gnc_lot_get_slots (GNCLot *);

#endif /* GNC_LOT_H */
