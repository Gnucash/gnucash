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

void gnc_lot_add_split (GNCLot *, Split *);
void gnc_lot_remove_split (GNCLot *, Split *);

/* The gnc_lot_get_account() routine returns the account with which 
 *    this lot is associated. */
Account * gnc_lot_get_account (GNCLot *);

/* The gnc_lot_get_balance() routine returns the balance of the lot. 
 *    The commodity in which this balance is expressed is the commodity 
 *    of the account. */
gnc_numeric gnc_lot_get_balance (GNCLot *);

/* flag: is this lot closed? */
gboolean gnc_lot_is_closed (GNCLot *);

kvp_frame * gnc_lot_get_slots (GNCLot *);

#endif /* GNC_LOT_H */
