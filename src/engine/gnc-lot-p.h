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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
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

#include "gnc-lot.h"
#include "Account.h"

struct gnc_lot_struct
{
    QofInstance inst;

    /* Account to which this lot applies.  All splits in the lot must
     * belong to this account.
     */
    Account * account;

    /* List of splits that belong to this lot. */
    SplitList *splits;

    /* Handy cached value to indicate if lot is closed. */
    /* If value is negative, then the cache is invalid. */
    signed char is_closed;

    /* traversal marker, handy for preventing recursion */
    unsigned char marker;
};

struct _GncLotClass
{
    QofInstanceClass parent_class;
};

#define gnc_lot_set_guid(L,G)  qof_instance_set_guid(QOF_INSTANCE(L),&(G))

/* Register with the Query engine */
gboolean gnc_lot_register (void);

#endif /* GNC_LOT_P_H */
