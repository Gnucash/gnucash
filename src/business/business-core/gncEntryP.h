/********************************************************************\
 * gncEntryP.h -- the Core Busines Entry Interface                  *
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
 *                                                                  *
\********************************************************************/

/*
 * Copyright (C) 2001 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#ifndef GNC_ENTRYP_H_
#define GNC_ENTRYP_H_

#include "gncEntry.h"
#include "qofid-p.h"

gboolean gncEntryRegister (void);
void gncEntrySetGUID (GncEntry *entry, const GUID *guid);
void gncEntrySetOrder (GncEntry *entry, GncOrder *order);
void gncEntrySetInvoice (GncEntry *entry, GncInvoice *invoice);
void gncEntrySetBill (GncEntry *entry, GncInvoice *bill);
void gncEntrySetDirty (GncEntry *entry, gboolean dirty);

#define gncEntrySetGUID(E,G) qof_entity_set_guid(QOF_ENTITY(E),(G))

#endif /* GNC_ENTRYP_H_ */
