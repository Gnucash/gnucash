/********************************************************************\
 * gncEntryP.h -- the Core Business Entry Interface                  *
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
 *                                                                  *
\********************************************************************/

/*
 * Copyright (C) 2001 Derek Atkins
 * Copyright (C) 2003 Linas Vepstas <linas@linas.org>
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#ifndef GNC_ENTRYP_H_
#define GNC_ENTRYP_H_

#include "gncEntry.h"

#ifdef __cplusplus
extern "C" {
#endif

gboolean gncEntryRegister (void);
void gncEntrySetGUID (GncEntry *entry, const GncGUID *guid);
void gncEntrySetOrder (GncEntry *entry, GncOrder *order);
void gncEntrySetInvoice (GncEntry *entry, GncInvoice *invoice);
void gncEntrySetBill (GncEntry *entry, GncInvoice *bill);
void gncEntrySetDirty (GncEntry *entry, gboolean dirty);

#define gncEntrySetGUID(E,G) qof_instance_set_guid(QOF_INSTANCE(E),(G))

#ifdef __cplusplus
}
#endif

#endif /* GNC_ENTRYP_H_ */
