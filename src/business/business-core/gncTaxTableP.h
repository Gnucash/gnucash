/********************************************************************\
 * gncTaxTableP.h -- the Gnucash Tax Table private interface        *
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
 * Copyright (C) 2002 Derek Atkins
 * Copyright (C) 2003 Linas Vepstas <linas@linas.org>
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#ifndef GNC_TAXTABLEP_H_
#define GNC_TAXTABLEP_H_

#include "gncTaxTable.h"

gboolean gncTaxTableRegister (void);

void gncTaxTableSetParent (GncTaxTable *table, GncTaxTable *parent);
void gncTaxTableSetChild (GncTaxTable *table, GncTaxTable *child);
void gncTaxTableSetRefcount (GncTaxTable *table, gint64 refcount);
void gncTaxTableMakeInvisible (GncTaxTable *table);

gboolean gncTaxTableGetInvisible (const GncTaxTable *table);

/** The gncCloneTaxTable() routine makes a copy of the indicated
 *  tax table, placing it in the indicated book.  It copies
 *  the tax table name and list of entries.
 *  It also copies (as needed) both parents and children, so that
 *  the parent-child relationship is correctly mirrored in the
 *  clone.
 * XXX the refcount is mis-handled. This needs fixin....
 *  It then adds a pair of 'gemini' kvp pointers so that each copy
 *  can be found from the other.
 */
GncTaxTable * gncCloneTaxTable (GncTaxTable *from, QofBook *book);

/** The gncTaxTableObtainTwin() will find the 'twin' of the
 *  indicated tax table in the indicated book.  If the twin doesn't
 *  yet exist in the book, it will be created (by calling
 *  gncCloneTaxTable()) and placed into the book.
 *
 * We called this routine 'Obtain' instead of "Get" to distinguish
 * it from the other Get routines, which work in fundamentally
 * different ways.
 */
GncTaxTable * gncTaxTableObtainTwin (const GncTaxTable *from, QofBook *book);

GncTaxTable* gncTaxTableEntryGetTable( const GncTaxTableEntry* entry );

#define gncTaxTableSetGUID(E,G) qof_instance_set_guid(QOF_INSTANCE(E),(G))

#endif /* GNC_TAXTABLEP_H_ */
