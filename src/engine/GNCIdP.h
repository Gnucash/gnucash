/********************************************************************\
 * GNCIdP.h -- Gnucash entity identifier engine-only API            *
 * Copyright (C) 2000 Dave Peticolas <peticola@cs.ucdavis.edu>      *
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

#ifndef GNC_ID_P_H
#define GNC_ID_P_H 1

#include "GNCId.h"

/* This file defines an engine-only API for using gnucash entity
 * identifiers. */

/* Generate a new id. This function is guaranteed to return an id that
 * is unique within the scope of all GnuCash entities being managed by
 * the current invocation of GnuCash. GnuCash routines should always
 * use this function and not guid_new! */
void xaccGUIDNew (GUID *guid);

/* Lookup an entity given an id and a type. If there is no entity
 * associated with the id, or if it has a different type, NULL
 * is returned. */
void * xaccLookupEntity (const GUID * guid, GNCIdType entity_type);

/* Store the given entity under the given id with the given type. */
void xaccStoreEntity (void * entity, const GUID * guid, GNCIdType entity_type);

/* Remove any existing association between an entity and the given
 * id. The entity is not changed in any way. */
void xaccRemoveEntity (const GUID * guid);

GHashTable *xaccGetAndResetEntityTable (void);
void xaccSetEntityTable (GHashTable *et);

/* Initialize and shutdown the GNC Id system. */
void xaccGUIDInit (void);
void xaccGUIDShutdown (void);

#endif
