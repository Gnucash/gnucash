/********************************************************************\
 * qofid-p.h -- QOF entity identifier engine-only API               *
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

#ifndef QOF_ID_P_H
#define QOF_ID_P_H 

#include <glib.h>

#include "qofid.h"

/* This file defines an engine-only API for using gnucash entity
 * identifiers. */

/* Create and destroy entity tables */
QofEntityTable * qof_entity_new (void);
void qof_entity_destroy (QofEntityTable *table);

/* Generate a new id. This function is guaranteed to return an id that
 * is unique within the scope of all GnuCash entities being managed by
 * the current invocation of GnuCash. GnuCash routines should always
 * use this function and not guid_new! 
 *
 * When considered over all possible instances of gnucash, the odds of 
 * this routine returning a non-unique id are still astronomically small.
 * If you had a gazzillion computers computing new ids, for the entire
 * age of the universe, you'd still have a one-in-a-million chance of
 * coming up with a duplicate.  2^128 is a really really big number.
 */
void qof_entity_guid_new (QofEntityTable *entity_table, GUID *guid);

/* Lookup an entity given an id and a type. If there is no entity
 * associated with the id, or if it has a different type, NULL
 * is returned. Input: GUID, returns reference (pointer) to object. 
 */
gpointer qof_entity_lookup (QofEntityTable *entity_table,
                           const GUID * guid, QofIdType entity_type);

/* Store the given entity under the given id with the given type. */
void qof_entity_store (QofEntityTable *entity_table,
                      gpointer entity, const GUID * guid,
                      QofIdType entity_type);

/* Remove any existing association between an entity and the given
 * id. The entity is not changed in any way. */
void qof_entity_remove (QofEntityTable *entity_table, const GUID * guid);

/* Call a function for each object of type 'type' in the entity table */
void qof_entity_foreach (QofEntityTable *entity_table, QofIdType type,
			QofEntityForeachCB cb_func, gpointer user_data);

#endif /* QOF_ID_P_H */
