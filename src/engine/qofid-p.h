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
/** @addtogroup Object
    @{ */
/** @addtogroup Object_Private
    Private interfaces, not meant to be used by applications.
    @{ */
/** @name  Entity_Private
    @{ */

#ifndef QOF_ID_P_H
#define QOF_ID_P_H 

#include <glib.h>

#include "qofid.h"

/* This file defines an engine-only API for using gnucash entity
 * identifiers. */

/** Set the ID of the entity, over-riding teh previous ID. 
 *  Very dangerous, use only for file i/o work. 
 */
void qof_entity_set_guid (QofEntity *ent, const GUID *guid);

/** Take entity, remove it from whatever collection its currently
 *  in, and place it in a new collection.  To be used only for
 *  moving entity from one book to another.
 */
void qof_collection_insert_entity (QofCollection *, QofEntity *);

/** reset value of dirty flag */
void qof_collection_mark_clean (QofCollection *);
void qof_collection_mark_dirty (QofCollection *);

/* @} */
/* @} */
/* @} */
#endif /* QOF_ID_P_H */
