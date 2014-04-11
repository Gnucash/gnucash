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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/
/** @addtogroup Object
    @{ */
/** @addtogroup Object_Private
    @{ */
/** @name  Entity_Private
    @{ */

#ifndef QOF_ID_P_H
#define QOF_ID_P_H 

#include "qofid.h"

/* This file defines an engine-only API for using QOF entity
 * identifiers. */

/** Take entity, remove it from whatever collection its currently
 *  in, and place it in a new collection.  To be used only for
 *  moving entity from one book to another.
 */
void qof_collection_insert_entity (QofCollection *, QofInstance *);

/** reset value of dirty flag */
void qof_collection_mark_clean (QofCollection *);
void qof_collection_mark_dirty (QofCollection *);
void qof_collection_print_dirty (const QofCollection *col, gpointer dummy);

/* @} */
/* @} */
/* @} */
#endif /* QOF_ID_P_H */
