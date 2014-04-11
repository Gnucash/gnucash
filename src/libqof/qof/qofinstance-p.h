/********************************************************************\
 * qofinstance-p.h -- private fields common to all object instances *
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
 * Object instance holds many common fields that most
 * gnucash objects use.
 * 
 * Copyright (C) 2003 Linas Vepstas <linas@linas.org>
 * Copyright (c) 2007 David Hampton <hampton@employees.org>
 */

#ifndef QOF_INSTANCE_P_H
#define QOF_INSTANCE_P_H

#include "qofinstance.h"

/** Set the collection this instance belongs to.  This function should never
 *  be called by user code. Instead call the qof_collection_insert_entity()
 *  function. */
void qof_instance_set_collection (gconstpointer ptr, QofCollection *col);

void qof_instance_set_slots (QofInstance *, KvpFrame *);

/*  Set the last_update time. Reserved for use by the SQL backend;
 *  used for comparing version in local memory to that in remote 
 *  server. 
 */
void qof_instance_set_last_update (QofInstance *inst, Timespec ts);

/** Set the dirty flag of just the instance. Don't modify the
 *  collection flag at all. */
void qof_instance_set_dirty_flag (gconstpointer inst, gboolean flag);

#endif /* QOF_INSTANCE_P_H */
