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
 */

#ifndef QOF_INSTANCE_P_H
#define QOF_INSTANCE_P_H

#include "qofinstance.h"

/*
 * UNDER CONSTRUCTION!
 * This is mostly scaffolding for now,
 * eventually, it may hold more fields, such as refrence counting...
 *
 */
struct QofInstance_s
{
   /* Globally unique id identifying this instance */
   QofEntity entity;

   /* The entity_table in which this instance is stored */
   QofBook * book;

  /* kvp_data is a key-value pair database for storing arbirtary
   * information associated with this instance.  
   * See src/engine/kvp_doc.txt for a list and description of the 
   * important keys. */
   KvpFrame *kvp_data;

   /*  Timestamp used to track the last modification to this 
    *  instance.  Typically used to compare two versions of the
    *  same object, to see which is newer.  When used with the 
    *  SQL backend, this field is reserved for SQL use, to compare
    *  the version in local memory to the remote, server version.
    */
   Timespec last_update;

   /*  Keep track of nesting level of begin/end edit calls */
   int    editlevel;

   /*  In process of being destroyed */
   gboolean  do_free;

   /*  dirty/clean flag. If dirty, then this instance has been modified,
    *  but has not yet been written out to storage (file/database)
    */
   gboolean  dirty;

   /* True iff this instance has never been committed. */
   gboolean infant;
};

void qof_instance_set_slots (QofInstance *, KvpFrame *);

/*  Set the last_update time. Reserved for use by the SQL backend;
 *  used for comparing version in local memory to that in remote 
 *  server. 
 */
void qof_instance_set_last_update (QofInstance *inst, Timespec ts);

#endif /* QOF_INSTANCE_P_H */
