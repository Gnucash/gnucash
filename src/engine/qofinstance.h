/********************************************************************\
 * qofinstance.h -- fields common to all object instances           *
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
 * Object instance holds many common fields that most
 * gnucash objects use.
 * 
 * Copyright (C) 2003 Linas Vepstas <linas@linas.org>
 */

#ifndef QOF_INSTANCE_H
#define QOF_INSTANCE_H

#include "guid.h"
#include "kvp_frame.h"
#include "qofbook.h"

/* --- type macros --- */
/* cheesy, but will do for now, eventually should be more gtk-like, handle
 * thunks, etc.  */
#define QOF_INSTANCE(object) ((QofInstance *)(object))

typedef struct QofInstance_s QofInstance;

struct QofInstance_s
{
/*
 * UNDER CONSTRUCTION!
 * This is mostly scaffolding for now,
 * eventually, it may hold more fields, such as refrence counting...
 *
 */

   /* Globally unique account id identifying this instance */
   GUID guid;

   /* The entity_table in which this instance is stored */
   QofBook * book;

  /* kvp_data is a key-value pair database for storing arbirtary
   * information associated with this instance.  
   * See src/engine/kvp_doc.txt for a list and description of the 
   * important keys. */
   KvpFrame *kvp_data;

   /* Keep track of nesting level of begin/end edit calls */
   int    editlevel;

   /* In process of being destroyed */
   gboolean  do_free;

   /* This instance has not been saved yet */
   gboolean  dirty;
};

/** Initialise the memory associated with an instance */
void qof_instance_init (QofInstance *, QofBook *);

/** release the data associated with this instance. Dont actually free 
 * the memory associated with teh instance. */
void qof_instance_release (QofInstance *inst);

/** Return the book pointer */
QofBook * qof_instance_get_book (QofInstance *);

/* Return the GUID of this instance */
const GUID * qof_instance_get_guid (QofInstance *);

/** return the pointer to the kvp_data */
KvpFrame* qof_instance_get_slots (QofInstance *);

/** pair things up. Currently, this routine only inserts a
 * pair of guid-pointers pointing to each other.  it
 * doesn't copy any data.
 */
void qof_instance_gemini (QofInstance *to, QofInstance *from);

/** Look up the gemini'ed twin to 'src' in the book 'book'. */
QofInstance * qof_instance_lookup_twin (QofInstance *src, QofBook *book);

#endif /* QOF_INSTANCE_H */
