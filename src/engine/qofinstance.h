/*
 * qofinstance.h
 *
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
   GUID guid;
   QofBook * book;
   KvpFrame *kvp_data;

   int    editlevel;
   gboolean  do_free;
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

#endif /* QOF_INSTANCE_H */
