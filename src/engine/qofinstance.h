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
#include "kvp_frame.h"

/* --- type macros --- */
/* cheesy, but will do for now, eventually should be more gtk-like, handle
 * thunks, etc.  */
#define QOF_INSTANCE(object) ((QofInstance *)(object))

typedef struct QofInstance_s QofInstance;

struct QofInstance_s
{
/*
 * UNDER CONSTRUCTION!
 * This is temp scaffolding for now,
 * eventually, it should hold at least the following fields:
 * (and maybe more, such as refrence counting...)
 *
   GUID guid;
   QofBook * book;
   int    editlevel;
   gboolean  do_free;
   gboolean  dirty;
 */
   KvpFrame *kvp_data;
};

/** Initialise the memory associated with an instance */
void qof_instance_init (QofInstance *inst);

/** release the data associated with this instance. Dont actually free 
 * the memory associated with teh instance. */
void qof_instance_release (QofInstance *inst);

/** return the pointer to the kvp_data */
KvpFrame* qof_instance_get_slots (QofInstance *);

#endif /* QOF_INSTANCE_H */
