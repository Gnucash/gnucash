/*
 * gncObjectP.h -- the Core Object Registration Interface
 * Copyright (C) 2001, 2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#ifndef GNC_OBJECTP_H_
#define GNC_OBJECTP_H_

#include "gncObject.h"

/* Initialize the object registration subsystem */
void gncObjectInitialize (void);
void gncObjectShutdown (void);

/* To be called from within the book */
void gncObjectBookBegin (GNCBook *book);
void gncObjectBookEnd (GNCBook *book);

gboolean gncObjectIsDirty (GNCBook *book);

#endif /* GNC_OBJECTP_H_ */
