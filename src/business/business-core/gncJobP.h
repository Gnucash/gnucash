/*
 * gncJobP.h -- the Core Job Interface
 * Copyright (C) 2001 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#ifndef GNC_JOBP_H_
#define GNC_JOBP_H_

#include "gncBusiness.h"
#include "gncJob.h"

gboolean gncJobRegister (void);
gint gncJobNextID (GncBusiness *business);
void gncJobSetGUID (GncJob *job, const GUID *guid);

#endif /* GNC_JOBP_H_ */
