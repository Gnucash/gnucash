/*
 * gncEmployeeP.h -- the Core Employee Interface
 * Copyright (C) 2001 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#ifndef GNC_EMPLOYEEP_H_
#define GNC_EMPLOYEEP_H_

#include "gncEmployee.h"

gboolean gncEmployeeRegister (void);
gint gncEmployeeNextID (GNCBook *book);
void gncEmployeeSetGUID (GncEmployee *employee, const GUID *guid);

#endif /* GNC_EMPLOYEEP_H_ */
