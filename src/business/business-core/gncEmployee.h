/*
 * gncEmployee.h -- the Core Employee Interface
 * Copyright (C) 2001 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#ifndef GNC_EMPLOYEE_H_
#define GNC_EMPLOYEE_H_

struct _gncEmployee;
typedef struct _gncEmployee GncEmployee;

#include "gncBusiness.h"
#include "gncAddress.h"

#define GNC_EMPLOYEE_MODULE_NAME "gncEmployee"

/* Create/Destroy Functions */

GncEmployee *gncEmployeeCreate (GncBusiness *business);
void gncEmployeeDestroy (GncEmployee *employee);

/* Set Functions */

void gncEmployeeSetID (GncEmployee *employee, const char *id);
void gncEmployeeSetUsername (GncEmployee *employee, const char *username);
void gncEmployeeSetLanguage (GncEmployee *employee, const char *language);
void gncEmployeeSetAcl (GncEmployee *employee, const char *acl);
void gncEmployeeSetWorkday (GncEmployee *employee, gnc_numeric workday);
void gncEmployeeSetRate (GncEmployee *employee, gnc_numeric rate);
void gncEmployeeSetActive (GncEmployee *employee, gboolean active);

/* Get Functions */

GncBusiness * gncEmployeeGetBusiness (GncEmployee *employee);
const GUID * gncEmployeeGetGUID (GncEmployee *employee);
const char * gncEmployeeGetID (GncEmployee *employee);
const char * gncEmployeeGetUsername (GncEmployee *employee);
GncAddress * gncEmployeeGetAddr (GncEmployee *employee);
const char * gncEmployeeGetLanguage (GncEmployee *employee);
const char * gncEmployeeGetAcl (GncEmployee *employee);
gnc_numeric gncEmployeeGetWorkday (GncEmployee *employee);
gnc_numeric gncEmployeeGetRate (GncEmployee *employee);
gboolean gncEmployeeGetActive (GncEmployee *employee);

gboolean gncEmployeeIsDirty (GncEmployee *employee);

void gncEmployeeCommitEdit (GncEmployee *employee);

#endif /* GNC_EMPLOYEE_H_ */
