/*
 * gncJob.h -- the Core Job Interface
 * Copyright (C) 2001 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#ifndef GNC_JOB_H_
#define GNC_JOB_H_

struct _gncJob;
typedef struct _gncJob GncJob;

#include "gncBusiness.h"
#include "gncAddress.h"
#include "gncCustomer.h"

#define GNC_JOB_MODULE_NAME "gncJob"

/* Create/Destroy Functions */

GncJob *gncJobCreate (GncBusiness *business);
void gncJobDestroy (GncJob *job);

/* Set Functions */

void gncJobSetID (GncJob *job, const char *id);
void gncJobSetName (GncJob *job, const char *username);
void gncJobSetDesc (GncJob *job, const char *language);
void gncJobSetCustomer (GncJob *job, GncCustomer *customer);
void gncJobSetActive (GncJob *job, gboolean active);

void gncJobCommitEdit (GncJob *job);

/* Get Functions */

GncBusiness * gncJobGetBusiness (GncJob *job);
const GUID * gncJobGetGUID (GncJob *job);
const char * gncJobGetID (GncJob *job);
const char * gncJobGetName (GncJob *job);
const char * gncJobGetDesc (GncJob *job);
GncCustomer * gncJobGetCustomer (GncJob *job);
gboolean gncJobGetActive (GncJob *job);

gboolean gncJobIsDirty (GncJob *job);

/* Other functions */

gint gncJobSortFunc (gconstpointer a, gconstpointer b);

#endif /* GNC_JOB_H_ */
