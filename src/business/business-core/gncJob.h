/*
 * gncJob.h -- the Core Job Interface
 * Copyright (C) 2001, 2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#ifndef GNC_JOB_H_
#define GNC_JOB_H_

typedef struct _gncJob GncJob;

#include "gncAddress.h"
#include "gncOwner.h"

#define GNC_JOB_MODULE_NAME "gncJob"

/* Create/Destroy Functions */

GncJob *gncJobCreate (GNCBook *book);
void gncJobDestroy (GncJob *job);

/* Set Functions */

void gncJobSetID (GncJob *job, const char *id);
void gncJobSetName (GncJob *job, const char *jobname);
void gncJobSetReference (GncJob *job, const char *owner_reference);
void gncJobSetOwner (GncJob *job, GncOwner *owner);
void gncJobSetActive (GncJob *job, gboolean active);

void gncJobCommitEdit (GncJob *job);

/* Get Functions */

GNCBook * gncJobGetBook (GncJob *job);
const GUID * gncJobGetGUID (GncJob *job);
const char * gncJobGetID (GncJob *job);
const char * gncJobGetName (GncJob *job);
const char * gncJobGetReference (GncJob *job);
GncOwner * gncJobGetOwner (GncJob *job);
gboolean gncJobGetActive (GncJob *job);

GncJob * gncJobLookup (GNCBook *book, const GUID *guid);
gboolean gncJobIsDirty (GncJob *job);

/* Other functions */

int gncJobCompare (const GncJob *a, const GncJob *b);

#define JOB_GUID	"guid"
#define JOB_ID		"id"
#define JOB_NAME	"name"
#define JOB_REFERENCE	"reference"
#define JOB_OWNER	"owner"

#endif /* GNC_JOB_H_ */
