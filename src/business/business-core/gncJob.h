/********************************************************************\
 * gncJob.h -- the Core Job Interface                               *
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

GncJob *gncJobCreate (QofBook *book);
void gncJobDestroy (GncJob *job);

/* Set Functions */

void gncJobSetID (GncJob *job, const char *id);
void gncJobSetName (GncJob *job, const char *jobname);
void gncJobSetReference (GncJob *job, const char *owner_reference);
void gncJobSetOwner (GncJob *job, GncOwner *owner);
void gncJobSetActive (GncJob *job, gboolean active);

void gncJobBeginEdit (GncJob *job);
void gncJobCommitEdit (GncJob *job);

/* Get Functions */

const char * gncJobGetID (GncJob *job);
const char * gncJobGetName (GncJob *job);
const char * gncJobGetReference (GncJob *job);
GncOwner * gncJobGetOwner (GncJob *job);
gboolean gncJobGetActive (GncJob *job);

GUID gncJobRetGUID (GncJob *job);
GncJob *gncJobLookupDirect (GUID guid, QofBook *book);

GncJob * gncJobLookup (QofBook *book, const GUID *guid);
gboolean gncJobIsDirty (GncJob *job);

/* Other functions */

int gncJobCompare (const GncJob *a, const GncJob *b);

#define JOB_ID		"id"
#define JOB_NAME	"name"
#define JOB_REFERENCE	"reference"
#define JOB_OWNER	"owner"
#define JOB_ACTIVE	"active"

/** deprecated functions */
#define gncJobGetBook(x) qof_instance_get_book(QOF_INSTANCE(x))
#define gncJobGetGUID(x) qof_instance_get_guid(QOF_INSTANCE(x))

#endif /* GNC_JOB_H_ */
