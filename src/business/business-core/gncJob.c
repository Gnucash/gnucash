/********************************************************************\
 * gncJob.c -- the Core Job Interface                               *
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
 * Copyright (C) 2003 Linas Vepstas <linas@linas.org>
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#include "config.h"

#include <glib.h>
#include <string.h>

#include "guid.h"
#include "messages.h"
#include "gnc-engine-util.h"
#include "gnc-numeric.h"
#include "gnc-event-p.h"
#include "gnc-be-utils.h"

#include "qofbook.h"
#include "qofclass.h"
#include "qofinstance.h"
#include "qofinstance-p.h"
#include "qofid.h"
#include "qofid-p.h"
#include "qofobject.h"
#include "qofquery.h"
#include "qofquerycore.h"

#include "gncBusiness.h"
#include "gncJob.h"
#include "gncJobP.h"
#include "gncOwnerP.h"

struct _gncJob 
{
  QofInstance inst; 
  char *        id;
  char *        name;
  char *        desc;
  GncOwner      owner;
  gboolean      active;
};

static short        module = MOD_BUSINESS;

#define _GNC_MOD_NAME        GNC_ID_JOB

/* ================================================================== */
/* misc inline functions */

#define CACHE_INSERT(str) g_cache_insert(gnc_engine_get_string_cache(), (gpointer)(str));
#define CACHE_REMOVE(str) g_cache_remove(gnc_engine_get_string_cache(), (str));

G_INLINE_FUNC void mark_job (GncJob *job);
G_INLINE_FUNC void
mark_job (GncJob *job)
{
  job->inst.dirty = TRUE;
  qof_collection_mark_dirty (job->inst.entity.collection);
  gnc_engine_gen_event (&job->inst.entity, GNC_EVENT_MODIFY);
}

/* ================================================================== */
/* Create/Destroy Functions */

GncJob *gncJobCreate (QofBook *book)
{
  GncJob *job;

  if (!book) return NULL;

  job = g_new0 (GncJob, 1);
  qof_instance_init (&job->inst, _GNC_MOD_NAME, book);

  job->id = CACHE_INSERT ("");
  job->name = CACHE_INSERT ("");
  job->desc = CACHE_INSERT ("");
  job->active = TRUE;

  /* GncOwner not initialized */
  gnc_engine_gen_event (&job->inst.entity, GNC_EVENT_CREATE);

  return job;
}

GncJob *
gncCloneJob (GncJob *from, QofBook *book)
{
  GncJob *job;
                                                                                
  if (!book) return NULL;
                                                                                
  job = g_new0 (GncJob, 1);
  qof_instance_init (&job->inst, _GNC_MOD_NAME, book);
  qof_instance_gemini (&job->inst, &from->inst);
                                                                                
  job->id = CACHE_INSERT (from->id);
  job->name = CACHE_INSERT (from->name);
  job->desc = CACHE_INSERT (from->desc);
  job->active = from->active;

  job->owner = gncCloneOwner(&from->owner, book);

  gnc_engine_gen_event (&job->inst.entity, GNC_EVENT_CREATE);
                                                                                
  return job;
}

void gncJobDestroy (GncJob *job)
{
  if (!job) return;
  job->inst.do_free = TRUE;
  gncJobCommitEdit (job);
}

static void gncJobFree (GncJob *job)
{
  if (!job) return;

  gnc_engine_gen_event (&job->inst.entity, GNC_EVENT_DESTROY);

  CACHE_REMOVE (job->id);
  CACHE_REMOVE (job->name);
  CACHE_REMOVE (job->desc);

  switch (gncOwnerGetType (&(job->owner))) {
  case GNC_OWNER_CUSTOMER:
    gncCustomerRemoveJob (gncOwnerGetCustomer(&job->owner), job);
    break;
  case GNC_OWNER_VENDOR:
    gncVendorRemoveJob (gncOwnerGetVendor(&job->owner), job);
    break;
  default:
    break;
  }

  qof_instance_release (&job->inst);
  g_free (job);
}

GncJob *
gncJobObtainTwin (GncJob *from, QofBook *book)
{
  GncJob *job;
  if (!from) return NULL;
                                                                                
  job = (GncJob *) qof_instance_lookup_twin (QOF_INSTANCE(from), book);
  if (!job)
  {
    job = gncCloneJob (from, book);
  }
  return job;
}

/* ================================================================== */
/* Set Functions */

#define SET_STR(obj, member, str) { \
        char * tmp; \
        \
        if (!safe_strcmp (member, str)) return; \
        gncJobBeginEdit (obj); \
        tmp = CACHE_INSERT (str); \
        CACHE_REMOVE (member); \
        member = tmp; \
        }

void gncJobSetID (GncJob *job, const char *id)
{
  if (!job) return;
  if (!id) return;
  SET_STR(job, job->id, id);
  mark_job (job);
  gncJobCommitEdit (job);
}

void gncJobSetName (GncJob *job, const char *name)
{
  if (!job) return;
  if (!name) return;
  SET_STR(job, job->name, name);
  mark_job (job);
  gncJobCommitEdit (job);
}

void gncJobSetReference (GncJob *job, const char *desc)
{
  if (!job) return;
  if (!desc) return;
  SET_STR(job, job->desc, desc);
  mark_job (job);
  gncJobCommitEdit (job);
}

void gncJobSetOwner (GncJob *job, GncOwner *owner)
{
  if (!job) return;
  if (!owner) return;
  if (gncOwnerEqual (owner, &(job->owner))) return;

  switch (gncOwnerGetType (owner)) {
  case GNC_OWNER_CUSTOMER:
  case GNC_OWNER_VENDOR:
    break;
  default:
    PERR("Unsupported Owner type: %d", gncOwnerGetType(owner));
    return;
  }

  gncJobBeginEdit (job);

  switch (gncOwnerGetType (&(job->owner))) {
  case GNC_OWNER_CUSTOMER:
    gncCustomerRemoveJob (gncOwnerGetCustomer(&job->owner), job);
    break;
  case GNC_OWNER_VENDOR:
    gncVendorRemoveJob (gncOwnerGetVendor(&job->owner), job);
    break;
  default:
    break;
  }

  gncOwnerCopy (owner, &(job->owner));

  switch (gncOwnerGetType (&(job->owner))) {
  case GNC_OWNER_CUSTOMER:
    gncCustomerAddJob (gncOwnerGetCustomer(&job->owner), job);
    break;
  case GNC_OWNER_VENDOR:
    gncVendorAddJob (gncOwnerGetVendor(&job->owner), job);
    break;
  default:
    break;
  }

  mark_job (job);
  gncJobCommitEdit (job);
}

void gncJobSetActive (GncJob *job, gboolean active)
{
  if (!job) return;
  if (active == job->active) return;
  gncJobBeginEdit (job);
  job->active = active;
  mark_job (job);
  gncJobCommitEdit (job);
}

void gncJobBeginEdit (GncJob *job)
{
  GNC_BEGIN_EDIT (&job->inst);
}

static void gncJobOnError (QofInstance *inst, QofBackendError errcode)
{
  PERR("Job QofBackend Failure: %d", errcode);
}

static inline void job_free (QofInstance *inst)
{
  GncJob *job = (GncJob *)inst;
  gncJobFree (job);
}

static inline void gncJobOnDone (QofInstance *qof) { }

void gncJobCommitEdit (GncJob *job)
{
  GNC_COMMIT_EDIT_PART1 (&job->inst);
  GNC_COMMIT_EDIT_PART2 (&job->inst, gncJobOnError,
                         gncJobOnDone, job_free);
}

/* ================================================================== */
/* Get Functions */

const char * gncJobGetID (GncJob *job)
{
  if (!job) return NULL;
  return job->id;
}

const char * gncJobGetName (GncJob *job)
{
  if (!job) return NULL;
  return job->name;
}

const char * gncJobGetReference (GncJob *job)
{
  if (!job) return NULL;
  return job->desc;
}

GncOwner * gncJobGetOwner (GncJob *job)
{
  if (!job) return NULL;
  return &(job->owner);
}

gboolean gncJobGetActive (GncJob *job)
{
  if (!job) return FALSE;
  return job->active;
}

/* Other functions */

int gncJobCompare (const GncJob * a, const GncJob *b) {
  if (!a && !b) return 0;
  if (!a && b) return 1;
  if (a && !b) return -1;

  return (safe_strcmp(a->id, b->id));
}

/* ================================================================== */
/* Package-Private functions */

static const char * _gncJobPrintable (gpointer item)
{
  GncJob *c;
  if (!item) return NULL;
  c = item;
  return c->name;
}

static QofObject gncJobDesc = 
{
  interface_version:  QOF_OBJECT_VERSION,
  e_type:             _GNC_MOD_NAME,
  type_label:         "Job",
  book_begin:         NULL,
  book_end:           NULL,
  is_dirty:           qof_collection_is_dirty,
  mark_clean:         qof_collection_mark_clean,
  foreach:            qof_collection_foreach,
  printable:          _gncJobPrintable
};

gboolean gncJobRegister (void)
{
  static QofParam params[] = {
    { JOB_ID, QOF_TYPE_STRING, (QofAccessFunc)gncJobGetID, NULL },
    { JOB_NAME, QOF_TYPE_STRING, (QofAccessFunc)gncJobGetName, NULL },
    { JOB_ACTIVE, QOF_TYPE_BOOLEAN, (QofAccessFunc)gncJobGetActive, NULL },
    { JOB_REFERENCE, QOF_TYPE_STRING, (QofAccessFunc)gncJobGetReference, NULL },
    { JOB_OWNER, GNC_ID_OWNER, (QofAccessFunc)gncJobGetOwner, NULL },
    { QOF_QUERY_PARAM_ACTIVE, QOF_TYPE_BOOLEAN, (QofAccessFunc)gncJobGetActive, NULL },
    { QOF_QUERY_PARAM_BOOK, QOF_ID_BOOK, (QofAccessFunc)qof_instance_get_book, NULL },
    { QOF_QUERY_PARAM_GUID, QOF_TYPE_GUID, (QofAccessFunc)qof_instance_get_guid, NULL },
    { NULL },
  };

  qof_class_register (_GNC_MOD_NAME, (QofSortFunc)gncJobCompare, params);

  return qof_object_register (&gncJobDesc);
}

gint64 gncJobNextID (QofBook *book)
{
  return qof_book_get_counter (book, _GNC_MOD_NAME);
}
