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
#include "gnc-book.h"
#include "gnc-event-p.h"
#include "gnc-be-utils.h"

#include "qofclass.h"
#include "qofinstance.h"
#include "qofid.h"
#include "qofid-p.h"
#include "qofquerycore.h"
#include "qofquery.h"

#include "gncBusiness.h"
#include "gncJob.h"
#include "gncJobP.h"

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

#define _GNC_MOD_NAME        GNC_JOB_MODULE_NAME

/* ================================================================== */
/* misc inline functions */

#define CACHE_INSERT(str) g_cache_insert(gnc_engine_get_string_cache(), (gpointer)(str));
#define CACHE_REMOVE(str) g_cache_remove(gnc_engine_get_string_cache(), (str));

G_INLINE_FUNC void mark_job (GncJob *job);
G_INLINE_FUNC void
mark_job (GncJob *job)
{
  job->inst.dirty = TRUE;
  gncBusinessSetDirtyFlag (job->inst.book, _GNC_MOD_NAME, TRUE);
  gnc_engine_generate_event (&job->inst.guid, _GNC_MOD_NAME, GNC_EVENT_MODIFY);
}

static inline void addObj (GncJob *job)
{
  gncBusinessAddObject (job->inst.book, _GNC_MOD_NAME, job, &job->inst.guid);
}

static inline void remObj (GncJob *job)
{
  gncBusinessRemoveObject (job->inst.book, _GNC_MOD_NAME, &job->inst.guid);
}

/* ================================================================== */
/* Create/Destroy Functions */

GncJob *gncJobCreate (QofBook *book)
{
  GncJob *job;

  if (!book) return NULL;

  job = g_new0 (GncJob, 1);
  qof_instance_init (&job->inst, book);

  job->id = CACHE_INSERT ("");
  job->name = CACHE_INSERT ("");
  job->desc = CACHE_INSERT ("");
  job->active = TRUE;

  addObj (job);
  gnc_engine_generate_event (&job->inst.guid, _GNC_MOD_NAME, GNC_EVENT_CREATE);

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

  gnc_engine_generate_event (&job->inst.guid, _GNC_MOD_NAME, GNC_EVENT_DESTROY);

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

  remObj (job);
  qof_instance_release (&job->inst);

  g_free (job);
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

void gncJobSetGUID (GncJob *job, const GUID *guid)
{
  if (!job || !guid) return;
  if (guid_equal (guid, &job->inst.guid)) return;

  gncJobBeginEdit (job);
  remObj (job);
  job->inst.guid = *guid;
  addObj (job);
  gncJobCommitEdit (job);
}

void gncJobSetOwner (GncJob *job, GncOwner *owner)
{
  if (!job) return;
  if (!owner) return;
  if (gncOwnerEqual (owner, &(job->owner))) return;
  /* XXX: Fail if we have ANY orders or invoices */

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
  GNC_BEGIN_EDIT (&job->inst, _GNC_MOD_NAME);
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
  GNC_COMMIT_EDIT_PART2 (&job->inst, _GNC_MOD_NAME, gncJobOnError,
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

GUID gncJobRetGUID (GncJob *job)
{
  const GUID *guid = qof_instance_get_guid (&job->inst);
  if (guid)
    return *guid;
  return *guid_null ();
}

gboolean gncJobGetActive (GncJob *job)
{
  if (!job) return FALSE;
  return job->active;
}

GncJob * gncJobLookup (QofBook *book, const GUID *guid)
{
  if (!book || !guid) return NULL;
  return qof_entity_lookup (gnc_book_get_entity_table (book),
                           guid, _GNC_MOD_NAME);
}

GncJob * gncJobLookupDirect (GUID guid, QofBook *book)
{
  if (!book) return NULL;
  return gncJobLookup (book, &guid);
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

static void _gncJobCreate (QofBook *book)
{
  gncBusinessCreate (book, _GNC_MOD_NAME);
}

static void _gncJobDestroy (QofBook *book)
{
  gncBusinessDestroy (book, _GNC_MOD_NAME);
}

static gboolean _gncJobIsDirty (QofBook *book)
{
  return gncBusinessIsDirty (book, _GNC_MOD_NAME);
}

static void _gncJobMarkClean (QofBook *book)
{
  gncBusinessSetDirtyFlag (book, _GNC_MOD_NAME, FALSE);
}

static void _gncJobForeach (QofBook *book, QofEntityForeachCB cb,
                            gpointer user_data)
{
  gncBusinessForeach (book, _GNC_MOD_NAME, cb, user_data);
}

static const char * _gncJobPrintable (gpointer item)
{
  GncJob *c;

  if (!item) return NULL;

  c = item;
  return c->name;
}

static QofObject gncJobDesc = {
  QOF_OBJECT_VERSION,
  _GNC_MOD_NAME,
  "Job",
  _gncJobCreate,
  _gncJobDestroy,
  _gncJobIsDirty,
  _gncJobMarkClean,
  _gncJobForeach,
  _gncJobPrintable
};

gboolean gncJobRegister (void)
{
  static QofParam params[] = {
    { JOB_ID, QOF_TYPE_STRING, (QofAccessFunc)gncJobGetID, NULL },
    { JOB_NAME, QOF_TYPE_STRING, (QofAccessFunc)gncJobGetName, NULL },
    { JOB_ACTIVE, QOF_TYPE_BOOLEAN, (QofAccessFunc)gncJobGetActive, NULL },
    { JOB_REFERENCE, QOF_TYPE_STRING, (QofAccessFunc)gncJobGetReference, NULL },
    { JOB_OWNER, GNC_OWNER_MODULE_NAME, (QofAccessFunc)gncJobGetOwner, NULL },
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
  return gnc_book_get_counter (book, _GNC_MOD_NAME);
}
