/*
 * gncJob.c -- the Core Job Interface
 * Copyright (C) 2001 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#include "config.h"

#include <glib.h>
#include <string.h>

#include "guid.h"
#include "messages.h"
#include "gnc-engine-util.h"
#include "gnc-numeric.h"
#include "gnc-book-p.h"
#include "GNCIdP.h"

#include "gncBusiness.h"
#include "gncJob.h"
#include "gncJobP.h"
#include "gncCustomer.h"

struct _gncJob {
  GNCBook *	book;
  GUID		guid;
  char *	id;
  char *	name;
  char *	desc;
  GncCustomer *	cust;
  gboolean	active;
  gboolean	dirty;
};

#define _GNC_MOD_NAME	GNC_JOB_MODULE_NAME

#define CACHE_INSERT(str) g_cache_insert(gnc_engine_get_string_cache(), (gpointer)(str));
#define CACHE_REMOVE(str) g_cache_remove(gnc_engine_get_string_cache(), (str));

static void addObj (GncJob *job);
static void remObj (GncJob *job);

/* Create/Destroy Functions */

GncJob *gncJobCreate (GNCBook *book)
{
  GncJob *job;

  if (!book) return NULL;

  job = g_new0 (GncJob, 1);
  job->book = book;
  job->dirty = FALSE;

  job->id = CACHE_INSERT ("");
  job->name = CACHE_INSERT ("");
  job->desc = CACHE_INSERT ("");
  job->cust = NULL;
  job->active = TRUE;

  xaccGUIDNew (&job->guid, book);
  addObj (job);

  return job;
}

void gncJobDestroy (GncJob *job)
{
  if (!job) return;

  CACHE_REMOVE (job->id);
  CACHE_REMOVE (job->name);
  CACHE_REMOVE (job->desc);

  if (job->cust)
    gncCustomerRemoveJob (job->cust, job);

  remObj (job);

  g_free (job);
}

/* Set Functions */

#define SET_STR(member, str) { \
	char * tmp; \
	\
	tmp = CACHE_INSERT (str); \
	CACHE_REMOVE (member); \
	member = tmp; \
	}

void gncJobSetID (GncJob *job, const char *id)
{
  if (!job) return;
  if (!id) return;
  SET_STR(job->id, id);
  job->dirty = TRUE;
}

void gncJobSetName (GncJob *job, const char *name)
{
  if (!job) return;
  if (!name) return;
  SET_STR(job->name, name);
  job->dirty = TRUE;
}

void gncJobSetDesc (GncJob *job, const char *desc)
{
  if (!job) return;
  if (!desc) return;
  SET_STR(job->desc, desc);
  job->dirty = TRUE;
}

void gncJobSetGUID (GncJob *job, const GUID *guid)
{
  if (!job || !guid) return;
  if (guid_equal (guid, &job->guid)) return;

  remObj (job);
  job->guid = *guid;
  addObj (job);
}

void gncJobSetCustomer (GncJob *job, GncCustomer *cust)
{
  if (!job) return;
  if (!cust) return;
  if (cust == job->cust) return;
  /* XXX: Fail if we have ANY orders or invoices */

  if (cust)
    gncCustomerRemoveJob (job->cust, job);
  job->cust = cust;
  gncCustomerAddJob (cust, job);

  job->dirty = TRUE;
}

void gncJobSetActive (GncJob *job, gboolean active)
{
  if (!job) return;
  if (active == job->active) return;
  job->active = active;
  job->dirty = TRUE;
}

void gncJobCommitEdit (GncJob *job)
{
  /* XXX: COMMIT TO DATABASE */
  job->dirty = FALSE;
}

/* Get Functions */

GNCBook * gncJobGetBook (GncJob *job)
{
  if (!job) return NULL;
  return job->book;
}

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

const char * gncJobGetDesc (GncJob *job)
{
  if (!job) return NULL;
  return job->desc;
}

GncCustomer * gncJobGetCustomer (GncJob *job)
{
  if (!job) return NULL;
  return job->cust;
}

const GUID * gncJobGetGUID (GncJob *job)
{
  if (!job) return NULL;
  return &job->guid;
}

gboolean gncJobGetActive (GncJob *job)
{
  if (!job) return FALSE;
  return job->active;
}

GncJob * gncJobLookup (GNCBook *book, const GUID *guid)
{
  if (!book || !guid) return NULL;
  return xaccLookupEntity (gnc_book_get_entity_table (book),
			   guid, _GNC_MOD_NAME);
}

gboolean gncJobIsDirty (GncJob *job)
{
  if (!job) return FALSE;
  return job->dirty;
}

/* Other functions */

gint gncJobSortFunc (gconstpointer a, gconstpointer b) {
  GncJob *ja = (GncJob *) a;
  GncJob *jb = (GncJob *) b;

  if (!a || !b) return 0;

  return (safe_strcmp(ja->id, jb->id));
}


/* Package-Private functions */

static void addObj (GncJob *job)
{
  GHashTable *ht;

  xaccStoreEntity (gnc_book_get_entity_table (job->book),
		   job, &job->guid, _GNC_MOD_NAME);

  ht = gnc_book_get_data (job->book, _GNC_MOD_NAME);
  g_hash_table_insert (ht, &job->guid, job);
}

static void remObj (GncJob *job)
{
  GHashTable *ht;

  xaccRemoveEntity (gnc_book_get_entity_table (job->book), &job->guid);
  ht = gnc_book_get_data (job->book, _GNC_MOD_NAME);
  g_hash_table_remove (ht, &job->guid);
}

static GList * _gncJobGetList (GNCBook *obj, gboolean show_all)
{
  if (!obj) return NULL;

  /* XXX */
  return NULL;
}

static const char * _gncJobPrintable (gpointer item)
{
  GncJob *c;

  if (!item) return NULL;

  c = item;
  return c->name;
}

static void _gncJobCreate (GNCBook *book)
{
  GHashTable *ht;

  if (!book) return;

  ht = guid_hash_table_new ();
  gnc_book_set_data (book, _GNC_MOD_NAME, ht);
}

static void _gncJobDestroy (GNCBook *book)
{
  GHashTable *ht;

  if (!book) return;

  ht = gnc_book_get_data (book, _GNC_MOD_NAME);

  /* XXX : Destroy the objects? */
  g_hash_table_destroy (ht);
}

static GncBusinessObject gncJobDesc = {
  GNC_BUSINESS_VERSION,
  GNC_JOB_MODULE_NAME,
  "Job",
  _gncJobCreate,
  _gncJobDestroy,
  NULL,				/* get_list */
  _gncJobPrintable
};

gboolean gncJobRegister (void)
{
  return gncBusinessRegister (&gncJobDesc);
}

static gint lastJob = 57;

gint gncJobNextID (GNCBook *book)
{
  return ++lastJob;	/* XXX: Look into Database! */
}
