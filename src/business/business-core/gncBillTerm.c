/*
 * gncBillTerm.c -- the Gnucash Billing Terms interface
 * Copyright (C) 2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#include "config.h"

#include <glib.h>

#include "messages.h"
#include "gnc-numeric.h"
#include "gnc-engine-util.h"
#include "gnc-book-p.h"
#include "GNCIdP.h"
#include "QueryObject.h"
#include "gnc-event-p.h"

#include "gncBusiness.h"
#include "gncBillTermP.h"


struct _gncBillTerm {
  GUID		guid;
  char *	name;
  char *	desc;
  GncBillTermType	type;
  gint		due_days;
  gint		disc_days;
  gnc_numeric	discount;
  gint		cutoff;

  gint64	refcount;
  GNCBook *	book;
  GncBillTerm *	parent;		/* if non-null, we are an immutable child */
  GncBillTerm *	child;		/* if non-null, we have not changed */
  gboolean	invisible;
  gboolean	dirty;
};

struct _book_info {
  GncBookInfo	bi;
  GList *	terms;		/* visible terms */
};

#define _GNC_MOD_NAME	GNC_BILLTERM_MODULE_NAME

#define CACHE_INSERT(str) g_cache_insert(gnc_engine_get_string_cache(), (gpointer)(str));
#define CACHE_REMOVE(str) g_cache_remove(gnc_engine_get_string_cache(), (str));

#define SET_STR(member, str) { \
	char * tmp; \
	\
	if (!safe_strcmp (member, str)) return; \
	tmp = CACHE_INSERT (str); \
	CACHE_REMOVE (member); \
	member = tmp; \
	}

static void add_or_rem_object (GncBillTerm *term, gboolean add);
static void addObj (GncBillTerm *term);
static void remObj (GncBillTerm *term);
static void maybe_resort_list (GncBillTerm *term);

G_INLINE_FUNC void mark_term (GncBillTerm *term);
G_INLINE_FUNC void
mark_term (GncBillTerm *term)
{
  term->dirty = TRUE;

  gnc_engine_generate_event (&term->guid, GNC_EVENT_MODIFY);
}

/* Create/Destroy Functions */
GncBillTerm * gncBillTermCreate (GNCBook *book)
{
  GncBillTerm *term;
  if (!book) return NULL;

  term = g_new0 (GncBillTerm, 1);
  term->book = book;
  term->name = CACHE_INSERT ("");
  term->desc = CACHE_INSERT ("");
  term->discount = gnc_numeric_zero ();
  xaccGUIDNew (&term->guid, book);
  addObj (term);
  gnc_engine_generate_event (&term->guid, GNC_EVENT_CREATE);
  return term;
}

void gncBillTermDestroy (GncBillTerm *term)
{
  if (!term) return;

  gnc_engine_generate_event (&term->guid, GNC_EVENT_DESTROY);
  CACHE_REMOVE (term->name);
  CACHE_REMOVE (term->desc);
  remObj (term);

  g_free (term);
}

/* Set Functions */
void gncBillTermSetGUID (GncBillTerm *term, const GUID *guid)
{
  if (!term || !guid) return;
  if (guid_equal (guid, &term->guid)) return;

  remObj (term);
  term->guid = *guid;
  addObj (term);
}

void gncBillTermSetName (GncBillTerm *term, const char *name)
{
  if (!term || !name) return;
  SET_STR (term->name, name);
  mark_term (term);
  maybe_resort_list (term);
}

void gncBillTermSetDescription (GncBillTerm *term, const char *desc)
{
  if (!term || !desc) return;
  SET_STR (term->desc, desc);
  mark_term (term);
  maybe_resort_list (term);
}

void gncBillTermSetType (GncBillTerm *term, GncBillTermType type)
{
  if (!term) return;
  if (term->type == type) return;
  term->type = type;
  mark_term (term);
}

void gncBillTermSetDueDays (GncBillTerm *term, gint days)
{
  if (!term) return;
  if (term->due_days == days) return;
  term->due_days = days;
  mark_term (term);
}

void gncBillTermSetDiscountDays (GncBillTerm *term, gint days)
{
  if (!term) return;
  if (term->disc_days == days) return;
  term->disc_days = days;
  mark_term (term);
}

void gncBillTermSetDiscount (GncBillTerm *term, gnc_numeric discount)
{
  if (!term) return;
  if (gnc_numeric_eq (term->discount, discount)) return;
  term->discount = discount;
  mark_term (term);
}

void gncBillTermSetCutoff (GncBillTerm *term, gint cutoff)
{
  if (!term) return;
  if (term->cutoff == cutoff) return;
  term->cutoff = cutoff;
  mark_term (term);
}

void gncBillTermSetParent (GncBillTerm *term, GncBillTerm *parent)
{
  if (!term) return;
  term->parent = parent;
  term->refcount = 0;
  gncBillTermMakeInvisible (term);
}

void gncBillTermSetChild (GncBillTerm *term, GncBillTerm *child)
{
  if (!term) return;
  term->child = child;
}

void gncBillTermIncRef (GncBillTerm *term)
{
  if (!term) return;
  if (term->parent) return;	/* children dont need refcounts */
  term->refcount++;
}

void gncBillTermDecRef (GncBillTerm *term)
{
  if (!term) return;
  if (term->parent) return;	/* children dont need refcounts */
  term->refcount--;
  g_return_if_fail (term->refcount >= 0);
}

void gncBillTermSetRefcount (GncBillTerm *term, gint64 refcount)
{
  if (!term) return;
  term->refcount = refcount;
}

void gncBillTermMakeInvisible (GncBillTerm *term)
{
  if (!term) return;
  term->invisible = TRUE;
  add_or_rem_object (term, FALSE);
}

void gncBillTermChanged (GncBillTerm *term)
{
  if (!term) return;
  term->child = NULL;
}

void gncBillTermCommitEdit (GncBillTerm *term)
{
  if (!term) return;

  /* XXX Commit to DB */
  if (term->dirty)
    gncBusinessSetDirtyFlag (term->book, _GNC_MOD_NAME, TRUE);
  term->dirty = FALSE;
}


/* Get Functions */
GncBillTerm * gncBillTermLookup (GNCBook *book, const GUID *guid)
{
  if (!book || !guid) return NULL;
  return xaccLookupEntity (gnc_book_get_entity_table (book),
			   guid, _GNC_MOD_NAME);
}

GncBillTerm *gncBillTermLookupByName (GNCBook *book, const char *name)
{
  GList *list = gncBillTermGetTerms (book);

  for ( ; list; list = list->next) {
    GncBillTerm *term = list->data;
    if (!safe_strcmp (term->name, name))
      return list->data;
  }
  return NULL;
}

GList * gncBillTermGetTerms (GNCBook *book)
{
  struct _book_info *bi;
  if (!book) return NULL;

  bi = gnc_book_get_data (book, _GNC_MOD_NAME);
  return bi->terms;
}


const GUID *gncBillTermGetGUID (GncBillTerm *term)
{
  if (!term) return NULL;
  return &term->guid;
}

GNCBook *gncBillTermGetBook (GncBillTerm *term)
{
  if (!term) return NULL;
  return term->book;
}

const char *gncBillTermGetName (GncBillTerm *term)
{
  if (!term) return NULL;
  return term->name;
}

const char *gncBillTermGetDescription (GncBillTerm *term)
{
  if (!term) return NULL;
  return term->desc;
}

GncBillTermType gncBillTermGetType (GncBillTerm *term)
{
  if (!term) return 0;
  return term->type;
}

gint gncBillTermGetDueDays (GncBillTerm *term)
{
  if (!term) return 0;
  return term->due_days;
}

gint gncBillTermGetDiscountDays (GncBillTerm *term)
{
  if (!term) return 0;
  return term->disc_days;
}

gnc_numeric gncBillTermGetDiscount (GncBillTerm *term)
{
  if (!term) return gnc_numeric_zero ();
  return term->discount;
}

gint gncBillTermGetCutoff (GncBillTerm *term)
{
  if (!term) return 0;
  return term->cutoff;
}

static GncBillTerm *gncBillTermCopy (GncBillTerm *term)
{
  GncBillTerm *t;

  if (!term) return NULL;
  t = gncBillTermCreate (term->book);
  gncBillTermSetName (t, term->name);
  gncBillTermSetDescription (t, term->desc);

  return t;
}

GncBillTerm *gncBillTermReturnChild (GncBillTerm *term, gboolean make_new)
{
  GncBillTerm *child = NULL;

  if (!term) return NULL;
  if (term->child) return term->child;
  if (make_new) {
    child = gncBillTermCopy (term);
    gncBillTermSetChild (term, child);
    gncBillTermSetParent (child, term);
  }
  return child;
}

GncBillTerm *gncBillTermGetParent (GncBillTerm *term)
{
  if (!term) return NULL;
  return term->parent;
}

gint64 gncBillTermGetRefcount (GncBillTerm *term)
{
  if (!term) return 0;
  return term->refcount;
}

gboolean gncBillTermGetInvisible (GncBillTerm *term)
{
  if (!term) return FALSE;
  return term->invisible;
}

int gncBillTermCompare (GncBillTerm *a, GncBillTerm *b)
{
  int ret;

  if (!a && !b) return 0;
  if (!a) return -1;
  if (!b) return 1;

  ret = safe_strcmp (a->name, b->name);
  if (ret) return ret;

  return safe_strcmp (a->desc, b->desc);
}

gboolean gncBillTermIsDirty (GncBillTerm *term)
{
  if (!term) return FALSE;
  return term->dirty;
}

/********************************************************/
/* functions to compute dates from Bill Terms           */

#define SECS_PER_DAY 86400

/* Based on the timespec and a proximo type, compute the month and
 * year this is due.  The actual day is filled in below.
 */
static void
compute_monthyear (GncBillTerm *term, Timespec post_date,
		   int *month, int *year)
{
  int iday, imonth, iyear;
  int cutoff = term->cutoff;

  g_return_if_fail (term->type == GNC_TERM_TYPE_PROXIMO);

  gnc_timespec2dmy (post_date, &iday, &imonth, &iyear);

  if (cutoff <= 0)
    cutoff += gnc_timespec_last_mday (post_date);

  if (iday <= cutoff) {
    /* We apply this to next month */
    imonth++;
  } else {
    /* We apply to the following month */
    imonth += 2;
  }

  if (imonth > 12) {
    iyear++;
    imonth -= 12;
  }

  if (month) *month = imonth;
  if (year) *year = iyear;
}

static Timespec
compute_time (GncBillTerm *term, Timespec post_date, int days)
{
  Timespec res = post_date;
  int day, month, year;

  switch (term->type) {
  case GNC_TERM_TYPE_DAYS:
    res.tv_sec += (SECS_PER_DAY * days);
    break;
  case GNC_TERM_TYPE_PROXIMO:
    compute_monthyear (term, post_date, &month, &year);
    day = gnc_date_my_last_mday (month, year);
    if (days < day)
      day = days;
    res = gnc_dmy2timespec (day, month, year);
    break;
  }
  return res;
}

Timespec
gncBillTermComputeDueDate (GncBillTerm *term, Timespec post_date)
{
  Timespec res = post_date;
  if (!term) return res;

  return compute_time (term, post_date, term->due_days);
}

Timespec
gncBillTermComputeDiscountDate (GncBillTerm *term, Timespec post_date)
{
  Timespec res = post_date;
  if (!term) return res;

  return compute_time (term, post_date, term->disc_days);
}

/* Package-Private functions */

static void maybe_resort_list (GncBillTerm *term)
{
  struct _book_info *bi;

  if (term->parent || term->invisible) return;
  bi = gnc_book_get_data (term->book, _GNC_MOD_NAME);
  bi->terms = g_list_sort (bi->terms, (GCompareFunc)gncBillTermCompare);
}

static void add_or_rem_object (GncBillTerm *term, gboolean add)
{
  struct _book_info *bi;

  if (!term) return;
  bi = gnc_book_get_data (term->book, _GNC_MOD_NAME);

  if (add)
    bi->terms = g_list_insert_sorted (bi->terms, term,
				       (GCompareFunc)gncBillTermCompare);
  else
    bi->terms = g_list_remove (bi->terms, term);
}

static void addObj (GncBillTerm *term)
{
  gncBusinessAddObject (term->book, _GNC_MOD_NAME, term, &term->guid);
  add_or_rem_object (term, TRUE);
}

static void remObj (GncBillTerm *term)
{
  gncBusinessRemoveObject (term->book, _GNC_MOD_NAME, &term->guid);
  add_or_rem_object (term, FALSE);
}

static void _gncBillTermCreate (GNCBook *book)
{
  struct _book_info *bi;

  if (!book) return;

  bi = g_new0 (struct _book_info, 1);
  bi->bi.ht = guid_hash_table_new ();
  gnc_book_set_data (book, _GNC_MOD_NAME, bi);
}

static void _gncBillTermDestroy (GNCBook *book)
{
  struct _book_info *bi;

  if (!book) return;

  bi = gnc_book_get_data (book, _GNC_MOD_NAME);

  /* XXX : Destroy the objects? */
  g_hash_table_destroy (bi->bi.ht);
  g_list_free (bi->terms);
  g_free (bi);
}

static gboolean _gncBillTermIsDirty (GNCBook *book)
{
  return gncBusinessIsDirty (book, _GNC_MOD_NAME);
}

static void _gncBillTermMarkClean (GNCBook *book)
{
  gncBusinessSetDirtyFlag (book, _GNC_MOD_NAME, FALSE);
}

static void _gncBillTermForeach (GNCBook *book, foreachObjectCB cb,
			      gpointer user_data)
{
  gncBusinessForeach (book, _GNC_MOD_NAME, cb, user_data);
}

static GncObject_t gncBillTermDesc = {
  GNC_OBJECT_VERSION,
  _GNC_MOD_NAME,
  "Billing Term",
  _gncBillTermCreate,
  _gncBillTermDestroy,
  _gncBillTermIsDirty,
  _gncBillTermMarkClean,
  _gncBillTermForeach,
  NULL				/* printable */
};

gboolean gncBillTermRegister (void)
{
  static QueryObjectDef params[] = {
    { QUERY_PARAM_BOOK, GNC_ID_BOOK, (QueryAccess)gncBillTermGetBook },
    { QUERY_PARAM_GUID, QUERYCORE_GUID, (QueryAccess)gncBillTermGetGUID },
    { NULL },
  };

  gncQueryObjectRegister (_GNC_MOD_NAME, (QuerySort)gncBillTermCompare, params);

  return gncObjectRegister (&gncBillTermDesc);
}
