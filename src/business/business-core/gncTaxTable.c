/*
 * gncTaxTable.c -- the Gnucash Tax Table interface
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
#include "gncTaxTableP.h"


struct _gncTaxTable {
  GUID		guid;
  char *	name;
  GList *	entries;

  Timespec	modtime;	/* internal date of last modtime */
  gint64	refcount;
  GNCBook *	book;
  GncTaxTable *	parent;		/* if non-null, we are an immutable child */
  GncTaxTable *	child;		/* if non-null, we have not changed */
  gboolean	invisible;
  gboolean	dirty;
};

struct _gncTaxTableEntry {
  GncTaxTable *	table;
  Account *	account;
  GncAmountType	type;
  gnc_numeric	amount;
};

struct _book_info {
  GncBookInfo	bi;
  GList *	tables;		/* visible tables */
};

#define _GNC_MOD_NAME	GNC_TAXTABLE_MODULE_NAME

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

static void add_or_rem_object (GncTaxTable *table, gboolean add);
static void addObj (GncTaxTable *table);
static void remObj (GncTaxTable *table);

G_INLINE_FUNC void mark_table (GncTaxTable *table);
G_INLINE_FUNC void
mark_table (GncTaxTable *table)
{
  table->dirty = TRUE;

  gnc_engine_generate_event (&table->guid, GNC_EVENT_MODIFY);
}

G_INLINE_FUNC void mod_table (GncTaxTable *table);
G_INLINE_FUNC void
mod_table (GncTaxTable *table)
{
  timespecFromTime_t (&table->modtime, time(NULL));
}

/* Create/Destroy Functions */
GncTaxTable * gncTaxTableCreate (GNCBook *book)
{
  GncTaxTable *table;
  if (!book) return NULL;

  table = g_new0 (GncTaxTable, 1);
  table->book = book;
  table->name = CACHE_INSERT ("");
  xaccGUIDNew (&table->guid, book);
  addObj (table);
  gnc_engine_generate_event (&table->guid, GNC_EVENT_CREATE);
  return table;
}

void gncTaxTableDestroy (GncTaxTable *table)
{
  GList *list;
  if (!table) return;

  gnc_engine_generate_event (&table->guid, GNC_EVENT_DESTROY);
  CACHE_REMOVE (table->name);
  remObj (table);

  for (list = table->entries; list; list=list->next)
    gncTaxTableEntryDestroy (list->data);

  g_list_free (table->entries);
  g_free (table);
}

GncTaxTableEntry * gncTaxTableEntryCreate (void)
{
  GncTaxTableEntry *entry;
  entry = g_new0 (GncTaxTableEntry, 1);
  entry->amount = gnc_numeric_zero ();
  return entry;
}

void gncTaxTableEntryDestroy (GncTaxTableEntry *entry)
{
  if (!entry) return;
  g_free (entry);
}


/* Set Functions */
void gncTaxTableSetGUID (GncTaxTable *table, const GUID *guid)
{
  if (!table || !guid) return;
  if (guid_equal (guid, &table->guid)) return;

  remObj (table);
  table->guid = *guid;
  addObj (table);
}

void gncTaxTableSetName (GncTaxTable *table, const char *name)
{
  if (!table || !name) return;
  SET_STR (table->name, name);
  mark_table (table);
}

void gncTaxTableSetParent (GncTaxTable *table, GncTaxTable *parent)
{
  if (!table) return;
  table->parent = parent;
  table->refcount = 0;
  gncTaxTableMakeInvisible (table);
}

void gncTaxTableSetChild (GncTaxTable *table, GncTaxTable *child)
{
  if (!table) return;
  table->child = child;
}

void gncTaxTableIncRef (GncTaxTable *table)
{
  if (!table) return;
  if (table->parent) return;	/* children dont need refcounts */
  table->refcount++;
}

void gncTaxTableDecRef (GncTaxTable *table)
{
  if (!table) return;
  if (table->parent) return;	/* children dont need refcounts */
  table->refcount--;
  g_return_if_fail (table->refcount >= 0);
}

void gncTaxTableSetRefcount (GncTaxTable *table, gint64 refcount)
{
  if (!table) return;
  table->refcount = refcount;
}

void gncTaxTableMakeInvisible (GncTaxTable *table)
{
  if (!table) return;
  table->invisible = TRUE;
  add_or_rem_object (table, FALSE);
}

void gncTaxTableEntrySetAccount (GncTaxTableEntry *entry, Account *account)
{
  if (!entry || !account) return;
  if (entry->account == account) return;
  entry->account = account;
  if (entry->table) {
    mark_table (entry->table);
    mod_table (entry->table);
  }
}

void gncTaxTableEntrySetType (GncTaxTableEntry *entry, GncAmountType type)
{
  if (!entry) return;
  if (entry->type == type) return;
  entry->type = type;
  if (entry->table) {
    mark_table (entry->table);
    mod_table (entry->table);
  }
}

void gncTaxTableEntrySetAmount (GncTaxTableEntry *entry, gnc_numeric amount)
{
  if (!entry) return;
  if (gnc_numeric_eq (entry->amount, amount)) return;
  entry->amount = amount;
  if (entry->table) {
    mark_table (entry->table);
    mod_table (entry->table);
  }
}

void gncTaxTableAddEntry (GncTaxTable *table, GncTaxTableEntry *entry)
{
  if (!table || !entry) return;
  if (entry->table == table) return; /* already mine */
  if (entry->table)
    gncTaxTableRemoveEntry (entry->table, entry);

  entry->table = table;
  table->entries = g_list_insert_sorted (table->entries, entry,
					 (GCompareFunc)gncTaxTableEntryCompare);
  mark_table (table);
  mod_table (table);
}

void gncTaxTableRemoveEntry (GncTaxTable *table, GncTaxTableEntry *entry)
{
  if (!table || !entry) return;
  entry->table = NULL;
  table->entries = g_list_remove (table->entries, entry);
  mark_table (table);
  mod_table (table);
}

void gncTaxTableChanged (GncTaxTable *table)
{
  if (!table) return;
  table->child = NULL;
}

void gncTaxTableCommitEdit (GncTaxTable *table)
{
  if (!table) return;

  if (table->dirty)
    gncBusinessSetDirtyFlag (table->book, _GNC_MOD_NAME, TRUE);
  table->dirty = FALSE;
}


/* Get Functions */
GncTaxTable * gncTaxTableLookup (GNCBook *book, const GUID *guid)
{
  if (!book || !guid) return NULL;
  return xaccLookupEntity (gnc_book_get_entity_table (book),
			   guid, _GNC_MOD_NAME);
}

GncTaxTable *gncTaxTableLookupByName (GNCBook *book, const char *name)
{
  GList *list = gncTaxTableGetTables (book);

  for ( ; list; list = list->next) {
    GncTaxTable *table = list->data;
    if (!safe_strcmp (table->name, name))
      return list->data;
  }
  return NULL;
}

GList * gncTaxTableGetTables (GNCBook *book)
{
  struct _book_info *bi;
  if (!book) return NULL;

  bi = gnc_book_get_data (book, _GNC_MOD_NAME);
  return bi->tables;
}


const GUID *gncTaxTableGetGUID (GncTaxTable *table)
{
  if (!table) return NULL;
  return &table->guid;
}

GNCBook *gncTaxTableGetBook (GncTaxTable *table)
{
  if (!table) return NULL;
  return table->book;
}

const char *gncTaxTableGetName (GncTaxTable *table)
{
  if (!table) return NULL;
  return table->name;
}

static GncTaxTableEntry *gncTaxTableEntryCopy (GncTaxTableEntry *entry)
{
  GncTaxTableEntry *e;
  if (!entry) return NULL;

  e = gncTaxTableEntryCreate ();
  gncTaxTableEntrySetAccount (e, entry->account);
  gncTaxTableEntrySetType (e, entry->type);
  gncTaxTableEntrySetAmount (e, entry->amount);

  return e;
}

static GncTaxTable *gncTaxTableCopy (GncTaxTable *table)
{
  GncTaxTable *t;
  GList *list;

  if (!table) return NULL;
  t = gncTaxTableCreate (table->book);
  gncTaxTableSetName (t, table->name);
  for (list = table->entries; list; list=list->next) {
    GncTaxTableEntry *entry, *e;
    entry = list->data;
    e = gncTaxTableEntryCopy (entry);
    gncTaxTableAddEntry (t, e);
  }
  return t;
}

GncTaxTable *gncTaxTableReturnChild (GncTaxTable *table, gboolean make_new)
{
  GncTaxTable *child = NULL;

  if (!table) return NULL;
  if (table->child) return table->child;
  if (make_new) {
    child = gncTaxTableCopy (table);
    gncTaxTableSetChild (table, child);
    gncTaxTableSetParent (child, table);
  }
  return child;
}

GncTaxTable *gncTaxTableGetParent (GncTaxTable *table)
{
  if (!table) return NULL;
  return table->parent;
}

GList *gncTaxTableGetEntries (GncTaxTable *table)
{
  if (!table) return NULL;
  return table->entries;
}

gint64 gncTaxTableGetRefcount (GncTaxTable *table)
{
  if (!table) return 0;
  return table->refcount;
}

Timespec gncTaxTableLastModified (GncTaxTable *table)
{
  Timespec ts = { 0 , 0 };
  if (!table) return ts;
  return table->modtime;
}

gboolean gncTaxTableGetInvisible (GncTaxTable *table)
{
  if (!table) return FALSE;
  return table->invisible;
}

Account * gncTaxTableEntryGetAccount (GncTaxTableEntry *entry)
{
  if (!entry) return NULL;
  return entry->account;
}

GncAmountType gncTaxTableEntryGetType (GncTaxTableEntry *entry)
{
  if (!entry) return 0;
  return entry->type;
}

gnc_numeric gncTaxTableEntryGetAmount (GncTaxTableEntry *entry)
{
  if (!entry) return gnc_numeric_zero();
  return entry->amount;
}

int gncTaxTableEntryCompare (GncTaxTableEntry *a, GncTaxTableEntry *b)
{
  char *name_a, *name_b;
  int retval;

  if (!a && !b) return 0;
  if (!a) return -1;
  if (!b) return 1;
    
  name_a = xaccAccountGetFullName (a->account, ':');
  name_b = xaccAccountGetFullName (b->account, ':');
  /* for comparison purposes it doesn't matter what we use as a separator */
  retval = safe_strcmp(name_a, name_b);
  g_free(name_a);
  g_free(name_b);

  if (retval)
    return retval;

  return gnc_numeric_compare (a->amount, b->amount);
}

int gncTaxTableCompare (GncTaxTable *a, GncTaxTable *b)
{
  if (!a && !b) return 0;
  if (!a) return -1;
  if (!b) return 1;
  return safe_strcmp (a->name, b->name);
}


/*
 * This will add value to the account-value for acc, creating a new
 * list object if necessary
 */
GList *gncAccountValueAdd (GList *list, Account *acc, gnc_numeric value)
{
  GList *li;
  GncAccountValue *res = NULL;

  g_return_val_if_fail (acc, list);
  g_return_val_if_fail (gnc_numeric_check (value) == GNC_ERROR_OK, list);

  /* Try to find the account in the list */
  for (li = list; li; li = li->next) {
    res = li->data;
    if (res->account == acc) {
      res->value = gnc_numeric_add (res->value, value, GNC_DENOM_AUTO,
				    GNC_DENOM_LCD);
      return list;
    }
  }
  /* Nope, didn't find it. */

  res = g_new0 (GncAccountValue, 1);
  res->account = acc;
  res->value = value;
  return g_list_prepend (list, res);
}

/* Merge l2 into l1.  l2 is not touched. */
GList *gncAccountValueAddList (GList *l1, GList *l2)
{
  GList *li;

  for (li = l2; li; li = li->next ) {
    GncAccountValue *val = li->data;
    l1 = gncAccountValueAdd (l1, val->account, val->value);
  }

  return l1;
}

/* return the total for this list */
gnc_numeric gncAccountValueTotal (GList *list)
{
  gnc_numeric total = gnc_numeric_zero ();

  for ( ; list ; list = list->next) {
    GncAccountValue *val = list->data;
    total = gnc_numeric_add (total, val->value, GNC_DENOM_AUTO, GNC_DENOM_LCD);
  }
  return total;
}

/* Destroy a list of accountvalues */
void gncAccountValueDestroy (GList *list)
{
  GList *node;
  for ( node = list; node ; node = node->next)
    g_free (node->data);

  g_list_free (list);
}


/* Package-Private functions */

static void add_or_rem_object (GncTaxTable *table, gboolean add)
{
  struct _book_info *bi;

  if (!table) return;
  bi = gnc_book_get_data (table->book, _GNC_MOD_NAME);

  if (add)
    bi->tables = g_list_insert_sorted (bi->tables, table,
				       (GCompareFunc)gncTaxTableCompare);
  else
    bi->tables = g_list_remove (bi->tables, table);
}

static void addObj (GncTaxTable *table)
{
  gncBusinessAddObject (table->book, _GNC_MOD_NAME, table, &table->guid);
  add_or_rem_object (table, TRUE);
}

static void remObj (GncTaxTable *table)
{
  gncBusinessRemoveObject (table->book, _GNC_MOD_NAME, &table->guid);
  add_or_rem_object (table, FALSE);
}

static void _gncTaxTableCreate (GNCBook *book)
{
  struct _book_info *bi;

  if (!book) return;

  bi = g_new0 (struct _book_info, 1);
  bi->bi.ht = guid_hash_table_new ();
  gnc_book_set_data (book, _GNC_MOD_NAME, bi);
}

static void _gncTaxTableDestroy (GNCBook *book)
{
  struct _book_info *bi;

  if (!book) return;

  bi = gnc_book_get_data (book, _GNC_MOD_NAME);

  /* XXX : Destroy the objects? */
  g_hash_table_destroy (bi->bi.ht);
  g_list_free (bi->tables);
  g_free (bi);
}

static gboolean _gncTaxTableIsDirty (GNCBook *book)
{
  return gncBusinessIsDirty (book, _GNC_MOD_NAME);
}

static void _gncTaxTableMarkClean (GNCBook *book)
{
  gncBusinessSetDirtyFlag (book, _GNC_MOD_NAME, FALSE);
}

static void _gncTaxTableForeach (GNCBook *book, foreachObjectCB cb,
			      gpointer user_data)
{
  gncBusinessForeach (book, _GNC_MOD_NAME, cb, user_data);
}

static GncObject_t gncTaxTableDesc = {
  GNC_OBJECT_VERSION,
  _GNC_MOD_NAME,
  "Tax Table",
  _gncTaxTableCreate,
  _gncTaxTableDestroy,
  _gncTaxTableIsDirty,
  _gncTaxTableMarkClean,
  _gncTaxTableForeach,
  NULL				/* printable */
};

gboolean gncTaxTableRegister (void)
{
  static QueryObjectDef params[] = {
    { QUERY_PARAM_BOOK, GNC_ID_BOOK, (QueryAccess)gncTaxTableGetBook },
    { QUERY_PARAM_GUID, QUERYCORE_GUID, (QueryAccess)gncTaxTableGetGUID },
    { NULL },
  };

  gncQueryObjectRegister (_GNC_MOD_NAME, (QuerySort)gncTaxTableCompare, params);

  return gncObjectRegister (&gncTaxTableDesc);
}
