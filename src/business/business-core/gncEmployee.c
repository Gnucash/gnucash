/*
 * gncEmployee.c -- the Core Employee Interface
 * Copyright (C) 2001,2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#include "config.h"

#include <glib.h>
#include <string.h>

#include "guid.h"
#include "messages.h"
#include "gnc-engine-util.h"
#include "gnc-book-p.h"
#include "GNCIdP.h"
#include "gncObject.h"
#include "QueryObject.h"

#include "gncBusiness.h"
#include "gncEmployee.h"
#include "gncEmployeeP.h"
#include "gncAddress.h"

struct _gncEmployee {
  GNCBook *	book;
  GUID		guid;
  char *	id;
  char *	username;
  char *	language;
  char *	acl;
  GncAddress *	addr;
  gnc_numeric	workday;
  gnc_numeric	rate;
  gboolean	active;
  gboolean	dirty;
};

#define _GNC_MOD_NAME	GNC_EMPLOYEE_MODULE_NAME

#define CACHE_INSERT(str) g_cache_insert(gnc_engine_get_string_cache(), (gpointer)(str));
#define CACHE_REMOVE(str) g_cache_remove(gnc_engine_get_string_cache(), (str));

static void addObj (GncEmployee *employee);
static void remObj (GncEmployee *employee);

/* Create/Destroy Functions */

GncEmployee *gncEmployeeCreate (GNCBook *book)
{
  GncEmployee *employee;

  if (!book) return NULL;

  employee = g_new0 (GncEmployee, 1);
  employee->book = book;
  employee->dirty = FALSE;

  employee->id = CACHE_INSERT ("");
  employee->username = CACHE_INSERT ("");
  employee->language = CACHE_INSERT ("");
  employee->acl = CACHE_INSERT ("");
  employee->addr = gncAddressCreate (book);
  employee->workday = gnc_numeric_zero();
  employee->rate = gnc_numeric_zero();
  employee->active = TRUE;
  
  xaccGUIDNew (&employee->guid, book);
  addObj (employee);

  return employee;
}

void gncEmployeeDestroy (GncEmployee *employee)
{
  if (!employee) return;

  CACHE_REMOVE (employee->id);
  CACHE_REMOVE (employee->username);
  CACHE_REMOVE (employee->language);
  CACHE_REMOVE (employee->acl);
  gncAddressDestroy (employee->addr);

  remObj (employee);
  g_free (employee);
}

/* Set Functions */

#define SET_STR(member, str) { \
	char * tmp; \
	\
	if (!strcmp (member, str)) return; \
	tmp = CACHE_INSERT (str); \
	CACHE_REMOVE (member); \
	member = tmp; \
	}

void gncEmployeeSetID (GncEmployee *employee, const char *id)
{
  if (!employee) return;
  if (!id) return;
  SET_STR(employee->id, id);
  employee->dirty = TRUE;
}

void gncEmployeeSetUsername (GncEmployee *employee, const char *username)
{
  if (!employee) return;
  if (!username) return;
  SET_STR(employee->username, username);
  employee->dirty = TRUE;
}

void gncEmployeeSetLanguage (GncEmployee *employee, const char *language)
{
  if (!employee) return;
  if (!language) return;
  SET_STR(employee->language, language);
  employee->dirty = TRUE;
}

void gncEmployeeSetGUID (GncEmployee *employee, const GUID *guid)
{
  if (!employee || !guid) return;
  if (guid_equal (guid, &employee->guid)) return;
  remObj (employee);
  employee->guid = *guid;
  addObj (employee);
}

void gncEmployeeSetAcl (GncEmployee *employee, const char *acl)
{
  if (!employee) return;
  if (!acl) return;
  SET_STR(employee->acl, acl);
  employee->dirty = TRUE;
}

void gncEmployeeSetWorkday (GncEmployee *employee, gnc_numeric workday)
{
  if (!employee) return;
  if (gnc_numeric_equal (workday, employee->workday)) return;
  employee->workday = workday;
  employee->dirty = TRUE;
}

void gncEmployeeSetRate (GncEmployee *employee, gnc_numeric rate)
{
  if (!employee) return;
  if (gnc_numeric_equal (rate, employee->rate)) return;
  employee->rate = rate;
  employee->dirty = TRUE;
}

void gncEmployeeSetActive (GncEmployee *employee, gboolean active)
{
  if (!employee) return;
  if (active == employee->active) return;
  employee->active = active;
  employee->dirty = TRUE;
}

/* Get Functions */

GNCBook * gncEmployeeGetBook (GncEmployee *employee)
{
  if (!employee) return NULL;
  return employee->book;
}

const GUID * gncEmployeeGetGUID (GncEmployee *employee)
{
  if (!employee) return NULL;
  return &employee->guid;
}

const char * gncEmployeeGetID (GncEmployee *employee)
{
  if (!employee) return NULL;
  return employee->id;
}

const char * gncEmployeeGetUsername (GncEmployee *employee)
{
  if (!employee) return NULL;
  return employee->username;
}

GncAddress * gncEmployeeGetAddr (GncEmployee *employee)
{
  if (!employee) return NULL;
  return employee->addr;
}

const char * gncEmployeeGetLanguage (GncEmployee *employee)
{
  if (!employee) return NULL;
  return employee->language;
}

const char * gncEmployeeGetAcl (GncEmployee *employee)
{
  if (!employee) return NULL;
  return employee->acl;
}

gnc_numeric gncEmployeeGetWorkday (GncEmployee *employee)
{
  if (!employee) return gnc_numeric_zero();
  return employee->workday;
}

gnc_numeric gncEmployeeGetRate (GncEmployee *employee)
{
  if (!employee) return gnc_numeric_zero();
  return employee->rate;
}

gboolean gncEmployeeGetActive (GncEmployee *employee)
{
  if (!employee) return FALSE;
  return employee->active;
}

GncEmployee * gncEmployeeLookup (GNCBook *book, const GUID *guid)
{
  if (!book || !guid) return NULL;
  return xaccLookupEntity (gnc_book_get_entity_table (book),
			   guid, _GNC_MOD_NAME);
}

gboolean gncEmployeeIsDirty (GncEmployee *employee)
{
  if (!employee) return FALSE;
  return (employee->dirty || gncAddressIsDirty (employee->addr));
}

void gncEmployeeCommitEdit (GncEmployee *employee)
{

  /* XXX COMMIT TO DATABASE */
  employee->dirty = FALSE;
}

/* Other functions */

int gncEmployeeCompare (GncEmployee *a, GncEmployee *b)
{
  if (!a && !b) return 0;
  if (!a && b) return 1;
  if (a && !b) return -1;

  return(strcmp(a->username, b->username));
}

/* Package-Private functions */

static void addObj (GncEmployee *employee)
{
  GHashTable *ht;

  xaccStoreEntity (gnc_book_get_entity_table (employee->book),
		   employee, &employee->guid, _GNC_MOD_NAME);

  ht = gnc_book_get_data (employee->book, _GNC_MOD_NAME);
  g_hash_table_insert (ht, &employee->guid, employee);
}

static void remObj (GncEmployee *employee)
{
  GHashTable *ht;

  xaccRemoveEntity (gnc_book_get_entity_table (employee->book),
		    &employee->guid);
  ht = gnc_book_get_data (employee->book, _GNC_MOD_NAME);
  g_hash_table_remove (ht, &employee->guid);
}

static void _gncEmployeeForeach (GNCBook *book, foreachObjectCB cb,
				 gpointer user_data)
{
  if (!book || !cb) return;
  gncBusinessForeach (book, _GNC_MOD_NAME, cb, user_data);
}

static const char * _gncEmployeePrintable (gpointer item)
{
  GncEmployee *v;

  if (!item) return NULL;

  v = item;
  return v->username;
}

static void _gncEmployeeCreate (GNCBook *book)
{
  GHashTable *ht;

  if (!book) return;

  ht = guid_hash_table_new ();
  gnc_book_set_data (book, _GNC_MOD_NAME, ht);
}

static void _gncEmployeeDestroy (GNCBook *book)
{
  GHashTable *ht;

  if (!book) return;

  ht = gnc_book_get_data (book, _GNC_MOD_NAME);

  /* XXX : Destroy the objects? */
  g_hash_table_destroy (ht);
}

static GncObject_t gncEmployeeDesc = {
  GNC_OBJECT_VERSION,
  _GNC_MOD_NAME,
  "Employee",
  _gncEmployeeCreate,
  _gncEmployeeDestroy,
  _gncEmployeeForeach,
  _gncEmployeePrintable
};

gboolean gncEmployeeRegister (void)
{
  static QueryObjectDef params[] = {
    { EMPLOYEE_GUID, QUERYCORE_GUID, (QueryAccess)gncEmployeeGetGUID },
    { EMPLOYEE_ID, QUERYCORE_STRING, (QueryAccess)gncEmployeeGetID },
    { EMPLOYEE_USERNAME, QUERYCORE_STRING, (QueryAccess)gncEmployeeGetUsername },
    { QUERY_PARAM_BOOK, GNC_ID_BOOK, (QueryAccess)gncEmployeeGetBook },
    { NULL },
  };

  gncQueryObjectRegister (_GNC_MOD_NAME, (QuerySort)gncEmployeeCompare,params);

  return gncObjectRegister (&gncEmployeeDesc);
}

static gint lastEmployee = 2;

gint gncEmployeeNextID (GNCBook *book)
{
  return ++lastEmployee;		/* XXX: Look into Database! */
}
