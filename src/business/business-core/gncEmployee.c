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
#include "gnc-event-p.h"

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

G_INLINE_FUNC void mark_employee (GncEmployee *employee);
G_INLINE_FUNC void
mark_employee (GncEmployee *employee)
{
  employee->dirty = TRUE;

  gnc_engine_generate_event (&employee->guid, GNC_EVENT_MODIFY);
}

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
  employee->addr = gncAddressCreate (book, &employee->guid);
  employee->workday = gnc_numeric_zero();
  employee->rate = gnc_numeric_zero();
  employee->active = TRUE;
  
  xaccGUIDNew (&employee->guid, book);
  addObj (employee);

  gnc_engine_generate_event (&employee->guid, GNC_EVENT_CREATE);

  return employee;
}

void gncEmployeeDestroy (GncEmployee *employee)
{
  if (!employee) return;

  gnc_engine_generate_event (&employee->guid, GNC_EVENT_DESTROY);

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
  mark_employee (employee);
}

void gncEmployeeSetUsername (GncEmployee *employee, const char *username)
{
  if (!employee) return;
  if (!username) return;
  SET_STR(employee->username, username);
  mark_employee (employee);
}

void gncEmployeeSetLanguage (GncEmployee *employee, const char *language)
{
  if (!employee) return;
  if (!language) return;
  SET_STR(employee->language, language);
  mark_employee (employee);
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
  mark_employee (employee);
}

void gncEmployeeSetWorkday (GncEmployee *employee, gnc_numeric workday)
{
  if (!employee) return;
  if (gnc_numeric_equal (workday, employee->workday)) return;
  employee->workday = workday;
  mark_employee (employee);
}

void gncEmployeeSetRate (GncEmployee *employee, gnc_numeric rate)
{
  if (!employee) return;
  if (gnc_numeric_equal (rate, employee->rate)) return;
  employee->rate = rate;
  mark_employee (employee);
}

void gncEmployeeSetActive (GncEmployee *employee, gboolean active)
{
  if (!employee) return;
  if (active == employee->active) return;
  employee->active = active;
  mark_employee (employee);
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
  gncBusinessAddObject (employee->book, _GNC_MOD_NAME, employee,
			&employee->guid);
}

static void remObj (GncEmployee *employee)
{
  gncBusinessRemoveObject (employee->book, _GNC_MOD_NAME, &employee->guid);
}

static void _gncEmployeeCreate (GNCBook *book)
{
  gncBusinessCreate (book, _GNC_MOD_NAME);
}

static void _gncEmployeeDestroy (GNCBook *book)
{
  gncBusinessDestroy (book, _GNC_MOD_NAME);
}

static gboolean _gncEmployeeIsDirty (GNCBook *book)
{
  return gncBusinessIsDirty (book, _GNC_MOD_NAME);
}

static void _gncEmployeeForeach (GNCBook *book, foreachObjectCB cb,
				 gpointer user_data)
{
  gncBusinessForeach (book, _GNC_MOD_NAME, cb, user_data);
}

static const char * _gncEmployeePrintable (gpointer item)
{
  GncEmployee *v;

  if (!item) return NULL;

  v = item;
  return v->username;
}

static GncObject_t gncEmployeeDesc = {
  GNC_OBJECT_VERSION,
  _GNC_MOD_NAME,
  "Employee",
  _gncEmployeeCreate,
  _gncEmployeeDestroy,
  _gncEmployeeIsDirty,
  _gncEmployeeForeach,
  _gncEmployeePrintable
};

gboolean gncEmployeeRegister (void)
{
  static QueryObjectDef params[] = {
    { EMPLOYEE_ID, QUERYCORE_STRING, (QueryAccess)gncEmployeeGetID },
    { EMPLOYEE_USERNAME, QUERYCORE_STRING, (QueryAccess)gncEmployeeGetUsername },
    { EMPLOYEE_ADDR, GNC_ADDRESS_MODULE_NAME, (QueryAccess)gncEmployeeGetAddr },
    { QUERY_PARAM_BOOK, GNC_ID_BOOK, (QueryAccess)gncEmployeeGetBook },
    { QUERY_PARAM_GUID, QUERYCORE_GUID, (QueryAccess)gncEmployeeGetGUID },
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
