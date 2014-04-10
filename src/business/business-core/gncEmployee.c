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
#include "gnc-book.h"
#include "gnc-commodity.h"
#include "gnc-engine-util.h"
#include "GNCIdP.h"
#include "gncObject.h"
#include "QueryCore.h"
#include "QueryNew.h"
#include "QueryObject.h"
#include "gnc-event-p.h"
#include "gnc-be-utils.h"

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
  gnc_commodity * currency;
  gnc_numeric	workday;
  gnc_numeric	rate;
  gboolean	active;
  gboolean	dirty;

  Account *	ccard_acc;

  int		editlevel;
  gboolean	do_free;
};

static short	module = MOD_BUSINESS;

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
  gncBusinessSetDirtyFlag (employee->book, _GNC_MOD_NAME, TRUE);

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
  employee->do_free = TRUE;
  gncEmployeeCommitEdit(employee);
}

static void gncEmployeeFree (GncEmployee *employee)
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

#define SET_STR(obj, member, str) { \
	char * tmp; \
	\
	if (!safe_strcmp (member, str)) return; \
	gncEmployeeBeginEdit (obj); \
	tmp = CACHE_INSERT (str); \
	CACHE_REMOVE (member); \
	member = tmp; \
	}

void gncEmployeeSetID (GncEmployee *employee, const char *id)
{
  if (!employee) return;
  if (!id) return;
  SET_STR(employee, employee->id, id);
  mark_employee (employee);
  gncEmployeeCommitEdit (employee);
}

void gncEmployeeSetUsername (GncEmployee *employee, const char *username)
{
  if (!employee) return;
  if (!username) return;
  SET_STR(employee, employee->username, username);
  mark_employee (employee);
  gncEmployeeCommitEdit (employee);
}

void gncEmployeeSetLanguage (GncEmployee *employee, const char *language)
{
  if (!employee) return;
  if (!language) return;
  SET_STR(employee, employee->language, language);
  mark_employee (employee);
  gncEmployeeCommitEdit (employee);
}

void gncEmployeeSetGUID (GncEmployee *employee, const GUID *guid)
{
  if (!employee || !guid) return;
  if (guid_equal (guid, &employee->guid)) return;
  gncEmployeeBeginEdit (employee);
  remObj (employee);
  employee->guid = *guid;
  addObj (employee);
  gncEmployeeCommitEdit (employee);
}

void gncEmployeeSetAcl (GncEmployee *employee, const char *acl)
{
  if (!employee) return;
  if (!acl) return;
  SET_STR(employee, employee->acl, acl);
  mark_employee (employee);
  gncEmployeeCommitEdit (employee);
}

void gncEmployeeSetWorkday (GncEmployee *employee, gnc_numeric workday)
{
  if (!employee) return;
  if (gnc_numeric_equal (workday, employee->workday)) return;
  gncEmployeeBeginEdit (employee);
  employee->workday = workday;
  mark_employee (employee);
  gncEmployeeCommitEdit (employee);
}

void gncEmployeeSetRate (GncEmployee *employee, gnc_numeric rate)
{
  if (!employee) return;
  if (gnc_numeric_equal (rate, employee->rate)) return;
  gncEmployeeBeginEdit (employee);
  employee->rate = rate;
  mark_employee (employee);
  gncEmployeeCommitEdit (employee);
}

void gncEmployeeSetCurrency (GncEmployee *employee, gnc_commodity *currency)
{
  if (!employee || !currency) return;
  if (employee->currency && 
      gnc_commodity_equal (employee->currency, currency))
    return;
  gncEmployeeBeginEdit (employee);
  employee->currency = currency;
  mark_employee (employee);
  gncEmployeeCommitEdit (employee);
}

void gncEmployeeSetActive (GncEmployee *employee, gboolean active)
{
  if (!employee) return;
  if (active == employee->active) return;
  gncEmployeeBeginEdit (employee);
  employee->active = active;
  mark_employee (employee);
  gncEmployeeCommitEdit (employee);
}

void gncEmployeeSetCCard (GncEmployee *employee, Account* ccard_acc)
{
  if (!employee) return;
  if (ccard_acc == employee->ccard_acc) return;
  gncEmployeeBeginEdit (employee);
  employee->ccard_acc = ccard_acc;
  mark_employee (employee);
  gncEmployeeCommitEdit (employee);
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

gnc_commodity * gncEmployeeGetCurrency (GncEmployee *employee)
{
  if (!employee) return NULL;
  return employee->currency;
}

gboolean gncEmployeeGetActive (GncEmployee *employee)
{
  if (!employee) return FALSE;
  return employee->active;
}

Account * gncEmployeeGetCCard (GncEmployee *employee)
{
  if (!employee) return NULL;
  return employee->ccard_acc;
}

GncEmployee * gncEmployeeLookup (GNCBook *book, const GUID *guid)
{
  if (!book || !guid) return NULL;
  return xaccLookupEntity (gnc_book_get_entity_table (book),
			   guid, _GNC_MOD_NAME);
}

GUID gncEmployeeRetGUID (GncEmployee *employee)
{
  if (!employee)
    return *xaccGUIDNULL();

  return employee->guid;
}

GncEmployee * gncEmployeeLookupDirect (GUID guid, GNCBook *book)
{
  if (!book) return NULL;
  return gncEmployeeLookup (book, &guid);
}

gboolean gncEmployeeIsDirty (GncEmployee *employee)
{
  if (!employee) return FALSE;
  return (employee->dirty || gncAddressIsDirty (employee->addr));
}

void gncEmployeeBeginEdit (GncEmployee *employee)
{
  GNC_BEGIN_EDIT (employee, _GNC_MOD_NAME);
}

static void gncEmployeeOnError (GncEmployee *employee, GNCBackendError errcode)
{
  PERR("Employee Backend Failure: %d", errcode);
}

static void gncEmployeeOnDone (GncEmployee *employee)
{
  employee->dirty = FALSE;
  gncAddressClearDirty (employee->addr);
}

void gncEmployeeCommitEdit (GncEmployee *employee)
{
  GNC_COMMIT_EDIT_PART1 (employee);
  GNC_COMMIT_EDIT_PART2 (employee, _GNC_MOD_NAME, gncEmployeeOnError,
			 gncEmployeeOnDone, gncEmployeeFree);
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

static void _gncEmployeeMarkClean (GNCBook *book)
{
  gncBusinessSetDirtyFlag (book, _GNC_MOD_NAME, FALSE);
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
  return gncAddressGetName(v->addr);
}

static GncObject_t gncEmployeeDesc = {
  GNC_OBJECT_VERSION,
  _GNC_MOD_NAME,
  "Employee",
  _gncEmployeeCreate,
  _gncEmployeeDestroy,
  _gncEmployeeIsDirty,
  _gncEmployeeMarkClean,
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
    { QUERY_PARAM_ACTIVE, QUERYCORE_BOOLEAN, (QueryAccess)gncEmployeeGetActive },
    { NULL },
  };

  gncQueryObjectRegister (_GNC_MOD_NAME, (QuerySort)gncEmployeeCompare,params);

  return gncObjectRegister (&gncEmployeeDesc);
}

gint64 gncEmployeeNextID (GNCBook *book)
{
  return gnc_book_get_counter (book, _GNC_MOD_NAME);
}
