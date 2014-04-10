/*
 * gncEmployee.c -- the Core Employee Interface
 * Copyright (C) 2001 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#include "config.h"

#include <glib.h>
#include <string.h>

#include "guid.h"
#include "messages.h"
#include "gnc-engine-util.h"

#include "gncEmployee.h"
#include "gncEmployeeP.h"
#include "gncAddress.h"
#include "gncBusiness.h"

struct _gncEmployee {
  GncBusiness *	business;
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

#define CACHE_INSERT(str) g_cache_insert(gnc_engine_get_string_cache(), (gpointer)(str));
#define CACHE_REMOVE(str) g_cache_remove(gnc_engine_get_string_cache(), (str));

/* Create/Destroy Functions */

GncEmployee *gncEmployeeCreate (GncBusiness *business)
{
  GncEmployee *employee;

  if (!business) return NULL;

  employee = g_new0 (GncEmployee, 1);
  employee->business = business;
  employee->dirty = FALSE;

  employee->id = CACHE_INSERT ("");
  employee->username = CACHE_INSERT ("");
  employee->language = CACHE_INSERT ("");
  employee->acl = CACHE_INSERT ("");
  employee->addr = gncAddressCreate (business);
  employee->workday = gnc_numeric_zero();
  employee->rate = gnc_numeric_zero();
  employee->active = TRUE;
  
  guid_new (&employee->guid);

  gncBusinessAddEntity (business, GNC_EMPLOYEE_MODULE_NAME, &employee->guid,
			employee);

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

  gncBusinessRemoveEntity (employee->business, GNC_EMPLOYEE_MODULE_NAME,
			   &employee->guid);

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
  gncBusinessRemoveEntity (employee->business, GNC_EMPLOYEE_MODULE_NAME,
			   &employee->guid);
  employee->guid = *guid;
  gncBusinessAddEntity (employee->business, GNC_EMPLOYEE_MODULE_NAME,
			&employee->guid, employee);
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

GncBusiness * gncEmployeeGetBusiness (GncEmployee *employee)
{
  if (!employee) return NULL;
  return employee->business;
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

static gint gncEmployeeSortFunc (gconstpointer a, gconstpointer b) {
  GncEmployee *ea = (GncEmployee *) a;
  GncEmployee *eb = (GncEmployee *) b;
  return(strcmp(ea->username, eb->username));
}

/* Package-Private functions */

struct _iterate {
  GList *list;
  gboolean show_all;
};

static void get_list (gpointer key, gpointer item, gpointer arg)
{
  struct _iterate *iter = arg;
  GncEmployee *employee = item;

  if (iter->show_all || gncEmployeeGetActive (employee)) {
    iter->list = g_list_insert_sorted (iter->list, employee, gncEmployeeSortFunc);
  }
}

static GList * _gncEmployeeGetList (GncBusiness *bus, gboolean show_all)
{
  GHashTable *ht;
  struct _iterate iter;

  if (!bus) return NULL;

  iter.list = NULL;
  iter.show_all = show_all;

  ht = gncBusinessEntityTable (bus, GNC_EMPLOYEE_MODULE_NAME);
  if (ht)
    g_hash_table_foreach (ht, get_list, &iter);

  return iter.list;
}

static const char * _gncEmployeePrintable (gpointer item)
{
  GncEmployee *v;

  if (!item) return NULL;

  v = item;
  return v->username;
}

static void _gncEmployeeDestroy (GncBusiness *bus)
{
  if (!bus) return;

  /* XXX: should we be sure to destroy all the employee objects? */
}

static GncBusinessObject gncEmployeeDesc = {
  GNC_BUSINESS_VERSION,
  GNC_EMPLOYEE_MODULE_NAME,
  "Employee",
  _gncEmployeeDestroy,
  _gncEmployeeGetList,
  _gncEmployeePrintable
};

gboolean gncEmployeeRegister (void)
{
  return gncBusinessRegister (&gncEmployeeDesc);
}

static gint lastEmployee = 2;

gint gncEmployeeNextID (GncBusiness *business)
{
  return ++lastEmployee;		/* XXX: Look into Database! */
}
