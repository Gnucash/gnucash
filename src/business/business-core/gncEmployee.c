/********************************************************************\
 * gncEmployee.c -- the Core Employee Interface                     *
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
 * Copyright (C) 2001,2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#include "config.h"

#include <glib.h>
#include <string.h>

#include "guid.h"
#include "messages.h"
#include "gnc-commodity.h"
#include "gnc-engine-util.h"
#include "gnc-event-p.h"
#include "gnc-be-utils.h"

#include "qofbook.h"
#include "qofclass.h"
#include "qofid.h"
#include "qofid-p.h"
#include "qofinstance.h"
#include "qofinstance-p.h"
#include "qofobject.h"
#include "qofquery.h"
#include "qofquerycore.h"

#include "gncBusiness.h"
#include "gncEmployee.h"
#include "gncEmployeeP.h"
#include "gncAddress.h"

struct _gncEmployee 
{
  QofInstance inst;
  char *          id;
  char *          username;
  GncAddress *    addr;
  gnc_commodity * currency;
  gboolean        active;
  
  char *          language;
  char *          acl;
  gnc_numeric     workday;
  gnc_numeric     rate;

  Account *        ccard_acc;
};

static short        module = MOD_BUSINESS;

#define _GNC_MOD_NAME        GNC_EMPLOYEE_MODULE_NAME

#define CACHE_INSERT(str) g_cache_insert(gnc_engine_get_string_cache(), (gpointer)(str));
#define CACHE_REMOVE(str) g_cache_remove(gnc_engine_get_string_cache(), (str));

G_INLINE_FUNC void mark_employee (GncEmployee *employee);
G_INLINE_FUNC void
mark_employee (GncEmployee *employee)
{
  employee->inst.dirty = TRUE;
  gncBusinessSetDirtyFlag (employee->inst.book, _GNC_MOD_NAME, TRUE);

  gnc_engine_gen_event (&employee->inst.entity, GNC_EVENT_MODIFY);
}

/* Create/Destroy Functions */

GncEmployee *gncEmployeeCreate (QofBook *book)
{
  GncEmployee *employee;

  if (!book) return NULL;

  employee = g_new0 (GncEmployee, 1);
  qof_instance_init (&employee->inst, _GNC_MOD_NAME, book);
  
  employee->id = CACHE_INSERT ("");
  employee->username = CACHE_INSERT ("");
  employee->language = CACHE_INSERT ("");
  employee->acl = CACHE_INSERT ("");
  employee->addr = gncAddressCreate (book, &employee->inst.entity.guid, _GNC_MOD_NAME);
  employee->workday = gnc_numeric_zero();
  employee->rate = gnc_numeric_zero();
  employee->active = TRUE;
  
  gnc_engine_gen_event (&employee->inst.entity, GNC_EVENT_CREATE);

  return employee;
}

void gncEmployeeDestroy (GncEmployee *employee)
{
  if (!employee) return;
  employee->inst.do_free = TRUE;
  gncEmployeeCommitEdit(employee);
}

static void gncEmployeeFree (GncEmployee *employee)
{
  if (!employee) return;

  gnc_engine_gen_event (&employee->inst.entity, GNC_EVENT_DESTROY);

  CACHE_REMOVE (employee->id);
  CACHE_REMOVE (employee->username);
  CACHE_REMOVE (employee->language);
  CACHE_REMOVE (employee->acl);
  gncAddressDestroy (employee->addr);

  qof_instance_release (&employee->inst);
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

GncEmployee * gncEmployeeLookup (QofBook *book, const GUID *guid)
{
  ELOOKUP(GncEmployee);
}

gboolean gncEmployeeIsDirty (GncEmployee *employee)
{
  if (!employee) return FALSE;
  return (employee->inst.dirty || gncAddressIsDirty (employee->addr));
}

void gncEmployeeBeginEdit (GncEmployee *employee)
{
  GNC_BEGIN_EDIT (&employee->inst);
}

static inline void gncEmployeeOnError (QofInstance *employee, QofBackendError errcode)
{
  PERR("Employee QofBackend Failure: %d", errcode);
}

static inline void gncEmployeeOnDone (QofInstance *inst)
{
  GncEmployee *employee = (GncEmployee *) inst;
  gncAddressClearDirty (employee->addr);
}

static inline void emp_free (QofInstance *inst)
{
  GncEmployee *employee = (GncEmployee *) inst;
  gncEmployeeFree (employee);
}


void gncEmployeeCommitEdit (GncEmployee *employee)
{
  GNC_COMMIT_EDIT_PART1 (&employee->inst);
  GNC_COMMIT_EDIT_PART2 (&employee->inst, gncEmployeeOnError,
                         gncEmployeeOnDone, emp_free);
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
static void _gncEmployeeCreate (QofBook *book)
{
  gncBusinessCreate (book, _GNC_MOD_NAME);
}

static void _gncEmployeeDestroy (QofBook *book)
{
  gncBusinessDestroy (book, _GNC_MOD_NAME);
}

static gboolean _gncEmployeeIsDirty (QofBook *book)
{
  return gncBusinessIsDirty (book, _GNC_MOD_NAME);
}

static void _gncEmployeeMarkClean (QofBook *book)
{
  gncBusinessSetDirtyFlag (book, _GNC_MOD_NAME, FALSE);
}

static void _gncEmployeeForeach (QofBook *book, QofForeachCB cb,
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

static QofObject gncEmployeeDesc = {
  QOF_OBJECT_VERSION,
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
  static QofParam params[] = {
    { EMPLOYEE_ID, QOF_TYPE_STRING, (QofAccessFunc)gncEmployeeGetID, NULL },
    { EMPLOYEE_USERNAME, QOF_TYPE_STRING, (QofAccessFunc)gncEmployeeGetUsername, NULL },
    { EMPLOYEE_ADDR, GNC_ADDRESS_MODULE_NAME, (QofAccessFunc)gncEmployeeGetAddr, NULL },
    { QOF_QUERY_PARAM_ACTIVE, QOF_TYPE_BOOLEAN, (QofAccessFunc)gncEmployeeGetActive, NULL },
    { QOF_QUERY_PARAM_BOOK, QOF_ID_BOOK, (QofAccessFunc)qof_instance_get_book, NULL },
    { QOF_QUERY_PARAM_GUID, QOF_TYPE_GUID, (QofAccessFunc)qof_instance_get_guid, NULL },
    { NULL },
  };

  qof_class_register (_GNC_MOD_NAME, (QofSortFunc)gncEmployeeCompare,params);

  return qof_object_register (&gncEmployeeDesc);
}

gint64 gncEmployeeNextID (QofBook *book)
{
  return qof_book_get_counter (book, _GNC_MOD_NAME);
}
