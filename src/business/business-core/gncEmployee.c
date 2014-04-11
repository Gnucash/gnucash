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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

/*
 * Copyright (C) 2001,2002 Derek Atkins
 * Copyright (C) 2003 Linas Vepstas <linas@linas.org>
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#include "config.h"

#include <glib.h>
#include <string.h>

#include "Account.h"
#include "gnc-commodity.h"
#include "gncAddressP.h"
#include "gncEmployee.h"
#include "gncEmployeeP.h"

struct _gncEmployee 
{
  QofInstance     inst;
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

struct _gncEmployeeClass
{
  QofInstanceClass parent_class;
};

static QofLogModule log_module = GNC_MOD_BUSINESS;

#define _GNC_MOD_NAME        GNC_ID_EMPLOYEE

G_INLINE_FUNC void mark_employee (GncEmployee *employee);
void mark_employee (GncEmployee *employee)
{
  qof_instance_set_dirty(&employee->inst);
  qof_event_gen (&employee->inst, QOF_EVENT_MODIFY, NULL);
}

/* ============================================================== */
/* GObject Initialization */
QOF_GOBJECT_IMPL(gnc_employee, GncEmployee, QOF_TYPE_INSTANCE);

static void
gnc_employee_init(GncEmployee* emp)
{
}

static void
gnc_employee_dispose_real (GObject *empp)
{
}

static void
gnc_employee_finalize_real(GObject* empp)
{
}

/* Create/Destroy Functions */
GncEmployee *gncEmployeeCreate (QofBook *book)
{
  GncEmployee *employee;

  if (!book) return NULL;

  employee = g_object_new (GNC_TYPE_EMPLOYEE, NULL);
  qof_instance_init_data (&employee->inst, _GNC_MOD_NAME, book);
  
  employee->id = CACHE_INSERT ("");
  employee->username = CACHE_INSERT ("");
  employee->language = CACHE_INSERT ("");
  employee->acl = CACHE_INSERT ("");
  employee->addr = gncAddressCreate (book, &employee->inst);
  employee->workday = gnc_numeric_zero();
  employee->rate = gnc_numeric_zero();
  employee->active = TRUE;
  
  qof_event_gen (&employee->inst, QOF_EVENT_CREATE, NULL);

  return employee;
}

void gncEmployeeDestroy (GncEmployee *employee)
{
  if (!employee) return;
  qof_instance_set_destroying(employee, TRUE);
  gncEmployeeCommitEdit(employee);
}

static void gncEmployeeFree (GncEmployee *employee)
{
  if (!employee) return;

  qof_event_gen (&employee->inst, QOF_EVENT_DESTROY, NULL);

  CACHE_REMOVE (employee->id);
  CACHE_REMOVE (employee->username);
  CACHE_REMOVE (employee->language);
  CACHE_REMOVE (employee->acl);
  gncAddressBeginEdit (employee->addr);
  gncAddressDestroy (employee->addr);

  /* qof_instance_release (&employee->inst); */
  g_object_unref (employee);
}

GncEmployee *
gncCloneEmployee (GncEmployee *from, QofBook *book)
{
  GncEmployee *employee;
  if (!book || !from) return NULL;

  employee = g_object_new (GNC_TYPE_EMPLOYEE, NULL);
  qof_instance_init_data(&employee->inst, _GNC_MOD_NAME, book);
  qof_instance_gemini (&employee->inst, &from->inst);

  employee->id = CACHE_INSERT (from->id);
  employee->username = CACHE_INSERT (from->username);
  employee->language = CACHE_INSERT (from->language);
  employee->acl = CACHE_INSERT (from->acl);
  employee->addr = gncCloneAddress (from->addr, &employee->inst, book);
  employee->workday = from->workday;
  employee->rate = from->rate;
  employee->active = from->active;
  employee->currency = gnc_commodity_obtain_twin(from->currency, book);
  employee->ccard_acc = 
     GNC_ACCOUNT(qof_instance_lookup_twin(QOF_INSTANCE(from->ccard_acc), book));
  
  qof_event_gen (&employee->inst, QOF_EVENT_CREATE, NULL);

  return employee;
}

GncEmployee *
gncEmployeeObtainTwin (GncEmployee *from, QofBook *book)
{
  GncEmployee *employee;
  if (!book) return NULL;

  employee = (GncEmployee *) qof_instance_lookup_twin (QOF_INSTANCE(from), book);
  if (!employee)
  {
    employee = gncCloneEmployee (from, book);
  }

  return employee;
}

/* ============================================================== */
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

void
qofEmployeeSetAddr (GncEmployee *employee, QofInstance *addr_ent)
{
	GncAddress *addr;

	if(!employee || !addr_ent) { return; }
	addr = (GncAddress*)addr_ent;
	if(addr == employee->addr) { return; }
	if(employee->addr != NULL) {
		gncAddressBeginEdit(employee->addr);
		gncAddressDestroy(employee->addr);
	}
	gncEmployeeBeginEdit(employee);
	employee->addr = addr;
	gncEmployeeCommitEdit(employee);
}

/* ============================================================== */
/* Get Functions */
const char * gncEmployeeGetID (const GncEmployee *employee)
{
  if (!employee) return NULL;
  return employee->id;
}

const char * gncEmployeeGetUsername (const GncEmployee *employee)
{
  if (!employee) return NULL;
  return employee->username;
}

GncAddress * gncEmployeeGetAddr (const GncEmployee *employee)
{
  if (!employee) return NULL;
  return employee->addr;
}

const char * gncEmployeeGetLanguage (const GncEmployee *employee)
{
  if (!employee) return NULL;
  return employee->language;
}

const char * gncEmployeeGetAcl (const GncEmployee *employee)
{
  if (!employee) return NULL;
  return employee->acl;
}

gnc_numeric gncEmployeeGetWorkday (const GncEmployee *employee)
{
  if (!employee) return gnc_numeric_zero();
  return employee->workday;
}

gnc_numeric gncEmployeeGetRate (const GncEmployee *employee)
{
  if (!employee) return gnc_numeric_zero();
  return employee->rate;
}

gnc_commodity * gncEmployeeGetCurrency (const GncEmployee *employee)
{
  if (!employee) return NULL;
  return employee->currency;
}

gboolean gncEmployeeGetActive (const GncEmployee *employee)
{
  if (!employee) return FALSE;
  return employee->active;
}

Account * gncEmployeeGetCCard (const GncEmployee *employee)
{
  if (!employee) return NULL;
  return employee->ccard_acc;
}

gboolean gncEmployeeIsDirty (const GncEmployee *employee)
{
  if (!employee) return FALSE;
  return (qof_instance_get_dirty_flag(employee)
          || gncAddressIsDirty (employee->addr));
}

void gncEmployeeBeginEdit (GncEmployee *employee)
{
  qof_begin_edit(&employee->inst);
}

static void gncEmployeeOnError (QofInstance *employee, QofBackendError errcode)
{
  PERR("Employee QofBackend Failure: %d", errcode);
}

static void gncEmployeeOnDone (QofInstance *inst)
{
  GncEmployee *employee = (GncEmployee *) inst;
  gncAddressClearDirty (employee->addr);
}

static void emp_free (QofInstance *inst)
{
  GncEmployee *employee = (GncEmployee *) inst;
  gncEmployeeFree (employee);
}


void gncEmployeeCommitEdit (GncEmployee *employee)
{
  if (!qof_commit_edit (QOF_INSTANCE(employee))) return;
  qof_commit_edit_part2 (&employee->inst, gncEmployeeOnError,
                         gncEmployeeOnDone, emp_free);
}

/* ============================================================== */
/* Other functions */

int gncEmployeeCompare (const GncEmployee *a, const GncEmployee *b)
{
  if (!a && !b) return 0;
  if (!a && b) return 1;
  if (a && !b) return -1;

  return(strcmp(a->username, b->username));
}

/* Package-Private functions */

static const char * _gncEmployeePrintable (gpointer item)
{
  GncEmployee *v = item;
  if (!item) return NULL;
  return gncAddressGetName(v->addr);
}

static QofObject gncEmployeeDesc = 
{
  interface_version:  QOF_OBJECT_VERSION,
  e_type:             _GNC_MOD_NAME,
  type_label:         "Employee",
  create:             (gpointer)gncEmployeeCreate,
  book_begin:         NULL,
  book_end:           NULL,
  is_dirty:           qof_collection_is_dirty,
  mark_clean:         qof_collection_mark_clean,
  foreach:            qof_collection_foreach,
  printable:          _gncEmployeePrintable,
  version_cmp:        (int (*)(gpointer, gpointer)) qof_instance_version_cmp,
};

gboolean gncEmployeeRegister (void)
{
  static QofParam params[] = {
    { EMPLOYEE_ID, QOF_TYPE_STRING, (QofAccessFunc)gncEmployeeGetID, (QofSetterFunc)gncEmployeeSetID },
    { EMPLOYEE_USERNAME, QOF_TYPE_STRING, (QofAccessFunc)gncEmployeeGetUsername, 
		(QofSetterFunc)gncEmployeeSetUsername },
	{ EMPLOYEE_LANGUAGE, QOF_TYPE_STRING, (QofAccessFunc)gncEmployeeGetLanguage, 
		(QofSetterFunc)gncEmployeeSetLanguage },
	{ EMPLOYEE_ACL, QOF_TYPE_STRING, (QofAccessFunc)gncEmployeeGetAcl, (QofSetterFunc)gncEmployeeSetAcl },
	{ EMPLOYEE_WORKDAY, QOF_TYPE_NUMERIC, (QofAccessFunc)gncEmployeeGetWorkday,
		(QofSetterFunc)gncEmployeeSetWorkday },
	{ EMPLOYEE_RATE, QOF_TYPE_NUMERIC, (QofAccessFunc)gncEmployeeGetRate, (QofSetterFunc)gncEmployeeSetRate },
    { EMPLOYEE_ADDR, GNC_ID_ADDRESS, (QofAccessFunc)gncEmployeeGetAddr, (QofSetterFunc)qofEmployeeSetAddr },
    { EMPLOYEE_CC,  GNC_ID_ACCOUNT, (QofAccessFunc)gncEmployeeGetCCard, (QofSetterFunc)gncEmployeeSetCCard },
    { QOF_PARAM_ACTIVE, QOF_TYPE_BOOLEAN, (QofAccessFunc)gncEmployeeGetActive, (QofSetterFunc)gncEmployeeSetActive },
    { QOF_PARAM_BOOK, QOF_ID_BOOK, (QofAccessFunc)qof_instance_get_book, NULL },
    { QOF_PARAM_GUID, QOF_TYPE_GUID, (QofAccessFunc)qof_instance_get_guid, NULL },
    { NULL },
  };

  qof_class_register (_GNC_MOD_NAME, (QofSortFunc)gncEmployeeCompare,params);

  return qof_object_register (&gncEmployeeDesc);
}

gint64 gncEmployeeNextID (QofBook *book)
{
  return qof_book_get_counter (book, _GNC_MOD_NAME);
}
