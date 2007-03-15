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

/* GObject declarations */

static void gnc_employee_class_init(GncEmployeeClass *klass);
static void gnc_employee_init(GncEmployee *sp);
static void gnc_employee_finalize(GObject *object);


struct _GncEmployeePrivate
{
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


typedef struct _GncEmployeeSignal GncEmployeeSignal;
typedef enum _GncEmployeeSignalType GncEmployeeSignalType;

enum _GncEmployeeSignalType {
	/* Signals */
	LAST_SIGNAL
};

/* properties */
enum
{
        PROP_0
};

struct _GncEmployeeSignal {
	GncEmployee *object;
};

static guint gnc_employee_signals[LAST_SIGNAL] = { 0 };
static GObjectClass *parent_class = NULL;

GType
gnc_employee_get_type()
{
	static GType type = 0;

	if(type == 0) {
		static const GTypeInfo our_info = {
			sizeof (GncEmployeeClass),
			NULL,
			NULL,
			(GClassInitFunc)gnc_employee_class_init,
			NULL,
			NULL,
			sizeof (GncEmployee),
			0,
			(GInstanceInitFunc)gnc_employee_init,
		};

		type = g_type_register_static(QOF_TYPE_INSTANCE, 
			"GncEmployee", &our_info, 0);
	}

	return type;
}

static void
gnc_employee_class_init(GncEmployeeClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS(klass);

	parent_class = g_type_class_peek_parent(klass);
	object_class->finalize = gnc_employee_finalize;
	object_class->set_property = gnc_employee_set_property;
    object_class->get_property = gnc_employee_get_property;

	/* Install properties */
	
	/* Create signals here:*/
 	
}

static void
gnc_employee_init(GncEmployee *obj)
{
	/* Initialize private members, etc. */
}

static void
gnc_employee_finalize(GObject *object)
{
	
	/* Free private members, etc. */
	
	G_OBJECT_CLASS(parent_class)->finalize(object);
}

static void
gnc_employee_set_property (GObject *object,
				  guint param_id,
				  const GValue *value,
				  GParamSpec *pspec)
{
	GncEmployee *obj;
	
	obj = GNC_EMPLOYEE (object);
	switch (param_id) {		
		default:
   			/* We don't have any other property... */
    		G_OBJECT_WARN_INVALID_PROPERTY_ID(object,property_id,pspec);
    	break;
	}
}

static void
gnc_employee_get_property (GObject      *object,
                        guint         property_id,
                        GValue       *value,
                        GParamSpec   *pspec)
{
  GncEmployee *obj;
  
  obj = GNC_EMPLOYEE (object);

  switch (property_id) {
  default:
    /* We don't have any other property... */
    G_OBJECT_WARN_INVALID_PROPERTY_ID(object,property_id,pspec);
    break;
  }
}

/************************************************/

static QofLogModule log_module = GNC_MOD_BUSINESS;

#define _GNC_MOD_NAME        GNC_ID_EMPLOYEE

G_INLINE_FUNC void mark_employee (GncEmployee *employee);
void mark_employee (GncEmployee *employee)
{
  qof_instance_set_dirty(QOF_INSTANCE (employee));
  qof_event_gen (QOF_INSTANCE (employee), QOF_EVENT_MODIFY, NULL);
}

/* ============================================================== */
/* Create/Destroy Functions */

GncEmployee *gncEmployeeCreate (QofBook *book)
{
  GncEmployee *employee;

  if (!book) return NULL;

  employee = GNC_EMPLOYEE (g_object_new (GNC_TYPE_EMPLOYEE, NULL));
  employee->priv = g_new0 (GncEmployeePrivate, 1);
  
  qof_instance_init (QOF_INSTANCE (employee) , _GNC_MOD_NAME, book);
  
  employee->priv->id = CACHE_INSERT ("");
  employee->priv->username = CACHE_INSERT ("");
  employee->priv->language = CACHE_INSERT ("");
  employee->priv->acl = CACHE_INSERT ("");
  employee->priv->addr = gncAddressCreate (book, QOF_INSTANCE (employee));
  employee->priv->workday = gnc_numeric_zero();
  employee->priv->rate = gnc_numeric_zero();
  employee->priv->active = TRUE;
  
  qof_event_gen (QOF_INSTANCE (employee), QOF_EVENT_CREATE, NULL);

  return employee;
}

void gncEmployeeDestroy (GncEmployee *employee)
{
  if (!employee) return;
  qof_instance_mark_free (QOF_INSTANCE (employee));
  gncEmployeeCommitEdit(employee);
}

static void gncEmployeeFree (GncEmployee *employee)
{
  if (!employee) return;

  qof_event_gen (QOF_INSTANCE (employee), QOF_EVENT_DESTROY, NULL);

  CACHE_REMOVE (employee->priv->id);
  CACHE_REMOVE (employee->priv->username);
  CACHE_REMOVE (employee->priv->language);
  CACHE_REMOVE (employee->priv->acl);
  gncAddressDestroy (employee->priv->addr);

  qof_instance_release (QOF_INSTANCE (employee));
}

GncEmployee *
gncCloneEmployee (GncEmployee *from, QofBook *book)
{
  GncEmployee *employee;
  if (!book || !from) return NULL;

  employee = g_new0 (GncEmployee, 1);
  qof_instance_init(QOF_INSTANCE (employee), _GNC_MOD_NAME, book);
  qof_instance_gemini (QOF_INSTANCE (employee), &from->inst);

  employee->priv->id = CACHE_INSERT (from->id);
  employee->priv->username = CACHE_INSERT (from->username);
  employee->priv->language = CACHE_INSERT (from->language);
  employee->priv->acl = CACHE_INSERT (from->acl);
  employee->priv->addr = gncCloneAddress (from->addr, QOF_INSTANCE (employee), book);
  employee->priv->workday = from->workday;
  employee->priv->rate = from->rate;
  employee->priv->active = from->active;
  employee->priv->currency = gnc_commodity_obtain_twin(from->currency, book);
  employee->priv->ccard_acc = 
     GNC_ACCOUNT(qof_instance_lookup_twin(QOF_INSTANCE(from->ccard_acc), book));
  
  qof_event_gen (QOF_INSTANCE (employee), QOF_EVENT_CREATE, NULL);

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
  SET_STR(employee, employee->priv->id, id);
  mark_employee (employee);
  gncEmployeeCommitEdit (employee);
}

void gncEmployeeSetUsername (GncEmployee *employee, const char *username)
{
  if (!employee) return;
  if (!username) return;
  SET_STR(employee, employee->priv->username, username);
  mark_employee (employee);
  gncEmployeeCommitEdit (employee);
}

void gncEmployeeSetLanguage (GncEmployee *employee, const char *language)
{
  if (!employee) return;
  if (!language) return;
  SET_STR(employee, employee->priv->language, language);
  mark_employee (employee);
  gncEmployeeCommitEdit (employee);
}

void gncEmployeeSetAcl (GncEmployee *employee, const char *acl)
{
  if (!employee) return;
  if (!acl) return;
  SET_STR(employee, employee->priv->acl, acl);
  mark_employee (employee);
  gncEmployeeCommitEdit (employee);
}

void gncEmployeeSetWorkday (GncEmployee *employee, gnc_numeric workday)
{
  if (!employee) return;
  if (gnc_numeric_equal (workday, employee->priv->workday)) return;
  gncEmployeeBeginEdit (employee);
  employee->priv->workday = workday;
  mark_employee (employee);
  gncEmployeeCommitEdit (employee);
}

void gncEmployeeSetRate (GncEmployee *employee, gnc_numeric rate)
{
  if (!employee) return;
  if (gnc_numeric_equal (rate, employee->priv->rate)) return;
  gncEmployeeBeginEdit (employee);
  employee->priv->rate = rate;
  mark_employee (employee);
  gncEmployeeCommitEdit (employee);
}

void gncEmployeeSetCurrency (GncEmployee *employee, gnc_commodity *currency)
{
  if (!employee || !currency) return;
  if (employee->priv->currency && 
      gnc_commodity_equal (employee->priv->currency, currency))
    return;
  gncEmployeeBeginEdit (employee);
  employee->priv->currency = currency;
  mark_employee (employee);
  gncEmployeeCommitEdit (employee);
}

void gncEmployeeSetActive (GncEmployee *employee, gboolean active)
{
  if (!employee) return;
  if (active == employee->priv->active) return;
  gncEmployeeBeginEdit (employee);
  employee->priv->active = active;
  mark_employee (employee);
  gncEmployeeCommitEdit (employee);
}

void gncEmployeeSetCCard (GncEmployee *employee, Account* ccard_acc)
{
  if (!employee) return;
  if (ccard_acc == employee->priv->ccard_acc) return;
  gncEmployeeBeginEdit (employee);
  employee->priv->ccard_acc = ccard_acc;
  mark_employee (employee);
  gncEmployeeCommitEdit (employee);
}

void
qofEmployeeSetAddr (GncEmployee *employee, QofEntity *addr_ent)
{
	GncAddress *addr;

	if(!employee || !addr_ent) { return; }
	addr = (GncAddress*)addr_ent;
	if(addr == employee->priv->addr) { return; }
	if(employee->priv->addr != NULL) { gncAddressDestroy(employee->priv->addr); }
	gncEmployeeBeginEdit(employee);
	employee->priv->addr = addr;
	gncEmployeeCommitEdit(employee);
}

/* ============================================================== */
/* Get Functions */
const char * gncEmployeeGetID (GncEmployee *employee)
{
  if (!employee) return NULL;
  return employee->priv->id;
}

const char * gncEmployeeGetUsername (GncEmployee *employee)
{
  if (!employee) return NULL;
  return employee->priv->username;
}

GncAddress * gncEmployeeGetAddr (GncEmployee *employee)
{
  if (!employee) return NULL;
  return employee->priv->addr;
}

const char * gncEmployeeGetLanguage (GncEmployee *employee)
{
  if (!employee) return NULL;
  return employee->priv->language;
}

const char * gncEmployeeGetAcl (GncEmployee *employee)
{
  if (!employee) return NULL;
  return employee->priv->acl;
}

gnc_numeric gncEmployeeGetWorkday (GncEmployee *employee)
{
  if (!employee) return gnc_numeric_zero();
  return employee->priv->workday;
}

gnc_numeric gncEmployeeGetRate (GncEmployee *employee)
{
  if (!employee) return gnc_numeric_zero();
  return employee->priv->rate;
}

gnc_commodity * gncEmployeeGetCurrency (GncEmployee *employee)
{
  if (!employee) return NULL;
  return employee->priv->currency;
}

gboolean gncEmployeeGetActive (GncEmployee *employee)
{
  if (!employee) return FALSE;
  return employee->priv->active;
}

Account * gncEmployeeGetCCard (GncEmployee *employee)
{
  if (!employee) return NULL;
  return employee->priv->ccard_acc;
}

gboolean gncEmployeeIsDirty (GncEmployee *employee)
{
  if (!employee) return FALSE;
  return (qof_instance_is_dirty (QOF_INSTANCE (employee) || gncAddressIsDirty (employee->priv->addr));
}

void gncEmployeeBeginEdit (GncEmployee *employee)
{
  QOF_BEGIN_EDIT (QOF_INSTANCE (employee));
}

static void gncEmployeeOnError (QofInstance *employee, QofBackendError errcode)
{
  PERR("Employee QofBackend Failure: %d", errcode);
}

static void gncEmployeeOnDone (QofInstance *inst)
{
  GncEmployee *employee = (GncEmployee *) inst;
  gncAddressClearDirty (employee->priv->addr);
}

static void emp_free (QofInstance *inst)
{
  GncEmployee *employee = (GncEmployee *) inst;
  gncEmployeeFree (employee);
}


void gncEmployeeCommitEdit (GncEmployee *employee)
{
  if (!qof_commit_edit (QOF_INSTANCE(employee))) return;
  qof_commit_edit_part2 (QOF_INSTANCE (employee), gncEmployeeOnError,
                         gncEmployeeOnDone, emp_free);
}

/* ============================================================== */
/* Other functions */

int gncEmployeeCompare (GncEmployee *a, GncEmployee *b)
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
