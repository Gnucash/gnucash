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

#include <config.h>

#include <glib.h>
#include <string.h>
#include <qofinstance-p.h>

#include "Account.h"
#include "gnc-commodity.h"
#include "gncAddressP.h"
#include "gncEmployee.h"
#include "gncEmployeeP.h"
#include "gnc-lot.h"
#include "gncOwner.h"

static gint empl_qof_event_handler_id = 0;
static void empl_handle_qof_events (QofInstance *entity, QofEventId event_type,
                                    gpointer user_data, gpointer event_data);

struct _gncEmployee
{
    QofInstance     inst;
    char *          id;
    char *          username;
    GncAddress *    addr;
    gnc_commodity * currency;
    gboolean        active;
    gnc_numeric *   balance;

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

static inline void mark_employee (GncEmployee *employee);
void mark_employee (GncEmployee *employee)
{
    qof_instance_set_dirty(&employee->inst);
    qof_event_gen (&employee->inst, QOF_EVENT_MODIFY, NULL);
}

/* ============================================================== */

enum
{
    PROP_0,
    PROP_USERNAME,		/* Table */
    PROP_ID,			/* Table */
    PROP_LANGUAGE,		/* Table */
    PROP_ACL,			/* Table */
    PROP_ACTIVE,		/* Table */
    PROP_CURRENCY,		/* Table */
    PROP_CCARD,			/* Table */
    PROP_WORKDAY,		/* Table (numeric) */
    PROP_RATE,			/* Table (numeric) */
    PROP_ADDRESS,		/* Table, 8 fields */
    PROP_PDF_DIRNAME,		/* KVP */
    PROP_LAST_POSTED,		/* KVP */
    PROP_PAYMENT_LAST_ACCT,	/* KVP */
};

/* GObject Initialization */
G_DEFINE_TYPE(GncEmployee, gnc_employee, QOF_TYPE_INSTANCE);

static void
gnc_employee_init(GncEmployee* emp)
{
}

static void
gnc_employee_dispose(GObject *empp)
{
    G_OBJECT_CLASS(gnc_employee_parent_class)->dispose(empp);
}

static void
gnc_employee_finalize(GObject* empp)
{
    G_OBJECT_CLASS(gnc_employee_parent_class)->finalize(empp);
}

/* Note that g_value_set_object() refs the object, as does
 * g_object_get(). But g_object_get() only unrefs once when it disgorges
 * the object, leaving an unbalanced ref, which leaks. So instead of
 * using g_value_set_object(), use g_value_take_object() which doesn't
 * ref the object when used in get_property().
 */
static void
gnc_employee_get_property (GObject         *object,
                           guint            prop_id,
                           GValue          *value,
                           GParamSpec      *pspec)
{
    GncEmployee *emp;
    g_return_if_fail(GNC_IS_EMPLOYEE(object));
    emp = GNC_EMPLOYEE(object);
    switch (prop_id)
    {
    case PROP_USERNAME:
        g_value_set_string(value, emp->username);
        break;
    case PROP_ID:
        g_value_set_string(value, emp->id);
        break;
    case PROP_ACTIVE:
        g_value_set_boolean(value, emp->active);
        break;
    case PROP_LANGUAGE:
        g_value_set_string(value, emp->language);
        break;
    case PROP_CURRENCY:
        g_value_take_object(value, emp->currency);
        break;
    case PROP_ACL:
        g_value_set_string(value, emp->acl);
        break;
    case PROP_ADDRESS:
        g_value_take_object(value, emp->addr);
        break;
    case PROP_WORKDAY:
        g_value_set_boxed(value, &emp->workday);
        break;
    case PROP_RATE:
        g_value_set_boxed(value, &emp->rate);
        break;
    case PROP_CCARD:
        g_value_take_object(value, emp->ccard_acc);
        break;
    case PROP_PDF_DIRNAME:
        qof_instance_get_kvp (QOF_INSTANCE (emp), value, 1, OWNER_EXPORT_PDF_DIRNAME);
        break;
    case PROP_LAST_POSTED:
        qof_instance_get_kvp (QOF_INSTANCE (emp), value, 1, LAST_POSTED_TO_ACCT);
        break;
    case PROP_PAYMENT_LAST_ACCT:
        qof_instance_get_kvp (QOF_INSTANCE (emp), value, 2, GNC_PAYMENT, GNC_LAST_ACCOUNT);
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
        break;
    }
}

static void
gnc_employee_set_property (GObject         *object,
                           guint            prop_id,
                           const GValue          *value,
                           GParamSpec      *pspec)
{
    GncEmployee *emp;
    g_return_if_fail(GNC_IS_EMPLOYEE(object));
    emp = GNC_EMPLOYEE(object);
    g_assert (qof_instance_get_editlevel(emp));
    switch (prop_id)
    {
    case PROP_USERNAME:
        gncEmployeeSetUsername(emp, g_value_get_string(value));
        break;
    case PROP_ID:
        gncEmployeeSetID(emp, g_value_get_string(value));
        break;
    case PROP_ACTIVE:
        gncEmployeeSetActive(emp, g_value_get_boolean(value));
        break;
    case PROP_LANGUAGE:
        gncEmployeeSetLanguage(emp, g_value_get_string(value));
        break;
    case PROP_CURRENCY:
        gncEmployeeSetCurrency(emp, g_value_get_object(value));
        break;
    case PROP_ACL:
        gncEmployeeSetAcl(emp, g_value_get_string(value));
        break;
    case PROP_ADDRESS:
        qofEmployeeSetAddr(emp, g_value_get_object(value));
        break;
    case PROP_WORKDAY:
        gncEmployeeSetWorkday(emp, *(gnc_numeric*)g_value_get_boxed(value));
        break;
    case PROP_RATE:
        gncEmployeeSetRate(emp, *(gnc_numeric*)g_value_get_boxed(value));
        break;
    case PROP_CCARD:
        gncEmployeeSetCCard(emp, g_value_get_object(value));
        break;
    case PROP_PDF_DIRNAME:
        qof_instance_set_kvp (QOF_INSTANCE (emp), value, 1, OWNER_EXPORT_PDF_DIRNAME);
        break;
    case PROP_LAST_POSTED:
        qof_instance_set_kvp (QOF_INSTANCE (emp), value, 1, LAST_POSTED_TO_ACCT);
        break;
    case PROP_PAYMENT_LAST_ACCT:
        qof_instance_set_kvp (QOF_INSTANCE (emp), value, 2, GNC_PAYMENT, GNC_LAST_ACCOUNT);
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
        break;
    }
}

/** Does this object refer to a specific object */
static gboolean
impl_refers_to_object(const QofInstance* inst, const QofInstance* ref)
{
    GncEmployee* emp;

    g_return_val_if_fail(inst != NULL, FALSE);
    g_return_val_if_fail(GNC_IS_EMPLOYEE(inst), FALSE);

    emp = GNC_EMPLOYEE(inst);

    if (GNC_IS_COMMODITY(ref))
    {
        return (emp->currency == GNC_COMMODITY(ref));
    }
    else if (GNC_IS_ACCOUNT(ref))
    {
        return (emp->ccard_acc == GNC_ACCOUNT(ref));
    }

    return FALSE;
}

/** Returns a list of my type of object which refers to an object.  For example, when called as
        qof_instance_get_typed_referring_object_list(taxtable, account);
    it will return the list of taxtables which refer to a specific account.  The result should be the
    same regardless of which taxtable object is used.  The list must be freed by the caller but the
    objects on the list must not.
 */
static GList*
impl_get_typed_referring_object_list(const QofInstance* inst, const QofInstance* ref)
{
    if (!GNC_IS_COMMODITY(ref) && !GNC_IS_ACCOUNT(ref))
    {
        return NULL;
    }

    return qof_instance_get_referring_object_list_from_collection(qof_instance_get_collection(inst), ref);
}

static void
gnc_employee_class_init (GncEmployeeClass *klass)
{
    GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
    QofInstanceClass* qof_class = QOF_INSTANCE_CLASS(klass);

    gobject_class->dispose = gnc_employee_dispose;
    gobject_class->finalize = gnc_employee_finalize;
    gobject_class->set_property = gnc_employee_set_property;
    gobject_class->get_property = gnc_employee_get_property;

    qof_class->get_display_name = NULL;
    qof_class->refers_to_object = impl_refers_to_object;
    qof_class->get_typed_referring_object_list = impl_get_typed_referring_object_list;

    g_object_class_install_property
    (gobject_class,
     PROP_USERNAME,
     g_param_spec_string ("username",
                          "Employee Name",
                          "The employee name is an arbitrary string "
                          "assigned by the user which provides the employee "
                          "name.",
                          NULL,
                          G_PARAM_READWRITE));

    g_object_class_install_property
    (gobject_class,
     PROP_ID,
     g_param_spec_string ("id",
                          "Employee ID",
                          "The employee ID is an arbitrary string "
                          "assigned by the user which provides the employee "
                          "ID.",
                          NULL,
                          G_PARAM_READWRITE));

    g_object_class_install_property
    (gobject_class,
     PROP_ACTIVE,
     g_param_spec_boolean ("active",
                           "Active",
                           "TRUE if the employee is active.  FALSE if inactive.",
                           FALSE,
                           G_PARAM_READWRITE));

    g_object_class_install_property
    (gobject_class,
     PROP_LANGUAGE,
     g_param_spec_string ("language",
                          "Employee Language",
                          "The language is an arbitrary string "
                          "assigned by the user which provides the language spoken "
                          " by the employee.",
                          NULL,
                          G_PARAM_READWRITE));

    g_object_class_install_property
    (gobject_class,
     PROP_CURRENCY,
     g_param_spec_object ("currency",
                          "Currency",
                          "The currency property denotes the currency used by this employee.",
                          GNC_TYPE_COMMODITY,
                          G_PARAM_READWRITE));

    g_object_class_install_property
    (gobject_class,
     PROP_ACL,
     g_param_spec_string ("acl",
                          "Employee ACL",
                          "The acl is an arbitrary string "
                          "assigned by the user which provides ??? "
                          " for the employee.",
                          NULL,
                          G_PARAM_READWRITE));

    g_object_class_install_property
    (gobject_class,
     PROP_ADDRESS,
     g_param_spec_object ("address",
                          "Address",
                          "The address property contains the address information for this employee.",
                          GNC_TYPE_ADDRESS,
                          G_PARAM_READWRITE));

    g_object_class_install_property
    (gobject_class,
     PROP_WORKDAY,
     g_param_spec_boxed("workday",
                        "Workday rate",
                        "The daily rate for this employee.",
                        GNC_TYPE_NUMERIC,
                        G_PARAM_READWRITE));

    g_object_class_install_property
    (gobject_class,
     PROP_RATE,
     g_param_spec_boxed("rate",
                        "Hourly rate",
                        "The hourly rate for this employee.",
                        GNC_TYPE_NUMERIC,
                        G_PARAM_READWRITE));

    g_object_class_install_property
    (gobject_class,
     PROP_CCARD,
     g_param_spec_object ("credit-card-account",
                          "Credit card account",
                          "The credit card account for this employee.",
                          GNC_TYPE_ACCOUNT,
                          G_PARAM_READWRITE));

    g_object_class_install_property
    (gobject_class,
     PROP_PDF_DIRNAME,
     g_param_spec_string ("export-pdf-dir",
                          "Export PDF Directory Name",
                          "A subdirectory for exporting PDF reports which is "
			  "appended to the target directory when writing them "
			  "out. It is retrieved from preferences and stored on "
			  "each 'Owner' object which prints items after "
			  "printing.",
                          NULL,
                          G_PARAM_READWRITE));

    g_object_class_install_property(
       gobject_class,
       PROP_LAST_POSTED,
       g_param_spec_boxed("invoice-last-posted-account",
			  "Invoice Last Posted Account",
			  "The last account to which an invoice belonging to "
			  "this owner was posted.",
			  GNC_TYPE_GUID,
			  G_PARAM_READWRITE));

    g_object_class_install_property(
       gobject_class,
       PROP_PAYMENT_LAST_ACCT,
       g_param_spec_boxed("payment-last-account",
			  "Payment Last Account",
			  "The last account to which an payment belonging to "
			  "this owner was posted.",
			  GNC_TYPE_GUID,
			  G_PARAM_READWRITE));
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
    employee->balance = NULL;

    if (empl_qof_event_handler_id == 0)
        empl_qof_event_handler_id = qof_event_register_handler (empl_handle_qof_events, NULL);

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
    g_free (employee->balance);

    /* qof_instance_release (&employee->inst); */
    g_object_unref (employee);
}

/* ============================================================== */
/* Set Functions */

#define SET_STR(obj, member, str) { \
        char * tmp; \
        \
        if (!g_strcmp0 (member, str)) return; \
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

/* Employees don't have a name property defined, but
 * in order to get a consistent interface with other owner types,
 * this function fakes one by setting the name property of
 * the employee's address.
 */
void gncEmployeeSetName (GncEmployee *employee, const char *name)
{
    if (!employee) return;
    if (!name) return;
    gncAddressSetName (gncEmployeeGetAddr (employee), name);
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

    if (!employee || !addr_ent)
    {
        return;
    }
    addr = (GncAddress*)addr_ent;
    if (addr == employee->addr)
    {
        return;
    }
    if (employee->addr != NULL)
    {
        gncAddressBeginEdit(employee->addr);
        gncAddressDestroy(employee->addr);
    }
    gncEmployeeBeginEdit(employee);
    employee->addr = addr;
    mark_employee (employee);
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

/* Employees don't have a name property defined, but
 * in order to get a consistent interface with other owner types,
 * this function fakes one by returning the name property of
 * the employee's address.
 */
const char * gncEmployeeGetName (const GncEmployee *employee)
{
    if (!employee) return NULL;
    return gncAddressGetName ( gncEmployeeGetAddr (employee));
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
    gnc_engine_signal_commit_error( errcode );
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

gboolean gncEmployeeEqual(const GncEmployee* a, const GncEmployee* b)
{
    if (a == NULL && b == NULL) return TRUE;
    if (a == NULL || b == NULL ) return FALSE;

    g_return_val_if_fail(GNC_IS_EMPLOYEE(a), FALSE);
    g_return_val_if_fail(GNC_IS_EMPLOYEE(b), FALSE);

    if (g_strcmp0(a->id, b->id) != 0)
    {
        PWARN("IDs differ: %s vs %s", a->id, b->id);
        return FALSE;
    }

    if (g_strcmp0(a->username, b->username) != 0)
    {
        PWARN("Usernames differ: %s vs %s", a->username, b->username);
        return FALSE;
    }

    if (!gncAddressEqual(a->addr, b->addr))
    {
        PWARN("Addresses differ");
        return FALSE;
    }

    if (!gnc_commodity_equal(a->currency, b->currency))
    {
        PWARN("Currencies differ");
        return FALSE;
    }

    if (a->active != b->active)
    {
        PWARN("Active flags differ");
        return FALSE;
    }

    if (g_strcmp0(a->language, b->language) != 0)
    {
        PWARN("Languages differ: %s vs %s", a->language, b->language);
        return FALSE;
    }

    if (g_strcmp0(a->acl, b->acl) != 0)
    {
        PWARN("ACLs differ: %s vs %s", a->acl, b->acl);
        return FALSE;
    }

    if (!xaccAccountEqual(a->ccard_acc, b->ccard_acc, TRUE))
    {
        PWARN("Accounts differ");
        return FALSE;
    }

    if (!gnc_numeric_equal(a->workday, b->workday))
    {
        PWARN("Workdays differ");
        return FALSE;
    }

    if (!gnc_numeric_equal(a->rate, b->rate))
    {
        PWARN("Rates differ");
        return FALSE;
    }

    return TRUE;
}

/* Package-Private functions */

static const char * _gncEmployeePrintable (gpointer item)
{
    GncEmployee *v = item;
    if (!item) return NULL;
    return gncAddressGetName(v->addr);
}

/**
 * Listen for qof events.
 *
 * - If the address of an employee has changed, mark the employee as dirty.
 * - If a lot related to an employee has changed, clear the employee's
 *   cached balance as it likely has become invalid.
 *
 * @param entity Entity for the event
 * @param event_type Event type
 * @param user_data User data registered with the handler
 * @param event_data Event data passed with the event.
 */
static void
empl_handle_qof_events (QofInstance *entity, QofEventId event_type,
                        gpointer user_data, gpointer event_data)
{

    /* Handle address change events */
    if ((GNC_IS_ADDRESS (entity) &&
        (event_type & QOF_EVENT_MODIFY) != 0))
    {
        if (GNC_IS_EMPLOYEE (event_data))
        {
            GncEmployee* empl = GNC_EMPLOYEE (event_data);
            gncEmployeeBeginEdit (empl);
            mark_employee (empl);
            gncEmployeeCommitEdit (empl);
        }
        return;
    }

    /* Handle lot change events */
    if (GNC_IS_LOT (entity))
    {
        GNCLot *lot = GNC_LOT (entity);
        GncOwner lot_owner;
        const GncOwner *end_owner = NULL;
        GncInvoice *invoice = gncInvoiceGetInvoiceFromLot (lot);

        /* Determine the owner associated with the lot */
        if (invoice)
            /* Invoice lots */
            end_owner = gncOwnerGetEndOwner (gncInvoiceGetOwner (invoice));
        else if (gncOwnerGetOwnerFromLot (lot, &lot_owner))
            /* Pre-payment lots */
            end_owner = gncOwnerGetEndOwner (&lot_owner);

        if (gncOwnerGetType (end_owner) == GNC_OWNER_EMPLOYEE)
        {
            /* Clear the cached balance */
            GncEmployee* empl = gncOwnerGetEmployee (end_owner);
            g_free (empl->balance);
            empl->balance = NULL;
        }
        return;
    }
}

static void
destroy_employee_on_book_close(QofInstance *ent, gpointer data)
{
    GncEmployee* e = GNC_EMPLOYEE(ent);

    gncEmployeeBeginEdit(e);
    gncEmployeeDestroy(e);
}

/** Handles book end - frees all employees from the book
 *
 * @param book Book being closed
 */
static void
gnc_employee_book_end(QofBook* book)
{
    QofCollection *col;

    col = qof_book_get_collection(book, GNC_ID_EMPLOYEE);
    qof_collection_foreach(col, destroy_employee_on_book_close, NULL);
}

static QofObject gncEmployeeDesc =
{
    DI(.interface_version = ) QOF_OBJECT_VERSION,
    DI(.e_type            = ) _GNC_MOD_NAME,
    DI(.type_label        = ) "Employee",
    DI(.create            = ) (gpointer)gncEmployeeCreate,
    DI(.book_begin        = ) NULL,
    DI(.book_end          = ) gnc_employee_book_end,
    DI(.is_dirty          = ) qof_collection_is_dirty,
    DI(.mark_clean        = ) qof_collection_mark_clean,
    DI(.foreach           = ) qof_collection_foreach,
    DI(.printable         = ) _gncEmployeePrintable,
    DI(.version_cmp       = ) (int (*)(gpointer, gpointer)) qof_instance_version_cmp,
};

gboolean gncEmployeeRegister (void)
{
    static QofParam params[] =
    {
        { EMPLOYEE_ID, QOF_TYPE_STRING, (QofAccessFunc)gncEmployeeGetID, (QofSetterFunc)gncEmployeeSetID },
        {
            EMPLOYEE_USERNAME, QOF_TYPE_STRING, (QofAccessFunc)gncEmployeeGetUsername,
            (QofSetterFunc)gncEmployeeSetUsername
        },
        {
            EMPLOYEE_NAME, QOF_TYPE_STRING, (QofAccessFunc)gncEmployeeGetName,
            (QofSetterFunc)gncEmployeeSetName
        },
        {
            EMPLOYEE_LANGUAGE, QOF_TYPE_STRING, (QofAccessFunc)gncEmployeeGetLanguage,
            (QofSetterFunc)gncEmployeeSetLanguage
        },
        { EMPLOYEE_ACL, QOF_TYPE_STRING, (QofAccessFunc)gncEmployeeGetAcl, (QofSetterFunc)gncEmployeeSetAcl },
        {
            EMPLOYEE_WORKDAY, QOF_TYPE_NUMERIC, (QofAccessFunc)gncEmployeeGetWorkday,
            (QofSetterFunc)gncEmployeeSetWorkday
        },
        { EMPLOYEE_RATE, QOF_TYPE_NUMERIC, (QofAccessFunc)gncEmployeeGetRate, (QofSetterFunc)gncEmployeeSetRate },
        { EMPLOYEE_ADDR, GNC_ID_ADDRESS, (QofAccessFunc)gncEmployeeGetAddr, (QofSetterFunc)qofEmployeeSetAddr },
        { EMPLOYEE_CC,  GNC_ID_ACCOUNT, (QofAccessFunc)gncEmployeeGetCCard, (QofSetterFunc)gncEmployeeSetCCard },
        { QOF_PARAM_ACTIVE, QOF_TYPE_BOOLEAN, (QofAccessFunc)gncEmployeeGetActive, (QofSetterFunc)gncEmployeeSetActive },
        { QOF_PARAM_BOOK, QOF_ID_BOOK, (QofAccessFunc)qof_instance_get_book, NULL },
        { QOF_PARAM_GUID, QOF_TYPE_GUID, (QofAccessFunc)qof_instance_get_guid, NULL },
        { NULL },
    };

    qof_class_register (_GNC_MOD_NAME, (QofSortFunc)gncEmployeeCompare, params);

    return qof_object_register (&gncEmployeeDesc);
}

gchar *gncEmployeeNextID (QofBook *book)
{
    return qof_book_increment_and_format_counter (book, _GNC_MOD_NAME);
}

const gnc_numeric*
gncEmployeeGetCachedBalance (GncEmployee *empl)
{
    return empl->balance;
}

void gncEmployeeSetCachedBalance (GncEmployee *empl, const gnc_numeric *new_bal)
{
    if (!new_bal)
    {
        if (empl->balance)
        {
            g_free (empl->balance);
            empl->balance = NULL;
        }
        return;
    }

    if (!empl->balance)
        empl->balance = g_new0 (gnc_numeric, 1);

    *empl->balance = *new_bal;
}
