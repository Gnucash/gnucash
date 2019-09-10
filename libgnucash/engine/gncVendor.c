/********************************************************************\
 * gncVendor.c -- the Core Vendor Interface                         *
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
 * Copyright (C) 2001, 2002 Derek Atkins
 * Copyright (C) 2003 <linas@linas.org>
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#include <config.h>

#include <glib.h>
#include <string.h>
#include <qofinstance-p.h>

#include "gnc-commodity.h"
#include "gncAddressP.h"
#include "gncBillTermP.h"
#include "gncInvoice.h"
#include "gncJobP.h"
#include "gncTaxTableP.h"
#include "gncVendor.h"
#include "gncVendorP.h"

static gint vend_qof_event_handler_id = 0;
static void vend_handle_qof_events (QofInstance *entity, QofEventId event_type,
                                    gpointer user_data, gpointer event_data);
static void qofVendorSetAddr (GncVendor *vendor, QofInstance *addr_ent);
static const char* qofVendorGetTaxIncluded(const GncVendor *vendor);
static void qofVendorSetTaxIncluded(GncVendor *vendor, const char* type_string);

struct _gncVendor
{
    QofInstance     inst;

    char *          id;
    char *          name;
    char *          notes;
    GncBillTerm *   terms;
    GncAddress *    addr;
    gnc_commodity * currency;
    GncTaxTable*    taxtable;
    gboolean        taxtable_override;
    GncTaxIncluded  taxincluded;
    gboolean        active;
    GList *         jobs;
    gnc_numeric *   balance; /* cached vendor balance, will not be stored */
};

struct _gncVendorClass
{
    QofInstanceClass parent_class;
};

static QofLogModule log_module = GNC_MOD_BUSINESS;

#define _GNC_MOD_NAME        GNC_ID_VENDOR

/* ============================================================ */
/* Misc inline funcs */

static inline void mark_vendor (GncVendor *vendor);
void mark_vendor (GncVendor *vendor)
{
    qof_instance_set_dirty(&vendor->inst);
    qof_event_gen (&vendor->inst, QOF_EVENT_MODIFY, NULL);
}

/* ============================================================== */

enum
{
    PROP_0,
    PROP_NAME,			/* Table */
    PROP_ID,			/* Table */
    PROP_NOTES,			/* Table */
    PROP_CURRENCY,		/* Table */
    PROP_ACTIVE,		/* Table */
    PROP_TAXTABLE_OVERRIDE,	/* Table */
    PROP_BILLTERMS,		/* Table */
    PROP_TAXTABLE,		/* Table */
    PROP_ADDRESS,		/* Table, 8 fields */
    PROP_TAX_INCLUDED,		/* Table */
    PROP_TAX_INCLUDED_STR,	/* Alternate setter for PROP_TAX_INCLUDED */
    PROP_PDF_DIRNAME,		/* KVP */
    PROP_LAST_POSTED,		/* KVP */
    PROP_PAYMENT_LAST_ACCT,	/* KVP */
};

/* GObject Initialization */
G_DEFINE_TYPE(GncVendor, gnc_vendor, QOF_TYPE_INSTANCE);

static void
gnc_vendor_init(GncVendor* vendor)
{
}

static void
gnc_vendor_dispose(GObject *vendorp)
{
    G_OBJECT_CLASS(gnc_vendor_parent_class)->dispose(vendorp);
}

static void
gnc_vendor_finalize(GObject* vendorp)
{
    G_OBJECT_CLASS(gnc_vendor_parent_class)->finalize(vendorp);
}

/* Note that g_value_set_object() refs the object, as does
 * g_object_get(). But g_object_get() only unrefs once when it disgorges
 * the object, leaving an unbalanced ref, which leaks. So instead of
 * using g_value_set_object(), use g_value_take_object() which doesn't
 * ref the object when used in get_property().
 */
static void
gnc_vendor_get_property (GObject         *object,
                         guint            prop_id,
                         GValue          *value,
                         GParamSpec      *pspec)
{
    GncVendor *vendor;
    gchar *key;

    g_return_if_fail(GNC_IS_VENDOR(object));

    vendor = GNC_VENDOR(object);
    switch (prop_id)
    {
    case PROP_NAME:
        g_value_set_string(value, vendor->name);
        break;
    case PROP_ID:
        g_value_set_string(value, vendor->id);
        break;
    case PROP_NOTES:
        g_value_set_string(value, vendor->notes);
        break;
    case PROP_CURRENCY:
        g_value_take_object(value, vendor->currency);
        break;
    case PROP_ACTIVE:
        g_value_set_boolean(value, vendor->active);
        break;
    case PROP_TAXTABLE_OVERRIDE:
        g_value_set_boolean(value, vendor->taxtable_override);
        break;
    case PROP_BILLTERMS:
        g_value_take_object(value, vendor->terms);
        break;
    case PROP_TAXTABLE:
        g_value_take_object(value, vendor->taxtable);
        break;
    case PROP_ADDRESS:
        g_value_take_object(value, vendor->addr);
        break;
    case PROP_TAX_INCLUDED:
        g_value_set_int(value, vendor->taxincluded);
        break;
    case PROP_TAX_INCLUDED_STR:
        g_value_set_string(value, qofVendorGetTaxIncluded(vendor));
        break;
    case PROP_PDF_DIRNAME:
        qof_instance_get_kvp (QOF_INSTANCE (vendor), value, 1, OWNER_EXPORT_PDF_DIRNAME);
        break;
    case PROP_LAST_POSTED:
        qof_instance_get_kvp (QOF_INSTANCE (vendor), value, 1, LAST_POSTED_TO_ACCT);
        break;
    case PROP_PAYMENT_LAST_ACCT:
        qof_instance_get_kvp (QOF_INSTANCE (vendor), value, 2, GNC_PAYMENT, GNC_LAST_ACCOUNT);
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
        break;
    }
}

static void
gnc_vendor_set_property (GObject         *object,
                         guint            prop_id,
                         const GValue          *value,
                         GParamSpec      *pspec)
{
    GncVendor *vendor;
    gchar *key;

    g_return_if_fail(GNC_IS_VENDOR(object));

    vendor = GNC_VENDOR(object);
    g_assert (qof_instance_get_editlevel(vendor));

    switch (prop_id)
    {
    case PROP_NAME:
        gncVendorSetName(vendor, g_value_get_string(value));
        break;
    case PROP_ID:
        gncVendorSetID(vendor, g_value_get_string(value));
        break;
    case PROP_NOTES:
        gncVendorSetNotes(vendor, g_value_get_string(value));
        break;
    case PROP_CURRENCY:
        gncVendorSetCurrency(vendor, g_value_get_object(value));
        break;
    case PROP_ACTIVE:
        gncVendorSetActive(vendor, g_value_get_boolean(value));
        break;
    case PROP_TAXTABLE_OVERRIDE:
        gncVendorSetTaxTableOverride(vendor, g_value_get_boolean(value));
        break;
    case PROP_BILLTERMS:
        gncVendorSetTerms(vendor, g_value_get_object(value));
        break;
    case PROP_TAXTABLE:
        gncVendorSetTaxTable(vendor, g_value_get_object(value));
        break;
    case PROP_ADDRESS:
        qofVendorSetAddr(vendor, g_value_get_object(value));
        break;
    case PROP_TAX_INCLUDED:
        gncVendorSetTaxIncluded(vendor, (GncTaxIncluded)g_value_get_int(value));
        break;
    case PROP_TAX_INCLUDED_STR:
        qofVendorSetTaxIncluded(vendor, g_value_get_string(value));
        break;
    case PROP_PDF_DIRNAME:
        qof_instance_set_kvp (QOF_INSTANCE (vendor), value, 1, OWNER_EXPORT_PDF_DIRNAME);
        break;
    case PROP_LAST_POSTED:
        qof_instance_set_kvp (QOF_INSTANCE (vendor), value, 1, LAST_POSTED_TO_ACCT);
        break;
    case PROP_PAYMENT_LAST_ACCT:
        qof_instance_set_kvp (QOF_INSTANCE (vendor), value, 2, GNC_PAYMENT, GNC_LAST_ACCOUNT);
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
    GncVendor* v;

    g_return_val_if_fail(inst != NULL, FALSE);
    g_return_val_if_fail(GNC_IS_VENDOR(inst), FALSE);

    v = GNC_VENDOR(inst);

    if (GNC_IS_BILLTERM(ref))
    {
        return (v->terms == GNC_BILLTERM(ref));
    }
    else if (GNC_IS_TAXTABLE(ref))
    {
        return (v->taxtable == GNC_TAXTABLE(ref));
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
    if (!GNC_IS_BILLTERM(ref) && !GNC_IS_TAXTABLE(ref))
    {
        return NULL;
    }

    return qof_instance_get_referring_object_list_from_collection(qof_instance_get_collection(inst), ref);
}

static void
gnc_vendor_class_init (GncVendorClass *klass)
{
    GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
    QofInstanceClass* qof_class = QOF_INSTANCE_CLASS(klass);

    gobject_class->dispose = gnc_vendor_dispose;
    gobject_class->finalize = gnc_vendor_finalize;
    gobject_class->set_property = gnc_vendor_set_property;
    gobject_class->get_property = gnc_vendor_get_property;

    qof_class->get_display_name = NULL;
    qof_class->refers_to_object = impl_refers_to_object;
    qof_class->get_typed_referring_object_list = impl_get_typed_referring_object_list;

    g_object_class_install_property
    (gobject_class,
     PROP_NAME,
     g_param_spec_string ("name",
                          "Vendor Name",
                          "The vendor name is an arbitrary string "
                          "assigned by the user to provide the vendor name.",
                          NULL,
                          G_PARAM_READWRITE));

    g_object_class_install_property
    (gobject_class,
     PROP_ID,
     g_param_spec_string ("id",
                          "Vendor ID",
                          "The vendor id is an arbitrary string "
                          "assigned by the user to identify the vendor.",
                          NULL,
                          G_PARAM_READWRITE));

    g_object_class_install_property
    (gobject_class,
     PROP_NOTES,
     g_param_spec_string ("notes",
                          "Vendor notes",
                          "The vendor notes is an arbitrary string "
                          "assigned by the user to add extra information about the vendor.",
                          NULL,
                          G_PARAM_READWRITE));

    g_object_class_install_property
    (gobject_class,
     PROP_CURRENCY,
     g_param_spec_object ("currency",
                          "Currency",
                          "The currency property denotes the currency used by this vendor.",
                          GNC_TYPE_COMMODITY,
                          G_PARAM_READWRITE));

    g_object_class_install_property
    (gobject_class,
     PROP_ACTIVE,
     g_param_spec_boolean ("active",
                           "Active",
                           "TRUE if the vendor is active.  FALSE if inactive.",
                           FALSE,
                           G_PARAM_READWRITE));

    g_object_class_install_property
    (gobject_class,
     PROP_TAXTABLE_OVERRIDE,
     g_param_spec_boolean ("tax-table-override",
                           "Tax table override",
                           "TRUE if the vendor has a specific tax table which overrides the default "
                           "tax table.  FALSE if the default table should be used.",
                           FALSE,
                           G_PARAM_READWRITE));

    g_object_class_install_property
    (gobject_class,
     PROP_BILLTERMS,
     g_param_spec_object ("terms",
                          "Terms",
                          "The billing terms used by this vendor.",
                          GNC_TYPE_BILLTERM,
                          G_PARAM_READWRITE));

    g_object_class_install_property
    (gobject_class,
     PROP_TAXTABLE,
     g_param_spec_object ("tax-table",
                          "Tax table",
                          "The tax table which applies to this vendor.",
                          GNC_TYPE_TAXTABLE,
                          G_PARAM_READWRITE));

    g_object_class_install_property
    (gobject_class,
     PROP_ADDRESS,
     g_param_spec_object ("address",
                          "Address",
                          "The address property contains the address information for this vendor.",
                          GNC_TYPE_ADDRESS,
                          G_PARAM_READWRITE));

    g_object_class_install_property
    (gobject_class,
     PROP_TAX_INCLUDED,
     g_param_spec_int  ("tax-included",
                        "Tax included",
                        "The tax-included property contains the information about tax calculation this vendor.",
                        GNC_TAXINCLUDED_YES,       /* min */
                        GNC_TAXINCLUDED_USEGLOBAL, /* max */
                        GNC_TAXINCLUDED_USEGLOBAL, /* default */
                        G_PARAM_READWRITE));

    g_object_class_install_property
    (gobject_class,
     PROP_TAX_INCLUDED_STR,
     g_param_spec_string("tax-included-string",
                         "Tax included string",
                         "The tax-included-string property contains a character version of tax-included.",
                         FALSE,
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
GncVendor *gncVendorCreate (QofBook *book)
{
    GncVendor *vendor;

    if (!book) return NULL;

    vendor = g_object_new (GNC_TYPE_VENDOR, NULL);
    qof_instance_init_data (&vendor->inst, _GNC_MOD_NAME, book);

    vendor->id = CACHE_INSERT ("");
    vendor->name = CACHE_INSERT ("");
    vendor->notes = CACHE_INSERT ("");
    vendor->addr = gncAddressCreate (book, &vendor->inst);
    vendor->taxincluded = GNC_TAXINCLUDED_USEGLOBAL;
    vendor->active = TRUE;
    vendor->jobs = NULL;
    vendor->balance = NULL;

    if (vend_qof_event_handler_id == 0)
        vend_qof_event_handler_id = qof_event_register_handler (vend_handle_qof_events, NULL);

    qof_event_gen (&vendor->inst, QOF_EVENT_CREATE, NULL);

    return vendor;
}

void gncVendorDestroy (GncVendor *vendor)
{
    if (!vendor) return;
    qof_instance_set_destroying(vendor, TRUE);
    gncVendorCommitEdit (vendor);
}

static void gncVendorFree (GncVendor *vendor)
{
    if (!vendor) return;

    qof_event_gen (&vendor->inst, QOF_EVENT_DESTROY, NULL);

    CACHE_REMOVE (vendor->id);
    CACHE_REMOVE (vendor->name);
    CACHE_REMOVE (vendor->notes);
    gncAddressBeginEdit (vendor->addr);
    gncAddressDestroy (vendor->addr);
    g_list_free (vendor->jobs);
    g_free (vendor->balance);

    if (vendor->terms)
        gncBillTermDecRef (vendor->terms);
    if (vendor->taxtable)
        gncTaxTableDecRef (vendor->taxtable);

    /* qof_instance_release (&vendor->inst); */
    g_object_unref (vendor);
}

/* ============================================================== */
/* Set Functions */

#define SET_STR(obj, member, str) { \
        char * tmp; \
        \
        if (!g_strcmp0 (member, str)) return; \
        gncVendorBeginEdit (obj); \
        tmp = CACHE_INSERT (str); \
        CACHE_REMOVE (member); \
        member = tmp; \
        }

void gncVendorSetID (GncVendor *vendor, const char *id)
{
    if (!vendor) return;
    if (!id) return;
    SET_STR(vendor, vendor->id, id);
    mark_vendor (vendor);
    gncVendorCommitEdit (vendor);
}

void gncVendorSetName (GncVendor *vendor, const char *name)
{
    if (!vendor) return;
    if (!name) return;
    SET_STR(vendor, vendor->name, name);
    mark_vendor (vendor);
    gncVendorCommitEdit (vendor);
}

void gncVendorSetNotes (GncVendor *vendor, const char *notes)
{
    if (!vendor) return;
    if (!notes) return;
    SET_STR(vendor, vendor->notes, notes);
    mark_vendor (vendor);
    gncVendorCommitEdit (vendor);
}

void gncVendorSetTerms (GncVendor *vendor, GncBillTerm *terms)
{
    if (!vendor) return;
    if (vendor->terms == terms) return;

    gncVendorBeginEdit (vendor);
    if (vendor->terms)
        gncBillTermDecRef (vendor->terms);
    vendor->terms = terms;
    if (vendor->terms)
        gncBillTermIncRef (vendor->terms);
    mark_vendor (vendor);
    gncVendorCommitEdit (vendor);
}

void gncVendorSetTaxIncluded (GncVendor *vendor, GncTaxIncluded taxincl)
{
    if (!vendor) return;
    if (taxincl == vendor->taxincluded) return;
    gncVendorBeginEdit (vendor);
    vendor->taxincluded = taxincl;
    mark_vendor (vendor);
    gncVendorCommitEdit (vendor);
}

void gncVendorSetCurrency (GncVendor *vendor, gnc_commodity *currency)
{
    if (!vendor || !currency) return;
    if (vendor->currency &&
            gnc_commodity_equal (vendor->currency, currency))
        return;
    gncVendorBeginEdit (vendor);
    vendor->currency = currency;
    mark_vendor (vendor);
    gncVendorCommitEdit (vendor);
}

void gncVendorSetActive (GncVendor *vendor, gboolean active)
{
    if (!vendor) return;
    if (active == vendor->active) return;
    gncVendorBeginEdit (vendor);
    vendor->active = active;
    mark_vendor (vendor);
    gncVendorCommitEdit (vendor);
}

void gncVendorSetTaxTableOverride (GncVendor *vendor, gboolean override)
{
    if (!vendor) return;
    if (vendor->taxtable_override == override) return;
    gncVendorBeginEdit (vendor);
    vendor->taxtable_override = override;
    mark_vendor (vendor);
    gncVendorCommitEdit (vendor);
}

void gncVendorSetTaxTable (GncVendor *vendor, GncTaxTable *table)
{
    if (!vendor) return;
    if (vendor->taxtable == table) return;
    gncVendorBeginEdit (vendor);
    if (vendor->taxtable)
        gncTaxTableDecRef (vendor->taxtable);
    if (table)
        gncTaxTableIncRef (table);
    vendor->taxtable = table;
    mark_vendor (vendor);
    gncVendorCommitEdit (vendor);
}

static void
qofVendorSetAddr (GncVendor *vendor, QofInstance *addr_ent)
{
    GncAddress *addr;

    if (!vendor || !addr_ent)
    {
        return;
    }
    addr = (GncAddress*)addr_ent;
    if (addr == vendor->addr)
    {
        return;
    }
    if (vendor->addr != NULL)
    {
        gncAddressBeginEdit(vendor->addr);
        gncAddressDestroy(vendor->addr);
    }
    gncVendorBeginEdit(vendor);
    vendor->addr = addr;
    gncVendorCommitEdit(vendor);
}

static void
qofVendorSetTaxIncluded(GncVendor *vendor, const char* type_string)
{
    GncTaxIncluded inc;

    if (!gncTaxIncludedStringToType(type_string, &inc))
    {
        return;
    }
    gncVendorBeginEdit(vendor);
    vendor->taxincluded = inc;
    gncVendorCommitEdit(vendor);
}

/* ============================================================== */
/* Get Functions */

const char * gncVendorGetID (const GncVendor *vendor)
{
    if (!vendor) return NULL;
    return vendor->id;
}

const char * gncVendorGetName (const GncVendor *vendor)
{
    if (!vendor) return NULL;
    return vendor->name;
}

GncAddress * gncVendorGetAddr (const GncVendor *vendor)
{
    if (!vendor) return NULL;
    return vendor->addr;
}

const char * gncVendorGetNotes (const GncVendor *vendor)
{
    if (!vendor) return NULL;
    return vendor->notes;
}

GncBillTerm * gncVendorGetTerms (const GncVendor *vendor)
{
    if (!vendor) return 0;
    return vendor->terms;
}

GncTaxIncluded gncVendorGetTaxIncluded (const GncVendor *vendor)
{
    if (!vendor) return GNC_TAXINCLUDED_USEGLOBAL;
    return vendor->taxincluded;
}

gnc_commodity * gncVendorGetCurrency (const GncVendor *vendor)
{
    if (!vendor) return NULL;
    return vendor->currency;
}

gboolean gncVendorGetActive (const GncVendor *vendor)
{
    if (!vendor) return FALSE;
    return vendor->active;
}

gboolean gncVendorGetTaxTableOverride (const GncVendor *vendor)
{
    if (!vendor) return FALSE;
    return vendor->taxtable_override;
}

GncTaxTable* gncVendorGetTaxTable (const GncVendor *vendor)
{
    if (!vendor) return NULL;
    return vendor->taxtable;
}

static const char*
qofVendorGetTaxIncluded(const GncVendor *vendor)
{
    return gncTaxIncludedTypeToString(vendor->taxincluded);
}

/* Note that JobList changes do not affect the "dirtiness" of the vendor */
void gncVendorAddJob (GncVendor *vendor, GncJob *job)
{
    if (!vendor) return;
    if (!job) return;

    if (g_list_index(vendor->jobs, job) == -1)
        vendor->jobs = g_list_insert_sorted (vendor->jobs, job,
                                             (GCompareFunc)gncJobCompare);

    qof_event_gen (&vendor->inst, QOF_EVENT_MODIFY, NULL);
}

void gncVendorRemoveJob (GncVendor *vendor, GncJob *job)
{
    GList *node;

    if (!vendor) return;
    if (!job) return;

    node = g_list_find (vendor->jobs, job);
    if (!node)
    {
        /*    PERR ("split not in account"); */
    }
    else
    {
        vendor->jobs = g_list_remove_link (vendor->jobs, node);
        g_list_free_1 (node);
    }

    qof_event_gen (&vendor->inst, QOF_EVENT_MODIFY, NULL);
}

void gncVendorBeginEdit (GncVendor *vendor)
{
    qof_begin_edit(&vendor->inst);
}

static void gncVendorOnError (QofInstance *vendor, QofBackendError errcode)
{
    PERR("Vendor QofBackend Failure: %d", errcode);
    gnc_engine_signal_commit_error( errcode );
}

static void gncVendorOnDone (QofInstance *inst)
{
    GncVendor *vendor = (GncVendor *) inst;
    gncAddressClearDirty (vendor->addr);
}

static void vendor_free (QofInstance *inst)
{
    GncVendor *vendor = (GncVendor *) inst;
    gncVendorFree (vendor);
}

void gncVendorCommitEdit (GncVendor *vendor)
{
    if (!qof_commit_edit (QOF_INSTANCE(vendor))) return;
    qof_commit_edit_part2 (&vendor->inst, gncVendorOnError,
                           gncVendorOnDone, vendor_free);
}

/* ============================================================== */
/* Other functions */

int gncVendorCompare (const GncVendor *a, const GncVendor *b)
{
    if (!a && !b) return 0;
    if (!a && b) return 1;
    if (a && !b) return -1;

    return(strcmp(a->name, b->name));
}

gboolean gncVendorEqual(const GncVendor *a, const GncVendor *b)
{
    if (a == NULL && b == NULL) return TRUE;
    if (a == NULL ||  b == NULL) return FALSE;

    g_return_val_if_fail(GNC_IS_VENDOR(a), FALSE);
    g_return_val_if_fail(GNC_IS_VENDOR(b), FALSE);

    if (g_strcmp0(a->id, b->id) != 0)
    {
        PWARN("IDs differ: %s vs %s", a->id, b->id);
        return FALSE;
    }

    if (g_strcmp0(a->name, b->name) != 0)
    {
        PWARN("Names differ: %s vs %s", a->name, b->name);
        return FALSE;
    }

    if (g_strcmp0(a->notes, b->notes) != 0)
    {
        PWARN("Notes differ");
        return FALSE;
    }

    if (!gncBillTermEqual(a->terms, b->terms))
    {
        PWARN("BillTerms differ");
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

    if (!gncTaxTableEqual(a->taxtable, b->taxtable))
    {
        PWARN("Tax tables differ");
        return FALSE;
    }

    if (a->taxtable_override != b->taxtable_override)
    {
        PWARN("Tax table override flags differ");
        return FALSE;
    }

    if (a->taxincluded != b->taxincluded)
    {
        PWARN("Tax included flags differ");
        return FALSE;
    }

    if (a->active != b->active)
    {
        PWARN("Active flags differ");
        return FALSE;
    }

//    GList *         jobs;
    return TRUE;
}

gboolean
gncVendorIsDirty (const GncVendor *vendor)
{
    if (!vendor) return FALSE;
    return (qof_instance_get_dirty_flag(vendor)
            || gncAddressIsDirty (vendor->addr));
}

/**
 * Listen for qof events.
 *
 * - If the address of a vendor has changed, mark the vendor as dirty.
 * - If a lot related to a vendor has changed, clear the vendor's
 *   cached balance as it likely has become invalid.
 *
 * @param entity Entity for the event
 * @param event_type Event type
 * @param user_data User data registered with the handler
 * @param event_data Event data passed with the event.
 */
static void
vend_handle_qof_events (QofInstance *entity, QofEventId event_type,
                        gpointer user_data, gpointer event_data)
{
    /* Handle address change events */
    if ((GNC_IS_ADDRESS (entity) &&
        (event_type & QOF_EVENT_MODIFY) != 0))
    {
        if (GNC_IS_VENDOR (event_data))
        {
            GncVendor* vend = GNC_VENDOR (event_data);
            gncVendorBeginEdit (vend);
            mark_vendor (vend);
            gncVendorCommitEdit (vend);
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

        if (gncOwnerGetType (end_owner) == GNC_OWNER_VENDOR)
        {
            /* Clear the cached balance */
            GncVendor* vend = gncOwnerGetVendor (end_owner);
            g_free (vend->balance);
            vend->balance = NULL;
        }
        return;
    }
}

/* ============================================================== */
/* Package-Private functions */

static const char * _gncVendorPrintable (gpointer item)
{
    GncVendor *v = item;
    if (!item) return NULL;
    return v->name;
}

static void
destroy_vendor_on_book_close(QofInstance *ent, gpointer data)
{
    GncVendor* v = GNC_VENDOR(ent);

    gncVendorBeginEdit(v);
    gncVendorDestroy(v);
}

/** Handles book end - frees all vendors from the book
 *
 * @param book Book being closed
 */
static void
gnc_vendor_book_end(QofBook* book)
{
    QofCollection *col;

    col = qof_book_get_collection(book, GNC_ID_VENDOR);
    qof_collection_foreach(col, destroy_vendor_on_book_close, NULL);
}

static QofObject gncVendorDesc =
{
    DI(.interface_version = ) QOF_OBJECT_VERSION,
    DI(.e_type            = ) _GNC_MOD_NAME,
    DI(.type_label        = ) "Vendor",
    DI(.create            = ) (gpointer)gncVendorCreate,
    DI(.book_begin        = ) NULL,
    DI(.book_end          = ) gnc_vendor_book_end,
    DI(.is_dirty          = ) qof_collection_is_dirty,
    DI(.mark_clean        = ) qof_collection_mark_clean,
    DI(.foreach           = ) qof_collection_foreach,
    DI(.printable         = ) _gncVendorPrintable,
    DI(.version_cmp       = ) (int (*)(gpointer, gpointer)) qof_instance_version_cmp,
};

gboolean gncVendorRegister (void)
{
    static QofParam params[] =
    {
        { VENDOR_ID, QOF_TYPE_STRING, (QofAccessFunc)gncVendorGetID, (QofSetterFunc)gncVendorSetID },
        { VENDOR_NAME, QOF_TYPE_STRING, (QofAccessFunc)gncVendorGetName, (QofSetterFunc)gncVendorSetName },
        { VENDOR_ADDR,    GNC_ID_ADDRESS, (QofAccessFunc)gncVendorGetAddr, (QofSetterFunc)qofVendorSetAddr },
        { VENDOR_NOTES,   QOF_TYPE_STRING, (QofAccessFunc)gncVendorGetNotes, (QofSetterFunc)gncVendorSetNotes },
        { VENDOR_TERMS,   GNC_ID_BILLTERM, (QofAccessFunc)gncVendorGetTerms, (QofSetterFunc)gncVendorSetTerms },
        {
            VENDOR_TAX_OVERRIDE, QOF_TYPE_BOOLEAN, (QofAccessFunc)gncVendorGetTaxTableOverride,
            (QofSetterFunc)gncVendorSetTaxTableOverride
        },
        {
            VENDOR_TAX_TABLE, GNC_ID_TAXTABLE, (QofAccessFunc)gncVendorGetTaxTable,
            (QofSetterFunc)gncVendorSetTaxTable
        },
        {
            VENDOR_TAX_INC, QOF_TYPE_STRING, (QofAccessFunc)qofVendorGetTaxIncluded,
            (QofSetterFunc)qofVendorSetTaxIncluded
        },
        { QOF_PARAM_BOOK, QOF_ID_BOOK, (QofAccessFunc)qof_instance_get_book, NULL },
        { QOF_PARAM_GUID, QOF_TYPE_GUID, (QofAccessFunc)qof_instance_get_guid, NULL },
        { QOF_PARAM_ACTIVE, QOF_TYPE_BOOLEAN, (QofAccessFunc)gncVendorGetActive, NULL },
        { NULL },
    };

    if (!qof_choice_add_class(GNC_ID_INVOICE, GNC_ID_VENDOR, INVOICE_OWNER))
    {
        return FALSE;
    }
    if (!qof_choice_add_class(GNC_ID_JOB, GNC_ID_VENDOR, JOB_OWNER))
    {
        return FALSE;
    }

    qof_class_register (_GNC_MOD_NAME, (QofSortFunc)gncVendorCompare, params);

    return qof_object_register (&gncVendorDesc);
}

gchar *gncVendorNextID (QofBook *book)
{
    return qof_book_increment_and_format_counter (book, _GNC_MOD_NAME);
}

const gnc_numeric*
gncVendorGetCachedBalance (GncVendor *vend)
{
    return vend->balance;
}

void gncVendorSetCachedBalance (GncVendor *vend, const gnc_numeric *new_bal)
{
    if (!new_bal)
    {
        if (vend->balance)
        {
            g_free (vend->balance);
            vend->balance = NULL;
        }
        return;
    }

    if (!vend->balance)
        vend->balance = g_new0 (gnc_numeric, 1);

    *vend->balance = *new_bal;
}
