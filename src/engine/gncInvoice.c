/********************************************************************\
 * gncInvoice.c -- the Core Business Invoice                        *
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
 * Copyright (C) 2001,2002,2006 Derek Atkins
 * Copyright (C) 2003 Linas Vepstas <linas@linas.org>
 * Copyright (c) 2005 Neil Williams <linux@codehelp.co.uk>
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#include <config.h>

#include <glib.h>
#include <glib/gi18n.h>
#include <qofinstance-p.h>

#include "Transaction.h"
#include "Account.h"
#include "gncBillTermP.h"
#include "gncEntry.h"
#include "gncEntryP.h"
#include "gnc-features.h"
#include "gncJobP.h"
#include "gncInvoice.h"
#include "gncInvoiceP.h"
#include "gncOwnerP.h"
#include "engine-helpers.h"

struct _gncInvoice
{
    QofInstance   inst;

    char          *id;
    char          *notes;
    gboolean      active;

    char          *billing_id;
    char          *printname;
    GncBillTerm   *terms;
    GList         *entries;
    GList         *prices;
    GncOwner      owner;
    GncOwner      billto;
    GncJob        *job;
    Timespec      date_opened;
    Timespec      date_posted;

    gnc_numeric   to_charge_amount;

    gnc_commodity *currency;

    Account       *posted_acc;
    Transaction   *posted_txn;
    GNCLot        *posted_lot;
};

struct _gncInvoiceClass
{
    QofInstanceClass parent_class;
};

static QofLogModule log_module = GNC_MOD_BUSINESS;

#define _GNC_MOD_NAME     GNC_ID_INVOICE

#define GNC_INVOICE_IS_CN "credit-note"

#define SET_STR(obj, member, str) { \
	char * tmp; \
	\
	if (!g_strcmp0 (member, str)) return; \
	gncInvoiceBeginEdit (obj); \
	tmp = CACHE_INSERT (str); \
	CACHE_REMOVE (member); \
	member = tmp; \
	}

static void mark_invoice (GncInvoice *invoice);
static void
mark_invoice (GncInvoice *invoice)
{
    qof_instance_set_dirty(&invoice->inst);
    qof_event_gen (&invoice->inst, QOF_EVENT_MODIFY, NULL);
}

QofBook * gncInvoiceGetBook(GncInvoice *x)
{
    return qof_instance_get_book(QOF_INSTANCE(x));
}

/* ================================================================== */

enum
{
    PROP_0,
//  PROP_ID,		/* Table */
//  PROP_DATE_OPENED,	/* Table */
//  PROP_DATE_POSTED,	/* Table */
    PROP_NOTES,		/* Table */
//  PROP_ACTIVE,	/* Table */
//  PROP_CURRENCY,	/* Table */
//  PROP_OWNER_TYPE,	/* Table */
//  PROP_OWNER,		/* Table */
//  PROP_TERMS,		/* Table */
//  PROP_BILLING_ID,	/* Table */
//  PROP_POST_TXN,	/* Table */
//  PROP_POST_LOT,	/* Table */
//  PROP_POST_ACCOUNT,	/* Table */
//  PROP_BILLTO_TYPE,	/* Table */
//  PROP_BILLTO,	/* Table */
//  PROP_CHARGE_AMOUNT, /* Table, (numeric) */
};

/* GObject Initialization */
G_DEFINE_TYPE(GncInvoice, gnc_invoice, QOF_TYPE_INSTANCE);

static void
gnc_invoice_init(GncInvoice* inv)
{
}

static void
gnc_invoice_dispose(GObject *invp)
{
    G_OBJECT_CLASS(gnc_invoice_parent_class)->dispose(invp);
}

static void
gnc_invoice_finalize(GObject* invp)
{
    G_OBJECT_CLASS(gnc_invoice_parent_class)->finalize(invp);
}

static void
gnc_invoice_get_property (GObject         *object,
                          guint            prop_id,
                          GValue          *value,
                          GParamSpec      *pspec)
{
    GncInvoice *inv;

    g_return_if_fail(GNC_IS_INVOICE(object));

    inv = GNC_INVOICE(object);
    switch (prop_id)
    {
    case PROP_NOTES:
        g_value_set_string(value, inv->notes);
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
        break;
    }
}

static void
gnc_invoice_set_property (GObject         *object,
                          guint            prop_id,
                          const GValue          *value,
                          GParamSpec      *pspec)
{
    GncInvoice *inv;

    g_return_if_fail(GNC_IS_INVOICE(object));

    inv = GNC_INVOICE(object);
    g_assert (qof_instance_get_editlevel(inv));

    switch (prop_id)
    {
    case PROP_NOTES:
        gncInvoiceSetNotes(inv, g_value_get_string(value));
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
        break;
    }
}

/** Returns a string representing this object */
static gchar*
impl_get_display_name(const QofInstance* inst)
{
    GncInvoice* inv;
    QofInstance* owner;
    gchar* s;

    g_return_val_if_fail(inst != NULL, FALSE);
    g_return_val_if_fail(GNC_IS_INVOICE(inst), FALSE);

    inv = GNC_INVOICE(inst);
    owner = qofOwnerGetOwner(&inv->owner);
    if (owner != NULL)
    {
        gchar* display_name;

        display_name = qof_instance_get_display_name(owner);
        s = g_strdup_printf("Invoice %s (%s)", inv->id, display_name);
        g_free(display_name);
    }
    else
    {
        s = g_strdup_printf("Invoice %s", inv->id);
    }

    return s;
}

/** Does this object refer to a specific object */
static gboolean
impl_refers_to_object(const QofInstance* inst, const QofInstance* ref)
{
    GncInvoice* inv;

    g_return_val_if_fail(inst != NULL, FALSE);
    g_return_val_if_fail(GNC_IS_INVOICE(inst), FALSE);

    inv = GNC_INVOICE(inst);

    if (GNC_IS_BILLTERM(ref))
    {
        return (inv->terms == GNC_BILLTERM(ref));
    }
    else if (GNC_IS_JOB(ref))
    {
        return (inv->job == GNC_JOB(ref));
    }
    else if (GNC_IS_COMMODITY(ref))
    {
        return (inv->currency == GNC_COMMODITY(ref));
    }
    else if (GNC_IS_ACCOUNT(ref))
    {
        return (inv->posted_acc == GNC_ACCOUNT(ref));
    }
    else if (GNC_IS_TRANSACTION(ref))
    {
        return (inv->posted_txn == GNC_TRANSACTION(ref));
    }
    else if (GNC_IS_LOT(ref))
    {
        return (inv->posted_lot == GNC_LOT(ref));
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
    if (!GNC_IS_BILLTERM(ref) && !GNC_IS_JOB(ref) && !GNC_IS_COMMODITY(ref) && !GNC_IS_ACCOUNT(ref)
            && !GNC_IS_TRANSACTION(ref) && !GNC_IS_LOT(ref))
    {
        return NULL;
    }

    return qof_instance_get_referring_object_list_from_collection(qof_instance_get_collection(inst), ref);
}

static void
gnc_invoice_class_init (GncInvoiceClass *klass)
{
    GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
    QofInstanceClass* qof_class = QOF_INSTANCE_CLASS(klass);

    gobject_class->dispose = gnc_invoice_dispose;
    gobject_class->finalize = gnc_invoice_finalize;
    gobject_class->set_property = gnc_invoice_set_property;
    gobject_class->get_property = gnc_invoice_get_property;

    qof_class->get_display_name = impl_get_display_name;
    qof_class->refers_to_object = impl_refers_to_object;
    qof_class->get_typed_referring_object_list = impl_get_typed_referring_object_list;

    g_object_class_install_property
    (gobject_class,
     PROP_NOTES,
     g_param_spec_string ("notes",
                          "Invoice Notes",
                          "The invoice notes is an arbitrary string "
                          "assigned by the user to provide notes regarding "
                          "this invoice.",
                          NULL,
                          G_PARAM_READWRITE));
}

/* Create/Destroy Functions */
GncInvoice *gncInvoiceCreate (QofBook *book)
{
    GncInvoice *invoice;

    if (!book) return NULL;

    invoice = g_object_new (GNC_TYPE_INVOICE, NULL);
    qof_instance_init_data (&invoice->inst, _GNC_MOD_NAME, book);

    invoice->id = CACHE_INSERT ("");
    invoice->notes = CACHE_INSERT ("");
    invoice->billing_id = CACHE_INSERT ("");

    invoice->billto.type = GNC_OWNER_CUSTOMER;
    invoice->active = TRUE;

    invoice->to_charge_amount = gnc_numeric_zero();

    qof_event_gen (&invoice->inst, QOF_EVENT_CREATE, NULL);

    return invoice;
}

GncInvoice *gncInvoiceCopy (const GncInvoice *from)
{
    GncInvoice *invoice;
    QofBook* book;
    GList *node;
    gint64 is_cn;

    g_assert(from);
    book = qof_instance_get_book(from);
    g_assert(book);

    invoice = g_object_new (GNC_TYPE_INVOICE, NULL);
    qof_instance_init_data (&invoice->inst, _GNC_MOD_NAME, book);

    gncInvoiceBeginEdit(invoice);

    invoice->id = CACHE_INSERT (from->id);
    invoice->notes = CACHE_INSERT (from->notes);
    invoice->billing_id = CACHE_INSERT (from->billing_id);
    invoice->active = from->active;

    is_cn = kvp_frame_get_gint64(from->inst.kvp_data, GNC_INVOICE_IS_CN);
    kvp_frame_set_gint64(invoice->inst.kvp_data, GNC_INVOICE_IS_CN, is_cn);

    invoice->terms = from->terms;
    gncBillTermIncRef (invoice->terms);

    gncOwnerCopy(&from->billto, &invoice->billto);
    gncOwnerCopy(&from->owner, &invoice->owner);
    invoice->job = from->job; // FIXME: Need IncRef or similar here?!?

    invoice->to_charge_amount = from->to_charge_amount;
    invoice->date_opened = from->date_opened;

    // Oops. Do not forget to copy the pointer to the correct currency here.
    invoice->currency = from->currency;

    // Copy all invoice->entries
    for (node = from->entries; node; node = node->next)
    {
        GncEntry *from_entry = node->data;
        GncEntry *to_entry = gncEntryCreate(book);
        gncEntryCopy(from_entry, to_entry, FALSE);

        switch (gncInvoiceGetOwnerType (invoice))
        {
        case GNC_OWNER_VENDOR:
        case GNC_OWNER_EMPLOYEE:
            // this is a vendor bill, or an expense voucher
            gncBillAddEntry(invoice, to_entry);
            break;
        case GNC_OWNER_CUSTOMER:
        default:
            // this is an invoice
            gncInvoiceAddEntry(invoice, to_entry);
            break;
        }
    }

    // FIXME: The prices are not (yet) copied; is this a problem?

    // Posted-date and the posted Txn is intentionally not copied; the
    // copy isn't "posted" but needs to be posted by the user.
    mark_invoice (invoice);
    gncInvoiceCommitEdit(invoice);

    return invoice;
}

void gncInvoiceDestroy (GncInvoice *invoice)
{
    if (!invoice) return;
    qof_instance_set_destroying(invoice, TRUE);
    gncInvoiceCommitEdit (invoice);
}

static void gncInvoiceFree (GncInvoice *invoice)
{
    if (!invoice) return;

    qof_event_gen (&invoice->inst, QOF_EVENT_DESTROY, NULL);

    CACHE_REMOVE (invoice->id);
    CACHE_REMOVE (invoice->notes);
    CACHE_REMOVE (invoice->billing_id);
    g_list_free (invoice->entries);
    g_list_free (invoice->prices);

    if (invoice->printname) g_free (invoice->printname);

    if (invoice->terms)
        gncBillTermDecRef (invoice->terms);

    /* qof_instance_release (&invoice->inst); */
    g_object_unref (invoice);
}

/* ================================================================== */
/* Set Functions */

void gncInvoiceSetID (GncInvoice *invoice, const char *id)
{
    if (!invoice || !id) return;
    SET_STR (invoice, invoice->id, id);
    mark_invoice (invoice);
    gncInvoiceCommitEdit (invoice);
}

void gncInvoiceSetOwner (GncInvoice *invoice, GncOwner *owner)
{
    if (!invoice || !owner) return;
    if (gncOwnerEqual (&invoice->owner, owner)) return;
    gncInvoiceBeginEdit (invoice);
    gncOwnerCopy (owner, &invoice->owner);
    mark_invoice (invoice);
    gncInvoiceCommitEdit (invoice);
}

static void
qofInvoiceSetOwner (GncInvoice *invoice, QofInstance *ent)
{
    if (!invoice || !ent)
    {
        return;
    }
    gncInvoiceBeginEdit (invoice);
    qofOwnerSetEntity(&invoice->owner, ent);
    mark_invoice (invoice);
    gncInvoiceCommitEdit (invoice);
}

static void
qofInvoiceSetBillTo (GncInvoice *invoice, QofInstance *ent)
{
    if (!invoice || !ent)
    {
        return;
    }
    gncInvoiceBeginEdit (invoice);
    qofOwnerSetEntity(&invoice->billto, ent);
    mark_invoice (invoice);
    gncInvoiceCommitEdit (invoice);
}

void gncInvoiceSetDateOpenedGDate (GncInvoice *invoice, const GDate *date)
{
    g_assert (date);
    gncInvoiceSetDateOpened(invoice, timespecCanonicalDayTime(gdate_to_timespec(*date)));
}

void gncInvoiceSetDateOpened (GncInvoice *invoice, Timespec date)
{
    if (!invoice) return;
    if (timespec_equal (&invoice->date_opened, &date)) return;
    gncInvoiceBeginEdit (invoice);
    invoice->date_opened = date;
    mark_invoice (invoice);
    gncInvoiceCommitEdit (invoice);
}

void gncInvoiceSetDatePosted (GncInvoice *invoice, Timespec date)
{
    if (!invoice) return;
    if (timespec_equal (&invoice->date_posted, &date)) return;
    gncInvoiceBeginEdit (invoice);
    invoice->date_posted = date;
    mark_invoice (invoice);
    gncInvoiceCommitEdit (invoice);
}

void gncInvoiceSetTerms (GncInvoice *invoice, GncBillTerm *terms)
{
    if (!invoice) return;
    if (invoice->terms == terms) return;
    gncInvoiceBeginEdit (invoice);
    if (invoice->terms)
        gncBillTermDecRef (invoice->terms);
    invoice->terms = terms;
    if (invoice->terms)
        gncBillTermIncRef (invoice->terms);
    mark_invoice (invoice);
    gncInvoiceCommitEdit (invoice);
}

void gncInvoiceSetBillingID (GncInvoice *invoice, const char *billing_id)
{
    if (!invoice) return;
    SET_STR (invoice, invoice->billing_id, billing_id);
    mark_invoice (invoice);
    gncInvoiceCommitEdit (invoice);
}

void gncInvoiceSetNotes (GncInvoice *invoice, const char *notes)
{
    if (!invoice || !notes) return;
    SET_STR (invoice, invoice->notes, notes);
    mark_invoice (invoice);
    gncInvoiceCommitEdit (invoice);
}

void gncInvoiceSetActive (GncInvoice *invoice, gboolean active)
{
    if (!invoice) return;
    if (invoice->active == active) return;
    gncInvoiceBeginEdit (invoice);
    invoice->active = active;
    mark_invoice (invoice);
    gncInvoiceCommitEdit (invoice);
}

void gncInvoiceSetIsCreditNote (GncInvoice *invoice, gboolean credit_note)
{
    if (!invoice) return;
    gncInvoiceBeginEdit (invoice);
    kvp_frame_set_gint64(invoice->inst.kvp_data, GNC_INVOICE_IS_CN,
                         credit_note ? 1 : 0);
    mark_invoice (invoice);
    gncInvoiceCommitEdit (invoice);

    /* If this is a credit note, set a feature flag for it in the book
     * This will prevent older GnuCash versions that don't support
     * credit notes to open this file. */
    if (credit_note)
        gnc_features_set_used (gncInvoiceGetBook (invoice), GNC_FEATURE_CREDIT_NOTES);
}

void gncInvoiceSetCurrency (GncInvoice *invoice, gnc_commodity *currency)
{
    if (!invoice || !currency) return;
    if (invoice->currency &&
            gnc_commodity_equal (invoice->currency, currency))
        return;
    gncInvoiceBeginEdit (invoice);
    invoice->currency = currency;
    mark_invoice (invoice);
    gncInvoiceCommitEdit (invoice);
}

void gncInvoiceSetBillTo (GncInvoice *invoice, GncOwner *billto)
{
    if (!invoice || !billto) return;
    if (gncOwnerEqual (&invoice->billto, billto)) return;

    gncInvoiceBeginEdit (invoice);
    gncOwnerCopy (billto, &invoice->billto);
    mark_invoice (invoice);
    gncInvoiceCommitEdit (invoice);
}

void gncInvoiceSetToChargeAmount (GncInvoice *invoice, gnc_numeric amount)
{
    if (!invoice) return;
    if (gnc_numeric_equal (invoice->to_charge_amount, amount)) return;
    gncInvoiceBeginEdit (invoice);
    invoice->to_charge_amount = amount;
    mark_invoice (invoice);
    gncInvoiceCommitEdit (invoice);
}

void gncInvoiceSetPostedTxn (GncInvoice *invoice, Transaction *txn)
{
    if (!invoice) return;
    g_return_if_fail (invoice->posted_txn == NULL);

    gncInvoiceBeginEdit (invoice);
    invoice->posted_txn = txn;
    mark_invoice (invoice);
    gncInvoiceCommitEdit (invoice);
}

void gncInvoiceSetPostedLot (GncInvoice *invoice, GNCLot *lot)
{
    if (!invoice) return;
    g_return_if_fail (invoice->posted_lot == NULL);

    gncInvoiceBeginEdit (invoice);
    invoice->posted_lot = lot;
    mark_invoice (invoice);
    gncInvoiceCommitEdit (invoice);
}

void gncInvoiceSetPostedAcc (GncInvoice *invoice, Account *acc)
{
    if (!invoice) return;
    g_return_if_fail (invoice->posted_acc == NULL);

    gncInvoiceBeginEdit (invoice);
    invoice->posted_acc = acc;
    mark_invoice (invoice);
    gncInvoiceCommitEdit (invoice);
}

void gncInvoiceAddEntry (GncInvoice *invoice, GncEntry *entry)
{
    GncInvoice *old;

    g_assert(invoice);
    g_assert(entry);
    if (!invoice || !entry) return;

    old = gncEntryGetInvoice (entry);
    if (old == invoice) return;	/* I already own this one */
    if (old) gncInvoiceRemoveEntry (old, entry);

    gncInvoiceBeginEdit (invoice);
    gncEntrySetInvoice (entry, invoice);
    invoice->entries = g_list_insert_sorted (invoice->entries, entry,
                       (GCompareFunc)gncEntryCompare);
    mark_invoice (invoice);
    gncInvoiceCommitEdit (invoice);
}

void gncInvoiceRemoveEntry (GncInvoice *invoice, GncEntry *entry)
{
    if (!invoice || !entry) return;

    gncInvoiceBeginEdit (invoice);
    gncEntrySetInvoice (entry, NULL);
    invoice->entries = g_list_remove (invoice->entries, entry);
    mark_invoice (invoice);
    gncInvoiceCommitEdit (invoice);
}

void gncInvoiceAddPrice (GncInvoice *invoice, GNCPrice *price)
{
    GList *node;
    gnc_commodity *commodity;

    if (!invoice || !price) return;

    /* Keep only one price per commodity per invoice
     * So if a price was set previously remove it first */
    node = g_list_first(invoice->prices);
    commodity = gnc_price_get_commodity (price);
    while (node != NULL)
    {
        GNCPrice *curr = (GNCPrice*)node->data;
        if (gnc_commodity_equal (commodity, gnc_price_get_commodity (curr)))
            break;
        node = g_list_next (node);
    }

    gncInvoiceBeginEdit (invoice);
    if (node)
        invoice->prices = g_list_delete_link (invoice->prices, node);
    invoice->prices = g_list_prepend(invoice->prices, price);
    mark_invoice (invoice);
    gncInvoiceCommitEdit (invoice);
}

void gncBillAddEntry (GncInvoice *bill, GncEntry *entry)
{
    GncInvoice *old;

    g_assert(bill);
    g_assert(entry);
    if (!bill || !entry) return;

    old = gncEntryGetBill (entry);
    if (old == bill) return;	/* I already own this one */
    if (old) gncBillRemoveEntry (old, entry);

    gncInvoiceBeginEdit (bill);
    gncEntrySetBill (entry, bill);
    bill->entries = g_list_insert_sorted (bill->entries, entry,
                                          (GCompareFunc)gncEntryCompare);
    mark_invoice (bill);
    gncInvoiceCommitEdit (bill);
}

void gncBillRemoveEntry (GncInvoice *bill, GncEntry *entry)
{
    if (!bill || !entry) return;

    gncInvoiceBeginEdit (bill);
    gncEntrySetBill (entry, NULL);
    bill->entries = g_list_remove (bill->entries, entry);
    mark_invoice (bill);
    gncInvoiceCommitEdit (bill);
}

void gncInvoiceSortEntries (GncInvoice *invoice)
{
    if (!invoice) return;
    invoice->entries = g_list_sort(invoice->entries,
                                   (GCompareFunc)gncEntryCompare);
    gncInvoiceBeginEdit (invoice);
    mark_invoice(invoice);
    gncInvoiceCommitEdit (invoice);
}

void gncInvoiceRemoveEntries (GncInvoice *invoice)
{
    GList *node;

    if (!invoice) return;

    for (node = invoice->entries; node; node = node->next)
    {
        GncEntry *entry = node->data;

        switch (gncInvoiceGetOwnerType (invoice))
        {
        case GNC_OWNER_VENDOR:
        case GNC_OWNER_EMPLOYEE:
            // this is a vendor bill, or an expense voucher
            gncBillRemoveEntry (invoice, entry);
            break;
        case GNC_OWNER_CUSTOMER:
        default:
            // this is an invoice
            gncInvoiceRemoveEntry (invoice, entry);
            break;
        }

        /* If the entry is no longer referenced by any document,
         * remove it.
         */
        if (!(gncEntryGetInvoice (entry) ||
              gncEntryGetBill (entry) ||
              gncEntryGetOrder (entry)))
        {
            gncEntryBeginEdit (entry);
            gncEntryDestroy (entry);
        }
    }
}

/* ================================================================== */
/* Get Functions */

const char * gncInvoiceGetID (const GncInvoice *invoice)
{
    if (!invoice) return NULL;
    return invoice->id;
}

const GncOwner * gncInvoiceGetOwner (const GncInvoice *invoice)
{
    if (!invoice) return NULL;
    return &invoice->owner;
}

static QofInstance*
qofInvoiceGetOwner (GncInvoice *invoice)
{
    GncOwner *owner;

    if (!invoice)
    {
        return NULL;
    }
    owner = &invoice->owner;
    return QOF_INSTANCE(owner);
}

static QofInstance*
qofInvoiceGetBillTo (GncInvoice *invoice)
{
    GncOwner *billto;

    if (!invoice)
    {
        return NULL;
    }
    billto = &invoice->billto;
    return QOF_INSTANCE(billto);
}

Timespec gncInvoiceGetDateOpened (const GncInvoice *invoice)
{
    Timespec ts;
    ts.tv_sec = 0;
    ts.tv_nsec = 0;
    if (!invoice) return ts;
    return invoice->date_opened;
}

Timespec gncInvoiceGetDatePosted (const GncInvoice *invoice)
{
    Timespec ts;
    ts.tv_sec = 0;
    ts.tv_nsec = 0;
    if (!invoice) return ts;
    return invoice->date_posted;
}

Timespec gncInvoiceGetDateDue (const GncInvoice *invoice)
{
    Transaction *txn;
    Timespec ts;
    ts.tv_sec = 0;
    ts.tv_nsec = 0;
    if (!invoice) return ts;
    txn = gncInvoiceGetPostedTxn (invoice);
    if (!txn) return ts;
    return xaccTransRetDateDueTS (txn);
}

GncBillTerm * gncInvoiceGetTerms (const GncInvoice *invoice)
{
    if (!invoice) return NULL;
    return invoice->terms;
}

const char * gncInvoiceGetBillingID (const GncInvoice *invoice)
{
    if (!invoice) return NULL;
    return invoice->billing_id;
}

const char * gncInvoiceGetNotes (const GncInvoice *invoice)
{
    if (!invoice) return NULL;
    return invoice->notes;
}

GncOwnerType gncInvoiceGetOwnerType (const GncInvoice *invoice)
{
    const GncOwner *owner;
    g_return_val_if_fail (invoice, GNC_OWNER_NONE);

    owner = gncOwnerGetEndOwner (gncInvoiceGetOwner (invoice));
    return (gncOwnerGetType (owner));
}

static gnc_numeric
gncInvoiceGetTotalInternal (GncInvoice *invoice, gboolean use_value,
                            gboolean use_tax,
                            gboolean use_payment_type, GncEntryPaymentType type)
{
    GList *node;
    gnc_numeric total = gnc_numeric_zero();
    gboolean is_cust_doc, is_cn;

    g_return_val_if_fail (invoice, total);

    /* Is the current document an invoice/credit note related to a customer or a vendor/employee ?
     * The GncEntry code needs to know to return the proper entry amounts
     */
    is_cust_doc = (gncInvoiceGetOwnerType (invoice) == GNC_OWNER_CUSTOMER);
    is_cn = gncInvoiceGetIsCreditNote (invoice);

    for (node = gncInvoiceGetEntries(invoice); node; node = node->next)
    {
        GncEntry *entry = node->data;
        gnc_numeric value, tax;

        if (use_payment_type && gncEntryGetBillPayment (entry) != type)
            continue;

        value = gncEntryGetDocValue (entry, FALSE, is_cust_doc, is_cn);
        if (gnc_numeric_check (value) == GNC_ERROR_OK)
        {
            if (use_value)
                total = gnc_numeric_add (total, value, GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD);
        }
        else
            g_warning ("bad value in our entry");

        if (use_tax)
        {
            tax = gncEntryGetDocTaxValue (entry, FALSE, is_cust_doc, is_cn);
            if (gnc_numeric_check (tax) == GNC_ERROR_OK)
                total = gnc_numeric_add (total, tax, GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD);
            else
                g_warning ("bad tax-value in our entry");
        }
    }
    return total;
}

gnc_numeric gncInvoiceGetTotal (GncInvoice *invoice)
{
    if (!invoice) return gnc_numeric_zero();
    return gncInvoiceGetTotalInternal(invoice, TRUE, TRUE, FALSE, 0);
}

gnc_numeric gncInvoiceGetTotalSubtotal (GncInvoice *invoice)
{
    if (!invoice) return gnc_numeric_zero();
    return gncInvoiceGetTotalInternal(invoice, TRUE, FALSE, FALSE, 0);
}

gnc_numeric gncInvoiceGetTotalTax (GncInvoice *invoice)
{
    if (!invoice) return gnc_numeric_zero();
    return gncInvoiceGetTotalInternal(invoice, FALSE, TRUE, FALSE, 0);
}

gnc_numeric gncInvoiceGetTotalOf (GncInvoice *invoice, GncEntryPaymentType type)
{
    if (!invoice) return gnc_numeric_zero();
    return gncInvoiceGetTotalInternal(invoice, TRUE, TRUE, TRUE, type);
}

GList * gncInvoiceGetTypeListForOwnerType (GncOwnerType type)
{
    GList *type_list = NULL;
    switch (type)
    {
    case GNC_OWNER_CUSTOMER:
        type_list = g_list_append (type_list, GINT_TO_POINTER(GNC_INVOICE_CUST_INVOICE));
        type_list = g_list_append (type_list, GINT_TO_POINTER(GNC_INVOICE_CUST_CREDIT_NOTE));
        return type_list;
    case GNC_OWNER_VENDOR:
        type_list = g_list_append (type_list, GINT_TO_POINTER(GNC_INVOICE_VEND_INVOICE));
        type_list = g_list_append (type_list, GINT_TO_POINTER(GNC_INVOICE_VEND_CREDIT_NOTE));
        return type_list;
    case GNC_OWNER_EMPLOYEE:
        type_list = g_list_append (type_list, GINT_TO_POINTER(GNC_INVOICE_EMPL_INVOICE));
        type_list = g_list_append (type_list, GINT_TO_POINTER(GNC_INVOICE_EMPL_CREDIT_NOTE));
        return type_list;
    default:
        return NULL;
    }

}

GncInvoiceType gncInvoiceGetType (const GncInvoice *invoice)
{
    if (!invoice) return GNC_INVOICE_UNDEFINED;
    switch (gncInvoiceGetOwnerType (invoice))
    {
    case GNC_OWNER_CUSTOMER:
        return (gncInvoiceGetIsCreditNote(invoice) ?
                GNC_INVOICE_CUST_CREDIT_NOTE :
                GNC_INVOICE_CUST_INVOICE);
    case GNC_OWNER_VENDOR:
        return (gncInvoiceGetIsCreditNote(invoice) ?
                GNC_INVOICE_VEND_CREDIT_NOTE :
                GNC_INVOICE_VEND_INVOICE);
    case GNC_OWNER_EMPLOYEE:
        return (gncInvoiceGetIsCreditNote(invoice) ?
                GNC_INVOICE_EMPL_CREDIT_NOTE :
                GNC_INVOICE_EMPL_INVOICE);
    default:
        PWARN ("No invoice types defined for owner %d",
               gncInvoiceGetOwnerType (invoice));
        return GNC_INVOICE_UNDEFINED;
    }
}

const char * gncInvoiceGetTypeString (const GncInvoice *invoice)
{
    GncInvoiceType type = gncInvoiceGetType(invoice);
    switch (type)
    {
    case GNC_INVOICE_CUST_INVOICE:
        return _("Invoice");
    case GNC_INVOICE_VEND_INVOICE:
        return _("Bill");
    case GNC_INVOICE_EMPL_INVOICE:
        return _("Expense");
    case GNC_INVOICE_CUST_CREDIT_NOTE:
    case GNC_INVOICE_VEND_CREDIT_NOTE:
    case GNC_INVOICE_EMPL_CREDIT_NOTE:
        return _("Credit Note");
    default:
        PWARN("Unknown invoice type");
        return NULL;
    }
}

gnc_commodity * gncInvoiceGetCurrency (const GncInvoice *invoice)
{
    if (!invoice) return NULL;
    return invoice->currency;
}

GncOwner * gncInvoiceGetBillTo (GncInvoice *invoice)
{
    if (!invoice) return NULL;
    return &invoice->billto;
}

GNCLot * gncInvoiceGetPostedLot (const GncInvoice *invoice)
{
    if (!invoice) return NULL;
    return invoice->posted_lot;
}

Transaction * gncInvoiceGetPostedTxn (const GncInvoice *invoice)
{
    if (!invoice) return NULL;
    return invoice->posted_txn;
}

Account * gncInvoiceGetPostedAcc (const GncInvoice *invoice)
{
    if (!invoice) return NULL;
    return invoice->posted_acc;
}

gboolean gncInvoiceGetActive (const GncInvoice *invoice)
{
    if (!invoice) return FALSE;
    return invoice->active;
}

gboolean gncInvoiceGetIsCreditNote (const GncInvoice *invoice)
{
    if (!invoice) return FALSE;
    if (kvp_frame_get_gint64(invoice->inst.kvp_data, GNC_INVOICE_IS_CN))
        return TRUE;
    else
        return FALSE;
}


gnc_numeric gncInvoiceGetToChargeAmount (const GncInvoice *invoice)
{
    if (!invoice) return gnc_numeric_zero();
    return invoice->to_charge_amount;
}

EntryList * gncInvoiceGetEntries (GncInvoice *invoice)
{
    if (!invoice) return NULL;
    return invoice->entries;
}

GNCPrice * gncInvoiceGetPrice(GncInvoice *invoice, gnc_commodity *commodity)
{
    GList *node = g_list_first(invoice->prices);

    while (node != NULL)
    {
        GNCPrice *curr = (GNCPrice*)node->data;

        if (gnc_commodity_equal(commodity, gnc_price_get_commodity(curr)))
            return curr;

        node = g_list_next(node);
    }

    return NULL;
}

static QofCollection*
qofInvoiceGetEntries (GncInvoice *invoice)
{
    QofCollection *entry_coll;
    GList         *list;
    QofInstance     *entry;

    entry_coll = qof_collection_new(GNC_ID_ENTRY);
    for (list = gncInvoiceGetEntries(invoice); list != NULL; list = list->next)
    {
        entry = QOF_INSTANCE(list->data);
        qof_collection_add_entity(entry_coll, entry);
    }
    return entry_coll;
}

static void
qofInvoiceEntryCB (QofInstance *ent, gpointer user_data)
{
    GncInvoice *invoice;

    invoice = (GncInvoice*)user_data;
    if (!invoice || !ent)
    {
        return;
    }
    switch (gncInvoiceGetOwnerType (invoice))
    {
    case GNC_OWNER_VENDOR:
    {
        gncBillAddEntry (invoice, (GncEntry*) ent);
        break;
    }
    default :
    {
        gncInvoiceAddEntry(invoice, (GncEntry*)ent);
        break;
    }
    }
}

static void
qofInvoiceSetEntries(GncInvoice *invoice, QofCollection *entry_coll)
{
    if (!entry_coll)
    {
        return;
    }
    if (0 == g_strcmp0(qof_collection_get_type(entry_coll), GNC_ID_ENTRY))
    {
        qof_collection_foreach(entry_coll, qofInvoiceEntryCB, invoice);
    }
}

static GncJob*
qofInvoiceGetJob (const GncInvoice *invoice)
{
    if (!invoice)
    {
        return NULL;
    }
    return invoice->job;
}

static void
qofInvoiceSetJob (GncInvoice *invoice, GncJob *job)
{
    if (!invoice)
    {
        return;
    }
    invoice->job = job;
}

static void
gncInvoiceDetachFromLot (GNCLot *lot)
{
    if (!lot) return;

    gnc_lot_begin_edit (lot);
    qof_instance_set (QOF_INSTANCE (lot), "invoice", NULL, NULL);
    gnc_lot_commit_edit (lot);
}

static void
gncInvoiceAttachToLot (GncInvoice *invoice, GNCLot *lot)
{
    GncGUID *guid;
    if (!invoice || !lot)
        return;

    if (invoice->posted_lot) return;	/* Cannot reset invoice's lot */
    guid  = (GncGUID*)qof_instance_get_guid (QOF_INSTANCE (invoice));
    gnc_lot_begin_edit (lot);
    qof_instance_set (QOF_INSTANCE (lot), "invoice", guid, NULL);
    gnc_lot_commit_edit (lot);
    gncInvoiceSetPostedLot (invoice, lot);
}

GncInvoice * gncInvoiceGetInvoiceFromLot (GNCLot *lot)
{
    GncGUID *guid = NULL;
    QofBook *book;

    if (!lot) return NULL;

    book = gnc_lot_get_book (lot);
    qof_instance_get (QOF_INSTANCE (lot), "invoice", &guid, NULL);
    return gncInvoiceLookup(book, guid);
}

static void
gncInvoiceAttachToTxn (GncInvoice *invoice, Transaction *txn)
{
    if (!invoice || !txn)
        return;

    if (invoice->posted_txn) return;	/* Cannot reset invoice's txn */

    xaccTransBeginEdit (txn);
    qof_instance_set (QOF_INSTANCE (txn), "invoice", //Prop INVOICE
		      qof_instance_get_guid (QOF_INSTANCE (invoice)), NULL);
    xaccTransSetTxnType (txn, TXN_TYPE_INVOICE);
    xaccTransCommitEdit (txn);
    gncInvoiceSetPostedTxn (invoice, txn);
}

GncInvoice *
gncInvoiceGetInvoiceFromTxn (const Transaction *txn)
{
    GncGUID *guid = NULL;
    QofBook *book;

    if (!txn) return NULL;

    book = xaccTransGetBook (txn);
    qof_instance_get (QOF_INSTANCE (txn), "invoice", &guid, NULL);
    return gncInvoiceLookup(book, guid);
}

gboolean gncInvoiceAmountPositive (const GncInvoice *invoice)
{
    switch (gncInvoiceGetType (invoice))
    {
    case GNC_INVOICE_CUST_INVOICE:
    case GNC_INVOICE_VEND_CREDIT_NOTE:
    case GNC_INVOICE_EMPL_CREDIT_NOTE:
        return TRUE;
    case GNC_INVOICE_CUST_CREDIT_NOTE:
    case GNC_INVOICE_VEND_INVOICE:
    case GNC_INVOICE_EMPL_INVOICE:
        return FALSE;
    case GNC_INVOICE_UNDEFINED:
    default:
        /* Should never be reached.
         * If it is, perhaps a new value is added to GncInvoiceType ? */
        g_assert_not_reached();
        return FALSE;
    }
}

GHashTable *gncInvoiceGetForeignCurrencies (const GncInvoice *invoice)
{
    EntryList *entries_iter;
    gboolean is_cust_doc = (gncInvoiceGetOwnerType (invoice) == GNC_OWNER_CUSTOMER);
    gboolean is_cn = gncInvoiceGetIsCreditNote (invoice);
    GHashTable *amt_hash = g_hash_table_new_full (g_direct_hash, g_direct_equal,
                                                  NULL, g_free);

    for (entries_iter = invoice->entries; entries_iter != NULL; entries_iter = g_list_next(entries_iter))
    {
        GncEntry *entry = (GncEntry*)entries_iter->data;
        Account *this_acc;
        gnc_commodity *account_currency;
        AccountValueList *tt_amts = NULL, *tt_iter;

        /* Check entry's account currency */
        this_acc = (is_cust_doc ? gncEntryGetInvAccount (entry) :
                    gncEntryGetBillAccount (entry));
        account_currency = xaccAccountGetCommodity (this_acc);

        if (this_acc &&
                !gnc_commodity_equal (gncInvoiceGetCurrency (invoice), account_currency))
        {
            gnc_numeric *curr_amt = (gnc_numeric*) g_hash_table_lookup (amt_hash, account_currency);
            gnc_numeric *entry_amt = (gnc_numeric*) g_new0 (gnc_numeric, 1);
            *entry_amt = gncEntryGetDocValue (entry, FALSE, is_cust_doc, is_cn);
            if (curr_amt)
                *entry_amt = gnc_numeric_add (*entry_amt, *curr_amt, GNC_DENOM_AUTO, GNC_HOW_RND_ROUND_HALF_UP);
            g_hash_table_insert (amt_hash, account_currency, entry_amt);
        }

        /* Check currencies of each account in the tax table linked
         * to the current entry */
        tt_amts = gncEntryGetDocTaxValues (entry, is_cust_doc, is_cn);

        if (!tt_amts)
            continue;

        for (tt_iter = tt_amts; tt_iter != NULL; tt_iter = g_list_next(tt_iter))
        {
            GncAccountValue *tt_amt_val = (GncAccountValue*)tt_iter->data;
            Account *tt_acc = tt_amt_val->account;
            gnc_commodity *tt_acc_currency = xaccAccountGetCommodity (tt_acc);

            if (tt_acc &&
                    !gnc_commodity_equal (gncInvoiceGetCurrency (invoice), tt_acc_currency))
            {
                gnc_numeric *curr_amt = (gnc_numeric*) g_hash_table_lookup (amt_hash, tt_acc_currency);
                gnc_numeric *tt_acc_amt = (gnc_numeric*) g_new0 (gnc_numeric, 1);
                *tt_acc_amt = tt_amt_val->value;
                if (curr_amt)
                    *tt_acc_amt = gnc_numeric_add (*tt_acc_amt, *curr_amt, GNC_DENOM_AUTO, GNC_HOW_RND_ROUND_HALF_UP);
                g_hash_table_insert (amt_hash, tt_acc_currency, tt_acc_amt);
            }
        }
        gncAccountValueDestroy (tt_amts);
    }
    return amt_hash;
}

static gboolean gncInvoicePostAddSplit (QofBook *book,
                                        Account *acc,
                                        Transaction *txn,
                                        gnc_numeric value,
                                        const gchar *memo,
                                        const gchar *type,
                                        GncInvoice *invoice)
{
    Split *split;

    split = xaccMallocSplit (book);
    /* set action and memo? */

    xaccSplitSetMemo (split, memo);
    /* set per book option */
    gnc_set_num_action (NULL, split, gncInvoiceGetID(invoice), type);

    /* Need to insert this split into the account AND txn before
     * we set the Base Value.  Otherwise SetBaseValue complains
     * that we don't have an account and fails to set the value.
     */
    xaccAccountBeginEdit (acc);
    xaccAccountInsertSplit (acc, split);
    xaccAccountCommitEdit (acc);
    xaccTransAppendSplit (txn, split);

    /* General note on the split creations below:
     * Invoice and bill amounts are always stored as positive values in entries
     * So to convert them to proper splits, the amounts may have to be reverted
     * to have the proper effect on the account balance.
     * Credit notes have the opposite effect of invoices/bills, but their amounts
     * are stored as negative values as well. So to convert them into splits
     * they can be treated exactly the same as their invoice/bill counter parts.
     * The net effect is that the owner type is sufficient to determine whether a
     * value has to be reverted when converting an invoice/bill/cn amount to a split.
     */
    if (gnc_commodity_equal(xaccAccountGetCommodity(acc), invoice->currency))
    {
        xaccSplitSetBaseValue (split, value,
                               invoice->currency);
    }
    else
    {
        /*need to do conversion */
        GNCPrice *price = gncInvoiceGetPrice(invoice, xaccAccountGetCommodity(acc));

        if (price == NULL)
        {
            /*This is an error, which shouldn't even be able to happen.
              We can't really do anything sensible about it, and this is
                        a user-interface free zone so we can't try asking the user
              again either, have to return NULL*/
            return FALSE;
        }
        else
        {
            gnc_numeric converted_amount;
            xaccSplitSetValue(split, value);
            converted_amount = gnc_numeric_div(value, gnc_price_get_value(price), GNC_DENOM_AUTO, GNC_HOW_RND_ROUND_HALF_UP);
            DEBUG("converting from %f to %f\n", gnc_numeric_to_double(value), gnc_numeric_to_double(converted_amount));
            xaccSplitSetAmount(split, converted_amount);
        }
    }

    return TRUE;
}

Transaction * gncInvoicePostToAccount (GncInvoice *invoice, Account *acc,
                                       Timespec *post_date, Timespec *due_date,
                                       const char * memo, gboolean accumulatesplits,
                                       gboolean autopay)
{
    Transaction *txn;
    QofBook *book;
    GNCLot *lot = NULL;
    GList *iter;
    GList *splitinfo = NULL;
    gnc_numeric total;
    gboolean is_cust_doc;
    gboolean is_cn;
    const char *name, *type;
    char *lot_title;
    Account *ccard_acct = NULL;
    const GncOwner *owner;

    if (!invoice || !acc) return NULL;

    gncInvoiceBeginEdit (invoice);
    book = qof_instance_get_book(invoice);

    /* Stabilize the Billing Terms of this invoice */
    if (invoice->terms)
        gncInvoiceSetTerms (invoice,
                            gncBillTermReturnChild (invoice->terms, TRUE));

    /* GncEntry functions need to know if the invoice/credit note is for a customer or a vendor/employee. */
    is_cust_doc = (gncInvoiceGetOwnerType (invoice) == GNC_OWNER_CUSTOMER);
    is_cn = gncInvoiceGetIsCreditNote (invoice);

    /* Figure out if we need to separate out "credit-card" items */
    owner = gncOwnerGetEndOwner (gncInvoiceGetOwner (invoice));
    if (gncInvoiceGetOwnerType (invoice) == GNC_OWNER_EMPLOYEE)
        ccard_acct = gncEmployeeGetCCard (gncOwnerGetEmployee (owner));

    /* Create a new lot for this invoice */
    lot = gnc_lot_new (book);
    gnc_lot_begin_edit (lot);

    type = gncInvoiceGetTypeString (invoice);

    /* Set the lot title */
    lot_title = g_strdup_printf ("%s %s", type, gncInvoiceGetID (invoice));
    gnc_lot_set_title (lot, lot_title);
    g_free (lot_title);

    /* Create a new transaction */
    txn = xaccMallocTransaction (book);
    xaccTransBeginEdit (txn);

    name = gncOwnerGetName (gncOwnerGetEndOwner (gncInvoiceGetOwner (invoice)));

    /* Set Transaction Description (Owner Name) , Num (invoice ID or type, based
     * on book option), Currency */
    xaccTransSetDescription (txn, name ? name : "");
    gnc_set_num_action (txn, NULL, gncInvoiceGetID (invoice), type);
    xaccTransSetCurrency (txn, invoice->currency);

    /* Entered and Posted at date */
    xaccTransSetDateEnteredSecs (txn, gnc_time (NULL));
    if (post_date)
    {
        xaccTransSetDatePostedTS (txn, post_date);
        gncInvoiceSetDatePosted (invoice, *post_date);
    }

    if (due_date)
        xaccTransSetDateDueTS (txn, due_date);

    /* Iterate through the entries; sum up everything for each account.
     * then create the appropriate splits in this txn.
     */
    total = gnc_numeric_zero();
    for (iter = gncInvoiceGetEntries(invoice); iter; iter = iter->next)
    {
        gnc_numeric value, tax;
        GList *taxes;
        GncEntry * entry = iter->data;
        Account *this_acc;

        /* Stabilize the TaxTable in this entry */
        gncEntryBeginEdit (entry);
        if (is_cust_doc)
            gncEntrySetInvTaxTable
            (entry, gncTaxTableReturnChild (gncEntryGetInvTaxTable (entry), TRUE));
        else
        {
            gncEntrySetBillTaxTable
            (entry, gncTaxTableReturnChild (gncEntryGetBillTaxTable (entry), TRUE));

            /* If this is a bill, and the entry came from an invoice originally, copy the price */
            if (gncEntryGetBillable (entry))
                gncEntrySetInvPrice (entry, gncEntryGetBillPrice (entry));
        }
        gncEntryCommitEdit (entry);

        /* Obtain the Entry's Value and TaxValues */
        value = gncEntryGetBalValue (entry, FALSE, is_cust_doc);
        tax   = gncEntryGetBalTaxValue (entry, FALSE, is_cust_doc);
        taxes = gncEntryGetBalTaxValues (entry, is_cust_doc);

        /* add the value for the account split */
        this_acc = (is_cust_doc ? gncEntryGetInvAccount (entry) :
                    gncEntryGetBillAccount (entry));
        if (this_acc)
        {
            if (gnc_numeric_check (value) == GNC_ERROR_OK)
            {
                if (accumulatesplits)
                    splitinfo = gncAccountValueAdd (splitinfo, this_acc, value);
                else if (!gncInvoicePostAddSplit (book, this_acc, txn, value,
                                                  gncEntryGetDescription (entry),
                                                  type, invoice))
                {
                    /*This is an error, which shouldn't even be able to happen.
                      We can't really do anything sensible about it, and this is
                      a user-interface free zone so we can't try asking the user
                      again either, have to return NULL*/
                    return NULL;
                }

                /* If there is a credit-card account, and this is a CCard
                 * payment type, the don't add it to the total, and instead
                 * create a split to the CC Acct with a memo of the entry
                 * description instead of the provided memo.  Note that the
                 * value reversal is the same as the post account.
                 *
                 * Note: we don't have to worry about the tax values --
                 * expense vouchers don't have them.
                 */
                if (ccard_acct && gncEntryGetBillPayment (entry) == GNC_PAYMENT_CARD)
                {
                    Split *split;

                    split = xaccMallocSplit (book);
                    xaccSplitSetMemo (split, gncEntryGetDescription (entry));
                    /* set action based on book option */
                    gnc_set_num_action (NULL, split, gncInvoiceGetID (invoice), type);
                    xaccAccountBeginEdit (ccard_acct);
                    xaccAccountInsertSplit (ccard_acct, split);
                    xaccAccountCommitEdit (ccard_acct);
                    xaccTransAppendSplit (txn, split);
                    xaccSplitSetBaseValue (split, gnc_numeric_neg (value),
                                           invoice->currency);

                }
                else
                    total = gnc_numeric_add (total, value, GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD);

            }
            else
                g_warning ("bad value in our entry");
        }

        /* now merge in the TaxValues */
        splitinfo = gncAccountValueAddList (splitinfo, taxes);

        /* ... and add the tax total */
        if (gnc_numeric_check (tax) == GNC_ERROR_OK)
            total = gnc_numeric_add (total, tax, GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD);
        else
            g_warning ("bad tax in our entry");

        gncAccountValueDestroy (taxes);
    } /* for */

    /* Iterate through the splitinfo list and generate the splits */
    for (iter = splitinfo; iter; iter = iter->next)
    {
        GncAccountValue *acc_val = iter->data;
        if (!gncInvoicePostAddSplit (book, acc_val->account, txn, acc_val->value,
                                     memo, type, invoice))
        {
            /*This is an error, which shouldn't even be able to happen.
              We can't really do anything sensible about it, and this is
              a user-interface free zone so we can't try asking the user
              again either, have to return NULL*/
            return NULL;
        }
    }

    /* If there is a ccard account, we may have an additional "to_card" payment.
     * we should make that now.
     */
    if (ccard_acct && !gnc_numeric_zero_p (invoice->to_charge_amount))
    {
        Split *split = xaccMallocSplit (book);

        /* To charge amount is stored in document value. We need balance value here
         * so convert if necessary. */
        gnc_numeric to_charge_bal_amount = (is_cn ? gnc_numeric_neg (invoice->to_charge_amount)
                                            : invoice->to_charge_amount);

        /* Set memo. */
        xaccSplitSetMemo (split, _("Extra to Charge Card"));
        /* Set action based on book option */
        gnc_set_num_action (NULL, split, gncInvoiceGetID (invoice), type);

        xaccAccountBeginEdit (ccard_acct);
        xaccAccountInsertSplit (ccard_acct, split);
        xaccAccountCommitEdit (ccard_acct);
        xaccTransAppendSplit (txn, split);
        xaccSplitSetBaseValue (split, gnc_numeric_neg (to_charge_bal_amount),
                               invoice->currency);

        total = gnc_numeric_sub (total, to_charge_bal_amount,
                                 GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD);
    }

    /* Now create the Posted split (which is the opposite sign of the above splits) */
    {
        Split *split = xaccMallocSplit (book);

        /* Set memo */
        xaccSplitSetMemo (split, memo);
        /* Set action based on book option */
        gnc_set_num_action (NULL, split, gncInvoiceGetID (invoice), type);

        xaccAccountBeginEdit (acc);
        xaccAccountInsertSplit (acc, split);
        xaccAccountCommitEdit (acc);
        xaccTransAppendSplit (txn, split);
        xaccSplitSetBaseValue (split, gnc_numeric_neg (total),
                               invoice->currency);

        /* add this split to the lot */
        gnc_lot_add_split (lot, split);
    }

    /* Now attach this invoice to the txn, lot, and account */
    gncInvoiceAttachToLot (invoice, lot);
    gncInvoiceAttachToTxn (invoice, txn);
    gncInvoiceSetPostedAcc (invoice, acc);

    xaccTransSetReadOnly (txn, _("Generated from an invoice. Try unposting the invoice."));
    xaccTransCommitEdit (txn);

    gncAccountValueDestroy (splitinfo);

    gnc_lot_commit_edit (lot);
    /* Not strictly necessary, since it was done by the Set calls
     * above, but good insurance. */
    DEBUG("Committing Invoice %s", invoice->id);
    mark_invoice (invoice);
    gncInvoiceCommitEdit (invoice);

    /* If requested, attempt to automatically apply open payments
     * and reverse documents to this lot to close it (or at least
     * reduce its balance) */
    if (autopay)
        gncInvoiceAutoApplyPayments (invoice);

    return txn;
}

gboolean
gncInvoiceUnpost (GncInvoice *invoice, gboolean reset_tax_tables)
{
    Transaction *txn;
    GNCLot *lot;
    GList *lot_split_list, *lot_split_iter;

    if (!invoice) return FALSE;
    if (!gncInvoiceIsPosted (invoice)) return FALSE;

    txn = gncInvoiceGetPostedTxn (invoice);
    g_return_val_if_fail (txn, FALSE);

    lot = gncInvoiceGetPostedLot (invoice);
    g_return_val_if_fail (lot, FALSE);

    /* Destroy the Posted Transaction */
    xaccTransClearReadOnly (txn);
    xaccTransBeginEdit (txn);
    xaccTransDestroy (txn);
    xaccTransCommitEdit (txn);

    /* Disconnect the lot from the invoice; re-attach to the invoice owner */
    gncInvoiceDetachFromLot (lot);
    gncOwnerAttachToLot (&invoice->owner, lot);

    /* Check if this invoice was linked to other lots (payments/inverse signed
     * invoices).
     * If this is the case, recreate the link transaction between all the remaining lots.
     *
     * Note that before GnuCash 2.6 payments were not stored in separate lots, but
     * always ended up in invoice lots when matched to an invoice. Over-payments
     * were copied to a new lot, to which later an invoice was added again and so on.
     * These over-payments were handled with automatic payment forward transactions.
     * You could consider these transactions to be links between lots as well, but
     * to avoid some unexpected behavior, these will not be altered here.
     */

    // Note: make a copy of the lot list here, when splits are deleted from the lot,
    //       the original list may be destroyed by the lot code.
    lot_split_list = g_list_copy (gnc_lot_get_split_list (lot));
    for (lot_split_iter = lot_split_list; lot_split_iter; lot_split_iter = lot_split_iter->next)
    {
        Split *split = lot_split_iter->data;
        GList *other_split_list, *list_iter;
        Transaction *other_txn = xaccSplitGetParent (split);
        GList *lot_list = NULL;

        /* Only work with transactions that link invoices and payments.
         * Note: this check also catches the possible case of NULL splits. */
        if (xaccTransGetTxnType (other_txn) != TXN_TYPE_LINK)
            continue;

        /* Save a list of lots this linking transaction linked to */
        other_split_list = xaccTransGetSplitList (other_txn);
        for (list_iter = other_split_list; list_iter; list_iter = list_iter->next)
        {
            Split *other_split = list_iter->data;
            GNCLot *other_lot = xaccSplitGetLot (other_split);

            /* Omit the lot we are about to delete */
            if (other_lot == lot)
                continue;

            lot_list = g_list_prepend (lot_list, other_lot);
        }
        /* Maintain original split order */
        lot_list = g_list_reverse (lot_list);

        /* Now remove this link transaction. */
        xaccTransClearReadOnly (other_txn);
        xaccTransBeginEdit (other_txn);
        xaccTransDestroy (other_txn);
        xaccTransCommitEdit (other_txn);

        /* Re-balance the saved lots as well as is possible */
        gncOwnerAutoApplyPaymentsWithLots (&invoice->owner, lot_list);

        /* If any of the saved lots has no more splits, then destroy it.
         * Otherwise if any has an invoice associated with it,
         * send it a modified event to reset its paid status */
        for (list_iter = lot_list; list_iter; list_iter = list_iter->next)
        {
            GNCLot *other_lot = list_iter->data;
            GncInvoice *other_invoice = gncInvoiceGetInvoiceFromLot (other_lot);

            if (!gnc_lot_count_splits (other_lot))
                gnc_lot_destroy (other_lot);
            else if (other_invoice)
                qof_event_gen (QOF_INSTANCE(other_invoice), QOF_EVENT_MODIFY, NULL);
        }
    }
    g_list_free (lot_split_list);

    /* If the lot has no splits, then destroy it */
    if (!gnc_lot_count_splits (lot))
        gnc_lot_destroy (lot);

    /* Clear out the invoice posted information */
    gncInvoiceBeginEdit (invoice);

    invoice->posted_acc = NULL;
    invoice->posted_txn = NULL;
    invoice->posted_lot = NULL;
    invoice->date_posted.tv_sec = invoice->date_posted.tv_nsec = 0;

    /* if we've been asked to reset the tax tables, then do so */
    if (reset_tax_tables)
    {
        gboolean is_cust_doc = (gncInvoiceGetOwnerType(invoice) == GNC_OWNER_CUSTOMER);
        GList *iter;

        for (iter = gncInvoiceGetEntries(invoice); iter; iter = iter->next)
        {
            GncEntry *entry = iter->data;

            gncEntryBeginEdit(entry);
            if (is_cust_doc)
                gncEntrySetInvTaxTable(entry,
                                       gncTaxTableGetParent(gncEntryGetInvTaxTable(entry)));
            else
                gncEntrySetBillTaxTable(entry,
                                        gncTaxTableGetParent(gncEntryGetBillTaxTable(entry)));
            gncEntryCommitEdit(entry);
        }
    }

    mark_invoice (invoice);
    gncInvoiceCommitEdit (invoice);

    return TRUE;
}

struct lotmatch
{
    const GncOwner *owner;
    gboolean positive_balance;
};

static gboolean
gnc_lot_match_owner_balancing (GNCLot *lot, gpointer user_data)
{
    struct lotmatch *lm = user_data;
    GncOwner owner_def;
    const GncOwner *owner;
    gnc_numeric balance = gnc_lot_get_balance (lot);

    /* Could (part of) this lot serve to balance the lot
     * for which this query was run ?*/
    if (lm->positive_balance == gnc_numeric_positive_p (balance))
        return FALSE;

    /* Is it ours? Either the lot owner or the lot invoice owner should match */
    if (!gncOwnerGetOwnerFromLot (lot, &owner_def))
    {
        const GncInvoice *invoice = gncInvoiceGetInvoiceFromLot (lot);
        if (!invoice)
            return FALSE;
        owner = gncOwnerGetEndOwner (gncInvoiceGetOwner (invoice));
    }
    else
        owner = gncOwnerGetEndOwner (&owner_def);

    return gncOwnerEqual (owner, lm->owner);
}

void gncInvoiceAutoApplyPayments (GncInvoice *invoice)
{
    GNCLot *inv_lot;
    Account *acct;
    const GncOwner *owner;
    GList *lot_list;
    struct lotmatch lm;

    /* General note: "paying" in this context means balancing
     * a lot, by linking opposite signed lots together. So below the term
     * "payment" can both mean a true payment or it can mean a document of
     * the opposite sign (invoice vs credit note). It just
     * depends on what type of document was given as parameter
     * to this function. */

    /* Payments can only be applied to posted invoices */
    g_return_if_fail (invoice);
    g_return_if_fail (invoice->posted_lot);

    inv_lot = invoice->posted_lot;
    acct = invoice->posted_acc;
    owner = gncOwnerGetEndOwner (gncInvoiceGetOwner (invoice));

    /* Find all lots whose balance (or part of their balance) could be
     * used to close this lot.
     * To be eligible, the lots have to have an opposite signed balance
     * and be for the same owner.
     * For example, for an invoice lot, payment lots and credit note lots
     * could be used. */
    lm.positive_balance =  gnc_numeric_positive_p (gnc_lot_get_balance (inv_lot));
    lm.owner = owner;
    lot_list = xaccAccountFindOpenLots (acct, gnc_lot_match_owner_balancing,
                                        &lm, NULL);

    lot_list = g_list_prepend (lot_list, inv_lot);
    gncOwnerAutoApplyPaymentsWithLots (owner, lot_list);
    g_list_free (lot_list);
}

/*
 * Create a payment of "amount" for the invoice owner and attempt
 * to balance it with the given invoice.
 */
void
gncInvoiceApplyPayment (const GncInvoice *invoice, Transaction *txn,
                        Account *xfer_acc, gnc_numeric amount,
                        gnc_numeric exch, Timespec date,
                        const char *memo, const char *num)
{
    GNCLot *payment_lot;
    GList *selected_lots = NULL;
    const GncOwner *owner;

    /* Verify our arguments */
    if (!invoice || !gncInvoiceIsPosted (invoice) || !xfer_acc) return;

    owner = gncOwnerGetEndOwner (gncInvoiceGetOwner (invoice));
    g_return_if_fail (owner->owner.undefined);

    /* Create a lot for this payment */
    payment_lot = gncOwnerCreatePaymentLot (owner, txn, invoice->posted_acc, xfer_acc,
                                            amount, exch, date, memo, num);

    /* Select the invoice as only payment candidate */
    selected_lots = g_list_prepend (selected_lots, invoice->posted_lot);

    /* And link the invoice lot and the payment lot together as well as possible. */
    if (payment_lot)
        selected_lots = g_list_prepend (selected_lots, payment_lot);
    gncOwnerAutoApplyPaymentsWithLots (owner, selected_lots);
}

static gboolean gncInvoiceDateExists (const Timespec *date)
{
    g_return_val_if_fail (date, FALSE);
    if (date->tv_sec || date->tv_nsec) return TRUE;
    return FALSE;
}

gboolean gncInvoiceIsPosted (const GncInvoice *invoice)
{
    if (!invoice) return FALSE;
    return gncInvoiceDateExists (&(invoice->date_posted));
}

gboolean gncInvoiceIsPaid (const GncInvoice *invoice)
{
    if (!invoice) return FALSE;
    if (!invoice->posted_lot) return FALSE;
    return gnc_lot_is_closed(invoice->posted_lot);
}

/* ================================================================== */

void gncInvoiceBeginEdit (GncInvoice *invoice)
{
    qof_begin_edit(&invoice->inst);
}

static void gncInvoiceOnError (QofInstance *inst, QofBackendError errcode)
{
    PERR("Invoice QofBackend Failure: %d", errcode);
    gnc_engine_signal_commit_error( errcode );
}

static void gncInvoiceOnDone (QofInstance *invoice) { }

static void invoice_free (QofInstance *inst)
{
    GncInvoice *invoice = (GncInvoice *) inst;
    gncInvoiceFree (invoice);
}

void gncInvoiceCommitEdit (GncInvoice *invoice)
{
    if (!qof_commit_edit (QOF_INSTANCE(invoice))) return;
    qof_commit_edit_part2 (&invoice->inst, gncInvoiceOnError,
                           gncInvoiceOnDone, invoice_free);
}

int gncInvoiceCompare (const GncInvoice *a, const GncInvoice *b)
{
    int compare;

    if (a == b) return 0;
    if (!a) return -1;
    if (!b) return 1;

    compare = g_strcmp0 (a->id, b->id);
    if (compare) return compare;

    compare = timespec_cmp (&(a->date_opened), &(b->date_opened));
    if (compare) return compare;

    compare = timespec_cmp (&(a->date_posted), &(b->date_posted));
    if (compare) return compare;

    return qof_instance_guid_compare(a, b);
}

gboolean gncInvoiceEqual(const GncInvoice *a, const GncInvoice *b)
{
    if (a == NULL && b == NULL) return TRUE;
    if (a == NULL || b == NULL) return FALSE;

    g_return_val_if_fail(GNC_IS_INVOICE(a), FALSE);
    g_return_val_if_fail(GNC_IS_INVOICE(b), FALSE);

    if (g_strcmp0(a->id, b->id) != 0)
    {
        PWARN("IDs differ: %s vs %s", a->id, b->id);
        return FALSE;
    }

    if (g_strcmp0(a->notes, b->notes) != 0)
    {
        PWARN("Notes differ: %s vs %s", a->notes, b->notes);
        return FALSE;
    }

    if (g_strcmp0(a->billing_id, b->billing_id) != 0)
    {
        PWARN("Billing IDs differ: %s vs %s", a->billing_id, b->billing_id);
        return FALSE;
    }

    if (g_strcmp0(a->printname, b->printname) != 0)
    {
        PWARN("Printnames differ: %s vs %s", a->printname, b->printname);
        return FALSE;
    }

    if (a->active != b->active)
    {
        PWARN("Active flags differ");
        return FALSE;
    }

    if (!gncBillTermEqual(a->terms, b->terms))
    {
        PWARN("Billterms differ");
        return FALSE;
    }

    if (!gncJobEqual(a->job, b->job))
    {
        PWARN("Jobs differ");
        return FALSE;
    }

    if (!gnc_commodity_equal(a->currency, b->currency))
    {
        PWARN("Currencies differ");
        return FALSE;
    }

    if (!xaccAccountEqual(a->posted_acc, b->posted_acc, TRUE))
    {
        PWARN("Posted accounts differ");
        return FALSE;
    }

    if (!xaccTransEqual(a->posted_txn, b->posted_txn, TRUE, TRUE, TRUE, FALSE))
    {
        PWARN("Posted tx differ");
        return FALSE;
    }

#if 0
    if (!gncLotEqual(a->posted_lot, b->posted_lot))
    {
        PWARN("Posted lots differ");
        return FALSE;
    }
#endif

    /* FIXME: Need real checks */
#if 0
    GList       *entries;
    GList       *prices;
    GncOwner    owner;
    GncOwner    billto;
    Timespec    date_opened;
    Timespec    date_posted;

    gnc_numeric	to_charge_amount;
#endif

    return TRUE;
}

/* ============================================================= */
/* Package-Private functions */

static const char * _gncInvoicePrintable (gpointer obj)
{
    GncInvoice *invoice = obj;

    g_return_val_if_fail (invoice, NULL);

    if (qof_instance_get_dirty_flag(invoice) || invoice->printname == NULL)
    {
        if (invoice->printname) g_free (invoice->printname);

        invoice->printname =
            g_strdup_printf ("%s%s", invoice->id,
                             gncInvoiceIsPosted (invoice) ? _(" (posted)") : "");
    }

    return invoice->printname;
}

static void
destroy_invoice_on_book_close(QofInstance *ent, gpointer data)
{
    GncInvoice* invoice = GNC_INVOICE(ent);

    gncInvoiceBeginEdit(invoice);
    gncInvoiceDestroy(invoice);
}

static void
gnc_invoice_book_end(QofBook* book)
{
    QofCollection *col;

    col = qof_book_get_collection(book, GNC_ID_INVOICE);
    qof_collection_foreach(col, destroy_invoice_on_book_close, NULL);
}

static QofObject gncInvoiceDesc =
{
    DI(.interface_version = ) QOF_OBJECT_VERSION,
    DI(.e_type            = ) _GNC_MOD_NAME,
    DI(.type_label        = ) "Invoice",
    DI(.create            = ) (gpointer)gncInvoiceCreate,
    DI(.book_begin        = ) NULL,
    DI(.book_end          = ) gnc_invoice_book_end,
    DI(.is_dirty          = ) qof_collection_is_dirty,
    DI(.mark_clean        = ) qof_collection_mark_clean,
    DI(.foreach           = ) qof_collection_foreach,
    DI(.printable         = ) _gncInvoicePrintable,
    DI(.version_cmp       = ) (int (*)(gpointer, gpointer)) qof_instance_version_cmp,
};

static void
reg_lot (void)
{
    static QofParam params[] =
    {
        {
            INVOICE_FROM_LOT, _GNC_MOD_NAME,
            (QofAccessFunc)gncInvoiceGetInvoiceFromLot, NULL
        },
        { NULL },
    };

    qof_class_register (GNC_ID_LOT, NULL, params);
}

static void
reg_txn (void)
{
    static QofParam params[] =
    {
        {
            INVOICE_FROM_TXN, _GNC_MOD_NAME,
            (QofAccessFunc)gncInvoiceGetInvoiceFromTxn, NULL
        },
        { NULL },
    };

    qof_class_register (GNC_ID_TRANS, NULL, params);
}

gboolean gncInvoiceRegister (void)
{
    static QofParam params[] =
    {
        { INVOICE_ID,        QOF_TYPE_STRING,  (QofAccessFunc)gncInvoiceGetID,     (QofSetterFunc)gncInvoiceSetID },
        { INVOICE_OWNER,     GNC_ID_OWNER,     (QofAccessFunc)gncInvoiceGetOwner, NULL },
        { INVOICE_OPENED,    QOF_TYPE_DATE,    (QofAccessFunc)gncInvoiceGetDateOpened, (QofSetterFunc)gncInvoiceSetDateOpened },
        { INVOICE_DUE,       QOF_TYPE_DATE,    (QofAccessFunc)gncInvoiceGetDateDue, NULL },
        { INVOICE_POSTED,    QOF_TYPE_DATE,    (QofAccessFunc)gncInvoiceGetDatePosted, (QofSetterFunc)gncInvoiceSetDatePosted },
        { INVOICE_IS_POSTED, QOF_TYPE_BOOLEAN, (QofAccessFunc)gncInvoiceIsPosted, NULL },
        { INVOICE_IS_PAID,   QOF_TYPE_BOOLEAN, (QofAccessFunc)gncInvoiceIsPaid,    NULL },
        { INVOICE_BILLINGID, QOF_TYPE_STRING,  (QofAccessFunc)gncInvoiceGetBillingID, (QofSetterFunc)gncInvoiceSetBillingID },
        { INVOICE_NOTES,     QOF_TYPE_STRING,  (QofAccessFunc)gncInvoiceGetNotes,   (QofSetterFunc)gncInvoiceSetNotes },
        { INVOICE_ACC,       GNC_ID_ACCOUNT,   (QofAccessFunc)gncInvoiceGetPostedAcc, (QofSetterFunc)gncInvoiceSetPostedAcc },
        { INVOICE_POST_TXN,  GNC_ID_TRANS,     (QofAccessFunc)gncInvoiceGetPostedTxn, (QofSetterFunc)gncInvoiceSetPostedTxn },
        { INVOICE_POST_LOT,  GNC_ID_LOT,       (QofAccessFunc)gncInvoiceGetPostedLot, NULL/*(QofSetterFunc)gncInvoiceSetPostedLot*/ },
        { INVOICE_TYPE,      QOF_TYPE_INT32,   (QofAccessFunc)gncInvoiceGetType,    NULL },
        { INVOICE_TYPE_STRING, QOF_TYPE_STRING, (QofAccessFunc)gncInvoiceGetTypeString,    NULL },
        { INVOICE_TERMS,     GNC_ID_BILLTERM,  (QofAccessFunc)gncInvoiceGetTerms,   (QofSetterFunc)gncInvoiceSetTerms },
        { INVOICE_BILLTO,    GNC_ID_OWNER,     (QofAccessFunc)gncInvoiceGetBillTo, NULL  },
        { INVOICE_ENTRIES,   QOF_TYPE_COLLECT, (QofAccessFunc)qofInvoiceGetEntries, (QofSetterFunc)qofInvoiceSetEntries },
        { INVOICE_JOB,       GNC_ID_JOB,       (QofAccessFunc)qofInvoiceGetJob,     (QofSetterFunc)qofInvoiceSetJob },
        { QOF_PARAM_ACTIVE,  QOF_TYPE_BOOLEAN, (QofAccessFunc)gncInvoiceGetActive, (QofSetterFunc)gncInvoiceSetActive },
        { INVOICE_IS_CN,     QOF_TYPE_BOOLEAN, (QofAccessFunc)gncInvoiceGetIsCreditNote, (QofSetterFunc)gncInvoiceSetIsCreditNote },
        { QOF_PARAM_BOOK,    QOF_ID_BOOK,      (QofAccessFunc)qof_instance_get_book, NULL },
        { QOF_PARAM_GUID,    QOF_TYPE_GUID,    (QofAccessFunc)qof_instance_get_guid, NULL },
        { NULL },
    };

    qof_class_register (_GNC_MOD_NAME, (QofSortFunc)gncInvoiceCompare, params);
    reg_lot ();
    reg_txn ();

    /* Make the compiler happy... */
    if (0)
    {
        qofInvoiceSetEntries(NULL, NULL);
        qofInvoiceGetEntries(NULL);
        qofInvoiceSetOwner(NULL, NULL);
        qofInvoiceGetOwner(NULL);
        qofInvoiceSetBillTo(NULL, NULL);
        qofInvoiceGetBillTo(NULL);
    }
    if (!qof_choice_create(GNC_ID_INVOICE))
    {
        return FALSE;
    }
    return qof_object_register (&gncInvoiceDesc);
}

gchar *gncInvoiceNextID (QofBook *book, const GncOwner *owner)
{
    gchar *nextID;
    switch (gncOwnerGetType(gncOwnerGetEndOwner(owner)))
    {
    case GNC_OWNER_CUSTOMER:
        nextID = qof_book_increment_and_format_counter (book, "gncInvoice");
        break;
    case GNC_OWNER_VENDOR:
        nextID = qof_book_increment_and_format_counter (book, "gncBill");
        break;
    case GNC_OWNER_EMPLOYEE:
        nextID = qof_book_increment_and_format_counter (book, "gncExpVoucher");
        break;
    default:
        nextID = qof_book_increment_and_format_counter (book, _GNC_MOD_NAME);
        break;
    }
    return nextID;
}
