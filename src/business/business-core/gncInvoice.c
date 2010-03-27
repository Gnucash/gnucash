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

#include "config.h"

#include <glib.h>
#include <glib/gi18n.h>

#include "Transaction.h"
#include "Account.h"
#include "gncBillTermP.h"
#include "gncEntry.h"
#include "gncEntryP.h"
#include "gncJobP.h"
#include "gncInvoice.h"
#include "gncInvoiceP.h"
#include "gncOwnerP.h"

struct _gncInvoice
{
    QofInstance inst;

    char        *id;
    char        *notes;
    gboolean    active;

    char        *billing_id;
    char        *printname;
    GncBillTerm *terms;
    GList       *entries;
    GList       *prices;
    GncOwner    owner;
    GncOwner    billto;
    GncJob      *job;
    Timespec    date_opened;
    Timespec    date_posted;

    gnc_numeric	to_charge_amount;

    gnc_commodity * currency;

    Account     *posted_acc;
    Transaction *posted_txn;
    GNCLot      *posted_lot;
};

struct _gncInvoiceClass
{
    QofInstanceClass parent_class;
};

static QofLogModule log_module = GNC_MOD_BUSINESS;

#define _GNC_MOD_NAME	GNC_ID_INVOICE

#define GNC_INVOICE_ID		"gncInvoice"
#define GNC_INVOICE_GUID	"invoice-guid"

#define SET_STR(obj, member, str) { \
	char * tmp; \
	\
	if (!safe_strcmp (member, str)) return; \
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
    PROP_NOTES
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
    else {
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

GncInvoice *
gncCloneInvoice (GncInvoice *from, QofBook *book)
{
    GList *node;
    GncInvoice *invoice;

    if (!book) return NULL;

    invoice = g_object_new (GNC_TYPE_INVOICE, NULL);
    qof_instance_init_data (&invoice->inst, _GNC_MOD_NAME, book);

    invoice->id = CACHE_INSERT (from->id);
    invoice->notes = CACHE_INSERT (from->notes);
    invoice->billing_id = CACHE_INSERT (from->billing_id);
    invoice->active = from->active;

    invoice->billto = gncCloneOwner (&from->billto, book);
    invoice->owner = gncCloneOwner (&from->owner, book);
    invoice->job = (GncJob*)gncJobObtainTwin (from->job, book);
    invoice->terms = gncBillTermObtainTwin (from->terms, book);
    gncBillTermIncRef (invoice->terms);


    invoice->to_charge_amount = from->to_charge_amount;
    invoice->printname = NULL; /* that's right, NULL. See below. */
    invoice->date_opened = from->date_opened;
    invoice->date_posted = from->date_posted;

    invoice->currency = gnc_commodity_obtain_twin (from->currency, book);

    invoice->entries = NULL;
    for (node = g_list_last(from->entries); node; node = node->next)
    {
        GncEntry *entry = node->data;
        entry = gncEntryObtainTwin (entry, book);
        invoice->entries = g_list_prepend (invoice->entries, entry);
    }

    invoice->prices = NULL;
    for (node = g_list_last(from->prices); node; node = node->next)
    {
        GNCPrice *price = node->data;
        price = gnc_price_clone(price, book);
        invoice->prices = g_list_prepend (invoice->prices, price);
    }

    /* XXX should probably be obtain-twin not lookup-twin */
    invoice->posted_acc =
        GNC_ACCOUNT(qof_instance_lookup_twin(QOF_INSTANCE(from->posted_acc), book));
#if 0
    XXX not done * /
    Transaction * posted_txn;
    GNCLot *	posted_lot;
#endif

    qof_event_gen (&invoice->inst, QOF_EVENT_CREATE, NULL);

    return invoice;
}

GncInvoice *
gncInvoiceObtainTwin (GncInvoice *from, QofBook *book)
{
    GncInvoice *invoice;
    if (!book) return NULL;

    invoice = (GncInvoice *) qof_instance_lookup_twin (QOF_INSTANCE(from), book);
    if (!invoice)
    {
        invoice = gncCloneInvoice (from, book);
    }

    return invoice;
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

    if (!invoice || !entry) return;

    old = gncEntryGetInvoice (entry);
    if (old == invoice) return;	/* I already own this one */
    if (old) gncInvoiceRemoveEntry (old, entry);

    gncEntrySetInvoice (entry, invoice);
    invoice->entries = g_list_insert_sorted (invoice->entries, entry,
                       (GCompareFunc)gncEntryCompare);
    mark_invoice (invoice);
}

void gncInvoiceRemoveEntry (GncInvoice *invoice, GncEntry *entry)
{
    if (!invoice || !entry) return;

    gncEntrySetInvoice (entry, NULL);
    invoice->entries = g_list_remove (invoice->entries, entry);
    mark_invoice (invoice);
}

void gncInvoiceAddPrice (GncInvoice *invoice, GNCPrice *price)
{
    if (!invoice || !price) return;

    invoice->prices = g_list_prepend(invoice->prices, price);
    mark_invoice (invoice);
}

void gncInvoiceRemovePrice (GncInvoice *invoice, GNCPrice *price)
{
    if (!invoice || !price) return;

    invoice->prices = g_list_remove (invoice->prices, price);
    mark_invoice (invoice);
}

void gncBillAddEntry (GncInvoice *bill, GncEntry *entry)
{
    GncInvoice *old;

    if (!bill || !entry) return;

    old = gncEntryGetBill (entry);
    if (old == bill) return;	/* I already own this one */
    if (old) gncBillRemoveEntry (old, entry);

    gncEntrySetBill (entry, bill);
    bill->entries = g_list_insert_sorted (bill->entries, entry,
                                          (GCompareFunc)gncEntryCompare);
    mark_invoice (bill);
}

void gncBillRemoveEntry (GncInvoice *bill, GncEntry *entry)
{
    if (!bill || !entry) return;

    gncEntrySetBill (entry, NULL);
    bill->entries = g_list_remove (bill->entries, entry);
    mark_invoice (bill);
}

void gncBillAddPrice (GncInvoice *bill, GNCPrice *price)
{
    if (!bill || !price) return;

    bill->prices = g_list_prepend(bill->prices, price);
    mark_invoice (bill);
}

void gncBillRemovePrice (GncInvoice *bill, GNCPrice *price)
{
    if (!bill || !price) return;

    bill->prices = g_list_remove (bill->prices, price);
    mark_invoice (bill);
}


void gncInvoiceSortEntries (GncInvoice *invoice)
{
    if (!invoice) return;
    invoice->entries = g_list_sort(invoice->entries,
                                   (GCompareFunc)gncEntryCompare);
    mark_invoice(invoice);
}

/* ================================================================== */
/* Get Functions */

const char * gncInvoiceGetID (const GncInvoice *invoice)
{
    if (!invoice) return NULL;
    return invoice->id;
}

GncOwner * gncInvoiceGetOwner (GncInvoice *invoice)
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

GncOwnerType gncInvoiceGetOwnerType (GncInvoice *invoice)
{
    GncOwner *owner;
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
    gboolean reverse;

    g_return_val_if_fail (invoice, total);

    reverse = (gncInvoiceGetOwnerType (invoice) == GNC_OWNER_CUSTOMER);

    for (node = gncInvoiceGetEntries(invoice); node; node = node->next)
    {
        GncEntry *entry = node->data;
        gnc_numeric value, tax;

        if (use_payment_type && gncEntryGetBillPayment (entry) != type)
            continue;

        gncEntryGetValue (entry, reverse, &value, NULL, &tax, NULL);

        if (gnc_numeric_check (value) == GNC_ERROR_OK)
        {
            if (use_value)
                total = gnc_numeric_add (total, value, GNC_DENOM_AUTO, GNC_DENOM_LCD);
        }
        else
            g_warning ("bad value in our entry");

        if (gnc_numeric_check (tax) == GNC_ERROR_OK)
        {
            if (use_tax)
                total = gnc_numeric_add (total, tax, GNC_DENOM_AUTO, GNC_DENOM_LCD);
        }
        else
            g_warning ("bad tax-value in our entry");
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

const char * gncInvoiceGetType (GncInvoice *invoice)
{
    if (!invoice) return NULL;

    switch (gncInvoiceGetOwnerType (invoice))
    {
    case GNC_OWNER_CUSTOMER:
        return _("Invoice");
    case GNC_OWNER_VENDOR:
        return _("Bill");
    case GNC_OWNER_EMPLOYEE:
        return _("Expense");
    default:
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

GList * gncInvoiceGetPrices(GncInvoice *invoice)
{
    if (!invoice) return NULL;
    return invoice->prices;
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
    if (0 == safe_strcmp(qof_collection_get_type(entry_coll), GNC_ID_ENTRY))
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
    KvpFrame *kvp;

    if (!lot) return;

    kvp = gnc_lot_get_slots (lot);
    kvp_frame_set_slot_path (kvp, NULL, GNC_INVOICE_ID, GNC_INVOICE_GUID, NULL);
}

static void
gncInvoiceAttachToLot (GncInvoice *invoice, GNCLot *lot)
{
    KvpFrame *kvp;
    KvpValue *value;

    if (!invoice || !lot)
        return;

    if (invoice->posted_lot) return;	/* Cannot reset invoice's lot */

    kvp = gnc_lot_get_slots (lot);
    value = kvp_value_new_guid (qof_instance_get_guid (QOF_INSTANCE(invoice)));
    kvp_frame_set_slot_path (kvp, value, GNC_INVOICE_ID, GNC_INVOICE_GUID, NULL);
    kvp_value_delete (value);
    gncInvoiceSetPostedLot (invoice, lot);
}

GncInvoice * gncInvoiceGetInvoiceFromLot (GNCLot *lot)
{
    KvpFrame *kvp;
    KvpValue *value;
    GncGUID *guid;
    QofBook *book;

    if (!lot) return NULL;

    book = gnc_lot_get_book (lot);
    kvp = gnc_lot_get_slots (lot);
    value = kvp_frame_get_slot_path (kvp, GNC_INVOICE_ID, GNC_INVOICE_GUID, NULL);
    if (!value) return NULL;

    guid = kvp_value_get_guid (value);
    return gncInvoiceLookup(book, guid);
}

static void
gncInvoiceAttachToTxn (GncInvoice *invoice, Transaction *txn)
{
    KvpFrame *kvp;
    KvpValue *value;

    if (!invoice || !txn)
        return;

    if (invoice->posted_txn) return;	/* Cannot reset invoice's txn */

    xaccTransBeginEdit (txn);
    kvp = xaccTransGetSlots (txn);
    value = kvp_value_new_guid (qof_instance_get_guid(QOF_INSTANCE(invoice)));
    kvp_frame_set_slot_path (kvp, value, GNC_INVOICE_ID, GNC_INVOICE_GUID, NULL);
    kvp_value_delete (value);
    xaccTransSetTxnType (txn, TXN_TYPE_INVOICE);
    xaccTransCommitEdit (txn);
    gncInvoiceSetPostedTxn (invoice, txn);
}

GncInvoice *
gncInvoiceGetInvoiceFromTxn (const Transaction *txn)
{
    KvpFrame *kvp;
    KvpValue *value;
    GncGUID *guid;
    QofBook *book;

    if (!txn) return NULL;

    book = xaccTransGetBook (txn);
    kvp = xaccTransGetSlots (txn);
    value = kvp_frame_get_slot_path (kvp, GNC_INVOICE_ID, GNC_INVOICE_GUID, NULL);
    if (!value) return NULL;

    guid = kvp_value_get_guid (value);
    return gncInvoiceLookup(book, guid);
}

struct lotmatch
{
    GncOwner *owner;
    gboolean reverse;
};

static gboolean
gnc_lot_match_owner_payment (GNCLot *lot, gpointer user_data)
{
    struct lotmatch *lm = user_data;
    GncOwner owner_def, *owner;
    gnc_numeric balance = gnc_lot_get_balance (lot);

    /* Is this a payment lot */
    if (gnc_numeric_positive_p (lm->reverse ? balance :
                                gnc_numeric_neg (balance)))
        return FALSE;

    /* Is there an invoice attached? */
    if (gncInvoiceGetInvoiceFromLot (lot))
        return FALSE;

    /* Is it ours? */
    if (!gncOwnerGetOwnerFromLot (lot, &owner_def))
        return FALSE;
    owner = gncOwnerGetEndOwner (&owner_def);

    return gncOwnerEqual (owner, lm->owner);
}

Transaction * gncInvoicePostToAccount (GncInvoice *invoice, Account *acc,
                                       Timespec *post_date, Timespec *due_date,
                                       const char * memo, gboolean accumulatesplits)
{
    Transaction *txn;
    QofBook *book;
    GNCLot *lot = NULL;
    GList *iter;
    GList *splitinfo = NULL;
    gnc_numeric total;
    gboolean reverse;
    const char *name, *type;
    char *lot_title;
    Account *ccard_acct = NULL;
    GncOwner *owner;

    if (!invoice || !acc) return NULL;

    gncInvoiceBeginEdit (invoice);
    book = qof_instance_get_book(invoice);

    /* Stabilize the Billing Terms of this invoice */
    if (invoice->terms)
        gncInvoiceSetTerms (invoice,
                            gncBillTermReturnChild (invoice->terms, TRUE));

    /* Figure out if we need to "reverse" the numbers. */
    reverse = (gncInvoiceGetOwnerType (invoice) == GNC_OWNER_CUSTOMER);

    /* Figure out if we need to separate out "credit-card" items */
    owner = gncOwnerGetEndOwner (gncInvoiceGetOwner (invoice));
    if (gncInvoiceGetOwnerType (invoice) == GNC_OWNER_EMPLOYEE)
        ccard_acct = gncEmployeeGetCCard (gncOwnerGetEmployee (owner));

    /* Find an existing payment-lot for this owner */
    {
        LotList *lot_list;
        struct lotmatch lm;

        lm.reverse = reverse;
        lm.owner = owner;

        lot_list = xaccAccountFindOpenLots (acc, gnc_lot_match_owner_payment,
                                            &lm, NULL);
        if (lot_list)
            lot = lot_list->data;

        g_list_free (lot_list);
    }

    /* Create a new lot for this invoice, if we need to do so */
    if (!lot)
        lot = gnc_lot_new (book);
    gnc_lot_begin_edit (lot);

    type = gncInvoiceGetType (invoice);

    /* Set the lot title */
    lot_title = g_strdup_printf ("%s %s", type, gncInvoiceGetID (invoice));
    gnc_lot_set_title (lot, lot_title);
    g_free (lot_title);

    /* Create a new transaction */
    txn = xaccMallocTransaction (book);
    xaccTransBeginEdit (txn);

    name = gncOwnerGetName (gncOwnerGetEndOwner (gncInvoiceGetOwner (invoice)));

    /* Set Transaction Description (Owner Name) , Num (invoice ID), Currency */
    xaccTransSetDescription (txn, name ? name : "");
    xaccTransSetNum (txn, gncInvoiceGetID (invoice));
    xaccTransSetCurrency (txn, invoice->currency);

    /* Entered and Posted at date */
    xaccTransSetDateEnteredSecs (txn, time(NULL));
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
        if (reverse)
            gncEntrySetInvTaxTable
            (entry, gncTaxTableReturnChild (gncEntryGetInvTaxTable (entry), TRUE));
        else
        {
            gncEntrySetBillTaxTable
            (entry, gncTaxTableReturnChild (gncEntryGetBillTaxTable (entry), TRUE));

            /* If this is a bill, and the entry is billable, copy the price */
            if (gncEntryGetBillable (entry))
                gncEntrySetInvPrice (entry, gncEntryGetBillPrice (entry));
        }
        gncEntryCommitEdit (entry);

        /* Obtain the Entry's Value and TaxValues */
        gncEntryGetValue (entry, reverse, &value, NULL, &tax, &taxes);

        /* add the value for the account split */
        this_acc = (reverse ? gncEntryGetInvAccount (entry) :
                    gncEntryGetBillAccount (entry));
        if (this_acc)
        {
            if (gnc_numeric_check (value) == GNC_ERROR_OK)
            {
                if (accumulatesplits)
                {
                    splitinfo = gncAccountValueAdd (splitinfo, this_acc, value);
                }
                else
                {
                    Split *split;

                    split = xaccMallocSplit (book);
                    /* set action and memo? */

                    xaccSplitSetMemo (split, gncEntryGetDescription (entry));
                    xaccSplitSetAction (split, type);

                    /* Need to insert this split into the account AND txn before
                     * we set the Base Value.  Otherwise SetBaseValue complains
                     * that we don't have an account and fails to set the value.
                     */
                    xaccAccountBeginEdit (this_acc);
                    xaccAccountInsertSplit (this_acc, split);
                    xaccAccountCommitEdit (this_acc);
                    xaccTransAppendSplit (txn, split);

                    if (gnc_commodity_equal(xaccAccountGetCommodity(this_acc), invoice->currency))
                    {
                        xaccSplitSetBaseValue (split, (reverse ? gnc_numeric_neg (value)
                                                       : value),
                                               invoice->currency);
                    }
                    else
                    {
                        /*need to do conversion */
                        GNCPrice *price = gncInvoiceGetPrice(invoice, xaccAccountGetCommodity(this_acc));

                        if (price == NULL)
                        {
                            /*This is an error, which shouldn't even be able to happen.
                              We can't really do anything sensible about it, and this is
                            		    a user-interface free zone so we can't try asking the user
                              again either, have to return NULL*/
                            return NULL;
                        }
                        else
                        {
                            gnc_numeric converted_amount;
                            xaccSplitSetValue(split, (reverse ? gnc_numeric_neg(value) : value));
                            converted_amount = gnc_numeric_div(value, gnc_price_get_value(price), GNC_DENOM_AUTO, GNC_HOW_RND_ROUND);
                            printf("converting from %f to %f\n", gnc_numeric_to_double(value), gnc_numeric_to_double(converted_amount));
                            xaccSplitSetAmount(split, reverse ? gnc_numeric_neg(converted_amount) : converted_amount);
                        }
                    }
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
                    /* set action? */
                    xaccSplitSetMemo (split, gncEntryGetDescription (entry));
                    xaccSplitSetAction (split, type);
                    xaccAccountBeginEdit (ccard_acct);
                    xaccAccountInsertSplit (ccard_acct, split);
                    xaccAccountCommitEdit (ccard_acct);
                    xaccTransAppendSplit (txn, split);
                    xaccSplitSetBaseValue (split, (reverse ? value : gnc_numeric_neg (value)),
                                           invoice->currency);

                }
                else
                    total = gnc_numeric_add (total, value, GNC_DENOM_AUTO, GNC_DENOM_LCD);

            }
            else
                g_warning ("bad value in our entry");
        }

        /* now merge in the TaxValues */
        splitinfo = gncAccountValueAddList (splitinfo, taxes);

        /* ... and add the tax total */
        if (gnc_numeric_check (tax) == GNC_ERROR_OK)
            total = gnc_numeric_add (total, tax, GNC_DENOM_AUTO, GNC_DENOM_LCD);
        else
            g_warning ("bad tax in our entry");

    } /* for */

    /* Iterate through the splitinfo list and generate the splits */
    for (iter = splitinfo; iter; iter = iter->next)
    {
        Split *split;
        GncAccountValue *acc_val = iter->data;

        split = xaccMallocSplit (book);
        /* set action and memo? */

        xaccSplitSetMemo (split, memo);
        xaccSplitSetAction (split, type);

        xaccAccountBeginEdit (acc_val->account);
        xaccAccountInsertSplit (acc_val->account, split);
        xaccAccountCommitEdit (acc_val->account);
        xaccTransAppendSplit (txn, split);

        if (gnc_commodity_equal(xaccAccountGetCommodity(acc_val->account), invoice->currency))
        {
            xaccSplitSetBaseValue (split, (reverse ? gnc_numeric_neg (acc_val->value)
                                           : acc_val->value),
                                   invoice->currency);
        }
        else
        {
            /*need to do conversion */
            GNCPrice *price = gncInvoiceGetPrice(invoice, xaccAccountGetCommodity(acc_val->account));

            if (price == NULL)
            {
                /*This is an error, which shouldn't even be able to happen.
                  We can't really do anything sensible about it, and this is
                  a user-interface free zone so we can't try asking the user
                  again either, have to return NULL*/
                return NULL;
            }
            else
            {
                gnc_numeric converted_amount;
                xaccSplitSetValue(split, (reverse ? gnc_numeric_neg(acc_val->value) : acc_val->value));
                converted_amount = gnc_numeric_div(acc_val->value, gnc_price_get_value(price), GNC_DENOM_AUTO, GNC_HOW_RND_ROUND);
                printf("converting from %f to %f\n", gnc_numeric_to_double(acc_val->value), gnc_numeric_to_double(converted_amount));

                xaccSplitSetAmount(split, reverse ? gnc_numeric_neg(converted_amount) : converted_amount);
            }
        }
    }

    /* If there is a ccard account, we may have an additional "to_card" payment.
     * we should make that now..
     */
    if (ccard_acct && !gnc_numeric_zero_p (invoice->to_charge_amount))
    {
        Split *split = xaccMallocSplit (book);

        /* Set memo.  action? */
        xaccSplitSetMemo (split, _("Extra to Charge Card"));
        xaccSplitSetAction (split, type);

        xaccAccountBeginEdit (ccard_acct);
        xaccAccountInsertSplit (ccard_acct, split);
        xaccAccountCommitEdit (ccard_acct);
        xaccTransAppendSplit (txn, split);
        xaccSplitSetBaseValue (split, (reverse ? invoice->to_charge_amount :
                                       gnc_numeric_neg(invoice->to_charge_amount)),
                               invoice->currency);

        total = gnc_numeric_sub (total, invoice->to_charge_amount,
                                 GNC_DENOM_AUTO, GNC_DENOM_LCD);
    }

    /* Now create the Posted split (which is negative -- it's a credit) */
    {
        Split *split = xaccMallocSplit (book);

        /* Set action/memo */
        xaccSplitSetMemo (split, memo);
        xaccSplitSetAction (split, type);

        xaccAccountBeginEdit (acc);
        xaccAccountInsertSplit (acc, split);
        xaccAccountCommitEdit (acc);
        xaccTransAppendSplit (txn, split);
        xaccSplitSetBaseValue (split, (reverse ? total : gnc_numeric_neg (total)),
                               invoice->currency);

        /* add this split to the lot */
        gnc_lot_add_split (lot, split);
    }

    /* Now attach this invoice to the txn, lot, and account */
    gncInvoiceAttachToLot (invoice, lot);
    gncInvoiceAttachToTxn (invoice, txn);
    gncInvoiceSetPostedAcc (invoice, acc);

    xaccTransSetReadOnly (txn, _("Generated from an invoice.  Try unposting the invoice."));
    xaccTransCommitEdit (txn);

    gncAccountValueDestroy (splitinfo);

    /* check the lot -- if we still look like a payment lot, then that
     * means we need to create a balancing split and create a new payment
     * lot for the next invoice
     *
     * we're looking for a positive balance for bill/AP, and a negative balance
     * for invoice/AR.
     * (because bill payments debit AP accounts and invoice payments
     * credit AR accounts)
     */
    total = gnc_lot_get_balance (lot);

    if ( (gnc_numeric_negative_p (total) && reverse) ||
            (gnc_numeric_positive_p (total) && !reverse) )
    {
        Transaction *t2;
        GNCLot *lot2;
        Split *split;
        /* Translators: This is the memo of an auto-created split */
        char *memo2 = _("Automatic Payment Forward");
        char *action2 = _("Auto Split");

        t2 = xaccMallocTransaction (book);
        lot2 = gnc_lot_new (book);
        gnc_lot_begin_edit (lot2);
        gncOwnerAttachToLot (gncOwnerGetEndOwner (gncInvoiceGetOwner (invoice)),
                             lot2);

        xaccTransBeginEdit (t2);
        xaccAccountBeginEdit (acc);

        /* Set Transaction Description (Owner Name), Currency */
        xaccTransSetDescription (t2, name ? name : "");
        xaccTransSetCurrency (t2, invoice->currency);

        /* Entered and Posted at date */
        xaccTransSetDateEnteredSecs (t2, time(NULL));
        if (post_date)
            xaccTransSetDatePostedTS (t2, post_date);

        /* Balance out this lot */
        split = xaccMallocSplit (book);
        xaccSplitSetMemo (split, memo2);
        xaccSplitSetAction (split, action2);
        xaccAccountInsertSplit (acc, split);
        xaccTransAppendSplit (t2, split);
        // the value of total used here is correct for both bill/AP and
        // invoice/AR. See the comment before this if block
        xaccSplitSetBaseValue (split, gnc_numeric_neg (total),
                               invoice->currency);
        gnc_lot_add_split (lot, split);

        /* And apply the pre-payment to a new lot */
        split = xaccMallocSplit (book);
        xaccSplitSetMemo (split, memo2);
        xaccSplitSetAction (split, action2);
        xaccAccountInsertSplit (acc, split);
        xaccTransAppendSplit (t2, split);
        xaccSplitSetBaseValue (split, total, invoice->currency);
        gnc_lot_add_split (lot2, split);

        gnc_lot_commit_edit (lot2);
        xaccTransCommitEdit (t2);
        xaccAccountCommitEdit (acc);
    }

    gnc_lot_commit_edit (lot);
    gncInvoiceCommitEdit (invoice);

    return txn;
}

gboolean
gncInvoiceUnpost (GncInvoice *invoice, gboolean reset_tax_tables)
{
    Transaction *txn;
    GNCLot *lot;

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
        gboolean reverse = (gncInvoiceGetOwnerType(invoice) == GNC_OWNER_CUSTOMER);
        GList *iter;

        for (iter = gncInvoiceGetEntries(invoice); iter; iter = iter->next)
        {
            GncEntry *entry = iter->data;

            gncEntryBeginEdit(entry);
            if (reverse)
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

static gboolean
gnc_lot_match_invoice_owner (GNCLot *lot, gpointer user_data)
{
    GncOwner owner_def, *owner, *this_owner = user_data;
    GncInvoice *invoice;

    /* If this lot is not for this owner, then ignore it */
    invoice = gncInvoiceGetInvoiceFromLot (lot);
    if (invoice)
    {
        owner = gncInvoiceGetOwner (invoice);
        owner = gncOwnerGetEndOwner (owner);
    }
    else
    {
        if (!gncOwnerGetOwnerFromLot (lot, &owner_def))
            return FALSE;
        owner = gncOwnerGetEndOwner (&owner_def);
    }

    return gncOwnerEqual (owner, this_owner);
}

static gint
gnc_lot_sort_func (GNCLot *a, GNCLot *b)
{
    GncInvoice *ia, *ib;
    Timespec da, db;

    ia = gncInvoiceGetInvoiceFromLot (a);
    ib = gncInvoiceGetInvoiceFromLot (b);

    da = gncInvoiceGetDateDue (ia);
    db = gncInvoiceGetDateDue (ib);

    return timespec_cmp (&da, &db);
}

/*
 * Apply a payment of "amount" for the owner, between the xfer_account
 * (bank or other asset) and the posted_account (A/R or A/P).
 *
 * XXX: yes, this should be in gncOwner, but all the other logic is
 * in gncInvoice...
 */
Transaction *
gncOwnerApplyPayment (GncOwner *owner, GncInvoice* invoice,
                      Account *posted_acc, Account *xfer_acc,
                      gnc_numeric amount, gnc_numeric exch, Timespec date,
                      const char *memo, const char *num)
{
    QofBook *book;
    Account *inv_posted_acc;
    Transaction *txn;
    Split *split;
    GList *lot_list, *fifo = NULL;
    GNCLot *lot, *inv_posted_lot = NULL, *prepay_lot = NULL;
    GncInvoice *this_invoice;
    const char *name;
    gnc_commodity *commodity;
    gnc_numeric split_amt;
    gboolean reverse, inv_passed = TRUE;
    gnc_numeric payment_value = amount;

    /* Verify our arguments */
    if (!owner || !posted_acc || !xfer_acc) return NULL;
    g_return_val_if_fail (owner->owner.undefined != NULL, NULL);

    /* Compute the ancillary data */
    book = gnc_account_get_book (posted_acc);
    name = gncOwnerGetName (gncOwnerGetEndOwner (owner));
    commodity = gncOwnerGetCurrency (owner);
    reverse = (gncOwnerGetType (owner) == GNC_OWNER_CUSTOMER);

    txn = xaccMallocTransaction (book);
    xaccTransBeginEdit (txn);

    /* Set up the transaction */
    xaccTransSetDescription (txn, name ? name : "");
    xaccTransSetNum (txn, num);
    xaccTransSetCurrency (txn, commodity);
    xaccTransSetDateEnteredSecs (txn, time(NULL));
    xaccTransSetDatePostedTS (txn, &date);
    xaccTransSetTxnType (txn, TXN_TYPE_PAYMENT);


    /* The split for the transfer account */
    split = xaccMallocSplit (book);
    xaccSplitSetMemo (split, memo);
    xaccSplitSetAction (split, _("Payment"));
    xaccAccountBeginEdit (xfer_acc);
    xaccAccountInsertSplit (xfer_acc, split);
    xaccAccountCommitEdit (xfer_acc);
    xaccTransAppendSplit (txn, split);

    if (gnc_commodity_equal(xaccAccountGetCommodity(xfer_acc), commodity))
    {
        xaccSplitSetBaseValue (split, reverse ? amount :
                               gnc_numeric_neg (amount), commodity);
    }
    else
    {
        /* Need to value the payment in terms of the owner commodity */
        xaccSplitSetAmount(split, reverse ? amount : gnc_numeric_neg (amount));
        payment_value = gnc_numeric_mul(amount, exch, GNC_DENOM_AUTO, GNC_HOW_RND_ROUND);
        xaccSplitSetValue(split, reverse ? payment_value : gnc_numeric_neg(payment_value));
    }


    /* Now, find all "open" lots in the posting account for this
     * company and apply the payment on a FIFO basis.  Create
     * a new split for each open lot until the payment is gone.
     */

    fifo = xaccAccountFindOpenLots (posted_acc, gnc_lot_match_invoice_owner,
                                    owner,
                                    (GCompareFunc)gnc_lot_sort_func);

    /* Check if an invoice was passed in, and if so, does it match the
     * account, and is it an open lot?  If so, put it at the beginning
     * of the lot list fifo so we post to this invoice's lot first.
     */
    if (invoice)
    {
        inv_posted_acc = gncInvoiceGetPostedAcc(invoice);
        inv_posted_lot = gncInvoiceGetPostedLot(invoice);
        if (inv_posted_acc && inv_posted_lot &&
                guid_equal(xaccAccountGetGUID(inv_posted_acc),
                           xaccAccountGetGUID(posted_acc)) &&
                !gnc_lot_is_closed(inv_posted_lot))
        {
            /* Put this invoice at the beginning of the FIFO */
            fifo = g_list_prepend (fifo, inv_posted_lot);
            inv_passed = FALSE;
        }
    }

    xaccAccountBeginEdit (posted_acc);

    /* Now iterate over the fifo until the payment is fully applied
     * (or all the lots are paid)
     */
    for (lot_list = fifo; lot_list; lot_list = lot_list->next)
    {
        gnc_numeric balance;

        lot = lot_list->data;

        /* Skip this lot if it matches the invoice that was passed in and
         * we've seen it already.  This way we post to it the first time
         * (from the beginning of the lot-list) but not when we reach it
         * the second time.
         */
        if (inv_posted_lot &&
                guid_equal(qof_instance_get_guid(QOF_INSTANCE(lot)),
                           qof_instance_get_guid(QOF_INSTANCE(inv_posted_lot))))
        {
            if (inv_passed)
                continue;
            else
                inv_passed = TRUE;
        }

        balance = gnc_lot_get_balance (lot);

        if (!reverse)
            balance = gnc_numeric_neg (balance);

        /* If the balance is "negative" then skip this lot.
         * (just save the pre-payment lot for later)
         */
        if (gnc_numeric_negative_p (balance))
        {
            if (prepay_lot)
            {
                g_warning ("Multiple pre-payment lots are found.  Skipping.");
            }
            else
            {
                prepay_lot = lot;
            }
            continue;
        }

        /*
         * If the payment_value <= the balance; we're done -- apply the payment_value.
         * Otherwise, apply the balance, subtract that from the payment_value,
         * and move on to the next one.
         */
        if (gnc_numeric_compare (payment_value, balance) <= 0)
        {
            /* payment_value <= balance */
            split_amt = payment_value;
        }
        else
        {
            /* payment_value > balance */
            split_amt = balance;
        }

        /* reduce the payment_value by split_amt */
        payment_value = gnc_numeric_sub (payment_value, split_amt, GNC_DENOM_AUTO, GNC_DENOM_LCD);

        /* Create the split for this lot in the post account */
        split = xaccMallocSplit (book);
        xaccSplitSetMemo (split, memo);
        xaccSplitSetAction (split, _("Payment"));
        xaccAccountInsertSplit (posted_acc, split);
        xaccTransAppendSplit (txn, split);
        xaccSplitSetBaseValue (split, reverse ? gnc_numeric_neg (split_amt) :
                               split_amt, commodity);
        gnc_lot_add_split (lot, split);

        /* Now send an event for the invoice so it gets updated as paid */
        this_invoice = gncInvoiceGetInvoiceFromLot(lot);
        if (this_invoice)
            qof_event_gen (&this_invoice->inst, QOF_EVENT_MODIFY, NULL);

        if (gnc_numeric_zero_p (payment_value))
            break;
    }

    g_list_free (fifo);

    /* If there is still money left here, then create a pre-payment lot */
    if (gnc_numeric_positive_p (payment_value))
    {
        if (prepay_lot == NULL)
        {
            prepay_lot = gnc_lot_new (book);
            gncOwnerAttachToLot (owner, prepay_lot);
        }

        split = xaccMallocSplit (book);
        xaccSplitSetMemo (split, memo);
        xaccSplitSetAction (split, _("Pre-Payment"));
        xaccAccountInsertSplit (posted_acc, split);
        xaccTransAppendSplit (txn, split);
        xaccSplitSetBaseValue (split, reverse ? gnc_numeric_neg (payment_value) :
                               payment_value, commodity);
        gnc_lot_add_split (prepay_lot, split);
    }

    xaccAccountCommitEdit (posted_acc);

    /* Commit this new transaction */
    xaccTransCommitEdit (txn);

    return txn;
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
    if (!a && b) return -1;
    if (a && !b) return 1;

    compare = safe_strcmp (a->id, b->id);
    if (compare) return compare;

    compare = timespec_cmp (&(a->date_opened), &(b->date_opened));
    if (compare) return compare;

    compare = timespec_cmp (&(a->date_posted), &(b->date_posted));
    if (compare) return compare;

    return qof_instance_guid_compare(a, b);
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

static QofObject gncInvoiceDesc =
{
    DI(.interface_version =) QOF_OBJECT_VERSION,
    DI(.e_type            =) _GNC_MOD_NAME,
    DI(.type_label        =) "Invoice",
    DI(.create            =) (gpointer)gncInvoiceCreate,
    DI(.book_begin        =) NULL,
    DI(.book_end          =) NULL,
    DI(.is_dirty          =) qof_collection_is_dirty,
    DI(.mark_clean        =) qof_collection_mark_clean,
    DI(.foreach           =) qof_collection_foreach,
    DI(.printable         =) _gncInvoicePrintable,
    DI(.version_cmp       =) (int (*)(gpointer, gpointer)) qof_instance_version_cmp,
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
        { INVOICE_ID,      QOF_TYPE_STRING, (QofAccessFunc)gncInvoiceGetID,     (QofSetterFunc)gncInvoiceSetID },
        { INVOICE_OWNER,   GNC_ID_OWNER, (QofAccessFunc)gncInvoiceGetOwner, NULL },
        { INVOICE_OPENED,  QOF_TYPE_DATE,   (QofAccessFunc)gncInvoiceGetDateOpened, (QofSetterFunc)gncInvoiceSetDateOpened },
        { INVOICE_DUE,     QOF_TYPE_DATE,   (QofAccessFunc)gncInvoiceGetDateDue, NULL },
        { INVOICE_POSTED,  QOF_TYPE_DATE,   (QofAccessFunc)gncInvoiceGetDatePosted, (QofSetterFunc)gncInvoiceSetDatePosted },
        { INVOICE_IS_POSTED, QOF_TYPE_BOOLEAN, (QofAccessFunc)gncInvoiceIsPosted, NULL },
        { INVOICE_IS_PAID, QOF_TYPE_BOOLEAN, (QofAccessFunc)gncInvoiceIsPaid,    NULL },
        { INVOICE_BILLINGID, QOF_TYPE_STRING, (QofAccessFunc)gncInvoiceGetBillingID, (QofSetterFunc)gncInvoiceSetBillingID },
        { INVOICE_NOTES,   QOF_TYPE_STRING, (QofAccessFunc)gncInvoiceGetNotes,   (QofSetterFunc)gncInvoiceSetNotes },
        { INVOICE_ACC,     GNC_ID_ACCOUNT,  (QofAccessFunc)gncInvoiceGetPostedAcc, (QofSetterFunc)gncInvoiceSetPostedAcc },
        { INVOICE_POST_TXN, GNC_ID_TRANS,   (QofAccessFunc)gncInvoiceGetPostedTxn, (QofSetterFunc)gncInvoiceSetPostedTxn },
        { INVOICE_POST_LOT, GNC_ID_LOT,     (QofAccessFunc)gncInvoiceGetPostedLot, NULL/*(QofSetterFunc)gncInvoiceSetPostedLot*/ },
        { INVOICE_TYPE,    QOF_TYPE_STRING, (QofAccessFunc)gncInvoiceGetType,    NULL },
        { INVOICE_TERMS,   GNC_ID_BILLTERM, (QofAccessFunc)gncInvoiceGetTerms,   (QofSetterFunc)gncInvoiceSetTerms },
        { INVOICE_BILLTO,  GNC_ID_OWNER, (QofAccessFunc)gncInvoiceGetBillTo, NULL  },
        { INVOICE_ENTRIES, QOF_TYPE_COLLECT, (QofAccessFunc)qofInvoiceGetEntries, (QofSetterFunc)qofInvoiceSetEntries },
        { INVOICE_JOB,     GNC_ID_JOB,      (QofAccessFunc)qofInvoiceGetJob,     (QofSetterFunc)qofInvoiceSetJob },
        { QOF_PARAM_ACTIVE, QOF_TYPE_BOOLEAN, (QofAccessFunc)gncInvoiceGetActive, (QofSetterFunc)gncInvoiceSetActive },
        { QOF_PARAM_BOOK, QOF_ID_BOOK, (QofAccessFunc)qof_instance_get_book, NULL },
        { QOF_PARAM_GUID, QOF_TYPE_GUID, (QofAccessFunc)qof_instance_get_guid, NULL },
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

gint64 gncInvoiceNextID (QofBook *book, GncOwner *owner)
{
    gint64 nextID;
    switch (gncOwnerGetType(gncOwnerGetEndOwner(owner)))
    {
    case GNC_OWNER_CUSTOMER:
        nextID = qof_book_get_counter (book, "gncInvoice");
        break;
    case GNC_OWNER_VENDOR:
        nextID = qof_book_get_counter (book, "gncBill");
        break;
    case GNC_OWNER_EMPLOYEE:
        nextID = qof_book_get_counter (book, "gncExpVoucher");
        break;
    default:
        nextID = qof_book_get_counter (book, _GNC_MOD_NAME);
        break;
    }
    return nextID;
}
