/********************************************************************\
 * gncEntry.c -- the Core Business Entry Interface                  *
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
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#include <config.h>

#include <glib.h>
#include <qofinstance-p.h>

#include "gnc-commodity.h"

#include "gncEntry.h"
#include "gncEntryP.h"
#include "gnc-features.h"
#include "gncInvoice.h"
#include "gncOrder.h"

struct _gncEntry
{
    QofInstance inst;

    Timespec	date;
    Timespec	date_entered;
    char *	desc;
    char *	action;
    char *	notes;
    gnc_numeric 	quantity;

    /* customer invoice data */
    Account *	i_account;
    gnc_numeric 	i_price;
    gboolean	i_taxable;
    gboolean	i_taxincluded;
    GncTaxTable *	i_tax_table;
    gnc_numeric 	i_discount;
    GncAmountType	i_disc_type;
    GncDiscountHow i_disc_how;

    /* vendor bill data */
    Account *	b_account;
    gnc_numeric 	b_price;
    gboolean	b_taxable;
    gboolean	b_taxincluded;
    GncTaxTable *	b_tax_table;
    gboolean	billable;
    GncOwner	billto;

    /* employee bill data */
    GncEntryPaymentType b_payment;

    /* my parent(s) */
    GncOrder *	order;
    GncInvoice *	invoice;
    GncInvoice *	bill;

    /* CACHED VALUES */
    gboolean	values_dirty;

    /* customer invoice */
    gnc_numeric	i_value;
    gnc_numeric	i_value_rounded;
    GList *	i_tax_values;
    gnc_numeric	i_tax_value;
    gnc_numeric	i_tax_value_rounded;
    gnc_numeric	i_disc_value;
    gnc_numeric	i_disc_value_rounded;
    Timespec	i_taxtable_modtime;

    /* vendor bill */
    gnc_numeric	b_value;
    gnc_numeric	b_value_rounded;
    GList *	b_tax_values;
    gnc_numeric	b_tax_value;
    gnc_numeric	b_tax_value_rounded;
    Timespec	b_taxtable_modtime;
};

struct _gncEntryClass
{
    QofInstanceClass parent_class;
};

static QofLogModule log_module = GNC_MOD_BUSINESS;


/* You must edit the functions in this block in tandem.
 * KEEP THIS FUNCTION IN SYNC with the one below! */
const char *
gncEntryDiscountHowToString (GncDiscountHow how)
{
    switch (how)
    {
    case (GNC_DISC_PRETAX):
        return "PRETAX";
    case (GNC_DISC_SAMETIME):
        return "SAMETIME";
    case (GNC_DISC_POSTTAX):
        return "POSTTAX";
    default:
        g_warning ("asked to translate unknown discount-how %d.\n", how);
        break;
    }
    return NULL;
}

/* You must edit the functions in this block in tandem.
 * KEEP THIS FUNCTION IN SYNC with the one above! */
gboolean gncEntryDiscountStringToHow (const char *str, GncDiscountHow *how)
{
    if (g_strcmp0 ("PRETAX", str) == 0)
    {
        *how = GNC_DISC_PRETAX;
        return TRUE;
    }
    if (g_strcmp0 ("SAMETIME", str) == 0)
    {
        *how = GNC_DISC_SAMETIME;
        return TRUE;
    }
    if (g_strcmp0 ("POSTTAX", str) == 0)
    {
        *how = GNC_DISC_POSTTAX;
        return TRUE;
    }
    g_warning ("asked to translate unknown discount-how string %s.\n",
               str ? str : "(null)");

    return FALSE;
}

/* You must edit the functions in this block in tandem.
 * KEEP THIS FUNCTION IN SYNC with the one below! */
const char * gncEntryPaymentTypeToString (GncEntryPaymentType type)
{
    switch (type)
    {
    case (GNC_PAYMENT_CASH):
        return "CASH";
    case (GNC_PAYMENT_CARD):
        return "CARD";
    default:
        g_warning ("asked to translate unknown payment type %d.\n", type);
        break;
    }
    return NULL ;
}

/* You must edit the functions in this block in tandem.
 * KEEP THIS FUNCTION IN SYNC with the one above! */
gboolean gncEntryPaymentStringToType (const char *str, GncEntryPaymentType *type)
{
    if (g_strcmp0 ("CASH", str) == 0)
    {
        *type = GNC_PAYMENT_CASH;
        return TRUE;
    }
    if (g_strcmp0 ("CARD", str) == 0)
    {
        *type = GNC_PAYMENT_CARD;
        return TRUE;
    }
    g_warning ("asked to translate unknown discount-how string %s.\n",
               str ? str : "(null)");

    return FALSE;
}

#define _GNC_MOD_NAME GNC_ID_ENTRY

#define SET_STR(obj, member, str) { \
	char * tmp; \
	\
	if (!g_strcmp0 (member, str)) return; \
	gncEntryBeginEdit (obj); \
	tmp = CACHE_INSERT (str); \
	CACHE_REMOVE (member); \
	member = tmp; \
	}

G_INLINE_FUNC void mark_entry (GncEntry *entry);
void mark_entry (GncEntry *entry)
{
    qof_instance_set_dirty(&entry->inst);
    qof_event_gen (&entry->inst, QOF_EVENT_MODIFY, NULL);
}

/* ================================================================ */

enum
{
    PROP_0,
//  PROP_DATE,		/* Table */
//  PROP_DATE_ENTERED,	/* Table */
    PROP_DESCRIPTION,	/* Table */
//  PROP_ACTION,	/* Table */
//  PROP_NOTES,		/* Table */
//  PROP_QUANTITY,	/* Table (numeric) */
//  PROP_I_ACCT,	/* Table */
//  PROP_I_PRICE,	/* Table (numeric) */
//  PROP_I_DISCOUNT,	/* Table (numeric) */
//  PROP_INVOICE,	/* Table */
//  PROP_I_DISC_TYPE,	/* Table */
//  PROP_I_DISC_HOW,	/* Table */
//  PROP_I_TAXABLE,	/* Table */
//  PROP_I_TAX_INCL,	/* Table */
//  PROP_I_TAXTABLE,	/* Table */
//  PROP_B_ACCT,	/* Table */
//  PROP_B_PRICE,	/* Table (numeric) */
//  PROP_BILL,		/* Table */
//  PROP_B_TAXTABLE_1,	/* Table */
//  PROP_B_TAX_INCL,	/* Table */
//  PROP_B_TAXTABLE,	/* Table */
//  PROP_B_PAYTYPE,	/* Table */
//  PROP_BILLABLE,	/* Table */
//  PROP_BILLTO_TYPE,	/* Table */
//  PROP_BILLTO,	/* Table */
//  PROP_ORDER,		/* Table */
};

/* GObject Initialization */
G_DEFINE_TYPE(GncEntry, gnc_entry, QOF_TYPE_INSTANCE);

static void
gnc_entry_init(GncEntry* entry)
{
}

static void
gnc_entry_dispose(GObject *entryp)
{
    G_OBJECT_CLASS(gnc_entry_parent_class)->dispose(entryp);
}

static void
gnc_entry_finalize(GObject* entryp)
{
    G_OBJECT_CLASS(gnc_entry_parent_class)->finalize(entryp);
}

static void
gnc_entry_get_property (GObject         *object,
                        guint            prop_id,
                        GValue          *value,
                        GParamSpec      *pspec)
{
    GncEntry *entry;

    g_return_if_fail(GNC_IS_ENTRY(object));

    entry = GNC_ENTRY(object);
    switch (prop_id)
    {
    case PROP_DESCRIPTION:
        g_value_set_string(value, entry->desc);
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
        break;
    }
}

static void
gnc_entry_set_property (GObject         *object,
                        guint            prop_id,
                        const GValue          *value,
                        GParamSpec      *pspec)
{
    GncEntry *entry;

    g_return_if_fail(GNC_IS_ENTRY(object));

    entry = GNC_ENTRY(object);
    g_assert (qof_instance_get_editlevel(entry));

    switch (prop_id)
    {
    case PROP_DESCRIPTION:
        gncEntrySetDescription(entry, g_value_get_string(value));
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
        break;
    }
}

/** Return displayable name */
static gchar*
impl_get_display_name(const QofInstance* inst)
{
    GncEntry* entry;
    gchar* display_name;
    gchar* s;

    g_return_val_if_fail(inst != NULL, FALSE);
    g_return_val_if_fail(GNC_IS_ENTRY(inst), FALSE);

    entry = GNC_ENTRY(inst);
    if (entry->order != NULL)
    {
        display_name = qof_instance_get_display_name(QOF_INSTANCE(entry->order));
        s = g_strdup_printf("Entry in %s", display_name);
        g_free(display_name);
        return s;
    }
    if (entry->invoice != NULL)
    {
        display_name = qof_instance_get_display_name(QOF_INSTANCE(entry->invoice));
        s = g_strdup_printf("Entry in %s", display_name);
        g_free(display_name);
        return s;
    }
    if (entry->bill != NULL)
    {
        display_name = qof_instance_get_display_name(QOF_INSTANCE(entry->bill));
        s = g_strdup_printf("Entry in %s", display_name);
        g_free(display_name);
        return s;
    }

    return g_strdup_printf("Entry %p", inst);
}

/** Does this object refer to a specific object */
static gboolean
impl_refers_to_object(const QofInstance* inst, const QofInstance* ref)
{
    GncEntry* entry;

    g_return_val_if_fail(inst != NULL, FALSE);
    g_return_val_if_fail(GNC_IS_ENTRY(inst), FALSE);

    entry = GNC_ENTRY(inst);

    if (GNC_IS_ACCOUNT(ref))
    {
        Account* acc = GNC_ACCOUNT(ref);
        return (entry->i_account == acc || entry->b_account == acc);
    }
    else if (GNC_IS_TAXTABLE(ref))
    {
        GncTaxTable* tt = GNC_TAXTABLE(ref);
        return (entry->i_tax_table == tt || entry->b_tax_table == tt);
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
    if (!GNC_IS_ACCOUNT(ref) && !GNC_IS_TAXTABLE(ref))
    {
        return NULL;
    }

    return qof_instance_get_referring_object_list_from_collection(qof_instance_get_collection(inst), ref);
}

static void
gnc_entry_class_init (GncEntryClass *klass)
{
    GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
    QofInstanceClass* qof_class = QOF_INSTANCE_CLASS(klass);

    gobject_class->dispose = gnc_entry_dispose;
    gobject_class->finalize = gnc_entry_finalize;
    gobject_class->set_property = gnc_entry_set_property;
    gobject_class->get_property = gnc_entry_get_property;

    qof_class->get_display_name = impl_get_display_name;
    qof_class->refers_to_object = impl_refers_to_object;
    qof_class->get_typed_referring_object_list = impl_get_typed_referring_object_list;

    g_object_class_install_property
    (gobject_class,
     PROP_DESCRIPTION,
     g_param_spec_string ("description",
                          "Entry Description",
                          "The description is an arbitrary string "
                          "assigned by the user.  It provides identification "
                          "for this entry.",
                          NULL,
                          G_PARAM_READWRITE));
}

/* Create/Destroy Functions */
GncEntry *gncEntryCreate (QofBook *book)
{
    GncEntry *entry;
    gnc_numeric zero = gnc_numeric_zero ();

    if (!book) return NULL;

    entry = g_object_new (GNC_TYPE_ENTRY, NULL);
    qof_instance_init_data (&entry->inst, _GNC_MOD_NAME, book);

    entry->desc = CACHE_INSERT ("");
    entry->action = CACHE_INSERT ("");
    entry->notes = CACHE_INSERT ("");
    entry->quantity = zero;

    entry->i_price = zero;
    entry->i_taxable = TRUE;
    entry->i_discount = zero;
    entry->i_disc_type = GNC_AMT_TYPE_PERCENT;
    entry->i_disc_how = GNC_DISC_PRETAX;

    entry->b_price = zero;
    entry->b_taxable = TRUE;
    entry->billto.type = GNC_OWNER_CUSTOMER;
    entry->b_payment = GNC_PAYMENT_CASH;

    entry->values_dirty = TRUE;

    qof_event_gen (&entry->inst, QOF_EVENT_CREATE, NULL);

    return entry;
}

void gncEntryDestroy (GncEntry *entry)
{
    if (!entry) return;
    qof_instance_set_destroying(entry, TRUE);
    gncEntryCommitEdit(entry);
}

static void gncEntryFree (GncEntry *entry)
{
    if (!entry) return;

    qof_event_gen (&entry->inst, QOF_EVENT_DESTROY, NULL);

    CACHE_REMOVE (entry->desc);
    CACHE_REMOVE (entry->action);
    CACHE_REMOVE (entry->notes);
    if (entry->i_tax_values)
        gncAccountValueDestroy (entry->i_tax_values);
    if (entry->b_tax_values)
        gncAccountValueDestroy (entry->b_tax_values);
    if (entry->i_tax_table)
        gncTaxTableDecRef (entry->i_tax_table);
    if (entry->b_tax_table)
        gncTaxTableDecRef (entry->b_tax_table);

    /* qof_instance_release (&entry->inst); */
    g_object_unref (entry);
}

/* ================================================================ */
/* Set Functions */

void gncEntrySetDate (GncEntry *entry, Timespec date)
{
    gboolean first_date = FALSE;
    Timespec zero_time = { 0, 0 };

    if (!entry) return;
    if (timespec_equal (&entry->date, &date)) return;
    if (timespec_equal (&entry->date, &zero_time))
        first_date = TRUE;
    gncEntryBeginEdit (entry);
    entry->date = date;
    mark_entry (entry);
    gncEntryCommitEdit (entry);

    /* Don't re-sort the first time we set the date on this entry */
    if (!first_date)
    {
        if (entry->invoice)
            gncInvoiceSortEntries(entry->invoice);
        if (entry->bill)
            gncInvoiceSortEntries(entry->bill);
    }
}

void gncEntrySetDateGDate (GncEntry *entry, const GDate* date)
{
    if (!entry || !date || !g_date_valid(date))
        return;

    /* Watch out: Here we are deviating from the initial convention that a
    GDate always converts to the start time of the day. Instead, the GDate is
    converted to "noon" on the respective date. This is not nice, but this
    convention was used for the Timespec of GncEntry all the time, so we better
    stick to it.*/
    gncEntrySetDate(entry, timespecCanonicalDayTime(gdate_to_timespec(*date)));
}

void gncEntrySetDateEntered (GncEntry *entry, Timespec date)
{
    if (!entry) return;
    if (timespec_equal (&entry->date_entered, &date)) return;
    gncEntryBeginEdit (entry);
    entry->date_entered = date;
    mark_entry (entry);
    gncEntryCommitEdit (entry);
}

void gncEntrySetDescription (GncEntry *entry, const char *desc)
{
    if (!entry || !desc) return;
    SET_STR (entry, entry->desc, desc);
    mark_entry (entry);
    gncEntryCommitEdit (entry);
}

void gncEntrySetAction (GncEntry *entry, const char *action)
{
    if (!entry || !action) return;
    SET_STR (entry, entry->action, action);
    mark_entry (entry);
    gncEntryCommitEdit (entry);
}

void gncEntrySetNotes (GncEntry *entry, const char *notes)
{
    if (!entry || !notes) return;
    SET_STR (entry, entry->notes, notes);
    mark_entry (entry);
    gncEntryCommitEdit (entry);
}

void gncEntrySetQuantity (GncEntry *entry, gnc_numeric quantity)
{
    if (!entry) return;
    if (gnc_numeric_eq (entry->quantity, quantity)) return;
    gncEntryBeginEdit (entry);
    entry->quantity = quantity;
    entry->values_dirty = TRUE;
    mark_entry (entry);
    gncEntryCommitEdit (entry);
}

void gncEntrySetDocQuantity (GncEntry *entry, gnc_numeric quantity, gboolean is_cn)
{
    if (!entry) return;
    if (gnc_numeric_eq (entry->quantity, (is_cn ? gnc_numeric_neg (quantity) : quantity))) return;
    gncEntryBeginEdit (entry);
    entry->quantity = (is_cn ? gnc_numeric_neg (quantity) : quantity);
    entry->values_dirty = TRUE;
    mark_entry (entry);
    gncEntryCommitEdit (entry);
}

/* Customer Invoices */

void gncEntrySetInvAccount (GncEntry *entry, Account *acc)
{
    if (!entry) return;
    if (entry->i_account == acc) return;
    gncEntryBeginEdit (entry);
    entry->i_account = acc;
    mark_entry (entry);
    gncEntryCommitEdit (entry);
}

void gncEntrySetInvPrice (GncEntry *entry, gnc_numeric price)
{
    if (!entry) return;
    if (gnc_numeric_eq (entry->i_price, price)) return;
    gncEntryBeginEdit (entry);
    entry->i_price = price;
    entry->values_dirty = TRUE;
    mark_entry (entry);
    gncEntryCommitEdit (entry);
}

void gncEntrySetInvTaxable (GncEntry *entry, gboolean taxable)
{
    if (!entry) return;
    if (entry->i_taxable == taxable) return;
    gncEntryBeginEdit (entry);
    entry->i_taxable = taxable;
    entry->values_dirty = TRUE;
    mark_entry (entry);
    gncEntryCommitEdit (entry);
}

void gncEntrySetInvTaxIncluded (GncEntry *entry, gboolean taxincluded)
{
    if (!entry) return;
    if (entry->i_taxincluded == taxincluded) return;
    gncEntryBeginEdit (entry);
    entry->i_taxincluded = taxincluded;
    entry->values_dirty = TRUE;
    mark_entry (entry);
    gncEntryCommitEdit (entry);
}

void gncEntrySetInvTaxTable (GncEntry *entry, GncTaxTable *table)
{
    if (!entry) return;
    if (entry->i_tax_table == table) return;
    gncEntryBeginEdit (entry);
    if (entry->i_tax_table)
        gncTaxTableDecRef (entry->i_tax_table);
    if (table)
        gncTaxTableIncRef (table);
    entry->i_tax_table = table;
    entry->values_dirty = TRUE;
    mark_entry (entry);
    gncEntryCommitEdit (entry);
}

void gncEntrySetInvDiscount (GncEntry *entry, gnc_numeric discount)
{
    if (!entry) return;
    if (gnc_numeric_eq (entry->i_discount, discount)) return;
    gncEntryBeginEdit (entry);
    entry->i_discount = discount;
    entry->values_dirty = TRUE;
    mark_entry (entry);
    gncEntryCommitEdit (entry);
}

void gncEntrySetInvDiscountType (GncEntry *entry, GncAmountType type)
{
    if (!entry) return;
    if (entry->i_disc_type == type) return;

    gncEntryBeginEdit (entry);
    entry->i_disc_type = type;
    entry->values_dirty = TRUE;
    mark_entry (entry);
    gncEntryCommitEdit (entry);
}

void gncEntrySetInvDiscountHow (GncEntry *entry, GncDiscountHow how)
{
    if (!entry) return;
    if (entry->i_disc_how == how) return;

    gncEntryBeginEdit (entry);
    entry->i_disc_how = how;
    entry->values_dirty = TRUE;
    mark_entry (entry);
    gncEntryCommitEdit (entry);
}

void qofEntrySetInvDiscType (GncEntry *entry, const char *type_string)
{
    GncAmountType type;

    if (!entry) return;
    gncAmountStringToType(type_string, &type);
    if (entry->i_disc_type == type) return;
    gncEntryBeginEdit (entry);
    entry->i_disc_type = type;
    entry->values_dirty = TRUE;
    mark_entry (entry);
    gncEntryCommitEdit (entry);

}

void qofEntrySetInvDiscHow  (GncEntry *entry, const char *type)
{
    GncDiscountHow how = GNC_DISC_PRETAX;

    if (!entry) return;
    gncEntryBeginEdit (entry);
    gncEntryDiscountStringToHow(type, &how);
    if (entry->i_disc_how == how) return;
    entry->i_disc_how = how;
    entry->values_dirty = TRUE;
    mark_entry (entry);
    gncEntryCommitEdit (entry);
}

/* Vendor Bills */

void gncEntrySetBillAccount (GncEntry *entry, Account *acc)
{
    if (!entry) return;
    if (entry->b_account == acc) return;
    gncEntryBeginEdit (entry);
    entry->b_account = acc;
    mark_entry (entry);
    gncEntryCommitEdit (entry);
}

void gncEntrySetBillPrice (GncEntry *entry, gnc_numeric price)
{
    if (!entry) return;
    if (gnc_numeric_eq (entry->b_price, price)) return;
    gncEntryBeginEdit (entry);
    entry->b_price = price;
    entry->values_dirty = TRUE;
    mark_entry (entry);
    gncEntryCommitEdit (entry);
}

void gncEntrySetBillTaxable (GncEntry *entry, gboolean taxable)
{
    if (!entry) return;
    if (entry->b_taxable == taxable) return;
    gncEntryBeginEdit (entry);
    entry->b_taxable = taxable;
    entry->values_dirty = TRUE;
    mark_entry (entry);
    gncEntryCommitEdit (entry);
}

void gncEntrySetBillTaxIncluded (GncEntry *entry, gboolean taxincluded)
{
    if (!entry) return;
    if (entry->b_taxincluded == taxincluded) return;
    gncEntryBeginEdit (entry);
    entry->b_taxincluded = taxincluded;
    entry->values_dirty = TRUE;
    mark_entry (entry);
    gncEntryCommitEdit (entry);
}

void gncEntrySetBillTaxTable (GncEntry *entry, GncTaxTable *table)
{
    if (!entry) return;
    if (entry->b_tax_table == table) return;
    gncEntryBeginEdit (entry);
    if (entry->b_tax_table)
        gncTaxTableDecRef (entry->b_tax_table);
    if (table)
        gncTaxTableIncRef (table);
    entry->b_tax_table = table;
    entry->values_dirty = TRUE;
    mark_entry (entry);
    gncEntryCommitEdit (entry);
}

void gncEntrySetBillable (GncEntry *entry, gboolean billable)
{
    if (!entry) return;
    if (entry->billable == billable) return;

    gncEntryBeginEdit (entry);
    entry->billable = billable;
    mark_entry (entry);
    gncEntryCommitEdit (entry);
}

void gncEntrySetBillTo (GncEntry *entry, GncOwner *billto)
{
    if (!entry || !billto) return;
    if (gncOwnerEqual (&entry->billto, billto)) return;

    gncEntryBeginEdit (entry);
    gncOwnerCopy (billto, &entry->billto);
    mark_entry (entry);
    gncEntryCommitEdit (entry);
}

void gncEntrySetBillPayment (GncEntry *entry, GncEntryPaymentType type)
{
    if (!entry) return;
    if (entry->b_payment == type) return;
    gncEntryBeginEdit (entry);
    entry->b_payment = type;
    mark_entry (entry);
    gncEntryCommitEdit (entry);
}

/* Called from gncOrder when we're added to the Order */
void gncEntrySetOrder (GncEntry *entry, GncOrder *order)
{
    if (!entry) return;
    if (entry->order == order) return;
    gncEntryBeginEdit (entry);
    entry->order = order;
    mark_entry (entry);
    gncEntryCommitEdit (entry);

}

/* called from gncInvoice when we're added to the Invoice */
void gncEntrySetInvoice (GncEntry *entry, GncInvoice *invoice)
{
    if (!entry) return;
    if (entry->invoice == invoice) return;
    gncEntryBeginEdit (entry);
    entry->invoice = invoice;
    mark_entry (entry);
    gncEntryCommitEdit (entry);
}

/* called from gncInvoice when we're added to the Invoice/Bill */
void gncEntrySetBill (GncEntry *entry, GncInvoice *bill)
{
    if (!entry) return;
    if (entry->bill == bill) return;
    gncEntryBeginEdit (entry);
    entry->bill = bill;
    mark_entry (entry);
    gncEntryCommitEdit (entry);
}

void gncEntryCopy (const GncEntry *src, GncEntry *dest, gboolean add_entry)
{
    if (!src || !dest) return;

    gncEntryBeginEdit (dest);
    dest->date 			= src->date;
    dest->date_entered		= src->date_entered; /* ??? */
    gncEntrySetDescription (dest, src->desc);
    gncEntrySetAction (dest, src->action);
    gncEntrySetNotes (dest, src->notes);
    dest->quantity		= src->quantity;

    dest->i_account		= src->i_account;
    dest->i_price			= src->i_price;
    dest->i_taxable		= src->i_taxable;
    dest->i_taxincluded		= src->i_taxincluded;
    dest->i_discount		= src->i_discount;
    dest->i_disc_type		= src->i_disc_type;
    dest->i_disc_how		= src->i_disc_how;

    /* vendor bill data */
    dest->b_account		= src->b_account;
    dest->b_price			= src->b_price;
    dest->b_taxable		= src->b_taxable;
    dest->b_taxincluded		= src->b_taxincluded;
    dest->billable		= src->billable;
    dest->billto			= src->billto;

    if (src->i_tax_table)
        gncEntrySetInvTaxTable (dest, src->i_tax_table);

    if (src->b_tax_table)
        gncEntrySetBillTaxTable (dest, src->b_tax_table);

    if (add_entry)
    {
        if (src->order)
            gncOrderAddEntry (src->order, dest);

        if (src->invoice)
            gncInvoiceAddEntry (src->invoice, dest);

        if (src->bill)
            gncBillAddEntry (src->bill, dest);
    }

    dest->values_dirty = TRUE;
    mark_entry (dest);
    gncEntryCommitEdit (dest);
}

/* ================================================================ */
/* Get Functions */

Timespec gncEntryGetDate (const GncEntry *entry)
{
    Timespec ts;
    ts.tv_sec = 0;
    ts.tv_nsec = 0;
    if (!entry) return ts;
    return entry->date;
}

GDate gncEntryGetDateGDate(const GncEntry *entry)
{
    return timespec_to_gdate(gncEntryGetDate(entry));
}

Timespec gncEntryGetDateEntered (const GncEntry *entry)
{
    Timespec ts;
    ts.tv_sec = 0;
    ts.tv_nsec = 0;
    if (!entry) return ts;
    return entry->date_entered;
}

const char * gncEntryGetDescription (const GncEntry *entry)
{
    if (!entry) return NULL;
    return entry->desc;
}

const char * gncEntryGetAction (const GncEntry *entry)
{
    if (!entry) return NULL;
    return entry->action;
}

const char * gncEntryGetNotes (const GncEntry *entry)
{
    if (!entry) return NULL;
    return entry->notes;
}

gnc_numeric gncEntryGetQuantity (const GncEntry *entry)
{
    if (!entry) return gnc_numeric_zero();
    return entry->quantity;
}

gnc_numeric gncEntryGetDocQuantity (const GncEntry *entry, gboolean is_cn)
{
    gnc_numeric value = gncEntryGetQuantity (entry);
    return (is_cn ? gnc_numeric_neg (value) : value);
}

/* Customer Invoice */

Account * gncEntryGetInvAccount (const GncEntry *entry)
{
    if (!entry) return NULL;
    return entry->i_account;
}

gnc_numeric gncEntryGetInvPrice (const GncEntry *entry)
{
    if (!entry) return gnc_numeric_zero();
    return entry->i_price;
}

gnc_numeric gncEntryGetInvDiscount (const GncEntry *entry)
{
    if (!entry) return gnc_numeric_zero();
    return entry->i_discount;
}

GncAmountType gncEntryGetInvDiscountType (const GncEntry *entry)
{
    if (!entry) return 0;
    return entry->i_disc_type;
}

GncDiscountHow gncEntryGetInvDiscountHow (const GncEntry *entry)
{
    if (!entry) return 0;
    return entry->i_disc_how;
}

char* qofEntryGetInvDiscType (const GncEntry *entry)
{
    char *type_string;

    if (!entry) return 0;
    type_string = g_strdup(gncAmountTypeToString(entry->i_disc_type));
    return type_string;
}

char* qofEntryGetInvDiscHow (const GncEntry *entry)
{
    char *type_string;

    if (!entry) return 0;
    type_string = g_strdup(gncEntryDiscountHowToString(entry->i_disc_how));
    return type_string;
}

gboolean gncEntryGetInvTaxable (const GncEntry *entry)
{
    if (!entry) return FALSE;
    return entry->i_taxable;
}

gboolean gncEntryGetInvTaxIncluded (const GncEntry *entry)
{
    if (!entry) return FALSE;
    return entry->i_taxincluded;
}

GncTaxTable * gncEntryGetInvTaxTable (const GncEntry *entry)
{
    if (!entry) return NULL;
    return entry->i_tax_table;
}

/* vendor bills */

Account * gncEntryGetBillAccount (const GncEntry *entry)
{
    if (!entry) return NULL;
    return entry->b_account;
}

gnc_numeric gncEntryGetBillPrice (const GncEntry *entry)
{
    if (!entry) return gnc_numeric_zero();
    return entry->b_price;
}

gboolean gncEntryGetBillTaxable (const GncEntry *entry)
{
    if (!entry) return FALSE;
    return entry->b_taxable;
}

gboolean gncEntryGetBillTaxIncluded (const GncEntry *entry)
{
    if (!entry) return FALSE;
    return entry->b_taxincluded;
}

GncTaxTable * gncEntryGetBillTaxTable (const GncEntry *entry)
{
    if (!entry) return NULL;
    return entry->b_tax_table;
}

gboolean gncEntryGetBillable (const GncEntry *entry)
{
    if (!entry) return FALSE;
    return entry->billable;
}

GncOwner * gncEntryGetBillTo (GncEntry *entry)
{
    if (!entry) return NULL;
    return &entry->billto;
}

GncEntryPaymentType gncEntryGetBillPayment (const GncEntry* entry)
{
    if (!entry) return 0;
    return entry->b_payment;
}

GncInvoice * gncEntryGetInvoice (const GncEntry *entry)
{
    if (!entry) return NULL;
    return entry->invoice;
}

GncInvoice * gncEntryGetBill (const GncEntry *entry)
{
    if (!entry) return NULL;
    return entry->bill;
}

GncOrder * gncEntryGetOrder (const GncEntry *entry)
{
    if (!entry) return NULL;
    return entry->order;
}

/* ================================================================ */
/*
 * This is the logic of computing the total for an Entry, so you know
 * what values to put into various Splits or to display in the ledger.
 * In other words, we combine the quantity, unit-price, discount and
 * taxes together, depending on various flags.
 *
 * There are four potential ways to combine these numbers:
 * Discount:     Pre-Tax   Post-Tax
 *   Tax   :     Included  Not-Included
 *
 * The process is relatively simple:
 *
 *  1) compute the aggregate price (price*qty)
 *  2) if taxincluded, then back-compute the aggregate pre-tax price
 *  3) apply discount and taxes in the appropriate order
 *  4) return the requested results.
 *
 * Step 2 can be done with aggregate taxes; no need to compute them all
 * unless the caller asked for the tax_value.
 *
 * Note that the returned "value" is such that
 *   value + tax == "total to pay"
 * which means in the case of tax-included that the returned
 * "value" may be less than the aggregate price, even without a
 * discount.  If you want to display the tax-included value, you need
 * to add the value and taxes together.  In other words, the value is
 * the amount the merchant gets; the taxes are the amount the gov't
 * gets, and the customer pays the sum or value + taxes.
 *
 * The SCU is the denominator to convert the value.
 *
 * The discount return value is just for entertainment -- you may want
 * to let a consumer know how much they saved.
 */
void gncEntryComputeValue (gnc_numeric qty, gnc_numeric price,
                           const GncTaxTable *tax_table, gboolean tax_included,
                           gnc_numeric discount, GncAmountType discount_type,
                           GncDiscountHow discount_how, int SCU,
                           gnc_numeric *value, gnc_numeric *discount_value,
                           GList **tax_value)
{
    gnc_numeric aggregate;
    gnc_numeric pretax;
    gnc_numeric result;
    gnc_numeric tax;
    gnc_numeric percent = gnc_numeric_create (100, 1);
    gnc_numeric tpercent = gnc_numeric_zero ();
    gnc_numeric tvalue = gnc_numeric_zero ();

    GList     * entries = gncTaxTableGetEntries (tax_table);
    GList     * node;

    /* Step 1: compute the aggregate price */

    aggregate = gnc_numeric_mul (qty, price, GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD);

    /* Step 2: compute the pre-tax aggregate */

    /* First, compute the aggregate tpercent and tvalue numbers */
    for (node = entries; node; node = node->next)
    {
        GncTaxTableEntry *entry = node->data;
        gnc_numeric amount = gncTaxTableEntryGetAmount (entry);

        switch (gncTaxTableEntryGetType (entry))
        {
        case GNC_AMT_TYPE_VALUE:
            tvalue = gnc_numeric_add (tvalue, amount, GNC_DENOM_AUTO,
                                      GNC_HOW_DENOM_LCD);
            break;
        case GNC_AMT_TYPE_PERCENT:
            tpercent = gnc_numeric_add (tpercent, amount, GNC_DENOM_AUTO,
                                        GNC_HOW_DENOM_LCD);
            break;
        default:
            g_warning ("Unknown tax type: %d", gncTaxTableEntryGetType (entry));
            break;
        }
    }
    /* now we need to convert from 5% -> .05 */
    tpercent = gnc_numeric_div (tpercent, percent, GNC_DENOM_AUTO,
                                GNC_HOW_DENOM_LCD);

    /* Next, actually compute the pre-tax aggregate value based on the
     * taxincluded flag.
     */
    if (tax_table && tax_included)
    {
        /* Back-compute the pre-tax aggregate value.
         * We know that aggregate = pretax + pretax*tpercent + tvalue, so
         * pretax = (aggregate-tvalue)/(1+tpercent)
         */
        pretax = gnc_numeric_sub (aggregate, tvalue, GNC_DENOM_AUTO,
                                  GNC_HOW_DENOM_LCD);
        pretax = gnc_numeric_div (pretax,
                                  gnc_numeric_add (tpercent,
                                          gnc_numeric_create (1, 1),
                                          GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD),
                                  GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD);
    }
    else
    {
        pretax = aggregate;
    }

    /* Step 3:  apply discount and taxes in the appropriate order */

    /*
     * There are two ways to apply discounts and taxes.  In one way, you
     * always compute the discount off the pretax number, and compute
     * the taxes off of either the pretax value or "pretax-discount"
     * value.  In the other way, you always compute the tax on "pretax",
     * and compute the discount on either "pretax" or "pretax+taxes".
     *
     * I don't know which is the "correct" way.
     */

    /*
     * Type:    discount    tax
     * PRETAX   pretax      pretax-discount
     * SAMETIME pretax      pretax
     * POSTTAX  pretax+tax  pretax
     */

    switch (discount_how)
    {
    case GNC_DISC_PRETAX:
    case GNC_DISC_SAMETIME:
        /* compute the discount from pretax */

        if (discount_type == GNC_AMT_TYPE_PERCENT)
        {
            discount = gnc_numeric_div (discount, percent, GNC_DENOM_AUTO,
                                        GNC_HOW_DENOM_LCD);
            discount = gnc_numeric_mul (pretax, discount, GNC_DENOM_AUTO,
                                        GNC_HOW_DENOM_LCD);
        }

        result = gnc_numeric_sub (pretax, discount, GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD);

        /* Figure out when to apply the tax, pretax or pretax-discount */
        if (discount_how == GNC_DISC_PRETAX)
            pretax = result;
        break;

    case GNC_DISC_POSTTAX:
        /* compute discount on pretax+taxes */

        if (discount_type == GNC_AMT_TYPE_PERCENT)
        {
            gnc_numeric after_tax;

            tax = gnc_numeric_mul (pretax, tpercent, GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD);
            after_tax = gnc_numeric_add (pretax, tax, GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD);
            after_tax = gnc_numeric_add (after_tax, tvalue, GNC_DENOM_AUTO,
                                         GNC_HOW_DENOM_LCD);
            discount = gnc_numeric_div (discount, percent, GNC_DENOM_AUTO,
                                        GNC_HOW_DENOM_LCD);
            discount = gnc_numeric_mul (after_tax, discount, GNC_DENOM_AUTO,
                                        GNC_HOW_DENOM_LCD);
        }

        result = gnc_numeric_sub (pretax, discount, GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD);
        break;

    default:
        g_warning ("unknown DiscountHow value: %d", discount_how);
        break;
    }

    /* Step 4:  return the requested results. */

    /* result == amount merchant gets
     * discount == amount of discount
     * need to compute taxes (based on 'pretax') if the caller wants it.
     */

    if (discount_value != NULL)
    {
        if (SCU) discount = gnc_numeric_convert(discount, SCU, GNC_HOW_RND_ROUND_HALF_UP);
        *discount_value = discount;
    }

    if (value != NULL)
    {
        if (SCU) result = gnc_numeric_convert(result, SCU, GNC_HOW_RND_ROUND_HALF_UP);
        *value = result;
    }

    /* Now... Compute the list of tax values (if the caller wants it) */

    if (tax_value != NULL)
    {
        GList *	taxes = NULL;

        for (node = entries; node; node = node->next)
        {
            GncTaxTableEntry *entry = node->data;
            Account *acc = gncTaxTableEntryGetAccount (entry);
            gnc_numeric amount = gncTaxTableEntryGetAmount (entry);

            g_return_if_fail (acc);

            switch (gncTaxTableEntryGetType (entry))
            {
            case GNC_AMT_TYPE_VALUE:
                if (SCU) amount = gnc_numeric_convert(amount, SCU, GNC_HOW_RND_ROUND_HALF_UP);
                taxes = gncAccountValueAdd (taxes, acc, amount);
                break;
            case GNC_AMT_TYPE_PERCENT:
                amount = gnc_numeric_div (amount, percent, GNC_DENOM_AUTO,
                                          GNC_HOW_DENOM_LCD);
                tax = gnc_numeric_mul (pretax, amount, GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD);
                if (SCU) tax = gnc_numeric_convert(tax, SCU, GNC_HOW_RND_ROUND_HALF_UP);
                taxes = gncAccountValueAdd (taxes, acc, tax);
                break;
            default:
                break;
            }
        }
        *tax_value = taxes;
    }

    return;
}

static int
get_entry_commodity_denom (const GncEntry *entry)
{
    gnc_commodity *c;
    if (!entry)
        return 0;
    if (entry->invoice)
    {
        c = gncInvoiceGetCurrency (entry->invoice);
        if (c)
            return (gnc_commodity_get_fraction (c));
    }
    if (entry->bill)
    {
        c = gncInvoiceGetCurrency (entry->bill);
        if (c)
            return (gnc_commodity_get_fraction (c));
    }
    return 100000;
}

static void
gncEntryRecomputeValues (GncEntry *entry)
{
    int denom;

    /* See if either tax table changed since we last computed values */
    if (entry->i_tax_table)
    {
        Timespec modtime = gncTaxTableLastModified (entry->i_tax_table);
        if (timespec_cmp (&entry->i_taxtable_modtime, &modtime))
        {
            entry->values_dirty = TRUE;
            entry->i_taxtable_modtime = modtime;
        }
    }
    if (entry->b_tax_table)
    {
        Timespec modtime = gncTaxTableLastModified (entry->b_tax_table);
        if (timespec_cmp (&entry->b_taxtable_modtime, &modtime))
        {
            entry->values_dirty = TRUE;
            entry->b_taxtable_modtime = modtime;
        }
    }

    if (!entry->values_dirty)
        return;

    /* Clear the last-computed tax values */
    if (entry->i_tax_values)
    {
        gncAccountValueDestroy (entry->i_tax_values);
        entry->i_tax_values = NULL;
    }
    if (entry->b_tax_values)
    {
        gncAccountValueDestroy (entry->b_tax_values);
        entry->b_tax_values = NULL;
    }

    /* Determine the commodity denominator */
    denom = get_entry_commodity_denom (entry);

    gncEntryBeginEdit (entry);
    /* Compute the invoice values */
    gncEntryComputeValue (entry->quantity, entry->i_price,
                          (entry->i_taxable ? entry->i_tax_table : NULL),
                          entry->i_taxincluded,
                          entry->i_discount, entry->i_disc_type,
                          entry->i_disc_how,
                          denom,
                          &(entry->i_value), &(entry->i_disc_value),
                          &(entry->i_tax_values));

    /* Compute the bill values */
    gncEntryComputeValue (entry->quantity, entry->b_price,
                          (entry->b_taxable ? entry->b_tax_table : NULL),
                          entry->b_taxincluded,
                          gnc_numeric_zero(), GNC_AMT_TYPE_VALUE, GNC_DISC_PRETAX,
                          denom,
                          &(entry->b_value), NULL, &(entry->b_tax_values));

    entry->i_value_rounded = gnc_numeric_convert (entry->i_value, denom,
                             GNC_HOW_RND_ROUND_HALF_UP);
    entry->i_disc_value_rounded = gnc_numeric_convert (entry->i_disc_value, denom,
                                  GNC_HOW_RND_ROUND_HALF_UP);
    entry->i_tax_value = gncAccountValueTotal (entry->i_tax_values);
    entry->i_tax_value_rounded = gnc_numeric_convert (entry->i_tax_value, denom,
                                 GNC_HOW_RND_ROUND_HALF_UP);

    entry->b_value_rounded = gnc_numeric_convert (entry->b_value, denom,
                             GNC_HOW_RND_ROUND_HALF_UP);
    entry->b_tax_value = gncAccountValueTotal (entry->b_tax_values);
    entry->b_tax_value_rounded = gnc_numeric_convert (entry->b_tax_value, denom,
                                 GNC_HOW_RND_ROUND_HALF_UP);
    entry->values_dirty = FALSE;
    mark_entry (entry);
    gncEntryCommitEdit (entry);
}

/* The "Int" functions below are for internal use only.
 * Outside this file, use the "Doc" or "Bal" variants found below instead. */
static gnc_numeric gncEntryGetIntValue (GncEntry *entry, gboolean round, gboolean is_cust_doc)
{
    if (!entry) return gnc_numeric_zero();
    gncEntryRecomputeValues (entry);
    if (round)
        return (is_cust_doc ? entry->i_value_rounded : entry->b_value_rounded);
    else
        return (is_cust_doc ? entry->i_value : entry->b_value);
}

static gnc_numeric gncEntryGetIntTaxValue (GncEntry *entry, gboolean round, gboolean is_cust_doc)
{
    if (!entry) return gnc_numeric_zero();
    gncEntryRecomputeValues (entry);
    if (round)
        return (is_cust_doc ? entry->i_tax_value_rounded : entry->b_tax_value_rounded);
    else
        return (is_cust_doc ? entry->i_tax_value : entry->b_tax_value);
}

/* Careful: the returned list is managed by the entry, and will only be valid for a short time */
static AccountValueList * gncEntryGetIntTaxValues (GncEntry *entry, gboolean is_cust_doc)
{
    if (!entry) return NULL;
    gncEntryRecomputeValues (entry);
    return (is_cust_doc ? entry->i_tax_values : entry->b_tax_values);
}

static gnc_numeric gncEntryGetIntDiscountValue (GncEntry *entry, gboolean round, gboolean is_cust_doc)
{
    if (!entry) return gnc_numeric_zero();
    gncEntryRecomputeValues (entry);
    if (round)
        return (is_cust_doc ? entry->i_disc_value_rounded : gnc_numeric_zero());
    else
        return (is_cust_doc ? entry->i_disc_value : gnc_numeric_zero());
}

gnc_numeric gncEntryGetDocValue (GncEntry *entry, gboolean round, gboolean is_cust_doc, gboolean is_cn)
{
    gnc_numeric value = gncEntryGetIntValue (entry, round, is_cust_doc);
    return (is_cn ? gnc_numeric_neg (value) : value);
}

gnc_numeric gncEntryGetDocTaxValue (GncEntry *entry, gboolean round, gboolean is_cust_doc, gboolean is_cn)
{
    gnc_numeric value = gncEntryGetIntTaxValue (entry, round, is_cust_doc);
    return (is_cn ? gnc_numeric_neg (value) : value);
}

/* Careful: the returned list is NOT owned by the entry and should be freed by the caller */
AccountValueList * gncEntryGetDocTaxValues (GncEntry *entry, gboolean is_cust_doc, gboolean is_cn)
{
    AccountValueList *int_values = gncEntryGetIntTaxValues (entry, is_cust_doc);
    AccountValueList *values = NULL, *node;

    /* Make a copy of the list with negated values if necessary. */
    for (node = int_values; node; node = node->next)
    {
        GncAccountValue *acct_val = node->data;
        values = gncAccountValueAdd (values, acct_val->account,
                                     (is_cn ? gnc_numeric_neg (acct_val->value)
                                      : acct_val->value));
    }

    return values;
}

gnc_numeric gncEntryGetDocDiscountValue (GncEntry *entry, gboolean round, gboolean is_cust_doc, gboolean is_cn)
{
    gnc_numeric value = gncEntryGetIntDiscountValue (entry, round, is_cust_doc);
    return (is_cn ? gnc_numeric_neg (value) : value);
}

gnc_numeric gncEntryGetBalValue (GncEntry *entry, gboolean round, gboolean is_cust_doc)
{
    gnc_numeric value = gncEntryGetIntValue (entry, round, is_cust_doc);
    return (is_cust_doc ? gnc_numeric_neg (value) : value);
}

gnc_numeric gncEntryGetBalTaxValue (GncEntry *entry, gboolean round, gboolean is_cust_doc)
{
    gnc_numeric value = gncEntryGetIntTaxValue (entry, round, is_cust_doc);
    return (is_cust_doc ? gnc_numeric_neg (value) : value);
}

/* Careful: the returned list is NOT owned by the entry and should be freed by the caller */
AccountValueList * gncEntryGetBalTaxValues (GncEntry *entry, gboolean is_cust_doc)
{
    AccountValueList *int_values = gncEntryGetIntTaxValues (entry, is_cust_doc);
    AccountValueList *values = NULL, *node;

    /* Make a copy of the list with negated values if necessary. */
    for (node = int_values; node; node = node->next)
    {
        GncAccountValue *acct_val = node->data;
        values = gncAccountValueAdd (values, acct_val->account,
                                     (is_cust_doc ? gnc_numeric_neg (acct_val->value)
                                      : acct_val->value));
    }

    return values;
}

gnc_numeric gncEntryGetBalDiscountValue (GncEntry *entry, gboolean round, gboolean is_cust_doc)
{
    gnc_numeric value = gncEntryGetIntDiscountValue (entry, round, is_cust_doc);
    return (is_cust_doc ? gnc_numeric_neg (value) : value);
}

/* XXX this existence of this routine is just wrong */
gboolean gncEntryIsOpen (const GncEntry *entry)
{
    if (!entry) return FALSE;
    return (qof_instance_get_editlevel(entry) > 0);
}

/* ================================================================ */

void gncEntryBeginEdit (GncEntry *entry)
{
    qof_begin_edit(&entry->inst);
}

static void gncEntryOnError (QofInstance *entry, QofBackendError errcode)
{
    PERR("Entry QofBackend Failure: %d", errcode);
    gnc_engine_signal_commit_error( errcode );
}

static void gncEntryOnDone (QofInstance *inst) {}

static void entry_free (QofInstance *inst)
{
    GncEntry *entry = (GncEntry *)inst;
    gncEntryFree (entry);
}

void gncEntryCommitEdit (GncEntry *entry)
{
    /* GnuCash 2.6.3 and earlier didn't handle entry kvp's... */
    if (!kvp_frame_is_empty (entry->inst.kvp_data))
        gnc_features_set_used (qof_instance_get_book (QOF_INSTANCE (entry)), GNC_FEATURE_KVP_EXTRA_DATA);

    if (!qof_commit_edit (QOF_INSTANCE(entry))) return;
    qof_commit_edit_part2 (&entry->inst, gncEntryOnError,
                           gncEntryOnDone, entry_free);
}

int gncEntryCompare (const GncEntry *a, const GncEntry *b)
{
    int compare;

    if (a == b) return 0;
    if (!a && b) return -1;
    if (a && !b) return 1;

    compare = timespec_cmp (&(a->date), &(b->date));
    if (compare) return compare;

    compare = timespec_cmp (&(a->date_entered), &(b->date_entered));
    if (compare) return compare;

    compare = g_strcmp0 (a->desc, b->desc);
    if (compare) return compare;

    compare = g_strcmp0 (a->action, b->action);
    if (compare) return compare;

    return qof_instance_guid_compare(a, b);
}

#define CHECK_STRING(X, Y, FIELD) \
    if (g_strcmp0((X)->FIELD, (Y)->FIELD) != 0) \
    { \
        PWARN("%s differ: %s vs %s", #FIELD, (X)->FIELD, (Y)->FIELD); \
        return FALSE; \
    }

#define CHECK_ACCOUNT(X, Y, FIELD) \
    if (!xaccAccountEqual((X)->FIELD, (Y)->FIELD, TRUE)) \
    { \
        PWARN("%s differ", #FIELD); \
        return FALSE; \
    }

#define CHECK_NUMERIC(X, Y, FIELD) \
    if (!gnc_numeric_equal((X)->FIELD, (Y)->FIELD)) \
    { \
        PWARN("%s differ", #FIELD); \
        return FALSE; \
    }

#define CHECK_VALUE(X, Y, FIELD) \
    if ((X)->FIELD != (Y)->FIELD) \
    { \
        PWARN("%s differ", #FIELD); \
        return FALSE; \
    }


/* ============================================================= */
/* Object declaration */

static void
destroy_entry_on_book_close(QofInstance *ent, gpointer data)
{
    GncEntry* entry = GNC_ENTRY(ent);

    gncEntryBeginEdit(entry);
    gncEntryDestroy(entry);
}

/** Handles book end - frees all entries from the book
 *
 * @param book Book being closed
 */
static void
gnc_entry_book_end(QofBook* book)
{
    QofCollection *col;

    col = qof_book_get_collection(book, GNC_ID_ENTRY);
    qof_collection_foreach(col, destroy_entry_on_book_close, NULL);
}

static QofObject gncEntryDesc =
{
    DI(.interface_version = ) QOF_OBJECT_VERSION,
    DI(.e_type            = ) _GNC_MOD_NAME,
    DI(.type_label        = ) "Order/Invoice/Bill Entry",
    DI(.create            = ) (gpointer)gncEntryCreate,
    DI(.book_begin        = ) NULL,
    DI(.book_end          = ) gnc_entry_book_end,
    DI(.is_dirty          = ) qof_collection_is_dirty,
    DI(.mark_clean        = ) qof_collection_mark_clean,
    DI(.foreach           = ) qof_collection_foreach,
    DI(.printable         = ) NULL,
    DI(.version_cmp       = ) (int (*)(gpointer, gpointer)) qof_instance_version_cmp,
};

gboolean gncEntryRegister (void)
{
    static QofParam params[] =
    {
        { ENTRY_DATE, QOF_TYPE_DATE, (QofAccessFunc)gncEntryGetDate, (QofSetterFunc)gncEntrySetDate },
        { ENTRY_DATE_ENTERED, QOF_TYPE_DATE, (QofAccessFunc)gncEntryGetDateEntered, (QofSetterFunc)gncEntrySetDateEntered },
        { ENTRY_DESC, QOF_TYPE_STRING, (QofAccessFunc)gncEntryGetDescription, (QofSetterFunc)gncEntrySetDescription },
        { ENTRY_ACTION, QOF_TYPE_STRING, (QofAccessFunc)gncEntryGetAction, (QofSetterFunc)gncEntrySetAction },
        { ENTRY_NOTES, QOF_TYPE_STRING, (QofAccessFunc)gncEntryGetNotes, (QofSetterFunc)gncEntrySetNotes },
        { ENTRY_QTY, QOF_TYPE_NUMERIC, (QofAccessFunc)gncEntryGetQuantity, (QofSetterFunc)gncEntrySetQuantity },
        { ENTRY_IPRICE, QOF_TYPE_NUMERIC, (QofAccessFunc)gncEntryGetInvPrice, (QofSetterFunc)gncEntrySetInvPrice },
        { ENTRY_BPRICE, QOF_TYPE_NUMERIC, (QofAccessFunc)gncEntryGetBillPrice, (QofSetterFunc)gncEntrySetBillPrice },
        { ENTRY_INVOICE, GNC_ID_INVOICE, (QofAccessFunc)gncEntryGetInvoice, NULL },
        { ENTRY_IACCT, GNC_ID_ACCOUNT,  (QofAccessFunc)gncEntryGetInvAccount,  (QofSetterFunc)gncEntrySetInvAccount  },
        { ENTRY_BACCT, GNC_ID_ACCOUNT,  (QofAccessFunc)gncEntryGetBillAccount, (QofSetterFunc)gncEntrySetBillAccount },
        { ENTRY_BILL, GNC_ID_INVOICE, (QofAccessFunc)gncEntryGetBill, NULL },
        {
            ENTRY_INV_DISC_TYPE, QOF_TYPE_STRING, (QofAccessFunc)qofEntryGetInvDiscType,
            (QofSetterFunc)qofEntrySetInvDiscType
        },
        {
            ENTRY_INV_DISC_HOW, QOF_TYPE_STRING, (QofAccessFunc)qofEntryGetInvDiscHow,
            (QofSetterFunc)qofEntrySetInvDiscHow
        },
        {
            ENTRY_INV_TAXABLE, QOF_TYPE_BOOLEAN, (QofAccessFunc)gncEntryGetInvTaxable,
            (QofSetterFunc)gncEntrySetInvTaxable
        },
        {
            ENTRY_INV_TAX_INC, QOF_TYPE_BOOLEAN, (QofAccessFunc)gncEntryGetInvTaxIncluded,
            (QofSetterFunc)gncEntrySetInvTaxIncluded
        },
        {
            ENTRY_BILL_TAXABLE, QOF_TYPE_BOOLEAN, (QofAccessFunc)gncEntryGetBillTaxable,
            (QofSetterFunc)gncEntrySetBillTaxable
        },
        {
            ENTRY_BILL_TAX_INC, QOF_TYPE_BOOLEAN, (QofAccessFunc)gncEntryGetBillTaxIncluded,
            (QofSetterFunc)gncEntrySetBillTaxIncluded
        },
        { ENTRY_BILLABLE, QOF_TYPE_BOOLEAN, (QofAccessFunc)gncEntryGetBillable, (QofSetterFunc)gncEntrySetBillable },
        { ENTRY_BILLTO, GNC_ID_OWNER, (QofAccessFunc)gncEntryGetBillTo, (QofSetterFunc)gncEntrySetBillTo },
        { ENTRY_ORDER, GNC_ID_ORDER, (QofAccessFunc)gncEntryGetOrder, NULL },
        { QOF_PARAM_BOOK, QOF_ID_BOOK, (QofAccessFunc)qof_instance_get_book, NULL },
        { QOF_PARAM_GUID, QOF_TYPE_GUID, (QofAccessFunc)qof_instance_get_guid, NULL },
        { NULL },
    };

    qof_class_register (_GNC_MOD_NAME, (QofSortFunc)gncEntryCompare, params);

    return qof_object_register (&gncEntryDesc);
}
