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

#include "config.h"

#include <glib.h>

#include "gnc-commodity.h"

#include "gncEntry.h"
#include "gncEntryP.h"
#include "gncInvoice.h"
#include "gncOrder.h"

/* GObject declarations */

static void gnc_entry_class_init(GncEntryClass *klass);
static void gnc_entry_init(GncEntry *sp);
static void gnc_entry_finalize(GObject *object);

struct _GncEntryPrivate
{
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

typedef struct _GncEntrySignal GncEntrySignal;
typedef enum _GncEntrySignalType GncEntrySignalType;

enum _GncEntrySignalType {
	/* Signals */
	LAST_SIGNAL
};

/* properties */
enum
{
        PROP_0
};

struct _GncEntrySignal {
	GncEntry *object;
};

static guint gnc_entry_signals[LAST_SIGNAL] = { 0 };
static GObjectClass *parent_class = NULL;

GType
gnc_entry_get_type()
{
	static GType type = 0;

	if(type == 0) {
		static const GTypeInfo our_info = {
			sizeof (GncEntryClass),
			NULL,
			NULL,
			(GClassInitFunc)gnc_entry_class_init,
			NULL,
			NULL,
			sizeof (GncEntry),
			0,
			(GInstanceInitFunc)gnc_entry_init,
		};

		type = g_type_register_static(QOF_TYPE_INSTANCE, 
			"GncEntry", &our_info, 0);
	}

	return type;
}

static void
gnc_entry_class_init(GncEntryClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS(klass);

	parent_class = g_type_class_peek_parent(klass);
	object_class->finalize = gnc_entry_finalize;
	object_class->set_property = gnc_entry_set_property;
    object_class->get_property = gnc_entry_get_property;

	/* Install properties */
	
	/* Create signals here:*/
 	
}

static void
gnc_entry_init(GncEntry *obj)
{
	/* Initialize private members, etc. */
}

static void
gnc_entry_finalize(GObject *object)
{
	
	/* Free private members, etc. */
	
	G_OBJECT_CLASS(parent_class)->finalize(object);
}

static void
gnc_entry_set_property (GObject *object,
				  guint param_id,
				  const GValue *value,
				  GParamSpec *pspec)
{
	GncEntry *obj;
	
	obj = GNC_ENTRY (object);
	switch (param_id) {		
		default:
   			/* We don't have any other property... */
    		G_OBJECT_WARN_INVALID_PROPERTY_ID(object,property_id,pspec);
    	break;
	}
}

static void
gnc_entry_get_property (GObject      *object,
                        guint         property_id,
                        GValue       *value,
                        GParamSpec   *pspec)
{
  GncEntry *obj;
  
  obj = GNC_ENTRY (object);

  switch (property_id) {
  default:
    /* We don't have any other property... */
    G_OBJECT_WARN_INVALID_PROPERTY_ID(object,property_id,pspec);
    break;
  }
}



static QofLogModule log_module = GNC_MOD_BUSINESS;

/* You must edit the functions in this block in tandem.  KEEP THEM IN
   SYNC! */

#define GNC_RETURN_ENUM_AS_STRING(x,s) case (x): return (s);
const char *
gncEntryDiscountHowToString (GncDiscountHow how)
{
  switch(how)
  {
    GNC_RETURN_ENUM_AS_STRING(GNC_DISC_PRETAX, "PRETAX");
    GNC_RETURN_ENUM_AS_STRING(GNC_DISC_SAMETIME, "SAMETIME");
    GNC_RETURN_ENUM_AS_STRING(GNC_DISC_POSTTAX, "POSTTAX");
    default:
      g_warning ("asked to translate unknown discount-how %d.\n", how);
      break;
  }
  return(NULL);
}

const char * gncEntryPaymentTypeToString (GncEntryPaymentType type)
{
  switch(type)
  {
    GNC_RETURN_ENUM_AS_STRING(GNC_PAYMENT_CASH, "CASH");
    GNC_RETURN_ENUM_AS_STRING(GNC_PAYMENT_CARD, "CARD");
    default:
      g_warning ("asked to translate unknown payment type %d.\n", type);
      break;
  }
  return(NULL);
}
#undef GNC_RETURN_ENUM_AS_STRING
#define GNC_RETURN_ON_MATCH(s,x,r) \
  if(safe_strcmp((s), (str)) == 0) { *(r) = x; return(TRUE); }
gboolean gncEntryDiscountStringToHow (const char *str, GncDiscountHow *how)
{
  GNC_RETURN_ON_MATCH ("PRETAX", GNC_DISC_PRETAX, how);
  GNC_RETURN_ON_MATCH ("SAMETIME", GNC_DISC_SAMETIME, how);
  GNC_RETURN_ON_MATCH ("POSTTAX", GNC_DISC_POSTTAX, how);
  g_warning ("asked to translate unknown discount-how string %s.\n",
       str ? str : "(null)");

  return(FALSE);
}
gboolean gncEntryPaymentStringToType (const char *str, GncEntryPaymentType *type)
{
  GNC_RETURN_ON_MATCH ("CASH", GNC_PAYMENT_CASH, type);
  GNC_RETURN_ON_MATCH ("CARD", GNC_PAYMENT_CARD, type);
  g_warning ("asked to translate unknown discount-how string %s.\n",
       str ? str : "(null)");

  return(FALSE);
}
#undef GNC_RETURN_ON_MATCH

#define _GNC_MOD_NAME	GNC_ID_ENTRY

#define SET_STR(obj, member, str) { \
	char * tmp; \
	\
	if (!safe_strcmp (member, str)) return; \
	gncEntryBeginEdit (obj); \
	tmp = CACHE_INSERT (str); \
	CACHE_REMOVE (member); \
	member = tmp; \
	}

G_INLINE_FUNC void mark_entry (GncEntry *entry);
void mark_entry (GncEntry *entry)
{
  qof_instance_set_dirty(GNC_INSTANCE (entry));
  qof_event_gen (GNC_ENTITY (entry), QOF_EVENT_MODIFY, NULL);
}

/* ================================================================ */
/* Create/Destroy Functions */

GncEntry *gncEntryCreate (QofBook *book)
{
  GncEntry *entry;
  gnc_numeric zero = gnc_numeric_zero ();

  if (!book) return NULL;

  entry = GNC_TYPE_ENTRY (g_object_new (GNC_TYPE_ENTRY, NULL));
  qof_instance_init (GNC_INSTANCE (entry), _GNC_MOD_NAME, book);
  
  entry->priv = g_new0 (GncEntry, 1);
  entry->priv->desc = CACHE_INSERT ("");
  entry->priv->action = CACHE_INSERT ("");
  entry->priv->notes = CACHE_INSERT ("");
  entry->priv->quantity = zero;

  entry->priv->i_price = zero;
  entry->priv->i_taxable = TRUE;
  entry->priv->i_discount = zero;
  entry->priv->i_disc_type = GNC_AMT_TYPE_PERCENT;
  entry->priv->i_disc_how = GNC_DISC_PRETAX;

  entry->priv->b_price = zero;
  entry->priv->b_taxable = TRUE;
  entry->priv->billto.type = GNC_OWNER_CUSTOMER;
  entry->priv->b_payment = GNC_PAYMENT_CASH;

  entry->priv->values_dirty = TRUE;

  qof_event_gen (GNC_ENTITY (entry), QOF_EVENT_CREATE, NULL);

  return entry;
}

void gncEntryDestroy (GncEntry *entry)
{
  if (!entry) return;
  entry->priv->inst.do_free = TRUE;
  gncEntryCommitEdit(entry);
}

static void gncEntryFree (GncEntry *entry)
{
  if (!entry) return;

  qof_event_gen (GNC_ENTITY (entry), QOF_EVENT_DESTROY, NULL);

  CACHE_REMOVE (entry->priv->desc);
  CACHE_REMOVE (entry->priv->action);
  CACHE_REMOVE (entry->priv->notes);
  if (entry->priv->i_tax_values)
    gncAccountValueDestroy (entry->priv->i_tax_values);
  if (entry->priv->b_tax_values)
    gncAccountValueDestroy (entry->priv->b_tax_values);
  if (entry->priv->i_tax_table)
    gncTaxTableDecRef (entry->priv->i_tax_table);
  if (entry->priv->b_tax_table)
    gncTaxTableDecRef (entry->priv->b_tax_table);

  qof_instance_release (GNC_INSTANCE (entry));
}

GncEntry *
gncCloneEntry (GncEntry *from, QofBook *book)
{
  /* XXX unfinished */
  return NULL;
}

GncEntry *
gncEntryObtainTwin (GncEntry *from, QofBook *book)
{
  GncEntry *entry;
  if (!book) return NULL;

  entry = (GncEntry *) qof_instance_lookup_twin (QOF_INSTANCE(from), book);
  if (!entry)
  {
    entry = gncCloneEntry (from, book);
  }

  return entry;
}


/* ================================================================ */
/* Set Functions */

void gncEntrySetDate (GncEntry *entry, Timespec date)
{
  gboolean first_date = FALSE;
  Timespec zero_time = { 0, 0 };

  if (!entry) return;
  if (timespec_equal (&entry->priv->date, &date)) return;
  if (timespec_equal (&entry->priv->date, &zero_time))
    first_date = TRUE;
  gncEntryBeginEdit (entry);
  entry->priv->date = date;
  mark_entry (entry);
  gncEntryCommitEdit (entry);

  /* Don't re-sort the first time we set the date on this entry */
  if (!first_date) {
    if (entry->priv->invoice)
      gncInvoiceSortEntries(entry->priv->invoice);
    if (entry->priv->bill)
      gncInvoiceSortEntries(entry->priv->bill);
  }
}

void gncEntrySetDateEntered (GncEntry *entry, Timespec date)
{
  if (!entry) return;
  if (timespec_equal (&entry->priv->date_entered, &date)) return;
  gncEntryBeginEdit (entry);
  entry->priv->date_entered = date;
  mark_entry (entry);
  gncEntryCommitEdit (entry);
}

void gncEntrySetDescription (GncEntry *entry, const char *desc)
{
  if (!entry || !desc) return;
  SET_STR (entry, entry->priv->desc, desc);
  mark_entry (entry);
  gncEntryCommitEdit (entry);
}

void gncEntrySetAction (GncEntry *entry, const char *action)
{
  if (!entry || !action) return;
  SET_STR (entry,entry->priv->action, action);
  mark_entry (entry);
  gncEntryCommitEdit (entry);
}

void gncEntrySetNotes (GncEntry *entry, const char *notes)
{
  if (!entry || !notes) return;
  SET_STR (entry, entry->priv->notes, notes);
  mark_entry (entry);
  gncEntryCommitEdit (entry);
}

void gncEntrySetQuantity (GncEntry *entry, gnc_numeric quantity)
{
  if (!entry) return;
  if (gnc_numeric_eq (entry->priv->quantity, quantity)) return;
  gncEntryBeginEdit (entry);
  entry->priv->quantity = quantity;
  entry->priv->values_dirty = TRUE;
  mark_entry (entry);
  gncEntryCommitEdit (entry);
}

/* Customer Invoices */

void gncEntrySetInvAccount (GncEntry *entry, Account *acc)
{
  if (!entry) return;
  if (entry->priv->i_account == acc) return;
  gncEntryBeginEdit (entry);
  entry->priv->i_account = acc;
  mark_entry (entry);
  gncEntryCommitEdit (entry);
}

void gncEntrySetInvPrice (GncEntry *entry, gnc_numeric price)
{
  if (!entry) return;
  if (gnc_numeric_eq (entry->priv->i_price, price)) return;
  gncEntryBeginEdit (entry);
  entry->priv->i_price = price;
  entry->priv->values_dirty = TRUE;
  mark_entry (entry);
  gncEntryCommitEdit (entry);
}

void gncEntrySetInvTaxable (GncEntry *entry, gboolean taxable)
{
  if (!entry) return;
  if (entry->priv->i_taxable == taxable) return;
  gncEntryBeginEdit (entry);
  entry->priv->i_taxable = taxable;
  entry->priv->values_dirty = TRUE;
  mark_entry (entry);
  gncEntryCommitEdit (entry);
}

void gncEntrySetInvTaxIncluded (GncEntry *entry, gboolean taxincluded)
{
  if (!entry) return;
  if (entry->priv->i_taxincluded == taxincluded) return;
  gncEntryBeginEdit (entry);
  entry->priv->i_taxincluded = taxincluded;
  entry->priv->values_dirty = TRUE;
  mark_entry (entry);
  gncEntryCommitEdit (entry);
}

void gncEntrySetInvTaxTable (GncEntry *entry, GncTaxTable *table)
{
  if (!entry) return;
  if (entry->priv->i_tax_table == table) return;
  gncEntryBeginEdit (entry);
  if (entry->priv->i_tax_table)
    gncTaxTableDecRef (entry->priv->i_tax_table);
  if (table)
    gncTaxTableIncRef (table);
  entry->priv->i_tax_table = table;
  entry->priv->values_dirty = TRUE;
  mark_entry (entry);
  gncEntryCommitEdit (entry);
}

void gncEntrySetInvDiscount (GncEntry *entry, gnc_numeric discount)
{
  if (!entry) return;
  if (gnc_numeric_eq (entry->priv->i_discount, discount)) return;
  gncEntryBeginEdit (entry);
  entry->priv->i_discount = discount;
  entry->priv->values_dirty = TRUE;
  mark_entry (entry);
  gncEntryCommitEdit (entry);
}

void gncEntrySetInvDiscountType (GncEntry *entry, GncAmountType type)
{
  if (!entry) return;
  if (entry->priv->i_disc_type == type) return;

  gncEntryBeginEdit (entry);
  entry->priv->i_disc_type = type;
  entry->priv->values_dirty = TRUE;
  mark_entry (entry);
  gncEntryCommitEdit (entry);
}

void gncEntrySetInvDiscountHow (GncEntry *entry, GncDiscountHow how)
{
  if (!entry) return;
  if (entry->priv->i_disc_how == how) return;

  gncEntryBeginEdit (entry);
  entry->priv->i_disc_how = how;
  entry->priv->values_dirty = TRUE;
  mark_entry (entry);
  gncEntryCommitEdit (entry);
}

void qofEntrySetInvDiscType (GncEntry *entry, const char *type_string)
{
	GncAmountType type;

	if (!entry) return;
	gncAmountStringToType(type_string, &type);
	if (entry->priv->i_disc_type == type) return;
	gncEntryBeginEdit (entry);
	entry->priv->i_disc_type = type;
	entry->priv->values_dirty = TRUE;
	mark_entry (entry);
	gncEntryCommitEdit (entry);

}

void qofEntrySetInvDiscHow  (GncEntry *entry, const char *type)
{
	GncDiscountHow how;

	if (!entry) return;
	gncEntryBeginEdit (entry);
	gncEntryDiscountStringToHow(type, &how);
	if (entry->priv->i_disc_how == how) return;
	entry->priv->i_disc_how = how;
	entry->priv->values_dirty = TRUE;
	mark_entry (entry);
	gncEntryCommitEdit (entry);
}

/* Vendor Bills */

void gncEntrySetBillAccount (GncEntry *entry, Account *acc)
{
  if (!entry) return;
  if (entry->priv->b_account == acc) return;
  gncEntryBeginEdit (entry);
  entry->priv->b_account = acc;
  mark_entry (entry);
  gncEntryCommitEdit (entry);
}

void gncEntrySetBillPrice (GncEntry *entry, gnc_numeric price)
{
  if (!entry) return;
  if (gnc_numeric_eq (entry->priv->b_price, price)) return;
  gncEntryBeginEdit (entry);
  entry->priv->b_price = price;
  entry->priv->values_dirty = TRUE;
  mark_entry (entry);
  gncEntryCommitEdit (entry);
}

void gncEntrySetBillTaxable (GncEntry *entry, gboolean taxable)
{
  if (!entry) return;
  if (entry->priv->b_taxable == taxable) return;
  gncEntryBeginEdit (entry);
  entry->priv->b_taxable = taxable;
  entry->priv->values_dirty = TRUE;
  mark_entry (entry);
  gncEntryCommitEdit (entry);
}

void gncEntrySetBillTaxIncluded (GncEntry *entry, gboolean taxincluded)
{
  if (!entry) return;
  if (entry->priv->b_taxincluded == taxincluded) return;
  gncEntryBeginEdit (entry);
  entry->priv->b_taxincluded = taxincluded;
  entry->priv->values_dirty = TRUE;
  mark_entry (entry);
  gncEntryCommitEdit (entry);
}

void gncEntrySetBillTaxTable (GncEntry *entry, GncTaxTable *table)
{
  if (!entry) return;
  if (entry->priv->b_tax_table == table) return;
  gncEntryBeginEdit (entry);
  if (entry->priv->b_tax_table)
    gncTaxTableDecRef (entry->priv->b_tax_table);
  if (table)
    gncTaxTableIncRef (table);
  entry->priv->b_tax_table = table;
  entry->priv->values_dirty = TRUE;
  mark_entry (entry);
  gncEntryCommitEdit (entry);
}

void gncEntrySetBillable (GncEntry *entry, gboolean billable)
{
  if (!entry) return;
  if (entry->priv->billable == billable) return;

  gncEntryBeginEdit (entry);
  entry->priv->billable = billable;
  mark_entry (entry);
  gncEntryCommitEdit (entry);
}

void gncEntrySetBillTo (GncEntry *entry, GncOwner *billto)
{
  if (!entry || !billto) return;
  if (gncOwnerEqual (&entry->priv->billto, billto)) return;

  gncEntryBeginEdit (entry);
  gncOwnerCopy (billto, &entry->priv->billto);
  mark_entry (entry);
  gncEntryCommitEdit (entry);
}

void gncEntrySetBillPayment (GncEntry *entry, GncEntryPaymentType type)
{
  if (!entry) return;
  if (entry->priv->b_payment == type) return;
  gncEntryBeginEdit (entry);
  entry->priv->b_payment = type;
  mark_entry (entry);
  gncEntryCommitEdit (entry);
}

/* Called from gncOrder when we're added to the Order */
void gncEntrySetOrder (GncEntry *entry, GncOrder *order)
{
  if (!entry) return;
  if (entry->priv->order == order) return;
  gncEntryBeginEdit (entry);
  entry->priv->order = order;
  mark_entry (entry);
  gncEntryCommitEdit (entry);

  /* Generate an event modifying the Order's end-owner */
#if 0  
  qof_event_gen (gncOwnerGetEndGUID (gncOrderGetOwner (order)),
		 QOF_EVENT_MODIFY, NULL);
#endif
}

/* called from gncInvoice when we're added to the Invoice */
void gncEntrySetInvoice (GncEntry *entry, GncInvoice *invoice)
{
  if (!entry) return;
  if (entry->priv->invoice == invoice) return;
  gncEntryBeginEdit (entry);
  entry->priv->invoice = invoice;
  mark_entry (entry);
  gncEntryCommitEdit (entry);
}

/* called from gncInvoice when we're added to the Invoice/Bill */
void gncEntrySetBill (GncEntry *entry, GncInvoice *bill)
{
  if (!entry) return;
  if (entry->priv->bill == bill) return;
  gncEntryBeginEdit (entry);
  entry->priv->bill = bill;
  mark_entry (entry);
  gncEntryCommitEdit (entry);
}

void gncEntryCopy (const GncEntry *src, GncEntry *dest)
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

  if (src->order)
    gncOrderAddEntry (src->order, dest);

  if (src->invoice)
    gncInvoiceAddEntry (src->invoice, dest);

  if (src->bill)
    gncBillAddEntry (src->bill, dest);

  dest->values_dirty = TRUE;
  gncEntryCommitEdit (dest);
}

/* ================================================================ */
/* Get Functions */

Timespec gncEntryGetDate (GncEntry *entry)
{
  Timespec ts; ts.tv_sec = 0; ts.tv_nsec = 0;
  if (!entry) return ts;
  return entry->priv->date;
}

Timespec gncEntryGetDateEntered (GncEntry *entry)
{
  Timespec ts; ts.tv_sec = 0; ts.tv_nsec = 0;
  if (!entry) return ts;
  return entry->priv->date_entered;
}

const char * gncEntryGetDescription (GncEntry *entry)
{
  if (!entry) return NULL;
  return entry->priv->desc;
}

const char * gncEntryGetAction (GncEntry *entry)
{
  if (!entry) return NULL;
  return entry->priv->action;
}

const char * gncEntryGetNotes (GncEntry *entry)
{
  if (!entry) return NULL;
  return entry->priv->notes;
}

gnc_numeric gncEntryGetQuantity (GncEntry *entry)
{
  if (!entry) return gnc_numeric_zero();
  return entry->priv->quantity;
}

/* Customer Invoice */

Account * gncEntryGetInvAccount (GncEntry *entry)
{
  if (!entry) return NULL;
  return entry->priv->i_account;
}

gnc_numeric gncEntryGetInvPrice (GncEntry *entry)
{
  if (!entry) return gnc_numeric_zero();
  return entry->priv->i_price;
}

gnc_numeric gncEntryGetInvDiscount (GncEntry *entry)
{
  if (!entry) return gnc_numeric_zero();
  return entry->priv->i_discount;
}

GncAmountType gncEntryGetInvDiscountType (GncEntry *entry)
{
  if (!entry) return 0;
  return entry->priv->i_disc_type;
}

GncDiscountHow gncEntryGetInvDiscountHow (GncEntry *entry)
{
  if (!entry) return 0;
  return entry->priv->i_disc_how;
}

char* qofEntryGetInvDiscType (GncEntry *entry)
{
	char *type_string;

	if (!entry) return 0;
	type_string = g_strdup(gncAmountTypeToString(entry->priv->i_disc_type));
	return type_string;
}

char* qofEntryGetInvDiscHow (GncEntry *entry)
{
	char *type_string;

	if (!entry) return 0;
	type_string = g_strdup(gncEntryDiscountHowToString(entry->priv->i_disc_how));
	return type_string;
}

gboolean gncEntryGetInvTaxable (GncEntry *entry)
{
  if (!entry) return FALSE;
  return entry->priv->i_taxable;
}

gboolean gncEntryGetInvTaxIncluded (GncEntry *entry)
{
  if (!entry) return FALSE;
  return entry->priv->i_taxincluded;
}

GncTaxTable * gncEntryGetInvTaxTable (GncEntry *entry)
{
  if (!entry) return NULL;
  return entry->priv->i_tax_table;
}

/* vendor bills */

Account * gncEntryGetBillAccount (GncEntry *entry)
{
  if (!entry) return NULL;
  return entry->priv->b_account;
}

gnc_numeric gncEntryGetBillPrice (GncEntry *entry)
{
  if (!entry) return gnc_numeric_zero();
  return entry->priv->b_price;
}

gboolean gncEntryGetBillTaxable (GncEntry *entry)
{
  if (!entry) return FALSE;
  return entry->priv->b_taxable;
}

gboolean gncEntryGetBillTaxIncluded (GncEntry *entry)
{
  if (!entry) return FALSE;
  return entry->priv->b_taxincluded;
}

GncTaxTable * gncEntryGetBillTaxTable (GncEntry *entry)
{
  if (!entry) return NULL;
  return entry->priv->b_tax_table;
}

gboolean gncEntryGetBillable (GncEntry *entry)
{
  if (!entry) return FALSE;
  return entry->priv->billable;
}

GncOwner * gncEntryGetBillTo (GncEntry *entry)
{
  if (!entry) return NULL;
  return &entry->priv->billto;
}

GncEntryPaymentType gncEntryGetBillPayment (GncEntry* entry)
{
  if (!entry) return 0;
  return entry->priv->b_payment;
}

GncInvoice * gncEntryGetInvoice (GncEntry *entry)
{
  if (!entry) return NULL;
  return entry->priv->invoice;
}

GncInvoice * gncEntryGetBill (GncEntry *entry)
{
  if (!entry) return NULL;
  return entry->priv->bill;
}

GncOrder * gncEntryGetOrder (GncEntry *entry)
{
  if (!entry) return NULL;
  return entry->priv->order;
}

/* ================================================================ */
/*
 * This is the logic of computing the total for an Entry, so you know
 * what values to put into various Splits or to display in the ledger.
 * In other words, we combine the quantity, unit-price, discount and
 * taxes together, depending on various flags.
 *
 * There are four potental ways to combine these numbers:
 * Discount:     Pre-Tax   Post-Tax
 *   Tax   :     Included  Not-Included
 *
 * The process is relatively simple:
 *
 *  1) compute the agregate price (price*qty)
 *  2) if taxincluded, then back-compute the agregate pre-tax price
 *  3) apply discount and taxes in the appropriate order
 *  4) return the requested results.
 *
 * step 2 can be done with agregate taxes; no need to compute them all
 * unless the caller asked for the tax_value.
 *
 * Note that the returned "value" is such that value + tax == "total
 * to pay," which means in the case of tax-included that the returned
 * "value" may be less than the agregate price, even without a
 * discount.  If you want to display the tax-included value, you need
 * to add the value and taxes together.  In other words, the value is
 * the amount the merchant gets; the taxes are the amount the gov't
 * gets, and the customer pays the sum or value + taxes.
 *
 * The SCU is the denominator to convert the value.
 *
 * The discount return value is just for entertainment -- you may way
 * to let a consumer know how much they saved.
 */
void gncEntryComputeValue (gnc_numeric qty, gnc_numeric price,
			   GncTaxTable *tax_table, gboolean tax_included,
			   gnc_numeric discount, GncAmountType discount_type,
			   GncDiscountHow discount_how, int SCU,
			   gnc_numeric *value, gnc_numeric *discount_value,
			   GList **tax_value)
{
  gnc_numeric	aggregate;
  gnc_numeric	pretax;
  gnc_numeric	result;
  gnc_numeric	tax;
  gnc_numeric	percent = gnc_numeric_create (100, 1);
  gnc_numeric	tpercent = gnc_numeric_zero ();
  gnc_numeric	tvalue = gnc_numeric_zero ();

  GList * 	entries = gncTaxTableGetEntries (tax_table);
  GList * 	node;

  /* Step 1: compute the aggregate price */

  aggregate = gnc_numeric_mul (qty, price, GNC_DENOM_AUTO, GNC_DENOM_LCD);

  /* Step 2: compute the pre-tax aggregate */

  /* First, compute the aggregate tpercent and tvalue numbers */
  for (node = entries; node; node = node->next) {
    GncTaxTableEntry *entry = node->data;
    gnc_numeric amount = gncTaxTableEntryGetAmount (entry);

    switch (gncTaxTableEntryGetType (entry)) {
    case GNC_AMT_TYPE_VALUE:
      tvalue = gnc_numeric_add (tvalue, amount, GNC_DENOM_AUTO,
				GNC_DENOM_LCD);
      break;
    case GNC_AMT_TYPE_PERCENT:
      tpercent = gnc_numeric_add (tpercent, amount, GNC_DENOM_AUTO,
				  GNC_DENOM_LCD);
      break;
    default:
      g_warning ("Unknown tax type: %d", gncTaxTableEntryGetType (entry));
    }
  }
  /* now we need to convert from 5% -> .05 */
  tpercent = gnc_numeric_div (tpercent, percent, GNC_DENOM_AUTO,
			      GNC_DENOM_LCD);

  /* Next, actually compute the pre-tax aggregate value based on the
   * taxincluded flag.
   */
  if (tax_table && tax_included) {
    /* Back-compute the pre-tax aggregate value.
     * We know that aggregate = pretax + pretax*tpercent + tvalue, so
     * pretax = (aggregate-tvalue)/(1+tpercent)
     */
    pretax = gnc_numeric_sub (aggregate, tvalue, GNC_DENOM_AUTO,
			      GNC_DENOM_LCD);
    pretax = gnc_numeric_div (pretax,
			      gnc_numeric_add (tpercent,
					       gnc_numeric_create (1, 1),
					       GNC_DENOM_AUTO, GNC_DENOM_LCD),
			      GNC_DENOM_AUTO, GNC_DENOM_LCD);
  } else {
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
   * Type:	discount	tax
   * PRETAX	pretax		pretax-discount
   * SAMETIME	pretax		pretax
   * POSTTAX	pretax+tax	pretax
   */

  switch (discount_how) {
  case GNC_DISC_PRETAX:
  case GNC_DISC_SAMETIME:
    /* compute the discount from pretax */

    if (discount_type == GNC_AMT_TYPE_PERCENT) {
      discount = gnc_numeric_div (discount, percent, GNC_DENOM_AUTO, 
				  GNC_DENOM_LCD);
      discount = gnc_numeric_mul (pretax, discount, GNC_DENOM_AUTO,
				  GNC_DENOM_LCD);
    }

    result = gnc_numeric_sub (pretax, discount, GNC_DENOM_AUTO, GNC_DENOM_LCD);

    /* Figure out when to apply the tax, pretax or pretax-discount */
    if (discount_how == GNC_DISC_PRETAX)
      pretax = result;
    break;

  case GNC_DISC_POSTTAX:
    /* compute discount on pretax+taxes */

    if (discount_type == GNC_AMT_TYPE_PERCENT) {
      gnc_numeric after_tax;

      tax = gnc_numeric_mul (pretax, tpercent, GNC_DENOM_AUTO, GNC_DENOM_LCD);
      after_tax = gnc_numeric_add (pretax, tax, GNC_DENOM_AUTO, GNC_DENOM_LCD);
      after_tax = gnc_numeric_add (after_tax, tvalue, GNC_DENOM_AUTO,
				   GNC_DENOM_LCD);
      discount = gnc_numeric_div (discount, percent, GNC_DENOM_AUTO,
				  GNC_DENOM_LCD);
      discount = gnc_numeric_mul (after_tax, discount, GNC_DENOM_AUTO,
				  GNC_DENOM_LCD);
    }

    result = gnc_numeric_sub (pretax, discount, GNC_DENOM_AUTO, GNC_DENOM_LCD);
    break;

  default:
    g_warning ("unknown DiscountHow value: %d", discount_how);
  }

  /* Step 4:  return the requested results. */

  /* result == amount merchant gets
   * discount == amount of discount
   * need to compute taxes (based on 'pretax') if the caller wants it.
   */

  if (discount_value != NULL) {
    if (SCU) discount = gnc_numeric_convert(discount, SCU, GNC_RND_ROUND);
    *discount_value = discount;
  }

  if (value != NULL) {
    if (SCU) result = gnc_numeric_convert(result, SCU, GNC_RND_ROUND);
    *value = result;
  }

  /* Now... Compute the list of tax values (if the caller wants it) */

  if (tax_value != NULL) {
    GList *	taxes = NULL;

    for (node = entries; node; node = node->next) {
      GncTaxTableEntry *entry = node->data;
      Account *acc = gncTaxTableEntryGetAccount (entry);
      gnc_numeric amount = gncTaxTableEntryGetAmount (entry);

      g_return_if_fail (acc);

      switch (gncTaxTableEntryGetType (entry)) {
      case GNC_AMT_TYPE_VALUE:
	if (SCU) amount = gnc_numeric_convert(amount, SCU, GNC_RND_ROUND);
	taxes = gncAccountValueAdd (taxes, acc, amount);
	break;
      case GNC_AMT_TYPE_PERCENT:
	amount = gnc_numeric_div (amount, percent, GNC_DENOM_AUTO,
				  GNC_DENOM_LCD);
	tax = gnc_numeric_mul (pretax, amount, GNC_DENOM_AUTO, GNC_DENOM_LCD);
	if (SCU) tax = gnc_numeric_convert(tax, SCU, GNC_RND_ROUND);
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
get_entry_commodity_denom (GncEntry *entry)
{
  gnc_commodity *c;
  if (!entry)
    return 0;
  if (entry->priv->invoice) {
    c = gncInvoiceGetCurrency (entry->priv->invoice);
    if (c)
      return (gnc_commodity_get_fraction (c));
  }
  if (entry->priv->bill) {
    c = gncInvoiceGetCurrency (entry->priv->bill);
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
  if (entry->priv->i_tax_table) {
    Timespec modtime = gncTaxTableLastModified (entry->priv->i_tax_table);
    if (timespec_cmp (&entry->priv->i_taxtable_modtime, &modtime)) {
      entry->priv->values_dirty = TRUE;
      entry->priv->i_taxtable_modtime = modtime;
    }
  }
  if (entry->priv->b_tax_table) {
    Timespec modtime = gncTaxTableLastModified (entry->priv->b_tax_table);
    if (timespec_cmp (&entry->priv->b_taxtable_modtime, &modtime)) {
      entry->priv->values_dirty = TRUE;
      entry->priv->b_taxtable_modtime = modtime;
    }
  }

  if (!entry->priv->values_dirty)
    return;

  /* Clear the last-computed tax values */
  if (entry->priv->i_tax_values) {
    gncAccountValueDestroy (entry->priv->i_tax_values);
    entry->priv->i_tax_values = NULL;
  }
  if (entry->priv->b_tax_values) {
    gncAccountValueDestroy (entry->priv->b_tax_values);
    entry->priv->b_tax_values = NULL;
  }

  /* Determine the commodity denominator */
  denom = get_entry_commodity_denom (entry);

  /* Compute the invoice values */
  gncEntryComputeValue (entry->priv->quantity, entry->priv->i_price,
			(entry->priv->i_taxable ? entry->priv->i_tax_table : NULL),
			entry->priv->i_taxincluded,
			entry->priv->i_discount, entry->priv->i_disc_type,
			entry->priv->i_disc_how,
			denom,
			&(entry->priv->i_value), &(entry->priv->i_disc_value),
			&(entry->priv->i_tax_values));

  /* Compute the bill values */
  gncEntryComputeValue (entry->priv->quantity, entry->priv->b_price,
			(entry->priv->b_taxable ? entry->priv->b_tax_table : NULL),
			entry->priv->b_taxincluded,
			gnc_numeric_zero(), GNC_AMT_TYPE_VALUE, GNC_DISC_PRETAX,
			denom,
			&(entry->priv->b_value), NULL, &(entry->priv->b_tax_values));

  entry->priv->i_value_rounded = gnc_numeric_convert (entry->priv->i_value, denom,
						GNC_RND_ROUND);
  entry->priv->i_disc_value_rounded = gnc_numeric_convert (entry->priv->i_disc_value, denom,
						     GNC_RND_ROUND);
  entry->priv->i_tax_value = gncAccountValueTotal (entry->priv->i_tax_values);
  entry->priv->i_tax_value_rounded = gnc_numeric_convert (entry->priv->i_tax_value, denom,
						    GNC_RND_ROUND);

  entry->priv->b_value_rounded = gnc_numeric_convert (entry->priv->b_value, denom,
						GNC_RND_ROUND);
  entry->priv->b_tax_value = gncAccountValueTotal (entry->priv->b_tax_values);
  entry->priv->b_tax_value_rounded = gnc_numeric_convert (entry->priv->b_tax_value, denom,
						    GNC_RND_ROUND);
  entry->priv->values_dirty = FALSE;
}

void gncEntryGetValue (GncEntry *entry, gboolean is_inv, gnc_numeric *value,
		       gnc_numeric *discount_value, gnc_numeric *tax_value,
		       GList **tax_values)
{
  if (!entry) return;
  gncEntryRecomputeValues (entry);
  if (value)
    *value = (is_inv ? entry->priv->i_value : entry->priv->b_value);
  if (discount_value)
    *discount_value = (is_inv ? entry->priv->i_disc_value : gnc_numeric_zero());
  if (tax_value)
    *tax_value = (is_inv ? entry->priv->i_tax_value : entry->priv->b_tax_value);
  if (tax_values)
    *tax_values = (is_inv ? entry->priv->i_tax_values : entry->priv->b_tax_values);
}

gnc_numeric gncEntryReturnValue (GncEntry *entry, gboolean is_inv)
{
  if (!entry) return gnc_numeric_zero();
  gncEntryRecomputeValues (entry);
  return (is_inv ? entry->priv->i_value_rounded : entry->priv->b_value_rounded);
}

gnc_numeric gncEntryReturnTaxValue (GncEntry *entry, gboolean is_inv)
{
  if (!entry) return gnc_numeric_zero();
  gncEntryRecomputeValues (entry);
  return (is_inv ? entry->priv->i_tax_value_rounded : entry->priv->b_tax_value_rounded);
}

AccountValueList * gncEntryReturnTaxValues (GncEntry *entry, gboolean is_inv)
{
  if (!entry) return NULL;
  gncEntryRecomputeValues (entry);
  return (is_inv ? entry->priv->i_tax_values : entry->priv->b_tax_values);
}

gnc_numeric gncEntryReturnDiscountValue (GncEntry *entry, gboolean is_inv)
{
  if (!entry) return gnc_numeric_zero();
  gncEntryRecomputeValues (entry);
  return (is_inv ? entry->priv->i_disc_value_rounded : gnc_numeric_zero());
}

/* XXXX this exsitnace of this routine is just wrong */
gboolean gncEntryIsOpen (GncEntry *entry)
{
  if (!entry) return FALSE;
  return (entry->priv->inst.editlevel > 0);
}

/* ================================================================ */

void gncEntryBeginEdit (GncEntry *entry)
{
  QOF_BEGIN_EDIT (GNC_INSTANCE (entry));
}

static void gncEntryOnError (QofInstance *entry, QofBackendError errcode)
{
  PERR("Entry QofBackend Failure: %d", errcode);
}

static void gncEntryOnDone (QofInstance *inst) {}

static void entry_free (QofInstance *inst)
{
  GncEntry *entry = (GncEntry *)inst;
  gncEntryFree (entry);
}

void gncEntryCommitEdit (GncEntry *entry)
{
  if (!qof_commit_edit (QOF_INSTANCE(entry))) return;
  qof_commit_edit_part2 (GNC_INSTANCE (entry), gncEntryOnError,
			 gncEntryOnDone, entry_free);
}

int gncEntryCompare (GncEntry *a, GncEntry *b)
{
  int compare;

  if (a == b) return 0;
  if (!a && b) return -1;
  if (a && !b) return 1;

  compare = timespec_cmp (&(a->date), &(b->date));
  if (compare) return compare;

  compare = timespec_cmp (&(a->date_entered), &(b->date_entered));
  if (compare) return compare;

  compare = safe_strcmp (a->desc, b->desc);
  if (compare) return compare;

  compare = safe_strcmp (a->action, b->action);
  if (compare) return compare;

  return guid_compare (&(a->inst.entity.guid), &(b->inst.entity.guid));
}

/* ============================================================= */
/* Object declaration */

static QofObject gncEntryDesc = 
{
  interface_version:  QOF_OBJECT_VERSION,
  e_type:             _GNC_MOD_NAME,
  type_label:         "Order/Invoice/Bill Entry",
  create:             (gpointer)gncEntryCreate,
  book_begin:         NULL,
  book_end:           NULL,
  is_dirty:           qof_collection_is_dirty,
  mark_clean:         qof_collection_mark_clean,
  foreach:            qof_collection_foreach,
  printable:          NULL,
  version_cmp:        (int (*)(gpointer, gpointer)) qof_instance_version_cmp,
};

gboolean gncEntryRegister (void)
{
  static QofParam params[] = {
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
    { ENTRY_INV_DISC_TYPE, QOF_TYPE_STRING, (QofAccessFunc)qofEntryGetInvDiscType,
       (QofSetterFunc)qofEntrySetInvDiscType },
    { ENTRY_INV_DISC_HOW, QOF_TYPE_STRING, (QofAccessFunc)qofEntryGetInvDiscHow,
       (QofSetterFunc)qofEntrySetInvDiscHow },
    { ENTRY_INV_TAXABLE, QOF_TYPE_BOOLEAN, (QofAccessFunc)gncEntryGetInvTaxable,
       (QofSetterFunc)gncEntrySetInvTaxable },
    { ENTRY_INV_TAX_INC, QOF_TYPE_BOOLEAN, (QofAccessFunc)gncEntryGetInvTaxIncluded,
       (QofSetterFunc)gncEntrySetInvTaxIncluded },
    { ENTRY_BILL_TAXABLE, QOF_TYPE_BOOLEAN, (QofAccessFunc)gncEntryGetInvTaxable,
       (QofSetterFunc)gncEntrySetInvTaxable },
    { ENTRY_BILL_TAX_INC, QOF_TYPE_BOOLEAN, (QofAccessFunc)gncEntryGetBillTaxIncluded,
       (QofSetterFunc)gncEntrySetBillTaxIncluded },
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
