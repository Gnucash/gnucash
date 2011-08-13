/********************************************************************\
 * gncOwner.c -- Business Interface:  Object OWNERs                 *
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
 * Copyright (C) 2003 Linas Vepstas <linas@linas.org>
 * Copyright (c) 2005 Neil Williams <linux@codehelp.co.uk>
 * Copyright (c) 2006 David Hampton <hampton@employees.org>
 * Copyright (c) 2011 Geert Janssens <geert@kobaltwit.be>
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#include "config.h"

#include <glib.h>
#include <glib/gi18n.h>
#include <string.h>		/* for memcpy() */

#include "gncCustomerP.h"
#include "gncEmployeeP.h"
#include "gncJobP.h"
#include "gncOwner.h"
#include "gncOwnerP.h"
#include "gncVendorP.h"
#include "gncInvoice.h"
#include "gnc-commodity.h"
#include "Transaction.h"
#include "Split.h"

#define _GNC_MOD_NAME   GNC_ID_OWNER

#define GNC_OWNER_ID    "gncOwner"
#define GNC_OWNER_TYPE  "owner-type"
#define GNC_OWNER_GUID  "owner-guid"

GncOwner * gncOwnerNew (void)
{
    GncOwner *o;

    o = g_new0 (GncOwner, 1);
    o->type = GNC_OWNER_NONE;
    return o;
}

void gncOwnerFree (GncOwner *owner)
{
    if (!owner) return;
    g_free (owner);
}

void gncOwnerBeginEdit (GncOwner *owner)
{
    if (!owner) return;
    switch (owner->type)
    {
    case GNC_OWNER_NONE :
    case GNC_OWNER_UNDEFINED :
        break;
    case GNC_OWNER_CUSTOMER :
    {
        gncCustomerBeginEdit(owner->owner.customer);
        break;
    }
    case GNC_OWNER_JOB :
    {
        gncJobBeginEdit(owner->owner.job);
        break;
    }
    case GNC_OWNER_VENDOR :
    {
        gncVendorBeginEdit(owner->owner.vendor);
        break;
    }
    case GNC_OWNER_EMPLOYEE :
    {
        gncEmployeeBeginEdit(owner->owner.employee);
        break;
    }
    }
}


void gncOwnerCommitEdit (GncOwner *owner)
{
    if (!owner) return;
    switch (owner->type)
    {
    case GNC_OWNER_NONE :
    case GNC_OWNER_UNDEFINED :
        break;
    case GNC_OWNER_CUSTOMER :
    {
        gncCustomerCommitEdit(owner->owner.customer);
        break;
    }
    case GNC_OWNER_JOB :
    {
        gncJobCommitEdit(owner->owner.job);
        break;
    }
    case GNC_OWNER_VENDOR :
    {
        gncVendorCommitEdit(owner->owner.vendor);
        break;
    }
    case GNC_OWNER_EMPLOYEE :
    {
        gncEmployeeCommitEdit(owner->owner.employee);
        break;
    }
    }
}


void gncOwnerDestroy (GncOwner *owner)
{
    if (!owner) return;
    switch (owner->type)
    {
    case GNC_OWNER_NONE :
    case GNC_OWNER_UNDEFINED :
        break;
    case GNC_OWNER_CUSTOMER :
    {
        gncCustomerDestroy(owner->owner.customer);
        break;
    }
    case GNC_OWNER_JOB :
    {
        gncJobDestroy(owner->owner.job);
        break;
    }
    case GNC_OWNER_VENDOR :
    {
        gncVendorDestroy(owner->owner.vendor);
        break;
    }
    case GNC_OWNER_EMPLOYEE :
    {
        gncEmployeeDestroy(owner->owner.employee);
        break;
    }
    }
}

void gncOwnerInitUndefined (GncOwner *owner, gpointer obj)
{
    if (!owner) return;
    owner->type = GNC_OWNER_UNDEFINED;
    owner->owner.undefined = obj;
}

void gncOwnerInitCustomer (GncOwner *owner, GncCustomer *customer)
{
    if (!owner) return;
    owner->type = GNC_OWNER_CUSTOMER;
    owner->owner.customer = customer;
}

void gncOwnerInitJob (GncOwner *owner, GncJob *job)
{
    if (!owner) return;
    owner->type = GNC_OWNER_JOB;
    owner->owner.job = job;
}

void gncOwnerInitVendor (GncOwner *owner, GncVendor *vendor)
{
    if (!owner) return;
    owner->type = GNC_OWNER_VENDOR;
    owner->owner.vendor = vendor;
}

void gncOwnerInitEmployee (GncOwner *owner, GncEmployee *employee)
{
    if (!owner) return;
    owner->type = GNC_OWNER_EMPLOYEE;
    owner->owner.employee = employee;
}

GncOwnerType gncOwnerGetType (const GncOwner *owner)
{
    if (!owner) return GNC_OWNER_NONE;
    return owner->type;
}

QofIdTypeConst
qofOwnerGetType(const GncOwner *owner)
{
    return gncOwnerTypeToQofIdType(owner->type);
}

QofIdTypeConst gncOwnerTypeToQofIdType(GncOwnerType t)
{
    QofIdTypeConst type = NULL;
    switch (t)
    {
    case GNC_OWNER_NONE :
    {
        type = NULL;
        break;
    }
    case GNC_OWNER_UNDEFINED :
    {
        type = NULL;
        break;
    }
    case GNC_OWNER_CUSTOMER :
    {
        type = GNC_ID_CUSTOMER;
        break;
    }
    case GNC_OWNER_JOB :
    {
        type = GNC_ID_JOB;
        break;
    }
    case GNC_OWNER_VENDOR :
    {
        type = GNC_ID_VENDOR;
        break;
    }
    case GNC_OWNER_EMPLOYEE :
    {
        type = GNC_ID_EMPLOYEE;
        break;
    }
    }
    return type;
}

QofInstance*
qofOwnerGetOwner (const GncOwner *owner)
{
    QofInstance *ent;

    if (!owner)
    {
        return NULL;
    }
    ent = NULL;
    switch (owner->type)
    {
    case GNC_OWNER_NONE :
    {
        break;
    }
    case GNC_OWNER_UNDEFINED :
    {
        break;
    }
    case GNC_OWNER_CUSTOMER :
    {
        ent = QOF_INSTANCE(owner->owner.customer);
        break;
    }
    case GNC_OWNER_JOB :
    {
        ent = QOF_INSTANCE(owner->owner.job);
        break;
    }
    case GNC_OWNER_VENDOR :
    {
        ent = QOF_INSTANCE(owner->owner.vendor);
        break;
    }
    case GNC_OWNER_EMPLOYEE :
    {
        ent = QOF_INSTANCE(owner->owner.employee);
        break;
    }
    }
    return ent;
}

void
qofOwnerSetEntity (GncOwner *owner, QofInstance *ent)
{
    if (!owner || !ent)
    {
        return;
    }
    if (0 == safe_strcmp(ent->e_type, GNC_ID_CUSTOMER))
    {
        owner->type = GNC_OWNER_CUSTOMER;
        gncOwnerInitCustomer(owner, (GncCustomer*)ent);
    }
    else if (0 == safe_strcmp(ent->e_type, GNC_ID_JOB))
    {
        owner->type = GNC_OWNER_JOB;
        gncOwnerInitJob(owner, (GncJob*)ent);
    }
    else if (0 == safe_strcmp(ent->e_type, GNC_ID_VENDOR))
    {
        owner->type = GNC_OWNER_VENDOR;
        gncOwnerInitVendor(owner, (GncVendor*)ent);
    }
    else if (0 == safe_strcmp(ent->e_type, GNC_ID_EMPLOYEE))
    {
        owner->type = GNC_OWNER_EMPLOYEE;
        gncOwnerInitEmployee(owner, (GncEmployee*)ent);
    }
    else
    {
        owner->type = GNC_OWNER_NONE;
        owner->owner.undefined = NULL;
    }
}

gpointer gncOwnerGetUndefined (const GncOwner *owner)
{
    if (!owner) return NULL;
    if (owner->type != GNC_OWNER_UNDEFINED) return NULL;
    return owner->owner.undefined;
}

GncCustomer * gncOwnerGetCustomer (const GncOwner *owner)
{
    if (!owner) return NULL;
    if (owner->type != GNC_OWNER_CUSTOMER) return NULL;
    return owner->owner.customer;
}

GncJob * gncOwnerGetJob (const GncOwner *owner)
{
    if (!owner) return NULL;
    if (owner->type != GNC_OWNER_JOB) return NULL;
    return owner->owner.job;
}

GncVendor * gncOwnerGetVendor (const GncOwner *owner)
{
    if (!owner) return NULL;
    if (owner->type != GNC_OWNER_VENDOR) return NULL;
    return owner->owner.vendor;
}

GncEmployee * gncOwnerGetEmployee (const GncOwner *owner)
{
    if (!owner) return NULL;
    if (owner->type != GNC_OWNER_EMPLOYEE) return NULL;
    return owner->owner.employee;
}

void gncOwnerCopy (const GncOwner *src, GncOwner *dest)
{
    if (!src || !dest) return;
    if (src == dest) return;
    memcpy (dest, src, sizeof (*dest));
}

GncOwner
gncCloneOwner (const GncOwner *from, QofBook *book)
{
    GncOwner owner = { GNC_OWNER_NONE };
    if (!from) return owner;
    owner.type = from->type;
    switch (from->type)
    {
    case GNC_OWNER_NONE:
        return owner;
    case GNC_OWNER_UNDEFINED:
        owner.owner.undefined = from->owner.undefined;  /* XXX probably wrong ! */
        return owner;
    case GNC_OWNER_CUSTOMER:
        owner.owner.customer = gncCustomerObtainTwin (from->owner.customer, book);
        return owner;
    case GNC_OWNER_JOB:
        owner.owner.job = gncJobObtainTwin (from->owner.job, book);
        return owner;
    case GNC_OWNER_VENDOR:
        owner.owner.vendor = gncVendorObtainTwin (from->owner.vendor, book);
        return owner;
    case GNC_OWNER_EMPLOYEE:
        owner.owner.employee = gncEmployeeObtainTwin (from->owner.employee, book);
        return owner;
    default:
        return owner;
    }
}

gboolean gncOwnerEqual (const GncOwner *a, const GncOwner *b)
{
    if (!a || !b) return FALSE;
    if (gncOwnerGetType (a) != gncOwnerGetType (b)) return FALSE;
    return (a->owner.undefined == b->owner.undefined);
}

int gncOwnerGCompareFunc (const GncOwner *a, const GncOwner *b)
{
    if (gncOwnerEqual (a, b))
        return 0;
    else
        return 1;
}

const char * gncOwnerGetID (const GncOwner *owner)
{
    if (!owner) return NULL;
    switch (owner->type)
    {
    case GNC_OWNER_NONE:
    case GNC_OWNER_UNDEFINED:
    default:
        return NULL;
    case GNC_OWNER_CUSTOMER:
        return gncCustomerGetID (owner->owner.customer);
    case GNC_OWNER_JOB:
        return gncJobGetID (owner->owner.job);
    case GNC_OWNER_VENDOR:
        return gncVendorGetID (owner->owner.vendor);
    case GNC_OWNER_EMPLOYEE:
        return gncEmployeeGetID (owner->owner.employee);
    }
}

const char * gncOwnerGetName (const GncOwner *owner)
{
    if (!owner) return NULL;
    switch (owner->type)
    {
    case GNC_OWNER_NONE:
    case GNC_OWNER_UNDEFINED:
    default:
        return NULL;
    case GNC_OWNER_CUSTOMER:
        return gncCustomerGetName (owner->owner.customer);
    case GNC_OWNER_JOB:
        return gncJobGetName (owner->owner.job);
    case GNC_OWNER_VENDOR:
        return gncVendorGetName (owner->owner.vendor);
    case GNC_OWNER_EMPLOYEE:
        return gncAddressGetName(gncEmployeeGetAddr (owner->owner.employee));
    }
}

GncAddress * gncOwnerGetAddr (const GncOwner *owner)
{
    if (!owner) return NULL;
    switch (owner->type)
    {
    case GNC_OWNER_NONE:
    case GNC_OWNER_UNDEFINED:
    case GNC_OWNER_JOB:
    default:
        return NULL;
    case GNC_OWNER_CUSTOMER:
        return gncCustomerGetAddr (owner->owner.customer);
    case GNC_OWNER_VENDOR:
        return gncVendorGetAddr (owner->owner.vendor);
    case GNC_OWNER_EMPLOYEE:
        return gncEmployeeGetAddr (owner->owner.employee);
    }
}

gnc_commodity * gncOwnerGetCurrency (const GncOwner *owner)
{
    if (!owner) return NULL;
    switch (owner->type)
    {
    case GNC_OWNER_NONE:
    case GNC_OWNER_UNDEFINED:
    default:
        return NULL;
    case GNC_OWNER_CUSTOMER:
        return gncCustomerGetCurrency (owner->owner.customer);
    case GNC_OWNER_VENDOR:
        return gncVendorGetCurrency (owner->owner.vendor);
    case GNC_OWNER_EMPLOYEE:
        return gncEmployeeGetCurrency (owner->owner.employee);
    case GNC_OWNER_JOB:
        return gncOwnerGetCurrency (gncJobGetOwner (owner->owner.job));
    }
}

gboolean gncOwnerGetActive (const GncOwner *owner)
{
    if (!owner) return FALSE;
    switch (owner->type)
    {
    case GNC_OWNER_NONE:
    case GNC_OWNER_UNDEFINED:
    default:
        return FALSE;
    case GNC_OWNER_CUSTOMER:
        return gncCustomerGetActive (owner->owner.customer);
    case GNC_OWNER_VENDOR:
        return gncVendorGetActive (owner->owner.vendor);
    case GNC_OWNER_EMPLOYEE:
        return gncEmployeeGetActive (owner->owner.employee);
    case GNC_OWNER_JOB:
        return gncJobGetActive (owner->owner.job);
    }
}

const GncGUID * gncOwnerGetGUID (const GncOwner *owner)
{
    if (!owner) return NULL;

    switch (owner->type)
    {
    case GNC_OWNER_NONE:
    case GNC_OWNER_UNDEFINED:
    default:
        return NULL;
    case GNC_OWNER_CUSTOMER:
        return qof_instance_get_guid (QOF_INSTANCE(owner->owner.customer));
    case GNC_OWNER_JOB:
        return qof_instance_get_guid (QOF_INSTANCE(owner->owner.job));
    case GNC_OWNER_VENDOR:
        return qof_instance_get_guid (QOF_INSTANCE(owner->owner.vendor));
    case GNC_OWNER_EMPLOYEE:
        return qof_instance_get_guid (QOF_INSTANCE(owner->owner.employee));
    }
}

void
gncOwnerSetName (const GncOwner *owner, const gchar *name)
{
    if (!owner) return;
    switch (owner->type)
    {
    case GNC_OWNER_CUSTOMER:
        gncCustomerSetName (owner->owner.customer, name);
        break;
    case GNC_OWNER_VENDOR:
        gncVendorSetName (owner->owner.vendor, name);
        break;
    case GNC_OWNER_EMPLOYEE:
        gncAddressSetName (gncEmployeeGetAddr (owner->owner.employee), name);
        break;
    case GNC_OWNER_JOB:
        gncJobSetName (owner->owner.job, name);
        break;
    case GNC_OWNER_NONE:
    case GNC_OWNER_UNDEFINED:
    default:
        break;
    }
}

void
gncOwnerSetActive (const GncOwner *owner, gboolean active)
{
    if (!owner) return;
    switch (owner->type)
    {
    case GNC_OWNER_CUSTOMER:
        gncCustomerSetActive (owner->owner.customer, active);
        break;
    case GNC_OWNER_VENDOR:
        gncVendorSetActive (owner->owner.vendor, active);
        break;
    case GNC_OWNER_EMPLOYEE:
        gncEmployeeSetActive (owner->owner.employee, active);
        break;
    case GNC_OWNER_JOB:
        gncJobSetActive (owner->owner.job, active);
        break;
    case GNC_OWNER_NONE:
    case GNC_OWNER_UNDEFINED:
    default:
        break;
    }
}

GncGUID gncOwnerRetGUID (GncOwner *owner)
{
    const GncGUID *guid = gncOwnerGetGUID (owner);
    if (guid)
        return *guid;
    return *guid_null ();
}

GncOwner * gncOwnerGetEndOwner (GncOwner *owner)
{
    if (!owner) return NULL;
    switch (owner->type)
    {
    case GNC_OWNER_NONE:
    case GNC_OWNER_UNDEFINED:
    default:
        return NULL;
    case GNC_OWNER_CUSTOMER:
    case GNC_OWNER_VENDOR:
    case GNC_OWNER_EMPLOYEE:
        return owner;
    case GNC_OWNER_JOB:
        return gncJobGetOwner (owner->owner.job);
    }
}

int gncOwnerCompare (const GncOwner *a, const GncOwner *b)
{
    if (!a && !b) return 0;
    if (!a && b) return 1;
    if (a && !b) return -1;

    if (a->type != b->type)
        return (a->type - b->type);

    switch (a->type)
    {
    case GNC_OWNER_NONE:
    case GNC_OWNER_UNDEFINED:
    default:
        return 0;
    case GNC_OWNER_CUSTOMER:
        return gncCustomerCompare (a->owner.customer, b->owner.customer);
    case GNC_OWNER_VENDOR:
        return gncVendorCompare (a->owner.vendor, b->owner.vendor);
    case GNC_OWNER_EMPLOYEE:
        return gncEmployeeCompare (a->owner.employee, b->owner.employee);
    case GNC_OWNER_JOB:
        return gncJobCompare (a->owner.job, b->owner.job);
    }
}

const GncGUID * gncOwnerGetEndGUID (GncOwner *owner)
{
    if (!owner) return NULL;
    owner = gncOwnerGetEndOwner (owner);
    return gncOwnerGetGUID (owner);
}

void gncOwnerAttachToLot (const GncOwner *owner, GNCLot *lot)
{
    KvpFrame *kvp;
    KvpValue *value;

    if (!owner || !lot)
        return;

    kvp = gnc_lot_get_slots (lot);

    value = kvp_value_new_gint64 (gncOwnerGetType (owner));
    kvp_frame_set_slot_path (kvp, value, GNC_OWNER_ID, GNC_OWNER_TYPE, NULL);
    kvp_value_delete (value);

    value = kvp_value_new_guid (gncOwnerGetGUID (owner));
    kvp_frame_set_slot_path (kvp, value, GNC_OWNER_ID, GNC_OWNER_GUID, NULL);
    kvp_value_delete (value);

}

gboolean gncOwnerGetOwnerFromLot (GNCLot *lot, GncOwner *owner)
{
    KvpFrame *kvp;
    KvpValue *value;
    GncGUID *guid;
    QofBook *book;
    GncOwnerType type;

    if (!lot || !owner) return FALSE;

    book = gnc_lot_get_book (lot);
    kvp = gnc_lot_get_slots (lot);

    value = kvp_frame_get_slot_path (kvp, GNC_OWNER_ID, GNC_OWNER_TYPE, NULL);
    if (!value) return FALSE;

    type = kvp_value_get_gint64 (value);

    value = kvp_frame_get_slot_path (kvp, GNC_OWNER_ID, GNC_OWNER_GUID, NULL);
    if (!value) return FALSE;

    guid = kvp_value_get_guid (value);
    if (!guid)
        return FALSE;

    switch (type)
    {
    case GNC_OWNER_CUSTOMER:
        gncOwnerInitCustomer (owner, gncCustomerLookup (book, guid));
        break;
    case GNC_OWNER_VENDOR:
        gncOwnerInitVendor (owner, gncVendorLookup (book, guid));
        break;
    case GNC_OWNER_EMPLOYEE:
        gncOwnerInitEmployee (owner, gncEmployeeLookup (book, guid));
        break;
    case GNC_OWNER_JOB:
        gncOwnerInitJob (owner, gncJobLookup (book, guid));
        break;
    default:
        return FALSE;
    }

    return (owner->owner.undefined != NULL);
}

gboolean gncOwnerIsValid (const GncOwner *owner)
{
    if (!owner) return FALSE;
    return (owner->owner.undefined != NULL);
}

KvpFrame* gncOwnerGetSlots(GncOwner* owner)
{
    if (!owner) return NULL;

    switch (gncOwnerGetType(owner))
    {
    case GNC_OWNER_CUSTOMER:
    case GNC_OWNER_VENDOR:
    case GNC_OWNER_EMPLOYEE:
        return qof_instance_get_slots(QOF_INSTANCE(owner->owner.undefined));
    case GNC_OWNER_JOB:
        return gncOwnerGetSlots(gncJobGetOwner(gncOwnerGetJob(owner)));
    default:
        return NULL;
    }
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
        payment_value = gnc_numeric_mul(amount, exch, GNC_DENOM_AUTO, GNC_HOW_RND_ROUND_HALF_UP);
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
        payment_value = gnc_numeric_sub (payment_value, split_amt, GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD);

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
            qof_event_gen (QOF_INSTANCE(&this_invoice), QOF_EVENT_MODIFY, NULL);

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

GList *
gncOwnerGetAccountTypesList (const GncOwner *owner)
{
    g_return_val_if_fail (owner, NULL);

    switch (gncOwnerGetType (owner))
    {
    case GNC_OWNER_CUSTOMER:
        return (g_list_prepend (NULL, (gpointer)ACCT_TYPE_RECEIVABLE));
    case GNC_OWNER_VENDOR:
    case GNC_OWNER_EMPLOYEE:
        return (g_list_prepend (NULL, (gpointer)ACCT_TYPE_PAYABLE));
        break;
    default:
        return (g_list_prepend (NULL, (gpointer)ACCT_TYPE_NONE));
    }
}

GList *
gncOwnerGetCommoditiesList (const GncOwner *owner)
{
    g_return_val_if_fail (owner, NULL);
    g_return_val_if_fail (gncOwnerGetCurrency(owner), NULL);

    return (g_list_prepend (NULL, gncOwnerGetCurrency(owner)));
}

/*********************************************************************/
/* Owner balance calculation routines                                */

/*
 * Given an owner, extract the open balance from the owner and then
 * convert it to the desired currency.
 */
gnc_numeric
gncOwnerGetBalanceInCurrency (GncOwner *owner,
                              const gnc_commodity *report_currency)
{
    gnc_numeric balance = gnc_numeric_zero ();
    GList *acct_list, *acct_node, *acct_types, *lot_list = NULL, *lot_node;
    QofBook *book;
    gnc_commodity *owner_currency;
    GNCPriceDB *pdb;

    g_return_val_if_fail (owner, gnc_numeric_zero ());

    /* Get account list */
    book       = qof_instance_get_book (qofOwnerGetOwner (owner));
    acct_list  = gnc_account_get_descendants (gnc_book_get_root_account (book));
    acct_types = gncOwnerGetAccountTypesList (owner);
    owner_currency = gncOwnerGetCurrency (owner);

    /* For each account */
    for (acct_node = acct_list; acct_node; acct_node = acct_node->next)
    {
        Account *account = acct_node->data;

        /* Check if this account can have lots for the owner, otherwise skip to next */
        if (g_list_index (acct_types, (gpointer)xaccAccountGetType (account))
                == -1)
            continue;


        if (!gnc_commodity_equal (owner_currency, xaccAccountGetCommodity (account)))
            continue;

        /* Get a list of open lots for this owner and account */
        lot_list = xaccAccountFindOpenLots (account, gnc_lot_match_invoice_owner,
                                            owner,
                                            (GCompareFunc)gnc_lot_sort_func);
        /* For each lot */
        for (lot_node = lot_list; lot_node; lot_node = lot_node->next)
        {
            GNCLot *lot = lot_node->data;
            gnc_numeric lot_balance = gnc_lot_get_balance (lot);
            balance = gnc_numeric_add (balance, lot_balance,
                      gnc_commodity_get_fraction (owner_currency), GNC_HOW_RND_ROUND_HALF_UP);
        }
    }

    pdb = gnc_pricedb_get_db (book);

    if (report_currency)
        balance = gnc_pricedb_convert_balance_latest_price (
                      pdb, balance, owner_currency, report_currency);

    return balance;
}


/* XXX: Yea, this is broken, but it should work fine for Queries.
 * We're single-threaded, right?
 */
static GncOwner *
owner_from_lot (GNCLot *lot)
{
    static GncOwner owner;

    if (!lot) return NULL;
    if (gncOwnerGetOwnerFromLot (lot, &owner))
        return &owner;

    return NULL;
}

static void
reg_lot (void)
{
    static QofParam params[] =
    {
        { OWNER_FROM_LOT, _GNC_MOD_NAME, (QofAccessFunc)owner_from_lot, NULL },
        { NULL },
    };

    qof_class_register (GNC_ID_LOT, NULL, params);
}

gboolean gncOwnerGetOwnerFromTypeGuid (QofBook *book, GncOwner *owner, QofIdType type, GncGUID *guid)
{
    if (!book || !owner || !type || !guid) return FALSE;

    if (0 == safe_strcmp(type, GNC_ID_CUSTOMER))
    {
        GncCustomer *customer = gncCustomerLookup(book, guid);
        gncOwnerInitCustomer(owner, customer);
        return (NULL != customer);
    }
    else if (0 == safe_strcmp(type, GNC_ID_JOB))
    {
        GncJob *job = gncJobLookup(book, guid);
        gncOwnerInitJob(owner, job);
        return (NULL != job);
    }
    else if (0 == safe_strcmp(type, GNC_ID_VENDOR))
    {
        GncVendor *vendor = gncVendorLookup(book, guid);
        gncOwnerInitVendor(owner, vendor);
        return (NULL != vendor);
    }
    else if (0 == safe_strcmp(type, GNC_ID_EMPLOYEE))
    {
        GncEmployee *employee = gncEmployeeLookup(book, guid);
        gncOwnerInitEmployee(owner, employee);
        return (NULL != employee);
    }
    return 0;
}

gboolean gncOwnerRegister (void)
{
    static QofParam params[] =
    {
        { OWNER_TYPE, QOF_TYPE_INT64,      (QofAccessFunc)gncOwnerGetType,          NULL },
        { OWNER_CUSTOMER, GNC_ID_CUSTOMER, (QofAccessFunc)gncOwnerGetCustomer,      NULL },
        { OWNER_JOB, GNC_ID_JOB,           (QofAccessFunc)gncOwnerGetJob,           NULL },
        { OWNER_VENDOR, GNC_ID_VENDOR,     (QofAccessFunc)gncOwnerGetVendor,        NULL },
        { OWNER_EMPLOYEE, GNC_ID_EMPLOYEE, (QofAccessFunc)gncOwnerGetEmployee,      NULL },
        { OWNER_PARENT, GNC_ID_OWNER,      (QofAccessFunc)gncOwnerGetEndOwner,      NULL },
        { OWNER_PARENTG, QOF_TYPE_GUID,    (QofAccessFunc)gncOwnerGetEndGUID,       NULL },
        { OWNER_NAME, QOF_TYPE_STRING,     (QofAccessFunc)gncOwnerGetName, NULL },
        { QOF_PARAM_GUID, QOF_TYPE_GUID,   (QofAccessFunc)gncOwnerGetGUID, NULL },
        { NULL },
    };

    qof_class_register (GNC_ID_OWNER, (QofSortFunc)gncOwnerCompare, params);
    reg_lot ();

    return TRUE;
}
