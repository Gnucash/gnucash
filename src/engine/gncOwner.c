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
#include "engine-helpers.h"

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
    if (0 == g_strcmp0(ent->e_type, GNC_ID_CUSTOMER))
    {
        owner->type = GNC_OWNER_CUSTOMER;
        gncOwnerInitCustomer(owner, (GncCustomer*)ent);
    }
    else if (0 == g_strcmp0(ent->e_type, GNC_ID_JOB))
    {
        owner->type = GNC_OWNER_JOB;
        gncOwnerInitJob(owner, (GncJob*)ent);
    }
    else if (0 == g_strcmp0(ent->e_type, GNC_ID_VENDOR))
    {
        owner->type = GNC_OWNER_VENDOR;
        gncOwnerInitVendor(owner, (GncVendor*)ent);
    }
    else if (0 == g_strcmp0(ent->e_type, GNC_ID_EMPLOYEE))
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

gboolean GNC_IS_OWNER (QofInstance *ent)
{
    if (!ent)
        return FALSE;

    return (GNC_IS_VENDOR(ent) ||
            GNC_IS_CUSTOMER(ent) ||
            GNC_IS_EMPLOYEE(ent) ||
            GNC_IS_JOB(ent));
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
        return gncEmployeeGetName (owner->owner.employee);
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

const GncOwner * gncOwnerGetEndOwner (const GncOwner *owner)
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

const GncGUID * gncOwnerGetEndGUID (const GncOwner *owner)
{
    if (!owner) return NULL;
    return gncOwnerGetGUID (gncOwnerGetEndOwner (owner));
}

void gncOwnerAttachToLot (const GncOwner *owner, GNCLot *lot)
{
    KvpFrame *kvp;
    KvpValue *value;

    if (!owner || !lot)
        return;

    kvp = gnc_lot_get_slots (lot);
    gnc_lot_begin_edit (lot);

    value = kvp_value_new_gint64 (gncOwnerGetType (owner));
    kvp_frame_set_slot_path (kvp, value, GNC_OWNER_ID, GNC_OWNER_TYPE, NULL);
    kvp_value_delete (value);

    value = kvp_value_new_guid (gncOwnerGetGUID (owner));
    kvp_frame_set_slot_path (kvp, value, GNC_OWNER_ID, GNC_OWNER_GUID, NULL);
    qof_instance_set_dirty (QOF_INSTANCE (lot));
    gnc_lot_commit_edit (lot);
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

gboolean
gncOwnerLotMatchOwnerFunc (GNCLot *lot, gpointer user_data)
{
    const GncOwner *req_owner = user_data;
    GncOwner lot_owner;
    const GncOwner *end_owner;
    GncInvoice *invoice = gncInvoiceGetInvoiceFromLot (lot);

    /* Determine the owner associated to the lot */
    if (invoice)
        /* Invoice lots */
        end_owner = gncOwnerGetEndOwner (gncInvoiceGetOwner (invoice));
    else if (gncOwnerGetOwnerFromLot (lot, &lot_owner))
        /* Pre-payment lots */
        end_owner = gncOwnerGetEndOwner (&lot_owner);
    else
        return FALSE;

    /* Is this a lot for the requested owner ? */
    return gncOwnerEqual (end_owner, req_owner);
}

gint
gncOwnerLotsSortFunc (GNCLot *lotA, GNCLot *lotB)
{
    GncInvoice *ia, *ib;
    Timespec da, db;

    ia = gncInvoiceGetInvoiceFromLot (lotA);
    ib = gncInvoiceGetInvoiceFromLot (lotB);

    if (ia)
        da = gncInvoiceGetDateDue (ia);
    else
        da = xaccTransRetDatePostedTS (xaccSplitGetParent (gnc_lot_get_earliest_split (lotA)));

    if (ib)
        db = gncInvoiceGetDateDue (ib);
    else
        db = xaccTransRetDatePostedTS (xaccSplitGetParent (gnc_lot_get_earliest_split (lotB)));

    return timespec_cmp (&da, &db);
}

GNCLot *
gncOwnerCreatePaymentLot (const GncOwner *owner, Transaction *txn,
                          Account *posted_acc, Account *xfer_acc,
                          gnc_numeric amount, gnc_numeric exch, Timespec date,
                          const char *memo, const char *num)
{
    QofBook *book;
    Split *split;
    const char *name;
    gnc_commodity *commodity;
    Split *xfer_split = NULL;
    GNCLot *payment_lot;

    /* Verify our arguments */
    if (!owner || !posted_acc || !xfer_acc) return NULL;
    g_return_val_if_fail (owner->owner.undefined != NULL, NULL);

    /* Compute the ancillary data */
    book = gnc_account_get_book (posted_acc);
    name = gncOwnerGetName (gncOwnerGetEndOwner ((GncOwner*)owner));
    commodity = gncOwnerGetCurrency (owner);
//    reverse = use_reversed_payment_amounts(owner);

    if (txn)
    {
        /* Pre-existing transaction was specified. We completely clear it,
         * except for the split in the transfer account, unless the
         * transaction can't be reused (wrong currency, wrong transfer account).
         * In that case, the transaction is simply removed and an new
         * one created. */

        xfer_split = xaccTransFindSplitByAccount(txn, xfer_acc);

        if (xaccTransGetCurrency(txn) != gncOwnerGetCurrency (owner))
        {
            g_message("Uh oh, mismatching currency/commodity between selected transaction and owner. We fall back to manual creation of a new transaction.");
            xfer_split = NULL;
        }

        if (!xfer_split)
        {
            g_message("Huh? Asset account not found anymore. Fully deleting old txn and now creating a new one.");

            xaccTransBeginEdit (txn);
            xaccTransDestroy (txn);
            xaccTransCommitEdit (txn);

            txn = NULL;
        }
        else
        {
            int i = 0;
            xaccTransBeginEdit (txn);
            while (i < xaccTransCountSplits(txn))
            {
                Split *split = xaccTransGetSplit (txn, i);
                if (split == xfer_split)
                {
                    ++i;
                }
                else
                {
                    xaccSplitDestroy(split);
                }
            }
            xaccTransCommitEdit (txn);
        }
    }

    /* Create the transaction if we don't have one yet */
    if (!txn)
        txn = xaccMallocTransaction (book);

    /* Insert a split for the transfer account if we don't have one yet */
    if (!xfer_split)
    {
        xaccTransBeginEdit (txn);

        /* Set up the transaction */
        xaccTransSetDescription (txn, name ? name : "");
        /* set per book option */
        gnc_set_num_action (txn, NULL, num, _("Payment"));
        xaccTransSetCurrency (txn, commodity);
        xaccTransSetDateEnteredSecs (txn, gnc_time (NULL));
        xaccTransSetDatePostedTS (txn, &date);
        xaccTransSetTxnType (txn, TXN_TYPE_PAYMENT);


        /* The split for the transfer account */
        split = xaccMallocSplit (book);
        xaccSplitSetMemo (split, memo);
        /* set per book option */
        gnc_set_num_action (NULL, split, num, _("Payment"));
        xaccAccountBeginEdit (xfer_acc);
        xaccAccountInsertSplit (xfer_acc, split);
        xaccAccountCommitEdit (xfer_acc);
        xaccTransAppendSplit (txn, split);

        if (gnc_commodity_equal(xaccAccountGetCommodity(xfer_acc), commodity))
        {
            xaccSplitSetBaseValue (split, amount, commodity);
        }
        else
        {
            /* Need to value the payment in terms of the owner commodity */
            gnc_numeric payment_value = gnc_numeric_mul(amount,
                                        exch, GNC_DENOM_AUTO, GNC_HOW_RND_ROUND_HALF_UP);

            xaccSplitSetAmount(split, amount);
            xaccSplitSetValue(split, payment_value);
        }
    }

    /* Add a split in the post account */
    split = xaccMallocSplit (book);
    xaccSplitSetMemo (split, memo);
    /* set per book option */
    gnc_set_num_action (NULL, split, num, _("Payment"));
    xaccAccountBeginEdit (posted_acc);
    xaccAccountInsertSplit (posted_acc, split);
    xaccAccountCommitEdit (posted_acc);
    xaccTransAppendSplit (txn, split);
    xaccSplitSetBaseValue (split, gnc_numeric_neg (amount), commodity);

    /* Create a new lot for the payment */
    payment_lot = gnc_lot_new (book);
    gncOwnerAttachToLot (owner, payment_lot);
    gnc_lot_add_split (payment_lot, split);


    /* Commit this new transaction */
    xaccTransCommitEdit (txn);

    return payment_lot;
}

void gncOwnerAutoApplyPaymentsWithLots (const GncOwner *owner, GList *lots)
{
    GList *base_iter;

    /* General note: in the code below the term "payment" can
     * both mean a true payment or a document of
     * the opposite sign (invoice vs credit note) relative to
     * the lot being processed. In general this function will
     * perform a balancing action on a set of lots, so you
     * will also find frequent references to balancing instead. */

    /* Payments can only be applied when at least an owner
     * and a list of lots to use are given */
    if (!owner) return;
    if (!lots) return;

    for (base_iter = lots; base_iter; base_iter = base_iter->next)
    {
        GNCLot *base_lot = base_iter->data;
        QofBook *book;
        Account *acct;
        const gchar *name;
        GList *lot_list, *lot_iter;
        Transaction *txn = NULL;
        gnc_numeric base_lot_bal, val_to_pay, val_paid = { 0, 1 };
        gboolean base_bal_is_pos;
        const gchar *action, *memo;

        /* Only attempt to apply payments to open lots.
         * Note that due to the iterative nature of this function lots
         * in the list may become closed before they are evaluated as
         * base lot, so we should check this for each lot. */
        base_lot_bal = gnc_lot_get_balance (base_lot);
        if (gnc_numeric_zero_p (base_lot_bal))
            continue;

        book = gnc_lot_get_book (base_lot);
        acct = gnc_lot_get_account (base_lot);
        name = gncOwnerGetName (gncOwnerGetEndOwner (owner));
        lot_list = base_iter->next;

        /* Strings used when creating splits later on. */
        action = _("Lot Link");
        memo   = _("Internal link between invoice and payment lots");

        /* Note: to balance the lot the payment to assign
         * must have the opposite sign of the existing lot balance */
        val_to_pay = gnc_numeric_neg (base_lot_bal);
        base_bal_is_pos = gnc_numeric_positive_p (base_lot_bal);


        /* Create splits in a linking transaction between lots until
         * - either the invoice lot is balanced
         * - or there are no more balancing lots.
         */
        for (lot_iter = lot_list; lot_iter; lot_iter = lot_iter->next)
        {
            gnc_numeric payment_lot_balance;
            Split *split;
            Account *bal_acct;
            gnc_numeric  split_amt;

            GNCLot *balancing_lot = lot_iter->data;

            /* Only attempt to use open lots to balance the base lot.
             * Note that due to the iterative nature of this function lots
             * in the list may become closed before they are evaluated as
             * base lot, so we should check this for each lot. */
            if (gnc_lot_is_closed (balancing_lot))
                continue;

            /* Balancing transactions for invoice/payments can only happen
             * in the same account. */
            bal_acct = gnc_lot_get_account (balancing_lot);
            if (acct != bal_acct)
                continue;

            payment_lot_balance = gnc_lot_get_balance (balancing_lot);

            /* Only attempt to balance if the base lot and balancing lot are
             * of the opposite sign. (Otherwise we would increase the balance
             * of the lot - Duh */
            if (base_bal_is_pos == gnc_numeric_positive_p (payment_lot_balance))
                continue;

            /*
             * If there is less to pay than there's open in the lot; we're done -- apply the base_lot_vale.
             * Note that payment_value and balance are opposite in sign, so we have to compare absolute values here
             *
             * Otherwise, apply the balance, subtract that from the payment_value,
             * and move on to the next one.
             */
            if (gnc_numeric_compare (gnc_numeric_abs (val_to_pay), gnc_numeric_abs (payment_lot_balance)) <= 0)
            {
                /* abs(val_to_pay) <= abs(balance) */
                split_amt = val_to_pay;
            }
            else
            {
                /* abs(val_to_pay) > abs(balance)
                 * Remember payment_value and balance are opposite in sign,
                 * and we want a payment to neutralize the current balance
                 * so we need to negate here */
                split_amt = payment_lot_balance;
            }

            /* If not created yet, create a new transaction linking
             * the base lot and the balancing lot(s) */
            if (!txn)
            {
                Timespec ts = xaccTransRetDatePostedTS (xaccSplitGetParent (gnc_lot_get_latest_split (base_lot)));

                xaccAccountBeginEdit (acct);

                txn = xaccMallocTransaction (book);
                xaccTransBeginEdit (txn);

                xaccTransSetDescription (txn, name ? name : "");
                xaccTransSetCurrency (txn, xaccAccountGetCommodity(acct));
                xaccTransSetDateEnteredSecs (txn, gnc_time (NULL));
                xaccTransSetDatePostedTS (txn, &ts);
                xaccTransSetTxnType (txn, TXN_TYPE_LINK);
            }

            /* Create the split for this link in current balancing lot */
            split = xaccMallocSplit (book);
            xaccSplitSetMemo (split, memo);
            /* set Action using utility function */
            gnc_set_num_action (NULL, split, NULL, action);
            xaccAccountInsertSplit (acct, split);
            xaccTransAppendSplit (txn, split);
            xaccSplitSetBaseValue (split, gnc_numeric_neg (split_amt), xaccAccountGetCommodity(acct));
            gnc_lot_add_split (balancing_lot, split);

            /* If the balancing lot was linked to a document (invoice/credit note),
             * send an event for it as well so it gets potentially updated as paid */
            {
                GncInvoice *this_invoice = gncInvoiceGetInvoiceFromLot(balancing_lot);
                if (this_invoice)
                    qof_event_gen (QOF_INSTANCE(this_invoice), QOF_EVENT_MODIFY, NULL);
            }

            val_paid   = gnc_numeric_add (val_paid, split_amt, GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD);
            val_to_pay = gnc_numeric_sub (val_to_pay, split_amt, GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD);
            if (gnc_numeric_zero_p (val_to_pay))
                break;
        }


        /* If the above loop managed to create a transaction and some balancing splits,
         * create the final split for the link transaction in the base lot */
        if (txn)
        {
            GncInvoice *this_invoice;
            Split *split = xaccMallocSplit (book);

            xaccSplitSetMemo (split, memo);
            /* set Action with utiltity function */
            gnc_set_num_action (NULL, split, NULL, action);
            xaccAccountInsertSplit (acct, split);
            xaccTransAppendSplit (txn, split);
            xaccSplitSetBaseValue (split, val_paid, xaccAccountGetCommodity(acct));
            gnc_lot_add_split (base_lot, split);

            xaccTransCommitEdit (txn);
            xaccAccountCommitEdit (acct);

            /* If the base lot was linked to a document (invoice/credit note),
             * send an event for it as well so it gets potentially updated as paid */
            this_invoice = gncInvoiceGetInvoiceFromLot(base_lot);
            if (this_invoice)
                qof_event_gen (QOF_INSTANCE(this_invoice), QOF_EVENT_MODIFY, NULL);

        }

    }
}

/*
 * Create a payment of "amount" for the owner and match it with
 * the set of lots passed in. If not lots were given all open
 * lots for the owner are considered.
 */
void
gncOwnerApplyPayment (const GncOwner *owner, Transaction *txn, GList *lots,
                      Account *posted_acc, Account *xfer_acc,
                      gnc_numeric amount, gnc_numeric exch, Timespec date,
                      const char *memo, const char *num, gboolean auto_pay)
{
    GNCLot *payment_lot;
    GList *selected_lots = NULL;

    /* Verify our arguments */
    if (!owner || !posted_acc || !xfer_acc) return;
    g_return_if_fail (owner->owner.undefined);

    /* Create a lot for this payment */
    payment_lot = gncOwnerCreatePaymentLot (owner, txn, posted_acc, xfer_acc,
                                            amount, exch, date, memo, num);

    if (lots)
        selected_lots = lots;
    else if (auto_pay)
        selected_lots = xaccAccountFindOpenLots (posted_acc, gncOwnerLotMatchOwnerFunc,
                        (gpointer)owner, NULL);

    /* And link the selected lots and the payment lot together as well as possible.
     * If the payment was bigger than the selected documents/overpayments, only
     * part of the payment will be used. Similarly if more documents were selected
     * than the payment value set, not all documents will be marked as paid. */
    if (payment_lot)
        selected_lots = g_list_prepend (selected_lots, payment_lot);
    gncOwnerAutoApplyPaymentsWithLots (owner, selected_lots);
    g_list_free (selected_lots);
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
gncOwnerGetBalanceInCurrency (const GncOwner *owner,
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
        lot_list = xaccAccountFindOpenLots (account, gncOwnerLotMatchOwnerFunc,
                                            (gpointer)owner, NULL);
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

    if (0 == g_strcmp0(type, GNC_ID_CUSTOMER))
    {
        GncCustomer *customer = gncCustomerLookup(book, guid);
        gncOwnerInitCustomer(owner, customer);
        return (NULL != customer);
    }
    else if (0 == g_strcmp0(type, GNC_ID_JOB))
    {
        GncJob *job = gncJobLookup(book, guid);
        gncOwnerInitJob(owner, job);
        return (NULL != job);
    }
    else if (0 == g_strcmp0(type, GNC_ID_VENDOR))
    {
        GncVendor *vendor = gncVendorLookup(book, guid);
        gncOwnerInitVendor(owner, vendor);
        return (NULL != vendor);
    }
    else if (0 == g_strcmp0(type, GNC_ID_EMPLOYEE))
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
