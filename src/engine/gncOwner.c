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

#include <config.h>

#include <glib.h>
#include <glib/gi18n.h>
#include <string.h>		/* for memcpy() */
#include <qofinstance-p.h>

#include "gncCustomerP.h"
#include "gncEmployeeP.h"
#include "gncJobP.h"
#include "gncOwner.h"
#include "gncOwnerP.h"
#include "gncVendorP.h"
#include "gncInvoice.h"
#include "gnc-commodity.h"
#include "Scrub2.h"
#include "Split.h"
#include "Transaction.h"
#include "engine-helpers.h"

#define _GNC_MOD_NAME   GNC_ID_OWNER

#define GNC_OWNER_ID    "gncOwner"

static QofLogModule log_module = GNC_MOD_ENGINE;

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
     if (!owner || !lot)
        return;

    gnc_lot_begin_edit (lot);

    qof_instance_set (QOF_INSTANCE (lot),
		      GNC_OWNER_TYPE, (gint64)gncOwnerGetType (owner),
		      GNC_OWNER_GUID, gncOwnerGetGUID (owner),
		      NULL);
    gnc_lot_commit_edit (lot);
}

gboolean gncOwnerGetOwnerFromLot (GNCLot *lot, GncOwner *owner)
{
    GncGUID *guid = NULL;
    QofBook *book;
    GncOwnerType type = GNC_OWNER_NONE;
    guint64 type64 = 0;

    if (!lot || !owner) return FALSE;

    book = gnc_lot_get_book (lot);
    qof_instance_get (QOF_INSTANCE (lot),
		      GNC_OWNER_TYPE, &type64,
		      GNC_OWNER_GUID, &guid,
		      NULL);
    type = (GncOwnerType) type64;
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
                    gnc_set_num_action (NULL, split, num, _("Payment"));
                    ++i;
                }
                else
                {
                    xaccSplitDestroy(split);
                }
            }
            /* Note: don't commit transaction now - that would insert an imbalance split.*/
        }
    }

    /* Create the transaction if we don't have one yet */
    if (!txn)
    {
        txn = xaccMallocTransaction (book);
        xaccTransBeginEdit (txn);
    }

    /* Insert a split for the transfer account if we don't have one yet */
    if (!xfer_split)
    {

        /* Set up the transaction */
        xaccTransSetDescription (txn, name ? name : "");
        /* set per book option */
        xaccTransSetCurrency (txn, commodity);
        xaccTransSetDateEnteredSecs (txn, gnc_time (NULL));
        xaccTransSetDatePostedTS (txn, &date);


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

    /* Mark the transaction as a payment */
    gnc_set_num_action (txn, NULL, num, _("Payment"));
    xaccTransSetTxnType (txn, TXN_TYPE_PAYMENT);

    /* Commit this new transaction */
    xaccTransCommitEdit (txn);

    return payment_lot;
}

typedef enum
{
    is_equal     = 8,
    is_more      = 4,
    is_less      = 2,
    is_pay_split = 1
} split_flags;

Split *gncOwnerFindOffsettingSplit (GNCLot *lot, gnc_numeric target_value)
{
    SplitList *ls_iter = NULL;
    Split *best_split = NULL;
    gnc_numeric best_val = { 0, 1};
    gint best_flags = 0;

    if (!lot)
        return NULL;

    for (ls_iter = gnc_lot_get_split_list (lot); ls_iter; ls_iter = ls_iter->next)
    {
        Split *split = ls_iter->data;
        Transaction *txn;
        gnc_numeric split_value;
        gint new_flags = 0;
        gint val_cmp = 0;

        if (!split)
            continue;


        txn = xaccSplitGetParent (split);
        if (!txn)
        {
            // Ooops - the split doesn't belong to any transaction !
            // This is not expected so issue a warning and continue with next split
            PWARN("Encountered a split in a payment lot that's not part of any transaction. "
                  "This is unexpected! Skipping split %p.", split);
            continue;
        }

        // Check if this split has the opposite sign of the target value we want to offset
        split_value = xaccSplitGetValue (split);
        if (gnc_numeric_positive_p (target_value) == gnc_numeric_positive_p (split_value))
            continue;

        // Ok we have found a split that potentially can offset the target value
        // Let's see if it's better than what we have found already.
        val_cmp = gnc_numeric_compare (gnc_numeric_abs (split_value),
                                       gnc_numeric_abs (target_value));
        if (val_cmp == 0)
            new_flags += is_equal;
        else if (val_cmp > 0)
            new_flags += is_more;
        else
            new_flags += is_less;

        if (xaccTransGetTxnType (txn) != TXN_TYPE_LINK)
            new_flags += is_pay_split;

        if ((new_flags >= best_flags) &&
            (gnc_numeric_compare (gnc_numeric_abs (split_value),
                                  gnc_numeric_abs (best_val)) > 0))
        {
            // The new split is a better match than what we found so far
            best_split = split;
            best_flags = new_flags;
            best_val   = split_value;
        }
    }

    return best_split;
}

gboolean
gncOwnerReduceSplitTo (Split *split, gnc_numeric target_value)
{
    gnc_numeric split_val = xaccSplitGetValue (split);
    gnc_numeric rem_val;
    Split *rem_split;
    Transaction *txn;
    GNCLot *lot;

    if (gnc_numeric_positive_p (split_val) != gnc_numeric_positive_p (target_value))
        return FALSE; // Split and target value have to be of the same sign

    if (gnc_numeric_equal (split_val, target_value))
        return FALSE; // Split already has the target value

    rem_val = gnc_numeric_sub (split_val, target_value, GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD); // note: values are of opposite sign
    rem_split = xaccMallocSplit (xaccSplitGetBook (split));
    xaccSplitCopyOnto (split, rem_split);
    xaccSplitSetValue (rem_split, rem_val);

    txn = xaccSplitGetParent (split);
    xaccTransBeginEdit (txn);
    xaccSplitSetValue (split, target_value);
    xaccSplitSetParent (rem_split, txn);
    xaccTransCommitEdit (txn);

    lot = xaccSplitGetLot (split);
    gnc_lot_add_split (lot, rem_split);

    return TRUE;
}

void
gncOwnerSetLotLinkMemo (Transaction *ll_txn)
{
    gchar *memo_prefix = _("Offset between documents: ");
    gchar *new_memo;
    SplitList *lts_iter;
    SplitList *splits = NULL, *siter;
    GList *titles = NULL, *titer;

    if (!ll_txn)
        return;

    if (xaccTransGetTxnType (ll_txn) != TXN_TYPE_LINK)
        return;

    // Find all splits in the lot link transaction that are also in a document lot
    for (lts_iter = xaccTransGetSplitList (ll_txn); lts_iter; lts_iter = lts_iter->next)
    {
        Split *split = lts_iter->data;
        GNCLot *lot;
        GncInvoice *invoice;
        gchar *title;

        if (!split)
            continue;

        lot = xaccSplitGetLot (split);
        if (!lot)
            continue;

        invoice = gncInvoiceGetInvoiceFromLot (lot);
        if (!invoice)
            continue;

        title = g_strdup_printf ("%s %s", gncInvoiceGetTypeString (invoice), gncInvoiceGetID (invoice));

        titles = g_list_insert_sorted (titles, title, (GCompareFunc)g_strcmp0);
        splits = g_list_prepend (splits, split); // splits don't need to be sorted
    }

    if (!titles)
        return; // We didn't find document lots

    // Create the memo as we'd want it to be
    new_memo = g_strconcat (memo_prefix, titles->data, NULL);
    for (titer = titles->next; titer; titer = titer->next)
    {
        gchar *tmp_memo = g_strconcat (new_memo, " - ", titer->data, NULL);
        g_free (new_memo);
        new_memo = tmp_memo;
    }
    g_list_free_full (titles, g_free);

    // Update the memos of all the splits we found previously (if needed)
    for (siter = splits; siter; siter = siter->next)
    {
        if (g_strcmp0 (xaccSplitGetMemo (siter->data), new_memo) != 0)
            xaccSplitSetMemo (siter->data, new_memo);
    }

    g_list_free (splits);
    g_free (new_memo);
}

/* Find an existing lot link transaction in the given lot
 * Only use a lot link that already links at least two
 * documents (to avoid perpetuating the lot link proliferation
 * that happened in 2.6.0-2.6.3).
 */
static Transaction *
get_ll_transaction_from_lot (GNCLot *lot)
{
    SplitList *ls_iter;

    /* This should really only be called on a document lot */
    if (!gncInvoiceGetInvoiceFromLot (lot))
        return NULL;

    /* The given lot is a valid document lot. Now iterate over all
     * other lot links in this lot to find one more document lot.
     */
    for (ls_iter = gnc_lot_get_split_list (lot); ls_iter; ls_iter = ls_iter->next)
    {
        Split *ls = ls_iter->data;
        Transaction *ll_txn = xaccSplitGetParent (ls);
        SplitList *ts_iter;

        if (xaccTransGetTxnType (ll_txn) != TXN_TYPE_LINK)
            continue;

        for (ts_iter = xaccTransGetSplitList (ll_txn); ts_iter; ts_iter = ts_iter->next)
        {
            Split *ts = ts_iter->data;
            GNCLot *tslot = xaccSplitGetLot (ts);

            if (!tslot)
                continue;

            if (tslot == lot)
                continue;

            if (gncInvoiceGetInvoiceFromLot (lot))
                return ll_txn; /* Got one more document lot - mission accomplished */
        }
    }

    /* The lot doesn't have an ll_txn with the requested criteria... */
    return NULL;
}

static void
gncOwnerCreateLotLink (GNCLot *from_lot, GNCLot *to_lot, const GncOwner *owner)
{
    const gchar *action = _("Lot Link");
    Account *acct = gnc_lot_get_account (from_lot);
    const gchar *name = gncOwnerGetName (gncOwnerGetEndOwner (owner));
    Transaction *ll_txn = NULL;
    gnc_numeric from_lot_bal, to_lot_bal;
    Timespec from_ts, to_ts;
    time64 time_posted;
    Split *split;

    /* Sanity check */
    if (!gncInvoiceGetInvoiceFromLot (from_lot) ||
        !gncInvoiceGetInvoiceFromLot (to_lot))
        return;

    /* Determine transaction date based on lot splits */
    from_ts = xaccTransRetDatePostedTS (xaccSplitGetParent (gnc_lot_get_latest_split (from_lot)));
    to_ts   = xaccTransRetDatePostedTS (xaccSplitGetParent (gnc_lot_get_latest_split (to_lot)));
    if (timespecToTime64 (from_ts) >= timespecToTime64 (to_ts))
        time_posted = timespecToTime64 (from_ts);
    else
        time_posted = timespecToTime64 (to_ts);

    /* Figure out how much we can offset between the lots */
    from_lot_bal = gnc_lot_get_balance (from_lot);
    to_lot_bal = gnc_lot_get_balance (to_lot);
    if (gnc_numeric_compare (gnc_numeric_abs (from_lot_bal),
                             gnc_numeric_abs (to_lot_bal)) > 0)
        from_lot_bal = gnc_numeric_neg (to_lot_bal);
    else
        to_lot_bal = gnc_numeric_neg (from_lot_bal);

    xaccAccountBeginEdit (acct);

    /* Look for a pre-existing lot link we can extend */
    ll_txn = get_ll_transaction_from_lot (from_lot);

    if (!ll_txn)
        ll_txn = get_ll_transaction_from_lot (to_lot);

    if (!ll_txn)
    {
        /* No pre-existing lot link. Create one. */
        Timespec ts;

        timespecFromTime64 (&ts, time_posted);

        ll_txn = xaccMallocTransaction (gnc_lot_get_book (from_lot));
        xaccTransBeginEdit (ll_txn);

        xaccTransSetDescription (ll_txn, name ? name : "(Unknown)");
        xaccTransSetCurrency (ll_txn, xaccAccountGetCommodity(acct));
        xaccTransSetDateEnteredSecs (ll_txn, gnc_time (NULL));
        xaccTransSetDatePostedTS (ll_txn, &ts);
        xaccTransSetTxnType (ll_txn, TXN_TYPE_LINK);
    }
    else
    {
        Timespec ts = xaccTransRetDatePostedTS (ll_txn);
        xaccTransBeginEdit (ll_txn);

        /* Maybe we need to update the post date of the transaction ? */
        if (time_posted > timespecToTime64 (ts))
        {
            timespecFromTime64 (&ts, time_posted);
            xaccTransSetDatePostedTS (ll_txn, &ts);

        }
    }

    /* Create a split for the from_lot */
    split = xaccMallocSplit (gnc_lot_get_book (from_lot));
    /* set Action using utility function */
    gnc_set_num_action (NULL, split, NULL, action);
    xaccAccountInsertSplit (acct, split);
    xaccTransAppendSplit (ll_txn, split);
    /* To offset the lot balance, the split must be of the opposite sign */
    xaccSplitSetBaseValue (split, gnc_numeric_neg (from_lot_bal), xaccAccountGetCommodity(acct));
    gnc_lot_add_split (from_lot, split);

    /* Create a split for the to_lot */
    split = xaccMallocSplit (gnc_lot_get_book (to_lot));
    /* set Action using utility function */
    gnc_set_num_action (NULL, split, NULL, action);
    xaccAccountInsertSplit (acct, split);
    xaccTransAppendSplit (ll_txn, split);
    /* To offset the lot balance, the split must be of the opposite sign */
    xaccSplitSetBaseValue (split, gnc_numeric_neg (to_lot_bal), xaccAccountGetCommodity(acct));
    gnc_lot_add_split (to_lot, split);

    xaccTransCommitEdit (ll_txn);


    /* Do some post-cleaning on the lots
     * The above actions may have created splits that are
     * in the same transaction and lot. These can be merged.
     */
    xaccScrubMergeLotSubSplits (to_lot, FALSE);
    xaccScrubMergeLotSubSplits (from_lot, FALSE);
    /* And finally set the same memo for all remaining splits
     * It's a convenience for the users to identify all documents
     * involved in the link.
     */
    gncOwnerSetLotLinkMemo (ll_txn);
    xaccAccountCommitEdit (acct);
}

static void gncOwnerOffsetLots (GNCLot *from_lot, GNCLot *to_lot, const GncOwner *owner)
{
    gnc_numeric target_offset;
    Split *split;

    /* from lot should not be a document lot because we're removing a split from there ! */
    if (gncInvoiceGetInvoiceFromLot (from_lot))
    {
        PWARN ("from_lot %p is a document lot. That is not allowed in gncOwnerOffsetLots", from_lot);
        return;
    }

    /* Get best matching split from from_lot to offset to_lot */
    target_offset = gnc_lot_get_balance (to_lot);
    if (gnc_numeric_zero_p (target_offset))
        return; // to_lot is already balanced, nothing more to do

    split = gncOwnerFindOffsettingSplit (from_lot, target_offset);
    if (!split)
        return; // No suitable offsetting split found, nothing more to do

    /* If the offsetting split is bigger than the amount needed to balance
     * to_lot, reduce the split so its reduced value closes to_lot exactly.
     * Note the negation in the reduction function. The split must be of
     * opposite sign of to_lot's balance in order to be able to close it.
     */
    if (gnc_numeric_compare (gnc_numeric_abs (xaccSplitGetValue (split)),
                             gnc_numeric_abs (target_offset)) > 0)
        gncOwnerReduceSplitTo (split, gnc_numeric_neg (target_offset));

    /* Move the reduced split from from_lot to to_lot */
    gnc_lot_add_split (to_lot, split);

}

void gncOwnerAutoApplyPaymentsWithLots (const GncOwner *owner, GList *lots)
{
    GList *left_iter;

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

    for (left_iter = lots; left_iter; left_iter = left_iter->next)
    {
        GNCLot *left_lot = left_iter->data;
        gnc_numeric left_lot_bal;
        gboolean left_lot_has_doc;
        gboolean left_modified = FALSE;
        Account *acct;
        GList *right_iter;

        /* Only attempt to apply payments to open lots.
         * Note that due to the iterative nature of this function lots
         * in the list may become empty/closed before they are evaluated as
         * base lot, so we should check this for each lot. */
        if (!left_lot)
            continue;
        if (gnc_lot_count_splits (left_lot) == 0)
        {
            gnc_lot_destroy (left_lot);
            left_iter->data = NULL;
            continue;
        }
        if (gnc_lot_is_closed (left_lot))
            continue;

        acct = gnc_lot_get_account (left_lot);
        xaccAccountBeginEdit (acct);

        left_lot_bal = gnc_lot_get_balance (left_lot);
        left_lot_has_doc = (gncInvoiceGetInvoiceFromLot (left_lot) != NULL);

        /* Attempt to offset left_lot with any of the remaining lots. To do so
         * iterate over the remaining lots adding lot links or moving payments
         * around.
         */
        for (right_iter = left_iter->next; right_iter; right_iter = right_iter->next)
        {
            GNCLot *right_lot = right_iter->data;
            gnc_numeric right_lot_bal;
            gboolean right_lot_has_doc;

            /* Only attempt to use open lots to balance the base lot.
             * Note that due to the iterative nature of this function lots
             * in the list may become empty/closed before they are evaluated as
             * base lot, so we should check this for each lot. */
            if (!right_lot)
                continue;
            if (gnc_lot_count_splits (right_lot) == 0)
            {
                gnc_lot_destroy (right_lot);
                right_iter->data = NULL;
                continue;
            }
            if (gnc_lot_is_closed (right_lot))
                continue;

            /* Balancing transactions for invoice/payments can only happen
             * in the same account. */
            if (acct != gnc_lot_get_account (right_lot))
                continue;


            /* Only attempt to balance if the base lot and balancing lot are
             * of the opposite sign. (Otherwise we would increase the balance
             * of the lot - Duh */
            right_lot_bal = gnc_lot_get_balance (right_lot);
            if (gnc_numeric_positive_p (left_lot_bal) == gnc_numeric_positive_p (right_lot_bal))
                continue;

            /* Ok we found two lots than can (partly) offset each other.
             * Depending on the lot types, a different action is needed to accomplish this.
             * 1. Both lots are document lots (invoices/credit notes)
             *    -> Create a lot linking transaction between the lots
             * 2. Both lots are payment lots (lots without a document attached)
             *    -> Use part of the bigger lot to the close the smaller lot
             * 3. One document lot with one payment lot
             *    -> Use (part of) the payment to offset (part of) the document lot,
             *       Which one will be closed depends on which is the bigger one
             */
            right_lot_has_doc = (gncInvoiceGetInvoiceFromLot (right_lot) != NULL);
            if (left_lot_has_doc && right_lot_has_doc)
                gncOwnerCreateLotLink (left_lot, right_lot, owner);
            else if (!left_lot_has_doc && !right_lot_has_doc)
            {
                gint cmp = gnc_numeric_compare (gnc_numeric_abs (left_lot_bal),
                                                gnc_numeric_abs (right_lot_bal));
                if (cmp >= 0)
                    gncOwnerOffsetLots (left_lot, right_lot, owner);
                else
                    gncOwnerOffsetLots (right_lot, left_lot, owner);
            }
            else
            {
                GNCLot *doc_lot = left_lot_has_doc ? left_lot : right_lot;
                GNCLot *pay_lot = left_lot_has_doc ? right_lot : left_lot;
                // Ok, let's try to move a payment from pay_lot to doc_lot
                gncOwnerOffsetLots (pay_lot, doc_lot, owner);
            }

            /* If we get here, then right_lot was modified
             * If the lot has a document, send an event for send an event for it as well
             * so it gets potentially updated as paid */

            {
                GncInvoice *this_invoice = gncInvoiceGetInvoiceFromLot(right_lot);
                if (this_invoice)
                    qof_event_gen (QOF_INSTANCE(this_invoice), QOF_EVENT_MODIFY, NULL);
            }
            left_modified = TRUE;
        }

        /* If left_lot was modified and the lot has a document,
         * send an event for send an event for it as well
         * so it gets potentially updated as paid */
        if (left_modified)
        {
            GncInvoice *this_invoice = gncInvoiceGetInvoiceFromLot(left_lot);
            if (this_invoice)
                qof_event_gen (QOF_INSTANCE(this_invoice), QOF_EVENT_MODIFY, NULL);
        }
        xaccAccountCommitEdit (acct);

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
    GNCLot *payment_lot = NULL;
    GList *selected_lots = NULL;

    /* Verify our arguments */
    if (!owner || !posted_acc
               || (!xfer_acc && !gnc_numeric_zero_p (amount)) ) return;
    g_return_if_fail (owner->owner.undefined);

    /* If there's a real amount to transfer create a lot for this payment */
    if (!gnc_numeric_zero_p (amount))
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
            GncInvoice *invoice = gncInvoiceGetInvoiceFromLot(lot);
            if (invoice)
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
