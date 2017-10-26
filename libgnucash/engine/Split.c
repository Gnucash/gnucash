/********************************************************************\
 * Split.c -- split implementation                                  *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1997-2003 Linas Vepstas <linas@linas.org>          *
 * Copyright (C) 2000 Bill Gribble <grib@billgribble.com>           *
 * Copyright (c) 2006 David Hampton <hampton@employees.org>         *
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

#include "config.h"

#include <platform.h>
#if PLATFORM(WINDOWS)
#include <windows.h>
#endif

#include <glib.h>
#include <glib/gi18n.h>
#include <stdlib.h>
#include <string.h>
#ifdef HAVE_SYS_TIME_H
# include <sys/time.h>
#endif
#include <time.h>
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#include "qof.h"
#include "qofbook.h"
#include "Split.h"
#include "Account.h"
#include "Scrub.h"
#include "TransactionP.h"
#include "TransLog.h"
#include "cap-gains.h"
#include "gnc-commodity.h"
#include "gnc-engine.h"
#include "gnc-lot.h"
#include "gnc-event.h"
#include "qofinstance-p.h"

const char *void_former_amt_str = "void-former-amount";
const char *void_former_val_str = "void-former-value";

#define PRICE_SIGFIGS 6

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_ENGINE;

/* KVP key values used for SX info stored Split's slots. */
#define GNC_SX_ID                    "sched-xaction"
#define GNC_SX_ACCOUNT               "account"
#define GNC_SX_CREDIT_FORMULA        "credit-formula"
#define GNC_SX_DEBIT_FORMULA         "debit-formula"
#define GNC_SX_CREDIT_NUMERIC        "credit-numeric"
#define GNC_SX_DEBIT_NUMERIC         "debit-numeric"
#define GNC_SX_SHARES                "shares"

enum
{
    PROP_0,
    PROP_TX,                    /* Table */
    PROP_ACCOUNT,               /* Table */
    PROP_MEMO,                  /* Table */
    PROP_ACTION,                /* Table */
//    PROP_RECONCILE_STATE,     /* Table */
    PROP_RECONCILE_DATE,        /* Table */
    PROP_VALUE,                 /* Table, in 2 fields */
    PROP_SX_ACCOUNT,            /* KVP */
    PROP_SX_CREDIT_FORMULA,     /* KVP */
    PROP_SX_CREDIT_NUMERIC,     /* KVP */
    PROP_SX_DEBIT_FORMULA,      /* KVP */
    PROP_SX_DEBIT_NUMERIC,      /* KVP */
    PROP_SX_SHARES,             /* KVP */
    PROP_LOT,                   /* KVP */
    PROP_ONLINE_ACCOUNT,        /* KVP */
    PROP_GAINS_SPLIT,           /* KVP */
    PROP_GAINS_SOURCE,          /* KVP */
    PROP_RUNTIME_0,
    PROP_AMOUNT,                /* Runtime */

};

/* GObject Initialization */
G_DEFINE_TYPE(Split, gnc_split, QOF_TYPE_INSTANCE)

static void
gnc_split_init(Split* split)
{
    /* fill in some sane defaults */
    split->acc         = NULL;
    split->orig_acc    = NULL;
    split->parent      = NULL;
    split->lot         = NULL;

    split->action      = CACHE_INSERT("");
    split->memo        = CACHE_INSERT("");
    split->reconciled  = NREC;
    split->amount      = gnc_numeric_zero();
    split->value       = gnc_numeric_zero();

    split->date_reconciled.tv_sec  = 0;
    split->date_reconciled.tv_nsec = 0;

    split->balance             = gnc_numeric_zero();
    split->cleared_balance     = gnc_numeric_zero();
    split->reconciled_balance  = gnc_numeric_zero();

    split->gains = GAINS_STATUS_UNKNOWN;
    split->gains_split = NULL;
}

static void
gnc_split_dispose(GObject *splitp)
{
    G_OBJECT_CLASS(gnc_split_parent_class)->dispose(splitp);
}

static void
gnc_split_finalize(GObject* splitp)
{
    G_OBJECT_CLASS(gnc_split_parent_class)->finalize(splitp);
}
/* Note that g_value_set_object() refs the object, as does
 * g_object_get(). But g_object_get() only unrefs once when it disgorges
 * the object, leaving an unbalanced ref, which leaks. So instead of
 * using g_value_set_object(), use g_value_take_object() which doesn't
 * ref the object when used in get_property().
 */
static void
gnc_split_get_property(GObject         *object,
                       guint            prop_id,
                       GValue          *value,
                       GParamSpec      *pspec)
{
    Split *split;
    gchar *key;

    g_return_if_fail(GNC_IS_SPLIT(object));

    split = GNC_SPLIT(object);
    switch (prop_id)
    {
        case PROP_ACTION:
            g_value_set_string(value, split->action);
            break;
        case PROP_MEMO:
            g_value_set_string(value, split->memo);
            break;
        case PROP_VALUE:
            g_value_set_boxed(value, &split->value);
            break;
        case PROP_AMOUNT:
            g_value_set_boxed(value, &split->amount);
            break;
        case PROP_RECONCILE_DATE:
            g_value_set_boxed(value, &split->date_reconciled);
            break;
        case PROP_TX:
            g_value_take_object(value, split->parent);
            break;
        case PROP_ACCOUNT:
            g_value_take_object(value, split->acc);
            break;
        case PROP_LOT:
            g_value_take_object(value, split->lot);
            break;
        case PROP_SX_CREDIT_FORMULA:
            key = GNC_SX_ID "/" GNC_SX_CREDIT_FORMULA;
            qof_instance_get_kvp (QOF_INSTANCE (split), key, value);
            break;
        case PROP_SX_CREDIT_NUMERIC:
            key = GNC_SX_ID "/" GNC_SX_CREDIT_NUMERIC;
            qof_instance_get_kvp (QOF_INSTANCE (split), key, value);
            break;
        case PROP_SX_DEBIT_FORMULA:
            key = GNC_SX_ID "/" GNC_SX_DEBIT_FORMULA;
            qof_instance_get_kvp (QOF_INSTANCE (split), key, value);
            break;
        case PROP_SX_DEBIT_NUMERIC:
            key = GNC_SX_ID "/" GNC_SX_DEBIT_NUMERIC;
            qof_instance_get_kvp (QOF_INSTANCE (split), key, value);
            break;
        case PROP_SX_ACCOUNT:
            key = GNC_SX_ID "/" GNC_SX_ACCOUNT;
            qof_instance_get_kvp (QOF_INSTANCE (split), key, value);
            break;
        case PROP_SX_SHARES:
            key = GNC_SX_ID "/" GNC_SX_SHARES;
            qof_instance_get_kvp (QOF_INSTANCE (split), key, value);
            break;
        case PROP_ONLINE_ACCOUNT:
            key = "online_id";
            qof_instance_get_kvp (QOF_INSTANCE (split), key, value);
            break;
        case PROP_GAINS_SPLIT:
            key = "gains-split";
            qof_instance_get_kvp (QOF_INSTANCE (split), key, value);
            break;
        case PROP_GAINS_SOURCE:
            key = "gains-source";
            qof_instance_get_kvp (QOF_INSTANCE (split), key, value);
            break;
        default:
            G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
            break;
    }
}

static void
gnc_split_set_property(GObject         *object,
                       guint            prop_id,
                       const GValue     *value,
                       GParamSpec      *pspec)
{
    Split *split;
    gnc_numeric* number;
    gchar *key;

    g_return_if_fail(GNC_IS_SPLIT(object));

    split = GNC_SPLIT(object);
    if (prop_id < PROP_RUNTIME_0 && split->parent != NULL)
        g_assert (qof_instance_get_editlevel(split->parent));

    switch (prop_id)
    {
        case PROP_ACTION:
            xaccSplitSetAction(split, g_value_get_string(value));
            break;
        case PROP_MEMO:
            xaccSplitSetMemo(split, g_value_get_string(value));
            break;
        case PROP_VALUE:
            number = g_value_get_boxed(value);
            xaccSplitSetValue(split, *number);
            break;
        case PROP_AMOUNT:
            number = g_value_get_boxed(value);
            xaccSplitSetAmount(split, *number);
            break;
        case PROP_RECONCILE_DATE:
            xaccSplitSetDateReconciledTS(split, g_value_get_boxed(value));
            break;
        case PROP_TX:
            xaccSplitSetParent(split, g_value_get_object(value));
            break;
        case PROP_ACCOUNT:
            xaccSplitSetAccount(split, g_value_get_object(value));
            break;
        case PROP_LOT:
            xaccSplitSetLot(split, g_value_get_object(value));
            break;
        case PROP_SX_CREDIT_FORMULA:
            key = GNC_SX_ID "/" GNC_SX_CREDIT_FORMULA;
            qof_instance_set_kvp (QOF_INSTANCE (split), key, value);
            break;
        case PROP_SX_CREDIT_NUMERIC:
            key = GNC_SX_ID "/" GNC_SX_CREDIT_NUMERIC;
            qof_instance_set_kvp (QOF_INSTANCE (split), key, value);
            break;
        case PROP_SX_DEBIT_FORMULA:
            key = GNC_SX_ID "/" GNC_SX_DEBIT_FORMULA;
            qof_instance_set_kvp (QOF_INSTANCE (split), key, value);
            break;
        case PROP_SX_DEBIT_NUMERIC:
            key = GNC_SX_ID "/" GNC_SX_DEBIT_NUMERIC;
            qof_instance_set_kvp (QOF_INSTANCE (split), key, value);
            break;
        case PROP_SX_ACCOUNT:
            key = GNC_SX_ID "/" GNC_SX_ACCOUNT;
            qof_instance_set_kvp (QOF_INSTANCE (split), key, value);
            break;
        case PROP_SX_SHARES:
            key = GNC_SX_ID "/" GNC_SX_SHARES;
            qof_instance_set_kvp (QOF_INSTANCE (split), key, value);
            break;
        case PROP_ONLINE_ACCOUNT:
            key = "online_id";
            qof_instance_set_kvp (QOF_INSTANCE (split), key, value);
            break;
        case PROP_GAINS_SPLIT:
            key = "gains-split";
            qof_instance_set_kvp (QOF_INSTANCE (split), key, value);
            break;
        case PROP_GAINS_SOURCE:
            key = "gains-source";
            qof_instance_set_kvp (QOF_INSTANCE (split), key, value);
            break;
        default:
            G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
            break;
    }
}

static void
gnc_split_class_init(SplitClass* klass)
{
    GObjectClass* gobject_class = G_OBJECT_CLASS(klass);

    gobject_class->dispose = gnc_split_dispose;
    gobject_class->finalize = gnc_split_finalize;
    gobject_class->set_property = gnc_split_set_property;
    gobject_class->get_property = gnc_split_get_property;

    g_object_class_install_property
        (gobject_class,
         PROP_ACTION,
         g_param_spec_string("action",
                             "Action",
                             "The action is an arbitrary string assigned "
                             "by the user.  It is intended to be a short "
                             "string that contains extra information about "
                             "this split.",
                             NULL,
                             G_PARAM_READWRITE));

    g_object_class_install_property
        (gobject_class,
         PROP_MEMO,
         g_param_spec_string("memo",
                             "Memo",
                             "The action is an arbitrary string assigned "
                             "by the user.  It is intended to be a short "
                             "string that describes the purpose of "
                             "this split.",
                             NULL,
                             G_PARAM_READWRITE));

    g_object_class_install_property
        (gobject_class,
         PROP_VALUE,
         g_param_spec_boxed("value",
                            "Split Value",
                            "The value for this split in the common currency. "
                            "The value and the amount provide enough information to "
                            "calculate a conversion rate.",
                            GNC_TYPE_NUMERIC,
                            G_PARAM_READWRITE));

    g_object_class_install_property
        (gobject_class,
         PROP_AMOUNT,
         g_param_spec_boxed("amount",
                            "Split Amount",
                            "The value for this split in the currency of its account. "
                            "The value and the amount provide enough information to "
                            "calculate a conversion rate.",
                            GNC_TYPE_NUMERIC,
                            G_PARAM_READWRITE));

    g_object_class_install_property
        (gobject_class,
         PROP_RECONCILE_DATE,
         g_param_spec_boxed("reconcile-date",
                            "Reconcile Date",
                            "The date this split was reconciled.",
                            GNC_TYPE_TIMESPEC,
                            G_PARAM_READWRITE));

    g_object_class_install_property
        (gobject_class,
         PROP_TX,
         g_param_spec_object ("transaction",
                              "Transaction",
                              "The transaction that this split belongs to.",
                              GNC_TYPE_TRANSACTION,
                              G_PARAM_READWRITE));

    g_object_class_install_property
        (gobject_class,
         PROP_ACCOUNT,
         g_param_spec_object ("account",
                              "Account",
                              "The account that this split belongs to.",
                              GNC_TYPE_ACCOUNT,
                              G_PARAM_READWRITE));

    g_object_class_install_property
        (gobject_class,
         PROP_LOT,
         g_param_spec_object ("lot",
                              "Lot",
                              "The lot that this split belongs to.",
                              GNC_TYPE_LOT,
                              G_PARAM_READWRITE));

    g_object_class_install_property
        (gobject_class,
         PROP_SX_DEBIT_FORMULA,
         g_param_spec_string("sx-debit-formula",
                             "Schedule Transaction Debit Formula",
                             "The formula used to calculate the actual debit "
                             "amount when a real split is generated from this "
                             "SX split.",
                             NULL,
                             G_PARAM_READWRITE));

    g_object_class_install_property
        (gobject_class,
         PROP_SX_DEBIT_NUMERIC,
         g_param_spec_boxed("sx-debit-numeric",
                            "Scheduled Transaction Debit Numeric",
                            "Numeric value to plug into the Debit Formula when a "
                            "real split is generated from this SX split.",
                            GNC_TYPE_NUMERIC,
                            G_PARAM_READWRITE));

    g_object_class_install_property
        (gobject_class,
         PROP_SX_CREDIT_FORMULA,
         g_param_spec_string("sx-credit-formula",
                             "Schedule Transaction Credit Formula",
                             "The formula used to calculate the actual credit "
                             "amount when a real split is generated from this "
                             "SX split.",
                             NULL,
                             G_PARAM_READWRITE));

    g_object_class_install_property
        (gobject_class,
         PROP_SX_CREDIT_NUMERIC,
         g_param_spec_boxed("sx-credit-numeric",
                            "Scheduled Transaction Credit Numeric",
                            "Numeric value to plug into the Credit Formula when a "
                            "real split is generated from this SX split.",
                            GNC_TYPE_NUMERIC,
                            G_PARAM_READWRITE));
/* FIXME: PROP_SX_SHARES should be stored as a gnc_numeric, but the function
 * which uses it, gnc_template_register_save_shares_cell, stores a
 * phony string. This is maintained until backwards compatibility can
 * be established.
 */
    g_object_class_install_property
        (gobject_class,
         PROP_SX_SHARES,
         g_param_spec_string("sx-shares",
                             "Scheduled Transaction Shares",
                             "Numeric value of shares to insert in a new split when "
                             "it's generated from this SX split.",
                             NULL,
                             G_PARAM_READWRITE));

    g_object_class_install_property
        (gobject_class,
         PROP_SX_ACCOUNT,
         g_param_spec_boxed("sx-account",
                            "Scheduled Transaction Account",
                            "The target account for a scheduled transaction split.",
                            GNC_TYPE_GUID,
                            G_PARAM_READWRITE));

    g_object_class_install_property
        (gobject_class,
         PROP_ONLINE_ACCOUNT,
         g_param_spec_string ("online-id",
                              "Online Account ID",
                              "The online account which corresponds to this "
                              "account for OFX/HCBI import",
                              NULL,
                              G_PARAM_READWRITE));

    g_object_class_install_property
        (gobject_class,
         PROP_GAINS_SPLIT,
         g_param_spec_boxed ("gains-split",
                             "Gains Split",
                             "The capital gains split associated with this "
                             "split when this split represents the proceeds "
                             "from the sale of a commodity inside a Lot.",
                             GNC_TYPE_GUID,
                             G_PARAM_READWRITE));

    g_object_class_install_property
        (gobject_class,
         PROP_GAINS_SOURCE,
         g_param_spec_boxed ("gains-source",
                             "Gains Source",
                             "The source split for which this split this is "
                             "the gains split. ",
                             GNC_TYPE_GUID,
                             G_PARAM_READWRITE));
}

/********************************************************************\
 * xaccInitSplit
 * Initialize a Split structure
\********************************************************************/

static void
xaccInitSplit(Split * split, QofBook *book)
{
    qof_instance_init_data(&split->inst, GNC_ID_SPLIT, book);
}

void
xaccSplitReinit(Split * split)
{
    /* fill in some sane defaults */
    split->acc         = NULL;
    split->orig_acc    = NULL;
    split->parent      = NULL;
    split->lot         = NULL;

    CACHE_REPLACE(split->action, "");
    CACHE_REPLACE(split->memo, "");
    split->reconciled  = NREC;
    split->amount      = gnc_numeric_zero();
    split->value       = gnc_numeric_zero();

    split->date_reconciled.tv_sec  = 0;
    split->date_reconciled.tv_nsec = 0;

    split->balance             = gnc_numeric_zero();
    split->cleared_balance     = gnc_numeric_zero();
    split->reconciled_balance  = gnc_numeric_zero();

    qof_instance_set_idata(split, 0);

    split->gains = GAINS_STATUS_UNKNOWN;
    split->gains_split = NULL;
}

/********************************************************************\
\********************************************************************/

Split *
xaccMallocSplit(QofBook *book)
{
    Split *split;
    g_return_val_if_fail (book, NULL);

    split = g_object_new (GNC_TYPE_SPLIT, NULL);
    xaccInitSplit (split, book);

    return split;
}

/********************************************************************\
\********************************************************************/
/* This routine is not exposed externally, since it does weird things,
 * like not really setting up the parent account correctly, and ditto
 * the parent transaction.  This routine is prone to programmer error
 * if not used correctly.  It is used only by the edit-rollback code.
 * Don't get duped!
 */

Split *
xaccDupeSplit (const Split *s)
{
    Split *split = g_object_new (GNC_TYPE_SPLIT, NULL);

    /* Trash the entity table. We don't want to mistake the cloned
     * splits as something official.  If we ever use this split, we'll
     * have to fix this up.
     */
    split->inst.e_type = NULL;
    qof_instance_copy_guid(split, s);
    qof_instance_copy_book(split, s);

    split->parent = s->parent;
    split->acc = s->acc;
    split->orig_acc = s->orig_acc;
    split->lot = s->lot;

    split->memo = CACHE_INSERT(s->memo);
    split->action = CACHE_INSERT(s->action);

    qof_instance_copy_kvp (QOF_INSTANCE (split), QOF_INSTANCE (s));

    split->reconciled = s->reconciled;
    split->date_reconciled = s->date_reconciled;

    split->value = s->value;
    split->amount = s->amount;

    /* no need to futz with the balances;  these get wiped each time ...
     * split->balance             = s->balance;
     * split->cleared_balance     = s->cleared_balance;
     * split->reconciled_balance  = s->reconciled_balance;
     */

    return split;
}

Split *
xaccSplitCloneNoKvp (const Split *s)
{
    Split *split = g_object_new (GNC_TYPE_SPLIT, NULL);

    split->parent              = NULL;
    split->memo                = CACHE_INSERT(s->memo);
    split->action              = CACHE_INSERT(s->action);
    split->reconciled          = s->reconciled;
    split->date_reconciled     = s->date_reconciled;
    split->value               = s->value;
    split->amount              = s->amount;
    split->balance             = s->balance;
    split->cleared_balance     = s->cleared_balance;
    split->reconciled_balance  = s->reconciled_balance;

    split->gains = GAINS_STATUS_UNKNOWN;
    split->gains_split = NULL;

    qof_instance_init_data(&split->inst, GNC_ID_SPLIT,
                           qof_instance_get_book(s));
    xaccAccountInsertSplit(s->acc, split);
    if (s->lot)
    {
        /* CHECKME: Is this right? */
        gnc_lot_add_split(s->lot, split);
    }
    return split;
}

void
xaccSplitCopyKvp (const Split *from, Split *to)
{
    qof_instance_copy_kvp (QOF_INSTANCE (to), QOF_INSTANCE (from));
}

/*################## Added for Reg2 #################*/

/* This is really a helper for xaccTransCopyOnto. It doesn't reparent
   the 'to' split to from's transaction, because xaccTransCopyOnto is
   responsible for parenting the split to the correct transaction.
   Also, from's parent transaction may not even be a valid
   transaction, so this function may not modify anything about 'from'
   or from's transaction.
*/
void
xaccSplitCopyOnto(const Split *from_split, Split *to_split)
{
    if (!from_split || !to_split) return;
    xaccTransBeginEdit (to_split->parent);

    xaccSplitSetMemo(to_split, xaccSplitGetMemo(from_split));
    xaccSplitSetAction(to_split, xaccSplitGetAction(from_split));
    xaccSplitSetAmount(to_split, xaccSplitGetAmount(from_split));
    xaccSplitSetValue(to_split, xaccSplitGetValue(from_split));
    /* Setting the account is okay here because, even though the from
       split might not really belong to the account it claims to,
       setting the account won't cause any event involving from. */
    xaccSplitSetAccount(to_split, xaccSplitGetAccount(from_split));
    /* N.B. Don't set parent. */

    qof_instance_set_dirty(QOF_INSTANCE(to_split));
    xaccTransCommitEdit(to_split->parent);
}

/*################## Added for Reg2 #################*/


#ifdef DUMP_FUNCTIONS
void
xaccSplitDump (const Split *split, const char *tag)
{
    printf("  %s Split %p", tag, split);
    printf("    Book:     %p\n", qof_instance_get_book(split));
    printf("    Account:  %p (%s)\n", split->acc,
           split->acc ? xaccAccountGetName(split->acc) : "");
    printf("    Commod:   %s\n",
           split->acc ?
           gnc_commodity_get_printname(xaccAccountGetCommodity(split->acc))
           : "");
    printf("    Lot:      %p\n", split->lot);
    printf("    Parent:   %p\n", split->parent);
    printf("    Gains:    %p\n", split->gains_split);
    printf("    Memo:     %s\n", split->memo ? split->memo : "(null)");
    printf("    Action:   %s\n", split->action ? split->action : "(null)");
    printf("    KVP Data: %s\n", qof_instance_kvp_as_string (QOF_INSTANCE (split)));
    printf("    Recncld:  %c (date %s)\n", split->reconciled,
           gnc_print_date(split->date_reconciled));
    printf("    Value:    %s\n", gnc_numeric_to_string(split->value));
    printf("    Amount:   %s\n", gnc_numeric_to_string(split->amount));
    printf("    Balance:  %s\n", gnc_numeric_to_string(split->balance));
    printf("    CBalance: %s\n", gnc_numeric_to_string(split->cleared_balance));
    printf("    RBalance: %s\n",
           gnc_numeric_to_string(split->reconciled_balance));
    printf("    idata:    %x\n", qof_instance_get_idata(split));
}
#endif

/********************************************************************\
\********************************************************************/

void
xaccFreeSplit (Split *split)
{
    if (!split) return;

    /* Debug double-free's */
    if (((char *) 1) == split->memo)
    {
        PERR ("double-free %p", split);
        return;
    }
    CACHE_REMOVE(split->memo);
    CACHE_REMOVE(split->action);

    /* Just in case someone looks up freed memory ... */
    split->memo        = (char *) 1;
    split->action      = NULL;
    split->reconciled  = NREC;
    split->amount      = gnc_numeric_zero();
    split->value       = gnc_numeric_zero();
    split->parent      = NULL;
    split->lot         = NULL;
    split->acc         = NULL;
    split->orig_acc    = NULL;

    split->date_reconciled.tv_sec = 0;
    split->date_reconciled.tv_nsec = 0;
    G_OBJECT_CLASS (QOF_INSTANCE_GET_CLASS (&split->inst))->dispose(G_OBJECT (split));
    // Is this right?
    if (split->gains_split) split->gains_split->gains_split = NULL;
    /* qof_instance_release(&split->inst); */
    g_object_unref(split);
}

void mark_split (Split *s)
{
    if (s->acc)
    {
        g_object_set(s->acc, "sort-dirty", TRUE, "balance-dirty", TRUE, NULL);
    }

    /* set dirty flag on lot too. */
    if (s->lot) gnc_lot_set_closed_unknown(s->lot);
}

/*
 * Helper routine for xaccSplitEqual.
 */
static gboolean
xaccSplitEqualCheckBal (const char *tag, gnc_numeric a, gnc_numeric b)
{
    char *str_a, *str_b;

    if (gnc_numeric_equal (a, b))
        return TRUE;

    str_a = gnc_numeric_to_string (a);
    str_b = gnc_numeric_to_string (b);

    PINFO ("%sbalances differ: %s vs %s", tag, str_a, str_b);

    g_free (str_a);
    g_free (str_b);

    return FALSE;
}

/********************************************************************
 * xaccSplitEqual
 ********************************************************************/
gboolean
xaccSplitEqual(const Split *sa, const Split *sb,
               gboolean check_guids,
               gboolean check_balances,
               gboolean check_txn_splits)
{
    gboolean same_book;

    if (!sa && !sb) return TRUE; /* Arguable. FALSE is better, methinks */

    if (!sa || !sb)
    {
        PINFO ("one is NULL");
        return FALSE;
    }

    if (sa == sb) return TRUE;

    same_book = qof_instance_get_book(QOF_INSTANCE(sa)) == qof_instance_get_book(QOF_INSTANCE(sb));

    if (check_guids)
    {
        if (qof_instance_guid_compare(sa, sb) != 0)
        {
            PINFO ("GUIDs differ");
            return FALSE;
        }
    }

    /* If the same book, since these strings are cached we can just use pointer equality */
    if ((same_book && sa->memo != sb->memo) || (!same_book && g_strcmp0(sa->memo, sb->memo) != 0))
    {
        PINFO ("memos differ: (%p)%s vs (%p)%s",
               sa->memo, sa->memo, sb->memo, sb->memo);
        return FALSE;
    }

    if ((same_book && sa->action != sb->action) || (!same_book && g_strcmp0(sa->action, sb->action) != 0))
    {
        PINFO ("actions differ: %s vs %s", sa->action, sb->action);
        return FALSE;
    }

    if (qof_instance_compare_kvp (QOF_INSTANCE (sa), QOF_INSTANCE (sb)) != 0)
    {
        char *frame_a;
        char *frame_b;

        frame_a = qof_instance_kvp_as_string (QOF_INSTANCE (sa));
        frame_b = qof_instance_kvp_as_string (QOF_INSTANCE (sb));

        PINFO ("kvp frames differ:\n%s\n\nvs\n\n%s", frame_a, frame_b);

        g_free (frame_a);
        g_free (frame_b);

        return FALSE;
    }

    if (sa->reconciled != sb->reconciled)
    {
        PINFO ("reconcile flags differ: %c vs %c", sa->reconciled, sb->reconciled);
        return FALSE;
    }

    if (timespec_cmp(&(sa->date_reconciled), &(sb->date_reconciled)))
    {
        PINFO ("reconciled date differs");
        return FALSE;
    }

    if (!gnc_numeric_eq(xaccSplitGetAmount (sa), xaccSplitGetAmount (sb)))
    {
        char *str_a;
        char *str_b;

        str_a = gnc_numeric_to_string (xaccSplitGetAmount (sa));
        str_b = gnc_numeric_to_string (xaccSplitGetAmount (sb));

        PINFO ("amounts differ: %s vs %s", str_a, str_b);

        g_free (str_a);
        g_free (str_b);

        return FALSE;
    }

    if (!gnc_numeric_eq(xaccSplitGetValue (sa), xaccSplitGetValue (sb)))
    {
        char *str_a;
        char *str_b;

        str_a = gnc_numeric_to_string (xaccSplitGetValue (sa));
        str_b = gnc_numeric_to_string (xaccSplitGetValue (sb));

        PINFO ("values differ: %s vs %s", str_a, str_b);

        g_free (str_a);
        g_free (str_b);

        return FALSE;
    }

    if (check_balances)
    {
        if (!xaccSplitEqualCheckBal ("", sa->balance, sb->balance))
            return FALSE;
        if (!xaccSplitEqualCheckBal ("cleared ", sa->cleared_balance,
                                     sb->cleared_balance))
            return FALSE;
        if (!xaccSplitEqualCheckBal ("reconciled ", sa->reconciled_balance,
                                     sb->reconciled_balance))
            return FALSE;
    }

    if (!xaccTransEqual(sa->parent, sb->parent, check_guids, check_txn_splits,
                        check_balances, FALSE))
    {
        PINFO ("transactions differ");
        return FALSE;
    }

    return TRUE;
}


/*################## Added for Reg2 #################*/
/********************************************************************
 * xaccSplitListGetUniqueTransactions
 ********************************************************************/
GList *
xaccSplitListGetUniqueTransactions(const GList *splits)
{
    const GList *snode;
    GList *transList = NULL;

    for(snode = splits; snode; snode = snode->next)
    {
        Transaction *trans = xaccSplitGetParent((Split *)(snode->data));

        GList *item = g_list_find (transList, trans);
        if (item == NULL)
            transList = g_list_append (transList, trans);
    }
    return transList;
}
/*################## Added for Reg2 #################*/


/********************************************************************
 * Account funcs
 ********************************************************************/

Account *
xaccSplitGetAccount (const Split *s)
{
    return s ? s->acc : NULL;
}

void
xaccSplitSetAccount (Split *s, Account *acc)
{
    Transaction *trans;

    g_return_if_fail(s && acc);
    g_return_if_fail(qof_instance_books_equal(acc, s));

    trans = s->parent;
    if (trans)
        xaccTransBeginEdit(trans);

    s->acc = acc;
    qof_instance_set_dirty(QOF_INSTANCE(s));

    if (trans)
        xaccTransCommitEdit(trans);
}

static void commit_err (QofInstance *inst, QofBackendError errcode)
{
    PERR("commit error: %d", errcode);
    gnc_engine_signal_commit_error( errcode );
}

/* An engine-private helper for completing xaccTransCommitEdit(). */
void
xaccSplitCommitEdit(Split *s)
{
    Account *acc = NULL;
    Account *orig_acc = NULL;

    g_return_if_fail(s);
    if (!qof_instance_is_dirty(QOF_INSTANCE(s)))
        return;

    orig_acc = s->orig_acc;

    if (GNC_IS_ACCOUNT(s->acc))
        acc = s->acc;

    /* Remove from lot (but only if it hasn't been moved to
       new lot already) */
    if (s->lot && (gnc_lot_get_account(s->lot) != acc || qof_instance_get_destroying(s)))
        gnc_lot_remove_split (s->lot, s);

    /* Possibly remove the split from the original account... */
    if (orig_acc && (orig_acc != acc || qof_instance_get_destroying(s)))
    {
        if (!gnc_account_remove_split(orig_acc, s))
        {
            PERR("Account lost track of moved or deleted split.");
        }
    }

    /* ... and insert it into the new account if needed */
    if (acc && (orig_acc != acc) && !qof_instance_get_destroying(s))
    {
        if (gnc_account_insert_split(acc, s))
        {
            /* If the split's lot belonged to some other account, we
               leave it so. */
            if (s->lot && (NULL == gnc_lot_get_account(s->lot)))
                xaccAccountInsertLot (acc, s->lot);
        }
        else
        {
            PERR("Account grabbed split prematurely.");
        }
        xaccSplitSetAmount(s, xaccSplitGetAmount(s));
    }

    if (s->parent != s->orig_parent)
    {
        //FIXME: find better event
        if (s->orig_parent)
            qof_event_gen(&s->orig_parent->inst, QOF_EVENT_MODIFY,
                          NULL);
    }
    if (s->lot)
    {
        /* A change of value/amnt affects gains display, etc. */
        qof_event_gen (QOF_INSTANCE(s->lot), QOF_EVENT_MODIFY, NULL);
    }

    /* Important: we save off the original parent transaction and account
       so that when we commit, we can generate signals for both the
       original and new transactions, for the _next_ begin/commit cycle. */
    s->orig_acc = s->acc;
    s->orig_parent = s->parent;
    if (!qof_commit_edit_part2(QOF_INSTANCE(s), commit_err, NULL,
                               (void (*) (QofInstance *)) xaccFreeSplit))
        return;

    if (acc)
    {
        g_object_set(acc, "sort-dirty", TRUE, "balance-dirty", TRUE, NULL);
        xaccAccountRecomputeBalance(acc);
    }
}

/* An engine-private helper for completing xaccTransRollbackEdit(). */
void
xaccSplitRollbackEdit(Split *s)
{

    /* Don't use setters because we want to allow NULL.  This is legit
       only because we don't emit events for changing accounts until
       the final commit. */
    if (s->acc != s->orig_acc)
        s->acc = s->orig_acc;

    /* Undestroy if needed */
    if (qof_instance_get_destroying(s) && s->parent)
    {
        GncEventData ed;
        qof_instance_set_destroying(s, FALSE);
        ed.node = s;
        ed.idx = -1; /* unused */
        qof_event_gen(&s->parent->inst, GNC_EVENT_ITEM_ADDED, &ed);
    }

    /* But for the parent trans, we want the intermediate events, so
       we use the setter. */
    xaccSplitSetParent(s, s->orig_parent);
}

/********************************************************************\
\********************************************************************/

Split *
xaccSplitLookup (const GncGUID *guid, QofBook *book)
{
    QofCollection *col;
    if (!guid || !book) return NULL;
    col = qof_book_get_collection (book, GNC_ID_SPLIT);
    return (Split *) qof_collection_lookup_entity (col, guid);
}

/********************************************************************\
\********************************************************************/
/* Routines for marking splits dirty, and for sending out change
 * events.  Note that we can't just mark-n-generate-event in one
 * step, since sometimes we need to mark things up before its suitable
 * to send out a change event.
 */

/* CHECKME: This function modifies the Split without dirtying or
   checking its parent.  Is that correct? */
void
xaccSplitDetermineGainStatus (Split *split)
{
    Split *other;
    GValue v = G_VALUE_INIT;
    GncGUID *guid = NULL;

    if (GAINS_STATUS_UNKNOWN != split->gains) return;

    other = xaccSplitGetCapGainsSplit (split);
    if (other)
    {
        split->gains = GAINS_STATUS_A_VDIRTY | GAINS_STATUS_DATE_DIRTY;
        split->gains_split = other;
        return;
    }

    qof_instance_get_kvp (QOF_INSTANCE (split), "gains-source", &v);
    if (G_VALUE_HOLDS_BOXED (&v))
        guid = (GncGUID*)g_value_get_boxed (&v);
    if (!guid)
    {
        // CHECKME: We leave split->gains_split alone.  Is that correct?
        split->gains = GAINS_STATUS_A_VDIRTY | GAINS_STATUS_DATE_DIRTY;
    }
    else
    {
        QofCollection *col;
        col = qof_book_get_collection (qof_instance_get_book(split),
				       GNC_ID_SPLIT);
        split->gains = GAINS_STATUS_GAINS;
        other = (Split *) qof_collection_lookup_entity (col, guid);
        split->gains_split = other;
    }
}

/********************************************************************\
\********************************************************************/

static inline int
get_currency_denom(const Split * s)
{
    if (!s)
    {
        return 0;
    }
    else if (!s->parent || !s->parent->common_currency)
    {
        return 1000000; /* Max supported denom to avoid premature rounding. */
    }
    else
    {
        return gnc_commodity_get_fraction (s->parent->common_currency);
    }
}

static inline int
get_commodity_denom(const Split * s)
{
    if (!s)
    {
        return 0;
    }
    else if (!s->acc)
    {
        return 1000000; /* Max supported denom to avoid premature rounding. */
    }
    else
    {
        return xaccAccountGetCommoditySCU(s->acc);
    }
}

/********************************************************************\
\********************************************************************/

void
xaccSplitSetSharePriceAndAmount (Split *s, gnc_numeric price, gnc_numeric amt)
{
    if (!s) return;
    ENTER (" ");
    xaccTransBeginEdit (s->parent);

    s->amount = gnc_numeric_convert(amt, get_commodity_denom(s),
                                    GNC_HOW_RND_ROUND_HALF_UP);
    s->value  = gnc_numeric_mul(s->amount, price,
                                get_currency_denom(s), GNC_HOW_RND_ROUND_HALF_UP);

    SET_GAINS_A_VDIRTY(s);
    mark_split (s);
    qof_instance_set_dirty(QOF_INSTANCE(s));
    xaccTransCommitEdit(s->parent);
    LEAVE ("");
}

static void
qofSplitSetSharePrice (Split *split, gnc_numeric price)
{
    g_return_if_fail(split);
    split->value = gnc_numeric_mul(xaccSplitGetAmount(split),
                                   price, get_currency_denom(split),
                                   GNC_HOW_RND_ROUND_HALF_UP);
}

void
xaccSplitSetSharePrice (Split *s, gnc_numeric price)
{
    if (!s) return;
    ENTER (" ");
    xaccTransBeginEdit (s->parent);

    s->value = gnc_numeric_mul(xaccSplitGetAmount(s),
                               price, get_currency_denom(s),
                               GNC_HOW_RND_ROUND_HALF_UP);

    SET_GAINS_VDIRTY(s);
    mark_split (s);
    qof_instance_set_dirty(QOF_INSTANCE(s));
    xaccTransCommitEdit(s->parent);
    LEAVE ("");
}

static void
qofSplitSetAmount (Split *split, gnc_numeric amt)
{
    g_return_if_fail(split);
    if (split->acc)
    {
        split->amount = gnc_numeric_convert(amt,
                                            get_commodity_denom(split), GNC_HOW_RND_ROUND_HALF_UP);
    }
    else
    {
        split->amount = amt;
    }
}

/* The amount of the split in the _account's_ commodity. */
void
xaccSplitSetAmount (Split *s, gnc_numeric amt)
{
    if (!s) return;
    g_return_if_fail(gnc_numeric_check(amt) == GNC_ERROR_OK);
    ENTER ("(split=%p) old amt=%" G_GINT64_FORMAT "/%" G_GINT64_FORMAT
           " new amt=%" G_GINT64_FORMAT "/%" G_GINT64_FORMAT, s,
           s->amount.num, s->amount.denom, amt.num, amt.denom);

    xaccTransBeginEdit (s->parent);
    if (s->acc)
    {
        s->amount = gnc_numeric_convert(amt, get_commodity_denom(s),
                                        GNC_HOW_RND_ROUND_HALF_UP);
        g_assert (gnc_numeric_check (s->amount) == GNC_ERROR_OK);
    }
    else
        s->amount = amt;

    SET_GAINS_ADIRTY(s);
    mark_split (s);
    qof_instance_set_dirty(QOF_INSTANCE(s));
    xaccTransCommitEdit(s->parent);
    LEAVE("");
}

static void
qofSplitSetValue (Split *split, gnc_numeric amt)
{
    g_return_if_fail(split);
    split->value = gnc_numeric_convert(amt,
                                       get_currency_denom(split), GNC_HOW_RND_ROUND_HALF_UP);
    g_assert(gnc_numeric_check (split->value) != GNC_ERROR_OK);
}

/* The value of the split in the _transaction's_ currency. */
void
xaccSplitSetValue (Split *s, gnc_numeric amt)
{
    gnc_numeric new_val;
    if (!s) return;

    g_return_if_fail(gnc_numeric_check(amt) == GNC_ERROR_OK);
    ENTER ("(split=%p) old val=%" G_GINT64_FORMAT "/%" G_GINT64_FORMAT
           " new val=%" G_GINT64_FORMAT "/%" G_GINT64_FORMAT, s,
           s->value.num, s->value.denom, amt.num, amt.denom);

    xaccTransBeginEdit (s->parent);
    new_val = gnc_numeric_convert(amt, get_currency_denom(s),
                                  GNC_HOW_RND_ROUND_HALF_UP);
    if (gnc_numeric_check(new_val) == GNC_ERROR_OK &&
        !(gnc_numeric_zero_p (new_val) && !gnc_numeric_zero_p (amt)))
        s->value = new_val;
    else PERR("numeric error %s in converting the split value's denominator with amount %s and denom  %d", gnc_numeric_errorCode_to_string(gnc_numeric_check(new_val)), gnc_numeric_to_string(amt), get_currency_denom(s));

    SET_GAINS_VDIRTY(s);
    mark_split (s);
    qof_instance_set_dirty(QOF_INSTANCE(s));
    xaccTransCommitEdit(s->parent);
    LEAVE ("");
}

/********************************************************************\
\********************************************************************/

gnc_numeric
xaccSplitGetBalance (const Split *s)
{
    return s ? s->balance : gnc_numeric_zero();
}

gnc_numeric
xaccSplitGetClearedBalance (const Split *s)
{
    return s ? s->cleared_balance : gnc_numeric_zero();
}

gnc_numeric
xaccSplitGetReconciledBalance (const Split *s)
{
    return s ? s->reconciled_balance : gnc_numeric_zero();
}

void
xaccSplitSetBaseValue (Split *s, gnc_numeric value,
                       const gnc_commodity * base_currency)
{
    const gnc_commodity *currency;
    const gnc_commodity *commodity;

    if (!s) return;
    xaccTransBeginEdit (s->parent);

    if (!s->acc)
    {
        PERR ("split must have a parent account");
        return;
    }

    currency = xaccTransGetCurrency (s->parent);
    commodity = xaccAccountGetCommodity (s->acc);

    /* If the base_currency is the transaction's commodity ('currency'),
     * set the value.  If it's the account commodity, set the
     * amount. If both, set both. */
    if (gnc_commodity_equiv(currency, base_currency))
    {
        if (gnc_commodity_equiv(commodity, base_currency))
        {
            s->amount = gnc_numeric_convert(value,
                                            get_commodity_denom(s),
                                            GNC_HOW_RND_ROUND_HALF_UP);
        }
        s->value = gnc_numeric_convert(value,
                                       get_currency_denom(s),
                                       GNC_HOW_RND_ROUND_HALF_UP);
    }
    else if (gnc_commodity_equiv(commodity, base_currency))
    {
        s->amount = gnc_numeric_convert(value, get_commodity_denom(s),
                                        GNC_HOW_RND_ROUND_HALF_UP);
    }
    else
    {
        PERR ("inappropriate base currency %s "
              "given split currency=%s and commodity=%s\n",
              gnc_commodity_get_printname(base_currency),
              gnc_commodity_get_printname(currency),
              gnc_commodity_get_printname(commodity));
        return;
    }

    SET_GAINS_A_VDIRTY(s);
    mark_split (s);
    qof_instance_set_dirty(QOF_INSTANCE(s));
    xaccTransCommitEdit(s->parent);
}

gnc_numeric
xaccSplitGetBaseValue (const Split *s, const gnc_commodity * base_currency)
{
    if (!s || !s->acc || !s->parent) return gnc_numeric_zero();

    /* be more precise -- the value depends on the currency we want it
     * expressed in.  */
    if (gnc_commodity_equiv(xaccTransGetCurrency(s->parent), base_currency))
        return xaccSplitGetValue(s);
    if (gnc_commodity_equiv(xaccAccountGetCommodity(s->acc), base_currency))
        return xaccSplitGetAmount(s);

    PERR ("inappropriate base currency %s "
          "given split currency=%s and commodity=%s\n",
          gnc_commodity_get_printname(base_currency),
          gnc_commodity_get_printname(xaccTransGetCurrency (s->parent)),
          gnc_commodity_get_printname(xaccAccountGetCommodity(s->acc)));
    return gnc_numeric_zero();
}

/********************************************************************\
\********************************************************************/

gnc_numeric
xaccSplitConvertAmount (const Split *split, const Account * account)
{
    gnc_commodity *acc_com, *to_commodity;
    Transaction *txn;
    gnc_numeric amount, value, convrate;
    Account * split_acc;

    amount = xaccSplitGetAmount (split);

    /* If this split is attached to this account, OR */
    split_acc = xaccSplitGetAccount (split);
    if (split_acc == account)
        return amount;

    /* If split->account->commodity == to_commodity, return the amount */
    acc_com = xaccAccountGetCommodity (split_acc);
    to_commodity = xaccAccountGetCommodity (account);
    if (acc_com && gnc_commodity_equal (acc_com, to_commodity))
        return amount;

    /* Ok, this split is not for the viewed account, and the commodity
     * does not match.  So we need to do some conversion.
     *
     * First, we can cheat.  If this transaction is balanced and has
     * exactly two splits, then we can implicitly determine the exchange
     * rate and just return the 'other' split amount.
     */
    txn = xaccSplitGetParent (split);
    if (txn && xaccTransIsBalanced (txn))
    {
        const Split *osplit = xaccSplitGetOtherSplit (split);

        if (osplit)
        {
            gnc_commodity* split_comm =
                xaccAccountGetCommodity(xaccSplitGetAccount(osplit));
            if (!gnc_commodity_equal(to_commodity, split_comm))
            {
                gchar guidstr[GUID_ENCODING_LENGTH+1];
                guid_to_string_buff(xaccSplitGetGUID(osplit),guidstr);
                PERR("The split's (%s) amount can't be converted from %s into %s.",
                     guidstr,
                     gnc_commodity_get_mnemonic(split_comm),
                     gnc_commodity_get_mnemonic(to_commodity)
                    );
                return gnc_numeric_zero();
            }
            return gnc_numeric_neg (xaccSplitGetAmount (osplit));
        }
    }

    /* ... otherwise, we need to compute the amount from the conversion
     * rate into _this account_.  So, find the split into this account,
     * compute the conversion rate (based on amount/value), and then multiply
     * this times the split value.
     */
    value = xaccSplitGetValue (split);

    if (gnc_numeric_zero_p (value))
    {
        return value;
    }

    convrate = xaccTransGetAccountConvRate(txn, account);
    return gnc_numeric_mul (value, convrate,
                            gnc_commodity_get_fraction (to_commodity),
                            GNC_HOW_RND_ROUND_HALF_UP);
}

/********************************************************************\
\********************************************************************/

gboolean
xaccSplitDestroy (Split *split)
{
    Account *acc;
    Transaction *trans;
    GncEventData ed;

    if (!split) return TRUE;

    acc = split->acc;
    trans = split->parent;
    if (acc && !qof_instance_get_destroying(acc)
        && xaccTransGetReadOnly(trans))
        return FALSE;

    xaccTransBeginEdit(trans);
    ed.node = split;
    ed.idx = xaccTransGetSplitIndex(trans, split);
    qof_instance_set_dirty(QOF_INSTANCE(split));
    qof_instance_set_destroying(split, TRUE);
    qof_event_gen(&trans->inst, GNC_EVENT_ITEM_REMOVED, &ed);
    xaccTransCommitEdit(trans);

    return TRUE;
}

/********************************************************************\
\********************************************************************/

gint
xaccSplitOrder (const Split *sa, const Split *sb)
{
    int retval;
    int comp;
    char *da, *db;
    gboolean action_for_num;

    if (sa == sb) return 0;
    /* nothing is always less than something */
    if (!sa) return -1;
    if (!sb) return +1;

    /* sort in transaction order, but use split action rather than trans num
     * according to book option */
    action_for_num = qof_book_use_split_action_for_num_field
        (xaccSplitGetBook (sa));
    if (action_for_num)
        retval = xaccTransOrder_num_action (sa->parent, sa->action,
                                            sb->parent, sb->action);
    else
        retval = xaccTransOrder (sa->parent, sb->parent);
    if (retval) return retval;

    /* otherwise, sort on memo strings */
    da = sa->memo ? sa->memo : "";
    db = sb->memo ? sb->memo : "";
    retval = g_utf8_collate (da, db);
    if (retval)
        return retval;

    /* otherwise, sort on action strings */
    da = sa->action ? sa->action : "";
    db = sb->action ? sb->action : "";
    retval = g_utf8_collate (da, db);
    if (retval != 0)
        return retval;

    /* the reconciled flag ... */
    if (sa->reconciled < sb->reconciled) return -1;
    if (sa->reconciled > sb->reconciled) return +1;

    /* compare amounts */
    comp = gnc_numeric_compare(xaccSplitGetAmount(sa), xaccSplitGetAmount (sb));
    if (comp < 0) return -1;
    if (comp > 0) return +1;

    comp = gnc_numeric_compare(xaccSplitGetValue(sa), xaccSplitGetValue (sb));
    if (comp < 0) return -1;
    if (comp > 0) return +1;

    /* if dates differ, return */
    DATE_CMP(sa, sb, date_reconciled);

    /* else, sort on guid - keeps sort stable. */
    retval = qof_instance_guid_compare(sa, sb);
    if (retval) return retval;

    return 0;
}

gint
xaccSplitOrderDateOnly (const Split *sa, const Split *sb)
{
    Transaction *ta, *tb;

    if (sa == sb) return 0;
    /* nothing is always less than something */
    if (!sa) return -1;
    if (!sb) return +1;

    ta = sa->parent;
    tb = sb->parent;
    if ( !ta && !tb ) return 0;
    if ( !tb ) return -1;
    if ( !ta ) return +1;

    /* if dates differ, return */
    DATE_CMP(ta, tb, date_posted);

    /* If the dates are the same, do not change the order */
    return -1;
}

static gboolean
get_corr_account_split(const Split *sa, const Split **retval)
{
    *retval = NULL;
    g_return_val_if_fail(sa, FALSE);

    if (xaccTransCountSplits (sa->parent) > 2)
        return FALSE;

    *retval = xaccSplitGetOtherSplit (sa);
    if (*retval)
        return TRUE;
    else
        return FALSE;
}

/* TODO: these static consts can be shared. */
const char *
xaccSplitGetCorrAccountName(const Split *sa)
{
    static const char *split_const = NULL;
    const Split *other_split;

    if (!get_corr_account_split(sa, &other_split))
    {
        if (!split_const)
            split_const = _("-- Split Transaction --");

        return split_const;
    }

    return xaccAccountGetName(other_split->acc);
}

char *
xaccSplitGetCorrAccountFullName(const Split *sa)
{
    static const char *split_const = NULL;
    const Split *other_split;

    if (!get_corr_account_split(sa, &other_split))
    {
        if (!split_const)
            split_const = _("-- Split Transaction --");

        return g_strdup(split_const);
    }
    return gnc_account_get_full_name(other_split->acc);
}

const char *
xaccSplitGetCorrAccountCode(const Split *sa)
{
    static const char *split_const = NULL;
    const Split *other_split;

    if (!get_corr_account_split(sa, &other_split))
    {
        if (!split_const)
            /* Translators: This string has a disambiguation prefix */
            split_const = Q_("Displayed account code of the other account in a multi-split transaction|Split");

        return split_const;
    }
    return xaccAccountGetCode(other_split->acc);
}

/* TODO: It's not too hard to make this function avoid the malloc/free. */
int
xaccSplitCompareAccountFullNames(const Split *sa, const Split *sb)
{
    Account *aa, *ab;
    char *full_a, *full_b;
    int retval;
    if (!sa && !sb) return 0;
    if (!sa) return -1;
    if (!sb) return 1;

    aa = sa->acc;
    ab = sb->acc;
    full_a = gnc_account_get_full_name(aa);
    full_b = gnc_account_get_full_name(ab);
    retval = g_utf8_collate(full_a, full_b);
    g_free(full_a);
    g_free(full_b);
    return retval;
}


int
xaccSplitCompareAccountCodes(const Split *sa, const Split *sb)
{
    Account *aa, *ab;
    if (!sa && !sb) return 0;
    if (!sa) return -1;
    if (!sb) return 1;

    aa = sa->acc;
    ab = sb->acc;

    return g_strcmp0(xaccAccountGetCode(aa), xaccAccountGetCode(ab));
}

int
xaccSplitCompareOtherAccountFullNames(const Split *sa, const Split *sb)
{
    char *ca, *cb;
    int retval;
    if (!sa && !sb) return 0;
    if (!sa) return -1;
    if (!sb) return 1;

    /* doesn't matter what separator we use
     * as long as they are the same
     */

    ca = xaccSplitGetCorrAccountFullName(sa);
    cb = xaccSplitGetCorrAccountFullName(sb);
    retval = g_strcmp0(ca, cb);
    g_free(ca);
    g_free(cb);
    return retval;
}

int
xaccSplitCompareOtherAccountCodes(const Split *sa, const Split *sb)
{
    const char *ca, *cb;
    if (!sa && !sb) return 0;
    if (!sa) return -1;
    if (!sb) return 1;

    ca = xaccSplitGetCorrAccountCode(sa);
    cb = xaccSplitGetCorrAccountCode(sb);
    return g_strcmp0(ca, cb);
}

static void
qofSplitSetMemo (Split *split, const char* memo)
{
    g_return_if_fail(split);
    CACHE_REPLACE(split->memo, memo);
}

void
xaccSplitSetMemo (Split *split, const char *memo)
{
    if (!split || !memo) return;
    xaccTransBeginEdit (split->parent);

    CACHE_REPLACE(split->memo, memo);
    qof_instance_set_dirty(QOF_INSTANCE(split));
    xaccTransCommitEdit(split->parent);

}

static void
qofSplitSetAction (Split *split, const char *actn)
{
    g_return_if_fail(split);
    CACHE_REPLACE(split->action, actn);
}

void
xaccSplitSetAction (Split *split, const char *actn)
{
    if (!split || !actn) return;
    xaccTransBeginEdit (split->parent);

    CACHE_REPLACE(split->action, actn);
    qof_instance_set_dirty(QOF_INSTANCE(split));
    xaccTransCommitEdit(split->parent);

}

static void
qofSplitSetReconcile (Split *split, char recn)
{
    g_return_if_fail(split);
    switch (recn)
    {
        case NREC:
        case CREC:
        case YREC:
        case FREC:
        case VREC:
            split->reconciled = recn;
            mark_split (split);
            xaccAccountRecomputeBalance (split->acc);
            break;
        default:
            PERR("Bad reconciled flag");
            break;
    }
}

void
xaccSplitSetReconcile (Split *split, char recn)
{
    if (!split || split->reconciled == recn) return;
    xaccTransBeginEdit (split->parent);

    switch (recn)
    {
        case NREC:
        case CREC:
        case YREC:
        case FREC:
        case VREC:
            split->reconciled = recn;
            mark_split (split);
            qof_instance_set_dirty(QOF_INSTANCE(split));
            xaccAccountRecomputeBalance (split->acc);
            break;
        default:
            PERR("Bad reconciled flag");
            break;
    }
    xaccTransCommitEdit(split->parent);

}

void
xaccSplitSetDateReconciledSecs (Split *split, time64 secs)
{
    if (!split) return;
    xaccTransBeginEdit (split->parent);

    split->date_reconciled.tv_sec = secs;
    split->date_reconciled.tv_nsec = 0;
    qof_instance_set_dirty(QOF_INSTANCE(split));
    xaccTransCommitEdit(split->parent);

}

void
xaccSplitSetDateReconciledTS (Split *split, Timespec *ts)
{
    if (!split || !ts) return;
    xaccTransBeginEdit (split->parent);

    split->date_reconciled = *ts;
    qof_instance_set_dirty(QOF_INSTANCE(split));
    xaccTransCommitEdit(split->parent);

}

void
xaccSplitGetDateReconciledTS (const Split * split, Timespec *ts)
{
    if (!split || !ts) return;
    *ts = (split->date_reconciled);
}

Timespec
xaccSplitRetDateReconciledTS (const Split * split)
{
    Timespec ts = {0, 0};
    return split ? split->date_reconciled : ts;
}

/*################## Added for Reg2 #################*/
time64
xaccSplitGetDateReconciled (const Split * split)
{
    return split ? split->date_reconciled.tv_sec : 0;
}
/*################## Added for Reg2 #################*/

/********************************************************************\
\********************************************************************/

/* return the parent transaction of the split */
Transaction *
xaccSplitGetParent (const Split *split)
{
    return split ? split->parent : NULL;
}

void
xaccSplitSetParent(Split *s, Transaction *t)
{
    Transaction *old_trans;
    GncEventData ed;

    g_return_if_fail(s);
    if (s->parent == t) return;

    if (s->parent != s->orig_parent && s->orig_parent != t)
        PERR("You may not add the split to more than one transaction"
             " during the BeginEdit/CommitEdit block.");
    xaccTransBeginEdit(t);
    old_trans = s->parent;

    xaccTransBeginEdit(old_trans);

    ed.node = s;
    if (old_trans)
    {
        ed.idx = xaccTransGetSplitIndex(old_trans, s);
        qof_event_gen(&old_trans->inst, GNC_EVENT_ITEM_REMOVED, &ed);
    }
    s->parent = t;

    xaccTransCommitEdit(old_trans);
    qof_instance_set_dirty(QOF_INSTANCE(s));

    if (t)
    {
        /* Convert split to new transaction's commodity denominator */
        xaccSplitSetValue(s, xaccSplitGetValue(s));

        /* add ourselves to the new transaction's list of pending splits. */
        if (NULL == g_list_find(t->splits, s))
            t->splits = g_list_append(t->splits, s);

        ed.idx = -1; /* unused */
        qof_event_gen(&t->inst, GNC_EVENT_ITEM_ADDED, &ed);
    }
    xaccTransCommitEdit(t);
}


GNCLot *
xaccSplitGetLot (const Split *split)
{
    return split ? split->lot : NULL;
}

void
xaccSplitSetLot(Split* split, GNCLot* lot)
{
    xaccTransBeginEdit (split->parent);
    split->lot = lot;
    qof_instance_set_dirty(QOF_INSTANCE(split));
    xaccTransCommitEdit(split->parent);
}

const char *
xaccSplitGetMemo (const Split *split)
{
    return split ? split->memo : NULL;
}

const char *
xaccSplitGetAction (const Split *split)
{
    return split ? split->action : NULL;
}

char
xaccSplitGetReconcile (const Split *split)
{
    return split ? split->reconciled : ' ';
}


gnc_numeric
xaccSplitGetAmount (const Split * split)
{
    return split ? split->amount : gnc_numeric_zero();
}

gnc_numeric
xaccSplitGetValue (const Split * split)
{
    return split ? split->value : gnc_numeric_zero();
}

gnc_numeric
xaccSplitGetSharePrice (const Split * split)
{
    gnc_numeric amt, val, price;
    if (!split) return gnc_numeric_create(1, 1);


    /* if amount == 0 and value == 0, then return 1.
     * if amount == 0 and value != 0 then return 0.
     * otherwise return value/amount
     */

    amt = xaccSplitGetAmount(split);
    val = xaccSplitGetValue(split);
    if (gnc_numeric_zero_p(amt))
    {
        if (gnc_numeric_zero_p(val))
            return gnc_numeric_create(1, 1);
        return gnc_numeric_create(0, 1);
    }
    price = gnc_numeric_div(val, amt,
                            GNC_DENOM_AUTO,
                            GNC_HOW_DENOM_SIGFIGS(PRICE_SIGFIGS) |
                            GNC_HOW_RND_ROUND_HALF_UP);

    /* During random checks we can get some very weird prices.  Let's
     * handle some overflow and other error conditions by returning
     * zero.  But still print an error to let us know it happened.
     */
    if (gnc_numeric_check(price))
    {
        PERR("Computing share price failed (%d): [ %" G_GINT64_FORMAT " / %"
             G_GINT64_FORMAT " ] / [ %" G_GINT64_FORMAT " / %" G_GINT64_FORMAT " ]",
             gnc_numeric_check(price), val.num, val.denom, amt.num, amt.denom);
        return gnc_numeric_create(0, 1);
    }

    return price;
}

/********************************************************************\
\********************************************************************/

QofBook *
xaccSplitGetBook (const Split *split)
{
    return qof_instance_get_book(QOF_INSTANCE(split));
}

const char *
xaccSplitGetType(const Split *s)
{
    GValue v = G_VALUE_INIT;
    const char *split_type = NULL;

    if (!s) return NULL;
    qof_instance_get_kvp (QOF_INSTANCE (s), "split-type", &v);
    if (G_VALUE_HOLDS_STRING (&v))
        split_type = g_value_get_string (&v);
    return split_type ? split_type : "normal";
}

/* reconfigure a split to be a stock split - after this, you shouldn't
   mess with the value, just the amount. */
void
xaccSplitMakeStockSplit(Split *s)
{
    GValue v = G_VALUE_INIT;
    xaccTransBeginEdit (s->parent);

    s->value = gnc_numeric_zero();
    g_value_init (&v, G_TYPE_STRING);
    g_value_set_string (&v, "stock-split");
    qof_instance_set_kvp (QOF_INSTANCE (s), "split-type", &v);
    SET_GAINS_VDIRTY(s);
    mark_split(s);
    qof_instance_set_dirty(QOF_INSTANCE(s));
    xaccTransCommitEdit(s->parent);
}

void
xaccSplitAddPeerSplit (Split *split, const Split *other_split,
                       time64 timestamp)
{
    const GncGUID* guid;

    g_return_if_fail (split != NULL);
    g_return_if_fail (other_split != NULL);

    guid = qof_instance_get_guid (QOF_INSTANCE (other_split));
    xaccTransBeginEdit (split->parent);
    qof_instance_kvp_add_guid (QOF_INSTANCE (split), "lot-split",
                               timespec_now(), "peer_guid", guid_copy(guid));
    mark_split (split);
    qof_instance_set_dirty (QOF_INSTANCE (split));
    xaccTransCommitEdit (split->parent);
}

gboolean
xaccSplitHasPeers (const Split *split)
{
    return qof_instance_has_slot (QOF_INSTANCE (split), "lot-split");
}

gboolean
xaccSplitIsPeerSplit (const Split *split, const Split *other_split)
{
    const GncGUID* guid;

    g_return_val_if_fail (split != NULL, FALSE);
    g_return_val_if_fail (other_split != NULL, FALSE);

    guid = qof_instance_get_guid (QOF_INSTANCE (other_split));
    return qof_instance_kvp_has_guid (QOF_INSTANCE (split), "lot-split",
                                      "peer_guid", guid);
}

void
xaccSplitRemovePeerSplit (Split *split, const Split *other_split)
{
    const GncGUID* guid;

    g_return_if_fail (split != NULL);
    g_return_if_fail (other_split != NULL);

    guid = qof_instance_get_guid (QOF_INSTANCE (other_split));
    xaccTransBeginEdit (split->parent);
    qof_instance_kvp_remove_guid (QOF_INSTANCE (split), "lot-split",
                                  "peer_guid", guid);
    mark_split (split);
    qof_instance_set_dirty (QOF_INSTANCE (split));
    xaccTransCommitEdit (split->parent);
}

void
xaccSplitMergePeerSplits (Split *split, const Split *other_split)
{
    xaccTransBeginEdit (split->parent);
    qof_instance_kvp_merge_guids (QOF_INSTANCE (split),
                                  QOF_INSTANCE (other_split), "lot-split");
    mark_split (split);
    qof_instance_set_dirty (QOF_INSTANCE (split));
    xaccTransCommitEdit (split->parent);
}

/********************************************************************\
\********************************************************************/
/* In the old world, the 'other split' was the other split of a
 * transaction that contained only two splits.  In the new world,
 * a split may have been cut up between multiple lots, although
 * in a conceptual sense, if lots hadn't been used, there would be
 * only a pair.  So we handle this conceptual case: we can still
 * identify, unambiguously, the 'other' split when 'this' split
 * as been cut up across lots.  We do this by looking for the
 * 'lot-split' keyword, which occurs only in cut-up splits.
 */

Split *
xaccSplitGetOtherSplit (const Split *split)
{
    int i;
    Transaction *trans;
    int count, num_splits;
    Split *other = NULL;
    gboolean lot_split;
    gboolean trading_accts;

    if (!split) return NULL;
    trans = split->parent;
    if (!trans) return NULL;

    trading_accts = xaccTransUseTradingAccounts (trans);
    num_splits = xaccTransCountSplits(trans);
    count = num_splits;
    lot_split = qof_instance_has_slot(QOF_INSTANCE (split), "lot-split");
    if (!lot_split && !trading_accts && (2 != count)) return NULL;

    for (i = 0; i < num_splits; i++)
    {
        Split *s = xaccTransGetSplit(trans, i);
        if (s == split)
        {
            --count;
            continue;
        }
        if (qof_instance_has_slot (QOF_INSTANCE (s), "lot-split"))
        {
            --count;
            continue;
        }
        if (trading_accts &&
            xaccAccountGetType(xaccSplitGetAccount(s)) == ACCT_TYPE_TRADING)
        {
            --count;
            continue;
        }
        other = s;
    }
    return (1 == count) ? other : NULL;
}

/********************************************************************\
\********************************************************************/

gnc_numeric
xaccSplitVoidFormerAmount(const Split *split)
{
    GValue v = G_VALUE_INIT;
    gnc_numeric *num = NULL;
    g_return_val_if_fail(split, gnc_numeric_zero());
    qof_instance_get_kvp (QOF_INSTANCE (split), void_former_amt_str, &v);
    if (G_VALUE_HOLDS_BOXED (&v))
        num = (gnc_numeric*)g_value_get_boxed (&v);
    return num ? *num : gnc_numeric_zero();
}

gnc_numeric
xaccSplitVoidFormerValue(const Split *split)
{
    GValue v = G_VALUE_INIT;
    gnc_numeric *num = NULL;
    g_return_val_if_fail(split, gnc_numeric_zero());
    qof_instance_get_kvp (QOF_INSTANCE (split), void_former_val_str, &v);
    if (G_VALUE_HOLDS_BOXED (&v))
        num = (gnc_numeric*)g_value_get_boxed (&v);
    return num ? *num : gnc_numeric_zero();
}

void
xaccSplitVoid(Split *split)
{
    gnc_numeric zero = gnc_numeric_zero(), num;
    GValue v = G_VALUE_INIT;

    g_value_init (&v, GNC_TYPE_NUMERIC);
    num =  xaccSplitGetAmount(split);
    g_value_set_boxed (&v, &num);
    qof_instance_set_kvp (QOF_INSTANCE (split), void_former_amt_str, &v);
    num =  xaccSplitGetValue(split);
    g_value_set_boxed (&v, &num);
    qof_instance_set_kvp (QOF_INSTANCE (split), void_former_val_str, &v);

    /* Marking dirty handled by SetAmount etc. */
    xaccSplitSetAmount (split, zero);
    xaccSplitSetValue (split, zero);
    xaccSplitSetReconcile(split, VREC);
}

void
xaccSplitUnvoid(Split *split)
{
    xaccSplitSetAmount (split, xaccSplitVoidFormerAmount(split));
    xaccSplitSetValue (split, xaccSplitVoidFormerValue(split));
    xaccSplitSetReconcile(split, NREC);
    qof_instance_set_kvp (QOF_INSTANCE (split), void_former_amt_str, NULL);
    qof_instance_set_kvp (QOF_INSTANCE (split), void_former_val_str, NULL);
    qof_instance_set_dirty (QOF_INSTANCE (split));
}

/********************************************************************\
\********************************************************************/
/* QofObject function implementation */

/* Hook into the QofObject registry */

#ifdef _MSC_VER
/* MSVC compiler doesn't have C99 "designated initializers"
 * so we wrap them in a macro that is empty on MSVC. */
# define DI(x) /* */
#else
# define DI(x) x
#endif
static QofObject split_object_def =
{
    DI(.interface_version = ) QOF_OBJECT_VERSION,
    DI(.e_type            = ) GNC_ID_SPLIT,
    DI(.type_label        = ) "Split",
    DI(.create            = ) (gpointer)xaccMallocSplit,
    DI(.book_begin        = ) NULL,
    DI(.book_end          = ) NULL,
    DI(.is_dirty          = ) qof_collection_is_dirty,
    DI(.mark_clean        = ) qof_collection_mark_clean,
    DI(.foreach           = ) qof_collection_foreach,
    DI(.printable         = ) (const char * (*)(gpointer)) xaccSplitGetMemo,
    DI(.version_cmp       = ) (int (*)(gpointer, gpointer)) qof_instance_version_cmp,
};

static gpointer
split_account_guid_getter (gpointer obj, const QofParam *p)
{
    Split *s = obj;
    Account *acc;

    if (!s) return NULL;
    acc = xaccSplitGetAccount (s);
    if (!acc) return NULL;
    return ((gpointer)xaccAccountGetGUID (acc));
}

static double    /* internal use only */
DxaccSplitGetShareAmount (const Split * split)
{
    return split ? gnc_numeric_to_double(xaccSplitGetAmount(split)) : 0.0;
}

static gpointer
no_op (gpointer obj, const QofParam *p)
{
    return obj;
}

static void
qofSplitSetParentTrans(Split *s, QofInstance *ent)
{
    Transaction *trans = (Transaction*)ent;

    g_return_if_fail(trans);
    xaccSplitSetParent(s, trans);
}

static void
qofSplitSetAccount(Split *s, QofInstance *ent)
{
    Account *acc = (Account*)ent;

    g_return_if_fail(acc);
    xaccSplitSetAccount(s, acc);
}

gboolean xaccSplitRegister (void)
{
    static const QofParam params[] =
        {
            {
                SPLIT_DATE_RECONCILED, QOF_TYPE_DATE,
                (QofAccessFunc)xaccSplitRetDateReconciledTS,
                (QofSetterFunc)xaccSplitSetDateReconciledTS
            },

            /* d-* are deprecated query params, should not be used in new
             * queries, should be removed from old queries. */
            {
                "d-share-amount", QOF_TYPE_DOUBLE,
                (QofAccessFunc)DxaccSplitGetShareAmount, NULL
            },
            {
                "d-share-int64", QOF_TYPE_INT64,
                (QofAccessFunc)qof_entity_get_guid, NULL
            },
            {
                SPLIT_BALANCE, QOF_TYPE_NUMERIC,
                (QofAccessFunc)xaccSplitGetBalance, NULL
            },
            {
                SPLIT_CLEARED_BALANCE, QOF_TYPE_NUMERIC,
                (QofAccessFunc)xaccSplitGetClearedBalance, NULL
            },
            {
                SPLIT_RECONCILED_BALANCE, QOF_TYPE_NUMERIC,
                (QofAccessFunc)xaccSplitGetReconciledBalance, NULL
            },
            {
                SPLIT_MEMO, QOF_TYPE_STRING,
                (QofAccessFunc)xaccSplitGetMemo, (QofSetterFunc)qofSplitSetMemo
            },
            {
                SPLIT_ACTION, QOF_TYPE_STRING,
                (QofAccessFunc)xaccSplitGetAction, (QofSetterFunc)qofSplitSetAction
            },
            {
                SPLIT_RECONCILE, QOF_TYPE_CHAR,
                (QofAccessFunc)xaccSplitGetReconcile,
                (QofSetterFunc)qofSplitSetReconcile
            },
            {
                SPLIT_AMOUNT, QOF_TYPE_NUMERIC,
                (QofAccessFunc)xaccSplitGetAmount, (QofSetterFunc)qofSplitSetAmount
            },
            {
                SPLIT_SHARE_PRICE, QOF_TYPE_NUMERIC,
                (QofAccessFunc)xaccSplitGetSharePrice,
                (QofSetterFunc)qofSplitSetSharePrice
            },
            {
                SPLIT_VALUE, QOF_TYPE_DEBCRED,
                (QofAccessFunc)xaccSplitGetValue, (QofSetterFunc)qofSplitSetValue
            },
            { SPLIT_TYPE, QOF_TYPE_STRING, (QofAccessFunc)xaccSplitGetType, NULL },
            {
                SPLIT_VOIDED_AMOUNT, QOF_TYPE_NUMERIC,
                (QofAccessFunc)xaccSplitVoidFormerAmount, NULL
            },
            {
                SPLIT_VOIDED_VALUE, QOF_TYPE_NUMERIC,
                (QofAccessFunc)xaccSplitVoidFormerValue, NULL
            },
            { SPLIT_LOT, GNC_ID_LOT, (QofAccessFunc)xaccSplitGetLot, NULL },
            {
                SPLIT_TRANS, GNC_ID_TRANS,
                (QofAccessFunc)xaccSplitGetParent,
                (QofSetterFunc)qofSplitSetParentTrans
            },
            {
                SPLIT_ACCOUNT, GNC_ID_ACCOUNT,
                (QofAccessFunc)xaccSplitGetAccount, (QofSetterFunc)qofSplitSetAccount
            },
            { SPLIT_ACCOUNT_GUID, QOF_TYPE_GUID, split_account_guid_getter, NULL },
            /*  these are no-ops to register the parameter names (for sorting) but
                they return an allocated object which getters cannot do.  */
            { SPLIT_ACCT_FULLNAME, SPLIT_ACCT_FULLNAME, no_op, NULL },
            { SPLIT_CORR_ACCT_NAME, SPLIT_CORR_ACCT_NAME, no_op, NULL },
            { SPLIT_CORR_ACCT_CODE, SPLIT_CORR_ACCT_CODE, no_op, NULL },
            { QOF_PARAM_BOOK, QOF_ID_BOOK, (QofAccessFunc)xaccSplitGetBook, NULL },
            {
                QOF_PARAM_GUID, QOF_TYPE_GUID,
                (QofAccessFunc)qof_entity_get_guid, NULL
            },
            { NULL },
        };

    qof_class_register (GNC_ID_SPLIT, (QofSortFunc)xaccSplitOrder, params);
    qof_class_register (SPLIT_ACCT_FULLNAME,
                        (QofSortFunc)xaccSplitCompareAccountFullNames, NULL);
    qof_class_register (SPLIT_CORR_ACCT_NAME,
                        (QofSortFunc)xaccSplitCompareOtherAccountFullNames,
                        NULL);
    qof_class_register (SPLIT_CORR_ACCT_CODE,
                        (QofSortFunc)xaccSplitCompareOtherAccountCodes, NULL);

    return qof_object_register (&split_object_def);
}

SplitTestFunctions*
_utest_split_fill_functions (void)
{
    SplitTestFunctions *func = g_new (SplitTestFunctions, 1);

    func->xaccSplitEqualCheckBal = xaccSplitEqualCheckBal;
    func->get_currency_denom = get_currency_denom;
    func->get_commodity_denom = get_commodity_denom;
    func->get_corr_account_split = get_corr_account_split;
    return func;
}

/************************ END OF ************************************\
\************************* FILE *************************************/
