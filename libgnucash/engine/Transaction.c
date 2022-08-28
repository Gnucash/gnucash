/********************************************************************\
 * Transaction.c -- transaction implementation                      *
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

#include <config.h>

#include <platform.h>
#if PLATFORM(WINDOWS)
#include <windows.h>
#endif

#include <glib.h>
#include <glib/gi18n.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <time.h>
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#include "AccountP.h"
#include "Scrub.h"
#include "Scrub3.h"
#include "TransactionP.h"
#include "SplitP.h"
#include "TransLog.h"
#include "cap-gains.h"
#include "gnc-commodity.h"
#include "gnc-engine.h"
#include "gnc-lot.h"
#include "gnc-event.h"
#include <gnc-date.h>
#include "SchedXaction.h"
#include "gncBusiness.h"
#include <qofinstance-p.h>
#include "gncInvoice.h"
#include "gncOwner.h"

/* Notes about xaccTransBeginEdit(), xaccTransCommitEdit(), and
 *  xaccTransRollback():
 *
 * Why use it:
 *
 *   Data consistency: Wrapping your changes to financial data inside
 *   a BeginEdit/CommitEdit block allows the engine to verify that
 *   your changes still leave the financial objects in an internally
 *   consistent state.  This is true even though you may make a series
 *   of individual changes that are not consistent by themselves.  In
 *   this way, it's like telling the engine, "Okay, I've finished my
 *   edits.  Please check my work."
 *
 *   Data integrity: The other benefit of the BeginEdit/CommitEdit
 *   block is that it allows the engine (and the backend) to remember
 *   the last known correct state of your data.  This allows you to
 *   undo any changes that you don't want to keep.  In this way, it's
 *   like telling the engine telling the back end, "Yes, I really mean
 *   it.  Remember this data." or "Nevermind, scratch that."  The
 *   important feature here is that if things go bad, for whatever
 *   reason (e.g. the application crashed, you lost the backend), your
 *   data remains in the state it was in just after the previous
 *   xaccTransCommitEdit().  [assuming no nesting, which probably
 *   isn't useful outside the engine.]
 *
 *   Note that the backend doesn't care about data consistency -
 *   that's the engine's job.
 *
 * Example Use:
 *
 *   xaccTransBeginEdit(trans);
 *
 *
 *   split = xaccMallocSplit(book);
 *   xaccSplitSetAccount(split, acc);
 *   xaccSplitSetParent(split, trans);  // Adding a new split
 *
 *   xaccSplitSetValue(split, val);     // Changing a split
 *
 *   xaccSplitDestroy(split);           // Removing a split
 *
 *   xaccTransSetNum(trans, "501");     // Changing the trans
 *
 *   if (really_do_it)
 *      xaccTransCommitEdit(trans);
 *   else
 *      xaccTransRollbackEdit(trans);
 *
 * How it works:
 *
 *   Calling xaccTransBeginEdit() starts a BeginEdit/CommitEdit block.
 *   Inside the block any changes to the transaction or any splits in
 *   the transaction are considered "pending".  What does that mean?
 *
 *   In general that means that if you set and then get the
 *   transaction's or split's parameters inside the
 *   BeginEdit/CommitEdit block, you'll get the values you just set.
 *   However, if you change an object's many-to-one relationship with
 *   another object, you won't see the change from the "many" side
 *   until the CommitEdit.  For example, if you move a split from one
 *   account into another, you can see the change with
 *   xaccSplitGetAccount(), but both Accounts' split lists won't be
 *   updated until the CommitEdit.  Correspondingly, no signals
 *   (events) will be generated for those "foreign" objects, or the
 *   Transaction, until the CommitEdit.
 *
 *   This behavior is important because, when we're finally ready to
 *   commit to the backend, we can't be 100% sure that the backend
 *   will still be available.  We have to offer the backend all of the
 *   new state as if it were already "true", but we need to save all of
 *   the old state in case the backend won't accept our commit.  If
 *   the backend commit fails, we have to restore all the old state.
 *   If the backend commit succeeds, and *only* after it succeeds, we
 *   can advertise the new state to the rest of the engine (and gui).
 *
 *  Q: Who owns the ref of an added split if the Transaction is rolled
 *  back?
 *
 *  A: This is a design decision.  If the answer is 'the user',
 *  then the burden is on the api user to check the transaction after
 *  every commit to see if the added split is really in the
 *  transaction.  If they don't they risk leaking the split if the
 *  commit was rolled back.  Another design is to answer 'the engine'.
 *  In that case the burden is on the engine to free a newly added
 *  split if the commit is rolled back.  Unfortunately the engine
 *  objects aren't ref-counted, so this is tricky.
 *
 *  In the current implementation, the answer is 'the engine', but
 *  that means that you must not add the split to two different
 *  transactions during the begin/commit block, because if one rolls
 *  back, they will both think they own the split.  This is one
 *  specific example of the general problem that the outcome of two
 *  parallel begin/commit edit blocks for two transactions where edits
 *  for both transactions involve the same splits and one or more
 *  edit-blocks is rolled-back, is poorly-defined.
 *
 *
 *
 * Design notes on event-generation: transaction-modified-events
 * should not be generated until transaction commit or rollback
 * time.  They should not be generated as each field is tweaked.
 * This for two reasons:
 * 1) Most editing events make multiple changes to a transaction,
 *    which would generate a flurry of (needless) events, if they
 *    weren't saved up till the commit.
 * 2) Technically, its incorrect to use transaction data
 *    until the transaction is committed.  The GUI element that
 *    is changing the data can look at it, but all of the rest
 *    of the GUI should ignore the data until its committed.
 */

const char *trans_notes_str = "notes";
const char *void_reason_str = "void-reason";
const char *void_time_str = "void-time";
const char *void_former_notes_str = "void-former-notes";
const char *trans_is_closing_str = "book_closing";
const char *doclink_uri_str = "assoc_uri"; // this is the old name for the document link, kept for compatibility

/* KVP entry for date-due value */
#define TRANS_DATE_DUE_KVP       "trans-date-due"
#define TRANS_TXN_TYPE_KVP       "trans-txn-type"
#define TRANS_READ_ONLY_REASON   "trans-read-only"
#define TRANS_REVERSED_BY        "reversed-by"
#define GNC_SX_FROM              "from-sched-xaction"

#define ISO_DATELENGTH 32 /* length of an iso 8601 date string. */

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_ENGINE;

enum
{
    PROP_0,
    PROP_CURRENCY,	/* Table */
    PROP_NUM,		/* Table */
    PROP_POST_DATE,	/* Table */
    PROP_ENTER_DATE,	/* Table */
    PROP_DESCRIPTION,	/* Table */
    PROP_INVOICE,	/* KVP */
    PROP_SX_TXN,	/* KVP */
    PROP_ONLINE_ACCOUNT,/* KVP */
};

void
check_open (const Transaction *trans)
{
    if (trans && 0 >= qof_instance_get_editlevel(trans))
        PERR ("transaction %p not open for editing", trans);
}
/********************************************************************\
\********************************************************************/
gboolean
xaccTransStillHasSplit(const Transaction *trans, const Split *s)
{
    return (s && s->parent == trans && !qof_instance_get_destroying(s));
}

/* Executes 'cmd_block' for each split currently in the transaction,
 * using the in-edit state.  Use the variable 's' for each split. */
#define FOR_EACH_SPLIT(trans, cmd_block) if (trans->splits) {		\
        GList *splits;                                                  \
        for (splits = (trans)->splits; splits; splits = splits->next) { \
            Split *s = splits->data;                                    \
            if (xaccTransStillHasSplit(trans, s)) {                     \
                cmd_block;                                              \
            }                                                           \
        }                                                               \
    }

static inline void mark_trans (Transaction *trans);
void mark_trans (Transaction *trans)
{
    FOR_EACH_SPLIT(trans, mark_split(s));
}

static inline void gen_event_trans (Transaction *trans);
void gen_event_trans (Transaction *trans)
{
    GList *node;

    for (node = trans->splits; node; node = node->next)
    {
        Split *s = node->data;
        Account *account = s->acc;
        GNCLot *lot = s->lot;
        if (account)
            qof_event_gen (&account->inst, GNC_EVENT_ITEM_CHANGED, s);

        if (lot)
        {
            /* A change of transaction date might affect opening date of lot */
            qof_event_gen (QOF_INSTANCE(lot), QOF_EVENT_MODIFY, NULL);
        }
    }
}

static const char*
is_unset = "unset";

/* GObject Initialization */
G_DEFINE_TYPE(Transaction, gnc_transaction, QOF_TYPE_INSTANCE)

static void
gnc_transaction_init(Transaction* trans)
{
    ENTER ("trans=%p", trans);
    /* Fill in some sane defaults */
    trans->num         = CACHE_INSERT("");
    trans->description = CACHE_INSERT("");
    trans->common_currency = NULL;
    trans->splits = NULL;
    trans->date_entered  = 0;
    trans->date_posted  = 0;
    trans->marker = 0;
    trans->orig = NULL;
    trans->readonly_reason = (char*) is_unset;
    trans->isClosingTxn_cached = -1;
    trans->notes = (char*) is_unset;
    trans->doclink = (char*) is_unset;
    trans->void_reason = (char*) is_unset;
    trans->txn_type = TXN_TYPE_UNCACHED;
    LEAVE (" ");
}

static void
gnc_transaction_dispose(GObject *txnp)
{
    G_OBJECT_CLASS(gnc_transaction_parent_class)->dispose(txnp);
}

static void
gnc_transaction_finalize(GObject* txnp)
{
    G_OBJECT_CLASS(gnc_transaction_parent_class)->finalize(txnp);
}

/* Note that g_value_set_object() refs the object, as does
 * g_object_get(). But g_object_get() only unrefs once when it disgorges
 * the object, leaving an unbalanced ref, which leaks. So instead of
 * using g_value_set_object(), use g_value_take_object() which doesn't
 * ref the object when used in get_property().
 */
static void
gnc_transaction_get_property(GObject* object,
                             guint prop_id,
                             GValue* value,
                             GParamSpec* pspec)
{
    Transaction* tx;
    gchar *key;
    Time64 time;

    g_return_if_fail(GNC_IS_TRANSACTION(object));

    tx = GNC_TRANSACTION(object);
    switch (prop_id)
    {
    case PROP_NUM:
        g_value_set_string(value, tx->num);
        break;
    case PROP_DESCRIPTION:
        g_value_set_string(value, tx->description);
        break;
    case PROP_CURRENCY:
        g_value_take_object(value, tx->common_currency);
        break;
    case PROP_POST_DATE:
        time.t = tx->date_posted;
        g_value_set_boxed(value, &time);
        break;
    case PROP_ENTER_DATE:
        time.t = tx->date_entered;
        g_value_set_boxed(value, &time);
        break;
    case PROP_INVOICE:
        qof_instance_get_kvp (QOF_INSTANCE (tx), value, 2, GNC_INVOICE_ID, GNC_INVOICE_GUID);
        break;
    case PROP_SX_TXN:
        qof_instance_get_kvp (QOF_INSTANCE (tx), value, 1, GNC_SX_FROM);
        break;
    case PROP_ONLINE_ACCOUNT:
        qof_instance_get_kvp (QOF_INSTANCE (tx), value, 1, "online_id");
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
        break;
    }
}

static void
gnc_transaction_set_property(GObject* object,
                             guint prop_id,
                             const GValue* value,
                             GParamSpec* pspec)
{
    Transaction* tx;
    gchar *key;
    Time64 *t;

    g_return_if_fail(GNC_IS_TRANSACTION(object));

    tx = GNC_TRANSACTION(object);
    g_assert (qof_instance_get_editlevel(tx));

    switch (prop_id)
    {
    case PROP_NUM:
        xaccTransSetNum( tx, g_value_get_string(value));
        break;
    case PROP_DESCRIPTION:
        xaccTransSetDescription(tx, g_value_get_string(value));
        break;
    case PROP_CURRENCY:
        xaccTransSetCurrency(tx, g_value_get_object(value));
        break;
    case PROP_POST_DATE:
        t = (Time64*)g_value_get_boxed(value);
        xaccTransSetDatePostedSecs(tx, t->t);
        break;
    case PROP_ENTER_DATE:
        t = (Time64*)g_value_get_boxed(value);
        xaccTransSetDateEnteredSecs(tx, t->t);
        break;
    case PROP_INVOICE:
        qof_instance_set_kvp (QOF_INSTANCE (tx), value, 2, GNC_INVOICE_ID, GNC_INVOICE_GUID);
        break;
    case PROP_SX_TXN:
        qof_instance_set_kvp (QOF_INSTANCE (tx), value, 1, GNC_SX_FROM);
        break;
    case PROP_ONLINE_ACCOUNT:
        qof_instance_set_kvp (QOF_INSTANCE (tx), value, 1, "online_id");
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
        break;
    }
}

static void
gnc_transaction_class_init(TransactionClass* klass)
{
    GObjectClass* gobject_class = G_OBJECT_CLASS(klass);

    gobject_class->dispose = gnc_transaction_dispose;
    gobject_class->finalize = gnc_transaction_finalize;
    gobject_class->set_property = gnc_transaction_set_property;
    gobject_class->get_property = gnc_transaction_get_property;

    g_object_class_install_property
    (gobject_class,
     PROP_NUM,
     g_param_spec_string("num",
                         "Transaction Number",
                         "The transactionNumber is an arbitrary string "
                         "assigned by the user.  It is intended to be "
                         "a short 1-6 character string that is displayed "
                         "by the register.  For checks, it is usually the "
                         "check number.  For other types of transactions, "
                         "it can be any string.",
                         NULL,
                         G_PARAM_READWRITE));

    g_object_class_install_property
    (gobject_class,
     PROP_DESCRIPTION,
     g_param_spec_string("description",
                         "Transaction Description",
                         "The transaction description is an arbitrary string "
                         "assigned by the user.  It is usually the customer, "
                         "vendor or other organization associated with the "
                         "transaction.",
                         NULL,
                         G_PARAM_READWRITE));

    g_object_class_install_property
    (gobject_class,
     PROP_CURRENCY,
     g_param_spec_object ("currency",
                          "Currency",
                          "The base currency for this transaction.",
                          GNC_TYPE_COMMODITY,
                          G_PARAM_READWRITE));

    g_object_class_install_property
    (gobject_class,
     PROP_POST_DATE,
     g_param_spec_boxed("post-date",
                        "Post Date",
                        "The date the transaction occurred.",
                        GNC_TYPE_TIME64,
                        G_PARAM_READWRITE));

    g_object_class_install_property
    (gobject_class,
     PROP_ENTER_DATE,
     g_param_spec_boxed("enter-date",
                        "Enter Date",
                        "The date the transaction was entered.",
                        GNC_TYPE_TIME64,
                        G_PARAM_READWRITE));

     g_object_class_install_property(
       gobject_class,
        PROP_INVOICE,
        g_param_spec_boxed("invoice",
			   "Invoice attached to lot",
			   "Used by GncInvoice",
			   GNC_TYPE_GUID,
			   G_PARAM_READWRITE));

     g_object_class_install_property(
       gobject_class,
        PROP_SX_TXN,
        g_param_spec_boxed("from-sched-xaction",
			   "From Scheduled Transaction",
			   "Used by Scheduled Transastions to record the "
			   "originating template transaction for created "
			   "transactions",
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
}

/********************************************************************\
 * xaccInitTransaction
 * Initialize a transaction structure
\********************************************************************/

static void
xaccInitTransaction (Transaction * trans, QofBook *book)
{
    ENTER ("trans=%p", trans);
    qof_instance_init_data (&trans->inst, GNC_ID_TRANS, book);
    LEAVE (" ");
}

/********************************************************************\
\********************************************************************/

Transaction *
xaccMallocTransaction (QofBook *book)
{
    Transaction *trans;

    g_return_val_if_fail (book, NULL);

    trans = g_object_new(GNC_TYPE_TRANSACTION, NULL);
    xaccInitTransaction (trans, book);
    qof_event_gen (&trans->inst, QOF_EVENT_CREATE, NULL);

    return trans;
}

#ifdef DUMP_FUNCTIONS
/* Please don't delete this function.  Although it is not called by
   any other code in GnuCash, it is useful when debugging.  For example
   it can be called using the gdb "call" command when stopped at a
   breakpoint.  */
void
xaccTransDump (const Transaction *trans, const char *tag)
{
    GList *node;
    char datebuff[MAX_DATE_LENGTH + 1];

    printf("%s Trans %p", tag, trans);
    memset(datebuff, 0, sizeof(datebuff));
    qof_print_date_buff(datebuff, MAX_DATE_LENGTH, trans->date_entered);
    printf("    Entered:     %s\n", datebuff);
    memset(datebuff, 0, sizeof(datebuff));
    qof_print_date_buff(datebuff, MAX_DATE_LENGTH, trans->date_posted);
    printf("    Posted:      %s\n", datebuff);
    printf("    Num:         %s\n", trans->num ? trans->num : "(null)");
    printf("    Description: %s\n",
           trans->description ? trans->description : "(null)");
    printf("    Currency:    %s\n",
           gnc_commodity_get_printname(trans->common_currency));
    printf("    version:     %x\n", qof_instance_get_version(trans));
    printf("    version_chk: %x\n", qof_instance_get_version_check(trans));
    printf("    editlevel:   %x\n", qof_instance_get_editlevel(trans));
    printf("    orig:        %p\n", trans->orig);
    printf("    idata:       %x\n", qof_instance_get_idata(trans));
    printf("    splits:      ");
    for (node = trans->splits; node; node = node->next)
    {
        printf("%p ", node->data);
    }
    printf("\n");
    for (node = trans->splits; node; node = node->next)
    {
        xaccSplitDump(node->data, tag);
    }
    printf("\n");
}
#endif

void
xaccTransSortSplits (Transaction *trans)
{
    GList *node, *new_list = NULL;
    Split *split;

    /* first debits */
    for (node = trans->splits; node; node = node->next)
    {
        split = node->data;
        if (gnc_numeric_negative_p (xaccSplitGetValue(split)))
            continue;
        new_list = g_list_prepend (new_list, split);
    }

    /* then credits */
    for (node = trans->splits; node; node = node->next)
    {
        split = node->data;
        if (!gnc_numeric_negative_p (xaccSplitGetValue(split)))
            continue;
        new_list = g_list_prepend (new_list, split);
    }

    /* install newly sorted list */
    g_list_free(trans->splits);
    trans->splits = g_list_reverse (new_list);
}


/********************************************************************\
\********************************************************************/
/* This routine is not exposed externally, since it does weird things,
 * like not really owning the splits correctly, and other weirdnesses.
 * This routine is prone to programmer snafu if not used correctly.
 * It is used only by the edit-rollback code.
 */
static Transaction *
dupe_trans (const Transaction *from)
{
    Transaction *to;
    GList *node;

    to = g_object_new (GNC_TYPE_TRANSACTION, NULL);

    CACHE_REPLACE (to->num, from->num);
    CACHE_REPLACE (to->description, from->description);

    to->splits = g_list_copy (from->splits);
    for (node = to->splits; node; node = node->next)
    {
        node->data = xaccDupeSplit (node->data);
    }

    to->date_entered = from->date_entered;
    to->date_posted = from->date_posted;
    qof_instance_copy_version(to, from);
    to->orig = NULL;

    to->common_currency = from->common_currency;

    /* Trash the guid and entity table. We don't want to mistake
     * the cloned transaction as something official.  If we ever
     * use this transaction, we'll have to fix this up.
     */
    to->inst.e_type = NULL;
    qof_instance_set_guid(to, guid_null());
    qof_instance_copy_book(to, from);
    qof_instance_copy_kvp (QOF_INSTANCE(to), QOF_INSTANCE(from));

    return to;
}

/********************************************************************\
 * Use this routine to externally duplicate a transaction.  It creates
 * a full fledged transaction with unique guid, splits, etc. and
 * writes it to the database.
\********************************************************************/
Transaction *
xaccTransCloneNoKvp (const Transaction *from)
{
    Transaction *to;
    Split *split;
    GList *node;

    qof_event_suspend();
    to = g_object_new (GNC_TYPE_TRANSACTION, NULL);

    to->date_entered    = from->date_entered;
    to->date_posted     = from->date_posted;
    CACHE_REPLACE (to->num, from->num);
    CACHE_REPLACE (to->description, from->description);
    to->common_currency = from->common_currency;
    qof_instance_copy_version(to, from);
    qof_instance_copy_version_check(to, from);

    to->orig            = NULL;

    qof_instance_init_data (&to->inst, GNC_ID_TRANS,
			    qof_instance_get_book(from));

    xaccTransBeginEdit(to);
    for (node = from->splits; node; node = node->next)
    {
        split = xaccSplitCloneNoKvp(node->data);
        split->parent = to;
        to->splits = g_list_append (to->splits, split);
    }
    qof_instance_set_dirty(QOF_INSTANCE(to));
    xaccTransCommitEdit(to);
    qof_event_resume();

    return to;
}

Transaction *
xaccTransClone (const Transaction *from)
{
    Transaction *to = xaccTransCloneNoKvp (from);

    if (g_list_length (to->splits) != g_list_length (from->splits))
    {
        PERR ("Cloned transaction has different number of splits from original");
        xaccTransDestroy (to);
        return NULL;
    }

    xaccTransBeginEdit (to);
    qof_instance_copy_kvp (QOF_INSTANCE (to), QOF_INSTANCE (from));

    for (GList* lfrom = from->splits, *lto = to->splits; lfrom && lto;
         lfrom = g_list_next (lfrom), lto = g_list_next (lto))
        xaccSplitCopyKvp (lfrom->data, lto->data);

    xaccTransCommitEdit (to);
    return to;
}

/*################## Added for Reg2 #################*/

/********************************************************************\
 * Copy a transaction to the 'clipboard' transaction using
 *  dupe_trans. The 'clipboard' transaction must never
 *  be dereferenced.
\********************************************************************/
Transaction * xaccTransCopyToClipBoard(const Transaction *from_trans)
{
    Transaction *to_trans;

    if (!from_trans)
        return NULL;

    to_trans = dupe_trans(from_trans);
    return to_trans;
}

/********************************************************************\
 * Copy a transaction to another using the function below without
 *  changing any account information.
\********************************************************************/
void
xaccTransCopyOnto(const Transaction *from_trans, Transaction *to_trans)
{
    xaccTransCopyFromClipBoard(from_trans, to_trans, NULL, NULL, TRUE);
}

/********************************************************************\
 * This function explicitly must robustly handle some unusual input.
 *
 *  'from_trans' may be a duped trans (see dupe_trans), so its
 *   splits may not really belong to the accounts that they say they do.
 *
 *  'from_acc' need not be a valid account. It may be an already freed
 *   Account. Therefore, it must not be dereferenced at all.
 *
 *   Neither 'from_trans', nor 'from_acc', nor any of 'from's splits may
 *   be modified in any way.
 *
 *   'no_date' if TRUE will not copy the date posted.
 *
 *   The 'to_trans' transaction will end up with valid copies of from's
 *   splits.  In addition, the copies of any of from's splits that were
 *   in from_acc (or at least claimed to be) will end up in to_acc.
\********************************************************************/
void
xaccTransCopyFromClipBoard(const Transaction *from_trans, Transaction *to_trans,
                           const Account *from_acc, Account *to_acc, gboolean no_date)
{
    gboolean change_accounts = FALSE;
    GList *node;

    if (!from_trans || !to_trans)
        return;

    change_accounts = from_acc && GNC_IS_ACCOUNT(to_acc) && from_acc != to_acc;
    xaccTransBeginEdit(to_trans);

    FOR_EACH_SPLIT(to_trans, xaccSplitDestroy(s));
    g_list_free(to_trans->splits);
    to_trans->splits = NULL;

    xaccTransSetCurrency(to_trans, xaccTransGetCurrency(from_trans));
    xaccTransSetDescription(to_trans, xaccTransGetDescription(from_trans));

    if ((xaccTransGetNum(to_trans) == NULL) || (g_strcmp0 (xaccTransGetNum(to_trans), "") == 0))
        xaccTransSetNum(to_trans, xaccTransGetNum(from_trans));

    xaccTransSetNotes(to_trans, xaccTransGetNotes(from_trans));
    xaccTransSetDocLink(to_trans, xaccTransGetDocLink (from_trans));
    if(!no_date)
    {
        xaccTransSetDatePostedSecs(to_trans, xaccTransRetDatePosted (from_trans));
    }

    /* Each new split will be parented to 'to' */
    for (node = from_trans->splits; node; node = node->next)
    {
        Split *new_split = xaccMallocSplit( qof_instance_get_book(QOF_INSTANCE(from_trans)));
        xaccSplitCopyOnto(node->data, new_split);
        if (change_accounts && xaccSplitGetAccount(node->data) == from_acc)
            xaccSplitSetAccount(new_split, to_acc);
        xaccSplitSetParent(new_split, to_trans);
    }
    xaccTransCommitEdit(to_trans);
}

/*################## Added for Reg2 #################*/

/********************************************************************\
 Free the transaction.
\********************************************************************/
static void
xaccFreeTransaction (Transaction *trans)
{
    GList *node;

    if (!trans) return;

    ENTER ("(addr=%p)", trans);
    if (((char *) 1) == trans->num)
    {
        PERR ("double-free %p", trans);
        LEAVE (" ");
        return;
    }

    /* free up the destination splits */
    for (node = trans->splits; node; node = node->next)
        xaccFreeSplit (node->data);
    g_list_free (trans->splits);
    trans->splits = NULL;

    /* free up transaction strings */
    CACHE_REMOVE(trans->num);
    CACHE_REMOVE(trans->description);
    if (trans->readonly_reason != is_unset)
        g_free (trans->readonly_reason);
    if (trans->doclink != is_unset)
        g_free (trans->doclink);
    if (trans->void_reason != is_unset)
        g_free (trans->void_reason);
    if (trans->notes != is_unset)
        g_free (trans->notes);

    /* Just in case someone looks up freed memory ... */
    trans->num         = (char *) 1;
    trans->description = NULL;
    trans->date_entered = 0;
    trans->date_posted = 0;
    trans->readonly_reason = NULL;
    trans->doclink = NULL;
    trans->notes = NULL;
    trans->void_reason = NULL;
    if (trans->orig)
    {
        xaccFreeTransaction (trans->orig);
        trans->orig = NULL;
    }

    /* qof_instance_release (&trans->inst); */
    g_object_unref(trans);

    LEAVE ("(addr=%p)", trans);
}

/********************************************************************
 xaccTransEqual

 Compare two transactions for equality.  We don't pay any attention to
 rollback issues here, and we only care about equality of "permanent
 fields", basically the things that would survive a file save/load
 cycle.

 ********************************************************************/

/* return 0 when splits have equal guids */
static gint
compare_split_guids (gconstpointer a, gconstpointer b)
{
    const Split *sa = a;
    const Split *sb = b;

    if (sa == sb) return 0;
    if (!sa || !sb) return 1;

    return guid_compare (xaccSplitGetGUID (sa), xaccSplitGetGUID (sb));
}

gboolean
xaccTransEqual(const Transaction *ta, const Transaction *tb,
               gboolean check_guids,
               gboolean check_splits,
               gboolean check_balances,
               gboolean assume_ordered)
{
    gboolean same_book;

    if (!ta && !tb) return TRUE; /* Arguable.  FALSE may be better. */

    if (!ta || !tb)
    {
        PINFO ("one is NULL");
        return FALSE;
    }

    if (ta == tb) return TRUE;

    same_book = qof_instance_get_book(QOF_INSTANCE(ta)) == qof_instance_get_book(QOF_INSTANCE(tb));

    if (check_guids)
    {
        if (qof_instance_guid_compare(ta, tb) != 0)
        {
            PINFO ("GUIDs differ");
            return FALSE;
        }
    }

    if (!gnc_commodity_equal(ta->common_currency, tb->common_currency))
    {
        PINFO ("commodities differ %s vs %s",
               gnc_commodity_get_unique_name (ta->common_currency),
               gnc_commodity_get_unique_name (tb->common_currency));
        return FALSE;
    }

    if (ta->date_entered != tb->date_entered)
    {
        char buf1[100];
        char buf2[100];

        (void)gnc_time64_to_iso8601_buff(ta->date_entered, buf1);
        (void)gnc_time64_to_iso8601_buff(tb->date_entered, buf2);
        PINFO ("date entered differs: '%s' vs '%s'", buf1, buf2);
        return FALSE;
    }

    if (ta->date_posted != tb->date_posted)
    {
        char buf1[100];
        char buf2[100];

        (void)gnc_time64_to_iso8601_buff(ta->date_posted, buf1);
        (void)gnc_time64_to_iso8601_buff(tb->date_posted, buf2);
        PINFO ("date posted differs: '%s' vs '%s'", buf1, buf2);
        return FALSE;
    }

    /* If the same book, since we use cached strings, we can just compare pointer
     * equality for num and description
     */
    if ((same_book && ta->num != tb->num) || (!same_book && g_strcmp0(ta->num, tb->num) != 0))
    {
        PINFO ("num differs: %s vs %s", ta->num, tb->num);
        return FALSE;
    }

    if ((same_book && ta->description != tb->description)
            || (!same_book && g_strcmp0(ta->description, tb->description)))
    {
        PINFO ("descriptions differ: %s vs %s", ta->description, tb->description);
        return FALSE;
    }

    if (qof_instance_compare_kvp (QOF_INSTANCE (ta), QOF_INSTANCE (tb)) != 0)
    {
        char *frame_a;
        char *frame_b;

        frame_a = qof_instance_kvp_as_string (QOF_INSTANCE (ta));
        frame_b = qof_instance_kvp_as_string (QOF_INSTANCE (tb));


        PINFO ("kvp frames differ:\n%s\n\nvs\n\n%s", frame_a, frame_b);

        g_free (frame_a);
        g_free (frame_b);

        return FALSE;
    }

    if (check_splits)
    {
        if ((!ta->splits && tb->splits) || (!tb->splits && ta->splits))
        {
            PINFO ("only one has splits");
            return FALSE;
        }

        if (ta->splits && tb->splits)
        {
            GList *node_a, *node_b;

            for (node_a = ta->splits, node_b = tb->splits;
                    node_a;
                    node_a = node_a->next, node_b = node_b->next)
            {
                Split *split_a = node_a->data;
                Split *split_b;

                /* don't presume that the splits are in the same order */
                if (!assume_ordered)
                    node_b = g_list_find_custom (tb->splits, split_a,
                                                 compare_split_guids);

                if (!node_b)
                {
                    gchar guidstr[GUID_ENCODING_LENGTH+1];
                    guid_to_string_buff (xaccSplitGetGUID (split_a),guidstr);

                    PINFO ("first has split %s and second does not",guidstr);
                    return FALSE;
                }

                split_b = node_b->data;

                if (!xaccSplitEqual (split_a, split_b, check_guids, check_balances,
                                     FALSE))
                {
                    char str_a[GUID_ENCODING_LENGTH + 1];
                    char str_b[GUID_ENCODING_LENGTH + 1];

                    guid_to_string_buff (xaccSplitGetGUID (split_a), str_a);
                    guid_to_string_buff (xaccSplitGetGUID (split_b), str_b);

                    PINFO ("splits %s and %s differ", str_a, str_b);
                    return FALSE;
                }
            }

            if (g_list_length (ta->splits) != g_list_length (tb->splits))
            {
                PINFO ("different number of splits");
                return FALSE;
            }
        }
    }

    return TRUE;
}

/********************************************************************\
xaccTransUseTradingAccounts

Returns true if the transaction should include trading account splits if
it involves more than one commodity.
\********************************************************************/

gboolean xaccTransUseTradingAccounts(const Transaction *trans)
{
    return qof_book_use_trading_accounts(qof_instance_get_book (trans));
}

/********************************************************************\
\********************************************************************/

Transaction *
xaccTransLookup (const GncGUID *guid, QofBook *book)
{
    QofCollection *col;
    if (!guid || !book) return NULL;
    col = qof_book_get_collection (book, GNC_ID_TRANS);
    return (Transaction *) qof_collection_lookup_entity (col, guid);
}

/********************************************************************\
\********************************************************************/

gnc_numeric
xaccTransGetImbalanceValue (const Transaction * trans)
{
    gnc_numeric imbal = gnc_numeric_zero();
    if (!trans) return imbal;

    ENTER("(trans=%p)", trans);
    /* Could use xaccSplitsComputeValue, except that we want to use
       GNC_HOW_DENOM_EXACT */
    FOR_EACH_SPLIT(trans, imbal =
                       gnc_numeric_add(imbal, xaccSplitGetValue(s),
                                       GNC_DENOM_AUTO, GNC_HOW_DENOM_EXACT));
    LEAVE("(trans=%p) imbal=%s", trans, gnc_num_dbg_to_string(imbal));
    return imbal;
}

MonetaryList *
xaccTransGetImbalance (const Transaction * trans)
{
    /* imbal_value is used if either (1) the transaction has a non currency
       split or (2) all the splits are in the same currency.  If there are
       no non-currency splits and not all splits are in the same currency then
       imbal_list is used to compute the imbalance. */
    MonetaryList *imbal_list = NULL;
    gnc_numeric imbal_value = gnc_numeric_zero();
    gboolean trading_accts;

    if (!trans) return imbal_list;

    ENTER("(trans=%p)", trans);

    trading_accts = xaccTransUseTradingAccounts (trans);

    /* If using trading accounts and there is at least one split that is not
       in the transaction currency or a split that has a price or exchange
       rate other than 1, then compute the balance in each commodity in the
       transaction.  Otherwise (all splits are in the transaction's currency)
       then compute the balance using the value fields.

       Optimize for the common case of only one currency and a balanced
       transaction. */
    FOR_EACH_SPLIT(trans,
    {
        gnc_commodity *commodity;
        commodity = xaccAccountGetCommodity(xaccSplitGetAccount(s));
        if (trading_accts &&
        (imbal_list ||
        ! gnc_commodity_equiv(commodity, trans->common_currency) ||
        ! gnc_numeric_equal(xaccSplitGetAmount(s), xaccSplitGetValue(s))))
        {
            /* Need to use (or already are using) a list of imbalances in each of
               the currencies used in the transaction. */
            if (! imbal_list)
            {
                /* All previous splits have been in the transaction's common
                   currency, so imbal_value is in this currency. */
                imbal_list = gnc_monetary_list_add_value(imbal_list,
                trans->common_currency,
                imbal_value);
            }
            imbal_list = gnc_monetary_list_add_value(imbal_list, commodity,
                         xaccSplitGetAmount(s));
        }

        /* Add it to the value accumulator in case we need it. */
        imbal_value = gnc_numeric_add(imbal_value, xaccSplitGetValue(s),
                                      GNC_DENOM_AUTO, GNC_HOW_DENOM_EXACT);
    } );


    if (!imbal_list && !gnc_numeric_zero_p(imbal_value))
    {
        /* Not balanced and no list, create one.  If we found multiple currencies
           and no non-currency commodity then imbal_list will already exist and
           we won't get here. */
        imbal_list = gnc_monetary_list_add_value(imbal_list,
                     trans->common_currency,
                     imbal_value);
    }

    /* Delete all the zero entries from the list, perhaps leaving an
       empty list */
    imbal_list = gnc_monetary_list_delete_zeros(imbal_list);

    LEAVE("(trans=%p), imbal=%p", trans, imbal_list);
    return imbal_list;
}

gboolean
xaccTransIsBalanced (const Transaction *trans)
{
    MonetaryList *imbal_list;
    gboolean result;
    gnc_numeric imbal = gnc_numeric_zero();
    gnc_numeric imbal_trading = gnc_numeric_zero();

    if (trans == NULL) return FALSE;

    if (xaccTransUseTradingAccounts(trans))
    {
        /* Transaction is imbalanced if the value is imbalanced in either
           trading or non-trading splits.  One can't be used to balance
           the other. */
        FOR_EACH_SPLIT(trans,
        {
            Account *acc = xaccSplitGetAccount(s);
            if (!acc || xaccAccountGetType(acc) != ACCT_TYPE_TRADING)
            {
                imbal = gnc_numeric_add(imbal, xaccSplitGetValue(s),
                                        GNC_DENOM_AUTO, GNC_HOW_DENOM_EXACT);
            }
            else
            {
                imbal_trading = gnc_numeric_add(imbal_trading, xaccSplitGetValue(s),
                                                GNC_DENOM_AUTO, GNC_HOW_DENOM_EXACT);
            }
        }
        );
    }
    else
        imbal = xaccTransGetImbalanceValue(trans);

    if (! gnc_numeric_zero_p(imbal) || ! gnc_numeric_zero_p(imbal_trading))
        return FALSE;

    if (!xaccTransUseTradingAccounts (trans))
        return TRUE;

    imbal_list = xaccTransGetImbalance(trans);
    result = imbal_list == NULL;
    gnc_monetary_list_free(imbal_list);
    return result;
}

gnc_numeric
xaccTransGetAccountValue (const Transaction *trans,
                          const Account *acc)
{
    gnc_numeric total = gnc_numeric_zero ();
    if (!trans || !acc) return total;

    FOR_EACH_SPLIT(trans, if (acc == xaccSplitGetAccount(s))
{
    total = gnc_numeric_add (total, xaccSplitGetValue (s),
                             GNC_DENOM_AUTO,
                             GNC_HOW_DENOM_EXACT);
    });
    return total;
}

gnc_numeric
xaccTransGetAccountAmount (const Transaction *trans, const Account *acc)
{
    gnc_numeric total = gnc_numeric_zero ();
    if (!trans || !acc) return total;

    total = gnc_numeric_convert (total, xaccAccountGetCommoditySCU (acc),
                                 GNC_HOW_RND_ROUND_HALF_UP);
    FOR_EACH_SPLIT(trans, if (acc == xaccSplitGetAccount(s))
                   total = gnc_numeric_add_fixed(
                               total, xaccSplitGetAmount(s)));
    return total;
}

/*################## Added for Reg2 #################*/
gboolean
xaccTransGetRateForCommodity(const Transaction *trans,
                             const gnc_commodity *split_com,
                             const Split *split, gnc_numeric *rate)
{
    GList *splits;
    gnc_commodity *trans_curr;

    if (trans == NULL || split_com == NULL || split == NULL)
	return FALSE;

    trans_curr = xaccTransGetCurrency (trans);
    if (gnc_commodity_equal (trans_curr, split_com))
    {
        if (rate)
            *rate = gnc_numeric_create (1, 1);
        return TRUE;
    }

    for (splits = trans->splits; splits; splits = splits->next)
    {
        Split *s = splits->data;
        gnc_commodity *comm;

        if (!xaccTransStillHasSplit (trans, s)) continue;

        if (s == split)
        {
            comm = xaccAccountGetCommodity (xaccSplitGetAccount(s));
            if (gnc_commodity_equal (split_com, comm))
            {
                gnc_numeric amt = xaccSplitGetAmount (s);
                gnc_numeric val = xaccSplitGetValue (s);

                if (!gnc_numeric_zero_p (xaccSplitGetAmount (s)) &&
                    !gnc_numeric_zero_p (xaccSplitGetValue (s)))
                {
                    if (rate)
                        *rate = gnc_numeric_div (amt, val, GNC_DENOM_AUTO,
                                                GNC_HOW_DENOM_REDUCE);
                    return TRUE;
                }
            }
        }
    }
    return FALSE;
}
/*################## Added for Reg2 #################*/

gnc_numeric
xaccTransGetAccountConvRate(const Transaction *txn, const Account *acc)
{
    gnc_numeric amount, value, convrate;
    GList *splits;
    Split *s;
    gboolean found_acc_match = FALSE;
    gnc_commodity *acc_commod = xaccAccountGetCommodity(acc);

    /* We need to compute the conversion rate into _this account_.  So,
     * find the first split into this account, compute the conversion
     * rate (based on amount/value), and then return this conversion
     * rate.
     */
    if (gnc_commodity_equal(acc_commod, xaccTransGetCurrency(txn)))
        return gnc_numeric_create(1, 1);

    for (splits = txn->splits; splits; splits = splits->next)
    {
        Account *split_acc;
        gnc_commodity *split_commod;

        s = splits->data;

        if (!xaccTransStillHasSplit(txn, s))
            continue;
        split_acc = xaccSplitGetAccount (s);
        split_commod = xaccAccountGetCommodity (split_acc);
        if (! (split_acc == acc ||
                gnc_commodity_equal (split_commod, acc_commod)))
            continue;

        found_acc_match = TRUE;
        amount = xaccSplitGetAmount (s);

        /* Ignore splits with "zero" amount */
        if (gnc_numeric_zero_p (amount))
            continue;

        value = xaccSplitGetValue (s);
        if (gnc_numeric_zero_p (value))
            PWARN("How can amount be nonzero and value be zero?");

        convrate = gnc_numeric_div(amount, value, GNC_DENOM_AUTO, GNC_HOW_DENOM_REDUCE);
        return convrate;
    }

    if (acc)
    {
        /* If we did find a matching account but its amount was zero,
         * then perhaps this is a "special" income/loss transaction
         */
        if (found_acc_match)
            return gnc_numeric_zero();
        else
            PERR("Cannot convert transaction -- no splits with proper conversion ratio");
    }
    return gnc_numeric_create (100, 100);
}

gnc_numeric
xaccTransGetAccountBalance (const Transaction *trans,
                            const Account *account)
{
    GList *node;
    Split *last_split = NULL;

    // Not really the appropriate error value.
    g_return_val_if_fail(account && trans, gnc_numeric_error(GNC_ERROR_ARG));

    for (node = trans->splits; node; node = node->next)
    {
        Split *split = node->data;

        if (!xaccTransStillHasSplit(trans, split))
            continue;
        if (xaccSplitGetAccount(split) != account)
            continue;

        if (!last_split)
        {
            last_split = split;
            continue;
        }

        /* This test needs to correspond to the comparison function used when
           sorting the splits for computing the running balance. */
        if (xaccSplitOrder (last_split, split) < 0)
            last_split = split;
    }

    return xaccSplitGetBalance (last_split);
}

/********************************************************************\
\********************************************************************/
/* The new routine for setting the common currency */

gnc_commodity *
xaccTransGetCurrency (const Transaction *trans)
{
    return trans ? trans->common_currency : NULL;
}

/* Helper functions for xaccTransSetCurrency */
static gnc_numeric
find_new_rate(Transaction *trans, gnc_commodity *curr)
{
    GList *node;
    gnc_numeric rate = gnc_numeric_zero();
    for (node = trans->splits; node != NULL; node = g_list_next (node))
    {
        Split *split = GNC_SPLIT(node->data);
        gnc_commodity *split_com =
            xaccAccountGetCommodity(xaccSplitGetAccount(split));
        if (gnc_commodity_equal(curr, split_com))
        {
/* This looks backwards, but the amount of the balancing transaction
 * that we're going to use it on is in the value's currency. */
            rate = gnc_numeric_div(xaccSplitGetAmount(split),
                                   xaccSplitGetValue(split),
                                   GNC_DENOM_AUTO, GNC_HOW_RND_NEVER);
            break;
        }
    }
    return rate;
}

static void
split_set_new_value(Split* split, gnc_commodity *curr, gnc_commodity *old_curr,
                    gnc_numeric rate)
{
    gnc_commodity *split_com =
        xaccAccountGetCommodity(xaccSplitGetAccount(split));
    if (gnc_commodity_equal(curr, split_com))
        xaccSplitSetValue(split, xaccSplitGetAmount(split));
    else if (gnc_commodity_equal(old_curr, split_com))
        xaccSplitSetSharePrice(split, rate);
    else
    {
        gnc_numeric old_rate = gnc_numeric_div(xaccSplitGetValue(split),
                                               xaccSplitGetAmount(split),
                                               GNC_DENOM_AUTO,
                                               GNC_HOW_RND_NEVER);
        gnc_numeric new_rate = gnc_numeric_div(old_rate, rate, GNC_DENOM_AUTO,
                                               GNC_HOW_RND_NEVER);
        xaccSplitSetSharePrice(split, new_rate);
    }
}

/**
 * Set a new currency on a transaction.
 * When we do that to a transaction with splits we need to re-value
 * all of the splits in the new currency.
 * @param trans: The transaction to change
 * @param curr: The new currency to set.
 */
void
xaccTransSetCurrency (Transaction *trans, gnc_commodity *curr)
{
    gnc_commodity *old_curr = trans->common_currency;
    if (!trans || !curr || trans->common_currency == curr) return;
    xaccTransBeginEdit(trans);

    trans->common_currency = curr;
    if (old_curr != NULL && trans->splits != NULL)
    {
        gnc_numeric rate = find_new_rate(trans, curr);
        if (!gnc_numeric_zero_p (rate))
        {
            FOR_EACH_SPLIT(trans, split_set_new_value(s, curr, old_curr, rate));
        }
        else
        {
            FOR_EACH_SPLIT(trans, xaccSplitSetValue(s, xaccSplitGetValue(s)));
        }
    }

    qof_instance_set_dirty(QOF_INSTANCE(trans));
    mark_trans(trans);  /* Dirty balance of every account in trans */
    xaccTransCommitEdit(trans);
}

/********************************************************************\
\********************************************************************/

void
xaccTransBeginEdit (Transaction *trans)
{
    if (!trans) return;
    if (!qof_begin_edit(&trans->inst)) return;

    if (qof_book_shutting_down(qof_instance_get_book(trans))) return;

    if (!qof_book_is_readonly(qof_instance_get_book(trans)))
    {
        xaccOpenLog ();
        xaccTransWriteLog (trans, 'B');
    }

    /* Make a clone of the transaction; we will use this
     * in case we need to roll-back the edit. */
    trans->orig = dupe_trans (trans);
}

/********************************************************************\
\********************************************************************/

void
xaccTransDestroy (Transaction *trans)
{
    if (!trans) return;

    if (!xaccTransGetReadOnly (trans) ||
            qof_book_shutting_down(qof_instance_get_book(trans)))
    {
        xaccTransBeginEdit(trans);
        qof_instance_set_destroying(trans, TRUE);
        xaccTransCommitEdit(trans);
    }
}

static void
destroy_gains (Transaction *trans)
{
    SplitList *node;
    for (node = trans->splits; node; node = node->next)
    {
        Split *s = node->data;
        if (!xaccTransStillHasSplit(trans, s))
            continue;

        if (GAINS_STATUS_UNKNOWN == s->gains) xaccSplitDetermineGainStatus(s);
        if (s->gains_split && (GAINS_STATUS_GAINS & s->gains_split->gains))
        {
            Transaction *t = s->gains_split->parent;
            xaccTransDestroy (t);
            s->gains_split = NULL;
        }
    }
}

static void
do_destroy (Transaction *trans)
{
    SplitList *node;
    gboolean shutting_down = qof_book_shutting_down(qof_instance_get_book(trans));

    /* If there are capital-gains transactions associated with this,
     * they need to be destroyed too unless we're shutting down in
     * which case all transactions will be destroyed. */
    if (!shutting_down)
        destroy_gains (trans);

    /* Make a log in the journal before destruction.  */
    if (!shutting_down && !qof_book_is_readonly(qof_instance_get_book(trans)))
        xaccTransWriteLog (trans, 'D');

    qof_event_gen (&trans->inst, QOF_EVENT_DESTROY, NULL);

    /* We only own the splits that still think they belong to us.   This is done
       in 2 steps.  In the first, the splits are marked as being destroyed, but they
       are not destroyed yet.  In the second, the destruction is committed which will
       do the actual destruction.  If both steps are done for a split before they are
       done for the next split, then a split will still be on the split list after it
       has been freed.  This can cause other parts of the code (e.g. in xaccSplitDestroy())
       to reference the split after it has been freed. */
    for (node = trans->splits; node; node = node->next)
    {
        Split *s = node->data;
        if (s && s->parent == trans)
        {
            xaccSplitDestroy(s);
        }
    }
    for (node = trans->splits; node; node = node->next)
    {
        Split *s = node->data;
        if (s && s->parent == trans)
        {
            xaccSplitCommitEdit(s);
        }
    }
    g_list_free (trans->splits);
    trans->splits = NULL;
    xaccFreeTransaction (trans);
}

/********************************************************************\
\********************************************************************/

/* Temporary hack for data consistency */
static int scrub_data = 1;
void xaccEnableDataScrubbing(void)
{
    scrub_data = 1;
}
void xaccDisableDataScrubbing(void)
{
    scrub_data = 0;
}

/* Check for an implicitly deleted transaction */
static gboolean was_trans_emptied(Transaction *trans)
{
    FOR_EACH_SPLIT(trans, return FALSE);
    return TRUE;
}

static void trans_on_error(Transaction *trans, QofBackendError errcode)
{
    /* If the backend puked, then we must roll-back
     * at this point, and let the user know that we failed.
     * The GUI should check for error conditions ...
     */
    if (ERR_BACKEND_MODIFIED == errcode)
    {
        PWARN("Another user has modified this transaction\n"
              "\tjust a moment ago. Please look at their changes,\n"
              "\tand try again, if needed.\n");
    }

    xaccTransRollbackEdit(trans);
    gnc_engine_signal_commit_error( errcode );
}

static void trans_cleanup_commit(Transaction *trans)
{
    GList *slist, *node;

    /* ------------------------------------------------- */
    /* Make sure all associated splits are in proper order
     * in their accounts with the correct balances. */

    /* Iterate over existing splits */
    slist = g_list_copy(trans->splits);
    for (node = slist; node; node = node->next)
    {
        Split *s = node->data;
        if (!qof_instance_is_dirty(QOF_INSTANCE(s)))
            continue;

        if ((s->parent != trans) || qof_instance_get_destroying(s))
        {
            /* Existing split either moved to another transaction or
               was destroyed, drop from list */
            GncEventData ed;
            ed.node = trans;
            ed.idx = g_list_index(trans->splits, s);
            trans->splits = g_list_remove(trans->splits, s);
            qof_event_gen(&s->inst, QOF_EVENT_REMOVE, &ed);
        }

        if (s->parent == trans)
        {
            /* Split was either added, destroyed or just changed */
            if (qof_instance_get_destroying(s))
                qof_event_gen(&s->inst, QOF_EVENT_DESTROY, NULL);
            else qof_event_gen(&s->inst, QOF_EVENT_MODIFY, NULL);
            xaccSplitCommitEdit(s);
        }
    }
    g_list_free(slist);

    if (!qof_book_is_readonly(qof_instance_get_book(trans)))
        xaccTransWriteLog (trans, 'C');

    /* Get rid of the copy we made. We won't be rolling back,
     * so we don't need it any more.  */
    PINFO ("get rid of rollback trans=%p", trans->orig);
    xaccFreeTransaction (trans->orig);
    trans->orig = NULL;

    /* Sort the splits. Why do we need to do this ?? */
    /* Good question.  Who knows?  */
    xaccTransSortSplits(trans);

    /* Put back to zero. */
    qof_instance_decrease_editlevel(trans);
    g_assert(qof_instance_get_editlevel(trans) == 0);

    gen_event_trans (trans); //TODO: could be conditional
    qof_event_gen (&trans->inst, QOF_EVENT_MODIFY, NULL);
}

void
xaccTransCommitEdit (Transaction *trans)
{
    if (!trans) return;
    ENTER ("(trans=%p)", trans);

    if (!qof_commit_edit (QOF_INSTANCE(trans)))
    {
        LEAVE("editlevel non-zero");
        return;
    }

    /* We increment this for the duration of the call
     * so other functions don't result in a recursive
     * call to xaccTransCommitEdit. */
    qof_instance_increase_editlevel(trans);

    if (was_trans_emptied(trans))
        qof_instance_set_destroying(trans, TRUE);

    /* Before committing the transaction, we are going to enforce certain
     * constraints.  In particular, we want to enforce the cap-gains
     * and the balanced lot constraints.  These constraints might
     * change the number of splits in this transaction, and the
     * transaction itself might be deleted.  This is also why
     * we can't really enforce these constraints elsewhere: they
     * can cause pointers to splits and transactions to disappear out
     * from under the holder.
     */
    if (!qof_instance_get_destroying(trans) && scrub_data &&
            !qof_book_shutting_down(xaccTransGetBook(trans)))
    {
        /* If scrubbing gains recurses through here, don't call it again. */
        scrub_data = 0;
        /* The total value of the transaction should sum to zero.
         * Call the trans scrub routine to fix it. Indirectly, this
         * routine also performs a number of other transaction fixes too.
         */
        xaccTransScrubImbalance (trans, NULL, NULL);
        /* Get the cap gains into a consistent state as well. */

        /* Lot Scrubbing is temporarily disabled. */
        if (g_getenv("GNC_AUTO_SCRUB_LOTS") != NULL)
            xaccTransScrubGains (trans, NULL);

        /* Allow scrubbing in transaction commit again */
        scrub_data = 1;
    }

    /* Record the time of last modification */
    if (0 == trans->date_entered)
    {
        trans->date_entered = gnc_time(NULL);
        qof_instance_set_dirty(QOF_INSTANCE(trans));
    }

    trans->txn_type = TXN_TYPE_UNCACHED;
    qof_commit_edit_part2(QOF_INSTANCE(trans),
                          (void (*) (QofInstance *, QofBackendError))
                          trans_on_error,
                          (void (*) (QofInstance *)) trans_cleanup_commit,
                          (void (*) (QofInstance *)) do_destroy);
    LEAVE ("(trans=%p)", trans);
}

#define SWAP_STR(a, b) do { const char *tmp = (a); (a) = (b); (b) = tmp; } while (0);
#define SWAP(a, b)     do { gpointer tmp = (a); (a) = (b); (b) = tmp; } while (0);

/* Ughhh. The Rollback function is terribly complex, and, what's worse,
 * it only rolls back the basics.  The TransCommit functions did a bunch
 * of Lot/Cap-gains scrubbing that don't get addressed/undone here, and
 * so the rollback can potentially leave a bit of a mess behind.  We
 * really need a more robust undo capability.  Part of the problem is
 * that the biggest user of the undo is the multi-user backend, which
 * also adds complexity.
 */
void
xaccTransRollbackEdit (Transaction *trans)
{
    GList *node, *onode;
    QofBackend *be;
    Transaction *orig;
    GList *slist;
    int num_preexist, i;

/* FIXME: This isn't quite the right way to handle nested edits --
 * there should be a stack of transaction states that are popped off
 * and restored at each level -- but it does prevent restoring to the
 * editlevel 0 state until one is returning to editlevel 0, and
 * thereby prevents a crash caused by trans->orig getting NULLed too
 * soon.
 */
    if (!qof_instance_get_editlevel (QOF_INSTANCE (trans))) return;
    if (qof_instance_get_editlevel (QOF_INSTANCE (trans)) > 1) {
	 qof_instance_decrease_editlevel (QOF_INSTANCE (trans));
	 return;
    }

    ENTER ("trans addr=%p\n", trans);

    check_open(trans);

    /* copy the original values back in. */

    orig = trans->orig;
    SWAP_STR(trans->num, orig->num);
    SWAP_STR(trans->description, orig->description);
    trans->date_entered = orig->date_entered;
    trans->date_posted = orig->date_posted;
    SWAP(trans->common_currency, orig->common_currency);
    qof_instance_swap_kvp (QOF_INSTANCE (trans), QOF_INSTANCE (orig));

    /* The splits at the front of trans->splits are exactly the same
       splits as in the original, but some of them may have changed, so
       we restore only those. */
/* FIXME: Runs off the transaction's splits, so deleted splits are not
 * restored!
 */
    num_preexist = g_list_length(orig->splits);
    slist = g_list_copy(trans->splits);
    for (i = 0, node = slist, onode = orig->splits; node;
            i++, node = node->next, onode = onode ? onode->next : NULL)
    {
        Split *s = node->data;

        if (!qof_instance_is_dirty(QOF_INSTANCE(s)))
            continue;

        if (i < num_preexist && onode)
        {
            Split *so = onode->data;

            xaccSplitRollbackEdit(s);
            SWAP_STR(s->action, so->action);
            SWAP_STR(s->memo, so->memo);
	    qof_instance_copy_kvp (QOF_INSTANCE (s), QOF_INSTANCE (so));
            s->reconciled = so->reconciled;
            s->amount = so->amount;
            s->value = so->value;
            s->lot = so->lot;
            s->gains_split = so->gains_split;
            //SET_GAINS_A_VDIRTY(s);
            s->date_reconciled = so->date_reconciled;
            qof_instance_mark_clean(QOF_INSTANCE(s));
            xaccFreeSplit(so);
        }
        else
        {
            /* Potentially added splits */
            if (trans != xaccSplitGetParent(s))
            {
                trans->splits = g_list_remove(trans->splits, s);
                /* New split added, but then moved to another
                   transaction */
                continue;
            }
            xaccSplitRollbackEdit(s);
            trans->splits = g_list_remove(trans->splits, s);
            g_assert(trans != xaccSplitGetParent(s));
            /* NB: our memory management policy here is that a new split
               added to the transaction which is then rolled-back still
               belongs to the engine.  Specifically, it's freed by the
               transaction to which it was added.  Don't add the Split to
               more than one transaction during the begin/commit block! */
            if (NULL == xaccSplitGetParent(s))
            {
                xaccFreeSplit(s);  // a newly malloc'd split
            }
        }
    }
    g_list_free(slist);
    g_list_free(orig->splits);
    orig->splits = NULL;

    /* Now that the engine copy is back to its original version,
     * get the backend to fix it in the database */
    be = qof_book_get_backend(qof_instance_get_book(trans));
    /** \todo Fix transrollbackedit in QOF so that rollback
    is exposed via the API. */
    if (qof_backend_can_rollback (be))
    {
        QofBackendError errcode;

        /* clear errors */
        do
        {
            errcode = qof_backend_get_error (be);
        }
        while (ERR_BACKEND_NO_ERR != errcode);

        qof_backend_rollback_instance (be, &(trans->inst));

        errcode = qof_backend_get_error (be);
        if (ERR_BACKEND_MOD_DESTROY == errcode)
        {
            /* The backend is asking us to delete this transaction.
             * This typically happens because another (remote) user
             * has deleted this transaction, and we haven't found
             * out about it until this user tried to edit it.
             */
            xaccTransDestroy (trans);
            do_destroy (trans);

            /* push error back onto the stack */
            qof_backend_set_error (be, errcode);
            LEAVE ("deleted trans addr=%p\n", trans);
            return;
        }
        if (ERR_BACKEND_NO_ERR != errcode)
        {
            PERR ("Rollback Failed.  Ouch!");
            /* push error back onto the stack */
            qof_backend_set_error (be, errcode);
        }
    }

    if (!qof_book_is_readonly(qof_instance_get_book(trans)))
        xaccTransWriteLog (trans, 'R');

    xaccFreeTransaction (trans->orig);

    trans->orig = NULL;
    qof_instance_set_destroying(trans, FALSE);

    /* Put back to zero. */
    qof_instance_decrease_editlevel(trans);
    /* FIXME: The register code seems to depend on the engine to
       generate an event during rollback, even though the state is just
       reverting to what it was. */
    gen_event_trans (trans);

    LEAVE ("trans addr=%p\n", trans);
}

gboolean
xaccTransIsOpen (const Transaction *trans)
{
    return trans ? (0 < qof_instance_get_editlevel(trans)) : FALSE;
}

#define SECS_PER_DAY 86400

int
xaccTransOrder (const Transaction *ta, const Transaction *tb)
{
    return xaccTransOrder_num_action (ta, NULL, tb, NULL);
}

/* Order a pair of potentially numeric string as numbers if both
 * strings begin with numbers, ordering the remainder of the string
 * lexically if the numeric parts are equal, and the whole strings
 * lexically otherwise.
 *
 * Note that this won't work well for numbers > 10^18 and that
 * negative numbers are treated as strings and will cause the pair to
 * be ordered lexically.
 */

static int
order_by_int64_or_string (const char* a, const char* b)
{
     char *end_a = NULL, *end_b = NULL;
     int cmp = 0;
     uint64_t na = strtoull(a, &end_a, 10);
     uint64_t nb = strtoull(b, &end_b, 10);
     if (na && nb)
     {
          if (na != nb)
               return na < nb ? -1 : 1;
          cmp = g_utf8_collate(end_a, end_b);
     }
     else
     {
          cmp = g_utf8_collate(a, b);
     }
     return cmp < 0 ? -1 : cmp > 0 ? 1 : 0;
}

int
xaccTransOrder_num_action (const Transaction *ta, const char *actna,
                            const Transaction *tb, const char *actnb)
{
    const char *da, *db;
    int retval;
    int64_t na, nb;

    if ( ta && !tb ) return -1;
    if ( !ta && tb ) return +1;
    if ( !ta && !tb ) return 0;

    if (ta->date_posted != tb->date_posted)
        return (ta->date_posted > tb->date_posted) - (ta->date_posted < tb->date_posted);

    /* Always sort closing transactions after normal transactions */
    {
        gboolean ta_is_closing = xaccTransGetIsClosingTxn (ta);
        gboolean tb_is_closing = xaccTransGetIsClosingTxn (tb);
        if (ta_is_closing != tb_is_closing)
            return (ta_is_closing - tb_is_closing);
    }

    /* otherwise, sort on number string */
    if (actna && actnb) /* split action string, if not NULL */
    {
         retval = order_by_int64_or_string (actna, actnb);
    }
    else                /* else transaction num string */
    {
         retval = order_by_int64_or_string (ta->num, tb->num);
    }
    if (retval)
         return retval;

    if (ta->date_entered != tb->date_entered)
        return (ta->date_entered > tb->date_entered) - (ta->date_entered < tb->date_entered);

    /* otherwise, sort on description string */
    da = ta->description ? ta->description : "";
    db = tb->description ? tb->description : "";
    retval = g_utf8_collate (da, db);
    if (retval)
        return retval;

    /* else, sort on guid - keeps sort stable. */
    return qof_instance_guid_compare(ta, tb);
}

/********************************************************************\
\********************************************************************/

static inline void
xaccTransSetDateInternal(Transaction *trans, time64 *dadate, time64 val)
{
    xaccTransBeginEdit(trans);

#if 0 /* gnc_ctime is expensive so change to 1 only if you need to debug setting
       * dates. */
   {
        time64 secs = (time64) val.tv_sec;
        gchar *tstr = gnc_ctime (&secs);
        PINFO ("addr=%p set date to %" G_GUINT64_FORMAT ".%09ld %s\n",
               trans, val.tv_sec, val.tv_nsec, tstr ? tstr : "(null)");
        g_free(tstr);
    }
#endif
    *dadate = val;
    qof_instance_set_dirty(QOF_INSTANCE(trans));
    mark_trans(trans);
    xaccTransCommitEdit(trans);

    /* Because the date has changed, we need to make sure that each of
     * the splits is properly ordered in each of their accounts. We
     * could do that here, simply by reinserting each split into its
     * account. However, in some ways this is bad behaviour, and it
     * seems much better/nicer to defer that until the commit phase,
     * i.e. until the user has called the xaccTransCommitEdit()
     * routine. So, for now, we are done. */
}

static inline void
set_gains_date_dirty (Transaction *trans)
{
    FOR_EACH_SPLIT(trans, s->gains |= GAINS_STATUS_DATE_DIRTY);
}

void
xaccTransSetDatePostedSecs (Transaction *trans, time64 secs)
{
    if (!trans) return;
    xaccTransSetDateInternal(trans, &trans->date_posted, secs);
    set_gains_date_dirty(trans);
}

void
xaccTransSetDatePostedSecsNormalized (Transaction *trans, time64 time)
{
    GDate date;
    gnc_gdate_set_time64(&date, time);
    xaccTransSetDatePostedGDate(trans, date);
}

void
xaccTransSetDatePostedGDate (Transaction *trans, GDate date)
{
    GValue v = G_VALUE_INIT;
    if (!trans) return;

    /* We additionally save this date into a kvp frame to ensure in
     * the future a date which was set as *date* (without time) can
     * clearly be distinguished from the time64. */
    g_value_init (&v, G_TYPE_DATE);
    g_value_set_boxed (&v, &date);
    qof_instance_set_kvp (QOF_INSTANCE(trans), &v, 1, TRANS_DATE_POSTED);
    g_value_unset (&v);
    /* mark dirty and commit handled by SetDateInternal */
    xaccTransSetDateInternal(trans, &trans->date_posted,
                             gdate_to_time64(date));
    set_gains_date_dirty (trans);
}

void
xaccTransSetDateEnteredSecs (Transaction *trans, time64 secs)
{
    if (!trans) return;
    xaccTransSetDateInternal(trans, &trans->date_entered, secs);
}

static void
qofTransSetDatePosted (Transaction *trans, time64 time)
{
    if (!trans) return;
    if (!qof_begin_edit(&trans->inst)) return;
    xaccTransSetDateInternal(trans, &trans->date_posted, time);
    set_gains_date_dirty(trans);
    qof_commit_edit(&trans->inst);
}

static void
qofTransSetDateEntered (Transaction *trans, time64 time)
{
    if (!trans) return;
    if (!qof_begin_edit(&trans->inst)) return;
    xaccTransSetDateInternal(trans, &trans->date_entered, time);
    qof_commit_edit(&trans->inst);
}

void
xaccTransSetDate (Transaction *trans, int day, int mon, int year)
{
    GDate *date;
    if (!trans) return;
    date = g_date_new_dmy(day, mon, year);
    if (!g_date_valid(date))
    {
        PWARN("Attempted to set invalid date %d-%d-%d; set today's date instead.",
              year, mon, day);
        g_free(date);
        date = gnc_g_date_new_today();
    }
    xaccTransSetDatePostedGDate(trans, *date);
    g_free(date);
}

void
xaccTransSetDateDue (Transaction * trans, time64 time)
{
    GValue v = G_VALUE_INIT;
    if (!trans) return;
    g_value_init (&v, GNC_TYPE_TIME64);
    g_value_set_boxed (&v, &time);
    xaccTransBeginEdit(trans);
    qof_instance_set_kvp (QOF_INSTANCE (trans), &v, 1, TRANS_DATE_DUE_KVP);
    qof_instance_set_dirty(QOF_INSTANCE(trans));
    g_value_unset (&v);
    xaccTransCommitEdit(trans);
}

void
xaccTransSetTxnType (Transaction *trans, char type)
{
    char s[2] = {type, '\0'};
    GValue v = G_VALUE_INIT;
    g_return_if_fail(trans);
    g_value_init (&v, G_TYPE_STRING);
    qof_instance_get_kvp (QOF_INSTANCE (trans), &v, 1, TRANS_TXN_TYPE_KVP);
    if (!g_strcmp0 (s, g_value_get_string (&v)))
    {
        g_value_unset (&v);
        return;
    }
    g_value_set_string (&v, s);
    xaccTransBeginEdit(trans);
    qof_instance_set_kvp (QOF_INSTANCE (trans), &v, 1, TRANS_TXN_TYPE_KVP);
    qof_instance_set_dirty(QOF_INSTANCE(trans));
    g_value_unset (&v);
    xaccTransCommitEdit(trans);
}

void xaccTransClearReadOnly (Transaction *trans)
{
    if (trans)
    {
        xaccTransBeginEdit(trans);
        qof_instance_set_kvp (QOF_INSTANCE (trans), NULL, 1, TRANS_READ_ONLY_REASON);
        qof_instance_set_dirty(QOF_INSTANCE(trans));
        xaccTransCommitEdit(trans);

        if (trans->readonly_reason != is_unset)
            g_free (trans->readonly_reason);
        trans->readonly_reason = NULL;
    }
}

void
xaccTransSetReadOnly (Transaction *trans, const char *reason)
{
    if (trans && reason)
    {
        GValue v = G_VALUE_INIT;
        g_value_init (&v, G_TYPE_STRING);
        g_value_set_string (&v, reason);
        xaccTransBeginEdit(trans);
        qof_instance_set_kvp (QOF_INSTANCE (trans), &v, 1, TRANS_READ_ONLY_REASON);
        qof_instance_set_dirty(QOF_INSTANCE(trans));
        g_value_unset (&v);
        xaccTransCommitEdit(trans);

        if (trans->readonly_reason != is_unset)
            g_free (trans->readonly_reason);
        trans->readonly_reason = g_strdup (reason);
    }
}

/********************************************************************\
\********************************************************************/

/* QOF does not open the trans before setting a parameter,
but the call uses check_open so we cannot use the call directly. */
static void
qofTransSetNum (Transaction *trans, const char *xnum)
{
    if (!qof_begin_edit(&trans->inst)) return;
    xaccTransSetNum(trans, xnum);
    qof_commit_edit(&trans->inst);
}

void
xaccTransSetNum (Transaction *trans, const char *xnum)
{
    if (!trans || !xnum) return;
    xaccTransBeginEdit(trans);

    CACHE_REPLACE(trans->num, xnum);
    qof_instance_set_dirty(QOF_INSTANCE(trans));
    mark_trans(trans);  /* Dirty balance of every account in trans */
    xaccTransCommitEdit(trans);
}

static void
qofTransSetDescription (Transaction *trans, const char *desc)
{
    if (!qof_begin_edit(&trans->inst)) return;
    xaccTransSetDescription(trans, desc);
    qof_commit_edit(&trans->inst);
}

void
xaccTransSetDescription (Transaction *trans, const char *desc)
{
    if (!trans || !desc) return;
    xaccTransBeginEdit(trans);

    CACHE_REPLACE(trans->description, desc);
    qof_instance_set_dirty(QOF_INSTANCE(trans));
    xaccTransCommitEdit(trans);
}

void
xaccTransSetDocLink (Transaction *trans, const char *doclink)
{
    if (!trans || !doclink) return;

    if (trans->doclink != is_unset)
    {
        if (!g_strcmp0 (doclink, trans->doclink))
            return;

        g_free (trans->doclink);
    }
    xaccTransBeginEdit(trans);
    if (doclink[0] == '\0')
    {
        trans->doclink = NULL;
        qof_instance_set_kvp (QOF_INSTANCE (trans), NULL, 1, doclink_uri_str);
    }
    else
    {
        GValue v = G_VALUE_INIT;
        trans->doclink = g_strdup (doclink);
        g_value_init (&v, G_TYPE_STRING);
        g_value_set_string (&v, doclink);
        qof_instance_set_kvp (QOF_INSTANCE (trans), &v, 1, doclink_uri_str);
        g_value_unset (&v);
    }
    qof_instance_set_dirty(QOF_INSTANCE(trans));
    xaccTransCommitEdit(trans);
}

static void
qofTransSetNotes (Transaction *trans, const char *notes)
{
    if (!qof_begin_edit(&trans->inst)) return;
    xaccTransSetNotes(trans, notes);
    qof_commit_edit(&trans->inst);
}

void
xaccTransSetNotes (Transaction *trans, const char *notes)
{
    GValue v = G_VALUE_INIT;
    if (!trans || !notes) return;
    if (trans->notes != is_unset)
    {
        if (!g_strcmp0 (notes, trans->notes))
            return;

        g_free (trans->notes);
    }
    g_value_init (&v, G_TYPE_STRING);
    g_value_set_string (&v, notes);
    xaccTransBeginEdit(trans);

    trans->notes = g_strdup (notes);
    qof_instance_set_kvp (QOF_INSTANCE (trans), &v, 1, trans_notes_str);
    qof_instance_set_dirty(QOF_INSTANCE(trans));
    g_value_unset (&v);
    xaccTransCommitEdit(trans);
}

void
xaccTransSetIsClosingTxn (Transaction *trans, gboolean is_closing)
{
    if (!trans) return;
    xaccTransBeginEdit(trans);

    if (is_closing)
    {
        GValue v = G_VALUE_INIT;
        g_value_init (&v, G_TYPE_INT64);
        g_value_set_int64 (&v, 1);
        qof_instance_set_kvp (QOF_INSTANCE (trans), &v, 1, trans_is_closing_str);
        g_value_unset (&v);
        trans->isClosingTxn_cached = 1;
    }
    else
    {
        qof_instance_set_kvp (QOF_INSTANCE (trans), NULL, 1, trans_is_closing_str);
        trans->isClosingTxn_cached = 0;
    }
    qof_instance_set_dirty(QOF_INSTANCE(trans));
    xaccTransCommitEdit(trans);
}


/********************************************************************\
\********************************************************************/

Split *
xaccTransGetSplit (const Transaction *trans, int i)
{
    int j = 0;
    if (!trans || i < 0) return NULL;

    FOR_EACH_SPLIT(trans, { if (i == j) return s; j++; });
    return NULL;
}

int
xaccTransGetSplitIndex(const Transaction *trans, const Split *split)
{
    int j = 0;
    g_return_val_if_fail(trans && split, -1);

    FOR_EACH_SPLIT(trans, { if (s == split) return j; j++; });
    return -1;
}

SplitList *
xaccTransGetSplitList (const Transaction *trans)
{
    return trans ? trans->splits : NULL;
}

SplitList *
xaccTransGetPaymentAcctSplitList (const Transaction *trans)
{
    GList *pay_splits = NULL;
    FOR_EACH_SPLIT (trans,
                    const Account *account = xaccSplitGetAccount(s);
                    if (account && gncBusinessIsPaymentAcctType(xaccAccountGetType(account)))
                        pay_splits = g_list_prepend (pay_splits, s);
    );

    pay_splits = g_list_reverse (pay_splits);
    return pay_splits;
}

SplitList *
xaccTransGetAPARAcctSplitList (const Transaction *trans, gboolean strict)
{
    GList *apar_splits = NULL;
    if (!trans) return NULL;

    FOR_EACH_SPLIT (trans,
                    const Account *account = xaccSplitGetAccount(s);
                    if (account && xaccAccountIsAPARType(xaccAccountGetType(account)))
                    {

                        if (!strict)
                            apar_splits = g_list_prepend (apar_splits, s);
                        else
                        {
                            GncOwner owner;
                            GNCLot *lot = xaccSplitGetLot(s);
                            if (lot &&
                                (gncInvoiceGetInvoiceFromLot (lot) ||
                                gncOwnerGetOwnerFromLot (lot, &owner)))
                                apar_splits = g_list_prepend (apar_splits, s);
                        }
                    }
    );

    apar_splits = g_list_reverse (apar_splits);
    return apar_splits;
}

Split *xaccTransGetFirstPaymentAcctSplit(const Transaction *trans)
{
    FOR_EACH_SPLIT (trans,
                    const Account *account = xaccSplitGetAccount(s);
                    if (account && gncBusinessIsPaymentAcctType(xaccAccountGetType(account)))
                        return s;
                   );

    return NULL;
}

Split *xaccTransGetFirstAPARAcctSplit (const Transaction *trans, gboolean strict)
{
    FOR_EACH_SPLIT (trans,
                    const Account *account = xaccSplitGetAccount(s);
                    if (account && xaccAccountIsAPARType(xaccAccountGetType(account)))
                    {
                        GNCLot *lot;
                        GncOwner owner;

                        if (!strict)
                            return s;

                        lot = xaccSplitGetLot(s);
                        if (lot &&
                            (gncInvoiceGetInvoiceFromLot (lot) ||
                            gncOwnerGetOwnerFromLot (lot, &owner)))
                            return s;
                    }
                   );

    return NULL;
}

int
xaccTransCountSplits (const Transaction *trans)
{
    gint i = 0;
    g_return_val_if_fail (trans != NULL, 0);
    FOR_EACH_SPLIT(trans, i++);
    return i;
}

const char *
xaccTransGetNum (const Transaction *trans)
{
    return trans ? trans->num : NULL;
}

const char *
xaccTransGetDescription (const Transaction *trans)
{
    return trans ? trans->description : NULL;
}

const char *
xaccTransGetDocLink (const Transaction *trans)
{
    g_return_val_if_fail (trans, NULL);
    if (trans->doclink == is_unset)
    {
        GValue v = G_VALUE_INIT;
        Transaction *t = (Transaction*) trans;
        qof_instance_get_kvp (QOF_INSTANCE (trans), &v, 1, doclink_uri_str);
        t->doclink = G_VALUE_HOLDS_STRING (&v) ? g_value_dup_string (&v) : NULL;
        g_value_unset (&v);
    }
    return trans->doclink;
}

const char *
xaccTransGetNotes (const Transaction *trans)
{
    g_return_val_if_fail (trans, NULL);
    if (trans->notes == is_unset)
    {
        GValue v = G_VALUE_INIT;
        Transaction *t = (Transaction*) trans;
        qof_instance_get_kvp (QOF_INSTANCE (trans), &v, 1, trans_notes_str);
        t->notes = G_VALUE_HOLDS_STRING (&v) ? g_value_dup_string (&v) : NULL;
        g_value_unset (&v);
    }
    return trans->notes;
}

gboolean
xaccTransGetIsClosingTxn (const Transaction *trans)
{
    if (!trans) return FALSE;
    if (trans->isClosingTxn_cached == -1)
    {
        Transaction* trans_nonconst = (Transaction*) trans;
        GValue v = G_VALUE_INIT;
        qof_instance_get_kvp (QOF_INSTANCE (trans), &v, 1, trans_is_closing_str);
        if (G_VALUE_HOLDS_INT64 (&v))
            trans_nonconst->isClosingTxn_cached = (g_value_get_int64 (&v) ? 1 : 0);
        else
            trans_nonconst->isClosingTxn_cached = 0;
        g_value_unset (&v);
    }
    return (trans->isClosingTxn_cached == 1)
            ? TRUE
            : FALSE;
}

/********************************************************************\
\********************************************************************/

time64
xaccTransGetDate (const Transaction *trans)
{
    return trans ? trans->date_posted : 0;
}

/*################## Added for Reg2 #################*/
time64
xaccTransGetDateEntered (const Transaction *trans)
{
    return trans ? trans->date_entered : 0;
}
/*################## Added for Reg2 #################*/

time64
xaccTransRetDatePosted (const Transaction *trans)
{
    return trans ? trans->date_posted : 0;
}

GDate
xaccTransGetDatePostedGDate (const Transaction *trans)
{
    GDate result;
    g_date_clear (&result, 1);
    if (trans)
    {
        /* Can we look up this value in the kvp slot? If yes, use it
         * from there because it doesn't suffer from time zone
         * shifts. */
        GValue v = G_VALUE_INIT;
        qof_instance_get_kvp (QOF_INSTANCE (trans), &v, 1, TRANS_DATE_POSTED);
        if (G_VALUE_HOLDS_BOXED (&v))
             result = *(GDate*)g_value_get_boxed (&v);
        g_value_unset (&v);
        if (! g_date_valid (&result) || gdate_to_time64 (result) == INT64_MAX)
        {
             /* Well, this txn doesn't have a valid GDate saved in a slot.
              * time64_to_gdate() uses local time and we want UTC so we have
              * to write it out.
              */
             time64 time = xaccTransGetDate(trans);
             struct tm *stm = gnc_gmtime(&time);
             if (stm)
             {
                 g_date_set_dmy(&result, stm->tm_mday,
                                (GDateMonth)(stm->tm_mon + 1),
                                stm->tm_year + 1900);
                 free(stm);
             }
        }
    }
    return result;
}

time64
xaccTransRetDateEntered (const Transaction *trans)
{
    return trans ? trans->date_entered : 0;
}

time64
xaccTransRetDateDue(const Transaction *trans)
{
    time64 ret = 0;
    GValue v = G_VALUE_INIT;
    if (!trans) return 0;
    qof_instance_get_kvp (QOF_INSTANCE (trans), &v, 1, TRANS_DATE_DUE_KVP);
    if (G_VALUE_HOLDS_BOXED (&v))
    {
        ret = ((Time64*)g_value_get_boxed (&v))->t;
        g_value_unset (&v);
    }
    if (!ret)
        return xaccTransRetDatePosted (trans);
    return ret;
}

char
xaccTransGetTxnType (Transaction *trans)
{
    gboolean has_nonAPAR_amount = FALSE;

    if (!trans) return TXN_TYPE_NONE;

    if (trans->txn_type != TXN_TYPE_UNCACHED)
        return trans->txn_type;

    trans->txn_type = TXN_TYPE_NONE;
    for (GList *n = xaccTransGetSplitList (trans); n; n = g_list_next (n))
    {
        Account *acc = xaccSplitGetAccount (n->data);

        if (!acc)
            continue;

        if (!xaccAccountIsAPARType (xaccAccountGetType (acc)) &&
            !gnc_numeric_zero_p (xaccSplitGetValue (n->data)))
            has_nonAPAR_amount = TRUE;
        else if (trans->txn_type == TXN_TYPE_NONE)
        {
            GNCLot *lot = xaccSplitGetLot (n->data);
            GncInvoice *invoice = gncInvoiceGetInvoiceFromLot (lot);
            GncOwner owner;

            if (invoice && trans == gncInvoiceGetPostedTxn (invoice))
                trans->txn_type = TXN_TYPE_INVOICE;
            else if (invoice || gncOwnerGetOwnerFromLot (lot, &owner))
                trans->txn_type = TXN_TYPE_PAYMENT;
        }
    }

    if (!has_nonAPAR_amount && (trans->txn_type == TXN_TYPE_PAYMENT))
        trans->txn_type = TXN_TYPE_LINK;

    return trans->txn_type;
}

const char *
xaccTransGetReadOnly (Transaction *trans)
{
    if (!trans)
        return NULL;

    if (trans->readonly_reason == is_unset)
    {
        GValue v = G_VALUE_INIT;
        qof_instance_get_kvp (QOF_INSTANCE(trans), &v, 1, TRANS_READ_ONLY_REASON);
        trans->readonly_reason = G_VALUE_HOLDS_STRING (&v) ?
            g_value_dup_string (&v) : NULL;
        g_value_unset (&v);
    }
    return trans->readonly_reason;
}

static gboolean
xaccTransIsSXTemplate (const Transaction * trans)
{
    Split *split0 = xaccTransGetSplit (trans, 0);
    if (split0 != NULL)
    {
	char* formula = NULL;
	g_object_get (split0, "sx-debit-formula", &formula, NULL);
	if (formula != NULL)
	{
	    g_free (formula);
	    return TRUE;
	}
	g_object_get (split0, "sx-credit-formula", &formula, NULL);
 	if (formula != NULL)
	{
	    g_free (formula);
	    return TRUE;
	}
    }
    return FALSE;
}

gboolean xaccTransIsReadonlyByPostedDate(const Transaction *trans)
{
    GDate *threshold_date;
    GDate trans_date;
    const QofBook *book = xaccTransGetBook (trans);
    gboolean result;
    g_assert(trans);

    if (!qof_book_uses_autoreadonly(book))
    {
        return FALSE;
    }

    if (xaccTransIsSXTemplate (trans))
	return FALSE;

    threshold_date = qof_book_get_autoreadonly_gdate(book);
    g_assert(threshold_date); // ok because we checked uses_autoreadonly before
    trans_date = xaccTransGetDatePostedGDate(trans);

//    g_warning("there is auto-read-only with days=%d, trans_date_day=%d, threshold_date_day=%d",
//              qof_book_get_num_days_autofreeze(book),
//              g_date_get_day(&trans_date),
//              g_date_get_day(threshold_date));

    if (g_date_compare(&trans_date, threshold_date) < 0)
    {
        //g_warning("we are auto-read-only");
        result = TRUE;
    }
    else
    {
        result = FALSE;
    }
    g_date_free(threshold_date);
    return result;
}

/*################## Added for Reg2 #################*/

gboolean xaccTransInFutureByPostedDate (const Transaction *trans)
{
    time64 present;
    gboolean result;
    g_assert(trans);

    present = gnc_time64_get_today_end ();

    if (trans->date_posted > present)
        result = TRUE;
    else
        result = FALSE;

    return result;
}

/*################## Added for Reg2 #################*/

gboolean
xaccTransHasReconciledSplitsByAccount (const Transaction *trans,
                                       const Account *account)
{
    GList *node;

    for (node = xaccTransGetSplitList (trans); node; node = node->next)
    {
        Split *split = node->data;

        if (!xaccTransStillHasSplit(trans, split))
            continue;
        if (account && (xaccSplitGetAccount(split) != account))
            continue;

        switch (xaccSplitGetReconcile (split))
        {
        case YREC:
        case FREC:
            return TRUE;

        default:
            break;
        }
    }

    return FALSE;
}

gboolean
xaccTransHasReconciledSplits (const Transaction *trans)
{
    return xaccTransHasReconciledSplitsByAccount (trans, NULL);
}


gboolean
xaccTransHasSplitsInStateByAccount (const Transaction *trans,
                                    const char state,
                                    const Account *account)
{
    GList *node;

    for (node = xaccTransGetSplitList (trans); node; node = node->next)
    {
        Split *split = node->data;

        if (!xaccTransStillHasSplit(trans, split))
            continue;
        if (account && (xaccSplitGetAccount(split) != account))
            continue;

        if (split->reconciled == state)
            return TRUE;
    }

    return FALSE;
}

gboolean
xaccTransHasSplitsInState (const Transaction *trans, const char state)
{
    return xaccTransHasSplitsInStateByAccount (trans, state, NULL);
}


/********************************************************************\
\********************************************************************/


/* ====================================================================== */

static int
counter_thunk(Transaction *t, void *data)
{
    (*((guint*)data))++;
    return 0;
}

guint
gnc_book_count_transactions(QofBook *book)
{
    guint count = 0;
    xaccAccountTreeForEachTransaction(gnc_book_get_root_account(book),
                                      counter_thunk, (void*)&count);
    return count;
}

/********************************************************************\
\********************************************************************/

void
xaccTransVoid(Transaction *trans, const char *reason)
{
    GValue v = G_VALUE_INIT;
    char iso8601_str[ISO_DATELENGTH + 1] = "";

    g_return_if_fail(trans && reason);

    /* Prevent voiding transactions that are already marked
     * read only, for example generated by the business features.
     */
    if (xaccTransGetReadOnly (trans))
    {
        PWARN ("Refusing to void a read-only transaction!");
        return;
    }
    xaccTransBeginEdit(trans);
    qof_instance_get_kvp (QOF_INSTANCE (trans), &v, 1, trans_notes_str);
    if (G_VALUE_HOLDS_STRING (&v))
        qof_instance_set_kvp (QOF_INSTANCE (trans), &v, 1, void_former_notes_str);
    else
        g_value_init (&v, G_TYPE_STRING);

    g_value_set_string (&v, _("Voided transaction"));
    qof_instance_set_kvp (QOF_INSTANCE (trans), &v, 1, trans_notes_str);
    g_value_set_string (&v, reason);
    qof_instance_set_kvp (QOF_INSTANCE (trans), &v, 1, void_reason_str);
    if (trans->void_reason != is_unset)
        g_free (trans->void_reason);
    trans->void_reason = g_strdup (reason);

    gnc_time64_to_iso8601_buff (gnc_time(NULL), iso8601_str);
    g_value_set_string (&v, iso8601_str);
    qof_instance_set_kvp (QOF_INSTANCE (trans), &v, 1, void_time_str);
    g_value_unset (&v);

    FOR_EACH_SPLIT(trans, xaccSplitVoid(s));

    /* Dirtying taken care of by SetReadOnly */
    xaccTransSetReadOnly(trans, _("Transaction Voided"));
    xaccTransCommitEdit(trans);
}

gboolean
xaccTransGetVoidStatus(const Transaction *trans)
{
    const char *s = xaccTransGetVoidReason (trans);
    return (s && *s);
}

const char *
xaccTransGetVoidReason(const Transaction *trans)
{
    g_return_val_if_fail (trans, NULL);
    if (trans->void_reason == is_unset)
    {
        GValue v = G_VALUE_INIT;
        Transaction *t = (Transaction*) trans;
        qof_instance_get_kvp (QOF_INSTANCE (trans), &v, 1, void_reason_str);
        t->void_reason = G_VALUE_HOLDS_STRING (&v) ? g_value_dup_string (&v) : NULL;
        g_value_unset (&v);
    }
    return trans->void_reason;
}

time64
xaccTransGetVoidTime(const Transaction *tr)
{
    GValue v = G_VALUE_INIT;
    const char *s = NULL;
    time64 void_time = 0;

    g_return_val_if_fail(tr, void_time);
    qof_instance_get_kvp (QOF_INSTANCE (tr), &v, 1, void_time_str);
    if (G_VALUE_HOLDS_STRING (&v))
    {
        s = g_value_get_string (&v);
        if (s)
            void_time = gnc_iso8601_to_time64_gmt (s);
    }
    g_value_unset (&v);
    return void_time;
}

void
xaccTransUnvoid (Transaction *trans)
{
    GValue v = G_VALUE_INIT;
    const char *s = NULL;
    g_return_if_fail(trans);

    s = xaccTransGetVoidReason (trans);
    if (s == NULL) return; /* Transaction isn't voided. Bail. */
    xaccTransBeginEdit(trans);

    qof_instance_get_kvp (QOF_INSTANCE (trans), &v, 1, void_former_notes_str);
    if (G_VALUE_HOLDS_STRING (&v))
        qof_instance_set_kvp (QOF_INSTANCE (trans), &v, 1, trans_notes_str);
    qof_instance_set_kvp (QOF_INSTANCE (trans), NULL, 1, void_former_notes_str);
    qof_instance_set_kvp (QOF_INSTANCE (trans), NULL, 1, void_reason_str);
    qof_instance_set_kvp (QOF_INSTANCE (trans), NULL, 1, void_time_str);
    g_value_unset (&v);
    g_free (trans->void_reason);
    trans->void_reason = NULL;

    FOR_EACH_SPLIT(trans, xaccSplitUnvoid(s));

    /* Dirtying taken care of by ClearReadOnly */
    xaccTransClearReadOnly(trans);
    xaccTransCommitEdit(trans);
}

Transaction *
xaccTransReverse (Transaction *orig)
{
    Transaction *trans;
    GValue v = G_VALUE_INIT;
    g_return_val_if_fail(orig, NULL);

    /* First edit, dirty, and commit orig to ensure that any trading
     * splits are correctly balanced.
     */
    xaccTransBeginEdit (orig);
    qof_instance_set_dirty (QOF_INSTANCE (orig));
    xaccTransCommitEdit (orig);

    trans = xaccTransClone(orig);
    g_return_val_if_fail (trans, NULL);
    xaccTransBeginEdit(trans);

    /* Reverse the values on each split. Clear per-split info. */
    FOR_EACH_SPLIT(trans,
    {
        xaccSplitSetAmount(s, gnc_numeric_neg(xaccSplitGetAmount(s)));
        xaccSplitSetValue(s, gnc_numeric_neg(xaccSplitGetValue(s)));
        xaccSplitSetReconcile(s, NREC);
    });

    /* Now update the original with a pointer to the new one */
    g_value_init (&v, GNC_TYPE_GUID);
    g_value_set_boxed (&v, xaccTransGetGUID(trans));
    qof_instance_set_kvp (QOF_INSTANCE (orig), &v, 1, TRANS_REVERSED_BY);
    g_value_unset (&v);

    /* Make sure the reverse transaction is not read-only */
    xaccTransClearReadOnly(trans);

    qof_instance_set_dirty(QOF_INSTANCE(trans));
    xaccTransCommitEdit(trans);
    return trans;
}

Transaction *
xaccTransGetReversedBy(const Transaction *trans)
{
    GValue v = G_VALUE_INIT;
    Transaction *retval = NULL;
    g_return_val_if_fail(trans, NULL);
    qof_instance_get_kvp (QOF_INSTANCE(trans), &v, 1, TRANS_REVERSED_BY);
    if (G_VALUE_HOLDS_BOXED (&v))
    {
        GncGUID* guid = g_value_get_boxed (&v);
        retval = xaccTransLookup(guid, qof_instance_get_book (trans));
    }
    g_value_unset (&v);
    return retval;
}

void
xaccTransScrubSplits (Transaction *trans)
{
    gnc_commodity *currency;

    if (!trans) return;

    xaccTransBeginEdit(trans);
    /* The split scrub expects the transaction to have a currency! */
    currency = xaccTransGetCurrency (trans);
    if (!currency)
        PERR ("Transaction doesn't have a currency!");

    FOR_EACH_SPLIT(trans, xaccSplitScrub(s));
    xaccTransCommitEdit(trans);
}

/* ============================================================== */
/** The xaccTransScrubGainsDate() routine is used to keep the posted date
 *    of gains splits in sync with the posted date of the transaction
 *    that caused the gains.
 *
 *    The posted date is kept in sync using a lazy-evaluation scheme.
 *    If xaccTransactionSetDatePosted() is called, the date change is
 *    accepted, and the split is marked date-dirty.  If the posted date
 *    is queried for (using GetDatePosted()), then the transaction is
 *    evaluated. If it's a gains-transaction, then its date is copied
 *    from the source transaction that created the gains.
 */

static void
xaccTransScrubGainsDate (Transaction *trans)
{
    SplitList *node;
    for (node = trans->splits; node; node = node->next)
    {
        Split *s = node->data;

        if (!xaccTransStillHasSplit(trans, s)) continue;
        xaccSplitDetermineGainStatus(s);

        if ((GAINS_STATUS_GAINS & s->gains) &&
            s->gains_split &&
            ((s->gains_split->gains & GAINS_STATUS_DATE_DIRTY) ||
             (s->gains & GAINS_STATUS_DATE_DIRTY)))
        {
            Transaction *source_trans = s->gains_split->parent;
            s->gains &= ~GAINS_STATUS_DATE_DIRTY;
            s->gains_split->gains &= ~GAINS_STATUS_DATE_DIRTY;
            xaccTransSetDatePostedSecs(trans, source_trans->date_posted);
            FOR_EACH_SPLIT(trans, s->gains &= ~GAINS_STATUS_DATE_DIRTY);
        }
    }
}

/* ============================================================== */

void
xaccTransScrubGains (Transaction *trans, Account *gain_acc)
{
    SplitList *node;

    ENTER("(trans=%p)", trans);
    /* Lock down posted date, its to be synced to the posted date
     * for the source of the cap gains. */
    xaccTransScrubGainsDate(trans);

    /* Fix up the split amount */
restart:
    for (node = trans->splits; node; node = node->next)
    {
        Split *s = node->data;

        if (!xaccTransStillHasSplit(trans, s)) continue;

        xaccSplitDetermineGainStatus(s);
        if (s->gains & GAINS_STATUS_ADIRTY)
        {
            gboolean altered = FALSE;
            s->gains &= ~GAINS_STATUS_ADIRTY;
            if (s->lot)
                altered = xaccScrubLot(s->lot);
            else
                altered = xaccSplitAssign(s);
            if (altered) goto restart;
        }
    }

    /* Fix up gains split value */
    FOR_EACH_SPLIT(trans,
                   if ((s->gains & GAINS_STATUS_VDIRTY) ||
                       (s->gains_split &&
                        (s->gains_split->gains & GAINS_STATUS_VDIRTY)))
                       xaccSplitComputeCapGains(s, gain_acc);
        );

    LEAVE("(trans=%p)", trans);
}

Split *
xaccTransFindSplitByAccount(const Transaction *trans, const Account *acc)
{
    if (!trans || !acc) return NULL;
    FOR_EACH_SPLIT(trans, if (xaccSplitGetAccount(s) == acc) return s);
    return NULL;
}

static void
record_price (Split *split,
              PriceSource source)
{
    Transaction *trans;
    Account *account;
    QofBook* book;
    GNCPriceDB* pricedb;
    gnc_commodity* comm;
    gnc_commodity* curr;
    GNCPrice* price;
    gnc_numeric price_value, value, amount;
    int scu;
    time64 time;
    gboolean swap;

    account = xaccSplitGetAccount (split);
    if (!xaccAccountIsPriced (account))
    {
       return;
    }
    amount = xaccSplitGetAmount (split);
    if (gnc_numeric_zero_p (amount))
    {
       return;
    }
    trans = xaccSplitGetParent (split);
    value = gnc_numeric_div (xaccSplitGetValue (split), amount,
                             GNC_DENOM_AUTO,
                             GNC_HOW_DENOM_EXACT);
    book = qof_instance_get_book (QOF_INSTANCE (account));
    pricedb = gnc_pricedb_get_db (book);
    comm = xaccAccountGetCommodity (account);
    curr = xaccTransGetCurrency (trans);
    scu = gnc_commodity_get_fraction (curr);
    swap = FALSE;
    time = xaccTransGetDate (trans);
    price = gnc_pricedb_lookup_day_t64 (pricedb, comm, curr, time);
    if (gnc_commodity_equiv (comm, gnc_price_get_currency (price)))
        swap = TRUE;

    if (price)
    {
        PriceSource oldsource = gnc_price_get_source (price);
        price_value = gnc_price_get_value (price);
        if (gnc_numeric_equal (swap ? gnc_numeric_invert (value) : value,
                               price_value))
        {
            gnc_price_unref (price);
            return;
        }
        if (oldsource < source &&
            !(oldsource == PRICE_SOURCE_XFER_DLG_VAL &&
             source == PRICE_SOURCE_SPLIT_REG))
        {
            /* Existing price is preferred over this one. */
            gnc_price_unref (price);
            return;
        }
        if (swap)
        {
            value = gnc_numeric_invert (value);
            scu = gnc_commodity_get_fraction (comm);
        }
        value = gnc_numeric_convert (value, scu * COMMODITY_DENOM_MULT,
                                     GNC_HOW_RND_ROUND_HALF_UP);
        gnc_price_begin_edit (price);
        gnc_price_set_time64 (price, time);
        gnc_price_set_source (price, source);
        gnc_price_set_typestr (price, PRICE_TYPE_TRN);
        gnc_price_set_value (price, value);
        gnc_price_commit_edit (price);
        gnc_price_unref (price);
        return;
    }

    value = gnc_numeric_convert (value, scu * COMMODITY_DENOM_MULT,
                                 GNC_HOW_RND_ROUND_HALF_UP);
    price = gnc_price_create (book);
    gnc_price_begin_edit (price);
    gnc_price_set_commodity (price, comm);
    gnc_price_set_currency (price, curr);
    gnc_price_set_time64 (price, time);
    gnc_price_set_source (price, source);
    gnc_price_set_typestr (price, PRICE_TYPE_TRN);
    gnc_price_set_value (price, value);
    gnc_pricedb_add_price (pricedb, price);
    gnc_price_commit_edit (price);
}

void
xaccTransRecordPrice (Transaction *trans, PriceSource source)
{
   /* XXX: This should have been part of xaccSplitCommitEdit. */
   for (GList *n = xaccTransGetSplitList (trans); n; n = n->next)
      record_price (n->data, source);
}

/********************************************************************\
\********************************************************************/
/* QofObject function implementation */

static void
destroy_tx_on_book_close(QofInstance *ent, gpointer data)
{
    Transaction* tx = GNC_TRANSACTION(ent);

    xaccTransDestroy(tx);
}

/** Handles book end - frees all transactions from the book
 *
 * @param book Book being closed
 */
static void
gnc_transaction_book_end(QofBook* book)
{
    QofCollection *col;

    col = qof_book_get_collection(book, GNC_ID_TRANS);
    qof_collection_foreach(col, destroy_tx_on_book_close, NULL);
}

#ifdef _MSC_VER
/* MSVC compiler doesn't have C99 "designated initializers"
 * so we wrap them in a macro that is empty on MSVC. */
# define DI(x) /* */
#else
# define DI(x) x
#endif

/* Hook into the QofObject registry */
static QofObject trans_object_def =
{
    DI(.interface_version = ) QOF_OBJECT_VERSION,
    DI(.e_type            = ) GNC_ID_TRANS,
    DI(.type_label        = ) "Transaction",
    DI(.create            = ) (gpointer)xaccMallocTransaction,
    DI(.book_begin        = ) NULL,
    DI(.book_end          = ) gnc_transaction_book_end,
    DI(.is_dirty          = ) qof_collection_is_dirty,
    DI(.mark_clean        = ) qof_collection_mark_clean,
    DI(.foreach           = ) qof_collection_foreach,
    DI(.printable         = ) (const char * (*)(gpointer)) xaccTransGetDescription,
    DI(.version_cmp       = ) (int (*)(gpointer, gpointer)) qof_instance_version_cmp,
};

static gboolean
trans_is_balanced_p (const Transaction *trans)
{
    return trans ? xaccTransIsBalanced(trans) : FALSE;
}

gboolean xaccTransRegister (void)
{
    static QofParam params[] =
        {
            {
                TRANS_NUM, QOF_TYPE_STRING,
                (QofAccessFunc)xaccTransGetNum,
                (QofSetterFunc)qofTransSetNum,
                qof_string_number_compare_func
            },
            {
                TRANS_DESCRIPTION, QOF_TYPE_STRING,
                (QofAccessFunc)xaccTransGetDescription,
                (QofSetterFunc)qofTransSetDescription
            },
            {
                TRANS_DATE_ENTERED, QOF_TYPE_DATE,
                (QofAccessFunc)xaccTransRetDateEntered,
                (QofSetterFunc)xaccTransSetDateEnteredSecs
            },
            {
                TRANS_DATE_POSTED, QOF_TYPE_DATE,
                (QofAccessFunc)xaccTransRetDatePosted,
                (QofSetterFunc)xaccTransSetDatePostedSecs
            },
            {
                TRANS_DATE_DUE, QOF_TYPE_DATE,
                (QofAccessFunc)xaccTransRetDateDue, NULL
            },
            {
                TRANS_IMBALANCE, QOF_TYPE_NUMERIC,
                (QofAccessFunc)xaccTransGetImbalanceValue, NULL
            },
            {
                TRANS_NOTES, QOF_TYPE_STRING,
                (QofAccessFunc)xaccTransGetNotes,
                (QofSetterFunc)qofTransSetNotes
            },
            {
                TRANS_DOCLINK, QOF_TYPE_STRING,
                (QofAccessFunc)xaccTransGetDocLink,
                (QofSetterFunc)xaccTransSetDocLink
            },
            {
                TRANS_IS_CLOSING, QOF_TYPE_BOOLEAN,
                (QofAccessFunc)xaccTransGetIsClosingTxn, NULL
            },
            {
                TRANS_IS_BALANCED, QOF_TYPE_BOOLEAN,
                (QofAccessFunc)trans_is_balanced_p, NULL
            },
            {
                TRANS_TYPE, QOF_TYPE_CHAR,
                (QofAccessFunc)xaccTransGetTxnType,
                (QofSetterFunc)xaccTransSetTxnType
            },
            {
                TRANS_VOID_STATUS, QOF_TYPE_BOOLEAN,
                (QofAccessFunc)xaccTransGetVoidStatus, NULL
            },
            {
                TRANS_VOID_REASON, QOF_TYPE_STRING,
                (QofAccessFunc)xaccTransGetVoidReason, NULL
            },
            {
                TRANS_VOID_TIME, QOF_TYPE_DATE,
                (QofAccessFunc)xaccTransGetVoidTime, NULL
            },
            {
                TRANS_SPLITLIST, GNC_ID_SPLIT,
                (QofAccessFunc)xaccTransGetSplitList, NULL
            },
            {
                QOF_PARAM_BOOK, QOF_ID_BOOK,
                (QofAccessFunc)qof_instance_get_book, NULL
            },
            {
                QOF_PARAM_GUID, QOF_TYPE_GUID,
                (QofAccessFunc)qof_entity_get_guid, NULL
            },
            { NULL },
        };

    qof_class_register (GNC_ID_TRANS, (QofSortFunc)xaccTransOrder, params);

    return qof_object_register (&trans_object_def);
}

TransTestFunctions*
_utest_trans_fill_functions (void)
{
    TransTestFunctions *func = g_new (TransTestFunctions, 1);

    func->mark_trans = mark_trans;
    func->gen_event_trans = gen_event_trans;
    func->xaccFreeTransaction = xaccFreeTransaction;
    func->destroy_gains = destroy_gains;
    func->do_destroy = do_destroy;
    func->was_trans_emptied = was_trans_emptied;
    func->trans_on_error = trans_on_error;
    func->trans_cleanup_commit = trans_cleanup_commit;
    func->xaccTransScrubGainsDate = xaccTransScrubGainsDate;
    func->dupe_trans = dupe_trans;
    return func;
}

/************************ END OF ************************************\
\************************* FILE *************************************/
