/********************************************************************
 * utest-Transaction.c: GLib g_test test suite for Transaction.c.   *
 * Copyright 2012 John Ralls <jralls@ceridwen.us>		    *
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
 * along with this program; if not, you can retrieve it from        *
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html            *
 * or contact:                                                      *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 ********************************************************************/
#include <config.h>
#include <string.h>
#include <glib.h>
#include <unittest-support.h>
/* Add specific headers for this class */
#include "../Transaction.h"
#include "../TransactionP.h"
#include "../Split.h"
#include "../Account.h"
#include "../gnc-lot.h"
#include "../gnc-event.h"
#include <qof.h>
#include <qofbackend-p.h>

#ifdef HAVE_GLIB_2_38
#define _Q "'"
#else
#define _Q "`"
#endif

static const gchar *suitename = "/engine/Transaction";
void test_suite_transaction ( void );

/* Copied from Transaction.c. Changing these values will break
 * existing databases, which is a good reason to fail a test.
 */
static const char *trans_notes_str = "notes";
static const char *void_reason_str = "void-reason";
static const char *void_time_str = "void-time";
static const char *void_former_notes_str = "void-former-notes";
const char *trans_is_closing_str = "book_closing";
#define TRANS_DATE_DUE_KVP       "trans-date-due"
#define TRANS_TXN_TYPE_KVP       "trans-txn-type"
#define TRANS_READ_ONLY_REASON   "trans-read-only"
#define TRANS_REVERSED_BY        "reversed-by"

#define ISO_DATELENGTH 32 /* length of an iso 8601 date string. */


typedef struct
{
    Transaction *txn;
    Account *acc1;
    Account *acc2;
    gnc_commodity *curr;
    gnc_commodity *comm;
    TransTestFunctions *func;
    GSList *hdlrs;
} Fixture;

typedef struct
{
    Fixture base;
    Transaction *gains_txn;
    Account *gains_acc;
} GainsFixture;

typedef struct
{
    QofBackend be;
    gchar last_call[12];
    QofBackendError result_err;
} MockBackend;

static void
mock_backend_set_error (MockBackend *mbe, QofBackendError err)
{
    mbe->result_err = err;
}

static void
mock_backend_rollback (QofBackend *be, QofInstance *foo)
{
    MockBackend *mbe = (MockBackend *)be;
    g_strlcpy (mbe->last_call, "rollback", sizeof (mbe->last_call));
    mbe->be.last_err = mbe->result_err;
}

static MockBackend*
mock_backend_new (void)
{
    MockBackend *mbe = g_new0 (MockBackend, 1);
    mbe->be.rollback = mock_backend_rollback;
    memset (mbe->last_call, 0, sizeof (mbe->last_call));
    return mbe;
}

static void
setup (Fixture *fixture, gconstpointer pData)
{
    QofBook *book = qof_book_new ();
    MockBackend *mbe = mock_backend_new ();
    Split *split1 = NULL, *split2 = NULL;
    Transaction *txn;
    Timespec entered = gnc_dmy2timespec (20, 4, 2012);
    Timespec posted = gnc_dmy2timespec (21, 4, 2012);
    KvpFrame *frame = kvp_frame_new ();

    qof_book_set_backend (book, (QofBackend*)mbe);
    split1 = xaccMallocSplit (book);
    split2 = xaccMallocSplit (book);
    txn = xaccMallocTransaction (book);
    fixture->txn = txn;
    fixture->curr = gnc_commodity_new (book, "Gnu Rand", "CURRENCY", "GNR", "", 240);
    fixture->comm = gnc_commodity_new (book, "Wildebeest Fund", "FUND", "WBFXX", "", 1000);
    fixture->acc1 = xaccMallocAccount (book);
    fixture->acc2 = xaccMallocAccount (book);
    xaccAccountSetCommodity (fixture->acc1, fixture->comm);
    xaccAccountSetCommodity (fixture->acc2, fixture->curr);
    txn->date_posted.tv_sec = posted.tv_sec;
    txn->date_posted.tv_nsec = posted.tv_nsec;
    txn->date_entered.tv_sec = entered.tv_sec;
    txn->date_entered.tv_nsec = entered.tv_nsec;
    split1->memo = CACHE_INSERT ("foo");
    split1->action = CACHE_INSERT ("bar");
    split1->amount = gnc_numeric_create (100000, 1000);
    split1->value = gnc_numeric_create (3200, 240);
    split2->amount = gnc_numeric_create (-3200, 240);
    split2->value = gnc_numeric_create (-3200, 240);
    split1->acc = fixture->acc1;
    split2->acc = fixture->acc2;
    txn->num = CACHE_INSERT ("123");
    txn->description = CACHE_INSERT ("Waldo Pepper");
    xaccTransBeginEdit (txn);
    {
        xaccTransSetCurrency (txn, fixture->curr);
        xaccSplitSetParent (split1, txn);
        xaccSplitSetParent (split2, txn);
        kvp_frame_set_string (frame, trans_notes_str, "Salt pork sausage");
        kvp_frame_set_double (frame, "/qux/quux/corge", 123.456);
        qof_instance_set_slots (QOF_INSTANCE (txn), frame);
    }
    xaccTransCommitEdit (txn);
    xaccAccountSortSplits(fixture->acc1, FALSE);
    xaccAccountSortSplits(fixture->acc2, FALSE);
    xaccAccountRecomputeBalance(fixture->acc1);
    xaccAccountRecomputeBalance(fixture->acc2);
    qof_instance_mark_clean (QOF_INSTANCE (split1));
    qof_instance_mark_clean (QOF_INSTANCE (split2));
    qof_instance_mark_clean (QOF_INSTANCE (txn));
    fixture->func = _utest_trans_fill_functions();
    fixture->hdlrs = NULL;
}

static void
setup_with_gains (GainsFixture *fixture, gconstpointer pData)
{
    QofBook *book;
    Fixture *base = &(fixture->base);
    Split *gains_split1 = NULL, *gains_split2 = NULL;
    Split *base_split = NULL;

    setup (base, NULL);

    book = qof_instance_get_book (QOF_INSTANCE (base->txn));
    fixture->gains_txn = xaccMallocTransaction (book);
    fixture->gains_acc = xaccMallocAccount (book);
    xaccAccountSetCommodity (fixture->gains_acc, base->curr);
    gains_split1 = xaccMallocSplit (book);
    gains_split2 = xaccMallocSplit (book);
    gains_split1->acc = base->acc1;
    gains_split2->acc = fixture->gains_acc;
    gains_split1->amount = gnc_numeric_create (30, 240);
    gains_split1->value = gnc_numeric_create (30, 240);
    gains_split2->amount = gnc_numeric_create (-30, 240);
    gains_split2->value = gnc_numeric_create (-30, 240);
    xaccTransBeginEdit (fixture->gains_txn);
    {
        xaccTransSetCurrency (fixture->gains_txn, base->curr);
        xaccSplitSetParent (gains_split1, fixture->gains_txn);
        xaccSplitSetParent (gains_split2, fixture->gains_txn);
    }
    xaccTransCommitEdit (fixture->gains_txn);
    base_split = g_list_nth_data (base->txn->splits, 1);
    base_split->gains_split = gains_split1;
}


/* Add a log handler to the handlers list to be cleared at teardown */


static void
teardown (Fixture *fixture, gconstpointer pData)
{
    QofBook *book = qof_instance_get_book (QOF_INSTANCE (fixture->txn));
    MockBackend *mbe = (MockBackend *)qof_book_get_backend (book);

    test_destroy (fixture->txn);
    test_destroy (fixture->acc1);
    test_destroy (fixture->acc2);
    test_destroy (fixture->curr);
    test_destroy (fixture->comm);
    g_free (mbe);
    test_destroy (book);
    g_slist_free_full (fixture->hdlrs, test_free_log_handler);
    test_clear_error_list();
}

static void
teardown_with_gains (GainsFixture *fixture, gconstpointer pData)
{
    Fixture *base = &(fixture->base);
    test_destroy (fixture->gains_acc);
    teardown (base, NULL);
}

/* check_open
void check_open (const Transaction *trans)// Local: 1:0:0
*/
static void
test_check_open (Fixture *fixture, gconstpointer pData)
{
    gchar *msg = g_strdup_printf ("[check_open()] transaction %p not open for editing", fixture->txn);
    GLogLevelFlags loglevel = G_LOG_LEVEL_CRITICAL | G_LOG_FLAG_FATAL;
    TestErrorStruct *check = test_error_struct_new ("gnc.engine", loglevel,
                             msg);
    g_free (msg);
    fixture->hdlrs = test_log_set_fatal_handler (fixture->hdlrs, check,
                     (GLogFunc)test_checked_handler);
    check_open (fixture->txn);
    g_assert_cmpint (check->hits, ==, 1);
    xaccTransBeginEdit (fixture->txn);
    check_open (fixture->txn);
    g_assert_cmpint (check->hits, ==, 1);
    /* Don't commit the edit, there's nothing to balance! */
    xaccTransRollbackEdit (fixture->txn);
}
/* xaccTransStillHasSplit
gboolean
xaccTransStillHasSplit(const Transaction *trans, const Split *s)// C: 8 in 3  Local: 7:0:0
*/
static void
test_xaccTransStillHasSplit (Fixture *fixture, gconstpointer pData)
{
    QofBook *book = qof_instance_get_book (QOF_INSTANCE (fixture->txn));
    Split *split = xaccMallocSplit (book);
    g_assert (!xaccTransStillHasSplit (fixture->txn, split));
    xaccSplitSetParent (split, fixture->txn);
    g_assert (xaccTransStillHasSplit (fixture->txn, split));
    qof_instance_set_destroying (split, TRUE);
    g_assert (!xaccTransStillHasSplit (fixture->txn, split));
    qof_instance_set_destroying (split, FALSE);
    g_assert (xaccTransStillHasSplit (fixture->txn, split));
    xaccSplitSetParent (split, NULL);
    g_assert (!xaccTransStillHasSplit (fixture->txn, split));

    test_destroy (split);
}
// Make Static
/* mark_trans
void mark_trans (Transaction *trans)// Local: 3:0:0
*/
#define check_split_dirty(xsplit, test)                \
{                                                      \
    gboolean sort_dirty, balance_dirty;                \
    Split *split = xsplit;                             \
    g_object_get (split->acc,                          \
		  "sort-dirty", &sort_dirty,           \
		  "balance-dirty", &balance_dirty,     \
		  NULL);                               \
    g_assert_cmpint (sort_dirty, ==, test);            \
    g_assert_cmpint (balance_dirty, ==, test);         \
}

static void
test_mark_trans (Fixture *fixture, gconstpointer pData)
{
    gboolean dirty_split = FALSE;
    GList *splits = NULL;

    for (splits = (fixture->txn)->splits; splits; splits = splits->next)
    {
        if (!splits->data) continue;
        g_assert (!qof_instance_get_dirty_flag (splits->data));
        check_split_dirty (splits->data, FALSE);
    }
    fixture->func->mark_trans (fixture->txn);
    g_assert (!qof_instance_get_dirty_flag (fixture->txn));
    for (splits = (fixture->txn)->splits; splits; splits = splits->next)
    {
        if (!splits->data) continue;
        g_assert (!qof_instance_get_dirty_flag (splits->data));
        check_split_dirty (splits->data, TRUE);
    }
}
/* gen_event_trans
void gen_event_trans (Transaction *trans)// Local: 2:0:0
*/
static void
test_gen_event_trans (Fixture *fixture, gconstpointer pData)
{
    Split *split = fixture->txn->splits->data;
    GNCLot *lot = gnc_lot_new (qof_instance_get_book (QOF_INSTANCE (fixture->txn)));
    TestSignal sig1 = test_signal_new (QOF_INSTANCE (fixture->acc1),
                                       GNC_EVENT_ITEM_CHANGED, split);
    TestSignal sig2 = test_signal_new (QOF_INSTANCE (lot),
                                       QOF_EVENT_MODIFY, NULL);
    gnc_lot_add_split (lot, split);
    test_signal_assert_hits (sig1, 1);
    test_signal_assert_hits (sig2, 3);
    fixture->func->gen_event_trans (fixture->txn);
    test_signal_assert_hits (sig1, 2);
    test_signal_assert_hits (sig2, 4);

    test_signal_free (sig1);
    test_signal_free (sig2);
    test_destroy (lot);
}
/* gnc_transaction_init
G_DEFINE_TYPE(Transaction, gnc_transaction, QOF_TYPE_INSTANCE)
static void
gnc_transaction_init(Transaction* trans)*/
static void
test_gnc_transaction_init ()
{
    Transaction *txn = g_object_new (GNC_TYPE_TRANSACTION, NULL);
    g_assert_cmpstr (txn->num, ==, "");
    g_assert_cmpstr (txn->description, ==, "");
    g_assert (txn->common_currency == NULL);
    g_assert (txn->splits == NULL);
    g_assert_cmpint (txn->date_entered.tv_sec, ==, 0);
    g_assert_cmpint (txn->date_entered.tv_nsec, ==, 0);
    g_assert_cmpint (txn->date_posted.tv_sec, ==, 0);
    g_assert_cmpint (txn->date_posted.tv_nsec, ==, 0);
    g_assert_cmpint (txn->marker, ==, 0);
    g_assert (txn->orig == NULL);

    test_destroy (txn);
}
/* gnc_transaction_dispose
static void
gnc_transaction_dispose(GObject *txnp)*/
static void
test_gnc_transaction_dispose ()
{
    QofBook *book = qof_book_new ();
    Transaction *txn = g_object_new (GNC_TYPE_TRANSACTION, "book", book, NULL);
    Split *split = g_object_new (GNC_TYPE_SPLIT, "book", book, NULL);
    Split *s_ref = split;
    gnc_commodity *curr = gnc_commodity_new (book, "Gnu Rand", "CURRENCY",
                          "GNR", "", 240), *t_curr = NULL;
    gnc_commodity *c_ref = curr;
    g_object_add_weak_pointer (G_OBJECT (split), (gpointer*) &s_ref);
    g_object_add_weak_pointer (G_OBJECT (curr), (gpointer*) &c_ref);
    txn->splits = g_list_append (txn->splits, split);
    txn->common_currency = curr;

    g_assert (txn->splits != NULL);
    g_assert (s_ref != NULL);
    g_assert (c_ref != NULL);

    g_object_run_dispose (G_OBJECT (txn));
    /* If gnc_transaction_dispose was written correctly, txn->splits and
     * txn->curr would be null and all of the splits would be destroyed,
     * so all of these would be equal instead of unequal.
     */
    g_assert (txn->splits != NULL);
    g_assert (txn->common_currency != NULL);
    g_assert (s_ref != NULL);
    g_assert (c_ref != NULL);
    /* And these would be unnecessary -- in fact, they would assert */
    test_destroy (split);
    test_destroy (curr);

    test_destroy (txn);
    test_destroy (book);
}
/* gnc_transaction_finalize
static void
gnc_transaction_finalize(GObject* txnp)*/
static void
test_gnc_transaction_finalize ()
{
    Transaction *txn = g_object_new (GNC_TYPE_TRANSACTION, NULL);
    test_destroy (txn);
}
/* gnc_transaction_get_property
 * gnc_transaction_set_property
static void
gnc_transaction_set_property(GObject* object,*/
static void
test_gnc_transaction_set_get_property (Fixture *fixture, gconstpointer pData)
{
    QofBook *book = qof_book_new ();
    Transaction *txn = g_object_new (GNC_TYPE_TRANSACTION, "book", book, NULL);
    gchar *num = "42", *desc = "The Answer", *t_num = NULL, *t_desc = NULL, *phony = NULL;
    gnc_commodity *curr = gnc_commodity_new (book, "Gnu Rand", "CURRENCY",
                          "GNR", "", 240), *t_curr = NULL;
    Timespec now = timespec_now (), *t_entered = NULL, *t_posted = NULL;
    time_t secs = (time_t)now.tv_sec;
    gchar *msg1 = "g_object_set_valist: object class " _Q "Transaction' has no property named " _Q "bogus'";
    gchar *msg2 = g_strdup_printf ("[xaccTransSetDateInternal] addr=%p set date to %" G_GUINT64_FORMAT ".%09ld %s",
                                   txn, now.tv_sec, now.tv_nsec, ctime (&secs));
    GLogLevelFlags loglevel1 = G_LOG_LEVEL_WARNING | G_LOG_FLAG_FATAL;
    GLogLevelFlags loglevel2 = G_LOG_LEVEL_INFO;
    TestErrorStruct *check1 = test_error_struct_new ("GLib-GObject",
                              loglevel1, msg1);
    TestErrorStruct *check2 = test_error_struct_new ("gnc.engine",
                              loglevel2, msg2);
    g_free (msg2);
    fixture->hdlrs = test_log_set_fatal_handler (fixture->hdlrs, check1,
                     (GLogFunc)test_checked_handler);
    fixture->hdlrs = test_log_set_handler (fixture->hdlrs, check2,
                                           (GLogFunc)test_checked_handler);
    g_assert_cmpstr (txn->num, ==, "");
    g_assert_cmpstr (txn->description, ==, "");
    g_assert (txn->common_currency == NULL);
    g_assert_cmpint (txn->date_entered.tv_sec, ==, 0);
    g_assert_cmpint (txn->date_entered.tv_nsec, ==, 0);
    g_assert_cmpint (txn->date_posted.tv_sec, ==, 0);
    g_assert_cmpint (txn->date_posted.tv_nsec, ==, 0);
    /* Kick up the edit counter to keep from committing */
    xaccTransBeginEdit (txn);
    g_object_set (G_OBJECT (txn),
                  "num", num,
                  "description", desc,
                  "currency", curr,
                  "post-date", &now,
                  "enter-date", &now,
                  "bogus", phony,
                  NULL);

    g_assert_cmpstr (txn->num, ==, num);
    g_assert_cmpstr (txn->description, ==, desc);
    g_assert (txn->common_currency == curr);
    g_assert (timespec_equal (&(txn->date_entered), &now));
    g_assert (timespec_equal (&(txn->date_posted), &now));
    g_assert_cmpint (check1->hits, ==, 1);
    g_assert_cmpint (check2->hits, ==, 2);

    g_free (check1->msg);
    check1->msg = g_strdup ("g_object_get_valist: object class " _Q "Transaction' has no property named " _Q "bogus'");
    g_object_get (G_OBJECT (txn),
                  "num", &t_num,
                  "description", &t_desc,
                  "currency", &t_curr,
                  "post-date", &t_posted,
                  "enter-date", &t_entered,
                  "bogus", &phony,
                  NULL);

    g_assert_cmpstr (t_num, ==, num);
    g_assert_cmpstr (t_desc, ==, desc);
    g_assert (t_curr == curr);
    g_assert (timespec_equal (t_entered, &now));
    g_assert (timespec_equal (t_posted, &now));
    g_assert_cmpint (check1->hits, ==, 2);
    g_assert_cmpint (check2->hits, ==, 2);
    xaccTransRollbackEdit (txn);
    test_destroy (txn);
    test_destroy (curr);
    test_destroy (book);
    g_free (t_entered);
}
/* gnc_transaction_class_init
 * xaccInitTransaction
No way to really test class_init directly -- though the above tests cover everything pretty well indirectly. xaccInitTransaction is a useless one-line function that sets the book in the parent QofInstance.
 */
static void
test_xaccMallocTransaction (Fixture *fixture, gconstpointer pData)
{
    QofBook *book = qof_book_new ();
    TestSignal sig1 = test_signal_new (NULL, QOF_EVENT_CREATE,NULL);
    Transaction *txn;
#ifdef __clang__
#define _func "Transaction *xaccMallocTransaction(QofBook *)"
#else
#define _func "xaccMallocTransaction"
#endif
    gchar *msg = _func ": assertion " _Q "book' failed";
#undef _func
    gchar *logdomain = "gnc.engine";
    guint loglevel = G_LOG_LEVEL_CRITICAL | G_LOG_FLAG_FATAL;
    TestErrorStruct *check = test_error_struct_new ("gnc.engine", loglevel,
                             msg);
    fixture->hdlrs = test_log_set_fatal_handler (fixture->hdlrs, check,
                     (GLogFunc)test_checked_handler);
    test_signal_assert_hits (sig1, 0);
    txn = xaccMallocTransaction (NULL);
    g_assert (txn == NULL);
    g_assert_cmpint (check->hits, ==, 1);
    test_signal_assert_hits (sig1, 0);

    txn = xaccMallocTransaction (book);
    g_assert (txn != NULL);
    g_assert_cmpint (check->hits, ==, 1);
    test_signal_assert_hits (sig1, 1);

    test_destroy (txn);
    test_destroy (book);
    test_signal_free (sig1);
}
/* xaccTransSortSplits
void
xaccTransSortSplits (Transaction *trans)// Local: 1:0:0
*/
static void
test_xaccTransSortSplits (Fixture *fixture, gconstpointer pData)
{
    Transaction *txn = fixture->txn;
    QofBook *book = qof_instance_get_book (QOF_INSTANCE (txn));
    Split *split1 = txn->splits->data;
    Split *split2 = txn->splits->next->data;
    Split *split[3];
    guint i;
    GList *node;
    gnc_numeric values[3];

    values[0] = gnc_numeric_create (100, 240);
    values[1] = gnc_numeric_create (75, 240);
    values[2] = gnc_numeric_create (-125, 240);
    /* Prevent xaccTransCommitEdit in xaccSplitSetParent from doing anything */
    xaccTransBeginEdit (txn);
    for (i = 0; i < G_N_ELEMENTS (split); i++)
    {
        split[i] = xaccMallocSplit (book);
        split[i]->value = values[i];
        split[i]->acc = fixture->acc1;
        xaccSplitSetParent (split[i], txn);
    }

    node = txn->splits;
    g_assert (node->data == split1);
    node = g_list_next (node);
    g_assert (node->data == split2);
    node = g_list_next (node);
    g_assert (node->data == split[0]);
    node = g_list_next (node);
    g_assert (node->data == split[1]);
    node = g_list_next (node);
    g_assert (node->data == split[2]);

    xaccTransSortSplits (txn);

    node = txn->splits;
    g_assert (node->data == split1);
    node = g_list_next (node);
    g_assert (node->data == split[0]);
    node = g_list_next (node);
    g_assert (node->data == split[1]);
    node = g_list_next (node);
    g_assert (node->data == split2);
    node = g_list_next (node);
    g_assert (node->data == split[2]);

    xaccTransCommitEdit (txn);
}
/* dupe_trans
static Transaction *
dupe_trans (const Transaction *from)// Local: 1:0:0
*/
static void
test_dupe_trans (Fixture *fixture, gconstpointer pData)
{
    Timespec posted = gnc_dmy2timespec (12, 7, 2011);
    Timespec entered = gnc_dmy2timespec (14, 7, 2011);
    Transaction *newtxn = NULL, *oldtxn = fixture->txn;
    QofBook *old_book = qof_instance_get_book (QOF_INSTANCE (oldtxn));
    GList *newnode, *oldnode = oldtxn->splits;

    oldtxn->date_posted = posted;
    oldtxn->date_entered = entered;
    kvp_frame_set_string (oldtxn->inst.kvp_data, "/foo/bar/baz",
                          "The Great Waldo Pepper");

    newtxn = fixture->func->dupe_trans (oldtxn);

    g_assert_cmpstr (newtxn->num, ==, oldtxn->num);
    g_assert_cmpstr (newtxn->description, ==, oldtxn->description);
    for (newnode = newtxn->splits; newnode && oldnode;
            newnode = g_list_next (newnode))
    {
        g_assert (xaccSplitEqual (newnode->data, oldnode->data,
                                  TRUE, FALSE, TRUE));
        oldnode = g_list_next (oldnode);
    }
    g_assert (newnode == NULL);
    g_assert (oldnode == NULL);
    g_assert (timespec_equal (&(newtxn->date_posted), &posted));
    g_assert (timespec_equal (&(newtxn->date_entered), &entered));
    g_assert (qof_instance_version_cmp (QOF_INSTANCE (newtxn),
                                        QOF_INSTANCE (oldtxn)) == 0);
    g_assert (newtxn->orig == NULL);
    g_assert (newtxn->common_currency == fixture->curr);
    g_assert (newtxn->inst.e_type == NULL);
    g_assert (guid_equal (qof_instance_get_guid (QOF_INSTANCE (newtxn)),
                          guid_null ()));
    g_assert (qof_instance_get_book (QOF_INSTANCE (newtxn)) == old_book);
    g_assert (kvp_frame_compare (oldtxn->inst.kvp_data, newtxn->inst.kvp_data) == 0);

    test_destroy (newtxn);
}
/* xaccTransClone
Transaction *
xaccTransClone (const Transaction *from)// C: 1  Local: 1:0:0
*/
static void
test_xaccTransClone (Fixture *fixture, gconstpointer pData)
{
    Timespec posted = gnc_dmy2timespec (12, 7, 2011);
    Timespec entered = gnc_dmy2timespec (14, 7, 2011);
    Transaction *newtxn = NULL, *oldtxn = fixture->txn;
    QofBook *old_book = qof_instance_get_book (QOF_INSTANCE (oldtxn));
    GList *newnode, *oldnode;
    int foo, bar;

    oldtxn->date_posted = posted;
    oldtxn->date_entered = entered;
    newtxn = xaccTransClone (oldtxn);

    g_assert_cmpstr (newtxn->num, ==, oldtxn->num);
    g_assert_cmpstr (newtxn->description, ==, oldtxn->description);

    g_assert_cmpint (xaccTransCountSplits (oldtxn), ==,
                     xaccTransCountSplits (newtxn));

    xaccTransSortSplits (newtxn);
    xaccTransSortSplits (oldtxn);

    oldnode = oldtxn->splits;
    for (newnode = newtxn->splits; newnode && oldnode;
            newnode = g_list_next (newnode))
    {
        g_assert (xaccSplitEqual (newnode->data, oldnode->data,
                                  FALSE, FALSE, FALSE));
        oldnode = g_list_next (oldnode);
    }
    g_assert (newnode == NULL);
    g_assert (oldnode == NULL);
    g_assert (timespec_equal (&(newtxn->date_posted), &posted));
    g_assert (timespec_equal (&(newtxn->date_entered), &entered));
    g_assert (qof_instance_version_cmp (QOF_INSTANCE (newtxn),
                                        QOF_INSTANCE (oldtxn)) == 0);
    g_assert_cmpint (qof_instance_get_version_check (newtxn), ==,
                     qof_instance_get_version_check (oldtxn));
    g_assert (newtxn->orig == NULL);
    g_assert (newtxn->common_currency == fixture->curr);

    g_assert (qof_instance_get_book (QOF_INSTANCE (newtxn)) == old_book);
    g_assert (kvp_frame_compare (oldtxn->inst.kvp_data, newtxn->inst.kvp_data) == 0);

    test_destroy (newtxn);
}

/* xaccTransCopyOnto
void
xaccTransCopyOnto (const Transaction *from_trans, Transaction *to_trans)//Register 2
convenience function for xaccTransCopyFromClipboard (from_trans, to_trans, NULL, NULL, TRUE)
*/
/* xaccTransCopyFromClipboard
void
xaccTransCopyFromClipboard (const Transaction *from_trans,
			    Transaction *to_trans,
			    const Account *from_acc,
			    Account *to_acc, gboolean no_date) // Register 2
*/
static void
test_xaccTransCopyFromClipBoard (Fixture *fixture, gconstpointer pData)
{
    Transaction *txn = fixture->txn;
    QofBook *book = qof_instance_get_book (QOF_INSTANCE (txn));
    Account *acc1 = xaccMallocAccount (book);
    Transaction *to_txn = xaccMallocTransaction (book);
    Timespec now = timespec_now();
    Timespec never = {0, 0};
    KvpFrame *to_frame = to_txn->inst.kvp_data;

    xaccAccountSetCommodity (acc1, fixture->comm);
    xaccTransCopyFromClipBoard (txn, to_txn, fixture->acc1, acc1, FALSE);
    g_assert (gnc_commodity_equal (txn->common_currency,
                                   to_txn->common_currency));
    g_assert (timespec_equal (&(to_txn->date_entered), &now));
    g_assert (timespec_equal (&(to_txn->date_posted), &txn->date_posted));
    g_assert_cmpstr (txn->num, ==, to_txn->num);
    /* Notes also tests that KVP is copied */
    g_assert_cmpstr (xaccTransGetNotes (txn), ==, xaccTransGetNotes (to_txn));
    g_assert_cmpstr (xaccTransGetDescription (txn), ==,
                     xaccTransGetDescription (to_txn));
    g_assert_cmpstr (xaccTransGetNotes (txn), ==, xaccTransGetNotes (to_txn));
    g_assert_cmpint (xaccTransCountSplits (txn), ==,
                     xaccTransCountSplits (to_txn));
}

static void
test_xaccTransCopyFromClipBoard_no_start (Fixture *fixture, gconstpointer pData)
{
    Transaction *txn = fixture->txn;
    QofBook *book = qof_instance_get_book (QOF_INSTANCE (txn));
    Account *acc1 = xaccMallocAccount (book);
    Transaction *to_txn = xaccMallocTransaction (book);
    Timespec now = timespec_now();
    Timespec never = {0, 0};

    xaccAccountSetCommodity (acc1, fixture->comm);
    xaccTransCopyFromClipBoard (txn, to_txn, fixture->acc1, acc1, TRUE);
    g_assert (gnc_commodity_equal (txn->common_currency,
                                   to_txn->common_currency));
    g_assert (timespec_equal (&(to_txn->date_entered), &now));
    g_assert (timespec_equal (&(to_txn->date_posted), &never));
    g_assert_cmpstr (to_txn->num, ==, txn->num);
    /* Notes also tests that KVP is copied */
    g_assert_cmpstr (xaccTransGetNotes (txn), ==, xaccTransGetNotes (to_txn));
    g_assert_cmpstr (xaccTransGetDescription (txn), ==,
                     xaccTransGetDescription (to_txn));
    g_assert_cmpstr (xaccTransGetNotes (txn), ==, xaccTransGetNotes (to_txn));
    g_assert_cmpint (xaccTransCountSplits (txn), ==,
                     xaccTransCountSplits (to_txn));
}

/* xaccFreeTransaction
static void
xaccFreeTransaction (Transaction *trans)// Local: 4:0:0
*/
static void
test_xaccFreeTransaction (Fixture *fixture, gconstpointer pData)
{
    Transaction *txn = fixture->txn;
    Transaction *orig = xaccMallocTransaction (qof_instance_get_book (QOF_INSTANCE (txn)));
    Split *split = txn->splits->data;
    gchar *txn_num = "321";
    g_object_add_weak_pointer (G_OBJECT (txn->splits->data), (gpointer)&split);
    /* so the "free" doesn't, leaving the structure for us to test */
    g_object_ref (txn);
    g_object_ref (orig);
    orig->num = CACHE_INSERT (txn_num);
    txn->orig = orig;

    fixture->func->xaccFreeTransaction (txn);

    g_assert (split == NULL);
    g_assert (txn->splits == NULL);
    g_assert_cmpint (GPOINTER_TO_INT(txn->num), ==, 1);
    g_assert (txn->description == NULL);
    g_assert_cmpint (txn->date_entered.tv_sec, ==, 0);
    g_assert_cmpint (txn->date_entered.tv_nsec, ==, 0);
    g_assert_cmpint (txn->date_posted.tv_sec, ==, 0);
    g_assert_cmpint (txn->date_posted.tv_nsec, ==, 0);
    g_assert_cmpint (GPOINTER_TO_INT(orig->num), ==, 1);
    g_assert (txn->orig == NULL);
    test_destroy (orig);

    g_test_log_set_fatal_handler ((GTestLogFatalFunc) test_log_handler, NULL);

}
/* compare_split_guids
static gint
compare_split_guids (gconstpointer a, gconstpointer b)// Local: 0:1:0

Pass-through function, test with TransEqual
*/
/* xaccTransEqual
gboolean
xaccTransEqual(const Transaction *ta, const Transaction *tb,// C: 2 in 2  Local: 0:0:0
*/
#define DATE_BUF_SIZE 100
static void
test_xaccTransEqual (Fixture *fixture, gconstpointer pData)
{

    QofBook *book = qof_instance_get_book (QOF_INSTANCE (fixture->txn));
    QofBook *book2 = qof_book_new ();
    Transaction *txn0 = fixture->txn;
    Transaction *clone = xaccTransClone (txn0);
    Transaction *txn1 = xaccTransClone (txn0);
    const GncGUID *guid_f_txn = qof_instance_get_guid (txn0);
    gchar entered[DATE_BUF_SIZE], posted[DATE_BUF_SIZE];
    gchar *msg1 = "[xaccTransEqual] one is NULL";
    gchar *msg2 = NULL;
    gchar *cleanup_fmt = "[trans_cleanup_commit] get rid of rollback trans=%p";
    gchar split_guid0[GUID_ENCODING_LENGTH + 1];
    gchar split_guid1[GUID_ENCODING_LENGTH + 1];
    gchar *logdomain = "gnc.engine";
    guint loglevel = G_LOG_LEVEL_INFO;
    TestErrorStruct *check = test_error_struct_new (logdomain, loglevel, msg1);
    TestErrorStruct check2 = {loglevel, logdomain, msg2, 0};
    TestErrorStruct check3 = {loglevel, logdomain, "", 0};
    TestErrorStruct *cleanup = test_error_struct_new (logdomain, loglevel, "");
    Split *split0 = xaccTransGetSplit (txn0, 0);
    Split *split1;
    test_add_error (check);
    test_add_error (&check2);
    test_add_error (cleanup);

    fixture->hdlrs = test_log_set_handler (fixture->hdlrs, check,
                                           (GLogFunc)test_list_handler);
    /* Booleans are check_guids, check_splits, check_balances, assume_ordered */
    g_assert (xaccTransEqual (NULL, NULL, TRUE, TRUE, TRUE, TRUE));
    g_assert (!xaccTransEqual (txn0, NULL, TRUE, TRUE, TRUE, TRUE));
    g_assert (!xaccTransEqual (NULL, txn0, TRUE, TRUE, TRUE, TRUE));
    g_assert (xaccTransEqual (txn0, txn0, TRUE, TRUE, TRUE, TRUE));

    qof_instance_set_book (txn1, book2);
    qof_instance_set_guid (txn1, guid_f_txn);
    g_assert_cmpint (check->hits, ==, 2);
    check->hits = 0;

    g_assert_cmpint (xaccTransCountSplits (txn0), ==,
                     xaccTransCountSplits (txn1));

    g_free (check->msg);
    check->msg = g_strdup ("[xaccTransEqual] GUIDs differ");
    g_assert (!xaccTransEqual (clone, txn0, TRUE, TRUE, TRUE, TRUE));
    qof_instance_set_guid (clone, guid_f_txn);
    g_assert (xaccTransEqual (clone, txn0, TRUE, FALSE, TRUE, TRUE));
    g_assert_cmpint (check->hits, ==, 1);
    xaccTransBeginEdit (clone);
    cleanup->msg = g_strdup_printf (cleanup_fmt, clone->orig);
    /* This changes the amount and value of the first split */
    xaccTransSetCurrency (clone, fixture->comm);
    xaccTransCommitEdit (clone);
    g_free (cleanup->msg);
    g_free (check->msg);
    check->msg = g_strdup_printf ("[xaccTransEqual] commodities differ %s vs %s", gnc_commodity_get_unique_name (fixture->comm), gnc_commodity_get_unique_name (fixture->curr));
    g_assert (!xaccTransEqual (clone, txn0, TRUE, FALSE, TRUE, TRUE));
    g_assert_cmpint (check->hits, ==, 2);

    gnc_timespec_to_iso8601_buff (clone->date_posted, posted);
    gnc_timespec_to_iso8601_buff (clone->date_entered, entered);
    xaccTransBeginEdit (clone);
    cleanup->msg = g_strdup_printf (cleanup_fmt, clone->orig);
    /* This puts the value of the first split back, but leaves the amount changed */
    xaccTransSetCurrency (clone, fixture->curr);
    clone->date_posted.tv_sec = txn0->date_entered.tv_sec;
    xaccTransCommitEdit (clone);
    g_free (cleanup->msg);
    g_free (check->msg);
    check->msg = g_strdup_printf ("[xaccTransEqual] date posted differs: '%s' vs '%s'", entered, posted);
    g_assert (!xaccTransEqual (clone, txn0, TRUE, FALSE, TRUE, TRUE));
    g_assert_cmpint (check->hits, ==, 3);

    xaccTransBeginEdit (clone);
    cleanup->msg = g_strdup_printf (cleanup_fmt, clone->orig);
    clone->date_posted.tv_sec = txn0->date_posted.tv_sec;
    clone->date_entered.tv_sec = txn0->date_posted.tv_sec;
    xaccTransCommitEdit (clone);
    g_free (cleanup->msg);
    g_free (check->msg);
    check->msg = g_strdup_printf ("[xaccTransEqual] date entered differs: '%s' vs '%s'", posted, entered);
    g_assert (!xaccTransEqual (clone, txn0, TRUE, FALSE, TRUE, TRUE));
    g_assert_cmpint (check->hits, ==, 4);

    xaccTransBeginEdit (clone);
    cleanup->msg = g_strdup_printf (cleanup_fmt, clone->orig);
    clone->date_entered.tv_sec = txn0->date_entered.tv_sec;
    clone->num = "123";
    xaccTransCommitEdit (clone);
    g_free (cleanup->msg);
    g_free (check->msg);
    check->msg = g_strdup ("[xaccTransEqual] num differs: 123 vs 123");
    g_assert (!xaccTransEqual (clone, txn0, TRUE, FALSE, TRUE, TRUE));
    g_assert_cmpint (check->hits, ==, 5);
    g_assert (xaccTransEqual (txn1, clone, TRUE, FALSE, TRUE, TRUE));
    g_assert_cmpint (check->hits, ==, 5);

    txn1->num = "321";
    g_free (check->msg);
    check->msg = g_strdup ("[xaccTransEqual] num differs: 321 vs 123");
    g_assert (!xaccTransEqual (txn1, txn0, TRUE, FALSE, TRUE, TRUE));
    g_assert_cmpint (check->hits, ==, 6);

    clone->num = CACHE_INSERT("123");
    txn1->num = "123";
    clone->description = "salt pork";
    g_free (check->msg);
    check->msg = g_strdup ("[xaccTransEqual] descriptions differ: salt pork vs Waldo Pepper");
    g_assert (!xaccTransEqual (clone, txn0, TRUE, FALSE, TRUE, TRUE));
    g_assert_cmpint (check->hits, ==, 7);
    g_assert (xaccTransEqual (txn1, txn0, TRUE, FALSE, TRUE, TRUE));
    g_assert_cmpint (check->hits, ==, 7);
    g_assert (!xaccTransEqual (clone, txn1, TRUE, FALSE, TRUE, TRUE));
    g_assert_cmpint (check->hits, ==, 8);

    xaccTransBeginEdit (clone);
    cleanup->msg = g_strdup_printf (cleanup_fmt, clone->orig);
    clone->description = CACHE_INSERT ("Waldo Pepper");
    kvp_frame_set_double (qof_instance_get_slots (QOF_INSTANCE (clone)),
                          "/qux/quux/corge", 654.321);
    xaccTransCommitEdit (clone);
    g_free (cleanup->msg);
    g_free (check->msg);
    check->msg = g_strdup ("[xaccTransEqual] kvp frames differ:\n{\n    notes => KVP_VALUE_STRING(Salt pork sausage),\n    qux => KVP_VALUE_FRAME({\n    quux => KVP_VALUE_FRAME({\n    corge => KVP_VALUE_DOUBLE(654.321),\n}\n),\n}\n),\n}\n\n\nvs\n\n{\n    notes => KVP_VALUE_STRING(Salt pork sausage),\n    qux => KVP_VALUE_FRAME({\n    quux => KVP_VALUE_FRAME({\n    corge => KVP_VALUE_DOUBLE(123.456),\n}\n),\n}\n),\n}\n");

    g_assert (!xaccTransEqual (clone, txn0, TRUE, FALSE, TRUE, TRUE));

    g_assert_cmpint (check->hits, ==, 9);
    xaccTransBeginEdit (clone);
    cleanup->msg = g_strdup_printf (cleanup_fmt, clone->orig);
    clone->description = CACHE_INSERT ("Waldo Pepper");
    kvp_frame_set_double (qof_instance_get_slots (QOF_INSTANCE (clone)),
                          "/qux/quux/corge", 123.456);
    xaccTransCommitEdit (clone);
    g_free (cleanup->msg);
    g_free (check->msg);
    check->msg = g_strdup ("[xaccSplitEqual] GUIDs differ");
    split1 = xaccTransGetSplit (clone, 0);
    guid_to_string_buff (qof_instance_get_guid (split0), split_guid0);
    guid_to_string_buff (qof_instance_get_guid (split1), split_guid1);
    check2.msg = g_strdup_printf (
                     "[xaccTransEqual] splits %s and %s differ", split_guid1, split_guid0);

    g_assert (!xaccTransEqual (clone, txn0, TRUE, TRUE, TRUE, TRUE));
    g_assert (xaccTransEqual (clone, txn0, FALSE, FALSE, FALSE, TRUE));
    g_assert_cmpint (check->hits, ==, 10);
    g_assert_cmpint (check2.hits, ==, 1);

    g_free (check->msg);
    g_free (check2.msg);
    check->msg = g_strdup("[xaccSplitEqual] amounts differ: 13333/1000 vs 100000/1000");
    check2.msg = g_strdup_printf (
                     "[xaccTransEqual] splits %s and %s differ", split_guid0, split_guid0);
    qof_instance_set_guid (split1, qof_instance_get_guid (split0));
    g_assert (!xaccTransEqual (clone, txn0, TRUE, TRUE, TRUE, TRUE));
    g_assert (xaccTransEqual (clone, txn0, TRUE, FALSE, FALSE, TRUE));
    g_assert_cmpint (check->hits, ==, 11);
    g_assert_cmpint (check2.hits, ==, 2);

    qof_instance_set_guid (xaccTransGetSplit (txn1, 0),
                           qof_instance_get_guid (split0));
    qof_instance_set_guid (xaccTransGetSplit (txn1, 1),
                           qof_instance_get_guid (xaccTransGetSplit (txn0, 1)));
    g_free (check->msg);
    {
        Split* split00 = xaccTransGetSplit (txn0, 0);
        Split* split01 = xaccTransGetSplit (txn0, 1);
        Split* split10 = xaccTransGetSplit (txn1, 0);
        Split* split11 = xaccTransGetSplit (txn1, 1);
        gchar *bal00 = gnc_numeric_to_string (split00->balance);
        gchar *bal01 = gnc_numeric_to_string (split01->balance);
        gchar *bal10 = gnc_numeric_to_string (split10->balance);
        gchar *bal11 = gnc_numeric_to_string (split11->balance);
        check->msg = g_strdup_printf("[xaccSplitEqualCheckBal] balances differ: %s vs %s", bal10, bal00);
        check3.msg = g_strdup_printf("[xaccSplitEqualCheckBal] balances differ: %s vs %s", bal11, bal01);

        test_add_error (&check3);
        g_assert (!xaccTransEqual (txn1, txn0, TRUE, TRUE, TRUE, TRUE));
        g_assert (xaccTransEqual (txn1, txn0, TRUE, TRUE, FALSE, TRUE));
        g_assert_cmpint (check->hits, ==, 12);
        g_assert_cmpint (check2.hits, ==, 3);
        g_assert_cmpint (check3.hits, ==, 0);

        split10->balance = split00->balance;
        split11->balance = split01->balance;
        g_assert (xaccTransEqual (txn1, txn0, TRUE, TRUE, TRUE, TRUE));
    }
    g_free (check3.msg);
    g_free (check2.msg);
}
/* xaccTransUseTradingAccounts
xaccTransUseTradingAccounts
Returns true if the transaction should include trading account splits if
it involves more than one commodity.
gboolean xaccTransUseTradingAccounts(const Transaction *trans)// C: 10 in 7  Local: 2:0:0
Pass-through, no need to test.
*/
/* xaccTransLookup
Transaction *
xaccTransLookup (const GncGUID *guid, QofBook *book)// C: 22 in 7  Local: 1:0:0
*/
static void
test_xaccTransLookup (Fixture *fixture, gconstpointer pData)
{
    Transaction *txn = fixture->txn;
    QofInstance *inst = QOF_INSTANCE (txn);
    g_assert (xaccTransLookup (qof_instance_get_guid (inst),
                               qof_instance_get_book (inst)) == txn);
}
/* xaccTransGetImbalanceValue
gnc_numeric
xaccTransGetImbalanceValue (const Transaction * trans)// C: 11 in 5  Local: 1:1:0
*/
static void
test_xaccTransGetImbalanceValue (Fixture *fixture, gconstpointer pData)
{
    QofBook *book = qof_instance_get_book (QOF_INSTANCE (fixture->txn));
    Split *split1 = xaccMallocSplit (book);
    g_assert (gnc_numeric_equal (xaccTransGetImbalanceValue (fixture->txn),
                                 gnc_numeric_zero ()));
    split1->acc = fixture->acc1;
    split1->memo = CACHE_INSERT ("foo");
    split1->action = CACHE_INSERT ("bar");
    split1->amount = gnc_numeric_create (100000, 1000);
    split1->value = gnc_numeric_create (3200, 240);
    xaccTransBeginEdit (fixture->txn);
    xaccSplitSetParent (split1, fixture->txn);

    g_assert (gnc_numeric_equal (xaccTransGetImbalanceValue (fixture->txn),
                                 split1->value));
    xaccTransCommitEdit (fixture->txn);
}
/* xaccTransGetImbalance
MonetaryList *
xaccTransGetImbalance (const Transaction * trans)// C: 15 in 6  Local: 1:0:0
*/
static void
test_xaccTransGetImbalance (Fixture *fixture, gconstpointer pData)
{
    QofBook *book = qof_instance_get_book (QOF_INSTANCE (fixture->txn));
    Split *split1 = xaccMallocSplit (book);
    MonetaryList *mlist;
    g_assert (xaccTransGetImbalance (NULL) == NULL);
    mlist = xaccTransGetImbalance (fixture->txn);
    g_assert_cmpint (g_list_length (mlist), ==, 0);

    split1->acc = fixture->acc1;
    split1->memo = CACHE_INSERT ("foo");
    split1->action = CACHE_INSERT ("bar");
    split1->amount = gnc_numeric_create (100000, 1000);
    split1->value = gnc_numeric_create (3200, 240);
    xaccTransBeginEdit (fixture->txn);
    xaccSplitSetParent (split1, fixture->txn);
    mlist = xaccTransGetImbalance (fixture->txn);
    g_assert_cmpint (g_list_length (mlist), ==, 1);
    xaccTransCommitEdit (fixture->txn);
    gnc_monetary_list_free (mlist);
}

static void
test_xaccTransGetImbalance_trading (Fixture *fixture,
                                    gconstpointer pData)
{
    QofBook *book = qof_instance_get_book (QOF_INSTANCE (fixture->txn));
    Split *split1 = xaccMallocSplit (book);
    Split *split2 = xaccMallocSplit (book);
    Account *acc1 = xaccMallocAccount (book);
    Account *acc2 = xaccMallocAccount (book);
    gnc_numeric value;
    MonetaryList *mlist;
    qof_book_begin_edit (book);
    qof_instance_set (QOF_INSTANCE (book),
		      "trading-accts", "t",
		      NULL);
    qof_book_commit_edit (book);

 /* Without trading splits, the list is unbalanced */
    mlist = xaccTransGetImbalance (fixture->txn);
    g_assert_cmpint (g_list_length (mlist), ==, 2);
    gnc_monetary_list_free (mlist);

    xaccAccountSetCommodity (acc1, fixture->comm);
    xaccAccountSetCommodity (acc2, fixture->curr);
    xaccAccountSetType (acc1, ACCT_TYPE_TRADING);
    xaccAccountSetType (acc2, ACCT_TYPE_TRADING);
    /* The setup transaction is unbalanced in a trading-accounts environment. */
    g_assert (!xaccTransIsBalanced (fixture->txn));
    /* Make it look like a proper trading accounts transactionm */
    split1->acc = acc1;
    split1->memo = CACHE_INSERT ("foo");
    split1->action = CACHE_INSERT ("bar");
    split1->amount = gnc_numeric_create (-10000, 100);
    split1->value = gnc_numeric_create (-3200, 240);
    split2->acc = acc2;
    split2->memo = CACHE_INSERT ("foo");
    split2->action = CACHE_INSERT ("bar");
    split2->amount = gnc_numeric_create (3000, 240);
    split2->value = gnc_numeric_create (3200, 240);
    xaccTransBeginEdit (fixture->txn);
    xaccSplitSetParent (split1, fixture->txn);
    mlist = xaccTransGetImbalance (fixture->txn);
    g_assert_cmpint (g_list_length (mlist), ==, 1);
    gnc_monetary_list_free (mlist);
    xaccSplitSetParent (split2, fixture->txn);
    mlist = xaccTransGetImbalance (fixture->txn);
    g_assert_cmpint (g_list_length (mlist), ==, 1);
    gnc_monetary_list_free (mlist);
    split2->amount = gnc_numeric_create (3000, 240);
    split2->value = gnc_numeric_create (3000, 240);
    mlist = xaccTransGetImbalance (fixture->txn);
    g_assert_cmpint (g_list_length (mlist), ==, 1);
    gnc_monetary_list_free (mlist);
    split2->amount = gnc_numeric_create (3200, 240);
    split2->value = gnc_numeric_create (3200, 240);
    mlist = xaccTransGetImbalance (fixture->txn);
    g_assert_cmpint (g_list_length (mlist), ==, 0);
    gnc_monetary_list_free (mlist);

    xaccTransCommitEdit (fixture->txn);
    test_destroy (acc1);
    test_destroy (acc2);
}


/* xaccTransIsBalanced
gboolean
xaccTransIsBalanced (const Transaction *trans)// C: 4 in 4  Local: 1:0:0
*/
static void
test_xaccTransIsBalanced (Fixture *fixture, gconstpointer pData)
{
    QofBook *book = qof_instance_get_book (QOF_INSTANCE (fixture->txn));
    Split *split1 = xaccMallocSplit (book);
    g_assert (!xaccTransIsBalanced (NULL));
    g_assert (xaccTransIsBalanced (fixture->txn));

    split1->acc = fixture->acc1;
    split1->memo = CACHE_INSERT ("foo");
    split1->action = CACHE_INSERT ("bar");
    split1->amount = gnc_numeric_create (100000, 1000);
    split1->value = gnc_numeric_create (3200, 240);
    xaccTransBeginEdit (fixture->txn);
    xaccSplitSetParent (split1, fixture->txn);
    g_assert (! xaccTransIsBalanced (fixture->txn));
    xaccTransCommitEdit (fixture->txn);
}


static void
test_xaccTransIsBalanced_trading (Fixture *fixture, gconstpointer pData)
{
    QofBook *book = qof_instance_get_book (QOF_INSTANCE (fixture->txn));
    Split *split1 = xaccMallocSplit (book);
    Split *split2 = xaccMallocSplit (book);
    Account *acc1 = xaccMallocAccount (book);
    Account *acc2 = xaccMallocAccount (book);

    qof_book_begin_edit (book);
    qof_instance_set (QOF_INSTANCE (book),
		      "trading-accts", "t",
		      NULL);
    qof_book_commit_edit (book);

    xaccAccountSetCommodity (acc1, fixture->curr);
    xaccAccountSetCommodity (acc2, fixture->comm);
    xaccAccountSetType (acc1, ACCT_TYPE_TRADING);
    xaccAccountSetType (acc2, ACCT_TYPE_TRADING);
    /* The setup transaction is unbalanced in a trading-accounts environment. */
    g_assert (!xaccTransIsBalanced (fixture->txn));
    split1->acc = acc1;
    split1->memo = CACHE_INSERT ("foo");
    split1->action = CACHE_INSERT ("bar");
    split1->amount = gnc_numeric_create (3200, 240);
    split1->value = gnc_numeric_create (3200, 240);
    split2->acc = acc2;
    split2->memo = CACHE_INSERT ("foo");
    split2->action = CACHE_INSERT ("bar");
    split2->amount = gnc_numeric_create (-10000, 100);
    split2->value = gnc_numeric_create (-3000, 240);
    xaccTransBeginEdit (fixture->txn);
    xaccSplitSetParent (split1, fixture->txn);
    g_assert (!xaccTransIsBalanced (fixture->txn));
    xaccSplitSetParent (split2, fixture->txn);
    g_assert (!xaccTransIsBalanced (fixture->txn));
    split2->amount = gnc_numeric_create (-11000, 100);
    split2->value = gnc_numeric_create (-3200, 240);
    g_assert (!xaccTransIsBalanced (fixture->txn));
    split2->amount = gnc_numeric_create (-10000, 100);
    split2->value = gnc_numeric_create (-3200, 240);
    g_assert (xaccTransIsBalanced (fixture->txn));
    xaccTransRollbackEdit (fixture->txn);

    test_destroy (acc2);
    test_destroy (acc1);
}
/* xaccTransGetAccountValue
gnc_numeric
xaccTransGetAccountValue (const Transaction *trans,// SCM: 6 in 6 Local: 0:0:0
*/
static void
test_xaccTransGetAccountValue (Fixture *fixture, gconstpointer pData)
{
    gnc_numeric val1 = {3200, 240}, val2 = {-3200, 240};

    g_assert (gnc_numeric_zero_p (xaccTransGetAccountValue (fixture->txn, NULL)));
    g_assert (gnc_numeric_zero_p (xaccTransGetAccountValue (NULL, fixture->acc1)));
    g_assert (gnc_numeric_eq (xaccTransGetAccountValue (fixture->txn, fixture->acc1), val1));
    g_assert (gnc_numeric_eq (xaccTransGetAccountValue (fixture->txn, fixture->acc2), val2));

}
/* xaccTransGetAccountAmount
gnc_numeric
xaccTransGetAccountAmount (const Transaction *trans, const Account *acc)// C: 2 in 1  Local: 0:0:0
*/
static void
test_xaccTransGetAccountAmount (Fixture *fixture, gconstpointer pData)
{
    gnc_numeric amt1 = {100000, 1000}, amt2 = {-3200, 240};

    g_assert (gnc_numeric_zero_p (xaccTransGetAccountAmount (fixture->txn, NULL)));
    g_assert (gnc_numeric_zero_p (xaccTransGetAccountAmount (NULL, fixture->acc1)));
    g_assert (gnc_numeric_eq (xaccTransGetAccountAmount (fixture->txn, fixture->acc1), amt1));
    g_assert (gnc_numeric_eq (xaccTransGetAccountAmount (fixture->txn, fixture->acc2), amt2));

}
/* xaccTransGetRateForCommodity
gboolean
xaccTransGetRateForCommodity(const Transaction *trans,
                             const gnc_commodity *split_com,
                             const Split *split, gnc_numeric *rate)
*/
static void
test_xaccTransGetRateForCommodity (Fixture *fixture, gconstpointer pData)
{
    gnc_numeric rate = gnc_numeric_zero ();
    QofBook *book = qof_instance_get_book (QOF_INSTANCE (fixture->txn));
    Split *split0 = xaccMallocSplit (book);
    Split *split1 = xaccTransFindSplitByAccount(fixture->txn, fixture->acc1);
    g_assert (!xaccTransGetRateForCommodity (NULL, fixture->comm,
              split0, &rate));
    g_assert (!xaccTransGetRateForCommodity (fixture->txn, NULL,
              split0, &rate));
    g_assert (!xaccTransGetRateForCommodity (fixture->txn, fixture->comm,
              NULL, &rate));
    g_assert (xaccTransGetRateForCommodity (fixture->txn, fixture->curr,
                                            split0, &rate));
    g_assert (gnc_numeric_equal (rate, gnc_numeric_create (1, 1)));
    rate = gnc_numeric_zero ();
    g_assert (!xaccTransGetRateForCommodity (fixture->txn, fixture->comm,
              split0, &rate));
    g_assert (gnc_numeric_zero_p (rate));

    g_assert (xaccTransGetRateForCommodity (fixture->txn, fixture->comm,
                                            split1, &rate));
    g_assert (gnc_numeric_equal (rate, gnc_numeric_create (1800, 240)));

}
/* xaccTransGetAccountConvRate
gnc_numeric
xaccTransGetAccountConvRate(const Transaction *txn, const Account *acc)// C: 5 in 4  Local: 0:0:0
*/
static void
test_xaccTransGetAccountConvRate (Fixture *fixture, gconstpointer pData)
{
    gchar *msg1 = "[xaccTransGetAccountConvRate()] How can amount be nonzero and value be zero?";
    guint loglevel = G_LOG_LEVEL_WARNING | G_LOG_FLAG_FATAL;
    TestErrorStruct *check = test_error_struct_new ("gnc.engine", loglevel,
                             msg1);
    Split *split1 = xaccTransFindSplitByAccount(fixture->txn, fixture->acc1);
    gnc_numeric rate;

    fixture->hdlrs = test_log_set_fatal_handler (fixture->hdlrs, check,
                     (GLogFunc)test_checked_handler);

    g_assert (gnc_numeric_equal (xaccTransGetAccountConvRate (fixture->txn,
                                 fixture->acc2),
                                 gnc_numeric_create (1, 1)));

    g_assert (gnc_numeric_equal (xaccTransGetAccountConvRate (fixture->txn,
                                 fixture->acc1),
                                 gnc_numeric_create (1800, 240)));
    g_assert_cmpint (check->hits, ==, 0);
    split1->value = gnc_numeric_zero();
    rate = xaccTransGetAccountConvRate (fixture->txn, fixture->acc1);
    g_assert_cmpint (gnc_numeric_check (rate), ==, GNC_ERROR_ARG);
    g_assert_cmpint (check->hits, ==, 1);
}
/* xaccTransGetAccountBalance
gnc_numeric
xaccTransGetAccountBalance (const Transaction *trans,// C: 1  Local: 0:0:0
*/
static void
test_xaccTransGetAccountBalance (Fixture *fixture, gconstpointer pData)
{
#ifdef __clang__
#define _func "gnc_numeric xaccTransGetAccountBalance(const Transaction *, const Account *)"
#else
#define _func "xaccTransGetAccountBalance"
#endif
    gchar *msg1 = _func ": assertion " _Q "account && trans' failed";
#undef _func
    guint loglevel = G_LOG_LEVEL_CRITICAL | G_LOG_FLAG_FATAL;
    TestErrorStruct *check = test_error_struct_new ("gnc.engine", loglevel,
                             msg1);
    Split *split1 = xaccTransFindSplitByAccount(fixture->txn, fixture->acc1);
    gnc_numeric rate;

    fixture->hdlrs = test_log_set_fatal_handler (fixture->hdlrs, check,
                     (GLogFunc)test_checked_handler);

    rate = xaccTransGetAccountBalance (NULL, fixture->acc1);
    g_assert_cmpint (gnc_numeric_check (rate), ==, GNC_ERROR_ARG);
    g_assert_cmpint (check->hits, ==, 1);

    rate = xaccTransGetAccountBalance (fixture->txn, NULL);
    g_assert_cmpint (gnc_numeric_check (rate), ==, GNC_ERROR_ARG);
    g_assert_cmpint (check->hits, ==, 2);

    rate = xaccTransGetAccountBalance (fixture->txn, fixture->acc1);
    g_assert (gnc_numeric_equal (rate, gnc_numeric_create (100000, 1000)));
    g_assert_cmpint (check->hits, ==, 2);

    rate = xaccTransGetAccountBalance (fixture->txn, fixture->acc2);
    g_assert (gnc_numeric_equal (rate, gnc_numeric_create (-3200, 240)));
    g_assert_cmpint (check->hits, ==, 2);

}
/* xaccTransGetCurrency
gnc_commodity *
xaccTransGetCurrency (const Transaction *trans)// C: 33 in 17 SCM: 34 in 26 Local: 2:0:0
Simple Getter. No need to test.
*/
/* xaccTransSetCurrency
void
xaccTransSetCurrency (Transaction *trans, gnc_commodity *curr)// C: 22 in 18 SCM: 3 in 3 Local: 1:0:0
*/
static void
test_xaccTransSetCurrency (Fixture *fixture, gconstpointer pData)
{
    QofBook *book = qof_instance_get_book (QOF_INSTANCE (fixture->txn));
    gnc_commodity *curr = gnc_commodity_new (book, "Japanese Yen", "CURRENCY", "JPY", "¥", 1);
    Split *split1 = xaccTransFindSplitByAccount (fixture->txn, fixture->acc1);
    gnc_numeric old_val = xaccSplitGetValue (split1);
    /* Prevent commit in xaccTransSetCurrency() */
    xaccTransBeginEdit(fixture->txn);
    xaccTransSetCurrency (fixture->txn, curr);
    g_assert (fixture->txn->common_currency == curr);
    g_assert_cmpint (xaccSplitGetValue (split1).denom, ==,
                     gnc_commodity_get_fraction (curr));
    g_assert_cmpint (xaccSplitGetValue (split1).num, ==,
                     old_val.num / old_val.denom);
}
/* xaccTransBeginEdit
void
xaccTransBeginEdit (Transaction *trans)// C: 72 in 28 SCM: 5 in 5 Local: 16:0:0
*/
static void
test_xaccTransBeginEdit ()
{
    QofBook *book = qof_book_new ();
    Transaction *txn = xaccMallocTransaction (book);
    Transaction *dupe = NULL;
    gchar *msg1 = "[xaccOpenLog] Attempt to open disabled transaction log";
    gchar *msg2 = "[xaccTransWriteLog] Attempt to write disabled transaction log";
    guint loglevel = G_LOG_LEVEL_INFO;
    gchar *logdomain = "gnc.translog";
    TestErrorStruct *check1 = test_error_struct_new (logdomain, loglevel, msg1);
    TestErrorStruct *check2 = test_error_struct_new (logdomain, loglevel, msg2);
    guint hdlr = g_log_set_handler (logdomain, loglevel,
                                    (GLogFunc)test_list_handler, NULL);
    test_add_error (check1);
    test_add_error (check2);


    g_assert_cmpint (0, ==, qof_instance_get_editlevel (QOF_INSTANCE (txn)));
    g_assert (txn->orig == NULL);
    xaccTransBeginEdit (txn);
    g_assert_cmpint (1, ==, qof_instance_get_editlevel (QOF_INSTANCE (txn)));
    dupe = txn->orig;
    g_assert (txn->orig != NULL);
    g_assert_cmpint (1, ==, check1->hits);
    g_assert_cmpint (1, ==, check2->hits);
    xaccTransBeginEdit (txn);
    g_assert_cmpint (2, ==, qof_instance_get_editlevel (QOF_INSTANCE (txn)));
    g_assert (txn->orig == dupe);
    g_assert_cmpint (1, ==, check1->hits);
    g_assert_cmpint (1, ==, check2->hits);
    xaccTransRollbackEdit (txn);
    xaccTransRollbackEdit (txn);
    g_assert_cmpint (0, ==, qof_instance_get_editlevel (QOF_INSTANCE (txn)));
    g_assert (txn->orig == NULL);
    qof_book_mark_readonly (book);
    xaccTransBeginEdit (txn);
    dupe = txn->orig;
    g_assert_cmpint (1, ==, qof_instance_get_editlevel (QOF_INSTANCE (txn)));
    g_assert (txn->orig == dupe);
    g_assert_cmpint (1, ==, check1->hits);
    g_assert_cmpint (2, ==, check2->hits);
    xaccTransRollbackEdit (txn);
    g_assert_cmpint (0, ==, qof_instance_get_editlevel (QOF_INSTANCE (txn)));
    g_assert (txn->orig == NULL);
    qof_book_destroy (book);
    xaccTransBeginEdit (txn);
    g_assert_cmpint (1, ==, qof_instance_get_editlevel (QOF_INSTANCE (txn)));
    g_assert (txn->orig == NULL);
    g_assert_cmpint (1, ==, check1->hits);
    g_assert_cmpint (2, ==, check2->hits);

    g_log_remove_handler (logdomain, hdlr);
    test_clear_error_list ();
    test_error_struct_free (check1);
    test_error_struct_free (check2);
    /* qof_book_destroy has already removed enough of the innards that
       trying to unref the txn and book crashes. */
}
/* xaccTransDestroy
void
xaccTransDestroy (Transaction *trans)// C: 26 in 15 SCM: 4 in 4 Local: 3:0:0
*/
static void
test_xaccTransDestroy (Fixture *fixture, gconstpointer pData)
{
    Transaction *txn = fixture->txn;
    QofBook *book = qof_instance_get_book (QOF_INSTANCE (txn));
    Transaction *dupe = xaccTransClone (txn);

    xaccTransBeginEdit (txn);
    g_assert (!qof_instance_get_destroying (QOF_INSTANCE (txn)));
    g_assert (xaccTransEqual (txn, dupe, FALSE, TRUE, FALSE, TRUE));
    xaccTransDestroy (txn);
    g_assert (qof_instance_get_destroying (QOF_INSTANCE (txn)));
    g_assert (xaccTransEqual (txn, dupe, FALSE, TRUE, FALSE, TRUE));
    xaccTransRollbackEdit (txn);
    qof_book_mark_readonly (book);
    xaccTransBeginEdit (txn);
    xaccTransDestroy (txn);
    g_assert (qof_instance_get_destroying (QOF_INSTANCE (txn)));
    g_assert (xaccTransEqual (txn, dupe, FALSE, TRUE, FALSE, TRUE));
    xaccTransRollbackEdit (txn);

    test_destroy (dupe);
}
/* destroy_gains
static void
destroy_gains (Transaction *trans)// Local: 1:0:0 -- from do_destroy
*/
static void
test_destroy_gains (GainsFixture *fixture, gconstpointer pData)
{
    /* Don't try to test with a NULL transaction, this is an internal
     * function that isn't protected.
     */
    Fixture *base = &(fixture->base);
    Split *base_split = g_list_nth_data (base->txn->splits, 1);
    xaccTransBeginEdit (fixture->gains_txn); /* Protect it from being actually destroyed */
    base->func->destroy_gains (base->txn);
    g_assert (qof_instance_get_destroying (QOF_INSTANCE (fixture->gains_txn)));
    g_assert (base_split->gains_split == NULL);
    xaccTransCommitEdit (fixture->gains_txn);
}
/* do_destroy
static void
do_destroy (Transaction *trans)// Local: 1:1:0 callback passed to qof_commit_edit_part2 in XaccTransCommitEdit

NB: This function has a weird three-step process for destroying and freeing the splits, which isn't really testable.
*/
static void
test_do_destroy (GainsFixture *fixture, gconstpointer pData)
{
    Fixture *base = &(fixture->base);
    Split *base_split = g_list_nth_data (base->txn->splits, 1);
    QofBook *book = qof_instance_get_book (base->txn);
    TestSignal sig = test_signal_new (QOF_INSTANCE (base->txn),
                                      QOF_EVENT_DESTROY, NULL);
    g_object_add_weak_pointer (G_OBJECT (base->txn->splits->data),
                               (gpointer)&base_split);
    g_object_ref (base->txn);
    g_object_ref (fixture->gains_txn);

    base->func->do_destroy (base->txn);
    g_assert_cmpint (test_signal_return_hits (sig), ==, 1);
    g_assert (base->txn->description == NULL);
    g_assert_cmpint (GPOINTER_TO_INT(base->txn->num), ==, 1);
    g_assert (qof_instance_get_destroying (QOF_INSTANCE (fixture->gains_txn)));
    g_assert (base_split == NULL);

    test_signal_free (sig);
}
/* xaccEnableDataScrubbing
 * xaccDisableDataScrubbing
 Trivial setters
*/
/* was_trans_emptied
static gboolean was_trans_emptied(Transaction *trans)// Local: 1:0:0 xaccTransCommitEdit
*/
static void
test_was_trans_emptied (Fixture *fixture, gconstpointer pData)
{
    GList *list = fixture->txn->splits;
    g_assert (!fixture->func->was_trans_emptied (fixture->txn));
    fixture->txn->splits = NULL;
    g_assert (fixture->func->was_trans_emptied (fixture->txn));
    /* Restore the list so teardown can free the splits */
    fixture->txn->splits = list;
}
/* trans_on_error
static void trans_on_error(Transaction *trans, QofBackendError errcode)// Local: 0:1:0 callback for qof_commit_edit_part2, xaccTransCommitEdit
*/

static QofBackendError errorvalue = 0;
static void
commit_error_cb (gpointer data, QofBackendError errcode)
{
    errorvalue = errcode;
}

static void
test_trans_on_error (Fixture *fixture, gconstpointer pData)
{
    QofBackendError errcode = ERR_BACKEND_MODIFIED;
    gchar *msg =
        "[trans_on_error()] Another user has modified this transaction\n"
        "\tjust a moment ago. Please look at their changes,\n"
        "\tand try again, if needed.\n";
    gchar *logdomain = "gnc.engine";
    guint loglevel = G_LOG_LEVEL_WARNING | G_LOG_FLAG_FATAL;
    TestErrorStruct *check = test_error_struct_new (logdomain, loglevel, msg);
    fixture->hdlrs = test_log_set_fatal_handler (fixture->hdlrs, check,
                     (GLogFunc)test_checked_handler);
    gnc_engine_add_commit_error_callback ((EngineCommitErrorCallback)commit_error_cb, NULL);
    xaccTransBeginEdit (fixture->txn);
    g_assert_cmpint (qof_instance_get_editlevel (fixture->txn), ==, 1);
    fixture->func->trans_on_error (fixture->txn, errcode);
    g_assert_cmpint (check->hits, ==, 1);
    g_assert_cmpint ((guint)errorvalue, ==, (guint)errcode);
    g_assert_cmpint (qof_instance_get_editlevel (fixture->txn), ==, 0);
    errorvalue = 0;
}
/* trans_cleanup_commit
static void trans_cleanup_commit(Transaction *trans)// Local: 0:1:0 callback for qof_commit_edit_part2, xaccTransCommitEdit
*/
static void
test_trans_cleanup_commit (Fixture *fixture, gconstpointer pData)
{
    QofBook *book = qof_instance_get_book (QOF_INSTANCE (fixture->txn));
    Split *destr_split = xaccMallocSplit (book);
    Split *bogus_split = xaccMallocSplit (book);
    Split *split0 = fixture->txn->splits->data;
    Account *acct0 = split0->acc;
    Transaction *orig = NULL;
    TestSignal *sig_d_remove = test_signal_new (QOF_INSTANCE (destr_split),
                               QOF_EVENT_REMOVE, NULL);
    TestSignal *sig_b_remove = test_signal_new (QOF_INSTANCE (bogus_split),
                               QOF_EVENT_REMOVE, NULL);
    TestSignal *sig_d_destroy = test_signal_new (QOF_INSTANCE (destr_split),
                                QOF_EVENT_DESTROY, NULL);
    TestSignal *sig_b_modify = test_signal_new (QOF_INSTANCE (bogus_split),
                               QOF_EVENT_MODIFY, NULL);
    TestSignal *sig_t_modify = test_signal_new (QOF_INSTANCE (fixture->txn),
                               QOF_EVENT_MODIFY, NULL);
    TestSignal *sig_a_changed = test_signal_new (QOF_INSTANCE (acct0),
                                GNC_EVENT_ITEM_CHANGED, NULL);

    xaccTransBeginEdit (fixture->txn);
    orig = fixture->txn->orig;
    g_object_ref (orig);
    /* Check the txn-isn't-the-parent path */
    fixture->txn->splits = g_list_prepend (fixture->txn->splits, destr_split);
    fixture->txn->splits = g_list_prepend (fixture->txn->splits, bogus_split);
    qof_instance_set_dirty (QOF_INSTANCE (destr_split));
    qof_instance_set_dirty (QOF_INSTANCE (bogus_split));
    qof_instance_set_destroying (QOF_INSTANCE (destr_split), TRUE);
    /*Reverse the splits list so we can check later that it got sorted */
    fixture->txn->splits = g_list_reverse (fixture->txn->splits);
    g_assert (fixture->txn->splits->data != split0);
    fixture->func->trans_cleanup_commit (fixture->txn);

    g_assert_cmpint (test_signal_return_hits (sig_d_remove), ==, 1);
    g_assert_cmpint (test_signal_return_hits (sig_b_remove), ==, 1);
    g_assert_cmpint (test_signal_return_hits (sig_d_destroy), ==, 0);
    g_assert_cmpint (test_signal_return_hits (sig_b_modify), ==, 0);
    g_assert_cmpint (test_signal_return_hits (sig_t_modify), ==, 1);
    g_assert_cmpint (test_signal_return_hits (sig_a_changed), ==, 1);
    g_assert_cmpint (g_list_index (fixture->txn->splits, destr_split), ==, -1);
    g_assert_cmpint (g_list_index (fixture->txn->splits, bogus_split), ==, -1);
    g_assert (fixture->txn->orig == NULL);
    g_assert (fixture->txn->splits->data == split0);
    g_assert (qof_instance_get_destroying (destr_split));
    /* Note that the function itself aborts if qof_instance_editlevel != 0 */

    /* load things back up and test the txn-is-the-parent path */
    qof_instance_increase_editlevel (fixture->txn);
    destr_split->parent = fixture->txn;
    bogus_split->parent = fixture->txn;
    fixture->txn->splits = g_list_prepend (fixture->txn->splits, destr_split);
    fixture->txn->splits = g_list_prepend (fixture->txn->splits, bogus_split);

    fixture->txn->orig = orig;
    orig->num = fixture->txn->num;
    g_object_ref (orig);
    fixture->func->trans_cleanup_commit (fixture->txn);

    g_assert_cmpint (test_signal_return_hits (sig_d_remove), ==, 2);
    g_assert_cmpint (test_signal_return_hits (sig_b_remove), ==, 1);
    g_assert_cmpint (test_signal_return_hits (sig_d_destroy), ==, 1);
    g_assert_cmpint (test_signal_return_hits (sig_b_modify), ==, 1);
    g_assert_cmpint (test_signal_return_hits (sig_t_modify), ==, 2);
    g_assert_cmpint (test_signal_return_hits (sig_a_changed), ==, 2);
    g_assert_cmpint (g_list_index (fixture->txn->splits, destr_split), ==, -1);
    g_assert_cmpint (g_list_index (fixture->txn->splits, bogus_split), ==, 0);
    g_assert_cmpint (GPOINTER_TO_INT(orig->num), ==, 1);
    test_destroy (orig);

}
/* xaccTransCommitEdit
void
xaccTransCommitEdit (Transaction *trans)// C: 88 in 28 SCM: 5 in 5 Local: 16:0:0
Setup has to run transCommitEdit, so we have to do our own setup for this function.
*/
static void
test_xaccTransCommitEdit (void)
{
    QofBook *book = qof_book_new ();
    Split *split1 = xaccMallocSplit (book);
    Split *split2 = xaccMallocSplit (book);
    Transaction *txn = xaccMallocTransaction (book);
    Account *acc1 = xaccMallocAccount (book);
    Account *acc2 = xaccMallocAccount (book);
    gnc_commodity *curr = gnc_commodity_new (book, "Gnu Rand",
                          "CURRENCY", "GNR", "", 240);
    gnc_commodity *comm = gnc_commodity_new (book, "Wildebeest Fund",
                          "FUND", "WBFXX", "", 1000);

    Timespec posted = gnc_dmy2timespec (21, 4, 2012);

    TestSignal *sig_1_modify = test_signal_new (QOF_INSTANCE (split1),
                               QOF_EVENT_MODIFY, NULL);
    TestSignal *sig_2_modify = test_signal_new (QOF_INSTANCE (split2),
                               QOF_EVENT_MODIFY, NULL);
    TestSignal *sig_txn_destroy = test_signal_new (QOF_INSTANCE (txn),
                                  QOF_EVENT_DESTROY, NULL);


    xaccAccountSetCommodity (acc1, comm);
    xaccAccountSetCommodity (acc2, curr);
    txn->date_posted.tv_sec = posted.tv_sec;
    txn->date_posted.tv_nsec = posted.tv_nsec;
    split1->memo = CACHE_INSERT ("foo");
    split1->action = CACHE_INSERT ("bar");
    split1->amount = gnc_numeric_create (100000, 1000);
    split1->value = gnc_numeric_create (3200, 240);
    /* Note, deliberately imblanced to force xaccTransScrubImbalance
     * to create a balance split, thus showing that it got called.
     */
    split2->amount = gnc_numeric_create (-3000, 240);
    split2->value = gnc_numeric_create (-3000, 240);
    split1->acc = acc1;
    split2->acc = acc2;
    txn->num = CACHE_INSERT ("123");
    txn->description = CACHE_INSERT ("Waldo Pepper");
    xaccTransBeginEdit (txn);
    {
        xaccTransSetCurrency (txn, curr);
        xaccSplitSetParent (split1, txn);
        xaccSplitSetParent (split2, txn);
    }
    /* Setup's done, now test: */
    xaccTransCommitEdit (txn);

    g_assert_cmpint (txn->date_entered.tv_sec, !=, 0);
    /* Signals make sure that trans_cleanup_commit got called */
    g_assert_cmpint (test_signal_return_hits (sig_1_modify), ==, 1);
    g_assert_cmpint (test_signal_return_hits (sig_2_modify), ==, 1);
    g_assert_cmpint (g_list_length (txn->splits), ==, 3);

    xaccTransBeginEdit (txn);
    g_list_free (txn->splits);
    txn->splits = NULL;
    xaccTransCommitEdit (txn);
    g_assert_cmpint (test_signal_return_hits (sig_txn_destroy), ==, 1);

    test_signal_free (sig_1_modify);
    test_signal_free (sig_2_modify);
    test_signal_free (sig_txn_destroy);
    test_destroy (split1);
    test_destroy (split2);
    test_destroy (acc1);
    test_destroy (acc2);
    test_destroy (curr);
    test_destroy (comm);
    test_destroy (book);
}
/* xaccTransRollbackEdit
void
xaccTransRollbackEdit (Transaction *trans)// C: 2 in 2  Local: 1:0:0
*/
static void
test_xaccTransRollbackEdit (Fixture *fixture, gconstpointer pData)
{
    Transaction *txn = fixture->txn;
    Transaction *orig = NULL;
    QofBook *book = qof_instance_get_book (txn);
    Timespec new_post = timespec_now ();
    Timespec new_entered = timespecCanonicalDayTime (timespec_now ());
    Timespec orig_post = txn->date_posted;
    Timespec orig_entered = txn->date_entered;
    KvpFrame *base_frame = NULL;
    TestSignal *sig_account = test_signal_new (QOF_INSTANCE (fixture->acc1),
                              GNC_EVENT_ITEM_CHANGED, NULL);
    MockBackend *mbe = (MockBackend*)qof_book_get_backend (book);
    Split *split_00 = txn->splits->data, *split_01 = txn->splits->next->data;
    Split *split_02 = xaccMallocSplit (book);
    Split *split_10 = NULL, *split_11 = NULL;

    xaccTransBeginEdit (txn);
    qof_instance_set_destroying (txn, TRUE);
    orig = txn->orig;
    base_frame = orig->inst.kvp_data; /* DupeTransaction copies the kvp_frame */
    g_object_ref (orig); /* Keep rollback from actually freeing it */
    txn->num = "321";
    txn->description = "salt peanuts";
    txn->common_currency = NULL;
    txn->inst.kvp_data = NULL;
    txn->date_entered = new_entered;
    txn->date_posted = new_post;
    txn->splits->data = split_01;
    txn->splits->next->data = split_00;
    qof_instance_set_dirty (QOF_INSTANCE (split_01));
    xaccSplitSetParent (split_02, txn);
    g_object_ref (split_02);
    split_10 = xaccDupeSplit(orig->splits->data);
    g_object_ref (split_10);
    split_11 = xaccDupeSplit(orig->splits->next->data);
    g_object_ref (split_11);
    qof_instance_increase_editlevel (QOF_INSTANCE (txn)); /* So it's 2 */
    xaccTransRollbackEdit (txn);
    g_assert (txn->orig == orig);
    qof_instance_reset_editlevel (QOF_INSTANCE (txn)); /* Now it's 0 */
    xaccTransRollbackEdit (txn);
    g_assert (txn->orig == orig);
    qof_instance_increase_editlevel (QOF_INSTANCE (txn)); /* And back to 1 */
    xaccTransRollbackEdit (txn);
    g_assert (txn->orig == NULL);
    g_assert_cmpstr (txn->num, ==, "123");
    g_assert_cmpint (GPOINTER_TO_INT(orig->num), ==, 1);
    g_assert_cmpstr (txn->description, ==, "Waldo Pepper");
    g_assert (txn->inst.kvp_data == base_frame);
    g_assert (txn->common_currency == fixture->curr);
    g_assert (timespec_equal (&(txn->date_posted), &orig_post));
    g_assert (timespec_equal (&(txn->date_entered), &orig_entered));
    g_assert_cmpuint (test_signal_return_hits (sig_account), ==, 1);
    g_assert_cmpuint (g_list_length (txn->splits), ==, 2);
    g_assert_cmpint (GPOINTER_TO_INT(split_02->memo), ==, 1);
    g_assert (xaccSplitEqual (txn->splits->data, split_10,
                              FALSE, FALSE, FALSE));
    g_assert (xaccSplitEqual (txn->splits->next->data, split_10,
                              FALSE, FALSE, FALSE));
    g_assert_cmpstr (mbe->last_call, ==, "rollback");
    g_assert_cmpuint (qof_instance_get_editlevel (QOF_INSTANCE (txn)), ==, 0);
    g_assert (qof_instance_get_destroying (txn) == FALSE);
    test_signal_free (sig_account);
    g_object_unref (split_10);
    g_object_unref (split_11);
    g_object_unref (split_02);
    g_object_unref (orig);

}
/* A second xaccTransRollbackEdit test to check the backend error handling */
static void
test_xaccTransRollbackEdit_BackendErrors (Fixture *fixture, gconstpointer pData)
{
    MockBackend *mbe = (MockBackend*)qof_book_get_backend (qof_instance_get_book (fixture->txn));
    guint loglevel = G_LOG_LEVEL_CRITICAL | G_LOG_FLAG_FATAL;
    gchar *msg = "[xaccTransRollbackEdit()] Rollback Failed.  Ouch!";
    TestErrorStruct *check = test_error_struct_new ("gnc.engine",
                             loglevel, msg);
    fixture->hdlrs = test_log_set_fatal_handler (fixture->hdlrs, check,
                     (GLogFunc)test_checked_handler);
    g_object_ref (fixture->txn);
    xaccTransBeginEdit (fixture->txn);
    mock_backend_set_error (mbe, ERR_BACKEND_MODIFIED);
    xaccTransRollbackEdit (fixture->txn);
    g_assert_cmpint (check->hits, ==, 1);
    g_assert_cmpstr (mbe->last_call, ==, "rollback");
    memset (mbe->last_call, 0, sizeof (mbe->last_call));
    xaccTransBeginEdit (fixture->txn);
    mock_backend_set_error (mbe, ERR_BACKEND_MOD_DESTROY);
    xaccTransRollbackEdit (fixture->txn);
    g_assert_cmpint (GPOINTER_TO_INT(fixture->txn->num), ==, 1);
    g_assert_cmpstr (mbe->last_call, ==, "rollback");

}
/* xaccTransIsOpen C: 23 in 7 SCM: 1  Local: 0:0:0
 * xaccTransOrder C: 2 in 2 SCM: 12 in 12 Local: 0:1:0

 * Simple convenience functions. No test required.
 */
/* xaccTransOrder_num_action
int
xaccTransOrder_num_action (const Transaction *ta, const char *actna,
                           const Transaction *tb, const char *actnb)// C: 1 Local: 1:0:0
*/
static void
test_xaccTransOrder_num_action (Fixture *fixture, gconstpointer pData)
{
    Transaction *txnA = fixture->txn;
    Transaction *txnB = fixture->func->dupe_trans (txnA);

    g_assert_cmpint (xaccTransOrder_num_action (txnA, NULL, NULL, NULL), ==, -1);
    g_assert_cmpint (xaccTransOrder_num_action (NULL, NULL, txnA, NULL), ==, 1);
    g_assert_cmpint (xaccTransOrder_num_action (NULL, NULL, NULL, NULL), ==, 0);
    g_assert_cmpint (xaccTransOrder_num_action (txnA, NULL, txnB, NULL), ==,
                     qof_instance_guid_compare (txnA, txnB));
    txnB->description = CACHE_INSERT ("Salt Peanuts");
    g_assert_cmpint (xaccTransOrder_num_action (txnA, NULL, txnB, NULL), >=, 1);
    txnB->date_entered.tv_sec += 1;
    g_assert_cmpint (xaccTransOrder_num_action (txnA, NULL, txnB, NULL), ==, -1);
    txnB->num = CACHE_INSERT ("101");
    g_assert_cmpint (xaccTransOrder_num_action (txnA, NULL, txnB, NULL), ==, 1);
    txnB->num = CACHE_INSERT ("one-oh-one");
    g_assert_cmpint (xaccTransOrder_num_action (txnA, NULL, txnB, NULL), ==, 1);
    g_assert_cmpint (xaccTransOrder_num_action (txnA, "24", txnB, "42"), ==, -1);
    txnB->date_posted.tv_sec -= 1;
    g_assert_cmpint (xaccTransOrder_num_action (txnA, "24", txnB, "42"), ==, 1);

    fixture->func->xaccFreeTransaction (txnB);
}
/* xaccTransSetDateInternal Local: 7:0:0
 * set_gains_date_dirty Local: 4:0:0
 * xaccTransSetDatePostedSecs C: 17 in 13  Local: 0:0:0
 * xaccTransSetDatePostedGDate C: 1  Local: 1:0:0
 * xaccTransSetDateEnteredSecs C: 10 in 9  Local: 0:0:0
 * xaccTransSetDatePostedTS C: 9 in 8  Local: 2:0:0
 * xaccTransSetDateEnteredTS C: 3 in 3  Local: 1:0:0
 * xaccTransSetDate C: 43 in 23 SCM: 2 in 2 Local: 0:0:0
 * xaccTransSetDateDueTS C: 2 in 2  Local: 0:0:0
 * xaccTransSetTxnType C: 4 in 3  Local: 0:0:0
 * xaccTransClearReadOnly C: 4 in 2  Local: 1:0:0
 * xaccTransSetReadOnly C: 2 in 2  Local: 1:0:0
 * qofTransSetNum Local: 0:1:0
 * xaccTransSetNum C: 13 in 12 SCM: 3 in 3 Local: 2:0:0
 * qofTransSetDescription Local: 0:0:0
 * xaccTransSetDescription C: 20 in 18 SCM: 5 in 3 Local: 2:0:0
 * qofTransSetNotes Local: 0:0:0
 * xaccTransSetNotes C: 5 in 5 SCM: 3 in 3 Local: 1:0:0
 * xaccTransSetIsClosingTxn C: 1  Local: 0:0:0
 * xaccTransGetSplit C: 57 in 24 SCM: 30 in 21 Local: 0:0:0
 * xaccTransGetSplitIndex C: 7 in 2  Local: 0:0:0
 * xaccTransGetSplitList C: 23 in 15 SCM: 19 in 15 Local: 2:1:0
 * xaccTransCountSplits C: 17 in 9 SCM: 2 in 2 Local: 0:0:0
 * xaccTransGetNum C: 15 in 12 SCM: 13 in 13 Local: 0:1:0
 * xaccTransGetDescription C: 43 in 23 SCM: 9 in 9 Local: 0:2:0
 * xaccTransGetNotes C: 8 in 6 SCM: 7 in 7 Local: 0:1:0
 * xaccTransGetIsClosingTxn SCM: 1  Local: 0:1:0
 * xaccTransGetDate C: 42 in 19  Local: 0:0:0
 * xaccTransGetDatePostedTS C: 6 in 5  Local: 1:0:0
 * xaccTransGetDateEnteredTS C: 1  Local: 0:0:0
 * xaccTransRetDatePostedTS C: 10 in 6  Local: 1:1:0
 * xaccTransGetDatePostedGDate C: 1  Local: 1:0:0
 * xaccTransRetDateEnteredTS C: 1  Local: 0:1:0
 * xaccTransGetDateDueTS C: 1  Local: 1:0:0
 * xaccTransRetDateDueTS C: 1 SCM: 2 in 2 Local: 0:1:0
 * xaccTransGetTxnType C: 3 in 2 SCM: 12 in 6 Local: 0:1:0
 * xaccTransGetReadOnly C: 7 in 5  Local: 1:0:0
 * xaccTransIsReadonlyByPostedDate C: 2 in 2  Local: 0:0:0
 * xaccTransHasReconciledSplitsByAccount Local: 1:0:0
 * xaccTransHasReconciledSplits C: 4 in 3  Local: 0:0:0
 * xaccTransHasSplitsInStateByAccount Local: 1:0:0
 * xaccTransHasSplitsInState C: 4 in 1  Local: 0:0:0
 * counter_thunk Local: 0:1:0
 * gnc_book_count_transactions C: 3 in 2  Local: 0:0:0
 * xaccTransGetVoidStatus C: 3 in 2 SCM: 1  Local: 0:1:0

 * An absurdly long list of trivial accessors which don't need to be tested.
 */

/* xaccTransVoid
void
xaccTransVoid(Transaction *trans, const char *reason)// C: 1 SCM: 2 in 2 Local: 0:0:0
* xaccTransUnvoid
void
xaccTransUnvoid (Transaction *trans)// C: 1  Local: 0:0:0
*/

static void
test_xaccTransVoid (Fixture *fixture, gconstpointer pData)
{
    /* Actual function variables start here. */
    KvpFrame *frame = fixture->txn->inst.kvp_data;
    gchar *void_reason = "Voided for Unit Test";
    gchar *txn_notes = g_strdup (kvp_frame_get_string (frame, trans_notes_str));
    KvpValue *val;
    Timespec now = timespec_now ();
    char iso8601_str[ISO_DATELENGTH + 1] = "";
    GList *split = NULL;

    xaccTransVoid (fixture->txn, void_reason);
    g_assert_cmpstr (kvp_frame_get_string (frame, trans_notes_str), ==,
                     "Voided transaction");
    g_assert_cmpstr (kvp_frame_get_string (frame, void_former_notes_str), ==,
                     txn_notes);
    g_assert_cmpstr (kvp_frame_get_string (frame, void_reason_str), ==,
                     void_reason);
    gnc_timespec_to_iso8601_buff (now, iso8601_str);
    g_assert_cmpstr (kvp_frame_get_string (frame, void_time_str), ==,
                     iso8601_str);
    g_assert_cmpstr (kvp_frame_get_string (frame, TRANS_READ_ONLY_REASON), ==,
                     "Transaction Voided");
    for (split = fixture->txn->splits; split; split=g_list_next (split))
    {
        g_assert (gnc_numeric_zero_p (((Split*)(split->data))->value));
        g_assert (gnc_numeric_zero_p (((Split*)(split->data))->amount));
    }

    xaccTransUnvoid (fixture->txn);

    g_assert_cmpstr (kvp_frame_get_string (frame, trans_notes_str), ==,
                     txn_notes);
    g_assert (kvp_frame_get_slot (frame, void_former_notes_str) == NULL);
    g_assert (kvp_frame_get_slot (frame, void_reason_str) == NULL);
    g_assert (kvp_frame_get_slot (frame, void_time_str) == NULL);
    g_assert (kvp_frame_get_slot (frame, TRANS_READ_ONLY_REASON) == NULL);
    for (split = fixture->txn->splits; split; split=g_list_next (split))
    {
        g_assert (!gnc_numeric_zero_p (((Split*)(split->data))->value));
        g_assert (!gnc_numeric_zero_p (((Split*)(split->data))->amount));
    }

    g_free (txn_notes);

}
/* xaccTransReverse
Transaction *
xaccTransReverse (Transaction *orig)// C: 2 in 2  Local: 0:0:0
*/
static void
test_xaccTransReverse (Fixture *fixture, gconstpointer pData)
{
    Transaction *rev = xaccTransReverse (fixture->txn);
    KvpFrame *frame = fixture->txn->inst.kvp_data;
    GList *orig_splits = NULL, *rev_splits = NULL;

    g_assert (guid_equal (kvp_frame_get_guid (frame, TRANS_REVERSED_BY),
                          xaccTransGetGUID (rev)));

    g_assert (qof_instance_is_dirty (QOF_INSTANCE (rev)));
    g_assert_cmpint (g_list_length (fixture->txn->splits), ==,
                     g_list_length (rev->splits));
    for (orig_splits = fixture->txn->splits,
            rev_splits = g_list_reverse (rev->splits);
            orig_splits && rev_splits;
            orig_splits = g_list_next (orig_splits),
            rev_splits = g_list_next (rev_splits))
    {
        Split *orig_split = orig_splits->data;
        Split *rev_split = rev_splits->data;
        g_assert (gnc_numeric_equal (orig_split->amount,
                                     gnc_numeric_neg (rev_split->amount)));
        g_assert (gnc_numeric_equal (orig_split->value,
                                     gnc_numeric_neg (rev_split->value)));
        g_assert_cmpint (xaccSplitGetReconcile (rev_split), ==, NREC);
    }

    fixture->func->xaccFreeTransaction (rev);
}
/* xaccTransGetReversedBy C: 2 in 2  Local: 0:0:0
 * Trivial getter.
 */
/* xaccTransScrubSplits  C: 1  Local: 0:0:0
 * Trival pass-through.
 */
/* xaccTransScrubGainsDate
static void
xaccTransScrubGainsDate (Transaction *trans)// Local: 1:0:0
*/
static void
test_xaccTransScrubGainsDate_no_dirty (GainsFixture *fixture,
                                       gconstpointer pData)
{
    Split *base_split = g_list_nth_data (fixture->base.txn->splits, 1);
    Split *gains_split = base_split->gains_split;

    base_split->gains = GAINS_STATUS_GAINS;
    gains_split->gains = GAINS_STATUS_GAINS;

    fixture->base.func->xaccTransScrubGainsDate (fixture->base.txn);

    g_assert (!timespec_equal (&(fixture->base.txn->date_posted),
                               &(fixture->gains_txn->date_posted)));
    g_assert_cmphex (base_split->gains & GAINS_STATUS_DATE_DIRTY, ==, 0);
    g_assert_cmphex (base_split->gains_split->gains & GAINS_STATUS_DATE_DIRTY,
                     ==, 0);
}

static void
test_xaccTransScrubGainsDate_base_dirty (GainsFixture *fixture,
        gconstpointer pData)
{
    Split *base_split = g_list_nth_data (fixture->base.txn->splits, 1);
    Split *gains_split = base_split->gains_split;

    base_split->gains = GAINS_STATUS_GAINS | GAINS_STATUS_DATE_DIRTY;
    gains_split->gains = GAINS_STATUS_GAINS;

    fixture->base.func->xaccTransScrubGainsDate (fixture->base.txn);

    g_assert (timespec_equal (&(fixture->base.txn->date_posted),
                              &(fixture->gains_txn->date_posted)));
    g_assert_cmphex (base_split->gains & GAINS_STATUS_DATE_DIRTY, ==, 0);
    g_assert_cmphex (base_split->gains_split->gains & GAINS_STATUS_DATE_DIRTY,
                     ==, 0);
}

static void
test_xaccTransScrubGainsDate_gains_dirty (GainsFixture *fixture,
        gconstpointer pData)
{
    Split *base_split = g_list_nth_data (fixture->base.txn->splits, 1);
    Split *gains_split = base_split->gains_split;

    base_split->gains = GAINS_STATUS_GAINS;
    gains_split->gains = GAINS_STATUS_GAINS | GAINS_STATUS_DATE_DIRTY;

    fixture->base.func->xaccTransScrubGainsDate (fixture->base.txn);

    g_assert (timespec_equal (&(fixture->base.txn->date_posted),
                              &(fixture->gains_txn->date_posted)));
    g_assert_cmphex (base_split->gains & GAINS_STATUS_DATE_DIRTY, ==, 0);
    g_assert_cmphex (base_split->gains_split->gains & GAINS_STATUS_DATE_DIRTY,
                     ==, 0);
}

/* xaccTransScrubGains Local: 1:0:0
 * Non-trivial, but it passes through selected splits to functions in
 * cap-gains.c and Scrub3.c that are beyond the scope of this test
 * program.
 */
/* xaccTransFindSplitByAccount C: 7 in 5  Local: 0:0:0
 * destroy_tx_on_book_close Local: 0:1:0
 * gnc_transaction_book_end Local: 0:1:0
 * trans_is_balanced_p Local: 0:1:0
 * Trivial pass-through.
 */


void
test_suite_transaction (void)
{
    GNC_TEST_ADD (suitename, "check open", Fixture, NULL, setup, test_check_open, teardown);
    GNC_TEST_ADD (suitename, "xaccTransStillHasSplit", Fixture, NULL, setup, test_xaccTransStillHasSplit, teardown);
    GNC_TEST_ADD (suitename, "mark trans", Fixture, NULL, setup, test_mark_trans, teardown);
    GNC_TEST_ADD (suitename, "gen event trans", Fixture, NULL, setup, test_gen_event_trans, teardown);
    GNC_TEST_ADD_FUNC (suitename, "gnc transaction init", test_gnc_transaction_init);
    GNC_TEST_ADD_FUNC (suitename, "gnc transaction dispose", test_gnc_transaction_dispose);
    GNC_TEST_ADD_FUNC (suitename, "gnc transaction finalize", test_gnc_transaction_finalize);
    GNC_TEST_ADD (suitename, "gnc transaction set/get property", Fixture, NULL, setup, test_gnc_transaction_set_get_property, teardown);
    GNC_TEST_ADD (suitename, "xaccMallocTransaction", Fixture, NULL, setup, test_xaccMallocTransaction, teardown);
    GNC_TEST_ADD (suitename, "xaccTransSortSplits", Fixture, NULL, setup, test_xaccTransSortSplits, teardown);
    GNC_TEST_ADD (suitename, "dupe_trans", Fixture, NULL, setup, test_dupe_trans, teardown);
    GNC_TEST_ADD (suitename, "xaccTransClone", Fixture, NULL, setup, test_xaccTransClone, teardown);
    GNC_TEST_ADD (suitename, "xaccTransCopyFromClipBoard", Fixture, NULL, setup, test_xaccTransCopyFromClipBoard, teardown);
    GNC_TEST_ADD (suitename, "xaccTransCopyFromClipBoard No-Start", Fixture, NULL, setup, test_xaccTransCopyFromClipBoard_no_start, teardown);
    GNC_TEST_ADD (suitename, "xaccFreeTransaction", Fixture, NULL, setup, test_xaccFreeTransaction, teardown);
// GNC_TEST_ADD (suitename, "compare split guids", Fixture, NULL, setup, test_compare_split_guids, teardown);
    GNC_TEST_ADD (suitename, "xaccTransEqual", Fixture, NULL, setup, test_xaccTransEqual, teardown);
    GNC_TEST_ADD (suitename, "xaccTransLookup", Fixture, NULL, setup, test_xaccTransLookup, teardown);
    GNC_TEST_ADD (suitename, "xaccTransGetImbalanceValue", Fixture, NULL, setup, test_xaccTransGetImbalanceValue, teardown);
    GNC_TEST_ADD (suitename, "xaccTransGetImbalance", Fixture, NULL, setup, test_xaccTransGetImbalance, teardown);
    GNC_TEST_ADD (suitename, "xaccTransGetImbalance Trading Accounts", Fixture, NULL, setup, test_xaccTransGetImbalance_trading, teardown);
    GNC_TEST_ADD (suitename, "xaccTransIsBalanced", Fixture, NULL, setup, test_xaccTransIsBalanced, teardown);
    GNC_TEST_ADD (suitename, "xaccTransIsBalanced Trading Accounts", Fixture, NULL, setup, test_xaccTransIsBalanced_trading, teardown);
    GNC_TEST_ADD (suitename, "xaccTransGetAccountValue", Fixture, NULL, setup, test_xaccTransGetAccountValue, teardown);
    GNC_TEST_ADD (suitename, "xaccTransGetRateForCommodity", Fixture, NULL, setup, test_xaccTransGetRateForCommodity, teardown);
    GNC_TEST_ADD (suitename, "xaccTransGetAccountAmount", Fixture, NULL, setup, test_xaccTransGetAccountAmount, teardown);
    GNC_TEST_ADD (suitename, "xaccTransGetAccountConvRate", Fixture, NULL, setup, test_xaccTransGetAccountConvRate, teardown);
    GNC_TEST_ADD (suitename, "xaccTransGetAccountBalance", Fixture, NULL, setup, test_xaccTransGetAccountBalance, teardown);

    GNC_TEST_ADD (suitename, "xaccTransSetCurrency", Fixture, NULL, setup, test_xaccTransSetCurrency, teardown);
    GNC_TEST_ADD_FUNC (suitename, "xaccTransBeginEdit", test_xaccTransBeginEdit);
    GNC_TEST_ADD (suitename, "xaccTransDestroy", Fixture, NULL, setup, test_xaccTransDestroy, teardown);
    GNC_TEST_ADD (suitename, "destroy gains", GainsFixture, NULL, setup_with_gains, test_destroy_gains, teardown_with_gains);
    GNC_TEST_ADD (suitename, "do destroy", GainsFixture, NULL, setup_with_gains, test_do_destroy, teardown_with_gains);
    GNC_TEST_ADD (suitename, "was trans emptied", Fixture, NULL, setup, test_was_trans_emptied, teardown);
    GNC_TEST_ADD (suitename, "trans on error", Fixture, NULL, setup, test_trans_on_error, teardown);
    GNC_TEST_ADD (suitename, "trans cleanup commit", Fixture, NULL, setup, test_trans_cleanup_commit, teardown);
    GNC_TEST_ADD_FUNC (suitename, "xaccTransCommitEdit", test_xaccTransCommitEdit);
    GNC_TEST_ADD (suitename, "xaccTransRollbackEdit", Fixture, NULL, setup, test_xaccTransRollbackEdit, teardown);
    GNC_TEST_ADD (suitename, "xaccTransRollbackEdit - Backend Errors", Fixture, NULL, setup, test_xaccTransRollbackEdit_BackendErrors, teardown);
    GNC_TEST_ADD (suitename, "xaccTransOrder_num_action", Fixture, NULL, setup, test_xaccTransOrder_num_action, teardown);
    GNC_TEST_ADD (suitename, "xaccTransVoid", Fixture, NULL, setup, test_xaccTransVoid, teardown);
    GNC_TEST_ADD (suitename, "xaccTransReverse", Fixture, NULL, setup, test_xaccTransReverse, teardown);
    GNC_TEST_ADD (suitename, "xaccTransScrubGainsDate", GainsFixture, NULL, setup_with_gains, test_xaccTransScrubGainsDate_no_dirty, teardown_with_gains);
    GNC_TEST_ADD (suitename, "xaccTransScrubGainsDate", GainsFixture, NULL, setup_with_gains, test_xaccTransScrubGainsDate_base_dirty, teardown_with_gains);
    GNC_TEST_ADD (suitename, "xaccTransScrubGainsDate", GainsFixture, NULL, setup_with_gains, test_xaccTransScrubGainsDate_gains_dirty, teardown_with_gains);

}
