/********************************************************************
 * utest-Split.c: GLib g_test test suite for Split.c.		    *
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
#ifdef __cplusplus
extern "C"
{
#endif

#include "config.h"
#include <string.h>
#include <glib.h>
#include <unittest-support.h>
/* Add specific headers for this class */
#include <Split.h>
#include <SplitP.h>
#include <Account.h>
#include <Transaction.h>
#include <TransactionP.h>
#include <gnc-lot.h>
#include <gnc-event.h>
#include <qofinstance-p.h>

#ifdef HAVE_GLIB_2_38
#define _Q "'"
#else
#define _Q "`"
#endif

static const gchar *suitename = "/engine/Split";
void test_suite_split ( void );

#ifdef __cplusplus
}
#endif

typedef struct
{
    Split *split;
    SplitTestFunctions *func;
    gnc_commodity *curr;
    gnc_commodity *comm;
    GSList *hdlrs;
} Fixture;

static void
setup (Fixture *fixture, gconstpointer pData)
{
    QofBook *book = qof_book_new ();
    Transaction *txn = xaccMallocTransaction (book);
    Account *acc = xaccMallocAccount (book);
    GNCLot *lot = gnc_lot_new (book);
    gnc_numeric value = gnc_numeric_create (123, 240);
    gnc_numeric amount = gnc_numeric_create (321, 1000);
    Timespec time = timespec_now ();
    Split *gains_split = xaccMallocSplit (book);
    fixture->curr = gnc_commodity_new (book, "Gnu Rand", "CURRENCY", "GNR", "", 240);
    fixture->comm = gnc_commodity_new (book, "Wildebeest Fund", "FUND", "WBFXX", "", 1000);

    fixture->func = _utest_split_fill_functions ();
    fixture->split = xaccMallocSplit (book);
    xaccAccountSetCommodity (acc, fixture->comm);
    xaccTransBeginEdit (txn);
    xaccTransSetCurrency (txn, fixture->curr);
    xaccSplitSetParent (fixture->split, txn);
    xaccTransCommitEdit (txn);
    gnc_lot_set_account (lot, acc);
    fixture->split->action = static_cast<char*>(CACHE_INSERT ("foo"));
    fixture->split->memo = static_cast<char*>(CACHE_INSERT ("bar"));
    fixture->split->acc = acc;
    fixture->split->lot = lot;
    fixture->split->parent = txn;
    fixture->split->amount = amount;
    fixture->split->value = value;
    fixture->split->date_reconciled = time;
    fixture->split->reconciled = YREC;
    fixture->split->gains = GAINS_STATUS_VALU_DIRTY;
    fixture->split->gains_split = gains_split;

    fixture->split->balance = amount;
    fixture->split->cleared_balance = amount;
    fixture->split->reconciled_balance = amount;
    qof_instance_mark_clean (QOF_INSTANCE (fixture->split));
    qof_instance_mark_clean (QOF_INSTANCE (acc));
    qof_instance_mark_clean (QOF_INSTANCE (txn));
    fixture->hdlrs = NULL;
}

static void
teardown (Fixture *fixture, gconstpointer pData)
{
    QofBook *book = xaccSplitGetBook (fixture->split);
    Account *acc = xaccSplitGetAccount (fixture->split);
    Transaction *txn = xaccSplitGetParent (fixture->split);
    GNCLot *lot = xaccSplitGetLot (fixture->split);
    test_destroy (fixture->split->gains_split);
    test_destroy (lot);
    test_destroy (txn);
    test_destroy (acc);
    test_destroy (fixture->split);
    test_destroy (fixture->curr);
    test_destroy (fixture->comm);
    test_destroy (book);
    g_free (fixture->func);
    g_slist_free_full (fixture->hdlrs, test_free_log_handler);
    test_clear_error_list();
}

/* gnc_split_init
static void
gnc_split_init(Split* split)*/
static void
test_gnc_split_init ()
{
    Split *split = static_cast<Split*>(g_object_new (GNC_TYPE_SPLIT, NULL));
    g_assert (split->acc == NULL);
    g_assert (split->orig_acc == NULL);
    g_assert (split->parent == NULL);
    g_assert (split->lot == NULL);
    g_assert_cmpstr (split->action, ==, "");
    g_assert_cmpstr (split->memo, ==, "");
    g_assert_cmpint (split->reconciled, ==, NREC);
    g_assert (gnc_numeric_zero_p (split->amount));
    g_assert (gnc_numeric_zero_p (split->value));
    g_assert (gnc_numeric_zero_p (split->balance));
    g_assert (gnc_numeric_zero_p (split->cleared_balance));
    g_assert (gnc_numeric_zero_p (split->reconciled_balance));
    g_assert_cmpint (split->gains, ==, GAINS_STATUS_UNKNOWN);
    g_assert (split->gains_split == NULL);
    /* Make sure that the parent's init has been run */
    g_assert (split->inst.kvp_data != NULL);

    g_object_unref (split);
}
/* gnc_split_dispose
static void
gnc_split_dispose(GObject *splitp)*/
static void
test_gnc_split_dispose ()
{
    /* gnc_split_dispose doesn't do anything besides chain up to its
     * parent, so we twiddle the parent QofInstance to populate its
     * collection object (which is released by its dispose) then run
     * dispose on Split and make sure that the collection object is indeed
     * released.
     *
     * TODO: Split keeps references to two Account objects, two
     * Transaction objects, a Split object, and a GNCLot object. All of
     * these should be unreffed in gnc_split_dispose.
     */
    Split *split = static_cast<Split*>(g_object_new (GNC_TYPE_SPLIT, NULL));
    QofInstance *instance = QOF_INSTANCE (split);
    QofBook *book = qof_book_new ();

    qof_instance_init_data (instance, GNC_ID_SPLIT, book);

    g_assert_cmpstr (instance->e_type, ==, GNC_ID_SPLIT);

    g_object_run_dispose (G_OBJECT (split));

    g_assert (instance->e_type == NULL);

    g_object_unref (split);
    g_object_unref (book);
}
/* gnc_split_finalize
static void
gnc_split_finalize(GObject* splitp)
Can't actually test this, it's a class function.
*/
/* gnc_split_get__property
static void
gnc_split_get_property(GObject         *object,*/
/* gnc_split_set_property
static void
gnc_split_set_property(GObject         *object,*/
static void
test_gnc_split_set_get_property ()
{
    /* TODO: Several of the parameters set by gnc_split_init are not
     * properties, and two members of struct split_s (orig_parent and
     * reconciled) are neither initialized in gnc_split_init nor
     * properties.
     */
    QofBook *book = qof_book_new ();
    gnc_commodity *curr = gnc_commodity_new (book, "Gnu Rand", "CURRENCY", "GNR", "", 100);
    Transaction *txn = xaccMallocTransaction (book), *rtxn = NULL;
    Account *acc = xaccMallocAccount (book), *racc = NULL;
    GNCLot *lot = gnc_lot_new (book), *rlot = NULL;
    Split *split = xaccMallocSplit (book);
    Timespec time = timespec_now (), *rtime;
    char *r_action, *r_memo;
    gnc_numeric value = gnc_numeric_create (123, 100);
    gnc_numeric amount = gnc_numeric_create (321, 100);
    gnc_numeric *r_value, *r_amount;

    xaccAccountSetCommodity (acc, curr);
    xaccTransBeginEdit (txn);
    xaccTransSetCurrency (txn, curr);
    gnc_lot_set_account (lot, acc);
    qof_commit_edit (QOF_INSTANCE (txn));

    g_object_set (G_OBJECT (split),
                  "action", "foo",
                  "memo", "bar",
                  "value", &value,
                  "amount", &amount,
                  "reconcile-date", &time,
                  "account", acc,
                  "lot", lot,
                  "transaction", txn,
                  NULL);

    g_object_get (G_OBJECT (split),
                  "action", &r_action,
                  "memo", &r_memo,
                  "value", &r_value,
                  "amount", &r_amount,
                  "reconcile-date", &rtime,
                  "transaction", &rtxn,
                  "account", &racc,
                  "lot", &rlot,
                  NULL);

    g_assert_cmpstr (r_action, ==, "foo");
    g_assert_cmpstr (r_memo, ==, "bar");
    g_assert (gnc_numeric_equal (*r_value, value));
    /* Setting the transaction causes the amount to be scrubbed into the value */
    g_assert (gnc_numeric_equal (*r_amount, value));
    g_assert (timespec_equal (rtime, &time));
    g_assert (txn == rtxn);
    g_assert (acc == racc);
    g_assert (lot == rlot);

    /* The official destroy functions all step on each other. We'll take a
     * few leaks to save trouble; it will all work fine once the
     * refactoring is taken care of.
     */
    g_object_unref (txn);
    g_object_unref (acc);
    g_object_unref (lot);
    gnc_commodity_destroy (curr);
    g_object_unref (split);
    g_object_unref (book);
}

/* xaccInitSplit
static void
xaccInitSplit(Split * split, QofBook *book)// Local: 1:0:0
An utterly useless function that just chains up to QofInstance.
*/

// Not Used
/* xaccSplitReinit
void
xaccSplitReinit(Split * split)//
*/
/* xaccMallocSplit
Split *
xaccMallocSplit(QofBook *book)// C: 46 in 23 SCM: 3 in 2
*/
static void
test_xaccMallocSplit (Fixture *fixture, gconstpointer pData)
{
    /* We use this in the setup, so we can just verify that it works. */
    g_assert (fixture->split != NULL);
}
/* xaccDupeSplit
Split *
xaccDupeSplit (const Split *s)// C: 1
*/
static void
test_xaccDupeSplit (Fixture *fixture, gconstpointer pData)
{
    Split *f_split = fixture->split;
    Split *split = xaccDupeSplit (f_split);

    g_assert (split != fixture->split);
    g_assert (qof_instance_get_guid (split) != qof_instance_get_guid (f_split));
    g_assert (guid_equal (qof_instance_get_guid (split), qof_instance_get_guid (f_split)));
    g_assert (qof_instance_get_book (split) == qof_instance_get_book (f_split));
    g_assert (split->parent == f_split->parent);
    g_assert (split->acc == f_split->acc);
    g_assert (split->orig_acc == f_split->orig_acc);
    g_assert (split->lot == f_split->lot);
    g_assert_cmpstr (split->memo, ==, f_split->memo);
    g_assert_cmpstr (split->action, ==, f_split->action);
    g_assert (kvp_frame_compare (split->inst.kvp_data, f_split->inst.kvp_data) == 0);
    g_assert_cmpint (split->reconciled, ==, f_split->reconciled);
    g_assert (timespec_equal (&(split->date_reconciled), &(f_split->date_reconciled)));
    g_assert (gnc_numeric_equal (split->value, f_split->value));
    g_assert (gnc_numeric_equal (split->amount, f_split->amount));
    /* xaccDupeSplit intentionally doesn't copy the balances */
    g_assert (gnc_numeric_zero_p (split->balance));
    g_assert (gnc_numeric_zero_p (split->cleared_balance));
    g_assert (gnc_numeric_zero_p (split->reconciled_balance));
    /* FIXME: gains and gains_split are not copied */
    g_assert_cmpint (split->gains, !=, f_split->gains);
    g_assert (split->gains_split != f_split->gains_split);

}
/* xaccSplitCloneNoKvp
Split *
xaccSplitCloneNoKvp (const Split *s)// C: 1 
*/
static void
test_xaccSplitCloneNoKvp (Fixture *fixture, gconstpointer pData)
{
    Split *f_split = fixture->split;
    Split *split = xaccSplitCloneNoKvp (f_split);

    g_assert (split != fixture->split);
    g_assert (qof_instance_get_guid (split) != qof_instance_get_guid (f_split));
    g_assert_cmpint (guid_compare (qof_instance_get_guid (split), qof_instance_get_guid (f_split)), !=, 0);
    g_assert (qof_instance_get_book (split) == qof_instance_get_book (f_split));
    g_assert (split->parent == NULL);
    g_assert (split->acc == f_split->acc);
    /* Clone doesn't copy the orig_acc */
    g_assert (split->orig_acc == NULL);
    g_assert (split->lot == f_split->lot);
    g_assert_cmpstr (split->memo, ==, f_split->memo);
    g_assert_cmpstr (split->action, ==, f_split->action);
    g_assert (kvp_frame_is_empty (split->inst.kvp_data));
    g_assert_cmpint (split->reconciled, ==, f_split->reconciled);
    g_assert (timespec_equal (&(split->date_reconciled), &(f_split->date_reconciled)));
    g_assert (gnc_numeric_equal (split->value, f_split->value));
    g_assert (gnc_numeric_equal (split->amount, f_split->amount));
    g_assert (gnc_numeric_equal (split->balance, f_split->balance));
    g_assert (gnc_numeric_equal (split->cleared_balance, f_split->cleared_balance));
    g_assert (gnc_numeric_equal (split->reconciled_balance, f_split->reconciled_balance));
    g_assert_cmpint (split->gains, ==, GAINS_STATUS_UNKNOWN);
    g_assert (split->gains_split == NULL);
}
// Not Used
/* xaccSplitDump
void
xaccSplitDump (const Split *split, const char *tag)//
*/
/* xaccFreeSplit
void
xaccFreeSplit (Split *split)// C: 3 in 1
Not testable and wrong besides.
*/
/* mark_split
void mark_split (Split *s)// C: 2 in 2 SCM: 10 in 1 Local: 8:0:0
OK, weird. Doesn't mark the split, marks the account sort-dirty and balance-dirty parameters.
*/
static void
test_mark_split (Fixture *fixture, gconstpointer pData)
{
    gboolean sort_dirty, balance_dirty;
    g_object_get (fixture->split->acc,
                  "sort-dirty", &sort_dirty,
                  "balance-dirty", &balance_dirty,
                  NULL);
    g_assert_cmpint (sort_dirty, ==, FALSE);
    g_assert_cmpint (balance_dirty, ==, FALSE);

    mark_split (fixture->split);

    g_object_get (fixture->split->acc,
                  "sort-dirty", &sort_dirty,
                  "balance-dirty", &balance_dirty,
                  NULL);
    g_assert_cmpint (sort_dirty, ==, TRUE);
    g_assert_cmpint (balance_dirty, ==, TRUE);
}
// Not Used
/* xaccSplitEqualCheckBal
static gboolean
xaccSplitEqualCheckBal (const char *tag, gnc_numeric a, gnc_numeric b)//
*/
static void
test_xaccSplitEqualCheckBal (Fixture *fixture, gconstpointer pData)
{
    gchar *msg = "[xaccSplitEqualCheckBal] test balances differ: 123/100 vs 456/100";
    GLogLevelFlags loglevel = G_LOG_LEVEL_INFO;
    guint hdlr;
    TestErrorStruct check = { loglevel, "gnc.engine", msg, 0 };

    gnc_numeric foo = gnc_numeric_create (123, 100);
    gnc_numeric bar = gnc_numeric_create (456, 100);

    hdlr = g_log_set_handler ("gnc.engine", loglevel,
                              (GLogFunc)test_checked_handler, &check);

    g_assert_cmpint (fixture->func->xaccSplitEqualCheckBal ("test ", foo, foo), ==, TRUE);
    g_assert_cmpint (fixture->func->xaccSplitEqualCheckBal ("test ", foo, bar), ==, FALSE);
    g_assert_cmpint (check.hits, ==, 1);
    g_log_remove_handler ("gnc.engine", hdlr);

}
/* xaccSplitEqual
gboolean
xaccSplitEqual(const Split *sa, const Split *sb,// C: 2 in 2 SCM: 1
*/

static void
test_xaccSplitEqual (Fixture *fixture, gconstpointer pData)
{
    Split *split1 = xaccSplitCloneNoKvp (fixture->split);
    Split *split2 = xaccDupeSplit (fixture->split);
    gchar *msg01 = "[xaccSplitEqual] one is NULL";
    gchar *msg02 = "[xaccSplitEqual] GUIDs differ";
    gchar *msg03;
    gchar *msg04 = "[xaccSplitEqual] actions differ: foo vs bar";
    G_GNUC_UNUSED gchar *msg05 = "[xaccSplitEqual] kvp frames: differ foo vs bar";
    G_GNUC_UNUSED gchar *msg06 = "[xaccSplitEqual] reconcile flags differ: foo vs bar";
    G_GNUC_UNUSED gchar *msg07 = "[xaccSplitEqual] reconciled date differs";
    G_GNUC_UNUSED gchar *msg08 = "[xaccSplitEqual] amounts differ: foo vs bar";
    gchar *msg10 = "[xaccSplitEqual] transactions differ";
    gchar *msg11 = "[xaccTransEqual] one is NULL";
    gchar *msg12 = "[xaccSplitEqualCheckBal] balances differ: 321/1000 vs 0/1";
    gchar *msg13 = "[xaccSplitEqualCheckBal] cleared balances differ: 321/1000 vs 0/1";
    gchar *msg14 = "[xaccSplitEqualCheckBal] reconciled balances differ: 321/1000 vs 0/1";
    gchar *logdomain = "gnc.engine";
    GLogLevelFlags loglevel = G_LOG_LEVEL_INFO;
    TestErrorStruct checkA = { loglevel, logdomain, msg01, 0 };
    TestErrorStruct checkB = { loglevel, logdomain, msg10, 0 };
    TestErrorStruct checkC = { loglevel, logdomain, msg11, 0 };
    TestErrorStruct checkD = { loglevel, logdomain, msg14, 0 };
    guint hdlr;

    test_add_error (&checkA);
    test_add_error (&checkB);
    test_add_error (&checkC);
    test_add_error (&checkD);

    hdlr  = g_log_set_handler (logdomain, loglevel,
                               (GLogFunc)test_list_handler, &checkA);
    /* Note that check_splits is just passed through to xaccTransEqual, so we don't vary it here. */
    /* Test that a NULL comparison fails */
    g_assert (xaccSplitEqual (fixture->split, NULL, TRUE, TRUE, TRUE) == FALSE);
    g_assert (xaccSplitEqual (NULL, split1, TRUE, TRUE, TRUE) == FALSE);
    g_assert_cmpint (checkA.hits, ==, 2);
    g_assert_cmpint (checkB.hits, ==, 0);
    g_assert_cmpint (checkC.hits, ==, 0);
    g_assert_cmpint (checkD.hits, ==, 0);
    checkA.msg = msg02;
    /* Clone creates splits with different GUIDs: Make sure that it fails comparison */
    g_assert (xaccSplitEqual (fixture->split, split1, TRUE, TRUE, TRUE) == FALSE);
    /* Test that the parent comparison fails */
    g_assert (xaccSplitEqual (fixture->split, split1, FALSE, TRUE, TRUE) == FALSE);
    /* Now set split1's parent so that it passes -- we're also checking that the GUID check is disabled when we pass FALSE to check_guids */
    g_assert_cmpint (checkA.hits, ==, 3);
    g_assert_cmpint (checkB.hits, ==, 1);
    g_assert_cmpint (checkC.hits, ==, 1);
    g_assert_cmpint (checkD.hits, ==, 0);
    split1->parent = fixture->split->parent;
    g_assert (xaccSplitEqual (fixture->split, split1, FALSE, TRUE, TRUE) == TRUE);
    /* Now set the GUIDs equal and see that the comparison passes */
    qof_instance_increase_editlevel (split1->parent);
    g_object_set (G_OBJECT (split1),
                  "guid", qof_instance_get_guid (QOF_INSTANCE(fixture->split)),
                  NULL);
    qof_instance_increase_editlevel (split1->parent);
    g_assert (xaccSplitEqual (fixture->split, split1, TRUE, TRUE, TRUE) == TRUE);
    g_assert_cmpint (checkA.hits, ==, 3);
    g_assert_cmpint (checkB.hits, ==, 1);
    g_assert_cmpint (checkC.hits, ==, 1);
    g_assert_cmpint (checkD.hits, ==, 0);
    /* Change the memo and action and test that each in turn causes the comparison to fail */
    split1->memo = "baz";
    msg03 = g_strdup_printf ("[xaccSplitEqual] memos differ: (%p)%s vs (%p)%s",
                             fixture->split->memo, fixture->split->memo,
                             split1->memo, split1->memo);
    checkA.msg = msg03;
    g_assert (xaccSplitEqual (fixture->split, split1, TRUE, TRUE, TRUE) == FALSE);
    g_assert_cmpint (checkA.hits, ==, 4);
    g_assert_cmpint (checkB.hits, ==, 1);
    g_assert_cmpint (checkC.hits, ==, 1);
    g_assert_cmpint (checkD.hits, ==, 0);
    split1->memo = fixture->split->memo;
    split1->action = "bar";
    checkA.msg = msg04;
    g_assert (xaccSplitEqual (fixture->split, split1, TRUE, TRUE, TRUE) == FALSE);
    g_log_remove_handler (logdomain, hdlr);
    hdlr  = g_log_set_handler (logdomain, loglevel,
                               (GLogFunc)test_list_handler, &checkA);
    g_assert (xaccSplitEqual (fixture->split, split1, TRUE, TRUE, TRUE) == FALSE);
    g_assert_cmpint (checkA.hits, ==, 6);
    g_assert_cmpint (checkB.hits, ==, 1);
    g_assert_cmpint (checkC.hits, ==, 1);
    g_assert_cmpint (checkD.hits, ==, 0);
    /* Split2 doesn't have balances copied from fixture->split, so the balance test fails */
    checkB.msg = msg12;
    checkC.msg = msg13;
    g_assert (xaccSplitEqual (fixture->split, split2, TRUE, TRUE, TRUE) == FALSE);
    g_assert_cmpint (checkA.hits, ==, 6);
    g_assert_cmpint (checkB.hits, ==, 2);
    g_assert_cmpint (checkC.hits, ==, 1);
    g_assert_cmpint (checkD.hits, ==, 0);

    split2->balance = fixture->split->balance;
    g_assert (xaccSplitEqual (fixture->split, split2, TRUE, TRUE, TRUE) == FALSE);
    g_assert_cmpint (checkA.hits, ==, 6);
    g_assert_cmpint (checkB.hits, ==, 2);
    g_assert_cmpint (checkC.hits, ==, 2);
    g_assert_cmpint (checkD.hits, ==, 0);

    split2->cleared_balance = fixture->split->cleared_balance;
    g_assert (xaccSplitEqual (fixture->split, split2, TRUE, TRUE, TRUE) == FALSE);
    g_assert_cmpint (checkA.hits, ==, 6);
    g_assert_cmpint (checkB.hits, ==, 2);
    g_assert_cmpint (checkC.hits, ==, 2);
    g_assert_cmpint (checkD.hits, ==, 1);

    test_clear_error_list ();
    g_assert (xaccSplitEqual (fixture->split, split2, TRUE, FALSE, TRUE) == TRUE);
    g_object_unref (split1);
    g_object_unref (split2);
    test_clear_error_list ();
    g_log_remove_handler (logdomain, hdlr);

    g_free (msg03);
}
/* xaccSplitGetAccount
 * xaccSplitSetAccount
 * commit_err
static void commit_err (QofInstance *inst, QofBackendError errcode)
Used as a callback in xaccSplitCommitEdit; it will get tested there via intercepting the warning and testing the signal.
*/
/* xaccSplitCommitEdit
void
xaccSplitCommitEdit(Split *s)// C: 2 in 1
Note that there is no xaccSplitBeginEdit.
Note: Unfortunately most of the internals of this function can't be monitored without a mock Account object.
*/
typedef struct
{
    guint hits;
    QofBackendError lasterr;
} TestErr;

static void
test_error_callback (gpointer pdata, QofBackendError errcode)
{
    TestErr *data = static_cast<TestErr*>(pdata);
    ++(data->hits);
    data->lasterr = errcode;
}
static void
test_xaccSplitCommitEdit (Fixture *fixture, gconstpointer pData)
{
    gboolean sort_dirty, balance_dirty;
    gchar *msg1 = "[xaccSplitCommitEdit()] Account lost track of moved or deleted split.";
    gchar *msg2 = "[xaccSplitCommitEdit()] Account grabbed split prematurely.";
    gchar *logdomain = "gnc.engine";
    GLogLevelFlags loglevel = static_cast<GLogLevelFlags>(G_LOG_LEVEL_CRITICAL | G_LOG_FLAG_FATAL);
    guint infolevel = G_LOG_LEVEL_INFO;
    guint hdlr;
    TestErrorStruct checkA = { loglevel, logdomain, msg1, 0 };
    TestErrorStruct checkB = { loglevel, logdomain, msg2, 0 };
    TestSignal sig1, sig2;
    TestErr error = { 0, ERR_BACKEND_NO_ERR };
    Account *oacc = xaccMallocAccount (xaccSplitGetBook (fixture->split));
    Transaction *opar = xaccMallocTransaction (xaccSplitGetBook (fixture->split));
    test_add_error (&checkA);
    test_add_error (&checkB);
    g_test_log_set_fatal_handler ((GTestLogFatalFunc)test_list_handler, &checkA);
    hdlr = g_log_set_handler (logdomain, loglevel,
                              (GLogFunc)test_list_handler, &checkA);

    fixture->split->orig_acc = oacc;
    fixture->split->orig_parent = opar;

    gnc_engine_add_commit_error_callback ((EngineCommitErrorCallback)test_error_callback, &error);
    sig1 = test_signal_new (QOF_INSTANCE (fixture->split->orig_parent), QOF_EVENT_MODIFY, NULL);
    sig2 = test_signal_new (QOF_INSTANCE (fixture->split->lot), QOF_EVENT_MODIFY, NULL);

    qof_instance_set_dirty (QOF_INSTANCE (fixture->split));

    xaccSplitCommitEdit (fixture->split);

    g_object_get (fixture->split->acc,
                  "sort-dirty", &sort_dirty,
                  "balance-dirty", &balance_dirty,
                  NULL);
    g_assert_cmpint (sort_dirty, ==, TRUE);
    g_assert_cmpint (balance_dirty, ==, FALSE);
    g_assert (qof_instance_is_dirty (QOF_INSTANCE (fixture->split->parent)));
    g_assert (qof_instance_is_dirty (QOF_INSTANCE (fixture->split)));
    test_signal_assert_hits (sig1, 1);
    test_signal_assert_hits (sig2, 3);
    g_assert_cmpint (error.hits, ==, 0);
    g_assert_cmpint (error.lasterr, ==, ERR_BACKEND_NO_ERR);
    g_assert (fixture->split->orig_acc == fixture->split->acc);
    g_assert (fixture->split->orig_parent == fixture->split->parent);
    g_assert_cmpint (checkA.hits, ==, 4);
    g_assert_cmpint (checkB.hits, ==, 2);

    qof_instance_mark_clean (QOF_INSTANCE (fixture->split->parent));
    qof_instance_increase_editlevel (fixture->split->acc);
    g_object_set (fixture->split->acc,
                  "sort-dirty", FALSE,
                  "balance-dirty", FALSE,
                  NULL);
    qof_instance_decrease_editlevel (fixture->split->acc);

    qof_instance_set_dirty (QOF_INSTANCE (fixture->split));
    xaccSplitCommitEdit (fixture->split);
    g_object_get (fixture->split->acc,
                  "sort-dirty", &sort_dirty,
                  "balance-dirty", &balance_dirty,
                  NULL);
    g_assert_cmpint (sort_dirty, ==, TRUE);
    g_assert_cmpint (balance_dirty, ==, FALSE);
    g_assert (!qof_instance_is_dirty (QOF_INSTANCE (fixture->split->parent)));
    g_assert (qof_instance_is_dirty (QOF_INSTANCE (fixture->split)));
    test_signal_assert_hits (sig1, 1);
    test_signal_assert_hits (sig2, 4);
    g_assert_cmpint (checkA.hits, ==, 4);
    g_assert_cmpint (checkB.hits, ==, 2);
    g_assert_cmpint (error.hits, ==, 0);
    g_assert_cmpint (error.lasterr, ==, ERR_BACKEND_NO_ERR);
    g_assert (fixture->split->orig_acc == fixture->split->acc);
    g_assert (fixture->split->orig_parent == fixture->split->parent);


    g_log_remove_handler (logdomain, hdlr);
    test_signal_free (sig1);
    test_signal_free (sig2);
    test_destroy (oacc);
    test_destroy (opar);
}
/* xaccSplitRollbackEdit
void
xaccSplitRollbackEdit(Split *s)// C: 2 in 1
*/
static void
test_xaccSplitRollbackEdit (Fixture *fixture, gconstpointer pData)
{
    TestSignal sig1, sig2, sig3;
    Transaction *txn1 = fixture->split->parent;
    Transaction *txn2 = xaccMallocTransaction (xaccSplitGetBook (fixture->split));
    Split *split2 = xaccMallocSplit (xaccSplitGetBook (fixture->split));
    Account *acc = fixture->split->acc;

    /* Set a second split on txn1 so that txn1 isn't destroyed when we
     * remove fixture->split from it.
     */
    xaccTransBeginEdit (txn1);
    xaccSplitSetParent (split2, txn1);
    xaccTransCommitEdit (txn1);

    sig1 = test_signal_new (QOF_INSTANCE (txn1),
                            GNC_EVENT_ITEM_REMOVED, NULL);
    sig2 = test_signal_new (QOF_INSTANCE (txn2),
                            GNC_EVENT_ITEM_ADDED, NULL);
    sig3 = test_signal_new (QOF_INSTANCE (txn1),
                            GNC_EVENT_ITEM_ADDED, NULL);
    fixture->split->orig_acc = NULL;
    g_assert (fixture->split->acc != fixture->split->orig_acc);
    fixture->split->orig_parent = NULL;

    xaccSplitRollbackEdit (fixture->split);
    test_signal_assert_hits (sig1, 1);
    test_signal_assert_hits (sig2, 0);
    test_signal_assert_hits (sig3, 0);
    g_assert (fixture->split->acc == NULL);
    g_assert (fixture->split->parent == NULL);
    g_assert (fixture->split->orig_parent == NULL);

    fixture->split->acc = acc;
    fixture->split->orig_acc = acc;
    fixture->split->orig_parent = txn1;
    fixture->split->parent = txn2;
    qof_instance_set_destroying (fixture->split, TRUE);

    xaccSplitRollbackEdit (fixture->split);
    g_assert (fixture->split->acc == acc);
    g_assert (fixture->split->parent == txn1);
    g_assert (fixture->split->orig_parent == txn1);
    test_signal_assert_hits (sig1, 1);
    test_signal_assert_hits (sig2, 1);
    test_signal_assert_hits (sig3, 1);
    g_assert (fixture->split->parent == fixture->split->orig_parent);
    g_assert (fixture->split->parent == txn1);

    test_signal_free (sig1);
    test_signal_free (sig2);
    test_signal_free (sig3);
}
/* xaccSplitLookup
Split *
xaccSplitLookup (const GncGUID *guid, QofBook *book)// C: 24 in 9
*/
static void
test_xaccSplitLookup (Fixture *fixture, gconstpointer pData)
{
    QofBook *book = xaccSplitGetBook (fixture->split);
    const GncGUID *guid = xaccSplitGetGUID (fixture->split);
    Split *split = xaccSplitLookup (NULL, book);
    g_assert (split == NULL);
    split = xaccSplitLookup (guid, NULL);
    g_assert (split == NULL);
    split = xaccSplitLookup (guid, book);
    g_assert (split == fixture->split);
}
/* xaccSplitDetermineGainStatus
void
xaccSplitDetermineGainStatus (Split *split)// C: 7 in 2
*/
static void
test_xaccSplitDetermineGainStatus (Fixture *fixture, gconstpointer pData)
{
    guint gains = fixture->split->gains;
    Split *g_split = fixture->split->gains_split;
    const GncGUID *g_guid = xaccSplitGetGUID (g_split);
    g_assert_cmpint (gains, !=, GAINS_STATUS_UNKNOWN);
    xaccSplitDetermineGainStatus (fixture->split);
    g_assert_cmpint (fixture->split->gains, ==, gains);
    g_assert (fixture->split->gains_split == g_split);

    g_assert (g_split != NULL);
    fixture->split->gains = GAINS_STATUS_UNKNOWN;
    xaccSplitDetermineGainStatus (fixture->split);
    g_assert_cmpint (fixture->split->gains, ==, GAINS_STATUS_A_VDIRTY | GAINS_STATUS_DATE_DIRTY);
    g_assert (fixture->split->gains_split == g_split);

    fixture->split->gains = GAINS_STATUS_UNKNOWN;
    fixture->split->gains_split = NULL;
    g_assert (kvp_frame_get_slot (fixture->split->inst.kvp_data, "gains_source") == NULL);
    xaccSplitDetermineGainStatus (fixture->split);
    g_assert (fixture->split->gains_split == NULL);
    g_assert_cmpint (fixture->split->gains, ==, GAINS_STATUS_A_VDIRTY | GAINS_STATUS_DATE_DIRTY);

    kvp_frame_set_guid (fixture->split->inst.kvp_data, "gains-source", g_guid);
    g_assert (fixture->split->gains_split == NULL);
    fixture->split->gains = GAINS_STATUS_UNKNOWN;
    xaccSplitDetermineGainStatus (fixture->split);
    g_assert (fixture->split->gains_split == g_split);
    g_assert_cmpint (fixture->split->gains, ==, GAINS_STATUS_GAINS);
}
/* get_currency_denom
static inline int
get_currency_denom(const Split * s)// Local: 6:0:0
*/
static void
test_get_currency_denom (Fixture *fixture, gconstpointer pData)
{
    Transaction *txn = fixture->split->parent;
    const gint denom = gnc_commodity_get_fraction (fixture->curr);
    g_assert_cmpint (fixture->func->get_currency_denom (NULL), ==, 0);
    fixture->split->parent = NULL;
    g_assert_cmpint (fixture->func->get_currency_denom (fixture->split), ==, 100000);
    fixture->split->parent = txn;
    txn->common_currency = NULL;
    g_assert_cmpint (fixture->func->get_currency_denom (fixture->split), ==, 100000);
    txn->common_currency = fixture->curr;
    g_assert_cmpint (fixture->func->get_currency_denom (fixture->split), ==, denom);
}
/* get_commodity_denom
static inline int
get_commodity_denom(const Split * s)// Local: 5:0:0
*/
static void
test_get_commodity_denom (Fixture *fixture, gconstpointer pData)
{
    Account *acc = fixture->split->acc;
    const gint denom = gnc_commodity_get_fraction (fixture->comm);
    g_assert_cmpint (fixture->func->get_commodity_denom (NULL), ==, 0);
    fixture->split->acc = NULL;
    g_assert_cmpint (fixture->func->get_commodity_denom (fixture->split), ==, 100000);
    fixture->split->acc = acc;
    g_assert_cmpint (fixture->func->get_commodity_denom (fixture->split), ==, denom);
}
/* xaccSplitSetSharePriceAndAmount
void
xaccSplitSetSharePriceAndAmount (Split *s, gnc_numeric price, gnc_numeric amt)// C: 1
*/
static void
test_xaccSplitSetSharePriceAndAmount (Fixture *fixture, gconstpointer pData)
{
    gnc_numeric price = {678, 100};
    gnc_numeric amt = {10000, 1000};

    xaccSplitSetSharePriceAndAmount (fixture->split, price, amt);
    g_assert_cmpint (fixture->split->value.num, ==, 16272);
    g_assert_cmpint (fixture->split->amount.num, ==, 10000);
    g_assert (qof_instance_is_dirty (QOF_INSTANCE (fixture->split)));
    price.denom = 1000;

    xaccSplitSetSharePriceAndAmount (fixture->split, price, amt);
    g_assert_cmpint (fixture->split->value.num, ==, 1627);
    g_assert_cmpint (fixture->split->amount.num, ==, 10000);
    g_assert (qof_instance_is_dirty (QOF_INSTANCE (fixture->split)));
}
/* Used as a QofObject setter. Does the same as xaccSplitSetSharePrice
 * without the beginEdit/commitEdit and marking dirty.
 */
/* qofSplitSetSharePrice
static void
qofSplitSetSharePrice (Split *split, gnc_numeric price)//
*/
/* xaccSplitSetSharePrice
void
xaccSplitSetSharePrice (Split *s, gnc_numeric price)// C: 2 in 1
*/
static void
test_xaccSplitSetSharePrice (Fixture *fixture, gconstpointer pData)
{
    gnc_numeric price = {678, 100};

    xaccSplitSetSharePrice (fixture->split, price);
    g_assert_cmpint (fixture->split->value.num, ==, 522);
    g_assert_cmpint (fixture->split->amount.num, ==, 321);
    g_assert (qof_instance_is_dirty (QOF_INSTANCE (fixture->split)));
}
/* Used as a QofObject setter. Does the same as xaccSplitSetAmount
 * without the beginEdit/commitEdit and marking dirty.
 */
/* qofSplitSetAmount
static void
qofSplitSetAmount (Split *split, gnc_numeric amt)//
*/
/* xaccSplitSetAmount
void
xaccSplitSetAmount (Split *s, gnc_numeric amt)// C: 37 in 19 SCM: 18 in 2 Local: 4:0:0
*/
static void
test_xaccSplitSetAmount (Fixture *fixture, gconstpointer pData)
{
    gnc_numeric amt = {10000, 1000};

    xaccSplitSetAmount (fixture->split, amt);
    g_assert_cmpint (fixture->split->value.num, ==, 123);
    g_assert_cmpint (fixture->split->amount.num, ==, 10000);
    g_assert (qof_instance_is_dirty (QOF_INSTANCE (fixture->split)));
}
/* Used as a QofObject setter. Does the same as xaccSplitSetValue
 * without the beginEdit/commitEdit and marking dirty.
 */
/* qofSplitSetValue
static void
qofSplitSetValue (Split *split, gnc_numeric amt)//
*/
/* xaccSplitSetValue
void
xaccSplitSetValue (Split *s, gnc_numeric amt)// C: 43 in 18 SCM: 18 in 2 Local: 4:0:0
*/
static void
test_xaccSplitSetValue (Fixture *fixture, gconstpointer pData)
{
    gnc_numeric value = {678, 100};

    xaccSplitSetValue (fixture->split, value);
    g_assert_cmpint (fixture->split->value.num, ==, 1627);
    g_assert_cmpint (fixture->split->amount.num, ==, 321);
    g_assert (qof_instance_is_dirty (QOF_INSTANCE (fixture->split)));
}
/* xaccSplitGetBalance // C: 8 in 3 SCM: 4 in 3
 * xaccSplitGetClearedBalance // Not Used
 * xaccSplitGetReconciledBalance // Not Used
 Simple getters. No test.
*/
/* xaccSplitSetBaseValue
void
xaccSplitSetBaseValue (Split *s, gnc_numeric value,// C: 19 in 7
*/
static void
test_xaccSplitSetBaseValue (Fixture *fixture, gconstpointer pData)
{
    QofBook *book = xaccSplitGetBook (fixture->split);
    gnc_commodity *gnaira = gnc_commodity_new (book, "Gnaira", "CURRENCY", "GNA", "", 100);
    Account *acc = fixture->split->acc;
    gchar *msg1 = "[xaccSplitSetBaseValue()] split must have a parent account";
    gchar *fmt = "[xaccSplitSetBaseValue()] inappropriate base currency %s "
                 "given split currency=%s and commodity=%s\n";
    gchar *logdomain = "gnc.engine";
    GLogLevelFlags loglevel = static_cast<GLogLevelFlags>(G_LOG_LEVEL_CRITICAL | G_LOG_FLAG_FATAL);
    TestErrorStruct check = { loglevel, logdomain, msg1, 0 };
    gnc_numeric value = { 360, 240 };
    gnc_numeric old_val = fixture->split->value;
    gnc_numeric old_amt = fixture->split->amount;

    GLogFunc oldlogger = g_log_set_default_handler ((GLogFunc)test_null_handler, &check);
    g_test_log_set_fatal_handler ((GTestLogFatalFunc)test_checked_handler,
                                  &check);
    /* No parent account */
    fixture->split->acc = NULL;
    xaccSplitSetBaseValue (fixture->split, value, fixture->comm);
    g_assert (!qof_instance_is_dirty (QOF_INSTANCE (fixture->split)));
    xaccTransRollbackEdit (fixture->split->parent);
    g_assert_cmpint (check.hits, ==, 1);
    /* Base currency ==  currency, != commodity */
    fixture->split->acc = acc;
    xaccSplitSetBaseValue (fixture->split, value, fixture->curr);
    g_assert_cmpint (fixture->split->value.num, ==, 360);
    g_assert_cmpint (fixture->split->value.denom, ==, 240);
    g_assert_cmpint (fixture->split->amount.num, ==, 321);
    g_assert_cmpint (fixture->split->amount.denom, ==, 1000);
    g_assert (qof_instance_is_dirty (QOF_INSTANCE (fixture->split)));
    g_assert_cmpint (check.hits, ==, 1);

    /* Base currency == currency, == commodity */
    qof_instance_mark_clean (QOF_INSTANCE (fixture->split));
    fixture->split->value = old_val;
    fixture->split->amount = old_amt;
    xaccAccountSetCommodity(fixture->split->acc, fixture->curr);
    xaccSplitSetBaseValue (fixture->split, value, fixture->curr);
//    g_assert (gnc_numeric_equal (fixture->split->amount, old_amt));
    g_assert_cmpint (fixture->split->value.num, ==, 360);
    g_assert_cmpint (fixture->split->value.denom, ==, 240);
    g_assert_cmpint (fixture->split->amount.num, ==, 360);
    g_assert_cmpint (fixture->split->amount.denom, ==, 240);
    g_assert (qof_instance_is_dirty (QOF_INSTANCE (fixture->split)));
    g_assert_cmpint (check.hits, ==, 1);
    /* Base currency != currency, == commodity */
    qof_instance_mark_clean (QOF_INSTANCE (fixture->split));
    fixture->split->value = old_val;
    fixture->split->amount = old_amt;
    xaccAccountSetCommodity(fixture->split->acc, fixture->comm);
    xaccSplitSetBaseValue (fixture->split, value, fixture->comm);
    g_assert (gnc_numeric_equal (fixture->split->value, old_val));
    g_assert_cmpint (fixture->split->amount.num, ==, 1500);
    g_assert_cmpint (fixture->split->amount.denom, ==, 1000);
    g_assert (qof_instance_is_dirty (QOF_INSTANCE (fixture->split)));
    g_assert_cmpint (check.hits, ==, 1);

    /* Base currency != currency, != commodity */
    check.msg = g_strdup_printf (fmt, gnc_commodity_get_printname (gnaira),
                                 gnc_commodity_get_printname (fixture->curr),
                                 gnc_commodity_get_printname (fixture->comm));

    qof_instance_mark_clean (QOF_INSTANCE (fixture->split));
    fixture->split->amount = old_amt;
    fixture->split->amount = old_val;
    fixture->split->orig_parent = fixture->split->parent;
    xaccAccountSetCommodity(fixture->split->acc, fixture->comm);
    xaccSplitSetBaseValue (fixture->split, value, gnaira);
    g_assert (!qof_instance_is_dirty (QOF_INSTANCE (fixture->split)));
    g_assert_cmpint (check.hits, ==, 2);

    g_free (check.msg);
    g_log_set_default_handler (oldlogger, NULL);
    test_destroy (gnaira);
}
// Not Used
/* xaccSplitGetBaseValue
gnc_numeric
xaccSplitGetBaseValue (const Split *s, const gnc_commodity * base_currency)//
*/
/* xaccSplitConvertAmount
gnc_numeric
xaccSplitConvertAmount (const Split *split, const Account * account)// C: 1
*/
static void
test_xaccSplitConvertAmount (void)
{
    QofBook *book = qof_book_new ();
    gnc_commodity *gnaira = gnc_commodity_new (book, "Gnaira", "CURRENCY",
                            "GNA", "", 240);
    gnc_commodity *gncxx = gnc_commodity_new (book, "Wildebeest Fund", "FUND",
                           "GNCXX", "", 1000);
    gnc_commodity *gnm = gnc_commodity_new (book, "Gnome, Inc.", "NYSE",
                                            "GNM", "", 1000);
    gchar guidstr[GUID_ENCODING_LENGTH+1];

    Account *acc = xaccMallocAccount (book);
    Account *o_acc = xaccMallocAccount (book);
    Account *ya_acc = xaccMallocAccount (book);
    Transaction *txn = xaccMallocTransaction (book);
    Split *split = xaccMallocSplit (book);
    Split *o_split = xaccMallocSplit (book);

    gnc_numeric gnaira_amt = gnc_numeric_create (53214, 240);
    gnc_numeric gncxx_amt = gnc_numeric_create (300000, 1000);
    gnc_numeric result;

    gchar *logdomain = "gnc.engine";
    GLogLevelFlags loglevel = static_cast<GLogLevelFlags>(G_LOG_LEVEL_CRITICAL | G_LOG_FLAG_FATAL);
    TestErrorStruct check = { loglevel, logdomain, NULL, 0 };
    GLogFunc oldlogger = g_log_set_default_handler ((GLogFunc)test_null_handler, &check);

    guid_to_string_buff(xaccSplitGetGUID(o_split), guidstr);
    check.msg = g_strdup_printf ("[xaccSplitConvertAmount()] The split's (%s) amount can't be converted from GNCXX into GNM.", guidstr);
    xaccAccountSetCommodity (acc, gnaira);
    xaccAccountSetCommodity (o_acc, gnaira);
    xaccAccountSetCommodity (ya_acc, gnm);
    xaccTransBeginEdit (txn);
    xaccTransSetCurrency (txn, gnaira);
    xaccSplitSetParent (split, txn);
    xaccSplitSetAccount (split, acc);
    xaccSplitSetParent (o_split, txn);
    xaccSplitSetAccount (o_split, o_acc);
    split->amount = gnaira_amt;
    split->value = gnaira_amt;
    o_split->amount = gnc_numeric_neg (gnaira_amt);
    o_split->value = gnc_numeric_neg (gnaira_amt);

    /* accounts are equal, return the amount */
    xaccTransCommitEdit (txn);
    result = xaccSplitConvertAmount (split, acc);
    g_assert (gnc_numeric_equal (result, gnaira_amt));

    /* commodities are equal, return the amount */
    result = xaccSplitConvertAmount (split, o_acc);
    g_assert (gnc_numeric_equal (result, gnaira_amt));

    /* commodities are different, but transaction is balanced: Returns the
     * value of o_split
     */
    o_split->amount = gncxx_amt;
    xaccAccountSetCommodity (o_acc, gncxx);
    result = xaccSplitConvertAmount (split, o_acc);
    g_assert_cmpint (result.num, ==, -300000);
    g_assert_cmpint (result.denom, ==, 1000);

    /* commodities are different, transaction is balanced, account isn't
     * the balancing account. Raises error and returns 0,0
     */
    g_test_log_set_fatal_handler ((GTestLogFatalFunc)test_checked_handler,
                                  &check);
    result = xaccSplitConvertAmount (split, ya_acc);
    g_assert (gnc_numeric_zero_p (result));
    g_assert_cmpint (check.hits, ==, 1);

    /* Transaction isn't balanced, split has 0 value, returns that */
    split->value = gnc_numeric_zero ();
    result = xaccSplitConvertAmount (split, o_acc);
    g_assert (gnc_numeric_zero_p (result));
    g_assert_cmpint (check.hits, ==, 1);

    /* Transaction isn't balanced, compute a conversion */
    split->value = gnc_numeric_create (71330, 240);
    result = xaccSplitConvertAmount (split, o_acc);
    g_assert_cmpint (result.num, ==, -402131);
    g_assert_cmpint (result.denom, ==, 1000);
    g_assert_cmpint (check.hits, ==, 1);

    test_destroy (split);
    test_destroy (acc);
    test_destroy (o_split);
    test_destroy (o_acc);
    test_destroy (ya_acc);
    test_destroy (gnaira);
    test_destroy (gncxx);
    test_destroy (gnm);
    g_log_set_default_handler (oldlogger, NULL);
    g_free (check.msg);
}
/* xaccSplitDestroy
gboolean
xaccSplitDestroy (Split *split)// C: 17 in 9 SCM: 1
*/
static void
notify_destroy (gpointer pdata, GObject *obj)
{
    gboolean *data = (gboolean*)pdata;
    if (! (*data)) *data = TRUE;
}

static void
test_xaccSplitDestroy ()
{
    QofBook *book = qof_book_new ();
    gnc_commodity *gnaira = gnc_commodity_new (book, "Gnaira", "CURRENCY",
                            "GNA", "", 240);
    Account *acc = xaccMallocAccount (book);
    Transaction *txn = xaccMallocTransaction (book);
    Split *split1 = xaccMallocSplit (book);
    Split *split2 = xaccMallocSplit (book);
    gboolean is_destroyed = FALSE;

    xaccAccountSetCommodity (acc, gnaira);
    xaccSplitSetAccount (split1, acc);
    xaccSplitSetAccount (split2, acc);
    xaccTransBeginEdit (txn);
    xaccTransSetCurrency (txn, gnaira);
    xaccSplitSetParent (split1, txn);
    xaccSplitSetParent (split2, txn);
    xaccTransCommitEdit (txn);
    g_object_weak_ref (G_OBJECT (split1), notify_destroy, &is_destroyed);

    xaccSplitDestroy (split1);

    g_assert_cmpint (is_destroyed, ==, TRUE);

    test_destroy (split2);
    test_destroy (txn);
    test_destroy (acc);
    test_destroy (gnaira);
    test_destroy (book);
}
/* xaccSplitOrder
gint
xaccSplitOrder (const Split *sa, const Split *sb)// C: 5 in 3
*/
static void
test_xaccSplitOrder (Fixture *fixture, gconstpointer pData)
{
    const char *slot_path;
    Split *split = fixture->split;
    QofBook *book = xaccSplitGetBook (split);
    Split *o_split = xaccMallocSplit (book);
    Transaction *o_txn = xaccMallocTransaction (book);
    Transaction *txn = split->parent;

    g_assert_cmpint (xaccSplitOrder (split, split), ==, 0);
    g_assert_cmpint (xaccSplitOrder (NULL, split), ==, -1);
    g_assert_cmpint (xaccSplitOrder (split, NULL), ==, 1);
    /* This is testing the call to xaccTransOrder: split has a parent and
     * o_split doesn't, so xaccTransOrder returns -1.
     */
    g_assert_cmpint (xaccSplitOrder (split, o_split), ==, -1);

    /* This is testing the call to xaccTransOrder_num_action: split and o_split have
     * parents with the same date_posted so will sort on tran-num or
     * split-action based on book option.
     */
    o_split->parent = o_txn;
    split->parent->date_posted = timespec_now ();
    o_split->parent->date_posted = split->parent->date_posted;

    /* The book_use_split_action_for_num_field book option hasn't been set so it
     * should sort on tran-num, so xaccTransOrder_num_action returns -1.
     */
    split->parent->num = "123";
    split->action = "5";
    o_split->parent->num = "124";
    o_split->action = "6";
    g_assert_cmpint (xaccSplitOrder (split, o_split), ==, -1);

    /* Reverse, so xaccTransOrder_num_action returns +1.
     */
    split->parent->num = "124";
    o_split->parent->num = "123";
    g_assert_cmpint (xaccSplitOrder (split, o_split), ==, +1);

    /* Now set the book_use_split_action_for_num_field book option so it will
     * sort on split-action, so xaccTransOrder_num_action returns -1, initially.
     */

    /* create correct slot path */
    g_test_message( "Testing with use-split-action-for-num set to true - t" );
    qof_book_begin_edit (book);
    qof_instance_set (QOF_INSTANCE (book),
		      "split-action-num-field", "t",
		      NULL);
    qof_book_commit_edit (book);
    g_assert(qof_book_use_split_action_for_num_field(xaccSplitGetBook(split)) == TRUE);

    g_assert_cmpint (xaccSplitOrder (split, o_split), ==, -1);

    split->action = "7";
    g_assert_cmpint (xaccSplitOrder (split, o_split), ==, +1);

    /* Revert settings for the rest of the test */
    o_split->action = NULL;
    split->action = "foo";
    o_split->parent = NULL;
    qof_book_begin_edit (book);
    qof_instance_set (QOF_INSTANCE (book),
		      "split-action-num-field", "f",
		      NULL);
    qof_book_commit_edit (book);
    g_assert(qof_book_use_split_action_for_num_field(xaccSplitGetBook(split)) == FALSE);
    split->parent = NULL;
    /* This should return > 0 because o_split has no memo string */
    g_assert_cmpint (xaccSplitOrder (split, o_split), >, 0);
    o_split->memo = "baz";
    g_assert_cmpint (xaccSplitOrder (split, o_split), <, 0);
    /* This should return > 0 because o_split has no action string */
    o_split->memo = split->memo;
    g_assert_cmpint (xaccSplitOrder (split, o_split), >, 0);
    o_split->action = "waldo";
    g_assert_cmpint (xaccSplitOrder (split, o_split), <, 0);

    o_split->action = split->action;
    o_split->reconciled = NREC;
    g_assert_cmpint (xaccSplitOrder (split, o_split), ==, 1);
    split->reconciled = CREC;
    g_assert_cmpint (xaccSplitOrder (split, o_split), ==, -1);

    split->reconciled = o_split->reconciled = YREC;
    o_split->amount = gnc_numeric_create (300, 1000);
    g_assert_cmpint (xaccSplitOrder (split, o_split), ==, 1);
    o_split->amount = gnc_numeric_create (400, 1000);
    g_assert_cmpint (xaccSplitOrder (split, o_split), ==, -1);

    o_split->amount = split->amount;
    o_split->value = gnc_numeric_create (100, 240);
    g_assert_cmpint (xaccSplitOrder (split, o_split), ==, 1);
    o_split->value = gnc_numeric_create (200, 240);
    g_assert_cmpint (xaccSplitOrder (split, o_split), ==, -1);

    o_split->value = split->value;
    /* Make sure that it doesn't crash if o_split->date_reconciled == NULL */
    g_assert_cmpint (xaccSplitOrder (split, o_split), ==, 1);
    o_split->date_reconciled = timespec_now();
    o_split->date_reconciled.tv_sec -= 50;
    g_assert_cmpint (xaccSplitOrder (split, o_split), ==, 1);
    o_split->date_reconciled.tv_sec += 100;
    g_assert_cmpint (xaccSplitOrder (split, o_split), ==, -1);

    o_split->date_reconciled.tv_sec = split->date_reconciled.tv_sec;
    o_split->date_reconciled.tv_nsec = split->date_reconciled.tv_nsec;

    g_assert_cmpint (xaccSplitOrder (split, o_split), ==,
                     qof_instance_guid_compare (split, o_split));

    /* so that it won't assert during teardown */
    split->parent = txn;
    test_destroy (o_split);
    test_destroy (o_txn);
}
/* xaccSplitOrderDateOnly
gint
xaccSplitOrderDateOnly (const Split *sa, const Split *sb)// C: 2 in 1
*/
static void
test_xaccSplitOrderDateOnly (Fixture *fixture, gconstpointer pData)
{
    /* Doesn't do what you'd first think: It orders based on the
     * transaction date posted.
     */
    Split *split = fixture->split;
    Split *o_split = xaccMallocSplit (xaccSplitGetBook (split));
    Transaction *txn = split->parent;
    Transaction *o_txn = xaccMallocTransaction (xaccSplitGetBook (split));

    g_assert_cmpint (xaccSplitOrderDateOnly (split, split), ==, 0);
    g_assert_cmpint (xaccSplitOrderDateOnly (NULL, split), ==, -1);
    g_assert_cmpint (xaccSplitOrderDateOnly (split, NULL), ==, 1);
    /* This is testing the call to xaccTransOrder: split has a parent and
     * o_split doesn't, so xaccTransOrder returns -1.
     */
    o_split->parent = NULL;
    g_assert_cmpint (xaccSplitOrderDateOnly (split, o_split), ==, -1);
    split->parent = NULL;
    g_assert_cmpint (xaccSplitOrderDateOnly (split, o_split), ==, 0);
    o_split->parent = o_txn;
    g_assert_cmpint (xaccSplitOrderDateOnly (split, o_split), ==, 1);
    split->parent = txn;

    txn->date_posted = timespec_now ();
    o_txn->date_posted = timespec_now ();
    o_txn->date_posted.tv_sec -= 50;
    g_assert_cmpint (xaccSplitOrderDateOnly (split, o_split), ==, 1);
    o_txn->date_posted.tv_sec += 100;
    g_assert_cmpint (xaccSplitOrderDateOnly (split, o_split), ==, -1);
    o_txn->date_posted.tv_sec -= 50;
    g_assert_cmpint (xaccSplitOrderDateOnly (split, o_split), ==, -1);

    test_destroy (o_split);
    test_destroy (o_txn);
}
/* get_corr_account_split
static gboolean
get_corr_account_split(const Split *sa, const Split **retval)// Local: 3:0:0
*/
static void
test_get_corr_account_split (Fixture *fixture, gconstpointer pData)
{
    QofBook *book = xaccSplitGetBook (fixture->split);
    Transaction *txn = fixture->split->parent;
    Split *split1 = xaccMallocSplit (book);
    Split *split2 = xaccMallocSplit (book);
    Split *split3 = xaccMallocSplit (book);
    const Split *result = NULL;
    Account *acc1 = xaccMallocAccount (book);
    Account *acc2 = xaccMallocAccount (book);
    Account *acc3 = xaccMallocAccount (book);
#ifdef __clang__
#define _func "gboolean get_corr_account_split(const Split *, const Split **)"
#else
#define _func "get_corr_account_split"
#endif
    gchar *msg = _func ": assertion " _Q "sa' failed";
#undef _func
    GLogLevelFlags loglevel = static_cast<GLogLevelFlags>(G_LOG_LEVEL_CRITICAL | G_LOG_FLAG_FATAL);
    TestErrorStruct *check = test_error_struct_new ("gnc.engine",
                             loglevel, msg);
    fixture->hdlrs = test_log_set_fatal_handler (fixture->hdlrs, check,
                     (GLogFunc)test_checked_handler);

    xaccAccountSetCommodity (acc1, fixture->curr);
    xaccAccountSetCommodity (acc2, fixture->curr);
    xaccAccountSetCommodity (acc3, fixture->curr);

    split1->acc = acc1;
    split2->acc = acc2;
    split3->acc = acc3;

    split1->value = gnc_numeric_create (456, 240);
    split2->value = gnc_numeric_neg (fixture->split->value);
    split3->value = gnc_numeric_neg (split1->value);

    g_assert (!fixture->func->get_corr_account_split(fixture->split, &result));
    g_assert (result == NULL);

    xaccTransBeginEdit (txn);
    xaccSplitSetParent (split2, txn);
    xaccTransCommitEdit (txn);

    g_assert (fixture->func->get_corr_account_split(fixture->split, &result));
    g_assert (result == split2);

    xaccTransBeginEdit (txn);
    xaccSplitSetParent (split1, txn);
    xaccSplitSetParent (split3, txn);
    xaccTransCommitEdit (txn);

    g_assert (!fixture->func->get_corr_account_split(fixture->split, &result));
    g_assert (result == NULL);
    g_assert_cmpint (check->hits, ==, 0);

    g_assert (!fixture->func->get_corr_account_split(NULL, &result));
    g_assert (result == NULL);
    g_assert_cmpint (check->hits, ==, 1);

    test_destroy (split1);
    test_destroy (split2);
    test_destroy (split3);
    test_destroy (acc1);
    test_destroy (acc2);
    test_destroy (acc3);
}
// Not Used
/* xaccSplitGetCorrAccountName
const char *
xaccSplitGetCorrAccountName(const Split *sa)//
*/
/* xaccSplitGetCorrAccountFullName
char *
xaccSplitGetCorrAccountFullName(const Split *sa)// SCM: 1  Local: 2:0:0
*/
static void
test_xaccSplitGetCorrAccountFullName (Fixture *fixture, gconstpointer pData)
{
    QofBook *book = xaccSplitGetBook (fixture->split);
    Transaction *txn = fixture->split->parent;
    Split *split1 = xaccMallocSplit (book);
    Account *acc0 = xaccMallocAccount (book);
    Account *acc1 = xaccMallocAccount (book);
    Account *acc2 = xaccMallocAccount (book);

    gchar *name1 = "waldo", *name2 = "pepper", *result;
    gchar *err = "-- Split Transaction --";

    xaccAccountSetCommodity (acc2, fixture->curr);
    gnc_account_append_child (acc1, acc2);
    gnc_account_append_child (acc0, acc1);
    split1->acc = acc2;
    split1->value = gnc_numeric_neg (fixture->split->value);
    xaccAccountSetName (acc1, name1);
    xaccAccountSetName (acc2, name2);

    result = xaccSplitGetCorrAccountFullName (fixture->split);
    g_assert_cmpstr (result, ==, err);
    g_free (result);

    xaccTransBeginEdit (txn);
    xaccSplitSetParent (split1, txn);
    xaccTransCommitEdit (txn);

    result = xaccSplitGetCorrAccountFullName (fixture->split);
    g_assert_cmpstr (result, ==, gnc_account_get_full_name (acc2));
    g_free (result);

    test_destroy (split1);
    test_destroy (acc2);
    test_destroy (acc1);
    test_destroy (acc0);

}
// Make Static
/* xaccSplitGetCorrAccountCode
const char *
xaccSplitGetCorrAccountCode(const Split *sa)// Local: 2:0:0
*/
static void
test_xaccSplitGetCorrAccountCode (Fixture *fixture, gconstpointer pData)
{
    QofBook *book = xaccSplitGetBook (fixture->split);
    Transaction *txn = fixture->split->parent;
    Split *split1 = xaccMallocSplit (book);
    Account *acc1 = xaccMallocAccount (book);

    gchar *code = "foo";
    gchar *err = "Split";

    xaccAccountSetCommodity (acc1, fixture->curr);
    split1->acc = acc1;
    split1->value = gnc_numeric_neg (fixture->split->value);
    xaccAccountSetCode (acc1, code);

    g_assert_cmpstr (xaccSplitGetCorrAccountCode (fixture->split), ==, err);

    xaccTransBeginEdit (txn);
    xaccSplitSetParent (split1, txn);
    xaccTransCommitEdit (txn);

    g_assert_cmpstr (xaccSplitGetCorrAccountCode (fixture->split), ==, code);

    test_destroy (split1);
    test_destroy (acc1);
}
/* xaccSplitCompareAccountFullNames
int
xaccSplitCompareAccountFullNames(const Split *sa, const Split *sb)// SCM: 1
*/
static void
test_xaccSplitCompareAccountFullNames (Fixture *fixture, gconstpointer pData)
{
    QofBook *book = xaccSplitGetBook (fixture->split);
    Transaction *txn = fixture->split->parent;
    Split *split = fixture->split;
    Split *split1 = xaccMallocSplit (book);
    Account *acc0 = xaccMallocAccount (book);
    Account *acc1 = xaccMallocAccount (book);
    Account *acc2 = xaccMallocAccount (book);

    gchar *name1 = "waldo", *name2 = "pepper";

    xaccAccountSetCommodity (acc2, fixture->curr);
    gnc_account_append_child (acc1, acc2);
    gnc_account_append_child (acc0, acc1);
    gnc_account_append_child (acc1, fixture->split->acc);
    split1->acc = acc2;
    split1->value = gnc_numeric_neg (fixture->split->value);
    xaccAccountSetName (acc1, name1);
    xaccAccountSetName (acc2, name2);

    g_assert_cmpint (xaccSplitCompareAccountFullNames (NULL, NULL), ==, 0);
    g_assert_cmpint (xaccSplitCompareAccountFullNames (split, NULL), ==, 1);
    g_assert_cmpint (xaccSplitCompareAccountFullNames (NULL, split), ==, -1);
    g_assert_cmpint (xaccSplitCompareAccountFullNames (split, split1), <, 0);

    xaccTransBeginEdit (txn);
    xaccSplitSetParent (split1, txn);
    xaccTransCommitEdit (txn);

    test_destroy (split1);
    test_destroy (acc2);
    test_destroy (acc1);
    test_destroy (acc0);
}
/* xaccSplitCompareAccountCodes
int
xaccSplitCompareAccountCodes(const Split *sa, const Split *sb)// SCM: 1
*/
static void
test_xaccSplitCompareAccountCodes (Fixture *fixture, gconstpointer pData)
{
    QofBook *book = xaccSplitGetBook (fixture->split);
    Account *acc = fixture->split->acc;
    Account *acc1 = xaccMallocAccount (book);
    Split *split = fixture->split;
    Split *split1 = xaccMallocSplit (book);

    gnc_commodity *gnaira = gnc_commodity_new (book, "Gnaira", "CURRENCY",
                            "GNA", "", 240);
    xaccAccountSetCommodity (fixture->split->acc, gnaira);
    xaccAccountSetCommodity (acc1, gnaira);
    split1->acc = acc1;
    split1->value = gnc_numeric_neg (fixture->split->value);
    xaccAccountSetCode (acc1, "bar");
    xaccAccountSetCode (acc, "foo");

    g_assert_cmpint (xaccSplitCompareAccountCodes (NULL, NULL), ==, 0);
    g_assert_cmpint (xaccSplitCompareAccountCodes (split, NULL), ==, 1);
    g_assert_cmpint (xaccSplitCompareAccountCodes (NULL, split), ==, -1);
    g_assert_cmpint (xaccSplitCompareAccountCodes (split, split1), >, 0);

    test_destroy (split1);
    test_destroy (acc1);
    test_destroy (gnaira);
}
/* xaccSplitCompareOtherAccountFullNames
int
xaccSplitCompareOtherAccountFullNames(const Split *sa, const Split *sb)// SCM: 1
*/
static void
test_xaccSplitCompareOtherAccountFullNames (Fixture *fixture, gconstpointer pData)
{
    QofBook *book = xaccSplitGetBook (fixture->split);
    Transaction *txn = fixture->split->parent;
    Transaction *txn1 = xaccMallocTransaction (book);
    Split *split = fixture->split;
    Split *split1 = xaccMallocSplit (book);
    Split *split2 = xaccMallocSplit (book);
    Split *split3 = xaccMallocSplit (book);
    Account *acc = fixture->split->acc;
    Account *acc0 = xaccMallocAccount (book);
    Account *acc1 = xaccMallocAccount (book);
    Account *acc2 = xaccMallocAccount (book);

    gchar *name1 = "waldo", *name2 = "pepper";

    xaccAccountSetCommodity (acc2, fixture->curr);
    xaccAccountSetCommodity (acc1, fixture->curr);
    gnc_account_append_child (acc1, acc2);
    gnc_account_append_child (acc0, acc1);
    gnc_account_append_child (acc1, fixture->split->acc);
    xaccAccountSetName (acc1, name1);
    xaccAccountSetName (acc2, name2);
    split2->acc = acc1;
    split3->acc = acc;
    split1->value = gnc_numeric_create (456, 240);
    split2->value = gnc_numeric_neg (fixture->split->value);
    split3->value = gnc_numeric_neg (split1->value);

    xaccTransBeginEdit (txn);
    xaccSplitSetParent (split2, txn);
    xaccTransCommitEdit (txn);

    xaccTransBeginEdit (txn1);
    xaccTransSetCurrency (txn1, fixture->curr);
    xaccSplitSetParent (split1, txn1);
    xaccSplitSetParent (split3, txn1);
    xaccTransCommitEdit (txn1);

    g_assert_cmpint (xaccSplitCompareOtherAccountFullNames (NULL, NULL), ==, 0);
    g_assert_cmpint (xaccSplitCompareOtherAccountFullNames (split, NULL), ==, 1);
    g_assert_cmpint (xaccSplitCompareOtherAccountFullNames (NULL, split), ==, -1);
    g_assert_cmpint (xaccSplitCompareOtherAccountFullNames (split, split1), <, 0);

    xaccTransBeginEdit (txn);
    xaccSplitSetParent (split1, txn);
    xaccTransCommitEdit (txn);

    test_destroy (split1);
    test_destroy (split2);
    test_destroy (split3);
    test_destroy (txn1);
    test_destroy (acc2);
    test_destroy (acc1);
    test_destroy (acc0);
}
/* xaccSplitCompareOtherAccountCodes
int
xaccSplitCompareOtherAccountCodes(const Split *sa, const Split *sb)// SCM: 1
*/
static void
test_xaccSplitCompareOtherAccountCodes (Fixture *fixture, gconstpointer pData)
{
    QofBook *book = xaccSplitGetBook (fixture->split);
    Account *acc = fixture->split->acc;
    Account *acc1 = xaccMallocAccount (book);
    Transaction *txn = fixture->split->parent;
    Transaction *txn1 = xaccMallocTransaction (book);
    Split *split = fixture->split;
    Split *split1 = xaccMallocSplit (book);
    Split *split2 = xaccMallocSplit (book);
    Split *split3 = xaccMallocSplit (book);

    xaccAccountSetCommodity (acc1, fixture->curr);
    split2->acc = acc1;
    split3->acc = acc;
    split1->value = gnc_numeric_create (456, 240);
    split2->value = gnc_numeric_neg (fixture->split->value);
    split3->value = gnc_numeric_neg (split1->value);
    xaccAccountSetCode (acc1, "bar");
    xaccAccountSetCode (acc, "foo");

    xaccTransBeginEdit (txn);
    xaccSplitSetParent (split2, txn);
    xaccTransCommitEdit (txn);

    xaccTransBeginEdit (txn1);
    xaccTransSetCurrency (txn1, fixture->curr);
    xaccSplitSetParent (split1, txn1);
    xaccSplitSetParent (split3, txn1);
    xaccTransCommitEdit (txn1);

    g_assert_cmpint (xaccSplitCompareOtherAccountCodes (NULL, NULL), ==, 0);
    g_assert_cmpint (xaccSplitCompareOtherAccountCodes (split, NULL), ==, 1);
    g_assert_cmpint (xaccSplitCompareOtherAccountCodes (NULL, split), ==, -1);
    g_assert_cmpint (xaccSplitCompareOtherAccountCodes (split, split1), <, 0);

    test_destroy (split1);
    test_destroy (split2);
    test_destroy (split3);
    test_destroy (txn1);
    test_destroy (acc1);
}

/* xaccSplitSetParent
void
xaccSplitSetParent(Split *s, Transaction *t)// C: 10 in 7 SCM: 6 in 2 Local: 3:0:0
*/
static void
test_xaccSplitSetParent (Fixture *fixture, gconstpointer pData)
{
    QofBook *book = xaccSplitGetBook (fixture->split);
    Transaction *txn = fixture->split->parent;
    Transaction *txn1 = xaccMallocTransaction (book);
    Transaction *txn2 = xaccMallocTransaction (book);
    Split *split = fixture->split;
    TestSignal sig1, sig2;
    gchar *logdomain = "gnc.engine";
    GLogLevelFlags loglevel = static_cast<GLogLevelFlags>(G_LOG_LEVEL_CRITICAL | G_LOG_FLAG_FATAL);
    /* FIXME: This error doesn't actually stop execution, so we need to test for it happening. */
    gchar *msg = "[xaccSplitSetParent()] You may not add the split to more"
                 " than one transaction during the BeginEdit/CommitEdit block.";
    TestErrorStruct check = { loglevel, logdomain, msg, 0 };
    GLogFunc oldlogger = g_log_set_default_handler ((GLogFunc)test_null_handler,
                         &check);
    g_test_log_set_fatal_handler ((GTestLogFatalFunc)test_checked_handler, &check);
    sig1 = test_signal_new (QOF_INSTANCE (txn2), GNC_EVENT_ITEM_ADDED, NULL);
    sig2 = test_signal_new (QOF_INSTANCE (txn), GNC_EVENT_ITEM_REMOVED, NULL);

    xaccTransBeginEdit (txn2);
    xaccTransSetCurrency (txn2, fixture->curr);
    split->orig_parent = txn1;
    xaccSplitSetParent (split, txn2);
    g_assert (split->parent == txn2);
    g_assert (split->orig_parent == txn1);
    test_signal_assert_hits (sig1, 1);
    test_signal_assert_hits (sig2, 1);
    g_assert (qof_instance_is_dirty (QOF_INSTANCE (split)));
    g_assert_cmpint (check.hits, ==, 1);

    test_signal_free (sig1);
    test_signal_free (sig2);
    g_log_set_default_handler (oldlogger, NULL);
    /* txn already destroyed by xaccTransCommitEdit() */
}
/* xaccSplitGetSharePrice
gnc_numeric
xaccSplitGetSharePrice (const Split * split)// C: 3 in 3 SCM: 4 in 3
*/
static void
test_xaccSplitGetSharePrice (Fixture *fixture, gconstpointer pData)
{
    gnc_numeric result, quotient;
    gnc_numeric expected = gnc_numeric_create (1, 1);
    Split *split = fixture->split;
    /* Warning: this is a define in Split.c */
    const guint PRICE_SIGFIGS = 6;
    char *logdomain = "gnc.engine";
    GLogLevelFlags loglevel = static_cast<GLogLevelFlags>(G_LOG_LEVEL_CRITICAL | G_LOG_FLAG_FATAL);
    TestErrorStruct check = { loglevel, logdomain, NULL, 0 };
    guint hdlr = g_log_set_handler (logdomain, loglevel,
                                    (GLogFunc)test_checked_handler, &check);
    GLogFunc oldhandler = g_log_set_default_handler ((GLogFunc)test_null_handler, &check);
    g_test_log_set_fatal_handler ((GTestLogFatalFunc)test_checked_handler,
                                  &check);

    result = xaccSplitGetSharePrice (NULL);
    g_assert (gnc_numeric_equal (result, expected));

    expected = gnc_numeric_div (split->value, split->amount,
                                GNC_DENOM_AUTO,
                                GNC_HOW_DENOM_SIGFIGS (PRICE_SIGFIGS) |
                                GNC_HOW_RND_ROUND_HALF_UP);

    result = xaccSplitGetSharePrice (split);
    g_assert (gnc_numeric_equal (result, expected));
    g_assert_cmpint (check.hits, ==, 0);

    expected = gnc_numeric_create (0, 1);
    split->amount = gnc_numeric_zero ();
    result = xaccSplitGetSharePrice (split);
    g_assert (gnc_numeric_equal (result, expected));
    g_assert_cmpint (check.hits, ==, 0);

    split->value = gnc_numeric_zero ();
    expected = gnc_numeric_create (1, 1);
    result = xaccSplitGetSharePrice (split);
    g_assert (gnc_numeric_equal (result, expected));
    g_assert_cmpint (check.hits, ==, 0);

    /* Now invent some value/ammount pairs which cause numeric errors to test the limits */
/* This one was supposed to overflow, but it doesn't any more.
    split->amount = gnc_numeric_create (987654321, 10);
    split->value = gnc_numeric_create (3, 789304166);
    quotient = gnc_numeric_div (split->value, split->amount,
                                GNC_DENOM_AUTO,
                                GNC_HOW_DENOM_SIGFIGS (PRICE_SIGFIGS) |
                                GNC_HOW_RND_ROUND_HALF_UP);
    check.msg = g_strdup_printf ("[xaccSplitGetSharePrice()] "
                                 "Computing share price failed (%d): [ %"
                                 G_GINT64_FORMAT " / %" G_GINT64_FORMAT
                                 " ] / [ %" G_GINT64_FORMAT " / %"
                                 G_GINT64_FORMAT " ]",
                                 gnc_numeric_check(quotient), split->value.num,
                                 split->value.denom, split->amount.num,
                                 split->amount.denom);
    expected = gnc_numeric_create (0, 1);
    result = xaccSplitGetSharePrice (split);
    g_assert (gnc_numeric_equal (result, expected));
    g_assert_cmpint (check.hits, ==, 2);
    g_free (check.msg);
    */
    split->amount = gnc_numeric_create (987654321, 10);
    split->value = gnc_numeric_create (3, 0);
    quotient = gnc_numeric_div (split->value, split->amount,
                                GNC_DENOM_AUTO,
                                GNC_HOW_DENOM_SIGFIGS (PRICE_SIGFIGS) |
                                GNC_HOW_RND_ROUND_HALF_UP);
    check.msg = g_strdup_printf ("[xaccSplitGetSharePrice()] "
                                 "Computing share price failed (%d): [ %"
                                 G_GINT64_FORMAT " / %" G_GINT64_FORMAT
                                 " ] / [ %" G_GINT64_FORMAT " / %"
                                 G_GINT64_FORMAT " ]",
                                 gnc_numeric_check(quotient), split->value.num,
                                 split->value.denom, split->amount.num,
                                 split->amount.denom);
    expected = gnc_numeric_create (0, 1);
    result = xaccSplitGetSharePrice (split);
    g_assert (gnc_numeric_equal (result, expected));
    g_assert_cmpint (check.hits, ==, 2);
    g_free (check.msg);

    split->amount = gnc_numeric_create (9, 0);
    split->value = gnc_numeric_create (3, 789304166);
    quotient = gnc_numeric_div (split->value, split->amount,
                                GNC_DENOM_AUTO,
                                GNC_HOW_DENOM_SIGFIGS (PRICE_SIGFIGS) |
                                GNC_HOW_RND_ROUND_HALF_UP);
    check.msg = g_strdup_printf ("[xaccSplitGetSharePrice()] "
                                 "Computing share price failed (%d): [ %"
                                 G_GINT64_FORMAT " / %" G_GINT64_FORMAT
                                 " ] / [ %" G_GINT64_FORMAT " / %"
                                 G_GINT64_FORMAT " ]",
                                 gnc_numeric_check(quotient), split->value.num,
                                 split->value.denom, split->amount.num,
                                 split->amount.denom);
    expected = gnc_numeric_create (0, 1);
    result = xaccSplitGetSharePrice (split);
    g_assert (gnc_numeric_equal (result, expected));
    g_assert_cmpint (check.hits, ==, 4);
    g_free (check.msg);

    g_log_remove_handler (logdomain, hdlr);
    g_log_set_default_handler (oldhandler, NULL);

}
/* xaccSplitGetType // C: 4 in 2
 * xaccSplitMakeStockSplit // C: 1
 */
static void
test_xaccSplitMakeStockSplit (Fixture *fixture, gconstpointer pData)
{
    Split *split = fixture->split;
    g_assert_cmpstr (xaccSplitGetType (split), ==, "normal");

    xaccSplitMakeStockSplit (split);
    g_assert_cmpstr (xaccSplitGetType (split), ==, "stock-split");
    g_assert (qof_instance_is_dirty (QOF_INSTANCE (split)));
}
/* xaccSplitGetOtherSplit
Split *
xaccSplitGetOtherSplit (const Split *split)// C: 13 in 7 SCM: 10 in 4 Local: 1:0:0
*/
static void
test_xaccSplitGetOtherSplit (Fixture *fixture, gconstpointer pData)
{
    Split *split = fixture->split;
    Transaction *txn = split->parent;
    QofBook *book = xaccSplitGetBook (split);
    Split *split1 = xaccMallocSplit (book);
    Split *split2 = xaccMallocSplit (book);
    Account *acc2 = xaccMallocAccount (book);
    KvpValue *kvptrue = kvp_value_new_string ("t");

    g_assert (xaccSplitGetOtherSplit (NULL) == NULL);
    g_assert (xaccSplitGetOtherSplit (split1) == NULL);

    g_assert (xaccTransUseTradingAccounts (txn) == FALSE);
    g_assert (kvp_frame_get_slot (split->inst.kvp_data, "lot-split") == NULL);
    g_assert_cmpint (xaccTransCountSplits (txn), !=, 2);
    g_assert (xaccSplitGetOtherSplit (split) == NULL);

    split1->value = gnc_numeric_neg (split->value);
    xaccSplitSetParent (split1, txn);
    g_assert (xaccSplitGetOtherSplit (split) == split1);

    xaccSplitSetParent (split2, txn);
    g_assert (xaccSplitGetOtherSplit (split) == NULL);

    kvp_frame_set_slot (split->inst.kvp_data, "lot-split", kvptrue);
    g_assert (kvp_frame_get_slot (split->inst.kvp_data, "lot-split"));
    g_assert (xaccSplitGetOtherSplit (split) == NULL);

    kvp_frame_set_slot (split1->inst.kvp_data, "lot-split", kvptrue);
    g_assert (kvp_frame_get_slot (split1->inst.kvp_data, "lot-split"));
    g_assert (xaccSplitGetOtherSplit (split) == split2);

    kvp_frame_set_slot (split->inst.kvp_data, "lot-split", NULL);
    g_assert (kvp_frame_get_slot (split->inst.kvp_data, "lot-split") == NULL);
    kvp_frame_set_slot (split1->inst.kvp_data, "lot-split", NULL);
    g_assert (kvp_frame_get_slot (split1->inst.kvp_data, "lot-split") == NULL);
    qof_book_begin_edit (book);
    qof_instance_set (QOF_INSTANCE (book),
		      "trading-accts", "t",
		      NULL);
    qof_book_commit_edit (book);
    g_assert (xaccTransUseTradingAccounts (txn));
    g_assert (xaccSplitGetOtherSplit (split) == NULL);
    split2->acc = acc2;
    xaccAccountSetType (acc2, ACCT_TYPE_TRADING);
    g_assert (xaccSplitGetOtherSplit (split) == split1);

    test_destroy (split1);
    test_destroy (split2);
    test_destroy (acc2);
}
/* xaccSplitVoidFormerAmount // SCM: 1  Local: 1:0:0
 * xaccSplitVoidFormerValue // Local: 1:0:0
 * xaccSplitVoid // C: 1 SCM: 1
 * xaccSplitUnvoid // C: 1
*/
static void
test_xaccSplitVoid (Fixture *fixture, gconstpointer pData)
{
    gnc_numeric value = fixture->split->value;
    gnc_numeric amount = fixture->split->amount;
    g_assert (gnc_numeric_zero_p (xaccSplitVoidFormerAmount (fixture->split)));
    g_assert (gnc_numeric_zero_p (xaccSplitVoidFormerValue (fixture->split)));
    xaccSplitVoid (fixture->split);
    g_assert (gnc_numeric_zero_p (fixture->split->value));
    g_assert (gnc_numeric_zero_p (fixture->split->amount));
    g_assert (gnc_numeric_equal (xaccSplitVoidFormerAmount (fixture->split),
                                 amount));
    g_assert (gnc_numeric_equal (xaccSplitVoidFormerValue (fixture->split),
                                 value));
    g_assert_cmpint (fixture->split->reconciled, ==, VREC);
    xaccSplitUnvoid (fixture->split);
    g_assert (gnc_numeric_equal (fixture->split->value, value));
    g_assert (gnc_numeric_equal (fixture->split->amount, amount));
    g_assert (gnc_numeric_zero_p (xaccSplitVoidFormerAmount (fixture->split)));
    g_assert (gnc_numeric_zero_p (xaccSplitVoidFormerValue (fixture->split)));
    g_assert_cmpint (fixture->split->reconciled, ==, NREC);
}

/* The rest of these are simple setters and getters unworthy of testing:
 * qofSplitSetMemo // Not Used
 * xaccSplitSetMemo // C: 26 in 13 SCM: 2 in 2 Local: 1:0:0
 * qofSplitSetAction // Not Used
 * xaccSplitSetAction // C: 16 in 10 SCM: 1  Local: 1:0:0
 * qofSplitSetReconcile // Not Used
 * xaccSplitSetReconcile // C: 14 in 11 SCM: 7 in 1 Local: 2:0:0
 * xaccSplitSetDateReconciledSecs // C: 4 in 2
 * xaccSplitSetDateReconciledTS // C: 5 in 5  Local: 1:0:0
 * xaccSplitGetDateReconciledTS // C: 1
 * xaccSplitRetDateReconciledTS // C: 2 in 2
 * xaccSplitGetParent // C: 111 in 30 SCM: 67 in 17 Local: 1:0:0
 * xaccSplitGetLot // C: 7 in 6 SCM: 3 in 3
 * xaccSplitSetLot // C: 2 in 1  Local: 1:0:0
 * xaccSplitGetMemo // C: 26 in 15 SCM: 8 in 7
 * xaccSplitGetAction // C: 6 in 6 SCM: 3 in 3
 * xaccSplitGetReconcile // C: 18 in 11 SCM: 1
 * xaccSplitGetAmount // C: 64 in 20 SCM: 25 in 12 Local: 13:0:0
 * xaccSplitGetValue // C: 52 in 17 SCM: 33 in 14 Local: 11:0:0
 * xaccSplitGetBook // Not Used
 * split_account_guid_getter // Not Used
 * DxaccSplitGetShareAmount // Not Used
 * no_op // Not Used
 * qofSplitSetParentTrans // Not Used
 * qofSplitSetAccount // Not Used
 */
/* This is the QofObject initialization function:
 * xaccSplitRegister // C: 1  Local: 1:0:0
 */


void
test_suite_split (void)
{

    GNC_TEST_ADD_FUNC (suitename, "gnc split init", test_gnc_split_init);
    GNC_TEST_ADD_FUNC (suitename, "gnc split dispose", test_gnc_split_dispose);
    GNC_TEST_ADD_FUNC (suitename, "gnc split set & get property", test_gnc_split_set_get_property);
    GNC_TEST_ADD (suitename, "xaccMallocSplit", Fixture, NULL, setup, test_xaccMallocSplit, teardown);
    GNC_TEST_ADD (suitename, "xaccDupeSplit", Fixture, NULL, setup, test_xaccDupeSplit, teardown);
    GNC_TEST_ADD (suitename, "xaccSplitCloneNoKvp", Fixture, NULL, setup, test_xaccSplitCloneNoKvp, teardown);
    GNC_TEST_ADD (suitename, "mark split", Fixture, NULL, setup, test_mark_split, teardown);
    GNC_TEST_ADD (suitename, "xaccSplitEqualCheckBal", Fixture, NULL, setup, test_xaccSplitEqualCheckBal, teardown);
    GNC_TEST_ADD (suitename, "xaccSplitEqual", Fixture, NULL, setup, test_xaccSplitEqual, teardown);
    GNC_TEST_ADD (suitename, "xaccSplitCommitEdit", Fixture, NULL, setup, test_xaccSplitCommitEdit, teardown);
    GNC_TEST_ADD (suitename, "xaccSplitRollbackEdit", Fixture, NULL, setup, test_xaccSplitRollbackEdit, teardown);
    GNC_TEST_ADD (suitename, "xaccSplitLookup", Fixture, NULL, setup, test_xaccSplitLookup, teardown);
    GNC_TEST_ADD (suitename, "xaccSplitDetermineGainStatus", Fixture, NULL, setup, test_xaccSplitDetermineGainStatus, teardown);
    GNC_TEST_ADD (suitename, "get currency denom", Fixture, NULL, setup, test_get_currency_denom, teardown);
    GNC_TEST_ADD (suitename, "get commodity denom", Fixture, NULL, setup, test_get_commodity_denom, teardown);
    GNC_TEST_ADD (suitename, "xaccSplitSetSharePriceAndAmount", Fixture, NULL, setup, test_xaccSplitSetSharePriceAndAmount, teardown);
    GNC_TEST_ADD (suitename, "xaccSplitSetSharePrice", Fixture, NULL, setup, test_xaccSplitSetSharePrice, teardown);
    GNC_TEST_ADD (suitename, "xaccSplitSetAmount", Fixture, NULL, setup, test_xaccSplitSetAmount, teardown);
    GNC_TEST_ADD (suitename, "xaccSplitSetValue", Fixture, NULL, setup, test_xaccSplitSetValue, teardown);
    GNC_TEST_ADD (suitename, "xaccSplitSetBaseValue", Fixture, NULL, setup, test_xaccSplitSetBaseValue, teardown);
    GNC_TEST_ADD_FUNC (suitename, "xaccSplitConvertAmount", test_xaccSplitConvertAmount);
    GNC_TEST_ADD_FUNC (suitename, "xaccSplitDestroy", test_xaccSplitDestroy);
    GNC_TEST_ADD (suitename, "xaccSplitOrder", Fixture, NULL, setup, test_xaccSplitOrder, teardown);
    GNC_TEST_ADD (suitename, "xaccSplitOrderDateOnly", Fixture, NULL, setup, test_xaccSplitOrderDateOnly, teardown);
    GNC_TEST_ADD (suitename, "get corr account split", Fixture, NULL, setup, test_get_corr_account_split, teardown);
    GNC_TEST_ADD (suitename, "xaccSplitGetCorrAccountFullName", Fixture, NULL, setup, test_xaccSplitGetCorrAccountFullName, teardown);
    GNC_TEST_ADD (suitename, "xaccSplitGetCorrAccountCode", Fixture, NULL, setup, test_xaccSplitGetCorrAccountCode, teardown);
    GNC_TEST_ADD (suitename, "xaccSplitCompareAccountFullNames", Fixture, NULL, setup, test_xaccSplitCompareAccountFullNames, teardown);
    GNC_TEST_ADD (suitename, "xaccSplitCompareAccountCodes", Fixture, NULL, setup, test_xaccSplitCompareAccountCodes, teardown);
    GNC_TEST_ADD (suitename, "xaccSplitCompareOtherAccountFullNames", Fixture, NULL, setup, test_xaccSplitCompareOtherAccountFullNames, teardown);
    GNC_TEST_ADD (suitename, "xaccSplitCompareOtherAccountCodes", Fixture, NULL, setup, test_xaccSplitCompareOtherAccountCodes, teardown);
    GNC_TEST_ADD (suitename, "xaccSplitSetParent", Fixture, NULL, setup, test_xaccSplitSetParent, teardown);
    GNC_TEST_ADD (suitename, "xaccSplitGetSharePrice", Fixture, NULL, setup, test_xaccSplitGetSharePrice, teardown);
    GNC_TEST_ADD (suitename, "xaccSplitMakeStockSplit", Fixture, NULL, setup, test_xaccSplitMakeStockSplit, teardown);
    GNC_TEST_ADD (suitename, "xaccSplitGetOtherSplit", Fixture, NULL, setup, test_xaccSplitGetOtherSplit, teardown);
    GNC_TEST_ADD (suitename, "xaccSplitVoid", Fixture, NULL, setup, test_xaccSplitVoid, teardown);

}
