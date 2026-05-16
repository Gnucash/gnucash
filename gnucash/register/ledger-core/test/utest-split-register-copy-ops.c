/********************************************************************
 * utest-split-register-copy-ops.c:                                 *
 * Copyright 2019 Geert Janssens <geert@kobaltwit.be>               *
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
 * https://www.gnu.org/licenses/old-licenses/gpl-2.0.html           *
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
#include "split-register-copy-ops.h"
#include <stdint.h>
#include "SX-book-p.h"

static const gchar *suitename = "/register/ledger-core/split-register-copy-ops";
void test_suite_split_register_copy_ops ( void );

typedef struct
{
    gboolean swap_accts;
    gboolean docommit;
} SwapCommitPrefs;

typedef struct
{
    QofBook *book;
    Account *acc1;
    Account *acc2;
    gnc_commodity *curr;

    Transaction *txn;
} Fixture;

typedef struct
{
    QofBook *book;
    Account *acc1;
    Account *acc2;
    gnc_commodity *curr;

    Account *template_acct;
    Account *acc3;
    Account *acc4;

    FloatingTxn ft;
    FloatingSplit fs1, fs2;
} FlFixture;

typedef struct
{
    QofBook *book;
    Account *acc1;
    Account *acc2;
    gnc_commodity *curr;

    Account *template_acct;
    Account *acc3;
    Account *acc4;

    Transaction *txn;
    Transaction *template_txn;

} TemplateFixture;

static void
setup( Fixture *fixture, gconstpointer pData )
{
    time64 entered = gnc_dmy2time64 (20, 4, 2012);
    time64 posted = gnc_dmy2time64 (21, 4, 2012);

    Split *split1 = NULL;
    Split *split2 = NULL;

    fixture->book = qof_book_new();

    split1 = xaccMallocSplit (fixture->book);
    split2 = xaccMallocSplit (fixture->book);
    fixture->acc1 = xaccMallocAccount(fixture->book);
    fixture->acc2 = xaccMallocAccount(fixture->book);
    fixture->curr = gnc_commodity_new(fixture->book, "Gnu Rand", "CURRENCY", "GNR", "", 100);
    xaccAccountSetCommodity(fixture->acc1, fixture->curr);
    xaccAccountSetCommodity(fixture->acc2, fixture->curr);

    fixture->txn = xaccMallocTransaction (fixture->book);

    xaccSplitSetMemo (split1, CACHE_INSERT ("foo"));
    xaccSplitSetAction (split1, CACHE_INSERT ("bar"));
    xaccSplitSetAmount (split1, gnc_numeric_create (3200, 100));
    xaccSplitSetValue (split1, gnc_numeric_create (3200, 100));
    xaccSplitSetAccount (split1, fixture->acc1);

    xaccSplitSetAmount (split2, gnc_numeric_create (-3200, 100));
    xaccSplitSetValue (split2, gnc_numeric_create (-3200, 100));
    xaccSplitSetAccount (split2, fixture->acc2);

    xaccTransBeginEdit (fixture->txn);
    {
        xaccTransSetNum (fixture->txn, CACHE_INSERT ("123"));
        xaccTransSetDescription (fixture->txn, CACHE_INSERT ("Waldo Pepper"));
        xaccTransSetDatePostedSecs (fixture->txn, posted);
        xaccTransSetDateEnteredSecs (fixture->txn, entered);
        xaccTransSetCurrency (fixture->txn, fixture->curr);
        xaccSplitSetParent (split1, fixture->txn);
        xaccSplitSetParent (split2, fixture->txn);
        xaccTransSetNotes (fixture->txn, "Salt pork sausage");
    }
    xaccTransCommitEdit (fixture->txn);
    xaccAccountSortSplits(fixture->acc1, FALSE);
    xaccAccountSortSplits(fixture->acc2, FALSE);
    xaccAccountRecomputeBalance(fixture->acc1);
    xaccAccountRecomputeBalance(fixture->acc2);
}

static void
setup_template( TemplateFixture *fixture, gconstpointer pData )
{
    fixture->book = qof_book_new();

    Account *templateRoot = xaccMallocAccount(fixture->book);
    gnc_book_set_template_root (fixture->book, templateRoot);
    gnc_commodity *template_commodity = gnc_commodity_new(fixture->book, "template",
                                                          GNC_COMMODITY_NS_TEMPLATE,
                                                          "template", "template", 1);
    xaccAccountSetCommodity(templateRoot, template_commodity);

    fixture->template_acct = xaccMallocAccount(fixture->book);
    gnc_account_append_child (templateRoot, fixture->template_acct);

    // Normal Transaction
    time64 entered = gnc_dmy2time64 (20, 4, 2012);
    time64 posted = gnc_dmy2time64 (21, 4, 2012);

    Split *split1 = xaccMallocSplit(fixture->book);
    Split *split2 = xaccMallocSplit(fixture->book);

    fixture->acc1 = xaccMallocAccount(fixture->book);
    fixture->acc2 = xaccMallocAccount(fixture->book);

    fixture->curr = gnc_commodity_new(fixture->book, "Gnu Rand", "CURRENCY", "GNR", "", 100);
    xaccAccountSetCommodity(fixture->acc1, fixture->curr);
    xaccAccountSetCommodity(fixture->acc2, fixture->curr);

    fixture->txn = xaccMallocTransaction (fixture->book);

    xaccSplitSetMemo (split1, CACHE_INSERT ("foo"));
    xaccSplitSetAction (split1, CACHE_INSERT ("bar"));
    xaccSplitSetAmount (split1, gnc_numeric_create (3200, 100));
    xaccSplitSetValue (split1, gnc_numeric_create (3200, 100));
    xaccSplitSetAccount (split1, fixture->acc1);

    xaccSplitSetAmount (split2, gnc_numeric_create (-3200, 100));
    xaccSplitSetValue (split2, gnc_numeric_create (-3200, 100));
    xaccSplitSetAccount (split2, fixture->acc2);

    xaccTransBeginEdit (fixture->txn);
    {
        xaccTransSetNum (fixture->txn, CACHE_INSERT ("123"));
        xaccTransSetDescription (fixture->txn, CACHE_INSERT ("Waldo Pepper"));
        xaccTransSetDatePostedSecs (fixture->txn, posted);
        xaccTransSetDateEnteredSecs (fixture->txn, entered);
        xaccTransSetCurrency (fixture->txn, fixture->curr);
        xaccSplitSetParent (split1, fixture->txn);
        xaccSplitSetParent (split2, fixture->txn);
        xaccTransSetNotes (fixture->txn, "Salt pork sausage");
    }
    xaccTransCommitEdit (fixture->txn);
    xaccAccountSortSplits(fixture->acc1, FALSE);
    xaccAccountSortSplits(fixture->acc2, FALSE);
    xaccAccountRecomputeBalance(fixture->acc1);
    xaccAccountRecomputeBalance(fixture->acc2);


    // Template Transaction
    Split *split3 = xaccMallocSplit (fixture->book);
    Split *split4 = xaccMallocSplit (fixture->book);

    fixture->acc3 = xaccMallocAccount(fixture->book);
    fixture->acc4 = xaccMallocAccount(fixture->book);

    const GncGUID* guid_acc3 = xaccAccountGetGUID(fixture->acc3);
    const GncGUID* guid_acc4 = xaccAccountGetGUID(fixture->acc4);

    xaccAccountSetCommodity(fixture->template_acct, template_commodity);
    xaccAccountSetType(fixture->template_acct, ACCT_TYPE_BANK);

    xaccAccountSetCommodity(fixture->acc3, fixture->curr);
    xaccAccountSetCommodity(fixture->acc4, fixture->curr);

    fixture->template_txn = xaccMallocTransaction (fixture->book);

    xaccSplitSetMemo (split3, CACHE_INSERT ("foo"));
    xaccSplitSetAction (split3, CACHE_INSERT ("bar"));

    gnc_numeric numeric_zero = gnc_numeric_zero ();
    gnc_numeric numeric = gnc_numeric_create (41,2);

    qof_begin_edit (QOF_INSTANCE (split3));
    qof_instance_set (QOF_INSTANCE (split3),
              "sx-debit-formula", "",
              "sx-debit-numeric", &numeric_zero,
              "sx-credit-formula", "20.5",
              "sx-credit-numeric", &numeric,
              "sx-account", guid_acc3,
              "sx-shares", "10",
              NULL);

    xaccSplitSetMemo (split4, CACHE_INSERT ("zoo"));
    xaccSplitSetAction (split4, CACHE_INSERT ("tar"));

    qof_begin_edit (QOF_INSTANCE (split4));
    qof_instance_set (QOF_INSTANCE (split4),
              "sx-debit-formula", "20.5",
              "sx-debit-numeric", &numeric,
              "sx-credit-formula", "",
              "sx-credit-numeric", &numeric_zero,
              "sx-account", guid_acc4,
              "sx-shares", "10",
              NULL);

    xaccSplitSetAccount (split3, fixture->template_acct);
    xaccSplitSetAccount (split4, fixture->template_acct);

    xaccTransBeginEdit (fixture->template_txn);
    {
        xaccTransSetNum (fixture->template_txn, CACHE_INSERT ("123"));
        xaccTransSetDescription (fixture->template_txn, CACHE_INSERT ("Waldo Pepper"));
        xaccTransSetCurrency (fixture->template_txn, fixture->curr);

        xaccSplitSetParent (split3, fixture->template_txn);
        xaccSplitSetParent (split4, fixture->template_txn);
        xaccTransSetNotes (fixture->template_txn, "Salt pork sausage");
    }
    xaccTransCommitEdit (fixture->template_txn);
    xaccAccountSortSplits(fixture->acc3, FALSE);
    xaccAccountSortSplits(fixture->acc4, FALSE);

}
static void
teardown( Fixture *fixture, gconstpointer pData )
{
    xaccTransDestroy (fixture->txn);
    xaccAccountBeginEdit(fixture->acc1);
    xaccAccountDestroy(fixture->acc1);
    xaccAccountBeginEdit(fixture->acc2);
    xaccAccountDestroy(fixture->acc2);
    gnc_commodity_destroy(fixture->curr);
    qof_book_destroy( fixture->book );
};

static void
teardown_template( TemplateFixture *fixture, gconstpointer pData )
{
    xaccTransDestroy (fixture->txn);
    xaccTransDestroy (fixture->template_txn);

    xaccAccountBeginEdit(fixture->acc1);
    xaccAccountDestroy(fixture->acc1);
    xaccAccountBeginEdit(fixture->acc2);
    xaccAccountDestroy(fixture->acc2);

    xaccAccountBeginEdit(fixture->acc3);
    xaccAccountDestroy(fixture->acc3);
    xaccAccountBeginEdit(fixture->acc4);
    xaccAccountDestroy(fixture->acc4);

    xaccAccountBeginEdit(fixture->template_acct);
    xaccAccountDestroy(fixture->template_acct);

    gnc_commodity_destroy(fixture->curr);
    qof_book_destroy( fixture->book );
};

static void
flsetup( FlFixture *fixture, gconstpointer pData )
{
    time64 entered = gnc_dmy2time64 (20, 4, 2012);
    time64 posted = gnc_dmy2time64 (21, 4, 2012);
    time64 reconciled = gnc_dmy2time64 (22, 4, 2012);

    fixture->book = qof_book_new();
    fixture->acc1 = xaccMallocAccount(fixture->book);
    fixture->acc2 = xaccMallocAccount(fixture->book);
    fixture->curr = gnc_commodity_new(fixture->book, "Gnu Rand", "CURRENCY", "GNR", "", 100);
    xaccAccountSetCommodity(fixture->acc1, fixture->curr);
    xaccAccountSetCommodity(fixture->acc2, fixture->curr);

    fixture->ft.m_txn = NULL;
    fixture->ft.m_currency = fixture->curr;
    fixture->ft.m_date_entered = entered;
    fixture->ft.m_date_posted = posted;
    fixture->ft.m_num = CACHE_INSERT ("FtNum");
    fixture->ft.m_description = CACHE_INSERT ("FtDescription");
    fixture->ft.m_notes = CACHE_INSERT ("FtNotes");
    fixture->ft.m_doclink = CACHE_INSERT ("FtDocLink");

    fixture->fs1.m_split = NULL;
    fixture->fs1.m_account = fixture->acc1;
    fixture->fs1.m_transaction = NULL;
    fixture->fs1.m_memo = CACHE_INSERT ("Fs1Memo");
    fixture->fs1.m_action = CACHE_INSERT ("Fs1Action");
    fixture->fs1.m_reconcile_date = INT64_MAX;
    fixture->fs1.m_reconcile_state = NREC;
    fixture->fs1.m_value = gnc_numeric_create (4500, 100);
    fixture->fs1.m_amount = gnc_numeric_create (4500, 100);

    fixture->fs2.m_split = NULL;
    fixture->fs2.m_account = fixture->acc2;
    fixture->fs2.m_transaction = NULL;
    fixture->fs2.m_memo = CACHE_INSERT ("Fs2Memo");
    fixture->fs2.m_action = CACHE_INSERT ("Fs2Action");
    fixture->fs2.m_reconcile_date = reconciled;
    fixture->fs2.m_reconcile_state = YREC;
    fixture->fs2.m_value = gnc_numeric_create (-4500, 100);
    fixture->fs2.m_amount = gnc_numeric_create (-4500, 100);

    fixture->ft.m_splits = NULL;
    fixture->ft.m_splits = g_list_append (NULL, &fixture->fs1);
    fixture->ft.m_splits = g_list_append (fixture->ft.m_splits, &fixture->fs2);
}

static void
flteardown( FlFixture *fixture, gconstpointer pData )
{
    g_list_free (fixture->ft.m_splits);
    xaccAccountBeginEdit(fixture->acc1);
    xaccAccountDestroy(fixture->acc1);
    xaccAccountBeginEdit(fixture->acc2);
    xaccAccountDestroy(fixture->acc2);
    gnc_commodity_destroy(fixture->curr);
    qof_book_destroy( fixture->book );
};



// Not Used
/* gnc_float_split_get_reconcile_state - trivial getter, skipping
char gnc_float_split_get_reconcile_state (const FloatingSplit *fs)// Local: 0:0:0
*/
// Not Used
/* gnc_float_split_get_reconcile_date - trivial getter, skipping
time64 gnc_float_split_get_reconcile_date (const FloatingSplit *fs)// Local: 0:0:0
*/
/* gnc_float_split_get_amount - trivial getter, skipping
gnc_numeric gnc_float_split_get_amount (const FloatingSplit *fs)// C: 1  Local: 0:0:0
*/
/* static void
test_gnc_float_split_get_amount (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_float_split_get_value - trivial getter, skipping
gnc_numeric gnc_float_split_get_value (const FloatingSplit *fs)// C: 1  Local: 0:0:0
*/
/* static void
test_gnc_float_split_get_value (Fixture *fixture, gconstpointer pData)
{
}*/
// Not Used
/* gnc_float_split_set_split - trivial getter, skipping
void gnc_float_split_set_split(FloatingSplit *fs, Split *split)// Local: 0:0:0
*/
/* gnc_float_split_set_account - trivial setter, skipping
void gnc_float_split_set_account (FloatingSplit *fs, Account *account) // C: 2 in 1  Local: 0:0:0
*/
/* static void
test_gnc_float_split_set_account (Fixture *fixture, gconstpointer pData)
{
}*/
// Not Used
/* gnc_float_split_set_transaction - trivial setter, skipping
void gnc_float_split_set_transaction (FloatingSplit *fs, Transaction *transaction) // Local: 0:0:0
*/
/* gnc_float_split_set_memo
void gnc_float_split_set_memo (FloatingSplit *fs, const char *memo)// C: 1  Local: 0:0:0
*/
/* static void
test_gnc_float_split_set_memo (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_float_split_set_action - trivial setter, skipping
void gnc_float_split_set_action (FloatingSplit *fs, const char *action)// C: 1  Local: 0:0:0
*/
/* static void
test_gnc_float_split_set_action (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_float_split_set_reconcile_state - trivial setter, skipping
void gnc_float_split_set_reconcile_state (FloatingSplit *fs, char reconcile_state)// C: 1  Local: 0:0:0
*/
/* static void
test_gnc_float_split_set_reconcile_state (Fixture *fixture, gconstpointer pData)
{
}*/
// Not Used
/* gnc_float_split_set_reconcile_date - trivial setter, skipping
void gnc_float_split_set_reconcile_date (FloatingSplit *fs, time64 reconcile_date)// Local: 0:0:0
*/
/* gnc_float_split_set_amount - trivial setter, skipping
void gnc_float_split_set_amount (FloatingSplit *fs, const gnc_numeric amount)// C: 2 in 1  Local: 0:0:0
*/
/* static void
test_gnc_float_split_set_amount (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_float_split_set_value - trivial setter, skipping
void gnc_float_split_set_value (FloatingSplit *fs, const gnc_numeric value)// C: 2 in 1  Local: 0:0:0
*/
/* static void
test_gnc_float_split_set_value (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_split_to_float_split
FloatingSplit *gnc_split_to_float_split (Split *split)// C: 3 in 1  Local: 1:0:0
*/
static void
test_gnc_split_to_float_split (Fixture *fixture, gconstpointer pData)
{
    FloatingSplit *fs = NULL;
    Split *s = xaccTransFindSplitByAccount (fixture->txn, fixture->acc1);

    g_assert_nonnull (s);

    fs = gnc_split_to_float_split (s, FALSE);
    g_assert_true (fs->m_split == s);
    g_assert_true (fs->m_account == xaccSplitGetAccount (s));
    g_assert_true (fs->m_transaction == xaccSplitGetParent (s));
    g_assert_cmpstr (fs->m_memo, ==, xaccSplitGetMemo (s));
    g_assert_cmpstr (fs->m_action, ==, xaccSplitGetAction (s));
    g_assert_true (gnc_numeric_equal(fs->m_value, xaccSplitGetValue (s)));
    g_assert_true (gnc_numeric_equal(fs->m_amount, xaccSplitGetAmount (s)));

    gnc_float_split_free (fs);
}
/* gnc_float_split_to_split
void gnc_float_split_to_split (const FloatingSplit *fs, Split *split)// C: 2 in 1  Local: 1:0:0
*/
static void
test_gnc_float_split_to_split (Fixture *fixture, gconstpointer pData)
{
    gnc_numeric amt = gnc_numeric_create (500, 100);
    FloatingSplit fs = { NULL, fixture->acc1, NULL, "Memo1", "Action1", INT64_MAX, 'u', amt, amt };
    Split *s = xaccMallocSplit(fixture->book);
    Transaction *txn = xaccMallocTransaction (fixture->book);

    gnc_float_split_to_split (&fs, s, NULL);
    g_assert_true (fixture->acc1 == xaccSplitGetAccount (s));
    g_assert_cmpstr ("Memo1", ==, xaccSplitGetMemo (s));
    g_assert_cmpstr ("Action1", ==, xaccSplitGetAction (s));
    g_assert_true (gnc_numeric_equal(amt, xaccSplitGetValue (s)));
    g_assert_true (gnc_numeric_equal(amt, xaccSplitGetAmount (s)));


    xaccTransBeginEdit (txn);
    xaccTransSetCurrency (txn, fixture->curr);
    xaccSplitSetParent (s, txn);
    xaccTransCommitEdit (txn);
    xaccTransDestroy (txn);
}
// Not Used
/* gnc_float_txn_get_date_entered - trivial getter, skipping
time64 gnc_float_txn_get_date_entered (const FloatingTxn *ft)// Local: 0:0:0
*/
// Not Used
/* gnc_float_txn_get_date_posted - trivial getter, skipping
time64 gnc_float_txn_get_date_posted (const FloatingTxn *ft)// Local: 0:0:0
*/
// Not Used
/* gnc_float_txn_set_txn - trivial setter, skipping
void gnc_float_txn_set_txn (FloatingTxn *ft, Transaction *txn)// Local: 0:0:0
*/
// Not Used
/* gnc_float_txn_set_currency - trivial setter, skipping
void gnc_float_txn_set_currency (FloatingTxn *ft, gnc_commodity *currency)// Local: 0:0:0
*/
// Not Used
/* gnc_float_txn_set_date_entered - trivial setter, skipping
void gnc_float_txn_set_date_entered (FloatingTxn *ft, time64 date_entered)// Local: 0:0:0
*/
// Not Used
/* gnc_float_txn_set_date_posted - trivial setter, skipping
void gnc_float_txn_set_date_posted (FloatingTxn *ft, time64 date_posted)// Local: 0:0:0
*/
// Not Used
/* gnc_float_txn_set_num - trivial setter, skipping
void gnc_float_txn_set_num (FloatingTxn *ft, const char *num)// Local: 0:0:0
*/
// Not Used
/* gnc_float_txn_set_description - trivial setter, skipping
void gnc_float_txn_set_description (FloatingTxn *ft, const char *description)// Local: 0:0:0
*/
// Not Used
/* gnc_float_txn_set_notes - trivial setter, skipping
void gnc_float_txn_set_notes (FloatingTxn *ft, const char *notes)// Local: 0:0:0
*/
// Not Used
/* gnc_float_txn_set_doclink - trivial setter, skipping
void gnc_float_txn_set_doclink (FloatingTxn *ft, const char *doclink)// Local: 0:0:0
*/
// Not Used
/* gnc_float_txn_set_splits - trivial setter, skipping
void gnc_float_txn_set_splits (FloatingTxn *ft, SplitList *splits)// Local: 0:0:0
*/
/* gnc_float_txn_append_float_split
void gnc_float_txn_append_float_split (FloatingTxn *ft, FloatingSplit *fs)// C: 1  Local: 0:0:0
*/
/* static void
test_gnc_float_txn_append_float_split (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_txn_to_float_txn
FloatingTxn *gnc_txn_to_float_txn (Transaction *txn, gboolean use_cut_semantics)// C: 3 in 1  Local: 0:0:0
*/
static void
test_gnc_txn_to_float_txn (Fixture *fixture, gconstpointer pData)
{
    FloatingTxn *ft = NULL;
    SplitList *sl = xaccTransGetSplitList(fixture->txn), *siter;
    SplitList *fsiter;
    FloatingSplit *fs;
    Split *s;

    ft = gnc_txn_to_float_txn (fixture->txn, FALSE, FALSE);

    /* Check transaction fields */
    g_assert_true (ft->m_txn == fixture->txn);
    g_assert_true (ft->m_currency == xaccTransGetCurrency (fixture->txn));
    g_assert_cmpint (ft->m_date_entered, ==, xaccTransGetDateEntered (fixture->txn));
    g_assert_cmpint (ft->m_date_posted, ==, 0);
    g_assert_null (ft->m_num);
    g_assert_cmpstr (ft->m_description, ==, xaccTransGetDescription (fixture->txn));
    g_assert_cmpstr (ft->m_notes, ==, xaccTransGetNotes (fixture->txn));
    g_assert_cmpstr (ft->m_doclink, ==, xaccTransGetDocLink (fixture->txn));

    /* Check split fields of first split */
    siter = sl;
    s = siter->data;

    fsiter = ft->m_splits;
    fs = fsiter->data;

    g_assert_nonnull (fs);
    g_assert_true (fs->m_split == s);
    g_assert_true (fs->m_account == xaccSplitGetAccount (s));
    g_assert_true (fs->m_transaction == xaccSplitGetParent (s));
    g_assert_cmpstr (fs->m_memo, ==, xaccSplitGetMemo (s));
    g_assert_cmpstr (fs->m_action, ==, xaccSplitGetAction (s));
    g_assert_true (gnc_numeric_equal(fs->m_value, xaccSplitGetValue (s)));
    g_assert_true (gnc_numeric_equal(fs->m_amount, xaccSplitGetAmount (s)));

    /* Check split fields of second split */
    siter = siter->next;
    s = siter->data;

    fsiter = fsiter->next;
    fs = fsiter->data;

    g_assert_nonnull (fs);
    g_assert_true (fs->m_split == s);
    g_assert_true (fs->m_account == xaccSplitGetAccount (s));
    g_assert_true (fs->m_transaction == xaccSplitGetParent (s));
    g_assert_cmpstr (fs->m_memo, ==, xaccSplitGetMemo (s));
    g_assert_cmpstr (fs->m_action, ==, xaccSplitGetAction (s));
    g_assert_true (gnc_numeric_equal(fs->m_value, xaccSplitGetValue (s)));
    g_assert_true (gnc_numeric_equal(fs->m_amount, xaccSplitGetAmount (s)));

    g_assert_null (fsiter->next);

    gnc_float_txn_free (ft);
}
static void
test_gnc_txn_to_float_txn_cut_semantics (Fixture *fixture, gconstpointer pData)
{
    FloatingTxn *ft = NULL;
    SplitList *sl = xaccTransGetSplitList(fixture->txn), *siter;
    SplitList *fsiter;
    FloatingSplit *fs;
    Split *s;

    ft = gnc_txn_to_float_txn (fixture->txn, TRUE, FALSE);

    /* Check transaction fields */
    g_assert_true (ft->m_txn == fixture->txn);
    g_assert_true (ft->m_currency == xaccTransGetCurrency (fixture->txn));
    g_assert_cmpint (ft->m_date_entered, ==, xaccTransGetDateEntered (fixture->txn));
    g_assert_cmpint (ft->m_date_posted, ==, xaccTransGetDate (fixture->txn));
    g_assert_cmpstr (ft->m_num, ==, xaccTransGetNum (fixture->txn));
    g_assert_cmpstr (ft->m_description, ==, xaccTransGetDescription (fixture->txn));
    g_assert_cmpstr (ft->m_notes, ==, xaccTransGetNotes (fixture->txn));
    g_assert_cmpstr (ft->m_doclink, ==, xaccTransGetDocLink (fixture->txn));

    /* Check split fields of first split */
    siter = sl;
    s = siter->data;

    fsiter = ft->m_splits;
    fs = fsiter->data;

    g_assert_nonnull (fs);
    g_assert_true (fs->m_split == s);
    g_assert_true (fs->m_account == xaccSplitGetAccount (s));
    g_assert_true (fs->m_transaction == xaccSplitGetParent (s));
    g_assert_cmpstr (fs->m_memo, ==, xaccSplitGetMemo (s));
    g_assert_cmpstr (fs->m_action, ==, xaccSplitGetAction (s));
    g_assert_true (gnc_numeric_equal(fs->m_value, xaccSplitGetValue (s)));
    g_assert_true (gnc_numeric_equal(fs->m_amount, xaccSplitGetAmount (s)));

    /* Check split fields of second split */
    siter = siter->next;
    s = siter->data;

    fsiter = fsiter->next;
    fs = fsiter->data;

    g_assert_nonnull (fs);
    g_assert_true (fs->m_split == s);
    g_assert_true (fs->m_account == xaccSplitGetAccount (s));
    g_assert_true (fs->m_transaction == xaccSplitGetParent (s));
    g_assert_cmpstr (fs->m_memo, ==, xaccSplitGetMemo (s));
    g_assert_cmpstr (fs->m_action, ==, xaccSplitGetAction (s));
    g_assert_true (gnc_numeric_equal(fs->m_value, xaccSplitGetValue (s)));
    g_assert_true (gnc_numeric_equal(fs->m_amount, xaccSplitGetAmount (s)));

    g_assert_null (fsiter->next);

    gnc_float_txn_free (ft);
}


/* gnc_float_txn_get_float_split
FloatingSplit *gnc_float_txn_get_float_split (const FloatingTxn *ft, guint index)// C: 1 in 1  Local: 0:0:0
*/
static void
test_gnc_float_txn_get_float_split (FlFixture *fixture, gconstpointer pData)
{
    FloatingSplit *fs = gnc_float_txn_get_float_split(&fixture->ft, 0);

    g_assert_nonnull (fs);
    g_assert_true (fs->m_account == fixture->acc1);
    g_assert_cmpstr (fs->m_memo, ==, "Fs1Memo");
    g_assert_cmpstr (fs->m_action, ==, "Fs1Action");
    g_assert_true (gnc_numeric_equal(fs->m_value, gnc_numeric_create (4500, 100)));
    g_assert_true (gnc_numeric_equal(fs->m_amount, gnc_numeric_create (4500, 100)));

    fs = gnc_float_txn_get_float_split(&fixture->ft, 1);

    g_assert_nonnull (fs);
    g_assert_true (fs->m_account == fixture->acc2);
    g_assert_cmpstr (fs->m_memo, ==, "Fs2Memo");
    g_assert_cmpstr (fs->m_action, ==, "Fs2Action");
    g_assert_true (gnc_numeric_equal(fs->m_value, gnc_numeric_create (-4500, 100)));
    g_assert_true (gnc_numeric_equal(fs->m_amount, gnc_numeric_create (-4500, 100)));
}

/* gnc_float_txn_get_other_float_split
FloatingSplit *gnc_float_txn_get_other_float_split (const FloatingTxn *ft, FloatingSplit *fs)// C: 2 in 1  Local: 0:0:0
*/
static void
test_gnc_float_txn_get_other_float_split (FlFixture *fixture, gconstpointer pData)
{
    FloatingSplit *first = gnc_float_txn_get_float_split(&fixture->ft, 0);
    FloatingSplit *other = gnc_float_txn_get_other_float_split(&fixture->ft, first);

    g_assert_nonnull (other);
    g_assert_true (other->m_account == fixture->acc2);
    g_assert_cmpstr (other->m_memo, ==, "Fs2Memo");
    g_assert_cmpstr (other->m_action, ==, "Fs2Action");
    g_assert_true (gnc_numeric_equal(other->m_value, gnc_numeric_create (-4500, 100)));
    g_assert_true (gnc_numeric_equal(other->m_amount, gnc_numeric_create (-4500, 100)));

    first = gnc_float_txn_get_float_split(&fixture->ft, 1);
    other = gnc_float_txn_get_other_float_split(&fixture->ft, first);

    g_assert_nonnull (other);
    g_assert_true (other->m_account == fixture->acc1);
    g_assert_cmpstr (other->m_memo, ==, "Fs1Memo");
    g_assert_cmpstr (other->m_action, ==, "Fs1Action");
    g_assert_true (gnc_numeric_equal(other->m_value, gnc_numeric_create (4500, 100)));
    g_assert_true (gnc_numeric_equal(other->m_amount, gnc_numeric_create (4500, 100)));
}
/* gnc_float_txn_to_txn_swap_accounts
void gnc_float_txn_to_txn_swap_accounts (const FloatingTxn *ft, Transaction *txn, Account *acct1, Account *acct2, gboolean do_commit)// C: 1  Local: 1:0:0
*/
static void
impl_test_gnc_float_txn_to_txn_swap_accounts (FlFixture *fixture,  const SwapCommitPrefs *prefs)
{
    Transaction *txn = xaccMallocTransaction (fixture->book);
    Account *sw_acct1 = NULL, *sw_acct2 = NULL;
    Account *exp_acct1 = fixture->acc1, *exp_acct2 = fixture->acc2;
    SplitList *siter;
    Split *s;

    if (prefs->swap_accts)
    {
        sw_acct1 = fixture->acc1;
        sw_acct2 = fixture->acc2;
        exp_acct1 = fixture->acc2;
        exp_acct2 = fixture->acc1;
    }

    gnc_float_txn_to_txn_swap_accounts (&fixture->ft, txn, sw_acct1, sw_acct2, prefs->docommit);

    /* First compare transaction values */
    g_assert_true (fixture->ft.m_currency == xaccTransGetCurrency (txn));
    g_assert_cmpstr (fixture->ft.m_description, ==, "FtDescription");
    g_assert_cmpstr (fixture->ft.m_num, ==, "FtNum");
    g_assert_cmpstr (fixture->ft.m_notes, ==, "FtNotes");
    g_assert_cmpstr (fixture->ft.m_doclink, ==, "FtDocLink");
    g_assert_cmpint (fixture->ft.m_date_posted, ==, xaccTransGetDate (txn));

    /* Next compare values for first split */
    siter = xaccTransGetSplitList (txn);
    g_assert_nonnull (siter);

    s = siter->data;
    g_assert_nonnull (s);

    g_assert_true (exp_acct1 == xaccSplitGetAccount (s));
    g_assert_cmpstr (fixture->fs1.m_memo, ==, xaccSplitGetMemo (s));
    g_assert_cmpstr (fixture->fs1.m_action, ==, xaccSplitGetAction (s));
    g_assert_true (gnc_numeric_equal(fixture->fs1.m_value, xaccSplitGetValue (s)));
    g_assert_true (gnc_numeric_equal(fixture->fs1.m_value, xaccSplitGetAmount (s)));

    /* Next compare values for second split */
    siter = siter->next;
    g_assert_nonnull (siter);

    s = siter->data;
    g_assert_nonnull (s);

    g_assert_true (exp_acct2 == xaccSplitGetAccount (s));
    g_assert_cmpstr (fixture->fs2.m_memo, ==, xaccSplitGetMemo (s));
    g_assert_cmpstr (fixture->fs2.m_action, ==, xaccSplitGetAction (s));
    g_assert_true (gnc_numeric_equal(fixture->fs2.m_value, xaccSplitGetValue (s)));
    g_assert_true (gnc_numeric_equal(fixture->fs2.m_value, xaccSplitGetAmount (s)));

    /* Test there are only two splits */
    siter = siter->next;
    g_assert_null (siter);

    /* Verify whether transaction is still open or not  based on input value */
    g_assert_true (xaccTransIsOpen (txn) != prefs->docommit);

    xaccTransDestroy (txn);
}

static void
test_gnc_float_txn_to_txn_swap_accounts_noswap_nocommit (FlFixture *fixture, gconstpointer pData)
{
    SwapCommitPrefs prefs = {FALSE, FALSE};
    impl_test_gnc_float_txn_to_txn_swap_accounts(fixture, &prefs);
}

static void
test_gnc_float_txn_to_txn_swap_accounts_noswap_commit (FlFixture *fixture, gconstpointer pData)
{
    SwapCommitPrefs prefs = {FALSE, TRUE};
    impl_test_gnc_float_txn_to_txn_swap_accounts(fixture, &prefs);
}

static void
test_gnc_float_txn_to_txn_swap_accounts_swap_commit (FlFixture *fixture, gconstpointer pData)
{
    SwapCommitPrefs prefs = {TRUE, TRUE};
    impl_test_gnc_float_txn_to_txn_swap_accounts(fixture, &prefs);
}

static void
test_gnc_float_txn_to_txn_swap_accounts_swap_nocommit (FlFixture *fixture, gconstpointer pData)
{
    SwapCommitPrefs prefs = {TRUE, FALSE};
    impl_test_gnc_float_txn_to_txn_swap_accounts(fixture, &prefs);
}

static void
test_gnc_template_to_template (TemplateFixture *fixture, gconstpointer pData)
{
    FloatingTxn *ft = NULL;

    ft = gnc_txn_to_float_txn (fixture->template_txn, FALSE, TRUE);
    g_assert_true (ft->m_txn == fixture->template_txn);

    Transaction *new_txn = xaccMallocTransaction (fixture->book);
    gnc_float_txn_to_template_txn (ft, new_txn, fixture->template_acct, FALSE);

    /* Check transaction fields */
    g_assert_cmpstr (xaccTransGetDescription (fixture->template_txn), ==, xaccTransGetDescription (new_txn));
    g_assert_cmpstr (xaccTransGetNotes (fixture->template_txn), ==, xaccTransGetNotes (new_txn));

    SplitList *sl_template_txn = xaccTransGetSplitList (fixture->template_txn), *siter_template_txn;
    SplitList *sl_new_txn = xaccTransGetSplitList (new_txn), *siter_new_txn;

    /* Check split fields of first split */
    Split *s_template, *s_new_txn;

    siter_template_txn = sl_template_txn;
    siter_new_txn = sl_new_txn;

    s_template = siter_template_txn->data;
    s_new_txn = siter_new_txn->data;

    // First split
    g_assert_cmpstr (xaccSplitGetMemo (s_template), ==, xaccSplitGetMemo (s_new_txn));
    g_assert_cmpstr (xaccSplitGetAction (s_template), ==, xaccSplitGetAction (s_new_txn));

    GncGUID* guid = NULL;
    const gchar *cf, *df, *shares;
    gnc_numeric *cn, *dn;

    qof_instance_get (QOF_INSTANCE (s_new_txn), "sx-account", &guid, NULL);
    const GncGUID* guid_acc3 = xaccAccountGetGUID (fixture->acc3);
    g_assert_true (guid_equal (guid_acc3, guid));

    qof_instance_get (QOF_INSTANCE (s_new_txn), "sx-credit-formula", &cf, NULL);
    g_assert_cmpstr ("20.5", ==, cf);

    qof_instance_get (QOF_INSTANCE (s_new_txn), "sx-credit-numeric", &cn, NULL);
    g_assert_true (gnc_numeric_equal(gnc_numeric_create (41,2), *cn));

    qof_instance_get (QOF_INSTANCE (s_new_txn), "sx-debit-formula", &df, NULL);
    g_assert_cmpstr ("", ==, df);

    qof_instance_get (QOF_INSTANCE (s_new_txn), "sx-debit-numeric", &dn, NULL);
    g_assert_true (gnc_numeric_equal(gnc_numeric_zero(), *dn));

    qof_instance_get (QOF_INSTANCE (s_new_txn), "sx-shares", &shares, NULL);
    g_assert_cmpstr ("10", ==, shares);

    // Second split
    siter_new_txn = siter_new_txn->next;
    siter_template_txn = siter_template_txn->next;

    s_template = siter_template_txn->data;
    s_new_txn = siter_new_txn->data;

    g_assert_cmpstr (xaccSplitGetMemo (s_template), ==, xaccSplitGetMemo (s_new_txn));
    g_assert_cmpstr (xaccSplitGetAction (s_template), ==, xaccSplitGetAction (s_new_txn));

    qof_instance_get (QOF_INSTANCE (s_new_txn), "sx-account", &guid, NULL);
    const GncGUID* guid_acc4 = xaccAccountGetGUID (fixture->acc4);
    g_assert_true (guid_equal (guid_acc4, guid));

    qof_instance_get (QOF_INSTANCE (s_new_txn), "sx-credit-formula", &cf, NULL);
    g_assert_cmpstr ("", ==, cf);

    qof_instance_get (QOF_INSTANCE (s_new_txn), "sx-credit-numeric", &cn, NULL);
    g_assert_true (gnc_numeric_equal(gnc_numeric_zero(), *cn));

    qof_instance_get (QOF_INSTANCE (s_new_txn), "sx-debit-formula", &df, NULL);
    g_assert_cmpstr ("20.5", ==, df);

    qof_instance_get (QOF_INSTANCE (s_new_txn), "sx-debit-numeric", &dn, NULL);
    g_assert_true (gnc_numeric_equal(gnc_numeric_create (41,2), *dn));

    qof_instance_get (QOF_INSTANCE (s_new_txn), "sx-shares", &shares, NULL);
    g_assert_cmpstr ("10", ==, shares);

    g_assert_null (siter_template_txn->next);
    g_assert_null (siter_new_txn->next);

    guid_free (guid);
    g_free (cn);
    g_free (dn);

    gnc_float_txn_free (ft);
}

static void
test_gnc_normal_to_template (TemplateFixture *fixture, gconstpointer pData)
{
    FloatingTxn *ft = NULL;

    ft = gnc_txn_to_float_txn (fixture->txn, FALSE, FALSE);
    g_assert_true (ft->m_txn == fixture->txn);

    Transaction *new_txn = xaccMallocTransaction (fixture->book);
    gnc_float_txn_to_template_txn (ft, new_txn, fixture->template_acct, FALSE);

    /* Check transaction fields */
    g_assert_cmpstr (xaccTransGetDescription (fixture->txn), ==, xaccTransGetDescription (new_txn));
    g_assert_cmpstr (xaccTransGetNotes (fixture->txn), ==, xaccTransGetNotes (new_txn));

    SplitList *sl_txn = xaccTransGetSplitList (fixture->txn), *siter_txn;
    SplitList *sl_new_txn = xaccTransGetSplitList (new_txn), *siter_new_txn;

    /* Check split fields of first split */
    Split *s_txn, *s_new_txn;

    siter_txn = sl_txn;
    siter_new_txn = sl_new_txn;

    s_txn = siter_txn->data;
    s_new_txn = siter_new_txn->data;

    // First split
    g_assert_cmpstr (xaccSplitGetMemo (s_new_txn), ==, xaccSplitGetMemo (s_txn));
    g_assert_cmpstr (xaccSplitGetAction (s_new_txn), ==, xaccSplitGetAction (s_txn));

    GncGUID* guid = NULL;
    const gchar *cf, *df;
    gnc_numeric *cn, *dn;

    qof_instance_get (QOF_INSTANCE (s_new_txn), "sx-account", &guid, NULL);
    const GncGUID* guid_acc1 = xaccAccountGetGUID (fixture->acc1);
    g_assert_true (guid_equal (guid_acc1, guid));

    qof_instance_get (QOF_INSTANCE (s_new_txn), "sx-credit-formula", &cf, NULL);
    g_assert_cmpstr ("", ==, cf);

    qof_instance_get (QOF_INSTANCE (s_new_txn), "sx-credit-numeric", &cn, NULL);
    g_assert_true (gnc_numeric_equal(gnc_numeric_zero(), *cn));

    qof_instance_get (QOF_INSTANCE (s_new_txn), "sx-debit-formula", &df, NULL);
    g_assert_cmpstr ("32.", ==, df);

    qof_instance_get (QOF_INSTANCE (s_new_txn), "sx-debit-numeric", &dn, NULL);
    g_assert_true (gnc_numeric_equal(gnc_numeric_abs(xaccSplitGetAmount (s_txn)), *dn));

    // Second split
    siter_new_txn = siter_new_txn->next;
    siter_txn = siter_txn->next;

    s_txn = siter_txn->data;
    s_new_txn = siter_new_txn->data;

    g_assert_cmpstr (xaccSplitGetMemo (s_new_txn), ==, xaccSplitGetMemo (s_txn));
    g_assert_cmpstr (xaccSplitGetAction (s_new_txn), ==, xaccSplitGetAction (s_txn));

    qof_instance_get (QOF_INSTANCE (s_new_txn), "sx-account", &guid, NULL);
    const GncGUID* guid_acc2 = xaccAccountGetGUID (fixture->acc2);
    g_assert_true (guid_equal (guid_acc2, guid));

    qof_instance_get (QOF_INSTANCE (s_new_txn), "sx-credit-formula", &cf, NULL);
    g_assert_cmpstr ("32.", ==, cf);

    qof_instance_get (QOF_INSTANCE (s_new_txn), "sx-credit-numeric", &cn, NULL);
    g_assert_true (gnc_numeric_equal(gnc_numeric_abs(xaccSplitGetAmount (s_txn)), *cn));

    qof_instance_get (QOF_INSTANCE (s_new_txn), "sx-debit-formula", &df, NULL);
    g_assert_cmpstr ("", ==, df);

    qof_instance_get (QOF_INSTANCE (s_new_txn), "sx-debit-numeric", &dn, NULL);
    g_assert_true (gnc_numeric_equal(gnc_numeric_zero(), *dn));

    g_assert_null (siter_txn->next);
    g_assert_null (siter_new_txn->next);

    guid_free (guid);
    g_free (cn);
    g_free (dn);

    gnc_float_txn_free (ft);
}

static void
test_gnc_template_to_normal (TemplateFixture *fixture, gconstpointer pData)
{
    FloatingTxn *ft = NULL;

    ft = gnc_txn_to_float_txn (fixture->template_txn, FALSE, TRUE);
    g_assert_true (ft->m_txn == fixture->template_txn);

    Transaction *new_txn = xaccMallocTransaction (fixture->book);
    gnc_float_txn_to_txn (ft, new_txn, FALSE);

    /* Check transaction fields */
    g_assert_cmpstr (xaccTransGetDescription (fixture->template_txn), ==, xaccTransGetDescription (new_txn));
    g_assert_cmpstr (xaccTransGetNotes (fixture->template_txn), ==, xaccTransGetNotes (new_txn));

    SplitList *sl_template_txn = xaccTransGetSplitList (fixture->template_txn), *siter_template_txn;
    SplitList *sl_new_txn = xaccTransGetSplitList (new_txn), *siter_new_txn;

    /* Check split fields of first split */
    Split *s_template_txn, *s_new_txn;

    siter_template_txn = sl_template_txn;
    siter_new_txn = sl_new_txn;

    s_template_txn = siter_template_txn->data;
    s_new_txn = siter_new_txn->data;

    // First split
    g_assert_cmpstr (xaccSplitGetMemo (s_template_txn), ==, xaccSplitGetMemo (s_new_txn));
    g_assert_cmpstr (xaccSplitGetAction (s_template_txn), ==, xaccSplitGetAction (s_new_txn));

    GncGUID* guid = NULL;
    Account *template_split_account;
    gnc_numeric *cn, *dn;

    qof_instance_get (QOF_INSTANCE (s_template_txn), "sx-account", &guid, NULL);
    template_split_account = xaccAccountLookup (guid, fixture->book);
    g_assert_true (xaccSplitGetAccount (s_new_txn) == template_split_account);

    qof_instance_get (QOF_INSTANCE (s_template_txn), "sx-credit-numeric", &cn, NULL);
    g_assert_true (gnc_numeric_equal(xaccSplitGetAmount (s_new_txn), gnc_numeric_neg(*cn)));
    g_assert_true (gnc_numeric_equal(xaccSplitGetValue  (s_new_txn), gnc_numeric_neg(*cn)));

    // Second split
    siter_new_txn = siter_new_txn->next;
    siter_template_txn = siter_template_txn->next;

    s_template_txn = siter_template_txn->data;
    s_new_txn = siter_new_txn->data;

    g_assert_cmpstr (xaccSplitGetMemo (s_template_txn), ==, xaccSplitGetMemo (s_new_txn));
    g_assert_cmpstr (xaccSplitGetAction (s_template_txn), ==, xaccSplitGetAction (s_new_txn));

    qof_instance_get (QOF_INSTANCE (s_template_txn), "sx-account", &guid, NULL);
    template_split_account = xaccAccountLookup (guid, fixture->book);
    g_assert_true (xaccSplitGetAccount (s_new_txn) == template_split_account);

    qof_instance_get (QOF_INSTANCE (s_template_txn), "sx-debit-numeric", &dn, NULL);
    g_assert_true (gnc_numeric_equal(xaccSplitGetAmount (s_new_txn), *dn));
    g_assert_true (gnc_numeric_equal(xaccSplitGetValue  (s_new_txn), *dn));

    g_assert_null (siter_template_txn->next);
    g_assert_null (siter_new_txn->next);

    guid_free (guid);
    g_free (cn);
    g_free (dn);

    gnc_float_txn_free (ft);
}

void
test_suite_split_register_copy_ops (void)
{
    GNC_TEST_ADD (suitename, "gnc split to float split", Fixture, NULL, setup, test_gnc_split_to_float_split, teardown);
    GNC_TEST_ADD (suitename, "gnc float split to split", Fixture, NULL, setup, test_gnc_float_split_to_split, teardown);
    GNC_TEST_ADD (suitename, "gnc float txn to float txn", Fixture, NULL, setup, test_gnc_txn_to_float_txn, teardown);
    GNC_TEST_ADD (suitename, "gnc float txn to float txn cut semantics", Fixture, NULL, setup, test_gnc_txn_to_float_txn_cut_semantics, teardown);
    GNC_TEST_ADD (suitename, "gnc float txn get float split", FlFixture, NULL, flsetup, test_gnc_float_txn_get_float_split, flteardown);
    GNC_TEST_ADD (suitename, "gnc float txn get other float split", FlFixture, NULL, flsetup, test_gnc_float_txn_get_other_float_split, flteardown);

    GNC_TEST_ADD (suitename, "gnc float txn to txn noswap nocommit", FlFixture, NULL, flsetup, test_gnc_float_txn_to_txn_swap_accounts_noswap_nocommit, flteardown);
    GNC_TEST_ADD (suitename, "gnc float txn to txn noswap commit", FlFixture, NULL, flsetup, test_gnc_float_txn_to_txn_swap_accounts_noswap_commit, flteardown);
    GNC_TEST_ADD (suitename, "gnc float txn to txn swap commit", FlFixture, NULL, flsetup, test_gnc_float_txn_to_txn_swap_accounts_swap_commit, flteardown);
    GNC_TEST_ADD (suitename, "gnc float txn to txn swap nocommit", FlFixture, NULL, flsetup, test_gnc_float_txn_to_txn_swap_accounts_swap_nocommit, flteardown);

    GNC_TEST_ADD (suitename, "gnc template transaction to template transaction", TemplateFixture, NULL, setup_template, test_gnc_template_to_template, teardown_template);
    GNC_TEST_ADD (suitename, "gnc normal transaction to template transaction", TemplateFixture, NULL, setup_template, test_gnc_normal_to_template, teardown_template);
    GNC_TEST_ADD (suitename, "gnc template transaction to normal transaction", TemplateFixture, NULL, setup_template, test_gnc_template_to_normal, teardown_template);
}
