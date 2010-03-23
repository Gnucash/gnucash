/***************************************************************************
 *            test-dbi-stuff.c
 *
 *  Tests saving and loading to a dbi/sqlite3 db
 *
 *  Copyright (C) 2009  Phil Longstaff <plongstaff@rogers.com>
 ****************************************************************************/

/*
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301, USA.
 */

#include "config.h"
#include "qof.h"
#include "cashobjects.h"
#include "test-engine-stuff.h"
#include "test-stuff.h"
#include "test-dbi-stuff.h"

#include "Account.h"
#include "Split.h"
#include "Transaction.h"
#include "gnc-commodity.h"

static gboolean testAccountEqual(const Account *aa, const Account *ab, gboolean check_guids);
static QofLogModule log_module = "test-dbi";

static gboolean testTransEqual(const Transaction *ta, const Transaction *tb,
                               gboolean check_guids, gboolean check_splits,
                               gboolean check_balances, gboolean assume_ordered);
static gboolean test_commodity_equal(const gnc_commodity * a, const gnc_commodity * b);

/*
 * Helper routine for testSplitEqual.
 */
static gboolean
testSplitEqualCheckBal (const char *tag, gnc_numeric a, gnc_numeric b)
{
    char *str_a, *str_b;

    if (gnc_numeric_equal (a, b))
        return TRUE;

    str_a = gnc_numeric_to_string (a);
    str_b = gnc_numeric_to_string (b);

    PWARN ("%sbalances differ: %s vs %s", tag, str_a, str_b);

    g_free (str_a);
    g_free (str_b);

    return FALSE;
}

static gboolean
testSplitEqual(const Split *sa, const Split *sb,
               gboolean check_guids,
               gboolean check_balances,
               gboolean check_txn_splits)
{
    Timespec ts1, ts2;

    if (!sa && !sb) return TRUE; /* Arguable. FALSE is better, methinks */

    if (!sa || !sb)
    {
        PWARN ("one is NULL");
        return FALSE;
    }

    if (sa == sb) return TRUE;

    if (check_guids)
    {
        if (qof_instance_guid_compare(sa, sb) != 0)
        {
            PWARN ("GUIDs differ");
            return FALSE;
        }
    }

    /* Since these strings are cached we can just use pointer equality */
    if (strcmp(xaccSplitGetMemo(sa), xaccSplitGetMemo(sb)) != 0)
    {
        const gchar* memo_a = xaccSplitGetMemo(sa);
        const gchar* memo_b = xaccSplitGetMemo(sb);

        PWARN ("memos differ: (%p)%s vs (%p)%s",
               memo_a, memo_a, memo_b, memo_b);
        return FALSE;
    }

    if (strcmp(xaccSplitGetAction(sa), xaccSplitGetAction(sb)) != 0)
    {
        PWARN ("actions differ: %s vs %s", xaccSplitGetAction(sa), xaccSplitGetAction(sb));
        return FALSE;
    }

    if (kvp_frame_compare(qof_instance_get_slots(QOF_INSTANCE(sa)), qof_instance_get_slots(QOF_INSTANCE(sb))) != 0)
    {
        char *frame_a;
        char *frame_b;

        frame_a = kvp_frame_to_string (qof_instance_get_slots(QOF_INSTANCE(sa)));
        frame_b = kvp_frame_to_string (qof_instance_get_slots(QOF_INSTANCE(sb)));

        PWARN ("kvp frames differ:\n%s\n\nvs\n\n%s", frame_a, frame_b);

        g_free (frame_a);
        g_free (frame_b);

        return FALSE;
    }

    if (xaccSplitGetReconcile(sa) != xaccSplitGetReconcile(sb))
    {
        PWARN ("reconcile flags differ: %c vs %c", xaccSplitGetReconcile(sa), xaccSplitGetReconcile(sb));
        return FALSE;
    }

    ts1 = xaccSplitRetDateReconciledTS(sa);
    ts2 = xaccSplitRetDateReconciledTS(sb);
    if (timespec_cmp(&ts1, &ts2))
    {
        PWARN ("reconciled date differs");
        return FALSE;
    }

    if (!gnc_numeric_eq(xaccSplitGetAmount (sa), xaccSplitGetAmount (sb)))
    {
        char *str_a;
        char *str_b;

        str_a = gnc_numeric_to_string (xaccSplitGetAmount (sa));
        str_b = gnc_numeric_to_string (xaccSplitGetAmount (sb));

        PWARN ("amounts differ: %s vs %s", str_a, str_b);

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

        PWARN ("values differ: %s vs %s", str_a, str_b);

        g_free (str_a);
        g_free (str_b);

        return FALSE;
    }

    if (check_balances)
    {
        gnc_numeric bal1, bal2;

        bal1 = xaccSplitGetBalance(sa);
        bal2 = xaccSplitGetBalance(sb);
        if (!testSplitEqualCheckBal ("", bal1, bal2))
            return FALSE;
        bal1 = xaccSplitGetClearedBalance(sa);
        bal2 = xaccSplitGetClearedBalance(sb);
        if (!testSplitEqualCheckBal ("cleared ", bal1, bal2))
            return FALSE;
        bal1 = xaccSplitGetReconciledBalance(sa);
        bal2 = xaccSplitGetReconciledBalance(sb);
        if (!testSplitEqualCheckBal ("reconciled ", bal1, bal2))
            return FALSE;
    }

    if (!testTransEqual(xaccSplitGetParent(sa), xaccSplitGetParent(sb), check_guids, check_txn_splits,
                        check_balances, FALSE))
    {
        PWARN ("transactions differ");
        return FALSE;
    }

    return TRUE;
}

static gint
compare_split_guids (gconstpointer a, gconstpointer b)
{
    const Split *sa = a;
    const Split *sb = b;

    if (sa == sb) return 0;
    if (!sa || !sb) return 1;

    return guid_compare (xaccSplitGetGUID (sa), xaccSplitGetGUID (sb));
}

static gboolean
testTransEqual(const Transaction *ta, const Transaction *tb,
               gboolean check_guids,
               gboolean check_splits,
               gboolean check_balances,
               gboolean assume_ordered)
{
    Timespec ts1, ts2;

    if (!ta && !tb) return TRUE; /* Arguable.  FALSE may be better. */

    if (!ta || !tb)
    {
        PWARN ("one is NULL");
        return FALSE;
    }

    if (ta == tb) return TRUE;

    if (check_guids)
    {
        if (qof_instance_guid_compare(ta, tb) != 0)
        {
            PWARN ("GUIDs differ");
            return FALSE;
        }
    }

    if (!test_commodity_equal(xaccTransGetCurrency(ta), xaccTransGetCurrency(tb)))
    {
        PWARN ("commodities differ %s vs %s",
               gnc_commodity_get_unique_name (xaccTransGetCurrency(ta)),
               gnc_commodity_get_unique_name (xaccTransGetCurrency(tb)));
        return FALSE;
    }

    ts1 = xaccTransRetDateEnteredTS(ta);
    ts2 = xaccTransRetDateEnteredTS(tb);
    ts1.tv_nsec = ts2.tv_nsec = 0;
    if (timespec_cmp(&ts1, &ts2))
    {
        char buf1[100];
        char buf2[100];

        (void)gnc_timespec_to_iso8601_buff(ts1, buf1);
        (void)gnc_timespec_to_iso8601_buff(ts2, buf2);
        PWARN ("date entered differs: '%s' vs '%s'", buf1, buf2);
        return FALSE;
    }

    ts1 = xaccTransRetDatePostedTS(ta);
    ts2 = xaccTransRetDatePostedTS(tb);
    ts1.tv_nsec = ts2.tv_nsec = 0;
    if (timespec_cmp(&ts1, &ts2))
    {
        char buf1[100];
        char buf2[100];

        (void)gnc_timespec_to_iso8601_buff(ts1, buf1);
        (void)gnc_timespec_to_iso8601_buff(ts2, buf2);
        PWARN ("date posted differs: '%s' vs '%s'", buf1, buf2);
        return FALSE;
    }

    if (strcmp(xaccTransGetNum(ta), xaccTransGetNum(tb)) != 0)
    {
        PWARN ("num differs: %s vs %s", xaccTransGetNum(ta), xaccTransGetNum(tb));
        return FALSE;
    }

    if (strcmp(xaccTransGetDescription(ta), xaccTransGetDescription(tb)) != 0)
    {
        PWARN ("descriptions differ: %s vs %s", xaccTransGetDescription(ta), xaccTransGetDescription(tb));
        return FALSE;
    }

    if (kvp_frame_compare(qof_instance_get_slots(QOF_INSTANCE(ta)), qof_instance_get_slots(QOF_INSTANCE(tb))) != 0)
    {
        char *frame_a;
        char *frame_b;

        frame_a = kvp_frame_to_string (qof_instance_get_slots(QOF_INSTANCE(ta)));
        frame_b = kvp_frame_to_string (qof_instance_get_slots(QOF_INSTANCE(tb)));

        PWARN ("kvp frames differ:\n%s\n\nvs\n\n%s", frame_a, frame_b);

        g_free (frame_a);
        g_free (frame_b);

        return FALSE;
    }

    if (check_splits)
    {
        GList* splits1;
        GList* splits2;

        splits1 = xaccTransGetSplitList(ta);
        splits2 = xaccTransGetSplitList(tb);
        if ((!splits1 && splits2) || (!splits2 && splits1))
        {
            PWARN ("only one has splits");
            return FALSE;
        }

        if (splits1 && splits2)
        {
            GList *node_a, *node_b;

            for (node_a = splits1, node_b = splits2;
                    node_a;
                    node_a = node_a->next, node_b = node_b->next)
            {
                Split *split_a = node_a->data;
                Split *split_b;

                /* don't presume that the splits are in the same order */
                if (!assume_ordered)
                    node_b = g_list_find_custom (splits2, split_a,
                                                 compare_split_guids);

                if (!node_b)
                {
                    PWARN ("first has split %s and second does not",
                           guid_to_string (xaccSplitGetGUID (split_a)));
                    return FALSE;
                }

                split_b = node_b->data;

                if (!testSplitEqual (split_a, split_b, check_guids, check_balances,
                                     FALSE))
                {
                    char str_a[GUID_ENCODING_LENGTH+1];
                    char str_b[GUID_ENCODING_LENGTH+1];

                    guid_to_string_buff (xaccSplitGetGUID (split_a), str_a);
                    guid_to_string_buff (xaccSplitGetGUID (split_b), str_b);

                    PWARN ("splits %s and %s differ", str_a, str_b);
                    return FALSE;
                }
            }

            if (g_list_length (splits1) != g_list_length (splits2))
            {
                PWARN ("different number of splits");
                return FALSE;
            }
        }
    }

    return TRUE;
}

static gboolean
test_commodity_equal(const gnc_commodity * a, const gnc_commodity * b)
{
    if (a == b) return TRUE;

    if (!a || !b)
    {
        DEBUG ("one is NULL");
        return FALSE;
    }

    {
        const gchar* ns1 = gnc_commodity_get_namespace(a);
        const gchar* ns2 = gnc_commodity_get_namespace(b);

        if ( ns1 != ns2 && safe_strcmp(ns1, ns2) != 0 )
        {
            DEBUG ("namespaces differ: %s vs %s", ns1, ns2 );
            return FALSE;
        }
    }

    if (safe_strcmp(gnc_commodity_get_mnemonic(a), gnc_commodity_get_mnemonic(b)) != 0)
    {
        DEBUG ("mnemonics differ: %s vs %s", gnc_commodity_get_mnemonic(a), gnc_commodity_get_mnemonic(b));
        return FALSE;
    }

    if (safe_strcmp(gnc_commodity_get_fullname(a), gnc_commodity_get_fullname(b)) != 0)
    {
        DEBUG ("fullnames differ: %s vs %s", gnc_commodity_get_fullname(a), gnc_commodity_get_fullname(b));
        return FALSE;
    }

    if (safe_strcmp(gnc_commodity_get_cusip(a), gnc_commodity_get_cusip(b)) != 0)
    {
        DEBUG ("cusips differ: %s vs %s", gnc_commodity_get_cusip(a), gnc_commodity_get_cusip(b));
        return FALSE;
    }

    if (gnc_commodity_get_fraction(a) != gnc_commodity_get_fraction(b))
    {
        DEBUG ("fractions differ: %d vs %d", gnc_commodity_get_fraction(a), gnc_commodity_get_fraction(b));
        return FALSE;
    }

    return TRUE;
}

static gboolean
testAcctChildrenEqual(const GList *na,
                      const GList *nb,
                      gboolean check_guids)
{
    if ((!na && nb) || (na && !nb))
    {
        PWARN ("only one has accounts");
        return(FALSE);
    }

    while (na && nb)
    {
        Account *aa = na->data;
        Account *ab = nb->data;

        if (!testAccountEqual(aa, ab, check_guids))
        {
            char sa[GUID_ENCODING_LENGTH + 1];
            char sb[GUID_ENCODING_LENGTH + 1];

            guid_to_string_buff (xaccAccountGetGUID (aa), sa);
            guid_to_string_buff (xaccAccountGetGUID (ab), sb);

            PWARN ("accounts %s and %s differ", sa, sb);

            return(FALSE);
        }

        na = na->next;
        nb = nb->next;
    }

    if (na || nb)
    {
        PWARN ("different numbers of accounts");
        return(FALSE);
    }

    return(TRUE);
}

static gboolean
testAccountEqual(const Account *aa, const Account *ab, gboolean check_guids)
{
    if (!aa && !ab) return TRUE;

    g_return_val_if_fail(GNC_IS_ACCOUNT(aa), FALSE);
    g_return_val_if_fail(GNC_IS_ACCOUNT(ab), FALSE);

    if (xaccAccountGetType(aa) != xaccAccountGetType(ab))
    {
        PWARN ("'%s' and '%s': types differ: %d vs %d",
               xaccAccountGetName(aa), xaccAccountGetName(ab),
               xaccAccountGetType(aa), xaccAccountGetType(ab));
        return FALSE;
    }

    if (safe_strcmp(xaccAccountGetName(aa), xaccAccountGetName(ab)) != 0)
    {
        PWARN ("names differ: %s vs %s", xaccAccountGetName(aa), xaccAccountGetName(ab));
        return FALSE;
    }

    if (safe_strcmp(xaccAccountGetCode(aa), xaccAccountGetCode(ab)) != 0)
    {
        PWARN ("codes differ: %s vs %s", xaccAccountGetCode(aa), xaccAccountGetCode(ab));
        return FALSE;
    }

    if (safe_strcmp(xaccAccountGetDescription(aa), xaccAccountGetDescription(ab)) != 0)
    {
        PWARN ("descriptions differ: %s vs %s", xaccAccountGetDescription(aa), xaccAccountGetDescription(ab));
        return FALSE;
    }

    if (!test_commodity_equal(xaccAccountGetCommodity(aa), xaccAccountGetCommodity(ab)))
    {
        PWARN ("commodities differ");
        return FALSE;
    }

    if (check_guids)
    {
        if (qof_instance_guid_compare(aa, ab) != 0)
        {
            gchar guid_a[33];
            gchar guid_b[33];

            guid_to_string_buff( qof_entity_get_guid( QOF_INSTANCE(aa) ), guid_a );
            guid_to_string_buff( qof_entity_get_guid( QOF_INSTANCE(ab) ), guid_b );
            PWARN ("'%s' and '%s': GUIDs differ %s vs %s",
                   xaccAccountGetName(aa), xaccAccountGetName(ab),
                   guid_a, guid_b);
            return FALSE;
        }
    }

    if (kvp_frame_compare(qof_instance_get_slots(QOF_INSTANCE(aa)), qof_instance_get_slots(QOF_INSTANCE(ab))) != 0)
    {
        char *frame_a;
        char *frame_b;

        frame_a = kvp_frame_to_string (qof_instance_get_slots(QOF_INSTANCE(aa)));
        frame_b = kvp_frame_to_string (qof_instance_get_slots(QOF_INSTANCE(ab)));

        PWARN ("kvp frames differ:\n%s\n\nvs\n\n%s", frame_a, frame_b);

        g_free (frame_a);
        g_free (frame_b);

        return FALSE;
    }

    if (!gnc_numeric_equal(gnc_account_get_start_balance((Account*)aa), gnc_account_get_start_balance((Account*)ab)))
    {
        char *str_a;
        char *str_b;

        str_a = gnc_numeric_to_string(gnc_account_get_start_balance((Account*)aa));
        str_b = gnc_numeric_to_string(gnc_account_get_start_balance((Account*)ab));

        PWARN ("starting balances differ: %s vs %s", str_a, str_b);

        g_free (str_a);
        g_free (str_b);

        return FALSE;
    }

    if (!gnc_numeric_equal(gnc_account_get_start_cleared_balance((Account*)aa),
                           gnc_account_get_start_cleared_balance((Account*)ab)))
    {
        char *str_a;
        char *str_b;

        str_a = gnc_numeric_to_string(gnc_account_get_start_cleared_balance((Account*)aa));
        str_b = gnc_numeric_to_string(gnc_account_get_start_cleared_balance((Account*)ab));

        PWARN ("starting cleared balances differ: %s vs %s", str_a, str_b);

        g_free (str_a);
        g_free (str_b);

        return FALSE;
    }

    if (!gnc_numeric_equal(gnc_account_get_start_reconciled_balance((Account*)aa),
                           gnc_account_get_start_reconciled_balance((Account*)ab)))
    {
        char *str_a;
        char *str_b;

        str_a = gnc_numeric_to_string(gnc_account_get_start_reconciled_balance((Account*)aa));
        str_b = gnc_numeric_to_string(gnc_account_get_start_reconciled_balance((Account*)ab));

        PWARN ("starting reconciled balances differ: %s vs %s", str_a, str_b);

        g_free (str_a);
        g_free (str_b);

        return FALSE;
    }

    if (!gnc_numeric_equal(xaccAccountGetBalance(aa), xaccAccountGetBalance(ab)))
    {
        char *str_a;
        char *str_b;

        str_a = gnc_numeric_to_string(xaccAccountGetBalance(aa));
        str_b = gnc_numeric_to_string(xaccAccountGetBalance(ab));

        PWARN ("balances differ: %s vs %s", str_a, str_b);

        g_free (str_a);
        g_free (str_b);

        return FALSE;
    }

    if (!gnc_numeric_equal(xaccAccountGetClearedBalance(aa), xaccAccountGetClearedBalance(ab)))
    {
        char *str_a;
        char *str_b;

        str_a = gnc_numeric_to_string(xaccAccountGetClearedBalance(aa));
        str_b = gnc_numeric_to_string(xaccAccountGetClearedBalance(ab));

        PWARN ("cleared balances differ: %s vs %s", str_a, str_b);

        g_free (str_a);
        g_free (str_b);

        return FALSE;
    }

    if (!gnc_numeric_equal(xaccAccountGetReconciledBalance(aa), xaccAccountGetReconciledBalance(ab)))
    {
        char *str_a;
        char *str_b;

        str_a = gnc_numeric_to_string(xaccAccountGetReconciledBalance(aa));
        str_b = gnc_numeric_to_string(xaccAccountGetReconciledBalance(ab));

        PWARN ("reconciled balances differ: %s vs %s", str_a, str_b);

        g_free (str_a);
        g_free (str_b);

        return FALSE;
    }

    /* no parent; always compare downwards. */

    {
        GList *la = xaccAccountGetSplitList(aa);
        GList *lb = xaccAccountGetSplitList(ab);

        if ((la && !lb) || (!la && lb))
        {
            PWARN ("only one has splits");
            return FALSE;
        }

        if (la && lb)
        {
            /* presume that the splits are in the same order */
            while (la && lb)
            {
                Split *sa = (Split *) la->data;
                Split *sb = (Split *) lb->data;

                if (!testSplitEqual(sa, sb, check_guids, TRUE, FALSE))
                {
                    PWARN ("splits differ");
                    return(FALSE);
                }

                la = la->next;
                lb = lb->next;
            }

            if ((la != NULL) || (lb != NULL))
            {
                PWARN ("number of splits differs");
                return(FALSE);
            }
        }
    }

    if (!testAcctChildrenEqual(gnc_account_get_children(aa), gnc_account_get_children(ab), check_guids))
    {
        PWARN ("children differ");
        return FALSE;
    }

    return(TRUE);
}
static void
compare_accounts( QofBook* book_1, QofBook* book_2 )
{
    Account* root_1 = gnc_book_get_root_account( book_1 );
    Account* root_2 = gnc_book_get_root_account( book_2 );

    xaccAccountSetHidden( root_1, xaccAccountGetHidden( root_1 ) );
    do_test( testAccountEqual( root_1, root_2, TRUE ), "Accounts trees match" );
}

typedef struct
{
    QofBook* book_1;
    QofBook* book_2;
    gboolean result;
} CompareInfoStruct;

static void
compare_pricedbs( QofBook* book_1, QofBook* book_2 )
{
}

static void
compare_single_tx( QofInstance* inst, gpointer user_data )
{
    CompareInfoStruct* info = (CompareInfoStruct*)user_data;
    Transaction* tx_1 = GNC_TRANS(inst);
    Transaction* tx_2 = xaccTransLookup( qof_instance_get_guid(inst), info->book_2 );

    if (!testTransEqual( tx_1, tx_2, TRUE, TRUE, TRUE, FALSE ))
    {
        info->result = FALSE;
    }
}

static void
compare_txs( QofBook* book_1, QofBook* book_2 )
{
    CompareInfoStruct info;
    QofCollection* coll;

    coll = qof_book_get_collection( book_1, GNC_ID_TRANS );
    info.book_1 = book_1;
    info.book_2 = book_2;
    info.result = TRUE;
    qof_collection_foreach(coll, compare_single_tx, &info);

    do_test( info.result, "Transaction lists match" );
}

static void
compare_single_sx( QofInstance* inst, gpointer user_data )
{
#if 0
    CompareInfoStruct* info = (CompareInfoStruct*)user_data;
    Transaction* tx_1 = GNC_TRANS(inst);
    Transaction* tx_2 = xaccTransLookup( qof_instance_get_guid(inst), info->book_2 );

    if (!testTransEqual( tx_1, tx_2, TRUE, TRUE, TRUE, FALSE ))
    {
        info->result = FALSE;
    }
#endif
}

static void
compare_sxs( QofBook* book_1, QofBook* book_2 )
{
    QofCollection* coll;
    CompareInfoStruct info;

    coll = qof_book_get_collection( book_1, GNC_ID_SCHEDXACTION );
    info.book_1 = book_1;
    info.book_2 = book_2;
    info.result = TRUE;
    qof_collection_foreach(coll, compare_single_sx, &info);

    do_test( info.result, "Scheduled transaction lists match" );
}

static void
compare_single_lot( QofInstance* inst, gpointer user_data )
{
#if 0
    CompareInfoStruct* info = (CompareInfoStruct*)user_data;
    Transaction* tx_1 = GNC_TRANS(inst);
    Transaction* tx_2 = xaccTransLookup( qof_instance_get_guid(inst), info->book_2 );

    if (!testTransEqual( tx_1, tx_2, TRUE, TRUE, TRUE, FALSE ))
    {
        info->result = FALSE;
    }
#endif
}

static void
compare_lots( QofBook* book_1, QofBook* book_2 )
{
    QofCollection* coll;
    CompareInfoStruct info;

    coll = qof_book_get_collection( book_1, GNC_ID_LOT );
    info.book_1 = book_1;
    info.book_2 = book_2;
    info.result = TRUE;
    qof_collection_foreach(coll, compare_single_sx, &info);

    do_test( info.result, "Lot lists match" );
}

static void
compare_books( QofBook* book_1, QofBook* book_2 )
{
    compare_accounts( book_1, book_2 );
    compare_pricedbs( book_1, book_2 );
    compare_txs( book_1, book_2 );
    compare_sxs( book_1, book_2 );
    compare_lots( book_1, book_2 );
}

void
test_dbi_store_and_reload( const gchar* driver, QofSession* session_1, const gchar* url )
{
    QofSession* session_2;
    QofSession* session_3;

    printf( "Testing %s\n", driver );

    // Save the session data
    session_2 = qof_session_new();
    qof_session_begin( session_2, url, TRUE, TRUE );
    qof_session_swap_data( session_1, session_2 );
    qof_session_save( session_2, NULL );

    // Reload the session data
    session_3 = qof_session_new();
    qof_session_begin( session_3, url, FALSE, FALSE );
    qof_session_load( session_3, NULL );

    // Compare with the original data
    compare_books( qof_session_get_book( session_2 ), qof_session_get_book( session_3 ) );
    qof_session_end( session_1 );
    qof_session_destroy( session_1 );
    qof_session_end( session_2 );
    qof_session_destroy( session_2 );
    qof_session_end( session_3 );
    qof_session_destroy( session_3 );
}
