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
#include "qofsession-p.h"
#include "cashobjects.h"
#include "test-dbi-stuff.h"
#include <unittest-support.h>

#include "Account.h"
#include "Split.h"
#include "Transaction.h"
#include "gnc-commodity.h"
#include <SX-book.h>
#include <gnc-lot.h>
#include "../gnc-backend-dbi-priv.h"

G_GNUC_UNUSED static QofLogModule log_module = "test-dbi";

/* Placeholder for some old functions that need to be re-written and enabled */
static void do_test (G_GNUC_UNUSED gboolean foo, G_GNUC_UNUSED gchar* bar)
{
}

void
do_compare( QofBook* book_1, QofBook* book_2, const gchar* id,
            QofInstanceForeachCB cb, const gchar* msg )
{
    QofCollection* coll;
    CompareInfoStruct info;

    coll = qof_book_get_collection( book_1, id );
    info.book_1 = book_1;
    info.book_2 = book_2;
    info.result = TRUE;
    qof_collection_foreach(coll, cb, &info);
}

static void
compare_account_trees( QofBook* book_1, QofBook* book_2 )
{
    Account* root_1 = gnc_book_get_root_account( book_1 );
    Account* root_2 = gnc_book_get_root_account( book_2 );

    xaccAccountSetHidden( root_1, xaccAccountGetHidden( root_1 ) );
    g_assert (xaccAccountEqual( root_1, root_2, FALSE ));
}

static void
compare_single_tx( QofInstance* inst, gpointer user_data )
{
    CompareInfoStruct* info = (CompareInfoStruct*)user_data;
    Transaction* tx_1 = GNC_TRANS(inst);
    Transaction* tx_2 = xaccTransLookup( qof_instance_get_guid(inst),
                                         info->book_2 );

    g_assert (xaccTransEqual (tx_1, tx_2, TRUE, TRUE, TRUE, FALSE));
}

static void
compare_txs( QofBook* book_1, QofBook* book_2 )
{
    do_compare( book_1, book_2, GNC_ID_TRANS,
                compare_single_tx, "Transaction lists match" );
}

static SchedXaction*
get_sx_by_guid (QofBook* book, const GncGUID *guid)
{
    SchedXactions *sxes = gnc_book_get_schedxactions (book);
    GList *sxitem;
    for (sxitem = sxes->sx_list; sxitem != NULL; sxitem = sxitem->next)
    {
        const GncGUID *sx_guid;
        sx_guid = qof_instance_get_guid (QOF_INSTANCE(sxitem->data));
        if (guid_equal (sx_guid, guid))
            return sxitem->data;
    }
    return NULL;
}

/* Be sure to put the control GDate first, otherwise a date that isn't
 * properly carried over from the first instance won't assert.
 */

#define TEST_GDATES_EQUAL(gd1, gd2) \
    if (g_date_valid (gd1))			  \
    {						  \
        g_assert (g_date_valid (gd2));		  \
        g_assert (g_date_compare (gd1, gd2) == 0);\
    }

static void
compare_recurrences (GList *rl_1, GList *rl_2)
{
    GList *ritem1, *ritem2;

    if (rl_1 == NULL)
        return;

    g_assert (rl_2 != NULL);
    g_assert_cmpint (g_list_length (rl_1), ==, g_list_length (rl_2));
    for (ritem1 = rl_1, ritem2 = rl_2; ritem1 != NULL && ritem2 != NULL;
            ritem1 = g_list_next (ritem1), ritem2 = g_list_next (ritem2))
    {
        Recurrence *r1 = ritem1->data, *r2 = ritem2->data;

        TEST_GDATES_EQUAL (&r1->start, &r2->start);
        g_assert_cmpint (r1->ptype, ==, r2->ptype);
        g_assert_cmpint (r1->mult, ==, r2->mult);
        g_assert_cmpint (r1->wadj, ==, r2->wadj);
    }
}

static void
compare_single_sx( QofInstance* inst, gpointer user_data )
{
    CompareInfoStruct* info = (CompareInfoStruct*)user_data;
    SchedXaction* sx_1 = GNC_SCHEDXACTION (inst);
    SchedXaction* sx_2 = get_sx_by_guid (info->book_2,
                                         qof_instance_get_guid (inst));

    g_assert (sx_2 != NULL);
    g_assert_cmpstr (sx_1->name, ==, sx_2->name);
    compare_recurrences (sx_2->schedule, sx_1->schedule);
    TEST_GDATES_EQUAL(&sx_2->last_date, &sx_1->last_date);
    TEST_GDATES_EQUAL(&sx_2->start_date, &sx_1->start_date);
    TEST_GDATES_EQUAL(&sx_2->end_date, &sx_1->end_date);
    g_assert_cmpint (sx_2->num_occurances_total, ==,
                     sx_1->num_occurances_total);
    g_assert_cmpint (sx_2->num_occurances_remain, ==,
                     sx_1->num_occurances_remain);
    g_assert_cmpint (sx_2->instance_num, ==, sx_1->instance_num);
    g_assert_cmpint (sx_2->enabled, ==, sx_1->enabled);
    g_assert_cmpint (sx_2->autoCreateOption, ==, sx_1->autoCreateOption);
    g_assert_cmpint (sx_2->autoCreateNotify, ==, sx_1->autoCreateNotify);
    g_assert_cmpint (sx_2->advanceCreateDays, ==, sx_1->advanceCreateDays);
    g_assert_cmpint (sx_2->advanceRemindDays, ==, sx_1->advanceRemindDays);
}

static void
compare_sxs( QofBook* book_1, QofBook* book_2 )
{
    do_compare( book_1, book_2, GNC_ID_SCHEDXACTION,
                compare_single_sx, "Scheduled transaction lists match" );
}

static void
compare_single_lot( QofInstance* inst, gpointer user_data )
{
    CompareInfoStruct* info = (CompareInfoStruct*)user_data;
    GNCLot *lot_1 = GNC_LOT(inst);
    GNCLot *lot_2 = gnc_lot_lookup (qof_instance_get_guid(inst),
                                    info->book_2 );
    GList *split1, *splits1, *splits2;

    g_assert (xaccAccountEqual( gnc_lot_get_account (lot_1),
                                gnc_lot_get_account (lot_2), FALSE ));
    g_assert_cmpint (gnc_lot_is_closed (lot_1), ==, gnc_lot_is_closed (lot_2));

    g_assert (kvp_frame_compare (gnc_lot_get_slots (lot_1),
                                 gnc_lot_get_slots (lot_2)) == 0);
    splits1 = gnc_lot_get_split_list (lot_1);
    splits2 = gnc_lot_get_split_list (lot_2);
    g_assert_cmpint (g_list_length (splits1), ==, g_list_length (splits2));
    for (split1 = splits1; split1 != NULL; split1 = g_list_next (split1))
    {
        Split *split2;
        g_assert (GNC_IS_SPLIT (split1->data));
        split2 = xaccSplitLookup (qof_instance_get_guid (split1->data),
                                  info->book_2);
        g_assert (GNC_IS_SPLIT (split2));
        g_assert (xaccSplitEqual (split1->data, split2, TRUE, TRUE, TRUE));
    }
}

static void
compare_lots( QofBook* book_1, QofBook* book_2 )
{
    do_compare( book_1, book_2, GNC_ID_LOT, compare_single_lot, "Lot lists match" );
}

static void
test_conn_index_functions( QofBackend *qbe )
{
    GncDbiBackend *be = (GncDbiBackend*)qbe;
    GncDbiSqlConnection *conn = (GncDbiSqlConnection*)(be->sql_be.conn);
    GSList *index_list, *iter;

    index_list = conn->provider->get_index_list( be->conn );
    g_test_message ( "Returned from index list\n");
    g_assert (index_list != NULL);
    g_assert_cmpint (g_slist_length( index_list ), ==, 4);
    for ( iter = index_list; iter != NULL; iter = g_slist_next( iter) )
    {
        const char *errmsg;
        conn->provider->drop_index (be->conn, iter->data);
        g_assert (DBI_ERROR_NONE == dbi_conn_error( conn->conn, &errmsg));
    }

    g_slist_free( index_list );


}

static void
compare_pricedbs( QofBook* book_1, QofBook* book_2 )
{
    do_compare( book_1, book_2, GNC_ID_TRANS,
                compare_single_tx, "Transaction lists match" );
}

void
compare_books( QofBook* book_1, QofBook* book_2 )
{
    QofBackend *be = qof_book_get_backend( book_2 );
    compare_account_trees( book_1, book_2 );
    compare_pricedbs( book_1, book_2 );
    compare_txs( book_1, book_2 );
    compare_sxs( book_1, book_2 );
    compare_lots( book_1, book_2 );
}
