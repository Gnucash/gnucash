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
#include "gnc-main.h"
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

static void
compare_books( QofBook* book_1, QofBook* book_2 )
{
    QofBackend *be = qof_book_get_backend( book_2 );
    compare_account_trees( book_1, book_2 );
    compare_pricedbs( book_1, book_2 );
    compare_txs( book_1, book_2 );
    compare_sxs( book_1, book_2 );
    compare_lots( book_1, book_2 );
}

/* Given a synthetic session, use the same logic as
 * QofSession::save_as to save it to a specified sql url, then load it
 * back and compare. */
void
test_dbi_store_and_reload( const gchar* driver, QofSession* session_1, const gchar* url )
{
    QofSession* session_2;
    QofSession* session_3;
    QofBackend *be;

    gchar *msg = "[gnc_dbi_unlock()] There was no lock entry in the Lock table";
    gchar *log_domain = "gnc.backend.dbi";
    guint loglevel = G_LOG_LEVEL_WARNING | G_LOG_FLAG_FATAL, hdlr;
    TestErrorStruct check = { loglevel, log_domain, msg, 0 };
    GLogFunc dhdlr = g_log_set_default_handler ((GLogFunc)test_null_handler,
						&check);
    g_test_log_set_fatal_handler ((GTestLogFatalFunc)test_checked_handler,
				  &check);


    g_test_message ( "Testing %s\n", driver );

    // Save the session data
    session_2 = qof_session_new();
    hdlr = g_log_set_handler (log_domain, loglevel,
                              (GLogFunc)test_checked_handler, &check);
    qof_session_begin( session_2, url, FALSE, TRUE, TRUE );
    g_assert (session_2 != NULL);
    g_assert_cmpint (qof_session_get_error (session_2), ==, ERR_BACKEND_NO_ERR);
    qof_session_swap_data( session_1, session_2 );
    qof_session_save( session_2, NULL );
    g_assert (session_2 != NULL);
    g_assert_cmpint (qof_session_get_error (session_2), ==, ERR_BACKEND_NO_ERR);

    // Reload the session data
    session_3 = qof_session_new();
    g_assert (session_3 != NULL);
    qof_session_begin( session_3, url, TRUE, FALSE, FALSE );
    g_assert (session_3 != NULL);
    g_assert_cmpint (qof_session_get_error (session_3), ==, ERR_BACKEND_NO_ERR);
    qof_session_load( session_3, NULL );
    g_assert (session_3 != NULL);
    g_assert_cmpint (qof_session_get_error (session_3), ==, ERR_BACKEND_NO_ERR);
    // Compare with the original data
    compare_books (qof_session_get_book( session_2),
		   qof_session_get_book( session_3));
/* Session_1 belongs to the fixture and teardown() will clean it up */
    qof_session_end( session_2 );
    qof_session_destroy( session_2 );
    qof_session_end( session_3 );
    qof_session_destroy( session_3 );
    g_log_remove_handler (log_domain, hdlr);
    g_log_set_default_handler (dhdlr, NULL);
}

/* Given an already-created url (yeah, bad testing practice: Should
 * start fresh from a synthetic session) load and safe-save it, then
 * load it again into a new session and compare the two. Since
 * safe-save is a more-or-less atomic function call, there's no way to
 * be sure that it's actually doing what it's supposed to without
 * running this test in a debugger and stopping in the middle of the
 * safe-save and inspecting the database. */
void
test_dbi_safe_save( const gchar* driver,  const gchar* url )
{
    QofSession *session_1 = NULL, *session_2 = NULL;
    QofBackend *be;

    gchar *msg = "[gnc_dbi_unlock()] There was no lock entry in the Lock table";
    gchar *log_domain = "gnc.backend.dbi";
    guint loglevel = G_LOG_LEVEL_WARNING, hdlr;
    TestErrorStruct check = { loglevel, log_domain, msg };

    g_test_message ( "Testing safe save %s\n", driver );

    // Load the session data
    session_1 = qof_session_new();
    qof_session_begin( session_1, url, TRUE, FALSE, FALSE );
    if (session_1 && qof_session_get_error(session_1) != ERR_BACKEND_NO_ERR)
    {
        g_warning("Session Error: %d, %s", qof_session_get_error(session_1),
                  qof_session_get_error_message(session_1));
        do_test( FALSE, "DB Session Creation Failed");
        goto cleanup;
    }
    qof_session_load( session_1, NULL );
    /* Do a safe save */
    qof_session_safe_save( session_1, NULL );
    if (session_1 && qof_session_get_error(session_1) != ERR_BACKEND_NO_ERR)
    {
        g_warning("Session Error: %s", qof_session_get_error_message(session_1));
        do_test( FALSE, "DB Session Safe Save Failed");
        goto cleanup;
    }
    /* Destroy the session and reload it */

    session_2 = qof_session_new();
    qof_session_begin( session_2, url, TRUE, FALSE, FALSE );
    if (session_2 && qof_session_get_error(session_2) != ERR_BACKEND_NO_ERR)
    {
        g_warning("Session Error: %d, %s", qof_session_get_error(session_2),
                  qof_session_get_error_message(session_2));
        do_test( FALSE, "DB Session re-creation Failed");
        goto cleanup;
    }
    qof_session_load( session_2, NULL );
    compare_books( qof_session_get_book( session_1 ),
                   qof_session_get_book( session_2 ) );
    be = qof_book_get_backend( qof_session_get_book( session_2 ) );
    test_conn_index_functions( be );

cleanup:
    hdlr = g_log_set_handler (log_domain, loglevel,
                              (GLogFunc)test_checked_handler, &check);
    if (session_2 != NULL)
    {
        qof_session_end( session_2 );
        qof_session_destroy( session_2 );
    }
    if (session_1 != NULL)
    {
        qof_session_end( session_1 );
        qof_session_destroy( session_1 );
    }
    g_log_remove_handler (log_domain, hdlr);
    return;
}

/* Test the gnc_dbi_load logic that forces a newer database to be
 * opened read-only and an older one to be safe-saved. Again, it would
 * be better to do this starting from a fresh file, but instead we're
 * being lazy and using an existing one. */
void
test_dbi_version_control( const gchar* driver,  const gchar* url )
{

    QofSession *sess;
    QofBook *book;
    QofBackend *qbe;
    QofBackendError err;
    gint ourversion = gnc_get_long_version();

    g_test_message ( "Testing safe save %s\n", driver );

    // Load the session data
    sess = qof_session_new();
    qof_session_begin( sess, url, TRUE, FALSE, FALSE );
    if (sess && qof_session_get_error(sess) != ERR_BACKEND_NO_ERR)
    {
        g_warning("Session Error: %d, %s", qof_session_get_error(sess),
                  qof_session_get_error_message(sess));
        do_test( FALSE, "DB Session Creation Failed");
        goto cleanup;
    }
    qof_session_load( sess, NULL );
    qbe = qof_session_get_backend( sess );
    book = qof_session_get_book( sess );
    qof_book_begin_edit( book );
    gnc_sql_set_table_version( (GncSqlBackend*)qbe,
                               "Gnucash", GNUCASH_RESAVE_VERSION - 1 );
    qof_book_commit_edit( book );
    qof_session_end( sess );
    qof_session_destroy( sess );
    sess = qof_session_new();
    qof_session_begin( sess, url, TRUE, FALSE, FALSE );
    qof_session_load( sess, NULL );
    err = qof_session_pop_error( sess );
    do_test( err == ERR_SQL_DB_TOO_OLD, "DB Failed to flag too old" );
    qbe = qof_session_get_backend( sess );
    book = qof_session_get_book( sess );
    qof_book_begin_edit( book );
    gnc_sql_set_table_version( (GncSqlBackend*)qbe,
                               "Gnucash", ourversion );
    gnc_sql_set_table_version( (GncSqlBackend*)qbe,
                               "Gnucash-Resave", ourversion + 1 );
    qof_book_commit_edit( book );
    qof_session_end( sess );
    qof_session_destroy( sess );
    sess = qof_session_new();
    qof_session_begin( sess, url, TRUE, FALSE, FALSE );
    qof_session_load( sess, NULL );
    qof_session_ensure_all_data_loaded( sess );
    err = qof_session_pop_error( sess );
    do_test( err == ERR_SQL_DB_TOO_NEW, "DB Failed to flag too new" );
cleanup:
    qbe = qof_session_get_backend( sess );
    book = qof_session_get_book( sess );
    qof_book_begin_edit( book );
    gnc_sql_set_table_version( (GncSqlBackend*)qbe,
                               "Gnucash-Resave", GNUCASH_RESAVE_VERSION );
    qof_book_commit_edit( book );
    qof_session_end( sess );
    qof_session_destroy( sess );
}
