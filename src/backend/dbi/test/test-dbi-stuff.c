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

static QofLogModule log_module = "test-dbi";

void
do_compare( QofBook* book_1, QofBook* book_2, const gchar* id, QofInstanceForeachCB cb, const gchar* msg )
{
    QofCollection* coll;
    CompareInfoStruct info;

    coll = qof_book_get_collection( book_1, id );
    info.book_1 = book_1;
    info.book_2 = book_2;
    info.result = TRUE;
    qof_collection_foreach(coll, cb, &info);

    do_test( info.result, msg );
}

static void
compare_account_trees( QofBook* book_1, QofBook* book_2 )
{
    Account* root_1 = gnc_book_get_root_account( book_1 );
    Account* root_2 = gnc_book_get_root_account( book_2 );

    xaccAccountSetHidden( root_1, xaccAccountGetHidden( root_1 ) );
    do_test( xaccAccountEqual( root_1, root_2, TRUE ), "Accounts trees match" );
}

static void
compare_pricedbs( QofBook* book_1, QofBook* book_2 )
{
#if 0
    do_compare( book_1, book_2, GNC_ID_TRANS, compare_single_tx, "Transaction lists match" );
#endif
}

static void
compare_single_tx( QofInstance* inst, gpointer user_data )
{
    CompareInfoStruct* info = (CompareInfoStruct*)user_data;
    Transaction* tx_1 = GNC_TRANS(inst);
    Transaction* tx_2 = xaccTransLookup( qof_instance_get_guid(inst), info->book_2 );

    if (!xaccTransEqual( tx_1, tx_2, TRUE, TRUE, TRUE, FALSE ))
    {
        info->result = FALSE;
    }
}

static void
compare_txs( QofBook* book_1, QofBook* book_2 )
{
    do_compare( book_1, book_2, GNC_ID_TRANS, compare_single_tx, "Transaction lists match" );
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
    do_compare( book_1, book_2, GNC_ID_SCHEDXACTION, compare_single_sx, "Scheduled transaction lists match" );
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
    do_compare( book_1, book_2, GNC_ID_LOT, compare_single_lot, "Lot lists match" );
}

static void
compare_books( QofBook* book_1, QofBook* book_2 )
{
    compare_account_trees( book_1, book_2 );
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
