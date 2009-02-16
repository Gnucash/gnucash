/********************************************************************
 * gnc-book-sql.c: load and save data to SQL                        *
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
\********************************************************************/
/** @file gnc-book-sql.c
 *  @brief load and save data to SQL 
 *  @author Copyright (c) 2006-2008 Phil Longstaff <plongstaff@rogers.com>
 *
 * This file implements the top-level QofBackend API for saving/
 * restoring data to/from an SQL db
 */

#include "config.h"

#include <glib.h>

#include "qof.h"

#include "gnc-backend-sql.h"

#include "gnc-book-sql.h"
#include "gnc-slots-sql.h"

#include "gnc-engine.h"
#include "gnc-book.h"
#include "SX-book.h"
#include "SX-book-p.h"

#define BOOK_TABLE "books"
#define TABLE_VERSION 1

static QofLogModule log_module = G_LOG_DOMAIN;

static gpointer get_root_account_guid( gpointer pObject, const QofParam* );
static void set_root_account_guid( gpointer pObject, gpointer pValue );
static gpointer get_root_template_guid( gpointer pObject, const QofParam* );
static void set_root_template_guid( gpointer pObject, gpointer pValue );

static const GncSqlColumnTableEntry col_table[] =
{
	/*# -fullinitblock */
    { "guid",               CT_GUID, 0, COL_NNUL|COL_PKEY, "guid" },
    { "root_account_guid",  CT_GUID, 0, COL_NNUL,          NULL, NULL, get_root_account_guid,  set_root_account_guid },
    { "root_template_guid", CT_GUID, 0, COL_NNUL,          NULL, NULL, get_root_template_guid, set_root_template_guid },
    { NULL }
	/*# +fullinitblock */
};

/* ================================================================= */
static gpointer
get_root_account_guid( gpointer pObject, const QofParam* param )
{
    GNCBook* book = QOF_BOOK(pObject);
    const Account* root;

	g_return_val_if_fail( pObject != NULL, NULL );
	g_return_val_if_fail( QOF_IS_BOOK(pObject), NULL );

    root = gnc_book_get_root_account( book );
    return (gpointer)qof_instance_get_guid( QOF_INSTANCE(root) );
}

static void 
set_root_account_guid( gpointer pObject, gpointer pValue )
{
    GNCBook* book = QOF_BOOK(pObject);
    const Account* root;
    GUID* guid = (GUID*)pValue;

	g_return_if_fail( pObject != NULL );
	g_return_if_fail( QOF_IS_BOOK(pObject) );
	g_return_if_fail( pValue != NULL );

    root = gnc_book_get_root_account( book );
    qof_instance_set_guid( QOF_INSTANCE(root), guid );
}

static gpointer
get_root_template_guid( gpointer pObject, const QofParam* param )
{
    const GNCBook* book = QOF_BOOK(pObject);
    const Account* root;

	g_return_val_if_fail( pObject != NULL, NULL );
	g_return_val_if_fail( QOF_IS_BOOK(pObject), NULL );

    root = gnc_book_get_template_root( book );
    return (gpointer)qof_instance_get_guid( QOF_INSTANCE(root) );
}

static void 
set_root_template_guid( gpointer pObject, gpointer pValue )
{
    GNCBook* book = QOF_BOOK(pObject);
    GUID* guid = (GUID*)pValue;
    Account* root;

	g_return_if_fail( pObject != NULL );
	g_return_if_fail( QOF_IS_BOOK(pObject) );
	g_return_if_fail( pValue != NULL );

    root = gnc_book_get_template_root( book );
    if( root == NULL ) {
        root = xaccMallocAccount( book );
        xaccAccountBeginEdit( root );
        xaccAccountSetType( root, ACCT_TYPE_ROOT );
        xaccAccountCommitEdit( root );
        gnc_book_set_template_root( book, root );
    }
    qof_instance_set_guid( QOF_INSTANCE(root), guid );
}

/* ================================================================= */
static void
load_single_book( GncSqlBackend* be, GncSqlRow* row )
{
    const GUID* guid;
    GUID book_guid;
	GNCBook* pBook;

	g_return_if_fail( be != NULL );
	g_return_if_fail( row != NULL );

    guid = gnc_sql_load_guid( be, row );
    book_guid = *guid;

	pBook = be->primary_book;
	if( pBook == NULL ) {
	    pBook = gnc_book_new();
	}

    gnc_sql_load_object( be, row, GNC_ID_BOOK, pBook, col_table );
    gnc_sql_slots_load( be, QOF_INSTANCE(pBook) );

    qof_instance_mark_clean( QOF_INSTANCE(pBook) );
}

static void
load_all_books( GncSqlBackend* be )
{
    GncSqlStatement* stmt;
    GncSqlResult* result;

	g_return_if_fail( be != NULL );

    stmt = gnc_sql_create_select_statement( be, BOOK_TABLE );
    result = gnc_sql_execute_select_statement( be, stmt );
	gnc_sql_statement_dispose( stmt );
	if( result != NULL ) {
		GncSqlRow* row = gnc_sql_result_get_first_row( result );

		// If there are no rows, try committing the book
		if( row == NULL ) {
   	    	gnc_sql_save_book( be, QOF_INSTANCE(be->primary_book) );
		} else {
			// Otherwise, load the 1st book.
        	load_single_book( be, row );
		}

		gnc_sql_result_dispose( result );
    }
}

/* ================================================================= */
static void
create_book_tables( GncSqlBackend* be )
{
	gint version;

	g_return_if_fail( be != NULL );

	version = gnc_sql_get_table_version( be, BOOK_TABLE );
    if( version == 0 ) {
        gnc_sql_create_table( be, BOOK_TABLE, TABLE_VERSION, col_table );
    }
}

/* ================================================================= */
gboolean
gnc_sql_save_book( GncSqlBackend* be, QofInstance* inst)
{
	g_return_val_if_fail( be != NULL, FALSE );
	g_return_val_if_fail( inst != NULL, FALSE );
	g_return_val_if_fail( QOF_IS_BOOK(inst), FALSE );

	return gnc_sql_commit_standard_item( be, inst, BOOK_TABLE, GNC_ID_BOOK, col_table );
}

/* ================================================================= */
void
gnc_sql_init_book_handler( void )
{
    static GncSqlObjectBackend be_data =
    {
        GNC_SQL_BACKEND_VERSION,
        GNC_ID_BOOK,
        gnc_sql_save_book,      /* commit */
        load_all_books,         /* initial_load */
        create_book_tables,		/* create_tables */
		NULL,                   /* compile_query */
		NULL,                   /* run_query */
		NULL,                   /* free_query */
		NULL                    /* write */
    };

    (void)qof_object_register_backend( GNC_ID_BOOK, GNC_SQL_BACKEND, &be_data );
}
/* ========================== END OF FILE ===================== */
