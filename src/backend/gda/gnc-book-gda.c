/********************************************************************
 * gnc-book-gda.c: load and save data to SQL via libgda             *
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
/** @file gnc-book-gda.c
 *  @brief load and save data to SQL 
 *  @author Copyright (c) 2006 Phil Longstaff <plongstaff@rogers.com>
 *
 * This file implements the top-level QofBackend API for saving/
 * restoring data to/from an SQL db using libgda
 */

#include "config.h"

#include <glib.h>
#include <libgda/libgda.h>

#include "qof.h"

#include "gnc-backend-util-gda.h"

#include "gnc-book-gda.h"
#include "gnc-slots-gda.h"

#include "gnc-engine.h"
#include "gnc-book.h"
#include "SX-book.h"
#include "SX-book-p.h"

#define BOOK_TABLE "books"

static QofLogModule log_module = G_LOG_DOMAIN;

static gpointer get_root_account_guid( gpointer pObject, const QofParam* );
static void set_root_account_guid( gpointer pObject, gpointer pValue );
static gpointer get_root_template_guid( gpointer pObject, const QofParam* );
static void set_root_template_guid( gpointer pObject, gpointer pValue );

static const col_cvt_t col_table[] =
{
    { "guid",               CT_GUID, 0, COL_NNUL|COL_PKEY, "guid" },
    { "root_account_guid",  CT_GUID, 0, COL_NNUL,          NULL, NULL, get_root_account_guid,  set_root_account_guid },
    { "root_template_guid", CT_GUID, 0, COL_NNUL,          NULL, NULL, get_root_template_guid, set_root_template_guid },
    { NULL }
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
load_single_book( GncGdaBackend* be, GdaDataModel* pModel, int row )
{
    const GUID* guid;
    GUID book_guid;
	GNCBook* pBook;

	g_return_if_fail( be != NULL );
	g_return_if_fail( pModel != NULL );
	g_return_if_fail( row >= 0 );

    guid = gnc_gda_load_guid( be, pModel, row );
    book_guid = *guid;

	pBook = be->primary_book;
	if( pBook == NULL ) {
	    pBook = gnc_book_new();
	}

    gnc_gda_load_object( be, pModel, row, GNC_ID_BOOK, pBook, col_table );
    gnc_gda_slots_load( be, QOF_INSTANCE(pBook) );

    qof_instance_mark_clean( QOF_INSTANCE(pBook) );
}

static void
load_all_books( GncGdaBackend* be )
{
    static GdaQuery* query;
    GdaObject* ret;

	g_return_if_fail( be != NULL );

    if( query == NULL ) {
        query = gnc_gda_create_select_query( be, BOOK_TABLE );
    }
    ret = gnc_gda_execute_query( be, query );
    if( GDA_IS_DATA_MODEL( ret ) ) {
        GdaDataModel* pModel = GDA_DATA_MODEL(ret);
        int numRows = gda_data_model_get_n_rows( pModel );

		// If there are no rows, try committing the book
		if( numRows == 0 ) {
   	    	gnc_gda_save_book( QOF_INSTANCE(be->primary_book), be );
		} else {
			// Otherwise, load the 1st book.
            load_single_book( be, pModel, 0 );
		}
    }
}

/* ================================================================= */
static void
create_book_tables( GncGdaBackend* be )
{
	g_return_if_fail( be != NULL );

    gnc_gda_create_table_if_needed( be, BOOK_TABLE, col_table );
}

/* ================================================================= */
void
gnc_gda_save_book( QofInstance* inst, GncGdaBackend* be )
{
    const GUID* guid;
	gint op;

	g_return_if_fail( be != NULL );
	g_return_if_fail( inst != NULL );
	g_return_if_fail( QOF_IS_BOOK(inst) );

	if( qof_instance_get_destroying( inst ) ) {
		op = OP_DB_DELETE;
	} else if( be->is_pristine_db ) {
		op = OP_DB_ADD;
	} else {
		op = OP_DB_ADD_OR_UPDATE;
	}
    (void)gnc_gda_do_db_operation( be, op, BOOK_TABLE, GNC_ID_BOOK, inst, col_table );

    // Delete old slot info
    guid = qof_instance_get_guid( inst );

    // Now, commit any slots
    if( !qof_instance_get_destroying(inst) ) {
        gnc_gda_slots_save( be, guid, qof_instance_get_slots( inst ) );
    } else {
        gnc_gda_slots_delete( be, guid );
    }
}

/* ================================================================= */
void
gnc_gda_init_book_handler( void )
{
    static GncGdaDataType_t be_data =
    {
        GNC_GDA_BACKEND_VERSION,
        GNC_ID_BOOK,
        gnc_gda_save_book,                 /* commit */
        load_all_books,                    /* initial_load */
        create_book_tables 		           /* create_tables */
    };

    qof_object_register_backend( GNC_ID_BOOK, GNC_GDA_BACKEND, &be_data );
}
/* ========================== END OF FILE ===================== */
