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

static QofLogModule log_module = GNC_MOD_BACKEND;

static void commit_book( GncGdaBackend* be, QofInstance* inst );

static gpointer get_root_account_guid( gpointer pObject, const QofParam* );
static void set_root_account_guid( gpointer pObject, gpointer pValue );
static gpointer get_root_template_guid( gpointer pObject, const QofParam* );
static void set_root_template_guid( gpointer pObject, gpointer pValue );

static col_cvt_t col_table[] =
{
    { "guid",            CT_GUID,    0, COL_NNUL|COL_PKEY,    NULL,
            (QofAccessFunc)qof_instance_get_guid,
            (QofSetterFunc)qof_instance_set_guid },
    { "root_account_guid", CT_GUID,  0, COL_NNUL,             NULL,
            get_root_account_guid, set_root_account_guid },
    { "root_template_guid", CT_GUID, 0, COL_NNUL,             NULL,
            get_root_template_guid, set_root_template_guid },
    { NULL }
};

/* ================================================================= */
static gpointer
get_root_account_guid( gpointer pObject, const QofParam* param )
{
    GNCBook* book = QOF_BOOK(pObject);
    const Account* root = gnc_book_get_root_account( book );

    return (gpointer)qof_instance_get_guid( QOF_INSTANCE( root ) );
}

static void 
set_root_account_guid( gpointer pObject, gpointer pValue )
{
    GNCBook* book = QOF_BOOK(pObject);
    const Account* root = gnc_book_get_root_account( book );
    GUID* guid = (GUID*)pValue;

    qof_instance_set_guid( QOF_INSTANCE( root ), guid );
}

static gpointer
get_root_template_guid( gpointer pObject, const QofParam* param )
{
    const GNCBook* book = QOF_BOOK(pObject);
    const Account* root = gnc_book_get_template_root( book );

    return (gpointer)qof_instance_get_guid( QOF_INSTANCE( root ) );
}

static void 
set_root_template_guid( gpointer pObject, gpointer pValue )
{
    GNCBook* book = QOF_BOOK(pObject);
    GUID* guid = (GUID*)pValue;
    Account* root = gnc_book_get_template_root( book );

    if( root == NULL ) {
        root = xaccMallocAccount( book );
        xaccAccountBeginEdit( root );
        xaccAccountSetType( root, ACCT_TYPE_ROOT );
        xaccAccountCommitEdit( root );
        gnc_book_set_template_root( book, root );
    }
    qof_instance_set_guid( QOF_INSTANCE( root ), guid );
}

/* ================================================================= */
static GNCBook*
load_single_book( GncGdaBackend* be, GdaDataModel* pModel, int row,
            GNCBook* pBook )
{
    const GUID* guid;
    GUID book_guid;

    guid = gnc_gda_load_guid( pModel, row );
    book_guid = *guid;

    if( pBook == NULL ) {
        pBook = gnc_book_new();
    }

    gnc_gda_load_object( pModel, row, GNC_ID_BOOK, pBook, col_table );
    gnc_gda_slots_load( be, gnc_book_get_guid( pBook ),
                            qof_instance_get_slots( QOF_INSTANCE(pBook) ) );

    qof_instance_mark_clean( QOF_INSTANCE(pBook) );

    return pBook;
}

static void
load_all_books( GncGdaBackend* be )
{
    static GdaQuery* query;
    GdaObject* ret;
    QofBook* pBook = be->primary_book;

    if( query == NULL ) {
        query = gnc_gda_create_select_query( be, BOOK_TABLE );
    }
    ret = gnc_gda_execute_query( be, query );
    if( GDA_IS_DATA_MODEL( ret ) ) {
        GdaDataModel* pModel = GDA_DATA_MODEL(ret);
        int numRows = gda_data_model_get_n_rows( pModel );
        int r;

        for( r = 0; r < numRows; r++ ) {
            (void)load_single_book( be, pModel, r, NULL );
        }

	// If there are no rows, try committing the book
	if( numRows == 0 ) {
    	    commit_book( be, QOF_INSTANCE( be->primary_book ) );
	}
    }
}

/* ================================================================= */
static void
create_book_tables( GncGdaBackend* be )
{
    gnc_gda_create_table_if_needed( be, BOOK_TABLE, col_table );
}

/* ================================================================= */
static void
commit_book( GncGdaBackend* be, QofInstance* inst )
{
    GNCBook* pBook = QOF_BOOK(inst);
    const GUID* guid;

    (void)gnc_gda_do_db_operation( be,
                        qof_instance_get_destroying(inst) ? OP_DB_DELETE : OP_DB_ADD_OR_UPDATE,
                        BOOK_TABLE,
                        GNC_ID_BOOK, pBook,
                        col_table );

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
        commit_book,                /* commit */
        load_all_books,                    /* initial_load */
        create_book_tables            /* create_tables */
    };

    qof_object_register_backend( GNC_ID_BOOK, GNC_GDA_BACKEND, &be_data );
}
/* ========================== END OF FILE ===================== */
