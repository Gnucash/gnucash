/********************************************************************
 * gnc-backend-dbi-priv.h: load and save data to SQL via libdbi     *
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

/* Private structures and variables for gnc-backend-dbi.c and its unit tests */
#ifndef GNC_BACKEND_DBI_PRIV_H
#define GNC_BACKEND_DBI_PRIV_H
#ifdef __cplusplus
extern "C"
{
#endif
#include <dbi/dbi.h>
#ifdef __cplusplus
}
#endif
#include "gnc-backend-sql.h"

/**
 * Options to conn_table_operation
 * @var drop Drop (remove without recourse) the table from the database
 * @var empty Delete all of the records from the table
 * @var backup Rename the table "name" to "name_back"
 * @var rollback drop the name table if it exists and rename name_back to name
 * @var drop_backup Drop the backup table
 */
typedef enum
{
    drop = 0,
    empty,
    backup,
    rollback,
    drop_backup
} TableOpType;

/**
 * Return values from conn_test_dbi_library
 * @var GNC_DBI_PASS Did not find the large numbers bug
 * @var GNC_DBI_FAIL_SETUP Could not completed the test
 * @var GNC_DBI_FAIL_TEST Found the large numbers bug
 */
typedef enum
{
    GNC_DBI_PASS = 0,
    GNC_DBI_FAIL_SETUP,
    GNC_DBI_FAIL_TEST
} GncDbiTestResult;

typedef gchar* (*CREATE_TABLE_DDL_FN)   ( GncSqlConnection* conn,
                                          const gchar* table_name,
                                          const GList* col_info_list );
typedef GSList* (*GET_TABLE_LIST_FN)    ( dbi_conn conn, const gchar* dbname );
typedef void    (*APPEND_COLUMN_DEF_FN) ( GString* ddl, GncSqlColumnInfo* info );
typedef GSList* (*GET_INDEX_LIST_FN)    ( dbi_conn conn );
typedef void    (*DROP_INDEX_FN)        ( dbi_conn conn, const gchar* index );
typedef struct
{
    CREATE_TABLE_DDL_FN     create_table_ddl;
    GET_TABLE_LIST_FN       get_table_list;
    APPEND_COLUMN_DEF_FN    append_col_def;
    GET_INDEX_LIST_FN       get_index_list;
    DROP_INDEX_FN           drop_index;
} provider_functions_t;


struct GncDbiBackend_struct
{
    GncSqlBackend sql_be;

    dbi_conn conn;

    /*@ dependent @*/
    QofBook *primary_book;	/* The primary, main open book */
    gboolean	loading;		/* We are performing an initial load */
    gboolean  in_query;
    gboolean  supports_transactions;
    gboolean  is_pristine_db;	// Are we saving to a new pristine db?
    gboolean  exists;         // Does the database exist?

    gint obj_total;			// Total # of objects (for percentage calculation)
    gint operations_done;		// Number of operations (save/load) done
//  GHashTable* versions;		// Version number for each table
};

typedef struct GncDbiBackend_struct GncDbiBackend;

typedef struct
{
    GncSqlConnection base;

    /*@ observer @*/
    QofBackend* qbe;
    /*@ observer @*/
    dbi_conn conn;
    /*@ observer @*/
    provider_functions_t* provider;
    gboolean conn_ok;       // Used by the error handler routines to flag if the connection is ok to use
    gint last_error;        // Code of the last error that occurred. This is set in the error callback function
    gint error_repeat;      // Used in case of transient errors. After such error, another attempt at the
    // original call is allowed. error_repeat tracks the number of attempts and can
    // be used to prevent infinite loops.
    gboolean retry;         // Signals the calling function that it should retry (the error handler detected
    // transient error and managed to resolve it, but it can't run the original query)

} GncDbiSqlConnection;

#endif //GNC_BACKEND_DBI_PRIV_H
