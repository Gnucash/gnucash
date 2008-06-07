/********************************************************************
 * gnc-backend-util-sql.h: load and save data to SQL                *
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
/** @file gnc-backend-sql.h
 *  @brief load and save data to SQL
 *  @author Copyright (c) 2006-2008 Phil Longstaff <plongstaff@rogers.com>
 *
 * This file implements the top-level QofBackend API for saving/
 * restoring data to/from an SQL database
 */

#ifndef GNC_BACKEND_SQL_UTIL_H_
#define GNC_BACKEND_SQL_UTIL_H_

#include "qof.h"
#include <gmodule.h>

#include "qofbackend-p.h"

/**
 */
typedef struct col_cvt col_cvt_t;
typedef struct GncSqlStatement GncSqlStatement;
typedef struct GncSqlResult GncSqlResult;
typedef struct GncSqlConnection GncSqlConnection;
typedef struct GncSqlRow GncSqlRow;

struct GncSqlStatement
{
	void (*dispose)( GncSqlStatement* );
	gchar* (*toSql)( GncSqlStatement* );
	void (*addWhereCond)( GncSqlStatement*, QofIdTypeConst, gpointer, const col_cvt_t*, GValue* );
};
#define gnc_sql_statement_dispose(STMT) \
		(STMT)->dispose(STMT)
#define gnc_sql_statement_to_sql(STMT) \
		(STMT)->toSql(STMT)
#define gnc_sql_statement_add_where_cond(STMT,TYPENAME,OBJ,COLDESC,VALUE) \
		(STMT)->addWhereCond(STMT, TYPENAME, OBJ, COLDESC, VALUE)

typedef struct {
	const gchar* name;
	const gchar* type_name;
	gint size;
	gboolean is_primary_key;
	gboolean null_allowed;
} GncSqlColumnInfo;

struct GncSqlConnection
{
	void (*dispose)( GncSqlConnection* );
	GncSqlResult* (*executeSelectStatement)( GncSqlConnection*, GncSqlStatement* );
	gint (*executeNonSelectStatement)( GncSqlConnection*, GncSqlStatement* );
	GncSqlStatement* (*createStatementFromSql)( GncSqlConnection*, const gchar* );
	gboolean (*doesTableExist)( GncSqlConnection*, const gchar* );
	void (*beginTransaction)( GncSqlConnection* );
	void (*rollbackTransaction)( GncSqlConnection* );
	void (*commitTransaction)( GncSqlConnection* );
	const gchar* (*getColumnTypeName)( GncSqlConnection*, GType, gint size );
	void (*createTable)( GncSqlConnection*, const gchar*, const GList* );
	void (*createIndex)( GncSqlConnection*, const gchar*, const gchar*, const col_cvt_t* );
};
#define gnc_sql_connection_dispose(CONN) (CONN)->dispose(CONN)
#define gnc_sql_connection_execute_select_statement(CONN,STMT) \
		(CONN)->executeSelectStatement(CONN,STMT)
#define gnc_sql_connection_execute_nonselect_statement(CONN,STMT) \
		(CONN)->executeNonSelectStatement(CONN,STMT)
#define gnc_sql_connection_create_statement_from_sql(CONN,SQL) \
		(CONN)->createStatementFromSql(CONN,SQL)
#define gnc_sql_connection_does_table_exist(CONN,NAME) \
		(CONN)->doesTableExist(CONN,NAME)
#define gnc_sql_connection_begin_transaction(CONN) \
		(CONN)->beginTransaction(CONN)
#define gnc_sql_connection_rollback_transaction(CONN) \
		(CONN)->rollbackTransaction(CONN)
#define gnc_sql_connection_commit_transaction(CONN) \
		(CONN)->commitTransaction(CONN)
#define gnc_sql_connection_get_column_type_name(CONN,TYPE,SIZE) \
		(CONN)->getColumnTypeName(CONN,TYPE,SIZE)
#define gnc_sql_connection_create_table(CONN,NAME,COLLIST) \
		(CONN)->createTable(CONN,NAME,COLLIST)
#define gnc_sql_connection_create_index(CONN,INDEXNAME,TABLENAME,COLTABLE) \
		(CONN)->createIndex(CONN,INDEXNAME,TABLENAME,COLTABLE)

struct GncSqlRow
{
	const GValue* (*getValueAtColName)( GncSqlRow*, const gchar* );
	void (*dispose)( GncSqlRow* );
};
#define gnc_sql_row_get_value_at_col_name(ROW,N) \
		(ROW)->getValueAtColName(ROW,N)
#define gnc_sql_row_dispose(ROW) \
		(ROW)->dispose(ROW)

struct GncSqlResult
{
	int (*getNumRows)( GncSqlResult* );
    GncSqlRow* (*getFirstRow)( GncSqlResult* );
	GncSqlRow* (*getNextRow)( GncSqlResult* );
	void (*dispose)( GncSqlResult* );
};
#define gnc_sql_result_get_num_rows(RESULT) \
		(RESULT)->getNumRows(RESULT)
#define gnc_sql_result_get_first_row(RESULT) \
		(RESULT)->getFirstRow(RESULT)
#define gnc_sql_result_get_next_row(RESULT) \
		(RESULT)->getNextRow(RESULT)
#define gnc_sql_result_dispose(RESULT) \
		(RESULT)->dispose(RESULT)

struct GncSqlBackend_struct
{
  QofBackend be;

  GncSqlConnection* conn;

  QofBook *primary_book;	/* The primary, main open book */
  gboolean	loading;		/* We are performing an initial load */
  gboolean  in_query;
  gboolean  supports_transactions;
  gboolean  is_pristine_db;	// Are we saving to a new pristine db?

  gint obj_total;			// Total # of objects (for percentage calculation)
  gint operations_done;		// Number of operations (save/load) done
  GHashTable* versions;		// Version number for each table
};
typedef struct GncSqlBackend_struct GncSqlBackend;

/**
 * Struct used to pass in a new data type for GDA storage.  This contains
 * the set of callbacks to read and write GDA for new data objects..  New
 * types should register an instance of this object with the engine.
 *
 * commit()			- commit an object to the db
 * initial_load()	- load stuff when new db opened
 * create_tables()  - create any db tables
 * compile_query()  - compile a backend object query
 * run_query()      - run a compiled query
 * free_query()     - free a compiled query
 * write()          - write all objects
 */
#define GNC_SQL_BACKEND             "gnc:sql:1"
#define GNC_SQL_BACKEND_VERSION	1
typedef struct
{
  int		version;	/* backend version number */
  const gchar *	type_name;	/* The XML tag for this type */

  void		(*commit)( QofInstance* inst, GncSqlBackend* be );
  void		(*initial_load)( GncSqlBackend* pBackend );
  void		(*create_tables)( GncSqlBackend* pBackend );
  gpointer	(*compile_query)( GncSqlBackend* pBackend, QofQuery* pQuery );
  void		(*run_query)( GncSqlBackend* pBackend, gpointer pQuery );
  void		(*free_query)( GncSqlBackend* pBackend, gpointer pQuery );
  void		(*write)( GncSqlBackend* pBackend );
} GncSqlDataType_t;

// Type for conversion of db row to object.
#define CT_STRING "ct_string"
#define CT_GUID "ct_guid"
#define CT_INT "ct_int"
#define CT_INT64 "ct_int64"
#define CT_TIMESPEC "ct_timespec"
#define CT_GDATE "ct_gdate"
#define CT_NUMERIC "ct_numeric"
#define CT_DOUBLE "ct_double"
#define CT_BOOLEAN "ct_boolean"
#define CT_ACCOUNTREF "ct_accountref"
#define CT_COMMODITYREF "ct_commodityref"
#define CT_TXREF "ct_txref"
#define CT_LOTREF "ct_lotref"

struct col_cvt {
	const gchar* col_name;
	const gchar* col_type;
	gint size;
#define COL_PKEY	0x01
#define COL_NNUL	0x02
#define COL_UNIQUE	0x04
#define COL_AUTOINC	0x08
	gint flags;
	const gchar* gobj_param_name;	// If non-null, use g_object_get/g_object_set
	const gchar* param_name;	// If non null, use qof getter/setter
	QofAccessFunc getter;
	QofSetterFunc setter;
};

typedef enum {
	OP_DB_ADD,
	OP_DB_ADD_OR_UPDATE,
	OP_DB_DELETE
} E_DB_OPERATION;

typedef void (*GNC_SQL_LOAD_FN)( const GncSqlBackend* be,
								GncSqlRow* row,
                                QofSetterFunc setter, gpointer pObject,
                                const col_cvt_t* table );
typedef void (*GNC_SQL_ADD_COL_INFO_TO_LIST_FN)( const GncSqlBackend* be,
                        						const col_cvt_t* table_row,
												GList** pList );
typedef void (*GNC_SQL_ADD_COLNAME_TO_LIST_FN)( const col_cvt_t* table_row, GList** pList );
typedef void (*GNC_SQL_ADD_GVALUE_TO_SLIST_FN)( const GncSqlBackend* be,
                QofIdTypeConst obj_name, const gpointer pObject,
                const col_cvt_t* table_row, GSList** pList );

typedef struct {
    GNC_SQL_LOAD_FN                 load_fn;
    GNC_SQL_ADD_COL_INFO_TO_LIST_FN add_col_info_to_list_fn;
    GNC_SQL_ADD_COLNAME_TO_LIST_FN  add_colname_to_list_fn;
	GNC_SQL_ADD_GVALUE_TO_SLIST_FN	add_gvalue_to_slist_fn;
} col_type_handler_t;

GncSqlColumnInfo* gnc_sql_create_column_info( const col_cvt_t* table_row, const gchar* type, gint size );
QofAccessFunc gnc_sql_get_getter( QofIdTypeConst obj_name, const col_cvt_t* table_row );

void gnc_sql_add_colname_to_list( const col_cvt_t* table_row, GList** pList );

gboolean gnc_sql_do_db_operation( GncSqlBackend* pBackend,
									E_DB_OPERATION op,
									const gchar* table_name,
									QofIdTypeConst obj_name,
									gpointer pObject,
									const col_cvt_t* table );
GncSqlStatement* gnc_sql_build_insert_statement( GncSqlBackend* pBackend,
									const gchar* table_name,
									QofIdTypeConst obj_name,
									gpointer pObject,
									const col_cvt_t* table );
GncSqlStatement* gnc_sql_build_update_statement( GncSqlBackend* pBackend,
									const gchar* table_name,
									QofIdTypeConst obj_name,
									gpointer pObject,
									const col_cvt_t* table );
GncSqlStatement* gnc_sql_build_delete_statement( GncSqlBackend* pBackend,
									const gchar* table_name,
									QofIdTypeConst obj_name,
									gpointer pObject,
									const col_cvt_t* table );
gint gnc_sql_execute_statement( GncSqlBackend* pBackend, GncSqlStatement* statement );
GncSqlResult* gnc_sql_execute_select_statement( GncSqlBackend* pBackend, GncSqlStatement* statement );
GncSqlResult* gnc_sql_execute_sql_statement( GncSqlBackend* pBackend, GncSqlStatement* sqlStmt );
GncSqlResult* gnc_sql_execute_select_sql( const GncSqlBackend* pBackend, const gchar* sql );
gint gnc_sql_execute_nonselect_sql( const GncSqlBackend* pBackend, const gchar* sql );
GncSqlStatement* gnc_sql_create_statement_from_sql( const GncSqlBackend* pBackend, const gchar* sql );
int gnc_sql_execute_select_get_count( const GncSqlBackend* pBackend, const gchar* sql );
int gnc_sql_execute_statement_get_count( GncSqlBackend* pBackend, GncSqlStatement* statement );
void gnc_sql_load_object( const GncSqlBackend* be, GncSqlRow* row,
						QofIdTypeConst obj_name, gpointer pObject,
						const col_cvt_t* table );
gboolean gnc_sql_object_is_it_in_db( GncSqlBackend* be,
									const gchar* table_name,
									QofIdTypeConst obj_name, const gpointer pObject,
									const col_cvt_t* table );
gint gnc_sql_get_table_version( const GncSqlBackend* be, const gchar* table_name );
gboolean gnc_sql_create_table( const GncSqlBackend* be, const gchar* table_name,
								gint table_version, const col_cvt_t* col_table,
						GError** error );
void gnc_sql_create_index( const GncSqlBackend* be, const gchar* index_name,
						const gchar* table_name, const col_cvt_t* col_table );
const GUID* gnc_sql_load_guid( const GncSqlBackend* be, GncSqlRow* row );
const GUID* gnc_sql_load_tx_guid( const GncSqlBackend* be, GncSqlRow* row );
GncSqlStatement* gnc_sql_create_select_statement( const GncSqlBackend* be, const gchar* table_name,
										const col_cvt_t* col_table );
void gnc_sql_register_col_type_handler( const gchar* colType, const col_type_handler_t* handler );
void gnc_sql_register_standard_col_type_handlers( void );

void gnc_sql_add_gvalue_objectref_guid_to_slist( const GncSqlBackend* be, QofIdTypeConst obj_name,
                const gpointer pObject, const col_cvt_t* table_row, GSList** pList );
void gnc_sql_add_objectref_guid_col_info_to_list( const GncSqlBackend* be,
	            const col_cvt_t* table_row, GList** pList );
guint gnc_sql_append_guid_list_to_sql( GString* str, GList* list, guint maxCount );
void gnc_sql_add_subtable_colnames_to_list( const col_cvt_t* table_row, const col_cvt_t* subtable,
								GList** pList );
gchar* gnc_sql_get_sql_value( const GValue* value );

void _retrieve_guid_( gpointer pObject, gpointer pValue );
void gnc_sql_init_version_info( GncSqlBackend* be );
void gnc_sql_reset_version_info( GncSqlBackend* be );
void gnc_sql_finalize_version_info( GncSqlBackend* be );

G_MODULE_EXPORT const gchar *
g_module_check_init( GModule *module );

#endif /* GNC_BACKEND_SQL_UTIL_H_ */
