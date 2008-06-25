/********************************************************************
 * gnc-backend-sql.h: load and save data to SQL                     *
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

/**
 * @defgroup SQLBE SQL Backend Core
  @{
*/

/** @addtogroup Columns Columns
    @ingroup SQLBE
*/

/**
  @}
*/

/** @addtogroup SQLBE
    @{
*/
/** @addtogroup SQLBE

 * The SQL backend core is a library which can form the core for a QOF
 * backend based on an SQL library.

*/

/** @file gnc-backend-sql.h
 *  @brief load and save data to SQL
 *  @author Copyright (c) 2006-2008 Phil Longstaff <plongstaff@rogers.com>
 *
   @}
 */ 

#ifndef GNC_BACKEND_SQL_H_
#define GNC_BACKEND_SQL_H_

#include "qof.h"
#include "qofbackend-p.h"
#include <gmodule.h>

typedef struct GncSqlConnection GncSqlConnection;

/**
 * @struct GncSqlBackend
 *
 * Main SQL backend structure.
 */
struct GncSqlBackend
{
  QofBackend be;			/**< QOF backend */
  GncSqlConnection* conn;	/**< SQL connection */
  QofBook *primary_book;	/**< The primary, main open book */
  gboolean	loading;		/**< We are performing an initial load */
  gboolean  in_query;		/**< We are processing a query */
  gboolean  is_pristine_db;	/**< Are we saving to a new pristine db? */
  gint obj_total;			/**< Total # of objects (for percentage calculation) */
  gint operations_done;		/**< Number of operations (save/load) done */
  GHashTable* versions;		/**< Version number for each table */
};
typedef struct GncSqlBackend GncSqlBackend;

/**
 * Initialize the SQL backend.
 *
 * @param be SQL backend
 */
void gnc_sql_init( GncSqlBackend* be );

/**
 * Load the contents of an SQL database into a book.
 *
 * @param be SQL backend
 * @param book Book to be loaded
 */
void gnc_sql_load( GncSqlBackend* be, QofBook *book );

/**
 * Save the contents of a book to an SQL database.
 *
 * @param be SQL backend
 * @param book Book to be saved
 */
void gnc_sql_sync_all( GncSqlBackend* be, QofBook *book );

/**
 * An object is about to be edited.
 *
 * @param be SQL backend
 * @param inst Object being edited
 */
void gnc_sql_begin_edit( GncSqlBackend* be, QofInstance *inst );

/**
 * Object editing has been cancelled.
 *
 * @param be SQL backend
 * @param inst Object being edited
 */
void gnc_sql_rollback_edit( GncSqlBackend* qbe, QofInstance *inst );

/**
 * Object editting is complete and the object should be saved.
 *
 * @param be SQL backend
 * @param inst Object being edited
 */
void gnc_sql_commit_edit( GncSqlBackend* qbe, QofInstance *inst );

/**
 */
typedef struct GncSqlColumnTableEntry GncSqlColumnTableEntry;
typedef struct GncSqlStatement GncSqlStatement;
typedef struct GncSqlResult GncSqlResult;
typedef struct GncSqlRow GncSqlRow;

/**
 *@struct GncSqlStatement
 *
 * Struct which represents an SQL statement.  SQL backends must provide a
 * structure which implements all of the functions.
 */
struct GncSqlStatement
{
	void (*dispose)( GncSqlStatement* );
	gchar* (*toSql)( GncSqlStatement* );
	void (*addWhereCond)( GncSqlStatement*, QofIdTypeConst, gpointer, const GncSqlColumnTableEntry*, GValue* );
};
#define gnc_sql_statement_dispose(STMT) \
		(STMT)->dispose(STMT)
#define gnc_sql_statement_to_sql(STMT) \
		(STMT)->toSql(STMT)
#define gnc_sql_statement_add_where_cond(STMT,TYPENAME,OBJ,COLDESC,VALUE) \
		(STMT)->addWhereCond(STMT, TYPENAME, OBJ, COLDESC, VALUE)

/**
 * @struct GncSqlConnection
 *
 * Struct which represents the connection to an SQL database.  SQL backends
 * must provide a structure which implements all of the functions.
 */
struct GncSqlConnection
{
	void (*dispose)( GncSqlConnection* );
	GncSqlResult* (*executeSelectStatement)( GncSqlConnection*, GncSqlStatement* );
	gint (*executeNonSelectStatement)( GncSqlConnection*, GncSqlStatement* );
	GncSqlStatement* (*createStatementFromSql)( GncSqlConnection*, gchar* );
	gboolean (*doesTableExist)( GncSqlConnection*, const gchar* );
	void (*beginTransaction)( GncSqlConnection* );
	void (*rollbackTransaction)( GncSqlConnection* );
	void (*commitTransaction)( GncSqlConnection* );
	const gchar* (*getColumnTypeName)( GncSqlConnection*, GType, gint size );
	void (*createTable)( GncSqlConnection*, const gchar*, const GList* );
	void (*createIndex)( GncSqlConnection*, const gchar*, const gchar*, const GncSqlColumnTableEntry* );
	gchar* (*quoteString)( const GncSqlConnection*, gchar* );
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
#define gnc_sql_connection_quote_string(CONN,STR) \
		(CONN)->quoteString(CONN,STR)

/**
 * @struct GncSqlRow
 *
 * Struct used to represent a row in the result of an SQL SELECT statement.
 * SQL backends must provide a structure which implements all of the functions.
 */
struct GncSqlRow
{
	const GValue* (*getValueAtColName)( GncSqlRow*, const gchar* );
	void (*dispose)( GncSqlRow* );
};
#define gnc_sql_row_get_value_at_col_name(ROW,N) \
		(ROW)->getValueAtColName(ROW,N)
#define gnc_sql_row_dispose(ROW) \
		(ROW)->dispose(ROW)

/**
 * @struct GncSqlResult
 *
 * Struct used to represent the result of an SQL SELECT statement.  SQL
 * backends must provide a structure which implements all of the functions.
 */
struct GncSqlResult
{
	gint (*getNumRows)( GncSqlResult* );
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

/**
 * @struct GncSqlObjectBackend
 *
 * Struct used to handle a specific engine object type for an SQL backend.
 * This * handler should be registered with qof_object_register_backend().
 *
 * commit()			- commit an object to the db
 * initial_load()	- load stuff when new db opened
 * create_tables()  - create any db tables
 * compile_query()  - compile a backend object query
 * run_query()      - run a compiled query
 * free_query()     - free a compiled query
 * write()          - write all objects
 */
typedef struct
{
  int		version;		/**< Backend version number */
  const gchar *	type_name;	/**< Engine object type name */
  /** Commit an instance of this object to the database */
  void		(*commit)( GncSqlBackend* be, QofInstance* inst );
  /** Load all objects of this type from the database */
  void		(*initial_load)( GncSqlBackend* be );
  /** Create database tables for this object */
  void		(*create_tables)( GncSqlBackend* be );
  /** Compile a query on these objects */
  gpointer	(*compile_query)( GncSqlBackend* be, QofQuery* pQuery );
  /** Run a query on these objects */
  void		(*run_query)( GncSqlBackend* be, gpointer pQuery );
  /** Free a query on these objects */
  void		(*free_query)( GncSqlBackend* be, gpointer pQuery );
  /** Write all objects of this type to the database */
  void		(*write)( GncSqlBackend* be );
} GncSqlObjectBackend;
#define GNC_SQL_BACKEND             "gnc:sql:1"
#define GNC_SQL_BACKEND_VERSION	1

/**
 * @struct GncSqlColumnInfo
 */
typedef struct {
	const gchar* name;		/**< Column name */
	const gchar* type_name;	/**< Column SQL type name */
	gint size;				/**< Column size (string types) */
	gboolean is_primary_key;
	gboolean null_allowed;
} GncSqlColumnInfo;

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

/**
 * @struct GncSqlColumnTableEntry
 *
 * The GncSqlColumnTableEntry struct contains all of the information
 * required to copy information between an object and the database.
 * The database description for an object consists of an array of
 * GncSqlColumnTableEntry objects, with a final member having col_name == NULL.
 */
struct GncSqlColumnTableEntry {
	const gchar* col_name;	/**< Column name */
	const gchar* col_type;	/**< Column type */
	gint size;				/**< Column size in bytes, for string columns */
#define COL_PKEY	0x01
#define COL_NNUL	0x02
#define COL_UNIQUE	0x04
#define COL_AUTOINC	0x08
	gint flags;				/**< Column flags */
	const gchar* gobj_param_name; /**< If non-null, g_object param name */
	const gchar* qof_param_name;  /**< If non-null, qof parameter name */
	QofAccessFunc getter;	/**< General access function */
	QofSetterFunc setter;	/**< General setter function */
};

typedef enum {
	OP_DB_ADD,
	OP_DB_ADD_OR_UPDATE,
	OP_DB_DELETE
} E_DB_OPERATION;

typedef void (*GNC_SQL_LOAD_FN)( const GncSqlBackend* be,
								GncSqlRow* row,
                                QofSetterFunc setter, gpointer pObject,
                                const GncSqlColumnTableEntry* table );
typedef void (*GNC_SQL_ADD_COL_INFO_TO_LIST_FN)( const GncSqlBackend* be,
                        						const GncSqlColumnTableEntry* table_row,
												GList** pList );
typedef void (*GNC_SQL_ADD_COLNAME_TO_LIST_FN)( const GncSqlColumnTableEntry* table_row, GList** pList );
typedef void (*GNC_SQL_ADD_GVALUE_TO_SLIST_FN)( const GncSqlBackend* be,
                QofIdTypeConst obj_name, const gpointer pObject,
                const GncSqlColumnTableEntry* table_row, GSList** pList );

typedef struct {
    GNC_SQL_LOAD_FN                 load_fn;
    GNC_SQL_ADD_COL_INFO_TO_LIST_FN add_col_info_to_list_fn;
    GNC_SQL_ADD_COLNAME_TO_LIST_FN  add_colname_to_list_fn;
	GNC_SQL_ADD_GVALUE_TO_SLIST_FN	add_gvalue_to_slist_fn;
} col_type_handler_t;

/**
 * Returns the QOF access function for a column.
 *
 * @param obj_name QOF object type name
 * @param table_row DB table column
 * @return Access function
 */
QofAccessFunc gnc_sql_get_getter( QofIdTypeConst obj_name, const GncSqlColumnTableEntry* table_row );

/**
 * Adds a column name to a list.  If the column type spans multiple columns,
 * all of the column names for the pieces are added.
 *
 * @param table_row DB table column
 * @pList List
 */
void gnc_sql_add_colname_to_list( const GncSqlColumnTableEntry* table_row, GList** pList );

/**
 * Performs an operation on the database.
 *
 * @param be SQL backend struct
 * @param op Operation type
 * @param table_name SQL table name
 * @param obj_name QOF object type name
 * @param pObject Gnucash object
 * @param table DB table description
 * @return TRUE if successful, FALSE if not
 */
gboolean gnc_sql_do_db_operation( GncSqlBackend* be,
									E_DB_OPERATION op,
									const gchar* table_name,
									QofIdTypeConst obj_name,
									gpointer pObject,
									const GncSqlColumnTableEntry* table );

/**
 * Execute an SQL SELECT statement.
 *
 * @param be SQL backend struct
 * @param statement Statement
 * @return Results
 */
GncSqlResult* gnc_sql_execute_select_statement( GncSqlBackend* be, GncSqlStatement* statement );

/**
 * Executes an SQL SELECT statement from an SQL char string.
 *
 * @param be SQL backend struct
 * @param sql SQL SELECT string
 * @return Results
 */
GncSqlResult* gnc_sql_execute_select_sql( const GncSqlBackend* be, gchar* sql );

/**
 * Creates a statement from an SQL char string.
 *
 * @param be SQL backend struct
 * @param sql SQL char string
 * @return Statement
 */
GncSqlStatement* gnc_sql_create_statement_from_sql( const GncSqlBackend* be, gchar* sql );

/**
 * Loads a Gnucash object from the database.
 *
 * @param be SQL backend struct
 * @param row DB result row
 * @param obj_name QOF object type name
 * @param pObject Object to be loaded
 * @param table DB table description
 */
void gnc_sql_load_object( const GncSqlBackend* be, GncSqlRow* row,
						QofIdTypeConst obj_name, gpointer pObject,
						const GncSqlColumnTableEntry* table );

/**
 * Checks whether an object is in the database or not.
 *
 * @param be SQL backend struct
 * @param table_name DB table name
 * @param obj_name QOF object type name
 * @param pObject Object to be checked
 * @param table DB table description
 * @return TRUE if the object is in the database, FALSE otherwise
 */
gboolean gnc_sql_object_is_it_in_db( GncSqlBackend* be,
									const gchar* table_name,
									QofIdTypeConst obj_name, const gpointer pObject,
									const GncSqlColumnTableEntry* table );

/**
 * Returns the version number for a DB table.
 *
 * @param be SQL backend struct
 * @param table_name Table name
 * @return Version number, or 0 if the table does not exist
 */
gint gnc_sql_get_table_version( const GncSqlBackend* be, const gchar* table_name );

/**
 * Creates a table in the database
 *
 * @param be SQL backend struct
 * @param table_name Table name
 * @param table_version Table version
 * @param col_table DB table description
 * @return TRUE if successful, FALSE if unsuccessful
 */
gboolean gnc_sql_create_table( const GncSqlBackend* be, const gchar* table_name,
								gint table_version, const GncSqlColumnTableEntry* col_table );

/**
 * Creates an index in the database
 *
 * @param be SQL backend struct
 * @param index_name Index name
 * @param table_name Table name
 * @param col_table Columns that the index should index
 */
void gnc_sql_create_index( const GncSqlBackend* be, const gchar* index_name,
						const gchar* table_name, const GncSqlColumnTableEntry* col_table );

/**
 * Loads the object guid from a database row.  The table must have a column
 * named "guid" with type CT_GUID.
 *
 * @param be SQL backend struct
 * @param row Database row
 * @return GUID
 */
const GUID* gnc_sql_load_guid( const GncSqlBackend* be, GncSqlRow* row );

/**
 * Loads the transaction guid from a database row.  The table must have a column
 * named "tx_guid" with type CT_GUID.
 *
 * @param be SQL backend struct
 * @param row Database row
 * @return GUID
 */
const GUID* gnc_sql_load_tx_guid( const GncSqlBackend* be, GncSqlRow* row );

/**
 * Creates a basic SELECT statement for a table.
 *
 * @param be SQL backend struct
 * @param table_name Table name
 * @return Statement
 */
GncSqlStatement* gnc_sql_create_select_statement( const GncSqlBackend* be,
										const gchar* table_name );

/**
 * Registers a column handler for a new column type.
 *
 * @param colType Column type
 * @param handler Column handler
 */
void gnc_sql_register_col_type_handler( const gchar* colType, const col_type_handler_t* handler );

/**
 * Adds a GValue for an object reference GUID to the end of a GSList.
 *
 * @param be SQL backend struct
 * @param obj_name QOF object type name
 * @param pObject Object
 * @param table_row DB table column description
 * @param pList List
 */
void gnc_sql_add_gvalue_objectref_guid_to_slist( const GncSqlBackend* be,
							QofIdTypeConst obj_name, const gpointer pObject,
							const GncSqlColumnTableEntry* table_row, GSList** pList );

/**
 * Adds a column info structure for an object reference GUID to the end of a
 * GList.
 *
 * @param be SQL backend struct
 * @param table_row DB table column description
 * @param pList List
 */
void gnc_sql_add_objectref_guid_col_info_to_list( const GncSqlBackend* be,
	            const GncSqlColumnTableEntry* table_row, GList** pList );

/**
 * Appends the ascii strings for a list of GUIDs to the end of an SQL string.
 *
 * @param str SQL string
 * @param list List of GUIDs
 * @param maxCount Max # of GUIDs to append
 * @return Number of GUIDs appended
 */
guint gnc_sql_append_guid_list_to_sql( GString* str, GList* list, guint maxCount );

/**
 * Appends column names for a subtable to the end of a GList.
 *
 * @param table_row Main DB column description
 * @param subtable Sub-column description table
 * @param pList List
 */
void gnc_sql_add_subtable_colnames_to_list( const GncSqlColumnTableEntry* table_row,
								const GncSqlColumnTableEntry* subtable,
								GList** pList );

/**
 * Returns a string corresponding to the SQL representation of a GValue.  The
 * caller must free the string.
 *
 * @param conn SQL connection
 * @param value Value to be converted
 * @return String
 */
gchar* gnc_sql_get_sql_value( const GncSqlConnection* conn, const GValue* value );

/**
 * Initializes DB table version information.
 *
 * @param be SQL backend struct
 */
void gnc_sql_init_version_info( GncSqlBackend* be );

/**
 * Finalizes DB table version information.
 *
 * @param be SQL backend struct
 */
void gnc_sql_finalize_version_info( GncSqlBackend* be );

void _retrieve_guid_( gpointer pObject, gpointer pValue );

#endif /* GNC_BACKEND_SQL_H_ */
