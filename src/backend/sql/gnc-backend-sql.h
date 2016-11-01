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

/** The SQL backend core is a library which can form the core for a QOF
 *  backend based on an SQL library.
 *
 *  @file gnc-backend-sql.h
 *  @brief load and save data to SQL
 *  @author Copyright (c) 2006-2008 Phil Longstaff <plongstaff@rogers.com>
 */

#ifndef GNC_BACKEND_SQL_H
#define GNC_BACKEND_SQL_H
extern "C"
{
#include <qof.h>
#include <qofbackend-p.h>
#include <gmodule.h>
}

#include <string>
#include <vector>

#include "gnc-sql-object-backend.hpp"

using StrVec = std::vector<std::string>;
using InstanceVec = std::vector<QofInstance*>;
using PairVec = std::vector<std::pair<std::string, std::string>>;
using uint_t = unsigned int;
class GncSqlRow;

/**
 * Initialize the SQL backend.
 *
 * @param sql_be SQL backend
 */
void gnc_sql_init (GncSqlBackend* sql_be);

/**
 * Load the contents of an SQL database into a book.
 *
 * @param sql_be SQL backend
 * @param book Book to be loaded
 */
void gnc_sql_load (GncSqlBackend* sql_be,  QofBook* book,
                   QofBackendLoadType loadType);

/**
 * Register a commodity to be committed after loading is complete.
 *
 * Necessary to save corrections made while loading.
 * @param sql_be SQL backend
 * @param comm The commodity item to be committed.
 */
void gnc_sql_push_commodity_for_postload_processing (GncSqlBackend* sql_be,
                                                     gpointer comm);

/**
 * Save the contents of a book to an SQL database.
 *
 * @param sql_be SQL backend
 * @param book Book to be saved
 */
void gnc_sql_sync_all (GncSqlBackend* sql_be,  QofBook* book);

/**
 * An object is about to be edited.
 *
 * @param sql_be SQL backend
 * @param inst Object being edited
 */
void gnc_sql_begin_edit (GncSqlBackend* sql_be, QofInstance* inst);

/**
 * Object editing has been cancelled.
 *
 * @param qsql_be SQL backend
 * @param inst Object being edited
 */
void gnc_sql_rollback_edit (GncSqlBackend* qsql_be, QofInstance* inst);

/**
 * Object editting is complete and the object should be saved.
 *
 * @param qsql_be SQL backend
 * @param inst Object being edited
 */
void gnc_sql_commit_edit (GncSqlBackend* qsql_be, QofInstance* inst);

/**
 */

#define GNC_SQL_BACKEND             "gnc:sql:1"
#define GNC_SQL_BACKEND_VERSION 1

void gnc_sql_register_backend(OBEEntry&&);
void gnc_sql_register_backend(GncSqlObjectBackendPtr);
const OBEVec& gnc_sql_get_backend_registry();
GncSqlObjectBackendPtr gnc_sql_get_object_backend(const std::string& table_name);

/**
 * Data-passing struct for callbacks to qof_object_foreach() used in
 * GncSqlObjectBackend::write(). Once QofCollection is rewritten to use C++
 * containers we'll use std::foreach() and lambdas instead of callbacks and this
 * can go away.
 */
struct write_objects_t
{
    write_objects_t() = default;
    write_objects_t (GncSqlBackend* sql_be, bool o, GncSqlObjectBackendPtr e) :
        be{sql_be}, is_ok{o}, obe{e} {}
    void commit (QofInstance* inst) {
        if (is_ok) is_ok = obe->commit (be, inst);
    }
    GncSqlBackend* be;
    bool is_ok;
    GncSqlObjectBackendPtr obe;
};



/**
 * Performs an operation on the database.
 *
 * @param sql_be SQL backend struct
 * @param op Operation type
 * @param table_name SQL table name
 * @param obj_name QOF object type name
 * @param pObject Gnucash object
 * @param table DB table description
 * @return TRUE if successful, FALSE if not
 */
gboolean gnc_sql_do_db_operation (GncSqlBackend* sql_be,
                                  E_DB_OPERATION op,
                                  const gchar* table_name,
                                  QofIdTypeConst obj_name,
                                  gpointer pObject,
                                  const EntryVec& table);


/**
 * Loads a Gnucash object from the database.
 *
 * @param sql_be SQL backend struct
 * @param row DB result row
 * @param obj_name QOF object type name
 * @param pObject Object to be loaded
 * @param table DB table description
 */
void gnc_sql_load_object (const GncSqlBackend* sql_be, GncSqlRow& row,
                          QofIdTypeConst obj_name, gpointer pObject,
                          const EntryVec& table);

/**
 * Checks whether an object is in the database or not.
 *
 * @param sql_be SQL backend struct
 * @param table_name DB table name
 * @param obj_name QOF object type name
 * @param pObject Object to be checked
 * @param table DB table description
 * @return TRUE if the object is in the database, FALSE otherwise
 */
gboolean gnc_sql_object_is_it_in_db (GncSqlBackend* sql_be,
                                     const gchar* table_name,
                                     QofIdTypeConst obj_name,
                                     const gpointer pObject,
                                     const EntryVec& table );
/**
 * Loads the object guid from a database row.  The table must have a column
 * named "guid" with type CT_GUID.
 *
 * @param sql_be SQL backend struct
 * @param row Database row
 * @return GncGUID
 */

const GncGUID* gnc_sql_load_guid (const GncSqlBackend* sql_be, GncSqlRow& row);


/**
 * Appends the ascii strings for a list of GUIDs to the end of an SQL string.
 *
 * @param str SQL string
 * @param list List of GUIDs
 * @param maxCount Max # of GUIDs to append
 * @return Number of GUIDs appended
 */
uint_t gnc_sql_append_guids_to_sql (std::stringstream& sql,
                                    const InstanceVec& instances);

void _retrieve_guid_ (gpointer pObject,  gpointer pValue);

gpointer gnc_sql_compile_query (QofBackend* qof_be, QofQuery* pQuery);
void gnc_sql_free_query (QofBackend* qof_be, gpointer pQuery);
void gnc_sql_run_query (QofBackend* qof_be, gpointer pQuery);

#endif /* GNC_BACKEND_SQL_H */

/**
   @}  end of the SQL Backend Core doxygen group
*/
