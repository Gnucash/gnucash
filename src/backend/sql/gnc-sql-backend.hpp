/***********************************************************************\
 * gnc-sql-backend.hpp: Qof Backend for SQL Databases                  *
 *                                                                     *
 * Copyright 2016 John Ralls <jralls@ceridwen.us>                      *
 *                                                                     *
 * This program is free software; you can redistribute it and/or       *
 * modify it under the terms of the GNU General Public License as      *
 * published by the Free Software Foundation; either version 2 of      *
 * the License, or (at your option) any later version.                 *
 *                                                                     *
 * This program is distributed in the hope that it will be useful,     *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of      *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       *
 * GNU General Public License for more details.                        *
 *                                                                     *
 * You should have received a copy of the GNU General Public License   *
 * along with this program; if not, contact:                           *
 *                                                                     *
 * Free Software Foundation           Voice:  +1-617-542-5942          *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652          *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                      *
\***********************************************************************/

#ifndef __GNC_SQL_BACKEND_HPP__
#define __GNC_SQL_BACKEND_HPP__

extern "C"
{
#include <qof.h>
#include <Account.h>
}
#include <memory>
#include <exception>
#include <sstream>
#include <vector>
#include <qof-backend.hpp>

class GncSqlColumnTableEntry;
using GncSqlColumnTableEntryPtr = std::shared_ptr<GncSqlColumnTableEntry>;
using EntryVec = std::vector<GncSqlColumnTableEntryPtr>;
class GncSqlObjectBackend;
using GncSqlObjectBackendPtr = std::shared_ptr<GncSqlObjectBackend>;
using OBEEntry = std::tuple<std::string, GncSqlObjectBackendPtr>;
using OBEVec = std::vector<OBEEntry>;
class GncSqlConnection;
class GncSqlStatement;
using GncSqlStatementPtr = std::unique_ptr<GncSqlStatement>;
class GncSqlResult;
using GncSqlResultPtr = GncSqlResult*;
using VersionPair = std::pair<const std::string, unsigned int>;
using VersionVec = std::vector<VersionPair>;
using uint_t = unsigned int;

typedef enum
{
    OP_DB_INSERT,
    OP_DB_UPDATE,
    OP_DB_DELETE
} E_DB_OPERATION;

/**
 *
 * Main SQL backend structure.
 */
class GncSqlBackend
{
public:
    GncSqlBackend(GncSqlConnection *conn, QofBook* book);
    virtual ~GncSqlBackend() = default;
    /** Connect the backend to a GncSqlConnection.
     * Sets up version info. Calling with nullptr clears the connection and
     * destroys the version info.
     */
    void connect(GncSqlConnection *conn) noexcept;
    /**
     * Initializes DB table version information.
     */
    void init_version_info() noexcept;
    bool reset_version_info() noexcept;
    /**
     * Finalizes DB table version information.
     */
    void finalize_version_info() noexcept;
    /* FIXME: These are just pass-throughs of m_conn functions. */
    GncSqlStatementPtr create_statement_from_sql(const std::string& str) const noexcept;
    /** Executes an SQL SELECT statement and returns the result rows.  If an
     * error occurs, an entry is added to the log, an error status is returned
     * to qof and nullptr is returned.
     *
     * @param statement Statement
     * @return Results, or nullptr if an error has occured
     */
    GncSqlResultPtr execute_select_statement(const GncSqlStatementPtr& stmt) const noexcept;
    int execute_nonselect_statement(const GncSqlStatementPtr& stmt) const noexcept;
    std::string quote_string(const std::string&) const noexcept;
    /**
     * Creates a table in the database
     *
     * @param table_name Table name
     * @param col_table DB table description
     * @return TRUE if successful, FALSE if unsuccessful
     */
    bool create_table(const std::string& table_name, const EntryVec& col_table) const noexcept;
    /**
     * Creates a table in the database and sets its version
     *
     * @param table_name Table name
     * @param table_version Table version
     * @param col_table DB table description
     * @return TRUE if successful, FALSE if unsuccessful
     */
    bool create_table(const std::string& table_name, int table_version,
                      const EntryVec& col_table) noexcept;
    /**
     * Create/update all tables in the database
     */
    void create_tables() noexcept;

    /**
     * Creates an index in the database
     *
     * @param index_name Index name
     * @param table_name Table name
     * @param col_table Columns that the index should index
     * @return TRUE if successful, FALSE if unsuccessful
     */
    bool create_index(const std::string& index_name,
                      const std::string& table_name,
                      const EntryVec& col_table) const noexcept;
    /**
     * Adds one or more columns to an existing table.
     *
     * @param table_name SQL table name
     * @param new_col_table Column table for new columns
     * @return TRUE if successful, FALSE if unsuccessful
     */
    bool add_columns_to_table(const std::string& table_name,
                              const EntryVec& col_table) const noexcept;
    /**
     * Upgrades a table to a new structure.
     *
     * The upgrade is done by creating a new table with the new structure,
     * SELECTing the old data into the new table, deleting the old table, then
     * renaming the new table.  Therefore, this will only work if the new table
     * structure is similar enough to the old table that the SELECT will work.
     *
     * @param table_name SQL table name
     * @param col_table Column table
     */
    void upgrade_table (const std::string& table_name,
                        const EntryVec& col_table) noexcept;
    /**
     * Returns the version number for a DB table.
     *
     * @param table_name Table name
     * @return Version number, or 0 if the table does not exist
     */
    uint_t get_table_version(const std::string& table_name) const noexcept;
    bool set_table_version (const std::string& table_name, uint_t version) noexcept;
    /**
     * Load the contents of an SQL database into a book.
     *
     * @param book Book to be loaded
     */
    void load(QofBook*, QofBackendLoadType);
    /**
     * Save the contents of a book to an SQL database.
     *
     * @param book Book to be saved
     */
    void sync_all(QofBook*);
    /**
     * An object is about to be edited.
     *
     * @param inst Object being edited
     */
    void begin_edit(QofInstance*);
    /**
     * Object editting is complete and the object should be saved.
     *
     * @param inst Object being edited
     */
    void commit_edit(QofInstance*);
    /**
     * Object editing has been cancelled.
     *
     * @param inst Object being edited
     */
    void rollback_edit(QofInstance*);
    /**
     * Register a commodity to be committed after loading is complete.
     *
     * Necessary to save corrections made while loading.
     * @param comm The commodity item to be committed.
     */
    void commodity_for_postload_processing(gnc_commodity*);
    /**
     * Get the GncSqlObjectBackend for the indicated type.
     *
     * Required because we need to pass a pointer to this to a callback via a C
     * function.
     * @param type: The QofInstance type constant to select the object backend.
     */
    GncSqlObjectBackendPtr get_object_backend(const std::string& type) const noexcept;
    /**
     * Checks whether an object is in the database or not.
     *
     * @param table_name DB table name
     * @param obj_name QOF object type name
     * @param pObject Object to be checked
     * @param table DB table description
     * @return TRUE if the object is in the database, FALSE otherwise
     */
    bool object_in_db (const char* table_name, QofIdTypeConst obj_name,
                       const gpointer pObject, const EntryVec& table ) const noexcept;
    /**
     * Performs an operation on the database.
     *
     * @param op Operation type
     * @param table_name SQL table name
     * @param obj_name QOF object type name
     * @param pObject Gnucash object
     * @param table DB table description
     * @return TRUE if successful, FALSE if not
     */
    bool do_db_operation (E_DB_OPERATION op, const char* table_name,
                          QofIdTypeConst obj_name, gpointer pObject,
                          const EntryVec& table) const noexcept;
    /**
     * Ensure that a commodity referenced in another object is in fact saved
     * in the database.
     *
     * @param comm The commodity in question
     * @return true if the commodity needed to be saved.
     */
    bool save_commodity(gnc_commodity* comm) noexcept;
    QofBook* book() const noexcept { return m_book; }
    void set_loading(bool loading) noexcept { m_loading = loading; }
    bool pristine() const noexcept { return m_is_pristine_db; }
    void update_progress() const noexcept;
    void finish_progress() const noexcept;

protected:
    QofBackend qof_be;           /**< QOF backend. Not a pointer, nor really a member */
    GncSqlConnection* m_conn;  /**< SQL connection */
    QofBook* m_book;           /**< The primary, main open book */
    bool m_loading;        /**< We are performing an initial load */
    bool m_in_query;       /**< We are processing a query */
    bool m_is_pristine_db; /**< Are we saving to a new pristine db? */
    const char* m_timespec_format; /**< Server-specific date-time string format */
    VersionVec m_versions;    /**< Version number for each table */
private:
    bool write_account_tree(Account*);
    bool write_accounts();
    bool write_transactions();
    bool write_template_transactions();
    bool write_schedXactions();
    GncSqlStatementPtr build_insert_statement (const char* table_name,
                                               QofIdTypeConst obj_name,
                                               gpointer pObject,
                                               const EntryVec& table) const noexcept;
    GncSqlStatementPtr build_update_statement (const gchar* table_name,
                                               QofIdTypeConst obj_name,
                                               gpointer pObject,
                                               const EntryVec& table) const noexcept;
    GncSqlStatementPtr build_delete_statement (const char* table_name,
                                               QofIdTypeConst obj_name,
                                               gpointer pObject,
                                               const EntryVec& table) const noexcept;

    class ObjectBackendRegistry
    {
    public:
        ObjectBackendRegistry();
        ObjectBackendRegistry(const ObjectBackendRegistry&) = delete;
        ObjectBackendRegistry(const ObjectBackendRegistry&&) = delete;
        ObjectBackendRegistry operator=(const ObjectBackendRegistry&) = delete;
        ObjectBackendRegistry operator=(const ObjectBackendRegistry&&) = delete;
        ~ObjectBackendRegistry() = default;
        void register_backend(OBEEntry&& entry) noexcept;
        void register_backend(GncSqlObjectBackendPtr obe) noexcept;
        GncSqlObjectBackendPtr get_object_backend(const std::string& type) const;
        void load_remaining(GncSqlBackend*);
        OBEVec::iterator begin() { return m_registry.begin(); }
        OBEVec::iterator end() { return m_registry.end(); }
    private:
        OBEVec m_registry;
    };
    ObjectBackendRegistry m_backend_registry;
    std::vector<gnc_commodity*> m_postload_commodities;
};

#endif //__GNC_SQL_BACKEND_HPP__
