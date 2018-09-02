/***********************************************************************\
 * gnc-sql-object-backend.hpp: Encapsulate per-class table schema.     *
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

#ifndef __GNC_SQL_OBJECT_BACKEND_HPP__
#define __GNC_SQL_OBJECT_BACKEND_HPP__

extern "C"
{
#include <qof.h>
}
#include <memory>
#include <string>
#include <vector>

class GncSqlBackend;
class GncSqlColumnTableEntry;
using GncSqlColumnTableEntryPtr = std::shared_ptr<GncSqlColumnTableEntry>;
using EntryVec = std::vector<GncSqlColumnTableEntryPtr>;

#define GNC_SQL_BACKEND "gnc:sql:1"

/**
 * Encapsulates per-class table schema with functions to load, create a table,
 * commit a changed front-end object (note that database transaction semantics
 * are not yet implemented; edit/commit applies to the front-end object!) and
 * write all front-end objects of the type to the database. Additional functions
 * for creating and running queries existed but were unused and untested. They've
 * been temporarily removed until the front end is ready to use them.
 */
class GncSqlObjectBackend
{
public:
    GncSqlObjectBackend (int version, const std::string& type,
                         const std::string& table, const EntryVec& vec) :
        m_table_name{table}, m_version{version}, m_type_name{type},
        m_col_table(vec) {}
    virtual ~GncSqlObjectBackend() = default;
    /**
     * Load all objects of m_type in the database into memory.
     * @param sql_be The GncSqlBackend containing the database connection.
     */
    virtual void load_all (GncSqlBackend* sql_be) = 0;
    /**
     * Conditionally create or update a database table from m_col_table. The
     * condition is the version returned by querying the database's version
     * table: If it's 0 then the table wasn't found and will be created; All
     * tables areat least version 1. If the database's version is less than the
     * compiled version then the table schema is upgraded but the data isn't,
     * that's the engine's responsibility when the object is loaded. If the
     * version is greater than the compiled version then nothing is touched.
     * @param sql_be The GncSqlBackend containing the database connection.
     */
    virtual void create_tables (GncSqlBackend* sql_be);
    /**
     * UPDATE/INSERT a single instance of m_type_name into the database.
     * @param sql_be The GncSqlBackend containing the database.
     * @param inst The QofInstance to be written out.
     */
    virtual bool commit (GncSqlBackend* sql_be, QofInstance* inst);
    /**
     * Write all objects of m_type_name to the database.
     * @param sql_be The GncSqlBackend containing the database.
     * @return true if the objects were successfully written, false otherwise.
     */
    virtual bool write (GncSqlBackend* sql_be) { return true; }
    /**
     * Return the m_type_name for the class. This value is created at
     * compilation time and is called QofIdType or QofIdTypeConst in other parts
     * of GnuCash. Most values are defined in src/engine/gnc-engine.h.
     * @return m_type_name.
     */
    const char* type () const noexcept { return m_type_name.c_str(); }
    /**
     * Compare a version with the compiled version (m_version).
     * @return true if they match.
     */
    const bool is_version (int version) const noexcept {
        return version == m_version;
    }
    /**
     * Check the presence of an object in the backend's database.
     *
     * @param sql_be Backend owning the database
     * @param inst QofInstance to be checked.
     */
    bool instance_in_db(const GncSqlBackend* sql_be,
                        QofInstance* inst) const noexcept;
protected:
    const std::string m_table_name;
    const int m_version;
    const std::string m_type_name; /// The front-end QofIdType
    const EntryVec& m_col_table;   /// The ORM table definition.
};

using GncSqlObjectBackendPtr = std::shared_ptr<GncSqlObjectBackend>;

using OBEEntry = std::tuple<std::string, GncSqlObjectBackendPtr>;
using OBEVec = std::vector<OBEEntry>;

/**
 * Data-passing struct for callbacks to qof_object_foreach() used in
 * GncSqlObjectBackend::write(). Once QofCollection is rewritten to use C++
 * containers we'll use std::foreach() and lambdas instead of callbacks and this
 * can go away.
 */
struct write_objects_t
{
    write_objects_t() = default;
    write_objects_t (GncSqlBackend* sql_be, bool o, GncSqlObjectBackend* e) :
        be{sql_be}, is_ok{o}, obe{e} {}
    void commit (QofInstance* inst) {
        if (is_ok) is_ok = obe->commit (be, inst);
    }
    GncSqlBackend* be = nullptr;
    bool is_ok = false;
    GncSqlObjectBackend* obe = nullptr;
};


#endif //__GNC_SQL_OBJECT_BACKEND_HPP__
