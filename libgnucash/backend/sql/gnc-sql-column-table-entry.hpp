/***********************************************************************\
 * gnc-sql-column-table-entry.hpp: Column Specification for SQL Table. *
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

#ifndef __GNC_SQL_COLUMN_TABLE_ENTRY_HPP__
#define __GNC_SQL_COLUMN_TABLE_ENTRY_HPP__

extern "C"
{
#include <qof.h>
}
#include <memory>
#include <vector>
#include <iostream>
#include "gnc-sql-result.hpp"

struct GncSqlColumnInfo;
using ColVec = std::vector<GncSqlColumnInfo>;
using PairVec = std::vector<std::pair<std::string, std::string>>;
using InstanceVec = std::vector<QofInstance*>;
using uint_t = unsigned int;
class GncSqlBackend;

/**
 * Basic column type
 */
typedef enum
{
    BCT_STRING,
    BCT_INT,
    BCT_INT64,
    BCT_DATE,
    BCT_DOUBLE,
    BCT_DATETIME
} GncSqlBasicColumnType;

enum ColumnFlags : int
{
    COL_NO_FLAG = 0,
    COL_PKEY = 0x01,    /**< The column is a primary key */
    COL_NNUL = 0x02,    /**< The column may not contain a NULL value */
    COL_UNIQUE = 0x04,  /**< The column must contain unique values */
    COL_AUTOINC = 0x08  /**< The column is an auto-incrementing int */
};

// Type for conversion of db row to object.
enum GncSqlObjectType
{
    CT_STRING,
    CT_GUID,
    CT_INT,
    CT_INT64,
    CT_TIMESPEC,
    CT_TIME64,
    CT_GDATE,
    CT_NUMERIC,
    CT_DOUBLE,
    CT_BOOLEAN,
    CT_ACCOUNTREF,
    CT_BUDGETREF,
    CT_COMMODITYREF,
    CT_LOTREF,
    CT_TXREF,
    CT_ADDRESS,
    CT_BILLTERMREF,
    CT_INVOICEREF,
    CT_ORDERREF,
    CT_OWNERREF,
    CT_TAXTABLEREF
};

static inline std::string
quote_string(const std::string& str)
{
    if (str == "NULL" || str == "null") return "NULL";
    /* FIXME: This is here because transactions.num has a NULL
     * constraint, which is dumb; it's often empty.
     */
    if (str.empty()) return "''";
    std::string retval;
    retval.reserve(str.length() + 2);
    retval.insert(0, 1, '\'');
    for (auto c = str.begin(); c != str.end(); ++c)
    {
        if (*c == '\'')
            retval += *c;
        retval += *c;
    }
    retval += '\'';
    return retval;
}

/**
 * Contains all of the information required to copy information between an
 * object and the database for a specific object property.
 *
 * If an entry contains a gobj_param_name value, this string is used as the
 * property name for a call to g_object_get() or g_object_set().  If the
 * gobj_param_name value is NULL but qof_param_name is not NULL, this value
 * is used as the parameter name for a call to
 * qof_class_get_parameter_getter().  If both of these values are NULL, getter
 * and setter are the addresses of routines to return or set the parameter
 * value, respectively.
 *
 * The database description for an object consists of an array of
 * GncSqlColumnTableEntry objects, with a final member having col_name == NULL.
 */

class GncSqlColumnTableEntry
{
public:
    GncSqlColumnTableEntry (const char* name, const GncSqlObjectType type,
                            unsigned int s,
                            int f, const char* gobj_name = nullptr,
                            const char* qof_name = nullptr,
                            QofAccessFunc get = nullptr,
                            QofSetterFunc set = nullptr) :
        m_col_name{name}, m_col_type{type}, m_size{s},
        m_flags{static_cast<ColumnFlags>(f)},
        m_gobj_param_name{gobj_name}, m_qof_param_name{qof_name}, m_getter{get},
        m_setter{set} {}

    /**
     * Load a value into an object from the database row.
     */
    virtual void load(const GncSqlBackend* sql_be, GncSqlRow& row,
                      QofIdTypeConst obj_name, void* pObject) const noexcept = 0;
    /**
     * Add a GncSqlColumnInfo structure for the column type to a
     * ColVec.
     */
    virtual void add_to_table(ColVec& vec) const noexcept = 0;
    /**
     * Add a pair of the table column heading and object's value's string
     * representation to a PairVec; used for constructing WHERE clauses and
     * UPDATE statements.
     */
    virtual void add_to_query(QofIdTypeConst obj_name,
                              void* pObject, PairVec& vec) const noexcept = 0;
    /**
     * Retrieve the getter function depending on whether it's an auto-increment
     * field, a QofClass getter, or a function passed to the constructor.
     */
    QofAccessFunc get_getter(QofIdTypeConst obj_name) const noexcept;
    /**
     * Retrieve the setter function depending on whether it's an auto-increment
     * field, a QofClass getter, or a function passed to the constructor.
     */
    QofSetterFunc get_setter(QofIdTypeConst obj_name) const noexcept;
    /**
     * Retrieve the field name so that we don't need to make
     * create_single_col_select_statement and friend.
     */
    const char* name() const noexcept { return m_col_name; }
    /**
     * Report if the entry is an auto-increment field.
     */
    bool is_autoincr() const noexcept { return m_flags & COL_AUTOINC; }
    /* On the other hand, our implementation class and GncSqlColumnInfo need to
     * be able to read our member variables.
     */
    template<GncSqlObjectType Otype> friend class GncSqlColumnTableEntryImpl;
    friend struct GncSqlColumnInfo;
    template<typename T> void load_from_guid_ref(GncSqlRow& row,
                                                 QofIdTypeConst obj_name,
                                                 void* pObject, T get_ref)
        const noexcept
        {
            g_return_if_fail (pObject != NULL);

            try
            {
                GncGUID guid;
                auto val = row.get_string_at_col (m_col_name);
                if (string_to_guid (val.c_str(), &guid))
                {
                    auto target = get_ref(&guid);
                    if (target != nullptr)
                        set_parameter (pObject, target, get_setter(obj_name),
                                       m_gobj_param_name);
                }
            }
            catch (std::invalid_argument) {}
        }


protected:
    template <typename T> T
    get_row_value_from_object(QofIdTypeConst obj_name, const void* pObject) const;
    template <typename T> void
    add_value_to_vec(QofIdTypeConst obj_name,
                     const void* pObject, PairVec& vec) const;
/**
 * Adds a name/guid std::pair to a PairVec for creating a query.
 *
 * @param sql_be SQL backend struct
 * @param obj_name QOF object type name
 * @param pObject Object
 * @param pList List
 */
    void add_objectref_guid_to_query (QofIdTypeConst obj_name,
                                      const void* pObject,
                                      PairVec& vec) const noexcept;
/**
 * Adds a column info structure for an object reference GncGUID to a ColVec.
 *
 * @param sql_be SQL backend struct
 * @param pList List
 */
    void add_objectref_guid_to_table (ColVec& vec) const noexcept;
private:
    const char* m_col_name;        /**< Column name */
    const GncSqlObjectType m_col_type;        /**< Column type */
    unsigned int m_size;       /**< Column size in bytes, for string columns */
    ColumnFlags m_flags;           /**< Column flags */
    const char* m_gobj_param_name; /**< If non-null, g_object param name */
    const char* m_qof_param_name;  /**< If non-null, qof parameter name */
    QofAccessFunc m_getter;        /**< General access function */
    QofSetterFunc m_setter;        /**< General setter function */
    template <typename T> T get_row_value_from_object(QofIdTypeConst obj_name,
                                                      const void* pObject,
                                                      std::true_type) const;
    template <typename T> T get_row_value_from_object(QofIdTypeConst obj_name,
                                                      const void* pObject,
                                                      std::false_type) const;
    template <typename T> void add_value_to_vec(QofIdTypeConst obj_name,
                                                const void* pObject,
                                                PairVec& vec, std::true_type) const;
    template <typename T> void add_value_to_vec(QofIdTypeConst obj_name,
                                                const void* pObject,
                                                PairVec& vec, std::false_type) const;

};

template <GncSqlObjectType Type>
class GncSqlColumnTableEntryImpl : public GncSqlColumnTableEntry
{
public:
    GncSqlColumnTableEntryImpl (const char* name, const GncSqlObjectType type,
                                unsigned int s,
                                int f, const char* gobj_name = nullptr,
                                const char* qof_name = nullptr,
                                QofAccessFunc get = nullptr,
                                QofSetterFunc set = nullptr) :
        GncSqlColumnTableEntry (name, type, s, f, gobj_name,qof_name, get, set)
        {}
    void load(const GncSqlBackend* sql_be, GncSqlRow& row,  QofIdTypeConst obj_name,
              void* pObject) const noexcept override;
    void add_to_table(ColVec& vec) const noexcept override;
    void add_to_query(QofIdTypeConst obj_name, void* pObject, PairVec& vec)
        const noexcept override;
};

using GncSqlColumnTableEntryPtr = std::shared_ptr<GncSqlColumnTableEntry>;
using EntryVec = std::vector<GncSqlColumnTableEntryPtr>;

template <GncSqlObjectType Type>
std::shared_ptr<GncSqlColumnTableEntryImpl<Type>>
gnc_sql_make_table_entry(const char* name, unsigned int s, int f)
{
    return std::make_shared<GncSqlColumnTableEntryImpl<Type>>(name, Type, s, f);
}

template <GncSqlObjectType Type>
std::shared_ptr<GncSqlColumnTableEntryImpl<Type>>
gnc_sql_make_table_entry(const char* name, unsigned int s, int f,
                         const char* param)
{
    return std::make_shared<GncSqlColumnTableEntryImpl<Type>>(name, Type, s,
                                                              f, param);
}

class is_qof : public std::true_type {};

template <GncSqlObjectType Type>
std::shared_ptr<GncSqlColumnTableEntryImpl<Type>>
gnc_sql_make_table_entry(const char* name, unsigned int s, int f,
                         const char* param, bool qofp)
{
    return std::make_shared<GncSqlColumnTableEntryImpl<Type>>(name, Type, s,
                                                              f, nullptr,
                                                              param);
}

template <GncSqlObjectType Type>
std::shared_ptr<GncSqlColumnTableEntryImpl<Type>>
gnc_sql_make_table_entry(const char* name, unsigned int s, int f,
                         QofAccessFunc get, QofSetterFunc set)
{
    return std::make_shared<GncSqlColumnTableEntryImpl<Type>>(
        name, Type, s, f, nullptr, nullptr, get, set);
}


template <typename T> T
GncSqlColumnTableEntry::get_row_value_from_object(QofIdTypeConst obj_name,
                                                  const void* pObject) const
{
    return get_row_value_from_object<T>(obj_name, pObject,
                                        std::is_pointer<T>());
}

template <typename T> T
GncSqlColumnTableEntry::get_row_value_from_object(QofIdTypeConst obj_name,
                                                  const void* pObject,
                                                  std::true_type) const
{
    g_return_val_if_fail(obj_name != nullptr && pObject != nullptr, nullptr);
    T result = nullptr;
    if (m_gobj_param_name != nullptr)
        g_object_get(const_cast<void*>(pObject), m_gobj_param_name,
                     &result, nullptr);
    else
    {
        QofAccessFunc getter = get_getter(obj_name);
        if (getter != nullptr)
            result = reinterpret_cast<T>((getter)(const_cast<void*>(pObject),
                                                  nullptr));
    }
    return result;
}

template <typename T> T
GncSqlColumnTableEntry::get_row_value_from_object(QofIdTypeConst obj_name,
                                                  const void* pObject,
                                                  std::false_type) const
{
    g_return_val_if_fail(obj_name != nullptr && pObject != nullptr,
                         static_cast<T>(0));
    T result = static_cast<T>(0);
    if (m_gobj_param_name != nullptr)
        g_object_get(const_cast<void*>(pObject), m_gobj_param_name,
                     &result, nullptr);
    else
    {
        QofAccessFunc getter = get_getter(obj_name);
        if (getter != nullptr)
            result = reinterpret_cast<T>((getter)(const_cast<void*>(pObject),
                                                  nullptr));
    }
    return result;
}

template <typename T> void
GncSqlColumnTableEntry::add_value_to_vec(QofIdTypeConst obj_name,
                                         const void* pObject,
                                         PairVec& vec) const
{
    add_value_to_vec<T>(obj_name, pObject, vec, std::is_pointer<T>());
}

template <typename T> void
GncSqlColumnTableEntry::add_value_to_vec(QofIdTypeConst obj_name,
                                         const void* pObject,
                                         PairVec& vec, std::true_type) const
{
    T s = get_row_value_from_object<T>(obj_name, pObject);

    if (s != nullptr)
    {
        std::ostringstream stream;
        stream << *s;
        vec.emplace_back(std::make_pair(std::string{m_col_name}, stream.str()));
        return;
    }
}

template <typename T> void
GncSqlColumnTableEntry::add_value_to_vec(QofIdTypeConst obj_name,
                                         const void* pObject,
                                         PairVec& vec, std::false_type) const
{
    T s = get_row_value_from_object<T>(obj_name, pObject);

    std::ostringstream stream;
    stream << s;
    vec.emplace_back(std::make_pair(std::string{m_col_name}, stream.str()));
    return;
}

/**
 * Load an arbitrary object from a result row.
 *
 * @param sql_be: GncSqlBackend*, pass-through to the implementation loader.
 * @param row: The GncSqlResult
 * @param obj_name: The object-name with which to retrieve the setter func.
 * @param pObject: The target object being loaded.
 * @param table: The table description to interpret the row.
 */
void gnc_sql_load_object (const GncSqlBackend* sql_be, GncSqlRow& row,
                          QofIdTypeConst obj_name, gpointer pObject,
                          const EntryVec& table);
/**
 * Create a GncGUID from a guid stored in a row.
 *
 * @param sql_be: The active GncSqlBackend. Pass-throug to gnc_sql_load_object.
 * @param row: The GncSqlResult row.
 */
const GncGUID*
gnc_sql_load_guid (const GncSqlBackend* sql_be, GncSqlRow& row);

/**
 * Append the GUIDs of QofInstances to a SQL query.
 *
 * @param sql: The SQL Query in progress to which the GncGUIDS should be appended.
 * @param instances: The QofInstances
 * @return The number of instances
 */
uint_t gnc_sql_append_guids_to_sql (std::stringstream& sql,
                                    const InstanceVec& instances);

/**
 *  information required to create a column in a table.
 */
struct GncSqlColumnInfo
{
    GncSqlColumnInfo (std::string&& name, GncSqlBasicColumnType type,
                      unsigned int size = 0, bool unicode = false,
                      bool autoinc = false, bool primary = false,
                      bool not_null = false) :
        m_name{name}, m_type{type}, m_size{size}, m_unicode{unicode},
        m_autoinc{autoinc}, m_primary_key{primary}, m_not_null{not_null}
        {}
    GncSqlColumnInfo(const GncSqlColumnTableEntry& e, GncSqlBasicColumnType t,
                     unsigned int size = 0, bool unicode = true) :
        m_name{e.m_col_name}, m_type{t}, m_size{size}, m_unicode{unicode},
        m_autoinc(e.m_flags & COL_AUTOINC),
        m_primary_key(e.m_flags & COL_PKEY),
        m_not_null(e.m_flags & COL_NNUL) {}
    std::string m_name; /**< Column name */
    GncSqlBasicColumnType m_type; /**< Column basic type */
    unsigned int m_size; /**< Column size (string types) */
    bool m_unicode; /**< Column is unicode (string types) */
    bool m_autoinc; /**< Column is autoinc (int type) */
    bool m_primary_key; /**< Column is the primary key */
    bool m_not_null; /**< Column forbids NULL values */
};

inline bool operator==(const GncSqlColumnInfo& l,
                       const GncSqlColumnInfo& r)
{
    return l.m_name == r.m_name && l.m_type == r.m_type;
}

inline bool operator!=(const GncSqlColumnInfo& l,
                       const GncSqlColumnInfo& r)
{
    return !(l == r);
}

/**
 * Set an object property with a setter function.
 * @param pObject void* to the object being set.
 * @param item the value to be set in the property.
 * @param setter The function to set the property.
 * The void* is an obvious wart occasioned by the fact that we're using GLists
 * to hold objects. As the rewrite progresses we'll replace that with another
 * template parameter.
 */
template <typename T, typename P, typename F>
void set_parameter(T object, P item, F& setter)
{
    (*setter)(object, item);
}

template <typename T, typename P>
void set_parameter(T object, P item, QofSetterFunc setter, std::true_type)
{
    (*setter)(object, (void*)item);
}

template <typename T, typename P>
void set_parameter(T object, P item, QofSetterFunc setter, std::false_type)
{
    (*setter)(object, (void*)(&item));
}

template <typename T, typename P>
void set_parameter(T object, P item, QofSetterFunc setter)
{
    set_parameter(object, item, setter, std::is_pointer<P>());
}

/**
 * Set an object property with g_object_set.
 * @param pObject void* to the object being set.
 * @param item the value to set in the property.
 * @param property the property name.
 * The void* is an obvious wart. So is g_object_set, partly because it's GObject
 * but mostly because it works off of string comparisons.
 */
template <typename T, typename P>
void set_parameter(T object, P item, const char* property)
{
    qof_instance_increase_editlevel(object);
    g_object_set(object, property, item, nullptr);
    qof_instance_decrease_editlevel(object);
};

/**
 * Set an object property with either a g_object_set or a setter.
 *
 * See previous templates for the parameter meanings. This is clunky but fits in
 * the current architecture for refactoring.
 */
template <typename T, typename P, typename F>
void set_parameter(T object, P item, F setter, const char* property)
{
    if (property)
        set_parameter(object, item, property);
    else
        set_parameter(object, item, setter);
}

#endif //__GNC_SQL_COLUMN_TABLE_ENTRY_HPP__
