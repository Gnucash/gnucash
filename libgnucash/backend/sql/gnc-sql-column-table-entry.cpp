/********************************************************************
 * gnc-sql-column-table-entry.cpp: Implement GncSqlColumnTableEntry *
 *                                                                  *
 * Copyright 2016 John Ralls <jralls@ceridwen.us>                   *
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

extern "C"
{
#include <config.h>
#include <qof.h>
}
#include <sstream>
#include <iomanip>
#include <gnc-datetime.hpp>
#include "gnc-sql-backend.hpp"
#include "gnc-sql-object-backend.hpp"
#include "gnc-sql-column-table-entry.hpp"
#include "gnc-sql-result.hpp"

static QofLogModule log_module = G_LOG_DOMAIN;

/* ================================================================= */
static gpointer
get_autoinc_id (void* object, const QofParam* param)
{
    // Just need a 0 to force a new autoinc value
    return (gpointer)0;
}

static void
set_autoinc_id (void* object, void* item)
{
    // Nowhere to put the ID
}


QofAccessFunc
GncSqlColumnTableEntry::get_getter (QofIdTypeConst obj_name) const noexcept
{
    QofAccessFunc getter;

    g_return_val_if_fail (obj_name != NULL, NULL);

    if (m_flags & COL_AUTOINC)
    {
        getter = get_autoinc_id;
    }
    else if (m_qof_param_name != NULL)
    {
        getter = qof_class_get_parameter_getter (obj_name, m_qof_param_name);
    }
    else
    {
        getter = m_getter;
    }

    return getter;
}

QofSetterFunc
GncSqlColumnTableEntry::get_setter(QofIdTypeConst obj_name) const noexcept
{
    QofSetterFunc setter = nullptr;
    if (m_flags & COL_AUTOINC)
    {
        setter = set_autoinc_id;
    }
    else if (m_qof_param_name != nullptr)
    {
        g_assert (obj_name != NULL);
        setter = qof_class_get_parameter_setter (obj_name, m_qof_param_name);
    }
    else
    {
        setter = m_setter;
    }
    return setter;
}

void
GncSqlColumnTableEntry::add_objectref_guid_to_query (QofIdTypeConst obj_name,
                                                     const void* pObject,
                                                     PairVec& vec) const noexcept
{
    auto inst = get_row_value_from_object<QofInstance*>(obj_name, pObject);
    if (inst == nullptr) return;
    auto guid = qof_instance_get_guid (inst);
    if (guid != nullptr)
        vec.emplace_back (std::make_pair (std::string{m_col_name},
                                          quote_string(guid_to_string(guid))));
}

void
GncSqlColumnTableEntry::add_objectref_guid_to_table (ColVec& vec) const noexcept
{
    GncSqlColumnInfo info{*this, BCT_STRING, GUID_ENCODING_LENGTH, FALSE};
    vec.emplace_back(std::move(info));
}


/* ----------------------------------------------------------------- */
template<> void
GncSqlColumnTableEntryImpl<CT_STRING>::load (const GncSqlBackend* sql_be,
                                             GncSqlRow& row,
                                             QofIdTypeConst obj_name,
                                             gpointer pObject) const noexcept
{
    g_return_if_fail (pObject != NULL);
    g_return_if_fail (m_gobj_param_name != NULL || get_setter(obj_name) != NULL);

    try
    {
        auto s = row.get_string_at_col (m_col_name);
        set_parameter(pObject, s.c_str(), get_setter(obj_name), m_gobj_param_name);
    }
    catch (std::invalid_argument) {}
}

template<> void
GncSqlColumnTableEntryImpl<CT_STRING>::add_to_table(ColVec& vec) const noexcept
{
    GncSqlColumnInfo info{*this, BCT_STRING, m_size, TRUE};
    vec.emplace_back(std::move(info));
}

/* char is unusual in that we get a pointer but don't deref it to pass
 * it to operator<<().
 */
template<> void
GncSqlColumnTableEntryImpl<CT_STRING>::add_to_query(QofIdTypeConst obj_name,
                                                    const gpointer pObject,
                                                    PairVec& vec) const noexcept
{
    auto s = get_row_value_from_object<char*>(obj_name, pObject);

    if (s != nullptr)
    {
        std::ostringstream stream;
        stream << s;
        vec.emplace_back (std::make_pair (std::string{m_col_name},
                                          quote_string(stream.str())));
        return;
    }
}

/* ----------------------------------------------------------------- */
typedef gint (*IntAccessFunc) (const gpointer);
typedef void (*IntSetterFunc) (const gpointer, gint);

template<> void
GncSqlColumnTableEntryImpl<CT_INT>::load (const GncSqlBackend* sql_be,
                                          GncSqlRow& row,
                                          QofIdTypeConst obj_name,
                                          gpointer pObject) const noexcept
{

    g_return_if_fail (pObject != NULL);
    g_return_if_fail (m_gobj_param_name != NULL || get_setter(obj_name) != NULL);

    auto val = row.get_int_at_col(m_col_name);
    set_parameter(pObject, val,
                  reinterpret_cast<IntSetterFunc>(get_setter(obj_name)), m_gobj_param_name);
}

template<> void
GncSqlColumnTableEntryImpl<CT_INT>::add_to_table(ColVec& vec) const noexcept
{
    GncSqlColumnInfo info{*this, BCT_INT, 0, FALSE};
    vec.emplace_back(std::move(info));
}

template<> void
GncSqlColumnTableEntryImpl<CT_INT>::add_to_query(QofIdTypeConst obj_name,
                                                 const gpointer pObject,
                                                 PairVec& vec) const noexcept
{
    add_value_to_vec<int>(obj_name, pObject, vec);
}

/* ----------------------------------------------------------------- */
typedef gboolean (*BooleanAccessFunc) (const gpointer);
typedef void (*BooleanSetterFunc) (const gpointer, gboolean);

template<> void
GncSqlColumnTableEntryImpl<CT_BOOLEAN>::load (const GncSqlBackend* sql_be,
                                              GncSqlRow& row,
                                              QofIdTypeConst obj_name,
                                              gpointer pObject)
    const noexcept
{
    g_return_if_fail (pObject != NULL);
    g_return_if_fail (m_gobj_param_name != NULL || get_setter(obj_name) != NULL);

    auto val = row.get_int_at_col (m_col_name);
    set_parameter(pObject, static_cast<int>(val),
                  reinterpret_cast<BooleanSetterFunc>(get_setter(obj_name)),
                  m_gobj_param_name);
}

template<> void
GncSqlColumnTableEntryImpl<CT_BOOLEAN>::add_to_table(ColVec& vec) const noexcept
{
    GncSqlColumnInfo info{*this, BCT_INT, 0, FALSE};
    vec.emplace_back(std::move(info));
}

template<> void
GncSqlColumnTableEntryImpl<CT_BOOLEAN>::add_to_query(QofIdTypeConst obj_name,
                                                    const gpointer pObject,
                                                    PairVec& vec) const noexcept
{
    add_value_to_vec<int>(obj_name, pObject, vec);
}

/* ----------------------------------------------------------------- */
typedef gint64 (*Int64AccessFunc) (const gpointer);
typedef void (*Int64SetterFunc) (const gpointer, gint64);

template<> void
GncSqlColumnTableEntryImpl<CT_INT64>::load (const GncSqlBackend* sql_be,
                                            GncSqlRow& row,
                                            QofIdTypeConst obj_name,
                                            gpointer pObject)
    const noexcept
{
    g_return_if_fail (m_gobj_param_name != nullptr || get_setter(obj_name) != nullptr);

    auto val = row.get_int_at_col (m_col_name);
    set_parameter(pObject, val,
                  reinterpret_cast<Int64SetterFunc>(get_setter(obj_name)),
                  m_gobj_param_name);
}

template<> void
GncSqlColumnTableEntryImpl<CT_INT64>::add_to_table(ColVec& vec) const noexcept
{

    GncSqlColumnInfo info{*this, BCT_INT64, 0, FALSE};
    vec.emplace_back(std::move(info));
}

template<> void
GncSqlColumnTableEntryImpl<CT_INT64>::add_to_query(QofIdTypeConst obj_name,
                                                   const gpointer pObject,
                                                   PairVec& vec) const noexcept
{
    add_value_to_vec<int64_t>(obj_name, pObject, vec);
}
/* ----------------------------------------------------------------- */

template<> void
GncSqlColumnTableEntryImpl<CT_DOUBLE>::load (const GncSqlBackend* sql_be,
                                             GncSqlRow& row,
                                             QofIdTypeConst obj_name,
                                             gpointer pObject)
    const noexcept
{
    g_return_if_fail (pObject != NULL);
    g_return_if_fail (m_gobj_param_name != nullptr || get_setter(obj_name) != nullptr);
    double val;
    try
    {
        val = static_cast<double>(row.get_int_at_col(m_col_name));
    }
    catch (std::invalid_argument)
    {
        try
        {
            val = static_cast<double>(row.get_float_at_col(m_col_name));
        }
        catch (std::invalid_argument)
        {
            try
            {
                val = row.get_double_at_col(m_col_name);
            }
            catch (std::invalid_argument)
            {
                val = 0.0;
            }
        }
    }
    set_parameter(pObject, val, get_setter(obj_name), m_gobj_param_name);
}

template<> void
GncSqlColumnTableEntryImpl<CT_DOUBLE>::add_to_table(ColVec& vec) const noexcept
{
    GncSqlColumnInfo info{*this, BCT_DOUBLE, 0, FALSE};
    vec.emplace_back(std::move(info));
}

template<> void
GncSqlColumnTableEntryImpl<CT_DOUBLE>::add_to_query(QofIdTypeConst obj_name,
                                                    const gpointer pObject,
                                                    PairVec& vec) const noexcept
{
    add_value_to_vec<double*>(obj_name, pObject, vec);
}

/* ----------------------------------------------------------------- */

template<> void
GncSqlColumnTableEntryImpl<CT_GUID>::load (const GncSqlBackend* sql_be,
                                           GncSqlRow& row,
                                           QofIdTypeConst obj_name,
                                           gpointer pObject)
    const noexcept
{

    GncGUID guid;
    const GncGUID* pGuid;

    g_return_if_fail (pObject != NULL);
    g_return_if_fail (m_gobj_param_name != nullptr || get_setter(obj_name) != nullptr);

    std::string str;
    try
    {
        str = row.get_string_at_col(m_col_name);
    }
    catch (std::invalid_argument)
    {
        return;
    }
    if (string_to_guid (str.c_str(), &guid))
        set_parameter(pObject, &guid, get_setter(obj_name), m_gobj_param_name);
}

template<> void
GncSqlColumnTableEntryImpl<CT_GUID>::add_to_table(ColVec& vec) const noexcept
{
    GncSqlColumnInfo info{*this, BCT_STRING, GUID_ENCODING_LENGTH, FALSE};
    vec.emplace_back(std::move(info));
}

template<> void
GncSqlColumnTableEntryImpl<CT_GUID>::add_to_query(QofIdTypeConst obj_name,
                                                  const gpointer pObject,
                                                  PairVec& vec) const noexcept
{
    auto s = get_row_value_from_object<GncGUID*>(obj_name, pObject);

    if (s != nullptr)
    {

        vec.emplace_back (std::make_pair (std::string{m_col_name},
                                          quote_string(guid_to_string(s))));
        return;
    }
}
/* ----------------------------------------------------------------- */
typedef Timespec (*TimespecAccessFunc) (const gpointer);
typedef void (*TimespecSetterFunc) (const gpointer, Timespec*);

#define TIMESPEC_COL_SIZE (4+3+3+3+3+3)

template<> void
GncSqlColumnTableEntryImpl<CT_TIMESPEC>::load (const GncSqlBackend* sql_be,
                                               GncSqlRow& row,
                                               QofIdTypeConst obj_name,
                                               gpointer pObject) const noexcept
{

    Timespec ts = {0, 0};
    gboolean isOK = FALSE;


    g_return_if_fail (pObject != NULL);
    g_return_if_fail (m_gobj_param_name != nullptr || get_setter(obj_name) != nullptr);

    try
    {
        auto val = row.get_time64_at_col(m_col_name);
        timespecFromTime64 (&ts, val);
    }
    catch (std::invalid_argument)
    {
        try
        {
            auto val = row.get_string_at_col(m_col_name);
            GncDateTime time(val);
            ts.tv_sec = static_cast<time64>(time);
        }
        catch (std::invalid_argument)
        {
            return;
        }
    }
    set_parameter(pObject, &ts,
                  reinterpret_cast<TimespecSetterFunc>(get_setter(obj_name)),
                  m_gobj_param_name);
 }

template<> void
GncSqlColumnTableEntryImpl<CT_TIMESPEC>::add_to_table(ColVec& vec) const noexcept
{
    GncSqlColumnInfo info{*this, BCT_DATETIME, TIMESPEC_COL_SIZE, FALSE};
    vec.emplace_back(std::move(info));
}

template<> void
GncSqlColumnTableEntryImpl<CT_TIMESPEC>::add_to_query(QofIdTypeConst obj_name,
                                                      const gpointer pObject,
                                                      PairVec& vec) const noexcept
{
    TimespecAccessFunc ts_getter;
    Timespec ts;
/* Can't use get_row_value_from_object because g_object_get returns a
 * Timespec* and the getter returns a Timespec. Will be fixed by the
 * replacement of timespecs with time64s.
 */
    g_return_if_fail (obj_name != NULL);
    g_return_if_fail (pObject != NULL);
    
    if (m_gobj_param_name != NULL)
    {
        Timespec* pts;
        g_object_get (pObject, m_gobj_param_name, &pts, NULL);
        ts = *pts;
    }
    else
    {
        ts_getter = (TimespecAccessFunc)get_getter (obj_name);
        g_return_if_fail (ts_getter != NULL);
        ts = (*ts_getter) (pObject);
    }
    if (ts.tv_sec > MINTIME && ts.tv_sec < MAXTIME)
    {
        GncDateTime time(ts.tv_sec);
        vec.emplace_back (std::make_pair (std::string{m_col_name},
                                          time.format_zulu ("'%Y-%m-%d %H:%M:%S'")));
    }
    else
    {
        vec.emplace_back (std::make_pair (std::string{m_col_name},
                                          "NULL"));
    }
}
/* ----------------------------------------------------------------- */
typedef time64 (*Time64AccessFunc) (const gpointer);
typedef void (*Time64SetterFunc) (const gpointer, time64);

template<> void
GncSqlColumnTableEntryImpl<CT_TIME64>::load (const GncSqlBackend* sql_be,
                                            GncSqlRow& row,
                                            QofIdTypeConst obj_name,
                                            gpointer pObject)
    const noexcept
{
    time64 t;
    g_return_if_fail (m_gobj_param_name != nullptr || get_setter(obj_name) != nullptr);
    try
    {
        t = row.get_time64_at_col (m_col_name);
    }
    catch (std::invalid_argument)
    {
        try
        {
            auto val = row.get_string_at_col(m_col_name);
            GncDateTime time(val);
            t = static_cast<time64>(time);
        }
        catch (std::invalid_argument)
        {
            return;
        }
    }
    set_parameter(pObject, t,
                  reinterpret_cast<Time64SetterFunc>(get_setter(obj_name)),
                  m_gobj_param_name);
}

template<> void
GncSqlColumnTableEntryImpl<CT_TIME64>::add_to_table(ColVec& vec) const noexcept
{

    GncSqlColumnInfo info{*this, BCT_DATETIME, TIMESPEC_COL_SIZE, FALSE};
    vec.emplace_back(std::move(info));
}

template<> void
GncSqlColumnTableEntryImpl<CT_TIME64>::add_to_query(QofIdTypeConst obj_name,
                                                   const gpointer pObject,
                                                   PairVec& vec) const noexcept
{
    auto t = get_row_value_from_object<time64>(obj_name, pObject);
    if (t > MINTIME && t < MAXTIME)
    {
        GncDateTime time(t);
        vec.emplace_back (std::make_pair (std::string{m_col_name},
                                          time.format_zulu ("'%Y-%m-%d %H:%M:%S'")));
    }
    else
    {
        vec.emplace_back (std::make_pair (std::string{m_col_name},
                                          "NULL"));
    }
}

/* ----------------------------------------------------------------- */
#define DATE_COL_SIZE 8

template<> void
GncSqlColumnTableEntryImpl<CT_GDATE>::load (const GncSqlBackend* sql_be,
                                            GncSqlRow& row,
                                            QofIdTypeConst obj_name,
                                            gpointer pObject) const noexcept
{
    g_return_if_fail (pObject != NULL);
    g_return_if_fail (m_gobj_param_name != nullptr || get_setter(obj_name) != nullptr);
    if (row.is_col_null(m_col_name))
        return;
    GDate date;
    g_date_clear (&date, 1);
    try
    {
        /* timespec_to_gdate applies the tz, and gdates are saved
         * as ymd, so we don't want that.
         */
        auto time = row.get_time64_at_col(m_col_name);
        auto tm = gnc_gmtime(&time);
        g_date_set_dmy(&date, tm->tm_mday,
                       static_cast<GDateMonth>(tm->tm_mon + 1),
                       tm->tm_year + 1900);
        free(tm);
    }
    catch (std::invalid_argument)
    {
        try
        {
            std::string str = row.get_string_at_col(m_col_name);
            if (str.empty()) return;
            auto year = static_cast<GDateYear>(stoi (str.substr (0,4)));
            auto month = static_cast<GDateMonth>(stoi (str.substr (4,2)));
            auto day = static_cast<GDateDay>(stoi (str.substr (6,2)));

            if (year != 0 || month != 0 || day != (GDateDay)0)
                g_date_set_dmy(&date, day, month, year);

        }
        catch (std::invalid_argument)
        {
            return;
        }
    }
    set_parameter(pObject, &date, get_setter(obj_name), m_gobj_param_name);
}

template<> void
GncSqlColumnTableEntryImpl<CT_GDATE>::add_to_table(ColVec& vec) const noexcept
{
    GncSqlColumnInfo info{*this,  BCT_DATE, DATE_COL_SIZE, FALSE};
    vec.emplace_back(std::move(info));
}

template<> void
GncSqlColumnTableEntryImpl<CT_GDATE>::add_to_query(QofIdTypeConst obj_name,
                                                   const gpointer pObject,
                                                   PairVec& vec) const noexcept
{
    GDate *date = get_row_value_from_object<GDate*>(obj_name, pObject);

    if (date && g_date_valid (date))
    {
        std::ostringstream buf;
        buf << std::setfill ('0') << std::setw (4) << g_date_get_year (date) <<
            std::setw (2) << g_date_get_month (date) <<
            std::setw (2) << static_cast<int>(g_date_get_day (date));
        vec.emplace_back (std::make_pair (std::string{m_col_name},
                                          quote_string(buf.str())));
        return;
    }
}

/* ----------------------------------------------------------------- */
typedef gnc_numeric (*NumericGetterFunc) (const gpointer);
typedef void (*NumericSetterFunc) (gpointer, gnc_numeric*);

static const EntryVec numeric_col_table =
{
    gnc_sql_make_table_entry<CT_INT64>("num", 0, COL_NNUL, "guid"),
    gnc_sql_make_table_entry<CT_INT64>("denom", 0, COL_NNUL, "guid")
};

template<> void
GncSqlColumnTableEntryImpl<CT_NUMERIC>::load (const GncSqlBackend* sql_be,
                                              GncSqlRow& row,
                                              QofIdTypeConst obj_name,
                                              gpointer pObject) const noexcept
{


    g_return_if_fail (pObject != NULL);
    g_return_if_fail (m_gobj_param_name != nullptr || get_setter(obj_name) != nullptr);
    gnc_numeric n;
    try
    {
        auto buf = g_strdup_printf ("%s_num", m_col_name);
        auto num = row.get_int_at_col (buf);
        g_free (buf);
        buf = g_strdup_printf ("%s_denom", m_col_name);
        auto denom = row.get_int_at_col (buf);
        n = gnc_numeric_create (num, denom);
        g_free (buf);
    }
    catch (std::invalid_argument)
    {
        return;
    }
    set_parameter(pObject, &n,
                  reinterpret_cast<NumericSetterFunc>(get_setter(obj_name)),
                  m_gobj_param_name);
}

template<> void
GncSqlColumnTableEntryImpl<CT_NUMERIC>::add_to_table(ColVec& vec) const noexcept
{

    for (auto const& subtable_row : numeric_col_table)
    {
        gchar* buf = g_strdup_printf("%s_%s", m_col_name,
                                     subtable_row->m_col_name);
        GncSqlColumnInfo info(buf, BCT_INT64, 0, false, false,
                              m_flags & COL_PKEY, m_flags & COL_NNUL);
        g_free (buf);
        vec.emplace_back(std::move(info));
    }
}

template<> void
GncSqlColumnTableEntryImpl<CT_NUMERIC>::add_to_query(QofIdTypeConst obj_name,
                                                     const gpointer pObject,
                                                     PairVec& vec) const noexcept
{
/* We can't use get_row_value_from_object for the same reason as Timespec. */
    NumericGetterFunc getter;
    gnc_numeric n;

    g_return_if_fail (obj_name != NULL);
    g_return_if_fail (pObject != NULL);

    if (m_gobj_param_name != nullptr)
    {
        gnc_numeric* s;
        g_object_get (pObject, m_gobj_param_name, &s, NULL);
        n = *s;
    }
    else
    {
        getter = reinterpret_cast<NumericGetterFunc>(get_getter (obj_name));
        if (getter != NULL)
        {
            n = (*getter) (pObject);
        }
        else
        {
            n = gnc_numeric_zero ();
        }
    }

    std::ostringstream buf;
    std::string num_col{m_col_name};
    std::string denom_col{m_col_name};
    num_col += "_num";
    denom_col += "_denom";
    buf << gnc_numeric_num (n);
    vec.emplace_back (std::make_pair (num_col, buf.str ()));
    buf.str ("");
    buf << gnc_numeric_denom (n);
    vec.emplace_back (denom_col, buf.str ());
}

static void
_retrieve_guid_ (gpointer pObject,  gpointer pValue)
{
    GncGUID* pGuid = (GncGUID*)pObject;
    GncGUID* guid = (GncGUID*)pValue;

    g_return_if_fail (pObject != NULL);
    g_return_if_fail (pValue != NULL);

    memcpy (pGuid, guid, sizeof (GncGUID));
}

// Table to retrieve just the guid
static EntryVec guid_table
{
    gnc_sql_make_table_entry<CT_GUID>("guid", 0, 0, nullptr, _retrieve_guid_)
};

const GncGUID*
gnc_sql_load_guid (const GncSqlBackend* sql_be, GncSqlRow& row)
{
    static GncGUID guid;

    g_return_val_if_fail (sql_be != NULL, NULL);

    gnc_sql_load_object (sql_be, row, NULL, &guid, guid_table);

    return &guid;
}

void
gnc_sql_load_object (const GncSqlBackend* sql_be, GncSqlRow& row,
                     QofIdTypeConst obj_name, gpointer pObject,
                     const EntryVec& table)
{
    QofSetterFunc setter;

    g_return_if_fail (sql_be != NULL);
    g_return_if_fail (pObject != NULL);

    for (auto const& table_row : table)
    {
        table_row->load (sql_be, row, obj_name, pObject);
    }
}

uint_t
gnc_sql_append_guids_to_sql (std::stringstream& sql,
                             const InstanceVec& instances)
{
    char guid_buf[GUID_ENCODING_LENGTH + 1];

    for (auto inst : instances)
    {
        (void)guid_to_string_buff (qof_instance_get_guid (inst), guid_buf);

        if (inst != *(instances.begin()))
        {
            sql << ",";
        }
        sql << "'" << guid_buf << "'";
    }

    return instances.size();
}

/* This is necessary for 64-bit builds because g++ complains
 * that reinterpret_casting a void* (64 bits) to an int (32 bits)
 * loses precision, so we have to explicitly dispose of the precision.
 * FIXME: We shouldn't be storing ints in ptrs in the first place.
 */
#ifdef __LP64__
template <> int
GncSqlColumnTableEntry::get_row_value_from_object<int>(QofIdTypeConst obj_name,
                                                       const void* pObject,
                                                       std::false_type) const
{
    g_return_val_if_fail(obj_name != nullptr && pObject != nullptr, 0);
    int result = 0;
    if (m_gobj_param_name != nullptr)
        g_object_get(const_cast<void*>(pObject), m_gobj_param_name, &result,
		     nullptr);
    else
    {
        QofAccessFunc getter = get_getter(obj_name);
        if (getter != nullptr)
        {
            auto value = ((getter)(const_cast<void*>(pObject), nullptr));
            result = reinterpret_cast<uint64_t>(value) &
                UINT64_C(0x00000000FFFFFFFF);
        }
    }
    return result;
}
#endif
