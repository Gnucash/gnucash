/***********************************************************************\
 * gnc-sql-result.hpp: Encapsulate the results of a SQL query.         *
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

#ifndef __GNC_SQL_RESULT_HPP__
#define __GNC_SQL_RESULT_HPP__

extern "C"
{
#include <qof.h>
}
#include <cstdint>
#include <string>
#include <vector>

class GncSqlRow;
/**
 * Pure virtual class to iterate over a query result set.
 */

class GncSqlResult
{
public:
    virtual ~GncSqlResult() = default;
    virtual uint64_t size() const noexcept = 0;
    virtual GncSqlRow& begin() = 0;
    virtual GncSqlRow& end() = 0;
    friend GncSqlRow;
protected:
    class IteratorImpl {
    public:
        virtual ~IteratorImpl() = default;
        virtual GncSqlRow& operator++() = 0;
        virtual GncSqlResult* operator*() = 0;
        virtual int64_t get_int_at_col (const char* col) const = 0;
        virtual float get_float_at_col (const char* col) const = 0;
        virtual double get_double_at_col (const char* col) const = 0;
        virtual std::string get_string_at_col (const char* col) const = 0;
        virtual time64 get_time64_at_col (const char* col) const = 0;
        virtual bool is_col_null (const char* col) const noexcept = 0;
    };
};

/**
 * Row of SQL Query results.
 *
 * This is a "pointer" class of a pimpl pattern, the implementation being
 * GncSqlResult::IteratorImpl. It's designed to present a std::forward_iterator
 * like interface for use with range-for while allowing for wrapping a C API.
 *
 * Important Implementation Note: Operator++() as written requires that the
 * sentinel GncSqlRow returned by GncSqlResult::end() has m_impl = nullptr in
 * order to terminate the loop condition. This is a bit of a hack and might be a
 * problem with a different SQL interface library from libdbi.
 *
 * Note that GncSqlResult::begin and GncSqlRow::operator++() return
 * GncSqlRow&. This is necessary for operator++() to be called: Using operator
 * ++() on a pointer performs pointer arithmetic rather than calling the
 * pointed-to-class's operator++() and C++'s range-for uses operator++()
 * directly on whatever begin() gives it.
 */
class GncSqlRow
{
public:
    GncSqlRow (GncSqlResult::IteratorImpl* iter) : m_iter{iter} {}
    ~GncSqlRow() { }
    GncSqlRow& operator++();
    GncSqlRow& operator*() { return *this; }
    friend bool operator!=(const GncSqlRow&, const GncSqlRow&);
    int64_t get_int_at_col (const char* col) const {
        return m_iter->get_int_at_col (col); }
    float get_float_at_col (const char* col) const {
        return m_iter->get_float_at_col (col); }
    double get_double_at_col (const char* col) const {
        return m_iter->get_double_at_col (col); }
    std::string get_string_at_col (const char* col) const {
        return m_iter->get_string_at_col (col); }
    time64 get_time64_at_col (const char* col) const {
        return m_iter->get_time64_at_col (col); }
    bool is_col_null (const char* col) const noexcept {
        return m_iter->is_col_null (col); }
private:
    GncSqlResult::IteratorImpl* m_iter;
};

inline bool operator!=(const GncSqlRow& lr, const GncSqlRow& rr) {
    return lr.m_iter != rr.m_iter;
}

inline bool operator==(const GncSqlRow& lr, const GncSqlRow& rr) {
    return !(lr != rr);
}


#endif //__GNC_SQL_RESULT_HPP__
