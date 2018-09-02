/********************************************************************
 * gnc-dbisqlresult.hpp: Iterable wrapper for dbi_result.           *
 *                                                                  *
 * Copyright 2016 John Ralls <jralls@ceridwen.us                    *
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
#ifndef __GNC_DBISQLBACKEND_HPP__
#define __GNC_DBISQLBACKEND_HPP__

#include "gnc-backend-dbi.h"
#include <gnc-sql-result.hpp>

class GncDbiSqlConnection;

/**
 * An iterable wrapper for dbi_result; allows using C++11 range for.
 */
class GncDbiSqlResult : public GncSqlResult
{
public:
    GncDbiSqlResult(const GncDbiSqlConnection* conn, dbi_result result) :
        m_conn{conn}, m_dbi_result{result}, m_iter{this}, m_row{&m_iter},
        m_sentinel{nullptr} {}
    ~GncDbiSqlResult();
    uint64_t size() const noexcept;
    int dberror() const noexcept;
    GncSqlRow& begin();
    GncSqlRow& end() { return m_sentinel; }
protected:
    class IteratorImpl : public GncSqlResult::IteratorImpl
    {
    public:
        ~IteratorImpl() = default;
        IteratorImpl(GncDbiSqlResult* inst) : m_inst{inst} {}
        virtual GncSqlRow& operator++();
        virtual GncSqlRow& operator++(int) { return ++(*this); };
        virtual GncSqlResult* operator*() { return m_inst; }
        virtual int64_t get_int_at_col (const char* col) const;
        virtual double get_float_at_col (const char* col) const;
        virtual double get_double_at_col (const char* col) const;
        virtual std::string get_string_at_col (const char* col)const;
        virtual time64 get_time64_at_col (const char* col) const;
        virtual bool is_col_null(const char* col) const noexcept
        {
            return dbi_result_field_is_null(m_inst->m_dbi_result, col);
        }
    private:
        GncDbiSqlResult* m_inst = nullptr;
    };

private:
    const GncDbiSqlConnection* m_conn = nullptr;
    dbi_result m_dbi_result;
    IteratorImpl m_iter;
    GncSqlRow m_row;
    GncSqlRow m_sentinel;

};

#endif //__GNC_DBISQLRESULT_HPP__
