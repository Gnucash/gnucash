/********************************************************************
 * gnc-dbisqlresult.cpp: Encapsulate libdbi dbi_result              *
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

#include <guid.hpp>
extern "C"
{
#include <config.h>
#include <gnc-locale-utils.h>
#include <dbi/dbi.h>
/* For direct access to dbi data structs, sadly needed for datetime */
#include <dbi/dbi-dev.h>
}
#include <gnc-datetime.hpp>
#include "gnc-dbisqlresult.hpp"
#include "gnc-dbisqlconnection.hpp"

static QofLogModule log_module = G_LOG_DOMAIN;

GncDbiSqlResult::~GncDbiSqlResult()
{
    int status = dbi_result_free (m_dbi_result);

    if (status == 0)
        return;

    PERR ("Error %d in dbi_result_free() result.", m_conn->dberror() );
    qof_backend_set_error (m_conn->qbe(), ERR_BACKEND_SERVER_ERR);
}

int
GncDbiSqlResult::dberror() const noexcept
{
    return m_conn->dberror();
}

GncSqlRow&
GncDbiSqlResult::begin()
{

    if (m_dbi_result == nullptr ||
        dbi_result_get_numrows(m_dbi_result) == 0)
        return m_sentinel;
    int status = dbi_result_first_row(m_dbi_result);
    if (status)
        return m_row;
    int error = dberror(); //

    if (error != DBI_ERROR_BADIDX) //otherwise just an empty result set
    {
        PERR ("Error %d in dbi_result_first_row()", dberror());
        qof_backend_set_error (m_conn->qbe(), ERR_BACKEND_SERVER_ERR);
    }
    return m_sentinel;
}

uint64_t
GncDbiSqlResult::size() const noexcept
{
    return dbi_result_get_numrows(m_dbi_result);
}
/* --------------------------------------------------------- */

GncSqlRow&
GncDbiSqlResult::IteratorImpl::operator++()
{
    int status = dbi_result_next_row (m_inst->m_dbi_result);
    if (status)
        return m_inst->m_row;
    int error = m_inst->dberror();
    if (error == DBI_ERROR_BADIDX || error == 0) //ran off the end of the results
        return m_inst->m_sentinel;
    PERR("Error %d incrementing results iterator.", error);
    qof_backend_set_error (m_inst->m_conn->qbe(), ERR_BACKEND_SERVER_ERR);
    return m_inst->m_sentinel;
}

int64_t
GncDbiSqlResult::IteratorImpl::get_int_at_col(const char* col) const
{
    auto type = dbi_result_get_field_type (m_inst->m_dbi_result, col);
    if(type != DBI_TYPE_INTEGER)
        throw (std::invalid_argument{"Requested integer from non-integer column."});
    return dbi_result_get_longlong (m_inst->m_dbi_result, col);
}

float
GncDbiSqlResult::IteratorImpl::get_float_at_col(const char* col) const
{
    auto type = dbi_result_get_field_type (m_inst->m_dbi_result, col);
    auto attrs = dbi_result_get_field_attribs (m_inst->m_dbi_result, col);
    if(type != DBI_TYPE_DECIMAL ||
       (attrs & DBI_DECIMAL_SIZEMASK) != DBI_DECIMAL_SIZE4)
        throw (std::invalid_argument{"Requested float from non-float column."});
    gnc_push_locale (LC_NUMERIC, "C");
    auto retval =  dbi_result_get_float(m_inst->m_dbi_result, col);
    gnc_pop_locale (LC_NUMERIC);
    return retval;
}

double
GncDbiSqlResult::IteratorImpl::get_double_at_col(const char* col) const
{
    auto type = dbi_result_get_field_type (m_inst->m_dbi_result, col);
    auto attrs = dbi_result_get_field_attribs (m_inst->m_dbi_result, col);
    if(type != DBI_TYPE_DECIMAL ||
       (attrs & DBI_DECIMAL_SIZEMASK) != DBI_DECIMAL_SIZE8)
        throw (std::invalid_argument{"Requested double from non-double column."});
    gnc_push_locale (LC_NUMERIC, "C");
    auto retval =  dbi_result_get_double(m_inst->m_dbi_result, col);
    gnc_pop_locale (LC_NUMERIC);
    return retval;
}

std::string
GncDbiSqlResult::IteratorImpl::get_string_at_col(const char* col) const
{
    auto type = dbi_result_get_field_type (m_inst->m_dbi_result, col);
    auto attrs = dbi_result_get_field_attribs (m_inst->m_dbi_result, col);
    if(type != DBI_TYPE_STRING)
        throw (std::invalid_argument{"Requested string from non-string column."});
    gnc_push_locale (LC_NUMERIC, "C");
    auto strval = dbi_result_get_string(m_inst->m_dbi_result, col);
    if (strval == nullptr)
    {
        gnc_pop_locale (LC_NUMERIC);
        throw (std::invalid_argument{"Column empty."});
    }
    auto retval =  std::string{strval};
    gnc_pop_locale (LC_NUMERIC);
    return retval;
}
time64
GncDbiSqlResult::IteratorImpl::get_time64_at_col (const char* col) const
{
    auto type = dbi_result_get_field_type (m_inst->m_dbi_result, col);
    auto attrs = dbi_result_get_field_attribs (m_inst->m_dbi_result, col);
    if (type != DBI_TYPE_DATETIME)
        throw (std::invalid_argument{"Requested time64 from non-time64 column."});
    gnc_push_locale (LC_NUMERIC, "C");
#if HAVE_LIBDBI_TO_LONGLONG
    /* A less evil hack than the one equrie by libdbi-0.8, but
     * still necessary to work around the same bug.
     */
    auto retval = dbi_result_get_as_longlong(dbi_row->result,
                                             col_name);
#else
    /* A seriously evil hack to work around libdbi bug #15
     * https://sourceforge.net/p/libdbi/bugs/15/. When libdbi
     * v0.9 is widely available this can be replaced with
     * dbi_result_get_as_longlong.
     * Note: 0.9 is available in Debian Jessie and Fedora 21.
     */
    auto result = (dbi_result_t*) (m_inst->m_dbi_result);
    auto row = dbi_result_get_currow (result);
    auto idx = dbi_result_get_field_idx (result, col) - 1;
    time64 retval = result->rows[row]->field_values[idx].d_datetime;
    if (retval < MINTIME || retval > MAXTIME)
        retval = 0;
#endif //HAVE_LIBDBI_TO_LONGLONG
    gnc_pop_locale (LC_NUMERIC);
    return retval;
}


/* --------------------------------------------------------- */

