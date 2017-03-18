/********************************************************************
 * gnc-dbiprovider.cpp: Encapsulate differences among Dbi backends. *
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

#ifndef __GNC_DBIPROVIDER_HPP__
#define __GNC_DBIPROVIDER_HPP__

extern "C"
{
#include <dbi/dbi.h>
}
#include <string>
#include <vector>

/**
 * Provides the primary abstraction for different DBI backends.
 */
class GncSqlConnection;
struct GncSqlColumnInfo;
using ColVec = std::vector<GncSqlColumnInfo>;
using StrVec = std::vector<std::string>;
class GncDbiProvider
{
public:
    virtual ~GncDbiProvider() = default;
    virtual StrVec get_table_list(dbi_conn conn, const std::string& table) = 0;
    virtual void append_col_def(std::string& ddl,
                                const GncSqlColumnInfo& info) = 0;
    virtual StrVec get_index_list (dbi_conn conn) = 0;
    virtual void drop_index(dbi_conn conn, const std::string& index) = 0;
};

using GncDbiProviderPtr = std::unique_ptr<GncDbiProvider>;

#endif //__GNC_DBIPROVIDER_HPP__
