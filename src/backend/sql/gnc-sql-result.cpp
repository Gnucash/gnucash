/***********************************************************************\
 * gnc-sql-result.cpp: Encapsulate the results of a SQL query.         *
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

extern "C"
{
#include <config.h>
}
#include <sstream>
#include "gnc-sql-column-table-entry.hpp"

#include "gnc-sql-result.hpp"

GncSqlRow&
GncSqlRow::operator++()
{
    auto& new_row =  m_iter->operator++();
    if (new_row != *this)
        m_iter = nullptr;
    return new_row;
}

/*
  GncSqlResult*
  GncSqlRow::operator*()
  {
  return m_iter->operator*();
  }
*/
