/*
 * gnc-entry-sql.h -- entry sql backend
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

/** @file gnc-entry-sql.h
 *  @brief load and save entry data to SQL
 *  @author Copyright (c) 2007-2008 Phil Longstaff <plongstaff@rogers.com>
 *
 * This file implements the top-level QofBackend API for saving/
 * restoring data to/from an SQL database
 */

#ifndef GNC_ENTRY_SQL_H
#define GNC_ENTRY_SQL_H

#include "gnc-sql-object-backend.hpp"

class GncSqlEntryBackend : public GncSqlObjectBackend
{
public:
    GncSqlEntryBackend();
    void load_all(GncSqlBackend*) override;
    void create_tables(GncSqlBackend*) override;
    bool write(GncSqlBackend*) override;
};

#endif /* GNC_ENTRY_SQL_H */
