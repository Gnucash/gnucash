/********************************************************************
 * gnc-schedxaction-sql.h: load and save data to SQL                *
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
/** @file gnc-backend-sql.h
 *  @brief load and save data to SQL
 *  @author Copyright (c) 2006-2008 Phil Longstaff <plongstaff@rogers.com>
 *
 * This file implements the top-level QofBackend API for saving/
 * restoring data to/from an SQL database
 */

#ifndef GNC_SCHEDXACTION_SQL_H
#define GNC_SCHEDXACTION_SQL_H
extern "C"
{
#include "qof.h"
}
#include "gnc-sql-object-backend.hpp"

class GncSqlSchedXactionBackend : public GncSqlObjectBackend
{
public:
    GncSqlSchedXactionBackend();
    void load_all(GncSqlBackend*) override;
    bool commit (GncSqlBackend* sql_be, QofInstance* inst) override;
};

gboolean gnc_sql_save_schedxaction (GncSqlBackend* sql_be, QofInstance* inst);

#endif /* GNC_SCHEDXACTION_SQL_H */
