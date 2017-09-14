/********************************************************************
 * gnc-recurrence-sql.h: load and save data to SQL                  *
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
/** @file gnc-recurrence-sql.h
 *  @brief load and save accounts data to SQL
 *  @author Copyright (c) 2006-2008 Phil Longstaff <plongstaff@rogers.com>
 *
 * This file implements the top-level QofBackend API for saving/
 * restoring data to/from an SQL database
 */

#ifndef GNC_RECURRENCE_SQL_H
#define GNC_RECURRENCE_SQL_H
extern "C"
{
#include <glib.h>
#include "Recurrence.h"
#include "guid.h"
}
#include "gnc-sql-object-backend.hpp"

class GncSqlRecurrenceBackend : public GncSqlObjectBackend
{
public:
    GncSqlRecurrenceBackend();
    void load_all(GncSqlBackend*) override { return; }
    void create_tables(GncSqlBackend*) override;
    bool commit(GncSqlBackend*, QofInstance*) override { return false; }
};

gboolean gnc_sql_recurrence_save (GncSqlBackend* sql_be, const GncGUID* guid,
                                  const Recurrence* pRecurrence);
void gnc_sql_recurrence_save_list (GncSqlBackend* sql_be, const GncGUID* guid,
                                   GList* schedule);
gboolean gnc_sql_recurrence_delete (GncSqlBackend* sql_be, const GncGUID* guid);
Recurrence* gnc_sql_recurrence_load (GncSqlBackend* sql_be, const GncGUID* guid);
GList* gnc_sql_recurrence_load_list (GncSqlBackend* sql_be, const GncGUID* guid);

#endif /* GNC_RECURRENCE_SQL_H */
