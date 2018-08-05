/********************************************************************
 * gnc-slots-sql.h: load and save data to SQL                       *
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
/** @file gnc-slots-sql.h
 *  @brief load and save accounts data to SQL
 *  @author Copyright (c) 2006-2008 Phil Longstaff <plongstaff@rogers.com>
 *
 * This file implements the top-level QofBackend API for saving/
 * restoring data to/from an SQL database
 */

#ifndef GNC_SLOTS_SQL_H
#define GNC_SLOTS_SQL_H
extern "C"
{
#include <glib.h>
#include "guid.h"
#include "qof.h"
}
#include "gnc-sql-object-backend.hpp"

/**
 * Slots are neither loadable nor committable. Note that the default
 * write() implementation is also a no-op.
 */
class GncSqlSlotsBackend : public GncSqlObjectBackend
{
public:
    GncSqlSlotsBackend();
    void load_all(GncSqlBackend*) override { return; }
    void create_tables(GncSqlBackend*) override;
    bool commit(GncSqlBackend*, QofInstance*) override { return false; }
};

/**
 * gnc_sql_slots_save - Saves slots for an object to the db.
 *
 * @param sql_be SQL backend
 * @param guid Object guid
 * @param is_infant Is this an infant object?
 * @param inst The QodInstance owning the slots.
 * @return TRUE if successful, FALSE if error
 */
gboolean gnc_sql_slots_save (GncSqlBackend* sql_be, const GncGUID* guid,
                             gboolean is_infant, QofInstance* inst);

/**
 * gnc_sql_slots_delete - Deletes slots for an object from the db.
 *
 * @param sql_be SQL backend
 * @param guid Object guid
 * @return TRUE if successful, FALSE if error
 */
gboolean gnc_sql_slots_delete (GncSqlBackend* sql_be, const GncGUID* guid);

/** Loads slots for an object from the db.
 *
 * @param sql_be SQL backend
 */
void gnc_sql_slots_load (GncSqlBackend* sql_be, QofInstance* inst);

typedef QofInstance* (*BookLookupFn) (const GncGUID* guid,
                                      const QofBook* book);

/**
 * gnc_sql_slots_load_for_sql_subquery - Loads slots for all objects whose guid is
 * supplied by a subquery.  The subquery should be of the form "SELECT DISTINCT guid FROM ...".
 * This is faster than loading for one object at a time because fewer SQL queries * are used.
 *
 * @param sql_be SQL backend
 * @param subquery Subquery SQL string
 * @param lookup_fn Lookup function to get the right object from the book
 */
void gnc_sql_slots_load_for_sql_subquery (GncSqlBackend* sql_be,
                                          const std::string subquery,
                                          BookLookupFn lookup_fn);

void gnc_sql_init_slots_handler (void);

#endif /* GNC_SLOTS_SQL_H */
