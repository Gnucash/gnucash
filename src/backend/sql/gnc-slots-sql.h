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

#ifndef GNC_SLOTS_SQL_H_
#define GNC_SLOTS_SQL_H_

#include "qof.h"
#include <gmodule.h>

/**
 * gnc_sql_slots_save - Saves slots for an object to the db.
 *
 * @param be SQL backend
 * @param guid Object guid
 * @param is_infant Is this an infant object?
 * @param pFrame Top-level KVP frame
 * @return TRUE if successful, FALSE if error
 */
gboolean gnc_sql_slots_save( GncSqlBackend* be, const GUID* guid,
					gboolean is_infant, KvpFrame* pFrame );

/**
 * gnc_sql_slots_delete - Deletes slots for an object from the db.
 *
 * @param be SQL backend
 * @param guid Object guid
 * @return TRUE if successful, FALSE if error
 */
gboolean gnc_sql_slots_delete( GncSqlBackend* be, const GUID* guid );

/**
 * gnc_sql_slots_load - Loads slots for an object from the db.
 *
 * @param be SQL backend
 * @param guid Object guid
 */
void gnc_sql_slots_load( GncSqlBackend* be, QofInstance* inst );

/**
 * gnc_sql_slots_load_for_list - Loads slots for a list of objects from the db.
 * Loading slots for a list of objects can be faster than loading for one object
 * at a time because fewer SQL queries are used.
 *
 * @param be SQL backend
 * @param list List of objects
 */
void gnc_sql_slots_load_for_list( GncSqlBackend* be, GList* list );

/**
 * gnc_sql_slots_load_for_sql_subquery - Loads slots for all objects whose guid is
 * supplied by a subquery.  The subquery should be of the form "SELECT DISTINCT guid FROM ...".
 * This is faster than loading for one object at a time because fewer SQL queries * are used.
 *
 * @param be SQL backend
 * @param subquery Subquery SQL string
 * @param lookup_fn Lookup function to get the right object from the book
 */
typedef QofInstance* (*BookLookupFn)( const GUID* guid, const QofBook* book );

void gnc_sql_slots_load_for_sql_subquery( GncSqlBackend* be, const gchar* subquery,
						BookLookupFn lookup_fn );

void gnc_sql_init_slots_handler( void );

#endif /* GNC_SLOTS_SQL_H_ */
