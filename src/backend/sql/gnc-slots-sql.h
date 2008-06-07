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

void gnc_sql_slots_save( GncSqlBackend* be, const GUID* guid, KvpFrame* pFrame );
void gnc_sql_slots_delete( GncSqlBackend* be, const GUID* guid );
void gnc_sql_slots_load( GncSqlBackend* be, QofInstance* inst );
void gnc_sql_slots_load_for_list( GncSqlBackend* be, GList* list );

void gnc_sql_init_slots_handler( void );

#endif /* GNC_SLOTS_SQL_H_ */
