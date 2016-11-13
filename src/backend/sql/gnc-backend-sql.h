/********************************************************************
 * gnc-backend-sql.h: load and save data to SQL                     *
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

/**
 * @defgroup SQLBE SQL Backend Core
 @{
*/

/** @addtogroup Columns Columns
    @ingroup SQLBE
*/

/** The SQL backend core is a library which can form the core for a QOF
 *  backend based on an SQL library.
 *
 *  @file gnc-backend-sql.h
 *  @brief load and save data to SQL
 *  @author Copyright (c) 2006-2008 Phil Longstaff <plongstaff@rogers.com>
 */

#ifndef GNC_BACKEND_SQL_H
#define GNC_BACKEND_SQL_H
extern "C"
{
#include <qof.h>
#include <qofbackend-p.h>
#include <gmodule.h>
}

#include <string>
#include <vector>

#include "gnc-sql-object-backend.hpp"

using StrVec = std::vector<std::string>;
using PairVec = std::vector<std::pair<std::string, std::string>>;
class GncSqlRow;

#define GNC_SQL_BACKEND             "gnc:sql:1"
#define GNC_SQL_BACKEND_VERSION 1


gpointer gnc_sql_compile_query (QofBackend* qof_be, QofQuery* pQuery);
void gnc_sql_free_query (QofBackend* qof_be, gpointer pQuery);
void gnc_sql_run_query (QofBackend* qof_be, gpointer pQuery);

#endif /* GNC_BACKEND_SQL_H */

/**
   @}  end of the SQL Backend Core doxygen group
*/
