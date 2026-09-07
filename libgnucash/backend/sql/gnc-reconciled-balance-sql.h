/********************************************************************
 * gnc-reconciled-balance-sql.h: load and save data to SQL           *
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
/** @file gnc-reconciled-balance-sql.h
 *  @brief load and save reconciled balances to SQL
 */

#ifndef GNC_RECONCILED_BALANCE_SQL_H
#define GNC_RECONCILED_BALANCE_SQL_H

#include "gnc-sql-object-backend.hpp"

class GncSqlReconciledBalanceBackend : public GncSqlObjectBackend
{
public:
    GncSqlReconciledBalanceBackend();
    void load_all (GncSqlBackend*) override;
    void create_tables (GncSqlBackend*) override;
    bool write (GncSqlBackend*) override;
};

#endif /* GNC_RECONCILED_BALANCE_SQL_H */
