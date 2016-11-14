/***********************************************************************\
 * gnc-sql-object-backend.hpp: Encapsulate per-class table schema.     *
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
#include "gnc-sql-object-backend.hpp"
#include "gnc-sql-backend.hpp"
#include "gnc-sql-column-table-entry.hpp"
#include "gnc-slots-sql.h"

static QofLogModule log_module = G_LOG_DOMAIN;

bool
GncSqlObjectBackend::commit (GncSqlBackend* sql_be, QofInstance* inst)
{
    const GncGUID* guid;
    gboolean is_infant;
    E_DB_OPERATION op;
    gboolean is_ok;

    is_infant = qof_instance_get_infant (inst);
    if (qof_instance_get_destroying (inst))
    {
        op = OP_DB_DELETE;
    }
    else if (sql_be->pristine() || is_infant)
    {
        op = OP_DB_INSERT;
    }
    else
    {
        op = OP_DB_UPDATE;
    }
    is_ok = sql_be->do_db_operation(op, m_table_name.c_str(),
                                    m_type_name.c_str(), inst, m_col_table);

    if (is_ok)
    {
        // Now, commit any slots
        guid = qof_instance_get_guid (inst);
        if (!qof_instance_get_destroying (inst))
        {
            is_ok = gnc_sql_slots_save (sql_be, guid, is_infant, inst);
        }
        else
        {
            is_ok = gnc_sql_slots_delete (sql_be, guid);
        }
    }

    return is_ok;
}

void
GncSqlObjectBackend::create_tables (GncSqlBackend* sql_be)
{
    g_return_if_fail (sql_be != nullptr);
    int version = sql_be->get_table_version (m_table_name);
    if (version == 0) //No tables, otherwise version will sql_be >= 1.
    {
        sql_be->create_table(m_table_name, m_col_table);
        sql_be->set_table_version(m_table_name, m_version);
    }
    else if (version != m_version)
        PERR("Version mismatch in table %s, expecting %d but backend is %d."
             "Table creation aborted.", m_table_name.c_str(), m_version, version);
}

bool
GncSqlObjectBackend::instance_in_db(const GncSqlBackend* sql_be,
                                    QofInstance* inst) const noexcept
{
    return sql_be->object_in_db(m_table_name.c_str(), m_type_name.c_str(),
                                inst, m_col_table);
}
