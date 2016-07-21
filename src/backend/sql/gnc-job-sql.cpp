/********************************************************************\
 * gnc-job-sql.c -- job sql backend                                 *
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
 *                                                                  *
\********************************************************************/

/** @file gnc-job-sql.c
 *  @brief load and save address data to SQL
 *  @author Copyright (c) 2007-2008 Phil Longstaff <plongstaff@rogers.com>
 *
 * This file implements the top-level QofBackend API for saving/
 * restoring data to/from an SQL database
 */
extern "C"
{
#include "config.h"

#include <glib.h>
#include <stdlib.h>
#include <string.h>

#include "gncJobP.h"
}

#include "gnc-backend-sql.h"
#include "gnc-slots-sql.h"
#include "gnc-job-sql.h"

#define _GNC_MOD_NAME   GNC_ID_JOB

G_GNUC_UNUSED static QofLogModule log_module = G_LOG_DOMAIN;

#define TABLE_NAME "jobs"
#define TABLE_VERSION 1

#define MAX_ID_LEN 2048
#define MAX_NAME_LEN 2048
#define MAX_REFERENCE_LEN 2048

static EntryVec col_table
({
    gnc_sql_make_table_entry<CT_GUID>("guid", 0, COL_NNUL | COL_PKEY, "guid"),
    gnc_sql_make_table_entry<CT_STRING>("id", MAX_ID_LEN, COL_NNUL,
                                        JOB_ID, true),
    gnc_sql_make_table_entry<CT_STRING>("name", MAX_NAME_LEN, COL_NNUL, "name"),
    gnc_sql_make_table_entry<CT_STRING>("reference", MAX_REFERENCE_LEN,
                                        COL_NNUL, JOB_REFERENCE, true),
    gnc_sql_make_table_entry<CT_BOOLEAN>("active", 0, COL_NNUL,
                                         (QofAccessFunc)gncJobGetActive,
                                         (QofSetterFunc)gncJobSetActive),
    gnc_sql_make_table_entry<CT_OWNERREF>("owner", 0, 0,
                                          (QofAccessFunc)gncJobGetOwner,
                                          (QofSetterFunc)gncJobSetOwner),
});

class GncSqlJobBackend : public GncSqlObjectBackend
{
public:
    GncSqlJobBackend(int version, const std::string& type,
                      const std::string& table, const EntryVec& vec) :
        GncSqlObjectBackend(version, type, table, vec) {}
    void load_all(GncSqlBackend*) override;
    bool write(GncSqlBackend*) override;
};


static GncJob*
load_single_job (GncSqlBackend* be, GncSqlRow& row)
{
    const GncGUID* guid;
    GncJob* pJob;

    g_return_val_if_fail (be != NULL, NULL);

    guid = gnc_sql_load_guid (be, row);
    pJob = gncJobLookup (be->book(), guid);
    if (pJob == NULL)
    {
        pJob = gncJobCreate (be->book());
    }
    gnc_sql_load_object (be, row, GNC_ID_JOB, pJob, col_table);
    qof_instance_mark_clean (QOF_INSTANCE (pJob));

    return pJob;
}

void
GncSqlJobBackend::load_all (GncSqlBackend* be)
{
    g_return_if_fail (be != NULL);

    auto stmt = gnc_sql_create_select_statement (be, TABLE_NAME);
    auto result = gnc_sql_execute_select_statement (be, stmt);
    GList* list = NULL;

    for (auto row : *result)
    {
        GncJob* pJob = load_single_job (be, row);
        if (pJob != NULL)
        {
            list = g_list_append (list, pJob);
        }
    }

    if (list != NULL)
    {
        gnc_sql_slots_load_for_list (be, list);
        g_list_free (list);
    }
}

/* ================================================================= */
static gboolean
job_should_be_saved (GncJob* job)
{
    const char* id;

    g_return_val_if_fail (job != NULL, FALSE);

    /* make sure this is a valid job before we save it -- should have an ID */
    id = gncJobGetID (job);
    if (id == NULL || *id == '\0')
    {
        return FALSE;
    }

    return TRUE;
}

static void
write_single_job (QofInstance* term_p, gpointer data_p)
{
    auto s = reinterpret_cast<write_objects_t*>(data_p);

    g_return_if_fail (term_p != NULL);
    g_return_if_fail (GNC_IS_JOB (term_p));
    g_return_if_fail (data_p != NULL);

    if (s->is_ok && job_should_be_saved (GNC_JOB (term_p)))
    {
        s->commit (term_p);
    }
}

bool
GncSqlJobBackend::write (GncSqlBackend* be)
{
    g_return_val_if_fail (be != NULL, FALSE);
    write_objects_t data{be, true, this};

    qof_object_foreach (GNC_ID_JOB, be->book(), write_single_job, &data);

    return data.is_ok;
}

/* ================================================================= */
void
gnc_job_sql_initialize (void)
{
    static GncSqlJobBackend be_data {
        GNC_SQL_BACKEND_VERSION, GNC_ID_JOB, TABLE_NAME, col_table};
    gnc_sql_register_backend(&be_data);
}
/* ========================== END OF FILE ===================== */
