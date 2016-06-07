/********************************************************************
 * gnc-recurrence-sql.c: load and save data to SQL                  *
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
/** @file gnc-recurrence-sql.c
 *  @brief load and save data to SQL
 *  @author Copyright (c) 2006-2008 Phil Longstaff <plongstaff@rogers.com>
 *
 * This file implements the top-level QofBackend API for saving/
 * restoring data to/from an SQL db
 */
#include <guid.hpp>
extern "C"
{
#include "config.h"

#include <glib.h>

#include "qof.h"
#include "gnc-engine.h"
#include "Recurrence.h"

#if defined( S_SPLINT_S )
#include "splint-defs.h"
#endif
}

#include "gnc-backend-sql.h"
#include "gnc-recurrence-sql.h"

G_GNUC_UNUSED static QofLogModule log_module = G_LOG_DOMAIN;

#define TABLE_NAME "recurrences"
#define TABLE_VERSION 2

#define BUDGET_MAX_RECURRENCE_PERIOD_TYPE_LEN 2048
#define BUDGET_MAX_RECURRENCE_WEEKEND_ADJUST_LEN 2048

typedef struct
{
    GncSqlBackend* be;
    const GncGUID* guid;
    Recurrence* pRecurrence;
} recurrence_info_t;

static  gpointer get_obj_guid (gpointer pObject);
static void set_obj_guid (void);
static gint get_recurrence_mult (gpointer pObject);
static void set_recurrence_mult (gpointer pObject, gint value);
static  gpointer get_recurrence_period_type (gpointer pObject);
static void set_recurrence_period_type (gpointer pObject,  gpointer pValue);
static  gpointer get_recurrence_weekend_adjust (gpointer pObject);
static void set_recurrence_weekend_adjust (gpointer pObject,  gpointer pValue);
static  gpointer get_recurrence_period_start (gpointer pObject);
static void set_recurrence_period_start (gpointer pObject,  gpointer pValue);

static const GncSqlColumnTableEntry col_table[] =
{
    { "id",                      CT_INT,    0,                                     COL_PKEY | COL_NNUL | COL_AUTOINC },
    {
        "obj_guid",                CT_GUID,   0,                                     COL_NNUL, NULL, NULL,
        (QofAccessFunc)get_obj_guid, (QofSetterFunc)set_obj_guid
    },
    {
        "recurrence_mult",         CT_INT,    0,                                     COL_NNUL, NULL, NULL,
        (QofAccessFunc)get_recurrence_mult, (QofSetterFunc)set_recurrence_mult
    },
    {
        "recurrence_period_type",  CT_STRING, BUDGET_MAX_RECURRENCE_PERIOD_TYPE_LEN, COL_NNUL, NULL, NULL,
        (QofAccessFunc)get_recurrence_period_type, set_recurrence_period_type
    },
    {
        "recurrence_period_start", CT_GDATE,  0,                                     COL_NNUL, NULL, NULL,
        (QofAccessFunc)get_recurrence_period_start, set_recurrence_period_start
    },
    {
        "recurrence_weekend_adjust",  CT_STRING, BUDGET_MAX_RECURRENCE_WEEKEND_ADJUST_LEN, COL_NNUL, NULL, NULL,
        (QofAccessFunc)get_recurrence_weekend_adjust, set_recurrence_weekend_adjust
    },
    { NULL }
};

/* Special column table because we need to be able to access the table by
a column other than the primary key */
static const GncSqlColumnTableEntry guid_col_table[] =
{
    {
        "obj_guid", CT_GUID, 0, 0, NULL, NULL,
        (QofAccessFunc)get_obj_guid, (QofSetterFunc)set_obj_guid
    },
    { NULL }
};

/* Special column table used to upgrade table from version 1 to 2 */
static const GncSqlColumnTableEntry weekend_adjust_col_table[] =
{
    {
        "recurrence_weekend_adjust",  CT_STRING, BUDGET_MAX_RECURRENCE_WEEKEND_ADJUST_LEN, 0,
    },
    { NULL }
};

/* ================================================================= */

static  gpointer
get_obj_guid (gpointer pObject)
{
    recurrence_info_t* pInfo = (recurrence_info_t*)pObject;

    g_return_val_if_fail (pObject != NULL, NULL);

    return (gpointer)pInfo->guid;
}

static void
set_obj_guid (void)
{
    // Nowhere to put the GncGUID
}

static gint
get_recurrence_mult (gpointer pObject)
{
    recurrence_info_t* pInfo = (recurrence_info_t*)pObject;

    g_return_val_if_fail (pObject != NULL, 0);
    g_return_val_if_fail (pInfo->pRecurrence != NULL, 0);

    return (gint)pInfo->pRecurrence->mult;
}

static void
set_recurrence_mult (gpointer pObject, gint value)
{
    recurrence_info_t* pInfo = (recurrence_info_t*)pObject;

    g_return_if_fail (pObject != NULL);
    g_return_if_fail (pInfo->pRecurrence != NULL);

    pInfo->pRecurrence->mult = (guint16)value;
}

static  gpointer
get_recurrence_period_type (gpointer pObject)
{
    recurrence_info_t* pInfo = (recurrence_info_t*)pObject;

    g_return_val_if_fail (pObject != NULL, NULL);
    g_return_val_if_fail (pInfo->pRecurrence != NULL, NULL);

    return (gpointer)recurrencePeriodTypeToString (
               recurrenceGetPeriodType (pInfo->pRecurrence));
}

static void
set_recurrence_period_type (gpointer pObject, gpointer pValue)
{
    recurrence_info_t* pInfo = (recurrence_info_t*)pObject;

    g_return_if_fail (pObject != NULL);
    g_return_if_fail (pInfo->pRecurrence != NULL);
    g_return_if_fail (pValue != NULL);

    pInfo->pRecurrence->ptype = recurrencePeriodTypeFromString ((gchar*)pValue);
}

static  gpointer
get_recurrence_weekend_adjust (gpointer pObject)
{
    recurrence_info_t* pInfo = (recurrence_info_t*)pObject;

    g_return_val_if_fail (pObject != NULL, NULL);
    g_return_val_if_fail (pInfo->pRecurrence != NULL, NULL);

    return (gpointer)recurrenceWeekendAdjustToString (
               recurrenceGetWeekendAdjust (pInfo->pRecurrence));
}

static void
set_recurrence_weekend_adjust (gpointer pObject, gpointer pValue)
{
    recurrence_info_t* pInfo = (recurrence_info_t*)pObject;

    g_return_if_fail (pObject != NULL);
    g_return_if_fail (pInfo->pRecurrence != NULL);
    g_return_if_fail (pValue != NULL);

    pInfo->pRecurrence->wadj = recurrenceWeekendAdjustFromString ((gchar*)pValue);
}

static  gpointer
get_recurrence_period_start (gpointer pObject)
{
    recurrence_info_t* pInfo = (recurrence_info_t*)pObject;
    static GDate date;

    g_return_val_if_fail (pObject != NULL, NULL);
    g_return_val_if_fail (pInfo->pRecurrence != NULL, NULL);

    date = recurrenceGetDate (pInfo->pRecurrence);
    return (gpointer)&date;
}

static void
set_recurrence_period_start (gpointer pObject, gpointer pValue)
{
    recurrence_info_t* pInfo = (recurrence_info_t*)pObject;
    GDate* date = (GDate*)pValue;

    g_return_if_fail (pObject != NULL);
    g_return_if_fail (pInfo->pRecurrence != NULL);
    g_return_if_fail (pValue != NULL);

    pInfo->pRecurrence->start = *date;
}

/* ================================================================= */

gboolean
gnc_sql_recurrence_save (GncSqlBackend* be, const GncGUID* guid,
                         const Recurrence* r)
{
    recurrence_info_t recurrence_info;

    g_return_val_if_fail (be != NULL, FALSE);
    g_return_val_if_fail (guid != NULL, FALSE);
    g_return_val_if_fail (r != NULL, FALSE);

    (void)gnc_sql_recurrence_delete (be, guid);

    recurrence_info.be = be;
    recurrence_info.guid = guid;
    recurrence_info.pRecurrence = (Recurrence*)r;
    return gnc_sql_do_db_operation (be, OP_DB_INSERT, TABLE_NAME,
                                    TABLE_NAME, &recurrence_info, col_table);
}

void
gnc_sql_recurrence_save_list (GncSqlBackend* be, const GncGUID* guid,
                              GList* schedule)
{
    recurrence_info_t recurrence_info;
    GList* l;

    g_return_if_fail (be != NULL);
    g_return_if_fail (guid != NULL);

    (void)gnc_sql_recurrence_delete (be, guid);

    recurrence_info.be = be;
    recurrence_info.guid = guid;
    for (l = schedule; l != NULL; l = g_list_next (l))
    {
        recurrence_info.pRecurrence = (Recurrence*)l->data;
        (void)gnc_sql_do_db_operation (be, OP_DB_INSERT, TABLE_NAME,
                                       TABLE_NAME, &recurrence_info, col_table);
    }
}

gboolean
gnc_sql_recurrence_delete (GncSqlBackend* be, const GncGUID* guid)
{
    recurrence_info_t recurrence_info;

    g_return_val_if_fail (be != NULL, FALSE);
    g_return_val_if_fail (guid != NULL, FALSE);

    recurrence_info.be = be;
    recurrence_info.guid = guid;
    return gnc_sql_do_db_operation (be, OP_DB_DELETE, TABLE_NAME,
                                    TABLE_NAME, &recurrence_info, guid_col_table);
}

static void
load_recurrence (GncSqlBackend* be, GncSqlRow* row,  Recurrence* r)
{
    recurrence_info_t recurrence_info;

    g_return_if_fail (be != NULL);
    g_return_if_fail (row != NULL);
    g_return_if_fail (r != NULL);

    recurrence_info.be = be;
    recurrence_info.pRecurrence = r;

    gnc_sql_load_object (be, row, TABLE_NAME, &recurrence_info, col_table);
}

static  GncSqlResult*
gnc_sql_set_recurrences_from_db (GncSqlBackend* be, const GncGUID* guid)
{
    gchar* buf;
    gchar guid_buf[GUID_ENCODING_LENGTH + 1];
    GncSqlStatement* stmt;
    GncSqlResult* result;

    g_return_val_if_fail (be != NULL, NULL);
    g_return_val_if_fail (guid != NULL, NULL);

    (void)guid_to_string_buff (guid, guid_buf);
    buf = g_strdup_printf ("SELECT * FROM %s WHERE obj_guid='%s'", TABLE_NAME,
                           guid_buf);
    stmt = gnc_sql_connection_create_statement_from_sql (be->conn, buf);
    g_free (buf);
    result = gnc_sql_execute_select_statement (be, stmt);
    gnc_sql_statement_dispose (stmt);
    return result;
}

Recurrence*
gnc_sql_recurrence_load (GncSqlBackend* be, const GncGUID* guid)
{
    GncSqlResult* result;
    Recurrence* r = NULL;

    g_return_val_if_fail (be != NULL, NULL);
    g_return_val_if_fail (guid != NULL, NULL);

    result = gnc_sql_set_recurrences_from_db (be, guid);
    if (result != NULL)
    {
        guint numRows = gnc_sql_result_get_num_rows (result);

        if (numRows > 0)
        {
            if (numRows > 1)
            {
                g_warning ("More than 1 recurrence found: first one used");
            }
            r = g_new0 (Recurrence, 1);
            g_assert (r != NULL);
            load_recurrence (be, gnc_sql_result_get_first_row (result), r);
        }
        else
        {
            g_warning ("No recurrences found");
        }
        gnc_sql_result_dispose (result);
    }

    return r;
}

GList*
gnc_sql_recurrence_load_list (GncSqlBackend* be, const GncGUID* guid)
{
    GncSqlResult* result;
    GList* list = NULL;

    g_return_val_if_fail (be != NULL, NULL);
    g_return_val_if_fail (guid != NULL, NULL);

    result = gnc_sql_set_recurrences_from_db (be, guid);
    if (result != NULL)
    {
        GncSqlRow* row = gnc_sql_result_get_first_row (result);

        while (row != NULL)
        {
            Recurrence* pRecurrence = g_new0 (Recurrence, 1);
            g_assert (pRecurrence != NULL);
            load_recurrence (be, row, pRecurrence);
            list = g_list_append (list, pRecurrence);
            row = gnc_sql_result_get_next_row (result);
        }
        gnc_sql_result_dispose (result);
    }

    return list;
}

/* ================================================================= */
static void
upgrade_recurrence_table_1_2 (GncSqlBackend* be)
{
    /* Step 1: add field, but allow it to be null */
    gboolean ok = gnc_sql_add_columns_to_table (be, TABLE_NAME,
                                                weekend_adjust_col_table);
    if (!ok)
    {
        PERR ("Unable to add recurrence_weekend_adjust column\n");
        return;
    }

    /* Step 2: insert a default value in the newly created column */
    {
        gchar* weekend_adj_str = recurrenceWeekendAdjustToString (WEEKEND_ADJ_NONE);
        gchar* update_query = g_strdup_printf ("UPDATE %s SET %s = '%s';",
                                               TABLE_NAME,
                                               weekend_adjust_col_table[0].col_name,
                                               weekend_adj_str);
        (void)gnc_sql_execute_nonselect_sql (be, update_query);
        g_free (weekend_adj_str);
        g_free (update_query);
    }

    /* Step 3: rewrite the table, requiring the weekend_adj column to be non-null */
    gnc_sql_upgrade_table (be, TABLE_NAME, col_table);

}

static void
create_recurrence_tables (GncSqlBackend* be)
{
    gint version;
    gboolean ok;

    g_return_if_fail (be != NULL);

    version = gnc_sql_get_table_version (be, TABLE_NAME);
    if (version == 0)
    {
        (void)gnc_sql_create_table (be, TABLE_NAME, TABLE_VERSION, col_table);
    }
    else if (version < TABLE_VERSION)
    {
        /* Upgrade:
            1->2: Add recurrence_weekend_adjust field (mandatory, non-null field)
        */
        if (version == 1)
        {
            upgrade_recurrence_table_1_2 (be);
        }
        (void)gnc_sql_set_table_version (be, TABLE_NAME, TABLE_VERSION);
        PINFO ("Recurrence table upgraded from version %d to version %d\n", version,
               TABLE_VERSION);
    }
}

/* ================================================================= */
void
gnc_sql_init_recurrence_handler (void)
{
    static GncSqlObjectBackend be_data =
    {
        GNC_SQL_BACKEND_VERSION,
        GNC_ID_ACCOUNT,
        NULL,                           /* commit - cannot occur */
        NULL,                           /* initial_load - cannot occur */
        create_recurrence_tables,       /* create_tables */
        NULL,                           /* compile_query */
        NULL,                           /* run_query */
        NULL,                           /* free_query */
        NULL                            /* write */
    };

    (void)qof_object_register_backend (TABLE_NAME, GNC_SQL_BACKEND, &be_data);
}
/* ========================== END OF FILE ===================== */
