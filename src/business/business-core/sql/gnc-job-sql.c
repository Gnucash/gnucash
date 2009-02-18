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

#include "config.h"

#include <glib.h>
#include <stdlib.h>
#include <string.h>

#include "gnc-backend-sql.h"
#include "gnc-slots-sql.h"

#include "gncJobP.h"
#include "gnc-job-sql.h"
#include "gnc-owner-sql.h"

#define _GNC_MOD_NAME	GNC_ID_JOB

static QofLogModule log_module = G_LOG_DOMAIN;

#define TABLE_NAME "jobs"
#define TABLE_VERSION 1

#define MAX_ID_LEN 2048
#define MAX_NAME_LEN 2048
#define MAX_REFERENCE_LEN 2048

static GncSqlColumnTableEntry col_table[] =
{
	{ "guid",      CT_GUID,     0,                 COL_NNUL|COL_PKEY, "guid" },
	{ "id",        CT_STRING,   MAX_ID_LEN,        COL_NNUL,          NULL, JOB_ID },
	{ "name",      CT_STRING,   MAX_NAME_LEN,      COL_NNUL,          NULL, JOB_NAME },
	{ "reference", CT_STRING,   MAX_REFERENCE_LEN, COL_NNUL,          NULL, JOB_REFERENCE },
	{ "active",    CT_BOOLEAN,  0,                 COL_NNUL,          NULL, NULL,
		(QofAccessFunc)gncJobGetActive, (QofSetterFunc)gncJobSetActive },
	{ "owner",     CT_OWNERREF, 0,                 COL_NNUL,          NULL, NULL,
		(QofAccessFunc)gncJobGetOwner, (QofSetterFunc)gncJobSetOwner },
	{ NULL }
};

static GncJob*
load_single_job( GncSqlBackend* be, GncSqlRow* row )
{
    const GUID* guid;
	GncJob* pJob;

	g_return_val_if_fail( be != NULL, NULL );
	g_return_val_if_fail( row != NULL, NULL );

    guid = gnc_sql_load_guid( be, row );
    pJob = gncJobLookup( be->primary_book, guid );
    if( pJob == NULL ) {
        pJob = gncJobCreate( be->primary_book );
    }
    gnc_sql_load_object( be, row, GNC_ID_JOB, pJob, col_table );
    qof_instance_mark_clean( QOF_INSTANCE(pJob) );

	return pJob;
}

static void
load_all_jobs( GncSqlBackend* be )
{
    GncSqlStatement* stmt;
    GncSqlResult* result;
    QofBook* pBook;

	g_return_if_fail( be != NULL );

    pBook = be->primary_book;

    stmt = gnc_sql_create_select_statement( be, TABLE_NAME );
    result = gnc_sql_execute_select_statement( be, stmt );
	gnc_sql_statement_dispose( stmt );
    if( result != NULL ) {
        GncSqlRow* row;
		GList* list = NULL;

		row = gnc_sql_result_get_first_row( result );
        while( row != NULL ) {
            GncJob* pJob = load_single_job( be, row );
			if( pJob != NULL ) {
				list = g_list_append( list, pJob );
			}
			row = gnc_sql_result_get_next_row( result );
		}
		gnc_sql_result_dispose( result );

		if( list != NULL ) {
			gnc_sql_slots_load_for_list( be, list );
		}
    }
}

/* ================================================================= */
static void
create_job_tables( GncSqlBackend* be )
{
	gint version;

	g_return_if_fail( be != NULL );

	version = gnc_sql_get_table_version( be, TABLE_NAME );
    if( version == 0 ) {
        gnc_sql_create_table( be, TABLE_NAME, TABLE_VERSION, col_table );
    }
}

/* ================================================================= */
static gboolean
save_job( GncSqlBackend* be, QofInstance* inst )
{
	g_return_val_if_fail( inst != NULL, FALSE );
	g_return_val_if_fail( GNC_IS_JOB(inst), FALSE );
	g_return_val_if_fail( be != NULL, FALSE );

    return gnc_sql_commit_standard_item( be, inst, TABLE_NAME, GNC_ID_JOB, col_table );
}

/* ================================================================= */
static gboolean
job_should_be_saved( GncJob *job )
{
    const char *id;

	g_return_val_if_fail( job != NULL, FALSE );

    /* make sure this is a valid job before we save it -- should have an ID */
    id = gncJobGetID( job );
    if( id == NULL || *id == '\0' ) {
        return FALSE;
	}

    return TRUE;
}

static void
write_single_job( QofInstance *term_p, gpointer data_p )
{
	write_objects_t* s = (write_objects_t*)data_p;

	g_return_if_fail( term_p != NULL );
	g_return_if_fail( GNC_IS_JOB(term_p) );
	g_return_if_fail( data_p != NULL );

	if( s->is_ok && job_should_be_saved( GNC_JOB(term_p) ) ) {
    	s->is_ok = save_job( s->be, term_p );
	}
}

static gboolean
write_jobs( GncSqlBackend* be )
{
	write_objects_t data;

	g_return_val_if_fail( be != NULL, FALSE );

	data.be = be;
	data.is_ok = TRUE;
    qof_object_foreach( GNC_ID_JOB, be->primary_book, write_single_job, &data );

	return data.is_ok;
}

/* ================================================================= */
void
gnc_job_sql_initialize( void )
{
    static GncSqlObjectBackend be_data =
    {
        GNC_SQL_BACKEND_VERSION,
        GNC_ID_JOB,
        save_job,						/* commit */
        load_all_jobs,					/* initial_load */
        create_job_tables,				/* create_tables */
		NULL, NULL, NULL,
		write_jobs						/* write */
    };

    qof_object_register_backend( GNC_ID_JOB, GNC_SQL_BACKEND, &be_data );
}
/* ========================== END OF FILE ===================== */
