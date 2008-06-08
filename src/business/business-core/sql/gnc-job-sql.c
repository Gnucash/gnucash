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

#include "gnc-backend-util-sql.h"
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

static col_cvt_t col_table[] =
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

static void
load_single_job( GncSqlBackend* be, GncSqlRow* row )
{
    const GUID* guid;
	GncJob* pJob;

	g_return_if_fail( be != NULL );
	g_return_if_fail( row != NULL );

    guid = gnc_sql_load_guid( be, row );
    pJob = gncJobLookup( be->primary_book, guid );
    if( pJob == NULL ) {
        pJob = gncJobCreate( be->primary_book );
    }
    gnc_sql_load_object( be, row, GNC_ID_JOB, pJob, col_table );
    gnc_sql_slots_load( be, QOF_INSTANCE(pJob) );

    qof_instance_mark_clean( QOF_INSTANCE(pJob) );
}

static void
load_all_jobs( GncSqlBackend* be )
{
    GncSqlStatement* stmt;
    GncSqlResult* result;
    QofBook* pBook;

	g_return_if_fail( be != NULL );

    pBook = be->primary_book;

    stmt = gnc_sql_create_select_statement( be, TABLE_NAME, col_table );
    result = gnc_sql_execute_sql_statement( be, stmt );
	gnc_sql_statement_dispose( stmt );
    if( result != NULL ) {
        GncSqlRow* row;

		row = gnc_sql_result_get_first_row( result );
        while( row != NULL ) {
            load_single_job( be, row );
			row = gnc_sql_result_get_next_row( result );
		}
		gnc_sql_result_dispose( result );
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
    	GError* error = NULL;

        gnc_sql_create_table( be, TABLE_NAME, TABLE_VERSION, col_table, &error );
        if( error != NULL ) {
            PERR( "Error creating table: %s\n", error->message );
        }
    }
}

/* ================================================================= */
static void
save_job( QofInstance* inst, GncSqlBackend* be )
{
    const GUID* guid;

	g_return_if_fail( inst != NULL );
	g_return_if_fail( GNC_IS_JOB(inst) );
	g_return_if_fail( be != NULL );

    (void)gnc_sql_do_db_operation( be,
                        (qof_instance_get_destroying(inst) ? OP_DB_DELETE : OP_DB_ADD_OR_UPDATE ),
                        TABLE_NAME,
                        GNC_ID_JOB, inst,
                        col_table );

    // Now, commit or delete any slots
    guid = qof_instance_get_guid( inst );
    if( !qof_instance_get_destroying(inst) ) {
        gnc_sql_slots_save( be, guid, qof_instance_get_slots( inst ) );
    } else {
        gnc_sql_slots_delete( be, guid );
    }
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
write_single_job( QofInstance *term_p, gpointer be_p )
{
    GncSqlBackend* be = (GncSqlBackend*)be_p;

	g_return_if_fail( term_p != NULL );
	g_return_if_fail( GNC_IS_JOB(term_p) );
	g_return_if_fail( be_p != NULL );

	if( job_should_be_saved( GNC_JOB(term_p) ) ) {
    	save_job( term_p, be );
	}
}

static void
write_jobs( GncSqlBackend* be )
{
	g_return_if_fail( be != NULL );

    qof_object_foreach( GNC_ID_JOB, be->primary_book, write_single_job, (gpointer)be );
}

/* ================================================================= */
void
gnc_job_sql_initialize( void )
{
    static GncSqlDataType_t be_data =
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
