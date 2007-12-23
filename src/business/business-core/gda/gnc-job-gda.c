/********************************************************************\
 * gnc-job-gda.c -- job gda backend                                 *
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

#include "config.h"

#include <glib.h>
#include <stdlib.h>
#include <string.h>

#include <libgda/libgda.h>

#include "gnc-backend-util-gda.h"

#include "gncJobP.h"
#include "gnc-job-gda.h"
#include "gnc-owner-gda.h"

#define _GNC_MOD_NAME	GNC_ID_JOB

static QofLogModule log_module = GNC_MOD_BACKEND;

#define TABLE_NAME "jobs"

#define MAX_ID_LEN 50
#define MAX_NAME_LEN 50
#define MAX_REFERENCE_LEN 50

static col_cvt_t col_table[] =
{
	{ "guid",      CT_GUID,     0,                 COL_NNUL, "guid" },
	{ "id",        CT_STRING,   MAX_ID_LEN,        COL_NNUL, NULL, JOB_ID },
	{ "name",      CT_STRING,   MAX_NAME_LEN,      COL_NNUL, NULL, JOB_NAME },
	{ "reference", CT_STRING,   MAX_REFERENCE_LEN, COL_NNUL, NULL, JOB_REFERENCE },
	{ "active",    CT_BOOLEAN,  0,                 COL_NNUL, NULL, JOB_ACTIVE },
	{ "owner",     CT_OWNERREF, 0,                 COL_NNUL, NULL, JOB_OWNER },
	{ NULL }
};

static GncJob*
load_single_job( GncGdaBackend* be, GdaDataModel* pModel, int row )
{
    const GUID* guid;
    GUID job_guid;
	GncJob* pJob;

    guid = gnc_gda_load_guid( be, pModel, row );
    job_guid = *guid;

    pJob = gncJobLookup( be->primary_book, &job_guid );
    if( pJob == NULL ) {
        pJob = gncJobCreate( be->primary_book );
    }
    gnc_gda_load_object( be, pModel, row, GNC_ID_JOB, pJob, col_table );
    gnc_gda_slots_load( be, qof_instance_get_guid( QOF_INSTANCE( pJob )),
                        qof_instance_get_slots( QOF_INSTANCE(pJob) ) );

    qof_instance_mark_clean( QOF_INSTANCE(pJob) );

    return pJob;
}

static void
load_all_jobs( GncGdaBackend* be )
{
    static GdaQuery* query = NULL;
    GdaObject* ret;
    QofBook* pBook = be->primary_book;

    /* First time, create the query */
    if( query == NULL ) {
        query = gnc_gda_create_select_query( be, TABLE_NAME );
    }

    ret = gnc_gda_execute_query( be, query );
    if( GDA_IS_DATA_MODEL( ret ) ) {
        GdaDataModel* pModel = GDA_DATA_MODEL(ret);
        int numRows = gda_data_model_get_n_rows( pModel );
        int r;

        for( r = 0; r < numRows; r++ ) {
            (void)load_single_job( be, pModel, r );
		}
    }
}

/* ================================================================= */
static void
create_job_tables( GncGdaBackend* be )
{
    gnc_gda_create_table_if_needed( be, TABLE_NAME, col_table );
}

/* ================================================================= */
void
gnc_gda_save_job( GncGdaBackend* be, QofInstance* inst )
{
    GncJob* job = GNC_JOB(inst);
    const GUID* guid;

    (void)gnc_gda_do_db_operation( be,
                        (qof_instance_get_destroying(inst) ? OP_DB_DELETE : OP_DB_ADD_OR_UPDATE ),
                        TABLE_NAME,
                        GNC_ID_JOB, job,
                        col_table );

    // Now, commit or delete any slots
    guid = qof_instance_get_guid( inst );
    if( !qof_instance_get_destroying(inst) ) {
        gnc_gda_slots_save( be, guid, qof_instance_get_slots( inst ) );
    } else {
        gnc_gda_slots_delete( be, guid );
    }
}

/* ================================================================= */
void
gnc_job_gda_initialize( void )
{
    static GncGdaDataType_t be_data =
    {
        GNC_GDA_BACKEND_VERSION,
        GNC_ID_JOB,
        gnc_gda_save_job,				/* commit */
        load_all_jobs,					/* initial_load */
        create_job_tables				/* create_tables */
    };

    qof_object_register_backend( GNC_ID_JOB, GNC_GDA_BACKEND, &be_data );
}
/* ========================== END OF FILE ===================== */
