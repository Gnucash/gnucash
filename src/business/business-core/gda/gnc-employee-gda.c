/********************************************************************\
 * gnc-employee-xml-v2.c -- employee xml i/o implementation         *
 *                                                                  *
 * Copyright (C) 2002 Derek Atkins <warlord@MIT.EDU>                *
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

#include "gncEmployeeP.h"
#include "gnc-employee-gda.h"
#include "gnc-address-gda.h"
#include "gnc-commodity.h"

#include "gnc-backend-util-gda.h"

#define _GNC_MOD_NAME	GNC_ID_EMPLOYEE

static QofLogModule log_module = GNC_MOD_BACKEND;

#define MAX_USERNAME_LEN 50
#define MAX_ID_LEN 50
#define MAX_LANGUAGE_LEN 50
#define MAX_ACL_LEN 50

#define employee_addr_string "employee:addr"

#define TABLE_NAME "employees"

static col_cvt_t col_table[] =
{
	{ "guid",       CT_GUID,          0,                COL_NNUL, "guid" },
	{ "username",   CT_STRING,        MAX_USERNAME_LEN, COL_NNUL, NULL, EMPLOYEE_USERNAME },
	{ "id",         CT_STRING,        MAX_ID_LEN,       COL_NNUL, NULL, EMPLOYEE_ID },
	{ "language",   CT_STRING,        MAX_LANGUAGE_LEN, COL_NNUL, NULL, EMPLOYEE_LANGUAGE },
	{ "acl",        CT_STRING,        MAX_ACL_LEN,      COL_NNUL, NULL, EMPLOYEE_ACL },
	{ "active",     CT_BOOLEAN,       0,                COL_NNUL, NULL, QOF_PARAM_ACTIVE },
	{ "currency",   CT_COMMODITYREF,  0,                COL_NNUL, NULL, NULL,
			(QofAccessFunc)gncEmployeeGetCurrency, (QofSetterFunc)gncEmployeeSetCurrency },
	{ "ccard_guid", CT_ACCOUNTREF,    0,                COL_NNUL, NULL, EMPLOYEE_CC },
	{ "workday",    CT_NUMERIC,       0,                COL_NNUL, NULL, EMPLOYEE_WORKDAY },
	{ "rate",       CT_NUMERIC,       0,                COL_NNUL, NULL, EMPLOYEE_RATE },
    { NULL }
};

static GncEmployee*
load_single_employee( GncGdaBackend* be, GdaDataModel* pModel, int row )
{
    const GUID* guid;
    GUID emp_guid;
	GncEmployee* pEmployee;

    guid = gnc_gda_load_guid( be, pModel, row );
    emp_guid = *guid;

    pEmployee = gncEmployeeLookup( be->primary_book, &emp_guid );
    if( pEmployee == NULL ) {
        pEmployee = gncEmployeeCreate( be->primary_book );
    }
    gnc_gda_load_object( be, pModel, row, GNC_ID_EMPLOYEE, pEmployee, col_table );
    gnc_gda_slots_load( be, qof_instance_get_guid( QOF_INSTANCE( pEmployee )),
                        qof_instance_get_slots( QOF_INSTANCE(pEmployee) ) );

    qof_instance_mark_clean( QOF_INSTANCE(pEmployee) );

    return pEmployee;
}

static void
load_all_employees( GncGdaBackend* be )
{
    static GdaQuery* query = NULL;
    GdaObject* ret;
    QofBook* pBook = be->primary_book;
    gnc_commodity_table* pTable = gnc_commodity_table_get_table( pBook );

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
            (void)load_single_employee( be, pModel, r );
		}
    }
}

/* ================================================================= */
static void
create_employee_tables( GncGdaBackend* be )
{
    gnc_gda_create_table_if_needed( be, TABLE_NAME, col_table );
}

/* ================================================================= */
void
gnc_gda_save_employee( GncGdaBackend* be, QofInstance* inst )
{
    GncEmployee* emp = GNC_EMPLOYEE(inst);
    const GUID* guid;

    // Ensure the commodity is in the db
    gnc_gda_save_commodity( be, gncEmployeeGetCurrency( emp ) );

    (void)gnc_gda_do_db_operation( be,
                        (qof_instance_get_destroying(inst) ? OP_DB_DELETE : OP_DB_ADD_OR_UPDATE ),
                        TABLE_NAME,
                        GNC_ID_EMPLOYEE, emp,
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
gnc_employee_gda_initialize( void )
{
    static GncGdaDataType_t be_data =
    {
        GNC_GDA_BACKEND_VERSION,
        GNC_ID_EMPLOYEE,
        gnc_gda_save_employee,				/* commit */
        load_all_employees,					/* initial_load */
        create_employee_tables				/* create_tables */
    };

    qof_object_register_backend( GNC_ID_EMPLOYEE, GNC_GDA_BACKEND, &be_data );
}
/* ========================== END OF FILE ===================== */
