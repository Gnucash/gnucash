/********************************************************************\
 * gnc-employee-sql.c -- employee sql implementation                *
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

/** @file gnc-employee-sql.c
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

#include "gnc-commodity.h"

#include "gnc-backend-sql.h"
#include "gnc-slots-sql.h"
#include "gnc-commodity-sql.h"

#include "gncEmployeeP.h"
#include "gnc-employee-sql.h"
#include "gnc-address-sql.h"

#define _GNC_MOD_NAME	GNC_ID_EMPLOYEE

static QofLogModule log_module = G_LOG_DOMAIN;

#define MAX_USERNAME_LEN 2048
#define MAX_ID_LEN 2048
#define MAX_LANGUAGE_LEN 2048
#define MAX_ACL_LEN 2048

#define TABLE_NAME "employees"
#define TABLE_VERSION 1

static GncSqlColumnTableEntry col_table[] =
{
	{ "guid",       CT_GUID,          0,                COL_NNUL|COL_PKEY, "guid" },
	{ "username",   CT_STRING,        MAX_USERNAME_LEN, COL_NNUL,          NULL, EMPLOYEE_USERNAME },
	{ "id",         CT_STRING,        MAX_ID_LEN,       COL_NNUL,          NULL, EMPLOYEE_ID },
	{ "language",   CT_STRING,        MAX_LANGUAGE_LEN, COL_NNUL,          NULL, EMPLOYEE_LANGUAGE },
	{ "acl",        CT_STRING,        MAX_ACL_LEN,      COL_NNUL,          NULL, EMPLOYEE_ACL },
	{ "active",     CT_BOOLEAN,       0,                COL_NNUL,          NULL, QOF_PARAM_ACTIVE },
	{ "currency",   CT_COMMODITYREF,  0,                COL_NNUL,          NULL, NULL,
			(QofAccessFunc)gncEmployeeGetCurrency, (QofSetterFunc)gncEmployeeSetCurrency },
	{ "ccard_guid", CT_ACCOUNTREF,    0,                0,                 NULL, EMPLOYEE_CC },
	{ "workday",    CT_NUMERIC,       0,                COL_NNUL,          NULL, EMPLOYEE_WORKDAY },
	{ "rate",       CT_NUMERIC,       0,                COL_NNUL,          NULL, EMPLOYEE_RATE },
	{ "addr",       CT_ADDRESS,       0,                0,                 NULL, EMPLOYEE_ADDR },
    { NULL }
};

static void
load_single_employee( GncSqlBackend* be, GncSqlRow* row )
{
    const GUID* guid;
	GncEmployee* pEmployee;

	g_return_if_fail( be != NULL );
	g_return_if_fail( row != NULL );

    guid = gnc_sql_load_guid( be, row );
    pEmployee = gncEmployeeLookup( be->primary_book, guid );
    if( pEmployee == NULL ) {
        pEmployee = gncEmployeeCreate( be->primary_book );
    }
    gnc_sql_load_object( be, row, GNC_ID_EMPLOYEE, pEmployee, col_table );
    gnc_sql_slots_load( be, QOF_INSTANCE(pEmployee) );

    qof_instance_mark_clean( QOF_INSTANCE(pEmployee) );
}

static void
load_all_employees( GncSqlBackend* be )
{
    GncSqlStatement* stmt;
    GncSqlResult* result;
    QofBook* pBook;
    gnc_commodity_table* pTable;

	g_return_if_fail( be != NULL );

    pBook = be->primary_book;
    pTable = gnc_commodity_table_get_table( pBook );

    stmt = gnc_sql_create_select_statement( be, TABLE_NAME );
    result = gnc_sql_execute_select_statement( be, stmt );
	gnc_sql_statement_dispose( stmt );
    if( result != NULL ) {
        GncSqlRow* row;

		row = gnc_sql_result_get_first_row( result );
        while( row != NULL ) {
            load_single_employee( be, row );
		}
		gnc_sql_result_dispose( result );
    }
}

/* ================================================================= */
static void
create_employee_tables( GncSqlBackend* be )
{
	gint version;

	g_return_if_fail( be != NULL );

	version = gnc_sql_get_table_version( be, TABLE_NAME );
    if( version == 0 ) {
        gnc_sql_create_table( be, TABLE_NAME, TABLE_VERSION, col_table );
    }
}

/* ================================================================= */
static void
save_employee( GncSqlBackend* be, QofInstance* inst )
{
    GncEmployee* emp = GNC_EMPLOYEE(inst);
    const GUID* guid;

	g_return_if_fail( inst != NULL );
	g_return_if_fail( GNC_IS_EMPLOYEE(inst) );
	g_return_if_fail( be != NULL );

    // Ensure the commodity is in the db
    gnc_sql_save_commodity( be, gncEmployeeGetCurrency( emp ) );

    (void)gnc_sql_do_db_operation( be,
                        (qof_instance_get_destroying(inst) ? OP_DB_DELETE : OP_DB_ADD_OR_UPDATE ),
                        TABLE_NAME,
                        GNC_ID_EMPLOYEE, emp,
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
employee_should_be_saved( GncEmployee *employee )
{
    const char *id;

	g_return_val_if_fail( employee != NULL, FALSE );

    /* make sure this is a valid employee before we save it -- should have an ID */
    id = gncEmployeeGetID( employee );
    if( id == NULL || *id == '\0' ) {
        return FALSE;
	}

    return TRUE;
}
static void
write_single_employee( QofInstance *term_p, gpointer be_p )
{
    GncSqlBackend* be = (GncSqlBackend*)be_p;

	g_return_if_fail( term_p != NULL );
	g_return_if_fail( GNC_IS_EMPLOYEE(term_p) );
	g_return_if_fail( be_p != NULL );

	if( employee_should_be_saved( GNC_EMPLOYEE(term_p) ) ) {
    	save_employee( be, term_p );
	}
}

static void
write_employees( GncSqlBackend* be )
{
	g_return_if_fail( be != NULL );

    qof_object_foreach( GNC_ID_EMPLOYEE, be->primary_book, write_single_employee, (gpointer)be );
}

/* ================================================================= */
void
gnc_employee_sql_initialize( void )
{
    static GncSqlObjectBackend be_data =
    {
        GNC_SQL_BACKEND_VERSION,
        GNC_ID_EMPLOYEE,
        save_employee,						/* commit */
        load_all_employees,					/* initial_load */
        create_employee_tables,				/* create_tables */
		NULL, NULL, NULL,
		write_employees						/* write */
    };

    qof_object_register_backend( GNC_ID_EMPLOYEE, GNC_SQL_BACKEND, &be_data );
}
/* ========================== END OF FILE ===================== */
