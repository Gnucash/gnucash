/********************************************************************
 * gnc-backend-gda.h: load and save data to SQL via libgda          *
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
/** @file gnc-backend-gda.h
 *  @brief load and save data to SQL via libgda
 *  @author Copyright (c) 2006 Phil Longstaff <plongstaff@rogers.com>
 *
 * This file implements the top-level QofBackend API for saving/
 * restoring data to/from an SQL database via libgda
 */

#ifndef GNC_BACKEND_GDA_UTIL_H_
#define GNC_BACKEND_GDA_UTIL_H_

#include "qof.h"
#include <gmodule.h>

#include "qofbackend-p.h"
struct GncGdaBackend_struct
{
  QofBackend be;

  GdaClient* pClient;
  GdaConnection* pConnection;
  GdaDict* pDict;

  QofBook *primary_book;	/* The primary, main open book */
  gboolean	loading;		/* We are performing an initial load */
  gboolean  in_query;
  gboolean  supports_transactions;
  gboolean  is_pristine_db;	// Are we saving to a new pristine db?

  gint obj_total;			// Total # of objects (for percentage calculation)
  gint operations_done;		// Number of operations (save/load) done
};
typedef struct GncGdaBackend_struct GncGdaBackend;

/**
 * Struct used to pass in a new data type for GDA storage.  This contains
 * the set of callbacks to read and write GDA for new data objects..  New
 * types should register an instance of this object with the engine.
 *
 * commit()			- commit an object to the db
 * initial_load()	- load stuff when new db opened
 * create_tables()  - create any db tables
 * compile_query()  - compile a backend object query
 * run_query()      - run a compiled query
 * free_query()     - free a compiled query
 * write()          - write all objects
 */
#define GNC_GDA_BACKEND	"gnc:gda:1"
#define GNC_GDA_BACKEND_VERSION	1
typedef struct
{
  int		version;	/* backend version number */
  const gchar *	type_name;	/* The XML tag for this type */

  void		(*commit)( QofInstance* inst, GncGdaBackend* be );
  void		(*initial_load)( GncGdaBackend* pBackend );
  void		(*create_tables)( GncGdaBackend* pBackend );
  gpointer	(*compile_query)( GncGdaBackend* pBackend, QofQuery* pQuery );
  void		(*run_query)( GncGdaBackend* pBackend, gpointer pQuery );
  void		(*free_query)( GncGdaBackend* pBackend, gpointer pQuery );
  void		(*write)( GncGdaBackend* pBackend );
} GncGdaDataType_t;

// Type for conversion of db row to object.
#define CT_STRING "ct_string"
#define CT_GUID "ct_guid"
#define CT_INT "ct_int"
#define CT_INT64 "ct_int64"
#define CT_TIMESPEC "ct_timespec"
#define CT_GDATE "ct_gdate"
#define CT_NUMERIC "ct_numeric"
#define CT_DOUBLE "ct_double"
#define CT_BOOLEAN "ct_boolean"
#define CT_ACCOUNTREF "ct_accountref"
#define CT_COMMODITYREF "ct_commodityref"
#define CT_TXREF "ct_txref"
#define CT_LOTREF "ct_lotref"

typedef struct {
	const gchar* col_name;
	const gchar* col_type;
	gint size;
#define COL_PKEY	0x01
#define COL_NNUL	0x02
#define COL_UNIQUE	0x04
#define COL_AUTOINC	0x08
	gint flags;
	const gchar* gobj_param_name;	// If non-null, use g_object_get/g_object_set
	const gchar* param_name;	// If non null, use qof getter/setter
	QofAccessFunc getter;
	QofSetterFunc setter;
} col_cvt_t;

typedef enum {
	OP_DB_ADD,
	OP_DB_ADD_OR_UPDATE,
	OP_DB_DELETE
} E_DB_OPERATION;

typedef void (*GNC_GDA_LOAD_FN)( const GncGdaBackend* be, GdaDataModel* pModel, gint row,
                                QofSetterFunc setter, gpointer pObject,
                                const col_cvt_t* table );
typedef void (*GNC_GDA_CREATE_COL_FN)( GdaServerProvider* server,
				                        GdaConnection* cnn, GdaServerOperation* op,
                        				const col_cvt_t* table_row );
typedef void (*GNC_GDA_GET_GVALUE_QUERY_FN)( const GncGdaBackend* be,
                QofIdTypeConst obj_name, const gpointer pObject,
                const col_cvt_t* table_row, GdaQuery* query );
typedef GdaQueryCondition* (*GNC_GDA_GET_GVALUE_COND_FN)( const GncGdaBackend* be,
                QofIdTypeConst obj_name, const gpointer pObject,
                const col_cvt_t* table_row, GdaQuery* query );

typedef struct {
    GNC_GDA_LOAD_FN             load_fn;
    GNC_GDA_CREATE_COL_FN       create_col_fn;
    GNC_GDA_GET_GVALUE_QUERY_FN get_gvalue_query_fn;
    GNC_GDA_GET_GVALUE_COND_FN  get_gvalue_cond_fn;
} col_type_handler_t;

QofAccessFunc gnc_gda_get_getter( QofIdTypeConst obj_name, const col_cvt_t* table_row );
void gnc_gda_add_table_column( GdaServerOperation* op, const gchar* arg, const gchar* dbms_type,
            gint size, gint flags );

gboolean gnc_gda_do_db_operation( GncGdaBackend* pBackend,
									E_DB_OPERATION op,
									const gchar* table_name,
									QofIdTypeConst obj_name,
									gpointer pObject,
									const col_cvt_t* table );
GdaQuery* gnc_gda_build_insert_query( GncGdaBackend* pBackend,
									const gchar* table_name,
									QofIdTypeConst obj_name,
									gpointer pObject,
									const col_cvt_t* table );
GdaQuery* gnc_gda_build_update_query( GncGdaBackend* pBackend,
									const gchar* table_name,
									QofIdTypeConst obj_name,
									gpointer pObject,
									const col_cvt_t* table );
GdaQuery* gnc_gda_build_delete_query( GncGdaBackend* pBackend,
									const gchar* table_name,
									QofIdTypeConst obj_name,
									gpointer pObject,
									const col_cvt_t* table );
GdaObject* gnc_gda_execute_query( GncGdaBackend* pBackend, GdaQuery* pQuery );
GdaDataModel* gnc_gda_execute_sql( const GncGdaBackend* pBackend, const gchar* sql );
GdaQuery* gnc_gda_create_query_from_sql( const GncGdaBackend* pBackend, const gchar* sql );
int gnc_gda_execute_select_get_count( const GncGdaBackend* pBackend, const gchar* sql );
int gnc_gda_execute_query_get_count( GncGdaBackend* pBackend, GdaQuery* query );
void gnc_gda_load_object( const GncGdaBackend* be, GdaDataModel* pModel, int row,
						QofIdTypeConst obj_name, gpointer pObject,
						const col_cvt_t* table );
gboolean gnc_gda_object_is_it_in_db( GncGdaBackend* be,
									const gchar* table_name,
									QofIdTypeConst obj_name, const gpointer pObject,
									const col_cvt_t* table );
gboolean gnc_gda_does_table_exist( const GncGdaBackend* be, const gchar* table_name );
gboolean gnc_gda_create_table( const GncGdaBackend* be,
						const gchar* table_name, const col_cvt_t* col_table,
						GError** error );
gboolean gnc_gda_create_index( const GncGdaBackend* be, const gchar* index_name,
						const gchar* table_name, const col_cvt_t* col_table,
						GError** error );
void gnc_gda_create_table_if_needed( const GncGdaBackend* be,
						const gchar* table_name, const col_cvt_t* col_table );
const GUID* gnc_gda_load_guid( const GncGdaBackend* be, GdaDataModel* pModel, int row );
const GUID* gnc_gda_load_tx_guid( const GncGdaBackend* be, GdaDataModel* pModel, int row );
GdaQuery* gnc_gda_create_select_query( const GncGdaBackend* be, const gchar* table_name );
GdaQueryCondition* gnc_gda_create_condition_from_field( GdaQuery* query,
														const gchar* col_name,
														const GValue* value );
void gnc_gda_register_col_type_handler( const gchar* colType, const col_type_handler_t* handler );
void gnc_gda_register_standard_col_type_handlers( void );
void gnc_gda_add_field_to_query( GdaQuery* query, const gchar* col_name, const GValue* value );

void gnc_gda_get_gvalue_objectref_guid_for_query( const GncGdaBackend* be, QofIdTypeConst obj_name,
                const gpointer pObject, const col_cvt_t* table_row, GdaQuery* query );
GdaQueryCondition* gnc_gda_get_gvalue_objectref_guid_cond( const GncGdaBackend* be, QofIdTypeConst obj_name,
                const gpointer pObject, const col_cvt_t* table_row, GdaQuery* query );
void gnc_gda_create_objectref_guid_col( GdaServerProvider* server, GdaConnection* cnn,
	            GdaServerOperation* op, const col_cvt_t* table_row );
void gnc_gda_append_guid_list_to_sql( GString* str, GList* list );

void _retrieve_guid_( gpointer pObject, gpointer pValue );

G_MODULE_EXPORT const gchar *
g_module_check_init( GModule *module );

#endif /* GNC_BACKEND_GDA_UTIL_H_ */
