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
};
typedef struct GncGdaBackend_struct GncGdaBackend;

/**
 * Struct used to pass in a new data type for GDA storage.  This contains
 * the set of callbacks to read and write GDA for new data objects..  New
 * types should register an instance of this object with the engine.
 *
 * commit()			- commit an object to the db
 * initial_load()	- load stuff when new db opened
 */
#define GNC_GDA_BACKEND	"gnc:gda:1"
#define GNC_GDA_BACKEND_VERSION	1
typedef struct
{
  int		version;	/* backend version number */
  const gchar *	type_name;	/* The XML tag for this type */

  void		(*commit)( GncGdaBackend* pBackend, QofInstance* inst );
  void		(*initial_load)( GncGdaBackend* pBackend );
  void		(*create_tables)( GncGdaBackend* pBackend );
  gpointer	(*compile_query)( GncGdaBackend* pBackend, QofQuery* pQuery );
  void		(*run_query)( GncGdaBackend* pBackend, gpointer pQuery );
  void		(*free_query)( GncGdaBackend* pBackend, gpointer pQuery );
} GncGdaDataType_t;

// This is now a static inside the module
//QofBackend * libgncmod_backend_gda_LTX_gnc_backend_new(void);

// Type for conversion of db row to object.
typedef enum {
	CT_STRING,
	CT_GUID,
	CT_INT,
	CT_INT64,
	CT_TIMESPEC,
	CT_GDATE,
	CT_NUMERIC,
	CT_DOUBLE,
	CT_BOOLEAN
} E_COL_TYPE;

typedef struct {
	const gchar* col_name;
	E_COL_TYPE col_type;
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
GdaObject* gnc_gda_execute_sql( GncGdaBackend* pBackend, const gchar* sql );
int gnc_gda_execute_select_get_count( GncGdaBackend* pBackend, const gchar* sql );
int gnc_gda_execute_query_get_count( GncGdaBackend* pBackend, GdaQuery* query );
void gnc_gda_load_object( GdaDataModel* pModel, int row,
						QofIdTypeConst obj_name, gpointer pObject,
						const col_cvt_t* table );
gboolean gnc_gda_object_is_it_in_db( GncGdaBackend* be,
									const gchar* table_name,
									QofIdTypeConst obj_name, gpointer pObject,
									const col_cvt_t* table );
gboolean gnc_gda_create_table( GdaConnection* pConnection,
						const gchar* table_name, col_cvt_t* col_table,
						GError** error );
void gnc_gda_create_table_if_needed( GncGdaBackend* be,
						const gchar* table_name, col_cvt_t* col_table );
const GUID* gnc_gda_load_guid( GdaDataModel* pModel, int row );
GdaQuery* gnc_gda_create_select_query( const GncGdaBackend* be, const gchar* table_name );
GdaQueryCondition* gnc_gda_create_condition_from_field( GdaQuery* query,
														const gchar* col_name,
														const GValue* value );

G_MODULE_EXPORT const gchar *
g_module_check_init(GModule *module);

#endif /* GNC_BACKEND_GDA_UTIL_H_ */
