/********************************************************************\
 * qofqueryobject.h -- API for registering queriable objects        *
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

/** @file qofqueryobject.h
    @brief API for registering queriable objects 
    @author Copyright (C) 2002 Derek Atkins <warlord@MIT.EDU>
*/

#ifndef QOF_QUERYOBJECT_H
#define QOF_QUERYOBJECT_H

// #include "qofquery.h"
#include "qofid.h"

#define QOF_TYPE_STRING    "string"
#define QOF_TYPE_DATE    	"date"
#define QOF_TYPE_NUMERIC   "numeric"
#define QOF_TYPE_DEBCRED   "debcred"
#define QOF_TYPE_GUID    	"guid"
#define QOF_TYPE_INT32    	"gint32"
#define QOF_TYPE_INT64    	"gint64"
#define QOF_TYPE_DOUBLE    "double"
#define QOF_TYPE_BOOLEAN   "boolean"
#define QOF_TYPE_KVP    	"kvp"
#define QOF_TYPE_CHAR    	"character"

/** Type of Paramters (String, Date, Numeric, GUID, etc.) */
typedef const char * QofType;

/** The QofAccessFunc type defines an arbitrary function pointer
 *  for access functions.  This is needed because C doesn't have
 *  templates, so we just cast a lot.  Real functions must be of
 *  the form:
 *
 * <param_type> function (object_type *obj);
 */
typedef gpointer (*QofAccessFunc)(gpointer);

/** This structure is for each queriable parameter in an object
 *
 * -- param_name is the name of the parameter.
 * -- param_type is the type of the parameter, which can be either another
 *    object or it can be a core data type.
 * -- param_getgcn is the function to actually obtain the parameter
 */
typedef struct _QofQueryObject 
{
  const char       * param_name;
  QofType            param_type;
  QofAccessFunc      param_getfcn;
} QofQueryObject;

/** This function is the default sort function for a particular object type */
typedef int (*QofSortFunc)(gpointer, gpointer);

/** This function registers a new Gnucash Object with the QofQuery
 * subsystem.  In particular it registers the set of parameters and
 * converters to query the type-specific data.  Both "params" and
 * "converters" are NULL-terminated arrays of structures.  Either
 * argument may be NULL if there is nothing to be registered.
 */
void qof_query_object_register (QofIdTypeConst obj_name,
			     QofSortFunc default_sort_fcn,
			     const QofQueryObject *params);

/** An example:
 *
 * #define MY_QUERY_OBJ_MEMO	"memo"
 * #define MY_QUERY_OBJ_VALUE	"value"
 * #define MY_QUERY_OBJ_DATE	"date"
 * #define MY_QUERY_OBJ_ACCOUNT "account"
 * #define MY_QUERY_OBJ_TRANS	"trans"
 *
 * static QofQueryObject myQueryObjectParams[] = {
 * { MY_QUERY_OBJ_MEMO, QOF_TYPE_STRING, myMemoGetter },
 * { MY_QUERY_OBJ_VALUE, QOF_TYPE_NUMERIC, myValueGetter },
 * { MY_QUERY_OBJ_DATE, QOF_TYPE_DATE, myDateGetter },
 * { MY_QUERY_OBJ_ACCOUNT, GNC_ID_ACCOUNT, myAccountGetter },
 * { MY_QUERY_OBJ_TRANS, GNC_ID_TRANS, myTransactionGetter },
 * NULL };
 *
 * qof_query_object_register ("myObjectName", myQueryObjectCompare,
 *				    &myQueryObjectParams);
 */

/** Return the core datatype of the specified object's parameter */
QofType qof_query_object_parameter_type (QofIdTypeConst obj_name,
					   const char *param_name);

/** Return the registered Object Definition for the requested parameter */
const QofQueryObject * qof_query_object_get_parameter (QofIdTypeConst obj_name,
						   const char *parameter);

/** Return the object's parameter getter function */
QofAccessFunc qof_query_object_get_parameter_getter (QofIdTypeConst obj_name,
					      const char *parameter);


#endif /* QOF_QUERYOBJECT_H */
