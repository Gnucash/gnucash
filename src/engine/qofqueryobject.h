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

#include "qofquery.h"
#include "qofquerycore.h"

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
  QofQueryCoreType   param_type;
  QofAccessFunc param_getfcn;
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
 * { MY_QUERY_OBJ_MEMO, QOF_QUERYCORE_STRING, myMemoGetter },
 * { MY_QUERY_OBJ_VALUE, QOF_QUERYCORE_NUMERIC, myValueGetter },
 * { MY_QUERY_OBJ_DATE, QOF_QUERYCORE_DATE, myDateGetter },
 * { MY_QUERY_OBJ_ACCOUNT, GNC_ID_ACCOUNT, myAccountGetter },
 * { MY_QUERY_OBJ_TRANS, GNC_ID_TRANS, myTransactionGetter },
 * NULL };
 *
 * qof_query_object_registerParamters ("myObjectName", myQueryObjectCompare,
 *				    &myQueryObjectParams);
 */

/** Return the core datatype of the specified object's parameter */
QofQueryCoreType qof_query_object_parameter_type (QofIdTypeConst obj_name,
					   const char *param_name);

/** Return the registered Object Definition for the requested parameter */
const QofQueryObject * qof_query_object_get_parameter (QofIdTypeConst obj_name,
						   const char *parameter);

/** Return the object's parameter getter function */
QofAccessFunc qof_query_object_get_parameter_getter (QofIdTypeConst obj_name,
					      const char *parameter);


#endif /* QOF_QUERYOBJECT_H */
