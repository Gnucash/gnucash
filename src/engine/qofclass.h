/********************************************************************\
 * qofclass.h -- API for registering paramters on objects           *
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

/** @file qofclass.h
    @brief API for registering paramters on objects 
    @author Copyright (C) 2002 Derek Atkins <warlord@MIT.EDU>
    @author Copyright (C) 2003 Linas Vepstas <linas@linas.org>

  This file defines a class messaging system reminiscent of
  traditional OO-style setter and getter interfaces to object 
  properties.  A C-language object can declare a collection
  of setters and getters on itself that can then be used 
  to perform run-time (as opposed to compile-time) bindings 
  to the object.
*/

#ifndef QOF_CLASS_H
#define QOF_CLASS_H

#include "qofid.h"

/** Core types of objects that can be used in parameters.
 *  Note that QofIdTypes may also be used.  */

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

typedef struct _QofParam QofParam;

/** The QofAccessFunc defines an arbitrary function pointer
 *  for access functions.  This is needed because C doesn't have
 *  templates, so we just cast a lot.  Real functions must be of
 *  the form:
 *
 *        param_type getter_func (object_type *self);
 *  or
 *        param_type getter_func (object_type *self, QofParam *param);
 *
 * The additional argument 'param' allows generic getter functions
 * to be implemented, because this argument provides for a way to
 * identify the expected getter_func return type at runtime.  It
 * also provides a place for the user to hang additional user-defined
 * data.
 */
typedef gpointer (*QofAccessFunc)(gpointer object, const QofParam *param);

/** The QofSetterFunc defines an function pointer for parameter
 *  setters. Real functions must be of the form:
 *
 * void setter_func (object_type *self, param_type *param);
 */
typedef void (*QofSetterFunc) (gpointer, gpointer);

/** This structure is for each queriable parameter in an object
 *
 * -- param_name is the name of the parameter.
 * -- param_type is the type of the parameter, which can be either another
 *    object (QofIdType) or it can be a core data type (QofType).
 * -- param_getfcn is the function to actually obtain the parameter
 * -- param_setfcn is the function to actually set the parameter
 * -- param_userdata is a place where the user can place any desiered
 *    user-defined data (and thus can be used by the user-defined
 *    setter/getter).
 *
 * Either the getter or the setter may be NULL.
 *
 *  XXX todo/fixme: need to define a destroy callback, so that when
 * the param memory is freed, the callback can be used to release the 
 * user-defined data.
 */
struct _QofParam 
{
  const char       * param_name;
  QofType            param_type;
  QofAccessFunc      param_getfcn;
  QofSetterFunc      param_setfcn;
  gpointer           param_userdata;
};

/** This function is the default sort function for a particular object type */
typedef int (*QofSortFunc)(gpointer, gpointer);

/** This function registers a new object class with the Qof subsystem.
 *  In particular, it registers the set of setters and getters for
 *  controlling the object.   The getters are typically used by the
 *  query subsystem to query type specific data.   Note that there
 *  is no particular reqquirement for there to be a setter for every
 *  getter or even v.v. nor is there any requeirement for these to 
 *  map 'cleanly' or orthogonaly to the actual object design.
 *
 *  The "params" argument must be a NULL-terminated array of QofParam. 
 *  It may be NULL if there are no parameters to be registered.
 */
void qof_class_register (QofIdTypeConst obj_name,
			     QofSortFunc default_sort_fcn,
			     const QofParam *params);

/** An example:
 *
 * #define MY_OBJ_MEMO	"memo"
 * #define MY_OBJ_VALUE	"value"
 * #define MY_OBJ_DATE	"date"
 * #define MY_OBJ_ACCOUNT "account"
 * #define MY_OBJ_TRANS	"trans"
 *
 * static QofParam myParams[] = {
 * { MY_OBJ_MEMO, QOF_TYPE_STRING, myMemoGetter, NULL },
 * { MY_OBJ_VALUE, QOF_TYPE_NUMERIC, myValueGetter, NULL },
 * { MY_OBJ_DATE, QOF_TYPE_DATE, myDateGetter, NULL },
 * { MY_OBJ_ACCOUNT, GNC_ID_ACCOUNT, myAccountGetter, NULL },
 * { MY_OBJ_TRANS, GNC_ID_TRANS, myTransactionGetter, NULL },
 * NULL };
 *
 * qof_class_register ("myObjectName", myObjectCompare, &myParams);
 */

/** Return true if the the indicated type is registered, 
 *  else return FALSE.
 */
gboolean qof_class_is_registered (QofIdTypeConst obj_name);

/** Return the core datatype of the specified object's parameter */
QofType qof_class_get_parameter_type (QofIdTypeConst obj_name,
					   const char *param_name);

/** Return the registered Parameter Definition for the requested parameter */
const QofParam * qof_class_get_parameter (QofIdTypeConst obj_name,
						   const char *parameter);

/** Return the object's parameter getter function */
QofAccessFunc qof_class_get_parameter_getter (QofIdTypeConst obj_name,
					      const char *parameter);

/** Return the object's parameter setter function */
QofSetterFunc qof_class_get_parameter_setter (QofIdTypeConst obj_name,
					      const char *parameter);


#endif /* QOF_CLASS_H */
