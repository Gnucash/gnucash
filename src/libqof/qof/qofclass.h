/********************************************************************\
 * qofclass.h -- API for registering parameters on objects          *
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

/** @addtogroup Object
    @{ */

/** @addtogroup Class
  This file defines a class messaging system reminiscent of
  traditional OO-style setter and getter interfaces to object
  properties.  A C-language object can declare a collection
  of setters and getters on itself that can then be used
  to perform run-time (as opposed to compile-time) bindings
  to the object.

  To put it differently, a QOF class is a set of parameter
  getters and setters that are associated with an object type.
  Given a pointer to some thing, the setters and getters can
  be used to get and set values out of that thing.  Note
  that the pointer to "some thing" need not be a pointer
  to a QOF Entity or Instance (although QOF classes are
  more interesting when used with Entities/Instances).
  What "some thing" is defined entirely by the programmer
  declaring a new QOF Class.

  Because a QOF Class associates getters and setters with
  a type, one can then ask, at run time, what parameters
  are associated with a given type, even if those parameters
  were not known at compile time.  Thus, a QOF Class is
  sort-of like a DynAny implementation.  QOF classes can
  be used to provide "object introspection", i.e. asking
  object to describe itself.

  The QOF Query subsystem depends on QOF classes having been
  declared; the Query uses the getters to get values associated
  with particular instances.

  A QofAccessFunc or QofSetterFunc do not need to be public
  functions, if you need to add functions to an object with an
  established API, define the additional QOF routines as static.
  Only the register routine needs to be public.
@{ */

/** @file qofclass.h
    @brief API for registering paramters on objects
    @author Copyright (C) 2002 Derek Atkins <warlord@MIT.EDU>
    @author Copyright (C) 2003 Linas Vepstas <linas@linas.org>
    @author Copyright (c) 2005 Neil Williams <linux@codehelp.co.uk>
*/

#ifndef QOF_CLASS_H
#define QOF_CLASS_H

#include "qofid.h"

#define QOF_MOD_CLASS "qof.class"

/** \name Core types

Core data types for objects that can be used in parameters.
Note that QofIdTypes may also be used and will create a
single reference between two known objects.

 @{
 */

#define QOF_TYPE_STRING    "string"
#define QOF_TYPE_DATE      "date"
#define QOF_TYPE_NUMERIC   "numeric"
#define QOF_TYPE_DEBCRED   "debcred"
#define QOF_TYPE_GUID      "guid"
#define QOF_TYPE_INT32     "gint32"
#define QOF_TYPE_INT64     "gint64"
#define QOF_TYPE_DOUBLE    "double"
#define QOF_TYPE_BOOLEAN   "boolean"
#define QOF_TYPE_KVP       "kvp"
#define QOF_TYPE_CHAR      "character"
#define QOF_TYPE_COLLECT   "collection" /**< secondary collections
are used for one-to-many references between entities and are
implemented using ::QofCollection.
These are \b NOT the same as the main collections in the QofBook.

-# Each ::QofCollection contains one or many entities - *all* of a single type.
-# The entity type within the collection can be determined at run time.
-# Easy conversions to GList or whatever in the param_setfcn handler.
-# Each parameter can have its own collection.
-# Each entity can have a different *type* of collection to its siblings,
provided that it is acceptable to the set function.
-# Each object decides which types are acceptable for which parameter in the
    set functions. This is then part of the API for that object.

        QOF_TYPE_COLLECT has two functions, both related to one-to-many
links:
        - Represent a reference between 2 entities with a list of acceptable types.
        (one object linked to many types of single entities)
            - Represent a reference between one entity and many entities of another type.
            (one object linked to many entities of a single type.)

            If the set function can handle it, it could also be used for true one-to-many
links: one object linked to many entities of many types.

            n.b. Always subject to each collection holding only one type at runtime.
            (otherwise use books).

                */
                /** @} */
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
typedef gpointer (*QofAccessFunc)(gpointer object, /*@ null @*/ const QofParam *param);

/** The QofSetterFunc defines an function pointer for parameter
 *  setters. Real functions must be of the form:
 *
 * void setter_func (object_type *self, param_type *param);
 */
typedef void (*QofSetterFunc) (gpointer, /*@ null @*/ gpointer);

/* A callback for how to compare two (same-type) objects based on a
 * common getter (parameter member), using the provided comparison
 * options (which are the type-specific options).
 */
typedef gint (*QofCompareFunc) (gpointer a, gpointer b,
                                gint compare_options,
                                QofParam *getter);

/** This structure is for each queriable parameter in an object
 *
 * -- param_name is the name of the parameter.
 * -- param_type is the type of the parameter, which can be either another
 *    object (QofIdType) or it can be a core data type (QofType).
 * -- param_getfcn is the function to actually obtain the parameter
 * -- param_setfcn is the function to actually set the parameter
 * -- param_userdata is a place where the object author can place any
 *    desired object-author-defined data (and thus can be used by the
 *    author-defined setter/getter).
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
    QofCompareFunc     param_compfcn;
    gpointer           param_userdata;
};

/** This function is the default sort function for a particular object type */
typedef int (*QofSortFunc)(gconstpointer, gconstpointer);

/** This function registers a new object class with the Qof subsystem.
 *  In particular, it registers the set of setters and getters for
 *  controlling the object.   The getters are typically used by the
 *  query subsystem to query type specific data.   Note that there
 *  is no particular requirement for there to be a setter for every
 *  getter or even vice-versa, nor is there any requirement for these
 *  to map 'cleanly' or orthogonally to the underlying object.  The
 *  parameters are really just a set of value setting and getting
 *  routines.
 *
 *  The "params" argument must be a NULL-terminated array of QofParam.
 *  It may be NULL if there are no parameters to be registered.
 */
void qof_class_register (QofIdTypeConst obj_name,
                         QofSortFunc default_sort_fcn,
                         const QofParam *params);

/** An example:
 *
 * #define MY_OBJ_MEMO     "memo"
 * #define MY_OBJ_VALUE    "value"
 * #define MY_OBJ_DATE     "date"
 * #define MY_OBJ_ACCOUNT  "account"
 * #define MY_OBJ_TRANS    "trans"
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

/** Type definition for the class callback function. */
typedef void (*QofClassForeachCB) (QofIdTypeConst, gpointer);

/** Call the callback once for each object class that is registered
 *  with the system.  The 'user_data' is passed back to the callback.
 */
void qof_class_foreach (QofClassForeachCB, gpointer user_data);

/** Type definition for the paramter callback function. */
typedef void (*QofParamForeachCB) (QofParam *, gpointer user_data);

/** Call the callback once for each parameter on the indicated
 *  object class.  The 'user_data' is passed back to the callback.
 */
void qof_class_param_foreach (QofIdTypeConst obj_name,
                              QofParamForeachCB, gpointer user_data);

/** \brief List of the parameters that could be references.

Simple check to return a GList of all parameters
of this object type that are not known QOF data types.
Used for partial QofBook support, see ::QofInstanceReference
*/
GList* qof_class_get_referenceList(QofIdTypeConst type);


#endif /* QOF_CLASS_H */
/** @} */
/** @} */
