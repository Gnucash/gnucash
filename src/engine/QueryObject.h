/*
 * QueryObject.h -- API for registering queriable Gnucash objects
 * Copyright (C) 2002 Derek Atkins <warlord@MIT.EDU>
 *
 */

#ifndef GNC_QUERYOBJECT_H
#define GNC_QUERYOBJECT_H

#include "QueryNew.h"

/* Define an arbitrary function pointer for access functions.  This is
 * because C doesn't have templates, so we just cast a lot.  Real
 * functions must be of the form:
 * 
 * <param_type> function (object_type *obj);
 */
typedef gpointer (*QueryAccess)(gpointer);

/* This structure is for each queriable parameter in an object
 *
 * -- param_name is the name of the parameter.
 * -- param_type is the type of the parameter, which can be either another
 *    object or it can be a core data type.
 * -- param_getgcn is the function to actually obtain the parameter
 */
typedef struct query_object_def {
  const char *	param_name;
  QueryCoreType	param_type;
  QueryAccess	param_getfcn;
} QueryObjectDef;

/* This function is the default sort function for a particular object type */
typedef int (*QuerySort)(gpointer, gpointer);

/* This function registers a new Gnucash Object with the QueryNew
 * subsystem.  In particular it registers the set of parameters and
 * converters to query the type-specific data.  Both "params" and
 * "converters" are NULL-terminated arrays of structures.  Either
 * argument may be NULL if there is nothing to be registered.
 */
void gncQueryObjectRegister (GNCIdTypeConst obj_name,
			     QuerySort default_sort_fcn,
			     const QueryObjectDef *params);

/* An example:
 *
 * #define MY_QUERY_OBJ_MEMO	"memo"
 * #define MY_QUERY_OBJ_VALUE	"value"
 * #define MY_QUERY_OBJ_DATE	"date"
 * #define MY_QUERY_OBJ_ACCOUNT "account"
 * #define MY_QUERY_OBJ_TRANS	"trans"
 *
 * static QueryObjectDef myQueryObjectParams[] = {
 * { MY_QUERY_OBJ_MEMO, QUERYCORE_STRING, myMemoGetter },
 * { MY_QUERY_OBJ_VALUE, QUERYCORE_NUMERIC, myValueGetter },
 * { MY_QUERY_OBJ_DATE, QUERYCORE_DATE, myDateGetter },
 * { MY_QUERY_OBJ_ACCOUNT, GNC_ID_ACCOUNT, myAccountGetter },
 * { MY_QUERY_OBJ_TRANS, GNC_ID_TRANS, myTransactionGetter },
 * NULL };
 *
 * gncQueryObjectRegisterParamters ("myObjectName", myQueryObjectCompare,
 *				    &myQueryObjectParams);
 */

/* Return the core datatype of the specified object's parameter */
QueryCoreType gncQueryObjectParameterType (GNCIdTypeConst obj_name,
					   const char *param_name);

/* Return the registered Object Definition for the requested parameter */
const QueryObjectDef * gncQueryObjectGetParameter (GNCIdTypeConst obj_name,
						   const char *parameter);

#endif /* GNC_QUERYOBJECT_H */
