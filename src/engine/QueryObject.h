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
typedef void (*QueryAccess)(gpointer);

/* This structure is for each queriable parameter in an object */
typedef struct query_object_def {
  const char *	param_name;
  QueryCoreType	param_type;
  QueryAccess	param_getfcn;
} QueryObjectDef;

/* This function-type will convert from one object-type to another */
typedef gpointer (*QueryConvert)(gpointer);

typedef struct query_convert_def {
  GNCIdType 	desired_object_name;
  QueryConvert	object_getfcn;
} QueryConvertDef;

/* This function is the default sort function for a particular object type */
typedef int (*QuerySort)(gpointer, gpointer);

/* This function registers a new Gnucash Object with the QueryNew
 * subsystem.  In particular it registers the set of parameters and
 * converters to query the type-specific data.  Both "params" and
 * "converters" are NULL-terminated arrays of structures.  Either
 * argument may be NULL if there is nothing to be registered.
 */
void gncQueryObjectRegister (GNCIdType  obj_name,
			     QuerySort default_sort_fcn,
			     const QueryObjectDef *params,
			     const QueryConvertDef *converters);

/* An example:
 *
 * #define MY_QUERY_OBJ_MEMO	"memo"
 * #define MY_QUERY_OBJ_VALUE	"value"
 * #define MY_QUERY_OBJ_DATE	"date"
 *
 * static QueryObjectDef myQueryObjectParams[] = {
 * { MY_QUERY_OBJ_MEMO, QUERYCORE_STRING, myMemoGetter },
 * { MY_QUERY_OBJ_VALUE, QUERYCORE_NUMERIC, myValueGetter },
 * { MY_QUERY_OBJ_DATE, QUERYCORE_DATE, myDateGetter },
 * NULL };
 *
 * static QueryConvertDef myQueryObjectsConvs[] = {
 * { GNC_ID_ACCOUNT, myAccountGetter },
 * { GNC_ID_TRANS, myTransactionGetter },
 * NULL };
 *
 * gncQueryObjectRegisterParamters ("myObjectName", &myQueryObjectParams,
 *				    &myQueryObjectConvs);
 */

/* Return the core datatype of the specified object's parameter */
QueryCoreType gncQueryObjectParameterType (GNCIdType obj_name,
					   const char *param_name);

#endif /* GNC_QUERYOBJECT_H */
