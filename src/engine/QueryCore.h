/*
 * QueryCore.h -- API for providing core Query data types
 * Copyright (C) 2002 Derek Atkins <warlord@MIT.EDU>
 *
 */

#ifndef GNC_QUERYCORE_H
#define GNC_QUERYCORE_H

#include "QueryObject.h"	/* for QueryAccess */
#include "QueryNew.h"		/* for QueryPredData_t */
#include "gnc-numeric.h"
#include "date.h"
#include "kvp_frame.h"

#include <glib.h>

/* Head of Predicate Data structures.  All PData must start like this. */
typedef struct query_pred_data {
  const char *		type_name;
  query_compare_t	how;
} QueryPredDataDef;

/* 
 * An arbitrary Query Predicate.  Given the gnucash object and the
 * particular parameter get-function (obtained from the registry by
 * the Query internals), compare the object's parameter to the
 * predicate data
 */
typedef int (*QueryPredicate) (gpointer object,
			       QueryAccess get_fcn,
			       QueryPredData_t pdata);

/* A callback for how to destroy a query predicate's pdata */
typedef void (*QueryPredDataFree) (QueryPredData_t pdata);

/* A callback to copy a query's predicate data */
typedef QueryPredData_t (*QueryPredicateCopy) (QueryPredData_t pdata);

/* A callback for how to compare two (same-type) objects based on a
 * common get_fcn (parameter member), using the provided comparrison
 * options (which are the type-specific options).
 */
typedef int (*QueryCompare) (gpointer a, gpointer b,
                             gint compare_options,
			     QueryAccess get_fcn);


/* This function registers a new Core Object with the QueryNew
 * subsystem.  It maps the "core_name" object to the given
 * query_predicate and predicate_data_free functions.
 */
void gncQueryRegisterCoreObject (char const *type_name,
				 QueryPredicate pred,
				 QueryCompare comp,
				 QueryPredicateCopy copy,
				 QueryPredDataFree pd_free);


/* An example:
 *
 * gncQueryRegisterCoreObject (QUERYCORE_STRING, string_match_predicate,
 *			       string_compare_fcn, string_free_pdata);
 */


/* XXX: Define the core data type predicate_data structures here? */

/* Copy a predicate */
QueryPredData_t gncQueryCorePredicateCopy (QueryPredData_t pdata);

/* Destroy a type */
void gncQueryCorePredicateFree (QueryPredData_t pdata);

/* Core Data Type Predicates */
QueryPredData_t gncQueryStringPredicate (query_compare_t how, char *str,
					 string_match_t options,
					 gboolean is_regex);
QueryPredData_t gncQueryDatePredicate (query_compare_t how,
				       date_match_t options, Timespec date);
QueryPredData_t gncQueryNumericPredicate (query_compare_t how,
					  numeric_match_t options,
					  gnc_numeric value);
QueryPredData_t gncQueryGUIDPredicate (guid_match_t options, GList *guids);
QueryPredData_t gncQueryInt64Predicate (query_compare_t how, gint64 val);
QueryPredData_t gncQueryDoublePredicate (query_compare_t how, double val);
QueryPredData_t gncQueryBooleanPredicate (query_compare_t how, gboolean val);
QueryPredData_t gncQueryCharPredicate (char_match_t options,
				       const char *chars);
QueryPredData_t gncQueryKVPPredicate (query_compare_t how,
				      GSList *path, const kvp_value *value);

#endif /* GNC_QUERYCORE_H */
