/*
 * QueryCore.h -- API for providing core Query data types
 * Copyright (C) 2002 Derek Atkins <warlord@MIT.EDU>
 *
 */

#ifndef GNC_QUERYCORE_H
#define GNC_QUERYCORE_H

#include "gnc-numeric.h"
#include "date.h"
#include "kvp_frame.h"

#include <glib.h>

typedef struct query_pred_data *QueryPredData_t;

/* Type of Query Core Objects (String, Date, Numeric, GUID, etc. */
typedef const char * QueryCoreType;

/* Standard Query comparitors, for how to compare objects in a predicate.
 * Note that not all core types implement all comparitors
 */
typedef enum {
  COMPARE_LT = 1,
  COMPARE_LTE,
  COMPARE_EQUAL,
  COMPARE_GT,
  COMPARE_GTE,
  COMPARE_NEQ
} query_compare_t;

/*
 * List of known core query data-types... 
 * Each core query type defines it's set of optional "comparitor qualifiers".
 */
#define QUERYCORE_STRING	"string"
typedef enum {
  STRING_MATCH_NORMAL = 1,
  STRING_MATCH_CASEINSENSITIVE
} string_match_t;

#define QUERYCORE_DATE		"date"
typedef enum {
  DATE_MATCH_NORMAL = 1,
  DATE_MATCH_ROUNDED
} date_match_t;

#define QUERYCORE_NUMERIC	"numeric"
#define QUERYCORE_DEBCRED	"debcred"
typedef enum {
  NUMERIC_MATCH_DEBIT = 1,
  NUMERIC_MATCH_CREDIT,
  NUMERIC_MATCH_ANY
} numeric_match_t;

#define QUERYCORE_GUID		"guid"
typedef enum {
  GUID_MATCH_ANY = 1,
  GUID_MATCH_ALL,		/* You _must_ pass a GList of objects! */
  GUID_MATCH_NONE,
  GUID_MATCH_NULL,
} guid_match_t;

#define QUERYCORE_INT64		"gint64"
#define QUERYCORE_DOUBLE	"double"
#define QUERYCORE_BOOLEAN	"boolean"
#define QUERYCORE_KVP		"kvp"

/* A CHAR type is for a RECNCell */
#define QUERYCORE_CHAR		"character"
typedef enum {
  CHAR_MATCH_ANY = 1,
  CHAR_MATCH_NONE
} char_match_t;

/* Head of Predicate Data structures.  All PData must start like this. */
typedef struct query_pred_data {
  QueryCoreType		type_name;	/* QUERYCORE_* */
  query_compare_t	how;
} QueryPredDataDef;

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

#include "QueryObject.h"	/* for QueryAccess */

/* Copy a predicate */
QueryPredData_t gncQueryCorePredicateCopy (QueryPredData_t pdata);

/* Destroy a predicate */
void gncQueryCorePredicateFree (QueryPredData_t pdata);

/* Return a printable string for a core data object.  Caller needs
 * to g_free() the returned string
 */
char * gncQueryCoreToString (char const *type, gpointer object,
			     QueryAccess fcn);

#endif /* GNC_QUERYCORE_H */
