/*
 * QueryCoreP.h -- Internal API for providing core Query data types
 * Copyright (C) 2002 Derek Atkins <warlord@MIT.EDU>
 *
 */

#ifndef GNC_QUERYCOREP_H
#define GNC_QUERYCOREP_H

#include <sys/types.h>
#include <time.h>
#include <glib.h>
#include <regex.h>
#include <string.h>

#include "QueryCore.h"

/* Initalize the Query Core registry and install the default type handlers */
void gncQueryCoreInit(void);
void gncQueryCoreShutdown (void);

/* 
 * An arbitrary Query Predicate.  Given the gnucash object and the
 * particular parameter get-function (obtained from the registry by
 * the Query internals), compare the object's parameter to the
 * predicate data
 */
typedef int (*QueryPredicate) (gpointer object,
			       QueryAccess get_fcn,
			       QueryPredData_t pdata);

/* A callback for how to compare two (same-type) objects based on a
 * common get_fcn (parameter member), using the provided comparrison
 * options (which are the type-specific options).
 */
typedef int (*QueryCompare) (gpointer a, gpointer b,
                             gint compare_options,
			     QueryAccess get_fcn);

/* Lookup functions */
QueryPredicate gncQueryCoreGetPredicate (char const *type);
QueryCompare gncQueryCoreGetCompare (char const *type);

/* Compare two predicates */
gboolean gncQueryCorePredicateEqual (QueryPredData_t p1, QueryPredData_t p2);

/* Predicate Data Structures:
 *
 * These are defined such that you can cast between these types and
 * a QueryPredData_t.
 *
 * Note that these are provided for READ ONLY PURPOSES.  You should NEVER
 * write into these structures, change them, or use them to create a
 * Query.
 */

typedef struct {
  QueryPredDataDef	pd;
  string_match_t	options;
  gboolean		is_regex;
  char *		matchstring;
  regex_t		compiled;
} query_string_def, *query_string_t;

typedef struct {
  QueryPredDataDef	pd;
  date_match_t	options;
  Timespec	date;
} query_date_def, *query_date_t;

typedef struct {
  QueryPredDataDef	pd;
  numeric_match_t	options;
  gnc_numeric		amount;
} query_numeric_def, *query_numeric_t;

typedef struct {
  QueryPredDataDef	pd;
  guid_match_t	options;
  GList *	guids;
} query_guid_def, *query_guid_t;

typedef struct {
  QueryPredDataDef	pd;
  gint64	val;
} query_int64_def, *query_int64_t;

typedef struct {
  QueryPredDataDef	pd;
  double	val;
} query_double_def, *query_double_t;

typedef struct {
  QueryPredDataDef	pd;
  gboolean	val;
} query_boolean_def, *query_boolean_t;

typedef struct {
  QueryPredDataDef	pd;
  char_match_t	options;
  char *	char_list;
} query_char_def, *query_char_t;

typedef struct {
  QueryPredDataDef	pd;
  GSList *	path;
  kvp_value *	value;
} query_kvp_def, *query_kvp_t;

#endif /* GNC_QUERYCOREP_H */
