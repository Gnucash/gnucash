/*
 * QueryNew.h -- API for finding Gnucash objects
 * Copyright (C) 2002 Derek Atkins <warlord@MIT.EDU>
 *
 */

#ifndef GNC_QUERYNEW_H
#define GNC_QUERYNEW_H

#include "GNCId.h"
#include "QueryCore.h"

/* A Query */
typedef struct querynew_s QueryNew;

/* Query Term Operators, for combining Query Terms */
typedef enum {
  QUERY_AND=1,
  QUERY_OR,
  QUERY_NAND,
  QUERY_NOR,
  QUERY_XOR
} QueryOp;

/* Default sort object type */
#define QUERY_DEFAULT_SORT	"GnucashQueryDefaultSortObject"

/* "Known" Object Parameters -- all objects must support these */
#define QUERY_PARAM_BOOK	"book"
#define QUERY_PARAM_GUID	"guid"

/* Basic API Functions */

/* Create a new query.  A Query MUST be set with a 'search-for' type.
 * you can create and set this value in one step or two */
QueryNew * gncQueryCreate (void);
QueryNew * gncQueryCreateFor (GNCIdTypeConst obj_type);
void gncQueryDestroy (QueryNew *q);

/* Set the object type to be searched for */
void gncQuerySearchFor (QueryNew *query, GNCIdTypeConst obj_type);

/* Set the book to be searched (you can search multiple books) */
void gncQuerySetBook (QueryNew *q, GNCBook *book);


/* This is the general function that adds a new Query Term to a query.
 * It will find the 'obj_type' object of the search item and compare
 * the 'param_list' parameter to the predicate data via the comparitor.
 *
 * The param_list is a recursive list of parameters.  For example, you
 * can say 'split->memo' by creating a list of one element, "SPLIT_MEMO".
 * You can say 'split->account->name' by creating a list of two elements,
 * "SPLIT_ACCOUNT" and "ACCOUNT_NAME".  The list becomes the property of
 * the Query.
 *
 * For example:
 *
 * acct_name_pred_data = make_string_pred_data(STRING_MATCH_CASEINSENSITIVE,
 *					       account_name);
 * param_list = make_list (SPLIT_ACCOUNT, ACCOUNT_NAME, NULL);
 * gncQueryAddTerm (query, param_list, COMPARE_EQUAL,
 *		    acct_name_pred_data, QUERY_AND);
 */

void gncQueryAddTerm (QueryNew *query, GSList *param_list,
		      QueryPredData_t pred_data, QueryOp op);

void gncQueryAddGUIDMatch (QueryNew *q, GSList *param_list,
			   const GUID *guid, QueryOp op);
void gncQueryAddGUIDListMatch (QueryNew *q, GSList *param_list,
			       GList *guid_list, guid_match_t options,
			       QueryOp op);

/* Run the query: */
GList * gncQueryRun (QueryNew *query);

/* Return the results of the last query, without re-running */
GList * gncQueryLastRun (QueryNew *query);

void gncQueryClear (QueryNew *query);
void gncQueryPurgeTerms (QueryNew *q, GSList *param_list);
int gncQueryHasTerms (QueryNew *q);
int gncQueryNumTerms (QueryNew *q);

gboolean gncQueryHasTermType (QueryNew *q, GSList *term_param);

QueryNew * gncQueryCopy (QueryNew *q);
QueryNew * gncQueryInvert(QueryNew *q);

/* Merges two queries together.  Both queries must be compatible
 * search-types.  If both queries are set, they must search for the
 * same object type.  If only one is set, the resulting query will
 * search for the set type.  If neither query has the search-type set,
 * the result will be unset as well.
 */
QueryNew * gncQueryMerge(QueryNew *q1, QueryNew *q2, QueryOp op);

/* Like gncQueryMerge, but this will merge q2 into q1.  q2 remains
 * unchanged.
 */
void gncQueryMergeInPlace(QueryNew *q1, QueryNew *q2, QueryOp op);

/* The lists become the property of the Query and will be freed
 * by the query when it is destroyed.
 */
void gncQuerySetSortOrder (QueryNew *q,
			   GSList *primary_sort_params,
			   GSList *secondary_sort_params,
			   GSList *tertiary_sort_params);

void gncQuerySetSortOptions (QueryNew *q, gint prim_op, gint sec_op,
			     gint tert_op);

void gncQuerySetSortIncreasing (QueryNew *q, gboolean prim_inc,
				gboolean sec_inc, gboolean tert_inc);


void gncQuerySetMaxResults (QueryNew *q, int n);

/* compare two queries for equality. this is a simplistic
 * implementation -- logical equivalences between different
 * and/or trees are ignored. */
gboolean gncQueryEqual (QueryNew *q1, QueryNew *q2);

#endif /* GNC_QUERYNEW_H */
