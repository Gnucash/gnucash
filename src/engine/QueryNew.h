/********************************************************************\
 * QueryNew.h -- API for finding Gnucash objects                    *
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

/** @file QueryNew.h
    @breif API for finding Gnucash objects 
    @author Copyright (C) 2002 Derek Atkins <warlord@MIT.EDU>
*/


#ifndef GNC_QUERYNEW_H
#define GNC_QUERYNEW_H

#include "GNCId.h"
#include "QueryCore.h"
#include "gnc-book.h"

/** A Query */
typedef struct querynew_s QueryNew;

/** Query Term Operators, for combining Query Terms */
typedef enum {
  QUERY_FIRST_TERM=1,  /* First/only term is same as 'and' */
  QUERY_AND=1,
  QUERY_OR,
  QUERY_NAND,
  QUERY_NOR,
  QUERY_XOR
} QueryOp;

/** Default sort object type */
#define QUERY_DEFAULT_SORT	"GnucashQueryDefaultSortObject"

/** "Known" Object Parameters -- all objects must support these */
#define QUERY_PARAM_BOOK	"book"
#define QUERY_PARAM_GUID	"guid"
#define QUERY_PARAM_ACTIVE	"active" /* it's ok if an object does
					  * not support this */

/** Basic API Functions */

GSList * gncQueryBuildParamList (char const *param, ...);

/** Create a new query.  A Query MUST be set with a 'search-for' type.
 *  you can create and set this value in one step or two */
QueryNew * gncQueryCreate (void);
QueryNew * gncQueryCreateFor (GNCIdTypeConst obj_type);
void gncQueryDestroy (QueryNew *q);

/** Set the object type to be searched for */
void gncQuerySearchFor (QueryNew *query, GNCIdTypeConst obj_type);

/** Set the book to be searched (you can search multiple books) */
void gncQuerySetBook (QueryNew *q, GNCBook *book);


/** This is the general function that adds a new Query Term to a query.
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

void gncQueryAddBooleanMatch (QueryNew *q, GSList *param_list, gboolean value,
			      QueryOp op);

/** Run the query: */
GList * gncQueryRun (QueryNew *query);

/** Return the results of the last query, without re-running */
GList * gncQueryLastRun (QueryNew *query);

void gncQueryClear (QueryNew *query);
void gncQueryPurgeTerms (QueryNew *q, GSList *param_list);
int gncQueryHasTerms (QueryNew *q);
int gncQueryNumTerms (QueryNew *q);

gboolean gncQueryHasTermType (QueryNew *q, GSList *term_param);

QueryNew * gncQueryCopy (QueryNew *q);
QueryNew * gncQueryInvert(QueryNew *q);

/** Merges two queries together.  Both queries must be compatible
 * search-types.  If both queries are set, they must search for the
 * same object type.  If only one is set, the resulting query will
 * search for the set type.  If neither query has the search-type set,
 * the result will be unset as well.
 */
QueryNew * gncQueryMerge(QueryNew *q1, QueryNew *q2, QueryOp op);

/** Like gncQueryMerge, but this will merge q2 into q1.  q2 remains
 * unchanged.
 */
void gncQueryMergeInPlace(QueryNew *q1, QueryNew *q2, QueryOp op);

/** The lists become the property of the Query and will be freed
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

/** Compare two queries for equality. This is a simplistic
 * implementation -- logical equivalences between different
 * and/or trees are ignored. */
gboolean gncQueryEqual (QueryNew *q1, QueryNew *q2);

/* Print the Query in human-readable format.
 * Useful for debugging and development.
 */
void gncQueryPrint (QueryNew *query);

/* Return the type of data we're querying for */
GNCIdType gncQueryGetSearchFor (QueryNew *q);

/* Return the list of books we're using */
GList * gncQueryGetBooks (QueryNew *q);

#endif /* GNC_QUERYNEW_H */
