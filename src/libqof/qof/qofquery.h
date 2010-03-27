/********************************************************************\
 * qofquery.h -- find objects that match a certain expression.      *
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

/** @addtogroup Query

BASIC QUERY API:
With this API you can create arbitrary logical
queries to find sets of arbitrary object.  To make simple
queries (1 term, such as a search for a parameter with one value),
create the appropriate
QueryTerm structure and stick it in a Query object using
xaccInitQuery. The QueryTerm should be malloced but the Query object
will handle freeing it.  To make compound queries, make multiple
simple queries and combine them using qof_query_merge() and the logical
operations of your choice.

SQL QUERY API:
As an alternative to building queries one predicate at a time,
you can use the SQL query interface.  This interface will accept
a string containing an SQL query, parse it, convert it into the
core representation, and execute it.

STRUCTURE OF A QUERY: A Query is a logical function of any number of
QueryTerms.  A QueryTerm consists of a C function pointer (the
Predicate) and a PredicateData structure containing data passed to the
predicate funtion.  The PredicateData structure is a constant
associated with the Term and is identical for every object that is
tested.

The terms of the Query may represent any logical function and are
stored in canonical form, i.e. the function is expressed as a logical
sum of logical products.  So if you have QueryTerms a, b, c, d, e and
you have the logical function a(b+c) + !(c(d+e)), it gets stored as
ab + ac + !c + !c!e +!d!c + !d!e.  This may not be optimal for evaluation
of some functions but it's easy to store, easy to manipulate, and it
doesn't require a complete algebra system to deal with.

The representation is of a GList of GLists of QueryTerms.  The
"backbone" GList q->terms represents the OR-chain, and every item on
the backbone is a GList of QueryTerms representing an AND-chain
corresponding to a single product-term in the canonical
representation.  QueryTerms are duplicated when necessary to fill out
the canonical form, and the same predicate may be evaluated multiple
times per split for complex queries.  This is a place where we could
probably optimize.

 @{ */
/** @file qofquery.h
    @brief find objects that match a certain expression.
    @author Copyright (C) 2002 Derek Atkins <warlord@MIT.EDU>
    @author Copyright (C) 2003 Linas Vepstas <linas@linas.org>

*/

#ifndef QOF_QUERYNEW_H
#define QOF_QUERYNEW_H

#include "guid.h"
#include "qofbook.h"
#include "qofquerycore.h"
#include "qofchoice.h"

#define QOF_MOD_QUERY "qof.query"

/** A Query */
typedef struct _QofQuery QofQuery;

/** Query Term Operators, for combining Query Terms */
typedef enum
{
    QOF_QUERY_AND = 1,
    QOF_QUERY_OR,
    QOF_QUERY_NAND,
    QOF_QUERY_NOR,
    QOF_QUERY_XOR
} QofQueryOp;

/** First/only term is same as 'and' */
#define QOF_QUERY_FIRST_TERM QOF_QUERY_AND

/** Default sort object type */
#define QUERY_DEFAULT_SORT      "QofQueryDefaultSort"

/** "Known" Object Parameters -- all objects must support these */
#define QOF_PARAM_BOOK    "book"
#define QOF_PARAM_GUID    "guid"

/** "Known" Object Parameters -- some objects might support these */
#define QOF_PARAM_KVP     "kvp"
#define QOF_PARAM_ACTIVE  "active"
#define QOF_PARAM_VERSION "version"

/* --------------------------------------------------------- */
/** \name Query Subsystem Initialization and Shudown  */
// @{
/** Subsystem initialization and shutdown. Call init() once
 *  to initalize the query subsystem; call shutdown() to free
 *  up any resources associated with the query subsystem.
 *  Typically called during application startup, shutdown.
 */

void qof_query_init (void);
void qof_query_shutdown (void);
// @}

/* --------------------------------------------------------- */
/** \name Low-Level API Functions */
// @{

GSList * qof_query_build_param_list (char const *param, ...);

/** Create a new query.
 *  Before running the query, a 'search-for' type must be set
 *  otherwise nothing will be returned.  The results of the query
 *  is a list of the indicated search-for type.
 *
 *  Allocates and initializes a Query structure which must be
 *  freed by the user with qof_query_destroy().  A newly-allocated
 *  QofQuery object matches nothing (qof_query_run() will return NULL).
 */
QofQuery * qof_query_create (void);
QofQuery * qof_query_create_for (QofIdTypeConst obj_type);

/** Frees the resources associate with a Query object.  */
void qof_query_destroy (QofQuery *q);

/** Set the object type to be searched for.  The results of
 *  performing the query will be a list of this obj_type.
 */
void qof_query_search_for (QofQuery *query, QofIdTypeConst obj_type);

/** Set the book to be searched.  Books contain/identify collections
 *  of objects; the search will be performed over those books
 *  specified with this function.  If no books are set, no results
 *  will be returned (since there is nothing to search over).
 *
 *  You can search multiple books.  To specify multiple books, call
 *  this function multiple times with different arguments.
 * XXX needed qof_query_clear_books() to reset the list ...
 */
void qof_query_set_book (QofQuery *q, QofBook *book);


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
 * acct_name_pred_data = make_string_pred_data(QOF_STRING_MATCH_CASEINSENSITIVE,
 *                                          account_name);
 * param_list = make_list (SPLIT_ACCOUNT, ACCOUNT_NAME, NULL);
 * qof_query_add_term (query, param_list, QOF_COMPARE_EQUAL,
 *                    acct_name_pred_data, QOF_QUERY_AND);
 *
 * Please note that QofQuery does not, at this time, support joins.
 * That is, one cannot specify a predicate that is a parameter list.
 * Put another way, one cannot search for objects where
 *   obja->thingy == objb->stuff
 */

void qof_query_add_term (QofQuery *query, GSList *param_list,
                         QofQueryPredData *pred_data, QofQueryOp op);

/** DOCUMENT ME !! */
void qof_query_add_guid_match (QofQuery *q, GSList *param_list,
                               const GncGUID *guid, QofQueryOp op);
/** DOCUMENT ME !! */
void qof_query_add_guid_list_match (QofQuery *q, GSList *param_list,
                                    GList *guid_list, QofGuidMatch options,
                                    QofQueryOp op);

/** Handy-dandy convenience routines, avoids having to create
 * a separate predicate for boolean matches.  We might want to
 * create handy-dandy sugar routines for the other predicate types
 * as well. */
void qof_query_add_boolean_match (QofQuery *q,
                                  GSList *param_list,
                                  gboolean value,
                                  QofQueryOp op);

/** Perform the query, return the results.
 *  The returned list is a list of the 'search-for' type that was
 *  previously set with the qof_query_search_for() or the
 *  qof_query_create_for() routines.  The returned list will have
 *  been sorted using the indicated sort order, and trimed to the
 *  max_results length.
 *
 *  Do NOT free the resulting list.  This list is managed internally
 *  by QofQuery.
 */
GList * qof_query_run (QofQuery *query);

/** Return the results of the last query, without causing the query to
 *  be re-run.  Do NOT free the resulting list.  This list is managed
 *  internally by QofQuery.
 */
GList * qof_query_last_run (QofQuery *query);

/** Perform a subquery, return the results.
 *  Instead of running over a book, the subquery runs over the results
 *  of the primary query.
 *
 *  Do NOT free the resulting list.  This list is managed internally
 *  by QofQuery.
 */
GList * qof_query_run_subquery (QofQuery *subquery,
                                const QofQuery* primary_query);

/** Remove all query terms from query.  query matches nothing
 *  after qof_query_clear().
 */
void qof_query_clear (QofQuery *query);

/** Remove query terms of a particular type from q.  The "type" of a term
 *  is determined by the type of data that gets passed to the predicate
 *  function.
 * XXX ??? Huh? remove anything of that predicate type, or just
 * the particular predicate ?
 */
void qof_query_purge_terms (QofQuery *q, GSList *param_list);

/** Return boolean FALSE if there are no terms in the query
 *  Can be used as a predicate to see if the query has been
 *  initialized (return value > 0) or is "blank" (return value == 0).
 */
int qof_query_has_terms (QofQuery *q);

/** Return the number of terms in the canonical form of the query.
 */
int qof_query_num_terms (QofQuery *q);

/** DOCUMENT ME !! */
gboolean qof_query_has_term_type (QofQuery *q, GSList *term_param);
GSList * qof_query_get_term_type (QofQuery *q, GSList *term_param);

/** Make a copy of the indicated query */
QofQuery * qof_query_copy (QofQuery *q);

/** Make a copy of the indicated query, inverting the sense
 *  of the search.  In other words, if the original query search
 *  for all objects with a certain condition, the inverted query
 *  will search for all object with NOT that condition.  The union
 *  of the results returned by the original and inverted queries
 *  equals the set of all searched objects. These to sets are
 *  disjoint (share no members in common).
 *
 *  This will return a newly allocated QofQuery object, or NULL
 *  on error. Free it with qof_query_destroy() when no longer needed.
 */
QofQuery * qof_query_invert(QofQuery *q);

/** Combine two queries together using the Boolean set (logical)
 *  operator 'op'.  For example, if the operator 'op' is set to
 *  QUERY_AND, then the set of results returned by the query will
 *  will be the Boolean set intersection of the results returned
 *  by q1 and q2.  Similarly,  QUERY_OR maps to set union, etc.
 *
 *  Both queries must have compatible
 *  search-types.  If both queries are set, they must search for the
 *  same object type.  If only one is set, the resulting query will
 *  search for the set type.  If neither query has the search-type set,
 *  the result will be unset as well.
 *
 *  This will return a newly allocated QofQuery object, or NULL on
 *  error. Free it with qof_query_destroy() when no longer needed.
 *  Note that if either input query is NULL then the returned query is
 *  NOT newly allocated -- it will return the non-NULL query.  You
 *  only need to call this function when both q1 and q2 are non-NULL.
 */
QofQuery * qof_query_merge(QofQuery *q1, QofQuery *q2, QofQueryOp op);

/** Like qof_query_merge, but this will merge a copy of q2 into q1.
 *   q2 remains unchanged.
 */
void qof_query_merge_in_place(QofQuery *q1, QofQuery *q2, QofQueryOp op);

/**
 * When a query is run, the results are sorted before being returned.
 * This routine can be used to set the paramters on which the sort will
 * be performed.  Two objects in the result list will be compared using
 * the 'primary_sort_params', and sorted based on that order.  If the
 * comparison shows that they are equal, then the
 * 'secondary_sort_params' will be used.  If still equal, then the
 * tertiary params will be compared.  Any or all of these parameter
 * lists may be NULL.  Any of these parameter lists may be set to
 * QUERY_DEFAULT_SORT.
 *
 * Note that if there are more results than the 'max-results' value,
 * then only the *last* max-results will be returned.  For example,
 * if the sort is set to be increasing date order, then only the
 * objects with the most recent dates will be returned.
 *
 * The input lists become the property of QofQuery and are managed
 * by it.   They will be freed when the query is destroyed (or when
 * new lists are set).
 */
void qof_query_set_sort_order (QofQuery *q,
                               GSList *primary_sort_params,
                               GSList *secondary_sort_params,
                               GSList *tertiary_sort_params);

void qof_query_set_sort_options (QofQuery *q, gint prim_op, gint sec_op,
                                 gint tert_op);

/**
 * When a query is run, the results are sorted before being returned.
 * This routine can be used to control the direction of the ordering.
 * A value of TRUE indicates the sort will be in increasing order,
 * a value of FALSE will order results in decreasing order.
 *
 * Note that if there are more results than the 'max-results' value,
 * then only the *last* max-results will be returned.  For example,
 * if the sort is set to be increasing date order, then only the
 * objects with the most recent dates will be returned.
 */
void qof_query_set_sort_increasing (QofQuery *q, gboolean prim_inc,
                                    gboolean sec_inc, gboolean tert_inc);


/**
 * Set the maximum number of results that should be returned.
 * If 'max-results' is set to -1, then all of the results are
 * returned.  If there are more results than 'max-results',
 * then the result list is trimmed.  Note that there is an
 * important interplay between 'max-results' and the sort order:
 * only the last bit of results are returned.  For example,
 * if the sort order is set to be increasing date order, then
 * only the objects with the most recent dates will be returned.
 */
void qof_query_set_max_results (QofQuery *q, int n);

/** Compare two queries for equality.
 * Query terms are compared each to each.
 * This is a simplistic
 * implementation -- logical equivalences between different
 * and/or trees are ignored.
 */
gboolean qof_query_equal (const QofQuery *q1, const QofQuery *q2);

/** Log the Query
 *
 * \deprecated Do not call directly, use the standard log
 * module code: ::qof_log_set_level(QOF_MOD_QUERY, QOF_LOG_DEBUG);
 * or ::qof_log_set_default(QOF_LOG_DEBUG);
 */
void qof_query_print (QofQuery *query);

/** Return the type of data we're querying for */
/*@ dependent @*/
QofIdType qof_query_get_search_for (const QofQuery *q);

/** Return the list of books we're using */
GList * qof_query_get_books (QofQuery *q);

// @}
/* @} */
#endif /* QOF_QUERYNEW_H */
