/********************************************************************\
 * qofsql.h -- QOF client-side SQL parser                           *
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

/** @addtogroup Engine
    @{ */
/**
    @file qofsql.h
    @breif QOF client-side SQL parser.
    @author Copyright (C) 2004 Linas Vepstas <linas@linas.org>
*/

#ifndef QOF_SQL_QUERY_H
#define QOF_SQL_QUERY_H

#include <glib.h>
#include <qof/kvp_frame.h>
#include <qof/qofbook.h>
#include <qof/qofquery.h>

typedef struct _QofSqlQuery QofSqlQuery;

/** Create a new SQL-syntax query machine.
 */
QofSqlQuery * qof_sql_query_new (void);
void qof_sql_query_destroy (QofSqlQuery *);

/** Set the book to be searched (you can search multiple books)
 *  If no books are set, no results will be returned (since there
 *  is nothing to search over).
 */
void qof_sql_query_set_book (QofSqlQuery *q, QofBook *book);

/** Perform the query, return the results.
 *  The book must be set in order to be able to perform a query.
 *
 *  The returned list is a list of ... See below ... 
 *  The returned list will have been sorted using the indicated sort 
 *  order, (by default ascending order) and trimed to the
 *  max_results length.
 *  Do NOT free the resulting list.  This list is managed internally
 *  by QofSqlQuery.
 *
 * The types of SQL queries that are allowed at this point are very 
 * limited.  In general, only the following types of queries are 
 * supported:
 *   SELECT * FROM SomeObj WHERE (param_a < 10.0) AND (param_b = "asdf")
 *          SORT BY param_c DESC;
 * The returned list is a list of all of the instances of 'SomeObj' that
 * mathc the query.   The 'SORT' term is optional. The 'WHERE' term is
 * optional; but if you don't include 'WHERE', you will get a list of 
 * all of the object instances.  The Boolean operations 'AND' and 'OR'
 * together with parenthesis can be used to construct arbitrarily 
 * nested predicates.
 *
 * If the param is a KVP frame, then we use a special markup to 
 * indicate frame values.  The markup should look like 
 * /some/kvp/path:value. Thus, for example,
 *   SELECT * FROM SomeObj WHERE (param_a < '/some/kvp:10.0')
 * will search for the object where param_a is a KVP frame, and this
 * KVP frame contains a path '/some/kvp' and the value stored at that
 * path is floating-point and that float value is less than 10.0.
 *
 * The following are types of queries are NOT supported:
 *   SELECT a,b,c FROM ...
 * I am thinking of implementing the above as a list of KVP's
 * whose keys would be a,b,c and values would be the results of the
 * search.
 *
 * Also unsupported are joins:
 *   SELECT * FROM ObjA,ObjB WHERE (ObjA.thingy = ObjB.Stuff);
 * The problem with the above is that the search requires a nested
 * search loop, aka a 'join', which is not currently supported in the
 * underlying QofQuery code.
 *
 * Also unsupported:  UPDATE and INSERT. 
 */

GList * qof_sql_query_run (QofSqlQuery *query, const char * str);

/** Same as above, but just parse/pre-process the query; do
 *  not actually run it over the dataset.  The QofBook does not
 *  need to be set before calling this function.
 */
void qof_sql_query_parse (QofSqlQuery *query, const char * str);

/** Return the QofQuery form of the previously parsed query. */
QofQuery * qof_sql_query_get_query (QofSqlQuery *);

/** Run the previously parsed query.  The QofBook must be set 
 *  before this function can be called.  Note, teh QofBook can
 *  be changed between each successive call to this routine.
 *  This routine can be called after either qof_sql_query_parse()
 *  or qof_sql_query_run() because both will set up the parse.
 */
GList * qof_sql_query_rerun (QofSqlQuery *query);

/** 
 * Set the kvp frame to be used for formulating 'indirect' predicates.
 *
 * Although joins are not supported (see above), there is one special
 * hack that one can use to pass data indirectly into the predicates.
 * This is by using a KVP key name to reference the value to be used
 * for a predicate.  Thus, for example, 
 *   SELECT * FROM SomeObj WHERE (param_a = KVP:/some/key/path);
 * will look up the value stored at '/some/key/path', and use that
 * value to form the actual predicate.   So, for example, if 
 * the value stored at '/some/key/path' was 2, then the actual 
 * query run will be 
 *   SELECT * FROM SomeObj WHERE (param_a = 2);
 * The lookup occurs at the time that the query is formulated.
 *
 * The query does *not* take over ownership of the kvp frame,
 * nor does it copy it. Thus, the kvp frame must exist when the
 * query is formulated, and it is the responsibility of the 
 * caller to free it when no longer needed.
 *
 * Note that because this feature is a kind of a hack put in place
 * due to the lack of support for joins, it will probably go away
 * (be deprecated) if/when joins are implemented. 
 */
void qof_sql_query_set_kvp (QofSqlQuery *, KvpFrame *);

#endif /* QOF_SQL_QUERY_H */
/** @} */
