/********************************************************************\
 * qofsql.h -- QOF client-side SQL parser using libgda              *
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
@{ */
/**
    @file qofsql.h
    @brief QOF client-side SQL parser, interfacing with libgda.
    @author Copyright (C) 2004 Linas Vepstas <linas@linas.org>
*/

#ifndef QOF_SQL_QUERY_H
#define QOF_SQL_QUERY_H

#include "kvp_frame.h"
#include "qofbook.h"
#include "qofquery.h"

/** @addtogroup SQL SQL Interface to Query

The types of SQL queries that are allowed at this point are very 
limited.  In general, only the following types of queries are 
supported:
  SELECT * FROM SomeObj WHERE (param_a < 10.0) AND (param_b = "asdf")
         SORT BY param_c DESC;
  INSERT INTO SomeObj (param_a, param_b, param_c) VALUES
        ("value_a", true, "0/1");

For SELECT, the returned list is a list of all of the instances of 'SomeObj' that
match the query.  The 'SORT' term is optional. The 'WHERE' term is
optional; but if you don't include 'WHERE', you will get a list of 
all of the object instances.  The Boolean operations 'AND' and 'OR'
together with parenthesis can be used to construct arbitrarily 
nested predicates.

For INSERT, the returned list is a list containing the newly created instance
of 'SomeObj'.

Joins are not supported directly.
  SELECT * FROM ObjA,ObjB WHERE (ObjA.param_id = ObjB.param_other_id);
The problem with the above is that the search requires a nested
search loop, aka a 'join', which is not currently supported in the
underlying QofQuery code.

However, by repeating queries and adding the entities to a new session using
::qof_instance_copy_list, a series of queries can be added to a single
book. e.g. You can insert multiple entities and save out as a QSF XML
file or use multiple SELECT queries to build a precise list - this
can be used to replicate most of the functionality of a SQL join.

SELECT * from ObjA where param_id = value;
SELECT * from ObjB where param_other_id = value;

Equivalent to:
SELECT * from ObjA,ObjB where param_id = param_other_id and param_id = value;

When combined with a foreach callback on the value of param_id for each
entity in the QofBook, you can produce the effect of a join from running
the two SELECT queries for each value of param_id held in 'value'.

See ::QofInstanceForeachCB and ::qof_object_foreach.

Date queries handle full date and time strings, using the format
exported by the QSF backend. To query dates and times, convert
user input into UTC time using the ::QOF_UTC_DATE_FORMAT string.
e.g. set the UTC date format and call ::qof_print_time_buff
with a time_t obtained via ::timespecToTime_t.

If the param is a KVP frame, then we use a special markup to 
indicate frame values.  The markup should look like 
/some/kvp/path:value. Thus, for example,
  SELECT * FROM SomeObj WHERE (param_a < '/some/kvp:10.0')
will search for the object where param_a is a KVP frame, and this
KVP frame contains a path '/some/kvp' and the value stored at that
path is floating-point and that float value is less than 10.0.

The following are types of queries are NOT supported:
  SELECT a,b,c FROM ...
I am thinking of implementing the above as a list of KVP's
whose keys would be a,b,c and values would be the results of the
search. (Linas)

XXX (Neil W). Alternatively, I need to use something like this
when converting QOF objects between applications by using the
returned parameter values to create a second object. One application
using QOF could register objects from two applications and convert
data from one to the other by using SELECT a,b,c FROM ObjA;
SELECT d,f,k FROM ObjB; qof_object_new_instance(); ObjC_set_a(value_c);
ObjC_set_b(value_k) etc. What's needed is for the SELECT to return
a complete object that only contains the parameters selected.

 Also unsupported:  UPDATE. 
 
Certain SQL commands can have no QOF equivalent and will
generate a runtime parser error:
 - ALTER
 - CREATE
 - DROP
 - FLUSH
 - GRANT
 - KILL
 - LOCK
 - OPTIMIZE
 - REVOKE
 - USE

  @{ */
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

/** \brief Perform the query, return the results.

 *  The book must be set in order to be able to perform a query.
 *
 *  The returned list will have been sorted using the indicated sort 
 *  order, (by default ascending order) and trimmed to the
 *  max_results length.
 *  Do NOT free the resulting list.  This list is managed internally
 *  by QofSqlQuery.
 *
 */

GList * qof_sql_query_run (QofSqlQuery *query, const gchar * str);

/** Same ::qof_sql_query_run, but just parse/pre-process the query; do
  not actually run it over the dataset.  The QofBook does not
  need to be set before calling this function.
*/

void qof_sql_query_parse (QofSqlQuery *query, const gchar * str);

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

/** @} */
/** @} */
#endif /* QOF_SQL_QUERY_H */
