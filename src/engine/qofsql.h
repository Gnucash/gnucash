/********************************************************************\
 * qofsql.h -- QOF cleint-side SQL parser                           *
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

/**
    @file qofsql.h
    @breif QOF client-side SQL parser.
    @author Copyright (C) 2004 Linas Vepstas <linas@linas.org>
*/

#ifndef QOF_SQL_QUERY_H
#define QOF_SQL_QUERY_H

#include <glib.h>
#include "qofbook.h"

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
 *
 *  The returned list is a list of the 'search-for' type that was
 *  previously set with the qof_query_search_for() or the
 *  XXX fixme this doc is wrong.
 *  qof_query_create_for() routines.  The returned list will have
 *  been sorted using the indicated sort order, and trimed to the
 *  max_results length.
 *  Do NOT free the resulting list.  This list is managed internally
 *  by QofSqlQuery.
 */

GList * qof_sql_query_run (QofSqlQuery *query, const char * str);

#endif /* QOF_SQL_QUERY_H */
