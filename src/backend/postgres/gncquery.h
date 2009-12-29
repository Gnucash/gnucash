/********************************************************************\
 * gncquery.h : Convert gnucash engine Query into an SQL Query      *
 * Copyright (C) 2001 Linas Vepstas <linas@linas.org>               *
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
\********************************************************************/

/*
 * FILE:
 * gncquery.h
 *
 * FUNCTION:
 * Convert gnucash engine Query (a la Query.h) into an SQL Query
 *
 * The gnc engine query consists of doubly nested list of
 * query terms.  The inner list consists of terms that need to be
 * AND'ed together; the outer list OR's together the inner lists.
 *
 * The resulting query will resemble
 * SELECT * FROM gncSplit WHERE
 * (splitguid='deadbeef') OR
 * (memo='deposit' AND date_reconciled>'1998-07-01 11:00:00.345678 -0500')
 *
 * HISTORY:
 * Linas Vepstas January 2001
 */


#ifndef GNC_QUERY_H
#define GNC_QUERY_H

#include "Query.h"
#include "gnc-engine.h"

typedef struct _gnc_query sqlQuery;

sqlQuery *sqlQuery_new(void);
void sql_Query_destroy (sqlQuery *);

/* convert a gnc query to an sql query */
const char *sqlQuery_build (sqlQuery *, Query *);


#endif  /* GNC_QUERY_H */
