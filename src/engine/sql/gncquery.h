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
 * SELECT * FROM gncEntry WHERE 
 * (entryguid='deadbeef') OR
 * (memo='deposit' AND date_reconciled>'1998-07-01 11:00:00.345678 -0500')
 *
 * HISTORY:
 * Linas Vepstas January 2001
 */


#ifndef __GNC_QUERY_H__
#define __GNC_QUERY_H__

#include "Query.h"

typedef struct _gnc_query sqlQuery;

sqlQuery *sqlQuery_new(void);
void sql_Query_destroy (sqlQuery *);

/* convert a gnc query to an sql query */
const char *sqlQuery_build (sqlQuery *, Query *);


#endif  /* __GNC_QUERY_H__ */
