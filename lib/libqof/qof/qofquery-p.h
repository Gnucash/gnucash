/********************************************************************\
 * qofquery-p.h -- internal/private API for finding objects         *
 * Copyright (C) 2002 Derek Atkins <warlord@MIT.EDU>                *
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

#ifndef QOF_QUERY_P_H
#define QOF_QUERY_P_H

#include "qofquery.h"

typedef struct _QofQueryTerm QofQueryTerm;
typedef struct _QofQuerySort QofQuerySort;

/* Functions to get Query information */
int qof_query_get_max_results (const QofQuery *q);


/* Functions to get and look at QueryTerms */

/* This returns a List of List of Query Terms.  Each list of Query
 * Terms are ANDed together, and each list of ANDed terms are ORed
 * together.  So, what is returned is the 'or' list of 'and' lists
 * of query term objects.
 *
 * Note that you should NOT modify this list in any way.  It belongs
 * to the query.
 */
GList * qof_query_get_terms (const QofQuery *q);

GSList * qof_query_term_get_param_path (const QofQueryTerm *queryterm);
QofQueryPredData *qof_query_term_get_pred_data (const QofQueryTerm *queryterm);
gboolean qof_query_term_is_inverted (const QofQueryTerm *queryterm);


/* Functions to get and look at QuerySorts */

/* This function returns the primary, secondary, and tertiary sorts.
 * These are part of the query and should NOT be changed!
 */
void qof_query_get_sorts (QofQuery *q, QofQuerySort **primary,
                       QofQuerySort **secondary, QofQuerySort **tertiary);

GSList * qof_query_sort_get_param_path (const QofQuerySort *querysort);
gint qof_query_sort_get_sort_options (const QofQuerySort *querysort);
gboolean qof_query_sort_get_increasing (const QofQuerySort *querysort);

#endif /* QOF_QUERY_P_H */
