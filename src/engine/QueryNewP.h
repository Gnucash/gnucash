/********************************************************************\
 * QueryNewP.h -- internal/private API for finding Gnucash objects  *
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#ifndef GNC_QUERYNEWP_H
#define GNC_QUERYNEWP_H

#include "QueryNew.h"

typedef struct query_new_term *QueryNewTerm_t;
typedef struct query_new_sort *QueryNewSort_t;

/* Initialize/Shutdown */
void gncQueryNewInit (void);
void gncQueryNewShutdown (void);

/* Functions to get Query information */
int gncQueryGetMaxResults (QueryNew *q);


/* Functions to get and look at QueryTerms */

/* This returns a List of List of Query Terms.  Each list of Query
 * Terms are ANDed together, and each list of ANDed terms are ORed
 * together.  So, what is returned is the 'or' list of 'and' lists
 * of query term objects.
 *
 * Note that you should NOT modify this list in any way.  It belongs
 * to the query.
 */
GList * gncQueryGetTerms (QueryNew *q);

GSList * gncQueryTermGetParamPath (QueryNewTerm_t queryterm);
QueryPredData_t gncQueryTermGetPredData (QueryNewTerm_t queryterm);
gboolean gncQueryTermIsInverted (QueryNewTerm_t queryterm);


/* Functions to get and look at QuerySorts */

/* This function returns the primary, secondary, and tertiary sorts.
 * These are part of the query and should NOT be changed!
 */
void gncQueryGetSorts (QueryNew *q, QueryNewSort_t *primary,
		       QueryNewSort_t *secondary, QueryNewSort_t *tertiary);

GSList * gncQuerySortGetParamPath (QueryNewSort_t querysort);
gint gncQuerySortGetSortOptions (QueryNewSort_t querysort);
gboolean gncQuerySortGetIncreasing (QueryNewSort_t querysort);

#endif /* GNC_QUERYNEWP_H */
