/*
 *  Copyright (C) 2002 Derek Atkins
 *
 *  Authors: Derek Atkins <warlord@MIT.EDU>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of version 2 of the GNU General Public
 * License as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this program; if not, write to the
 * Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 */

#ifndef _GNCSEARCH_RECONCILED_H
#define _GNCSEARCH_RECONCILED_H

#include "search-core-type.h"
#include "Query.h"		/* for cleared_match_t */

#define GNC_TYPE_SEARCH_RECONCILED	  (gnc_search_reconciled_get_type ())
#define GNCSEARCH_RECONCILED(obj)	  GTK_CHECK_CAST (obj, GNC_TYPE_SEARCH_RECONCILED, GNCSearchReconciled)
#define GNCSEARCH_RECONCILED_CLASS(klass) GTK_CHECK_CLASS_CAST (klass, GNC_TYPE_SEARCH_RECONCILED, GNCSearchReconciledClass)
#define IS_GNCSEARCH_RECONCILED(obj)      GTK_CHECK_TYPE (obj, GNC_TYPE_SEARCH_RECONCILED)

typedef struct _GNCSearchReconciled	GNCSearchReconciled;
typedef struct _GNCSearchReconciledClass	GNCSearchReconciledClass;

struct _GNCSearchReconciled
{
    GNCSearchCoreType parent;

    char_match_t		how;
    cleared_match_t	value;
};

struct _GNCSearchReconciledClass
{
    GNCSearchCoreTypeClass parent_class;

    /* virtual methods */

    /* signals */
};

GType		gnc_search_reconciled_get_type	(void);
GNCSearchReconciled	*gnc_search_reconciled_new	(void);

/* methods */
void	gnc_search_reconciled_set_value(GNCSearchReconciled *fi, cleared_match_t value);
void	gnc_search_reconciled_set_how (GNCSearchReconciled *fi, char_match_t how);

#endif /* ! _GNCSEARCH_RECONCILED_H */
