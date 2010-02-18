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

#ifndef _GNCSEARCH_DATE_H
#define _GNCSEARCH_DATE_H

#include "search-core-type.h"
#include "QueryNew.h"
#include "qof.h"

#define GNC_TYPE_SEARCH_DATE		(gnc_search_date_get_type ())
#define GNCSEARCH_DATE(obj)		GTK_CHECK_CAST (obj, GNC_TYPE_SEARCH_DATE, GNCSearchDate)
#define GNCSEARCH_DATE_CLASS(klass)	GTK_CHECK_CLASS_CAST (klass, GNC_TYPE_SEARCH_DATE, GNCSearchDateClass)
#define IS_GNCSEARCH_DATE(obj)     	GTK_CHECK_TYPE (obj, GNC_TYPE_SEARCH_DATE)

typedef struct _GNCSearchDate	GNCSearchDate;
typedef struct _GNCSearchDateClass	GNCSearchDateClass;

struct _GNCSearchDate
{
    GNCSearchCoreType parent;

    query_compare_t	how;
    Timespec		ts;
};

struct _GNCSearchDateClass
{
    GNCSearchCoreTypeClass parent_class;

    /* virtual methods */

    /* signals */
};

GType		gnc_search_date_get_type	(void);
GNCSearchDate	*gnc_search_date_new	(void);

/* methods */
void	gnc_search_date_set_date (GNCSearchDate *fi, Timespec ts);
void	gnc_search_date_set_how (GNCSearchDate *fi, query_compare_t how);

#endif /* ! _GNCSEARCH_DATE_H */

