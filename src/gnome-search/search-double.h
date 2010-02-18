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

#ifndef _GNCSEARCH_DOUBLE_H
#define _GNCSEARCH_DOUBLE_H

#include "search-core-type.h"
#include "QueryNew.h"

#define GNC_TYPE_SEARCH_DOUBLE		(gnc_search_double_get_type ())
#define GNCSEARCH_DOUBLE(obj)		GTK_CHECK_CAST (obj, GNC_TYPE_SEARCH_DOUBLE, GNCSearchDouble)
#define GNCSEARCH_DOUBLE_CLASS(klass)	GTK_CHECK_CLASS_CAST (klass, GNC_TYPE_SEARCH_DOUBLE, GNCSearchDoubleClass)
#define IS_GNCSEARCH_DOUBLE(obj)	GTK_CHECK_TYPE (obj, GNC_TYPE_SEARCH_DOUBLE)

typedef struct _GNCSearchDouble	GNCSearchDouble;
typedef struct _GNCSearchDoubleClass	GNCSearchDoubleClass;

struct _GNCSearchDouble
{
    GNCSearchCoreType parent;

    query_compare_t	how;
    double		value;
};

struct _GNCSearchDoubleClass
{
    GNCSearchCoreTypeClass parent_class;

    /* virtual methods */

    /* signals */
};

GType		gnc_search_double_get_type	(void);
GNCSearchDouble	*gnc_search_double_new	(void);

/* methods */
void	gnc_search_double_set_value (GNCSearchDouble *fi, double val);
void	gnc_search_double_set_how (GNCSearchDouble *fi, query_compare_t how);

#endif /* ! _GNCSEARCH_DOUBLE_H */

