/*
 *  Copyright (C) 2002 Derek Atkins
 *
 *  Authors: Derek Atkins <warlord@MIT.EDU>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
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

#ifndef _GNCSEARCH_INT64_H
#define _GNCSEARCH_INT64_H

#include "search-core-type.h"
#include "qof.h"

#define GNC_TYPE_SEARCH_INT64		(gnc_search_int64_get_type ())
#define GNCSEARCH_INT64(obj)		G_TYPE_CHECK_INSTANCE_CAST (obj, GNC_TYPE_SEARCH_INT64, GNCSearchInt64)
#define GNCSEARCH_INT64_CLASS(klass)	G_TYPE_CHECK_CLASS_CAST (klass, GNC_TYPE_SEARCH_INT64, GNCSearchInt64Class)
#define IS_GNCSEARCH_INT64(obj)		G_TYPE_CHECK_INSTANCE_TYPE (obj, GNC_TYPE_SEARCH_INT64)

typedef struct _GNCSearchInt64	GNCSearchInt64;
typedef struct _GNCSearchInt64Class	GNCSearchInt64Class;

struct _GNCSearchInt64
{
    GNCSearchCoreType parent;

    QofQueryCompare	how;
    gint64		value;
};

struct _GNCSearchInt64Class
{
    GNCSearchCoreTypeClass parent_class;

    /* virtual methods */

    /* signals */
};

GType		gnc_search_int64_get_type	(void);
GNCSearchInt64	*gnc_search_int64_new	(void);

/* methods */
void	gnc_search_int64_set_value (GNCSearchInt64 *fi, gint64 val);
void	gnc_search_int64_set_how (GNCSearchInt64 *fi, QofQueryCompare how);

#endif /* ! _GNCSEARCH_INT64_H */

