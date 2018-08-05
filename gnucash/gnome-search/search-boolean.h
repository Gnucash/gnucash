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

#ifndef _GNCSEARCH_BOOLEAN_H
#define _GNCSEARCH_BOOLEAN_H

#include "search-core-type.h"
#include "qof.h"

#define GNC_TYPE_SEARCH_BOOLEAN		(gnc_search_boolean_get_type ())
#define GNCSEARCH_BOOLEAN(obj)		G_TYPE_CHECK_INSTANCE_CAST (obj, GNC_TYPE_SEARCH_BOOLEAN, GNCSearchBoolean)
#define GNCSEARCH_BOOLEAN_CLASS(klass)	G_TYPE_CHECK_CLASS_CAST (klass, GNC_TYPE_SEARCH_BOOLEAN, GNCSearchBooleanClass)
#define IS_GNCSEARCH_BOOLEAN(obj)	G_TYPE_CHECK_INSTANCE_TYPE (obj, GNC_TYPE_SEARCH_BOOLEAN)

typedef struct _GNCSearchBoolean	GNCSearchBoolean;
typedef struct _GNCSearchBooleanClass	GNCSearchBooleanClass;

struct _GNCSearchBoolean
{
    GNCSearchCoreType parent;

    gboolean		value;
};

struct _GNCSearchBooleanClass
{
    GNCSearchCoreTypeClass parent_class;

    /* virtual methods */

    /* signals */
};

GType		gnc_search_boolean_get_type	(void);
GNCSearchBoolean	*gnc_search_boolean_new	(void);

/* methods */
void	gnc_search_boolean_set_value (GNCSearchBoolean *fi, gboolean val);

#endif /* ! _GNCSEARCH_BOOLEAN_H */

