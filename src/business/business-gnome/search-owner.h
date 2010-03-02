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

#ifndef _GNCSEARCH_OWNER_H
#define _GNCSEARCH_OWNER_H

#include "search-core-type.h"
#include "QueryNew.h"

#define GNC_TYPE_SEARCH_OWNER (gnc_search_owner_get_type ())
#define GNCSEARCH_OWNER(obj)	GTK_CHECK_CAST (obj, gnc_search_owner_get_type (), GNCSearchOwner)
#define GNCSEARCH_OWNER_CLASS(klass)	GTK_CHECK_CLASS_CAST (klass, gnc_search_owner_get_type (), GNCSearchOwnerClass)
#define IS_GNCSEARCH_OWNER(obj)      GTK_CHECK_TYPE (obj, gnc_search_owner_get_type ())

typedef struct _GNCSearchOwner	GNCSearchOwner;
typedef struct _GNCSearchOwnerClass	GNCSearchOwnerClass;

struct _GNCSearchOwner
{
    GNCSearchCoreType parent;

    guid_match_t	how;
};

struct _GNCSearchOwnerClass
{
    GNCSearchCoreTypeClass parent_class;

    /* virtual methods */

    /* signals */
};

GType		gnc_search_owner_get_type	(void);
GNCSearchOwner	*gnc_search_owner_new	(void);

/* methods */

#endif /* ! _GNCSEARCH_OWNER_H */
