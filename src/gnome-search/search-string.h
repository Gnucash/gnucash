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

#ifndef _GNCSEARCH_STRING_H
#define _GNCSEARCH_STRING_H

#include "search-core-type.h"

#define GNC_TYPE_SEARCH_STRING		(gnc_search_string_get_type ())
#define GNCSEARCH_STRING(obj)		GTK_CHECK_CAST (obj, GNC_TYPE_SEARCH_STRING, GNCSearchString)
#define GNCSEARCH_STRING_CLASS(klass)	GTK_CHECK_CLASS_CAST (klass, GNC_TYPE_SEARCH_STRING, GNCSearchStringClass)
#define IS_GNCSEARCH_STRING(obj)	GTK_CHECK_TYPE (obj, GNC_TYPE_SEARCH_STRING)

typedef struct _GNCSearchString	GNCSearchString;
typedef struct _GNCSearchStringClass	GNCSearchStringClass;

typedef enum _search_string_how
{
    SEARCH_STRING_CONTAINS,
    SEARCH_STRING_NOT_CONTAINS,
    SEARCH_STRING_MATCHES_REGEX,
    SEARCH_STRING_NOT_MATCHES_REGEX
} GNCSearchString_Type;

struct _GNCSearchString
{
    GNCSearchCoreType parent;

    GNCSearchString_Type	how;
    gboolean		ign_case;
    char *		value;
};

struct _GNCSearchStringClass
{
    GNCSearchCoreTypeClass parent_class;

    /* virtual methods */

    /* signals */
};

GType		gnc_search_string_get_type	(void);
GNCSearchString	*gnc_search_string_new	(void);

/* methods */
void	gnc_search_string_set_value(GNCSearchString *fi, const char *value);
void	gnc_search_string_set_how (GNCSearchString *fi, GNCSearchString_Type how);
void	gnc_search_string_set_case (GNCSearchString *fi, gboolean ignore_case);

#endif /* ! _GNCSEARCH_STRING_H */

