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

#ifndef _GNCSEARCH_ACCOUNT_H
#define _GNCSEARCH_ACCOUNT_H

#include "search-core-type.h"
#include "QueryNew.h"

#define GNC_TYPE_SEARCH_ACCOUNT 	(gnc_search_account_get_type ())
#define GNCSEARCH_ACCOUNT(obj)		GTK_CHECK_CAST (obj, GNC_TYPE_SEARCH_ACCOUNT, GNCSearchAccount)
#define GNCSEARCH_ACCOUNT_CLASS(klass)	GTK_CHECK_CLASS_CAST (klass, GNC_TYPE_SEARCH_ACCOUNT, GNCSearchAccountClass)
#define IS_GNCSEARCH_ACCOUNT(obj)	GTK_CHECK_TYPE (obj, GNC_TYPE_SEARCH_ACCOUNT)

typedef struct _GNCSearchAccount	GNCSearchAccount;
typedef struct _GNCSearchAccountClass	GNCSearchAccountClass;

struct _GNCSearchAccount
{
    GNCSearchCoreType parent;

    guid_match_t	how;
};

struct _GNCSearchAccountClass
{
    GNCSearchCoreTypeClass parent_class;

    /* virtual methods */

    /* signals */
};

GType		gnc_search_account_get_type	(void);
GNCSearchAccount	*gnc_search_account_new	(void);
GNCSearchAccount	*gnc_search_account_matchall_new	(void);

/* methods */

#endif /* ! _GNCSEARCH_ACCOUNT_H */

