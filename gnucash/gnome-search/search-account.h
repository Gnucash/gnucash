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

#ifndef _GNCSEARCH_ACCOUNT_H
#define _GNCSEARCH_ACCOUNT_H

#include "search-core-type.h"
#include "qof.h"

#define GNC_TYPE_SEARCH_ACCOUNT 	(gnc_search_account_get_type ())
#define GNCSEARCH_ACCOUNT(obj)		G_TYPE_CHECK_INSTANCE_CAST (obj, GNC_TYPE_SEARCH_ACCOUNT, GNCSearchAccount)
#define GNCSEARCH_ACCOUNT_CLASS(klass)	G_TYPE_CHECK_CLASS_CAST (klass, GNC_TYPE_SEARCH_ACCOUNT, GNCSearchAccountClass)
#define IS_GNCSEARCH_ACCOUNT(obj)	G_TYPE_CHECK_INSTANCE_TYPE (obj, GNC_TYPE_SEARCH_ACCOUNT)

typedef struct _GNCSearchAccount	GNCSearchAccount;
typedef struct _GNCSearchAccountClass	GNCSearchAccountClass;

struct _GNCSearchAccount
{
    GNCSearchCoreType parent;

    QofGuidMatch	how;
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

