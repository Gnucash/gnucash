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
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#ifndef _GNCSEARCH_ACCOUNT_H
#define _GNCSEARCH_ACCOUNT_H

#include "search-core-type.h"
#include "QueryNew.h"

#define GNCSEARCH_ACCOUNT(obj)	GTK_CHECK_CAST (obj, gnc_search_account_get_type (), GNCSearchAccount)
#define GNCSEARCH_ACCOUNT_CLASS(klass)	GTK_CHECK_CLASS_CAST (klass, gnc_search_account_get_type (), GNCSearchAccountClass)
#define IS_GNCSEARCH_ACCOUNT(obj)      GTK_CHECK_TYPE (obj, gnc_search_account_get_type ())

typedef struct _GNCSearchAccount	GNCSearchAccount;
typedef struct _GNCSearchAccountClass	GNCSearchAccountClass;

struct _GNCSearchAccount {
  GNCSearchCoreType parent;
  struct _GNCSearchAccountPrivate *priv;

  guid_match_t	how;
};

struct _GNCSearchAccountClass {
  GNCSearchCoreTypeClass parent_class;

  /* virtual methods */

  /* signals */
};

guint		gnc_search_account_get_type	(void);
GNCSearchAccount	*gnc_search_account_new	(void);
GNCSearchAccount	*gnc_search_account_matchall_new	(void);

/* methods */

#endif /* ! _GNCSEARCH_ACCOUNT_H */

