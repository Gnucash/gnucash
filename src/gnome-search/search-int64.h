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

#ifndef _GNCSEARCH_INT64_H
#define _GNCSEARCH_INT64_H

#include "search-core-type.h"
#include "QueryNew.h"

#define GNCSEARCH_INT64(obj)	GTK_CHECK_CAST (obj, gnc_search_int64_get_type (), GNCSearchInt64)
#define GNCSEARCH_INT64_CLASS(klass)	GTK_CHECK_CLASS_CAST (klass, gnc_search_int64_get_type (), GNCSearchInt64Class)
#define IS_GNCSEARCH_INT64(obj)      GTK_CHECK_TYPE (obj, gnc_search_int64_get_type ())

typedef struct _GNCSearchInt64	GNCSearchInt64;
typedef struct _GNCSearchInt64Class	GNCSearchInt64Class;

struct _GNCSearchInt64 {
  GNCSearchCoreType parent;
  struct _GNCSearchInt64Private *priv;

  query_compare_t	how;
  gint64		value;
};

struct _GNCSearchInt64Class {
  GNCSearchCoreTypeClass parent_class;

  /* virtual methods */

  /* signals */
};

guint		gnc_search_int64_get_type	(void);
GNCSearchInt64	*gnc_search_int64_new	(void);

/* methods */
void	gnc_search_int64_set_value (GNCSearchInt64 *fi, gint64 val);
void	gnc_search_int64_set_how (GNCSearchInt64 *fi, query_compare_t how);

#endif /* ! _GNCSEARCH_INT64_H */

