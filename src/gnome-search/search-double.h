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

#ifndef _GNCSEARCH_DOUBLE_H
#define _GNCSEARCH_DOUBLE_H

#include "search-core-type.h"
#include "QueryNew.h"

#define GNCSEARCH_DOUBLE(obj)	GTK_CHECK_CAST (obj, gnc_search_double_get_type (), GNCSearchDouble)
#define GNCSEARCH_DOUBLE_CLASS(klass)	GTK_CHECK_CLASS_CAST (klass, gnc_search_double_get_type (), GNCSearchDoubleClass)
#define IS_GNCSEARCH_DOUBLE(obj)      GTK_CHECK_TYPE (obj, gnc_search_double_get_type ())

typedef struct _GNCSearchDouble	GNCSearchDouble;
typedef struct _GNCSearchDoubleClass	GNCSearchDoubleClass;

struct _GNCSearchDouble {
  GNCSearchCoreType parent;
  struct _GNCSearchDoublePrivate *priv;

  query_compare_t	how;
  double		value;
};

struct _GNCSearchDoubleClass {
  GNCSearchCoreTypeClass parent_class;

  /* virtual methods */

  /* signals */
};

guint		gnc_search_double_get_type	(void);
GNCSearchDouble	*gnc_search_double_new	(void);

/* methods */
void	gnc_search_double_set_value (GNCSearchDouble *fi, double val);
void	gnc_search_double_set_how (GNCSearchDouble *fi, query_compare_t how);

#endif /* ! _GNCSEARCH_DOUBLE_H */

