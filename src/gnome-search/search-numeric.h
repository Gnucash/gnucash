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

#ifndef _GNCSEARCH_NUMERIC_H
#define _GNCSEARCH_NUMERIC_H

#include "search-core-type.h"
#include "gnc-numeric.h"
#include "QueryNew.h"

#define GNCSEARCH_NUMERIC(obj)	GTK_CHECK_CAST (obj, gnc_search_numeric_get_type (), GNCSearchNumeric)
#define GNCSEARCH_NUMERIC_CLASS(klass)	GTK_CHECK_CLASS_CAST (klass, gnc_search_numeric_get_type (), GNCSearchNumericClass)
#define IS_GNCSEARCH_NUMERIC(obj)      GTK_CHECK_TYPE (obj, gnc_search_numeric_get_type ())

typedef struct _GNCSearchNumeric	GNCSearchNumeric;
typedef struct _GNCSearchNumericClass	GNCSearchNumericClass;

struct _GNCSearchNumeric {
  GNCSearchCoreType parent;
  struct _GNCSearchNumericPrivate *priv;

  query_compare_t	how;
  gnc_numeric		value;
};

struct _GNCSearchNumericClass {
  GNCSearchCoreTypeClass parent_class;

  /* virtual methods */

  /* signals */
};

guint		gnc_search_numeric_get_type	(void);
GNCSearchNumeric	*gnc_search_numeric_new	(void);

/* methods */
void	gnc_search_numeric_set_value (GNCSearchNumeric *fi, gnc_numeric val);
void	gnc_search_numeric_set_how (GNCSearchNumeric *fi, query_compare_t how);

#endif /* ! _GNCSEARCH_NUMERIC_H */

