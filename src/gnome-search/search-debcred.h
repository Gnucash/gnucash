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

#ifndef _GNCSEARCH_DEBCRED_H
#define _GNCSEARCH_DEBCRED_H

#include "search-core-type.h"
#include "gnc-numeric.h"
#include "QueryNew.h"

#define GNCSEARCH_DEBCRED(obj)	GTK_CHECK_CAST (obj, gnc_search_debcred_get_type (), GNCSearchDebcred)
#define GNCSEARCH_DEBCRED_CLASS(klass)	GTK_CHECK_CLASS_CAST (klass, gnc_search_debcred_get_type (), GNCSearchDebcredClass)
#define IS_GNCSEARCH_DEBCRED(obj)      GTK_CHECK_TYPE (obj, gnc_search_debcred_get_type ())

typedef struct _GNCSearchDebcred	GNCSearchDebcred;
typedef struct _GNCSearchDebcredClass	GNCSearchDebcredClass;

struct _GNCSearchDebcred {
  GNCSearchCoreType parent;
  struct _GNCSearchDebcredPrivate *priv;

  query_compare_t	how;
  gnc_numeric		value;
  numeric_match_t	option;
};

struct _GNCSearchDebcredClass {
  GNCSearchCoreTypeClass parent_class;

  /* virtual methods */

  /* signals */
};

guint		gnc_search_debcred_get_type	(void);
GNCSearchDebcred	*gnc_search_debcred_new	(void);

/* methods */
void	gnc_search_debcred_set_value (GNCSearchDebcred *fi, gnc_numeric val);
void	gnc_search_debcred_set_how (GNCSearchDebcred *fi, query_compare_t how);
void	gnc_search_debcred_set_option (GNCSearchDebcred *fi, numeric_match_t option);

#endif /* ! _GNCSEARCH_DEBCRED_H */

