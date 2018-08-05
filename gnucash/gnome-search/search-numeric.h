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

#ifndef _GNCSEARCH_NUMERIC_H
#define _GNCSEARCH_NUMERIC_H

#include "search-core-type.h"
#include "qof.h"
#include "qof.h"

#define GNC_TYPE_SEARCH_NUMERIC		(gnc_search_numeric_get_type ())
#define GNCSEARCH_NUMERIC(obj)		G_TYPE_CHECK_INSTANCE_CAST (obj, GNC_TYPE_SEARCH_NUMERIC, GNCSearchNumeric)
#define GNCSEARCH_NUMERIC_CLASS(klass)	G_TYPE_CHECK_CLASS_CAST (klass, GNC_TYPE_SEARCH_NUMERIC, GNCSearchNumericClass)
#define IS_GNCSEARCH_NUMERIC(obj)	G_TYPE_CHECK_INSTANCE_TYPE (obj, GNC_TYPE_SEARCH_NUMERIC)

typedef struct _GNCSearchNumeric	GNCSearchNumeric;
typedef struct _GNCSearchNumericClass	GNCSearchNumericClass;

struct _GNCSearchNumeric
{
    GNCSearchCoreType parent;
    struct _GNCSearchNumericPrivate *priv;

    QofQueryCompare	how;
    gnc_numeric		value;
    QofNumericMatch	option;
};

struct _GNCSearchNumericClass
{
    GNCSearchCoreTypeClass parent_class;

    /* virtual methods */

    /* signals */
};

GType		gnc_search_numeric_get_type	(void);
GNCSearchNumeric	*gnc_search_numeric_new	(void);
GNCSearchNumeric	*gnc_search_numeric_debcred_new (void);

/* methods */
void	gnc_search_numeric_set_value (GNCSearchNumeric *fi, gnc_numeric val);
void	gnc_search_numeric_set_how (GNCSearchNumeric *fi, QofQueryCompare how);
void	gnc_search_numeric_set_option (GNCSearchNumeric *fi, QofNumericMatch option);

#endif /* ! _GNCSEARCH_NUMERIC_H */

