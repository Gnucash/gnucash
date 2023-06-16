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

#ifndef _GNCSEARCH_DOUBLE_H
#define _GNCSEARCH_DOUBLE_H

#include "search-core-type.h"
#include "qof.h"

#define GNC_TYPE_SEARCH_DOUBLE		(gnc_search_double_get_type ())
G_DECLARE_FINAL_TYPE (GNCSearchDouble, gnc_search_double, GNC, SEARCH_DOUBLE, GNCSearchCoreType)

GNCSearchDouble	*gnc_search_double_new	(void);

/* methods */
void	gnc_search_double_set_value (GNCSearchDouble *fi, double val);
void	gnc_search_double_set_how (GNCSearchDouble *fi, QofQueryCompare how);

#endif /* ! _GNCSEARCH_DOUBLE_H */

