/*
 * search-param.h -- a container for a Search Parameter
 * Copyright (C) 2002 Derek Atkins <warlord@MIT.EDU>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

#ifndef _GNCSEARCH_PARAM_H
#define _GNCSEARCH_PARAM_H

#include "GNCId.h"

#define GNC_TYPE_SEARCH_PARAM	 (gnc_search_param_get_type ())
#define GNC_SEARCH_PARAM(o)	 (G_TYPE_CHECK_INSTANCE_CAST ((o), GNC_TYPE_SEARCH_PARAM, GNCSearchParam))
#define GNCSEARCH_PARAM_CLASS(k) (G_TYPE_CHECK_CLASS_CAST ((k), GNC_TYPE_SEARCH_PARAM, GNCSearchParamClass)
#define GNC_IS_SEARCH_PARAM(o)	 (G_TYPE_CHECK_INSTANCE_TYPE ((o), GNC_TYPE_SEARCH_PARAM))

typedef struct _GNCSearchParam	GNCSearchParam;
typedef struct _GNCSearchParamClass	GNCSearchParamClass;

struct _GNCSearchParam
{
    GObject gobject;

    const char *		title;
    GtkJustification	justify;
    gboolean		passive;
    gboolean		non_resizeable;
};

struct _GNCSearchParamClass
{
    GObjectClass gobject_class;

    /* virtual methods */

    /* signals */
};

/* These are internal functions */
GType			gnc_search_param_get_type (void);

/* Create a new search param */
GNCSearchParam *	gnc_search_param_new (void);
GNCSearchParam *	gnc_search_param_clone (GNCSearchParam *param);

/* use the param_path for this parameter.  This will automatically
 * compute the parameter type and the converter functions.
 */
void			gnc_search_param_set_param_path (GNCSearchParam *param,
        GNCIdTypeConst search_type,
        GSList *param_path);

/* List is property of the caller */
GSList *		gnc_search_param_get_param_path (GNCSearchParam *param);
GNCIdTypeConst		gnc_search_param_get_param_type (GNCSearchParam *param);
void			gnc_search_param_set_title (GNCSearchParam *param,
        const char *title);
void			gnc_search_param_set_justify (GNCSearchParam *param,
        GtkJustification justify);
void			gnc_search_param_set_passive (GNCSearchParam *param,
        gboolean value);
void			gnc_search_param_set_non_resizeable (GNCSearchParam *param,
        gboolean value);
gboolean		gnc_search_param_type_match (GNCSearchParam *a,
        GNCSearchParam *b);

/* Return the list of QueryAccess functions for this parameter.  This list
 * is owned by the param object -- users should not change it */
GSList *		gnc_search_param_get_converters (GNCSearchParam *param);

/* This will override the automatic param_type logic from "set_param_path()"
 * so that the programmer can force a particular UI to appear for a given
 * parameter path.  This should be used with care -- if used improperly
 * it could result in an invalid Query Term, where the path and the predicate
 * don't match types properly.
 */
void			gnc_search_param_override_param_type (GNCSearchParam *param,
        GNCIdTypeConst param_type);


/*************************************************************
 * Helper functions ..
 */

/* Create a paramter and prepend it to a GSList */
GList *			gnc_search_param_prepend (GList *list, char const *title,
        GNCIdTypeConst type_override,
        GNCIdTypeConst search_type,
        const char *param, ...);


GList *			gnc_search_param_prepend_with_justify (GList *list, char const *title,
        GtkJustification justify,
        GNCIdTypeConst type_override,
        GNCIdTypeConst search_type,
        const char *param, ...);

/* set a lookup function for this parameter (in lieu of setting the
 * param path) if you want to specify a direct lookup function when
 * using the compute_value interface.  Note that this wont work with
 * sorting or other query interfaces, it's only useful for the
 * query-list.
 */
typedef gpointer (*GNCSearchParamFcn)(gpointer object, gpointer arg);
void		gnc_search_param_set_param_fcn (GNCSearchParam *param,
        GNCIdTypeConst param_type,
        GNCSearchParamFcn fcn,
        gpointer arg);

/* Compute the value of this parameter for this object */
gpointer gnc_search_param_compute_value (GNCSearchParam *param, gpointer object);


#endif /* _GNCSEARCH_PARAM_H */
