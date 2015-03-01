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

#define GNC_TYPE_SEARCH_PARAM_SIMPLE	 (gnc_search_param_simple_get_type ())
#define GNC_SEARCH_PARAM_SIMPLE(o)	 \
    (G_TYPE_CHECK_INSTANCE_CAST ((o), GNC_TYPE_SEARCH_PARAM_SIMPLE, GNCSearchParamSimple))
#define GNCSEARCH_PARAM_SIMPLE_CLASS(k) \
    (G_TYPE_CHECK_CLASS_CAST ((k), GNC_TYPE_SEARCH_PARAM_SIMPLE, GNCSearchParamSimpleClass)
#define GNC_IS_SEARCH_PARAM_SIMPLE(o)	 (G_TYPE_CHECK_INSTANCE_TYPE ((o), GNC_TYPE_SEARCH_PARAM_SIMPLE))

typedef struct _GNCSearchParamSimple	GNCSearchParamSimple;
typedef struct _GNCSearchParamSimpleClass	GNCSearchParamSimpleClass;

struct _GNCSearchParamSimple
{
    GNCSearchParam  search_param;
};

struct _GNCSearchParamSimpleClass
{
    GNCSearchParamClass search_param_class;

    /* virtual methods */

    /* signals */
};

#define GNC_TYPE_SEARCH_PARAM_COMPOUND	 (gnc_search_param_compound_get_type ())
#define GNC_SEARCH_PARAM_COMPOUND(o)	 \
    (G_TYPE_CHECK_INSTANCE_CAST ((o), GNC_TYPE_SEARCH_PARAM_COMPOUND, GNCSearchParamCompound))
#define GNCSEARCH_PARAM_COMPOUND_CLASS(k) \
    (G_TYPE_CHECK_CLASS_CAST ((k), GNC_TYPE_SEARCH_PARAM_COMPOUND, GNCSearchParamCompoundClass)
#define GNC_IS_SEARCH_PARAM_COMPOUND(o)	 (G_TYPE_CHECK_INSTANCE_TYPE ((o), GNC_TYPE_SEARCH_PARAM_COMPOUND))

typedef struct _GNCSearchParamCompound	GNCSearchParamCompound;
typedef struct _GNCSearchParamCompoundClass	GNCSearchParamCompoundClass;

struct _GNCSearchParamCompound
{
    GNCSearchParam  search_param;
};

struct _GNCSearchParamCompoundClass
{
    GNCSearchParamClass search_param_class;

    /* virtual methods */

    /* signals */
};

typedef enum
{
    SEARCH_PARAM_ELEM = 0,
    SEARCH_PARAM_ANY = 1,
    SEARCH_PARAM_ALL = 2
} GNCSearchParamKind;

/* These are internal functions */
GType			gnc_search_param_get_type (void);
GType			gnc_search_param_simple_get_type (void);
GType			gnc_search_param_compound_get_type (void);

/* Create a new search param */
GNCSearchParamSimple *	gnc_search_param_simple_new (void);
GNCSearchParamCompound *	gnc_search_param_compound_new (void);

/* use the param_path for this parameter.  This will automatically
 * compute the parameter type and the converter functions.
 */
void			gnc_search_param_set_param_path (GNCSearchParamSimple *param,
        QofIdTypeConst search_type,
        GSList *param_path);

/* List is property of the caller */
GList *         gnc_search_param_get_search (GNCSearchParamCompound *param);
GSList *		gnc_search_param_get_param_path (GNCSearchParamSimple *param);
QofIdTypeConst		gnc_search_param_get_param_type (GNCSearchParam *param);
void			gnc_search_param_set_title (GNCSearchParam *param,
        const char *title);
GNCSearchParamKind gnc_search_param_get_kind (GNCSearchParam *param);
void			gnc_search_param_set_justify (GNCSearchParam *param,
        GtkJustification justify);
void			gnc_search_param_set_passive (GNCSearchParam *param,
        gboolean value);
void			gnc_search_param_set_non_resizeable (GNCSearchParam *param,
        gboolean value);
gboolean		gnc_search_param_type_match (GNCSearchParam *a,
        GNCSearchParam *b);

/* Return the list of QofAccessFunc functions for this parameter.  This list
 * is owned by the param object -- users should not change it */
GSList *		gnc_search_param_get_converters (GNCSearchParamSimple *param);

/* This will override the automatic param_type logic from "set_param_path()"
 * so that the programmer can force a particular UI to appear for a given
 * parameter path.  This should be used with care -- if used improperly
 * it could result in an invalid Query Term, where the path and the predicate
 * don't match types properly.
 */
void			gnc_search_param_override_param_type (GNCSearchParamSimple *param,
        QofIdTypeConst param_type);


/*************************************************************
 * Helper functions ..
 */

/* Create a paramter and prepend it to a GSList */
GList *			gnc_search_param_prepend (GList *list, char const *title,
        QofIdTypeConst type_override,
        QofIdTypeConst search_type,
        const char *param, ...);


GList *			gnc_search_param_prepend_with_justify (GList *list, char const *title,
        GtkJustification justify,
        QofIdTypeConst type_override,
        QofIdTypeConst search_type,
        const char *param, ...);

GList *         gnc_search_param_prepend_compound (GList *list, char const *title,
                                   GList *param_list,
                                   GtkJustification justify,
                                   GNCSearchParamKind kind);

/* set a lookup function for this parameter (in lieu of setting the
 * param path) if you want to specify a direct lookup function when
 * using the compute_value interface.  Note that this wont work with
 * sorting or other query interfaces, it's only useful for the
 * query-list.
 */
typedef gpointer (*GNCSearchParamFcn)(gpointer object, gpointer arg);
void		gnc_search_param_set_param_fcn (GNCSearchParamSimple *param,
        QofIdTypeConst param_type,
        GNCSearchParamFcn fcn,
        gpointer arg);

/* Compute the value of this parameter for this object */
gpointer gnc_search_param_compute_value (GNCSearchParamSimple *param, gpointer object);


#endif /* _GNCSEARCH_PARAM_H */
