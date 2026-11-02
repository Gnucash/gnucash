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


#define GNC_TYPE_SEARCH_PARAM    (gnc_search_param_get_type ())
G_DECLARE_DERIVABLE_TYPE (GNCSearchParam, gnc_search_param, GNC, SEARCH_PARAM, GObject)

struct _GNCSearchParamClass
{
    GObjectClass gobject_class;

    /* virtual methods */

    /* signals */
};

#define GNC_TYPE_SEARCH_PARAM_SIMPLE (gnc_search_param_simple_get_type ())
G_DECLARE_FINAL_TYPE (GNCSearchParamSimple, gnc_search_param_simple, GNC, SEARCH_PARAM_SIMPLE, GNCSearchParam)

#define GNC_TYPE_SEARCH_PARAM_COMPOUND (gnc_search_param_compound_get_type ())
G_DECLARE_FINAL_TYPE (GNCSearchParamCompound, gnc_search_param_compound, GNC, SEARCH_PARAM_COMPOUND, GNCSearchParam)

typedef enum
{
    SEARCH_PARAM_ELEM = 0,
    SEARCH_PARAM_ANY = 1,
    SEARCH_PARAM_ALL = 2
} GNCSearchParamKind;

/* Create a new search param */
GNCSearchParamSimple * gnc_search_param_simple_new (void);
GNCSearchParamCompound * gnc_search_param_compound_new (void);

/* use the param_path for this parameter.  This will automatically
 * compute the parameter type and the converter functions.
 */
void gnc_search_param_set_param_path (GNCSearchParamSimple *param,
                                      QofIdTypeConst search_type,
                                      GSList *param_path);

/* List is property of the caller */
GList * gnc_search_param_get_search (GNCSearchParamCompound *param);
GSList * gnc_search_param_get_param_path (GNCSearchParamSimple *param);
QofIdTypeConst gnc_search_param_get_param_type (GNCSearchParam *param);
const char *gnc_search_param_get_title (GNCSearchParam *param);
void gnc_search_param_set_title (GNCSearchParam *param,
                                 const char *title);
GNCSearchParamKind gnc_search_param_get_kind (GNCSearchParam *param);
GtkJustification gnc_search_param_get_justify (GNCSearchParam *param);
void gnc_search_param_set_justify (GNCSearchParam *param,
                                   GtkJustification justify);
gboolean gnc_search_param_get_passive (GNCSearchParam *param);
void gnc_search_param_set_passive (GNCSearchParam *param,
                                   gboolean value);
gboolean gnc_search_param_get_non_resizeable (GNCSearchParam *param);
void gnc_search_param_set_non_resizeable (GNCSearchParam *param,
                                          gboolean value);
gboolean gnc_search_param_type_match (GNCSearchParam *a,
                                      GNCSearchParam *b);

/* Return the list of QofAccessFunc functions for this parameter. This list
 * is owned by the param object -- users should not change it */
GSList * gnc_search_param_get_converters (GNCSearchParamSimple *param);

/* This will override the automatic param_type logic from "set_param_path()"
 * so that the programmer can force a particular UI to appear for a given
 * parameter path.  This should be used with care -- if used improperly
 * it could result in an invalid Query Term, where the path and the predicate
 * don't match types properly.
 */
void gnc_search_param_override_param_type (GNCSearchParamSimple *param,
                                           QofIdTypeConst param_type);

/*************************************************************
 * Helper functions ..
 */

/* Create a parameter and prepend it to a GSList */
GList * gnc_search_param_prepend (GList *list, char const *title,
                                  QofIdTypeConst type_override,
                                  QofIdTypeConst search_type,
                                  const char *param, ...);

GList * gnc_search_param_prepend_with_justify (GList *list, char const *title,
                                               GtkJustification justify,
                                               QofIdTypeConst type_override,
                                               QofIdTypeConst search_type,
                                               const char *param, ...);

GList * gnc_search_param_prepend_compound (GList *list, char const *title,
                                           GList *param_list,
                                           GtkJustification justify,
                                           GNCSearchParamKind kind);

/* set a lookup function for this parameter (in lieu of setting the
 * param path) if you want to specify a direct lookup function when
 * using the compute_value interface.  Note that this won't work with
 * sorting or other query interfaces, it's only useful for the
 * query-list.
 */
typedef gpointer (*GNCSearchParamFcn)(gpointer object, gpointer arg);
void gnc_search_param_set_param_fcn (GNCSearchParamSimple *param,
                                     QofIdTypeConst param_type,
                                     GNCSearchParamFcn fcn,
                                     gpointer arg);

/* check to see if this parameter is a lookup function */
gboolean gnc_search_param_has_param_fcn (GNCSearchParamSimple *param);

/* Compute the value of this parameter for this object */
gpointer gnc_search_param_compute_value (GNCSearchParamSimple *param,
                                         gpointer object);

#endif /* _GNCSEARCH_PARAM_H */
