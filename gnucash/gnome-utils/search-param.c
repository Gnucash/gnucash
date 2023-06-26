/*
 * search-param.c -- a container for a Search Parameter
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <gtk/gtk.h>
#include <string.h>
#include <stdarg.h>

#include "gnc-engine.h"
#include "qof.h"

#include "search-param.h"

static void gnc_search_param_finalize (GObject *obj);

static void gnc_search_param_simple_finalize (GObject *obj);

static void gnc_search_param_compound_finalize (GObject *obj);

typedef struct _GNCSearchParamPrivate GNCSearchParamPrivate;

struct _GNCSearchParamPrivate
{
    const char *        title;
    GtkJustification    justify;
    gboolean            passive;
    gboolean            non_resizeable;

    QofIdTypeConst  type;
};

#define GNC_SEARCH_PARAM_GET_PRIVATE(o) \
   ((GNCSearchParamPrivate*)gnc_search_param_get_instance_private ((GNCSearchParam*)o))

struct _GNCSearchParamSimple
{
    GNCSearchParam  search_param;

    GSList *        converters;
    GSList *        param_path;

    GNCSearchParamFcn   lookup_fcn;
    gpointer            lookup_arg;
};

struct _GNCSearchParamCompound
{
    GNCSearchParam  search_param;

    GList * sub_search;

    /* This defines the type of subsearch, either AND or OR */
    GNCSearchParamKind kind;
};

enum
{
    LAST_SIGNAL
};

#if LAST_SIGNAL > 0
static guint signals[LAST_SIGNAL] = { 0 };
#endif

/* Base class */

G_DEFINE_TYPE_WITH_PRIVATE(GNCSearchParam, gnc_search_param, G_TYPE_OBJECT)

static void
gnc_search_param_class_init (GNCSearchParamClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS(klass);

    object_class->finalize = gnc_search_param_finalize;
}

static void
gnc_search_param_init (GNCSearchParam *o)
{
}

static void
gnc_search_param_finalize (GObject *obj)
{
    g_return_if_fail (obj != NULL);
    g_return_if_fail (GNC_IS_SEARCH_PARAM(obj));

    G_OBJECT_CLASS(gnc_search_param_parent_class)->finalize (obj);
}

/* subclass for simple searches of a single element */

G_DEFINE_TYPE(GNCSearchParamSimple, gnc_search_param_simple, GNC_TYPE_SEARCH_PARAM)

static void
gnc_search_param_simple_class_init (GNCSearchParamSimpleClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS(klass);

    object_class->finalize = gnc_search_param_simple_finalize;
}

static void
gnc_search_param_simple_init (GNCSearchParamSimple *o)
{
}

static void
gnc_search_param_simple_finalize (GObject *obj)
{
    GNCSearchParamSimple        *o;

    g_return_if_fail (obj != NULL);
    g_return_if_fail (GNC_IS_SEARCH_PARAM_SIMPLE(obj));

    o = GNC_SEARCH_PARAM_SIMPLE(obj);

    g_slist_free (o->param_path);
    o->param_path = NULL;
    g_slist_free (o->converters);
    o->converters = NULL;

    G_OBJECT_CLASS(gnc_search_param_simple_parent_class)->finalize (obj);
}

/* Subclass for compound searches consisting of AND/OR of several elements */

G_DEFINE_TYPE(GNCSearchParamCompound, gnc_search_param_compound, GNC_TYPE_SEARCH_PARAM)

static void
gnc_search_param_compound_class_init (GNCSearchParamCompoundClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS(klass);

    object_class->finalize = gnc_search_param_compound_finalize;
}

static void
gnc_search_param_compound_init (GNCSearchParamCompound *o)
{
}

static void
gnc_search_param_compound_finalize (GObject *obj)
{
    GNCSearchParamCompound        *o;

    g_return_if_fail (obj != NULL);
    g_return_if_fail (GNC_IS_SEARCH_PARAM_COMPOUND(obj));

    o = GNC_SEARCH_PARAM_COMPOUND(obj);

    g_list_free (o->sub_search);
    o->sub_search = NULL;

    G_OBJECT_CLASS (gnc_search_param_compound_parent_class)->finalize (obj);
}

/**
 * gnc_search_param_simple_new:
 *
 * Create a new GNCSearchParamSimple object.
 *
 * Return value: A new #GNCSearchParam object.
 **/
GNCSearchParamSimple *
gnc_search_param_simple_new (void)
{
    GNCSearchParamSimple *o = (GNCSearchParamSimple *)
                               g_object_new (gnc_search_param_simple_get_type (), NULL);
    return o;
}

/**
 * gnc_search_param_compound_new:
 *
 * Create a new GNCSearchParam object.
 *
 * Return value: A new #GNCSearchParamCompound object.
 **/
GNCSearchParamCompound *
gnc_search_param_compound_new (void)
{
    GNCSearchParamCompound *o = (GNCSearchParamCompound *)
                                 g_object_new (gnc_search_param_compound_get_type (), NULL);
    return o;
}

void
gnc_search_param_set_param_path (GNCSearchParamSimple *param,
                                 QofIdTypeConst search_type,
                                 GSList *param_path)
{
    GNCSearchParamPrivate       *priv_base;
    QofIdTypeConst               type = NULL;
    GSList                      *converters = NULL;

    g_return_if_fail (GNC_IS_SEARCH_PARAM_SIMPLE(param));

    if (param->param_path)
    {
        g_slist_free (param->param_path);
    }
    param->param_path = g_slist_copy (param_path);

    /* Compute the parameter type */
    for (; param_path; param_path = param_path->next)
    {
        QofIdType param_name = param_path->data;
        const QofParam *objDef = qof_class_get_parameter (search_type,
                                                          param_name);

        /* If it doesn't exist, then we've reached the end */
        if (objDef == NULL)
            break;

        /* Save the converter */
        converters = g_slist_prepend (converters, (gpointer) objDef);

        /* And reset for the next parameter */
        type = search_type = objDef->param_type;
    }

    /* Save the type */
    priv_base = GNC_SEARCH_PARAM_GET_PRIVATE(param);
    priv_base->type = type;

    /* Save the converters */
    if (param->converters)
    {
        g_slist_free (param->converters);
    }
    param->converters = g_slist_reverse (converters);
}

void
gnc_search_param_override_param_type (GNCSearchParamSimple *param,
                                      QofIdTypeConst param_type)
{
    GNCSearchParamPrivate *priv;

    g_return_if_fail (GNC_IS_SEARCH_PARAM_SIMPLE(param));
    g_return_if_fail (param_type != NULL && *param_type != '\0');

    priv = GNC_SEARCH_PARAM_GET_PRIVATE(GNC_SEARCH_PARAM(param));
    priv->type = param_type;
    /* XXX: What about the converters? */
}

GList *
gnc_search_param_get_search (GNCSearchParamCompound *param)
{
    g_return_val_if_fail (GNC_IS_SEARCH_PARAM_COMPOUND(param), NULL);

    return param->sub_search;
}

GSList *
gnc_search_param_get_param_path (GNCSearchParamSimple *param)
{
    g_return_val_if_fail (GNC_IS_SEARCH_PARAM_SIMPLE(param), NULL);

    return g_slist_copy (param->param_path);
}

GSList *
gnc_search_param_get_converters (GNCSearchParamSimple *param)
{
    g_return_val_if_fail (GNC_IS_SEARCH_PARAM_SIMPLE(param), NULL);

    return param->converters;
}

QofIdTypeConst
gnc_search_param_get_param_type (GNCSearchParam *param)
{
    GNCSearchParamPrivate *priv;

    g_return_val_if_fail (GNC_IS_SEARCH_PARAM(param), NULL);

    priv = GNC_SEARCH_PARAM_GET_PRIVATE(param);
    return priv->type;
}

GNCSearchParamKind
gnc_search_param_get_kind (GNCSearchParam *param)
{
    if (GNC_IS_SEARCH_PARAM_SIMPLE(param))
        return SEARCH_PARAM_ELEM;

    g_return_val_if_fail (GNC_IS_SEARCH_PARAM_COMPOUND(param), SEARCH_PARAM_ELEM);

    return GNC_SEARCH_PARAM_COMPOUND (param)->kind;
}

const char*
gnc_search_param_get_title (GNCSearchParam *param)
{
    g_return_val_if_fail (GNC_IS_SEARCH_PARAM(param), NULL);

    GNCSearchParamPrivate *priv = GNC_SEARCH_PARAM_GET_PRIVATE(param);
    return priv->title;
}

void
gnc_search_param_set_title (GNCSearchParam *param, const char *title)
{
    g_return_if_fail (GNC_IS_SEARCH_PARAM(param));

    GNCSearchParamPrivate *priv = GNC_SEARCH_PARAM_GET_PRIVATE(param);
    priv->title = title;
}

GtkJustification
gnc_search_param_get_justify (GNCSearchParam *param)
{
    g_return_val_if_fail (GNC_IS_SEARCH_PARAM(param), GTK_JUSTIFY_LEFT);

    GNCSearchParamPrivate *priv = GNC_SEARCH_PARAM_GET_PRIVATE(param);
    return priv->justify;
}

void
gnc_search_param_set_justify (GNCSearchParam *param, GtkJustification justify)
{
    g_return_if_fail (GNC_IS_SEARCH_PARAM(param));

    GNCSearchParamPrivate *priv = GNC_SEARCH_PARAM_GET_PRIVATE(param);
    priv->justify = justify;
}

gboolean
gnc_search_param_get_passive (GNCSearchParam *param)
{
    g_return_val_if_fail (GNC_IS_SEARCH_PARAM(param), FALSE);

    GNCSearchParamPrivate *priv = GNC_SEARCH_PARAM_GET_PRIVATE(param);
    return priv->passive;
}

void
gnc_search_param_set_passive (GNCSearchParam *param, gboolean value)
{
    g_assert (GNC_IS_SEARCH_PARAM(param));

    GNCSearchParamPrivate *priv = GNC_SEARCH_PARAM_GET_PRIVATE(param);
    priv->passive = value;
}

gboolean
gnc_search_param_get_non_resizeable (GNCSearchParam *param)
{
    g_return_val_if_fail (GNC_IS_SEARCH_PARAM(param), FALSE);

    GNCSearchParamPrivate *priv = GNC_SEARCH_PARAM_GET_PRIVATE(param);
    return priv->non_resizeable;
}

void
gnc_search_param_set_non_resizeable (GNCSearchParam *param, gboolean value)
{
    g_assert (GNC_IS_SEARCH_PARAM(param));

    GNCSearchParamPrivate *priv = GNC_SEARCH_PARAM_GET_PRIVATE(param);
    priv->non_resizeable = value;
}

gboolean
gnc_search_param_type_match (GNCSearchParam *a, GNCSearchParam *b)
{
    GNCSearchParamPrivate *a_priv, *b_priv;

    g_return_val_if_fail (GNC_IS_SEARCH_PARAM(a), FALSE);
    g_return_val_if_fail (GNC_IS_SEARCH_PARAM(b), FALSE);

    a_priv = GNC_SEARCH_PARAM_GET_PRIVATE(a);
    b_priv = GNC_SEARCH_PARAM_GET_PRIVATE(b);

    if (a_priv->type == b_priv->type ||
            !g_strcmp0 (a_priv->type, b_priv->type))
        return TRUE;

    return FALSE;
}

static GList *
gnc_search_param_prepend_internal (GList *list, char const *title,
                                   GtkJustification justify,
                                   QofIdTypeConst type_override,
                                   QofIdTypeConst search_type,
                                   const char *param, va_list args)
{
    GNCSearchParamSimple *p;
    GSList               *path = NULL;
    const char           *this_param;

    p = gnc_search_param_simple_new ();
    gnc_search_param_set_title (GNC_SEARCH_PARAM(p), title);
    gnc_search_param_set_justify (GNC_SEARCH_PARAM(p), justify);

    for (this_param = param; this_param;
            this_param = va_arg (args, const char *))
    {
        path = g_slist_prepend (path, (gpointer)this_param);
    }

    /* put the path into the right order, and set it */
    path = g_slist_reverse (path);
    gnc_search_param_set_param_path (p, search_type, path);

    /* Maybe over-ride the type */
    if (type_override)
        gnc_search_param_override_param_type (p, type_override);

    /* And return it */
    return g_list_prepend (list, p);
}


GList *
gnc_search_param_prepend_with_justify (GList *list, char const *title,
                                       GtkJustification justify,
                                       QofIdTypeConst type_override,
                                       QofIdTypeConst search_type,
                                       const char *param, ...)
{
    GList  *result;
    va_list ap;

    g_return_val_if_fail (title, list);
    g_return_val_if_fail (search_type, list);
    g_return_val_if_fail (param, list);

    /* Build the parameter path */
    va_start (ap, param);
    result = gnc_search_param_prepend_internal (list, title, justify,
                                                type_override, search_type,
                                                param, ap);
    va_end (ap);
    return result;
}

GList *
gnc_search_param_prepend (GList *list, char const *title,
                          QofIdTypeConst type_override,
                          QofIdTypeConst search_type,
                          const char *param, ...)
{
    GList  *result;
    va_list ap;

    g_return_val_if_fail (title, list);
    g_return_val_if_fail (search_type, list);
    g_return_val_if_fail (param, list);

    /* Build the parameter path */
    va_start (ap, param);
    result = gnc_search_param_prepend_internal (list, title, GTK_JUSTIFY_LEFT,
                                                type_override, search_type,
                                                param, ap);
    va_end (ap);
    return result;
}

GList *
gnc_search_param_prepend_compound (GList *list, char const *title,
                                   GList *param_list,
                                   GtkJustification justify,
                                   GNCSearchParamKind kind)
{
    GList                         *p;
    QofIdTypeConst                 type = NULL;
    GNCSearchParamCompound        *param;
    GNCSearchParamPrivate         *basepriv;

    g_return_val_if_fail (title, list);
    g_return_val_if_fail (param_list, list);
    g_return_val_if_fail (kind == SEARCH_PARAM_ANY || kind == SEARCH_PARAM_ALL, list);

    /* "param_list" is a list of GNCSearchParamSimple.  Make sure all the types are the same */
    for (p = param_list; p; p = p->next)
    {
        GNCSearchParam *baseparam;
        g_return_val_if_fail (GNC_IS_SEARCH_PARAM(p->data), list);
        baseparam = GNC_SEARCH_PARAM(p->data);
        if (!type)
            type = gnc_search_param_get_param_type (baseparam);
        else
            g_return_val_if_fail (g_strcmp0 (type, gnc_search_param_get_param_type (baseparam)) == 0, list);
    }

    param = gnc_search_param_compound_new ();
    gnc_search_param_set_title (GNC_SEARCH_PARAM(param), title);
    gnc_search_param_set_justify (GNC_SEARCH_PARAM(param), justify);

    basepriv = GNC_SEARCH_PARAM_GET_PRIVATE(param);
    param->sub_search = g_list_copy (param_list);
    basepriv->type = type;
    param->kind = kind;

    return g_list_prepend (list, param);
}

void
gnc_search_param_set_param_fcn (GNCSearchParamSimple *param,
                                QofIdTypeConst param_type,
                                GNCSearchParamFcn fcn,
                                gpointer arg)
{
    g_return_if_fail (param);
    g_return_if_fail (param_type && *param_type);
    g_return_if_fail (fcn);
    g_return_if_fail (GNC_IS_SEARCH_PARAM_SIMPLE(param));

    param->lookup_fcn = fcn;
    param->lookup_arg = arg;
    gnc_search_param_override_param_type (param, param_type);
}

gboolean
gnc_search_param_has_param_fcn (GNCSearchParamSimple *param)
{
    g_return_val_if_fail (param, FALSE);
    g_return_val_if_fail (GNC_IS_SEARCH_PARAM_SIMPLE(param), FALSE);

    if (param->lookup_fcn)
        return TRUE;

    return FALSE;
}

/* Compute the value of this parameter for this object */
gpointer
gnc_search_param_compute_value (GNCSearchParamSimple *param, gpointer object)
{
    g_return_val_if_fail (param, NULL);
    g_return_val_if_fail (GNC_IS_SEARCH_PARAM_SIMPLE(param), NULL);

    if (param->lookup_fcn)
    {
        return ((param->lookup_fcn)(object, param->lookup_arg));
    }
    else
    {
        GSList *converters = gnc_search_param_get_converters (param);
        gpointer res = object;

        /* Do all the object conversions */
        for (; converters; converters = converters->next)
        {
            QofParam *qp = converters->data;
            res = (qp->param_getfcn) (res, qp);
        }
        return res;
    }
}
