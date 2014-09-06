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

static void gnc_search_param_class_init	(GNCSearchParamClass *klass);
static void gnc_search_param_init	(GNCSearchParam *gspaper);
static void gnc_search_param_finalize	(GObject *obj);

typedef struct _GNCSearchParamPrivate	GNCSearchParamPrivate;

struct _GNCSearchParamPrivate
{
    GSList *		converters;
    GSList *		param_path;
    QofIdTypeConst	type;

    GNCSearchParamFcn	lookup_fcn;
    gpointer		lookup_arg;
};

#define GNC_SEARCH_PARAM_GET_PRIVATE(o)  \
   (G_TYPE_INSTANCE_GET_PRIVATE ((o), GNC_TYPE_SEARCH_PARAM, GNCSearchParamPrivate))

static GObjectClass *parent_class;

enum
{
    LAST_SIGNAL
};

#if LAST_SIGNAL > 0
static guint signals[LAST_SIGNAL] = { 0 };
#endif

GType
gnc_search_param_get_type (void)
{
    static GType type = 0;

    if (type == 0)
    {
        static GTypeInfo type_info =
        {
            sizeof(GNCSearchParamClass),
            NULL,
            NULL,
            (GClassInitFunc)gnc_search_param_class_init,
            NULL,
            NULL,
            sizeof(GNCSearchParam),
            0,
            (GInstanceInitFunc)gnc_search_param_init
        };

        type = g_type_register_static (G_TYPE_OBJECT, "GNCSearchParam",
                                       &type_info, 0);
    }

    return type;
}

static void
gnc_search_param_class_init (GNCSearchParamClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS (klass);

    parent_class = g_type_class_peek_parent (klass);

    object_class->finalize = gnc_search_param_finalize;

    g_type_class_add_private(klass, sizeof(GNCSearchParamPrivate));
}

static void
gnc_search_param_init (GNCSearchParam *o)
{
}

static void
gnc_search_param_finalize (GObject *obj)
{
    GNCSearchParam *o;
    GNCSearchParamPrivate *priv;

    g_return_if_fail (obj != NULL);
    g_return_if_fail (GNC_IS_SEARCH_PARAM (obj));

    o = GNC_SEARCH_PARAM (obj);
    priv = GNC_SEARCH_PARAM_GET_PRIVATE(o);

    g_slist_free (priv->param_path);
    priv->param_path = NULL;
    g_slist_free (priv->converters);
    priv->converters = NULL;

    G_OBJECT_CLASS (parent_class)->finalize(obj);
}

/**
 * gnc_search_param_new:
 *
 * Create a new GNCSearchParam object.
 *
 * Return value: A new #GNCSearchParam object.
 **/
GNCSearchParam *
gnc_search_param_new (void)
{
    GNCSearchParam *o = (GNCSearchParam *)g_object_new(gnc_search_param_get_type (), NULL);
    return o;
}

void
gnc_search_param_set_param_path (GNCSearchParam *param,
                                 QofIdTypeConst search_type,
                                 GSList *param_path)
{
    GNCSearchParamPrivate *priv;
    QofIdTypeConst type = NULL;
    GSList *converters = NULL;

    g_return_if_fail (GNC_IS_SEARCH_PARAM (param));

    priv = GNC_SEARCH_PARAM_GET_PRIVATE(param);
    if (priv->param_path)
    {
        g_slist_free (priv->param_path);
    }
    priv->param_path = g_slist_copy (param_path);

    /* Compute the parameter type */
    for (; param_path; param_path = param_path->next)
    {
        QofIdType param_name = param_path->data;
        const QofParam *objDef =
            qof_class_get_parameter (search_type, param_name);

        /* If it doesn't exist, then we've reached the end */
        if (objDef == NULL)
            break;

        /* Save the converter */
        converters = g_slist_prepend (converters, (gpointer) objDef);

        /* And reset for the next parameter */
        type = search_type = objDef->param_type;
    }

    /* Save the type */
    priv->type = type;

    /* Save the converters */
    if (priv->converters)
    {
        g_slist_free (priv->converters);
    }
    priv->converters = g_slist_reverse (converters);
}

void
gnc_search_param_override_param_type (GNCSearchParam *param,
                                      QofIdTypeConst param_type)
{
    GNCSearchParamPrivate *priv;

    g_return_if_fail (GNC_IS_SEARCH_PARAM (param));
    g_return_if_fail (param_type != NULL && *param_type != '\0');

    priv = GNC_SEARCH_PARAM_GET_PRIVATE(param);
    priv->type = param_type;
    /* XXX: What about the converters? */
}

GSList *
gnc_search_param_get_param_path (GNCSearchParam *param)
{
    GNCSearchParamPrivate *priv;

    g_return_val_if_fail (GNC_IS_SEARCH_PARAM (param), NULL);

    priv = GNC_SEARCH_PARAM_GET_PRIVATE(param);
    return g_slist_copy (priv->param_path);
}

GSList *
gnc_search_param_get_converters (GNCSearchParam *param)
{
    GNCSearchParamPrivate *priv;

    g_return_val_if_fail (GNC_IS_SEARCH_PARAM (param), NULL);

    priv = GNC_SEARCH_PARAM_GET_PRIVATE(param);
    return priv->converters;
}

QofIdTypeConst
gnc_search_param_get_param_type (GNCSearchParam *param)
{
    GNCSearchParamPrivate *priv;

    g_return_val_if_fail (GNC_IS_SEARCH_PARAM (param), NULL);

    priv = GNC_SEARCH_PARAM_GET_PRIVATE(param);
    return priv->type;
}

void
gnc_search_param_set_title (GNCSearchParam *param, const char *title)
{
    g_return_if_fail (GNC_IS_SEARCH_PARAM (param));

    param->title = title;
}

void
gnc_search_param_set_justify (GNCSearchParam *param, GtkJustification justify)
{
    g_return_if_fail (GNC_IS_SEARCH_PARAM (param));

    param->justify = justify;
}

void
gnc_search_param_set_passive (GNCSearchParam *param, gboolean value)
{
    g_assert (GNC_IS_SEARCH_PARAM (param));

    param->passive = value;
}

void
gnc_search_param_set_non_resizeable (GNCSearchParam *param, gboolean value)
{
    g_assert (GNC_IS_SEARCH_PARAM (param));

    param->non_resizeable = value;
}

gboolean
gnc_search_param_type_match (GNCSearchParam *a, GNCSearchParam *b)
{
    GNCSearchParamPrivate *a_priv, *b_priv;

    g_return_val_if_fail (GNC_IS_SEARCH_PARAM (a), FALSE);
    g_return_val_if_fail (GNC_IS_SEARCH_PARAM (b), FALSE);

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
    GNCSearchParam *p;
    GSList *path = NULL;
    const char *this_param;

    p = gnc_search_param_new ();
    gnc_search_param_set_title (p, title);
    gnc_search_param_set_justify (p, justify);

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
    GList *result;
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
    GList *result;
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

void
gnc_search_param_set_param_fcn (GNCSearchParam *param,
                                QofIdTypeConst param_type,
                                GNCSearchParamFcn fcn,
                                gpointer arg)
{
    GNCSearchParamPrivate *priv;

    g_return_if_fail (param);
    g_return_if_fail (param_type && *param_type);
    g_return_if_fail (fcn);
    g_return_if_fail (GNC_IS_SEARCH_PARAM(param));

    priv = GNC_SEARCH_PARAM_GET_PRIVATE(param);
    priv->lookup_fcn = fcn;
    priv->lookup_arg = arg;
    gnc_search_param_override_param_type (param, param_type);
}

/* Compute the value of this parameter for this object */
gpointer
gnc_search_param_compute_value (GNCSearchParam *param, gpointer object)
{
    GNCSearchParamPrivate *priv;

    g_return_val_if_fail(param, NULL);
    g_return_val_if_fail(GNC_IS_SEARCH_PARAM(param), NULL);

    priv = GNC_SEARCH_PARAM_GET_PRIVATE(param);
    if (priv->lookup_fcn)
    {
        return ((priv->lookup_fcn)(object, priv->lookup_arg));
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
