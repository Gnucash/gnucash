/*
 * search-param.c -- a container for a Search Parameter 
 * Copyright (C) 2002 Derek Atkins <warlord@MIT.EDU
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <string.h>
#include <stdarg.h>
#include <gnome.h>

#include "gnc-engine-util.h"
#include "GNCId.h"
#include "QueryObject.h"

#include "search-param.h"

static void gnc_search_param_class_init	(GNCSearchParamClass *class);
static void gnc_search_param_init	(GNCSearchParam *gspaper);
static void gnc_search_param_finalise	(GtkObject *obj);

#define _PRIVATE(x) (((GNCSearchParam *)(x))->priv)
struct _GNCSearchParamPrivate {
  GSList *		converters;
  GSList *		param_path;
  GNCIdTypeConst	type;
};

static GtkObjectClass *parent_class;

enum {
  LAST_SIGNAL
};

static guint signals[LAST_SIGNAL] = { 0 };

guint
gnc_search_param_get_type (void)
{
  static guint type = 0;
	
  if (!type) {
    GtkTypeInfo type_info = {
      "GNCSearchParam",
      sizeof(GNCSearchParam),
      sizeof(GNCSearchParamClass),
      (GtkClassInitFunc)gnc_search_param_class_init,
      (GtkObjectInitFunc)gnc_search_param_init,
      (GtkArgSetFunc)NULL,
      (GtkArgGetFunc)NULL
    };
		
    type = gtk_type_unique(gtk_object_get_type (), &type_info);
  }
	
  return type;
}

static void
gnc_search_param_class_init (GNCSearchParamClass *class)
{
  GtkObjectClass *object_class;
	
  object_class = (GtkObjectClass *)class;
  parent_class = gtk_type_class (gtk_object_get_type ());

  object_class->finalize = gnc_search_param_finalise;

  /* override methods */

  /* signals */

  gtk_object_class_add_signals (object_class, signals, LAST_SIGNAL);
}

static void
gnc_search_param_init (GNCSearchParam *o)
{
  o->priv = g_malloc0 (sizeof (*o->priv));
}

static void
gnc_search_param_finalise (GtkObject *obj)
{
  GNCSearchParam *o = (GNCSearchParam *)obj;
  g_slist_free (o->priv->param_path);
  o->priv->param_path = NULL;
  g_slist_free (o->priv->converters);
  o->priv->converters = NULL;
  g_free(o->priv);
  ((GtkObjectClass *)(parent_class))->finalize(obj);
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
  GNCSearchParam *o = (GNCSearchParam *)gtk_type_new(gnc_search_param_get_type ());
  return o;
}

GNCSearchParam *
gnc_search_param_clone (GNCSearchParam *param)
{
  GNCSearchParam *n;

  g_assert (IS_GNCSEARCH_PARAM (param));

  n = gnc_search_param_new ();
  n->title = param->title;
  n->priv->param_path = g_slist_copy (param->priv->param_path);
  n->priv->type = param->priv->type;

  return n;
}

void
gnc_search_param_set_param_path (GNCSearchParam *param,
				 GNCIdTypeConst search_type,
				 GSList *param_path)
{
  GNCIdTypeConst type = NULL;
  GSList *converters = NULL;

  g_assert (IS_GNCSEARCH_PARAM (param));

  if (param->priv->param_path)
    g_slist_free (param->priv->param_path);
  param->priv->param_path = g_slist_copy (param_path);

  /* Compute the parameter type */
  for (; param_path; param_path = param_path->next) {
    GNCIdType param_name = param_path->data;
    const QueryObjectDef *objDef =
      gncQueryObjectGetParameter (search_type, param_name);

    /* If it doesn't exist, then we've reached the end */
    if (!objDef)
      break;

    /* Save the converter */
    converters = g_slist_prepend (converters, objDef->param_getfcn);

    /* And reset for the next parameter */
    type = search_type = objDef->param_type;
  }

  /* Save the type */
  param->priv->type = type;

  /* Save the converters */
  if (param->priv->converters)
    g_slist_free (param->priv->converters);
  param->priv->converters = g_slist_reverse (converters);
}

void
gnc_search_param_override_param_type (GNCSearchParam *param,
				      GNCIdTypeConst param_type)
{
  g_assert (IS_GNCSEARCH_PARAM (param));
  g_assert (param_type != NULL && *param_type != '\0');
  param->priv->type = param_type;
  /* XXX: What about the converters? */
}

GSList *
gnc_search_param_get_param_path (GNCSearchParam *param)
{
  g_assert (IS_GNCSEARCH_PARAM (param));

  return g_slist_copy (param->priv->param_path);
}

GSList *
gnc_search_param_get_converters (GNCSearchParam *param)
{
  g_assert (IS_GNCSEARCH_PARAM (param));

  return param->priv->converters;
}

GNCIdTypeConst
gnc_search_param_get_param_type (GNCSearchParam *param)
{
  g_assert (IS_GNCSEARCH_PARAM (param));

  return param->priv->type;
}

void
gnc_search_param_set_title (GNCSearchParam *param, const char *title)
{
  g_assert (IS_GNCSEARCH_PARAM (param));

  param->title = title;
}

void
gnc_search_param_set_justify (GNCSearchParam *param, GtkJustification justify)
{
  g_assert (IS_GNCSEARCH_PARAM (param));

  param->justify = justify;
}

gboolean
gnc_search_param_type_match (GNCSearchParam *a, GNCSearchParam *b)
{
  g_assert (IS_GNCSEARCH_PARAM (a));
  g_assert (IS_GNCSEARCH_PARAM (b));

  if (a->priv->type == b->priv->type ||
      !safe_strcmp (a->priv->type, b->priv->type))
    return TRUE;

  return FALSE;
}

static GList *
gnc_search_param_prepend_internal (GList *list, char const *title,
				   GtkJustification justify,
				   GNCIdTypeConst type_override,
				   GNCIdTypeConst search_type,
				   const char *param, va_list args)
{
  GNCSearchParam *p;
  GSList *path = NULL;
  const char *this_param;

  p = gnc_search_param_new ();
  gnc_search_param_set_title (p, title);

  for (this_param = param; this_param;
       this_param = va_arg (args, const char *)) {
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
				       GNCIdTypeConst type_override,
				       GNCIdTypeConst search_type,
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
			  GNCIdTypeConst type_override,
			  GNCIdTypeConst search_type,
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
