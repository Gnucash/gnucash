/*
 * search-param.c -- a container for a Search Parameter 
 * Copyright (C) 2002 Derek Atkins <warlord@MIT.EDU
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <string.h>
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

    /* And reset for the next parameter */
    type = search_type = objDef->param_type;
  }

  param->priv->type = type;
}

void
gnc_search_param_override_param_type (GNCSearchParam *param,
				      GNCIdTypeConst param_type)
{
  g_assert (IS_GNCSEARCH_PARAM (param));
  g_assert (param_type != NULL && *param_type != '\0');
  param->priv->type = param_type;
}

GSList *
gnc_search_param_get_param_path (GNCSearchParam *param)
{
  g_assert (IS_GNCSEARCH_PARAM (param));

  return param->priv->param_path;
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
