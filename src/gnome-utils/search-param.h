/*
 * search-param.h -- a container for a Search Parameter 
 * Copyright (C) 2002 Derek Atkins <warlord@MIT.EDU
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

struct _GNCSearchParam {
  GObject parent;
  struct _GNCSearchParamPrivate *priv;

  const char *		title;
  GtkJustification	justify;
};

struct _GNCSearchParamClass {
  GObjectClass parent_class;

  /* virtual methods */

  /* signals */
};

/* These are internal functions */
GType			gnc_search_param_get_type (void);

/* Create a new search param */
GNCSearchParam *	gnc_search_param_new (void);
GNCSearchParam *	gnc_search_param_clone (GNCSearchParam *param);

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
					  

#endif /* _GNCSEARCH_PARAM_H */
