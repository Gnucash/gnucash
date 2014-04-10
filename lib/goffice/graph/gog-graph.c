/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * go-graph.c :
 *
 * Copyright (C) 2003-2004 Jody Goldberg (jody@gnome.org)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of version 2 of the GNU General Public
 * License as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
 * USA
 */

#include <goffice/goffice-config.h>
#include <goffice/graph/gog-graph-impl.h>
#include <goffice/graph/gog-chart-impl.h>
#include <goffice/graph/gog-view.h>
#include <goffice/graph/gog-renderer.h>
#include <goffice/graph/gog-style.h>
#include <goffice/graph/gog-theme.h>
#include <goffice/graph/go-data.h>

#include <gsf/gsf-impl-utils.h>
#include <glib/gi18n.h>
#include <string.h>
#include <stdlib.h>

enum {
	GRAPH_PROP_0,
	GRAPH_PROP_THEME,
	GRAPH_PROP_THEME_NAME
};

enum {
	GRAPH_ADD_DATA,
	GRAPH_REMOVE_DATA,
	GRAPH_LAST_SIGNAL
};
static gulong gog_graph_signals [GRAPH_LAST_SIGNAL] = { 0, };
static GObjectClass *graph_parent_klass;
static GogViewClass *gview_parent_klass;

static void
gog_graph_set_property (GObject *obj, guint param_id,
			GValue const *value, GParamSpec *pspec)
{
	GogGraph *graph = GOG_GRAPH (obj);

	switch (param_id) {
	case GRAPH_PROP_THEME :
		gog_graph_set_theme (graph, g_value_get_object (value));
		break;
	case GRAPH_PROP_THEME_NAME :
		gog_graph_set_theme (graph,
			gog_theme_lookup (g_value_get_string (value)));
		break;

	default: G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, param_id, pspec);
		 return; /* NOTE : RETURN */
	}
}

static void
gog_graph_get_property (GObject *obj, guint param_id,
			GValue *value, GParamSpec *pspec)
{
	GogGraph *graph = GOG_GRAPH (obj);

	switch (param_id) {
	case GRAPH_PROP_THEME :
		g_value_set_object (value, graph->theme);
		break;
	case GRAPH_PROP_THEME_NAME :
		g_value_set_string (value, gog_theme_get_name (graph->theme));
		break;

	default: G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, param_id, pspec);
		 break;
	}
}

static void
gog_graph_finalize (GObject *obj)
{
	GogGraph *graph = GOG_GRAPH (obj);
	GSList *tmp;

	tmp = graph->data;
	graph->data = NULL;
	g_slist_foreach (tmp, (GFunc) g_object_unref, NULL);
	g_slist_free (tmp);

	/* on exit the role remove routines are not called */
	g_slist_free (graph->charts);

	if (graph->idle_handler != 0) {
		g_source_remove (graph->idle_handler);
		graph->idle_handler = 0;
	}

	(graph_parent_klass->finalize) (obj);
}

static char const *
gog_graph_type_name (GogObject const *gobj)
{
	return N_("Graph");
}

static void
role_chart_post_add (GogObject *parent, GogObject *chart)
{
	GogGraph *graph = GOG_GRAPH (parent);
	graph->charts = g_slist_prepend (graph->charts, chart);
	gog_chart_set_position (GOG_CHART (chart),
		0, GOG_GRAPH (graph)->num_rows, 1, 1);
}

static void
role_chart_pre_remove (GogObject *parent, GogObject *child)
{
	GogGraph *graph = GOG_GRAPH (parent);
	GogChart *chart = GOG_CHART (child);

	graph->charts = g_slist_remove (graph->charts, chart);
	gog_graph_validate_chart_layout (graph);
}

static void
gog_graph_update (GogObject *obj)
{
	GogGraph *graph = GOG_GRAPH (obj);
	if (graph->idle_handler != 0) {
		g_source_remove (graph->idle_handler);
		graph->idle_handler = 0;
	}
}

static void
gog_graph_class_init (GogGraphClass *klass)
{
	GObjectClass *gobject_klass   = (GObjectClass *) klass;
	GogObjectClass *gog_klass = (GogObjectClass *) klass;

	static GogObjectRole const roles[] = {
		{ N_("Chart"), "GogChart",	0,
		  GOG_POSITION_SPECIAL, GOG_POSITION_SPECIAL, GOG_OBJECT_NAME_BY_ROLE,
		  NULL, NULL, NULL, role_chart_post_add, role_chart_pre_remove, NULL },
		{ N_("Title"), "GogLabel",	0,
		  GOG_POSITION_COMPASS, GOG_POSITION_N|GOG_POSITION_ALIGN_CENTER, GOG_OBJECT_NAME_BY_ROLE,
		  NULL, NULL, NULL, NULL, NULL, NULL },
	};

	graph_parent_klass = g_type_class_peek_parent (klass);
	gobject_klass->set_property = gog_graph_set_property;
	gobject_klass->get_property = gog_graph_get_property;
	gobject_klass->finalize	    = gog_graph_finalize;

	gog_klass->update	= gog_graph_update;
	gog_klass->type_name	= gog_graph_type_name;
	gog_klass->view_type	= gog_graph_view_get_type ();
	gog_object_register_roles (gog_klass, roles, G_N_ELEMENTS (roles));

	gog_graph_signals [GRAPH_ADD_DATA] = g_signal_new ("add-data",
		G_TYPE_FROM_CLASS (klass),
		G_SIGNAL_RUN_LAST,
		G_STRUCT_OFFSET (GogGraphClass, add_data),
		NULL, NULL,
		g_cclosure_marshal_VOID__OBJECT,
		G_TYPE_NONE,
		1, G_TYPE_OBJECT);

	gog_graph_signals [GRAPH_REMOVE_DATA] = g_signal_new ("remove-data",
		G_TYPE_FROM_CLASS (klass),
		G_SIGNAL_RUN_LAST,
		G_STRUCT_OFFSET (GogGraphClass, remove_data),
		NULL, NULL,
		g_cclosure_marshal_VOID__OBJECT,
		G_TYPE_NONE,
		1, G_TYPE_OBJECT);

	g_object_class_install_property (gobject_klass, GRAPH_PROP_THEME,
		g_param_spec_object ("theme", "Theme",
			"the theme for elements of the graph",
			GOG_THEME_TYPE, G_PARAM_READWRITE));
	g_object_class_install_property (gobject_klass, GRAPH_PROP_THEME_NAME,
		g_param_spec_string ("theme-name", "ThemeName",
			"the name of the theme for elements of the graph",
			"default", G_PARAM_READWRITE|GOG_PARAM_PERSISTENT));
}

static void
gog_graph_init (GogGraph *graph)
{
	GogStyledObject *gso = GOG_STYLED_OBJECT (graph);

	graph->data = NULL;
	graph->num_cols = graph->num_rows = 0;
	graph->idle_handler = 0;
	graph->theme = gog_theme_lookup (NULL); /* default */

	/* Cheat and assign a name here, graphs will not have parents until we
	 * support graphs in graphs */
	GOG_OBJECT (graph)->user_name = g_strdup (_("Graph"));
	gog_theme_fillin_style (graph->theme,
		gso->style, GOG_OBJECT (graph), 0, TRUE);
	gog_styled_object_apply_theme (gso, gso->style);
}

GSF_CLASS (GogGraph, gog_graph,
	   gog_graph_class_init, gog_graph_init,
	   GOG_OUTLINED_OBJECT_TYPE)

/**
 * gog_graph_validate_chart_layout :
 * @graph : #GogGraph
 *
 * Check the layout of the chart grid and ensure that there are no empty
 * cols or rows, and resize as necessary
 */
gboolean
gog_graph_validate_chart_layout (GogGraph *graph)
{
	GSList *ptr;
	GogChart *chart = NULL;
	unsigned i, max_col, max_row;
	gboolean changed = FALSE;

	g_return_val_if_fail (GOG_GRAPH (graph) != NULL, FALSE);

	/* There won't be many of charts so we do the right thing */

	/* 1) find the max */
	max_col = max_row = 0;
	for (ptr = graph->charts ; ptr != NULL ; ptr = ptr->next) {
		chart = ptr->data;
		if (max_col < (chart->x + chart->cols))
			max_col = (chart->x + chart->cols);
		if (max_row < (chart->y + chart->rows))
			max_row = (chart->y + chart->rows);
	}

	/* 2) see if we need to contract any cols */
	for (i = 0 ; i < max_col ; ) {
		for (ptr = graph->charts ; ptr != NULL ; ptr = ptr->next) {
			chart = ptr->data;
			if (chart->x <= i && i < (chart->x + chart->cols))
				break;
		}
		if (ptr == NULL) {
			changed = TRUE;
			max_col--;
			for (ptr = graph->charts ; ptr != NULL ; ptr = ptr->next) {
				chart = ptr->data;
				if (chart->x > i)
					(chart->x)--;
			}
		} else
			i = chart->x + chart->cols;
	}

	/* 3) see if we need to contract any rows */
	for (i = 0 ; i < max_row ; ) {
		for (ptr = graph->charts ; ptr != NULL ; ptr = ptr->next) {
			chart = ptr->data;
			if (chart->y <= i && i < (chart->y + chart->rows))
				break;
		}
		if (ptr == NULL) {
			changed = TRUE;
			max_row--;
			for (ptr = graph->charts ; ptr != NULL ; ptr = ptr->next) {
				chart = ptr->data;
				if (chart->y > i)
					(chart->y)--;
			}
		} else
			i = chart->y + chart->rows;
	}
	changed |= (graph->num_cols != max_col || graph->num_rows != max_row);

	graph->num_cols = max_col;
	graph->num_rows = max_row;

	if (changed)
		gog_object_emit_changed (GOG_OBJECT (graph), TRUE);
	return changed;
}

unsigned
gog_graph_num_cols (GogGraph const *graph)
{
	g_return_val_if_fail (GOG_GRAPH (graph) != NULL, 1);
	return graph->num_cols;
}

unsigned
gog_graph_num_rows (GogGraph const *graph)
{
	g_return_val_if_fail (GOG_GRAPH (graph) != NULL, 1);
	return graph->num_rows;
}

/**
 * gog_graph_dup :
 * @graph : #GogGraph
 *
 * A convenience wrapper to make a deep copy of @graph.
 **/
GogGraph *
gog_graph_dup (GogGraph const *graph)
{
	GogObject *res = gog_object_dup (GOG_OBJECT (graph), NULL);
	return GOG_GRAPH (res);
}

GogTheme *
gog_graph_get_theme (GogGraph const *graph)
{
	g_return_val_if_fail (GOG_GRAPH (graph) != NULL, NULL);
	return graph->theme;
}

void
gog_graph_set_theme (GogGraph *graph, GogTheme *theme)
{
	g_return_if_fail (GOG_GRAPH (graph) != NULL);
	g_return_if_fail (GOG_THEME (theme) != NULL);
#ifdef GOG_WARN_TODO
#warning TODO
#endif
}

/**
 * gog_graph_get_data :
 * @graph : #GogGraph
 *
 * Returns a list of the GOData objects that are data to the graph.
 * The caller should _not_ modify or free the list.
 **/
GSList *
gog_graph_get_data (GogGraph const *graph)
{
	g_return_val_if_fail (GOG_GRAPH (graph) != NULL, NULL);
	return graph->data;
}

/**
 * gog_graph_ref_data :
 * @graph : #GogGraph
 * @dat : #GOData
 *
 * If @dat or something equivalent to it already exists in the graph use that.
 * Otherwaise use @dat.  Adds a gobject ref to the target and increments a
 * count of the number of refs made from this #GogGraph.
 **/
GOData *
gog_graph_ref_data (GogGraph *graph, GOData *dat)
{
	GObject *g_obj;
	gpointer res;
	unsigned count;

	if (dat == NULL)
		return NULL;

	g_return_val_if_fail (GOG_GRAPH (graph) != NULL, dat);
	g_return_val_if_fail (GO_DATA (dat) != NULL, dat);

	/* Does it already exist in the graph ? */
	g_obj = G_OBJECT (graph);
	res = g_object_get_qdata (g_obj, (GQuark)dat);
	if (res == NULL) {

		/* is there something like it already */
		GSList *existing = graph->data; 
		for (; existing != NULL ; existing = existing->next)
			if (go_data_eq (dat, existing->data))
				break;

		if (existing == NULL) {
			g_signal_emit (g_obj, gog_graph_signals [GRAPH_ADD_DATA], 0, dat);
			graph->data = g_slist_prepend (graph->data, dat);
			g_object_ref (dat);
		} else {
			dat = existing->data;
			res = g_object_get_qdata (g_obj, (GQuark)dat);
		}
	}

	count = GPOINTER_TO_UINT (res) + 1;
	g_object_set_qdata (g_obj, (GQuark)dat, GUINT_TO_POINTER (count));
	g_object_ref (dat);
	return dat;
}

/**
 * gog_graph_unref_data :
 * @graph : #GogGraph
 * @dat : #GOData
 *
 **/
void
gog_graph_unref_data (GogGraph *graph, GOData *dat)
{
	GObject *g_obj;
	gpointer res;
	unsigned count;

	if (dat == NULL)
		return;

	g_return_if_fail (GO_DATA (dat) != NULL);

	g_object_unref (dat);

	if (graph == NULL)
		return;

	g_return_if_fail (GOG_GRAPH (graph) != NULL);

	/* once we've been destroyed the list is gone */
	if (graph->data == NULL)
		return;

	g_obj = G_OBJECT (graph);
	res = g_object_get_qdata (g_obj, (GQuark)dat);

	g_return_if_fail (res != NULL);

	count = GPOINTER_TO_UINT (res);
	if (count-- <= 1) {
		/* signal before removing in case that unrefs */
		g_signal_emit (G_OBJECT (graph),
			gog_graph_signals [GRAPH_REMOVE_DATA], 0, dat);
		graph->data = g_slist_remove (graph->data, dat);
		g_object_unref (dat);
		g_object_set_qdata (g_obj, (GQuark)dat, NULL);
	} else
		/* store the decremented count */
		g_object_set_qdata (g_obj, (GQuark)dat, GUINT_TO_POINTER (count));
}

static gboolean
cb_graph_idle (GogGraph *graph)
{
	/* an update may queue an update in a different object,
	 * clear the handler early */
	graph->idle_handler = 0;
	gog_object_update (GOG_OBJECT (graph));
	return FALSE;
}

/**
 * gog_graph_request_update :
 * @graph : #GogGraph
 *
 * queue an update if one had not already be queued.
 **/
gboolean
gog_graph_request_update (GogGraph *graph)
{
	/* people may try to queue an update during destruction */
	if (G_OBJECT (graph)->ref_count <= 0)
		return FALSE;

	g_return_val_if_fail (GOG_GRAPH (graph) != NULL, FALSE);

	if (graph->idle_handler == 0) { /* higher priority than canvas */
		graph->idle_handler = g_idle_add_full (G_PRIORITY_HIGH_IDLE,
			(GSourceFunc) cb_graph_idle, graph, NULL);
		return TRUE;
	}
	return FALSE;
}

/**
 * gog_graph_force_update :
 * @graph : #GogGraph
 *
 * Do an update now if one has been queued.
 **/
void
gog_graph_force_update (GogGraph *graph)
{
	/* people may try to queue an update during destruction */
	if (G_OBJECT (graph)->ref_count > 0 && graph->idle_handler != 0) {
		g_source_remove (graph->idle_handler);
		graph->idle_handler = 0;
		gog_object_update (GOG_OBJECT (graph));
	}
}

/************************************************************************/

typedef GogOutlinedView		GogGraphView;
typedef GogOutlinedViewClass	GogGraphViewClass;

#define GOG_GRAPH_VIEW_TYPE	(gog_graph_view_get_type ())
#define GOG_GRAPH_VIEW(o)	(G_TYPE_CHECK_INSTANCE_CAST ((o), GOG_GRAPH_VIEW_TYPE, GogGraphView))
#define IS_GOG_GRAPH_VIEW(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOG_GRAPH_VIEW_TYPE))

enum {
	GRAPH_VIEW_PROP_0,
	GRAPH_VIEW_PROP_RENDERER
};

static void
gog_graph_view_set_property (GObject *gobject, guint param_id,
			     GValue const *value, GParamSpec *pspec)
{
	GogView *view = GOG_VIEW (gobject);

	switch (param_id) {
	case GRAPH_VIEW_PROP_RENDERER:
		g_return_if_fail (view->renderer == NULL);
		view->renderer = GOG_RENDERER (g_value_get_object (value));
		break;

	default: G_OBJECT_WARN_INVALID_PROPERTY_ID (gobject, param_id, pspec);
		return; /* NOTE : RETURN */
	}
}

static void
gog_graph_view_size_allocate (GogView *view, GogViewAllocation const *a)
{
	GSList *ptr;
	double h, w;
	unsigned x, y, rows, cols;
	GogView *child;
	GogGraph *graph = GOG_GRAPH (view->model);
	GogViewAllocation tmp, res = *a;

	(gview_parent_klass->size_allocate) (view, &res);

	if (gog_graph_num_cols (graph) <= 0 ||
	    gog_graph_num_rows (graph) <= 0)
		return;

	res = view->residual;
	w = res.w / gog_graph_num_cols (graph);
	h = res.h / gog_graph_num_rows (graph);
	for (ptr = view->children; ptr != NULL ; ptr = ptr->next) {
		child = ptr->data;
		if (child->model->position == GOG_POSITION_SPECIAL) {
			gog_chart_get_position (GOG_CHART (child->model),
				&x, &y, &cols, &rows);
			tmp.x = x * w + res.x;
			tmp.y = y * h + res.y;
			tmp.w = cols * w;
			tmp.h = rows * h;
			gog_view_size_allocate (child, &tmp);
		}
	}
}

static void
gog_graph_view_class_init (GogGraphViewClass *gview_klass)
{
	GogViewClass *view_klass    = (GogViewClass *) gview_klass;
	GObjectClass *gobject_klass = (GObjectClass *) view_klass;

	gview_parent_klass = g_type_class_peek_parent (gview_klass);
	gobject_klass->set_property = gog_graph_view_set_property;
	view_klass->size_allocate   = gog_graph_view_size_allocate;

	g_object_class_install_property (gobject_klass, GRAPH_VIEW_PROP_RENDERER,
		g_param_spec_object ("renderer", "renderer",
			"the renderer for this view",
			GOG_RENDERER_TYPE, G_PARAM_WRITABLE));
}

GSF_CLASS (GogGraphView, gog_graph_view,
	   gog_graph_view_class_init, NULL,
	   GOG_OUTLINED_VIEW_TYPE)
