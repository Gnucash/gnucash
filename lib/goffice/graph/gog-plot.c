/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-plot.c :
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
#include <goffice/graph/gog-plot-impl.h>
#include <goffice/graph/gog-plot-engine.h>
#include <goffice/graph/gog-series-impl.h>
#include <goffice/graph/gog-chart.h>
#include <goffice/graph/gog-axis.h>
#include <goffice/graph/gog-style.h>
#include <goffice/graph/gog-theme.h>
#include <goffice/graph/gog-graph.h>
#include <goffice/graph/gog-object-xml.h>
#include <goffice/graph/go-data.h>
#include <goffice/utils/go-math.h>
#include <glib/gi18n.h>

#include <gsf/gsf-impl-utils.h>

#define GOG_PLOT_GET_CLASS(o)	(G_TYPE_INSTANCE_GET_CLASS ((o), GOG_PLOT_TYPE, GogPlotClass))

enum {
	PLOT_PROP_0,
	PLOT_PROP_VARY_STYLE_BY_ELEMENT
};

static GObjectClass *plot_parent_klass;

static void
gog_plot_finalize (GObject *obj)
{
	GogPlot *plot = GOG_PLOT (obj);

	g_slist_free (plot->series); /* GogObject does the unref */

	gog_plot_axis_clear (plot, GOG_AXIS_SET_ALL); /* just in case */

	(*plot_parent_klass->finalize) (obj);
}

static gboolean
role_series_can_add (GogObject const *parent)
{
	GogPlot *plot = GOG_PLOT (parent);
	return g_slist_length (plot->series) < plot->desc.num_series_max;
}
static gboolean
role_series_can_remove (GogObject const *child)
{
	GogPlot const *plot = GOG_PLOT (child->parent);
	return g_slist_length (plot->series) > plot->desc.num_series_min;
}

static GogObject *
role_series_allocate (GogObject *plot)
{
	GogPlotClass *klass = GOG_PLOT_GET_CLASS (plot);
	GType type = klass->series_type;

	if (type == 0)
		type = GOG_SERIES_TYPE;
	return g_object_new (type, NULL);
}

static void
role_series_post_add (GogObject *parent, GogObject *child)
{
	GogPlot *plot = GOG_PLOT (parent);
	GogSeries *series = GOG_SERIES (child);
	unsigned num_dim;
	num_dim = plot->desc.series.num_dim;

	/* Alias things so that dim -1 is valid */
	series->values = g_new0 (GogDatasetElement, num_dim+1) + 1;
	series->plot = plot;

	/* if there are other series associated with the plot, and there are 
	 * shared dimensions, clone them over.  */
	if (series->plot->series != NULL) {
		GogGraph *graph = gog_object_get_graph (GOG_OBJECT (plot));
		GogSeriesDesc const *desc = &plot->desc.series;
		GogSeries const *src = plot->series->data;
		unsigned i;

		for (i = num_dim; i-- > 0 ; ) /* name is never shared */
			if (desc->dim[i].is_shared)
				gog_dataset_set_dim_internal (GOG_DATASET (series),
					i, src->values[i].data, graph);

		gog_series_check_validity (series);
	}

	/* APPEND to keep order, there won't be that many */
	plot->series = g_slist_append (plot->series, series);
	gog_plot_request_cardinality_update (plot);
}

static void
role_series_pre_remove (GogObject *parent, GogObject *series)
{
	GogPlot *plot = GOG_PLOT (parent);
	plot->series = g_slist_remove (plot->series, series);
	gog_plot_request_cardinality_update (plot);
}

static void
gog_plot_set_property (GObject *obj, guint param_id,
		       GValue const *value, GParamSpec *pspec)
{
	GogPlot *plot = GOG_PLOT (obj);
	gboolean b_tmp;

	switch (param_id) {
	case PLOT_PROP_VARY_STYLE_BY_ELEMENT:
		b_tmp = g_value_get_boolean (value) &&
			gog_plot_supports_vary_style_by_element (plot);
		if (plot->vary_style_by_element ^ b_tmp) {
			plot->vary_style_by_element = b_tmp;
			gog_plot_request_cardinality_update (plot);
		}
		break;

	default: G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, param_id, pspec);
		 return; /* NOTE : RETURN */
	}
}

static void
gog_plot_get_property (GObject *obj, guint param_id,
		       GValue *value, GParamSpec *pspec)
{
	GogPlot *plot = GOG_PLOT (obj);
	switch (param_id) {
	case PLOT_PROP_VARY_STYLE_BY_ELEMENT:
		g_value_set_boolean (value,
			plot->vary_style_by_element &&
			gog_plot_supports_vary_style_by_element (plot));
		break;

	default: G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, param_id, pspec);
		 break;
	}
}

static void
gog_plot_children_reordered (GogObject *obj)
{
	GSList *ptr, *accum = NULL;
	GogPlot *plot = GOG_PLOT (obj);

	for (ptr = obj->children; ptr != NULL ; ptr = ptr->next)
		if (IS_GOG_SERIES (ptr->data))
			accum = g_slist_prepend (accum, ptr->data);
	g_slist_free (plot->series);
	plot->series = g_slist_reverse (accum);

	gog_plot_request_cardinality_update (plot);
}

static void
gog_plot_class_init (GogObjectClass *gog_klass)
{
	static GogObjectRole const roles[] = {
		{ N_("Series"), "GogSeries",	0,
		  GOG_POSITION_SPECIAL, GOG_POSITION_SPECIAL, GOG_OBJECT_NAME_BY_ROLE,
		  role_series_can_add, role_series_can_remove,
		  role_series_allocate,
		  role_series_post_add, role_series_pre_remove, NULL },
	};
	GObjectClass *gobject_klass = (GObjectClass *) gog_klass;

	plot_parent_klass = g_type_class_peek_parent (gog_klass);
	gobject_klass->finalize		= gog_plot_finalize;
	gobject_klass->set_property	= gog_plot_set_property;
	gobject_klass->get_property	= gog_plot_get_property;
	g_object_class_install_property (gobject_klass, PLOT_PROP_VARY_STYLE_BY_ELEMENT,
		g_param_spec_boolean ("vary_style_by_element", "vary_style_by_element",
			"Use a different style for each segments",
			FALSE,
			G_PARAM_READWRITE|GOG_PARAM_PERSISTENT|GOG_PARAM_FORCE_SAVE));

	gog_klass->children_reordered = gog_plot_children_reordered;
	gog_object_register_roles (gog_klass, roles, G_N_ELEMENTS (roles));
}

static void
gog_plot_init (GogPlot *plot, GogPlotClass const *derived_plot_klass)
{
	/* keep a local copy so that we can over-ride things if desired */
	plot->desc = derived_plot_klass->desc;
	/* start as true so that we can queue an update when it changes */
	plot->cardinality_valid = TRUE;
}

GSF_CLASS_ABSTRACT (GogPlot, gog_plot,
		    gog_plot_class_init, gog_plot_init,
		    GOG_OBJECT_TYPE)

GogPlot *
gog_plot_new_by_type (GogPlotType const *type)
{
	GogPlot *res;

	g_return_val_if_fail (type != NULL, NULL);

	res = gog_plot_new_by_name (type->engine);
	if (res != NULL && type->properties != NULL)
		g_hash_table_foreach (type->properties,
			(GHFunc) gog_object_set_arg, res);
	return res;
}

/**
 * gog_plot_make_similar :
 * @dst :
 * @src :
 *
 * As much as possible have @dst use similar formatting and data allocation to
 * @src.
 *
 * return TRUE on failue
 **/
gboolean
gog_plot_make_similar (GogPlot *dst, GogPlot const *src)
{
	g_return_val_if_fail (GOG_PLOT (dst) != NULL, TRUE);
	g_return_val_if_fail (GOG_PLOT (src) != NULL, TRUE);

	return FALSE;
}

/* convenience routines */
GogSeries *
gog_plot_new_series (GogPlot *plot)
{
	GogObject *res = gog_object_add_by_name (GOG_OBJECT (plot), "Series", NULL);
	return res ? GOG_SERIES (res) : NULL;
}
GogPlotDesc const *
gog_plot_description (GogPlot const *plot)
{
	g_return_val_if_fail (GOG_PLOT (plot) != NULL, NULL);
	return &plot->desc;
}

static GogChart *
gog_plot_get_chart (GogPlot const *plot)
{
	return GOG_CHART (GOG_OBJECT (plot)->parent);
}

/* protected */
void
gog_plot_request_cardinality_update (GogPlot *plot)
{
	g_return_if_fail (GOG_PLOT (plot) != NULL);

	if (plot->cardinality_valid) {
		GogChart *chart = gog_plot_get_chart (plot);
		plot->cardinality_valid = FALSE;
		gog_object_request_update (GOG_OBJECT (plot));
		if (chart != NULL)
			gog_chart_request_cardinality_update (chart);
	}
}

/**
 * gog_plot_get_cardinality :
 * @plot : #GogPlot
 *
 * Return the number of logical elements in the plot, updating the cache if
 * necessary
 **/
void
gog_plot_get_cardinality (GogPlot *plot, unsigned *full, unsigned *visible)
{
	g_return_if_fail (GOG_PLOT (plot) != NULL);

	if (!plot->cardinality_valid) {
		GogSeries *series;
		GSList	  *ptr;
		gboolean   is_valid;
		unsigned   size = 0, no_legend = 0, i;

		plot->cardinality_valid = TRUE;
		gog_chart_get_cardinality (gog_plot_get_chart (plot), NULL, &i);
		plot->index_num = i;

		for (ptr = plot->series; ptr != NULL ; ptr = ptr->next) {
			series = GOG_SERIES (ptr->data);
			is_valid = gog_series_is_valid (GOG_SERIES (series));
			if (plot->vary_style_by_element) {
				if (is_valid && size < series->num_elements)
					size = series->num_elements;
				gog_series_set_index (series, plot->index_num, FALSE);
			} else {
				gog_series_set_index (series, i++, FALSE);
				if (!gog_series_has_legend (series))
					no_legend++;
			}
		}

		plot->full_cardinality =
			plot->vary_style_by_element ? size : (i - plot->index_num);
		plot->visible_cardinality = plot->full_cardinality - no_legend;
	}

	if (full != NULL)
		*full = plot->full_cardinality;
	if (visible != NULL)
		*visible = plot->visible_cardinality;
}

void
gog_plot_foreach_elem (GogPlot *plot, gboolean only_visible,
		       GogEnumFunc func, gpointer data)
{
	GSList *ptr;
	GogSeries const *series;
	GogStyle *style, *tmp_style;
	GODataVector *labels;
	unsigned i, n, num_labels = 0;
	char *label = NULL;
	GogTheme *theme = gog_object_get_theme (GOG_OBJECT (plot));
	GogPlotClass *klass = GOG_PLOT_GET_CLASS (plot);
	GList *overrides;

	g_return_if_fail (GOG_PLOT (plot) != NULL);
	g_return_if_fail (plot->cardinality_valid);

	if (klass->foreach_elem) {
		klass->foreach_elem (plot, only_visible, func, data);
		return;
	}

	ptr = plot->series;
	if (ptr == NULL)
		return;

	if (!plot->vary_style_by_element) {
		unsigned i = plot->index_num;
		for (; ptr != NULL ; ptr = ptr->next)
			if (!only_visible || gog_series_has_legend (ptr->data)) {
				func (i, gog_styled_object_get_style (ptr->data),
				      gog_object_get_name (ptr->data), data);
				i++;
			}
		return;
	}

	series = ptr->data; /* start with the first */
	labels = NULL;
	if (series->values[0].data != NULL) {
		labels = GO_DATA_VECTOR (series->values[0].data);
		num_labels = go_data_vector_get_len (labels);
	}
	style = gog_style_dup (series->base.style);
	n = only_visible ? plot->visible_cardinality : plot->full_cardinality;
	for (overrides = series->overrides, i = 0; i < n ; i++) {
		if (overrides != NULL &&
		    (GOG_SERIES_ELEMENT (overrides->data)->index == i)) {
			tmp_style = GOG_STYLED_OBJECT (overrides->data)->style;
			overrides = overrides->next;
		} else
			tmp_style = style;

		gog_theme_fillin_style (theme, tmp_style, GOG_OBJECT (series),
			plot->index_num + i, FALSE);
		if (labels != NULL)
 			label = (i < num_labels)
 				? go_data_vector_get_str (labels, i) : g_strdup ("");
		else
			label = NULL;
		if (label == NULL)
			label = g_strdup_printf ("%d", i);
		(func) (i, tmp_style, label, data);
		g_free (label);
	}
	g_object_unref (style);
}

/**
 * gog_plot_get_series :
 * @plot : #GogPlot
 *
 * A list of the series in @plot.  Do not modify or free the list.
 **/
GSList const *
gog_plot_get_series (GogPlot const *plot)
{
	g_return_val_if_fail (GOG_PLOT (plot) != NULL, NULL);
	return plot->series;
}

/**
 * gog_plot_get_axis_bounds :
 * @plot : #GogPlot
 * @axis : #GogAxisType
 * @bounds : #GogPlotBoundInfo
 *
 * Queries @plot for its axis preferences for @axis and stores the results in
 * @bounds.  All elements of @bounds are initialized to sane values before the
 * query _ACCEPT_ ::fmt.  The caller is responsible for initializing it.  This
 * is done so that once on plot has selected a format the others need not do
 * the lookup too if so desired.
 *
 * Caller is responsible for unrefing ::fmt.
 **/
GOData *
gog_plot_get_axis_bounds (GogPlot *plot, GogAxisType axis,
			  GogPlotBoundInfo *bounds)
{
	GogPlotClass *klass = GOG_PLOT_GET_CLASS (plot);

	g_return_val_if_fail (klass != NULL, NULL);
	g_return_val_if_fail (bounds != NULL, NULL);

	bounds->val.minima =  DBL_MAX;
	bounds->val.maxima = -DBL_MAX;
	bounds->logical.maxima = go_nan;
	bounds->logical.minima = go_nan;
	bounds->is_discrete = FALSE;
	bounds->center_on_ticks = FALSE;
	if (klass->axis_get_bounds == NULL)
		return NULL;
	return (klass->axis_get_bounds) (plot, axis, bounds);
}

gboolean
gog_plot_supports_vary_style_by_element (GogPlot const *plot)
{
	GogPlotClass *klass = GOG_PLOT_GET_CLASS (plot);

	g_return_val_if_fail (klass != NULL, FALSE);

	if (klass->supports_vary_style_by_element)
		return (klass->supports_vary_style_by_element) (plot);
	return TRUE; /* default */
}

GogAxisSet
gog_plot_axis_set_pref (GogPlot const *plot)
{
	GogPlotClass *klass = GOG_PLOT_GET_CLASS (plot);

	g_return_val_if_fail (klass != NULL, FALSE);
	if (klass->axis_set_pref != NULL)
		return (klass->axis_set_pref) (plot);
	return GOG_AXIS_SET_NONE;
}

gboolean
gog_plot_axis_set_is_valid (GogPlot const *plot, GogAxisSet type)
{
	GogPlotClass *klass = GOG_PLOT_GET_CLASS (plot);

	g_return_val_if_fail (klass != NULL, FALSE);
	if (klass->axis_set_is_valid != NULL)
		return (klass->axis_set_is_valid) (plot, type);
	return type == GOG_AXIS_SET_NONE;
}

gboolean
gog_plot_axis_set_assign (GogPlot *plot, GogAxisSet axis_set)
{
	GogPlotClass *klass = GOG_PLOT_GET_CLASS (plot);
	GogChart const *chart;
	GogAxisType type;

	g_return_val_if_fail (klass != NULL, FALSE);

	chart = gog_plot_get_chart (plot);
	for (type = 0 ; type < GOG_AXIS_TYPES ; type++) {
		if (plot->axis[type] != NULL) {
			if (!(axis_set & (1 << type))) {
				gog_axis_del_contributor (plot->axis[type], GOG_OBJECT (plot));
				plot->axis[type] = NULL;
			}
		} else if (axis_set & (1 << type)) {
			GSList *axes = gog_chart_get_axis (chart, type);
			if (axes != NULL) {
				gog_axis_add_contributor (axes->data, GOG_OBJECT (plot));
				plot->axis[type] = axes->data;
				g_slist_free (axes);
			}
		}
	}

	if (klass->axis_set_assign == NULL)
		return axis_set == GOG_AXIS_SET_NONE;

	return (klass->axis_set_assign) (plot, axis_set);
}

/**
 * gog_plot_axis_clear : 
 * @plot : #GogPlot
 * @filter : #GogAxisSet
 * 
 * A utility method to clear all existing axis associations flagged by @filter
 **/
void
gog_plot_axis_clear (GogPlot *plot, GogAxisSet filter)
{
	GogAxisType type;

	g_return_if_fail (GOG_PLOT (plot) != NULL);

	for (type = 0 ; type < GOG_AXIS_TYPES ; type++)
		if (plot->axis[type] != NULL && ((1 << type) & filter)) {
			gog_axis_del_contributor (plot->axis[type], GOG_OBJECT (plot));
			plot->axis[type] = NULL;
		}
}

GogAxis	*
gog_plot_get_axis (GogPlot const *plot, GogAxisType type)
{
	g_return_val_if_fail (GOG_PLOT (plot) != NULL, NULL);
	g_return_val_if_fail (type < GOG_AXIS_TYPES, NULL);
	g_return_val_if_fail (GOG_AXIS_UNKNOWN < type, NULL);
	return plot->axis[type];
}

/****************************************************************************/
/* a placeholder.  It seems likely that we will want this eventually        */
GSF_CLASS_ABSTRACT (GogPlotView, gog_plot_view,
		    NULL, NULL,
		    GOG_VIEW_TYPE)
