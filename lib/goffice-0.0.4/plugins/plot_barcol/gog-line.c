/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-line.c
 *
 * Copyright (C) 2003-2004 Emmanuel Pacaud (emmanuel.pacaud@univ-poitiers.fr)
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
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301
 * USA
 */

#include <goffice/goffice-config.h>
#include "gog-line.h"
#include "gog-1.5d.h"
#include <goffice/graph/gog-series-lines.h>
#include <goffice/graph/gog-view.h>
#include <goffice/graph/gog-chart.h>
#include <goffice/graph/gog-renderer.h>
#include <goffice/graph/gog-style.h>
#include <goffice/graph/gog-axis.h>
#include <goffice/data/go-data.h>
#include <goffice/utils/go-color.h>
#include <goffice/utils/go-marker.h>
#include <goffice/utils/go-math.h>

#include <glib/gi18n.h>
#include <gsf/gsf-impl-utils.h>

struct _GogLinePlot {
	GogPlot1_5d	base;
	gboolean	default_style_has_markers;
};

static GType gog_line_view_get_type (void);

enum {
	GOG_LINE_PROP_0,
	GOG_LINE_PROP_DEFAULT_STYLE_HAS_MARKERS
};

typedef GogSeries1_5d		GogLineSeries;
typedef GogSeries1_5dClass	GogLineSeriesClass;

static GogStyledObjectClass *series_parent_klass;

static void
gog_line_series_init_style (GogStyledObject *gso, GogStyle *style)
{
	GogSeries *series = GOG_SERIES (gso);
	GogLinePlot const *plot;

	series_parent_klass->init_style (gso, style);
	if (series->plot == NULL)
		return;

	plot = GOG_LINE_PLOT (series->plot);
	if (!plot->default_style_has_markers) {
		style->disable_theming |= GOG_STYLE_MARKER;
		if (style->marker.auto_shape) {
			GOMarker *m = go_marker_new ();
			go_marker_set_shape (m, GO_MARKER_NONE);
			gog_style_set_marker (style, m);
		}
	}
}

static void
gog_line_series_class_init (GogStyledObjectClass *gso_klass)
{
	series_parent_klass = g_type_class_peek_parent (gso_klass);
	gso_klass->init_style = gog_line_series_init_style;
}

GType gog_line_series_get_type (void);
GSF_DYNAMIC_CLASS (GogLineSeries, gog_line_series,
	gog_line_series_class_init, NULL,
	GOG_SERIES1_5D_TYPE)

static char const *
gog_line_plot_type_name (G_GNUC_UNUSED GogObject const *item)
{
	/* xgettext : the base for how to name bar/col plot objects
	 * eg The 2nd line plot in a chart will be called
	 * 	PlotLine2
	 */
	return N_("PlotLine");
}

static void
gog_line_update_stacked_and_percentage (GogPlot1_5d *model,
					double **vals, GogErrorBar **errors, unsigned const *lengths)
{
	unsigned i, j;
	double abs_sum, minima, maxima, sum, tmp, errplus, errminus;

	for (i = model->num_elements ; i-- > 0 ; ) {
		abs_sum = sum = 0.;
		minima =  DBL_MAX;
		maxima = -DBL_MAX;
		for (j = 0 ; j < model->num_series ; j++) {
			if (i >= lengths[j])
				continue;
			tmp = vals[j][i];
			if (!go_finite (tmp))
				continue;
		if (gog_error_bar_is_visible (errors[j])) {
				gog_error_bar_get_bounds (errors[j], i, &errminus, &errplus);
				errminus = errminus > 0. ? errminus: 0.;
				errplus = errplus > 0. ? errplus : 0.;
			} else
				errplus = errminus = 0.;
			sum += tmp;
			abs_sum += fabs (tmp);
			if (minima > sum - errminus)
				minima = sum - errminus;
			if (maxima < sum + errplus)
				maxima = sum + errplus;
		}
		if ((model->type == GOG_1_5D_AS_PERCENTAGE) &&
		    (go_sub_epsilon (abs_sum) > 0.)) {
			if (model->minima > minima / abs_sum)
				model->minima = minima / abs_sum;
			if (model->maxima < maxima / abs_sum)
				model->maxima = maxima / abs_sum;
		} else {
			if (model->minima > minima)
				model->minima = minima;
			if (model->maxima < maxima)
				model->maxima = maxima;
		}
	}
}

static void
gog_line_set_property (GObject *obj, guint param_id,
		       GValue const *value, GParamSpec *pspec)
{
	GogLinePlot *line = GOG_LINE_PLOT (obj);
	switch (param_id) {
	case GOG_LINE_PROP_DEFAULT_STYLE_HAS_MARKERS:
		line->default_style_has_markers = g_value_get_boolean (value);
		break;
	default: G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, param_id, pspec);
		 break;
	}
}
static void
gog_line_get_property (GObject *obj, guint param_id,
		       GValue *value, GParamSpec *pspec)
{
	GogLinePlot const *line = GOG_LINE_PLOT (obj);
	switch (param_id) {
	case GOG_LINE_PROP_DEFAULT_STYLE_HAS_MARKERS:
		g_value_set_boolean (value, line->default_style_has_markers);
		break;
	default: G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, param_id, pspec);
		 break;
	}
}

static void
gog_line_plot_class_init (GogPlot1_5dClass *gog_plot_1_5d_klass)
{
	GObjectClass *gobject_klass = (GObjectClass *) gog_plot_1_5d_klass;
	GogObjectClass *gog_klass = (GogObjectClass *) gog_plot_1_5d_klass;
	GogPlotClass *plot_klass = (GogPlotClass *) gog_plot_1_5d_klass;

	gobject_klass->set_property = gog_line_set_property;
	gobject_klass->get_property = gog_line_get_property;

	g_object_class_install_property (gobject_klass, GOG_LINE_PROP_DEFAULT_STYLE_HAS_MARKERS,
		g_param_spec_boolean ("default-style-has-markers", NULL,
			"Should the default style of a series include markers",
			TRUE, G_PARAM_READWRITE | GOG_PARAM_PERSISTENT));

	gog_klass->type_name	= gog_line_plot_type_name;
	gog_klass->view_type	= gog_line_view_get_type ();

	plot_klass->desc.series.style_fields = GOG_STYLE_LINE | GOG_STYLE_MARKER;
	plot_klass->series_type = gog_line_series_get_type ();

	gog_plot_1_5d_klass->update_stacked_and_percentage =
		gog_line_update_stacked_and_percentage;
}

static void
gog_line_plot_init (GogLinePlot *plot)
{
	plot->default_style_has_markers = TRUE;
	GOG_PLOT1_5D (plot)->support_drop_lines = TRUE;
}

GSF_DYNAMIC_CLASS (GogLinePlot, gog_line_plot,
	gog_line_plot_class_init, gog_line_plot_init,
	GOG_PLOT1_5D_TYPE)

/*****************************************************************************/

static char const *
gog_area_plot_type_name (G_GNUC_UNUSED GogObject const *item)
{
	/* xgettext : the base for how to name bar/col plot objects
	 * eg The 2nd line plot in a chart will be called
	 * 	PlotArea2
	 */
	return N_("PlotArea");
}

static void
gog_area_plot_class_init (GogObjectClass *gog_klass)
{
	GogPlotClass *plot_klass = (GogPlotClass *) gog_klass;

	plot_klass->desc.series.style_fields = GOG_STYLE_OUTLINE | GOG_STYLE_FILL;
	plot_klass->series_type = gog_series1_5d_get_type ();

	gog_klass->type_name	= gog_area_plot_type_name;
}

static void
gog_area_plot_init (GogPlot *plot)
{
	plot->render_before_axes = TRUE;
	GOG_PLOT1_5D (plot)->support_drop_lines = TRUE;
}

GSF_DYNAMIC_CLASS (GogAreaPlot, gog_area_plot,
	gog_area_plot_class_init, gog_area_plot_init,
	GOG_LINE_PLOT_TYPE)

/*****************************************************************************/

typedef struct {
	double 		x;
	double 		y;
	double		plus;
	double 		minus;
} ErrorBarData;

typedef GogPlotView		GogLineView;
typedef GogPlotViewClass	GogLineViewClass;

static void
gog_line_view_render (GogView *view, GogViewAllocation const *bbox)
{
	GogPlot1_5d const *model = GOG_PLOT1_5D (view->model);
	GogPlot1_5dType const type = model->type;
	GogSeries1_5d const *series;
	GogChart *chart = GOG_CHART (view->model->parent);
	GogChartMap *chart_map;
	GogViewAllocation const *area;
	unsigned i, j, k;
	unsigned num_elements = model->num_elements;
	unsigned num_series = model->num_series;
	GSList *ptr;
	double plus, minus;

	double **vals;
	ErrorBarData **error_data;
	GogStyle **styles;
	unsigned *lengths;
	ArtVpath **path, **drop_paths;
	GogErrorBar **errors;
	GogObjectRole const *role = NULL;
	GogSeriesLines **lines;

	double y_zero, drop_lines_y_zero;
	double abs_sum, sum, value;
	gboolean is_null, is_area_plot;

	GogAxisMap *x_map, *y_map;

	is_area_plot = GOG_IS_PLOT_AREA (model);

	if (num_elements <= 0 || num_series <= 0)
		return;

	area = gog_chart_view_get_plot_area (view->parent);
	chart_map = gog_chart_map_new (chart, area, 
				       GOG_PLOT (model)->axis[GOG_AXIS_X], 
				       GOG_PLOT (model)->axis[GOG_AXIS_Y],
				       NULL, FALSE);
	if (!gog_chart_map_is_valid (chart_map)) {
		gog_chart_map_free (chart_map);
		return;
	}
	
	x_map = gog_chart_map_get_axis_map (chart_map, 0);
	y_map = gog_chart_map_get_axis_map (chart_map, 1);
	
	/* Draw drop lines from point to axis start. See comment in
	 * GogXYPlotView::render */

	gog_axis_map_get_extents (y_map, &drop_lines_y_zero, NULL); 
	drop_lines_y_zero = gog_axis_map_to_view (y_map, drop_lines_y_zero);
	y_zero = gog_axis_map_get_baseline (y_map); 

	vals    = g_alloca (num_series * sizeof (double *));
	error_data = g_alloca (num_series * sizeof (ErrorBarData *));
	lengths = g_alloca (num_series * sizeof (unsigned));
	styles  = g_alloca (num_series * sizeof (GogStyle *));
	path    = g_alloca (num_series * sizeof (ArtVpath *));
	errors	= g_alloca (num_series * sizeof (GogErrorBar *));
	lines	= g_alloca (num_series * sizeof (GogSeriesLines *));
	drop_paths = g_alloca (num_series * sizeof (ArtVpath *));

	i = 0;
	for (ptr = model->base.series ; ptr != NULL ; ptr = ptr->next) {
		series = ptr->data;

		if (!gog_series_is_valid (GOG_SERIES (series)))
			continue;

		vals[i] = go_data_vector_get_values (
			GO_DATA_VECTOR (series->base.values[1].data));
		lengths[i] = go_data_vector_get_len (
			GO_DATA_VECTOR (series->base.values[1].data));
		styles[i] = GOG_STYLED_OBJECT (series)->style;

		if (!is_area_plot)
			path[i] = g_malloc (sizeof (ArtVpath) * (lengths[i] + 2));
		else if (type == GOG_1_5D_NORMAL)
			path[i] = g_malloc (sizeof (ArtVpath) * (lengths[i] + 5));
		else
			path[i] = g_malloc (sizeof (ArtVpath) * (2 * lengths[i] + 3));

		errors[i] = series->errors;
		if (gog_error_bar_is_visible (series->errors)) 
			error_data[i] = g_malloc (sizeof (ErrorBarData) * lengths[i]);
		else
			error_data[i] = NULL;
		if (series->has_drop_lines) {
			if (!role)
				role = gog_object_find_role_by_name (
							GOG_OBJECT (series), "Drop lines");
			lines[i] = GOG_SERIES_LINES (
					gog_object_get_child_by_role (GOG_OBJECT (series), role));
			drop_paths [i] = g_malloc (sizeof (ArtVpath) * (num_elements * 2 + 1));
			for (j = 0; j < num_elements; j++) {
				drop_paths[i][2 * j].code = ART_MOVETO;
				drop_paths[i][2 * j + 1].code = ART_LINETO;
				drop_paths[i][2 * j + 1].y = drop_lines_y_zero; 
			}
			drop_paths[i][2 * j].code = ART_END;
		} else
			lines[i] = NULL;
		i++;
	}

	for (j = 1; j <= num_elements; j++) {
		sum = abs_sum = 0.0;
		if (type == GOG_1_5D_AS_PERCENTAGE) {
			for (i = 0; i < num_series; i++)
				if (gog_axis_map_finite (y_map, vals[i][j-1]))
					abs_sum += fabs (vals[i][j-1]);
			is_null = (go_sub_epsilon (abs_sum) <= 0.);
		} else
			is_null = TRUE;

		for (i = 0; i < num_series; i++) {
			if (j > lengths[i])
				continue;

			if (vals[i] && gog_axis_map_finite (y_map, vals[i][j-1])) {
				value = vals[i][j-1];
				if (gog_error_bar_is_visible (errors[i])) {
					gog_error_bar_get_bounds (errors[i], j - 1, &minus, &plus);
				}
			} else {
				value = 0.0;
				minus = -1.0;
				plus = -1.;
			}
			k = 2 * lengths[i] - j + 1;

			if (is_area_plot && (type != GOG_1_5D_NORMAL)) {
				path[i][k].x = gog_axis_map_to_view (x_map, j);
				path[i][k].code = ART_LINETO;

				if (type == GOG_1_5D_STACKED)
					path[i][k].y = gog_axis_map_finite (y_map, sum) ?
						gog_axis_map_to_view (y_map, sum):
						y_zero;
				else
					path[i][k].y = is_null ? 
						y_zero :
						(gog_axis_map_finite (y_map, sum) ?
						 gog_axis_map_to_view (y_map, sum / abs_sum) :
						 y_zero);
			}

			path[i][j].x = gog_axis_map_to_view (x_map, j);
			if (type == GOG_1_5D_NORMAL && !is_area_plot) 
				if (gog_axis_map_finite (y_map, vals[i][j-1])) 
					if (j > 1 && path[i][j-1].code == ART_MOVETO_OPEN)
						path[i][j].code = ART_MOVETO;
					else
						path[i][j].code = ART_LINETO;
				else
					path[i][j].code = ART_MOVETO_OPEN;
			else
				path[i][j].code = ART_LINETO;

			sum += value;

			if (gog_error_bar_is_visible (errors[i])) 
				error_data[i][j-1].x = j;

			switch (type) {
				case GOG_1_5D_NORMAL :
					path[i][j].y = gog_axis_map_finite (y_map, value) ?
						gog_axis_map_to_view (y_map, value) :
						y_zero;
					if (gog_error_bar_is_visible (errors[i])) {
						error_data[i][j - 1].y = value;
						error_data[i][j - 1].minus = minus;
						error_data[i][j - 1].plus = plus;
					}
					break;

				case GOG_1_5D_STACKED :
					path[i][j].y = gog_axis_map_finite (y_map, sum) ?
						gog_axis_map_to_view (y_map, sum) :
						y_zero;
					if (gog_error_bar_is_visible (errors[i])) {
						error_data[i][j - 1].y = sum;
						error_data[i][j - 1].minus = minus;
						error_data[i][j - 1].plus = plus;
					}
					break;

				case GOG_1_5D_AS_PERCENTAGE :
					path[i][j].y = is_null ? 
						y_zero :
						(gog_axis_map_finite (y_map, sum) ?
						 gog_axis_map_to_view (y_map, sum  / abs_sum) :
						 y_zero);
					if (gog_error_bar_is_visible (errors[i])) {
						error_data[i][j - 1].y = is_null ? 0. : sum / abs_sum;
						error_data[i][j - 1].minus = is_null ? -1. : minus / abs_sum;
						error_data[i][j - 1].plus = is_null ? -1. : plus / abs_sum;
					}
					break;
			}
			if (lines[i]) {
				drop_paths[i][2 * j - 2].x = drop_paths[i][2 * j - 1].x = path[i][j].x;
				drop_paths[i][2 * j - 2].y = path[i][j].y;
			}

		}
	}
	
	gog_renderer_push_clip (view->renderer, 
				gog_renderer_get_rectangle_vpath (&view->allocation));

	for (i = 0; i < num_series; i++) {

		if (lengths[i] == 0)
			continue;

		gog_renderer_push_style (view->renderer, styles[i]);

		path[i][0].x = path[i][1].x;
		path[i][0].y = path[i][1].y;
		path[i][0].code = ART_MOVETO;

		if (!is_area_plot) {
			path[i][lengths[i] +1].code = ART_END;

			gog_renderer_draw_path (view->renderer, path[i]);
		} else {
			switch (type) {
				case GOG_1_5D_NORMAL :
					j = lengths[i] + 1;
					path[i][j].x = path[i][j-1].x;
					path[i][j].y = y_zero;
					path[i][j].code = ART_LINETO;
					j++;
					path[i][j].x = path[i][0].x;
					path[i][j].y = y_zero;
					path[i][j].code = ART_LINETO;
					j++;
					path[i][j].x = path[i][0].x;
					path[i][j].y = path[i][0].y;
					path[i][j].code = ART_LINETO;
					path[i][j+1].code = ART_END;
					break;

			case GOG_1_5D_STACKED :
			case GOG_1_5D_AS_PERCENTAGE :
				j = 2 * lengths[i] + 1;
				path[i][j].x = path[i][0].x;
				path[i][j].y = path[i][0].y;
				path[i][j].code = ART_LINETO;
				path[i][j+1].code = ART_END;
				break;
			}
			gog_renderer_draw_polygon (view->renderer, path[i], FALSE);
		}

		gog_renderer_pop_style (view->renderer);
	}

	/*Now draw drop lines */
	for (i = 0; i < num_series; i++)
		if (lines[i] != NULL) {
			gog_renderer_push_style (view->renderer,
				gog_styled_object_get_style (GOG_STYLED_OBJECT (lines[i])));
			gog_series_lines_render (lines[i], view->renderer, bbox, drop_paths[i], FALSE);
			gog_renderer_pop_style (view->renderer);
			g_free (drop_paths[i]);
		}

	/*Now draw error bars */
	for (i = 0; i < num_series; i++)
		if (gog_error_bar_is_visible (errors[i]))
			for (j = 0; j < lengths[i]; j++)
				gog_error_bar_render (errors[i], view->renderer, x_map, y_map,
						      error_data[i][j].x, error_data[i][j].y,
						      error_data[i][j].minus, error_data[i][j].plus, 
						      FALSE);

	gog_renderer_pop_clip (view->renderer);

	/*Now draw markers*/
	if (!is_area_plot) { 
		double x, y;
		double x_margin_min, x_margin_max, y_margin_min, y_margin_max, margin;

		margin = gog_renderer_line_size (view->renderer, 1.0);
		x_margin_min = view->allocation.x - margin;
		x_margin_max = view->allocation.x + view->allocation.w + margin;
		y_margin_min = view->allocation.y - margin;
		y_margin_max = view->allocation.y + view->allocation.h + margin;

		for (i = 0; i < num_series; i++) {
			if (lengths[i] == 0)
				continue;

			gog_renderer_push_style (view->renderer, styles[i]);

			for (j = 0; j < lengths[i]; j++) {
				x = path[i][j + 1].x;
				y = path[i][j + 1].y;
				if (x_margin_min <= x && x <= x_margin_max &&
				    y_margin_min <= y && y <= y_margin_max &&
				    path[i][j + 1].code != ART_MOVETO_OPEN) 
					gog_renderer_draw_marker (view->renderer, x, y);
			}
			gog_renderer_pop_style (view->renderer);
		}
	}

	for (i = 0; i < num_series; i++) {
		g_free (path[i]);
		g_free (error_data[i]);
	}

	gog_chart_map_free (chart_map);
}

static gboolean
gog_line_view_info_at_point (GogView *view, double x, double y,
			     GogObject const *cur_selection,
			     GogObject **obj, char **name)
{
	if (obj != NULL)
		*obj = view->model;
	if (name != NULL)
		*name = g_strdup (gog_object_get_name (GOG_OBJECT (view->model)));
	return TRUE;
}

static void
gog_line_view_class_init (GogViewClass *view_klass)
{
	view_klass->render	  = gog_line_view_render;
	view_klass->info_at_point = gog_line_view_info_at_point;
}

GSF_DYNAMIC_CLASS (GogLineView, gog_line_view,
	gog_line_view_class_init, NULL,
	GOG_PLOT_VIEW_TYPE)
