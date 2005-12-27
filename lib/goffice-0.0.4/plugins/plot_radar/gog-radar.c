/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-radar.c
 *
 * Copyright (C) 2004 Michael Devine (mdevine@cs.stanford.edu)
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
#include "gog-radar.h"
#include <goffice/graph/gog-axis.h>
#include <goffice/graph/gog-chart.h>
#include <goffice/graph/gog-grid-line.h>
#include <goffice/graph/gog-view.h>
#include <goffice/graph/gog-renderer.h>
#include <goffice/graph/gog-theme.h>
#include <goffice/graph/gog-style.h>
#include <goffice/data/go-data.h>
#include <goffice/utils/go-color.h>
#include <goffice/utils/go-marker.h>
#include <goffice/utils/go-math.h>
#include <goffice/app/module-plugin-defs.h>

#include <glib/gi18n.h>
#include <gsf/gsf-impl-utils.h>

#include <string.h>

typedef struct {
	GogPlotClass	base;
} GogRTPlotClass;

enum {
	PLOT_PROP_0,
	PLOT_PROP_DEFAULT_STYLE_HAS_MARKERS
};

GOFFICE_PLUGIN_MODULE_HEADER;

typedef struct {
	GogSeries base;
} GogRTSeries;
typedef GogSeriesClass GogRTSeriesClass;

#define GOG_RT_SERIES_TYPE	(gog_rt_series_get_type ())
#define GOG_RT_SERIES(o)	(G_TYPE_CHECK_INSTANCE_CAST ((o), GOG_RT_SERIES_TYPE, GogRTSeries))
#define GOG_IS_RT_SERIES(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOG_RT_SERIES_TYPE))

static GType gog_rt_series_get_type (void);
static GType gog_rt_view_get_type (void);

/*-----------------------------------------------------------------------------
 *
 *  GogRTPlot
 *
 *-----------------------------------------------------------------------------
 */

/*
 *  Accessor for setting GOGRTPlot member variables.
 *
 *  \param obj The rt plot as a GObject.  Must not be NULL.
 */
static void
gog_rt_plot_set_property (GObject *obj, guint param_id,
			      GValue const *value, GParamSpec *pspec)
{
	GogRTPlot *rt = GOG_RT_PLOT (obj);

	switch (param_id) {
	case PLOT_PROP_DEFAULT_STYLE_HAS_MARKERS:
		rt->default_style_has_markers = g_value_get_boolean (value);
		break;
	default: G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, param_id, pspec);
		 return; /* NOTE : RETURN */
	}
	gog_object_emit_changed (GOG_OBJECT (obj), TRUE);
}

/*
 *  Accessor for getting GOGRTPlot member variables.
 */
static void
gog_rt_plot_get_property (GObject *obj, guint param_id,
			      GValue *value, GParamSpec *pspec)
{
	GogRTPlot *rt = GOG_RT_PLOT (obj);

	switch (param_id) {
	case PLOT_PROP_DEFAULT_STYLE_HAS_MARKERS:
		g_value_set_boolean (value, rt->default_style_has_markers);
		break;
	default: G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, param_id, pspec);
		 break;
	}
}

static void
gog_rt_plot_update (GogObject *obj)
{
	GogRTPlot * model = GOG_RT_PLOT(obj);
	GogRTSeries const *series;
	unsigned num_elements = 0;
	double val_min, val_max, tmp_min, tmp_max;
	GSList *ptr;

	val_min =  DBL_MAX;
	val_max = -DBL_MAX;
	for (ptr = model->base.series; ptr != NULL; ptr = ptr->next) {
		series = ptr->data;
		if (!gog_series_is_valid (GOG_SERIES (series)))
			continue;

		if (num_elements < series->base.num_elements)
			num_elements = series->base.num_elements;
		go_data_vector_get_minmax (GO_DATA_VECTOR (
			series->base.values[1].data), &tmp_min, &tmp_max);
		if (val_min > tmp_min) val_min = tmp_min;
		if (val_max < tmp_max) val_max = tmp_max;
	}
	model->num_elements = num_elements;

	if (model->r.minima != val_min || model->r.maxima != val_max) {
		model->r.minima = val_min;
		model->r.maxima = val_max;
		gog_axis_bound_changed (model->base.axis [GOG_AXIS_RADIAL], GOG_OBJECT (model));
	}

	model->t.minima = 1;
	model->t.maxima = num_elements;

	gog_object_emit_changed (GOG_OBJECT (obj), FALSE);
}

static void
gog_rt_plot_guru_helper (GogPlot *plot, char const *hint)
{
	if (strcmp (hint, "circular-no-line") == 0) {
		GogAxis *axis = gog_plot_get_axis (plot, GOG_AXIS_CIRCULAR);
		GogStyle *style;

		g_return_if_fail (GOG_AXIS (axis) != NULL);

		style = gog_styled_object_get_style (GOG_STYLED_OBJECT (axis));
		style->line.dash_type = GO_LINE_NONE;
		style->line.auto_dash = FALSE;
	};
}

static void
gog_rt_plot_class_init (GogPlotClass *gog_plot_klass)
{
	GObjectClass   *gobject_klass = (GObjectClass *) gog_plot_klass;
	GogObjectClass *gog_object_klass = (GogObjectClass *) gog_plot_klass;

	/* Override methods of GObject */
	gobject_klass->set_property = gog_rt_plot_set_property;
	gobject_klass->get_property = gog_rt_plot_get_property;

	/* Fill in GOGObject superclass values */
	gog_object_klass->update	= gog_rt_plot_update;
	gog_object_klass->view_type	= gog_rt_view_get_type ();

	g_object_class_install_property (gobject_klass, 
					 PLOT_PROP_DEFAULT_STYLE_HAS_MARKERS,
		g_param_spec_boolean ("default-style-has-markers", NULL,
			"Should the default style of a series include markers",
			FALSE, G_PARAM_READWRITE | GOG_PARAM_PERSISTENT));

	/* Fill in GogPlotClass methods */
	gog_plot_klass->desc.num_series_min = 1;
	gog_plot_klass->desc.num_series_max = G_MAXINT;
	gog_plot_klass->series_type = gog_rt_series_get_type();

	gog_plot_klass->axis_set    = GOG_AXIS_SET_RADAR;
	gog_plot_klass->guru_helper = gog_rt_plot_guru_helper;
}

static void
gog_rt_plot_init (GogRTPlot *rt)
{
	rt->base.vary_style_by_element = FALSE;
	rt->default_style_has_markers = FALSE;
	rt->num_elements = 0;
}

GSF_DYNAMIC_CLASS (GogRTPlot, gog_rt_plot,
	   gog_rt_plot_class_init, gog_rt_plot_init,
	   GOG_PLOT_TYPE)

/*****************************************************************************/

typedef GogRTPlotClass GogRadarPlotClass;

static char const *
gog_radar_plot_type_name (G_GNUC_UNUSED GogObject const *item)
{
	/* xgettext : the base for how to name rt plot objects
	 * eg The 2nd rt plot in a chart will be called
	 * 	PlotRT2 */
	return N_("PlotRadar");
}

static GOData *
gog_radar_plot_axis_get_bounds (GogPlot *plot, GogAxisType axis, 
				GogPlotBoundInfo * bounds)
{
	GSList *ptr;
	GogRTPlot *rt = GOG_RT_PLOT (plot);

	switch (axis) {
	case GOG_AXIS_CIRCULAR:
		bounds->val.minima = rt->t.minima;
		bounds->val.maxima = rt->t.maxima;
		bounds->logical.minima = 0.;
		bounds->logical.maxima = go_nan;
		bounds->is_discrete    = TRUE;
		bounds->center_on_ticks = TRUE;

		for (ptr = plot->series; ptr != NULL ; ptr = ptr->next)
			if (gog_series_is_valid (GOG_SERIES (ptr->data)))
				return GOG_SERIES (ptr->data)->values[0].data;
		break;
	case GOG_AXIS_RADIAL:
		bounds->val.minima = rt->r.minima;
		bounds->val.maxima = rt->r.maxima;
		bounds->logical.maxima = bounds->logical.minima = go_nan;
		bounds->is_discrete = FALSE;
		break;
	default:
		g_warning("[GogRadarPlot::axis_set_bounds] bad axis (%i)", axis);
		break;
	} 
	
	return NULL;
}

static void
gog_radar_plot_class_init (GogPlotClass *gog_plot_klass)
{
	GogObjectClass *gog_object_klass = (GogObjectClass *) gog_plot_klass;

	/* Fill in GOGObject superclass values */
	gog_object_klass->type_name	= gog_radar_plot_type_name;

	{
		static GogSeriesDimDesc dimensions[] = {
			{ N_("Labels"), GOG_SERIES_SUGGESTED, TRUE,
			  GOG_DIM_LABEL, GOG_MS_DIM_CATEGORIES },
			{ N_("Values"), GOG_SERIES_REQUIRED, FALSE,
			  GOG_DIM_VALUE, GOG_MS_DIM_VALUES }
		};
		gog_plot_klass->desc.series.dim = dimensions;
		gog_plot_klass->desc.series.num_dim = G_N_ELEMENTS (dimensions);
		gog_plot_klass->desc.series.style_fields = GOG_STYLE_LINE | GOG_STYLE_MARKER;
	}

	gog_plot_klass->axis_get_bounds	= gog_radar_plot_axis_get_bounds;
}

GSF_DYNAMIC_CLASS (GogRadarPlot, gog_radar_plot,
	gog_radar_plot_class_init, NULL,
	GOG_RT_PLOT_TYPE)

/*****************************************************************************/

#define GOG_RADAR_AREA_PLOT_TYPE  (gog_radar_area_plot_get_type ())
#define GOG_RADAR_AREA_PLOT(o)	  (G_TYPE_CHECK_INSTANCE_CAST ((o), GOG_RADAR_AREA_PLOT_TYPE, GogRadarAreaPlot))
#define GOG_IS_PLOT_RADAR_AREA(o) (G_TYPE_CHECK_INSTANCE_TYPE ((o), GOG_RADAR_AREA_PLOT_TYPE))

typedef GogRadarPlot		GogRadarAreaPlot;
typedef GogRadarPlotClass	GogRadarAreaPlotClass;

GType gog_radar_area_plot_get_type (void);

static char const *
gog_radar_area_plot_type_name (G_GNUC_UNUSED GogObject const *item)
{
	/* xgettext : the base for how to name bar/col plot objects
	 * eg The 2nd line plot in a chart will be called
	 * 	PlotRadarArea2
	 */
	return N_("PlotRadarArea");
}
static void
gog_radar_area_plot_class_init (GogObjectClass *gog_klass)
{
	GogPlotClass *plot_klass = (GogPlotClass *) gog_klass;

	plot_klass->desc.series.style_fields = GOG_STYLE_OUTLINE | GOG_STYLE_FILL;

	gog_klass->type_name	= gog_radar_area_plot_type_name;
}

static void
gog_radar_area_plot_init (GogPlot *plot) 
{
	plot->render_before_axes = TRUE;
}
	
GSF_DYNAMIC_CLASS (GogRadarAreaPlot, gog_radar_area_plot,
	gog_radar_area_plot_class_init, gog_radar_area_plot_init,
	GOG_RADAR_PLOT_TYPE)

/*****************************************************************************/

typedef GogRTPlotClass GogPolarPlotClass;

static char const *
gog_polar_plot_type_name (G_GNUC_UNUSED GogObject const *item)
{
	/* xgettext : the base for how to name rt plot objects
	 * eg The 2nd rt plot in a chart will be called
	 * 	PlotPolar2 */
	return N_("PlotPolar");
}

static GOData *
gog_polar_plot_axis_get_bounds (GogPlot *plot, GogAxisType axis, 
				GogPlotBoundInfo * bounds)
{
	GogRTPlot *rt = GOG_RT_PLOT (plot);

	switch (axis) {
	case GOG_AXIS_CIRCULAR:
		bounds->val.minima = bounds->logical.minima= 0.;
		bounds->val.maxima = bounds->logical.maxima= 360.0;
		bounds->is_discrete    = FALSE;
		break;
	case GOG_AXIS_RADIAL:
		bounds->val.minima = bounds->logical.minima = 0.;
		bounds->val.maxima = rt->r.maxima;
		bounds->logical.maxima = go_nan;
		bounds->is_discrete = FALSE;
		break;
	default:
		g_warning("[GogPolarPlot::axis_set_bounds] bad axis (%i)", axis);
		break;
	} 
	
	return NULL;
}

static void
gog_polar_plot_class_init (GogPlotClass *gog_plot_klass)
{
	GogObjectClass *gog_object_klass = (GogObjectClass *) gog_plot_klass;

	/* Fill in GOGObject superclass values */
	gog_object_klass->type_name	= gog_polar_plot_type_name;

	{
		static GogSeriesDimDesc dimensions[] = {
			{ N_("Angle"), GOG_SERIES_SUGGESTED, FALSE,
			  GOG_DIM_INDEX, GOG_MS_DIM_CATEGORIES },
			{ N_("Magnitude"), GOG_SERIES_REQUIRED, FALSE,
			  GOG_DIM_VALUE, GOG_MS_DIM_VALUES }
		};
		gog_plot_klass->desc.series.dim = dimensions;
		gog_plot_klass->desc.series.num_dim = G_N_ELEMENTS (dimensions);
		gog_plot_klass->desc.series.style_fields = GOG_STYLE_LINE | GOG_STYLE_MARKER;
	}

	gog_plot_klass->axis_get_bounds	= gog_polar_plot_axis_get_bounds;
}

GSF_DYNAMIC_CLASS (GogPolarPlot, gog_polar_plot,
	gog_polar_plot_class_init, NULL,
	GOG_RT_PLOT_TYPE)

/*****************************************************************************/

typedef GogPlotView		GogRTView;
typedef GogPlotViewClass	GogRTViewClass;

static double
gogi_fmin (double a, double b)
{
	return (a < b) ? a : b;
}

static void
gog_rt_view_render (GogView *view, GogViewAllocation const *bbox)
{
	GogRTPlot const *model = GOG_RT_PLOT (view->model);
	GogAxis *r_axis, *c_axis;
	GogChart *chart = GOG_CHART (view->model->parent);
	GogAxisMap *r_map, *c_map;
	GogChartMap *chart_map;
	GogChartMapPolarData *parms;
	GogViewAllocation const *area;
	GSList   *ptr;
	ArtVpath *path, *clip_path;
	ArtBpath *bpath;
	double th0, theta_min, theta_max, theta;
	double rho_min, rho_max, rho;
	gboolean const is_area = GOG_IS_PLOT_RADAR_AREA (model);
	gboolean const is_polar = GOG_IS_PLOT_POLAR (model);

	r_axis = GOG_PLOT (model)->axis[GOG_AXIS_RADIAL];
	c_axis = GOG_PLOT (model)->axis[GOG_AXIS_CIRCULAR];
	g_return_if_fail (r_axis != NULL && c_axis != NULL);

	area = gog_chart_view_get_plot_area (view->parent);
	chart_map = gog_chart_map_new (chart, area, c_axis, r_axis, NULL, FALSE);
	if (!gog_chart_map_is_valid (chart_map)) {
		gog_chart_map_free (chart_map);
		return;
	}
	c_map = gog_chart_map_get_axis_map (chart_map, 0);
	r_map = gog_chart_map_get_axis_map (chart_map, 1);
	parms = gog_chart_map_get_polar_parms (chart_map);
	
	gog_axis_map_get_bounds (c_map, &theta_min, &theta_max);
	th0 = theta_min;
	gog_axis_map_get_bounds (r_map, &rho_min, &rho_max);
	/* convert theta value to radians */
	theta_min = gog_axis_map_to_view (c_map, theta_min);
	theta_max = gog_axis_map_to_view (c_map, theta_max);
	if (theta_min > theta_max) {
		/* angles may be inverted */
		theta = theta_min;
		theta_min = theta_max;
		theta_max = theta;
	}

	path = g_alloca ((model->num_elements + 2) * sizeof (ArtVpath));
	for (ptr = model->base.series; ptr != NULL; ptr = ptr->next) {

		GogRTSeries *series = GOG_RT_SERIES (ptr->data);
		GogStyle *style;
		gboolean closed_shape;
		unsigned count;
		double   *r_vals, *c_vals = NULL;

		if (!gog_series_is_valid (GOG_SERIES (series))) 
			continue;

		style = GOG_STYLED_OBJECT (series)->style;

		gog_renderer_push_style (view->renderer, style);

		closed_shape = (series->base.num_elements == model->num_elements);
		r_vals = go_data_vector_get_values (GO_DATA_VECTOR (series->base.values[1].data));
		if (is_polar)
			c_vals = go_data_vector_get_values (GO_DATA_VECTOR (series->base.values[0].data));
		for (count = 0; count < series->base.num_elements; count++) {

			if (!gog_axis_map_finite (r_map, r_vals [count])) {
				closed_shape = FALSE;
				continue;
			}
			
			path[count].code = ((count != 0 && !isnan (r_vals[count-1])) 
					    ? ART_LINETO : ART_MOVETO);

			rho = (!is_polar || (go_add_epsilon (r_vals[count] - rho_min) >= 0.0)) ?
				r_vals[count] : rho_min;
			gog_chart_map_2D_to_view (chart_map, 
						  is_polar ? c_vals[count] : count + th0, rho,
						  &path[count].x, &path[count].y);

			if (is_polar) theta = gog_axis_map_to_view (c_map, c_vals[count]);

			if ( !is_polar || 
			     (go_add_epsilon (r_vals[count] - rho_min) >= 0.0 && 
			      go_add_epsilon (rho_max - r_vals[count]) >= 0.0 &&
			      go_add_epsilon ((theta_max - theta_min) - fmod (theta_max - theta, 2 * M_PI)) >= 0.0 &&
			      go_add_epsilon ((theta_max - theta_min) - fmod (theta - theta_min, 2 * M_PI)) >= 0.0))
				gog_renderer_draw_marker (view->renderer, path[count].x, path[count].y);
		}

		if (!is_polar && series->base.num_elements == model->num_elements
		    && gog_axis_map_finite (r_map, r_vals[count-1])) {
			path[count].code = ART_LINETO; 
			path[count].x = path[0].x;
			path[count].y = path[0].y;
			count++;
		}
		path[count].code = ART_END;

		if (is_polar) { 
			bpath = gog_renderer_get_ring_wedge_bpath (parms->cx, parms->cy, parms->rx, parms->ry,
								   0.0, 0.0, -parms->th0, -parms->th1);
			clip_path = art_bez_path_to_vec (bpath, .1);
			g_free (bpath);
			gog_renderer_push_clip (view->renderer, clip_path);	
		}

		if (closed_shape && is_area) {
			gog_renderer_draw_polygon (view->renderer, path, FALSE);
		} else
			gog_renderer_draw_path (view->renderer, path);

		if (is_polar)
			gog_renderer_pop_clip (view->renderer);

		gog_renderer_pop_style (view->renderer);
	}
	gog_chart_map_free (chart_map);
}

static gboolean
gog_rt_view_info_at_point (GogView *view, double x, double y,
			      GogObject const *cur_selection,
			      GogObject **obj, char **name)
{
	double radius = gogi_fmin (view->allocation.h, view->allocation.w)/2.0;

	x -= view->allocation.x + view->allocation.w/2.;
	y -= view->allocation.y + view->allocation.h/2.;
	if ((x*x + y*y) > (radius*radius))
		return FALSE;
	
	return TRUE;
}

static void
gog_rt_view_class_init (GogViewClass *view_klass)
{
	view_klass->render	  = gog_rt_view_render;
	view_klass->info_at_point = gog_rt_view_info_at_point;
	view_klass->clip	  = TRUE;
}

GSF_DYNAMIC_CLASS (GogRTView, gog_rt_view,
	gog_rt_view_class_init, NULL,
	GOG_PLOT_VIEW_TYPE)

/*****************************************************************************/

static GogStyledObjectClass *series_parent_klass;

static void
gog_rt_series_update (GogObject *obj)
{
	GogRTSeries *series = GOG_RT_SERIES (obj);
	unsigned old_num = series->base.num_elements;
	double *vals;
	unsigned len = 0;

	if (series->base.values[1].data != NULL) {
		vals = go_data_vector_get_values (GO_DATA_VECTOR (series->base.values[1].data));
		len = go_data_vector_get_len (
			GO_DATA_VECTOR (series->base.values[1].data));
	}
	series->base.num_elements = len;

	/* queue plot and axis for redraw */
	gog_object_request_update (GOG_OBJECT (series->base.plot));
	if (old_num != len)
		gog_object_request_update (GOG_OBJECT (series->base.plot->axis[GOG_AXIS_CIRCULAR]));

	if (old_num != series->base.num_elements)
		gog_plot_request_cardinality_update (series->base.plot);

	if (((GogObjectClass *)series_parent_klass)->update)
		((GogObjectClass *)series_parent_klass)->update(obj);
}

static void
gog_rt_series_init_style (GogStyledObject *gso, GogStyle *style)
{
	GogSeries *series = GOG_SERIES (gso);
	GogRTPlot const *plot;

	series_parent_klass->init_style (gso, style);
	if (series->plot == NULL)
		return;

	plot = GOG_RT_PLOT (series->plot);

	if (!plot->default_style_has_markers &&
	    style->marker.auto_shape) 
		go_marker_set_shape (style->marker.mark, GO_MARKER_NONE);
}

static void
gog_rt_series_class_init (GogStyledObjectClass *gso_klass)
{
	GogObjectClass * obj_klass = (GogObjectClass *) gso_klass;

	series_parent_klass = g_type_class_peek_parent (gso_klass);
	gso_klass->init_style = gog_rt_series_init_style;
	obj_klass->update = gog_rt_series_update;
}

GSF_DYNAMIC_CLASS (GogRTSeries, gog_rt_series,
	gog_rt_series_class_init, NULL,
	GOG_SERIES_TYPE)

G_MODULE_EXPORT void
go_plugin_init (GOPlugin *plugin, GOCmdContext *cc)
{
	GTypeModule *module = go_plugin_get_type_module (plugin);
	gog_rt_plot_register_type (module);
	gog_radar_plot_register_type (module);
	gog_radar_area_plot_register_type (module);
	gog_polar_plot_register_type (module);
	gog_rt_view_register_type (module);
	gog_rt_series_register_type (module);
}

G_MODULE_EXPORT void
go_plugin_shutdown (GOPlugin *plugin, GOCmdContext *cc)
{
}
