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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
 * USA
 */

#include <goffice/goffice-config.h>
#include "gog-radar.h"
#include <goffice/graph/gog-axis.h>
#include <goffice/graph/gog-view.h>
#include <goffice/graph/gog-renderer.h>
#include <goffice/graph/gog-theme.h>
#include <goffice/graph/gog-style.h>
#include <goffice/graph/go-data.h>
#include <goffice/utils/go-color.h>
#include <goffice/utils/go-marker.h>
#include <goffice/utils/go-math.h>

#include <module-plugin-defs.h>
#include <glib/gi18n.h>
#include <gsf/gsf-impl-utils.h>

typedef struct {
	GogPlotClass	base;
} GogRadarPlotClass;

enum {
	PLOT_PROP_0,
	PLOT_PROP_DEFAULT_STYLE_HAS_MARKERS
};

GNUMERIC_MODULE_PLUGIN_INFO_DECL;

typedef struct {
	GogSeries base;
} GogRadarSeries;
typedef GogSeriesClass GogRadarSeriesClass;

#define GOG_RADAR_SERIES_TYPE	(gog_radar_series_get_type ())
#define GOG_RADAR_SERIES(o)	(G_TYPE_CHECK_INSTANCE_CAST ((o), GOG_RADAR_SERIES_TYPE, GogRadarSeries))
#define GOG_IS_RADAR_SERIES(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOG_RADAR_SERIES_TYPE))

static GType gog_radar_series_get_type (void);
static GType gog_radar_view_get_type (void);

/*-----------------------------------------------------------------------------
 *
 *  GogRadarPlot
 *
 *-----------------------------------------------------------------------------
 */

/*
 *  Accessor for setting GOGRadarPlot member variables.
 *
 *  \param obj The radar plot as a GObject.  Must not be NULL.
 */
static void
gog_radar_plot_set_property (GObject *obj, guint param_id,
			      GValue const *value, GParamSpec *pspec)
{
	GogRadarPlot *radar = GOG_RADAR_PLOT (obj);

	switch (param_id) {
	case PLOT_PROP_DEFAULT_STYLE_HAS_MARKERS:
		radar->default_style_has_markers = g_value_get_boolean (value);
		break;
	default: G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, param_id, pspec);
		 return; /* NOTE : RETURN */
	}
	gog_object_emit_changed (GOG_OBJECT (obj), TRUE);
}

/*
 *  Accessor for getting GOGRadarPlot member variables.
 */
static void
gog_radar_plot_get_property (GObject *obj, guint param_id,
			      GValue *value, GParamSpec *pspec)
{
	GogRadarPlot *radar = GOG_RADAR_PLOT (obj);

	switch (param_id) {
	case PLOT_PROP_DEFAULT_STYLE_HAS_MARKERS:
		g_value_set_boolean (value, radar->default_style_has_markers);
		break;
	default: G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, param_id, pspec);
		 break;
	}
}

static char const *
gog_radar_plot_type_name (G_GNUC_UNUSED GogObject const *item)
{
	/* xgettext : the base for how to name radar plot objects
	 * eg The 2nd radar plot in a chart will be called
	 * 	PlotRadar2 */
	return N_("PlotRadar");
}

static void
gog_radar_plot_update (GogObject *obj)
{
	GogRadarPlot * model = GOG_RADAR_PLOT(obj);
	GogRadarSeries const *series;
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

	if (model->minima != val_min || model->maxima != val_max) {
		model->minima = val_min;
		model->maxima = val_max;
		gog_axis_bound_changed (model->base.axis [GOG_AXIS_RADIAL], GOG_OBJECT (model));
	}

	gog_object_emit_changed (GOG_OBJECT (obj), FALSE);
}

static GogAxisSet
gog_radar_plot_axis_set_pref (GogPlot const *plot)
{
	return GOG_AXIS_SET_RADAR;
}

static gboolean
gog_radar_plot_axis_set_is_valid (GogPlot const *plot, GogAxisSet type)
{
	return type == GOG_AXIS_SET_RADAR;
}

static gboolean
gog_radar_plot_axis_set_assign (GogPlot *plot, GogAxisSet type)
{
	return type == GOG_AXIS_SET_RADAR;
}

static GOData *
gog_radar_plot_axis_get_bounds (GogPlot *plot, GogAxisType axis, 
				GogPlotBoundInfo * bounds)
{
	GSList *ptr;
	GogRadarPlot *radar = GOG_RADAR_PLOT (plot);

	switch (axis) {
	case GOG_AXIS_CIRCULAR:
		bounds->val.minima = 0.;
		bounds->val.maxima = radar->num_elements;
		bounds->logical.minima = 0.;
		bounds->logical.maxima = go_nan;
		bounds->is_discrete    = TRUE;

		for (ptr = plot->series; ptr != NULL ; ptr = ptr->next)
			if (gog_series_is_valid (GOG_SERIES (ptr->data)))
				return GOG_SERIES (ptr->data)->values[0].data;
		break;
	case GOG_AXIS_RADIAL:
		/* clip at the outer bound, but allow inner to round nicely */
		bounds->val.minima = radar->minima;
		bounds->val.maxima = bounds->logical.maxima = radar->maxima;
		bounds->is_discrete = FALSE;
		break;
	default:
		g_warning("gog_radar_plot_axis_bounds: bad axis");
		break;
	} 
	
	return NULL;
}

static void
gog_radar_plot_class_init (GogPlotClass *gog_plot_klass)
{
	GObjectClass   *gobject_klass = (GObjectClass *) gog_plot_klass;
	GogObjectClass *gog_object_klass = (GogObjectClass *) gog_plot_klass;

	/* Override methods of GObject */
	gobject_klass->set_property = gog_radar_plot_set_property;
	gobject_klass->get_property = gog_radar_plot_get_property;

	/* Fill in GOGObject superclass values */
	gog_object_klass->update	= gog_radar_plot_update;
	gog_object_klass->type_name	= gog_radar_plot_type_name;
	gog_object_klass->view_type	= gog_radar_view_get_type ();

	g_object_class_install_property (gobject_klass, 
					 PLOT_PROP_DEFAULT_STYLE_HAS_MARKERS,
		g_param_spec_boolean ("default-style-has-markers", NULL,
			"Should the default style of a series include markers",
			FALSE, G_PARAM_READWRITE | GOG_PARAM_PERSISTENT));

	{
		static GogSeriesDimDesc dimensions[] = {
			{ N_("Labels"), GOG_SERIES_SUGGESTED, TRUE,
			  GOG_DIM_LABEL, GOG_MS_DIM_CATEGORIES },
			{ N_("Values"), GOG_SERIES_REQUIRED, FALSE,
			  GOG_DIM_VALUE, GOG_MS_DIM_VALUES }
		};
		gog_plot_klass->desc.series.dim = dimensions;
		gog_plot_klass->desc.series.num_dim = G_N_ELEMENTS (dimensions);
		gog_plot_klass->desc.series.style_fields = (GOG_STYLE_LINE 
							    | GOG_STYLE_MARKER);
	}

	/* Fill in GogPlotClass methods */
	gog_plot_klass->desc.num_series_min = 1;
	gog_plot_klass->desc.num_series_max = G_MAXINT;
	gog_plot_klass->series_type = gog_radar_series_get_type();
	gog_plot_klass->axis_set_pref = gog_radar_plot_axis_set_pref;
	gog_plot_klass->axis_set_is_valid = gog_radar_plot_axis_set_is_valid;
	gog_plot_klass->axis_set_assign = gog_radar_plot_axis_set_assign;
	gog_plot_klass->axis_get_bounds	= gog_radar_plot_axis_get_bounds;
}

static void
gog_radar_plot_init (GogRadarPlot *radar)
{
	radar->base.vary_style_by_element = FALSE;
	radar->default_style_has_markers = FALSE;
	radar->num_elements = 0;
}

GSF_CLASS (GogRadarPlot, gog_radar_plot,
	   gog_radar_plot_class_init, gog_radar_plot_init,
	   GOG_PLOT_TYPE)

/*****************************************************************************/

#define GOG_RADAR_AREA_PLOT_TYPE	(gog_radar_area_plot_get_type ())
#define GOG_RADAR_AREA_PLOT(o)		(G_TYPE_CHECK_INSTANCE_CAST ((o), GOG_RADAR_AREA_PLOT_TYPE, GogRadarAreaPlot))
#define GOG_IS_PLOT_RADAR_AREA(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOG_RADAR_AREA_PLOT_TYPE))

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
	plot_klass->series_type = gog_radar_series_get_type();

	gog_klass->type_name	= gog_radar_area_plot_type_name;
}
GSF_CLASS (GogRadarAreaPlot, gog_radar_area_plot,
	   gog_radar_area_plot_class_init, NULL,
	   GOG_RADAR_PLOT_TYPE)

/*****************************************************************************/

typedef GogPlotView		GogRadarView;
typedef GogPlotViewClass	GogRadarViewClass;

#ifndef HAVE_FMIN
static double
fmin (double a, double b)
{
	return (a < b) ? a : b;
}
#endif

static void
gog_radar_view_render (GogView *view, GogViewAllocation const *bbox)
{
	GogRadarPlot const *model = GOG_RADAR_PLOT (view->model);
	unsigned  center_x, center_y;
	GSList   *ptr;
	ArtVpath *path;
	gboolean const is_area = GOG_IS_PLOT_RADAR_AREA (model);
	GogAxisMap *map;

	map = gog_axis_map_new (GOG_PLOT (model)->axis[GOG_AXIS_RADIAL], 
				0.,
				fmin (view->allocation.h, view->allocation.w) / 2.0);
	
	if (!gog_axis_map_is_valid (map)) {
		gog_axis_map_free (map);
		return;
	}

	/* center things */
	center_x = view->allocation.x + view->allocation.w/2.0;
	center_y = view->allocation.y + view->allocation.h/2.0;

	path = g_alloca ((model->num_elements + 2) * sizeof (ArtVpath));
	for (ptr = model->base.series; ptr != NULL; ptr = ptr->next) {

		GogRadarSeries *series = GOG_RADAR_SERIES (ptr->data);
		GogStyle *style;
		gboolean closed_shape;
		unsigned count;
		double   *vals;

		if (!gog_series_is_valid (GOG_SERIES (series))) 
			continue;

		style = GOG_STYLED_OBJECT (series)->style;

		gog_renderer_push_style (view->renderer, style);

		closed_shape = (series->base.num_elements == model->num_elements);
		vals = go_data_vector_get_values (GO_DATA_VECTOR (series->base.values[1].data));
		for (count = 0; count < series->base.num_elements; count++) {
			double rho, theta, x, y;

			if (!go_finite (vals [count])) {
				closed_shape = FALSE;
				continue;
			}

			theta = count * 2.0 * M_PI / model->num_elements;
			rho = gog_axis_map_to_canvas (map, vals[count]);

			x = center_x + rho * sin (theta);
			y = center_y - rho * cos (theta);

			path[count].code = ((count != 0 && !isnan (vals[count-1])) 
					    ? ART_LINETO : ART_MOVETO);
			path[count].x = x;
			path[count].y = y;

			gog_renderer_draw_marker(view->renderer, x, y);
		}

		if (series->base.num_elements == model->num_elements
		    && go_finite(vals[count-1])) {
			path[count].code = ART_LINETO; 
			path[count].x = path[0].x;
			path[count].y = path[0].y;
			count++;
		}
		path[count].code = ART_END;

		if (closed_shape && is_area)
			gog_renderer_draw_polygon (view->renderer, path, FALSE, bbox);
		else
			gog_renderer_draw_path (view->renderer, path, bbox);

		gog_renderer_pop_style (view->renderer);
	}

	gog_axis_map_free (map);
}

static gboolean
gog_radar_view_info_at_point (GogView *view, double x, double y,
			      GogObject const *cur_selection,
			      GogObject **obj, char **name)
{
	double radius = fmin (view->allocation.h, view->allocation.w)/2.0;

	x -= view->allocation.x + view->allocation.w/2.;
	y -= view->allocation.y + view->allocation.h/2.;
	if ((x*x + y*y) > (radius*radius))
		return FALSE;
	
	return TRUE;
}

static void
gog_radar_view_class_init (GogViewClass *view_klass)
{
	view_klass->render	  = gog_radar_view_render;
	view_klass->info_at_point = gog_radar_view_info_at_point;
	view_klass->clip	  = TRUE;
}

static GSF_CLASS (GogRadarView, gog_radar_view,
		  gog_radar_view_class_init, NULL,
		  GOG_PLOT_VIEW_TYPE)


/*****************************************************************************/

static GogStyledObjectClass *series_parent_klass;

static void
gog_radar_series_update (GogObject *obj)
{
	GogRadarSeries *series = GOG_RADAR_SERIES (obj);
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
gog_radar_series_init_style (GogStyledObject *gso, GogStyle *style)
{
	GogSeries *series = GOG_SERIES (gso);
	GogRadarPlot const *plot;

	series_parent_klass->init_style (gso, style);
	if (series->plot == NULL)
		return;

	plot = GOG_RADAR_PLOT (series->plot);
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
gog_radar_series_class_init (GogStyledObjectClass *gso_klass)
{
	GogObjectClass * obj_klass = (GogObjectClass *) gso_klass;

	series_parent_klass = g_type_class_peek_parent (gso_klass);
	gso_klass->init_style = gog_radar_series_init_style;
	obj_klass->update = gog_radar_series_update;
}

GSF_CLASS (GogRadarSeries, gog_radar_series,
	   gog_radar_series_class_init, NULL,
	   GOG_SERIES_TYPE)

void
plugin_init (void)
{
	gog_radar_plot_get_type ();
	gog_radar_area_plot_get_type ();
}

void
plugin_cleanup (void)
{
}
