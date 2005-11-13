/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * xl-surface.c
 *
 * Copyright (C) 2005 Jean Brefort (jean.brefort@normalesup.org)
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
#include "xl-surface.h"

#include <glib/gi18n.h>
#include <gsf/gsf-impl-utils.h>

#include <goffice/data/go-data-simple.h>
#include <goffice/graph/gog-axis.h>
#include <goffice/utils/go-format.h>
#include <goffice/utils/go-math.h>

static GogObjectClass *xl_contour_parent_klass;
typedef GogSeries XLSurfaceSeries;
typedef GogSeriesClass XLSurfaceSeriesClass;

#define XL_SURFACE_SERIES_TYPE	(xl_surface_series_get_type ())
#define XL_SURFACE_SERIES(o)	(G_TYPE_CHECK_INSTANCE_CAST ((o), XL_SURFACE_SERIES_TYPE, XLSurfaceSeries))
#define XL_IS_SURFACE_SERIES(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), XL_SURFACE_SERIES_TYPE))

static GType xl_surface_series_get_type (void);

/*****************************************************************************/

typedef GogContourPlotClass XLContourPlotClass;

static double *
xl_contour_plot_build_matrix (GogContourPlot const *plot,
			gboolean *cardinality_changed)
{
	unsigned i, j, length;
	GogAxisMap *map;
	GogAxisTick *zticks;
	GogAxis *axis = plot->base.axis[GOG_AXIS_PSEUDO_3D];
	unsigned nticks;
	double x[2], val;
	GogSeries *series = NULL;
	GODataVector *vec;
	unsigned n = plot->rows * plot->columns;
	double *data, minimum, maximum;
	unsigned max;
	GSList *ptr;

	if (!gog_axis_get_bounds (axis, &minimum, &maximum))
		return NULL;
	data = g_new (double, n);
	nticks = gog_axis_get_ticks (axis, &zticks);
	map = gog_axis_map_new (axis, 0, 1);
	for (i = j = 0; i < nticks; i++)
		if (zticks[i].type == GOG_AXIS_TICK_MAJOR) {
			x[j++] = gog_axis_map_to_view (map, zticks[i].position);
			if (j > 1)
				break;
		}
	x[1] -= x[0];

	for (i = 0, ptr = plot->base.series ; ptr != NULL ; ptr = ptr->next) {
		series = ptr->data;
		if (!gog_series_is_valid (GOG_SERIES (series)))
			continue;
		vec = GO_DATA_VECTOR (series->values[1].data);
		length = go_data_vector_get_len (vec);
		for (j = 0; j < plot->columns; j++) {
			/* The vector might be too short, excel is so ugly ;-) */
			val = (j < length)? gog_axis_map_to_view (map,
					go_data_vector_get_value (vec, j)): 0.;
			/* This is an excel compatible plot, so let's be compatible */
			if (val == go_nan || !go_finite (val))
				val = 0.;
			if (fabs (val) == DBL_MAX)
				val = go_nan;
			else {
				val = val/ x[1] - x[0];
				if (val < 0) {
					val = go_nan;
				}
			}
			data[i * plot->columns + j] = val;
		}
		i++;
	}
	g_return_val_if_fail (series != NULL, NULL);
	max = (unsigned) ceil (1 / x[1]);
	series = plot->base.series->data;
	if (series->num_elements != max) {
		series->num_elements = max;
		*cardinality_changed = TRUE;
	}
	gog_axis_map_free (map);
	return data;
}

static void
xl_contour_plot_update (GogObject *obj)
{
	GogContourPlot * model = GOG_CONTOUR_PLOT(obj);
	XLSurfaceSeries * series;
	double zmin =  DBL_MAX, zmax = -DBL_MAX, tmp_min, tmp_max;
	GSList *ptr;
	model->rows = 0;
	model->columns = 0;

	for (ptr = model->base.series ; ptr != NULL ; ptr = ptr->next) {
		series = ptr->data;
		if (!gog_series_is_valid (GOG_SERIES (series)))
			continue;
		if (series->num_elements > model->columns)
			model->columns = series->num_elements;
		model->rows++;
		go_data_vector_get_minmax (GO_DATA_VECTOR (
			series->values[1].data), &tmp_min, &tmp_max);
		if (zmin > tmp_min) zmin = tmp_min;
		if (zmax < tmp_max) zmax = tmp_max;
	}
	g_free (model->plotted_data);
	model->plotted_data = NULL;
	if ((zmin != model->z.minima)
			|| (zmax != model->z.maxima)) {
		model->z.minima = zmin;
		model->z.maxima = zmax;
		gog_axis_bound_changed (model->base.axis[GOG_AXIS_PSEUDO_3D], GOG_OBJECT (model));
	} else
		gog_plot_update_3d (GOG_PLOT (model));

	gog_axis_bound_changed (model->base.axis[GOG_AXIS_X], obj);
	gog_axis_bound_changed (model->base.axis[GOG_AXIS_Y], obj);
}

static GODataVector *
get_y_vector (GogPlot *plot)
{
	XLContourPlot *contour = XL_CONTOUR_PLOT (plot);
	GSList *ptr;
	int i;

	if (contour->y_labels)
		g_free (contour->y_labels);
	contour->y_labels = g_new0 (char const *, contour->base.rows);

	for (ptr = plot->series, i = 0 ; ptr != NULL ; ptr = ptr->next, i++) {
		XLSurfaceSeries *series = ptr->data;

		if (!gog_series_is_valid (GOG_SERIES (series)))
			continue;
		contour->y_labels[i] = go_data_scalar_get_str (GO_DATA_SCALAR (
				series->values[-1].data));
	}

	return GO_DATA_VECTOR (go_data_vector_str_new (contour->y_labels, i, NULL));
}

static GOData *
xl_contour_plot_axis_get_bounds (GogPlot *plot, GogAxisType axis, 
				GogPlotBoundInfo * bounds)
{
	XLContourPlot *contour = XL_CONTOUR_PLOT (plot);
	GODataVector *vec = NULL;
	GOFormat *fmt;

	if (axis == GOG_AXIS_X) {
		XLSurfaceSeries *series = XL_SURFACE_SERIES (plot->series->data);
		vec = GO_DATA_VECTOR (series->values[0].data);
		fmt = contour->base.x.fmt;
	} else if (axis == GOG_AXIS_Y) {
		if (!contour->base.rows)
			return NULL;
		vec = get_y_vector (plot);
		fmt = contour->base.y.fmt;
	} else {
		if (bounds->fmt == NULL && contour->base.z.fmt != NULL)
			bounds->fmt = go_format_ref (contour->base.z.fmt);
		bounds->val.minima = contour->base.z.minima;
		bounds->val.maxima = contour->base.z.maxima;
		return NULL;
	}
	if (bounds->fmt == NULL && fmt != NULL)
		bounds->fmt = go_format_ref (fmt);
	bounds->val.minima = 0.;
	bounds->logical.minima = 0.;
	bounds->logical.maxima = go_nan;
	bounds->is_discrete    = TRUE;
	bounds->center_on_ticks = TRUE;
	bounds->val.maxima = (axis == GOG_AXIS_X)? 
		contour->base.columns - 1.: 
		contour->base.rows - 1.;
	return (GOData*) vec;
}

static void
xl_contour_plot_finalize (GObject *obj)
{
	XLContourPlot *plot = XL_CONTOUR_PLOT (obj);
	if (plot->y_labels)
		g_free (plot->y_labels);
	G_OBJECT_CLASS (xl_contour_parent_klass)->finalize (obj);
}

static void
xl_contour_plot_class_init (GogContourPlotClass *klass)
{
	GogPlotClass *gog_plot_klass = (GogPlotClass*) klass;
	GogObjectClass *gog_object_klass = (GogObjectClass *) klass;
	GObjectClass   *gobject_klass = (GObjectClass *) klass;

	xl_contour_parent_klass = g_type_class_peek_parent (klass);

	gobject_klass->finalize     = xl_contour_plot_finalize;

	/* Fill in GOGObject superclass values */
	gog_object_klass->update	= xl_contour_plot_update;
	gog_object_klass->populate_editor	= NULL;

	{
		static GogSeriesDimDesc dimensions[] = {
			{ N_("X"), GOG_SERIES_REQUIRED, FALSE,
			  GOG_DIM_LABEL, GOG_MS_DIM_CATEGORIES },
			{ N_("Z"), GOG_SERIES_REQUIRED, FALSE,
			  GOG_DIM_VALUE, GOG_MS_DIM_VALUES },
		};
		gog_plot_klass->desc.series.dim = dimensions;
		gog_plot_klass->desc.series.num_dim = G_N_ELEMENTS (dimensions);
		gog_plot_klass->desc.series.style_fields = 0;
	}
	/* Fill in GogPlotClass methods */
	gog_plot_klass->axis_get_bounds	= xl_contour_plot_axis_get_bounds;
	gog_plot_klass->series_type = xl_surface_series_get_type();

	klass->build_matrix = xl_contour_plot_build_matrix;
}

static void
xl_contour_plot_init (XLContourPlot *contour)
{
	contour->y_labels = NULL;
}

GSF_DYNAMIC_CLASS (XLContourPlot, xl_contour_plot,
	xl_contour_plot_class_init, xl_contour_plot_init,
	GOG_CONTOUR_PLOT_TYPE)

/*****************************************************************************/

static GogStyledObjectClass *series_parent_klass;

static void
xl_surface_series_update (GogObject *obj)
{
	XLSurfaceSeries *series = XL_SURFACE_SERIES (obj);
	int x_len = 0, z_len = 0;
/*	unsigned old_num = series->num_elements;*/

	if (series->values[1].data != NULL)
		z_len = go_data_vector_get_len (
			GO_DATA_VECTOR (series->values[1].data));
	if (series->values[0].data != NULL)
		x_len = go_data_vector_get_len (
			GO_DATA_VECTOR (series->values[0].data));
	else
		x_len = z_len;
	series->num_elements = MIN (x_len, z_len);

	/* queue plot for redraw */
	gog_object_request_update (GOG_OBJECT (series->plot));
/*	if (old_num != series->base.num_elements)
		gog_plot_request_cardinality_update (series->plot);*/

	if (series_parent_klass->base.update)
		series_parent_klass->base.update (obj);
}

static void
xl_surface_series_init_style (GogStyledObject *gso, GogStyle *style)
{
	series_parent_klass->init_style (gso, style);
}

static void
xl_surface_series_class_init (GogStyledObjectClass *gso_klass)
{
	GogObjectClass * obj_klass = (GogObjectClass *) gso_klass;

	series_parent_klass = g_type_class_peek_parent (gso_klass);
	gso_klass->init_style = xl_surface_series_init_style;
	obj_klass->update = xl_surface_series_update;
}


GSF_DYNAMIC_CLASS (XLSurfaceSeries, xl_surface_series,
	xl_surface_series_class_init, NULL,
	GOG_SERIES_TYPE)
