/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-surface.c
 *
 * Copyright (C) 2004 Jean Brefort (jean.brefort@normalesup.org)
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
#include <goffice/graph/go-data.h>
#include <goffice/graph/gog-axis.h>
#include <goffice/graph/gog-renderer.h>
#include <goffice/graph/gog-theme.h>
#include <goffice/utils/go-format.h>
#include <goffice/utils/go-math.h>
#include <goffice/utils/go-color.h>
#include "gog-surface.h"

#include <module-plugin-defs.h>
#include <glib/gi18n.h>
#include <gsf/gsf-impl-utils.h>

#include <locale.h>
#include <string.h>

typedef struct {
	GogPlotClass	base;
} GogContourPlotClass;

GNUMERIC_MODULE_PLUGIN_INFO_DECL;

static GogObjectClass *plot_contour_parent_klass;

typedef struct {
	GogSeries base;
	
	unsigned rows, columns;
} GogSurfaceSeries;
typedef GogSeriesClass GogSurfaceSeriesClass;

#define GOG_SURFACE_SERIES_TYPE	(gog_surface_series_get_type ())
#define GOG_SURFACE_SERIES(o)	(G_TYPE_CHECK_INSTANCE_CAST ((o), GOG_SURFACE_SERIES_TYPE, GogSurfaceSeries))
#define GOG_IS_SURFACE_SERIES(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOG_SURFACE_SERIES_TYPE))

static GType gog_surface_series_get_type (void);
static GType gog_contour_view_get_type (void);

static gboolean
vary_uniformly (GODataVector *vec)
{
	int len = go_data_vector_get_len (vec), i = 0;
	double x, x0;
	if (len < 2)
		return TRUE;
	x0 = go_data_vector_get_value (vec, 0);
	x = go_data_vector_get_value (vec, 1);
	if (!go_finite( x0) || !go_finite (x))
		return FALSE;
	if (x > x0) {
		for (i = 2; i < len; i++) {
			x0 = go_data_vector_get_value (vec, i);
			if (!go_finite (x0) || (x0 <= x))
				return FALSE;
			x = x0;
		}
	} else if (x < x0) {
		for (i = 2; i < len; i++) {
			x0 = go_data_vector_get_value (vec, i);
			if (!go_finite (x0) || (x0 >= x))
				return FALSE;
			x = x0;
		}
	}
	return TRUE;
}

/*-----------------------------------------------------------------------------
 *
 *  GogContourPlot
 *
 *-----------------------------------------------------------------------------
 */
static char const *
gog_contour_plot_type_name (G_GNUC_UNUSED GogObject const *item)
{
	/* xgettext : the base for how to name contour plot objects
	*/
	return N_("PlotContour");
}

extern gpointer gog_contour_plot_pref (GogContourPlot *plot, GnmCmdContext *cc);
static gpointer
gog_contour_plot_editor (GogObject *item,
		    G_GNUC_UNUSED GogDataAllocator *dalloc,
		    GnmCmdContext *cc)
{
	return gog_contour_plot_pref (GOG_CONTOUR_PLOT (item), cc);
}
static void
gog_contour_plot_clear_formats (GogContourPlot *plot)
{
	if (plot->x.fmt != NULL) {
		go_format_unref (plot->x.fmt);
		plot->x.fmt = NULL;
	}
	if (plot->y.fmt != NULL) {
		go_format_unref (plot->y.fmt);
		plot->y.fmt = NULL;
	}
}

static void
gog_contour_plot_update (GogObject *obj)
{
	unsigned i;
	GogContourPlot * model = GOG_CONTOUR_PLOT(obj);
	GogSurfaceSeries * series = GOG_SURFACE_SERIES (model->base.series->data);
	GODataVector *vec;
	double tmp_min, tmp_max;

	if (model->x.fmt == NULL)
		model->x.fmt = go_data_preferred_fmt (series->base.values[0].data);
	if (model->y.fmt == NULL)
		model->y.fmt = go_data_preferred_fmt (series->base.values[1].data);

	if (series->base.num_elements != model->levels) {
		series->base.num_elements = model->levels;
		gog_plot_request_cardinality_update (&(model->base));
	}

	vec = GO_DATA_VECTOR (series->base.values[0].data);
	if (vary_uniformly (vec))
		go_data_vector_get_minmax (vec, &tmp_min, &tmp_max);
	else
		tmp_min = tmp_max = go_nan;
	if ((model->columns != series->columns)
			|| (tmp_min != model->x.minima)
			|| (tmp_max != model->x.maxima)) {
		model->columns = series->columns;
		model->x.minima = tmp_min;
		model->x.maxima = tmp_max;
		gog_axis_bound_changed (model->base.axis[0], GOG_OBJECT (model));
	}
	
	vec = GO_DATA_VECTOR (series->base.values[1].data);
	if (vary_uniformly (vec))
		go_data_vector_get_minmax (vec, &tmp_min, &tmp_max);
	else
		tmp_min = tmp_max = go_nan;
	if ((model->rows != series->rows)
			|| (tmp_min != model->y.minima)
			|| (tmp_max != model->y.maxima)) {
		model->rows = series->rows;
		model->y.minima = tmp_min;
		model->y.maxima = tmp_max;
		gog_axis_bound_changed (model->base.axis[1], GOG_OBJECT (model));
	}
	model->step = (model->zmax - model->zmin) / ((model->levels > 0)? model->levels: 1);
	if (isnan (model->step) || model->step == 0.)
		model->step = 1.;
	for (i = 0; i <= model->levels; i++)
		model->limits[i] =  model->zmin + i * model->step;

	gog_object_emit_changed (GOG_OBJECT (obj), FALSE);
	if (plot_contour_parent_klass->update)
		plot_contour_parent_klass->update (obj);
}

static GogAxisSet
gog_contour_plot_axis_set_pref (GogPlot const *plot)
{
	return GOG_AXIS_SET_XY;
}

static gboolean
gog_contour_plot_axis_set_is_valid (GogPlot const *plot, GogAxisSet type)
{
	return type == GOG_AXIS_SET_XY;
}

static gboolean
gog_contour_plot_axis_set_assign (GogPlot *plot, GogAxisSet type)
{
	return type == GOG_AXIS_SET_XY;
}

static GOData *
gog_contour_plot_axis_get_bounds (GogPlot *plot, GogAxisType axis, 
				GogPlotBoundInfo * bounds)
{
	GogSurfaceSeries *series = GOG_SURFACE_SERIES (plot->series->data);
	GogContourPlot *contour = GOG_CONTOUR_PLOT (plot);
	GODataVector *vec;
	double min, max;
	GOFormat *fmt;
	if (axis == GOG_AXIS_X) {
		vec = GO_DATA_VECTOR (series->base.values[0].data);
		fmt = contour->x.fmt;
		min = contour->x.minima;
		max = contour->x.maxima;
	} else {
		vec = GO_DATA_VECTOR (series->base.values[1].data);
		fmt = contour->y.fmt;
		min = contour->y.minima;
		max = contour->y.maxima;
	}
	if (bounds->fmt == NULL && fmt != NULL)
		bounds->fmt = go_format_ref (fmt);
	if (go_finite (min)) {
		bounds->logical.minima = bounds->val.minima = min;
		bounds->logical.maxima = bounds->val.maxima = max;
		bounds->is_discrete = FALSE;
	} else {
		bounds->val.minima = 0.;
		bounds->logical.minima = 0.;
		bounds->logical.maxima = go_nan;
		bounds->is_discrete    = TRUE;
		bounds->center_on_ticks = TRUE;
		bounds->val.maxima = (axis == GOG_AXIS_X)? series->columns: series->rows;
	}
	return (GOData*) vec;
}

static void 
gog_contour_plot_foreach_elem  (GogPlot *plot, gboolean only_visible,
				    GogEnumFunc func, gpointer data)
{
	unsigned i;
	char *label;
	static char separator = 0;
	GogStyle *style = gog_style_new ();
	GogTheme *theme = gog_object_get_theme (GOG_OBJECT (plot));
	GogContourPlot *contour = GOG_CONTOUR_PLOT (plot);
	GOColor *color;
	
	if (separator == 0) {
		struct lconv *lc = localeconv ();
		separator = (strcmp (lc->decimal_point, ","))? ',': ';';
	}
	/* build the colors table */
	color = g_new0 (GOColor, (contour->levels > 0)? contour->levels: 1);
	if (contour->levels < 2)
		color[0] = RGBA_WHITE;
	else for (i = 0; i < contour->levels; i++) {
		gog_theme_fillin_style (theme, style, GOG_OBJECT (plot->series->data), i, FALSE);
		color[i] = style->fill.pattern.back;
	}
	g_object_unref (style);

	style = gog_style_new ();/*dup (GOG_STYLED_OBJECT (plot->series->data)->style);*/
	style->interesting_fields = GOG_STYLE_FILL;
	style->disable_theming = GOG_STYLE_ALL;
	style->fill.type = GOG_FILL_STYLE_PATTERN;
	style->fill.pattern.pattern = GO_PATTERN_SOLID;

	for (i = 0; i < contour->levels; i++) {
		style->fill.pattern.back = color[i];
		label = g_strdup_printf ("[%g%c %g%c", contour->limits[i], separator,
					contour->limits[i + 1], (i == contour->levels - 1)? ']':'[');
		(func) (i, style, label, data);
		g_free (label);
	}
	g_object_unref (style);
	g_free (color);
}

static void
gog_contour_plot_finalize (GObject *obj)
{
	GogContourPlot *plot = GOG_CONTOUR_PLOT (obj);
	gog_contour_plot_clear_formats (plot);
	if (plot->limits != NULL)
		g_free (plot->limits);
	G_OBJECT_CLASS (plot_contour_parent_klass)->finalize (obj);
}

enum {
	GOG_CONTOUR_PROP_0,
	GOG_CONTOUR_PROP_LEVELS
};

static void
gog_contour_plot_set_property (GObject *obj, guint param_id,
			      GValue const *value, GParamSpec *pspec)
{
	GogContourPlot *plot = GOG_CONTOUR_PLOT (obj);

	switch (param_id) {
	case GOG_CONTOUR_PROP_LEVELS : {
		unsigned levels = g_value_get_uint (value);
		if (plot->levels != levels) {
			unsigned i;
			g_free (plot->limits);
			plot->limits = g_new (double, levels + 1);
			plot->levels = levels;
			plot->step = (plot->zmax - plot->zmin) / levels;
			if (isnan (plot->step) || plot->step == 0.)
				plot->step = 1.;
			for (i = 0; i < plot->levels; i++)
				plot->limits[i] =  plot->zmin + i * plot->step;
			gog_plot_request_cardinality_update (GOG_PLOT (plot));
		}
		break;
	}

	default: G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, param_id, pspec);
		 return; /* NOTE : RETURN */
	}

	gog_object_emit_changed (GOG_OBJECT (obj), FALSE);
}

static void
gog_contour_plot_get_property (GObject *obj, guint param_id,
			  GValue *value, GParamSpec *pspec)
{
	GogContourPlot *plot = GOG_CONTOUR_PLOT (obj);

	switch (param_id) {
	case GOG_CONTOUR_PROP_LEVELS :
		g_value_set_uint (value, plot->levels);
		break;
	default: G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, param_id, pspec);
		 break;
	}
}

static void
gog_contour_plot_class_init (GogPlotClass *gog_plot_klass)
{
	GObjectClass   *gobject_klass = (GObjectClass *) gog_plot_klass;
	GogObjectClass *gog_object_klass = (GogObjectClass *) gog_plot_klass;

	plot_contour_parent_klass = g_type_class_peek_parent (gog_plot_klass);

	gobject_klass->set_property = gog_contour_plot_set_property;
	gobject_klass->get_property = gog_contour_plot_get_property;
	gobject_klass->finalize     = gog_contour_plot_finalize;

	/* Fill in GOGObject superclass values */
	gog_object_klass->update	= gog_contour_plot_update;
	gog_object_klass->type_name	= gog_contour_plot_type_name;
	gog_object_klass->view_type	= gog_contour_view_get_type ();
	gog_object_klass->editor	= gog_contour_plot_editor;

	g_object_class_install_property (gobject_klass, GOG_CONTOUR_PROP_LEVELS,
		g_param_spec_uint  ("levels", "levels",
			"Number of slices.",
			1, 256, 6, /* max as 256 is not more stupid than another value */
			G_PARAM_READWRITE | GOG_PARAM_PERSISTENT));

	{
		static GogSeriesDimDesc dimensions[] = {
			{ N_("X"), GOG_SERIES_SUGGESTED, FALSE,
			  GOG_DIM_LABEL, GOG_MS_DIM_CATEGORIES },
			{ N_("Y"), GOG_SERIES_SUGGESTED, FALSE,
			  GOG_DIM_LABEL, GOG_MS_DIM_CATEGORIES },
			{ N_("Z"), GOG_SERIES_REQUIRED, FALSE,
			  GOG_DIM_MATRIX, GOG_MS_DIM_VALUES },
		};
		gog_plot_klass->desc.series.dim = dimensions;
		gog_plot_klass->desc.series.num_dim = G_N_ELEMENTS (dimensions);
		gog_plot_klass->desc.series.style_fields = GOG_STYLE_LINE;
	}

	/* Fill in GogPlotClass methods */
	gog_plot_klass->desc.num_series_min = 1;
	gog_plot_klass->desc.num_series_max = G_MAXINT;
	gog_plot_klass->series_type = gog_surface_series_get_type();
	gog_plot_klass->axis_set_pref = gog_contour_plot_axis_set_pref;
	gog_plot_klass->axis_set_is_valid = gog_contour_plot_axis_set_is_valid;
	gog_plot_klass->axis_set_assign = gog_contour_plot_axis_set_assign;
	gog_plot_klass->axis_get_bounds	= gog_contour_plot_axis_get_bounds;
	gog_plot_klass->foreach_elem = gog_contour_plot_foreach_elem;
}

static void
gog_contour_plot_init (GogContourPlot *contour)
{
	contour->levels = 6;
	contour->rows = contour->columns = 0;
	contour->limits = g_new (double, 7);
	contour->base.vary_style_by_element = TRUE;
	contour->x.minima = contour->x.maxima = contour->y.minima = contour->y.maxima =	go_nan;
	contour->x.fmt = contour->y.fmt = NULL;
}

GSF_CLASS (GogContourPlot, gog_contour_plot,
	   gog_contour_plot_class_init, gog_contour_plot_init,
	   GOG_PLOT_TYPE)

/*****************************************************************************/

typedef GogPlotView		GogContourView;
typedef GogPlotViewClass	GogContourViewClass;

static void
gog_contour_view_render (GogView *view, GogViewAllocation const *bbox)
{
	GogContourPlot const *plot = GOG_CONTOUR_PLOT (view->model);
	GogSurfaceSeries const *series = GOG_SURFACE_SERIES (plot->base.series->data);
	GODataMatrix *mat = GO_DATA_MATRIX (series->base.values[2].data);
	GODataVector *x_vec = 0, *y_vec = 0;
	GogAxisMap *x_map, *y_map;
	double zval0, zval1, zval2, zval3, t;
	double x[4], y[4], zval[4];
	unsigned z[4];
	unsigned z0 = 0, z1 = 0, z2 = 0, z3 = 0, zmin, zmax, nans, nan = 0;
	unsigned i, j, k, kmax, l, lmax, p, r = 0, s, h;
	GogRenderer *rend = view->renderer;
	GogStyle *style;
	GogTheme *theme = gog_object_get_theme (GOG_OBJECT (plot));
	double x0, x1, y0, y1;
	ArtVpath *path, *lines;
	GOColor *color;
	gboolean cw;

	x_map = gog_axis_map_new (plot->base.axis[0], 
				  view->residual.x , view->residual.w);
	y_map = gog_axis_map_new (plot->base.axis[1], 
				  view->residual.y + view->residual.h, 
				  -view->residual.h);

	if (!(gog_axis_map_is_valid (x_map) &&
	      gog_axis_map_is_valid (y_map))) {
		gog_axis_map_free (x_map);
		gog_axis_map_free (y_map);
		return;
	}

	/* Set cw to ensure that polygons will allways be drawn clockwise */
	if (gog_axis_is_discrete (plot->base.axis[0])) {
		x0 = gog_axis_map_to_canvas(x_map, 0.);
		x1 = gog_axis_map_to_canvas(x_map, 1.);
	}else {
		x_vec = GO_DATA_VECTOR (series->base.values[0].data);
		x0 = gog_axis_map_to_canvas(x_map, go_data_vector_get_value (x_vec, 0));
		x1 = gog_axis_map_to_canvas(x_map, go_data_vector_get_value (x_vec, 1));
	}
	if (gog_axis_is_discrete (plot->base.axis[1])) {
		y0 = gog_axis_map_to_canvas(y_map, 0.);
		y1 = gog_axis_map_to_canvas(y_map, 1.);
	}else {
		y_vec = GO_DATA_VECTOR (series->base.values[1].data);
		y0 = gog_axis_map_to_canvas(y_map, go_data_vector_get_value (y_vec, 0));
		y1 = gog_axis_map_to_canvas(y_map, go_data_vector_get_value (y_vec, 1));
	}
	cw = (x1 > x0) == (y1 > y0);

	style = gog_style_new ();
	path = art_new (ArtVpath, 10);
	/* build the colors table */
	color = g_new0 (GOColor, (plot->levels > 0)? plot->levels: 1);
	if (plot->levels < 2)
		color[0] = RGBA_WHITE;
	else for (i = 0; i < plot->levels; i++) {
		gog_theme_fillin_style (theme, style, GOG_OBJECT (series), i, FALSE);
		color[i] = style->fill.pattern.back;
	}
	g_object_unref (style);

	/* clip to avoid problems with logarithmic axes */
	gog_renderer_clip_push (rend, &(view->residual));

	style = gog_style_new ();
	style->interesting_fields = GOG_STYLE_FILL;
	style->disable_theming = GOG_STYLE_ALL;
	style->fill.type = GOG_FILL_STYLE_PATTERN;
	style->fill.pattern.pattern = GO_PATTERN_SOLID;

	lines = art_new (ArtVpath, lmax = 64);
	l = 0;

	for (j = 1; j < series->columns; j++) {
		if (gog_axis_is_discrete (plot->base.axis[0])) {
			x0 = gog_axis_map_to_canvas(x_map, j - 1);
			x1 = gog_axis_map_to_canvas(x_map, j);
		}else {
			x0 = gog_axis_map_to_canvas(x_map, go_data_vector_get_value (x_vec, j - 1));
			x1 = gog_axis_map_to_canvas(x_map, go_data_vector_get_value (x_vec, j));
		}
		
		for (i = 1; i < series->rows; i++) {
			if (gog_axis_is_discrete (plot->base.axis[1])) {
				y0 = gog_axis_map_to_canvas(y_map, i - 1);
				y1 = gog_axis_map_to_canvas(y_map, i);
			}else {
				y0 = gog_axis_map_to_canvas(y_map, go_data_vector_get_value (y_vec, i - 1));
				y1 = gog_axis_map_to_canvas(y_map, go_data_vector_get_value (y_vec, i));
			}
			nans = 0;
			nan = 4;
			zmin = plot->levels;
			zmax = 0;
			zval0 = go_data_matrix_get_value (mat, i - 1, j - 1);
			if (!isnan (zval0)) {
				z0 = floor ((zval0 - plot->zmin) / plot->step);
				if (z0 == plot->levels && z0 > 0)
					z0--;
				if (z0 > zmax)
					zmax = z0;
				if (z0 < zmin) {
					zmin = z0;
					r = 0;
				}
			} else {
				nans++;
				nan = 0;
			}
			zval1 = go_data_matrix_get_value (mat, i - 1, j);
			if (!isnan (zval1)) {
				z1 = floor ((zval1 - plot->zmin) / plot->step);
				if (z1 == plot->levels && z1 > 0)
					z1--;
				if (z1 > zmax)
					zmax = z1;
				if (z1 < zmin) {
					zmin = z1;
					r = 1;
				}
			} else {
				nans++;
				nan = 1;
			}
			zval2 = go_data_matrix_get_value (mat, i, j);
			if (!isnan (zval2)) {
				z2 = floor ((zval2 - plot->zmin) / plot->step);
				if (z2 == plot->levels && z2 > 0)
					z2--;
				if (z2 > zmax)
					zmax = z2;
				if (z2 < zmin) {
					zmin = z2;
					r = 2;
				}
			} else {
				nans++;
				nan = 2;
			}
			zval3 = go_data_matrix_get_value (mat, i, j - 1);
			if (!isnan (zval3)) {
				z3 = floor ((zval3 - plot->zmin) / plot->step);
				if (z3 == plot->levels && z3 > 0)
					z3--;
				if (z3 > zmax)
					zmax = z3;
				if (z3 < zmin) {
					zmin = z3;
					r = 3;
				}
			} else {
				nans++;
				nan = 3;
			}
			if (nans > 1)
				continue;
			/* Build the x, y and z arrays for the tile */
			k = r;
			s = 0;
			do {
				if (k != nan) {
					switch (k) {
					case 0:
						x[s] = x0;
						y[s] = y0;
						z[s] = z0;
						zval[s++] = zval0;
						break;
					case 1:
						x[s] = x1;
						y[s] = y0;
						z[s] = z1;
						zval[s++] = zval1;
						break;
					case 2:
						x[s] = x1;
						y[s] = y1;
						z[s] = z2;
						zval[s++] = zval2;
						break;
					default:
						x[s] = x0;
						y[s] = y1;
						z[s] = z3;
						zval[s++] = zval3;
					}
				}
				if (cw) {
					k++;
					k %= 4;
				} else {
					if (k == 0)
						k = 3;
					else
						k--;
				}
			} while (k != r);
			if (zmin == zmax) {
				/* paint everything with one color*/
				style->fill.pattern.back = color[zmin];
				gog_renderer_push_style (rend, style);
				path[0].code = ART_MOVETO;
				for (k = 0; k < s; ) {
					path[k].x = x[k];
					path[k].y = y[k];
					path[++k].code = ART_LINETO;
				}
				path[k].x = x[0];
				path[k].y = y[0];
				path[k + 1].code = ART_END;
				/* narrow parameter is TRUE below to avoid border effects */
				gog_renderer_draw_polygon (rend, path, TRUE, NULL);
				gog_renderer_pop_style (rend);
			} else {
				kmax = 3 - nans;
				if (!nans && (((z0 < z1) && (z1 > z2) && (z2 < z3) && (z3 > z0)) ||
					((z0 > z1) && (z1 < z2) && (z2 > z3) && (z3 < z0)))) {
					/* we have a saddle point */
					/* first find the most probable altitude of the saddle point */
					unsigned zn, zx;
					gboolean crossing = FALSE, up = FALSE, odd;
					double xl[8], yl[8];
					/* crossing is TRUE if the saddle point occurs at a slices border */
					zn = MAX (z[0], z[2]);
					if (zval[1] > zval[3])
						zx = (zval[3] == plot->limits[z[3]])? z[3] - 1: z[3];
					else
						zx =  (zval[1] == plot->limits[z[1]])? z[1] - 1: z[1];
					odd = (zx - zn) % 2;
					if (odd) {
						if ((zx - zn) == 1) {
							double sum = 0.;
							sum += (z[0] == zn)? zval[0]: plot->limits[zn];
							sum += (z[1] == zx)? zval[1]: plot->limits[zx + 1];
							sum += (z[2] == zn)? zval[2]: plot->limits[zn];
							sum += (z[3] == zx)? zval[3]: plot->limits[zx + 1];
							sum /= 4.;
							if (fabs ((sum - plot->limits[zx]) / plot->step) < DBL_EPSILON)
								crossing = TRUE;
							else
								up = (sum - plot->limits[zx]) < 0;
						} else
							crossing = TRUE;
						zn = (zn + zx) / 2;
						zx = zn + 1;
					} else
						zn = zx = (zn + zx) / 2;
					/* low values slices */
					if (z[0] < zn) {
						k = z[0];
						style->fill.pattern.back = color[k];
						k++;
						path[0].code = ART_MOVETO;
						path[1].code = ART_LINETO;
						path[2].code = ART_LINETO;
						path[3].code = ART_LINETO;
						path[4].code = ART_END;
						path[0].x = path[3].x = x[0];
						path[0].y = path[3].y = y[0];
						if ((l + 3) >= lmax)
							lines = art_renew (lines, ArtVpath, lmax += 64);
						lines[l].code = ART_MOVETO_OPEN;
						t = (plot->limits[k] - zval[0]) / (zval[3] - zval[0]);
						xl[7] = lines[l].x = path[1].x = x[0] + t * (x[3] - x[0]);
						yl[7] = lines[l++].y = path[1].y = y[0] + t * (y[3] - y[0]);
						lines[l].code = ART_LINETO;
						t = (plot->limits[k] - zval[0]) / (zval[1] - zval[0]);
						xl[0] = lines[l].x = path[2].x = x[0] + t * (x[1] - x[0]);
						yl[0] = lines[l++].y = path[2].y = y[0] + t * (y[1] - y[0]);
						gog_renderer_push_style (rend, style);
						gog_renderer_draw_polygon (rend, path, TRUE, NULL);
						gog_renderer_pop_style (rend);
						path[4].code = ART_LINETO;
						path[5].code = ART_END;
						while (k < zn) {
							style->fill.pattern.back = color[k];
							k++;
							path[0].x = path[4].x = xl[7];
							path[0].y = path[4].y = yl[7];
							path[3].x = xl[0];
							path[3].y = yl[0];
							if ((l + 3) >= lmax)
								lines = art_renew (lines, ArtVpath, lmax += 64);
							lines[l].code = ART_MOVETO_OPEN;
							t = (plot->limits[k] - zval[0]) / (zval[3] - zval[0]);
							xl[7] = lines[l].x = path[1].x = x[0] + t * (x[3] - x[0]);
							yl[7] = lines[l++].y = path[1].y = y[0] + t * (y[3] - y[0]);
							lines[l].code = ART_LINETO;
							t = (plot->limits[k] - zval[0]) / (zval[1] - zval[0]);
							xl[0] = lines[l].x = path[2].x = x[0] + t * (x[1] - x[0]);
							yl[0] = lines[l++].y = path[2].y = y[0] + t * (y[1] - y[0]);
							gog_renderer_push_style (rend, style);
							gog_renderer_draw_polygon (rend, path, TRUE, NULL);
							gog_renderer_pop_style (rend);
						}
					} else
						xl[0] = xl[7] = -1.;
					if (z[2] < zn) {
						k = z[2];
						style->fill.pattern.back = color[k];
						k++;
						path[0].code = ART_MOVETO;
						path[1].code = ART_LINETO;
						path[2].code = ART_LINETO;
						path[3].code = ART_LINETO;
						path[4].code = ART_END;
						path[0].x = path[3].x = x[2];
						path[0].y = path[3].y = y[2];
						if ((l + 3) >= lmax)
							lines = art_renew (lines, ArtVpath, lmax += 64);
						lines[l].code = ART_MOVETO_OPEN;
						t = (plot->limits[k] - zval[2]) / (zval[1] - zval[2]);
						xl[3] = lines[l].x = path[1].x = x[2] + t * (x[1] - x[2]);
						yl[3] = lines[l++].y = path[1].y = y[2] + t * (y[1] - y[2]);
						lines[l].code = ART_LINETO;
						t = (plot->limits[k] - zval[2]) / (zval[3] - zval[2]);
						xl[4] = lines[l].x = path[2].x = x[2] + t * (x[3] - x[2]);
						yl[4] = lines[l++].y = path[2].y = y[2] + t * (y[3] - y[2]);
						gog_renderer_push_style (rend, style);
						gog_renderer_draw_polygon (rend, path, TRUE, NULL);
						gog_renderer_pop_style (rend);
						path[4].code = ART_LINETO;
						path[5].code = ART_END;
						while (k < zn) {
							style->fill.pattern.back = color[k];
							k++;
							path[0].x = path[4].x = xl[3];
							path[0].y = path[4].y = yl[3];
							path[3].x = xl[4];
							path[3].y = yl[4];
							if ((l + 3) >= lmax)
								lines = art_renew (lines, ArtVpath, lmax += 64);
							lines[l].code = ART_MOVETO_OPEN;
							t = (plot->limits[k] - zval[2]) / (zval[1] - zval[2]);
							xl[3] = lines[l].x = path[1].x = x[2] + t * (x[1] - x[2]);
							yl[3] = lines[l++].y = path[1].y = y[2] + t * (y[1] - y[2]);
							lines[l].code = ART_LINETO;
							t = (plot->limits[k] - zval[2]) / (zval[3] - zval[2]);
							xl[4] = lines[l].x = path[2].x = x[2] + t * (x[3] - x[2]);
							yl[4] = lines[l++].y = path[2].y = y[2] + t * (y[3] - y[2]);
							gog_renderer_push_style (rend, style);
							gog_renderer_draw_polygon (rend, path, TRUE, NULL);
							gog_renderer_pop_style (rend);
						}
					} else
						xl[3] = xl[4] = -1.;
					/* high values slices */
					k = z[1];
					if (zval[1] == plot->limits[k])
						k--;
					if (k > zx) {
						path[0].code = ART_MOVETO;
						path[1].code = ART_LINETO;
						path[2].code = ART_LINETO;
						path[3].code = ART_LINETO;
						path[4].code = ART_END;
						path[0].x = path[3].x = x[1];
						path[0].y = path[3].y = y[1];
						if ((l + 3) >= lmax)
							lines = art_renew (lines, ArtVpath, lmax += 64);
						lines[l].code = ART_MOVETO_OPEN;
						t = (plot->limits[k] - zval[1]) / (zval[0] - zval[1]);
						xl[1] = lines[l].x = path[1].x = x[1] + t * (x[0] - x[1]);
						yl[1] = lines[l++].y = path[1].y = y[1] + t * (y[0] - y[1]);
						lines[l].code = ART_LINETO;
						t = (plot->limits[k] - zval[1]) / (zval[2] - zval[1]);
						xl[2] = lines[l].x = path[2].x = x[1] + t * (x[2] - x[1]);
						yl[2] = lines[l++].y = path[2].y = y[1] + t * (y[2] - y[1]);
						style->fill.pattern.back = color[k];
						gog_renderer_push_style (rend, style);
						gog_renderer_draw_polygon (rend, path, TRUE, NULL);
						gog_renderer_pop_style (rend);
						path[4].code = ART_LINETO;
						path[5].code = ART_END;
						k--;
						while (k > zx) {
							path[0].x = path[4].x = xl[1];
							path[0].y = path[4].y = yl[1];
							path[3].x = xl[2];
							path[3].y = yl[2];
							if ((l + 3) >= lmax)
								lines = art_renew (lines, ArtVpath, lmax += 64);
							lines[l].code = ART_MOVETO_OPEN;
							t = (plot->limits[k] - zval[1]) / (zval[0] - zval[1]);
							xl[1] = lines[l].x = path[1].x = x[1] + t * (x[0] - x[1]);
							yl[1] = lines[l++].y = path[1].y = y[1] + t * (y[0] - y[1]);
							lines[l].code = ART_LINETO;
							t = (plot->limits[k] - zval[1]) / (zval[2] - zval[1]);
							xl[2] = lines[l].x = path[2].x = x[1] + t * (x[2] - x[1]);
							yl[2] = lines[l++].y = path[2].y = y[1] + t * (y[2] - y[1]);
							style->fill.pattern.back = color[k];
							gog_renderer_push_style (rend, style);
							gog_renderer_draw_polygon (rend, path, TRUE, NULL);
							gog_renderer_pop_style (rend);
							k--;
						}
					} else
						xl[1] = xl[2] = -1.;
					k = z[3];
					if (zval[3] == plot->limits[k])
						k--;
					if (k > zx) {
						path[0].code = ART_MOVETO;
						path[1].code = ART_LINETO;
						path[2].code = ART_LINETO;
						path[3].code = ART_LINETO;
						path[4].code = ART_END;
						path[0].x = path[3].x = x[3];
						path[0].y = path[3].y = y[3];
						if ((l + 3) >= lmax)
							lines = art_renew (lines, ArtVpath, lmax += 64);
						lines[l].code = ART_MOVETO_OPEN;
						t = (plot->limits[k] - zval[3]) / (zval[2] - zval[3]);
						xl[5] = lines[l].x = path[1].x = x[3] + t * (x[2] - x[3]);
						yl[5] = lines[l++].y = path[1].y = y[3] + t * (y[2] - y[3]);
						lines[l].code = ART_LINETO;
						t = (plot->limits[k] - zval[3]) / (zval[0] - zval[3]);
						xl[6] = lines[l].x = path[2].x = x[3] + t * (x[0] - x[3]);
						yl[6] = lines[l++].y = path[2].y = y[3] + t * (y[0] - y[3]);
						style->fill.pattern.back = color[k];
						gog_renderer_push_style (rend, style);
						gog_renderer_draw_polygon (rend, path, TRUE, NULL);
						gog_renderer_pop_style (rend);
						path[4].code = ART_LINETO;
						path[5].code = ART_END;
						k--;
						while (k > zx) {
							path[0].x = path[4].x = xl[5];
							path[0].y = path[4].y = yl[5];
							path[3].x = xl[6];
							path[3].y = yl[6];
							if ((l + 3) >= lmax)
								lines = art_renew (lines, ArtVpath, lmax += 64);
							lines[l].code = ART_MOVETO_OPEN;
							t = (plot->limits[k] - zval[3]) / (zval[2] - zval[3]);
							xl[5] = lines[l].x = path[1].x = x[3] + t * (x[2] - x[3]);
							yl[5] = lines[l++].y = path[1].y = y[3] + t * (y[2] - y[3]);
							lines[l].code = ART_LINETO;
							t = (plot->limits[k] - zval[3]) / (zval[0] - zval[3]);
							xl[6] = lines[l].x = path[2].x = x[3] + t * (x[0] - x[3]);
							yl[6] = lines[l++].y = path[2].y = y[3] + t * (y[0] - y[3]);
							style->fill.pattern.back = color[k];
							gog_renderer_push_style (rend, style);
							gog_renderer_draw_polygon (rend, path, TRUE, NULL);
							gog_renderer_pop_style (rend);
							k--;
						}
					} else
						xl[5] = xl[6] = -1.;
					/* middle values slices */
					if (odd) {
						if (crossing) {
							double xb[4], yb[4], xc, yc;
							for (k = 0; k < 4; k++) {
								s = (k + 1) % 4;
								t =  (plot->limits[zx] - zval[s]) / (zval[k] - zval[s]);
								xb[k] = x[s] + t * (x[k] - x[s]);
								yb[k] = y[s] + t * (y[k] - y[s]);
							}
							if ((l + 5) >= lmax)
								lines = art_renew (lines, ArtVpath, lmax += 64);
							lines[l].code = ART_MOVETO_OPEN;
							lines[l].x = xb[0];
							lines[l++].y = yb[0];
							lines[l].code = ART_LINETO;
							lines[l].x = xb[2];
							lines[l++].y = yb[2];
							lines[l].code = ART_MOVETO_OPEN;
							lines[l].x = xb[1];
							lines[l++].y = yb[1];
							lines[l].code = ART_LINETO;
							lines[l].x = xb[3];
							lines[l++].y = yb[3];
							/* calculate the coordinates xc and yc of crossing point */
							t = ((xb[1] - xb[0]) * (yb[3] - yb[1])
								+ (xb[1] - xb[3]) * (yb[1] - yb[0])) /
								((xb[2] - xb[0]) * (yb[3] - yb[1])
								+ (xb[1] - xb[3]) * (yb[2] - yb[0]));
							xc = xb[0] + t * (xb[2] - xb[0]);
							yc = yb[0] + t * (yb[2] - yb[0]);
							/* fill */
							path[0].code = ART_MOVETO;
							path[1].code = ART_LINETO;
							path[2].code = ART_LINETO;
							path[3].code = ART_LINETO;
							path[4].code = ART_LINETO;
							if (xl[0] < 0.) {
								path[4].x = path[0].x = x[0];
								path[4].y = path[0].y = y[0];
								path[5].code = ART_END;
							} else {
								path[5].x = path[0].x = xl[7];
								path[5].y = path[0].y = yl[7];
								path[4].x = xl[0];
								path[4].y = yl[0];
								path[5].code = ART_LINETO;
								path[6].code = ART_END;
							}
							path[1].x = xb[3];
							path[1].y = yb[3];
							path[2].x = xc;
							path[2].y = yc;
							path[3].x = xb[0];
							path[3].y = yb[0];
							style->fill.pattern.back = color[zn];
							gog_renderer_push_style (rend, style);
							gog_renderer_draw_polygon (rend, path, TRUE, NULL);
							if (xl[2] < 0.) {
								path[4].x = path[0].x = x[2];
								path[4].y = path[0].y = y[2];
								path[5].code = ART_END;
							} else {
								path[5].x = path[0].x = xl[3];
								path[5].y = path[0].y = yl[3];
								path[4].x = xl[4];
								path[4].y = yl[4];
								path[5].code = ART_LINETO;
								path[6].code = ART_END;
							}
							path[1].x = xb[1];
							path[1].y = yb[1];
							path[3].x = xb[2];
							path[3].y = yb[2];
							gog_renderer_draw_polygon (rend, path, TRUE, NULL);
							gog_renderer_pop_style (rend);
							if (xl[2] < 0.) {
								path[4].x = path[0].x = x[1];
								path[4].y = path[0].y = y[1];
								path[5].code = ART_END;
							} else {
								path[5].x = path[0].x = xl[1];
								path[5].y = path[0].y = yl[1];
								path[4].x = xl[2];
								path[4].y = yl[2];
								path[5].code = ART_LINETO;
								path[6].code = ART_END;
							}
							path[1].x = xb[0];
							path[1].y = yb[0];
							path[3].x = xb[1];
							path[3].y = yb[1];
							style->fill.pattern.back = color[zx];
							gog_renderer_push_style (rend, style);
							gog_renderer_draw_polygon (rend, path, TRUE, NULL);
							if (xl[6] < 0.) {
								path[4].x = path[0].x = x[3];
								path[4].y = path[0].y = y[3];
								path[5].code = ART_END;
							} else {
								path[5].x = path[0].x = xl[5];
								path[5].y = path[0].y = yl[5];
								path[4].x = xl[6];
								path[4].y = yl[6];
								path[5].code = ART_LINETO;
								path[6].code = ART_END;
							}
							path[1].x = xb[2];
							path[1].y = yb[2];
							path[3].x = xb[3];
							path[3].y = yb[3];
							gog_renderer_draw_polygon (rend, path, TRUE, NULL);
							gog_renderer_pop_style (rend);
						} else {
							if (up) {
								/* saddle point is in the lower slice */
								/* draw the upper slices */
								path[0].code = ART_MOVETO;
								path[1].code = ART_LINETO;
								path[2].code = ART_LINETO;
								path[3].code = ART_LINETO;
								if (xl[1] < 0.) {
									path[4].code = ART_END;
									path[0].x = path[3].x = x[1];
									path[0].y = path[3].y = y[1];
								} else {
									path[4].code = ART_LINETO;
									path[5].code = ART_END;
									path[0].x = path[4].x = xl[1];
									path[0].y = path[4].y = yl[1];
									path[3].x = xl[2];
									path[3].y = yl[2];
								}
								if ((l + 5) >= lmax)
									lines = art_renew (lines, ArtVpath, lmax += 64);
								lines[l].code = ART_MOVETO_OPEN;
								t = (plot->limits[zx] - zval[1]) / (zval[0] - zval[1]);
								xl[1] = lines[l].x = path[1].x = x[1] + t * (x[0] - x[1]);
								yl[1] = lines[l++].y = path[1].y = y[1] + t * (y[0] - y[1]);
								lines[l].code = ART_LINETO;
								t = (plot->limits[zx] - zval[1]) / (zval[2] - zval[1]);
								xl[2] = lines[l].x = path[2].x = x[1] + t * (x[2] - x[1]);
								yl[2] = lines[l++].y = path[2].y = y[1] + t * (y[2] - y[1]);
								style->fill.pattern.back = color[zx];
								gog_renderer_push_style (rend, style);
								gog_renderer_draw_polygon (rend, path, TRUE, NULL);
								if (xl[5] < 0.) {
									path[4].code = ART_END;
									path[0].x = path[3].x = x[3];
									path[0].y = path[3].y = y[3];
								} else {
									path[4].code = ART_LINETO;
									path[5].code = ART_END;
									path[0].x = path[4].x = xl[5];
									path[0].y = path[4].y = yl[5];
									path[3].x = xl[6];
									path[3].y = yl[6];
								}
								if ((l + 5) >= lmax)
									lines = art_renew (lines, ArtVpath, lmax += 64);
								lines[l].code = ART_MOVETO_OPEN;
								t = (plot->limits[zx] - zval[3]) / (zval[2] - zval[3]);
								xl[5] = lines[l].x = path[1].x = x[3] + t * (x[2] - x[3]);
								yl[5] = lines[l++].y = path[1].y = y[3] + t * (y[2] - y[3]);
								lines[l].code = ART_LINETO;
								t = (plot->limits[zx] - zval[3]) / (zval[0] - zval[3]);
								xl[6] = lines[l].x = path[2].x = x[3] + t * (x[0] - x[3]);
								yl[6] = lines[l++].y = path[2].y = y[3] + t * (y[0] - y[3]);
								gog_renderer_draw_polygon (rend, path, TRUE, NULL);
								gog_renderer_pop_style (rend);
							} else {
								/* saddle point is in the upper slice */
								path[0].code = ART_MOVETO;
								path[1].code = ART_LINETO;
								path[2].code = ART_LINETO;
								path[3].code = ART_LINETO;
								if (xl[0] < 0.) {
									path[4].code = ART_END;
									path[0].x = path[3].x = x[0];
									path[0].y = path[3].y = y[0];
								} else {
									path[4].code = ART_LINETO;
									path[5].code = ART_END;
									path[0].x = path[4].x = xl[7];
									path[0].y = path[4].y = yl[7];
									path[3].x = xl[0];
									path[3].y = yl[0];
								}
								if ((l + 5) >= lmax)
									lines = art_renew (lines, ArtVpath, lmax += 64);
								lines[l].code = ART_MOVETO_OPEN;
								t = (plot->limits[k] - zval[0]) / (zval[3] - zval[0]);
								xl[7] = lines[l].x = path[1].x = x[0] + t * (x[3] - x[0]);
								yl[7] = lines[l++].y = path[1].y = y[0] + t * (y[3] - y[0]);
								lines[l].code = ART_LINETO;
								t = (plot->limits[k] - zval[0]) / (zval[1] - zval[0]);
								xl[0] = lines[l].x = path[2].x = x[0] + t * (x[1] - x[0]);
								yl[0] = lines[l++].y = path[2].y = y[0] + t * (y[1] - y[0]);
								style->fill.pattern.back = color[zn];
								gog_renderer_push_style (rend, style);
								gog_renderer_draw_polygon (rend, path, TRUE, NULL);
								if (xl[2] < 0.) {
									path[4].code = ART_END;
									path[0].x = path[3].x = x[2];
									path[0].y = path[3].y = y[2];
								} else {
									path[4].code = ART_LINETO;
									path[5].code = ART_END;
									path[0].x = path[4].x = xl[3];
									path[0].y = path[4].y = yl[3];
									path[3].x = xl[4];
									path[3].y = yl[4];
								}
								lines[l].code = ART_MOVETO_OPEN;
								t = (plot->limits[k] - zval[2]) / (zval[1] - zval[2]);
								xl[3] = lines[l].x = path[1].x = x[2] + t * (x[1] - x[2]);
								yl[3] = lines[l++].y = path[1].y = y[2] + t * (y[1] - y[2]);
								lines[l].code = ART_LINETO;
								t = (plot->limits[k] - zval[2]) / (zval[3] - zval[2]);
								xl[4] = lines[l].x = path[2].x = x[2] + t * (x[3] - x[2]);
								yl[4] = lines[l++].y = path[2].y = y[2] + t * (y[3] - y[2]);
								gog_renderer_draw_polygon (rend, path, TRUE, NULL);
								gog_renderer_pop_style (rend);
								zn = zx;
							}
							/* draw the saddle containing slice */
							k = 0;
							for (s = 0; s < 8; s++) {
								path[k].code = (k)? ART_LINETO: ART_MOVETO;
								if (xl[s] < 0.) {
									if (s == 7)
										break;
									else if (s > 0)
										s++;
									r = s / 2;
									path[k].x = x[r];
									path[k++].y = y[r];
								} else {
									path[k].x = xl[s];
									path[k++].y = yl[s];
								}
							}
							path[k].code = ART_LINETO;
							path[k].x = path[0].x;
							path[k++].y = path[0].y;
							path[k].code = ART_END;
							style->fill.pattern.back = color[zn];
							gog_renderer_push_style (rend, style);
							gog_renderer_draw_polygon (rend, path, TRUE, NULL);
							gog_renderer_pop_style (rend);
						}
					} else {
						k = 0;
						for (s = 0; s < 8; s++) {
							path[k].code = (k)? ART_LINETO: ART_MOVETO;
							if (xl[s] < 0.) {
								if (s == 7)
									break;
								else if (s > 0)
									s++;
								r = s / 2;
								path[k].x = x[r];
								path[k++].y = y[r];
							} else {
								path[k].x = xl[s];
								path[k++].y = yl[s];
							}
						}
						path[k].code = ART_LINETO;
						path[k].x = path[0].x;
						path[k++].y = path[0].y;
						path[k].code = ART_END;
						style->fill.pattern.back = color[zx];
						gog_renderer_push_style (rend, style);
						gog_renderer_draw_polygon (rend, path, TRUE, NULL);
						gog_renderer_pop_style (rend);
					}
				} else {
					/* no saddle point visible */
					if ((l + (zmax - zmin) * 2 + 1) >= lmax)
						lines = art_renew (lines, ArtVpath, lmax += 64);
					path[0].code = ART_MOVETO;
					path[0].x = x[0];
					path[0].y = y[0];
					p = 1;
					k = 1;
					s = 0;
					r = kmax;
					while (zmin < zmax) {
						style->fill.pattern.back = color[zmin];
						gog_renderer_push_style (rend, style);
						while (z[k] <= zmin && k < kmax) {
							path[p].code = ART_LINETO;
							path[p].x = x[k];
							path[p++].y = y[k++];
						}
						while (z[r] <= zmin && r > 0)
							r--;
						
						zmin++;
						t = (plot->limits[zmin] - zval[k - 1]) / (zval[k] - zval[k - 1]);
						path[p].code = ART_LINETO;
						lines[l].code = ART_MOVETO_OPEN;
						lines[l].x = path[p].x = x[k - 1] + t * (x[k] - x[k - 1]);
						lines[l++].y = path[p++].y = y[k - 1] + t * (y[k] - y[k - 1]);
						path[p].code = ART_LINETO;
						lines[l].code = ART_LINETO;
						if (r < kmax) {
							t = (plot->limits[zmin] - zval[r]) / (zval[r + 1] - zval[r]);
							lines[l].x = path[p].x = x[r] + t * (x[r + 1] - x[r]);
							lines[l++].y = path[p++].y = y[r] + t * (y[r + 1] - y[r]);
						} else {
							t = (plot->limits[zmin] - zval[r]) / (zval[0] - zval[r]);
							lines[l].x = path[p].x = x[r] + t * (x[0] - x[r]);
							lines[l++].y = path[p++].y = y[r] + t * (y[0] - y[r]);
						}
						if (s == 0) {
							for (h = r + 1; h <= kmax; h++) {
								path[p].code = ART_LINETO;
								path[p].x = x[h];
								path[p++].y = y[h];
							}
						} else {
							for (h = r + 1; h < s; h++) {
								path[p].code = ART_LINETO;
								path[p].x = x[h];
								path[p++].y = y[h];
							}
						}
						s = r + 1;
						path[p].code = ART_LINETO;
						path[p].x = path[0].x;
						path[p++].y = path[0].y;
						path[p].code = ART_END;
						gog_renderer_draw_polygon (rend, path, TRUE, NULL);
						gog_renderer_pop_style (rend);
						path[0].x = lines[l - 1].x;
						path[0].y = lines[l - 1].y;
						path[1].x = lines[l - 2].x;
						path[1].y = lines[l - 2].y;
						p = 2;
					}
					while (k < s) {
						path[p].code = ART_LINETO;
						path[p].x = x[k];
						path[p++].y = y[k++];
					}
					path[p].code = ART_LINETO;
					path[p].x = path[0].x;
					path[p++].y = path[0].y;
					path[p].code = ART_END;
					style->fill.pattern.back = color[zmin];
					gog_renderer_push_style (rend, style);
					gog_renderer_draw_polygon (rend, path, TRUE, NULL);
					gog_renderer_pop_style (rend);
				}
			}
		}
	}
	lines[l].code = ART_END;
	gog_renderer_push_style (rend, GOG_STYLED_OBJECT (series)->style);
	gog_renderer_draw_path  (rend, lines, NULL);
	gog_renderer_pop_style (rend);
	gog_renderer_clip_pop (rend);
	art_free (lines);
	art_free (path);
	g_object_unref (style);
	gog_axis_map_free (x_map);
	gog_axis_map_free (y_map);
}

static void
gog_contour_view_class_init (GogViewClass *view_klass)
{
	view_klass->render = gog_contour_view_render;
}

static GSF_CLASS (GogContourView, gog_contour_view,
		  gog_contour_view_class_init, NULL,
		  GOG_PLOT_VIEW_TYPE)

/*****************************************************************************/

static GogStyledObjectClass *series_parent_klass;

static void
gog_surface_series_update (GogObject *obj)
{
	GogSurfaceSeries *series = GOG_SURFACE_SERIES (obj);
	GOMatrixSize size, old_size;
	GogContourPlot *plot = GOG_CONTOUR_PLOT (series->base.plot);
	GODataMatrix *mat;
	GODataVector *vec;
	int length;
	size.rows = 0;
	size.columns = 0;
	if (series->base.values[2].data != NULL) {
		old_size.rows = series->rows;
		old_size.columns = series->columns;
		mat = GO_DATA_MATRIX (series->base.values[2].data);
		go_data_matrix_get_values (mat);
		size = go_data_matrix_get_size (mat);
		go_data_matrix_get_minmax (mat, &plot->zmin, &plot->zmax);
	}
	if (series->base.values[0].data != NULL) {
		vec = GO_DATA_VECTOR (series->base.values[0].data);
		go_data_vector_get_values (vec);
		length = go_data_vector_get_len (vec);
		if (length < size.columns)
			size.columns = length;
	}
	if (series->base.values[1].data != NULL) {
		vec = GO_DATA_VECTOR (series->base.values[1].data);
		go_data_vector_get_values (vec);
		length = go_data_vector_get_len (vec);
		if (length < size.rows)
			size.rows = length;
	}
	series->rows = size.rows;
	series->columns = size.columns;
	series->base.num_elements = plot->levels;

	/* queue plot for redraw */
	gog_object_request_update (GOG_OBJECT (series->base.plot));
	gog_plot_request_cardinality_update (series->base.plot);

	if (series_parent_klass->base.update)
		series_parent_klass->base.update (obj);
}

static void
gog_surface_series_init_style (GogStyledObject *gso, GogStyle *style)
{
	series_parent_klass->init_style (gso, style);
}

static void
gog_surface_series_class_init (GogStyledObjectClass *gso_klass)
{
	GogObjectClass * obj_klass = (GogObjectClass *) gso_klass;

	series_parent_klass = g_type_class_peek_parent (gso_klass);
	gso_klass->init_style = gog_surface_series_init_style;
	obj_klass->update = gog_surface_series_update;
}


GSF_CLASS (GogSurfaceSeries, gog_surface_series,
	   gog_surface_series_class_init, NULL,
	   GOG_SERIES_TYPE)

void
plugin_init (void)
{
	gog_contour_plot_get_type ();
}

void
plugin_cleanup (void)
{
}
