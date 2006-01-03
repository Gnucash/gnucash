/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-dropbar.c
 *
 * Copyright (C) 2005
 *	Jean Brefort (jean.brefort@normalesup.org)
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
#include "gog-dropbar.h"
#include <goffice/graph/gog-series-lines.h>
#include <goffice/graph/gog-view.h>
#include <goffice/graph/gog-renderer.h>
#include <goffice/utils/go-math.h>

#include <glib/gi18n.h>
#include <gsf/gsf-impl-utils.h>

static GogObjectClass *gog_dropbar_parent_klass;

static GType gog_dropbar_view_get_type (void);

static char const *
gog_dropbar_plot_type_name (G_GNUC_UNUSED GogObject const *item)
{
	/* xgettext : the base for how to name drop bar/col plot objects
	 * eg The 2nd drop bar/col plot in a chart will be called
	 * 	PlotDropBar2 */
	return N_("PlotDropBar");
}

static void
gog_dropbar_plot_class_init (GogPlot1_5dClass *gog_plot_1_5d_klass)
{
	GogObjectClass *gog_object_klass = (GogObjectClass *) gog_plot_1_5d_klass;
	GogPlotClass   *plot_klass = (GogPlotClass *) gog_plot_1_5d_klass;
	gog_dropbar_parent_klass = g_type_class_peek_parent (gog_plot_1_5d_klass);

	gog_object_klass->type_name	= gog_dropbar_plot_type_name;
	gog_object_klass->view_type	= gog_dropbar_view_get_type ();

	{
		static GogSeriesDimDesc dimensions[] = {
			{ N_("Labels"), GOG_SERIES_SUGGESTED, TRUE,
			  GOG_DIM_LABEL, GOG_MS_DIM_CATEGORIES },
			{ N_("Start"), GOG_SERIES_REQUIRED, FALSE,
			  GOG_DIM_VALUE, GOG_MS_DIM_START },
			{ N_("End"), GOG_SERIES_REQUIRED, FALSE,
			  GOG_DIM_VALUE, GOG_MS_DIM_END },
		};
		plot_klass->desc.series.dim = dimensions;
		plot_klass->desc.series.num_dim = G_N_ELEMENTS (dimensions);
	}

	gog_plot_1_5d_klass->update_stacked_and_percentage = NULL;
}

static void
gog_dropbar_plot_init (GogPlot1_5d *plot)
{
	plot->support_series_lines = FALSE;
	plot->support_lines = TRUE;
}

GSF_DYNAMIC_CLASS (GogDropBarPlot, gog_dropbar_plot,
	gog_dropbar_plot_class_init, gog_dropbar_plot_init,
	GOG_BARCOL_PLOT_TYPE)

/*****************************************************************************/
typedef GogPlotView		GogDropBarView;
typedef GogPlotViewClass	GogDropBarViewClass;

/**
 * FIXME FIXME FIXME Wrong description
 * barcol_draw_rect :
 * @rend : #GogRenderer
 * @flip :
 * @base : #GogViewAllocation
 * @rect : #GogViewAllocation
 *
 * A utility routine to build a vpath in @rect.  @rect is assumed to be in
 * coordinates relative to @base with 0,0 as the upper left.  @flip transposes
 * @rect and rotates it to put the origin in the bottom left.  Play fast and
 * loose with coordinates to keep widths >= 1.  If we allow things to be less
 * the background bleeds through.
 **/
static void
barcol_draw_rect (GogRenderer *rend, gboolean flip,
		  GogAxisMap *x_map,
		  GogAxisMap *y_map,
		  GogViewAllocation const *rect)
{
	ArtVpath path[6];
	double x0, x1, y0, y1;

	if (flip) {
		x0 = gog_axis_map_to_view (x_map, rect->y);
		x1 = gog_axis_map_to_view (x_map, rect->y + rect->h);
		y0 = gog_axis_map_to_view (y_map, rect->x);
		y1 = gog_axis_map_to_view (y_map, rect->x + rect->w);
		if (fabs (x1 - x0) < .5) {
			x1 += .25;
			x0 -= .25;
		}
	} else {
		x0 = gog_axis_map_to_view (x_map, rect->x);
		x1 = gog_axis_map_to_view (x_map, rect->x + rect->w);
		y0 = gog_axis_map_to_view (y_map, rect->y);
		y1 = gog_axis_map_to_view (y_map, rect->y + rect->h);
		if (fabs (y1 - y0) < .5) {
			y1 += .25;
			y0 -= .25;
		}
	}

	path[0].x = path[3].x = path[4].x = x0;
	path[1].x = path[2].x = x1;
	path[0].y = path[1].y = path[4].y = y0;
	path[2].y = path[3].y = y1;
	path[0].code = ART_MOVETO;
	path[1].code = ART_LINETO;
	path[2].code = ART_LINETO;
	path[3].code = ART_LINETO;
	path[4].code = ART_LINETO;
	path[5].code = ART_END;
	
	gog_renderer_draw_sharp_polygon (rend, path, 
					 fabs (x1 - x0) < 3. || fabs (y1 - y0) < 3.);
}

static void
gog_dropbar_view_render (GogView *view, GogViewAllocation const *bbox)
{
	GogBarColPlot const *model = GOG_BARCOL_PLOT (view->model);
	GogPlot1_5d const *gog_1_5d_model = GOG_PLOT1_5D (view->model);
	GogSeries1_5d const *series;
	GogAxisMap *x_map, *y_map;
	GogViewAllocation work;
	double *start_vals, *end_vals;
	double x;
	double step, offset, group_step;
	unsigned i, j;
	unsigned num_elements = gog_1_5d_model->num_elements;
	unsigned num_series = gog_1_5d_model->num_series;
	GSList *ptr;
	unsigned n, tmp;
	GogStyle *neg_style;
	ArtVpath **path1, **path2;
	GogObjectRole const *role = NULL;
	GogSeriesLines **lines;

	if (num_elements <= 0 || num_series <= 0)
		return;

	x_map = gog_axis_map_new (GOG_PLOT (model)->axis[0], 
				  view->allocation.x, view->allocation.w);
	y_map = gog_axis_map_new (GOG_PLOT (model)->axis[1], view->allocation.y + view->allocation.h, 
				  -view->allocation.h);
	
	if (!(gog_axis_map_is_valid (x_map) &&
	      gog_axis_map_is_valid (y_map))) {
		gog_axis_map_free (x_map);
		gog_axis_map_free (y_map);
		return;
	}

	/* lines, if any will be rendered after the bars, so we build the paths
	and render them at the end */
	path1    = g_alloca (num_series * sizeof (ArtVpath *));
	path2    = g_alloca (num_series * sizeof (ArtVpath *));
	lines    = g_alloca (num_series * sizeof (GogSeriesLines *));
	j = 0;
	step = 1. - model->overlap_percentage / 100.;
	group_step = model->gap_percentage / 100.;
	work.w = 1.0 / (1. + ((num_series - 1.0) * step) + group_step);
	step *= work.w;
	offset = - (step * (num_series - 1.0) + work.w) / 2.0; 


	for (ptr = gog_1_5d_model->base.series ; ptr != NULL ; ptr = ptr->next) {
		series = ptr->data;
		if (!gog_series_is_valid (GOG_SERIES (series)))
			continue;
		neg_style = gog_style_dup ((GOG_STYLED_OBJECT (series))->style);
		neg_style->outline.color ^= 0xffffff00;
		neg_style->fill.pattern.back ^= 0xffffff00;
		neg_style->fill.pattern.fore ^= 0xffffff00;
		x = offset;
		start_vals = go_data_vector_get_values (
			GO_DATA_VECTOR (series->base.values[1].data));
		n = go_data_vector_get_len (
			GO_DATA_VECTOR (series->base.values[1].data));
		end_vals = go_data_vector_get_values (
			GO_DATA_VECTOR (series->base.values[2].data));
		tmp = go_data_vector_get_len (
			GO_DATA_VECTOR (series->base.values[2].data));
		if (n > tmp)
			n = tmp;

		if (series->has_lines) {
			if (!role)
				role = gog_object_find_role_by_name (
							GOG_OBJECT (series), "Lines");
			lines[j] = GOG_SERIES_LINES (
					gog_object_get_child_by_role (GOG_OBJECT (series), role));
			path1[j] = g_new (ArtVpath, n + 1);
			path2[j] = g_new (ArtVpath, n + 1);
			path1[j][0].code = path2[j][0].code = ART_MOVETO;
			for (i = 1; i < n; i++)
				path1[j][i].code =path2[j][i].code = ART_LINETO;
			path1[j][n].code = path2[j][n].code = ART_END;
		} else
			path1[j] = NULL;
		for (i = 0; i < n; i++) {
			work.x = x;
			work.y = start_vals[i];
			work.h = end_vals[i] - work.y;
			if (series->has_lines) {
				if (model->horizontal) {
					path1[j][i].y = path2[j][i].y =
						gog_axis_map_to_view (y_map, work.x + work.w / 2.);
					path1[j][i].x = gog_axis_map_to_view (x_map, start_vals[i]);
					path2[j][i].x = gog_axis_map_to_view (x_map, end_vals[i]);
				} else {
					path1[j][i].x = path2[j][i].x =
						gog_axis_map_to_view (x_map, work.x + work.w / 2.);
					path1[j][i].y = gog_axis_map_to_view (y_map, start_vals[i]);
					path2[j][i].y = gog_axis_map_to_view (y_map, end_vals[i]);
				}
			}
			gog_renderer_push_style (view->renderer, (start_vals[i] <= end_vals[i])?
						GOG_STYLED_OBJECT (series)->style: neg_style);
					barcol_draw_rect (view->renderer, model->horizontal, x_map, y_map, &work);
			barcol_draw_rect (view->renderer, model->horizontal, x_map, y_map, &work);
			gog_renderer_pop_style (view->renderer);
			x += 1;
		}
		offset += step;
		g_object_unref (neg_style);
		j++;
	}
	for (j = 0; j < num_series; j++)
		if (path1[j] != NULL) {
			gog_renderer_push_style (view->renderer,
				gog_styled_object_get_style (GOG_STYLED_OBJECT (lines[j])));
			gog_series_lines_render (lines[j], view->renderer, bbox, path1[j], TRUE);
			gog_series_lines_render (lines[j], view->renderer, bbox, path2[j], FALSE);
			gog_renderer_pop_style (view->renderer);
			g_free (path2[j]);
			g_free (path1[j]);
		}

	gog_axis_map_free (x_map);
	gog_axis_map_free (y_map);
}

static gboolean
gog_dropbar_view_info_at_point (GogView *view, double x, double y,
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
gog_dropbar_view_class_init (GogViewClass *view_klass)
{
	view_klass->render	  = gog_dropbar_view_render;
	view_klass->info_at_point = gog_dropbar_view_info_at_point;
	view_klass->clip	  = TRUE;
}

GSF_DYNAMIC_CLASS (GogDropBarView, gog_dropbar_view,
	gog_dropbar_view_class_init, NULL,
	GOG_PLOT_VIEW_TYPE)
