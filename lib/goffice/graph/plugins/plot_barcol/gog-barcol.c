/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * go-barcol.c
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
#include "gog-barcol.h"
#include <goffice/graph/gog-view.h>
#include <goffice/graph/gog-renderer.h>
#include <goffice/graph/gog-style.h>
#include <goffice/graph/gog-axis.h>
#include <goffice/graph/go-data.h>
#include <goffice/utils/go-color.h>
#include <goffice/utils/go-math.h>

#include <glib/gi18n.h>
#include <gsf/gsf-impl-utils.h>

enum {
	BARCOL_PROP_0,
	BARCOL_PROP_GAP_PERCENTAGE,
	BARCOL_PROP_OVERLAP_PERCENTAGE,
	BARCOL_PROP_HORIZONTAL
};

static GogObjectClass *gog_barcol_parent_klass;

static GType gog_barcol_view_get_type (void);

static void
gog_barcol_plot_set_property (GObject *obj, guint param_id,
			      GValue const *value, GParamSpec *pspec)
{
	GogBarColPlot *barcol = GOG_BARCOL_PLOT (obj);

	switch (param_id) {
	case BARCOL_PROP_GAP_PERCENTAGE:
		barcol->gap_percentage = g_value_get_int (value);
		break;

	case BARCOL_PROP_OVERLAP_PERCENTAGE:
		barcol->overlap_percentage = g_value_get_int (value);
		break;
	case BARCOL_PROP_HORIZONTAL:
		barcol->horizontal = g_value_get_boolean (value);
		break;

	default: G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, param_id, pspec);
		 return; /* NOTE : RETURN */
	}
	gog_object_emit_changed (GOG_OBJECT (obj), TRUE);
}

static void
gog_barcol_plot_get_property (GObject *obj, guint param_id,
			      GValue *value, GParamSpec *pspec)
{
	GogBarColPlot *barcol = GOG_BARCOL_PLOT (obj);

	switch (param_id) {
	case BARCOL_PROP_GAP_PERCENTAGE:
		g_value_set_int (value, barcol->gap_percentage);
		break;
	case BARCOL_PROP_OVERLAP_PERCENTAGE:
		g_value_set_int (value, barcol->overlap_percentage);
		break;
	case BARCOL_PROP_HORIZONTAL:
		g_value_set_boolean (value, barcol->horizontal);
		break;
	default: G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, param_id, pspec);
		 break;
	}
}

static char const *
gog_barcol_plot_type_name (G_GNUC_UNUSED GogObject const *item)
{
	/* xgettext : the base for how to name bar/col plot objects
	 * eg The 2nd bar/col plot in a chart will be called
	 * 	PlotBarCol2 */
	return N_("PlotBarCol");
}

extern gpointer gog_barcol_plot_pref (GogBarColPlot *barcol, GnmCmdContext *cc);
static gpointer
gog_barcol_plot_editor (GogObject *item,
			G_GNUC_UNUSED GogDataAllocator *dalloc,
			GnmCmdContext *cc)
{
	return gog_barcol_plot_pref (GOG_BARCOL_PLOT (item), cc);
}

static gboolean
gog_barcol_swap_x_and_y (GogPlot1_5d *model)
{
	return GOG_BARCOL_PLOT (model)->horizontal;
}

static void
gog_barcol_update_stacked_and_percentage (GogPlot1_5d *model,
					  double **vals, GogErrorBar **errors, unsigned const *lengths)
{
	unsigned i, j;
	double neg_sum, pos_sum, tmp, errplus, errminus, tmpmin, tmpmax;

	for (i = model->num_elements ; i-- > 0 ; ) {
		neg_sum = pos_sum = 0.;
		tmpmin = DBL_MAX;
		tmpmax = -DBL_MAX;
		for (j = 0 ; j < model->num_series ; j++) {
			if (i >= lengths[j])
				continue;
			tmp = vals[j][i];
			if (!go_finite (tmp))
				continue;
			if (gog_error_bar_is_visible (errors[j])) {
					gog_error_bar_get_bounds (errors[j], i, &errminus, &errplus);
					errminus = errminus > 0. ? errminus : 0.;
					errplus = errplus > 0. ? errplus : 0.;
				} else
					errplus = errminus = 0.;
			if (tmp > 0.) {
				pos_sum += tmp;
				errminus = (pos_sum - errminus < neg_sum)? neg_sum - pos_sum + errminus: 0.;
			} else {
				neg_sum += tmp;
				errplus = (neg_sum + errplus > pos_sum)? neg_sum - pos_sum + errplus: 0.;
			}
				if (tmpmin > neg_sum - errminus)
					tmpmin = neg_sum - errminus;
				if (tmpmax < pos_sum + errplus)
					tmpmax = pos_sum + errplus;
		}
		if (GOG_1_5D_STACKED == model->type) {
			if (model->minima > tmpmin)
				model->minima = tmpmin;
			if (model->maxima < tmpmax)
				model->maxima = tmpmax;
		} else {
			if (model->minima > tmpmin / (pos_sum - neg_sum))
				model->minima = tmpmin / (pos_sum - neg_sum);
			if (model->maxima < tmpmax / (pos_sum - neg_sum))
				model->maxima = tmpmax / (pos_sum - neg_sum);
		}
	}
}

static GOData *
gog_barcol_axis_get_bounds (GogPlot *plot, GogAxisType axis,
			    GogPlotBoundInfo *bounds)
{
	GogPlot1_5d *model = GOG_PLOT1_5D (plot);
	GogPlot1_5dClass *plot1_5d_klass = GOG_PLOT1_5D_CLASS (gog_barcol_parent_klass);
	GOData *data;

	data = (plot1_5d_klass->base.axis_get_bounds) (plot, axis, bounds); 
	
	if (axis == gog_axis_get_atype (gog_plot1_5d_get_index_axis (model))) {
		bounds->val.minima -= .5;
		bounds->val.maxima += .5;
		bounds->logical.minima = -.5;
	}

	return data;
}

static void
gog_barcol_plot_class_init (GogPlot1_5dClass *gog_plot_1_5d_klass)
{
	GObjectClass   *gobject_klass = (GObjectClass *) gog_plot_1_5d_klass;
	GogObjectClass *gog_object_klass = (GogObjectClass *) gog_plot_1_5d_klass;
	GogPlotClass   *plot_klass = (GogPlotClass *) gog_plot_1_5d_klass;

	gog_barcol_parent_klass = g_type_class_peek_parent (gog_plot_1_5d_klass);
	gobject_klass->set_property = gog_barcol_plot_set_property;
	gobject_klass->get_property = gog_barcol_plot_get_property;

	g_object_class_install_property (gobject_klass, BARCOL_PROP_GAP_PERCENTAGE,
		g_param_spec_int ("gap_percentage", "gap percentage",
			"The padding around each group as a percentage of their width",
			0, 500, 150, G_PARAM_READWRITE | GOG_PARAM_PERSISTENT));
	g_object_class_install_property (gobject_klass, BARCOL_PROP_OVERLAP_PERCENTAGE,
		g_param_spec_int ("overlap_percentage", "overlap percentage",
			"The distance between series as a percentage of their width",
			-100, 100, 0, G_PARAM_READWRITE | GOG_PARAM_PERSISTENT));
	g_object_class_install_property (gobject_klass, BARCOL_PROP_HORIZONTAL,
		g_param_spec_boolean ("horizontal", "horizontal",
			"horizontal bars or vertical columns",
			FALSE,
			G_PARAM_READWRITE | GOG_PARAM_PERSISTENT));

	gog_object_klass->type_name	= gog_barcol_plot_type_name;
	gog_object_klass->editor	= gog_barcol_plot_editor;
	gog_object_klass->view_type	= gog_barcol_view_get_type ();

	plot_klass->desc.series.style_fields	= GOG_STYLE_OUTLINE | GOG_STYLE_FILL;
	plot_klass->axis_get_bounds   		= gog_barcol_axis_get_bounds;

	gog_plot_1_5d_klass->swap_x_and_y = gog_barcol_swap_x_and_y;
	gog_plot_1_5d_klass->update_stacked_and_percentage =
		gog_barcol_update_stacked_and_percentage;
}

static void
gog_barcol_plot_init (GogBarColPlot *model)
{
	model->gap_percentage = 150;
}

GSF_CLASS (GogBarColPlot, gog_barcol_plot,
	   gog_barcol_plot_class_init, gog_barcol_plot_init,
	   GOG_PLOT1_5D_TYPE)

/*****************************************************************************/
typedef GogPlotView		GogBarColView;
typedef GogPlotViewClass	GogBarColViewClass;

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
		x0 = gog_axis_map_to_canvas (x_map, rect->y);
		x1 = gog_axis_map_to_canvas (x_map, rect->y + rect->h);
		y0 = gog_axis_map_to_canvas (y_map, rect->x);
		y1 = gog_axis_map_to_canvas (y_map, rect->x + rect->w);
	} else {
		x0 = gog_axis_map_to_canvas (x_map, rect->x);
		x1 = gog_axis_map_to_canvas (x_map, rect->x + rect->w);
		y0 = gog_axis_map_to_canvas (y_map, rect->y);
		y1 = gog_axis_map_to_canvas (y_map, rect->y + rect->h);
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
					 fabs (x1 - x0) < 3. || fabs (y1 - y0) < 3.
					 , NULL);
}

typedef struct {
	double		plus;
	double 		minus;
	double		x;
	double		y;
} ErrorBarData;

static void
gog_barcol_view_render (GogView *view, GogViewAllocation const *bbox)
{
	GogBarColPlot const *model = GOG_BARCOL_PLOT (view->model);
	GogPlot1_5d const *gog_1_5d_model = GOG_PLOT1_5D (view->model);
	GogSeries1_5d const *series;
	GogViewAllocation work;
	GogRenderer *rend = view->renderer;
	GogAxisMap *x_map, *y_map;
	gboolean is_vertical = ! (model->horizontal);
	double **vals, sum, neg_base, pos_base, tmp;
	double x;
	double col_step, group_step, offset, data_scale;
	unsigned i, j;
	unsigned num_elements = gog_1_5d_model->num_elements;
	unsigned num_series = gog_1_5d_model->num_series;
	GogPlot1_5dType const type = gog_1_5d_model->type;
	GogStyle **styles;
	ErrorBarData **error_data;
	GogErrorBar **errors;
	GSList *ptr;
	unsigned *lengths;
	double plus, minus;

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

	vals = g_alloca (num_series * sizeof (double *));
	lengths = g_alloca (num_series * sizeof (unsigned));
	styles = g_alloca (num_series * sizeof (GogStyle *));
	errors = g_alloca (num_series * sizeof (GogErrorBar *));
	error_data = g_alloca (num_series * sizeof (ErrorBarData *));
	
	i = 0;
	for (ptr = gog_1_5d_model->base.series ; ptr != NULL ; ptr = ptr->next) {
		series = ptr->data;
		if (!gog_series_is_valid (GOG_SERIES (series)))
			continue;
		vals[i] = go_data_vector_get_values (
			GO_DATA_VECTOR (series->base.values[1].data));
		lengths[i] = go_data_vector_get_len (
			GO_DATA_VECTOR (series->base.values[1].data));
		styles[i] = GOG_STYLED_OBJECT (series)->style;
		errors[i] = series->errors;
		if (gog_error_bar_is_visible (series->errors)) 
			error_data[i] = g_malloc (sizeof (ErrorBarData) * lengths[i]);
		else 
			error_data[i] = NULL;
		i++;
	}

	/* work in coordinates drawing bars from the top */
	col_step = 1. - model->overlap_percentage / 100.;
	group_step = model->gap_percentage / 100.;
	work.h = 1.0 / (1. + ((num_series - 1.0) * col_step) + group_step);
	col_step *= work.h;
	offset = (col_step * (num_series - 1.0) + work.h) / 2.0; 
	data_scale = 1.0;

	for (i = 0 ; i < num_elements ; i++) {
		if (type == GOG_1_5D_AS_PERCENTAGE) {
			sum = 0.;
			for (j = num_series ; j-- > 0 ; ) {
				if (i >= lengths[j])
					continue;
				tmp = vals[j][i];
				if (!go_finite (tmp))
					continue;
				if (tmp > 0.)
					sum += tmp;
				else
					sum -= tmp;
			}

			data_scale = (fabs (go_sub_epsilon (sum)) > 0.0) ? 1.0 / sum : 1.0;
		}

		pos_base = neg_base = 0.0;
		for (j = 0 ; j < num_series ; j++) {
			
			work.y = (double) j * col_step + (double) i - offset;
			
			if (i >= lengths[j])
				continue;
			tmp = vals[j][i];
			if (!go_finite (tmp))
				continue;
			if (gog_error_bar_is_visible (errors[j])) {
				gog_error_bar_get_bounds (errors[j], i, &minus, &plus);
			}
			tmp *= data_scale;
			if (tmp >= 0.) {
				work.x = pos_base;
				work.w = tmp;
				if (GOG_1_5D_NORMAL != type)
					pos_base += tmp;
			} else {
				work.x = neg_base + tmp;
				work.w = -tmp;
				if (GOG_1_5D_NORMAL != type)
					neg_base += tmp;
			}

			gog_renderer_push_style (view->renderer, styles[j]);
			barcol_draw_rect (rend, is_vertical, x_map, y_map, &work);
			gog_renderer_pop_style (view->renderer);
			
			if (gog_error_bar_is_visible (errors[j])) {
				x = tmp > 0 ? work.x + work.w: work.x;
				error_data[j][i].plus = plus * data_scale;
				error_data[j][i].minus =minus * data_scale;
				if (is_vertical) {
					error_data[j][i].x = work.y + work.h / 2.0;
					error_data[j][i].y = x;
				} else {
					error_data[j][i].x = x;
					error_data[j][i].y = work.y + work.h / 2.0;
				}
			}
		}
	}
	/*Now draw error bars and clean*/
	for (i = 0; i < num_series; i++)
		if (gog_error_bar_is_visible (errors[i])) {
			for (j = 0; j < lengths[i]; j++)
				gog_error_bar_render (errors[i], view->renderer,
						      x_map, y_map,
						      error_data[i][j].x , error_data[i][j].y,
						      error_data[i][j].minus, error_data[i][j].plus,
						      model->horizontal);
			g_free (error_data[i]);
		}

	gog_axis_map_free (x_map);
	gog_axis_map_free (y_map);
}

static gboolean
gog_barcol_view_info_at_point (GogView *view, double x, double y,
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
gog_barcol_view_class_init (GogViewClass *view_klass)
{
	view_klass->render	  = gog_barcol_view_render;
	view_klass->info_at_point = gog_barcol_view_info_at_point;
	view_klass->clip	  = TRUE;
}

static GSF_CLASS (GogBarColView, gog_barcol_view,
		  gog_barcol_view_class_init, NULL,
		  GOG_PLOT_VIEW_TYPE)
