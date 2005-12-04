/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-minmax.c
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
#include "gog-minmax.h"
#include <goffice/graph/gog-series-lines.h>
#include <goffice/graph/gog-view.h>
#include <goffice/graph/gog-renderer.h>
#include <goffice/gtk/goffice-gtk.h>
#include <goffice/utils/go-marker.h>
#include <goffice/app/go-plugin.h>

#include <gtk/gtkspinbutton.h>
#include <glib/gi18n.h>
#include <gsf/gsf-impl-utils.h>

enum {
	MINMAX_PROP_0,
	MINMAX_PROP_GAP_PERCENTAGE,
	MINMAX_PROP_HORIZONTAL,
	MINMAX_PROP_DEFAULT_STYLE_HAS_MARKERS
};

static GogObjectClass *gog_minmax_parent_klass;

static GType gog_minmax_view_get_type (void);

typedef GogSeries1_5d		GogMinMaxSeries;
typedef GogSeries1_5dClass	GogMinMaxSeriesClass;

static GogStyledObjectClass *series_parent_klass;

static void
gog_minmax_series_init_style (GogStyledObject *gso, GogStyle *style)
{
	GogSeries *series = GOG_SERIES (gso);
	GogMinMaxPlot const *plot;

	series_parent_klass->init_style (gso, style);
	if (series->plot == NULL)
		return;

	plot = GOG_MINMAX_PLOT (series->plot);
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
gog_minmax_series_class_init (GogStyledObjectClass *gso_klass)
{
	series_parent_klass = g_type_class_peek_parent (gso_klass);
	gso_klass->init_style = gog_minmax_series_init_style;
}

GSF_DYNAMIC_CLASS (GogMinMaxSeries, gog_minmax_series,
	gog_minmax_series_class_init, NULL,
	GOG_SERIES1_5D_TYPE)

/*****************************************************************************/

static void
gog_minmax_plot_set_property (GObject *obj, guint param_id,
			      GValue const *value, GParamSpec *pspec)
{
	GogMinMaxPlot *minmax = GOG_MINMAX_PLOT (obj);

	switch (param_id) {
	case MINMAX_PROP_GAP_PERCENTAGE:
		minmax->gap_percentage = g_value_get_int (value);
		break;

	case MINMAX_PROP_HORIZONTAL:
		minmax->horizontal = g_value_get_boolean (value);
		break;

	case MINMAX_PROP_DEFAULT_STYLE_HAS_MARKERS:
		minmax->default_style_has_markers = g_value_get_boolean (value);
		break;

	default: G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, param_id, pspec);
		 return; /* NOTE : RETURN */
	}
	gog_object_emit_changed (GOG_OBJECT (obj), TRUE);
}

static void
gog_minmax_plot_get_property (GObject *obj, guint param_id,
			      GValue *value, GParamSpec *pspec)
{
	GogMinMaxPlot *minmax = GOG_MINMAX_PLOT (obj);

	switch (param_id) {
	case MINMAX_PROP_GAP_PERCENTAGE:
		g_value_set_int (value, minmax->gap_percentage);
		break;
	case MINMAX_PROP_HORIZONTAL:
		g_value_set_boolean (value, minmax->horizontal);
		break;
	case MINMAX_PROP_DEFAULT_STYLE_HAS_MARKERS:
		g_value_set_boolean (value, minmax->default_style_has_markers);
		break;
	default: G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, param_id, pspec);
		 break;
	}
}

static char const *
gog_minmax_plot_type_name (G_GNUC_UNUSED GogObject const *item)
{
	/* xgettext : the base for how to name min/max line plot objects
	 * eg The 2nd min/max line plot in a chart will be called
	 * 	PlotMinMax2 */
	return N_("PlotMinMax");
}

static GOData *
gog_minmax_axis_get_bounds (GogPlot *plot, GogAxisType axis,
			    GogPlotBoundInfo *bounds)
{
	GogPlot1_5d *model = GOG_PLOT1_5D (plot);
	GogPlot1_5dClass *plot1_5d_klass = GOG_PLOT1_5D_CLASS (gog_minmax_parent_klass);
	GOData *data;

	data = (plot1_5d_klass->base.axis_get_bounds) (plot, axis, bounds); 
	
	if (axis == gog_axis_get_atype (gog_plot1_5d_get_index_axis (model))) {
		bounds->val.minima -= .5;
		bounds->val.maxima += .5;
		bounds->logical.minima = -.5;
		bounds->center_on_ticks = FALSE;
	}

	return data;
}

static void
cb_gap_changed (GtkAdjustment *adj, GObject *minmax)
{
	g_object_set (minmax, "gap-percentage", (int)adj->value, NULL);
}

static void
gog_minmax_plot_populate_editor (GogObject *item,
				 GogEditor *editor,
				 G_GNUC_UNUSED GogDataAllocator *dalloc,
				 GOCmdContext *cc)
{
	GtkWidget  *w;
	GogMinMaxPlot *minmax = GOG_MINMAX_PLOT (item);
	char const *dir = go_plugin_get_dir_name (
		go_plugins_get_plugin_by_id ("GOffice_plot_barcol"));
	char	 *path = g_build_filename (dir, "gog-minmax-prefs.glade", NULL);
	GladeXML *gui = go_libglade_new (path, "gog_minmax_prefs", NULL, cc);

	g_free (path);
	if (gui == NULL)
		return;

	w = glade_xml_get_widget (gui, "gap_spinner");
	gtk_spin_button_set_value (GTK_SPIN_BUTTON (w), minmax->gap_percentage);
	g_signal_connect (G_OBJECT (gtk_spin_button_get_adjustment (GTK_SPIN_BUTTON (w))),
		"value_changed",
		G_CALLBACK (cb_gap_changed), minmax);

	w = glade_xml_get_widget (gui, "gog_minmax_prefs");
	g_object_set_data_full (G_OBJECT (w),
		"state", gui, (GDestroyNotify)g_object_unref);

	gog_editor_add_page (editor, w, _("Properties"));
	(GOG_OBJECT_CLASS(gog_minmax_parent_klass)->populate_editor) (item, editor, dalloc, cc);
}

static gboolean
gog_minmax_swap_x_and_y (GogPlot1_5d *model)
{
	return GOG_MINMAX_PLOT (model)->horizontal;
}

static void
gog_minmax_plot_class_init (GogPlot1_5dClass *gog_plot_1_5d_klass)
{
	GObjectClass   *gobject_klass = (GObjectClass *) gog_plot_1_5d_klass;
	GogObjectClass *gog_object_klass = (GogObjectClass *) gog_plot_1_5d_klass;
	GogPlotClass   *plot_klass = (GogPlotClass *) gog_plot_1_5d_klass;
	gog_minmax_parent_klass = g_type_class_peek_parent (gog_plot_1_5d_klass);

	gobject_klass->set_property = gog_minmax_plot_set_property;
	gobject_klass->get_property = gog_minmax_plot_get_property;

	g_object_class_install_property (gobject_klass, MINMAX_PROP_GAP_PERCENTAGE,
		g_param_spec_int ("gap-percentage", "gap percentage",
			"The padding around each group as a percentage of their width",
			0, 500, 150, G_PARAM_READWRITE | GOG_PARAM_PERSISTENT));
	g_object_class_install_property (gobject_klass, MINMAX_PROP_HORIZONTAL,
		g_param_spec_boolean ("horizontal", "horizontal",
			"horizontal or vertical lines",
			FALSE,
			G_PARAM_READWRITE | GOG_PARAM_PERSISTENT));
	g_object_class_install_property (gobject_klass, MINMAX_PROP_DEFAULT_STYLE_HAS_MARKERS,
		g_param_spec_boolean ("default-style-has-markers", NULL,
			"Should the default style of a series include markers",
			FALSE, G_PARAM_READWRITE | GOG_PARAM_PERSISTENT));

	gog_object_klass->type_name	= gog_minmax_plot_type_name;
	gog_object_klass->view_type	= gog_minmax_view_get_type ();
	gog_object_klass->populate_editor	= gog_minmax_plot_populate_editor;

	{
		static GogSeriesDimDesc dimensions[] = {
			{ N_("Labels"), GOG_SERIES_SUGGESTED, TRUE,
			  GOG_DIM_LABEL, GOG_MS_DIM_CATEGORIES },
			{ N_("Min"), GOG_SERIES_REQUIRED, FALSE,
			  GOG_DIM_VALUE, GOG_MS_DIM_LOW },
			{ N_("Max"), GOG_SERIES_REQUIRED, FALSE,
			  GOG_DIM_VALUE, GOG_MS_DIM_HIGH },
		};
		plot_klass->desc.series.dim = dimensions;
		plot_klass->desc.series.num_dim = G_N_ELEMENTS (dimensions);
	}
	plot_klass->desc.series.style_fields = GOG_STYLE_LINE | GOG_STYLE_MARKER;
	plot_klass->axis_get_bounds   		= gog_minmax_axis_get_bounds;
	plot_klass->series_type = gog_minmax_series_get_type ();

	gog_plot_1_5d_klass->swap_x_and_y = gog_minmax_swap_x_and_y;
	gog_plot_1_5d_klass->update_stacked_and_percentage = NULL;
}

static void
gog_minmax_plot_init (GogMinMaxPlot *minmax)
{
	minmax->default_style_has_markers = FALSE;
	minmax->gap_percentage = 150;
	GOG_PLOT1_5D (minmax)->support_lines = TRUE;
}

GSF_DYNAMIC_CLASS (GogMinMaxPlot, gog_minmax_plot,
	gog_minmax_plot_class_init, gog_minmax_plot_init,
	GOG_PLOT1_5D_TYPE)

/*****************************************************************************/
typedef GogPlotView		GogMinMaxView;
typedef GogPlotViewClass	GogMinMaxViewClass;

static void
gog_minmax_view_render (GogView *view, GogViewAllocation const *bbox)
{
	GogMinMaxPlot const *model = GOG_MINMAX_PLOT (view->model);
	GogPlot1_5d const *gog_1_5d_model = GOG_PLOT1_5D (view->model);
	GogSeries1_5d const *series;
	GogAxisMap *x_map, *y_map;
	gboolean is_vertical = ! (model->horizontal);
	double *max_vals, *min_vals;
	double x;
	double step, offset;
	unsigned i;
	unsigned num_elements = gog_1_5d_model->num_elements;
	unsigned num_series = gog_1_5d_model->num_series;
	GSList *ptr;
	unsigned n, tmp;
	ArtVpath path[3], *Mpath, *mpath;
	GogObjectRole const *role = NULL;
	GogSeriesLines *lines;

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

	step = 1. / (num_series + model->gap_percentage / 100.);
	offset = - step * (num_series - 1) / 2.;
	path[0].code = ART_MOVETO;
	path[1].code = ART_LINETO;
	path[2].code = ART_END;

	for (ptr = gog_1_5d_model->base.series ; ptr != NULL ; ptr = ptr->next) {
		series = ptr->data;
		if (!gog_series_is_valid (GOG_SERIES (series)))
			continue;
		x = offset;
		min_vals = go_data_vector_get_values (
			GO_DATA_VECTOR (series->base.values[1].data));
		n = go_data_vector_get_len (
			GO_DATA_VECTOR (series->base.values[1].data));
		max_vals = go_data_vector_get_values (
			GO_DATA_VECTOR (series->base.values[2].data));
		tmp = go_data_vector_get_len (
			GO_DATA_VECTOR (series->base.values[2].data));
		if (n > tmp)
			n = tmp;
		Mpath = g_new (ArtVpath, n + 1);
		mpath = g_new (ArtVpath, n + 1);
		gog_renderer_push_style (view->renderer, GOG_STYLED_OBJECT (series)->style);

		for (i = 0; i < n; i++) {

			if (is_vertical) {
				mpath[i].x = Mpath[i].x = path[0].x = path[1].x = gog_axis_map_to_view (x_map, x);
				mpath[i].y = path[0].y = gog_axis_map_to_view (y_map, min_vals[i]);
				Mpath[i].y = path[1].y = gog_axis_map_to_view (y_map, max_vals[i]);
			} else {
				mpath[i].y = Mpath[i].y = path[0].y = path[1].y =  gog_axis_map_to_view (y_map, x);
				mpath[i].x = path[0].x = gog_axis_map_to_view (x_map, min_vals[i]);
				Mpath[i].x =path[1].x = gog_axis_map_to_view (x_map, max_vals[i]);
			}
			gog_renderer_draw_sharp_path (view->renderer, path);
			x += 1;
		}
		if (series->has_lines) {
			if (!role)
				role = gog_object_find_role_by_name (
							GOG_OBJECT (series), "Lines");
			lines = GOG_SERIES_LINES (
					gog_object_get_child_by_role (GOG_OBJECT (series), role));
			mpath[0].code = Mpath[0].code = ART_MOVETO;
			for (i = 1; i < n; i++)
				mpath[i].code = Mpath[i].code = ART_LINETO;
			mpath[n].code = Mpath[n].code = ART_END;
			gog_renderer_push_style (view->renderer,
				gog_styled_object_get_style (GOG_STYLED_OBJECT (lines)));
			gog_series_lines_render (lines, view->renderer, bbox, mpath, TRUE);
			gog_series_lines_render (lines, view->renderer, bbox, Mpath, FALSE);
			gog_renderer_pop_style (view->renderer);
		}
		if (model->default_style_has_markers)
			for (i = 0; i < n; i++) {
				gog_renderer_draw_marker (view->renderer, mpath[i].x, mpath[i].y);
				gog_renderer_draw_marker (view->renderer, Mpath[i].x, Mpath[i].y);
			}
		gog_renderer_pop_style (view->renderer);
		g_free (Mpath);
		g_free (mpath);
		offset += step;
	}

	gog_axis_map_free (x_map);
	gog_axis_map_free (y_map);
}

static gboolean
gog_minmax_view_info_at_point (GogView *view, double x, double y,
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
gog_minmax_view_class_init (GogViewClass *view_klass)
{
	view_klass->render	  = gog_minmax_view_render;
	view_klass->info_at_point = gog_minmax_view_info_at_point;
	view_klass->clip	  = TRUE;
}

GSF_DYNAMIC_CLASS (GogMinMaxView, gog_minmax_view,
	gog_minmax_view_class_init, NULL,
	GOG_PLOT_VIEW_TYPE)
