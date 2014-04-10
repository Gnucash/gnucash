/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * go-boxplot.c
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
#include "gog-boxplot.h"
#include <goffice/graph/gog-series-impl.h>
#include <goffice/graph/gog-view.h>
#include <goffice/graph/gog-renderer.h>
#include <goffice/graph/gog-style.h>
#include <goffice/graph/gog-axis.h>
#include <goffice/data/go-data.h>
#include <goffice/utils/go-math.h>
#include <goffice/gtk/goffice-gtk.h>
#include <goffice/app/module-plugin-defs.h>

#include <glib/gi18n.h>
#include <glade/glade-xml.h>
#include <gtk/gtkspinbutton.h>
#include <gtk/gtkenums.h>
#include <gsf/gsf-impl-utils.h>
#include <string.h>

GOFFICE_PLUGIN_MODULE_HEADER;

static GogObjectClass *gog_box_plot_parent_klass;

static GType gog_box_plot_view_get_type (void);

static void
cb_gap_changed (GtkAdjustment *adj, GObject *boxplot)
{
	g_object_set (boxplot, "gap-percentage", (int)adj->value, NULL);
}

static gpointer
gog_box_plot_pref (GogObject *obj,
		   GogDataAllocator *dalloc, GOCmdContext *cc)
{
	GtkWidget  *w;
	GogBoxPlot *boxplot = GOG_BOX_PLOT (obj);
	char const *dir = go_plugin_get_dir_name (
		go_plugins_get_plugin_by_id ("GOffice_plot_boxes"));
	char	 *path = g_build_filename (dir, "gog-boxplot-prefs.glade", NULL);
	GladeXML *gui = go_libglade_new (path, "gog_box_plot_prefs", NULL, cc);

	g_free (path);
        if (gui == NULL)
                return NULL;

	w = glade_xml_get_widget (gui, "gap_spinner");
	gtk_spin_button_set_value (GTK_SPIN_BUTTON (w), boxplot->gap_percentage);
	g_signal_connect (G_OBJECT (gtk_spin_button_get_adjustment (GTK_SPIN_BUTTON (w))),
		"value_changed",
		G_CALLBACK (cb_gap_changed), boxplot);

	w = glade_xml_get_widget (gui, "gog_box_plot_prefs");
	g_object_set_data_full (G_OBJECT (w),
		"state", gui, (GDestroyNotify)g_object_unref);

	return w;
}

static void
gog_box_plot_populate_editor (GogObject *item,
			      GogEditor *editor,
			      G_GNUC_UNUSED GogDataAllocator *dalloc,
			      GOCmdContext *cc)
{
	gog_editor_add_page (editor, gog_box_plot_pref (item, dalloc, cc), _("Properties"));
	
	(GOG_OBJECT_CLASS(gog_box_plot_parent_klass)->populate_editor) (item, editor, dalloc, cc);
}

enum {
	BOX_PLOT_PROP_0,
	BOX_PLOT_PROP_GAP_PERCENTAGE,
};

typedef struct {
	GogSeries base;
	int	 gap_percentage;
	double vals[5];
} GogBoxPlotSeries;
typedef GogSeriesClass GogBoxPlotSeriesClass;

#define GOG_BOX_PLOT_SERIES_TYPE	(gog_box_plot_series_get_type ())
#define GOG_BOX_PLOT_SERIES(o)	(G_TYPE_CHECK_INSTANCE_CAST ((o), GOG_BOX_PLOT_SERIES_TYPE, GogBoxPlotSeries))
#define IS_GOG_BOX_PLOT_SERIES(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOG_BOX_PLOT_SERIES_TYPE))

GType gog_box_plot_series_get_type (void);

static char const *
gog_box_plot_type_name (G_GNUC_UNUSED GogObject const *item)
{
	/* xgettext : the base for how to name box-plot objects
	 * eg The 2nd box-plot in a chart will be called
	 * 	BoxPlot2 */
	return N_("Box-Plot");
}

static void
gog_box_plot_set_property (GObject *obj, guint param_id,
			      GValue const *value, GParamSpec *pspec)
{
	GogBoxPlot *boxplot = GOG_BOX_PLOT (obj);

	switch (param_id) {
	case BOX_PLOT_PROP_GAP_PERCENTAGE:
		boxplot->gap_percentage = g_value_get_int (value);
		break;
	default: G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, param_id, pspec);
		 return; /* NOTE : RETURN */
	}
	gog_object_emit_changed (GOG_OBJECT (obj), TRUE);
}

static void
gog_box_plot_get_property (GObject *obj, guint param_id,
			      GValue *value, GParamSpec *pspec)
{
	GogBoxPlot *boxplot = GOG_BOX_PLOT (obj);

	switch (param_id) {
	case BOX_PLOT_PROP_GAP_PERCENTAGE:
		g_value_set_int (value, boxplot->gap_percentage);
		break;
	default: G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, param_id, pspec);
		 break;
	}
}

static void
gog_box_plot_update (GogObject *obj)
{
	GogBoxPlot *model = GOG_BOX_PLOT (obj);
	GogBoxPlotSeries *series;
	GSList *ptr;
	double min, max;
	unsigned num_series = 0;

	min =  DBL_MAX;
	max = -DBL_MAX;
	for (ptr = model->base.series ; ptr != NULL ; ptr = ptr->next) {
		series = GOG_BOX_PLOT_SERIES (ptr->data);
		if (!gog_series_is_valid (GOG_SERIES (series)) ||
			!go_data_vector_get_len (GO_DATA_VECTOR (series->base.values[0].data)))
			continue;
		num_series++;
		if (series->vals[0] < min)
			min = series->vals[0];
		if (series->vals[4] > max)
			max = series->vals[4];
	}
	if (min == DBL_MAX)
		min = 0.;
	if (max == -DBL_MAX)
		max = 1.;
	if (model->min != min || model->max != max || model->num_series != num_series) {
		model->min = min;
		model->max = max;
		model->num_series = num_series;
		gog_axis_bound_changed (model->base.axis[0], GOG_OBJECT (model));
	}
	gog_object_emit_changed (GOG_OBJECT (obj), FALSE);
}

static GOData *
gog_box_plot_axis_get_bounds (GogPlot *plot, GogAxisType axis,
			      GogPlotBoundInfo *bounds)
{
	GogBoxPlot *model = GOG_BOX_PLOT (plot);

	bounds->val.minima = model->min;
	bounds->val.maxima = model->max;
	bounds->is_discrete = FALSE;

	return NULL;
}

static void
gog_box_plot_class_init (GogPlotClass *gog_plot_klass)
{
	GObjectClass *gobject_klass = (GObjectClass *) gog_plot_klass;
	GogObjectClass *gog_object_klass = (GogObjectClass *) gog_plot_klass;
	GogPlotClass   *plot_klass = (GogPlotClass *) gog_plot_klass;

	gog_box_plot_parent_klass = g_type_class_peek_parent (gog_plot_klass);

	gobject_klass->set_property = gog_box_plot_set_property;
	gobject_klass->get_property = gog_box_plot_get_property;
	g_object_class_install_property (gobject_klass, BOX_PLOT_PROP_GAP_PERCENTAGE,
		g_param_spec_int ("gap-percentage", "gap percentage",
			"The padding around each group as a percentage of their width",
			0, 500, 150, G_PARAM_READWRITE | GOG_PARAM_PERSISTENT));

	gog_object_klass->type_name	= gog_box_plot_type_name;
	gog_object_klass->view_type	= gog_box_plot_view_get_type ();
	gog_object_klass->update	= gog_box_plot_update;
	gog_object_klass->populate_editor = gog_box_plot_populate_editor;

	{
		static GogSeriesDimDesc dimensions[] = {
			{ N_("Values"), GOG_SERIES_REQUIRED, FALSE,
			  GOG_DIM_VALUE, GOG_MS_DIM_VALUES },
		};
		plot_klass->desc.series.dim = dimensions;
		plot_klass->desc.series.num_dim = G_N_ELEMENTS (dimensions);
	}
	plot_klass->desc.num_series_min = 1;
	plot_klass->desc.num_series_max = G_MAXINT;
	plot_klass->series_type = gog_box_plot_series_get_type ();
	plot_klass->axis_set = GOG_AXIS_SET_X;
	plot_klass->desc.series.style_fields	= GOG_STYLE_LINE | GOG_STYLE_FILL;
	plot_klass->axis_get_bounds   		= gog_box_plot_axis_get_bounds;
}

static void
gog_box_plot_init (GogBoxPlot *model)
{
	model->gap_percentage = 150;
}

GSF_DYNAMIC_CLASS (GogBoxPlot, gog_box_plot,
	gog_box_plot_class_init, gog_box_plot_init,
	GOG_PLOT_TYPE)

/*****************************************************************************/
typedef GogPlotView		GogBoxPlotView;
typedef GogPlotViewClass	GogBoxPlotViewClass;

static void
gog_box_plot_view_render (GogView *view, GogViewAllocation const *bbox)
{
	GogBoxPlot const *model = GOG_BOX_PLOT (view->model);
	GogAxisMap *x_map;
	GogBoxPlotSeries const *series;
	double hrect, hser, y, hbar;
	double min, qu1, med, qu3, max;
	ArtVpath	path[6];
	GogViewAllocation rect;
	GSList *ptr;
	double line_width;
	GogStyle *style;

	x_map = gog_axis_map_new (GOG_PLOT (model)->axis[0], 
				  view->allocation.x, view->allocation.w);

	if (!gog_axis_map_is_valid (x_map)) {
		gog_axis_map_free (x_map);
		return;
	}

	hser = view->allocation.h / model->num_series;
	hrect = hser / (1. + model->gap_percentage / 100.);
	y = view->allocation.y + view->allocation.h - hser / 2.;
	hrect /= 2.;
	hbar = hrect / 2.;
	path[0].code = ART_MOVETO;
	path[1].code = ART_LINETO;
	path[3].code = ART_LINETO;
	path[4].code = ART_LINETO;
	path[5].code = ART_END;
		
	for (ptr = model->base.series ; ptr != NULL ; ptr = ptr->next) {
		series = ptr->data;
		if (!gog_series_is_valid (GOG_SERIES (series)) ||
			!go_data_vector_get_len (GO_DATA_VECTOR (series->base.values[0].data)))
			continue;
		style = GOG_STYLED_OBJECT (series)->style;
		line_width = style->line.width / 2.;
		gog_renderer_push_style (view->renderer, style);
		min = gog_axis_map_to_view (x_map, series->vals[0]);
		qu1 = gog_axis_map_to_view (x_map, series->vals[1]);
		med = gog_axis_map_to_view (x_map, series->vals[2]);
		qu3 = gog_axis_map_to_view (x_map, series->vals[3]);
		max = gog_axis_map_to_view (x_map, series->vals[4]);
		rect.x = qu1;
		rect.w = qu3 - qu1;
		rect.y = y - hrect;
		rect.h = 2* hrect;
		gog_renderer_draw_sharp_rectangle (view->renderer, &rect);
		path[2].code = ART_END;
		path[0].y = y + hbar;
		path[1].y = y - hbar;
		path[0].x = path[1].x = min;
		gog_renderer_draw_sharp_path (view->renderer, path);
		path[0].x = path[1].x = max;
		gog_renderer_draw_sharp_path (view->renderer, path);
		path[0].y = path[1].y = y;
		path[0].x = qu3;
		gog_renderer_draw_sharp_path (view->renderer, path);
		path[0].x = min;
		path[1].x = qu1;
		gog_renderer_draw_sharp_path (view->renderer, path);
		path[0].x = path[1].x = med;
		path[0].y = y + hrect;
		path[1].y = y - hrect;
		gog_renderer_draw_sharp_path (view->renderer, path);
		path[2].code = ART_LINETO;
		path[0].x = path[3].x = path[4].x = qu1;
		path[1].x = path[2].x = qu3;
		path[0].y = path[1].y = path[4].y = y - hrect;
		path[2].y = path[3].y = y + hrect;
		gog_renderer_draw_sharp_path (view->renderer, path);
		gog_renderer_pop_style (view->renderer);
		y -= hser;
	}
	gog_axis_map_free (x_map);
}

static void
gog_box_plot_view_class_init (GogViewClass *view_klass)
{
	view_klass->render	  = gog_box_plot_view_render;
	view_klass->clip	  = TRUE;
}

GSF_DYNAMIC_CLASS (GogBoxPlotView, gog_box_plot_view,
	gog_box_plot_view_class_init, NULL,
	GOG_PLOT_VIEW_TYPE)

/*****************************************************************************/

static GogObjectClass *gog_box_plot_series_parent_klass;

static gint
float_compare (const double *a, const double *b)
{
        if (*a < *b)
                return -1;
	else if (*a == *b)
	        return 0;
	else
	        return 1;
}

static void
gog_box_plot_series_update (GogObject *obj)
{
	double *vals = NULL;
	int len = 0;
	GogBoxPlotSeries *series = GOG_BOX_PLOT_SERIES (obj);
	unsigned old_num = series->base.num_elements;

	if (series->base.values[0].data != NULL) {
		vals = go_data_vector_get_values (GO_DATA_VECTOR (series->base.values[0].data));
		len = go_data_vector_get_len 
			(GO_DATA_VECTOR (series->base.values[0].data));
	}
	series->base.num_elements = len;

	if (len > 0) {
		double *svals = g_new (double, len), x, fpos, residual;
		int n, pos;
		memcpy (svals, vals, len * sizeof (double));
		qsort (svals, len, sizeof (double), (int (*) (const void *, const void *))&float_compare);
		for (n = 0, x = 0.; n < 5; n++, x+= 0.25) { 
			fpos = (len - 1) * x;
			pos = (int) fpos;
			residual = fpos - pos;

			if (residual == 0.0 || pos + 1 >= len)
				series->vals[n] = svals[pos];
			else
				series->vals[n] = (1 - residual) * svals[pos] + residual * svals[pos + 1];
		}
		g_free (svals);
	}
	/* queue plot for redraw */
	gog_object_request_update (GOG_OBJECT (series->base.plot));
	if (old_num != series->base.num_elements)
		gog_plot_request_cardinality_update (series->base.plot);

	if (gog_box_plot_series_parent_klass->update)
		gog_box_plot_series_parent_klass->update (obj);
}

static void
gog_box_plot_series_init_style (GogStyledObject *gso, GogStyle *style)
{
	((GogStyledObjectClass*) gog_box_plot_series_parent_klass)->init_style (gso, style);

	style->outline.dash_type = GO_LINE_NONE;
}

static void
gog_box_plot_series_class_init (GogObjectClass *obj_klass)
{
	GogStyledObjectClass *gso_klass = (GogStyledObjectClass*) obj_klass;

	gog_box_plot_series_parent_klass = g_type_class_peek_parent (obj_klass);
	obj_klass->update = gog_box_plot_series_update;
	gso_klass->init_style = gog_box_plot_series_init_style;
}

GSF_DYNAMIC_CLASS (GogBoxPlotSeries, gog_box_plot_series,
	gog_box_plot_series_class_init, NULL,
	GOG_SERIES_TYPE)

/* Plugin initialization */

G_MODULE_EXPORT void
go_plugin_init (GOPlugin *plugin, GOCmdContext *cc)
{
	GTypeModule *module = go_plugin_get_type_module (plugin);
	gog_box_plot_register_type (module);
	gog_box_plot_view_register_type (module);
	gog_box_plot_series_register_type (module);
}

G_MODULE_EXPORT void
go_plugin_shutdown (GOPlugin *plugin, GOCmdContext *cc)
{
}
