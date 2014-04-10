/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * go-1.5d.c
 *
 * Copyright (C) 2003-2004
 *	Jody Goldberg (jody@gnome.org)
 *	Emmanuel Pacaud (emmanuel.pacaud@univ-poitiers.fr)
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
#include "gog-1.5d.h"
#include "gog-line.h"
#include "gog-barcol.h"
#include "gog-dropbar.h"
#include "gog-minmax.h"
#include <goffice/graph/gog-series-lines.h>
#include <goffice/graph/gog-view.h>
#include <goffice/graph/gog-renderer.h>
#include <goffice/graph/gog-axis.h>
#include <goffice/graph/gog-style.h>
#include <goffice/data/go-data.h>
#include <goffice/utils/go-color.h>
#include <goffice/utils/go-format.h>
#include <goffice/utils/go-math.h>
#include <goffice/app/module-plugin-defs.h>

#include <glib/gi18n.h>
#include <gtk/gtklabel.h>
#include <gsf/gsf-impl-utils.h>

GOFFICE_PLUGIN_MODULE_HEADER;

enum {
	GOG_1_5D_PROP_0,
	GOG_1_5D_PROP_TYPE,
	GOG_1_5D_PROP_IN_3D	/* place holder for XL */
};

static GogObjectClass *plot1_5d_parent_klass;

static void
gog_plot_1_5d_clear_formats (GogPlot1_5d *plot)
{
	if (plot->fmt != NULL) {
		go_format_unref (plot->fmt);
		plot->fmt = NULL;
	}
}

static void
gog_plot1_5d_finalize (GObject *obj)
{
	gog_plot_1_5d_clear_formats (GOG_PLOT1_5D (obj));
	G_OBJECT_CLASS (plot1_5d_parent_klass)->finalize (obj);
}

static void
gog_plot1_5d_set_property (GObject *obj, guint param_id,
			    GValue const *value, GParamSpec *pspec)
{
	GogPlot1_5d *gog_1_5d = GOG_PLOT1_5D (obj);
	gboolean tmp;

	switch (param_id) {
	case GOG_1_5D_PROP_TYPE: {
		char const *str = g_value_get_string (value);
		if (str == NULL)
			return;
		else if (!g_ascii_strcasecmp (str, "normal"))
			gog_1_5d->type = GOG_1_5D_NORMAL;
		else if (!g_ascii_strcasecmp (str, "stacked"))
			gog_1_5d->type = GOG_1_5D_STACKED;
		else if (!g_ascii_strcasecmp (str, "as_percentage"))
			gog_1_5d->type = GOG_1_5D_AS_PERCENTAGE;
		else
			return;
		break;
	case GOG_1_5D_PROP_IN_3D :
		tmp = g_value_get_boolean (value);
		if ((gog_1_5d->in_3d != 0) == (tmp != 0))
			return;
		gog_1_5d->in_3d = tmp;
		break;
	}

	default: G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, param_id, pspec);
		 return; /* NOTE : RETURN */
	}
	gog_object_emit_changed (GOG_OBJECT (obj), TRUE);
}

static void
gog_plot1_5d_get_property (GObject *obj, guint param_id,
			      GValue *value, GParamSpec *pspec)
{
	GogPlot1_5d *gog_1_5d = GOG_PLOT1_5D (obj);

	switch (param_id) {
	case GOG_1_5D_PROP_TYPE:
		switch (gog_1_5d->type) {
		case GOG_1_5D_NORMAL:
			g_value_set_static_string (value, "normal");
			break;
		case GOG_1_5D_STACKED:
			g_value_set_static_string (value, "stacked");
			break;
		case GOG_1_5D_AS_PERCENTAGE:
			g_value_set_static_string (value, "as_percentage");
			break;
		}
		break;
	case GOG_1_5D_PROP_IN_3D :
		g_value_set_boolean (value, gog_1_5d->in_3d);
		break;
		
	default: G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, param_id, pspec);
		 break;
	}
}

static GogAxis *
gog_plot1_5d_get_value_axis (GogPlot1_5d *model)
{
	GogPlot1_5dClass *klass = GOG_PLOT1_5D_GET_CLASS (model);
	if (klass->swap_x_and_y && (*klass->swap_x_and_y ) (model))
		return model->base.axis [GOG_AXIS_X];
	return model->base.axis [GOG_AXIS_Y];
}

GogAxis *
gog_plot1_5d_get_index_axis (GogPlot1_5d *model)
{
	GogPlot1_5dClass *klass = GOG_PLOT1_5D_GET_CLASS (model);
	if (klass->swap_x_and_y && (*klass->swap_x_and_y ) (model))
		return model->base.axis [GOG_AXIS_Y];
	return model->base.axis [GOG_AXIS_X];
}

static void
gog_plot1_5d_update (GogObject *obj)
{
	GogPlot1_5d *model = GOG_PLOT1_5D (obj);
	GogPlot1_5dClass *klass = GOG_PLOT1_5D_GET_CLASS (obj);
	GogSeries1_5d const *series;
	unsigned i, num_elements, num_series;
	double **vals, minima, maxima;
	double old_minima, old_maxima;
	unsigned *lengths;
	GSList *ptr;
	GOData *index_dim = NULL;
	GogPlot *plot_that_labeled_axis;
	GogAxis *axis;
	GogErrorBar **errors;
	gboolean index_changed = FALSE;

	old_minima =  model->minima;
	old_maxima =  model->maxima;
	model->minima =  DBL_MAX;
	model->maxima = -DBL_MAX;
	gog_plot_1_5d_clear_formats (model);

	num_elements = num_series = 0;
	for (ptr = model->base.series ; ptr != NULL ; ptr = ptr->next) {
		series = ptr->data;
		if (!gog_series_is_valid (GOG_SERIES (series)))
			continue;
		num_series++;

		if (GOG_SERIES1_5D (series)->index_changed) {
			GOG_SERIES1_5D (series)->index_changed = FALSE;
			index_changed = TRUE;
		}

		if (num_elements < series->base.num_elements)
			num_elements = series->base.num_elements;
		if (GOG_1_5D_NORMAL == model->type) {
			if (gog_error_bar_is_visible (series->errors))
				gog_error_bar_get_minmax (series->errors, &minima, &maxima);
			else
				go_data_vector_get_minmax (GO_DATA_VECTOR (
					series->base.values[1].data), &minima, &maxima);
			if (series->base.plot->desc.series.num_dim == 3) {
				double tmp_min, tmp_max;
				go_data_vector_get_minmax (GO_DATA_VECTOR (
					series->base.values[2].data), &tmp_min, &tmp_max);
				if (minima > tmp_min )
					minima = tmp_min;
				if (maxima < tmp_max)
					maxima = tmp_max;
			}
			if (model->minima > minima)
				model->minima = minima;
			if (model->maxima < maxima)
				model->maxima = maxima;
		}
		if (model->fmt == NULL)
			model->fmt = go_data_preferred_fmt (series->base.values[1].data);
		index_dim = GOG_SERIES (series)->values[0].data;
	}
	axis = gog_plot1_5d_get_index_axis (model);
	if (model->num_elements != num_elements ||
	    model->implicit_index ^ (index_dim == NULL) ||
	    (index_dim != gog_axis_get_labels (axis, &plot_that_labeled_axis) &&
	     GOG_PLOT (model) == plot_that_labeled_axis)) {
		model->num_elements = num_elements;
		model->implicit_index = (index_dim == NULL);
		gog_axis_bound_changed (axis, GOG_OBJECT (model));
	} else { 
		if (index_changed)  
			gog_axis_bound_changed (axis, GOG_OBJECT (model));
	}

	model->num_series = num_series;

	if (num_elements <= 0 || num_series <= 0)
		model->minima = model->maxima = 0.;
	else if (model->type != GOG_1_5D_NORMAL) {
		vals = g_alloca (num_series * sizeof (double *));
		errors = g_alloca (num_series * sizeof (GogErrorBar *));
		lengths = g_alloca (num_series * sizeof (unsigned));
		i = 0;
		for (ptr = model->base.series ; ptr != NULL ; ptr = ptr->next, i++) {
			series = ptr->data;
			/* we are guaranteed that at least 1 series is valid above */
			if (!gog_series_is_valid (GOG_SERIES (series)))
				continue;
			vals[i] = go_data_vector_get_values (
				GO_DATA_VECTOR (series->base.values[1].data));
			g_object_get (G_OBJECT (series), "errors", errors + i, NULL);
			if (errors[i])
				g_object_unref (errors[i]);
			lengths[i] = go_data_vector_get_len (
				GO_DATA_VECTOR (series->base.values[1].data));
		}

		if (klass->update_stacked_and_percentage)
			klass->update_stacked_and_percentage (model, vals, errors, lengths);
	}

	if (old_minima != model->minima || old_maxima != model->maxima)
		gog_axis_bound_changed (
			gog_plot1_5d_get_value_axis (model), GOG_OBJECT (model));

	gog_object_emit_changed (GOG_OBJECT (obj), FALSE);
	if (plot1_5d_parent_klass->update)
		plot1_5d_parent_klass->update (obj);
}

static GOData *
gog_plot1_5d_axis_get_bounds (GogPlot *plot, GogAxisType axis,
			      GogPlotBoundInfo *bounds)
{
	GogPlot1_5d *model = GOG_PLOT1_5D (plot);
	if (axis == gog_axis_get_atype (gog_plot1_5d_get_value_axis (model))) {
		bounds->val.minima = model->minima;
		bounds->val.maxima = model->maxima;
		if (model->type == GOG_1_5D_AS_PERCENTAGE) {
			if (model->minima >= -1.)
				bounds->logical.minima = -1.;
			if (model->maxima <= 1.)
				bounds->logical.maxima =  1.;
			if (bounds->fmt == NULL) {
				bounds->fmt = go_format_ref (
					go_format_default_percentage ());
			}
		} else if (bounds->fmt == NULL && model->fmt != NULL)
			bounds->fmt = go_format_ref (model->fmt);
		return NULL;
	} else if (axis == gog_axis_get_atype (gog_plot1_5d_get_index_axis (model))) {
		GSList *ptr;

		bounds->val.minima = 1.;
		bounds->val.maxima = model->num_elements;
		bounds->logical.minima = 1.;
		bounds->logical.maxima = go_nan;
		bounds->is_discrete    = TRUE;

		for (ptr = plot->series; ptr != NULL ; ptr = ptr->next)
			if (gog_series_is_valid (GOG_SERIES (ptr->data)))
				return GOG_SERIES (ptr->data)->values[0].data;
		return NULL;
	}

	g_warning ("not reached");
	return NULL;
}

static gboolean
gog_1_5d_supports_vary_style_by_element (GogPlot const *plot)
{
	GogPlot1_5d *gog_1_5d = GOG_PLOT1_5D (plot);
	return gog_1_5d->type == GOG_1_5D_NORMAL;
}

static void
gog_plot1_5d_class_init (GogPlotClass *plot_klass)
{
	GObjectClass *gobject_klass = (GObjectClass *) plot_klass;
	GogObjectClass *gog_klass = (GogObjectClass *) plot_klass;

	plot1_5d_parent_klass = g_type_class_peek_parent (plot_klass);
	gobject_klass->set_property = gog_plot1_5d_set_property;
	gobject_klass->get_property = gog_plot1_5d_get_property;
	gobject_klass->finalize = gog_plot1_5d_finalize;

	g_object_class_install_property (gobject_klass, GOG_1_5D_PROP_TYPE,
		g_param_spec_string ("type", "type",
			"How to group multiple series, normal, stacked, as_percentage",
			"normal", G_PARAM_READWRITE | GOG_PARAM_PERSISTENT));
	g_object_class_install_property (gobject_klass, GOG_1_5D_PROP_IN_3D,
		g_param_spec_boolean ("in-3d", "in-3d",
			"Place holder to all us to round trip pseudo 3d state",
			FALSE, G_PARAM_READWRITE | GOG_PARAM_PERSISTENT));

	gog_klass->update	= gog_plot1_5d_update;

	{
		static GogSeriesDimDesc dimensions[] = {
			{ N_("Labels"), GOG_SERIES_SUGGESTED, TRUE,
			  GOG_DIM_LABEL, GOG_MS_DIM_CATEGORIES },
			{ N_("Values"), GOG_SERIES_REQUIRED, FALSE,
			  GOG_DIM_VALUE, GOG_MS_DIM_VALUES },
/* Names of the error data are not translated since they are not used */
			{ "+err", GOG_SERIES_ERRORS, FALSE,
			  GOG_DIM_VALUE, GOG_MS_DIM_ERR_plus1 },
			{ "-err", GOG_SERIES_ERRORS, FALSE,
			  GOG_DIM_VALUE, GOG_MS_DIM_ERR_minus1 }
		};
		plot_klass->desc.series.dim = dimensions;
		plot_klass->desc.series.num_dim = G_N_ELEMENTS (dimensions);
	}
	plot_klass->desc.num_series_min = 1;
	plot_klass->desc.num_series_max = G_MAXINT;
	plot_klass->series_type = gog_series1_5d_get_type ();
	plot_klass->axis_get_bounds   	= gog_plot1_5d_axis_get_bounds;
	plot_klass->axis_set	      	= GOG_AXIS_SET_XY;
	plot_klass->supports_vary_style_by_element = gog_1_5d_supports_vary_style_by_element;
}

static void
gog_plot1_5d_init (GogPlot1_5d *plot)
{
	plot->fmt = NULL;
	plot->in_3d = FALSE;
	plot->support_series_lines = FALSE;
	plot->support_drop_lines = FALSE;
	plot->support_lines = FALSE;
}

GSF_DYNAMIC_CLASS_ABSTRACT (GogPlot1_5d, gog_plot1_5d,
	gog_plot1_5d_class_init, gog_plot1_5d_init,
	GOG_PLOT_TYPE)

/*****************************************************************************/

static gboolean
series_lines_can_add (GogObject const *parent)
{
	GogSeries1_5d *series = GOG_SERIES1_5D (parent);
	GogPlot1_5d *plot = GOG_PLOT1_5D (series->base.plot);
	/* series lines are supported to implement excel series lines in barcol
	plots and lines with dropbars and high-low lines */
	if (GOG_IS_PLOT_BARCOL (plot) && plot->type == GOG_1_5D_NORMAL)
		return FALSE;
	return (plot->support_series_lines &&
								!series->has_series_lines);
}

static void
series_lines_post_add (GogObject *parent, GogObject *child)
{
	GogSeries1_5d *series = GOG_SERIES1_5D (parent);
	series->has_series_lines = TRUE;
	gog_object_request_update (child);
}

static void
series_lines_pre_remove (GogObject *parent, GogObject *child)
{
	GogSeries1_5d *series = GOG_SERIES1_5D (parent);
	series->has_series_lines = FALSE;
}

/*****************************************************************************/

static gboolean
drop_lines_can_add (GogObject const *parent)
{
	GogSeries1_5d *series = GOG_SERIES1_5D (parent);
	return (GOG_PLOT1_5D (series->base.plot)->support_drop_lines &&
								!series->has_drop_lines);
}

static void
drop_lines_post_add (GogObject *parent, GogObject *child)
{
	GogSeries1_5d *series = GOG_SERIES1_5D (parent);
	series->has_drop_lines = TRUE;
	gog_object_request_update (child);
}

static void
drop_lines_pre_remove (GogObject *parent, GogObject *child)
{
	GogSeries1_5d *series = GOG_SERIES1_5D (parent);
	series->has_drop_lines = FALSE;
}

/*****************************************************************************/

static gboolean
lines_can_add (GogObject const *parent)
{
	GogSeries1_5d *series = GOG_SERIES1_5D (parent);
	return (GOG_PLOT1_5D (series->base.plot)->support_lines &&
		!series->has_lines);
}

static void
lines_post_add (GogObject *parent, GogObject *child)
{
	GogSeries1_5d *series = GOG_SERIES1_5D (parent);
	series->has_lines = TRUE;
	gog_object_request_update (child);
}

static void
lines_pre_remove (GogObject *parent, GogObject *child)
{
	GogSeries1_5d *series = GOG_SERIES1_5D (parent);
	series->has_lines = FALSE;
}

/*****************************************************************************/

static GogObjectClass *gog_series1_5d_parent_klass;

enum {
	SERIES_PROP_0,
	SERIES_PROP_ERRORS
};

static void
gog_series1_5d_dim_changed (GogSeries *series, int dim_i)
{
	if (dim_i == 0)
		GOG_SERIES1_5D (series)->index_changed = TRUE;
}

static void
gog_series1_5d_update (GogObject *obj)
{
	double *vals;
	int len = 0;
	GogSeries1_5d *series = GOG_SERIES1_5D (obj);
	unsigned old_num = series->base.num_elements;

	if (series->base.values[1].data != NULL) {
		vals = go_data_vector_get_values (GO_DATA_VECTOR (series->base.values[1].data));
		len = go_data_vector_get_len 
			(GO_DATA_VECTOR (series->base.values[1].data));
	}
	series->base.num_elements = len;

	if (series->base.plot->desc.series.num_dim == 3) {
		int tmp = 0;
		if (series->base.values[2].data != NULL) {
			vals = go_data_vector_get_values (GO_DATA_VECTOR (series->base.values[2].data));
			tmp = go_data_vector_get_len 
				(GO_DATA_VECTOR (series->base.values[2].data));
		}
		if (tmp < len)
			len = tmp;
	}

	/* queue plot for redraw */
	gog_object_request_update (GOG_OBJECT (series->base.plot));
	if (old_num != series->base.num_elements)
		gog_plot_request_cardinality_update (series->base.plot);

	if (gog_series1_5d_parent_klass->update)
		gog_series1_5d_parent_klass->update (obj);
}

static void
gog_series1_5d_set_property (GObject *obj, guint param_id,
				GValue const *value, GParamSpec *pspec)
{
	GogSeries1_5d *series=  GOG_SERIES1_5D (obj);
	GogErrorBar* bar;

	switch (param_id) {
	case SERIES_PROP_ERRORS :
		bar = g_value_get_object (value);
		if (series->errors == bar)
			return;
		if (bar) {
			bar = gog_error_bar_dup (bar);
			bar->series = GOG_SERIES (series);
			bar->dim_i = 1;
			bar->error_i = 2;
		}
		if (!series->base.needs_recalc) {
			series->base.needs_recalc = TRUE;
			gog_object_emit_changed (GOG_OBJECT (series), FALSE);
		}
		if (series->errors != NULL)
			g_object_unref (series->errors);
		series->errors = bar;
		break;
	}
}

static void
gog_series1_5d_get_property (GObject *obj, guint param_id,
			  GValue *value, GParamSpec *pspec)
{
	GogSeries1_5d *series=  GOG_SERIES1_5D (obj);

	switch (param_id) {
	case SERIES_PROP_ERRORS :
		g_value_set_object (value, series->errors);
		break;
	}
}

static void 
gog_series1_5d_populate_editor (GogObject *obj,
				GogEditor *editor,
				GogDataAllocator *dalloc,
				GOCmdContext *cc)
{
	GogSeries *series = GOG_SERIES (obj);
	GtkWidget * error_page;
	gboolean horizontal;

	(GOG_OBJECT_CLASS (gog_series1_5d_parent_klass)->populate_editor) (obj, editor, dalloc, cc);

	if (series->plot->desc.series.num_dim == 3)
		return;

	if (g_object_class_find_property (G_OBJECT_GET_CLASS (series->plot), "horizontal") == NULL)
		horizontal = FALSE;
	else
		g_object_get (G_OBJECT (series->plot), "horizontal", &horizontal, NULL);
	error_page = gog_error_bar_prefs (series, "errors", horizontal, dalloc, cc);
	gog_editor_add_page (editor, error_page, _("Error bars"));
}

static void
gog_series1_5d_finalize (GObject *obj)
{
	GogSeries1_5d *series = GOG_SERIES1_5D (obj);
	if (series->errors) {
		g_object_unref (series->errors);
		series->errors = NULL;
	}
	G_OBJECT_CLASS (gog_series1_5d_parent_klass)->finalize (obj);
}

static void
gog_series1_5d_class_init (GogObjectClass *obj_klass)
{
	static GogObjectRole const roles[] = {
		{ N_("Series lines"), "GogSeriesLines",	0,
			GOG_POSITION_SPECIAL, GOG_POSITION_SPECIAL, GOG_OBJECT_NAME_BY_ROLE,
			series_lines_can_add,
			NULL,
			NULL,
			series_lines_post_add,
			series_lines_pre_remove, NULL },
		{ N_("Drop lines"), "GogSeriesLines",	1,
			GOG_POSITION_SPECIAL, GOG_POSITION_SPECIAL, GOG_OBJECT_NAME_BY_ROLE,
			drop_lines_can_add,
			NULL,
			NULL,
			drop_lines_post_add,
			drop_lines_pre_remove,
			NULL },
		{ N_("Lines"), "GogSeriesLines",	1,
			GOG_POSITION_SPECIAL, GOG_POSITION_SPECIAL, GOG_OBJECT_NAME_BY_ROLE,
			lines_can_add,
			NULL,
			NULL,
			lines_post_add,
			lines_pre_remove,
			NULL },
	};

	GObjectClass *gobject_klass = (GObjectClass *) obj_klass;
	GogSeriesClass *gog_series_klass = (GogSeriesClass*) obj_klass;

	gog_series1_5d_parent_klass = g_type_class_peek_parent (obj_klass);

	gobject_klass->set_property = gog_series1_5d_set_property;
	gobject_klass->get_property = gog_series1_5d_get_property;
	gobject_klass->finalize 	      = gog_series1_5d_finalize;
	obj_klass->update 	      = gog_series1_5d_update;
	obj_klass->populate_editor    = gog_series1_5d_populate_editor;
	gog_series_klass->dim_changed = gog_series1_5d_dim_changed;

	gog_object_register_roles (obj_klass, roles, G_N_ELEMENTS (roles));

	g_object_class_install_property (gobject_klass, SERIES_PROP_ERRORS,
		g_param_spec_object ("errors", "errors",
			"GogErrorBar *",
			GOG_ERROR_BAR_TYPE, G_PARAM_READWRITE|GOG_PARAM_PERSISTENT));
}

static void
gog_series1_5d_init (GObject *obj)
{
	GogSeries1_5d *series=  GOG_SERIES1_5D (obj);

	series->errors = NULL;
	series->index_changed = FALSE;
	series->has_series_lines = FALSE;
	series->has_drop_lines = FALSE;
	series->has_lines = FALSE;
}

GSF_DYNAMIC_CLASS (GogSeries1_5d, gog_series1_5d,
	gog_series1_5d_class_init, gog_series1_5d_init,
	GOG_SERIES_TYPE)

/* Plugin initialization */

G_MODULE_EXPORT void
go_plugin_init (GOPlugin *plugin, GOCmdContext *cc)
{
	GTypeModule *module = go_plugin_get_type_module (plugin);
	gog_plot1_5d_register_type (module);
	gog_series1_5d_register_type (module);
	gog_barcol_plot_register_type (module);
	gog_barcol_view_register_type (module);
	gog_dropbar_plot_register_type (module);
	gog_dropbar_view_register_type (module);
	gog_line_series_register_type (module);
	gog_line_plot_register_type (module);
	gog_area_plot_register_type (module);
	gog_line_view_register_type (module);
	gog_minmax_series_register_type (module);
	gog_minmax_plot_register_type (module);
	gog_minmax_view_register_type (module);
}

G_MODULE_EXPORT void
go_plugin_shutdown (GOPlugin *plugin, GOCmdContext *cc)
{
}
