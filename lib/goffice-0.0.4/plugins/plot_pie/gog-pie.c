/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * go-pie.c
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
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301
 * USA
 */

#include <goffice/goffice-config.h>
#include "gog-pie.h"
#include <goffice/graph/gog-view.h>
#include <goffice/graph/gog-renderer.h>
#include <goffice/graph/gog-theme.h>
#include <goffice/graph/gog-style.h>
#include <goffice/data/go-data.h>
#include <goffice/utils/go-color.h>
#include <goffice/utils/go-math.h>
#include <goffice/app/module-plugin-defs.h>

#include <glib/gi18n.h>
#include <gsf/gsf-impl-utils.h>
#include <math.h>

static GObjectClass *ppe_parent_klass;

typedef GogSeriesElementClass GogPieSeriesElementClass;
enum {
	ELEMENT_PROP_0,
	ELEMENT_SEPARATION,
};

static void
gog_pie_series_element_set_property (GObject *obj, guint param_id,
				     GValue const *value, GParamSpec *pspec)
{
	GogPieSeriesElement *pse = GOG_PIE_SERIES_ELEMENT (obj);

	switch (param_id) {
	case ELEMENT_SEPARATION :
		pse->separation = g_value_get_float (value);
		break;

	default: G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, param_id, pspec);
		 return; /* NOTE : RETURN */
	}

	gog_object_emit_changed (GOG_OBJECT (obj), FALSE);
}

static void
gog_pie_series_element_get_property (GObject *obj, guint param_id,
				     GValue *value, GParamSpec *pspec)
{
	GogPieSeriesElement *pse = GOG_PIE_SERIES_ELEMENT (obj);

	switch (param_id) {
	case ELEMENT_SEPARATION :
		g_value_set_float (value, pse->separation);
		break;
	default: G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, param_id, pspec);
		 break;
	}
}

extern gpointer gog_pie_series_element_pref (GogPieSeriesElement *element, GOCmdContext *cc);
static gpointer
gog_pie_series_element_populate_editor (GogObject *gobj,
					GogEditor *editor,
			       GOCmdContext *cc)
{
	GtkWidget *widget = gog_pie_series_element_pref (GOG_PIE_SERIES_ELEMENT (gobj), cc);
	gog_editor_add_page (editor, widget, _("Settings"));
	return widget;
}

static void
gog_pie_series_element_class_init (GogPieSeriesElementClass *klass)
{
	GObjectClass *gobject_klass = (GObjectClass *) klass;

	gobject_klass->set_property = gog_pie_series_element_set_property;
	gobject_klass->get_property = gog_pie_series_element_get_property;
	
	ppe_parent_klass 	= g_type_class_peek_parent (klass);
	klass->gse_populate_editor	= gog_pie_series_element_populate_editor;

	g_object_class_install_property (gobject_klass, ELEMENT_SEPARATION,
		g_param_spec_float ("separation", "separation",
			"Amount a slice is extended as a percentage of the radius",
			0, G_MAXFLOAT, 0.,
			G_PARAM_READWRITE | GOG_PARAM_PERSISTENT));
}

GSF_DYNAMIC_CLASS (GogPieSeriesElement, gog_pie_series_element,
	gog_pie_series_element_class_init, NULL /*gog_pie_series_element_init*/,
	GOG_SERIES_ELEMENT_TYPE)

/*****************************************************************************/

typedef struct {
	GogPlotClass	base;
} GogPiePlotClass;

enum {
	PLOT_PROP_0,
	PLOT_PROP_INITIAL_ANGLE,
	PLOT_PROP_DEFAULT_SEPARATION,
	PLOT_PROP_IN_3D
};

GOFFICE_PLUGIN_MODULE_HEADER;

static GObjectClass *pie_parent_klass;
static GType gog_pie_view_get_type (void);

static void
gog_pie_plot_set_property (GObject *obj, guint param_id,
			   GValue const *value, GParamSpec *pspec)
{
	GogPiePlot *pie = GOG_PIE_PLOT (obj);
	float f;

	switch (param_id) {
	case PLOT_PROP_INITIAL_ANGLE :
		pie->initial_angle = g_value_get_float (value);
		break;
	case PLOT_PROP_DEFAULT_SEPARATION :
		f = g_value_get_float (value);
		pie->default_separation = MIN (f, 5.);
		break;
	case PLOT_PROP_IN_3D :
		pie->in_3d = g_value_get_boolean (value);
		break;

	default: G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, param_id, pspec);
		 return; /* NOTE : RETURN */
	}

	/* none of the attributes triggers a size change yet.
	 * When we add data labels we'll need it */
	gog_object_emit_changed (GOG_OBJECT (obj), FALSE);
}

static void
gog_pie_plot_get_property (GObject *obj, guint param_id,
			  GValue *value, GParamSpec *pspec)
{
	GogPiePlot *pie = GOG_PIE_PLOT (obj);

	switch (param_id) {
	case PLOT_PROP_INITIAL_ANGLE :
		g_value_set_float (value, pie->initial_angle);
		break;
	case PLOT_PROP_DEFAULT_SEPARATION :
		g_value_set_float (value, pie->default_separation);
		break;
	case PLOT_PROP_IN_3D :
		g_value_set_boolean (value, pie->in_3d);
		break;
	default: G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, param_id, pspec);
		 break;
	}
}

static char const *
gog_pie_plot_type_name (G_GNUC_UNUSED GogObject const *item)
{
	return N_("PlotPie");
}

extern gpointer gog_pie_plot_pref (GogPiePlot *pie, GOCmdContext *cc);
static void
gog_pie_plot_populate_editor (GogObject *item, 
			      GogEditor *editor,
		    G_GNUC_UNUSED GogDataAllocator *dalloc,
		    GOCmdContext *cc)
{
	gog_editor_add_page (editor, 
			     gog_pie_plot_pref (GOG_PIE_PLOT (item), cc),
			     _("Properties"));
}

static void
gog_pie_plot_update (GogObject *obj)
{
	gog_object_emit_changed (GOG_OBJECT (obj), FALSE);
}

static void
gog_pie_plot_class_init (GogPlotClass *plot_klass)
{
	GObjectClass *gobject_klass = (GObjectClass *) plot_klass;
	GogObjectClass *gog_klass = (GogObjectClass *) plot_klass;

	pie_parent_klass = g_type_class_peek_parent (plot_klass);
	gobject_klass->set_property = gog_pie_plot_set_property;
	gobject_klass->get_property = gog_pie_plot_get_property;

	gog_klass->update	= gog_pie_plot_update;
	gog_klass->type_name	= gog_pie_plot_type_name;
	gog_klass->populate_editor = gog_pie_plot_populate_editor;
	gog_klass->view_type	= gog_pie_view_get_type ();

	g_object_class_install_property (gobject_klass, PLOT_PROP_INITIAL_ANGLE,
		g_param_spec_float ("initial-angle", "initial-angle",
			"Degrees clockwise from 12 O'Clock.",
			0, G_MAXFLOAT, 0.,
			G_PARAM_READWRITE | GOG_PARAM_PERSISTENT));
	g_object_class_install_property (gobject_klass, PLOT_PROP_DEFAULT_SEPARATION,
		g_param_spec_float ("default-separation", "default-separation",
			"Default amount a slice is extended as a percentage of the radius",
			0, G_MAXFLOAT, 0.,
			G_PARAM_READWRITE | GOG_PARAM_PERSISTENT));
	g_object_class_install_property (gobject_klass, PLOT_PROP_IN_3D,
		g_param_spec_boolean ("in-3d", "in-3d",
			"Draw 3d wedges",
			FALSE,
			G_PARAM_READWRITE));

	{
		static GogSeriesDimDesc dimensions[] = {
			{ N_("Labels"), GOG_SERIES_SUGGESTED, TRUE,
			  GOG_DIM_LABEL, GOG_MS_DIM_CATEGORIES },
			{ N_("Values"), GOG_SERIES_REQUIRED, FALSE,
			  GOG_DIM_VALUE, GOG_MS_DIM_VALUES }
		};
		plot_klass->desc.series.dim = dimensions;
		plot_klass->desc.series.num_dim = G_N_ELEMENTS (dimensions);
		plot_klass->desc.series.style_fields = GOG_STYLE_OUTLINE | GOG_STYLE_FILL;
	}
	plot_klass->desc.num_series_min = 1;
	plot_klass->desc.num_series_max = 1;
	plot_klass->series_type  = gog_pie_series_get_type ();
}

static void
gog_pie_plot_init (GogPiePlot *pie)
{
	pie->base.vary_style_by_element = TRUE;
}

GSF_DYNAMIC_CLASS (GogPiePlot, gog_pie_plot,
	gog_pie_plot_class_init, gog_pie_plot_init,
	GOG_PLOT_TYPE)

/*****************************************************************************/

enum {
	RING_PLOT_PROP_0,
	RING_PLOT_PROP_CENTER_SIZE,
};

typedef GogPiePlotClass	GogRingPlotClass;
static GObjectClass *ring_parent_klass;

static void
gog_ring_plot_set_property (GObject *obj, guint param_id,
			    GValue const *value, GParamSpec *pspec)
{
	GogRingPlot *ring = GOG_RING_PLOT (obj);

	switch (param_id) {
	case RING_PLOT_PROP_CENTER_SIZE :
		ring->center_size = g_value_get_float (value);
		break;

	default: G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, param_id, pspec);
		 return; /* NOTE : RETURN */
	}

	/* none of the attributes triggers a size change yet.
	 * When we add data labels we'll need it */
	gog_object_emit_changed (GOG_OBJECT (obj), FALSE);
}

static void
gog_ring_plot_get_property (GObject *obj, guint param_id,
			    GValue *value, GParamSpec *pspec)
{
	GogRingPlot *ring = GOG_RING_PLOT (obj);

	switch (param_id) {
	case RING_PLOT_PROP_CENTER_SIZE :
		g_value_set_float (value, ring->center_size);
		break;
	default: G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, param_id, pspec);
		 break;
	}
}

static char const *
gog_ring_plot_type_name (G_GNUC_UNUSED GogObject const *item)
{
	return N_("PlotRing");
}

extern gpointer gog_ring_plot_pref (GogRingPlot *ring, GOCmdContext *cc);
static void
gog_ring_plot_populate_editor (GogObject *item,
			       GogEditor *editor,
		      G_GNUC_UNUSED GogDataAllocator *dalloc,
		      GOCmdContext *cc)
{
	gog_editor_add_page (editor,
			     gog_ring_plot_pref (GOG_RING_PLOT (item), cc),
			     _("Properties"));
}

static void
gog_ring_plot_class_init (GogPiePlotClass *pie_plot_klass)
{
	GObjectClass *gobject_klass = (GObjectClass *) pie_plot_klass;
	GogObjectClass *gog_klass = (GogObjectClass *) pie_plot_klass;
	GogPlotClass *plot_klass = (GogPlotClass *) pie_plot_klass;

	ring_parent_klass = g_type_class_peek_parent (pie_plot_klass);
	gobject_klass->set_property = gog_ring_plot_set_property;
	gobject_klass->get_property = gog_ring_plot_get_property;

	gog_klass->type_name	= gog_ring_plot_type_name;
	gog_klass->populate_editor = gog_ring_plot_populate_editor;

	g_object_class_install_property (gobject_klass, RING_PLOT_PROP_CENTER_SIZE,
		g_param_spec_float ("center-size", "center-size",
			"Size of the center hole as a percentage of the radius",
			0, 100., 0.,
			G_PARAM_READWRITE));

	plot_klass->desc.num_series_min = 1;
	plot_klass->desc.num_series_max = G_MAXINT;
	plot_klass->desc.series.style_fields = GOG_STYLE_OUTLINE | GOG_STYLE_FILL;
}

static void
gog_ring_plot_init (GogRingPlot *ring)
{
	ring->center_size = 0.5;
}

GSF_DYNAMIC_CLASS (GogRingPlot, gog_ring_plot,
	gog_ring_plot_class_init, gog_ring_plot_init,
	GOG_PIE_PLOT_TYPE)

/*****************************************************************************/
typedef GogPlotView		GogPieView;
typedef GogPlotViewClass	GogPieViewClass;

#define MAX_ARC_SEGMENTS 64

static void
gog_pie_view_render (GogView *view, GogViewAllocation const *bbox)
{
	GogPiePlot const *model = GOG_PIE_PLOT (view->model);
	GogPieSeries const *series = NULL;
	double separated_cx, separated_cy, cx, cy, r, dt, tmp, theta, len, *vals, scale;
	double default_sep;
	unsigned elem, j, n, k;
	ArtVpath path [MAX_ARC_SEGMENTS*2 + 4];
	GogTheme *theme = gog_object_get_theme (GOG_OBJECT (model));
	GogStyle *style;
	GSList *ptr;
	unsigned num_series = 0;
	unsigned index, mirror = 0; /* init mirror because gcc is silly */
	double center_radius;
	double center_size = 0.0;
	double r_ext, r_int, r_tot;
	double outline_width_max;
	gboolean has_hole;
	double separation_max, separation;
	GogPieSeriesElement *gpse;
	GList const *overrides;

	/* compute number of valid series */
	for (ptr = model->base.series ; ptr != NULL ; ptr = ptr->next) {
		series = ptr->data;
	  	if (!gog_series_is_valid (GOG_SERIES (ptr->data)))
			continue;
		num_series++;
	}

	if (num_series <= 0)
		return;

	separation_max = .0;
	outline_width_max = .0;
	if ((style = gog_styled_object_get_style (GOG_STYLED_OBJECT (series))))
		outline_width_max = gog_renderer_line_size (view->renderer, style->outline.width);
	for (overrides = gog_series_get_overrides (GOG_SERIES (series));
	     overrides != NULL; 
	     overrides = overrides->next) {
		separation = GOG_PIE_SERIES_ELEMENT (overrides->data)->separation;
		if (separation_max < separation)
			separation_max = separation; 
		style = gog_styled_object_get_style (GOG_STYLED_OBJECT (overrides->data));
		if (outline_width_max < style->outline.width)
			outline_width_max = style->outline.width;
	}
	if (separation_max < -model->default_separation)
		separation_max = -model->default_separation;

	if (GOG_IS_RING_PLOT (model))
		center_size = GOG_RING_PLOT(model)->center_size;
	else if (num_series > 1)
		num_series = 1;

	/* centre things */
	cx = view->allocation.x + view->allocation.w/2.;
	cy = view->allocation.y + view->allocation.h/2.;

	r_tot = view->allocation.h;
	if (r_tot > view->allocation.w)
		r_tot = view->allocation.w;
	r_tot /= 2. * (1. + model->default_separation + separation_max);
	default_sep = r_tot * model->default_separation;
	center_radius = r_tot * center_size;
	r = r_tot * (1. - center_size);

	elem = model->base.index_num;
	index = 1;
	for (ptr = model->base.series ; ptr != NULL ; ptr = ptr->next) {
		series = ptr->data;

		if (!gog_series_is_valid (GOG_SERIES (series)))
			continue;
		if (index > num_series) /* people snuck extra series into a pie */
			break;
		
		if (num_series == index) 
			r -= outline_width_max / 2.0;
		
		has_hole = center_radius > 0. || index > 1;
		r_int = center_radius + r * ((double)index - 1.0) / (double)num_series;
		r_ext = center_radius + r * (double)index / (double)num_series;

		theta = (model->initial_angle + series->initial_angle) * 2. * M_PI / 360. - M_PI / 2.;

		scale = 2 * M_PI / series->total;
		vals = go_data_vector_get_values (GO_DATA_VECTOR (series->base.values[1].data));

		style = GOG_STYLED_OBJECT (series)->style;
		if (model->base.vary_style_by_element)
			style = gog_style_dup (style);
		gog_renderer_push_style (view->renderer, style);

		overrides = gog_series_get_overrides (GOG_SERIES (series));
		for (k = 0 ; k < series->base.num_elements; k++) {
			len = fabs (vals[k]) * scale;
			if (!go_finite (len) || len < 1e-3)
				continue;
			
			gpse = NULL;
			if ((overrides != NULL) &&
			    (GOG_SERIES_ELEMENT (overrides->data)->index == k)) {
				gpse = GOG_PIE_SERIES_ELEMENT (overrides->data);
				overrides = overrides->next;
				gog_renderer_push_style (view->renderer, 
					gog_styled_object_get_style (
						GOG_STYLED_OBJECT (gpse)));
			} else if (model->base.vary_style_by_element)
				gog_theme_fillin_style (theme, style, GOG_OBJECT (series),
							model->base.index_num + k, FALSE);
				
			/* only separate the outer ring */
			separated_cx = cx;
			separated_cy = cy;
			if (num_series == index && (default_sep > 0. || gpse != NULL)) { 

				separation = default_sep;

				if (gpse != NULL) 
					separation += gpse->separation * r_tot;

				separated_cx += separation * cos (theta + len/2.);
				separated_cy += separation * sin (theta + len/2.);
			}
			theta += len;

			n = MAX_ARC_SEGMENTS * len / (2 * M_PI);
			if (n < 6)
				n = 6;
			else if (n > MAX_ARC_SEGMENTS)
				n = MAX_ARC_SEGMENTS;

			dt = (double)-len / (double)n;
			path[0].code = ART_MOVETO;
			path[0].x = separated_cx;
			path[0].y = separated_cy;
			if (has_hole) {
				path[0].x += r_int * cos (theta);
				path[0].y += r_int * sin (theta);
				mirror = 2*n + 3;
				path[mirror].code = ART_END;
			} else {
				path[n+2].code = ART_LINETO;
				path[n+2].x = separated_cx;
				path[n+2].y = separated_cy;
				path[n+3].code = ART_END;
			}
			for (tmp = theta, j = 0; j++ <= n ; tmp += dt) {
				path[j].code = ART_LINETO;
				path[j].x = separated_cx + r_ext * cos (tmp);
				path[j].y = separated_cy + r_ext * sin (tmp);
				if (has_hole) {
					path[mirror - j].code = ART_LINETO;
					path[mirror - j].x = separated_cx + r_int * cos (tmp);
					path[mirror - j].y = separated_cy + r_int * sin (tmp);
				}
			}

			gog_renderer_draw_polygon (view->renderer, path,
						   r * len < 5 /* drop outline for thin segments */);

			if (gpse != NULL)
				gog_renderer_pop_style (view->renderer);
		}

		gog_renderer_pop_style (view->renderer);
		if (model->base.vary_style_by_element)
			g_object_unref (style);

		index ++;
	}
}

static gboolean
gog_pie_view_info_at_point (GogView *view, double x, double y,
			    GogObject const *cur_selection,
			    GogObject **obj, char **name)
{
	GogPiePlot const *model = GOG_PIE_PLOT (view->model);
	GogPieSeries const *series = NULL;
	double *vals, scale, len = 0, theta, r = view->allocation.h;
	GSList *ptr;
	unsigned i;

	if (r > view->allocation.w)
		r = view->allocation.w;
	r /= 2.;
	x -= view->allocation.x + view->allocation.w/2.;
	y -= view->allocation.y + view->allocation.h/2.;

	if ((x*x + y*y) > (r*r))
		return FALSE;

	for (ptr = model->base.series ; ptr != NULL ; ptr = ptr->next)
		if (gog_series_is_valid (GOG_SERIES (series = ptr->data)))
			break;
	if (ptr == NULL)
		return FALSE;
	
	/* TODO what follows does not work for ring plots, so exit here */
	if (GOG_IS_RING_PLOT (view->model)) {
		if (obj != NULL)
			*obj = view->model;
		if (name != NULL)
			*name = NULL;
		return TRUE;
	}

	theta = (atan2 (y, x) * 180 / M_PI - model->initial_angle + 90.) / 360.;
	if (theta < 0)
		theta += 1.;

	vals = go_data_vector_get_values (GO_DATA_VECTOR (series->base.values[1].data));
	scale = 1 / series->total;
	for (i = 0 ; i < series->base.num_elements; i++) {
		len = fabs (vals[i]) * scale;
		if (go_finite (len) && len > 1e-3) {
			theta -= len;
			if (theta < 0)
				break;
		}
	}

	if (obj != NULL) {
		if (cur_selection == view->model) {
			*obj = GOG_OBJECT (gog_series_get_element (GOG_SERIES (series), i));
			if (*obj == NULL) {
				*obj = g_object_new (gog_pie_series_element_get_type (),
						     "index", i, NULL);
				gog_object_add_by_name (GOG_OBJECT (series), "Point", *obj);
			}
		} else
			*obj = view->model;
	}
	if (name != NULL)
		*name = g_strdup_printf (_("%s point %d\nValue %g (%g)"),
					 gog_object_get_name (GOG_OBJECT (series)),
					 i+1, vals[i], len);

	return TRUE;
}

static void
gog_pie_view_class_init (GogViewClass *view_klass)
{
	view_klass->render = gog_pie_view_render;
	view_klass->info_at_point  = gog_pie_view_info_at_point;
}

GSF_DYNAMIC_CLASS (GogPieView, gog_pie_view,
	gog_pie_view_class_init, NULL,
	GOG_PLOT_VIEW_TYPE)

/*****************************************************************************/

typedef GogSeriesClass GogPieSeriesClass;
enum {
	SERIES_PROP_0,
	SERIES_PROP_INITIAL_ANGLE,
	SERIES_PROP_SEPARATION,
};

static GogObjectClass *series_parent_klass;
static void
gog_pie_series_set_property (GObject *obj, guint param_id,
			     GValue const *value, GParamSpec *pspec)
{
	GogPieSeries *pie = GOG_PIE_SERIES (obj);

	switch (param_id) {
	case SERIES_PROP_INITIAL_ANGLE :
		pie->initial_angle = g_value_get_float (value);
		break;
	case SERIES_PROP_SEPARATION :
		pie->separation = g_value_get_float (value);
		break;
	default: G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, param_id, pspec);
		 return; /* NOTE : RETURN */
	}
	/* none of the attributes triggers a size change yet.
	 * When we add data labels we'll need it */
	gog_object_emit_changed (GOG_OBJECT (obj), FALSE);
}

static void
gog_pie_series_get_property (GObject *obj, guint param_id,
			  GValue *value, GParamSpec *pspec)
{
	GogPieSeries *pie = GOG_PIE_SERIES (obj);

	switch (param_id) {
	case SERIES_PROP_INITIAL_ANGLE :
		g_value_set_float (value, pie->initial_angle);
		break;
	case SERIES_PROP_SEPARATION :
		g_value_set_float (value, pie->separation);
		break;
	default: G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, param_id, pspec);
		 break;
	}
}

static void
gog_pie_series_update (GogObject *obj)
{
	double *vals = NULL, total;
	int len = 0;
	GogPieSeries *series = GOG_PIE_SERIES (obj);
	unsigned old_num = series->base.num_elements;

	if (series->base.values[1].data != NULL) {
		vals = go_data_vector_get_values (GO_DATA_VECTOR (series->base.values[1].data));
		len = go_data_vector_get_len (
			GO_DATA_VECTOR (series->base.values[1].data));
	}
	series->base.num_elements = len;

	for (total = 0. ; len-- > 0 ;)
		if (go_finite (vals[len]))
			total += fabs (vals[len]);
	series->total = total;

	/* queue plot for redraw */
	gog_object_request_update (GOG_OBJECT (series->base.plot));
	if (old_num != series->base.num_elements)
		gog_plot_request_cardinality_update (series->base.plot);

	if (series_parent_klass->update)
		series_parent_klass->update (obj);
}

static void
gog_pie_series_class_init (GObjectClass *gobject_klass)
{
	GogObjectClass *gog_klass = (GogObjectClass *)gobject_klass;
	GogSeriesClass *series_klass = (GogSeriesClass *)gobject_klass;

	series_parent_klass = g_type_class_peek_parent (gobject_klass);
	gog_klass->update = gog_pie_series_update;
	series_klass->series_element_type = GOG_PIE_SERIES_ELEMENT_TYPE;

	gobject_klass->set_property = gog_pie_series_set_property;
	gobject_klass->get_property = gog_pie_series_get_property;

	g_object_class_install_property (gobject_klass, SERIES_PROP_INITIAL_ANGLE,
		g_param_spec_float ("initial-angle", "initial-angle",
			"Degrees clockwise from 12 O'Clock.",
			0, G_MAXFLOAT, 0.,
			G_PARAM_READWRITE));
	g_object_class_install_property (gobject_klass, SERIES_PROP_SEPARATION,
		g_param_spec_float ("separation", "separation",
			"Default amount a slice is extended as a percentage of the radius",
			0, G_MAXFLOAT, 0.,
			G_PARAM_READWRITE));
}

GSF_DYNAMIC_CLASS (GogPieSeries, gog_pie_series,
	   gog_pie_series_class_init, NULL,
	   GOG_SERIES_TYPE)

G_MODULE_EXPORT void
go_plugin_init (GOPlugin *plugin, GOCmdContext *cc)
{
	GTypeModule *module = go_plugin_get_type_module (plugin);
	gog_pie_series_element_register_type (module);
	gog_pie_plot_register_type (module);
	gog_pie_view_register_type (module);
	gog_pie_series_register_type (module);
	gog_ring_plot_register_type (module);
}

G_MODULE_EXPORT void
go_plugin_shutdown (GOPlugin *plugin, GOCmdContext *cc)
{
}
