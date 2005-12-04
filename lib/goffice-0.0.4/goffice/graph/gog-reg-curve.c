/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-reg-curve.c :  
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
#include <goffice/graph/gog-reg-curve.h>
#include <goffice/graph/gog-data-allocator.h>
#include <goffice/graph/gog-plot-engine.h>
#include <goffice/graph/gog-style.h>
#include <goffice/graph/gog-theme.h>
#include <goffice/graph/gog-view.h>
#include <goffice/graph/gog-series-impl.h>
#include <goffice/graph/gog-plot-impl.h>
#include <goffice/graph/gog-axis.h>
#include <goffice/graph/gog-renderer.h>
#include <goffice/data/go-data.h>
#include <goffice/utils/go-line.h>
#include <goffice/utils/go-math.h>
#include <goffice/gtk/goffice-gtk.h>
#include <gsf/gsf-impl-utils.h>
#include <glib/gi18n.h>
#include <gtk/gtktable.h>
#include <gtk/gtktogglebutton.h>

#define GOG_REG_CURVE_GET_CLASS(o)	(G_TYPE_INSTANCE_GET_CLASS ((o), GOG_REG_CURVE_TYPE, GogRegCurveClass))

static GObjectClass *reg_curve_parent_klass;

static GType gog_reg_curve_view_get_type (void);

enum {
	REG_CURVE_PROP_0,
	REG_CURVE_PROP_SKIP_INVALID,
};

static void
gog_reg_curve_init_style (GogStyledObject *gso, GogStyle *style)
{
	style->interesting_fields = GOG_STYLE_LINE;
	gog_theme_fillin_style (gog_object_get_theme (GOG_OBJECT (gso)),
		style, GOG_OBJECT (gso), 0, FALSE);
}

static void
skip_invalid_toggled_cb (GtkToggleButton* btn, GObject *obj)
{
	g_object_set (obj, "skip-invalid", gtk_toggle_button_get_active (btn), NULL);
}

static void
gog_reg_curve_populate_editor (GogObject *gobj, 
			  GogEditor *editor, 
			  GogDataAllocator *dalloc, 
			  GOCmdContext *cc)
{
	GtkWidget *w;
	GtkTable *table;
	GladeXML *gui;
	GogDataset *set = GOG_DATASET (gobj);

	gui = go_libglade_new ("gog-reg-curve-prefs.glade", "reg-curve-prefs", NULL, cc);
	if (gui == NULL)
		return;

	gog_editor_add_page (editor, 
			     glade_xml_get_widget (gui, "reg-curve-prefs"),
			     _("Details"));

	table = GTK_TABLE (glade_xml_get_widget (gui, "reg-curve-prefs"));
	w = GTK_WIDGET (gog_data_allocator_editor (dalloc, set, 0, GOG_DATA_SCALAR));
	gtk_widget_show (w);
	gtk_table_attach (table, w, 1, 2, 0, 1, GTK_FILL | GTK_EXPAND, 0, 0, 0);
	w = GTK_WIDGET (gog_data_allocator_editor (dalloc, set, 1, GOG_DATA_SCALAR));
	gtk_widget_show (w);
	gtk_table_attach (table, w, 1, 2, 1, 2, GTK_FILL | GTK_EXPAND, 0, 0, 0);
	w = glade_xml_get_widget (gui, "skip-invalid");
	gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (w),
					(GOG_REG_CURVE (gobj))->skip_invalid);
	g_signal_connect (G_OBJECT (w), "toggled",
		G_CALLBACK (skip_invalid_toggled_cb), gobj);


	(GOG_OBJECT_CLASS(reg_curve_parent_klass)->populate_editor) (gobj, editor, dalloc, cc);
}

static void
gog_reg_curve_get_property (GObject *obj, guint param_id,
		       GValue *value, GParamSpec *pspec)
{
	GogRegCurve *rc = GOG_REG_CURVE (obj);
	switch (param_id) {
	case REG_CURVE_PROP_SKIP_INVALID:
		g_value_set_boolean (value, rc->skip_invalid);
		break;

	default: G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, param_id, pspec);
		 break;
	}
}

static void
gog_reg_curve_set_property (GObject *obj, guint param_id,
		       GValue const *value, GParamSpec *pspec)
{
	GogRegCurve *rc = GOG_REG_CURVE (obj);
	switch (param_id) {
	case REG_CURVE_PROP_SKIP_INVALID:
		rc->skip_invalid = g_value_get_boolean (value);
		gog_object_request_update (GOG_OBJECT (obj));
		break;

	default: G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, param_id, pspec);
		 return; /* NOTE : RETURN */
	}
}

static void
gog_reg_curve_finalize (GObject *obj)
{
	GogRegCurve *rc = GOG_REG_CURVE (obj);
	if (rc->bounds != NULL) {
		gog_dataset_finalize (GOG_DATASET (obj));
		g_free (rc->bounds);
		rc->bounds = NULL;
	}
	(*reg_curve_parent_klass->finalize) (obj);
}

static char const *
gog_reg_curve_type_name (GogObject const *gobj)
{
	return N_("Regression Curve");
}

static void
gog_reg_curve_class_init (GogObjectClass *gog_klass)
{
	static GogObjectRole const roles[] = {
		{ N_("Equation"), "GogRegEqn",	0,
		  GOG_POSITION_ANY_MANUAL, GOG_POSITION_MANUAL, GOG_OBJECT_NAME_BY_ROLE,
		  NULL, NULL, NULL, NULL, NULL, NULL },
	};
	GObjectClass *gobject_klass = (GObjectClass *) gog_klass;
	GogStyledObjectClass *style_klass = (GogStyledObjectClass *) gog_klass;
	GogRegCurveClass *reg_curve_klass = (GogRegCurveClass *) gog_klass;
	reg_curve_parent_klass = g_type_class_peek_parent (gog_klass);

	gobject_klass->get_property = gog_reg_curve_get_property;
	gobject_klass->set_property = gog_reg_curve_set_property;
	gobject_klass->finalize = gog_reg_curve_finalize;
	gog_klass->populate_editor	= gog_reg_curve_populate_editor;
	style_klass->init_style = gog_reg_curve_init_style;
	gog_klass->type_name	= gog_reg_curve_type_name;
	gog_klass->view_type	= gog_reg_curve_view_get_type ();
	gog_object_register_roles (gog_klass, roles, G_N_ELEMENTS (roles));

	reg_curve_klass->get_value_at = NULL;
	reg_curve_klass->get_equation = NULL;
	reg_curve_klass->get_R2 = NULL;

	g_object_class_install_property (gobject_klass, REG_CURVE_PROP_SKIP_INVALID,
		g_param_spec_boolean ("skip-invalid", "skip-invalid",
			"Skip invalid data", FALSE, G_PARAM_READWRITE|GOG_PARAM_PERSISTENT));
}

static void
gog_reg_curve_init (GogRegCurve *reg_curve)
{
	reg_curve->ninterp = 100;
	reg_curve->bounds = g_new0 (GogDatasetElement, 2);
}

static void
gog_reg_curve_dataset_dims (GogDataset const *set, int *first, int *last)
{
	*first = 0;
	*last = 1;
}

static GogDatasetElement *
gog_reg_curve_dataset_get_elem (GogDataset const *set, int dim_i)
{
	GogRegCurve const *rc = GOG_REG_CURVE (set);
	g_return_val_if_fail (2 > dim_i, NULL);
	g_return_val_if_fail (dim_i >= 0, NULL);
	return rc->bounds + dim_i;
}

static void
gog_reg_curve_dataset_dim_changed (GogDataset *set, int dim_i)
{
	gog_object_request_update (GOG_OBJECT (set));
}

static void
gog_reg_curve_dataset_init (GogDatasetClass *iface)
{
	iface->get_elem	   = gog_reg_curve_dataset_get_elem;
	iface->dims	   = gog_reg_curve_dataset_dims;
	iface->dim_changed	   = gog_reg_curve_dataset_dim_changed;
}

GSF_CLASS_FULL (GogRegCurve, gog_reg_curve,
	   NULL, NULL, gog_reg_curve_class_init, NULL,
	   gog_reg_curve_init, GOG_STYLED_OBJECT_TYPE, G_TYPE_FLAG_ABSTRACT,
		GSF_INTERFACE (gog_reg_curve_dataset_init, GOG_DATASET_TYPE))

GogRegCurve *
gog_reg_curve_new_by_type (GogRegCurveType const *type)
{
	GogRegCurve *res;

	g_return_val_if_fail (type != NULL, NULL);

	res = gog_reg_curve_new_by_name (type->engine);
	if (res != NULL && type->properties != NULL)
		g_hash_table_foreach (type->properties,
			(GHFunc) gog_object_set_arg, res);
	return res;
}

static double
gog_reg_curve_get_value_at (GogRegCurve *reg_curve, double x)
{
	return (GOG_REG_CURVE_GET_CLASS (reg_curve))->get_value_at (reg_curve, x);
}

gchar const*
gog_reg_curve_get_equation (GogRegCurve *reg_curve)
{
	return (GOG_REG_CURVE_GET_CLASS (reg_curve))->get_equation (reg_curve);
}

double
gog_reg_curve_get_R2 (GogRegCurve *reg_curve)
{
	return (GOG_REG_CURVE_GET_CLASS (reg_curve))->get_R2 (reg_curve);
}

void
gog_reg_curve_get_bounds (GogRegCurve *reg_curve, double *xmin, double *xmax)
{
	if (reg_curve->bounds[0].data) {
		*xmin = go_data_scalar_get_value (
			GO_DATA_SCALAR (reg_curve->bounds[0].data));
		if (*xmin == go_nan || !go_finite (*xmin))
			*xmin = -DBL_MAX;
	} else
		*xmin = -DBL_MAX;
	if (reg_curve->bounds[1].data) {
		*xmax = go_data_scalar_get_value (
			GO_DATA_SCALAR (reg_curve->bounds[1].data));
		if (*xmax == go_nan || !go_finite (*xmax))
			*xmax = DBL_MAX;
	} else
		*xmax = DBL_MAX;
}

/****************************************************************************/

typedef GogView		GogRegCurveView;
typedef GogViewClass	GogRegCurveViewClass;

#define GOG_REG_CURVE_VIEW_TYPE	(gog_reg_curve_view_get_type ())
#define GOG_REG_CURVE_VIEW(o)	(G_TYPE_CHECK_INSTANCE_CAST ((o), GOG_REG_CURVE_VIEW_TYPE, GogRegCurveView))
#define IS_GOG_REG_CURVE_VIEW(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOG_REG_CURVE_VIEW_TYPE))

static GogViewClass *reg_curve_view_parent_klass;

static void
gog_reg_curve_view_render (GogView *view, GogViewAllocation const *bbox)
{
	GogRegCurve *rc = GOG_REG_CURVE (view->model);
	GogSeries *series = GOG_SERIES ((GOG_OBJECT (rc))->parent);
	GogPlot *plot = series->plot;
	GogAxisMap *x_map, *y_map;
	double buf, *x, *y;
	GogStyle *style;
	ArtBpath *path;
	int i;
	GSList *ptr;

	x_map = gog_axis_map_new (plot->axis[0], 
				  view->residual.x , view->residual.w);
	y_map = gog_axis_map_new (plot->axis[1], 
				  view->residual.y + view->residual.h, 
				  -view->residual.h);

	if (!(gog_axis_map_is_valid (x_map) &&
	      gog_axis_map_is_valid (y_map))) {
		gog_axis_map_free (x_map);
		gog_axis_map_free (y_map);
		return;
	}
	
	gog_renderer_push_clip (view->renderer, 
		gog_renderer_get_rectangle_vpath (&view->residual)); 
	
	x = g_new (double, rc->ninterp + 3);
	y = g_new (double, rc->ninterp + 3);
	gog_axis_get_bounds (plot->axis[0], x, x + rc->ninterp + 1);
	x[0] = gog_axis_map_to_view (x_map, x[0]);
	x[rc->ninterp + 1] = gog_axis_map_to_view (x_map, x[rc->ninterp + 1]);
	buf = (x[rc->ninterp + 1] - x[0]) / (rc->ninterp + 1);
	for (i = 1; i <= rc->ninterp; i++)
		x[i] = x[0] + i * buf;

	for (i = 0; i <= rc->ninterp + 1; i++)
		y[i] = gog_axis_map_to_view (y_map,
					gog_reg_curve_get_value_at (rc, gog_axis_map_from_view (x_map, x[i])));

	path = go_line_build_bpath (x, y, rc->ninterp + 2);
	style = GOG_STYLED_OBJECT (rc)->style;
	gog_renderer_push_style (view->renderer, style);
	gog_renderer_draw_bezier_path (view->renderer, path);

	gog_renderer_pop_style (view->renderer);
	g_free (x);
	g_free (y);
	art_free (path);
	gog_axis_map_free (x_map);
	gog_axis_map_free (y_map);

	gog_renderer_pop_clip (view->renderer);
	
	for (ptr = view->children ; ptr != NULL ; ptr = ptr->next)
		gog_view_render	(ptr->data, bbox);
}

static void
gog_reg_curve_view_size_allocate (GogView *view, GogViewAllocation const *allocation)
{
	GSList *ptr;

	for (ptr = view->children; ptr != NULL; ptr = ptr->next)
		gog_view_size_allocate (GOG_VIEW (ptr->data), allocation);
	(reg_curve_view_parent_klass->size_allocate) (view, allocation);
}

static void
gog_reg_curve_view_class_init (GogRegCurveViewClass *gview_klass)
{
	GogViewClass *view_klass    = (GogViewClass *) gview_klass;
	reg_curve_view_parent_klass = g_type_class_peek_parent (gview_klass);

	view_klass->render	  = gog_reg_curve_view_render;
	view_klass->size_allocate = gog_reg_curve_view_size_allocate;
}

static GSF_CLASS (GogRegCurveView, gog_reg_curve_view,
		  gog_reg_curve_view_class_init, NULL,
		  GOG_VIEW_TYPE)
