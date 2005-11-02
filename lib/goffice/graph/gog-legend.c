/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-legend.c :
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
#include <goffice/graph/gog-legend.h>
#include <goffice/graph/gog-outlined-object.h>
#include <goffice/graph/gog-view.h>
#include <goffice/graph/gog-renderer.h>
#include <goffice/graph/gog-chart.h>
#include <goffice/graph/gog-style.h>
#include <goffice/graph/gog-theme.h>
#include <goffice/utils/go-color.h>
#include <goffice/utils/go-units.h>

#include <gsf/gsf-impl-utils.h>
#include <gtk/gtknotebook.h>
#include <glib/gi18n.h>

struct _GogLegend {
	GogOutlinedObject base;

	double	 swatch_size_pts;
	double	 swatch_padding_pts;
	gulong	 chart_cardinality_handle;
	gulong	 chart_child_name_changed_handle;
	unsigned cached_count;
	gboolean names_changed;
};

typedef GogStyledObjectClass GogLegendClass;

enum {
	LEGEND_PROP_0,
	LEGEND_SWATCH_SIZE_PTS,
	LEGEND_SWATCH_PADDING_PTS
};

static GType gog_legend_view_get_type (void);

static GObjectClass *parent_klass;

static void
gog_legend_set_property (GObject *obj, guint param_id,
			 GValue const *value, GParamSpec *pspec)
{
	GogLegend *legend = GOG_LEGEND (obj);

	switch (param_id) {
	case LEGEND_SWATCH_SIZE_PTS :
		legend->swatch_size_pts = g_value_get_double (value);
		break;
	case LEGEND_SWATCH_PADDING_PTS :
		legend->swatch_padding_pts = g_value_get_double (value);
		break;

	default: G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, param_id, pspec);
		 return; /* NOTE : RETURN */
	}
}

static void
gog_legend_get_property (GObject *obj, guint param_id,
			 GValue *value, GParamSpec *pspec)
{
	GogLegend *legend = GOG_LEGEND (obj);

	switch (param_id) {
	case LEGEND_SWATCH_SIZE_PTS :
		g_value_set_double (value, legend->swatch_size_pts);
		break;
	case LEGEND_SWATCH_PADDING_PTS :
		g_value_set_double (value, legend->swatch_padding_pts);
		break;

	default: G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, param_id, pspec);
		 break;
	}
}

static void
cb_chart_names_changed (GogLegend *legend)
{
	if (legend->names_changed)
		return;
	legend->names_changed = TRUE;
	gog_object_request_update (GOG_OBJECT (legend));
}

static void
gog_legend_parent_changed (GogObject *obj, gboolean was_set)
{
	GogObjectClass *gog_object_klass = GOG_OBJECT_CLASS (parent_klass);
	GogLegend *legend = GOG_LEGEND (obj);

	if (was_set) {
		if (legend->chart_cardinality_handle == 0)
			legend->chart_cardinality_handle =
				g_signal_connect_object (G_OBJECT (obj->parent),
					"notify::cardinality-valid",
					G_CALLBACK (gog_object_request_update),
					legend, G_CONNECT_SWAPPED);
		if (legend->chart_child_name_changed_handle == 0)
			legend->chart_child_name_changed_handle =
				g_signal_connect_object (G_OBJECT (obj->parent),
					"child-name-changed",
					G_CALLBACK (cb_chart_names_changed),
					legend, G_CONNECT_SWAPPED);
	} else {
		if (legend->chart_cardinality_handle != 0) {
			g_signal_handler_disconnect (G_OBJECT (obj->parent),
				legend->chart_cardinality_handle);
			legend->chart_cardinality_handle = 0;
		}
		if (legend->chart_child_name_changed_handle != 0) {
			g_signal_handler_disconnect (G_OBJECT (obj->parent),
				legend->chart_child_name_changed_handle);
			legend->chart_child_name_changed_handle = 0;
		}
	}

	gog_object_klass->parent_changed (obj, was_set);
}

static void
gog_legend_update (GogObject *obj)
{
	GogLegend *legend = GOG_LEGEND (obj);
	unsigned visible;
	gog_chart_get_cardinality (GOG_CHART (obj->parent), NULL, &visible);
	if (legend->cached_count != visible)
		legend->cached_count = visible;
	else if (!legend->names_changed)
		return;
	legend->names_changed = FALSE;
	gog_object_emit_changed	(obj, TRUE);
}

static gpointer
gog_legend_editor (GogObject *gobj, GogDataAllocator *dalloc, GnmCmdContext *cc)
{
	static guint legend_pref_page = 0;
	GtkWidget *notebook = gtk_notebook_new ();

	gog_styled_object_editor (GOG_STYLED_OBJECT (gobj), cc, notebook);
	gog_style_handle_notebook (notebook, &legend_pref_page);
	return notebook;
}

static void
gog_legend_init_style (GogStyledObject *gso, GogStyle *style)
{
	style->interesting_fields = GOG_STYLE_OUTLINE | GOG_STYLE_FILL | GOG_STYLE_FONT;
	gog_theme_fillin_style (gog_object_get_theme (GOG_OBJECT (gso)),
		style, GOG_OBJECT (gso), 0, FALSE);
}

static void
gog_legend_class_init (GogLegendClass *klass)
{
	static GogObjectRole const roles[] = {
		{ N_("Title"), "GogLabel",	0,
		  GOG_POSITION_COMPASS, GOG_POSITION_N|GOG_POSITION_ALIGN_CENTER, GOG_OBJECT_NAME_BY_ROLE,
		  NULL, NULL, NULL, NULL, NULL, NULL },
	};

	GObjectClass *gobject_klass   = (GObjectClass *) klass;
	GogObjectClass *gog_klass = (GogObjectClass *) klass;
	GogStyledObjectClass *style_klass = (GogStyledObjectClass *) klass;

	parent_klass = g_type_class_peek_parent (klass);
	gobject_klass->set_property = gog_legend_set_property;
	gobject_klass->get_property = gog_legend_get_property;

	gog_klass->parent_changed = gog_legend_parent_changed;
	gog_klass->update	  = gog_legend_update;
	gog_klass->editor	  = gog_legend_editor;
	gog_klass->view_type	  = gog_legend_view_get_type ();
	style_klass->init_style	  = gog_legend_init_style;
	gog_object_register_roles (gog_klass, roles, G_N_ELEMENTS (roles));

	g_object_class_install_property (gobject_klass, LEGEND_SWATCH_SIZE_PTS,
		g_param_spec_double ("swatch_size_pts", "Swatch Size pts",
			"size of the swatches in pts.",
			0, G_MAXDOUBLE, 0, G_PARAM_READWRITE|GOG_PARAM_PERSISTENT));
	g_object_class_install_property (gobject_klass, LEGEND_SWATCH_PADDING_PTS,
		g_param_spec_double ("swatch_padding_pts", "Swatch Padding pts",
			"padding between the swatches in pts.",
			0, G_MAXDOUBLE, 0, G_PARAM_READWRITE|GOG_PARAM_PERSISTENT));
}

static void
gog_legend_init (GogLegend *legend)
{
	legend->swatch_size_pts = GO_CM_TO_PT ((double).25);
	legend->swatch_padding_pts = GO_CM_TO_PT ((double).2);
	legend->cached_count = 0;
}

GSF_CLASS (GogLegend, gog_legend,
	   gog_legend_class_init, gog_legend_init,
	   GOG_OUTLINED_OBJECT_TYPE)

typedef struct {
	GogOutlinedView	base;
	double		line_height;
	gboolean	uses_lines;
} GogLegendView;
typedef GogOutlinedViewClass	GogLegendViewClass;

#define GOG_LEGEND_VIEW_TYPE	(gog_legend_view_get_type ())
#define GOG_LEGEND_VIEW(o)	(G_TYPE_CHECK_INSTANCE_CAST ((o), GOG_LEGEND_VIEW_TYPE, GogLegendView))
#define IS_GOG_LEGEND_VIEW(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOG_LEGEND_VIEW_TYPE))

static GogViewClass *lview_parent_klass;

typedef struct {
	GogView const *view;
	GogViewRequisition maximum;
	gboolean	uses_lines;
} size_closure;

static void
cb_size_elements (unsigned i, GogStyle const *style, char const *name,
		  size_closure *dat)
{
	GogViewRequisition req;
	gog_renderer_push_style (dat->view->renderer, style);
	gog_renderer_measure_text (dat->view->renderer, name, &req);
	gog_renderer_pop_style (dat->view->renderer);

	if (dat->maximum.w < req.w)
		dat->maximum.w = req.w;
	if (dat->maximum.h < req.h)
		dat->maximum.h = req.h;
	if (!dat->uses_lines && (style->interesting_fields & GOG_STYLE_LINE))
		dat->uses_lines = TRUE;
}

static void
gog_legend_view_size_request (GogView *v, GogViewRequisition *avail)
{
	size_closure dat;
	GogViewRequisition res;
	GogChart *chart = GOG_CHART (v->model->parent);
	GogLegend *l = GOG_LEGEND (v->model);
	unsigned n, mult = 1;

#ifdef GOG_WARN_TODO
#warning TODO : make this smarter (multiple columns and shrinking text)
#endif
	dat.view = v;
	dat.maximum.w = 0.;
	dat.maximum.h = gog_renderer_pt2r_y (v->renderer, l->swatch_size_pts);
	dat.uses_lines = FALSE;
	gog_chart_foreach_elem (chart, TRUE,
		(GogEnumFunc) cb_size_elements, &dat);
	((GogLegendView *)v)->line_height = dat.maximum.h;
	((GogLegendView *)v)->uses_lines = dat.uses_lines;

	if (dat.uses_lines)
		mult = 3;

	/* 1/2 between swatch and label */
	res.w = dat.maximum.w + gog_renderer_pt2r_x (v->renderer,
		mult * l->swatch_size_pts + .5 * l->swatch_padding_pts);
	gog_chart_get_cardinality (chart, NULL, &n);
	res.h = n * dat.maximum.h;

	gog_view_size_child_request (v, avail, &res);
	avail->w = res.w;
	avail->h = res.h;
	lview_parent_klass->size_request (v, avail);
}

typedef struct {
	GogView const *view;
	GogViewAllocation swatch;
	double step;
	double label_offset;
	double bottom;
	ArtVpath line_path[3];
} render_closure;

static void
cb_render_elements (unsigned i, GogStyle const *base_style, char const *name,
		    render_closure *dat)
{
	GogViewAllocation swatch = dat->swatch;
	GogView  const   *v = dat->view;
	GogStyle *style = NULL;
	GogViewAllocation pos;
	
	swatch.y += i * dat->step;
	/* Allow for floating point inaccuracy */
	if (swatch.y > dat->bottom + 0.0001)
		return;

	if (base_style->interesting_fields & GOG_STYLE_LINE) { /* line and marker */
		style = (GogStyle *)base_style;
		gog_renderer_push_style (v->renderer, style);
		dat->line_path[0].y = dat->line_path[1].y =  swatch.y + swatch.h / 2.;
		gog_renderer_draw_sharp_path (v->renderer, dat->line_path, NULL);
		gog_renderer_draw_marker (v->renderer,
			(dat->line_path[0].x + dat->line_path[1].x) / 2.,
			dat->line_path[0].y);
	} else {					/* area swatch */
		style = gog_style_dup (base_style);
		style->outline.width = 0; /* hairline */
		style->outline.color = RGBA_BLACK;

		gog_renderer_push_style (v->renderer, style);
		gog_renderer_draw_sharp_rectangle (v->renderer, &swatch, NULL);
	}
	pos.x = swatch.x + dat->label_offset;
	pos.y = swatch.y;
	pos.h = pos.w = -1;
	gog_renderer_draw_text (v->renderer, name, &pos, GTK_ANCHOR_NW, NULL);

	gog_renderer_pop_style (v->renderer);

	if (style != base_style)
		g_object_unref (style);
}

static void
gog_legend_view_render (GogView *v, GogViewAllocation const *bbox)
{
	render_closure dat;
	GogLegend *l = GOG_LEGEND (v->model);
	double pad_x = gog_renderer_pt2r_x (v->renderer, l->swatch_padding_pts);

	(lview_parent_klass->render) (v, bbox);

	dat.view = v;
	dat.swatch.x  = v->residual.x;
	dat.swatch.y  = v->residual.y;
	dat.swatch.w  = gog_renderer_pt2r_x (v->renderer, l->swatch_size_pts);
	dat.swatch.h  = gog_renderer_pt2r_y (v->renderer, l->swatch_size_pts);
	dat.label_offset = dat.swatch.w + pad_x / 2.;
	if (((GogLegendView *)v)->uses_lines) {
		dat.line_path[0].code = ART_MOVETO;
		dat.line_path[1].code = ART_LINETO;
		dat.line_path[2].code = ART_END;
		dat.line_path[0].x = dat.swatch.x;
		dat.line_path[1].x = dat.swatch.x + 3. * dat.swatch.w;
		dat.swatch.x += dat.swatch.w;
		dat.label_offset += dat.swatch.w;
	}
	dat.step      = ((GogLegendView *)v)->line_height;
	dat.bottom    = v->residual.y + v->residual.h -
		((GogLegendView *)v)->line_height;
	gog_chart_foreach_elem (GOG_CHART (v->model->parent), TRUE,
		(GogEnumFunc) cb_render_elements, &dat);
}

static void
gog_legend_view_class_init (GogLegendViewClass *gview_klass)
{
	GogViewClass *view_klass    = (GogViewClass *) gview_klass;

	lview_parent_klass = g_type_class_peek_parent (gview_klass);
	view_klass->size_request    = gog_legend_view_size_request;
	view_klass->render	    = gog_legend_view_render;
	view_klass->clip 	    = TRUE;
}

static GSF_CLASS (GogLegendView, gog_legend_view,
		  gog_legend_view_class_init, NULL,
		  GOG_OUTLINED_VIEW_TYPE)
