/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-grid-line.c
 *
 * Copyright (C) 2004 Emmanuel Pacaud (emmanuel.pacaud@univ-poitiers.fr)
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

#include <math.h>

#include <goffice/goffice-config.h>
#include <goffice/graph/gog-styled-object.h>
#include <goffice/graph/gog-chart.h>
#include <goffice/graph/gog-axis.h>
#include <goffice/graph/gog-grid-line.h>
#include <goffice/graph/gog-styled-object.h>
#include <goffice/graph/gog-style.h>
#include <goffice/graph/gog-view.h>
#include <goffice/graph/gog-theme.h>
#include <goffice/graph/gog-renderer.h>

//#include <src/gui-util.h>
#include <gui-util.h>
#include <glib/gi18n.h>

#include <gsf/gsf-impl-utils.h>

struct _GogGridLine {
	GogStyledObject	base;

	gboolean is_minor;
};

typedef GogStyledObjectClass GogGridLineClass;


static GType gog_grid_line_view_get_type (void);
static GogViewClass *gview_parent_klass;

enum {
	GRID_LINE_PROP_0,
	GRID_LINE_PROP_IS_MINOR,
};

gboolean
gog_grid_line_is_minor (GogGridLine *ggl)
{
	g_return_val_if_fail (GOG_GRID_LINE (ggl) != NULL, FALSE);

	return ggl->is_minor;
}

static void
gog_grid_line_init_style (GogStyledObject *gso, GogStyle *style)
{
	style->interesting_fields = GOG_STYLE_LINE;
	gog_theme_fillin_style (gog_object_get_theme (GOG_OBJECT (gso)),
		style, GOG_OBJECT (gso), 0, FALSE);
}

static void
gog_grid_line_set_property (GObject *obj, guint param_id,
			    GValue const *value, GParamSpec *pspec)
{
	GogGridLine *grid_line = GOG_GRID_LINE (obj);

	switch (param_id) {
		case GRID_LINE_PROP_IS_MINOR:
			grid_line->is_minor = g_value_get_boolean (value);
			break;

		default: 
			G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, param_id, pspec);
			return; /* NOTE : RETURN */
	}

	gog_object_emit_changed (GOG_OBJECT (obj), FALSE);
}

static void
gog_grid_line_get_property (GObject *obj, guint param_id,
			    GValue *value, GParamSpec *pspec)
{
	GogGridLine *grid_line = GOG_GRID_LINE (obj);

	switch (param_id) {
		case GRID_LINE_PROP_IS_MINOR:
			g_value_set_boolean (value, grid_line->is_minor);
			break;

		default: 
			G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, param_id, pspec);
			 break;
	}
}

static void
gog_grid_line_class_init (GogGridLineClass *klass)
{
	GObjectClass *gobject_klass = (GObjectClass *) klass;
	GogObjectClass *gog_klass = (GogObjectClass *) klass;
	GogStyledObjectClass *style_klass = (GogStyledObjectClass *) klass;

	gobject_klass->set_property = gog_grid_line_set_property;
	gobject_klass->get_property = gog_grid_line_get_property;
	gog_klass->view_type	= gog_grid_line_view_get_type ();
	style_klass->init_style = gog_grid_line_init_style;
	
	g_object_class_install_property (gobject_klass, GRID_LINE_PROP_IS_MINOR,
		g_param_spec_boolean ("is_minor", "is_minor",
			"Are these minor grid lines", FALSE, G_PARAM_READWRITE));
}

GSF_CLASS (GogGridLine, gog_grid_line,
	   gog_grid_line_class_init, NULL,
	   GOG_STYLED_OBJECT_TYPE)

/************************************************************************/

typedef GogView		GogGridLineView;
typedef GogViewClass	GogGridLineViewClass;

#define GOG_GRID_LINE_VIEW_TYPE		(gog_grid_line_view_get_type ())
#define GOG_GRID_LINE8VIEW(o)		(G_TYPE_CHECK_INSTANCE_CAST ((o), GOG_GRID_LINE_VIEW_TYPE, GogGridLineView))
#define IS_GOG_GRID_LINE_VIEW(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOG_GRID_LINE_VIEW_TYPE))

static void
gog_grid_line_view_render (GogView *view, GogViewAllocation const *bbox)
{
	GogGridLine *grid_line = GOG_GRID_LINE (view->model);
	GSList *axis_list;
	GogAxis *axis, *circular_axis;
	GogChart *chart;
	GogView *chart_view;
	GogStyle *style;
	GogAxisType axis_type;
	GogAxisMap *map = NULL;
	GogAxisTick *ticks;
	unsigned tick_nbr, i, j;
	double center_x, center_y, radius;
	double circular_min, circular_max;
	double angle, position;
	unsigned num_radii;
	ArtVpath path[3];
	ArtVpath *c_path;
	GogViewAllocation const *plot_area;
	double line_width;

	axis = GOG_AXIS (view->model->parent);
	g_return_if_fail (axis != NULL);
	chart = GOG_CHART (view->model->parent->parent);
	g_return_if_fail (chart != NULL);
	g_return_if_fail (view->parent != NULL);
	chart_view = GOG_VIEW (view->parent->parent);
	g_return_if_fail (chart_view != NULL);

	axis_type = gog_axis_get_atype (axis);
	tick_nbr = gog_axis_get_ticks (axis, &ticks);
	if (tick_nbr < 1)
		return;

	plot_area = gog_chart_view_get_plot_area (chart_view);
	style = gog_styled_object_get_style (GOG_STYLED_OBJECT (grid_line));
	gog_renderer_push_style (view->renderer, style);
	line_width = gog_renderer_line_size (view->renderer, style->line.width);
	
	path[0].code = ART_MOVETO;
	path[1].code = ART_LINETO;
	path[2].code = ART_END;

	if (line_width > 0) {
		switch (axis_type) {
			case GOG_AXIS_X:
			case GOG_AXIS_Y:

				switch (axis_type) {
					case GOG_AXIS_X:
						map = gog_axis_map_new (axis, plot_area->x, plot_area->w);
						break;
					case GOG_AXIS_Y:
						map = gog_axis_map_new (axis, plot_area->y +plot_area->h, -plot_area->h);
						break;
					default:
						return;
				}

				switch (axis_type) {
					case GOG_AXIS_X: 
						for (i = 0; i < tick_nbr; i++) {
							if ((ticks[i].type == GOG_AXIS_TICK_MAJOR && !grid_line->is_minor) ||
							    (ticks[i].type == GOG_AXIS_TICK_MINOR && grid_line->is_minor)) {
								path[0].y = plot_area->y;
								path[1].y = plot_area->y + plot_area->h;
								path[0].x = 
								path[1].x = gog_axis_map_to_canvas (map, ticks[i].position);
								gog_renderer_draw_sharp_path (view->renderer, path, NULL);
							}
						}
						break;
					case GOG_AXIS_Y: 
						for (i = 0; i < tick_nbr; i++) {
							if ((ticks[i].type == GOG_AXIS_TICK_MAJOR && !grid_line->is_minor) ||
							    (ticks[i].type == GOG_AXIS_TICK_MINOR && grid_line->is_minor)) {
								path[0].x = plot_area->x;
								path[1].x = plot_area->x + plot_area->w;
								path[0].y = 
								path[1].y = gog_axis_map_to_canvas (map, ticks[i].position);
								gog_renderer_draw_sharp_path (view->renderer, path, NULL);
							}
						}
						break;
					default:
						break;
				}
				gog_axis_map_free (map);
				break;

			case GOG_AXIS_RADIAL:
				center_x = plot_area->x + (plot_area->w/2);
				center_y = plot_area->y + (plot_area->h/2);
				radius = plot_area->h > plot_area->w 
					? plot_area->w / 2.0 
					: plot_area->h / 2.0;
				map = gog_axis_map_new (axis, 0., radius);
				
				axis_list = gog_chart_get_axis (chart, GOG_AXIS_CIRCULAR);
				if (axis_list == NULL)
					break;
				circular_axis = GOG_AXIS (axis_list->data);
				g_slist_free (axis_list);
				gog_axis_get_bounds (circular_axis, &circular_min, &circular_max);
				num_radii = rint (circular_max);
				if (num_radii < 3) {
					gog_axis_map_free (map);
					break;
				}
				c_path = g_new (ArtVpath, num_radii + 2);
				c_path[num_radii + 1].code = ART_END;

				for (i = 0; i < tick_nbr; i++) {
					if ((ticks[i].type == GOG_AXIS_TICK_MAJOR && !grid_line->is_minor) ||
					    (ticks[i].type == GOG_AXIS_TICK_MINOR && grid_line->is_minor)) {
						position = gog_axis_map_to_canvas (map, ticks[i].position);
						c_path[0].x = center_x;
						c_path[0].y = center_y - position;
						c_path[0].code = ART_MOVETO;
						for (j = 1; j < num_radii + 1; j++) {
							angle = j * 2.0 * M_PI/(num_radii) - M_PI / 2.;
							c_path[j].x = center_x + cos (angle) * position;
							c_path[j].y = center_y + sin (angle) * position; 
							c_path[j].code = ART_LINETO;
						}
						gog_renderer_draw_path (view->renderer, c_path, NULL);
					}
				}

				g_free (c_path);
				gog_axis_map_free (map);
				break;

			default:
				break;
		}
	}

	gog_renderer_pop_style (view->renderer);
}

static void
gog_grid_line_view_class_init (GogGridLineViewClass *gview_klass)
{
	GogViewClass *view_klass    = (GogViewClass *) gview_klass;

	gview_parent_klass = g_type_class_peek_parent (gview_klass);
	view_klass->render = gog_grid_line_view_render;
}

static GSF_CLASS (GogGridLineView, gog_grid_line_view,
	   gog_grid_line_view_class_init, NULL,
	   GOG_VIEW_TYPE)
