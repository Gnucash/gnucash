/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-grid.c
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
#include <goffice/graph/gog-grid.h>
#include <goffice/graph/gog-chart.h>
#include <goffice/graph/gog-styled-object.h>
#include <goffice/graph/gog-style.h>
#include <goffice/graph/gog-view.h>
#include <goffice/graph/gog-theme.h>
#include <goffice/graph/gog-renderer.h>

#include <goffice/utils/go-math.h>

#include <glib/gi18n.h>

#include <gsf/gsf-impl-utils.h>

struct _GogGrid {
	GogStyledObject	base;
};
typedef GogStyledObjectClass GogGridClass;


static GType gog_grid_view_get_type (void);
static GogViewClass *gview_parent_klass;

static void
gog_grid_init_style (GogStyledObject *gso, GogStyle *style)
{
	style->interesting_fields = GOG_STYLE_FILL | GOG_STYLE_OUTLINE;
	gog_theme_fillin_style (gog_object_get_theme (GOG_OBJECT (gso)),
		style, GOG_OBJECT (gso), 0, FALSE);
}

static void
gog_grid_class_init (GogGridClass *klass)
{
	GogObjectClass *gog_klass = (GogObjectClass *) klass;
	GogStyledObjectClass *style_klass = (GogStyledObjectClass *) klass;

	gog_klass->view_type	= gog_grid_view_get_type ();
	style_klass->init_style = gog_grid_init_style;
}

GSF_CLASS (GogGrid, gog_grid,
	   gog_grid_class_init, NULL,
	   GOG_STYLED_OBJECT_TYPE)

/************************************************************************/

typedef GogView		GogGridView;
typedef GogViewClass	GogGridViewClass;

#define GOG_GRID_VIEW_TYPE	(gog_grid_view_get_type ())
#define GOG_GRID_VIEW(o)	(G_TYPE_CHECK_INSTANCE_CAST ((o), GOG_GRID_VIEW_TYPE, GogGridView))
#define IS_GOG_GRID_VIEW(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOG_GRID_VIEW_TYPE))

static void
gog_grid_view_render (GogView *view, GogViewAllocation const *bbox)
{
	GogGrid *grid = GOG_GRID (view->model);
	GogChart *chart = GOG_CHART (gog_object_get_parent (view->model));
	
	gog_renderer_push_style (view->renderer, grid->base.style);
	switch (gog_chart_get_axis_set (chart)) {
		case GOG_AXIS_SET_X:
		case GOG_AXIS_SET_XY: {
			ArtVpath path[6];
			
			path[0].code = ART_MOVETO;
			path[1].code = ART_LINETO;
			path[2].code = ART_LINETO;
			path[3].code = ART_LINETO;
			path[4].code = ART_LINETO;
			path[5].code = ART_END;
			path[0].x = path[1].x = path[4].x = view->allocation.x;
			path[2].x = path[3].x = path[0].x + view->allocation.w;
			path[0].y = path[3].y = path[4].y = view->allocation.y; 
			path[1].y = path[2].y = path[0].y + view->allocation.h; 

			gog_renderer_draw_sharp_polygon (view->renderer, path, FALSE);
			break;
			}
		case GOG_AXIS_SET_RADAR: {
			GogAxis *c_axis, *r_axis;
			GogViewAllocation const *area = gog_chart_view_get_plot_area (view->parent);
			GogChartMap *c_map;
			GogAxisMap *map;
			GogChartMapPolarData *parms;
			GSList *axis_list;
			ArtVpath *path;
			double position, start, stop;
			unsigned step_nbr, i;
		       
			axis_list = gog_chart_get_axes (chart, GOG_AXIS_CIRCULAR);
			if (axis_list == NULL)
				break;
			c_axis = GOG_AXIS (axis_list->data);
			g_slist_free (axis_list);

			axis_list = gog_chart_get_axes (chart, GOG_AXIS_RADIAL);
			if (axis_list == NULL)
				break;
			r_axis = GOG_AXIS (axis_list->data);
			g_slist_free (axis_list);

			c_map = gog_chart_map_new (chart, area, c_axis, r_axis, NULL, FALSE);
			parms = gog_chart_map_get_polar_parms (c_map);
			map = gog_chart_map_get_axis_map (c_map, 1);
			gog_axis_map_get_extents (map, &start, &position);

			if (gog_axis_is_discrete (c_axis)) {
				map = gog_chart_map_get_axis_map (c_map, 0);
				gog_axis_map_get_extents (map, &start, &stop);
				step_nbr = go_rint (parms->th1 - parms->th0) + 1;
				path = art_new (ArtVpath, step_nbr + 2);
				for (i = 0; i <= step_nbr; i++) {
					gog_chart_map_2D_to_view (c_map, i + parms->th0, 
								  position, &path[i].x, &path[i].y);
					path[i].code = ART_LINETO;
				}
				path[0].code = ART_MOVETO;
				path[step_nbr + 1].code = ART_END;
				gog_renderer_draw_polygon (view->renderer, path, FALSE);
				g_free (path);
			} else {
				double a = gog_axis_map (map, position); 
				gog_renderer_draw_pie_wedge (view->renderer, parms->cx, parms->cy,
							     parms->rx * a, parms->ry * a,
							     -parms->th1, -parms->th0, FALSE);
			}
			gog_chart_map_free (c_map);
			break;
			}
		case GOG_AXIS_SET_XYZ:
		case GOG_AXIS_SET_XY_pseudo_3d:
		case GOG_AXIS_SET_ALL:
		case GOG_AXIS_SET_UNKNOWN:
		case GOG_AXIS_SET_NONE:
					 break;
	}
	gog_renderer_pop_style (view->renderer);

	(gview_parent_klass->render) (view, bbox);
}

static void
gog_grid_view_class_init (GogGridViewClass *gview_klass)
{
	GogViewClass *view_klass    = (GogViewClass *) gview_klass;

	gview_parent_klass = g_type_class_peek_parent (gview_klass);
	view_klass->render = gog_grid_view_render;
}

static GSF_CLASS (GogGridView, gog_grid_view,
	   gog_grid_view_class_init, NULL,
	   GOG_VIEW_TYPE)
