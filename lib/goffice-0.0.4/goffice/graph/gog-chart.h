/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-chart.h :
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

#ifndef GOG_CHART_H
#define GOG_CHART_H

#include <goffice/graph/goffice-graph.h>
#include <goffice/graph/gog-axis.h>
#include <glib-object.h>

G_BEGIN_DECLS

GogAxisSet gog_axis_set_from_str (char const *str);
	
typedef struct {
	double cx, cy;
	double rx, ry;
	double th0, th1;
} GogChartMapPolarData;

typedef struct _GogChartMap GogChartMap;

struct _GogChartMap {
	GogChart 		*chart;
	GogViewAllocation	 area;
	gpointer	 	 data;
	GogAxisMap		*axis_map[3];
	gboolean		 is_valid;

	void (*map_2D_to_view)	(GogChartMap *map, double x, double y, double *u, double *v);
};

GogChartMap 	*gog_chart_map_new 		(GogChart *chart, GogViewAllocation const *area,
						 GogAxis *axis0, GogAxis *axis1, GogAxis *axis2,
						 gboolean fill_area);
void 		 gog_chart_map_2D_to_view	(GogChartMap *map, double x, double y, double *u, double *v);
GogAxisMap	*gog_chart_map_get_axis_map 	(GogChartMap *map, unsigned i);
gboolean	 gog_chart_map_is_valid 	(GogChartMap *map);
void		 gog_chart_map_free 		(GogChartMap *map);

GogChartMapPolarData *gog_chart_map_get_polar_parms (GogChartMap *map);
	
#define GOG_CHART_TYPE	(gog_chart_get_type ())
#define GOG_CHART(o)	(G_TYPE_CHECK_INSTANCE_CAST ((o), GOG_CHART_TYPE, GogChart))
#define IS_GOG_CHART(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOG_CHART_TYPE))

GType gog_chart_get_type (void);

gboolean  gog_chart_get_position  (GogChart const *chart, unsigned *x, unsigned *y,
				   unsigned *cols, unsigned *rows);
void	  gog_chart_set_position  (GogChart *chart, unsigned x, unsigned y,
				   unsigned cols, unsigned rows);
gboolean  gog_chart_get_plot_area (GogChart *chart, GogViewAllocation *plot_area);
void 	  gog_chart_set_plot_area (GogChart *chart, GogViewAllocation const *plot_area);

void	 gog_chart_request_cardinality_update (GogChart *chart);
void	 gog_chart_get_cardinality (GogChart *chart,
				    unsigned *full, unsigned *visible);
void	 gog_chart_foreach_elem	   (GogChart *chart, gboolean only_visible,
				    GogEnumFunc handler, gpointer data);
GSList	*gog_chart_get_plots	   (GogChart const *chart);

GogAxisSet gog_chart_get_axis_set    (GogChart const *chart);
gboolean gog_chart_axis_set_is_valid (GogChart const *chart, GogAxisSet type);
gboolean gog_chart_axis_set_assign   (GogChart *chart, GogAxisSet type);
GSList	*gog_chart_get_axes	     (GogChart const *chart, GogAxisType type);

GogGrid *gog_chart_get_grid	     (GogChart const *chart);

/* View utils */
GogViewAllocation const *gog_chart_view_get_plot_area (GogView const *view);

G_END_DECLS

#endif /* GOG_CHART_H */
