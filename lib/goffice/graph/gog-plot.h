/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-plot.h : 
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
#ifndef GOG_PLOT_H
#define GOG_PLOT_H

#include <goffice/graph/goffice-graph.h>
#include <goffice/utils/goffice-utils.h>
#include <glib-object.h>

G_BEGIN_DECLS

typedef struct {
	struct {
		double minima, maxima;
	} val, logical;
	gboolean is_discrete;
	gboolean center_on_ticks;
	GOFormat *fmt;
} GogPlotBoundInfo;

#define GOG_PLOT_TYPE	(gog_plot_get_type ())
#define GOG_PLOT(o)	(G_TYPE_CHECK_INSTANCE_CAST ((o), GOG_PLOT_TYPE, GogPlot))
#define IS_GOG_PLOT(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOG_PLOT_TYPE))

GType	  gog_plot_get_type (void);
GogPlot  *gog_plot_new_by_type	(GogPlotType const *type);
GogPlot  *gog_plot_new_by_name	(char const *id);
gboolean  gog_plot_make_similar	(GogPlot *dst, GogPlot const *src);

void	  gog_plot_request_cardinality_update (GogPlot *plot);
void	  gog_plot_get_cardinality (GogPlot *plot,
				    unsigned *full, unsigned *visible);
void      gog_plot_foreach_elem    (GogPlot *plot, gboolean only_visible,
				    GogEnumFunc handler, gpointer data);
GSList const *gog_plot_get_series  (GogPlot const *plot);
GOData	 *gog_plot_get_axis_bounds (GogPlot *plot, GogAxisType axis,
				    GogPlotBoundInfo *bounds);

gboolean  gog_plot_supports_vary_style_by_element (GogPlot const *plot);

GogSeries	  *gog_plot_new_series	  (GogPlot *plot);
GogPlotDesc const *gog_plot_description	  (GogPlot const *plot);

GogAxisSet gog_plot_axis_set_pref	(GogPlot const *plot);
gboolean   gog_plot_axis_set_is_valid	(GogPlot const *plot, GogAxisSet type);
gboolean   gog_plot_axis_set_assign	(GogPlot *plot, GogAxisSet type);
void	   gog_plot_axis_clear		(GogPlot *plot, GogAxisSet filter);
GogAxis	  *gog_plot_get_axis		(GogPlot const *plot, GogAxisType type);

G_END_DECLS

#endif /* GOG_PLOT_H */
