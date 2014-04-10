/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-radar.h
 *
 * Copyright (C) 2004 Michael Devine (mdevine@cs.stanford.edu)
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

#ifndef GOG_RADAR_H
#define GOG_RADAR_H

#include <goffice/graph/gog-plot-impl.h>

G_BEGIN_DECLS

/*-----------------------------------------------------------------------------
 *
 * GogRadarPlot
 *
 *-----------------------------------------------------------------------------
 */

typedef struct {
	GogPlot	base;
	gboolean default_style_has_markers;
	unsigned num_elements;
	struct {
		double minima, maxima;
	} r, t;
} GogRTPlot;

typedef GogRTPlot GogRadarPlot;

typedef GogRTPlot GogPolarPlot;
	
#define GOG_RT_PLOT_TYPE	(gog_rt_plot_get_type ())
#define GOG_RT_PLOT(o)		(G_TYPE_CHECK_INSTANCE_CAST ((o), GOG_RT_PLOT_TYPE, GogRTPlot))
#define GOG_IS_PLOT_RT(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOG_RT_PLOT_TYPE))

GType gog_rt_plot_get_type (void);

#define GOG_RADAR_PLOT_TYPE	(gog_radar_plot_get_type ())
#define GOG_RADAR_PLOT(o)	(G_TYPE_CHECK_INSTANCE_CAST ((o), GOG_RADAR_PLOT_TYPE, GogRadarPlot))
#define GOG_IS_PLOT_RADAR(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOG_RADAR_PLOT_TYPE))

GType gog_radar_plot_get_type (void);

#define GOG_POLAR_PLOT_TYPE	(gog_polar_plot_get_type ())
#define GOG_POLAR_PLOT(o)	(G_TYPE_CHECK_INSTANCE_CAST ((o), GOG_POLAR_PLOT_TYPE, GogPolarPlot))
#define GOG_IS_PLOT_POLAR(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOG_POLAR_PLOT_TYPE))

GType gog_polar_plot_get_type (void);

G_END_DECLS

#endif /* GOG_RADAR_H */
