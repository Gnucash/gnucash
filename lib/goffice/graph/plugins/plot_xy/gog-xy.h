/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * go-xy.h
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

#ifndef GOG_XY_PLOT_H
#define GOG_XY_PLOT_H

#include <goffice/graph/gog-plot-impl.h>
#include <goffice/graph/gog-series-impl.h>

G_BEGIN_DECLS

typedef struct {
	GogPlot	base;

	struct {
		double minima, maxima;
		GOFormat *fmt;
	} x, y;
} Gog2DPlot;

typedef struct {
	Gog2DPlot	base;
	gboolean	default_style_has_markers;
	gboolean	default_style_has_lines;
} GogXYPlot;

typedef struct {
	Gog2DPlot	base;
	gboolean size_as_area;
	gboolean in_3d;
	gboolean show_negatives;
	float bubble_scale;
} GogBubblePlot;

#define GOG_2D_PLOT_TYPE	(gog_2d_plot_get_type ())
#define GOG_2D_PLOT(o)		(G_TYPE_CHECK_INSTANCE_CAST ((o), GOG_2D_PLOT_TYPE, Gog2DPlot))
#define GOG_IS_2D_PLOT(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOG_2D_PLOT_TYPE))

GType gog_2d_plot_get_type (void);

#define GOG_XY_PLOT_TYPE	(gog_xy_plot_get_type ())
#define GOG_XY_PLOT(o)		(G_TYPE_CHECK_INSTANCE_CAST ((o), GOG_XY_PLOT_TYPE, GogXYPlot))
#define GOG_IS_XY_PLOT(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOG_XY_PLOT_TYPE))

GType gog_xy_plot_get_type (void);

#define GOG_BUBBLE_PLOT_TYPE	(gog_bubble_plot_get_type ())
#define GOG_BUBBLE_PLOT(o)		(G_TYPE_CHECK_INSTANCE_CAST ((o), GOG_BUBBLE_PLOT_TYPE, GogBubblePlot))
#define GOG_IS_BUBBLE_PLOT(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOG_BUBBLE_PLOT_TYPE))

GType gog_bubble_plot_get_type (void);

typedef struct {
	GogSeries base;
	GogErrorBar *x_errors, *y_errors;
} GogXYSeries;

#define GOG_XY_SERIES_TYPE	(gog_xy_series_get_type ())
#define GOG_XY_SERIES(o)	(G_TYPE_CHECK_INSTANCE_CAST ((o), GOG_XY_SERIES_TYPE, GogXYSeries))
#define GOG_IS_XY_SERIES(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOG_XY_SERIES_TYPE))

G_END_DECLS

#endif /* GOG_XY_SERIES_H */
