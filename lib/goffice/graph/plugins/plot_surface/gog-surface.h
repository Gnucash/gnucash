/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-surface.h
 *
 * Copyright (C) 2004 Jean Brefort (jean.brefort@normalesup.org)
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

#ifndef GOG_SURFACE_H
#define GOG_SURFACE_H

#include <goffice/graph/gog-plot-impl.h>

G_BEGIN_DECLS

/*-----------------------------------------------------------------------------
 *
 * GogContourPlot
 *
 *-----------------------------------------------------------------------------
 */

typedef struct {
	GogPlot	base;
	
	unsigned levels;
	unsigned rows, columns;
	double zmin, zmax;
	double step, *limits;
	struct {
		double minima, maxima;
		GOFormat *fmt;
	} x, y;
} GogContourPlot;

#define GOG_CONTOUR_PLOT_TYPE	(gog_contour_plot_get_type ())
#define GOG_CONTOUR_PLOT(o)	(G_TYPE_CHECK_INSTANCE_CAST ((o), GOG_CONTOUR_PLOT_TYPE, GogContourPlot))
#define GOG_IS_PLOT_CONTOUR(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOG_CONTOUR_PLOT_TYPE))

GType gog_contour_plot_get_type (void);

G_END_DECLS

#endif /* GOG_SURFACE_H */
