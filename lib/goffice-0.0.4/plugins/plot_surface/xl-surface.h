/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * xl-contour.h
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

#ifndef XL_CONTOUR_H
#define XL_CONTOUR_H

#include "gog-surface.h"

G_BEGIN_DECLS

/*-----------------------------------------------------------------------------
 *
 * XLContourPlot
 *
 *-----------------------------------------------------------------------------
 */

typedef struct {
	GogContourPlot base;
	char const **y_labels;
} XLContourPlot;

#define XL_CONTOUR_PLOT_TYPE	(xl_contour_plot_get_type ())
#define XL_CONTOUR_PLOT(o)	(G_TYPE_CHECK_INSTANCE_CAST ((o), XL_CONTOUR_PLOT_TYPE, XLContourPlot))
#define XL_PLOT_CONTOUR(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), XL_CONTOUR_PLOT_TYPE))

GType xl_contour_plot_get_type (void);
void  xl_contour_plot_register_type (GTypeModule *plugin);
void  xl_surface_series_register_type (GTypeModule *plugin);

G_END_DECLS

#endif /* XL_CONTOUR_H */
