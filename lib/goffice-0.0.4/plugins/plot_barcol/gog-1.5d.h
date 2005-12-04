/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * go-1.5d.h
 *
 * Copyright (C) 2003-2004 Emmanuel Pacaud (emmanuel.pacaud@univ-poitiers.fr)
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

#ifndef GOG_1_5D_H
#define GOG_1_5D_H

#include <goffice/graph/gog-plot-impl.h>
#include <goffice/graph/gog-series-impl.h>
#include <goffice/graph/gog-error-bar.h>

G_BEGIN_DECLS

typedef enum {
	GOG_1_5D_NORMAL,
	GOG_1_5D_STACKED,
	GOG_1_5D_AS_PERCENTAGE
} GogPlot1_5dType;

typedef struct {
	GogPlot	base;
	GogPlot1_5dType type;
	gboolean	in_3d; /* placeholder */

	/* cached content */
	unsigned  num_series, num_elements;
	double    maxima, minima; /* meaning varies depending on type */
	gboolean  implicit_index;
	int support_series_lines:1;
	int support_drop_lines:1;
	int support_lines:1;
	GOFormat *fmt;
} GogPlot1_5d;
typedef struct {
	GogPlotClass	base;

	gboolean (*swap_x_and_y)		  (GogPlot1_5d *model);
	void     (*update_stacked_and_percentage) (GogPlot1_5d *model,
						   double **vals,
						   GogErrorBar **errors,
						   unsigned const *lengths);
} GogPlot1_5dClass;

#define GOG_PLOT1_5D_TYPE		(gog_plot1_5d_get_type ())
#define GOG_PLOT1_5D(o)			(G_TYPE_CHECK_INSTANCE_CAST ((o), GOG_PLOT1_5D_TYPE, GogPlot1_5d))
#define IS_GOG_PLOT1_5D(o)		(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOG_PLOT1_5D_TYPE))
#define GOG_PLOT1_5D_CLASS(k)		(G_TYPE_CHECK_CLASS_CAST ((k), GOG_PLOT1_5D_TYPE, GogPlot1_5dClass))
#define GOG_PLOT1_5D_GET_CLASS(o)	(G_TYPE_INSTANCE_GET_CLASS ((o), GOG_PLOT1_5D_TYPE, GogPlot1_5dClass))

GType gog_plot1_5d_get_type (void);
void  gog_plot1_5d_register_type (GTypeModule *module);

GogAxis * gog_plot1_5d_get_index_axis (GogPlot1_5d *model);

/***************************************************************************/

typedef struct {
	GogSeries base;
	GogErrorBar *errors;
	gboolean	index_changed; 
	int has_series_lines:1;
	int has_drop_lines:1;
	int has_lines:1;
} GogSeries1_5d;
typedef GogSeriesClass GogSeries1_5dClass;

#define GOG_SERIES1_5D_TYPE	(gog_series1_5d_get_type ())
#define GOG_SERIES1_5D(o)	(G_TYPE_CHECK_INSTANCE_CAST ((o), GOG_SERIES1_5D_TYPE, GogSeries1_5d))
#define IS_GOG_SERIES1_5D(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOG_SERIES1_5D_TYPE))

GType gog_series1_5d_get_type (void);
void  gog_series1_5d_register_type (GTypeModule *module);

G_END_DECLS

#endif /* GOG_1_5D_H */
