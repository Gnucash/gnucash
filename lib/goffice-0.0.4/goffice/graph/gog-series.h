/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-series.h : 
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
#ifndef GOG_SERIES_H
#define GOG_SERIES_H

#include <goffice/graph/goffice-graph.h>
#include <goffice/data/goffice-data.h>
#include <goffice/graph/gog-object.h>
#include <goffice/graph/gog-styled-object.h>

G_BEGIN_DECLS

#define GOG_SERIES_ELEMENT_TYPE	(gog_series_element_get_type ())
#define GOG_SERIES_ELEMENT(o)	(G_TYPE_CHECK_INSTANCE_CAST ((o), GOG_SERIES_ELEMENT_TYPE, GogSeriesElement))
#define IS_GOG_SERIES_ELEMENT(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOG_SERIES_ELEMENT_TYPE))
GType gog_series_element_get_type (void);

#define GOG_SERIES_TYPE		(gog_series_get_type ())
#define GOG_SERIES(o)		(G_TYPE_CHECK_INSTANCE_CAST ((o), GOG_SERIES_TYPE, GogSeries))
#define IS_GOG_SERIES(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOG_SERIES_TYPE))

GType gog_series_get_type (void);
gboolean      gog_series_is_valid   (GogSeries const *series);
gboolean      gog_series_has_legend (GogSeries const *series);
GODataScalar *gog_series_get_name   (GogSeries const *series);
GogPlot	     *gog_series_get_plot   (GogSeries const *series);
void	      gog_series_set_name   (GogSeries *series,
				     GODataScalar *val, GError **err);
void	      gog_series_set_dim    (GogSeries *series, int dim_i,
				     GOData *val, GError **err);
void	      gog_series_set_index  (GogSeries *series,
				     int ind, gboolean is_manual);

unsigned      	  gog_series_num_elements  (GogSeries const *series);
GList const  	 *gog_series_get_overrides (GogSeries const *series);

G_END_DECLS

#endif /* GOG_SERIES_H */
