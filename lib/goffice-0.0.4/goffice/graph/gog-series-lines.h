/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-series-lines.h :  
 *
 * Copyright (C) 2005 Jean Brefort (jean.brefort@normalesup.org)
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Library General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301, USA.
 */

#ifndef GOG_SERIES_LINES_H
#define GOG_SERIES_LINES_H

#include <goffice/graph/goffice-graph.h>
#include <goffice/graph/gog-styled-object.h>
#include <libart_lgpl/libart.h>

typedef GogStyledObject GogSeriesLines;
typedef GogStyledObjectClass GogSeriesLinesClass;

#define GOG_SERIES_LINES_TYPE		(gog_series_lines_get_type ())
#define GOG_SERIES_LINES(o)		(G_TYPE_CHECK_INSTANCE_CAST ((o), GOG_SERIES_LINES_TYPE, GogSeriesLines))
#define IS_GOG_SERIES_LINES(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOG_SERIES_LINES_TYPE))

GType gog_series_lines_get_type (void);
void  gog_series_lines_register_type (GTypeModule *module);
void gog_series_lines_render (GogSeriesLines *lines, GogRenderer *rend, GogViewAllocation const *bbox, ArtVpath *path, gboolean invert);

 #endif	/* GOG_SERIES_LINES_H */
