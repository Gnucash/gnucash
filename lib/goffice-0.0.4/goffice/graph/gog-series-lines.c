/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-series-lines.c :  
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

#include <goffice/goffice-config.h>
#include <goffice/graph/gog-plot.h>
#include <goffice/graph/gog-renderer.h>
#include <goffice/graph/gog-series-impl.h>
#include <goffice/graph/gog-style.h>
#include <goffice/graph/gog-theme.h>
#include <goffice/utils/go-marker.h>
#include "gog-series-lines.h"

#include <gsf/gsf-impl-utils.h>
#include <string.h>

static void
gog_series_lines_init_style (GogStyledObject *gso, GogStyle *style)
{
	GogStyle *parent_style = gog_styled_object_get_style (
			GOG_STYLED_OBJECT (gog_object_get_parent (GOG_OBJECT (gso))));
	GogPlot *plot  = GOG_PLOT (GOG_SERIES (
						gog_object_get_parent (GOG_OBJECT (gso)))->plot);
	char const *plot_name = G_OBJECT_TYPE_NAME (plot);
	style->interesting_fields =
		(parent_style->interesting_fields & GOG_STYLE_MARKER ||
		(!strcmp (plot_name, "GogBarColPlot") && strcmp (plot_name, "GogDropBarPlot")))?
			GOG_STYLE_LINE:
			GOG_STYLE_LINE | GOG_STYLE_MARKER;
	gog_theme_fillin_style (gog_object_get_theme (GOG_OBJECT (gso)),
		style, GOG_OBJECT (gso), 0, FALSE);
}

static void
gog_series_lines_update (GogObject *obj)
{
	gog_object_request_update (obj->parent);
}

static void
gog_series_lines_changed (GogObject *obj, gboolean size)
{
	gog_object_emit_changed (obj->parent, size);
}

static void
gog_series_lines_class_init (GogObjectClass *klass)
{
	GogStyledObjectClass *style_klass = (GogStyledObjectClass *) klass;

	klass->update = gog_series_lines_update;
	klass->changed = gog_series_lines_changed;
	style_klass->init_style = gog_series_lines_init_style;
}

GSF_CLASS (GogSeriesLines, gog_series_lines,
	gog_series_lines_class_init, NULL,
	GOG_STYLED_OBJECT_TYPE)

void
gog_series_lines_render (GogSeriesLines *lines, GogRenderer *rend, GogViewAllocation const *bbox, ArtVpath *path, gboolean invert)
{
	int i = 0;
	GogStyle *style = gog_styled_object_get_style (GOG_STYLED_OBJECT (lines));

	if (invert) {
		style = gog_style_dup (style);
		style->line.color ^= 0xffffff00;
		style->marker.mark->outline_color ^= 0xffffff00;
		style->marker.mark->fill_color ^= 0xffffff00;
	}
	gog_renderer_push_style (rend, style);
	gog_renderer_draw_sharp_path (rend, path);
	if ((style->interesting_fields & GOG_STYLE_MARKER) != 0)
		while (path[i].code != ART_END) {
			gog_renderer_draw_marker (rend, path[i].x, path[i].y);
			i++;
		}
	gog_renderer_pop_style (rend);
	if (invert)
		g_object_unref (style);
}
