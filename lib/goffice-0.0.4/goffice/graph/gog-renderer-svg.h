/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-renderer-svg.h :
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
#ifndef GOG_RENDERER_SVG_H
#define GOG_RENDERER_SVG_H

#include <goffice/graph/goffice-graph.h>
#include <gsf/gsf.h>

G_BEGIN_DECLS

gboolean gog_graph_export_to_svg (GogGraph *graph,
				  GsfOutput *output,
				  double width, double height, double scale);

G_END_DECLS

#endif /* GOG_RENDERER_SVG_H */
