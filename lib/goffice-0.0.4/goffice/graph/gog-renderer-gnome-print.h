/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-renderer-gnome-print.h : 
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
#ifndef GOG_RENDERER_GNOME_PRINT_H
#define GOG_RENDERER_GNOME_PRINT_H

#include <goffice/graph/goffice-graph.h>
#include <libgnomeprint/gnome-print.h>

G_BEGIN_DECLS

void gog_graph_print_to_gnome_print (GogGraph *graph,
				     GnomePrintContext *gp_context,
				     double width, double height);

G_END_DECLS

#endif /* GOG_RENDERER_GNOME_PRINT_H */
