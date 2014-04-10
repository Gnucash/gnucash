/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * go-line.h : 
 *
 * Copyright (C) 2004 Emmanuel Pacaud (emmanuel.pacaud@univ-poitiers.fr)
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

#ifndef GO_LINE_H
#define GO_LINE_H

#include <goffice/graph/goffice-graph.h>
#include <libart_lgpl/libart.h>
#include <glib.h>

G_BEGIN_DECLS

typedef enum {
	GO_LINE_NONE,
	GO_LINE_SOLID,
	GO_LINE_DASH,
	GO_LINE_DOT,
	GO_LINE_DASH_DOT,
	GO_LINE_DASH_DOT_DOT,
	GO_LINE_MAX
} GOLineDashType;

GOLineDashType	 go_line_dash_from_str		(char const *name);
char const 	*go_line_dash_as_str		(GOLineDashType type);

void 		 go_line_vpath_dash_free	(ArtVpathDash *dash);
ArtVpathDash 	*go_line_get_vpath_dash 	(GOLineDashType type, double scale);

ArtVpath 	*go_line_clip_vpath		(ArtVpath const *path, GogViewAllocation const *bbox);
ArtVpath 	*go_line_dash_vpath 		(ArtVpath const *path, ArtVpathDash const *dash, 
						 GogViewAllocation const *bbox);

gpointer	 go_line_dash_selector		(GOLineDashType default_type);

ArtBpath *go_line_build_bpath (double const *x, double const *y, int n);
ArtVpath *go_line_build_vpath (double const *x, double const *y, int n);

G_END_DECLS

#endif /* GO_LINE_H */
