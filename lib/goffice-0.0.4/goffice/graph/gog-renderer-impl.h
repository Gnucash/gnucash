/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-renderer-impl.h :
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

#ifndef GOG_RENDERER_IMPL_H
#define GOG_RENDERER_IMPL_H

#include <goffice/graph/goffice-graph.h>
#include <goffice/utils/goffice-utils.h>
#include <goffice/utils/go-line.h>
#include <goffice/graph/gog-renderer.h>

G_BEGIN_DECLS

typedef struct {
	ArtVpath *path;
	gpointer data;
} GogRendererClip;

struct _GogRenderer {
	GObject	 base;

	GogGraph *model;
	GogView	 *view;
	float	  scale, scale_x, scale_y;
	float	  zoom;
	
	GogRendererClip const *cur_clip;
	GSList	  *clip_stack;

	GClosure *font_watcher;
	gboolean  needs_update;

	GogStyle const *cur_style;
	GSList   *style_stack;

	ArtVpathDash *line_dash;
	ArtVpathDash *outline_dash;
};

typedef struct {
	GObjectClass base;

	/* Virtuals */
	void (*font_removed)	(GogRenderer *renderer, GOFont const *font);

	void (*push_style)     	(GogRenderer *renderer, GogStyle const *style);
	void (*pop_style)      	(GogRenderer *renderer);

 	void (*push_clip)	(GogRenderer *renderer, GogRendererClip *clip);
 	void (*pop_clip) 	(GogRenderer *renderer, GogRendererClip *clip);
 	
	void (*sharp_path)	(GogRenderer *renderer, ArtVpath *path, double line_width);

	void (*draw_path)      	(GogRenderer *renderer, ArtVpath const *path);
	void (*draw_polygon)   	(GogRenderer *renderer, ArtVpath const *path, gboolean narrow);
	
	void (*draw_bezier_path)	(GogRenderer *renderer, ArtBpath const *path);
	void (*draw_bezier_polygon)    	(GogRenderer *renderer, ArtBpath const *path, gboolean narrow);
	
	void (*draw_text)      	(GogRenderer *rend, char const *text,
				 GogViewAllocation const *pos, GtkAnchorType anchor,
				 GogViewAllocation *result);
	void (*draw_marker)    	(GogRenderer *rend, double x, double y);
	
	void (*get_text_OBR)	(GogRenderer *rend, char const *text, GOGeometryOBR *obr);
	
	double (*line_size)		(GogRenderer const *rend, double width);

	/* Signals */
	void (*request_update) (GogRenderer *renderer);
} GogRendererClass;

#define GOG_RENDERER_CLASS(k)	 (G_TYPE_CHECK_CLASS_CAST ((k), GOG_RENDERER_TYPE, GogRendererClass))
#define IS_GOG_RENDERER_CLASS(k) (G_TYPE_CHECK_CLASS_TYPE ((k), GOG_RENDERER_TYPE))

/* protected */
void gog_renderer_invalidate_size_requests (GogRenderer *rend);

G_END_DECLS

#endif /* GOG_RENDERER_GROUP_IMPL_H */
