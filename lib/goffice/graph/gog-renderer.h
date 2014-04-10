/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-renderer.h : An abstract interface for rendering engines
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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
 * USA
 */
#ifndef GOG_RENDERER_H
#define GOG_RENDERER_H

#include <goffice/graph/goffice-graph.h>
#include <gtk/gtkenums.h>
#include <libart_lgpl/libart.h>
#include <gdk/gdk.h>

#define GOG_RENDERER_TYPE	  (gog_renderer_get_type ())
#define GOG_RENDERER(o)           (G_TYPE_CHECK_INSTANCE_CAST((o), GOG_RENDERER_TYPE, GogRenderer))
#define IS_GOG_RENDERER(o)        (G_TYPE_CHECK_INSTANCE_TYPE((o), GOG_RENDERER_TYPE))
#define GOG_RENDERER_GET_CLASS(o) (G_TYPE_INSTANCE_GET_CLASS((o), GOG_RENDERER_TYPE, GogRendererClass))

GType gog_renderer_get_type            (void); 

void  gog_renderer_request_update (GogRenderer *r);

void  gog_renderer_push_style     (GogRenderer *r, GogStyle const *style);
void  gog_renderer_pop_style      (GogRenderer *r);

void  gog_renderer_clip_push 	  (GogRenderer *r, GogViewAllocation const *region);
void  gog_renderer_clip_pop       (GogRenderer *r);

void  gog_renderer_draw_sharp_path	(GogRenderer *r, ArtVpath *path,
					 GogViewAllocation const *bound);
void  gog_renderer_draw_sharp_polygon   (GogRenderer *r, ArtVpath *path,
					 gboolean narrow, GogViewAllocation const *bound);
void  gog_renderer_draw_sharp_rectangle (GogRenderer *r, GogViewAllocation const *rect,
					 GogViewAllocation const *bound);

void  gog_renderer_draw_path      (GogRenderer *r, ArtVpath const *path,
				   GogViewAllocation const *bound);
void  gog_renderer_draw_polygon   (GogRenderer *r, ArtVpath const *path,
				   gboolean narrow, GogViewAllocation const *bound);
void  gog_renderer_draw_rectangle (GogRenderer *r, GogViewAllocation const *rect,
				   GogViewAllocation const *bound);

void  gog_renderer_draw_text	  (GogRenderer *rend, char const *text,
				   GogViewAllocation const *pos, GtkAnchorType anchor,
				   GogViewAllocation *result);
void  gog_renderer_draw_marker	  (GogRenderer *rend, double x, double y);
void  gog_renderer_measure_text	  (GogRenderer *rend,
				   char const *text, GogViewRequisition *size);

/* measurement */
double gog_renderer_line_size	  	(GogRenderer const *r, double width);
double gog_renderer_pt2r_x   	  	(GogRenderer const *r, double d);
double gog_renderer_pt2r_y   	  	(GogRenderer const *r, double d);
double gog_renderer_pt2r   	  	(GogRenderer const *r, double d);

#endif /* GOG_RENDERER_H */
