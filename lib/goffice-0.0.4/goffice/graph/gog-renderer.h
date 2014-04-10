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
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301
 * USA
 */
#ifndef GOG_RENDERER_H
#define GOG_RENDERER_H

#include <goffice/graph/goffice-graph.h>
#include <goffice/utils/go-geometry.h>
#include <gtk/gtkenums.h>
#include <libart_lgpl/libart.h>
#include <gdk/gdk.h>

G_BEGIN_DECLS

#define GOG_RENDERER_TYPE	  (gog_renderer_get_type ())
#define GOG_RENDERER(o)           (G_TYPE_CHECK_INSTANCE_CAST((o), GOG_RENDERER_TYPE, GogRenderer))
#define IS_GOG_RENDERER(o)        (G_TYPE_CHECK_INSTANCE_TYPE((o), GOG_RENDERER_TYPE))
#define GOG_RENDERER_GET_CLASS(o) (G_TYPE_INSTANCE_GET_CLASS((o), GOG_RENDERER_TYPE, GogRendererClass))

GType gog_renderer_get_type            (void); 

void  gog_renderer_request_update (GogRenderer *r);

void  gog_renderer_push_style     (GogRenderer *r, GogStyle const *style);
void  gog_renderer_pop_style      (GogRenderer *r);

void  gog_renderer_push_clip 	  (GogRenderer *r, ArtVpath *path);
void  gog_renderer_pop_clip       (GogRenderer *r);

ArtVpath * gog_renderer_get_rectangle_vpath 	(GogViewAllocation const *rect);
ArtBpath * gog_renderer_get_ring_wedge_bpath	(double cx, double cy, 
						 double rx_out, double ry_out,
						 double rx_in, double ry_in,
						 double th0, double th1);

void  gog_renderer_draw_sharp_path	(GogRenderer *r, ArtVpath *path);
void  gog_renderer_draw_sharp_polygon   (GogRenderer *r, ArtVpath *path, gboolean narrow);
void  gog_renderer_draw_sharp_rectangle (GogRenderer *r, GogViewAllocation const *rect);

void  gog_renderer_draw_ring_wedge  	(GogRenderer *r, double cx, double cy, 
					 double rx_out, double ry_out,
					 double rx_in, double ry_in,
					 double th0, double th1, gboolean narrow);
void  gog_renderer_draw_path      	(GogRenderer *r, ArtVpath const *path);
void  gog_renderer_draw_polygon   	(GogRenderer *r, ArtVpath const *path, gboolean narrow);
void  gog_renderer_draw_rectangle 	(GogRenderer *r, GogViewAllocation const *rect);
void  gog_renderer_draw_bezier_path     (GogRenderer *r, ArtBpath const *path);

void  gog_renderer_draw_text	  (GogRenderer *rend, char const *text,
				   GogViewAllocation const *pos, GtkAnchorType anchor,
				   GogViewAllocation *result);
void  gog_renderer_draw_marker	  (GogRenderer *rend, double x, double y);

void  gog_renderer_get_text_OBR   (GogRenderer *rend, char const *text, GOGeometryOBR *obr);
void  gog_renderer_get_text_AABR  (GogRenderer *rend, char const *text, GOGeometryAABR *aabr);

#define gog_renderer_draw_arc(r,cx,cy,rx,ry,th0,th1) \
	gog_renderer_draw_ring_wedge (r,cx,cy,rx,ry,-1.,-1.,th0,th1,FALSE)
#define gog_renderer_draw_pie_wedge(r,cx,cy,rx,ry,th0,th1,narrow) \
	gog_renderer_draw_ring_wedge (r,cx,cy,rx,ry,0.,0.,th0,th1,narrow)

/* measurement */
double gog_renderer_line_size	  	(GogRenderer const *r, double width);
double gog_renderer_pt2r_x   	  	(GogRenderer const *r, double d);
double gog_renderer_pt2r_y   	  	(GogRenderer const *r, double d);
double gog_renderer_pt2r   	  	(GogRenderer const *r, double d);

G_END_DECLS

#endif /* GOG_RENDERER_H */
