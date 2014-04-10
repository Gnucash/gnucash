/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-renderer-cairo.c :
 *
 * Copyright (C) 2005 Emmanuel Pacaud (emmanuel.pacaud@univ-poitiers.fr)
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

/* TODO:
 *
 * 	- implement stretched image texture
 * 	- fix alpha channel of source image for texture
 * 	- fix font size
 * 	- implement mutiline text
 * 	- cache font properties
 * 	- implement rendering of marker with cairo 
 * 	  (that will fix grayish outline bug)
 */

#include <goffice/goffice-config.h>
#include <goffice/graph/gog-graph-impl.h>
#include <goffice/graph/gog-renderer-cairo.h>
#include <goffice/graph/gog-renderer-impl.h>
#include <goffice/graph/gog-style.h>
#include <goffice/graph/gog-view.h>
#include <goffice/utils/go-color.h>
#include <goffice/utils/go-font.h>
#include <goffice/utils/go-marker.h>
#include <goffice/utils/go-units.h>
#include <goffice/utils/go-math.h>

#include <libart_lgpl/art_render_gradient.h>
#include <libart_lgpl/art_render_svp.h>
#include <libart_lgpl/art_render_mask.h>
#include <pango/pangoft2.h>
#include <gsf/gsf-impl-utils.h>

#include <cairo/cairo.h>

#include <math.h>

#define DOUBLE_RGBA_R(x) (double)UINT_RGBA_R(x)/255.0
#define DOUBLE_RGBA_G(x) (double)UINT_RGBA_G(x)/255.0
#define DOUBLE_RGBA_B(x) (double)UINT_RGBA_B(x)/255.0
#define DOUBLE_RGBA_A(x) (double)UINT_RGBA_A(x)/255.0

#define GO_COLOR_TO_CAIRO(x) DOUBLE_RGBA_R(x),DOUBLE_RGBA_G(x),DOUBLE_RGBA_B(x),DOUBLE_RGBA_A(x)

struct _GogRendererCairo {
	GogRenderer base;

	int 		 w, h;

	cairo_t		*cairo;
	GdkPixbuf 	*pixbuf;

	cairo_surface_t *marker_surface;
	GdkPixbuf	*marker_pixbuf;
};

typedef GogRendererClass GogRendererCairoClass;

static GObjectClass *parent_klass;

static void
gog_renderer_cairo_finalize (GObject *obj)
{
	GogRendererCairo *crend = GOG_RENDERER_CAIRO (obj);
	cairo_surface_t *surface;

	if (crend->cairo != NULL){
		surface = cairo_get_target (crend->cairo);
		cairo_surface_destroy (surface);
		cairo_destroy (crend->cairo);
		crend->cairo = NULL;
	}
	if (crend->pixbuf != NULL) {
		g_object_unref (crend->pixbuf);
		crend->pixbuf = NULL;
	}
	if (crend->marker_surface != NULL) {
		cairo_surface_destroy (crend->marker_surface);
		g_object_unref (crend->marker_pixbuf);
		crend->marker_surface = NULL;
		crend->marker_pixbuf = NULL;
	}
	
	(*parent_klass->finalize) (obj);
}

static void
gog_renderer_cairo_clip_push (GogRenderer *rend, GogRendererClip *clip)
{
	GogRendererCairo *crend = GOG_RENDERER_CAIRO (rend);
	double x, y;

	/* Rounding clipping area trigger cairo fast clip */
	x = floor (clip->area.x + 0.5);
	y = floor (clip->area.y + 0.5);

	cairo_save (crend->cairo);
	cairo_rectangle (crend->cairo, x, y,
			 floor (clip->area.x + clip->area.w + 0.5) - x,
			 floor (clip->area.y + clip->area.h + 0.5) - y);
	cairo_clip (crend->cairo);
}

static void
gog_renderer_cairo_clip_pop (GogRenderer *rend, GogRendererClip *clip)
{
	GogRendererCairo *crend = GOG_RENDERER_CAIRO (rend);

	cairo_restore (crend->cairo);
}

static double
grc_line_size (GogRenderer const *rend, double width)
{
	if (go_sub_epsilon (width) <= 0.) /* cheesy version of hairline */
		return 1.;
	
	width *= rend->scale;
	if (width <= 1.)
		return width;

	return floor (width);
}

static double
gog_renderer_cairo_line_size (GogRenderer const *rend, double width)
{
	double size = grc_line_size (rend, width);

	if (size < 1.0)
		return ceil (size);

	return size;
}

static void
gog_renderer_cairo_sharp_path (GogRenderer *rend, ArtVpath *path, double line_width) 
{
	ArtVpath *iter = path;

	if (((int) (rint (line_width)) % 2 == 0) && line_width > 1.0) 
		while (iter->code != ART_END) {
			iter->x = floor (iter->x + .5);
			iter->y = floor (iter->y + .5);
			iter++;
		}
	else
		while (iter->code != ART_END) {
			iter->x = floor (iter->x) + .5;
			iter->y = floor (iter->y) + .5;
			iter++;
		}
}

static void
grc_path (cairo_t *cr, ArtVpath *vpath, ArtBpath *bpath)
{
	if (vpath) 
		while (vpath->code != ART_END) {
			switch (vpath->code) {
				case ART_MOVETO_OPEN:
				case ART_MOVETO:
					cairo_move_to (cr, vpath->x, vpath->y);
					break;
				case ART_LINETO:
					cairo_line_to (cr, vpath->x, vpath->y);
					break;
				default:
					break;
			}
			vpath++;
		}
	else
		while (bpath->code != ART_END) {
			switch (bpath->code) {
				case ART_MOVETO_OPEN:
				case ART_MOVETO:
					cairo_move_to (cr, bpath->x3, bpath->y3);
					break;
				case ART_LINETO:
					cairo_line_to (cr, bpath->x3, bpath->y3);
					break;
				case ART_CURVETO:
					cairo_curve_to (cr, 
						       bpath->x1, bpath->y1,
						       bpath->x2, bpath->y2,
						       bpath->x3, bpath->y3);
					break;
				default:
					break;
			}
			bpath++;
		}
}

/* Red and blue are inverted in a pixbuf compared to cairo */
static void
grc_invert_pixbuf_RB (unsigned char *pixels, int width, int height, int rowstride)
{
	int i,j;
	unsigned char a;
	
	for (i = 0; i < height; i++) {
		for (j = 0; j < width; j++) {
			a = pixels[0];
			pixels[0] = pixels[2];
			pixels[2] = a;
			pixels += 4;
		}
		pixels += rowstride - width * 4;
	}
}

static void
grc_draw_path (GogRenderer *rend, ArtVpath const *vpath, ArtBpath const*bpath)
{
	GogRendererCairo *crend = GOG_RENDERER_CAIRO (rend);
	GogStyle const *style = rend->cur_style;
	cairo_t *cr = crend->cairo;
	double width = grc_line_size (rend, style->line.width);

	g_return_if_fail (bpath != NULL || vpath != NULL);

	cairo_set_line_width (cr, width);
	if (rend->line_dash != NULL)
		cairo_set_dash (cr, 
				rend->line_dash->dash, 
				rend->line_dash->n_dash, 
				rend->line_dash->offset);
	grc_path (cr, (ArtVpath *) vpath, (ArtBpath *) bpath);
	cairo_set_source_rgba (cr, GO_COLOR_TO_CAIRO (style->line.color));
	cairo_stroke (cr);
}

static void
gog_renderer_cairo_draw_path (GogRenderer *rend, ArtVpath const *path)
{
	grc_draw_path (rend, path, NULL);
}
  
static void
gog_renderer_cairo_draw_bezier_path (GogRenderer *rend, ArtBpath const *path)
{
	grc_draw_path (rend, NULL, path);
}

static void
grc_draw_polygon (GogRenderer *rend, ArtVpath const *vpath, 
		  ArtBpath const *bpath, gboolean narrow)
{
	struct { unsigned x0i, y0i, x1i, y1i; } const grad_i[GO_GRADIENT_MAX] = {
		{0, 0, 0, 1},
		{0, 1, 0, 0},
		{0, 0, 0, 2},
		{0, 2, 0, 1},
		{0, 0, 1, 0},
		{1, 0, 0, 0},
		{0, 0, 2, 0},
		{2, 0, 1, 0},
		{0, 0, 1, 1},
		{1, 1, 0, 0},
		{0, 0, 2, 2},
		{2, 2, 1, 1},
		{1, 0, 0, 1},
		{0, 1, 1, 0},
		{1, 0, 2, 2},
		{2, 2, 0, 1}
	};
	
	GogRendererCairo *crend = GOG_RENDERER_CAIRO (rend);
	GogStyle const *style = rend->cur_style;
	cairo_t *cr = crend->cairo;
	cairo_pattern_t *cr_pattern = NULL;
	cairo_surface_t *cr_surface = NULL;
	GdkPixbuf *pixbuf = NULL;
	GOColor color;
	double width = grc_line_size (rend, style->line.width);
	double x[3], y[3];
	int i, j, w, h, rowstride;
	guint8 const *pattern;
	unsigned char *pixels, *iter;

	g_return_if_fail (bpath != NULL || vpath != NULL);

	narrow = narrow || (style->outline.dash_type == GO_LINE_NONE);

	if (narrow && style->fill.type == GOG_FILL_STYLE_NONE)
		return;

	cairo_set_line_width (cr, width);
	grc_path (cr, (ArtVpath *) vpath, (ArtBpath *) bpath);

	switch (style->fill.type) {
		case GOG_FILL_STYLE_PATTERN:
			if (go_pattern_is_solid (&style->fill.pattern, &color))
				cairo_set_source_rgba (cr, GO_COLOR_TO_CAIRO (color)); 
			else {
				GOColor fore = style->fill.pattern.fore;
				GOColor back = style->fill.pattern.back;
				int rowstride;

				pattern = go_pattern_get_pattern (&style->fill.pattern);
				pixbuf = gdk_pixbuf_new (GDK_COLORSPACE_RGB, TRUE, 8, 8, 8);
				iter = gdk_pixbuf_get_pixels (pixbuf);
				rowstride = gdk_pixbuf_get_rowstride (pixbuf);
				cr_surface = cairo_image_surface_create_for_data ( iter, 
					CAIRO_FORMAT_ARGB32, 8, 8, rowstride);
				for (i = 0; i < 8; i++) {
					for (j = 0; j < 8; j++) {
						color = pattern[i] & (1 << j) ? fore : back;
						iter[0] = UINT_RGBA_B (color);
						iter[1] = UINT_RGBA_G (color);
						iter[2] = UINT_RGBA_R (color);
						iter[3] = UINT_RGBA_A (color);
						iter += 4;
					}
					iter += rowstride - 32;
				}
				cr_pattern = cairo_pattern_create_for_surface (cr_surface);
				cairo_pattern_set_extend (cr_pattern, CAIRO_EXTEND_REPEAT);
				cairo_set_source (cr, cr_pattern);
			}
			break;

		case GOG_FILL_STYLE_GRADIENT:
			cairo_fill_extents (cr, &x[0], &y[0], &x[1], &y[1]); 
			x[2] = (x[1] - x[0]) / 2.0 + x[1];
			y[2] = (y[1] - y[0]) / 2.0 + y[1];
			cr_pattern = cairo_pattern_create_linear (
				x[grad_i[style->fill.gradient.dir].x0i],
				y[grad_i[style->fill.gradient.dir].y0i],
				x[grad_i[style->fill.gradient.dir].x1i],
				y[grad_i[style->fill.gradient.dir].y1i]);
			cairo_pattern_set_extend (cr_pattern, CAIRO_EXTEND_REFLECT);
			cairo_pattern_add_color_stop_rgba (cr_pattern, 0,
				GO_COLOR_TO_CAIRO (style->fill.pattern.back)); 
			cairo_pattern_add_color_stop_rgba (cr_pattern, 1,
				GO_COLOR_TO_CAIRO (style->fill.pattern.fore));
			cairo_set_source (cr, cr_pattern);
			break;

		case GOG_FILL_STYLE_IMAGE: 
			if (style->fill.image.image == NULL) {
				cairo_set_source_rgba (cr, 1, 1, 1, 1); 
				break;
			}
			pixbuf = gdk_pixbuf_add_alpha (style->fill.image.image, FALSE, 0, 0, 0);
			pixels = gdk_pixbuf_get_pixels (pixbuf);
			h = gdk_pixbuf_get_height (pixbuf);
			w = gdk_pixbuf_get_width (pixbuf);
			rowstride = gdk_pixbuf_get_rowstride (pixbuf);
			cr_surface = cairo_image_surface_create_for_data (pixels,
				CAIRO_FORMAT_ARGB32, w, h, rowstride);
			grc_invert_pixbuf_RB (pixels, w, h, rowstride);
			cr_pattern = cairo_pattern_create_for_surface (cr_surface);
			cairo_pattern_set_extend (cr_pattern, CAIRO_EXTEND_REPEAT);
			cairo_set_source (cr, cr_pattern);
			break;

		case GOG_FILL_STYLE_NONE:
			break; /* impossible */
	}

	if (style->fill.type != GOG_FILL_STYLE_NONE) {
		if (!narrow) 
			cairo_fill_preserve (cr);
		else 
			cairo_fill (cr);
	} 

	if (!narrow) {
		cairo_set_source_rgba (cr, GO_COLOR_TO_CAIRO (style->outline.color));
		if (rend->outline_dash != NULL)
			cairo_set_dash (cr, 
					rend->outline_dash->dash, 
					rend->outline_dash->n_dash, 
					rend->outline_dash->offset);
		cairo_stroke (cr);
	}

	if (cr_pattern != NULL)
		cairo_pattern_destroy (cr_pattern);
	if (cr_surface != NULL)
		cairo_surface_destroy (cr_surface);
	if (pixbuf)
		g_object_unref (pixbuf);
}

static void
gog_renderer_cairo_draw_polygon (GogRenderer *rend, ArtVpath const *path, 
				 gboolean narrow)
{
	grc_draw_polygon (rend, path, NULL, narrow);
}

static void
gog_renderer_cairo_draw_bezier_polygon (GogRenderer *rend, ArtBpath const *path,
					 gboolean narrow)
{
	grc_draw_polygon (rend, NULL, path, narrow);
}

static void
gog_renderer_cairo_draw_text (GogRenderer *rend, char const *text,
			      GogViewAllocation const *pos, GtkAnchorType anchor,
			      GogViewAllocation *result)
{
	GogRendererCairo *crend = (GogRendererCairo *) rend;
	GogStyle const *style = rend->cur_style;
	PangoFontDescription const *fd = style->font.font->desc;
	PangoWeight weight;
	cairo_t *cr = crend->cairo;
	cairo_text_extents_t text_extents;
	cairo_font_extents_t font_extents;
	cairo_font_slant_t slant;
	GOGeometryOBR obr;
	GOGeometryAABR aabr;
	char const *family;
	double size;

	family = pango_font_description_get_family (fd);
	size = pango_font_description_get_size (fd) / PANGO_SCALE;
	weight = pango_font_description_get_weight (fd);
	switch (pango_font_description_get_style (fd)) {
		case (PANGO_STYLE_NORMAL):  slant = CAIRO_FONT_SLANT_NORMAL; break;
		case (PANGO_STYLE_OBLIQUE): slant = CAIRO_FONT_SLANT_OBLIQUE; break;
		case (PANGO_STYLE_ITALIC):  slant = CAIRO_FONT_SLANT_ITALIC; break;
	}
/*	g_message ("family: %s, size: %g", family, size);*/
	/* FIXME: calculate dpi */
	size *= 96.0 * rend->scale * rend->zoom / 72.0;
	cairo_select_font_face (cr, family, slant, 
		weight > PANGO_WEIGHT_SEMIBOLD ?  CAIRO_FONT_WEIGHT_BOLD : CAIRO_FONT_WEIGHT_NORMAL);
	cairo_set_font_size (cr, size);
	cairo_text_extents (cr, text, &text_extents);
	cairo_font_extents (cr, &font_extents);
	
	obr.w = text_extents.width;
	obr.h = font_extents.ascent + font_extents.descent;
	obr.alpha = rend->cur_style->text_layout.angle * M_PI / 180.0;
	obr.x = pos->x;
	obr.y = pos->y;
	go_geometry_OBR_to_AABR (&obr, &aabr);

	switch (anchor) {
		case GTK_ANCHOR_NW: case GTK_ANCHOR_W: case GTK_ANCHOR_SW:
			obr.x += aabr.w / 2.0;
			break;
		case GTK_ANCHOR_NE : case GTK_ANCHOR_SE : case GTK_ANCHOR_E :
			obr.x -= aabr.w / 2.0;
			break;
		default : break;
	}
	if (obr.x <= 0)
		obr.x = 0;

	switch (anchor) {
		case GTK_ANCHOR_NW: case GTK_ANCHOR_N: case GTK_ANCHOR_NE:
			obr.y += aabr.h / 2.0;
			break;
		case GTK_ANCHOR_SE : case GTK_ANCHOR_S : case GTK_ANCHOR_SW :
			obr.y -= aabr.h / 2.0;
			break;
		default : break;
	}
	if (obr.y <= 0)
		obr.y = 0;

	cairo_save (cr);
	cairo_set_source_rgba (cr, GO_COLOR_TO_CAIRO (style->font.color)); 
	cairo_move_to (cr, obr.x, obr.y);
	cairo_rotate (cr, -obr.alpha);
	cairo_rel_move_to (cr, - obr.w / 2.0, + obr.h / 2.0 - font_extents.descent);
	cairo_show_text (cr, text);
	cairo_restore (cr);

	if (result != NULL) {
		result->x = aabr.x;
		result->y = aabr.y;
		result->w = aabr.w;
		result->h = aabr.h;
	}
}

static void
gog_renderer_cairo_get_text_OBR (GogRenderer *rend,
				 char const *text, GOGeometryOBR *obr)
{
	GogRendererCairo *crend = (GogRendererCairo *) rend;
	GogStyle const *style = rend->cur_style;
	PangoFontDescription const *fd = style->font.font->desc;
	PangoWeight weight;
	cairo_t *cr = crend->cairo;
	cairo_text_extents_t text_extents;
	cairo_font_extents_t font_extents;
	cairo_font_slant_t slant;
	char const *family;
	double size;
	
	family = pango_font_description_get_family (fd);
	/* FIXME: calculate dpi */
	size = pango_font_description_get_size (fd) / PANGO_SCALE / 72.0 * 96.0 * rend->scale * rend->zoom;
	weight = pango_font_description_get_weight (fd);
	switch (pango_font_description_get_style (fd)) {
		case (PANGO_STYLE_NORMAL):  slant = CAIRO_FONT_SLANT_NORMAL; break;
		case (PANGO_STYLE_OBLIQUE): slant = CAIRO_FONT_SLANT_OBLIQUE; break;
		case (PANGO_STYLE_ITALIC):  slant = CAIRO_FONT_SLANT_ITALIC; break;
	}
	cairo_select_font_face (cr, family, slant, 
		weight > PANGO_WEIGHT_SEMIBOLD ?  CAIRO_FONT_WEIGHT_BOLD : CAIRO_FONT_WEIGHT_NORMAL);
	cairo_set_font_size (cr, size);
	cairo_text_extents (cr, text, &text_extents);
	cairo_font_extents (cr, &font_extents);

	obr->w = text_extents.width;
	obr->h = font_extents.ascent + font_extents.descent;
}

static cairo_surface_t *
grc_get_marker_surface (GogRenderer *rend)
{
	GogRendererCairo *crend = GOG_RENDERER_CAIRO (rend);
	GogStyle const *style = rend->cur_style;
	GdkPixbuf *pixbuf, *marker_pixbuf;
	cairo_surface_t *surface;
	unsigned char *pixels;
	int height, width, rowstride;

	if (crend->marker_surface != NULL)
		return crend->marker_surface;
       	
	marker_pixbuf = go_marker_get_pixbuf (style->marker.mark, rend->scale);
	if (marker_pixbuf == NULL)
		return NULL;

	pixbuf = gdk_pixbuf_copy (go_marker_get_pixbuf (style->marker.mark, rend->scale));
       	pixels = gdk_pixbuf_get_pixels (pixbuf);
	height = gdk_pixbuf_get_height (pixbuf);
	width = gdk_pixbuf_get_width (pixbuf);
	rowstride = gdk_pixbuf_get_rowstride (pixbuf);
	surface = cairo_image_surface_create_for_data (pixels,
		CAIRO_FORMAT_ARGB32, width, height, rowstride);
	grc_invert_pixbuf_RB (pixels, width, height, rowstride);

	crend->marker_pixbuf = pixbuf;
	crend->marker_surface = surface;

	return surface;
}

static void
gog_renderer_cairo_draw_marker (GogRenderer *rend, double x, double y)
{
	GogRendererCairo *crend = GOG_RENDERER_CAIRO (rend);
	cairo_surface_t *surface;
	double width, height;

	surface = grc_get_marker_surface (rend);
	if (surface == NULL)
		return;
	width = cairo_image_surface_get_width (surface);
	height = cairo_image_surface_get_height (surface);
	cairo_set_source_surface (crend->cairo, surface, 
				  floor (floor (x + .5) - width / 2.0),
				  floor (floor (y + .5) - height / 2.0));
	cairo_paint (crend->cairo);
}

static void
gog_renderer_cairo_push_style (GogRenderer *rend, GogStyle const *style)
{
}

static void
gog_renderer_cairo_pop_style (GogRenderer *rend)
{
	GogRendererCairo *crend = GOG_RENDERER_CAIRO (rend);
	
	if (crend->marker_surface != NULL) {
		cairo_surface_destroy (crend->marker_surface);
		g_object_unref (crend->marker_pixbuf);
		crend->marker_surface = NULL;
		crend->marker_pixbuf = NULL;
	}
}

static void
gog_renderer_cairo_class_init (GogRendererClass *rend_klass)
{
	GObjectClass *gobject_klass   = (GObjectClass *) rend_klass;

	parent_klass = g_type_class_peek_parent (rend_klass);
	gobject_klass->finalize		= gog_renderer_cairo_finalize;
	rend_klass->push_style		= gog_renderer_cairo_push_style;
	rend_klass->pop_style		= gog_renderer_cairo_pop_style;
	rend_klass->clip_push  		= gog_renderer_cairo_clip_push;
	rend_klass->clip_pop     	= gog_renderer_cairo_clip_pop;
	rend_klass->sharp_path		= gog_renderer_cairo_sharp_path;
	rend_klass->draw_path	  	= gog_renderer_cairo_draw_path;
	rend_klass->draw_polygon  	= gog_renderer_cairo_draw_polygon;
	rend_klass->draw_bezier_path 	= gog_renderer_cairo_draw_bezier_path;
	rend_klass->draw_bezier_polygon = gog_renderer_cairo_draw_bezier_polygon;
	rend_klass->draw_text	  	= gog_renderer_cairo_draw_text;
	rend_klass->draw_marker	  	= gog_renderer_cairo_draw_marker;
	rend_klass->get_text_OBR	= gog_renderer_cairo_get_text_OBR;
	rend_klass->line_size		= gog_renderer_cairo_line_size;
}

static void
gog_renderer_cairo_init (GogRendererCairo *crend)
{
	crend->cairo = NULL;
	crend->pixbuf = NULL;
	crend->marker_surface = NULL;
	crend->marker_pixbuf = NULL;
	crend->w = crend->h = 0;
}

GSF_CLASS (GogRendererCairo, gog_renderer_cairo,
	   gog_renderer_cairo_class_init, gog_renderer_cairo_init,
	   GOG_RENDERER_TYPE)

GdkPixbuf *
gog_renderer_cairo_get_pixbuf (GogRendererCairo *crend)
{
	g_return_val_if_fail (crend != NULL, NULL);

	return crend->pixbuf;
}

static gboolean
grc_cairo_setup (GogRendererCairo *crend, int w, int h)
{
	cairo_surface_t *surface;

	if (w == crend->w && h == crend->h)
		return (w != 0 && h!= 0);
		
	if (crend->cairo != NULL) {
		surface = cairo_get_target (crend->cairo);
		cairo_surface_destroy (surface);
		cairo_destroy (crend->cairo);
		crend->cairo = NULL;
	}
	if (crend->pixbuf != NULL) {
		g_object_unref (crend->pixbuf);
		crend->pixbuf = NULL;
	}
	crend->w = w;
	crend->h = h;

	if (w ==0 || h == 0) 
		return FALSE;
	
	crend->pixbuf = gdk_pixbuf_new (GDK_COLORSPACE_RGB, TRUE, 8, crend->w, crend->h);
	if (crend->pixbuf == NULL) {
		g_warning ("GogRendererCairo::cairo_setup: chart is too large");
		return FALSE;
	}

	surface = cairo_image_surface_create_for_data (gdk_pixbuf_get_pixels (crend->pixbuf),
						       CAIRO_FORMAT_ARGB32, 
						       crend->w, crend->h,
						       gdk_pixbuf_get_rowstride (crend->pixbuf));
	crend->cairo = cairo_create (surface);
	cairo_set_line_join (crend->cairo, CAIRO_LINE_JOIN_ROUND);
	cairo_set_line_cap (crend->cairo, CAIRO_LINE_CAP_ROUND);

	return TRUE;
}

/**
 * gog_renderer_update :
 * @prend :
 * @w :
 * @h :
 *
 * Returns TRUE if the size actually changed.
 **/
gboolean
gog_renderer_cairo_update (GogRendererCairo *crend, int w, int h, double zoom)
{
	GogGraph *graph;
	GogView *view;
	GogViewAllocation allocation;
	gboolean redraw = TRUE;
	gboolean size_changed;

	g_return_val_if_fail (crend != NULL, FALSE);
	g_return_val_if_fail (crend->base.view != NULL, FALSE);

	size_changed = crend->w != w || crend->h != h;

	view = crend->base.view;
	graph = GOG_GRAPH (view->model);
	gog_graph_force_update (graph);
	allocation.x = allocation.y = 0.;
	allocation.w = w;
	allocation.h = h;
	if (!grc_cairo_setup (crend, w, h))
		return redraw;

	if (size_changed) {
		crend->base.scale_x = w / graph->width;
		crend->base.scale_y = h / graph->height;
		crend->base.scale = MIN (crend->base.scale_x, crend->base.scale_y);
		crend->base.zoom  = zoom;

		/* make sure we dont try to queue an update while updating */
		crend->base.needs_update = TRUE;

		/* scale just changed need to recalculate sizes */
		gog_renderer_invalidate_size_requests (&crend->base);
		gog_view_size_allocate (view, &allocation);
	} else 
		if (w != view->allocation.w || h != view->allocation.h)
			gog_view_size_allocate (view, &allocation);
		else
			redraw = gog_view_update_sizes (view);

	redraw |= crend->base.needs_update;
	crend->base.needs_update = FALSE;

	if (redraw) { 
		cairo_rectangle (crend->cairo, 0, 0, w, h);
		cairo_set_source_rgba (crend->cairo, 1, 1, 1, 0);
		cairo_fill (crend->cairo);
		
		gog_view_render	(view, NULL);
		
		grc_invert_pixbuf_RB (gdk_pixbuf_get_pixels (crend->pixbuf), w, h, 
				      gdk_pixbuf_get_rowstride (crend->pixbuf));
	}

	return redraw;
}
