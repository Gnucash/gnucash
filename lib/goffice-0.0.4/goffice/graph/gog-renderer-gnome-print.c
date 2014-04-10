/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-renderer-gnome-print.c :
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

#include <goffice/goffice-config.h>
#include <goffice/graph/gog-graph-impl.h>
#include <goffice/graph/gog-renderer-gnome-print.h>
#include <goffice/graph/gog-renderer-impl.h>
#include <goffice/graph/gog-style.h>
#include <goffice/graph/gog-view.h>
#include <goffice/utils/go-color.h>
#include <goffice/utils/go-units.h>
#include <goffice/utils/go-font.h>
#include <goffice/utils/go-marker.h>

#include <gsf/gsf-impl-utils.h>

#include <libart_lgpl/art_render_gradient.h>
#include <libart_lgpl/art_render_svp.h>

#include <math.h>
#include <string.h>

#include <libgnomeprint/gnome-print-pango.h>

#define GOG_RENDERER_GNOME_PRINT_TYPE	(gog_renderer_gnome_print_get_type ())
#define GOG_RENDERER_GNOME_PRINT(o)	(G_TYPE_CHECK_INSTANCE_CAST ((o), GOG_RENDERER_GNOME_PRINT_TYPE, GogRendererGnomePrint))
#define IS_GOG_RENDERER_GNOME_PRINT(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOG_RENDERER_GNOME_PRINT_TYPE))

typedef struct _GogRendererGnomePrint GogRendererGnomePrint;

struct _GogRendererGnomePrint {
	GogRenderer base;

	GPtrArray *fonts;
	GnomePrintContext *gp_context;
	PangoLayout *layout;

	GogViewAllocation bbox;
};

typedef GogRendererClass GogRendererGnomePrintClass;

static GObjectClass *parent_klass;

static GType gog_renderer_gnome_print_get_type (void);

static void
gog_renderer_gnome_print_finalize (GObject *obj)
{
	GogRendererGnomePrint *prend = GOG_RENDERER_GNOME_PRINT (obj);

	if (prend->gp_context != NULL) {
		g_object_unref (prend->gp_context);
		prend->gp_context = NULL;
	}

	if (prend->layout) {
		g_object_unref (prend->layout);
		prend->layout = NULL;
	}

	if (prend->fonts != NULL) {
		int i;
		for (i = prend->fonts->len; i-- > 0 ; ) {
			PangoFontDescription *fd = g_ptr_array_index (prend->fonts, i);
			if (fd != NULL)
				pango_font_description_free (fd);
		}

		g_ptr_array_free (prend->fonts, TRUE);
		prend->fonts = NULL;
	}

	(*parent_klass->finalize) (obj);
}

static PangoFontDescription *
get_font (GogRendererGnomePrint *prend, GOFont const *gf)
{
	PangoFontDescription *res = NULL;

	if (gf->font_index < (int)prend->fonts->len)
		res = g_ptr_array_index (prend->fonts, gf->font_index);
	else
		g_ptr_array_set_size (prend->fonts, gf->font_index + 1);

	if (res == NULL) {
		res = pango_font_description_copy (gf->desc);
		/* FIXME: Why do we do this?  */
		pango_font_description_set_size (res,
						 prend->base.zoom *
						 pango_font_description_get_size (res));
		g_ptr_array_index (prend->fonts, gf->font_index) = res;
	}

	return res;
}

static void
set_color (GogRendererGnomePrint *prend, GOColor color)
{
	double r = ((double) UINT_RGBA_R (color)) / 255.;
	double g = ((double) UINT_RGBA_G (color)) / 255.;
	double b = ((double) UINT_RGBA_B (color)) / 255.;
	double a = ((double) UINT_RGBA_A (color)) / 255.;
	gnome_print_setrgbcolor (prend->gp_context, r, g, b);
	gnome_print_setopacity (prend->gp_context, a);
}

static void
draw_path (GogRendererGnomePrint *prend, ArtVpath const *path)
{
	gnome_print_newpath (prend->gp_context);
	for ( ; path->code != ART_END ; path++)
		switch (path->code) {
		case ART_MOVETO_OPEN :
		case ART_MOVETO :
			gnome_print_moveto (prend->gp_context,
					    path->x, -path->y);
			break;
		case ART_LINETO :
			gnome_print_lineto (prend->gp_context,
					    path->x, -path->y);
			break;
		default :
			break;
		}
}

static void
gog_renderer_gnome_print_push_clip (GogRenderer *rend, GogRendererClip *clip)
{
	GogRendererGnomePrint *prend = GOG_RENDERER_GNOME_PRINT (rend);

	gnome_print_gsave (prend->gp_context);
	draw_path (prend, clip->path);
	gnome_print_clip (prend->gp_context);
}

static void
gog_renderer_gnome_print_pop_clip (GogRenderer *rend, GogRendererClip *clip)
{
	GogRendererGnomePrint *prend = GOG_RENDERER_GNOME_PRINT (rend);

	gnome_print_grestore (prend->gp_context);
}

static void
set_dash (GogRendererGnomePrint *prend, ArtVpathDash *dash)
{
	if (dash == NULL ||
	    dash->n_dash == 0)
		gnome_print_setdash (prend->gp_context, 0, NULL, 0.);
	else
		gnome_print_setdash (prend->gp_context, dash->n_dash, dash->dash, dash->offset);
}

static void
gog_renderer_gnome_print_draw_path (GogRenderer *renderer, ArtVpath const *path)
{
	GogRendererGnomePrint *prend = GOG_RENDERER_GNOME_PRINT (renderer);
	GogStyle const *style = renderer->cur_style;

	if (style->line.dash_type == GO_LINE_NONE)
		return;

	set_color (prend, style->line.color);
	set_dash (prend, renderer->line_dash);
	gnome_print_setlinewidth (prend->gp_context,
		gog_renderer_line_size (renderer, style->line.width));

	if (style->line.dash_type != GO_LINE_SOLID && renderer->cur_clip != NULL) {
		ArtVpath *clipped = go_line_clip_vpath (path, &prend->bbox);
		draw_path (prend, clipped);
		g_free (clipped);
	} else
		draw_path (prend, path);

	gnome_print_stroke (prend->gp_context);
}

static void
print_image (GogRendererGnomePrint *prend, GdkPixbuf *image, int w, int h)
{
	if (gdk_pixbuf_get_has_alpha (image))
		gnome_print_rgbaimage (prend->gp_context,
			gdk_pixbuf_get_pixels (image), w, h,
			gdk_pixbuf_get_rowstride (image));
	else
		gnome_print_rgbimage (prend->gp_context,
			gdk_pixbuf_get_pixels (image), w, h,
			gdk_pixbuf_get_rowstride (image));
}

#define PIXBUF_SIZE 1024
static void
gog_renderer_gnome_print_draw_polygon (GogRenderer *renderer, ArtVpath const *path,
				       gboolean narrow)
{
	GogRendererGnomePrint *prend = GOG_RENDERER_GNOME_PRINT (renderer);
	GogStyle const *style = renderer->cur_style;
	gboolean with_outline = (!narrow && style->outline.dash_type != GO_LINE_NONE);
	GdkPixbuf *image;
	ArtDRect bbox;
	ArtRender *render;
	gint i, j, imax, jmax, w, h, x, y;
	GOColor color;
	ArtGradientLinear gradient;
	ArtGradientStop stops[2];

	if (style->fill.type != GOG_FILL_STYLE_NONE || with_outline) {
		if (style->outline.dash_type != GO_LINE_SOLID && renderer->cur_clip != NULL) {
			ArtVpath *clipped = go_line_clip_vpath (path, &prend->bbox);
			draw_path (prend, clipped);
			g_free (clipped);
			draw_path (prend, path);
		} else
			draw_path (prend, path);
		gnome_print_closepath (prend->gp_context);
	}

	if (style->fill.type != GOG_FILL_STYLE_NONE) {

		art_vpath_bbox_drect (path, &bbox);

		switch (style->fill.type) {
		case GOG_FILL_STYLE_PATTERN:
			gnome_print_gsave (prend->gp_context);
			if (go_pattern_is_solid (&style->fill.pattern, &color)) {
				set_color (prend, color);
				gnome_print_fill (prend->gp_context);
			} else {
				ArtSVP *fill = art_svp_from_vpath ((ArtVpath *)path);
				image = gdk_pixbuf_new (GDK_COLORSPACE_RGB, TRUE, 8, bbox.x1, bbox.y1);
				gdk_pixbuf_fill (image, 0);
				go_pattern_render_svp (&style->fill.pattern,
					fill, 0, 0, bbox.x1, bbox.y1,
					gdk_pixbuf_get_pixels (image),
					gdk_pixbuf_get_rowstride (image));

				gnome_print_translate (prend->gp_context, 0, - bbox.y1);
				gnome_print_scale (prend->gp_context, bbox.x1, bbox.y1);
				gnome_print_rgbaimage (prend->gp_context,
					gdk_pixbuf_get_pixels (image),
					gdk_pixbuf_get_width (image),
					gdk_pixbuf_get_height (image),
					gdk_pixbuf_get_rowstride (image));

				art_free (fill);
				g_object_unref (image);
			}
			gnome_print_grestore (prend->gp_context);
			break;

		case GOG_FILL_STYLE_GRADIENT:
			image = gdk_pixbuf_new (GDK_COLORSPACE_RGB, TRUE, 8, PIXBUF_SIZE, PIXBUF_SIZE);
			gnome_print_gsave (prend->gp_context);
			gnome_print_clip (prend->gp_context);
			render = art_render_new (0, 0, PIXBUF_SIZE, PIXBUF_SIZE,
				gdk_pixbuf_get_pixels (image),
				gdk_pixbuf_get_rowstride (image),
				gdk_pixbuf_get_n_channels (image) - 1,
				8, ART_ALPHA_SEPARATE, NULL);

			go_gradient_setup (&gradient,
					   style->fill.gradient.dir,
					   style->fill.pattern.back, style->fill.pattern.fore,
					   0, 0, PIXBUF_SIZE, PIXBUF_SIZE,
					   stops);
			art_render_gradient_linear (render,
				&gradient, ART_FILTER_NEAREST);
			art_render_invoke (render);
			gnome_print_translate (prend->gp_context, bbox.x0, - bbox.y1);
			gnome_print_scale (prend->gp_context, bbox.x1 - bbox.x0, bbox.y1 - bbox.y0);
			gnome_print_rgbaimage (prend->gp_context,
				gdk_pixbuf_get_pixels (image),
				gdk_pixbuf_get_width (image),
				gdk_pixbuf_get_height (image),
				gdk_pixbuf_get_rowstride (image));
			gnome_print_grestore (prend->gp_context);
			g_object_unref (image);
			break;

		case GOG_FILL_STYLE_IMAGE:
			image = style->fill.image.image;
			if (image == NULL)
				break;
			gnome_print_gsave (prend->gp_context);
			gnome_print_clip (prend->gp_context);
			switch (style->fill.image.type) {
			case GOG_IMAGE_CENTERED:
				w = (bbox.x1 - bbox.x0) - gdk_pixbuf_get_width (image);
				if (w > 0) w /= 2.; else w = 0.;
				h = (bbox.y1 - bbox.y0) - gdk_pixbuf_get_height (image);
				if (h > 0) h /= 2.; else h = 0.;

				gnome_print_translate (prend->gp_context,
					bbox.x0 + w, - bbox.y1 - h);
				print_image (prend, image,
					gdk_pixbuf_get_width (image),
					gdk_pixbuf_get_height (image));
				break;
			case GOG_IMAGE_STRETCHED:
				gnome_print_translate (prend->gp_context, bbox.x0, - bbox.y1);
				gnome_print_scale (prend->gp_context, bbox.x1 - bbox.x0, bbox.y1 - bbox.y0);
				print_image (prend, image,
					gdk_pixbuf_get_width (image),
					gdk_pixbuf_get_height (image));
				break;

			case GOG_IMAGE_WALLPAPER:
				imax = (bbox.x1 - bbox.x0) / (w = gdk_pixbuf_get_width (image));
				jmax = (bbox.y1 - bbox.y0) / (h = gdk_pixbuf_get_height (image));
				x = 0;
				for (i = 0; i < imax; i++) {
					y = 0;
					for (j = 0; j < jmax; j++) {
						gnome_print_gsave (prend->gp_context);
						gnome_print_translate (prend->gp_context,
							bbox.x0 + x,
							- y - h - bbox.y0);
						gnome_print_scale (prend->gp_context, w, h);
						print_image (prend, image, w, h);
						gnome_print_grestore (prend->gp_context);
						y += h;
					}
					gnome_print_gsave (prend->gp_context);
					gnome_print_translate (prend->gp_context,
						bbox.x0 + x,
						- y - (int)(bbox.y1 - bbox.y0) % h - bbox.y0);
					gnome_print_scale (prend->gp_context, w, (int)(bbox.y1 - bbox.y0) % h);
					print_image (prend, image, w, (int)(bbox.y1 - bbox.y0) % h);
					gnome_print_grestore (prend->gp_context);
					x += w;
				}
				y = 0;
				for (j = 0; j < jmax; j++) {
					gnome_print_gsave (prend->gp_context);
					gnome_print_translate (prend->gp_context, bbox.x0 + x, - y - h - bbox.y0);
					gnome_print_scale (prend->gp_context, (int)(bbox.x1 - bbox.x0) % w, h);
					print_image (prend, image, (int)(bbox.x1 - bbox.x0) % w, h);
					gnome_print_grestore (prend->gp_context);
					y += h;
				}
				gnome_print_gsave (prend->gp_context);
				gnome_print_translate (prend->gp_context, bbox.x0 + x, - y - (int)(bbox.y1 - bbox.y0) % h - bbox.y0);
				gnome_print_scale (prend->gp_context, (int)(bbox.x1 - bbox.x0) % w, (int)(bbox.y1 - bbox.y0) % h);
				print_image (prend, image, (int)(bbox.x1 - bbox.x0) % w, (int)(bbox.y1 - bbox.y0) % h);
				gnome_print_grestore (prend->gp_context);
				break;
			}
			gnome_print_grestore (prend->gp_context);
			break;

		case GOG_FILL_STYLE_NONE:
			break; /* impossible */
		}
	}

	if (with_outline) {
		set_color (prend, style->outline.color);
		set_dash (prend, renderer->outline_dash);
		gnome_print_setlinewidth (prend->gp_context,
			gog_renderer_line_size (renderer, style->outline.width));
		gnome_print_stroke (prend->gp_context);
	}
}

static void
draw_bezier_path (GogRendererGnomePrint *prend, ArtBpath const *path)
{
	gnome_print_newpath (prend->gp_context);
	for ( ; path->code != ART_END ; path++)
		switch (path->code) {
		case ART_MOVETO_OPEN :
		case ART_MOVETO :
			gnome_print_moveto (prend->gp_context,
					    path->x3, -path->y3);
			break;
		case ART_LINETO :
			gnome_print_lineto (prend->gp_context,
					    path->x3, -path->y3);
			break;
		case ART_CURVETO :
			gnome_print_curveto (prend->gp_context,
					    path->x1, -path->y1,
					    path->x2, -path->y2,
					    path->x3, -path->y3);
			break;
		default :
			break;
		}
}

static void
gog_renderer_gnome_print_draw_bezier_path (GogRenderer *rend, ArtBpath const *path)
{
	GogRendererGnomePrint *prend = GOG_RENDERER_GNOME_PRINT (rend);
	GogStyle const *style = rend->cur_style;

	if (style->line.dash_type == GO_LINE_NONE)
		return;

	set_color (prend, style->line.color);
	set_dash (prend, rend->line_dash);
	gnome_print_setlinewidth (prend->gp_context,
		gog_renderer_line_size (rend, style->line.width));

	draw_bezier_path (prend, path);

	gnome_print_stroke (prend->gp_context);
}

static void
gog_renderer_gnome_print_draw_bezier_polygon (GogRenderer *rend, ArtBpath const *path,
					      gboolean narrow)
{
	ArtVpath *vpath = art_bez_path_to_vec (path, .1);
	gog_renderer_gnome_print_draw_polygon (rend, vpath, narrow);
	art_free (vpath);
}

static void
gog_renderer_gnome_print_draw_text (GogRenderer *rend, char const *text,
				    GogViewAllocation const *pos, GtkAnchorType anchor,
				    GogViewAllocation *result)
{
	GogRendererGnomePrint *prend = GOG_RENDERER_GNOME_PRINT (rend);
	PangoFontDescription *pango_font = get_font (prend,  rend->cur_style->font.font);

	if (text[0]) {
		GOColor fore_color = rend->cur_style->font.color;
		GOGeometryOBR obr;
		GOGeometryAABR aabr;
		int iw, ih;

		pango_layout_set_font_description (prend->layout, pango_font);
		pango_layout_set_text (prend->layout, text, -1);
		pango_layout_get_size (prend->layout, &iw, &ih);
		obr.w = iw / (double)PANGO_SCALE;
		obr.h = ih / (double)PANGO_SCALE;
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

		gnome_print_gsave (prend->gp_context);
		gnome_print_setrgbcolor (prend->gp_context,
			UINT_RGBA_R (fore_color) / 255.,
			UINT_RGBA_G (fore_color) / 255.,
			UINT_RGBA_B (fore_color) / 255.);
		gnome_print_moveto (prend->gp_context, 
				    obr.x - (obr.w / 2.0) * cos (obr.alpha) - (obr.h / 2.0) * sin (obr.alpha),
				    -obr.y - (obr.w / 2.0) * sin (obr.alpha) + (obr.h / 2.0) * cos (obr.alpha));
		gnome_print_rotate (prend->gp_context, rend->cur_style->text_layout.angle);
		gnome_print_pango_layout (prend->gp_context, prend->layout);
		gnome_print_grestore (prend->gp_context);
		if (result != NULL) {
			result->x = aabr.x;
			result->y = aabr.y;
			result->w = aabr.w;
			result->h = aabr.h;
		}
	}
}

static void
gog_renderer_gnome_print_get_text_OBR (GogRenderer *rend, char const *text, GOGeometryOBR *obr)
{
	GogRendererGnomePrint *prend = GOG_RENDERER_GNOME_PRINT (rend);
	PangoFontDescription *pango_font = get_font (prend,  rend->cur_style->font.font);
	int iw, ih;

	pango_layout_set_font_description (prend->layout, pango_font);
	pango_layout_set_text (prend->layout, text, -1);
	pango_layout_get_size (prend->layout, &iw, &ih);
	obr->w = iw / (double)PANGO_SCALE;
	obr->h = ih / (double)PANGO_SCALE;
}

static void
gog_renderer_gnome_print_draw_marker (GogRenderer *renderer, double x, double y)
{
	GogRendererGnomePrint *prend = GOG_RENDERER_GNOME_PRINT (renderer);
	GOMarker *marker = renderer->cur_style->marker.mark;
	ArtVpath const *outline_path_raw, *fill_path_raw;
	ArtVpath *outline_path, *fill_path;
	double scaling[6], translation[6], affine[6];
	double half_size;

	g_return_if_fail (marker != NULL);

	go_marker_get_paths (marker, &outline_path_raw, &fill_path_raw);

	if ((outline_path_raw == NULL) ||
	    (fill_path_raw == NULL))
		return;

	gnome_print_gsave (prend->gp_context);

	half_size = gog_renderer_line_size (renderer, marker->size) / 2.0;
	art_affine_scale (scaling, half_size, half_size);
	art_affine_translate (translation, x, y);
	art_affine_multiply (affine, scaling, translation);

	outline_path = art_vpath_affine_transform (outline_path_raw, affine);
	fill_path = art_vpath_affine_transform (fill_path_raw, affine);

	gnome_print_setlinecap (prend->gp_context, ART_PATH_STROKE_CAP_ROUND);
	set_color (prend, marker->fill_color);
	draw_path (prend, fill_path);
	gnome_print_closepath (prend->gp_context);
	gnome_print_fill (prend->gp_context);

	set_color (prend, marker->outline_color);
	gnome_print_setlinewidth (prend->gp_context,
		gog_renderer_line_size (renderer,
					go_marker_get_outline_width (marker)));
	draw_path (prend, outline_path);
	gnome_print_stroke (prend->gp_context);
	gnome_print_newpath (prend->gp_context);

	gnome_print_grestore (prend->gp_context);

	g_free (outline_path);
	g_free (fill_path);
}

static void
gog_renderer_gnome_print_class_init (GogRendererClass *rend_klass)
{
	GObjectClass *gobject_klass   = (GObjectClass *) rend_klass;

	parent_klass = g_type_class_peek_parent (rend_klass);
	gobject_klass->finalize	  	= gog_renderer_gnome_print_finalize;
	rend_klass->push_clip  		= gog_renderer_gnome_print_push_clip;
	rend_klass->pop_clip		= gog_renderer_gnome_print_pop_clip;
	rend_klass->draw_path	  	= gog_renderer_gnome_print_draw_path;
	rend_klass->draw_polygon  	= gog_renderer_gnome_print_draw_polygon;
	rend_klass->draw_bezier_path 	= gog_renderer_gnome_print_draw_bezier_path;
	rend_klass->draw_bezier_polygon = gog_renderer_gnome_print_draw_bezier_polygon;
	rend_klass->draw_text	  	= gog_renderer_gnome_print_draw_text;
	rend_klass->draw_marker	  	= gog_renderer_gnome_print_draw_marker;
	rend_klass->get_text_OBR	= gog_renderer_gnome_print_get_text_OBR;
}

static void
gog_renderer_gnome_print_init (GogRendererGnomePrint *prend)
{
	prend->gp_context = NULL;
	prend->fonts = g_ptr_array_new ();
	prend->bbox.x =
	prend->bbox.y = 0.0;
	prend->bbox.w =
	prend->bbox.h = 1.0;
}

static GSF_CLASS (GogRendererGnomePrint, gog_renderer_gnome_print,
		  gog_renderer_gnome_print_class_init, gog_renderer_gnome_print_init,
		  GOG_RENDERER_TYPE)

void
gog_graph_print_to_gnome_print (GogGraph *graph,
				GnomePrintContext *gp_context,
				double width, double height)
{
	GogViewAllocation allocation;
	GogRendererGnomePrint *prend =
		g_object_new (GOG_RENDERER_GNOME_PRINT_TYPE,
			      "model", graph,
			      "zoom", 1.,
			      NULL);

	gog_graph_force_update (graph);

	prend->gp_context = g_object_ref (gp_context);
	prend->layout = gnome_print_pango_create_layout (prend->gp_context);
	allocation.x = 0.;
	allocation.y = 0.;
	allocation.w = width;
	allocation.h = height;
	gog_view_size_allocate (prend->base.view, &allocation);
	prend->bbox = allocation;
	
	/* FIXME FIXME FIXME this is a workaround for a bug in libgnomeprint
	 * where line with width == 1.0 don't scale properly before an other
	 * line width is set.
	 * 
	 * http://bugzilla.gnome.org/show_bug.cgi?id=149452
	 */
	gnome_print_setlinewidth (prend->gp_context, 0.1);
	
	gog_view_render	(prend->base.view, NULL);
	g_object_unref (prend);
}
