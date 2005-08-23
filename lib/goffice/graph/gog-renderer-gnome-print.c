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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
 * USA
 */

#include <goffice/goffice-config.h>
/* <style.h> is only needed for gnm_font_find_closest_from_weight_slant */
#include <style.h>
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

#ifdef HAVE_GNOME_PRINT_PANGO_CREATE_LAYOUT
#include <libgnomeprint/gnome-print-pango.h>
#endif

#define GOG_RENDERER_GNOME_PRINT_TYPE	(gog_renderer_gnome_print_get_type ())
#define GOG_RENDERER_GNOME_PRINT(o)	(G_TYPE_CHECK_INSTANCE_CAST ((o), GOG_RENDERER_GNOME_PRINT_TYPE, GogRendererGnomePrint))
#define IS_GOG_RENDERER_GNOME_PRINT(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOG_RENDERER_GNOME_PRINT_TYPE))

typedef struct _GogRendererGnomePrint GogRendererGnomePrint;

struct _GogRendererGnomePrint {
	GogRenderer base;

	GPtrArray *fonts;
	GnomePrintContext *gp_context;
	PangoLayout *layout;
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
		GnomeFont *font;
		for (i = prend->fonts->len; i-- > 0 ; ) {
			font = g_ptr_array_index (prend->fonts, i);
			if (font != NULL)
				gnome_font_unref (font);
		}

		g_ptr_array_free (prend->fonts, TRUE);
		prend->fonts = NULL;
	}

	(*parent_klass->finalize) (obj);
}

/*
 * print_make_rectangle_path
 * @pc      print context
 * @left    left side x coordinate
 * @bottom  bottom side y coordinate
 * @right   right side x coordinate
 * @top     top side y coordinate
 *
 * Make a rectangular path.
 * */

static void
print_make_rectangle_path (GnomePrintContext *pc,
                           double left, double bottom,
                           double right, double top)
{
        g_return_if_fail (pc != NULL);

        gnome_print_newpath   (pc);
        gnome_print_moveto    (pc, left, bottom);
        gnome_print_lineto    (pc, left, top);
        gnome_print_lineto    (pc, right, top);
        gnome_print_lineto    (pc, right, bottom);
        gnome_print_closepath (pc);
}

static GnomeFont *
get_font (GogRendererGnomePrint *prend, GOFont const *gf)
{
	GnomeFont *res = NULL;

	if (gf->font_index < (int)prend->fonts->len)
		res = g_ptr_array_index (prend->fonts, gf->font_index);
	else
		g_ptr_array_set_size (prend->fonts, gf->font_index+1);

	if (res == NULL) {
		PangoFontDescription *desc = gf->desc;
		res = gnm_font_find_closest_from_weight_slant (
			pango_font_description_get_family (desc),
			pango_font_description_get_weight (desc) >= PANGO_WEIGHT_BOLD ? GNOME_FONT_BOLD : GNOME_FONT_REGULAR,
			pango_font_description_get_style (desc) != PANGO_STYLE_NORMAL,
			prend->base.zoom * pango_font_description_get_size (desc) / PANGO_SCALE);
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
gog_renderer_gnome_print_clip_push (GogRenderer *rend, GogRendererClip *clip)
{
	GogRendererGnomePrint *prend = GOG_RENDERER_GNOME_PRINT (rend);

	gnome_print_gsave (prend->gp_context);
	print_make_rectangle_path (prend->gp_context,
				   clip->area.x, -clip->area.y,
				   clip->area.w + clip->area.x,
				   -clip->area.h - clip->area.y);
	gnome_print_clip (prend->gp_context);
}

static void
gog_renderer_gnome_print_clip_pop (GogRenderer *rend, GogRendererClip *clip)
{
	GogRendererGnomePrint *prend = GOG_RENDERER_GNOME_PRINT (rend);

	gnome_print_grestore (prend->gp_context);
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
setup_clip (GogRendererGnomePrint *prend, GogViewAllocation const *bound)
{
	gnome_print_gsave (prend->gp_context);
	gnome_print_moveto (prend->gp_context, bound->x, -bound->y);
	gnome_print_lineto (prend->gp_context, bound->x + bound->w, -bound->y);
	gnome_print_lineto (prend->gp_context, bound->x + bound->w, -bound->y - bound->h);
	gnome_print_lineto (prend->gp_context, bound->x, -bound->y - bound->h);
	gnome_print_clip (prend->gp_context);
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
gog_renderer_gnome_print_draw_path (GogRenderer *renderer, ArtVpath const *path,
				    GogViewAllocation const *bound)
{
	GogRendererGnomePrint *prend = GOG_RENDERER_GNOME_PRINT (renderer);
	GogStyle const *style = renderer->cur_style;

	if (style->line.dash_type == GO_LINE_NONE)
		return;

	set_color (prend, style->line.color);
	set_dash (prend, renderer->line_dash);
	gnome_print_setlinewidth (prend->gp_context,
		gog_renderer_line_size (renderer, style->line.width));

	if (bound != NULL)
		setup_clip (prend, bound);
	
	if (style->line.dash_type != GO_LINE_SOLID && renderer->cur_clip != NULL) {
		ArtVpath *clipped = go_line_clip_vpath (path, &renderer->cur_clip->area);
		draw_path (prend, clipped);
		g_free (clipped);
	} else
		draw_path (prend, path);

	gnome_print_stroke (prend->gp_context);
	
	if (bound != NULL)
		gnome_print_grestore (prend->gp_context);
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
				       gboolean narrow, GogViewAllocation const *bound)
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

	if (bound != NULL)
		setup_clip (prend, bound);

	if (style->fill.type != GOG_FILL_STYLE_NONE || with_outline) {
		if (style->outline.dash_type != GO_LINE_SOLID && renderer->cur_clip != NULL) {
			ArtVpath *clipped = go_line_clip_vpath (path, &renderer->cur_clip->area);
			draw_path (prend, clipped);
			g_free (clipped);
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
	if (bound != NULL)
		gnome_print_grestore (prend->gp_context);
}

static void
gog_renderer_gnome_print_draw_text (GogRenderer *rend, char const *text,
				    GogViewAllocation const *pos, GtkAnchorType anchor,
				    GogViewAllocation *result)
{
	GogRendererGnomePrint *prend = GOG_RENDERER_GNOME_PRINT (rend);
	GnomeFont *gfont = get_font (prend,  rend->cur_style->font.font);

	if (text[0]) {
		double x, y, w, h;
#ifdef HAVE_GNOME_PRINT_PANGO_CREATE_LAYOUT
		int iw, ih;
		const double dummy_dpi = 300; /* FIXME: What exactly is this?  */
		PangoFontDescription *pango_font =   /* FIXME: can i get the pango font directly ? */
			gnome_font_get_pango_description (gfont, dummy_dpi);

		pango_layout_set_font_description (prend->layout, pango_font);
		pango_layout_set_text (prend->layout, text, -1);
		pango_layout_get_size (prend->layout, &iw, &ih);
		w = iw / (double)PANGO_SCALE;
		h = ih / (double)PANGO_SCALE;
#else
		/* This code will die when we require libgnomeprint 2.8  */
		double font_ascent = gnome_font_get_ascender (gfont);
		w = gnome_font_get_width_utf8 (gfont, text);
		h = font_ascent + gnome_font_get_descender (gfont);
#endif	
		x = pos->x;
		switch (anchor) {
		case GTK_ANCHOR_CENTER : case GTK_ANCHOR_N : case GTK_ANCHOR_S :
			x -= w / 2.0;
			break;
		case GTK_ANCHOR_NE : case GTK_ANCHOR_SE : case GTK_ANCHOR_E :
			x -= w;
			break;
		default : break;
		}
		if (x <= 0)
			x = 0;
	
		y = pos->y;
		switch (anchor) {
		case GTK_ANCHOR_CENTER : case GTK_ANCHOR_E : case GTK_ANCHOR_W :
			y -= h / 2.0;
			break;
		case GTK_ANCHOR_SE : case GTK_ANCHOR_S : case GTK_ANCHOR_SW :
			y -= h;
			break;
		default : break;
		}
		if (y <= 0)
			y = 0;
	
#ifdef GOG_WARN_TODO	
#warning "add clipping"
#endif

#ifdef HAVE_GNOME_PRINT_PANGO_CREATE_LAYOUT
		gnome_print_moveto (prend->gp_context,x, -y);
		gnome_print_pango_layout (prend->gp_context, prend->layout);
		pango_font_description_free (pango_font);
#else	
		/* This code will die when we require libgnomeprint 2.8  */		
		gnome_print_setfont (prend->gp_context, gfont);
		gnome_print_moveto (prend->gp_context, x, -y - font_ascent);
		gnome_print_show (prend->gp_context, text);
#endif	
		if (result != NULL) {
			result->x = x;
			result->y = y;
			result->w = w;
			result->h = h;
		}
	}
}

static void
gog_renderer_gnome_print_measure_text (GogRenderer *rend,
				       char const *text, GogViewRequisition *size)
{
	GogRendererGnomePrint *prend = GOG_RENDERER_GNOME_PRINT (rend);
	GnomeFont *gfont = get_font (prend,  rend->cur_style->font.font);
#ifdef HAVE_GNOME_PRINT_PANGO_CREATE_LAYOUT
	int iw, ih;
	const double dummy_dpi = 300; /* FIXME: What exactly is this?  */
	PangoFontDescription *pango_font =   /* FIXME: can i get the pango font directly ? */
		gnome_font_get_pango_description (gfont, dummy_dpi);

	pango_layout_set_font_description (prend->layout, pango_font);
	pango_layout_set_text (prend->layout, text, -1);
	pango_layout_get_size (prend->layout, &iw, &ih);
	size->w = iw / (double)PANGO_SCALE;
	size->h = ih / (double)PANGO_SCALE;
#else
	size->w = gnome_font_get_width_utf8 (gfont, text);
	size->h = gnome_font_get_ascender (gfont) - gnome_font_get_descender (gfont);
#endif
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
	rend_klass->clip_push  		= gog_renderer_gnome_print_clip_push;
	rend_klass->clip_pop		= gog_renderer_gnome_print_clip_pop;
	rend_klass->draw_path	  	= gog_renderer_gnome_print_draw_path;
	rend_klass->draw_polygon  	= gog_renderer_gnome_print_draw_polygon;
	rend_klass->draw_text	  	= gog_renderer_gnome_print_draw_text;
	rend_klass->draw_marker	  	= gog_renderer_gnome_print_draw_marker;
	rend_klass->measure_text  	= gog_renderer_gnome_print_measure_text;
}

static void
gog_renderer_gnome_print_init (GogRendererGnomePrint *prend)
{
	prend->gp_context = NULL;
	prend->fonts = g_ptr_array_new ();
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
	prend->gp_context = g_object_ref (gp_context);
#ifdef HAVE_GNOME_PRINT_PANGO_CREATE_LAYOUT
	prend->layout = gnome_print_pango_create_layout (prend->gp_context);
#else
	prend->layout = 0;
#endif
	allocation.x = 0.;
	allocation.y = 0.;
	allocation.w = width;
	allocation.h = height;
	gog_view_size_allocate (prend->base.view, &allocation);
	
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
