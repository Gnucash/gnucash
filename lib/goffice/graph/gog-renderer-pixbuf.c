/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-renderer-pixbuf.c :
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
#include <goffice/graph/gog-renderer-pixbuf.h>
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
//#include <src/application.h>
#include <application.h>

#include <math.h>

struct _GogRendererPixbuf {
	GogRenderer base;

	int 		 w, h;
	int		 x_offset, y_offset;
	GdkPixbuf 	*buffer;
	guchar    	*pixels; /* from pixbuf */
	int	   	 rowstride;

	PangoContext 	*pango_context;
	PangoLayout	*pango_layout;
};

typedef GogRendererClass GogRendererPixbufClass;

static GObjectClass *parent_klass;

static void
gog_renderer_pixbuf_finalize (GObject *obj)
{
	GogRendererPixbuf *prend = GOG_RENDERER_PIXBUF (obj);

	if (prend->buffer != NULL) {
		g_object_unref (prend->buffer);
		prend->buffer = NULL;
	}

	if (prend->pango_layout != NULL) {
		g_object_unref (prend->pango_layout);
		prend->pango_layout = NULL;
	}

	if (prend->pango_context != NULL) {
#ifdef HAVE_PANGO_CONTEXT_GET_FONT_MAP
		/* See http://bugzilla.gnome.org/show_bug.cgi?id=143542 */
		go_pango_fc_font_map_cache_clear (PANGO_FC_FONT_MAP (pango_context_get_font_map (prend->pango_context)));
#endif
		g_object_unref (prend->pango_context);
		prend->pango_context = NULL;
	}

	(*parent_klass->finalize) (obj);
}

typedef struct
{
	GdkPixbuf	*buffer;
	double		 x_offset;
	double		 y_offset;
} ClipData;

static void
gog_renderer_pixbuf_clip_push (GogRenderer *rend, GogRendererClip *clip)
{
	ClipData *clip_data;
	GdkRectangle graph_rect, clip_rect, res_rect;
	GogRendererPixbuf *prend = GOG_RENDERER_PIXBUF (rend);

	clip->data = g_new (ClipData, 1);
	clip_data = (ClipData *) clip->data;

	clip_data->x_offset = prend->x_offset;
	clip_data->y_offset = prend->y_offset;
	clip_data->buffer = NULL;

	graph_rect.x = graph_rect.y = 0;
	graph_rect.width = gdk_pixbuf_get_width (prend->buffer);
	graph_rect.height = gdk_pixbuf_get_height (prend->buffer);

	clip_rect.x = floor (clip->area.x - prend->x_offset + 0.5);
	clip_rect.y = floor (clip->area.y - prend->y_offset + 0.5);
	clip_rect.width = floor (clip->area.x - prend->x_offset + clip->area.w + 0.5) - clip_rect.x;
	clip_rect.height = floor (clip->area.y -prend->y_offset + clip->area.h + 0.5) - clip_rect.y;

	if (gdk_rectangle_intersect (&graph_rect, &clip_rect, &res_rect)) {
		clip_data->buffer = prend->buffer;
		prend->buffer = gdk_pixbuf_new_subpixbuf (clip_data->buffer,
							  res_rect.x, res_rect.y,
							  res_rect.width, res_rect.height);
		prend->x_offset += res_rect.x;
		prend->y_offset += res_rect.y;
	}

	if (prend->buffer == NULL)
		g_warning ("Pixbuf renderer: invalid clipping region");

	prend->pixels = gdk_pixbuf_get_pixels (prend->buffer);
	prend->w = gdk_pixbuf_get_width (prend->buffer);
	prend->h = gdk_pixbuf_get_height (prend->buffer);
	prend->rowstride = gdk_pixbuf_get_rowstride (prend->buffer);
}

static void
gog_renderer_pixbuf_clip_pop (GogRenderer *rend, GogRendererClip *clip)
{
	GogRendererPixbuf *prend = GOG_RENDERER_PIXBUF (rend);
	ClipData *clip_data = clip->data;

	if (clip_data->buffer != NULL) {
		if (prend->buffer != NULL)
			g_object_unref (prend->buffer);
		prend->buffer = clip_data->buffer;
	}
	prend->pixels = gdk_pixbuf_get_pixels (prend->buffer);
	prend->w = gdk_pixbuf_get_width (prend->buffer);
	prend->h = gdk_pixbuf_get_height (prend->buffer);
	prend->rowstride = gdk_pixbuf_get_rowstride (prend->buffer);
	prend->x_offset = clip_data->x_offset;
	prend->y_offset = clip_data->y_offset;

	g_free (clip->data);
	clip->data = NULL;
}

static ArtSVP *
clip_path (GogViewAllocation const *bound)
{
	ArtVpath path[6];
	path[0].code = ART_MOVETO;
	path[1].code = ART_LINETO;
	path[2].code = ART_LINETO;
	path[3].code = ART_LINETO;
	path[4].code = ART_LINETO;
	path[5].code = ART_END;
	path[0].x = path[1].x = path[4].x = bound->x;
	path[2].x = path[3].x = path[0].x + bound->w;
	path[0].y = path[3].y = path[4].y = bound->y;
	path[1].y = path[2].y = path[0].y + bound->h;
	return art_svp_from_vpath ((ArtVpath *)path);
}

static double
line_size (GogRenderer const *rend, double width)
{
	if (go_sub_epsilon (width) <= 0.) /* cheesy version of hairline */
		return 1.;
	
	width *= rend->scale;
	if (width <= 1.)
		return width;

	return floor (width);
}

static double
gog_renderer_pixbuf_line_size (GogRenderer const *rend, double width)
{
	double size = line_size (rend, width);

	if (size < 1.0)
		return ceil (size);

	return size;
}

static void
gog_renderer_pixbuf_sharp_path (GogRenderer *rend, ArtVpath *path, double line_width) 
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
gog_renderer_pixbuf_draw_path (GogRenderer *rend, ArtVpath const *path,
			       GogViewAllocation const *bound)
{
	GogRendererPixbuf *prend = GOG_RENDERER_PIXBUF (rend);
	GogStyle const *style = rend->cur_style;
	double width = line_size (rend, style->line.width);
	ArtSVP *svp;
	ArtVpath *dashed_path;

	switch (style->line.dash_type) {
		case GO_LINE_NONE:
			return;
		case GO_LINE_SOLID:
			svp = art_svp_vpath_stroke ((ArtVpath *) path,
						    ART_PATH_STROKE_JOIN_MITER, 
						    ART_PATH_STROKE_CAP_BUTT,
						    width, 4, 0.5);
			break;
		default:
			dashed_path = go_line_dash_vpath (path, rend->line_dash,  
				rend->cur_clip != NULL ? &rend->cur_clip->area : NULL);
			if (dashed_path == NULL)
				return;
			svp = art_svp_vpath_stroke (dashed_path,
						    ART_PATH_STROKE_JOIN_MITER, 
						    ART_PATH_STROKE_CAP_BUTT,
						    width, 4, 0.5);
			g_free (dashed_path);
	}

	if (bound != NULL) {
		ArtSVP *orig = svp;
		ArtSVP *clip = clip_path (bound);
		svp = art_svp_intersect (clip, orig);
		art_svp_free (clip);
		art_svp_free (orig);
	}

	go_color_render_svp (style->line.color, svp,
			     prend->x_offset,
			     prend->y_offset,
			     prend->w + prend->x_offset,
			     prend->h + prend->y_offset,
			     prend->pixels, prend->rowstride);
	art_svp_free (svp);
}

static ArtRender *
gog_art_renderer_new (GogRendererPixbuf *prend)
{
	return art_render_new (prend->x_offset,
			       prend->y_offset,
			       prend->w + prend->x_offset,
			       prend->h + prend->y_offset,
			       prend->pixels, prend->rowstride,
		gdk_pixbuf_get_n_channels (prend->buffer) - 1,
		8, ART_ALPHA_SEPARATE, NULL);
}

static void
gog_renderer_pixbuf_draw_polygon (GogRenderer *rend, ArtVpath const *path,
				  gboolean narrow, GogViewAllocation const *bound)
{
	GogRendererPixbuf *prend = GOG_RENDERER_PIXBUF (rend);
	GogStyle const *style = rend->cur_style;
	ArtVpath *dashed_path;
	ArtRender *render;
	ArtSVP *fill, *outline = NULL;
	ArtDRect bbox;
	ArtGradientLinear gradient;
	ArtGradientStop stops[2];
	GdkPixbuf *image;
	gint i, j, imax, jmax, h, w;
	double width = line_size (rend, style->outline.width);

	if (!narrow) {
		switch (style->outline.dash_type) {
			case GO_LINE_NONE:
				break;
			case GO_LINE_SOLID:
				outline = art_svp_vpath_stroke ((ArtVpath *) path,
								ART_PATH_STROKE_JOIN_MITER, 
								ART_PATH_STROKE_CAP_BUTT,
								width, 4, 0.5);
				break;
			default:
				dashed_path = go_line_dash_vpath (path, rend->outline_dash,  
					rend->cur_clip != NULL ? &rend->cur_clip->area : NULL);
				if (dashed_path != NULL) {
					outline = art_svp_vpath_stroke (dashed_path,
									ART_PATH_STROKE_JOIN_MITER, 
									ART_PATH_STROKE_CAP_BUTT,
									width, 4, 0.5);
					g_free (dashed_path);
				}
		}
		if (bound != NULL && outline != NULL) {
			ArtSVP *orig = outline;
			ArtSVP *clip = clip_path (bound);
			outline = art_svp_intersect (clip, orig);
			art_svp_free (clip);
			art_svp_free (orig);
		}
	}

	if (style->fill.type != GOG_FILL_STYLE_NONE) {
		fill = art_svp_from_vpath ((ArtVpath *)path);
		if (bound != NULL) {
			ArtSVP *orig = fill;
			ArtSVP *clip = clip_path (bound);
			fill = art_svp_intersect (clip, orig);
			art_svp_free (clip);
			art_svp_free (orig);
		}
#if 0 /* art_svp_minus is not implemented */
		if (outline != NULL) {
			ArtSVP *tmp = art_svp_minus (fill, outline);
			art_svp_free (fill);
			fill = tmp;
		}
#endif

		switch (style->fill.type) {
		case GOG_FILL_STYLE_PATTERN:
			go_pattern_render_svp (&style->fill.pattern,
				fill,
				prend->x_offset,
				prend->y_offset,
				prend->w + prend->x_offset,
				prend->h + prend->y_offset,
				prend->pixels, prend->rowstride);
			break;

		case GOG_FILL_STYLE_GRADIENT: {

			art_vpath_bbox_drect ((ArtVpath *)path, &bbox);
			render = gog_art_renderer_new (prend);
			art_render_svp (render, fill);

			go_gradient_setup (&gradient,
					   style->fill.gradient.dir,
					   style->fill.pattern.back, style->fill.pattern.fore,
					   bbox.x0, bbox.y0, bbox.x1, bbox.y1,
					   stops);

			art_render_gradient_linear (render,
				&gradient, ART_FILTER_NEAREST);
			art_render_invoke (render);
			break;
		}

		case GOG_FILL_STYLE_IMAGE: {
			GdkRectangle path_rect, clip_rect, dest_rect;

			image = style->fill.image.image;
			if (image == NULL)
				break;

			art_vpath_bbox_drect (path, &bbox);

			path_rect.x = bbox.x0 - prend->x_offset;
			path_rect.y = bbox.y0 - prend->y_offset;
			path_rect.width = bbox.x1 - bbox.x0;
			path_rect.height = bbox.y1 - bbox.y0;

			clip_rect.x = clip_rect.y = 0;
			clip_rect.width = prend->w;
			clip_rect.height = prend->h;

			if (gdk_rectangle_intersect (&path_rect, &clip_rect, &dest_rect)) {
				switch (style->fill.image.type) {
					case GOG_IMAGE_CENTERED:
						w = ((bbox.x1 - bbox.x0) - gdk_pixbuf_get_width (image)) / 2.;
						if (w < 0.) w = 0.;
						h = ((bbox.y1 - bbox.y0) - gdk_pixbuf_get_height (image)) / 2.;
						if (h < 0.) h = 0.;
						gdk_pixbuf_composite (image, prend->buffer,
								      dest_rect.x + w, dest_rect.y + h,
								      gdk_pixbuf_get_width (image),
								      gdk_pixbuf_get_height (image),
								      path_rect.x + w, path_rect.y + h,
								      1., 1.,
								      GDK_INTERP_BILINEAR, 255);
						break;
					case GOG_IMAGE_STRETCHED:
						gdk_pixbuf_composite (image, prend->buffer,
								      dest_rect.x, dest_rect.y,
								      dest_rect.width, dest_rect.height,
								      path_rect.x, path_rect.y,
								      path_rect.width /
								      (double)gdk_pixbuf_get_width (image),
								      path_rect.height /
								      (double)gdk_pixbuf_get_height (image),
								      GDK_INTERP_BILINEAR, 255);
						break;

					case GOG_IMAGE_WALLPAPER: {
						GdkRectangle image_rect, copy_rect;

						imax = path_rect.width /
							(image_rect.width = gdk_pixbuf_get_width (image));
						jmax = path_rect.height /
							(image_rect.height = gdk_pixbuf_get_height (image));

						image_rect.x = path_rect.x;
						for (i = 0; i <= imax; i++)
						{
							image_rect.y = path_rect.y;
							for (j = 0; j <= jmax; j++) {

								if (gdk_rectangle_intersect (&image_rect,
											     &dest_rect,
											     &copy_rect))
									gdk_pixbuf_copy_area (image,
											      copy_rect.x - image_rect.x,
											      copy_rect.y - image_rect.y,
											      copy_rect.width,
											      copy_rect.height,
											      prend->buffer,
											      copy_rect.x,
											      copy_rect.y);
								image_rect.y += image_rect.height;
							}
							image_rect.x +=image_rect.width;

						}
						break;
					}
				}
			}
			break;
		}

		case GOG_FILL_STYLE_NONE:
			break; /* impossible */
		}
		if (fill != NULL)
			art_svp_free (fill);
	}

	if (outline != NULL) {
		go_color_render_svp (style->outline.color, outline,
				     prend->x_offset,
				     prend->y_offset,
				     prend->w + prend->x_offset,
				     prend->h + prend->y_offset,
				     prend->pixels, prend->rowstride);
		art_svp_free (outline);
	}
}

static PangoContext *
gog_renderer_pixbuf_get_pango_context (GogRendererPixbuf *prend)
{
	PangoFT2FontMap *font_map;
	
	if (prend->pango_context != NULL)
		return prend->pango_context;

	font_map = PANGO_FT2_FONT_MAP (pango_ft2_font_map_new ());
	pango_ft2_font_map_set_resolution (font_map,
					   gnm_app_display_dpi_get (TRUE),
					   gnm_app_display_dpi_get (FALSE));
	prend->pango_context = pango_ft2_font_map_create_context  (font_map);
	g_object_unref (font_map);

	return prend->pango_context;
}

static PangoLayout *
gog_renderer_pixbuf_get_pango_layout (GogRendererPixbuf *prend)
{
	PangoContext *context;
	PangoAttribute *attr;
	PangoAttrList  *attrs = NULL;
	PangoFontDescription const *fd = prend->base.cur_style->font.font->desc;

	// +jsled: testing font display issues.
	if (0)
	{
		char *pfdStr;
		pfdStr = pango_font_description_to_string( fd );
		printf( "pfd=[%s]\n", pfdStr );
		g_free( pfdStr );
	}
	// -jsled

	if (prend->pango_layout != NULL)
		return prend->pango_layout;
	
	context = gog_renderer_pixbuf_get_pango_context (prend);

	prend->pango_layout = pango_layout_new (context);
	
	pango_layout_set_font_description (prend->pango_layout, fd);

	/*
	 * Manually scale the font size to compensate for
	 * Before the fix to http://bugzilla.gnome.org/show_bug.cgi?id=121543
	 * the scale would otherwise be ignored.
	 */
	attr = pango_attr_size_new (prend->base.zoom *
		pango_font_description_get_size (fd));
	attr->start_index = 0;
	attr->end_index = -1;

	attrs = pango_attr_list_new ();
	pango_attr_list_insert (attrs, attr);
	pango_layout_set_attributes (prend->pango_layout, attrs);
	pango_attr_list_unref (attrs);

	return prend->pango_layout;
}

static void
gog_renderer_pixbuf_draw_text (GogRenderer *rend, char const *text,
			       GogViewAllocation const *pos, GtkAnchorType anchor,
			       GogViewAllocation *result)
{
	FT_Bitmap ft_bitmap;
	GogRendererPixbuf *prend = GOG_RENDERER_PIXBUF (rend);
	PangoRectangle rect;
	PangoLayout   *layout;
	guint8 r, g, b, a, alpha, *dst, *src;
	int h, w, i, x, y;
	GogStyle const *style = rend->cur_style;

	layout = gog_renderer_pixbuf_get_pango_layout ((GogRendererPixbuf *) rend);
	pango_layout_set_text (layout, text, -1);
	pango_layout_get_extents (layout, NULL, &rect);
	rect.x = PANGO_PIXELS (rect.x);
	rect.y = PANGO_PIXELS (rect.y);
	x = (int)((pos->x - prend->x_offset) * PANGO_SCALE);
	y = (int)((pos->y - prend->y_offset) * PANGO_SCALE);
	switch (anchor) {
	case GTK_ANCHOR_CENTER : case GTK_ANCHOR_N : case GTK_ANCHOR_S :
		x -= rect.width / 2;
		break;
	case GTK_ANCHOR_NE : case GTK_ANCHOR_SE : case GTK_ANCHOR_E :
		x -= rect.width;
		break;
	default : break;
	}
	x = (x > 0) ? (x + PANGO_SCALE / 2) / PANGO_SCALE : 0;
	w = (rect.width + PANGO_SCALE / 2) / PANGO_SCALE;
/*	Makes rendering inconsitent with gnome-print and svg renderer */
/*	if (w > pos->w && pos->w >= 0)*/
/*		w = pos->w;*/
	if ((x + w) > prend->w)
		w = prend->w - x;

	switch (anchor) {
	case GTK_ANCHOR_CENTER : case GTK_ANCHOR_E : case GTK_ANCHOR_W :
		y -= rect.height / 2;
		break;
	case GTK_ANCHOR_SE : case GTK_ANCHOR_S : case GTK_ANCHOR_SW :
		y -= rect.height;
		break;
	default : break;
	}
	y = (y > 0) ? (y + PANGO_SCALE / 2) / PANGO_SCALE : 0;
	h = (rect.height + PANGO_SCALE / 2) / PANGO_SCALE;
/*	Makes rendering inconsitent with gnome-print and svg renderer */
/*	if (h > pos->h && pos->h >= 0)*/
/*		h = pos->h;*/
	if ((y + h) > prend->h)
		h = prend->h - y;

	if (result != NULL) {
		result->x = x;
		result->y = y;
		result->w = w;
		result->h = h;
	}

	if (w <= 0 || h <= 0) 
		return;

	ft_bitmap.rows         = h;
	ft_bitmap.width        = w;
	ft_bitmap.pitch        = (w+3) & ~3;
	ft_bitmap.buffer       = g_malloc0 (ft_bitmap.rows * ft_bitmap.pitch);
	ft_bitmap.num_grays    = 256;
	ft_bitmap.pixel_mode   = ft_pixel_mode_grays;
	ft_bitmap.palette_mode = 0;
	ft_bitmap.palette      = NULL;
	pango_ft2_render_layout (&ft_bitmap, layout, -rect.x, -rect.y);

	r = UINT_RGBA_R (style->font.color);
	g = UINT_RGBA_G (style->font.color);
	b = UINT_RGBA_B (style->font.color);
	a = UINT_RGBA_A (style->font.color);

	/* do the compositing manually, ArtRender as used in librsvg is dog
	 * slow, and I do not feel like leaping through 20 different data
	 * structures to composite 1 byte images, onto rgba */
	dst = prend->pixels;
	dst += (y * prend->rowstride);
	dst += (x + rect.x)* 4;
	src = ft_bitmap.buffer;

	while (h--) {
		for (i = w; i-- > 0 ; dst += 4, src++) {
			/* FIXME: Do the libart thing instead of divide by 255 */
			alpha = (a * (*src)) / 255;
			dst[0] = (dst[0] * (255 - alpha) + r * alpha) / 255;
			dst[1] = (dst[1] * (255 - alpha) + g * alpha) / 255;
			dst[2] = (dst[2] * (255 - alpha) + b * alpha) / 255;
			dst[3] = (dst[3] * (255 - alpha) + a * alpha) / 255;
		}
		dst += prend->rowstride - w*4;
		src += ft_bitmap.pitch - w;
	}

	g_free (ft_bitmap.buffer);
}

static void
gog_renderer_pixbuf_draw_marker (GogRenderer *rend, double x, double y)
{
	GdkRectangle r1, r2, dest;
	GogStyle const *style = rend->cur_style;
	GogRendererPixbuf *prend = GOG_RENDERER_PIXBUF (rend);
	GdkPixbuf const *marker_pixbuf = go_marker_get_pixbuf (style->marker.mark, rend->scale);

	if (marker_pixbuf == NULL)
		return;

	r2.x = r2.y = 0;
	r2.width = prend->w;
	r2.height = prend->h;

	r1.width = gdk_pixbuf_get_width (marker_pixbuf);
	r1.height = gdk_pixbuf_get_height (marker_pixbuf);
	r1.x = floor (floor (x + .5) - r1.width / 2.0 - prend->x_offset);
	r1.y = floor (floor (y + .5) - r1.height / 2.0 - prend->y_offset);

	if (gdk_rectangle_intersect (&r1, &r2, &dest))
		gdk_pixbuf_composite (marker_pixbuf, prend->buffer,
				      dest.x, dest.y,
				      dest.width, dest.height,
				      r1.x, r1.y,
				      1.0, 1.0,
				      GDK_INTERP_NEAREST,
				      255);
}

static void
gog_renderer_pixbuf_measure_text (GogRenderer *rend,
				  char const *text, GogViewRequisition *size)
{
	PangoRectangle  logical;
	PangoLayout    *layout;
	
	layout = gog_renderer_pixbuf_get_pango_layout ((GogRendererPixbuf *) rend);
	pango_layout_set_text (layout, text, -1);
	pango_layout_get_pixel_extents (layout, NULL, &logical);

	size->w = logical.width;
	size->h = logical.height;
}

static void
gog_renderer_pixbuf_pop_style (GogRenderer *rend)
{
	GogRendererPixbuf *prend = (GogRendererPixbuf *) rend;

	if (prend->pango_layout != NULL)
	{
		g_object_unref (prend->pango_layout);
		prend->pango_layout = NULL;
	}
}

static void
gog_renderer_pixbuf_push_style (GogRenderer *rend, GogStyle const *style)
{
	GogRendererPixbuf *prend = (GogRendererPixbuf *) rend;

	if (prend->pango_layout != NULL)
	{
		g_object_unref (prend->pango_layout);
		prend->pango_layout = NULL;
	}
}

static void
gog_renderer_pixbuf_class_init (GogRendererClass *rend_klass)
{
	GObjectClass *gobject_klass   = (GObjectClass *) rend_klass;

	parent_klass = g_type_class_peek_parent (rend_klass);
	gobject_klass->finalize		= gog_renderer_pixbuf_finalize;
	rend_klass->push_style		= gog_renderer_pixbuf_push_style;
	rend_klass->pop_style		= gog_renderer_pixbuf_pop_style;
	rend_klass->clip_push  		= gog_renderer_pixbuf_clip_push;
	rend_klass->clip_pop     	= gog_renderer_pixbuf_clip_pop;
	rend_klass->sharp_path		= gog_renderer_pixbuf_sharp_path;
	rend_klass->draw_path	  	= gog_renderer_pixbuf_draw_path;
	rend_klass->draw_polygon  	= gog_renderer_pixbuf_draw_polygon;
	rend_klass->draw_text	  	= gog_renderer_pixbuf_draw_text;
	rend_klass->draw_marker	  	= gog_renderer_pixbuf_draw_marker;
	rend_klass->measure_text  	= gog_renderer_pixbuf_measure_text;
	rend_klass->line_size		= gog_renderer_pixbuf_line_size;
}

static void
gog_renderer_pixbuf_init (GogRendererPixbuf *prend)
{
	prend->buffer = NULL;
	prend->w = prend->h = 1; /* just in case */
	prend->x_offset = prend->y_offset = 0;
	prend->pango_layout = NULL;
	prend->pango_context = NULL;
}

GSF_CLASS (GogRendererPixbuf, gog_renderer_pixbuf,
	   gog_renderer_pixbuf_class_init, gog_renderer_pixbuf_init,
	   GOG_RENDERER_TYPE)

GdkPixbuf *
gog_renderer_pixbuf_get (GogRendererPixbuf *prend)
{
	g_return_val_if_fail (prend != NULL, NULL);

	return prend->buffer;
}

#if 0 /* An initial non-working attempt to use different dpi to render
	 different zooms */

/* fontmaps are reasonably expensive use a cache to share them */
static GHashTable *fontmap_cache = NULL; /* PangoFT2FontMap hashed by y_dpi */
static gboolean
cb_remove_entry (gpointer key, PangoFT2FontMap *value, PangoFT2FontMap *target)
{
	return value == target;
}
static void
cb_map_is_gone (gpointer data, GObject *where_the_object_was)
{
	g_warning ("fontmap %p is gone",where_the_object_was);
	g_hash_table_foreach_steal (fontmap_cache,
		(GHRFunc) cb_remove_entry, where_the_object_was);
}
static void
cb_weak_unref (GObject *fontmap)
{
	g_object_weak_unref (fontmap, cb_map_is_gone, NULL);
}
static PangoFT2FontMap *
fontmap_from_cache (double x_dpi, double y_dpi)
{
	PangoFT2FontMap *fontmap = NULL;
	int key_dpi = floor (y_dpi + .5);
	gpointer key = GUINT_TO_POINTER (key_dpi);

	if (fontmap_cache != NULL)
		fontmap = g_hash_table_lookup (fontmap_cache, key);
	else
		fontmap_cache = g_hash_table_new_full (g_direct_hash, g_direct_equal,
			NULL, (GDestroyNotify) cb_weak_unref);

	if (fontmap == NULL) {
		fontmap = PANGO_FT2_FONT_MAP (pango_ft2_font_map_new ());
		pango_ft2_font_map_set_resolution (fontmap, x_dpi, y_dpi);
		g_object_weak_ref (G_OBJECT (fontmap), cb_map_is_gone, NULL);
		g_hash_table_insert (fontmap_cache, key, fontmap);
	} else
		g_object_ref (fontmap);

	g_warning ("fontmap %d = %p", key_dpi, fontmap);
	return fontmap;
}
#endif

/**
 * gog_renderer_update :
 * @prend :
 * @w :
 * @h :
 *
 * Returns TRUE if the size actually changed.
 **/
gboolean
gog_renderer_pixbuf_update (GogRendererPixbuf *prend, int w, int h, double zoom)
{
	gboolean redraw = TRUE;
	GogView *view;
	GogViewAllocation allocation;

	g_return_val_if_fail (prend != NULL, FALSE);
	g_return_val_if_fail (prend->base.view != NULL, FALSE);

	view = prend->base.view;
	allocation.x = allocation.y = 0.;
	allocation.w = w;
	allocation.h = h;
	if (prend->w != w || prend->h != h) {
		double dpi_x, dpi_y;

		prend->w = w;
		prend->h = h;
		prend->base.scale_x = w / prend->base.logical_width_pts;
		prend->base.scale_y = h / prend->base.logical_height_pts;
		prend->base.scale = MIN (prend->base.scale_x, prend->base.scale_y);
		prend->base.zoom  = zoom;
		dpi_x = gog_renderer_pt2r_x (&prend->base, GO_IN_TO_PT ((double)1.))
			/ zoom;
		dpi_y = gog_renderer_pt2r_y (&prend->base, GO_IN_TO_PT ((double)1.))
			/ zoom;

		if (prend->buffer != NULL) {
			g_object_unref (prend->buffer);
			prend->buffer = NULL;
		}

		if (prend->pango_layout != NULL) {
			g_object_unref (prend->pango_layout);
			prend->pango_layout = NULL;
		}

		if (prend->pango_context != NULL) {
			g_object_unref (prend->pango_context);
			prend->pango_context = NULL;
		}

		/* make sure we dont try to queue an update while updating */
		prend->base.needs_update = TRUE;

		/* scale just changed need to recalculate sizes */
		gog_renderer_invalidate_size_requests (&prend->base);
		gog_view_size_allocate (view, &allocation);
	} else if (w != view->allocation.w || h != view->allocation.h)
		gog_view_size_allocate (view, &allocation);
	else
		redraw = gog_view_update_sizes (view);

	redraw |= prend->base.needs_update;
	prend->base.needs_update = FALSE;

	gog_debug (0, g_warning ("rend_pixbuf:update = %d", redraw););

	if (redraw) {
		if (prend->buffer == NULL) {
			prend->buffer = gdk_pixbuf_new (GDK_COLORSPACE_RGB, TRUE, 8,
							prend->w, prend->h);
			if (prend->buffer == NULL) {
				g_warning ("Chart is too large");
				return FALSE;
			}
			prend->pixels    = gdk_pixbuf_get_pixels (prend->buffer);
			prend->rowstride = gdk_pixbuf_get_rowstride (prend->buffer);
		}
		gdk_pixbuf_fill (prend->buffer, 0);

		gog_view_render	(view, NULL);
	}

	return redraw;
}
