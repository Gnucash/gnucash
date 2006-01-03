/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-renderer.c :
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
#include <goffice/graph/gog-renderer-impl.h>
#include <goffice/graph/gog-style.h>
#include <goffice/graph/gog-graph.h>
#include <goffice/graph/gog-view.h>
#include <goffice/utils/go-units.h>
#include <goffice/utils/go-font.h>
#include <goffice/utils/go-math.h>

#include <gsf/gsf-impl-utils.h>

/* We need to define an hair line width for the svg and gnome_print renderer. 
 * 0.24 pt is the dot size of a 300 dpi printer, if the plot is printed at scale 1:1 */
#define GOG_RENDERER_HAIR_LINE_WIDTH	0.24

enum {
	RENDERER_PROP_0,
	RENDERER_PROP_MODEL,
	RENDERER_PROP_VIEW,
	RENDERER_PROP_ZOOM
};
enum {
	RENDERER_SIGNAL_REQUEST_UPDATE,
	RENDERER_SIGNAL_LAST
};
static gulong renderer_signals [RENDERER_SIGNAL_LAST] = { 0, };

static GObjectClass *parent_klass;

static void
gog_renderer_finalize (GObject *obj)
{
	GogRenderer *rend = GOG_RENDERER (obj);

	go_line_vpath_dash_free (rend->line_dash);
	rend->line_dash = NULL;
	go_line_vpath_dash_free (rend->outline_dash);
	rend->outline_dash = NULL;

	if (rend->clip_stack != NULL) 
		g_warning ("Missing calls to gog_renderer_pop_clip");

	if (rend->cur_style != NULL) {
		g_warning ("Missing calls to gog_renderer_style_pop left dangling style references");
		g_slist_foreach (rend->style_stack,
			(GFunc)g_object_unref, NULL);
		g_slist_free (rend->style_stack);
		rend->style_stack = NULL;
		g_object_unref ((gpointer)rend->cur_style);
		rend->cur_style = NULL;
	}

	if (rend->view != NULL) {
		g_object_unref (rend->view);
		rend->view = NULL;
	}

	if (rend->font_watcher != NULL) {
		go_font_cache_unregister (rend->font_watcher);
		g_closure_unref (rend->font_watcher);
		rend->font_watcher = NULL;
	}

	(*parent_klass->finalize) (obj);
}

static void
gog_renderer_set_property (GObject *obj, guint param_id,
			   GValue const *value, GParamSpec *pspec)
{
	GogRenderer *rend = GOG_RENDERER (obj);

	switch (param_id) {
	case RENDERER_PROP_MODEL:
		rend->model = GOG_GRAPH (g_value_get_object (value));
		if (rend->view != NULL)
			g_object_unref (rend->view);
		rend->view = g_object_new (gog_graph_view_get_type (),
					   "renderer",	rend,
					   "model",	rend->model,
					   NULL);
		gog_renderer_request_update (rend);
		break;

	case RENDERER_PROP_ZOOM:
		rend->zoom = g_value_get_double (value);
		break;

	default: G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, param_id, pspec);
		 return; /* NOTE : RETURN */
	}
}

static void
gog_renderer_get_property (GObject *obj, guint param_id,
			   GValue *value, GParamSpec *pspec)
{
	GogRenderer const *rend = GOG_RENDERER (obj);

	switch (param_id) {
	case RENDERER_PROP_MODEL:
		g_value_set_object (value, rend->model);
		break;
	case RENDERER_PROP_VIEW:
		g_value_set_object (value, rend->view);
		break;
	case RENDERER_PROP_ZOOM:
		g_value_set_double (value, rend->zoom);
		break;

	default: G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, param_id, pspec);
		 break;
	}
}

void
gog_renderer_request_update (GogRenderer *renderer)
{
	g_return_if_fail (GOG_RENDERER (renderer) != NULL);

	if (renderer->needs_update)
		return;
	renderer->needs_update = TRUE;
	g_signal_emit (G_OBJECT (renderer),
		renderer_signals [RENDERER_SIGNAL_REQUEST_UPDATE], 0);
}

void
gog_renderer_invalidate_size_requests (GogRenderer *rend)
{
	g_return_if_fail (GOG_RENDERER (rend) != NULL);

	if (rend->view)
		gog_renderer_request_update (rend);
}

static void
update_dash (GogRenderer *rend)
{
	double size;
	
	go_line_vpath_dash_free (rend->line_dash);
	rend->line_dash = NULL;
	go_line_vpath_dash_free (rend->outline_dash);
	rend->outline_dash = NULL;
	
	if (rend->cur_style == NULL)
		return;

	size = gog_renderer_line_size (rend, rend->cur_style->line.width);
	rend->line_dash = go_line_get_vpath_dash (rend->cur_style->line.dash_type, size);
	size = gog_renderer_line_size (rend, rend->cur_style->outline.width);
	rend->outline_dash = go_line_get_vpath_dash (rend->cur_style->outline.dash_type, size);
}

void
gog_renderer_push_style (GogRenderer *rend, GogStyle const *style)
{
	GogRendererClass *klass = GOG_RENDERER_GET_CLASS (rend);

	g_return_if_fail (klass != NULL);
	g_return_if_fail (GOG_STYLE (style) != NULL);

	if (rend->cur_style != NULL)
		rend->style_stack = g_slist_prepend (
			rend->style_stack, (gpointer)rend->cur_style);
	g_object_ref ((gpointer)style);
	rend->cur_style = style;

	if (klass->push_style)
		klass->push_style (rend, style);

	update_dash (rend);
}

void
gog_renderer_pop_style (GogRenderer *rend)
{
	GogRendererClass *klass = GOG_RENDERER_GET_CLASS (rend);

	g_return_if_fail (klass != NULL);
	g_return_if_fail (rend->cur_style != NULL);

	g_object_unref ((gpointer)rend->cur_style);
	if (rend->style_stack != NULL) {
		rend->cur_style = rend->style_stack->data;
		rend->style_stack = g_slist_remove (rend->style_stack,
			rend->cur_style);
	} else
		rend->cur_style = NULL;

	if (klass->pop_style)
		klass->pop_style (rend);

	update_dash (rend);
}

/**
 * gog_renderer_push_clip :
 * @rend      : #GogRenderer
 * @clip_path : #ArtVpath array
 *
 * Defines the current clipping region. 
 **/
void
gog_renderer_push_clip (GogRenderer *rend, ArtVpath *clip_path)
{
	GogRendererClip *clip;
	GogRendererClass *klass = GOG_RENDERER_GET_CLASS (rend);

	g_return_if_fail (klass != NULL);
	g_return_if_fail (clip_path != NULL);

	clip = g_new (GogRendererClip, 1);
	clip->path = clip_path;

	rend->clip_stack = g_slist_prepend (rend->clip_stack, clip);
	rend->cur_clip = clip;
	
	(klass->push_clip) (rend, clip);
}

/**
 * gog_renderer_pop_clip :
 * @rend : #GogRenderer
 *
 * End the current clipping.
 **/
void
gog_renderer_pop_clip (GogRenderer *rend)
{
	GogRendererClip *clip;
	GogRendererClass *klass = GOG_RENDERER_GET_CLASS (rend);

	g_return_if_fail (klass != NULL);
	g_return_if_fail (rend->clip_stack != NULL);

	clip = (GogRendererClip *) rend->clip_stack->data;

	(klass->pop_clip) (rend, clip);

	g_free (clip->path);
	g_free (clip);
	rend->clip_stack = g_slist_delete_link (rend->clip_stack, rend->clip_stack);

	if (rend->clip_stack != NULL)
		rend->cur_clip = (GogRendererClip *) rend->clip_stack->data;
	else
		rend->cur_clip = NULL;
}

/**
 * gog_renderer_get_rectangle_vpath :
 * @x      : upper left corner
 * @y      : upper left corner
 * @width  : rectangle width
 * @height : rectangle height
 *
 * a utility routine to build a rectangle path.
 **/
ArtVpath * 
gog_renderer_get_rectangle_vpath (GogViewAllocation const *rect)
{
	ArtVpath *path;

	path = g_new (ArtVpath, 6);

	path[0].x = path[3].x = path[4].x = rect->x;
	path[1].x = path[2].x = rect->x + rect->w;
	path[0].y = path[1].y = path[4].y = rect->y;
	path[2].y = path[3].y = rect->y + rect->h;
	path[0].code = ART_MOVETO;
	path[1].code = path[2].code = path[3].code = path[4].code = ART_LINETO;
	path[5].code = ART_END;

	return path;
}

/**
 * gog_renderer_get_ring_wedge_vpath :
 * @x : center x coordinate
 * @y : center y coordinate
 * @width  : x radius
 * @height : y radius
 * @th0 : start arc angle
 * @th1 : stop arc angle
 *
 * a utility routine to build a ring wedge path.
 **/
ArtBpath * 
gog_renderer_get_ring_wedge_bpath (double cx, double cy, 
				   double rx_out, double ry_out,
				   double rx_in, double ry_in,
				   double th0, double th1)
{
	ArtBpath *path;
	double th_arc, th_out, th_in, th_delta, t;
	int i, n_segs;
	gboolean fill = rx_in >= 0.0 && ry_in >= 0.0;
	gboolean draw_in, ellipse = FALSE;

 	if (rx_out <= 0.0 || ry_out <= 0.0 || rx_out < rx_in || ry_out < ry_in)
		return NULL;

	draw_in = fill && (rx_in > rx_out / 1E6) && (ry_in > ry_out / 1E6);

	if (th1 < th0) { t = th1; th1 = th0; th0 = t; }
	if (go_add_epsilon (th1 - th0) >= 2 * M_PI) {
		ellipse = TRUE;
		th1 = th0 + 2 * M_PI;
	}

	th_arc = th1 - th0;
	n_segs = ceil (fabs (th_arc / (M_PI * 0.5 + 0.001)));

	path = g_new (ArtBpath, (1 + n_segs) * (draw_in ? 2 : 1) + (fill ? (draw_in ? 2 : 3) : 1));

	path[0].x3 = cx + rx_out * cos (th1);
	path[0].y3 = cy + ry_out * sin (th1);
	path[0].code = ART_MOVETO;

	if (fill && !ellipse) {
		path[n_segs + 1].x3 = cx + rx_in * cos (th0);
		path[n_segs + 1].y3 = cy + ry_in * sin (th0);
		path[n_segs + 1].code = ART_LINETO;
		if (draw_in) {
			/* Ring wedge */
			path[2 * n_segs + 2].x3 = path[0].x3;
			path[2 * n_segs + 2].y3 = path[0].y3;
			path[2 * n_segs + 2].code = ART_LINETO;
			path[2 * n_segs + 3].code = ART_END;
		} else {
			/* Pie wedge */
			path[n_segs + 1].x3 = cx;
			path[n_segs + 1].y3 = cy;
			path[n_segs + 1].code = ART_LINETO;
			path[n_segs + 2].x3 = path[0].x3;
			path[n_segs + 2].y3 = path[0].y3;
			path[n_segs + 2].code = ART_LINETO;
			path[n_segs + 3].code = ART_END;
		}
	} else 
		/* Arc */
		path[n_segs + 1].code = ART_END;

	th_delta = th_arc / n_segs;
	t = - (8.0 / 3.0) * sin (th_delta * 0.25) * sin (th_delta * 0.25) / sin (th_delta * 0.5);
	th_out = th1;
	th_in = th0;
	for (i = 1; i <= n_segs; i++) {	
		path[i].x1 = cx + rx_out * (cos (th_out) - t * sin (th_out));
		path[i].y1 = cy + ry_out * (sin (th_out) + t * cos (th_out));
		path[i].x3 = cx + rx_out * cos (th_out - th_delta);
		path[i].y3 = cy + ry_out * sin (th_out - th_delta);
		path[i].x2 = path[i].x3 + rx_out * t * sin (th_out - th_delta);
		path[i].y2 = path[i].y3 - ry_out * t * cos (th_out - th_delta);
		path[i].code = ART_CURVETO;
		th_out -= th_delta;
		if (draw_in) {
			path[i+n_segs+1].x1 = cx + rx_in * (cos (th_in) - t * sin (th_in));
			path[i+n_segs+1].y1 = cy + ry_in * (sin (th_in) + t * cos (th_in));
			path[i+n_segs+1].x3 = cx + rx_in * cos (th_in + th_delta);
			path[i+n_segs+1].y3 = cy + ry_in * sin (th_in + th_delta);
			path[i+n_segs+1].x2 = path[i+n_segs+1].x3 + rx_in * t * sin (th_in + th_delta);
			path[i+n_segs+1].y2 = path[i+n_segs+1].y3 - ry_in * t * cos (th_in + th_delta);
			path[i+n_segs+1].code = ART_CURVETO;
			th_in += th_delta;
		}
	}
	return path;
}

/**
 * gog_renderer_draw_ring_wedge :
 * @renderer : #GogRenderer
 * @cx : center x coordinate
 * @cy : center y coordinate
 * @rx : x radius
 * @ry : y radius
 * @th0 : start arc angle
 * @th1 : stop arc angle
 *
 * a utility routine to draw an arc.
 **/

void
gog_renderer_draw_ring_wedge (GogRenderer *rend, double cx, double cy,
			      double rx_out, double ry_out, 
			      double rx_in, double ry_in,
			      double th0, double th1,
			      gboolean narrow)
{
	GogRendererClass *klass = GOG_RENDERER_GET_CLASS (rend);
	ArtBpath *path;

	g_return_if_fail (klass != NULL);
	g_return_if_fail (rend->cur_style != NULL);

	path = gog_renderer_get_ring_wedge_bpath (cx, cy, rx_out, ry_out, rx_in, ry_in, th0, th1);
	if (path == NULL)
		return;

	if (rx_in >= 0.0 && ry_in >= 0.0)
		(klass->draw_bezier_polygon) (rend, path, narrow);
	else
		(klass->draw_bezier_path) (rend, path);

	g_free (path);
}

/**
 * gog_renderer_draw_sharp_path :
 * @rend : #GogRenderer
 * @path  : #ArtVpath
 *
 * Draws @path using the outline elements of the current style,
 * trying to make line with sharp edge.
 **/
void
gog_renderer_draw_sharp_path (GogRenderer *rend, ArtVpath *path)
{
	GogRendererClass *klass = GOG_RENDERER_GET_CLASS (rend);

	g_return_if_fail (klass != NULL);
	g_return_if_fail (rend->cur_style != NULL);

	if (klass->sharp_path) {
		(klass->sharp_path) (rend, path, 
			gog_renderer_line_size (rend, rend->cur_style->line.width));
	}

	(klass->draw_path) (rend, path);
}

/**
 * gog_renderer_draw_path :
 * @rend : #GogRenderer
 * @path  : #ArtVpath
 *
 * Draws @path using the outline elements of the current style.
 **/
void
gog_renderer_draw_path (GogRenderer *rend, ArtVpath const *path)
{
	GogRendererClass *klass = GOG_RENDERER_GET_CLASS (rend);

	g_return_if_fail (klass != NULL);
	g_return_if_fail (rend->cur_style != NULL);

	(klass->draw_path) (rend, path);
}

/**
 * gog_renderer_draw_sharp_polygon :
 * @rend : #GogRenderer
 * @path  : #ArtVpath
 * @narrow : if TRUE skip any outline the current style specifies.
 *
 * Draws @path and fills it with the fill elements of the current style,
 * trying to draw line with sharp edge.
 * If @narrow is false it alos outlines it using the outline elements.
 **/
void
gog_renderer_draw_sharp_polygon (GogRenderer *rend, ArtVpath *path, gboolean narrow)
{
	GogRendererClass *klass = GOG_RENDERER_GET_CLASS (rend);

	g_return_if_fail (klass != NULL);
	g_return_if_fail (rend->cur_style != NULL);

	if (klass->sharp_path) {
		(klass->sharp_path) (rend, path,
			gog_renderer_line_size (rend, rend->cur_style->outline.width));
	}

	(klass->draw_polygon) (rend, path, narrow);
}

/**
 * gog_renderer_draw_polygon :
 * @rend : #GogRenderer
 * @path  : #ArtVpath
 * @narrow : if TRUE skip any outline the current style specifies.
 *
 * Draws @path and fills it with the fill elements of the current style.
 * If @narrow is false it alos outlines it using the outline elements.
 **/
void
gog_renderer_draw_polygon (GogRenderer *rend, ArtVpath const *path, gboolean narrow)
{
	GogRendererClass *klass = GOG_RENDERER_GET_CLASS (rend);

	g_return_if_fail (klass != NULL);
	g_return_if_fail (rend->cur_style != NULL);

	(klass->draw_polygon) (rend, path, narrow);
}


/**
 * gog_renderer_draw_bezier_path :
 * @rend : #GogRenderer
 * @path  : #ArtBpath
 *
 * Draws @path using the outline elements of the current style.
 **/
void
gog_renderer_draw_bezier_path (GogRenderer *rend, ArtBpath const *path)
{
	GogRendererClass *klass = GOG_RENDERER_GET_CLASS (rend);

	g_return_if_fail (klass != NULL);
	g_return_if_fail (rend->cur_style != NULL);

	(klass->draw_bezier_path) (rend, path);
}

/**
 * gog_renderer_draw_text :
 * @rend   : #GogRenderer
 * @text   : the string to draw
 * @pos    : #GogViewAllocation
 * @anchor : #GtkAnchorType how to draw relative to @pos
 * @result : an optionally NULL #GogViewAllocation
 *
 * Have @rend draw @text in the at @pos.{x,y} anchored by the @anchor corner.
 * If @pos.w or @pos.h are >= 0 then clip the results to less than that size.
 * If @result is supplied it will recieve the actual position of the result.
 **/
void
gog_renderer_draw_text (GogRenderer *rend, char const *text,
			GogViewAllocation const *pos, GtkAnchorType anchor,
			GogViewAllocation *result)
{
	GogRendererClass *klass = GOG_RENDERER_GET_CLASS (rend);

	g_return_if_fail (klass != NULL);
	g_return_if_fail (rend->cur_style != NULL);
	g_return_if_fail (text != NULL);

	if (*text == '\0') {
		if (result != NULL) {
			result->x = pos->x;
			result->y = pos->y;
			result->w = 0.;
			result->h = 0.;
		}
		return;
	}

	(klass->draw_text) (rend, text, pos, anchor, result);
}

/**
 * gog_renderer_draw_marker :
 * @rend : #GogRenderer
 * @pos  : #ArtPoint
 **/
void
gog_renderer_draw_marker (GogRenderer *rend, double x, double y)
{
	GogRendererClass *klass = GOG_RENDERER_GET_CLASS (rend);

	g_return_if_fail (klass != NULL);
	g_return_if_fail (rend->cur_style != NULL);

	(klass->draw_marker) (rend, x, y);
}

/**
 * gog_renderer_get_text_OBR :
 * @rend : #GogRenderer
 * @text : the string to draw
 * @obr : #GOGeometryOBR to store the Object Bounding Rectangle of @text.
 **/
void
gog_renderer_get_text_OBR (GogRenderer *rend, char const *text, GOGeometryOBR *obr)
{
	GogRendererClass *klass = GOG_RENDERER_GET_CLASS (rend);

	g_return_if_fail (klass != NULL);
	g_return_if_fail (rend->cur_style != NULL);
	g_return_if_fail (text != NULL);
	g_return_if_fail (obr != NULL);

	obr->x = obr->y = 0;
	if (*text == '\0') {
		/* Make sure invisible things don't skew size */
		obr->w = obr->h = 0;
		return;
	}

	(klass->get_text_OBR) (rend, text, obr);

	/* Make sure invisible things don't skew size */
	if (obr->w == 0)
		obr->h = 0;
	else if (obr->h == 0)
		obr->w = 0;
	
	obr->alpha = rend->cur_style->text_layout.angle * M_PI / 180.0;
}

/**
 * gog_renderer_get_text_AABR :
 * @rend : #GogRenderer
 * @text : the string to draw
 * @aabr : #GOGeometryAABR to store the Axis Aligned Bounding Rectangle of @text.
 **/
void
gog_renderer_get_text_AABR (GogRenderer *rend, char const *text, GOGeometryAABR *aabr)
{
	GOGeometryOBR obr;

	gog_renderer_get_text_OBR (rend, text, &obr);
	go_geometry_OBR_to_AABR (&obr, aabr);
}

static void
cb_font_removed (GogRenderer *rend, GOFont const *font)
{
	GogRendererClass *klass = GOG_RENDERER_GET_CLASS (rend);

	g_return_if_fail (klass != NULL);

	gog_debug (0, g_warning ("notify a '%s' that %p is invalid", 
				 G_OBJECT_TYPE_NAME (rend), font););

	if (klass->font_removed)
		(klass->font_removed) (rend, font);
}

static void
gog_renderer_class_init (GogRendererClass *renderer_klass)
{
	GObjectClass *gobject_klass = (GObjectClass *)renderer_klass;

	parent_klass = g_type_class_peek_parent (gobject_klass);
	gobject_klass->finalize	    = gog_renderer_finalize;
	gobject_klass->set_property = gog_renderer_set_property;
	gobject_klass->get_property = gog_renderer_get_property;

	renderer_klass->sharp_path = NULL;
	renderer_klass->line_size = NULL;

	g_object_class_install_property (gobject_klass, RENDERER_PROP_MODEL,
		g_param_spec_object ("model", "model",
			"the GogGraph this renderer displays",
			GOG_GRAPH_TYPE, G_PARAM_READWRITE));
	g_object_class_install_property (gobject_klass, RENDERER_PROP_VIEW,
		g_param_spec_object ("view", "view",
			"the GogView this renderer is displaying",
			GOG_VIEW_TYPE, G_PARAM_READABLE));
	g_object_class_install_property (gobject_klass, RENDERER_PROP_ZOOM,
		g_param_spec_double ("zoom", "zoom Height Pts",
			"global scale factor",
			1., G_MAXDOUBLE, 1., G_PARAM_READWRITE));

	renderer_signals [RENDERER_SIGNAL_REQUEST_UPDATE] = g_signal_new ("request-update",
		G_TYPE_FROM_CLASS (renderer_klass),
		G_SIGNAL_RUN_LAST,
		G_STRUCT_OFFSET (GogRendererClass, request_update),
		NULL, NULL,
		g_cclosure_marshal_VOID__VOID,
		G_TYPE_NONE, 0);
}

static void
gog_renderer_init (GogRenderer *rend)
{
	rend->cur_clip = NULL;
	rend->clip_stack = NULL;

	rend->line_dash = NULL;
	rend->outline_dash = NULL;

	rend->needs_update = FALSE;
	rend->cur_style    = NULL;
	rend->style_stack  = NULL;
	rend->zoom = rend->scale = rend->scale_x = rend->scale_y = 1.;
	rend->font_watcher = g_cclosure_new_swap (G_CALLBACK (cb_font_removed),
		rend, NULL);
	go_font_cache_register (rend->font_watcher);
}

GSF_CLASS (GogRenderer, gog_renderer,
	   gog_renderer_class_init, gog_renderer_init,
	   G_TYPE_OBJECT)

/**
 * gog_renderer_draw_rectangle :
 * @renderer : #GogRenderer
 * @rect : #GogViewAllocation
 *
 * A utility routine to build a vpath in @rect.
 **/
static void
draw_rectangle (GogRenderer *rend, GogViewAllocation const *rect, gboolean sharp)
{
	gboolean const narrow = (rect->w < 3.) || (rect->h < 3.);
	double o, o_2;
	ArtVpath path[6];

	if (!narrow) {
		o = gog_renderer_line_size (rend, rend->cur_style->outline.width);
		o_2 = o / 2.;
	} else
		o = o_2 = 0.;
	path[0].code = ART_MOVETO;
	path[1].code = ART_LINETO;
	path[2].code = ART_LINETO;
	path[3].code = ART_LINETO;
	path[4].code = ART_LINETO;
	path[5].code = ART_END;
	path[0].x = path[1].x = path[4].x = rect->x + o_2;
	path[2].x = path[3].x = path[0].x + rect->w - o;
	path[0].y = path[3].y = path[4].y = rect->y + o_2; 
	path[1].y = path[2].y = path[0].y + rect->h - o; 

	if (sharp)
		gog_renderer_draw_sharp_polygon (rend, path, narrow);
	else
		gog_renderer_draw_polygon (rend, path, narrow);
}

void
gog_renderer_draw_sharp_rectangle (GogRenderer *rend, GogViewAllocation const *rect)
{
	draw_rectangle (rend, rect, TRUE);
}

void
gog_renderer_draw_rectangle (GogRenderer *rend, GogViewAllocation const *rect)
{
	draw_rectangle (rend, rect, FALSE);
}

double
gog_renderer_line_size (GogRenderer const *rend, double width)
{
	GogRendererClass *klass = GOG_RENDERER_GET_CLASS (rend);

	if (klass->line_size)
		return (klass->line_size) (rend, width);
	
	if (go_sub_epsilon (width) <= 0.)
		width = GOG_RENDERER_HAIR_LINE_WIDTH;
	return width * rend->scale;
}

double
gog_renderer_pt2r_x (GogRenderer const *rend, double d)
{
	return d * rend->scale_x;
}

double
gog_renderer_pt2r_y (GogRenderer const *rend, double d)
{
	return d * rend->scale_y;
}

double
gog_renderer_pt2r (GogRenderer const *rend, double d)
{
	return d * rend->scale;
}
