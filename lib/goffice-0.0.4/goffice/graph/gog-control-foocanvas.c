/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-control-foocanvas.c :
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
#include <goffice/graph/gog-control-foocanvas.h>
#include <goffice/graph/gog-graph.h>
#include <goffice/graph/gog-object.h>
#include <goffice/cut-n-paste/foocanvas/foo-canvas-util.h>

#include <gsf/gsf-impl-utils.h>
#include <glib/gi18n.h>
#include <math.h>

enum {
	CTRL_FOO_PROP_0,
	CTRL_FOO_PROP_H,
	CTRL_FOO_PROP_W,
	CTRL_FOO_PROP_MODEL,
	CTRL_FOO_PROP_RENDERER
};

static GObjectClass *parent_klass;

#define GOG_CONTROL_FOOCANVAS_CLASS(k)     (G_TYPE_CHECK_CLASS_CAST ((k), GOG_CONTROL_FOOCANVAS_TYPE, GogControlFooCanvasClass))
#define IS_GOG_CONTROL_FOOCANVAS_CLASS(k)  (G_TYPE_CHECK_CLASS_TYPE ((k), GOG_CONTROL_FOOCANVAS_TYPE))
#define GOG_CONTROL_FOOCANVAS_GET_CLASS(k) (G_TYPE_INSTANCE_GET_CLASS ((k), GOG_CONTROL_FOOCANVAS_TYPE, GogControlFooCanvasClass))

static void
gog_control_foocanvas_set_property (GObject *gobject, guint param_id,
				    GValue const *value, GParamSpec *pspec)
{
	GogControlFooCanvas *ctrl = GOG_CONTROL_FOOCANVAS (gobject);
	gboolean setup_renderer = (ctrl->renderer == NULL);

	switch (param_id) {
	case CTRL_FOO_PROP_H: ctrl->new_h = g_value_get_double (value); break;
	case CTRL_FOO_PROP_W: ctrl->new_w = g_value_get_double (value); break;

	case CTRL_FOO_PROP_MODEL:
		if (ctrl->renderer != NULL)
			g_object_unref (ctrl->renderer);
#ifdef WITH_CAIRO
		ctrl->renderer = g_object_new (GOG_RENDERER_CAIRO_TYPE,
					       "model", g_value_get_object (value),
					       NULL);
#else
		ctrl->renderer = g_object_new (GOG_RENDERER_PIXBUF_TYPE,
					       "model", g_value_get_object (value),
					       NULL);
#endif
		break;

	case CTRL_FOO_PROP_RENDERER:
		if (ctrl->renderer != NULL)
			g_object_unref (ctrl->renderer);
#ifdef WITH_CAIRO
		ctrl->renderer = GOG_RENDERER_CAIRO (g_value_get_object (value));
#else
		ctrl->renderer = GOG_RENDERER_PIXBUF (g_value_get_object (value));
#endif
		if (ctrl->renderer != NULL)
			g_object_ref (ctrl->renderer);
		break;

	default: G_OBJECT_WARN_INVALID_PROPERTY_ID (gobject, param_id, pspec);
		return; /* NOTE : RETURN */
	}

	if (setup_renderer && ctrl->renderer != NULL)
		g_signal_connect_object (G_OBJECT (ctrl->renderer),
			"request_update",
			G_CALLBACK (foo_canvas_item_request_update),
			ctrl, G_CONNECT_SWAPPED);
	foo_canvas_item_request_update (FOO_CANVAS_ITEM (ctrl));
}

static void
gog_control_foocanvas_get_property (GObject *gobject, guint param_id,
				    GValue *value, GParamSpec *pspec)
{
	GogControlFooCanvas *ctrl = GOG_CONTROL_FOOCANVAS (gobject);

	switch (param_id) {
	case CTRL_FOO_PROP_H: g_value_set_double (value, ctrl->new_h); break;
	case CTRL_FOO_PROP_W: g_value_set_double (value, ctrl->new_w); break;
	case CTRL_FOO_PROP_RENDERER  : g_value_set_object (value, ctrl->renderer); break;

	default: G_OBJECT_WARN_INVALID_PROPERTY_ID (gobject, param_id, pspec);
		break;
	}
}

static void
gog_control_foocanvas_finalize (GObject *obj)
{
	GogControlFooCanvas *ctrl = GOG_CONTROL_FOOCANVAS (obj);

	if (ctrl->model != NULL) {
		g_object_unref (ctrl->model);
		ctrl->model = NULL;
	}
	if (ctrl->renderer != NULL) {
		g_object_unref (ctrl->renderer);
		ctrl->renderer = NULL;
	}
	(*parent_klass->finalize) (obj);
}

static void
gog_control_foocanvas_draw (FooCanvasItem *item, GdkDrawable *drawable,
			    GdkEventExpose *ev)
{
	GogControlFooCanvas *ctrl = GOG_CONTROL_FOOCANVAS (item);
#ifdef WITH_CAIRO
	GdkPixbuf *buffer = gog_renderer_cairo_get_pixbuf (ctrl->renderer);
#else
	GdkPixbuf *buffer = gog_renderer_pixbuf_get (ctrl->renderer);
#endif
	GdkRectangle display_rect, draw_rect;
	GdkRegion *draw_region;

	if (buffer) {
		display_rect.x = item->x1;
		display_rect.y = item->y1;
		display_rect.width  = item->x2 - item->x1;
		display_rect.height = item->y2 - item->y1;

		draw_region = gdk_region_rectangle (&display_rect);
		gdk_region_intersect (draw_region, ev->region);
		if (!gdk_region_empty (draw_region)) {
			gdk_region_get_clipbox (draw_region, &draw_rect);
			gdk_draw_pixbuf (drawable, NULL, buffer,
				/* pixbuf 0, 0 is at pix_rect.x, pix_rect.y */
				     draw_rect.x - display_rect.x,
				     draw_rect.y - display_rect.y,
				     draw_rect.x,
				     draw_rect.y,
				     draw_rect.width,
				     draw_rect.height,
				     GDK_RGB_DITHER_NORMAL, 0, 0);
		}
		gdk_region_destroy (draw_region);
	}

	/* we are a canvas group, there could be some children */
	if (FOO_CANVAS_ITEM_CLASS (parent_klass)->draw)
		(FOO_CANVAS_ITEM_CLASS (parent_klass)->draw) (item, drawable, ev);
}

static void
gog_control_foocanvas_update (FooCanvasItem *item,
			      double i2w_dx, double i2w_dy, gint flags)
{
	GogControlFooCanvas *ctrl = GOG_CONTROL_FOOCANVAS (item);
	gboolean redraw;
	int x1, x2, y1, y2;
	int orig_x1 = item->x1, orig_x2 = item->x2, orig_y1 = item->y1, orig_y2 = item->y2;

	if (FOO_CANVAS_ITEM_CLASS (parent_klass)->update)
		(FOO_CANVAS_ITEM_CLASS (parent_klass)->update) (item, i2w_dx, i2w_dy, flags);
	/* foo_canvas_group_update wipes the bbox */
	item->x1 = orig_x1;	item->x2 = orig_x2;
	item->y1 = orig_y1;	item->y2 = orig_y2;

	foo_canvas_w2c (item->canvas, ctrl->base.xpos, ctrl->base.ypos, &x1, &y1);
	foo_canvas_w2c (item->canvas, ctrl->base.xpos + ctrl->new_w, ctrl->base.ypos + ctrl->new_h, &x2, &y2);

#ifdef WITH_CAIRO
	redraw = gog_renderer_cairo_update (ctrl->renderer, x2-x1, y2-y1,
		item->canvas->pixels_per_unit);
#else
	redraw = gog_renderer_pixbuf_update (ctrl->renderer, x2-x1, y2-y1,
		item->canvas->pixels_per_unit);
#endif
	if (item->x1 != x1 || item->y1 != y1 || item->x2 != x2 || item->y2 != y2)
		foo_canvas_update_bbox (FOO_CANVAS_ITEM (ctrl), x1, y1, x2, y2);
	else if (redraw)
		foo_canvas_item_request_redraw (FOO_CANVAS_ITEM (ctrl));
}

static void
gog_control_foocanvas_bounds (FooCanvasItem *item,
			      double *x1, double *y1, double *x2, double *y2)
{
	*x1 = item->x1;
	*x2 = item->x2;
	*y1 = item->y1;
	*y2 = item->y2;
}

static double
gog_control_foocanvas_point (FooCanvasItem *item, double x, double y, int cx, int cy,
			     FooCanvasItem **actual_item)
{
	*actual_item = item;
	return 0.;
}

static void
gog_control_foocanvas_class_init (GogControlFooCanvasClass *klass)
{
	GObjectClass *gobject_klass = (GObjectClass *) klass;
	FooCanvasItemClass *item_klass = (FooCanvasItemClass *) klass;

	parent_klass = g_type_class_peek_parent (klass);

	gobject_klass->set_property = gog_control_foocanvas_set_property;
	gobject_klass->get_property = gog_control_foocanvas_get_property;
	gobject_klass->finalize	    = gog_control_foocanvas_finalize;
	item_klass->draw   = gog_control_foocanvas_draw;
	item_klass->update = gog_control_foocanvas_update;
	item_klass->bounds = gog_control_foocanvas_bounds;
	item_klass->point  = gog_control_foocanvas_point;

	g_object_class_install_property (gobject_klass, CTRL_FOO_PROP_H,
		 g_param_spec_double ("h", _("H"), _("Height"),
			0, G_MAXDOUBLE, 100., G_PARAM_READWRITE));
	g_object_class_install_property (gobject_klass, CTRL_FOO_PROP_W,
		 g_param_spec_double ("w", _("W"), _("Width"),
			0, G_MAXDOUBLE, 100., G_PARAM_READWRITE));
	g_object_class_install_property (gobject_klass, CTRL_FOO_PROP_MODEL,
		g_param_spec_object ("model", "model",
			"the GogObject this object displays",
			GOG_OBJECT_TYPE, G_PARAM_WRITABLE));
#ifdef WITH_CAIRO
	g_object_class_install_property (gobject_klass, CTRL_FOO_PROP_RENDERER,
		g_param_spec_object ("renderer", "renderer",
			"the GogRendererCairo being displayed",
			GOG_RENDERER_CAIRO_TYPE, G_PARAM_READWRITE));
#else
	g_object_class_install_property (gobject_klass, CTRL_FOO_PROP_RENDERER,
		g_param_spec_object ("renderer", "renderer",
			"the GogRendererPixbuf being displayed",
			GOG_RENDERER_PIXBUF_TYPE, G_PARAM_READWRITE));
#endif
}

static void
gog_control_foocanvas_init (GogControlFooCanvas *ctrl)
{
	ctrl->new_h = ctrl->new_w = 0.;
}

GSF_CLASS (GogControlFooCanvas, gog_control_foocanvas,
	   gog_control_foocanvas_class_init, gog_control_foocanvas_init,
	   FOO_TYPE_CANVAS_GROUP)

