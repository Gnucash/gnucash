/* File import from foocanvas to libgoffice by import-foocanvas.  Do not edit.  */

#undef GTK_DISABLE_DEPRECATED
#include <goffice/goffice-config.h>
#include <glib/gi18n.h>
#include <gsf/gsf-impl-utils.h>
/* GNOME libraries - GdkPixbuf item for the GNOME canvas
 *
 * Copyright (C) 1999 The Free Software Foundation
 *
 * Author: Federico Mena-Quintero <federico@gimp.org>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301
 * USA.
 */

#include <math.h>
#include <goffice/cut-n-paste/foocanvas/foo-canvas.h>
#include <goffice/cut-n-paste/foocanvas/foo-canvas-util.h>
#include <gdk-pixbuf/gdk-pixbuf.h>
#include "foo-canvas-pixbuf.h"

/* Private part of the FooCanvasPixbuf structure */
typedef struct {
	/* Our gdk-pixbuf */
	GdkPixbuf *pixbuf, *pixbuf_scaled;

	/* Width value */
	double width;

	/* Height value */
	double height;

	/* X translation */
	double x;

	/* Y translation */
	double y;

	/* Whether dimensions are set and whether they are in pixels or units */
	guint width_set : 1;
	guint width_in_pixels : 1;
	guint height_set : 1;
	guint height_in_pixels : 1;
	guint x_in_pixels : 1;
	guint y_in_pixels : 1;

	/* Whether the pixbuf has changed */
	guint need_pixbuf_update : 1;

	/* Whether the transformation or size have changed */
	guint need_xform_update : 1;

	/* Should the point method ignore transparent areas */
	guint point_ignores_alpha : 1;

	/* Anchor */
	GtkAnchorType anchor;

	/* Approximation method used for transformations */
	GdkInterpType interp_type;

} PixbufPrivate;

/* Object argument IDs */
enum {
	PROP_0,
	PROP_PIXBUF,
	PROP_WIDTH,
	PROP_WIDTH_SET,
	PROP_WIDTH_IN_PIXELS,
	PROP_HEIGHT,
	PROP_HEIGHT_SET,
	PROP_HEIGHT_IN_PIXELS,
	PROP_X,
	PROP_X_IN_PIXELS,
	PROP_Y,
	PROP_Y_IN_PIXELS,
	PROP_ANCHOR,
	PROP_INTERP_TYPE,
	PROP_POINT_IGNORES_ALPHA
};

static void foo_canvas_pixbuf_class_init (FooCanvasPixbufClass *class);
static void foo_canvas_pixbuf_init (FooCanvasPixbuf *cpb);
static void foo_canvas_pixbuf_destroy (GtkObject *object);
static void foo_canvas_pixbuf_set_property (GObject *object,
					    guint param_id,
					    const GValue *value,
					    GParamSpec *pspec);
static void foo_canvas_pixbuf_get_property (GObject *object,
					    guint param_id,
					    GValue *value,
					    GParamSpec *pspec);

static void foo_canvas_pixbuf_update    (FooCanvasItem *item,
					 double i2w_dx, double i2w_dy,
					 int flags);
static void foo_canvas_pixbuf_draw      (FooCanvasItem *item, GdkDrawable *drawable,
					 GdkEventExpose *expose);
static double foo_canvas_pixbuf_point   (FooCanvasItem *item, double x, double y, int cx, int cy,
					 FooCanvasItem **actual_item);
static void foo_canvas_pixbuf_translate (FooCanvasItem *item, double dx, double dy);
static void foo_canvas_pixbuf_bounds    (FooCanvasItem *item,
					 double *x1, double *y1, double *x2, double *y2);

static FooCanvasItemClass *parent_class;



/**
 * foo_canvas_pixbuf_get_type:
 * @void:
 *
 * Registers the #FooCanvasPixbuf class if necessary, and returns the type ID
 * associated to it.
 *
 * Return value: The type ID of the #FooCanvasPixbuf class.
 **/
GtkType
foo_canvas_pixbuf_get_type (void)
{
	static GtkType canvas_pixbuf_type = 0;

	if (!canvas_pixbuf_type) {
		/* FIXME: Convert to gobject style.  */
		static const GtkTypeInfo canvas_pixbuf_info = {
			(char *)"FooCanvasPixbuf",
			sizeof (FooCanvasPixbuf),
			sizeof (FooCanvasPixbufClass),
			(GtkClassInitFunc) foo_canvas_pixbuf_class_init,
			(GtkObjectInitFunc) foo_canvas_pixbuf_init,
			NULL, /* reserved_1 */
			NULL, /* reserved_2 */
			(GtkClassInitFunc) NULL
		};

		canvas_pixbuf_type = gtk_type_unique (foo_canvas_item_get_type (),
						      &canvas_pixbuf_info);
	}

	return canvas_pixbuf_type;
}

/* Class initialization function for the pixbuf canvas item */
static void
foo_canvas_pixbuf_class_init (FooCanvasPixbufClass *class)
{
        GObjectClass *gobject_class;
	GtkObjectClass *object_class;
	FooCanvasItemClass *item_class;

        gobject_class = (GObjectClass *) class;
	object_class = (GtkObjectClass *) class;
	item_class = (FooCanvasItemClass *) class;

	parent_class = gtk_type_class (foo_canvas_item_get_type ());

	gobject_class->set_property = foo_canvas_pixbuf_set_property;
	gobject_class->get_property = foo_canvas_pixbuf_get_property;

        g_object_class_install_property
                (gobject_class,
                 PROP_PIXBUF,
                 g_param_spec_object ("pixbuf", NULL, NULL,
                                      GDK_TYPE_PIXBUF,
                                      GSF_PARAM_STATIC | G_PARAM_READWRITE));
        g_object_class_install_property
                (gobject_class,
                 PROP_WIDTH,
                 g_param_spec_double ("width", NULL, NULL,
				      -G_MAXDOUBLE, G_MAXDOUBLE, 0,
				      GSF_PARAM_STATIC | G_PARAM_READWRITE));
        g_object_class_install_property
                (gobject_class,
                 PROP_WIDTH_SET,
                 g_param_spec_boolean ("width-set", NULL, NULL,
				       FALSE,
				       GSF_PARAM_STATIC | G_PARAM_READWRITE));
        g_object_class_install_property
                (gobject_class,
                 PROP_WIDTH_IN_PIXELS,
                 g_param_spec_boolean ("width-in-pixels", NULL, NULL,
				       FALSE,
				       GSF_PARAM_STATIC | G_PARAM_READWRITE));
        g_object_class_install_property
                (gobject_class,
                 PROP_HEIGHT,
                 g_param_spec_double ("height", NULL, NULL,
				      -G_MAXDOUBLE, G_MAXDOUBLE, 0,
				      GSF_PARAM_STATIC | G_PARAM_READWRITE));
        g_object_class_install_property
                (gobject_class,
                 PROP_HEIGHT_SET,
                 g_param_spec_boolean ("height-set", NULL, NULL,
				       FALSE,
				       GSF_PARAM_STATIC | G_PARAM_READWRITE));
        g_object_class_install_property
                (gobject_class,
                 PROP_HEIGHT_IN_PIXELS,
                 g_param_spec_boolean ("height-in-pixels", NULL, NULL,
				       FALSE,
				       GSF_PARAM_STATIC | G_PARAM_READWRITE));
        g_object_class_install_property
                (gobject_class,
                 PROP_X,
                 g_param_spec_double ("x", NULL, NULL,
				      -G_MAXDOUBLE, G_MAXDOUBLE, 0,
                                    GSF_PARAM_STATIC | G_PARAM_READWRITE));
        g_object_class_install_property
                (gobject_class,
                 PROP_X_IN_PIXELS,
                 g_param_spec_boolean ("x-in-pixels", NULL, NULL,
				       FALSE,
				       GSF_PARAM_STATIC | G_PARAM_READWRITE));
        g_object_class_install_property
                (gobject_class,
                 PROP_Y,
                 g_param_spec_double ("y", NULL, NULL,
				      -G_MAXDOUBLE, G_MAXDOUBLE, 0,
				      GSF_PARAM_STATIC | G_PARAM_READWRITE));
        g_object_class_install_property
                (gobject_class,
                 PROP_Y_IN_PIXELS,
                 g_param_spec_boolean ("y-in-pixels", NULL, NULL,
				       FALSE,
				       GSF_PARAM_STATIC | G_PARAM_READWRITE));
        g_object_class_install_property
                (gobject_class,
                 PROP_ANCHOR,
                 g_param_spec_enum ("anchor", NULL, NULL,
                                    GTK_TYPE_ANCHOR_TYPE,
                                    GTK_ANCHOR_NW,
                                    GSF_PARAM_STATIC | G_PARAM_READWRITE));
        g_object_class_install_property
                (gobject_class,
                 PROP_INTERP_TYPE,
                 g_param_spec_enum ("interp-type", NULL, NULL,
                                    GDK_TYPE_INTERP_TYPE,
                                    GDK_INTERP_BILINEAR,
                                    GSF_PARAM_STATIC | G_PARAM_READWRITE));
        g_object_class_install_property
                (gobject_class,
		 PROP_POINT_IGNORES_ALPHA,
                 g_param_spec_boolean ("point-ignores-alpha", NULL, NULL,
				       FALSE,
				       GSF_PARAM_STATIC | G_PARAM_READWRITE));

	object_class->destroy = foo_canvas_pixbuf_destroy;

	item_class->update = foo_canvas_pixbuf_update;
	item_class->draw = foo_canvas_pixbuf_draw;
	item_class->point = foo_canvas_pixbuf_point;
	item_class->translate = foo_canvas_pixbuf_translate;
	item_class->bounds = foo_canvas_pixbuf_bounds;
}

/* Object initialization function for the pixbuf canvas item */
static void
foo_canvas_pixbuf_init (FooCanvasPixbuf *gcp)
{
	PixbufPrivate *priv;

	priv = g_new0 (PixbufPrivate, 1);
	gcp->priv = priv;

	priv->width = 0.0;
	priv->height = 0.0;
	priv->x = 0.0;
	priv->y = 0.0;
	priv->anchor = GTK_ANCHOR_NW;
	priv->interp_type = GDK_INTERP_BILINEAR;
	priv->point_ignores_alpha = FALSE;
}

/* Destroy handler for the pixbuf canvas item */
static void
foo_canvas_pixbuf_destroy (GtkObject *object)
{
	FooCanvasItem *item;
	FooCanvasPixbuf *gcp;
	PixbufPrivate *priv;

	g_return_if_fail (object != NULL);
	g_return_if_fail (FOO_IS_CANVAS_PIXBUF (object));

	item = FOO_CANVAS_ITEM (object);
	gcp = (FOO_CANVAS_PIXBUF (object));
	priv = gcp->priv;

	/* remember, destroy can be run multiple times! */

	if (priv) {
	    foo_canvas_item_request_redraw (item);

	    if (priv->pixbuf)
		g_object_unref (priv->pixbuf);
	    if (priv->pixbuf_scaled)
		g_object_unref (priv->pixbuf_scaled);

	    g_free (priv);
	    gcp->priv = NULL;
	}

	if (GTK_OBJECT_CLASS (parent_class)->destroy)
		(* GTK_OBJECT_CLASS (parent_class)->destroy) (object);
}



/* Set_property handler for the pixbuf canvas item */
static void
foo_canvas_pixbuf_set_property (GObject            *object,
				guint               param_id,
				const GValue       *value,
				GParamSpec         *pspec)
{
	FooCanvasItem *item;
	FooCanvasPixbuf *gcp;
	PixbufPrivate *priv;
	GdkPixbuf *pixbuf;
	double val;

	g_return_if_fail (object != NULL);
	g_return_if_fail (FOO_IS_CANVAS_PIXBUF (object));

	item = FOO_CANVAS_ITEM (object);
	gcp = FOO_CANVAS_PIXBUF (object);
	priv = gcp->priv;

	switch (param_id) {
	case PROP_PIXBUF:
		if (g_value_get_object (value))
			pixbuf = GDK_PIXBUF (g_value_get_object (value));
		else
			pixbuf = NULL;
		if (pixbuf != priv->pixbuf) {
			if (pixbuf) {
				g_return_if_fail
				    (gdk_pixbuf_get_colorspace (pixbuf) == GDK_COLORSPACE_RGB);
				g_return_if_fail
				    (gdk_pixbuf_get_n_channels (pixbuf) == 3
				     || gdk_pixbuf_get_n_channels (pixbuf) == 4);
				g_return_if_fail
				    (gdk_pixbuf_get_bits_per_sample (pixbuf) == 8);

				g_object_ref (pixbuf);
			}

			if (priv->pixbuf)
				g_object_unref (priv->pixbuf);
			priv->pixbuf = pixbuf;

			if (priv->pixbuf_scaled) {
				g_object_unref (priv->pixbuf_scaled);
				priv->pixbuf_scaled = NULL;
			}
		}

		priv->need_pixbuf_update = TRUE;
		foo_canvas_item_request_update (item);
		break;

	case PROP_WIDTH:
		val = g_value_get_double (value);
		g_return_if_fail (val >= 0.0);
		priv->width = val;
		priv->need_xform_update = TRUE;
		foo_canvas_item_request_update (item);
		break;

	case PROP_WIDTH_SET:
		priv->width_set = g_value_get_boolean (value);
		priv->need_xform_update = TRUE;
		foo_canvas_item_request_update (item);
		break;

	case PROP_WIDTH_IN_PIXELS:
		priv->width_in_pixels = g_value_get_boolean (value);
		priv->need_xform_update = TRUE;
		foo_canvas_item_request_update (item);
		break;

	case PROP_HEIGHT:
		val = g_value_get_double (value);
		g_return_if_fail (val >= 0.0);
		priv->height = val;
		priv->need_xform_update = TRUE;
		foo_canvas_item_request_update (item);
		break;

	case PROP_HEIGHT_SET:
		priv->height_set = g_value_get_boolean (value);
		priv->need_xform_update = TRUE;
		foo_canvas_item_request_update (item);
		break;

	case PROP_HEIGHT_IN_PIXELS:
		priv->height_in_pixels = g_value_get_boolean (value);
		priv->need_xform_update = TRUE;
		foo_canvas_item_request_update (item);
		break;

	case PROP_X:
		priv->x = g_value_get_double (value);
		priv->need_xform_update = TRUE;
		foo_canvas_item_request_update (item);
		break;

	case PROP_X_IN_PIXELS:
		priv->x_in_pixels = g_value_get_boolean (value);
		priv->need_xform_update = TRUE;
		foo_canvas_item_request_update (item);
		break;

	case PROP_Y:
		priv->y = g_value_get_double (value);
		priv->need_xform_update = TRUE;
		foo_canvas_item_request_update (item);
		break;

	case PROP_Y_IN_PIXELS:
		priv->y_in_pixels = g_value_get_boolean (value);
		priv->need_xform_update = TRUE;
		foo_canvas_item_request_update (item);
		break;

	case PROP_ANCHOR:
		priv->anchor = g_value_get_enum (value);
		priv->need_xform_update = TRUE;
		foo_canvas_item_request_update (item);
		break;

	case PROP_INTERP_TYPE:
		priv->interp_type = g_value_get_enum (value);
		priv->need_xform_update = TRUE;
		foo_canvas_item_request_update (item);
		break;

	case PROP_POINT_IGNORES_ALPHA:
		priv->point_ignores_alpha = g_value_get_boolean (value);
		break;

	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
		break;
	}
}

/* Get_property handler for the pixbuf canvasi item */
static void
foo_canvas_pixbuf_get_property (GObject            *object,
				guint               param_id,
				GValue             *value,
				GParamSpec         *pspec)
{
	FooCanvasPixbuf *gcp;
	PixbufPrivate *priv;

	g_return_if_fail (object != NULL);
	g_return_if_fail (FOO_IS_CANVAS_PIXBUF (object));

	gcp = FOO_CANVAS_PIXBUF (object);
	priv = gcp->priv;

	switch (param_id) {
	case PROP_PIXBUF:
		g_value_set_object (value, G_OBJECT (priv->pixbuf));
		break;

	case PROP_WIDTH:
		g_value_set_double (value, priv->width);
		break;

	case PROP_WIDTH_SET:
		g_value_set_boolean (value, priv->width_set);
		break;

	case PROP_WIDTH_IN_PIXELS:
		g_value_set_boolean (value, priv->width_in_pixels);
		break;

	case PROP_HEIGHT:
		g_value_set_double (value, priv->height);
		break;

	case PROP_HEIGHT_SET:
		g_value_set_boolean (value, priv->height_set);
		break;

	case PROP_HEIGHT_IN_PIXELS:
		g_value_set_boolean (value, priv->height_in_pixels);
		break;

	case PROP_X:
		g_value_set_double (value, priv->x);
		break;

	case PROP_X_IN_PIXELS:
		g_value_set_boolean (value, priv->x_in_pixels);
		break;

	case PROP_Y:
		g_value_set_double (value, priv->y);
		break;

	case PROP_Y_IN_PIXELS:
		g_value_set_boolean (value, priv->y_in_pixels);
		break;

	case PROP_ANCHOR:
		g_value_set_enum (value, priv->anchor);
		break;

	case PROP_INTERP_TYPE:
		g_value_set_enum (value, priv->interp_type);
		break;

	case PROP_POINT_IGNORES_ALPHA:
		g_value_set_boolean (value, priv->point_ignores_alpha);
		break;

	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
		break;
	}
}



/* Bounds and utilities */


/* Recomputes the bounding box of a pixbuf canvas item.  The horizontal and
 * vertical dimensions may be specified in units or pixels, separately, so we
 * have to compute the components individually for each dimension.
 *
 * Returns the coordinates with respect to the parent items coordinates.
 */
static void
compute_bounding_box (FooCanvasPixbuf *gcp,
		      double i2w_dx, double i2w_dy,
		      double *bbox_x0, double *bbox_y0,
		      double *bbox_x1, double *bbox_y1)
{
	FooCanvasItem *item;
	PixbufPrivate *priv;
	double x, y;
	double width, height;

	item = FOO_CANVAS_ITEM (gcp);
	priv = gcp->priv;

	if (!priv->pixbuf) {
		*bbox_x0 = *bbox_y0 = *bbox_x1 = *bbox_y1 = 0.0;
		return;
	}

	if (priv->x_in_pixels) {
		x = i2w_dx + priv->x / item->canvas->pixels_per_unit;
	} else {
		x = i2w_dx + priv->x;
	}

	if (priv->y_in_pixels) {
		y = i2w_dy + priv->y / item->canvas->pixels_per_unit;
	} else {
		y = i2w_dy + priv->y;
	}

	if (priv->width_set) {
		width = priv->width;
	} else {
		width = gdk_pixbuf_get_width (priv->pixbuf);
	}

	if (priv->width_in_pixels)
		width /= item->canvas->pixels_per_unit;

	if (priv->height_set) {
		height = priv->height;
	} else {
		height = gdk_pixbuf_get_height (priv->pixbuf);
	}

	if (priv->height_in_pixels)
		height /= item->canvas->pixels_per_unit;


	switch (priv->anchor) {
	case GTK_ANCHOR_NW:
	case GTK_ANCHOR_W:
	case GTK_ANCHOR_SW:
		break;

	case GTK_ANCHOR_N:
	case GTK_ANCHOR_CENTER:
	case GTK_ANCHOR_S:
		x -= width / 2.0;
		break;

	case GTK_ANCHOR_NE:
	case GTK_ANCHOR_E:
	case GTK_ANCHOR_SE:
		x -= width;
		break;

        default:
                break;
	}

	switch (priv->anchor) {
	case GTK_ANCHOR_NW:
	case GTK_ANCHOR_N:
	case GTK_ANCHOR_NE:
		break;

	case GTK_ANCHOR_W:
	case GTK_ANCHOR_CENTER:
	case GTK_ANCHOR_E:
		y -= height / 2.0;
		break;

	case GTK_ANCHOR_SW:
	case GTK_ANCHOR_S:
	case GTK_ANCHOR_SE:
		y -= height;
		break;

        default:
                break;
	}

	*bbox_x0 = x;
	*bbox_y0 = y;
	*bbox_x1 = x + width;
	*bbox_y1 = y + height;
}



/* Update sequence */

/* Update handler for the pixbuf canvas item */
static void
foo_canvas_pixbuf_update (FooCanvasItem *item,
			    double i2w_dx, double i2w_dy,
			    int flags)
{
	FooCanvasPixbuf *gcp;
	PixbufPrivate *priv;
	double bbox_x0, bbox_y0, bbox_x1, bbox_y1;
	int w, h;

	gcp = FOO_CANVAS_PIXBUF (item);
	priv = gcp->priv;

	if (parent_class->update)
		(* parent_class->update) (item, i2w_dx, i2w_dy, flags);

	/* If we need a pixbuf update, or if the item changed visibility to
	 * shown, recompute the bounding box.
	 */
	if (priv->need_pixbuf_update || priv->need_xform_update ||
	    (flags & FOO_CANVAS_UPDATE_DEEP)) {

		foo_canvas_item_request_redraw (item);

		compute_bounding_box (gcp, i2w_dx, i2w_dy,
				      &bbox_x0, &bbox_y0,
				      &bbox_x1, &bbox_y1);

		foo_canvas_w2c_d (item->canvas,
				    bbox_x0, bbox_y0,
				    &item->x1, &item->y1);

		foo_canvas_w2c_d (item->canvas,
				    bbox_x1, bbox_y1,
				    &item->x2, &item->y2);

		item->x1 = floor (item->x1);
		item->y1 = floor (item->y1);
		item->x2 = ceil (item->x2);
		item->y2 = ceil (item->y2);

#ifdef FOO_CANVAS_PIXBUF_VERBOSE
		g_print ("BBox is %g %g %g %g\n", item->x1, item->y1, item->x2, item->y2);
#endif

		if (priv->pixbuf) {
			w = item->x2 - item->x1;
			h = item->y2 - item->y1;

			if (priv->pixbuf_scaled)
				g_object_unref (priv->pixbuf_scaled);
			if (gdk_pixbuf_get_width (priv->pixbuf) != w ||
			    gdk_pixbuf_get_height (priv->pixbuf) != h)
				priv->pixbuf_scaled = gdk_pixbuf_scale_simple (
					priv->pixbuf, w, h, priv->interp_type);
			else
				priv->pixbuf_scaled = g_object_ref (priv->pixbuf);
		}

		foo_canvas_item_request_redraw (item);

		priv->need_pixbuf_update = FALSE;
		priv->need_xform_update = FALSE;
	}
}



/* Draw handler for the pixbuf canvas item */
static void
foo_canvas_pixbuf_draw (FooCanvasItem *item, GdkDrawable *drawable,
			  GdkEventExpose *expose)
{
	FooCanvasPixbuf *gcp;
	PixbufPrivate *priv;
	GdkRectangle display_rect, draw_rect;
	GdkRegion *draw_region;
	int w, h;

	gcp = FOO_CANVAS_PIXBUF (item);
	priv = gcp->priv;

	if (!priv->pixbuf)
		return;

	/* Compute the area we need to repaint */

	w = item->x2 - item->x1;
	h = item->y2 - item->y1;

	display_rect.x = item->x1;
	display_rect.y = item->y1;
	display_rect.width  = w;
	display_rect.height = h;
	draw_region = gdk_region_rectangle (&display_rect);
	gdk_region_intersect (draw_region, expose->region);
	if (!gdk_region_empty (draw_region)) {
		gdk_region_get_clipbox (draw_region, &draw_rect);
		gdk_draw_pixbuf (drawable, NULL, priv->pixbuf_scaled,
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




/* Point handler for the pixbuf canvas item */
static double
foo_canvas_pixbuf_point (FooCanvasItem *item, double x, double y, int cx, int cy,
			   FooCanvasItem **actual_item)
{
	FooCanvasPixbuf *gcp;
	PixbufPrivate *priv;
	double x1, y1, x2, y2;
	int px, py;
	double no_hit;
	guchar *src;
	GdkPixbuf *pixbuf;

	gcp = FOO_CANVAS_PIXBUF (item);
	priv = gcp->priv;
	pixbuf = priv->pixbuf;

	*actual_item = item;

	no_hit = item->canvas->pixels_per_unit * 2 + 10;

	if (!priv->pixbuf)
		return no_hit;

	compute_bounding_box (gcp, 0.0, 0.0,
			      &x1, &y1, &x2, &y2);


	if (x < x1 || x >= x2 ||
	    y < y1 || y >= y2)
		return no_hit;

	if (!gdk_pixbuf_get_has_alpha (pixbuf) || priv->point_ignores_alpha)
		return 0.0;

	px = (x - x1) * gdk_pixbuf_get_width (pixbuf) / (x2 - x1);
	py = (y - y1) * gdk_pixbuf_get_height (pixbuf) / (y2 - y1);

	src = gdk_pixbuf_get_pixels (pixbuf) +
		py * gdk_pixbuf_get_rowstride (pixbuf) +
		px * gdk_pixbuf_get_n_channels (pixbuf);

	if (src[3] < 128)
		return no_hit;
	else
		return 0.0;
}



static void
foo_canvas_pixbuf_translate (FooCanvasItem *item, double dx, double dy)
{
	FooCanvasPixbuf *gcp;
	PixbufPrivate *priv;

	gcp = FOO_CANVAS_PIXBUF (item);
	priv = gcp->priv;

	if (priv->x_in_pixels) {
		priv->x += dx * item->canvas->pixels_per_unit;
	} else {
		priv->x += dx;
	}

	if (priv->y_in_pixels) {
		priv->y += dy * item->canvas->pixels_per_unit;
	} else {
		priv->y += dy;
	}

	priv->need_xform_update = TRUE;
}



/* Bounds handler for the pixbuf canvas item */
static void
foo_canvas_pixbuf_bounds (FooCanvasItem *item, double *x1, double *y1, double *x2, double *y2)
{
	FooCanvasPixbuf *gcp;
	PixbufPrivate *priv;

	gcp = FOO_CANVAS_PIXBUF (item);
	priv = gcp->priv;

	if (!priv->pixbuf) {
		*x1 = *y1 = *x2 = *y2 = 0.0;
		return;
	}

	compute_bounding_box (gcp, 0.0, 0.0,
			      x1, y1, x2, y2);
}
