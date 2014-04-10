/* File import from foocanvas to libgoffice by import-foocanvas.  Do not edit.  */

#include <goffice/goffice-config.h>
#include <glib/gi18n.h>
#include <gsf/gsf-impl-utils.h>
/*
 * Copyright (C) 1997, 1998, 1999, 2000 Free Software Foundation
 * All rights reserved.
 *
 * This file is part of the Gnome Library.
 *
 * The Gnome Library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * The Gnome Library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with the Gnome Library; see the file COPYING.LIB.  If
 * not, write to the Free Software Foundation, Inc., 51 Franklin St,
 * Fifth Floor, Boston, MA  02110-1301 USA.
 */
/*
  @NOTATION@
 */
/* Rectangle and ellipse item types for FooCanvas widget
 *
 * FooCanvas is basically a port of the Tk toolkit's most excellent canvas widget.  Tk is
 * copyrighted by the Regents of the University of California, Sun Microsystems, and other parties.
 *
 *
 * Author: Federico Mena <federico@nuclecu.unam.mx>
 */

#include <math.h>
#include "foo-canvas-rect-ellipse.h"
#include "foo-canvas-util.h"
#include <string.h>

#ifdef HAVE_RENDER
#include <gdk/gdkx.h>
#include <X11/extensions/Xrender.h>
#endif

/* Base class for rectangle and ellipse item types */

#define noVERBOSE

enum {
	PROP_0,
	PROP_X1,
	PROP_Y1,
	PROP_X2,
	PROP_Y2,
	PROP_FILL_COLOR,
	PROP_FILL_COLOR_GDK,
	PROP_FILL_COLOR_RGBA,
	PROP_OUTLINE_COLOR,
	PROP_OUTLINE_COLOR_GDK,
	PROP_OUTLINE_COLOR_RGBA,
	PROP_FILL_STIPPLE,
	PROP_OUTLINE_STIPPLE,
	PROP_WIDTH_PIXELS,
	PROP_WIDTH_UNITS
};


static void foo_canvas_re_class_init (FooCanvasREClass *class);
static void foo_canvas_re_init       (FooCanvasRE      *re);
static void foo_canvas_re_destroy    (GtkObject          *object);
static void foo_canvas_re_set_property (GObject              *object,
					  guint                 param_id,
					  const GValue         *value,
					  GParamSpec           *pspec);
static void foo_canvas_re_get_property (GObject              *object,
					  guint                 param_id,
					  GValue               *value,
					  GParamSpec           *pspec);

static void foo_canvas_re_update_shared (FooCanvasItem *item,
					   double i2w_dx, double i2w_dy, int flags);
static void foo_canvas_re_realize     (FooCanvasItem *item);
static void foo_canvas_re_unrealize   (FooCanvasItem *item);
static void foo_canvas_re_bounds      (FooCanvasItem *item, double *x1, double *y1, double *x2, double *y2);
static void foo_canvas_re_translate   (FooCanvasItem *item, double dx, double dy);
static void foo_canvas_rect_update      (FooCanvasItem *item, double i2w_dx, double i2w_dy, int flags);
static void foo_canvas_ellipse_update      (FooCanvasItem *item, double i2w_dx, double i2w_dy, int flags);

typedef struct {
  /*< public >*/
  int x0, y0, x1, y1;
}  Rect;

static Rect make_rect (int x0, int y0, int x1, int y1);
static void  diff_rects (Rect r1, Rect r2, int *count, Rect result[4]);

static FooCanvasItemClass *re_parent_class;
static FooCanvasREClass *rect_parent_class;


GType
foo_canvas_re_get_type (void)
{
	static GType re_type = 0;

	if (!re_type) {
		GTypeInfo re_info = {
		  sizeof (FooCanvasREClass),
		  (GBaseInitFunc) NULL,
		  (GBaseFinalizeFunc) NULL,
		  (GClassInitFunc) foo_canvas_re_class_init,
		  NULL,           /* class_finalize */
		  NULL,           /* class_data */
		  sizeof (FooCanvasRE),
		  0,              /* n_preallocs */
		  (GInstanceInitFunc) foo_canvas_re_init
		};

		re_type = g_type_register_static (foo_canvas_item_get_type (),
						  "FooCanvasRE",
						  &re_info,
						  0);
	}

	return re_type;
}

static void
foo_canvas_re_class_init (FooCanvasREClass *class)
{
	GObjectClass *gobject_class;
	GtkObjectClass *object_class;
	FooCanvasItemClass *item_class;

	gobject_class = (GObjectClass *) class;
	object_class = (GtkObjectClass *) class;
	item_class = (FooCanvasItemClass *) class;

	re_parent_class = g_type_class_peek_parent (class);

	gobject_class->set_property = foo_canvas_re_set_property;
	gobject_class->get_property = foo_canvas_re_get_property;

        g_object_class_install_property
                (gobject_class,
                 PROP_X1,
                 g_param_spec_double ("x1", NULL, NULL,
				      -G_MAXDOUBLE, G_MAXDOUBLE, 0,
				      GSF_PARAM_STATIC | G_PARAM_READWRITE));
        g_object_class_install_property
                (gobject_class,
                 PROP_Y1,
                 g_param_spec_double ("y1", NULL, NULL,
				      -G_MAXDOUBLE, G_MAXDOUBLE, 0,
				      GSF_PARAM_STATIC | G_PARAM_READWRITE));
        g_object_class_install_property
                (gobject_class,
                 PROP_X2,
                 g_param_spec_double ("x2", NULL, NULL,
				      -G_MAXDOUBLE, G_MAXDOUBLE, 0,
				      GSF_PARAM_STATIC | G_PARAM_READWRITE));
        g_object_class_install_property
                (gobject_class,
                 PROP_Y2,
                 g_param_spec_double ("y2", NULL, NULL,
				      -G_MAXDOUBLE, G_MAXDOUBLE, 0,
				      GSF_PARAM_STATIC | G_PARAM_READWRITE));
        g_object_class_install_property
                (gobject_class,
                 PROP_FILL_COLOR,
                 g_param_spec_string ("fill-color", NULL, NULL,
                                      NULL,
                                      GSF_PARAM_STATIC | G_PARAM_READWRITE));
        g_object_class_install_property
                (gobject_class,
                 PROP_FILL_COLOR_GDK,
                 g_param_spec_boxed ("fill-color-gdk", NULL, NULL,
				     GDK_TYPE_COLOR,
				     GSF_PARAM_STATIC | G_PARAM_READWRITE));
        g_object_class_install_property
                (gobject_class,
                 PROP_FILL_COLOR_RGBA,
                 g_param_spec_uint ("fill-color-rgba", NULL, NULL,
				    0, G_MAXUINT, 0,
				    GSF_PARAM_STATIC | G_PARAM_READWRITE));
        g_object_class_install_property
                (gobject_class,
                 PROP_FILL_STIPPLE,
                 g_param_spec_object ("fill-stipple", NULL, NULL,
                                      GDK_TYPE_DRAWABLE,
                                      GSF_PARAM_STATIC | G_PARAM_READWRITE));
        g_object_class_install_property
                (gobject_class,
                 PROP_OUTLINE_COLOR,
                 g_param_spec_string ("outline-color", NULL, NULL,
                                      NULL,
                                      GSF_PARAM_STATIC | G_PARAM_READWRITE));
        g_object_class_install_property
                (gobject_class,
                 PROP_OUTLINE_COLOR_GDK,
                 g_param_spec_boxed ("outline-color-gdk", NULL, NULL,
				     GDK_TYPE_COLOR,
				     GSF_PARAM_STATIC | G_PARAM_READWRITE));
        g_object_class_install_property
                (gobject_class,
                 PROP_OUTLINE_COLOR_RGBA,
                 g_param_spec_uint ("outline-color-rgba", NULL, NULL,
				    0, G_MAXUINT, 0,
				    GSF_PARAM_STATIC | G_PARAM_READWRITE));
        g_object_class_install_property
                (gobject_class,
                 PROP_OUTLINE_STIPPLE,
                 g_param_spec_object ("outline-stipple", NULL, NULL,
                                      GDK_TYPE_DRAWABLE,
                                      GSF_PARAM_STATIC | G_PARAM_READWRITE));
        g_object_class_install_property
                (gobject_class,
                 PROP_WIDTH_PIXELS,
                 g_param_spec_uint ("width-pixels", NULL, NULL,
				    0, G_MAXUINT, 0,
				    GSF_PARAM_STATIC | G_PARAM_READWRITE));
        g_object_class_install_property
                (gobject_class,
                 PROP_WIDTH_UNITS,
                 g_param_spec_double ("width-units", NULL, NULL,
				      0.0, G_MAXDOUBLE, 0.0,
				      GSF_PARAM_STATIC | G_PARAM_READWRITE));

	object_class->destroy = foo_canvas_re_destroy;

	item_class->realize = foo_canvas_re_realize;
	item_class->unrealize = foo_canvas_re_unrealize;
	item_class->translate = foo_canvas_re_translate;
	item_class->bounds = foo_canvas_re_bounds;
}

static void
foo_canvas_re_init (FooCanvasRE *re)
{
	re->x1 = 0.0;
	re->y1 = 0.0;
	re->x2 = 0.0;
	re->y2 = 0.0;
	re->width = 0.0;
}

static void
foo_canvas_re_destroy (GtkObject *object)
{
	FooCanvasRE *re;

	g_return_if_fail (object != NULL);
	g_return_if_fail (FOO_IS_CANVAS_RE (object));

	re = FOO_CANVAS_RE (object);

	/* remember, destroy can be run multiple times! */

	if (re->fill_stipple)
		g_object_unref (re->fill_stipple);
	re->fill_stipple = NULL;

	if (re->outline_stipple)
		g_object_unref (re->outline_stipple);
	re->outline_stipple = NULL;

	if (GTK_OBJECT_CLASS (re_parent_class)->destroy)
		(* GTK_OBJECT_CLASS (re_parent_class)->destroy) (object);
}

static void get_bounds (FooCanvasRE *re, double *px1, double *py1, double *px2, double *py2)
{
	FooCanvasItem *item;
	double x1, y1, x2, y2;
	int cx1, cy1, cx2, cy2;
	double hwidth;

#ifdef VERBOSE
	g_print ("re get_bounds\n");
#endif
	item = FOO_CANVAS_ITEM (re);

	if (re->width_pixels)
		hwidth = (re->width / item->canvas->pixels_per_unit) / 2.0;
	else
		hwidth = re->width / 2.0;

	x1 = re->x1;
	y1 = re->y1;
	x2 = re->x2;
	y2 = re->y2;

	foo_canvas_item_i2w (item, &x1, &y1);
	foo_canvas_item_i2w (item, &x2, &y2);
	foo_canvas_w2c (item->canvas, x1 - hwidth, y1 - hwidth, &cx1, &cy1);
	foo_canvas_w2c (item->canvas, x2 + hwidth, y2 + hwidth, &cx2, &cy2);
	*px1 = cx1;
	*py1 = cy1;
	*px2 = cx2;
	*py2 = cy2;

	/* Some safety fudging */

	*px1 -= 2;
	*py1 -= 2;
	*px2 += 2;
	*py2 += 2;
}

/* Convenience function to set a GC's foreground color to the specified pixel value */
static void
set_gc_foreground (GdkGC *gc, gulong pixel)
{
	GdkColor c;

	if (!gc)
		return;

	c.pixel = pixel;
	gdk_gc_set_foreground (gc, &c);
}

/* Sets the stipple pattern for the specified gc */
static void
set_stipple (GdkGC *gc, GdkBitmap **internal_stipple, GdkBitmap *stipple, int reconfigure)
{
	if (*internal_stipple && !reconfigure)
		g_object_unref (*internal_stipple);

	*internal_stipple = stipple;
	if (stipple && !reconfigure)
		g_object_ref (stipple);

	if (gc) {
		if (stipple) {
			gdk_gc_set_stipple (gc, stipple);
			gdk_gc_set_fill (gc, GDK_STIPPLED);
		} else
			gdk_gc_set_fill (gc, GDK_SOLID);
	}
}

/* Recalculate the outline width of the rectangle/ellipse and set it in its GC */
static void
set_outline_gc_width (FooCanvasRE *re)
{
	int width;

	if (!re->outline_gc)
		return;

	if (re->width_pixels)
		width = (int) re->width;
	else
		width = (int) (re->width * re->item.canvas->pixels_per_unit + 0.5);

	gdk_gc_set_line_attributes (re->outline_gc, width,
				    GDK_LINE_SOLID, GDK_CAP_PROJECTING, GDK_JOIN_MITER);
}

static void
foo_canvas_re_set_fill (FooCanvasRE *re, gboolean fill_set)
{
	if (re->fill_set != fill_set) {
		re->fill_set = fill_set;
		foo_canvas_item_request_update (FOO_CANVAS_ITEM (re));
	}
}

static void
foo_canvas_re_set_outline (FooCanvasRE *re, gboolean outline_set)
{
	if (re->outline_set != outline_set) {
		re->outline_set = outline_set;
		foo_canvas_item_request_update (FOO_CANVAS_ITEM (re));
	}
}

static void
foo_canvas_re_set_property (GObject              *object,
			      guint                 param_id,
			      const GValue         *value,
			      GParamSpec           *pspec)
{
	FooCanvasItem *item;
	FooCanvasRE *re;
	GdkColor color = { 0, 0, 0, 0, };
	GdkColor *pcolor;
	int have_pixel;

	g_return_if_fail (object != NULL);
	g_return_if_fail (FOO_IS_CANVAS_RE (object));

	item = FOO_CANVAS_ITEM (object);
	re = FOO_CANVAS_RE (object);
	have_pixel = FALSE;

	switch (param_id) {
	case PROP_X1:
		re->x1 = g_value_get_double (value);

		foo_canvas_item_request_update (item);
		break;

	case PROP_Y1:
		re->y1 = g_value_get_double (value);

		foo_canvas_item_request_update (item);
		break;

	case PROP_X2:
		re->x2 = g_value_get_double (value);

		foo_canvas_item_request_update (item);
		break;

	case PROP_Y2:
		re->y2 = g_value_get_double (value);

		foo_canvas_item_request_update (item);
		break;

	case PROP_FILL_COLOR:
	case PROP_FILL_COLOR_GDK:
	case PROP_FILL_COLOR_RGBA:
		switch (param_id) {
		case PROP_FILL_COLOR:
			if (g_value_get_string (value) &&
			    gdk_color_parse (g_value_get_string (value), &color))
				foo_canvas_re_set_fill (re, TRUE);
			else
				foo_canvas_re_set_fill (re, FALSE);

			re->fill_color = ((color.red & 0xff00) << 16 |
					  (color.green & 0xff00) << 8 |
					  (color.blue & 0xff00) |
					  0xff);
			break;

		case PROP_FILL_COLOR_GDK:
			pcolor = g_value_get_boxed (value);
			foo_canvas_re_set_fill (re, pcolor != NULL);

			if (pcolor) {
				GdkColormap *colormap;

				color = *pcolor;
				colormap = gtk_widget_get_colormap (GTK_WIDGET (item->canvas));
				gdk_rgb_find_color (colormap, &color);
				have_pixel = TRUE;
			}

			re->fill_color = ((color.red & 0xff00) << 16 |
					  (color.green & 0xff00) << 8 |
					  (color.blue & 0xff00) |
					  0xff);
			break;

		case PROP_FILL_COLOR_RGBA:
			foo_canvas_re_set_fill (re, TRUE);
			re->fill_color = g_value_get_uint (value);
			break;
		}
#ifdef VERBOSE
		g_print ("re fill color = %08x\n", re->fill_color);
#endif
		if (have_pixel)
			re->fill_pixel = color.pixel;
		else
			re->fill_pixel = foo_canvas_get_color_pixel (item->canvas, re->fill_color);

		set_gc_foreground (re->fill_gc, re->fill_pixel);

		foo_canvas_item_request_redraw (item);
		break;

	case PROP_OUTLINE_COLOR:
	case PROP_OUTLINE_COLOR_GDK:
	case PROP_OUTLINE_COLOR_RGBA:
		switch (param_id) {
		case PROP_OUTLINE_COLOR:
			if (g_value_get_string (value) &&
			    gdk_color_parse (g_value_get_string (value), &color))
				foo_canvas_re_set_outline (re, TRUE);
			else
				foo_canvas_re_set_outline (re, FALSE);

			re->outline_color = ((color.red & 0xff00) << 16 |
					     (color.green & 0xff00) << 8 |
					     (color.blue & 0xff00) |
					     0xff);
			break;

		case PROP_OUTLINE_COLOR_GDK:
			pcolor = g_value_get_boxed (value);
			foo_canvas_re_set_outline (re, pcolor != NULL);

			if (pcolor) {
				GdkColormap *colormap;

				color = *pcolor;
				colormap = gtk_widget_get_colormap (GTK_WIDGET (item->canvas));
				gdk_rgb_find_color (colormap, &color);

				have_pixel = TRUE;
			}

			re->outline_color = ((color.red & 0xff00) << 16 |
					     (color.green & 0xff00) << 8 |
					     (color.blue & 0xff00) |
					     0xff);
			break;

		case PROP_OUTLINE_COLOR_RGBA:
			foo_canvas_re_set_outline (re, TRUE);
			re->outline_color = g_value_get_uint (value);
			break;
		}
#ifdef VERBOSE
		g_print ("re outline color %x %x %x\n", color.red, color.green, color.blue);
#endif
		if (have_pixel)
			re->outline_pixel = color.pixel;
		else
			re->outline_pixel = foo_canvas_get_color_pixel (item->canvas,
									  re->outline_color);

		set_gc_foreground (re->outline_gc, re->outline_pixel);

		foo_canvas_item_request_redraw (item);
		break;

	case PROP_FILL_STIPPLE:
	        set_stipple (re->fill_gc, &re->fill_stipple, (GdkBitmap *) g_value_get_object (value), FALSE);

		break;

	case PROP_OUTLINE_STIPPLE:
	        set_stipple (re->outline_gc, &re->outline_stipple, (GdkBitmap *) g_value_get_object (value), FALSE);
		break;

	case PROP_WIDTH_PIXELS:
		re->width = g_value_get_uint (value);
		re->width_pixels = TRUE;
		set_outline_gc_width (re);

		foo_canvas_item_request_update (item);
		break;

	case PROP_WIDTH_UNITS:
		re->width = fabs (g_value_get_double (value));
		re->width_pixels = FALSE;
		set_outline_gc_width (re);

		foo_canvas_item_request_update (item);
		break;

	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
		break;
	}
}

/* Allocates a GdkColor structure filled with the specified pixel, and puts it into the specified
 * value for returning it in the get_property method.
 */
static void
get_color_value (FooCanvasRE *re, gulong pixel, GValue *value)
{
	GdkColor color;
	FooCanvasItem *item = (FooCanvasItem *) re;
	GdkColormap *colormap = gtk_widget_get_colormap (GTK_WIDGET (item->canvas));

	gdk_colormap_query_color (colormap, pixel, &color);
	g_value_set_boxed (value, &color);
}

static void
foo_canvas_re_get_property (GObject              *object,
			      guint                 param_id,
			      GValue               *value,
			      GParamSpec           *pspec)
{
	FooCanvasRE *re;

	g_return_if_fail (object != NULL);
	g_return_if_fail (FOO_IS_CANVAS_RE (object));

	re = FOO_CANVAS_RE (object);

	switch (param_id) {
	case PROP_X1:
		g_value_set_double (value,  re->x1);
		break;

	case PROP_Y1:
		g_value_set_double (value,  re->y1);
		break;

	case PROP_X2:
		g_value_set_double (value,  re->x2);
		break;

	case PROP_Y2:
		g_value_set_double (value,  re->y2);
		break;

	case PROP_FILL_COLOR_GDK:
		get_color_value (re, re->fill_pixel, value);
		break;

	case PROP_OUTLINE_COLOR_GDK:
		get_color_value (re, re->outline_pixel, value);
		break;

	case PROP_FILL_COLOR_RGBA:
		g_value_set_uint (value,  re->fill_color);
		break;

	case PROP_OUTLINE_COLOR_RGBA:
		g_value_set_uint (value,  re->outline_color);
		break;

	case PROP_FILL_STIPPLE:
		g_value_set_object (value,  (GObject *) re->fill_stipple);
		break;

	case PROP_OUTLINE_STIPPLE:
		g_value_set_object (value,  (GObject *) re->outline_stipple);
		break;

	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
		break;
	}
}

static void
set_colors_and_stipples (FooCanvasRE *re)
{
	set_gc_foreground (re->fill_gc, re->fill_pixel);
	set_gc_foreground (re->outline_gc, re->outline_pixel);
	set_stipple (re->fill_gc, &re->fill_stipple, re->fill_stipple, TRUE);
	set_stipple (re->outline_gc, &re->outline_stipple, re->outline_stipple, TRUE);
	set_outline_gc_width (re);
}

static void
foo_canvas_re_update_shared (FooCanvasItem *item, double i2w_dx, double i2w_dy, int flags)
{
	FooCanvasRE *re;

#ifdef VERBOSE
	g_print ("foo_canvas_re_update_shared\n");
#endif
	re = FOO_CANVAS_RE (item);

	if (re_parent_class->update)
		(* re_parent_class->update) (item, i2w_dx, i2w_dy, flags);

	set_colors_and_stipples (re);

#ifdef OLD_XFORM
	recalc_bounds (re);
#endif
}

static void
foo_canvas_re_realize (FooCanvasItem *item)
{
	FooCanvasRE *re;

#ifdef VERBOSE
	g_print ("foo_canvas_re_realize\n");
#endif
	re = FOO_CANVAS_RE (item);

	if (re_parent_class->realize)
		(* re_parent_class->realize) (item);

	re->fill_gc = gdk_gc_new (item->canvas->layout.bin_window);
	re->fill_pixel = foo_canvas_get_color_pixel (item->canvas, re->fill_color);
	re->outline_gc = gdk_gc_new (item->canvas->layout.bin_window);
	re->outline_pixel = foo_canvas_get_color_pixel (item->canvas, re->outline_color);
	set_colors_and_stipples (re);

#ifdef OLD_XFORM
	(* FOO_CANVAS_ITEM_CLASS (item->object.klass)->update) (item, NULL, NULL, 0);
#endif
}

static void
foo_canvas_re_unrealize (FooCanvasItem *item)
{
	FooCanvasRE *re;

	re = FOO_CANVAS_RE (item);

	g_object_unref (re->fill_gc);
	re->fill_gc = NULL;
	g_object_unref (re->outline_gc);
	re->outline_gc = NULL;

	if (re_parent_class->unrealize)
		(* re_parent_class->unrealize) (item);
}

static void
foo_canvas_re_translate (FooCanvasItem *item, double dx, double dy)
{
	FooCanvasRE *re;

#ifdef VERBOSE
	g_print ("foo_canvas_re_translate\n");
#endif
	re = FOO_CANVAS_RE (item);

	re->x1 += dx;
	re->y1 += dy;
	re->x2 += dx;
	re->y2 += dy;
}


static void
foo_canvas_re_bounds (FooCanvasItem *item, double *x1, double *y1, double *x2, double *y2)
{
	FooCanvasRE *re;
	double hwidth;

#ifdef VERBOSE
	g_print ("foo_canvas_re_bounds\n");
#endif
	re = FOO_CANVAS_RE (item);

	if (re->width_pixels)
		hwidth = (re->width / item->canvas->pixels_per_unit) / 2.0;
	else
		hwidth = re->width / 2.0;

	*x1 = re->x1 - hwidth;
	*y1 = re->y1 - hwidth;
	*x2 = re->x2 + hwidth;
	*y2 = re->y2 + hwidth;
}

/* Rectangle item */


static void foo_canvas_rect_class_init (FooCanvasRectClass *class);
static void foo_canvas_rect_init (FooCanvasRect *rect);
static void foo_canvas_rect_finalize (GObject *object);
static void foo_canvas_rect_realize  (FooCanvasItem *item);

static void   foo_canvas_rect_draw   (FooCanvasItem *item, GdkDrawable *drawable, GdkEventExpose *expose);
static double foo_canvas_rect_point  (FooCanvasItem *item, double x, double y, int cx, int cy,
				        FooCanvasItem **actual_item);

struct _FooCanvasRectPrivate {
	Rect last_update_rect;
	Rect last_outline_update_rect;
	int last_outline_update_width;

#ifdef HAVE_RENDER
	gboolean use_render;
	XRenderPictFormat *format;
#endif
};

GType
foo_canvas_rect_get_type (void)
{
	static GType rect_type = 0;

	if (!rect_type) {
		GTypeInfo rect_info = {
		  sizeof (FooCanvasRectClass),
		  (GBaseInitFunc) NULL,
		  (GBaseFinalizeFunc) NULL,
		  (GClassInitFunc) foo_canvas_rect_class_init,
		  NULL,           /* class_finalize */
		  NULL,           /* class_data */
		  sizeof (FooCanvasRect),
		  0,              /* n_preallocs */
		  (GInstanceInitFunc) foo_canvas_rect_init
		};

		rect_type = g_type_register_static (foo_canvas_re_get_type (),
						    "FooCanvasRect",
						    &rect_info,
						    0);
	}

	return rect_type;
}

static void
foo_canvas_rect_class_init (FooCanvasRectClass *class)
{
	FooCanvasItemClass *item_class;

	rect_parent_class = g_type_class_peek_parent (class);

	item_class = (FooCanvasItemClass *) class;

	item_class->draw = foo_canvas_rect_draw;
	item_class->point = foo_canvas_rect_point;
	item_class->update = foo_canvas_rect_update;
	item_class->realize = foo_canvas_rect_realize;

	G_OBJECT_CLASS (class)->finalize = foo_canvas_rect_finalize;

}

static void
foo_canvas_rect_init (FooCanvasRect *rect)
{
	rect->priv = g_new0 (FooCanvasRectPrivate, 1);
}

static void
foo_canvas_rect_finalize (GObject *object)
{
	FooCanvasRect *rect = FOO_CANVAS_RECT (object);

	if (rect->priv) {
		g_free (rect->priv);
	}

	G_OBJECT_CLASS (rect_parent_class)->finalize (object);
}

static void
foo_canvas_rect_realize  (FooCanvasItem *item)
{
#ifdef HAVE_RENDER
	FooCanvasRectPrivate *priv;
	int event_base, error_base;
	Display *dpy;

	priv = FOO_CANVAS_RECT (item)->priv;

	dpy = gdk_x11_drawable_get_xdisplay (GTK_WIDGET (item->canvas)->window);
	priv->use_render = XRenderQueryExtension (dpy, &event_base, &error_base);

	if (priv->use_render) {
		GdkVisual *gdk_visual;
		Visual *visual;

		gdk_visual = gtk_widget_get_visual (GTK_WIDGET (item->canvas));
		visual = gdk_x11_visual_get_xvisual (gdk_visual);

		priv->format = XRenderFindVisualFormat (dpy, visual);
	}
#endif

	if (FOO_CANVAS_ITEM_CLASS (rect_parent_class)->realize) {
		(* FOO_CANVAS_ITEM_CLASS (rect_parent_class)->realize) (item);
	}
}


static void
render_rect_alpha (FooCanvasRect *rect,
		   GdkDrawable *drawable,
		   int x, int y,
		   int width, int height,
		   guint32 rgba)
{
	GdkPixbuf *pixbuf;
	guchar *data;
	int rowstride, i;
	guchar r, g, b, a;
	FooCanvasRectPrivate *priv;

	if (width <= 0 || height <= 0 ) {
		return;
	}

	priv = rect->priv;

	r = (rgba >> 24) & 0xff;
	g = (rgba >> 16) & 0xff;
	b = (rgba >> 8) & 0xff;
	a = (rgba >> 0) & 0xff;

#ifdef HAVE_RENDER
	/* Every visual is not guaranteed to have a matching
	 * XRenderPictFormat. So make sure that format is not null before
	 * trying to render using Xrender calls.
	 */
	if (priv->use_render && (priv->format != NULL)) {
		GdkDrawable *real_drawable;
		int x_offset, y_offset;

		Display *dpy;
		Picture  pict;
		XRenderPictureAttributes attributes;
		XRenderColor color;

		gdk_window_get_internal_paint_info (drawable, &real_drawable,
						    &x_offset, &y_offset);

		dpy = gdk_x11_drawable_get_xdisplay (real_drawable);

		pict = XRenderCreatePicture (dpy,
					     gdk_x11_drawable_get_xid (real_drawable),
					     priv->format,
					     0,
					     &attributes);


		/* Convert to premultiplied alpha: */
		r = r * a / 255;
		g = g * a / 255;
		b = b * a / 255;

		color.red = (r << 8) + r;
		color.green = (g << 8) + g;
		color.blue = (b << 8) + b;
		color.alpha = (a << 8) + a;

		XRenderFillRectangle (dpy,
				      PictOpOver,
				      pict,
				      &color,
				      x - x_offset, y - y_offset,
				      width, height);

		XRenderFreePicture (dpy, pict);

		return;
	}
#endif
	pixbuf = gdk_pixbuf_new (GDK_COLORSPACE_RGB, TRUE, 8, width, height);
	data = gdk_pixbuf_get_pixels (pixbuf);
	rowstride = gdk_pixbuf_get_rowstride (pixbuf);

	r = (rgba >> 24) & 0xff;
	g = (rgba >> 16) & 0xff;
	b = (rgba >> 8) & 0xff;
	a = (rgba >> 0) & 0xff;

	for (i = 0; i < width*4; ) {
		data[i++] = r;
		data[i++] = g;
		data[i++] = b;
		data[i++] = a;
	}

	for (i = 1; i < height; i++) {
		memcpy (data + i*rowstride, data, width*4);
	}

	gdk_draw_pixbuf (drawable, NULL, pixbuf,
			 0, 0, x, y, width, height,
			 GDK_RGB_DITHER_NONE, 0, 0);
	g_object_unref (pixbuf);
}


static void
foo_canvas_rect_draw (FooCanvasItem *item, GdkDrawable *drawable, GdkEventExpose *expose)
{
	FooCanvasRE *re;
	double x1, y1, x2, y2;
	int cx1, cy1, cx2, cy2;
	double i2w_dx, i2w_dy;

	re = FOO_CANVAS_RE (item);

	/* Get canvas pixel coordinates */
	i2w_dx = 0.0;
	i2w_dy = 0.0;
	foo_canvas_item_i2w (item, &i2w_dx, &i2w_dy);

	x1 = re->x1 + i2w_dx;
	y1 = re->y1 + i2w_dy;
	x2 = re->x2 + i2w_dx;
	y2 = re->y2 + i2w_dy;

	foo_canvas_w2c (item->canvas, x1, y1, &cx1, &cy1);
	foo_canvas_w2c (item->canvas, x2, y2, &cx2, &cy2);

	if (re->fill_set) {
		if ((re->fill_color & 0xff) != 255) {
			GdkRectangle *rectangles;
			gint i, n_rectangles;
			GdkRectangle draw_rect;
			GdkRectangle part;

			draw_rect.x = cx1;
			draw_rect.y = cy1;
			draw_rect.width = cx2 - cx1 + 1;
			draw_rect.height = cy2 - cy1 + 1;

			/* For alpha mode, only render the parts of the region
			   that are actually exposed */
			gdk_region_get_rectangles (expose->region,
						   &rectangles,
						   &n_rectangles);

			for (i = 0; i < n_rectangles; i++) {
				if (gdk_rectangle_intersect (&rectangles[i],
							     &draw_rect,
							     &part)) {
					render_rect_alpha (FOO_CANVAS_RECT (item),
							   drawable,
							   part.x, part.y,
							   part.width, part.height,
							   re->fill_color);
				}
			}

			g_free (rectangles);
		} else {
			if (re->fill_stipple)
				foo_canvas_set_stipple_origin (item->canvas, re->fill_gc);

			gdk_draw_rectangle (drawable,
					    re->fill_gc,
					    TRUE,
					    cx1, cy1,
					    cx2 - cx1 + 1,
					    cy2 - cy1 + 1);
		}
	}

	if (re->outline_set) {
		if (re->outline_stipple)
			foo_canvas_set_stipple_origin (item->canvas, re->outline_gc);

		gdk_draw_rectangle (drawable,
				    re->outline_gc,
				    FALSE,
				    cx1,
				    cy1,
				    cx2 - cx1,
				    cy2 - cy1);
	}
}

static double
foo_canvas_rect_point (FooCanvasItem *item, double x, double y, int cx, int cy, FooCanvasItem **actual_item)
{
	FooCanvasRE *re;
	double x1, y1, x2, y2;
	double hwidth;
	double dx, dy;
	double tmp;

#ifdef VERBOSE
	g_print ("foo_canvas_rect_point\n");
#endif
	re = FOO_CANVAS_RE (item);

	*actual_item = item;

	/* Find the bounds for the rectangle plus its outline width */

	x1 = re->x1;
	y1 = re->y1;
	x2 = re->x2;
	y2 = re->y2;

	if (re->outline_set) {
		if (re->width_pixels)
			hwidth = (re->width / item->canvas->pixels_per_unit) / 2.0;
		else
			hwidth = re->width / 2.0;

		x1 -= hwidth;
		y1 -= hwidth;
		x2 += hwidth;
		y2 += hwidth;
	} else
		hwidth = 0.0;

	/* Is point inside rectangle (which can be hollow if it has no fill set)? */

	if ((x >= x1) && (y >= y1) && (x <= x2) && (y <= y2)) {
		if (re->fill_set || !re->outline_set)
			return 0.0;

		dx = x - x1;
		tmp = x2 - x;
		if (tmp < dx)
			dx = tmp;

		dy = y - y1;
		tmp = y2 - y;
		if (tmp < dy)
			dy = tmp;

		if (dy < dx)
			dx = dy;

		dx -= 2.0 * hwidth;

		if (dx < 0.0)
			return 0.0;
		else
			return dx;
	}

	/* Point is outside rectangle */

	if (x < x1)
		dx = x1 - x;
	else if (x > x2)
		dx = x - x2;
	else
		dx = 0.0;

	if (y < y1)
		dy = y1 - y;
	else if (y > y2)
		dy = y - y2;
	else
		dy = 0.0;

	return sqrt (dx * dx + dy * dy);
}

static void
request_redraw_borders (FooCanvas *canvas,
			Rect     *update_rect,
			int     width)
{
	foo_canvas_request_redraw (canvas,
				   update_rect->x0, update_rect->y0,
				   update_rect->x1, update_rect->y0 + width);
	foo_canvas_request_redraw (canvas,
				   update_rect->x0, update_rect->y1-width,
				   update_rect->x1, update_rect->y1);
	foo_canvas_request_redraw (canvas,
				   update_rect->x0,       update_rect->y0,
				   update_rect->x0+width, update_rect->y1);
	foo_canvas_request_redraw (canvas,
				   update_rect->x1-width, update_rect->y0,
				   update_rect->x1,       update_rect->y1);
}


static void
foo_canvas_rect_update (FooCanvasItem *item, double i2w_dx, double i2w_dy, gint flags)
{
	FooCanvasRE *re;
	double x1, y1, x2, y2;
	int cx1, cy1, cx2, cy2;
	int repaint_rects_count, i;
	int width_pixels;
	int width_lt, width_rb;
	Rect update_rect, repaint_rects[4];
	FooCanvasRectPrivate *priv;

	foo_canvas_re_update_shared (item, i2w_dx, i2w_dy, flags);

	re = FOO_CANVAS_RE (item);
	priv = FOO_CANVAS_RECT (item)->priv;

	x1 = re->x1 + i2w_dx;
	y1 = re->y1 + i2w_dy;
	x2 = re->x2 + i2w_dx;
	y2 = re->y2 + i2w_dy;

	foo_canvas_w2c (item->canvas, x1, y1, &cx1, &cy1);
	foo_canvas_w2c (item->canvas, x2, y2, &cx2, &cy2);

	update_rect = make_rect (cx1, cy1, cx2+1, cy2+1);
#if 0
	foo_canvas_request_redraw (item->canvas,
				   update_rect.x0, update_rect.y0,
				   update_rect.x1, update_rect.y1);
	foo_canvas_request_redraw (item->canvas,
				   priv->last_update_rect.x0, priv->last_update_rect.y0,
				   priv->last_update_rect.x1, priv->last_update_rect.y1);
#else
	diff_rects (update_rect, priv->last_update_rect,
		    &repaint_rects_count, repaint_rects);
	for (i = 0; i < repaint_rects_count; i++) {
		foo_canvas_request_redraw (item->canvas,
					   repaint_rects[i].x0, repaint_rects[i].y0,
					   repaint_rects[i].x1, repaint_rects[i].y1);
	}
#endif
	priv->last_update_rect = update_rect;

	if (re->outline_set) {
		/* Outline and bounding box */
		if (re->width_pixels)
			width_pixels = (int) re->width;
		else
			width_pixels = (int) floor (re->width * re->item.canvas->pixels_per_unit + 0.5);

		width_lt = width_pixels / 2;
		width_rb = (width_pixels + 1) / 2;

		cx1 -= width_lt;
		cy1 -= width_lt;
		cx2 += width_rb;
		cy2 += width_rb;

		update_rect = make_rect (cx1, cy1, cx2, cy2);
		request_redraw_borders (item->canvas, &update_rect,
					(width_lt + width_rb));
		request_redraw_borders (item->canvas, &priv->last_outline_update_rect,
					priv->last_outline_update_width);
		priv->last_outline_update_rect = update_rect;
		priv->last_outline_update_width = width_lt + width_rb;

		item->x1 = cx1;
		item->y1 = cy1;
		item->x2 = cx2+1;
		item->y2 = cy2+1;
	} else {
		item->x1 = cx1;
		item->y1 = cy1;
		item->x2 = cx2+1;
		item->y2 = cy2+1;
	}
}

/* Ellipse item */


static void foo_canvas_ellipse_class_init (FooCanvasEllipseClass *class);

static void   foo_canvas_ellipse_draw   (FooCanvasItem *item, GdkDrawable *drawable, GdkEventExpose *expose);
static double foo_canvas_ellipse_point  (FooCanvasItem *item, double x, double y, int cx, int cy,
					   FooCanvasItem **actual_item);


GType
foo_canvas_ellipse_get_type (void)
{
	static GType ellipse_type = 0;

	if (!ellipse_type) {
		GTypeInfo ellipse_info = {
		  sizeof (FooCanvasEllipseClass),
		  (GBaseInitFunc) NULL,
		  (GBaseFinalizeFunc) NULL,
		  (GClassInitFunc) foo_canvas_ellipse_class_init,
		  NULL,           /* class_finalize */
		  NULL,           /* class_data */
		  sizeof (FooCanvasEllipse),
		  0,              /* n_preallocs */
		  (GInstanceInitFunc) NULL

		};

		ellipse_type = g_type_register_static (foo_canvas_re_get_type (),
						       "FooCanvasEllipse",
						       &ellipse_info,
						       0);
	}

	return ellipse_type;
}

static void
foo_canvas_ellipse_class_init (FooCanvasEllipseClass *class)
{
	FooCanvasItemClass *item_class;

	item_class = (FooCanvasItemClass *) class;

	item_class->draw = foo_canvas_ellipse_draw;
	item_class->point = foo_canvas_ellipse_point;
	item_class->update = foo_canvas_ellipse_update;
}

static void
foo_canvas_ellipse_draw (FooCanvasItem *item, GdkDrawable *drawable, GdkEventExpose *expose)
{
	FooCanvasRE *re;
	int x1, y1, x2, y2;
	double i2w_dx, i2w_dy;

	re = FOO_CANVAS_RE (item);

	/* Get canvas pixel coordinates */

	i2w_dx = 0.0;
	i2w_dy = 0.0;
	foo_canvas_item_i2w (item, &i2w_dx, &i2w_dy);

	foo_canvas_w2c (item->canvas,
			  re->x1 + i2w_dx,
			  re->y1 + i2w_dy,
			  &x1, &y1);
	foo_canvas_w2c (item->canvas,
			  re->x2 + i2w_dx,
			  re->y2 + i2w_dy,
			  &x2, &y2);

	if (re->fill_set) {
		if (re->fill_stipple)
			foo_canvas_set_stipple_origin (item->canvas, re->fill_gc);

		gdk_draw_arc (drawable,
			      re->fill_gc,
			      TRUE,
			      x1,
			      y1,
			      x2 - x1,
			      y2 - y1,
			      0 * 64,
			      360 * 64);
	}

	if (re->outline_set) {
		if (re->outline_stipple)
			foo_canvas_set_stipple_origin (item->canvas, re->outline_gc);

		gdk_draw_arc (drawable,
			      re->outline_gc,
			      FALSE,
			      x1,
			      y1,
			      x2 - x1,
			      y2 - y1,
			      0 * 64,
			      360 * 64);
	}
}

static double
foo_canvas_ellipse_point (FooCanvasItem *item, double x, double y, int cx, int cy, FooCanvasItem **actual_item)
{
	FooCanvasRE *re;
	double dx, dy;
	double scaled_dist;
	double outline_dist;
	double center_dist;
	double width;
	double a, b;
	double diamx, diamy;

	re = FOO_CANVAS_RE (item);

	*actual_item = item;

	if (re->outline_set) {
		if (re->width_pixels)
			width = re->width / item->canvas->pixels_per_unit;
		else
			width = re->width;
	} else
		width = 0.0;

	/* Compute the distance between the center of the ellipse and the point, with the ellipse
	 * considered as being scaled to a circle.
	 */

	dx = x - (re->x1 + re->x2) / 2.0;
	dy = y - (re->y1 + re->y2) / 2.0;
	center_dist = sqrt (dx * dx + dy * dy);

	a = dx / ((re->x2 + width - re->x1) / 2.0);
	b = dy / ((re->y2 + width - re->y1) / 2.0);
	scaled_dist = sqrt (a * a + b * b);

	/* If the scaled distance is greater than 1, then we are outside.  Compute the distance from
	 * the point to the edge of the circle, then scale back to the original un-scaled coordinate
	 * system.
	 */

	if (scaled_dist > 1.0)
		return (center_dist / scaled_dist) * (scaled_dist - 1.0);

	/* We are inside the outer edge of the ellipse.  If it is filled, then we are "inside".
	 * Otherwise, do the same computation as above, but also check whether we are inside the
	 * outline.
	 */

	if (re->fill_set)
		return 0.0;

	if (scaled_dist > FOO_CANVAS_EPSILON)
		outline_dist = (center_dist / scaled_dist) * (1.0 - scaled_dist) - width;
	else {
		/* Handle very small distance */

		diamx = re->x2 - re->x1;
		diamy = re->y2 - re->y1;

		if (diamx < diamy)
			outline_dist = (diamx - width) / 2.0;
		else
			outline_dist = (diamy - width) / 2.0;
	}

	if (outline_dist < 0.0)
		return 0.0;

	return outline_dist;
}

static void
foo_canvas_ellipse_update (FooCanvasItem *item, double i2w_dx, double i2w_dy, gint flags)
{
	FooCanvasRE *re;
	double x0, y0, x1, y1;

#ifdef VERBOSE
	g_print ("foo_canvas_sllipse_update item %x\n", item);
#endif

	foo_canvas_re_update_shared (item, i2w_dx, i2w_dy, flags);
	re = FOO_CANVAS_RE (item);

	get_bounds (re, &x0, &y0, &x1, &y1);
	foo_canvas_update_bbox (item, x0, y0, x1, y1);
}

static int
rect_empty (const Rect *src) {
  return (src->x1 <= src->x0 || src->y1 <= src->y0);
}

static Rect
make_rect (int x0, int y0, int x1, int y1)
{
	Rect r;

	r.x0 = x0;
	r.y0 = y0;
	r.x1 = x1;
	r.y1 = y1;
	return r;
}

static gboolean
rects_intersect (Rect r1, Rect r2)
{
	if (r1.x0 >= r2.x1) {
		return FALSE;
	}
	if (r2.x0 >= r1.x1) {
		return FALSE;
	}
	if (r1.y0 >= r2.y1) {
		return FALSE;
	}
	if (r2.y0 >= r1.y1) {
		return FALSE;
	}
	return TRUE;
}

static void
diff_rects_guts (Rect ra, Rect rb, int *count, Rect result[4])
{
	if (ra.x0 < rb.x0) {
		result[(*count)++] = make_rect (ra.x0, ra.y0, rb.x0, ra.y1);
	}
	if (ra.y0 < rb.y0) {
		result[(*count)++] = make_rect (ra.x0, ra.y0, ra.x1, rb.y0);
	}
	if (ra.x1 < rb.x1) {
		result[(*count)++] = make_rect (ra.x1, rb.y0, rb.x1, rb.y1);
	}
	if (ra.y1 < rb.y1) {
		result[(*count)++] = make_rect (rb.x0, ra.y1, rb.x1, rb.y1);
	}
}

static void
diff_rects (Rect r1, Rect r2, int *count, Rect result[4])
{
	g_assert (count != NULL);
	g_assert (result != NULL);

	*count = 0;

	if (rects_intersect (r1, r2)) {
		diff_rects_guts (r1, r2, count, result);
		diff_rects_guts (r2, r1, count, result);
	} else {
		if (!rect_empty (&r1)) {
			result[(*count)++] = r1;
		}
		if (!rect_empty (&r2)) {
			result[(*count)++] = r2;
		}
	}
}
