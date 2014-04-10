/* File import from foocanvas to libgoffice by import-foocanvas.  Do not edit.  */

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

#ifndef FOO_CANVAS_RECT_ELLIPSE_H
#define FOO_CANVAS_RECT_ELLIPSE_H


#include <goffice/cut-n-paste/foocanvas/foo-canvas.h>

G_BEGIN_DECLS


/* Base class for rectangle and ellipse item types.  These are defined by their top-left and
 * bottom-right corners.  Rectangles and ellipses share the following arguments:
 *
 * name			type		read/write	description
 * ------------------------------------------------------------------------------------------
 * x1			double		RW		Leftmost coordinate of rectangle or ellipse
 * y1			double		RW		Topmost coordinate of rectangle or ellipse
 * x2			double		RW		Rightmost coordinate of rectangle or ellipse
 * y2			double		RW		Bottommost coordinate of rectangle or ellipse
 * fill_color		string		W		X color specification for fill color,
 *							or NULL pointer for no color (transparent)
 * fill_color_gdk	GdkColor*	RW		Allocated GdkColor for fill
 * outline_color	string		W		X color specification for outline color,
 *							or NULL pointer for no color (transparent)
 * outline_color_gdk	GdkColor*	RW		Allocated GdkColor for outline
 * fill_stipple		GdkBitmap*	RW		Stipple pattern for fill
 * outline_stipple	GdkBitmap*	RW		Stipple pattern for outline
 * width_pixels		uint		RW		Width of the outline in pixels.  The outline will
 *							not be scaled when the canvas zoom factor is changed.
 * width_units		double		RW		Width of the outline in canvas units.  The outline
 *							will be scaled when the canvas zoom factor is changed.
 */


#define FOO_TYPE_CANVAS_RE            (foo_canvas_re_get_type ())
#define FOO_CANVAS_RE(obj)            (GTK_CHECK_CAST ((obj), FOO_TYPE_CANVAS_RE, FooCanvasRE))
#define FOO_CANVAS_RE_CLASS(klass)    (GTK_CHECK_CLASS_CAST ((klass), FOO_TYPE_CANVAS_RE, FooCanvasREClass))
#define FOO_IS_CANVAS_RE(obj)         (GTK_CHECK_TYPE ((obj), FOO_TYPE_CANVAS_RE))
#define FOO_IS_CANVAS_RE_CLASS(klass) (GTK_CHECK_CLASS_TYPE ((klass), FOO_TYPE_CANVAS_RE))
#define FOO_CANVAS_RE_GET_CLASS(obj)  (GTK_CHECK_GET_CLASS ((obj), FOO_TYPE_CANVAS_RE, FooCanvasREClass))


typedef struct _FooCanvasRE      FooCanvasRE;
typedef struct _FooCanvasREClass FooCanvasREClass;

struct _FooCanvasRE {
	FooCanvasItem item;

	GdkBitmap *fill_stipple;	/* Stipple for fill */
	GdkBitmap *outline_stipple;	/* Stipple for outline */

	GdkGC *fill_gc;			/* GC for filling */
	GdkGC *outline_gc;		/* GC for outline */

	gulong fill_pixel;		/* Fill color */
	gulong outline_pixel;		/* Outline color */

	double x1, y1, x2, y2;		/* Corners of item */
	double width;			/* Outline width */

	guint fill_color;		/* Fill color, RGBA */
	guint outline_color;		/* Outline color, RGBA */

	/* Configuration flags */

	unsigned int fill_set : 1;	/* Is fill color set? */
	unsigned int outline_set : 1;	/* Is outline color set? */
	unsigned int width_pixels : 1;	/* Is outline width specified in pixels or units? */
};

struct _FooCanvasREClass {
	FooCanvasItemClass parent_class;
};


/* Standard Gtk function */
GType foo_canvas_re_get_type (void) G_GNUC_CONST;


/* Rectangle item.  No configurable or queryable arguments are available (use those in
 * FooCanvasRE).
 */


#define FOO_TYPE_CANVAS_RECT            (foo_canvas_rect_get_type ())
#define FOO_CANVAS_RECT(obj)            (GTK_CHECK_CAST ((obj), FOO_TYPE_CANVAS_RECT, FooCanvasRect))
#define FOO_CANVAS_RECT_CLASS(klass)    (GTK_CHECK_CLASS_CAST ((klass), FOO_TYPE_CANVAS_RECT, FooCanvasRectClass))
#define FOO_IS_CANVAS_RECT(obj)         (GTK_CHECK_TYPE ((obj), FOO_TYPE_CANVAS_RECT))
#define FOO_IS_CANVAS_RECT_CLASS(klass) (GTK_CHECK_CLASS_TYPE ((klass), FOO_TYPE_CANVAS_RECT))
#define FOO_CANVAS_RECT_GET_CLASS(obj)  (GTK_CHECK_GET_CLASS ((obj), FOO_TYPE_CANVAS_RECT, FooCanvasRectClass))


typedef struct _FooCanvasRect FooCanvasRect;
typedef struct _FooCanvasRectPrivate FooCanvasRectPrivate;
typedef struct _FooCanvasRectClass FooCanvasRectClass;

struct _FooCanvasRect {
	FooCanvasRE re;
	FooCanvasRectPrivate *priv;
};

struct _FooCanvasRectClass {
	FooCanvasREClass parent_class;
};


/* Standard Gtk function */
GType foo_canvas_rect_get_type (void) G_GNUC_CONST;


/* Ellipse item.  No configurable or queryable arguments are available (use those in
 * FooCanvasRE).
 */


#define FOO_TYPE_CANVAS_ELLIPSE            (foo_canvas_ellipse_get_type ())
#define FOO_CANVAS_ELLIPSE(obj)            (GTK_CHECK_CAST ((obj), FOO_TYPE_CANVAS_ELLIPSE, FooCanvasEllipse))
#define FOO_CANVAS_ELLIPSE_CLASS(klass)    (GTK_CHECK_CLASS_CAST ((klass), FOO_TYPE_CANVAS_ELLIPSE, FooCanvasEllipseClass))
#define FOO_IS_CANVAS_ELLIPSE(obj)         (GTK_CHECK_TYPE ((obj), FOO_TYPE_CANVAS_ELLIPSE))
#define FOO_IS_CANVAS_ELLIPSE_CLASS(klass) (GTK_CHECK_CLASS_TYPE ((klass), FOO_TYPE_CANVAS_ELLIPSE))
#define FOO_CANVAS_ELLIPSE_GET_CLASS(obj)  (GTK_CHECK_GET_CLASS ((obj), FOO_TYPE_CANVAS_ELLIPSE, FooCanvasEllipseClass))


typedef struct _FooCanvasEllipse FooCanvasEllipse;
typedef struct _FooCanvasEllipseClass FooCanvasEllipseClass;

struct _FooCanvasEllipse {
	FooCanvasRE re;
};

struct _FooCanvasEllipseClass {
	FooCanvasREClass parent_class;
};


/* Standard Gtk function */
GType foo_canvas_ellipse_get_type (void) G_GNUC_CONST;


G_END_DECLS

#endif
