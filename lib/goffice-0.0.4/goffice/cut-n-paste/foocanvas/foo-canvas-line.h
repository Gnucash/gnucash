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

/* Line/curve item type for FooCanvas widget
 *
 * FooCanvas is basically a port of the Tk toolkit's most excellent canvas widget.  Tk is
 * copyrighted by the Regents of the University of California, Sun Microsystems, and other parties.
 *
 *
 * Author: Federico Mena <federico@nuclecu.unam.mx>
 */

#ifndef FOO_CANVAS_LINE_H
#define FOO_CANVAS_LINE_H


#include <goffice/cut-n-paste/foocanvas/foo-canvas.h>


G_BEGIN_DECLS


/* Line item for the canvas.  This is a polyline with configurable width, cap/join styles, and arrowheads.
 * If arrowheads are enabled, then three values are used to specify their shape:
 *
 *	arrow_shape_a:  Distance from tip of arrowhead to the center point.
 *	arrow_shape_b:  Distance from tip of arrowhead to trailing point, measured along the shaft.
 *	arrow_shape_c:	Distance of trailing point from outside edge of shaft.
 *
 * The following object arguments are available:
 *
 * name			type			read/write	description
 * ------------------------------------------------------------------------------------------
 * points		FooCanvasPoints*	RW		Pointer to a FooCanvasPoints structure.
 *								This can be created by a call to
 *								foo_canvas_points_new() (in foo-canvas-util.h).
 *								X coordinates are in the even indices of the
 *								points->coords array, Y coordinates are in
 *								the odd indices.
 * fill_color		string			W		X color specification for line
 * fill_color_gdk	GdkColor*		RW		Pointer to an allocated GdkColor
 * fill_stipple		GdkBitmap*		RW		Stipple pattern for the line
 * width_pixels		uint			R		Width of the line in pixels.  The line width
 *								will not be scaled when the canvas zoom factor changes.
 * width_units		double			R		Width of the line in canvas units.  The line width
 *								will be scaled when the canvas zoom factor changes.
 * cap_style		GdkCapStyle		RW		Cap ("endpoint") style for the line.
 * join_style		GdkJoinStyle		RW		Join ("vertex") style for the line.
 * line_style		GdkLineStyle		RW		Line dash style
 * first_arrowhead	boolean			RW		Specifies whether to draw an arrowhead on the
 *								first point of the line.
 * last_arrowhead	boolean			RW		Specifies whether to draw an arrowhead on the
 *								last point of the line.
 * smooth		boolean			RW		Specifies whether to smooth the line using
 *								parabolic splines.
 * spline_steps		uint			RW		Specifies the number of steps to use when rendering curves.
 * arrow_shape_a	double			RW		First arrow shape specifier.
 * arrow_shape_b	double			RW		Second arrow shape specifier.
 * arrow_shape_c	double			RW		Third arrow shape specifier.
 */


#define FOO_TYPE_CANVAS_LINE            (foo_canvas_line_get_type ())
#define FOO_CANVAS_LINE(obj)            (GTK_CHECK_CAST ((obj), FOO_TYPE_CANVAS_LINE, FooCanvasLine))
#define FOO_CANVAS_LINE_CLASS(klass)    (GTK_CHECK_CLASS_CAST ((klass), FOO_TYPE_CANVAS_LINE, FooCanvasLineClass))
#define FOO_IS_CANVAS_LINE(obj)         (GTK_CHECK_TYPE ((obj), FOO_TYPE_CANVAS_LINE))
#define FOO_IS_CANVAS_LINE_CLASS(klass) (GTK_CHECK_CLASS_TYPE ((klass), FOO_TYPE_CANVAS_LINE))
#define FOO_CANVAS_LINE_GET_CLASS(obj)  (GTK_CHECK_GET_CLASS ((obj), FOO_TYPE_CANVAS_LINE, FooCanvasLineClass))


typedef struct _FooCanvasLine FooCanvasLine;
typedef struct _FooCanvasLineClass FooCanvasLineClass;

struct _FooCanvasLine {
	FooCanvasItem item;

	double *coords;		/* Array of coordinates for the line's points.  X coords are in the
				 * even indices, Y coords are in the odd indices.  If the line has
				 * arrowheads then the first and last points have been adjusted to
				 * refer to the necks of the arrowheads rather than their tips.  The
				 * actual endpoints are stored in the first_arrow and last_arrow
				 * arrays, if they exist.
				 */

	double *first_coords;	/* Array of points describing polygon for the first arrowhead */
	double *last_coords;	/* Array of points describing polygon for the last arrowhead */

	GdkGC *gc;		/* GC for drawing line */

	GdkBitmap *stipple;	/* Stipple pattern */

	double width;		/* Width of the line */

	double shape_a;		/* Distance from tip of arrowhead to center */
	double shape_b;		/* Distance from tip of arrowhead to trailing point, measured along shaft */
	double shape_c;		/* Distance of trailing points from outside edge of shaft */

	GdkCapStyle cap;	/* Cap style for line */
	GdkJoinStyle join;	/* Join style for line */
	GdkLineStyle line_style;/* Style for the line */

	gulong fill_pixel;	/* Color for line */

	guint32 fill_rgba;		/* RGBA color for outline */ /*AA*/

	int num_points;		/* Number of points in the line */
	guint fill_color;	/* Fill color, RGBA */

	int spline_steps;	/* Number of steps in each spline segment */

	guint width_pixels : 1;	/* Is the width specified in pixels or units? */
	guint first_arrow : 1;	/* Draw first arrowhead? */
	guint last_arrow : 1;	/* Draw last arrowhead? */
	guint smooth : 1;	/* Smooth line (with parabolic splines)? */
};

struct _FooCanvasLineClass {
	FooCanvasItemClass parent_class;
};


/* Standard Gtk function */
GtkType foo_canvas_line_get_type (void) G_GNUC_CONST;


G_END_DECLS

#endif
