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
/* Polygon item type for FooCanvas widget
 *
 * FooCanvas is basically a port of the Tk toolkit's most excellent canvas widget.  Tk is
 * copyrighted by the Regents of the University of California, Sun Microsystems, and other parties.
 *
 *
 * Author: Federico Mena <federico@nuclecu.unam.mx>
 */

#ifndef FOO_CANVAS_POLYGON_H
#define FOO_CANVAS_POLYGON_H


#include <goffice/cut-n-paste/foocanvas/foo-canvas.h>

G_BEGIN_DECLS


/* Polygon item for the canvas.  A polygon is a bit different from rectangles and ellipses in that
 * points inside it will always be considered "inside", even if the fill color is not set.  If you
 * want to have a hollow polygon, use a line item instead.
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
 * fill_color		string			W		X color specification for fill color,
 *								or NULL pointer for no color (transparent).
 * fill_color_gdk	GdkColor*		RW		Allocated GdkColor for fill.
 * outline_color	string			W		X color specification for outline color,
 *								or NULL pointer for no color (transparent).
 * outline_color_gdk	GdkColor*		RW		Allocated GdkColor for outline.
 * fill_stipple		GdkBitmap*		RW		Stipple pattern for fill
 * outline_stipple	GdkBitmap*		RW		Stipple pattern for outline
 * width_pixels		uint			RW		Width of the outline in pixels.  The outline will
 *								not be scaled when the canvas zoom factor is changed.
 * width_units		double			RW		Width of the outline in canvas units.  The outline
 *								will be scaled when the canvas zoom factor is changed.
 */

#define FOO_TYPE_CANVAS_POLYGON            (foo_canvas_polygon_get_type ())
#define FOO_CANVAS_POLYGON(obj)            (GTK_CHECK_CAST ((obj), FOO_TYPE_CANVAS_POLYGON, FooCanvasPolygon))
#define FOO_CANVAS_POLYGON_CLASS(klass)    (GTK_CHECK_CLASS_CAST ((klass), FOO_TYPE_CANVAS_POLYGON, FooCanvasPolygonClass))
#define FOO_IS_CANVAS_POLYGON(obj)         (GTK_CHECK_TYPE ((obj), FOO_TYPE_CANVAS_POLYGON))
#define FOO_IS_CANVAS_POLYGON_CLASS(klass) (GTK_CHECK_CLASS_TYPE ((klass), FOO_TYPE_CANVAS_POLYGON))
#define FOO_CANVAS_POLYGON_GET_CLASS(obj)  (GTK_CHECK_GET_CLASS ((obj), FOO_TYPE_CANVAS_POLYGON, FooCanvasPolygonClass))


typedef struct _FooCanvasPolygon FooCanvasPolygon;
typedef struct _FooCanvasPolygonClass FooCanvasPolygonClass;

struct _FooCanvasPolygon {
	FooCanvasItem item;

	double *coords;			/* Array of coordinates for the polygon's points.  X coords
					 * are in the even indices, Y coords are in the odd indices.
					 */
	GdkBitmap *fill_stipple;	/* Stipple for fill */
	GdkBitmap *outline_stipple;	/* Stipple for outline */

	GdkGC *fill_gc;			/* GC for filling */
	GdkGC *outline_gc;		/* GC for outline */

	gulong fill_pixel;		/* Color for fill */
	gulong outline_pixel;		/* Color for outline */
	double width;			/* Width of polygon's outline */

	int num_points;			/* Number of points in the polygon */
	guint fill_color;		/* Fill color, RGBA */
	guint outline_color;		/* Outline color, RGBA */

        guint32 fill_rgba;		/* RGBA color for filling */ /*AA*/
	guint32 outline_rgba;		/* RGBA color for outline */ /*AA*/

	guint fill_set : 1;		/* Is fill color set? */
	guint outline_set : 1;		/* Is outline color set? */
	guint width_pixels : 1;		/* Is outline width specified in pixels or units? */
};

struct _FooCanvasPolygonClass {
	FooCanvasItemClass parent_class;
};


/* Standard Gtk function */
GtkType foo_canvas_polygon_get_type (void) G_GNUC_CONST;


G_END_DECLS

#endif
