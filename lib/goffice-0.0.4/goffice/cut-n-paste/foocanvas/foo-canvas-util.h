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
/* Miscellaneous utility functions for the FooCanvas widget
 *
 * FooCanvas is basically a port of the Tk toolkit's most excellent canvas widget.  Tk is
 * copyrighted by the Regents of the University of California, Sun Microsystems, and other parties.
 *
 * Author: Federico Mena <federico@nuclecu.unam.mx>
 */

#ifndef FOO_CANVAS_UTIL_H
#define FOO_CANVAS_UTIL_H


G_BEGIN_DECLS


/* This structure defines an array of points.  X coordinates are stored in the even-numbered
 * indices, and Y coordinates are stored in the odd-numbered indices.  num_points indicates the
 * number of points, so the array is 2*num_points elements big.
 */
typedef struct {
	double *coords;
	int num_points;
	int ref_count;
} FooCanvasPoints;


/* Allocate a new FooCanvasPoints structure with enough space for the specified number of points */
FooCanvasPoints *foo_canvas_points_new (int num_points);

/* Increate ref count */
FooCanvasPoints *foo_canvas_points_ref (FooCanvasPoints *points);
#define foo_canvas_points_unref foo_canvas_points_free

/* Decrease ref count and free structure if it has reached zero */
void foo_canvas_points_free (FooCanvasPoints *points);

/* Given three points forming an angle, compute the coordinates of the inside and outside points of
 * the mitered corner formed by a line of a given width at that angle.
 *
 * If the angle is less than 11 degrees, then FALSE is returned and the return points are not
 * modified.  Otherwise, TRUE is returned.
 */
int foo_canvas_get_miter_points (double x1, double y1, double x2, double y2, double x3, double y3,
				   double width,
				   double *mx1, double *my1, double *mx2, double *my2);

/* Compute the butt points of a line segment.  If project is FALSE, then the results are as follows:
 *
 *            -------------------* (bx1, by1)
 *                               |
 *   (x1, y1) *------------------* (x2, y2)
 *                               |
 *            -------------------* (bx2, by2)
 *
 * that is, the line is not projected beyond (x2, y2).  If project is TRUE, then the results are as
 * follows:
 *
 *            -------------------* (bx1, by1)
 *                      (x2, y2) |
 *   (x1, y1) *-------------*    |
 *                               |
 *            -------------------* (bx2, by2)
 */
void foo_canvas_get_butt_points (double x1, double y1, double x2, double y2,
				   double width, int project,
				   double *bx1, double *by1, double *bx2, double *by2);

/* Calculate the distance from a polygon to a point.  The polygon's X coordinates are in the even
 * indices of the poly array, and the Y coordinates are in the odd indices.
 */
double foo_canvas_polygon_to_point (double *poly, int num_points, double x, double y);


void foo_canvas_item_reset_bounds (FooCanvasItem *item);

/* Sets the bbox to the new value, requesting full repaint. */
void foo_canvas_update_bbox (FooCanvasItem *item, int x1, int y1, int x2, int y2);

G_END_DECLS

#endif
