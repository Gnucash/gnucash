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
/* Miscellaneous utility functions for the FooCanvas widget
 *
 * FooCanvas is basically a port of the Tk toolkit's most excellent canvas widget.  Tk is
 * copyrighted by the Regents of the University of California, Sun Microsystems, and other parties.
 *
 *
 * Author: Federico Mena <federico@nuclecu.unam.mx>
 */


#include <sys/types.h>
#include <glib.h>
#include <math.h>
#include "foo-canvas.h"
#include "foo-canvas-util.h"

/*
 * Ok, so some systems require magic incantations for M_PI to be defined.
 * It's not important enough to worry about.
 */
#ifndef M_PI
#define M_PI 3.141592653589793238462643383279502884197169399375105820974944592307816406286208998628034825342117
#endif

/**
 * foo_canvas_points_new:
 * @num_points: The number of points to allocate space for in the array.
 *
 * Creates a structure that should be used to pass an array of points to
 * items.
 *
 * Return value: A newly-created array of points.  It should be filled in
 * by the user.
 **/
FooCanvasPoints *
foo_canvas_points_new (int num_points)
{
	FooCanvasPoints *points;

	g_return_val_if_fail (num_points > 1, NULL);

	points = g_new (FooCanvasPoints, 1);
	points->num_points = num_points;
	points->coords = g_new (double, 2 * num_points);
	points->ref_count = 1;

	return points;
}

/**
 * foo_canvas_points_ref:
 * @points: A canvas points structure.
 *
 * Increases the reference count of the specified points structure.
 *
 * Return value: The canvas points structure itself.
 **/
FooCanvasPoints *
foo_canvas_points_ref (FooCanvasPoints *points)
{
	g_return_val_if_fail (points != NULL, NULL);

	points->ref_count += 1;
	return points;
}

/**
 * foo_canvas_points_free:
 * @points: A canvas points structure.
 *
 * Decreases the reference count of the specified points structure.  If it
 * reaches zero, then the structure is freed.
 **/
void
foo_canvas_points_free (FooCanvasPoints *points)
{
	g_return_if_fail (points != NULL);

	points->ref_count -= 1;
	if (points->ref_count == 0) {
		g_free (points->coords);
		g_free (points);
	}
}

/**
 * foo_canvas_get_miter_points:
 * @x1: X coordinate of the first point
 * @y1: Y coordinate of the first point
 * @x2: X coordinate of the second (angle) point
 * @y2: Y coordinate of the second (angle) point
 * @x3: X coordinate of the third point
 * @y3: Y coordinate of the third point
 * @width: Width of the line
 * @mx1: The X coordinate of the first miter point is returned here.
 * @my1: The Y coordinate of the first miter point is returned here.
 * @mx2: The X coordinate of the second miter point is returned here.
 * @my2: The Y coordinate of the second miter point is returned here.
 *
 * Given three points forming an angle, computes the coordinates of the inside
 * and outside points of the mitered corner formed by a line of a given width at
 * that angle.
 *
 * Return value: FALSE if the angle is less than 11 degrees (this is the same
 * threshold as X uses.  If this occurs, the return points are not modified.
 * Otherwise, returns TRUE.
 **/
int
foo_canvas_get_miter_points (double x1, double y1, double x2, double y2, double x3, double y3,
			     double width,
			     double *mx1, double *my1, double *mx2, double *my2)
{
	double theta1;		/* angle of segment p2-p1 */
	double theta2;		/* angle of segment p2-p3 */
	double theta;		/* angle between line segments */
	double theta3;		/* angle that bisects theta1 and theta2 and points to p1 */
	double dist;		/* distance of miter points from p2 */
	double dx, dy;		/* x and y offsets corresponding to dist */

	double ELEVEN_DEGREES = 11.0 * M_PI / 180.0;

	/* Degenerate cases.  */
	if ((x1 == x2 && y1 == y2) || (x2 == x3 && y2 == y3))
		return FALSE;

	theta1 = atan2 (y1 - y2, x1 - x2);
	theta2 = atan2 (y3 - y2, x3 - x2);
	theta = theta1 - theta2;

	/* Normalize to (-pi; pi].  */
	if (theta > M_PI)
		theta -= 2.0 * M_PI;
	else if (theta <= -M_PI)
		theta += 2.0 * M_PI;

	if (fabs (theta) < ELEVEN_DEGREES)
		return FALSE;

	dist = fabs (0.5 * width / sin (0.5 * theta));

	theta3 = (theta1 + theta2) / 2.0;
	if (sin (theta3 - theta1) > 0.0)
		theta3 += M_PI;

	dx = dist * cos (theta3);
	dy = dist * sin (theta3);

	*mx1 = x2 + dx;
	*mx2 = x2 - dx;
	*my1 = y2 + dy;
	*my2 = y2 - dy;

	return TRUE;
}

/**
 * foo_canvas_get_butt_points:
 * @x1: X coordinate of first point in the line
 * @y1: Y cooordinate of first point in the line
 * @x2: X coordinate of second point (endpoint) of the line
 * @y2: Y coordinate of second point (endpoint) of the line
 * @width: Width of the line
 * @project: Whether the butt points should project out by width/2 distance
 * @bx1: X coordinate of first butt point is returned here
 * @by1: Y coordinate of first butt point is returned here
 * @bx2: X coordinate of second butt point is returned here
 * @by2: Y coordinate of second butt point is returned here
 *
 * Computes the butt points of a line segment.
 **/
void
foo_canvas_get_butt_points (double x1, double y1, double x2, double y2,
			      double width, int project,
			      double *bx1, double *by1, double *bx2, double *by2)
{
	double length;
	double dx, dy;

	width *= 0.5;
	dx = x2 - x1;
	dy = y2 - y1;
	length = sqrt (dx * dx + dy * dy);

	if (length < FOO_CANVAS_EPSILON) {
		*bx1 = *bx2 = x2;
		*by1 = *by2 = y2;
	} else {
		dx = -width * (y2 - y1) / length;
		dy = width * (x2 - x1) / length;

		*bx1 = x2 + dx;
		*bx2 = x2 - dx;
		*by1 = y2 + dy;
		*by2 = y2 - dy;

		if (project) {
			*bx1 += dy;
			*bx2 += dy;
			*by1 -= dx;
			*by2 -= dx;
		}
	}
}

/**
 * foo_canvas_polygon_to_point:
 * @poly: Vertices of the polygon.  X coordinates are in the even indices, and Y
 * coordinates are in the odd indices
 * @num_points: Number of points in the polygon
 * @x: X coordinate of the point
 * @y: Y coordinate of the point
 *
 * Computes the distance between a point and a polygon.
 *
 * Return value: The distance from the point to the polygon, or zero if the
 * point is inside the polygon.
 **/
double
foo_canvas_polygon_to_point (double *poly, int num_points, double x, double y)
{
	double best;
	int intersections;
	int i;
	double *p;
	double dx, dy;

	/* Iterate through all the edges in the polygon, updating best and intersections.
	 *
	 * When computing intersections, include left X coordinate of line within its range, but not
	 * Y coordinate.  Otherwise if the point lies exactly below a vertex we'll count it as two
	 * intersections.
	 */

	best = 1.0e36;
	intersections = 0;

	for (i = num_points, p = poly; i > 1; i--, p += 2) {
		double px, py, dist;

		/* Compute the point on the current edge closest to the point and update the
		 * intersection count.  This must be done separately for vertical edges, horizontal
		 * edges, and others.
		 */

		if (p[2] == p[0]) {
			/* Vertical edge */

			px = p[0];

			if (p[1] >= p[3]) {
				py = MIN (p[1], y);
				py = MAX (py, p[3]);
			} else {
				py = MIN (p[3], y);
				py = MAX (py, p[1]);
			}
		} else if (p[3] == p[1]) {
			/* Horizontal edge */

			py = p[1];

			if (p[0] >= p[2]) {
				px = MIN (p[0], x);
				px = MAX (px, p[2]);

				if ((y < py) && (x < p[0]) && (x >= p[2]))
					intersections++;
			} else {
				px = MIN (p[2], x);
				px = MAX (px, p[0]);

				if ((y < py) && (x < p[2]) && (x >= p[0]))
					intersections++;
			}
		} else {
			double m1, b1, m2, b2;
			int lower;

			/* Diagonal edge.  Convert the edge to a line equation (y = m1*x + b1), then
			 * compute a line perpendicular to this edge but passing through the point,
			 * (y = m2*x + b2).
			 */

			m1 = (p[3] - p[1]) / (p[2] - p[0]);
			b1 = p[1] - m1 * p[0];

			m2 = -1.0 / m1;
			b2 = y - m2 * x;

			px = (b2 - b1) / (m1 - m2);
			py = m1 * px + b1;

			if (p[0] > p[2]) {
				if (px > p[0]) {
					px = p[0];
					py = p[1];
				} else if (px < p[2]) {
					px = p[2];
					py = p[3];
				}
			} else {
				if (px > p[2]) {
					px = p[2];
					py = p[3];
				} else if (px < p[0]) {
					px = p[0];
					py = p[1];
				}
			}

			lower = (m1 * x + b1) > y;

			if (lower && (x >= MIN (p[0], p[2])) && (x < MAX (p[0], p[2])))
				intersections++;
		}

		/* Compute the distance to the closest point, and see if that is the best so far */

		dx = x - px;
		dy = y - py;
		dist = sqrt (dx * dx + dy * dy);
		if (dist < best)
			best = dist;
	}

	/* We've processed all the points.  If the number of intersections is odd, the point is
	 * inside the polygon.
	 */

	if (intersections & 0x1)
		return 0.0;
	else
		return best;
}

/**
 * foo_canvas_item_reset_bounds:
 * @item: A canvas item
 *
 * Resets the bounding box of a canvas item to an empty rectangle.
 **/
void
foo_canvas_item_reset_bounds (FooCanvasItem *item)
{
	item->x1 = 0.0;
	item->y1 = 0.0;
	item->x2 = 0.0;
	item->y2 = 0.0;
}

/**
 * foo_canvas_update_bbox:
 * @canvas: the canvas needing update
 * @x1: Left coordinate of the new bounding box
 * @y1: Top coordinate of the new bounding box
 * @x2: Right coordinate of the new bounding box
 * @y2: Bottom coordinate of the new bounding box
 *
 * Sets the bbox to the new value, requesting full repaint.
 **/
void
foo_canvas_update_bbox (FooCanvasItem *item, int x1, int y1, int x2, int y2)
{
	foo_canvas_item_request_redraw (item);
	item->x1 = x1;
	item->y1 = y1;
	item->x2 = x2;
	item->y2 = y2;
	foo_canvas_item_request_redraw (item);
}

