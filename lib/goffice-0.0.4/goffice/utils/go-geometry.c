/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-geometry.c : A collection of geometry related functions.
 *
 * Copyright (C) 2005 Emmanuel Pacaud <emmanuel.pacaud@univ-poitiers.fr>
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

#include <goffice/utils/go-geometry.h>
#include <goffice/goffice-config.h>
#include <math.h>

#define dist(x0, y0, x1, y1) sqrt(((x0) - (x1))*((x0) - (x1)) + ((y0) - (y1))*((y0) - (y1)))

/**
 * go_geometry_cartesian_to_polar:
 * @x, @y: cartesian coordinates.
 * @rho, @theta: polar coordinates.
 *
 * Convert cartesion coordinates to polar coordinates.
 **/
void
go_geometry_cartesian_to_polar (double x, double y, double *rho, double *theta)
{
	*rho = sqrt (x * x + y * y);
	if (x != 0.) {
		*theta = atan (y / x);
		if (x < 0.) {
			*theta += M_PI;
		}
	} else {
		if (y < 0.) {
			*theta = -M_PI/2.0;
		} else {
			*theta = M_PI/2.0;
		}
	}
}

/**
 * go_geometry_point_to_segment:
 * @xp, @yp: point coordinates
 * @xs, ys: segment strt coordinates:
 * @w, @h: extent of segment.
 *
 * Computes the distance between a point and a segment.
 **/
double
go_geometry_point_to_segment (double xp, double yp, double xs, double ys, double w, double h)
{
	double c1, c2, b;

	c1 = w * (xp - xs) + h * (yp - ys);
	if (c1 <= 0.0)
		return dist (xp, yp, xs, ys);

	c2 = w * w + h * h;
	if (c2 <= c1)
		return dist (xp, yp, xs + w, ys + h);

	b = c1 / c2;
	return dist (xp, yp, xs + b * w, ys + b * h);
}

/**
 * go_geometry_AABR_add:
 * @aabr0: #GOGeometryAABR
 * @aabr1: #GOGeometryAABR
 *
 * Computes the Axis Aligned Bounding Rectangle of aabr0 and aabr1,
 * and stores result in aabr0.
 **/
void
go_geometry_AABR_add (GOGeometryAABR *aabr0, GOGeometryAABR const *aabr1)
{
	double min, max;

	min = MIN (aabr0->x, aabr1->x);
	max = MAX (aabr0->x + aabr0->w, aabr1->x + aabr1->w);
	aabr0->x = min;
	aabr0->w = max - min;
	
	min = MIN (aabr0->y, aabr1->y);
	max = MAX (aabr0->y + aabr0->h, aabr1->y + aabr1->h);
	aabr0->y = min;
	aabr0->h = max - min;
}

/**
 * go_geometry_OBR_to_AABR:
 * @obr: #GOGeometryOBR
 * @aabr: #GOGeometryAABR
 * 
 * Returns an Axis Aligned Bouding Rectangle of obr.
 **/
void
go_geometry_OBR_to_AABR (GOGeometryOBR const *obr, GOGeometryAABR *aabr)
{
	double cos_alpha = cos (obr->alpha);
	double sin_alpha = sin (obr->alpha);

	aabr->w = fabs (obr->w * cos_alpha) + fabs (obr->h * sin_alpha);
	aabr->h = fabs (obr->w * sin_alpha) + fabs (obr->h * cos_alpha);
	aabr->x = obr->x - aabr->w / 2.0 ;
	aabr->y = obr->y - aabr->h / 2.0 ;
}

/** 
 * go_geometry_test_OBR_overlap:
 * @obr0: #GOGeometryOBR
 * @obr1: #GOGeometryOBR
 * 
 * Overlap test of Oriented Bounding Rectangles by the separating axis method.
 **/
gboolean
go_geometry_test_OBR_overlap (GOGeometryOBR const *obr0, GOGeometryOBR const *obr1)
{
        double TL, pa, pb;
        double cos_delta, sin_delta;
        double cos_alpha, sin_alpha;
        double delta_x, delta_y;
	double a00, a01, a10, a11;

        delta_x = obr1->x - obr0->x;
        delta_y = obr1->y - obr0->y;
        cos_delta = cos (obr1->alpha - obr0->alpha);
        sin_delta = sin (obr1->alpha - obr0->alpha);

        cos_alpha = fabs (cos (obr0->alpha));
        sin_alpha = fabs (sin (obr0->alpha));

	a00 = fabs (obr0->w / 2.0);
	a01 = fabs (obr0->h / 2.0);
	a10 = fabs (obr1->w / 2.0);
	a11 = fabs (obr1->h / 2.0);

        /* Separating axis parallel to obr0->w */
        TL = fabs (delta_x * cos_alpha - delta_y * sin_alpha);
        pa = a00;
        pb = a10 * cos_delta + a11 * sin_delta;
        if (TL > pa + pb) return FALSE;
        
        /* Separating axis parallel to obr->h */
        TL = fabs (delta_x * sin_alpha + delta_y * cos_alpha);
        pa = a01;
        pb = a10 * sin_delta + a11 * cos_delta;
        if (TL > pa + pb) return FALSE;

        cos_alpha = fabs (cos (obr1->alpha));
        sin_alpha = fabs (sin (obr1->alpha));

        /* Separating axis parallel to obr1->w */
        TL = fabs (delta_x * cos_alpha - delta_y * sin_alpha);
        pa = a00 * cos_delta + a01 * sin_delta;
        pb = a10;
        if (TL > pa + pb) return FALSE;

        /* Separating axis parallel to obr1->h */
        TL = fabs (delta_x * sin_alpha + delta_y * cos_alpha);
        pa = a00 * sin_delta + a01 * cos_delta;
        pb = a11;
        if (TL > pa + pb) return FALSE;
        
        return TRUE;
}

/** 
 * go_geometry_get_rotation_type:
 * @alpha: angle in radians
 *
 * Returns rotation type for handling of special angles (alpha = n * pi / 2)
 **/
GOGeometryRotationType 
go_geometry_get_rotation_type (double alpha) 
{
	unsigned index;

	if (alpha < 0 || alpha > 2 * M_PI)
		alpha = alpha - 2 * M_PI * floor (alpha / (2 * M_PI));
	
	if (fmod(alpha + GO_GEOMETRY_ANGLE_TOLERANCE, M_PI / 2.0) > 2 * GO_GEOMETRY_ANGLE_TOLERANCE)
		return GO_ROTATE_FREE;
	index = rint (2.0 * alpha / M_PI);
	return index < GO_ROTATE_FREE ? index : GO_ROTATE_NONE;
}

/**
 * go_geometry_update_label_OBR:
 * @obr: bouding rectangle of label
 * @alpha: angle of axis
 * @offset: minimum distance between label and axis
 * @side: side of label with respect to axis
 *
 * Convenience routine that computes position of a label relative to an axis. 
 **/
void
go_geometry_calc_label_position (GOGeometryOBR *obr, double alpha, double offset, GOGeometrySide side)
{
	GOGeometryAABR aabr;
	
	go_geometry_OBR_to_AABR (obr, &aabr);

	offset += (fabs (aabr.w * sin (alpha)) + fabs (aabr.h * cos (alpha))) / 2.0;
	obr->x = offset * ((side == GO_SIDE_RIGHT) ? - sin (alpha) : + sin (alpha));
	obr->y = offset * ((side == GO_SIDE_RIGHT) ? + cos (alpha) : - cos (alpha));
}	
