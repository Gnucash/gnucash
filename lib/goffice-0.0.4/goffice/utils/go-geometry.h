/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * go-geometry.h : A collection of geometry related functions.
 *
 * Copyright (C) 2005 Emmanuel Pacaud (emmanuel.pacaud@univ-poitiers.fr)
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

#ifndef GO_GEOMETRY_H
#define GO_GEOMETRY_H

#include <goffice/graph/goffice-graph.h>

G_BEGIN_DECLS

#define GO_GEOMETRY_ANGLE_TOLERANCE 1E-3

typedef enum {
	GO_SIDE_LEFT,
	GO_SIDE_RIGHT
} GOGeometrySide;

typedef enum {
	GO_ROTATE_NONE = 0,
	GO_ROTATE_COUNTERCLOCKWISE = 1,
	GO_ROTATE_UPSIDEDOWN = 2,
	GO_ROTATE_CLOCKWISE = 3,	
	GO_ROTATE_FREE = 4
} GOGeometryRotationType;

typedef struct {
        double x,y;     /* Center */
        double w,h;     /* Edges */
        double alpha;   /* Angle from x axis to w edge, in radians */
} GOGeometryOBR;

#define GOGeometryAABR GogViewAllocation /* x, y: top left corner */

void 		go_geometry_cartesian_to_polar 	(double x, double y, double *rho, double *theta);
double 		go_geometry_point_to_segment	(double xp, double yp, double xs, double ys, double w, double h);

void 		go_geometry_AABR_add 		(GOGeometryAABR *aabr0, GOGeometryAABR const *aabr1);
void		go_geometry_OBR_to_AABR 	(GOGeometryOBR const *obr, GOGeometryAABR *aabr);
gboolean 	go_geometry_test_OBR_overlap 	(GOGeometryOBR const *obr0, GOGeometryOBR const *obr1);

GOGeometryRotationType 
		go_geometry_get_rotation_type	(double angle);

void 		go_geometry_calc_label_position	(GOGeometryOBR *obr, double alpha, double offset, GOGeometrySide side);

G_END_DECLS

#endif /* GO_GEOMETRY_H */
