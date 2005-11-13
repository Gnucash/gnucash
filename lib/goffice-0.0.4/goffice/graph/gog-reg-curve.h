/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-reg-curve.h :  
 *
 * Copyright (C) 2005 Jean Brefort (jean.brefort@normalesup.org)
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

#ifndef GOG_REG_CURVE_H
#define GOG_REG_CURVE_H

#include <goffice/graph/gog-series.h>
#include <goffice/graph/gog-data-set.h>

struct  _GogRegCurve {
	GogStyledObject	base;
	
	GogSeries 	  *series;
	gboolean  	   weighted;
	GODataVector 	  *weights;
	GogDatasetElement *bounds;
	gboolean skip_invalid; /* do not take into account invalid data */
	int ninterp; /* how many points to use for display the curve as a vpath */
};

typedef struct {
	GogStyledObjectClass base;

	double 		(*get_value_at) (GogRegCurve *reg_curve, double x);
	gchar const* 	(*get_equation) (GogRegCurve *reg_curve);
	double 		(*get_R2) 	(GogRegCurve *reg_curve);
} GogRegCurveClass;

#define GOG_REG_CURVE_TYPE	(gog_reg_curve_get_type ())
#define GOG_REG_CURVE(o)	(G_TYPE_CHECK_INSTANCE_CAST ((o), GOG_REG_CURVE_TYPE, GogRegCurve))
#define IS_GOG_REG_CURVE(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOG_REG_CURVE_TYPE))

GType gog_reg_curve_get_type (void);

GogRegCurve *gog_reg_curve_new_by_name  (char const *id);
GogRegCurve *gog_reg_curve_new_by_type  (GogRegCurveType const *type);
gchar const *gog_reg_curve_get_equation (GogRegCurve *reg_curve);
double       gog_reg_curve_get_R2       (GogRegCurve *reg_curve);
void         gog_reg_curve_get_bounds   (GogRegCurve *reg_curve, double *xmin, double *xmax);

#endif /* GOG_REG_CURVE_H */
