/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-lin-reg.c :  
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

#include <goffice/goffice-config.h>
#include "gog-lin-reg.h"
#include <goffice/app/module-plugin-defs.h>
#include <goffice/data/go-data.h>
#include <goffice/graph/gog-series-impl.h>
#include <goffice/utils/go-math.h>
#include <goffice/utils/go-regression.h>

#include <gsf/gsf-impl-utils.h>

GOFFICE_PLUGIN_MODULE_HEADER;

static GogObjectClass *gog_lin_reg_curve_parent_klass;

static void
gog_lin_reg_curve_update (GogObject *obj)
{
	GogLinRegCurve *rc = GOG_LIN_REG_CURVE (obj);
	GogSeries *series = GOG_SERIES (obj->parent);
	double const *y_vals, *x_vals = NULL;
	double *vx, *vy;
	double x, y;
	double xmin, xmax;
	int i, used, tmp, nb;

	g_return_if_fail (gog_series_is_valid (GOG_SERIES (series)));

	gog_reg_curve_get_bounds (&rc->base, &xmin, &xmax);
	y_vals = go_data_vector_get_values (
		GO_DATA_VECTOR (series->values[1].data));
	nb = go_data_vector_get_len (
		GO_DATA_VECTOR (series->values[1].data));
	if (series->values[0].data) {
		x_vals = go_data_vector_get_values (
			GO_DATA_VECTOR (series->values[0].data));
		tmp = go_data_vector_get_len (
			GO_DATA_VECTOR (series->values[0].data));
		if (nb > tmp)
			nb = tmp;
	}
	vx = g_new (double, nb);
	vy = g_new (double, nb);
	for (i = 0, used = 0; i < nb; i++) {
		x = (x_vals)? x_vals[i]: i;
		y = y_vals[i];
		if (!go_finite (x) || !go_finite (y)) {
			if (rc->base.skip_invalid)
				continue;
			used = 0;
			break;
		}
		if (x < xmin || x > xmax)
			continue;
		vx[used] = x;
		vy[used] = y;
		used++;
	}
	rc->R2 = rc->a0 = rc->a1 = go_nan;
	if (used > 1) {
		double a[2];
		regression_stat_t *stats = go_regression_stat_new ();
		RegressionResult res = go_linear_regression (&vx, 1, vy, used,
								TRUE, a, stats);
		if (res == REG_ok) {
			rc->R2 = stats->sqr_r;
			rc->a0 = a[0];
			rc->a1 = a[1];
		}
		go_regression_stat_destroy (stats);
	}
	g_free (vx);
	g_free (vy);
	if (rc->equation) {
		g_free (rc->equation);
		rc->equation = NULL;
	}
	gog_object_emit_changed (GOG_OBJECT (obj), FALSE);
}

static double
gog_lin_reg_curve_get_value_at (GogRegCurve *curve, double x)
{
	GogLinRegCurve *lin = GOG_LIN_REG_CURVE (curve);
	return lin->a0 + lin->a1 * x;
}

static gchar const*
gog_lin_reg_curve_get_equation (GogRegCurve *curve)
{
	GogLinRegCurve *lin = GOG_LIN_REG_CURVE (curve);
	if (!lin->equation)
		lin->equation = (lin->a0 > 0.)?
				g_strdup_printf ("y = %g x + %g", lin->a1, lin->a0):
				g_strdup_printf ("y = %g x - %g", lin->a1, -lin->a0);
	return lin->equation;
}

static double
gog_lin_reg_curve_get_R2 (GogRegCurve *curve)
{
	return (GOG_LIN_REG_CURVE (curve))->R2;
}
	
static void
gog_lin_reg_curve_finalize (GObject *obj)
{
	GogLinRegCurve *model = GOG_LIN_REG_CURVE (obj);
	if (model->equation)
		g_free (model->equation);
	(G_OBJECT_CLASS (gog_lin_reg_curve_parent_klass))->finalize (obj);
}

static void
gog_lin_reg_curve_class_init (GogRegCurveClass *reg_curve_klass)
{
	GObjectClass *g_object_klass = (GObjectClass *) reg_curve_klass;
	GogObjectClass *gog_object_klass = (GogObjectClass *) reg_curve_klass;

	gog_lin_reg_curve_parent_klass = g_type_class_peek_parent (reg_curve_klass);

	g_object_klass->finalize = gog_lin_reg_curve_finalize;

	gog_object_klass->update = gog_lin_reg_curve_update;

	reg_curve_klass->get_value_at = gog_lin_reg_curve_get_value_at;
	reg_curve_klass->get_equation = gog_lin_reg_curve_get_equation;
	reg_curve_klass->get_R2 = gog_lin_reg_curve_get_R2;
}

static void
gog_lin_reg_curve_init (GogLinRegCurve *model)
{
	model->a0 = model->a1 = model->R2 = go_nan;
	model->equation = NULL;
}

GSF_DYNAMIC_CLASS (GogLinRegCurve, gog_lin_reg_curve,
	gog_lin_reg_curve_class_init, gog_lin_reg_curve_init,
	GOG_REG_CURVE_TYPE)

/* Plugin initialization */

G_MODULE_EXPORT void
go_plugin_init (GOPlugin *plugin, GOCmdContext *cc)
{
	gog_lin_reg_curve_register_type (go_plugin_get_type_module (plugin));
}

G_MODULE_EXPORT void
go_plugin_shutdown (GOPlugin *plugin, GOCmdContext *cc)
{
}
