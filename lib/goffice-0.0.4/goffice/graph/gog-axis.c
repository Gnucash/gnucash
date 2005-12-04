/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-axis.c :
 *
 * Copyright (C) 2003-2004 Jody Goldberg (jody@gnome.org)
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
#include <goffice/graph/gog-axis.h>
#include <goffice/graph/gog-axis-line-impl.h>
#include <goffice/graph/gog-grid-line.h>
#include <goffice/graph/gog-styled-object.h>
#include <goffice/graph/gog-style.h>
#include <goffice/graph/gog-theme.h>
#include <goffice/graph/gog-data-set.h>
#include <goffice/graph/gog-data-allocator.h>
#include <goffice/graph/gog-view.h>
#include <goffice/graph/gog-chart.h>
#include <goffice/graph/gog-label.h>
#include <goffice/graph/gog-plot.h>
#include <goffice/graph/gog-plot-impl.h>
#include <goffice/graph/gog-renderer.h>
#include <goffice/utils/go-format.h>
#include <goffice/utils/go-math.h>
#include <goffice/gtk/goffice-gtk.h>
#include <goffice/gtk/go-format-sel.h>

#include <gsf/gsf-impl-utils.h>
#include <glib/gi18n.h>

#include <gtk/gtkcellrenderertext.h>
#include <gtk/gtkcelllayout.h>
#include <gtk/gtkcheckbutton.h>
#include <gtk/gtkcombobox.h>
#include <gtk/gtklabel.h>
#include <gtk/gtkliststore.h>
#include <gtk/gtkmisc.h>
#include <gtk/gtknotebook.h>
#include <gtk/gtktable.h>
#include <gtk/gtktogglebutton.h>

#include <string.h>

/* this should be per model */
#define PAD_HACK	4	/* pts */

struct _GogAxis {
	GogAxisBase	 base;

	GogAxisType	 type;
	GSList		*contributors;

	GogDatasetElement source [GOG_AXIS_ELEM_CROSS_POINT];
	double		  auto_bound [GOG_AXIS_ELEM_CROSS_POINT];
	gboolean inverted; /* apply to all map type */

	double		min_val, max_val;
	double		logical_min_val, logical_max_val;
	gpointer	min_contrib, max_contrib; /* NULL means use the manual sources */
	gboolean	is_discrete;
	gboolean	center_on_ticks;
	GODataVector   *labels;
	GogPlot	       *plot_that_supplied_labels;
	GOFormat       *format, *assigned_format;

	GogAxisMapDesc const 	*map_desc;

	GogAxisTick	*ticks;
	unsigned	 tick_nbr;
};

/*****************************************************************************/

#define TICK_LABEL_PAD_VERT	0
#define TICK_LABEL_PAD_HORIZ	1

#define GOG_AXIS_MAX_TICK_NBR				1000
#define GOG_AXIS_LOG_AUTO_MAX_MAJOR_TICK_NBR 		8
#define GOG_AXIS_DISCRETE_AUTO_MAX_MAJOR_TICK_NBR 	20

static void gog_axis_set_ticks (GogAxis *axis,int tick_nbr, GogAxisTick *ticks);

static GogAxisTick *
get_adjusted_tick_array (GogAxisTick *ticks, int allocated_size, int exact_size) 
{
	GogAxisTick *tmp_ticks;
	
	if (exact_size > 0) {
		if (exact_size != allocated_size) {
			tmp_ticks = g_new (GogAxisTick, exact_size);
			memcpy (tmp_ticks, ticks, sizeof (GogAxisTick) * exact_size);
			g_free (ticks);
		} else
			tmp_ticks = ticks;
	} else {
		g_free (ticks);
		tmp_ticks = NULL;
	}

	return tmp_ticks;
}

static GogAxisTick *
create_invalid_axis_ticks (double min, double max) 
{
	GogAxisTick *ticks;
	
	ticks = g_new (GogAxisTick, 2);
	ticks[0].position = min;
	ticks[1].position = max;
	ticks[0].type = ticks[1].type = GOG_AXIS_TICK_MAJOR;
	ticks[0].label = g_strdup ("##");
	ticks[1].label = g_strdup ("##");

	return ticks;
}

/*
 * Discrete mapping
 */

typedef struct
{
	double min;
	double max;
	double scale;
	double a;
	double b;
} MapData;

static gboolean
map_discrete_init (GogAxisMap *map, double offset, double length)
{
	MapData *data;
	
	map->data = g_new (MapData, 1);
	data = map->data;

	if (gog_axis_get_bounds (map->axis, &data->min, &data->max)) {
		data->scale = 1.0 / (data->max - data->min);
		data->a = data->scale * length;
		data->b = offset - data->a * data->min;
		return TRUE;
	}
	data->min = 0.0;
	data->max = 1.0;
	data->scale = 1.0;
	data->a = length;
	data->b = offset;
	return FALSE;
}

static double
map_discrete (GogAxisMap *map, double value) 
{
	MapData *data = map->data;
	
	return (value - data->min) * data->scale;
}

static double
map_discrete_to_view (GogAxisMap *map, double value)
{
	MapData *data = map->data;

	return map->axis->inverted ? 
		(data->min + data->max - value) * data->a + data->b :
		value * data->a + data->b;
}

static double
map_discrete_from_view (GogAxisMap *map, double value)
{
	MapData *data = map->data;

	return map->axis->inverted ? 
		data->min + data->max - (value - data->b) / data->a :
		(value - data->b) / data->a;
}

static void
map_discrete_auto_bound (GogAxis *axis, 
			 double minimum, 
			 double maximum, 
			 double *bound)
{
	if ((maximum - minimum) > GOG_AXIS_DISCRETE_AUTO_MAX_MAJOR_TICK_NBR) 
		bound [GOG_AXIS_ELEM_MAJOR_TICK] = 
		bound [GOG_AXIS_ELEM_MINOR_TICK] =
			ceil ((maximum - minimum + 1.0) / 
			      (double) GOG_AXIS_DISCRETE_AUTO_MAX_MAJOR_TICK_NBR); 
	else
		bound [GOG_AXIS_ELEM_MAJOR_TICK] = 
		bound [GOG_AXIS_ELEM_MINOR_TICK] = 1.;

	bound [GOG_AXIS_ELEM_MIN] = minimum;
	bound [GOG_AXIS_ELEM_MAX] = maximum;
}

static void
map_discrete_calc_ticks (GogAxis *axis) 
{
	GogAxisTick *ticks;
	gboolean valid;
	double maximum, minimum;
	double tick_start, label_start;
	int tick_nbr, label_nbr;
	int i, j, index;
	int major_tick, major_label;

	major_tick = go_rint (gog_axis_get_entry (axis, GOG_AXIS_ELEM_MAJOR_TICK, NULL));
	major_label = go_rint (gog_axis_get_entry (axis, GOG_AXIS_ELEM_MINOR_TICK, NULL));
	if (major_tick < 1)
		major_tick = 1;
	if (major_label < 1)
		major_label = 1;
	
	valid = gog_axis_get_bounds (axis, &minimum, &maximum);
	if (!valid) {
		gog_axis_set_ticks (axis, 2, create_invalid_axis_ticks (0.0, 1.0));
		return;
	}
		
	tick_start = axis->center_on_ticks ? 
		ceil (minimum / (double) major_tick) * major_tick : 
		ceil ((minimum - 0.5) / (double) major_tick) * major_tick + 0.5;
	label_start = ceil (minimum / (double) major_label) * major_label;
	tick_nbr = floor (go_add_epsilon (maximum - tick_start) / major_tick + 1.0);
	label_nbr = floor (go_add_epsilon (maximum - label_start) / major_label + 1.0);
	tick_nbr = CLAMP (tick_nbr, 0, GOG_AXIS_MAX_TICK_NBR);
	label_nbr = CLAMP (label_nbr, 0, GOG_AXIS_MAX_TICK_NBR);
	if (tick_nbr < 1  && label_nbr < 1) {
		gog_axis_set_ticks (axis, 2, create_invalid_axis_ticks (0.0, 1.0));
		return;
	}
	ticks = g_new (GogAxisTick, tick_nbr + label_nbr);
	
	for (i = 0; i < tick_nbr; i++) {
		ticks[i].position = tick_start + (double) (i) * major_tick;
		ticks[i].type = GOG_AXIS_TICK_MAJOR;
		ticks[i].label = NULL;
	}
	for (i = 0, j = tick_nbr; i < label_nbr; i++, j++) {
		ticks[j].position = go_rint (label_start + (double) (i) * major_label);
		index = ticks[j].position - 1;
		ticks[j].type = GOG_AXIS_TICK_NONE;
		if (axis->labels != NULL) {
			if (index < go_data_vector_get_len (axis->labels) && index >= 0)
				ticks[j].label = go_data_vector_get_str (axis->labels, index);
			else
				ticks[j].label = NULL;
		}
		else
			ticks[j].label = g_strdup_printf ("%d", index + 1);
	}

	gog_axis_set_ticks (axis, tick_nbr + label_nbr, ticks);
}

/*
 *  Linear mapping 
 */

static gboolean
map_linear_init (GogAxisMap *map, double offset, double length)
{
	MapData *data;
	
	map->data = g_new (MapData, 1);
	data = (MapData *) map->data;

	if (gog_axis_get_bounds (map->axis, &data->min, &data->max)) {
		data->scale = 1 / (data->max - data->min);
		data->a = data->scale * length;
		data->b = offset - data->a * data->min;
		return TRUE;
	}
	data->min = 0.0;
	data->max = 1.0;
	data->scale = 1.0;
	data->a = length;
	data->b = offset;
	return FALSE;
}

static double
map_linear (GogAxisMap *map, double value) 
{
	MapData *data = map->data;
	
	return (value - data->min) * data->scale;
}

static double
map_linear_to_view (GogAxisMap *map, double value)
{
	MapData *data = map->data;

	return map->axis->inverted ? 
		(data->min + data->max - value) * data->a + data->b :
		value * data->a + data->b;
}

static double
map_linear_from_view (GogAxisMap *map, double value)
{
	MapData *data = map->data;

	return map->axis->inverted ? 
		data->min + data->max - (value - data->b) / data->a :
		(value - data->b) / data->a;
}

static double
map_baseline (GogAxisMap *map)
{
	MapData *data = map->data;

	if (0. < data->min)
		return map_linear_to_view (map, data->min);
	else if (0 > data->max)
		return map_linear_to_view (map, data->max);

	return map_linear_to_view (map, 0.);
}

static void
map_bounds (GogAxisMap *map, double *minimum, double *maximum)
{
	MapData *data = map->data;

	if (minimum != NULL) *minimum = data->min;
	if (maximum != NULL) *maximum = data->max;	
}

static void
map_linear_auto_bound (GogAxis *axis, double minimum, double maximum, double *bound)
{
	double step, range, mant;
	int expon;

	if (gog_axis_get_atype (axis) == GOG_AXIS_CIRCULAR) {
		bound[GOG_AXIS_ELEM_MIN] = 0.0;
		bound[GOG_AXIS_ELEM_MAX] = 360.0;
		bound[GOG_AXIS_ELEM_MAJOR_TICK] = 30.0;
		bound[GOG_AXIS_ELEM_MINOR_TICK] = 10.0;
		return;
	}	

	range = fabs (maximum - minimum);

	/* handle singletons */
	if (go_sub_epsilon (range) <= 0.) {
		if (maximum > 0)
			minimum = 0.;
		else if (minimum < 0.)
			maximum = 0.;
		else {
			maximum = 1;
			minimum = 0;
		}

		range = fabs (maximum - minimum);
	}

	step  = pow (10, go_fake_floor (log10 (range)));
	if (range/step < 1.6)
		step /= 5.;	/* .2 .4 .6 */
	else if (range/step < 3)
		step /= 2.;	/* 0 5 10 */
	else if (range/step > 8)
		step *= 2.;	/* 2 4 6 */

	/* we want the bounds to be loose so jump up a step if we get too close */
	mant = frexp (minimum / step, &expon);
	bound [GOG_AXIS_ELEM_MIN] = step * floor (ldexp (mant - DBL_EPSILON, expon));
	mant = frexp (maximum / step, &expon);
	bound [GOG_AXIS_ELEM_MAX] = step * ceil (ldexp (mant + DBL_EPSILON, expon));
	bound [GOG_AXIS_ELEM_MAJOR_TICK] = step;
	bound [GOG_AXIS_ELEM_MINOR_TICK] = step / 5.;

	/* pull to zero if its nearby (do not pull both directions to 0) */
	if (bound [GOG_AXIS_ELEM_MIN] > 0 &&
	    (bound [GOG_AXIS_ELEM_MIN] - 10. * step) < 0)
		bound [GOG_AXIS_ELEM_MIN] = 0;
	else if (bound [GOG_AXIS_ELEM_MAX] < 0 &&
	    (bound [GOG_AXIS_ELEM_MAX] + 10. * step) > 0)
		bound [GOG_AXIS_ELEM_MAX] = 0;

	/* The epsilon shift can pull us away from a zero we want to
	 * keep (eg percentage bars withno negative elements) */
	if (bound [GOG_AXIS_ELEM_MIN] < 0 && minimum >= 0.)
		bound [GOG_AXIS_ELEM_MIN] = 0;
	else if (bound [GOG_AXIS_ELEM_MAX] > 0 && maximum <= 0.)
		bound [GOG_AXIS_ELEM_MAX] = 0;
}

static void
map_linear_calc_ticks (GogAxis *axis) 
{
	GogAxisTick *ticks;
	double maximum, minimum, start;
	double tick_step;
	double major_tick, minor_tick, ratio;
	int tick_nbr, i;

	if (!gog_axis_get_bounds (axis, &minimum, &maximum)) {
		gog_axis_set_ticks (axis, 2, create_invalid_axis_ticks (0.0, 1.0));
		return;
	}

	major_tick = gog_axis_get_entry (axis, GOG_AXIS_ELEM_MAJOR_TICK, NULL);
	minor_tick = gog_axis_get_entry (axis, GOG_AXIS_ELEM_MINOR_TICK, NULL);
	if (major_tick <= 0.) major_tick = maximum - minimum;
	if (minor_tick <= 0.) minor_tick = maximum - minimum;
	if (minor_tick < major_tick) {
		minor_tick = major_tick / rint (major_tick / minor_tick);
		tick_step = minor_tick;
	} else
		tick_step = major_tick;
	
	start = ceil (minimum / tick_step) * tick_step;
	tick_nbr = floor (go_add_epsilon ((maximum - start) / tick_step + 1.0));
	if (tick_nbr < 1 || tick_nbr > GOG_AXIS_MAX_TICK_NBR) {
		gog_axis_set_ticks (axis, 0, NULL);
		return;
	}
	ticks = g_new0 (GogAxisTick, tick_nbr);

	for (i = 0; i < tick_nbr; i++) {
		ticks[i].position = start + (double) i * tick_step;
		if (fabs (ticks[i].position) < tick_step / 1E10)
			ticks[i].position = 0.0;
		ratio = ticks[i].position / major_tick;
		if (fabs (ratio - rint (ratio)) < 1E-3) {
			ticks[i].type = GOG_AXIS_TICK_MAJOR;
				if (axis->assigned_format == NULL || 
				    go_format_is_general (axis->assigned_format))
					ticks[i].label = go_format_value (axis->format, ticks[i].position);
				else
					ticks[i].label = go_format_value (axis->assigned_format, ticks[i].position);
			}
		else {
			ticks[i].type = GOG_AXIS_TICK_MINOR;
			ticks[i].label = NULL;
		}
	}
	gog_axis_set_ticks (axis, tick_nbr, ticks);
}

/*
 * Logarithmic mapping
 */

typedef struct
{
	double min;
	double max;
	double scale;
	double a;
	double b;
	double a_inv;
	double b_inv;
} MapLogData;

static gboolean
map_log_init (GogAxisMap *map, double offset, double length)
{
	MapLogData *data;
	
	map->data = g_new (MapLogData, 1);
	data = map->data;

	if (gog_axis_get_bounds (map->axis, &data->min, &data->max))  
		if (data->min > 0.0)  {
			data->min = log (data->min);
			data->max = log (data->max);
			data->scale = 1/ (data->max - data->min);
			data->a = data->scale * length;
			data->b = offset - data->a * data->min;
			data->a_inv = -data->scale * length;
			data->b_inv = offset + length - data->a_inv * data->min;
			return TRUE;
		}
	
	data->min = 0.0;
	data->max = log (10.0);
	data->scale = 1 / log(10.0);
	data->a = data->scale * length;
	data->b = offset;
	data->a_inv = -data->scale * length;
	data->b_inv = offset + length;

	return FALSE;
}

static double
map_log (GogAxisMap *map, double value) 
{
	MapLogData *data = map->data;
	
	return (log (value) - data->min) * data->scale;
}

static double
map_log_to_view (GogAxisMap *map, double value)
{
	MapLogData *data = map->data;
	double result;
	
	if (value <= 0.) 
		/* Make libart happy */
		result = map->axis->inverted ? -DBL_MAX : DBL_MAX;
	else
		result = map->axis->inverted ? 
			log (value) * data->a_inv + data->b_inv :
			log (value) * data->a + data->b;

	return result;
}

static double
map_log_from_view (GogAxisMap *map, double value)
{
	MapLogData *data = map->data;
	
	return  map->axis->inverted ? 
		exp ((value - data->b_inv) / data->a_inv) :
		exp ((value - data->b) / data->a);
}

static gboolean 
map_log_finite (double value)
{
	return go_finite (value) && value > 0.;
}

static double
map_log_baseline (GogAxisMap *map)
{
	MapLogData *data = map->data;

	return map->axis->inverted ?
		data->max * data->a_inv + data->b_inv :
		data->min * data->a + data->b;
}

static void
map_log_bounds (GogAxisMap *map, double *minimum, double *maximum)
{
	MapLogData *data = map->data;

	if (minimum != NULL) *minimum = exp (data->min);
	if (maximum != NULL) *maximum = exp (data->max);	
}

static void
map_log_auto_bound (GogAxis *axis, double minimum, double maximum, double *bound)
{
	double step;

	if (maximum <= 0.0)
		maximum = 1.0;
	if (minimum <= 0.0)
		minimum = maximum / 100.0;
	if (maximum < minimum)
		maximum = minimum * 100.0;

	maximum = ceil (log10 (maximum));
	minimum = floor (log10 (minimum));
	
	step = ceil ((maximum - minimum + 1.0) / 
		     (double) GOG_AXIS_LOG_AUTO_MAX_MAJOR_TICK_NBR); 

	bound [GOG_AXIS_ELEM_MIN] = pow ( 10.0, minimum);
	bound [GOG_AXIS_ELEM_MAX] = pow ( 10.0, maximum);
	bound [GOG_AXIS_ELEM_MAJOR_TICK] = step;
	bound [GOG_AXIS_ELEM_MINOR_TICK] = 8;
}

static void
map_log_calc_ticks (GogAxis *axis)
{
	GogAxisTick *ticks;
	double maximum, minimum;
	double position;
	int major_tick, minor_tick, major_label, start_tick;
	int tick_nbr, i, j;
	int count;

	major_label = rint (gog_axis_get_entry (axis, GOG_AXIS_ELEM_MAJOR_TICK, NULL));
	minor_tick = rint (gog_axis_get_entry (axis, GOG_AXIS_ELEM_MINOR_TICK, NULL) + 1.0);

	if (!gog_axis_get_bounds (axis, &minimum, &maximum) || major_label < 1) {
		gog_axis_set_ticks (axis, 2, create_invalid_axis_ticks (1.0, 10.0));
		return;
	}
	if (minimum <= 0.0) {
		gog_axis_set_ticks (axis, 2, create_invalid_axis_ticks (1.0, 10.0));
		return;
	}

	start_tick = ceil (log10 (minimum));
	tick_nbr = major_tick = ceil (ceil (log10 (maximum)) - floor (log10 (minimum)) + 1.0);
	tick_nbr *= minor_tick;
	if (tick_nbr < 1 || tick_nbr > GOG_AXIS_MAX_TICK_NBR) {
		gog_axis_set_ticks (axis, 0, NULL);
		return;
	}
	ticks = g_new0 (GogAxisTick, tick_nbr);

	count = 0;
	for (i = 0; i < major_tick; i++) {
		position = pow (10.0, i + start_tick);
		if (position >= go_sub_epsilon (minimum) && go_sub_epsilon (position) <= maximum) {
			ticks[count].position = position;
			if (i % major_label == 0) {
				ticks[count].type = GOG_AXIS_TICK_MAJOR;
				if (axis->assigned_format == NULL || 
				    go_format_is_general (axis->assigned_format))
					ticks[count].label = go_format_value (axis->format, ticks[count].position);
				else
					ticks[count].label = go_format_value (axis->assigned_format, ticks[count].position);
				count++;
			}
			else {
				ticks[count].type = GOG_AXIS_TICK_MINOR;
				ticks[count].label = NULL;
				count++;
			}
		} 
			for (j = 1; j < minor_tick; j++) {
				position = pow (10.0, i + start_tick) * (9.0 / (double)minor_tick * (double) j + 1.0);
				if (position >= go_sub_epsilon (minimum) && go_sub_epsilon (position) <= maximum) {
					ticks[count].position = position;
					ticks[count].type = GOG_AXIS_TICK_MINOR;
					ticks[count].label = NULL;
					count++;
				}
			}
	}

	ticks = get_adjusted_tick_array (ticks, tick_nbr, count);
	gog_axis_set_ticks (axis, count, ticks);
}

static const GogAxisMapDesc map_desc_discrete = 
{
	map_discrete,			map_discrete_to_view,
	map_discrete_from_view,		go_finite,
	map_baseline,			map_bounds,
	map_discrete_init,		NULL,
	map_discrete_auto_bound,	map_discrete_calc_ticks,
	N_("Discrete"),			N_("Discrete mapping")
};

static const GogAxisMapDesc map_descs[] = 
{
	{
		map_linear,		map_linear_to_view,
		map_linear_from_view,   go_finite,
		map_baseline,		map_bounds,
		map_linear_init, 	NULL,	
		map_linear_auto_bound, 	map_linear_calc_ticks,	
		N_("Linear"),		N_("Linear mapping")
	},
	{
		map_log,		map_log_to_view,
		map_log_from_view,	map_log_finite,
		map_log_baseline,	map_log_bounds,
		map_log_init,		NULL,	
		map_log_auto_bound, 	map_log_calc_ticks,	
		N_("Log"),		N_("Logarithm mapping")
	}
};

static void
gog_axis_map_set_by_num (GogAxis *axis, unsigned num)
{
	g_return_if_fail (GOG_AXIS (axis) != NULL);

	if (num >= 0 && num < G_N_ELEMENTS (map_descs))
		g_object_set (G_OBJECT (axis), "map-name", map_descs[num].name, NULL);
	else
		g_object_set (G_OBJECT (axis), "map-name", "", NULL);
}

static void
gog_axis_map_populate_combo (GogAxis *axis, GtkComboBox *combo)
{
	unsigned i;

	g_return_if_fail (GOG_AXIS (axis) != NULL);

	for (i = 0; i < G_N_ELEMENTS (map_descs); i++) {
		gtk_combo_box_append_text (combo, _(map_descs[i].name));
		if (!g_ascii_strcasecmp (map_descs[i].name,
					 axis->map_desc->name))
			gtk_combo_box_set_active (combo, i);
	}
}

static void
gog_axis_map_set (GogAxis *axis, char const *name) 
{
	unsigned i, map = 0;
	
	g_return_if_fail (GOG_AXIS (axis) != NULL);

	if (name != NULL)
		for (i = 0; i < G_N_ELEMENTS(map_descs); i++) 
			if (!g_ascii_strcasecmp (name, map_descs[i].name)) {
				map = i;
				break;
			}

	axis->map_desc = &map_descs[map];
}

/**
 * gog_axis_map_is_valid
 * @axis : #GogAxis
 *
 * Return TRUE if map is valid, ie bounds are valid.
 **/

gboolean
gog_axis_map_is_valid (GogAxisMap *map) 
{
	g_return_val_if_fail (map != NULL, FALSE);

	return map->is_valid;
}

/**
 * gog_axis_map_new :
 * @axis : #GogAxis
 * @offset : start of plot area.
 * @length : length of plot area.
 *
 * Return a new GogAxisMap for data mapping to plot window.
 * offset and length are optional parameters to be used with 
 * gog_axis_map_to_view in order to translates data coordinates 
 * into canvas space.
 **/

GogAxisMap *
gog_axis_map_new (GogAxis *axis, double offset, double length)
{
	GogAxisMap *map;
	
	g_return_val_if_fail (GOG_AXIS (axis) != NULL, NULL);
	
	map = g_new0 (GogAxisMap, 1);

	g_object_ref (axis);
	map->desc = axis->is_discrete ? &map_desc_discrete : axis->map_desc;
	map->axis = axis;
	map->data = NULL;
	map->is_valid = FALSE;

	if (map->desc->init != NULL)
		map->is_valid = map->desc->init (map, offset, length);

	return map;
}

/**
 * gog_axis_map :
 * @map : #GogAxisMap
 * value : value to map to plot space.
 *
 * Return a value where [0,1.0] means a data within plot
 * bounds.
 * */

double 
gog_axis_map (GogAxisMap *map,
	      double value)
{
	return (map->axis->inverted ?
		1.0 - map->desc->map (map, value) :
		map->desc->map (map, value));
}

/**
 * gog_axis_map_from_view :
 * @map : #GogAxisMap
 * @value : value to unmap from canvas space.
 **/

double 
gog_axis_map_from_view (GogAxisMap *map,
			double value)
{
	return map->desc->map_from_view (map, value);
}

/**
 * gog_axis_map_to_view :
 * @map : #GogAxisMap
 * @value : value to map to canvas space.
 *
 * Return a value in canvas coordinates, where
 * [offset,offset+length] means a data within plot bounds.
 **/

double 
gog_axis_map_to_view (GogAxisMap *map,
			double value)
{
	return map->desc->map_to_view (map, value);
}

/**
 * gog_axis_map_finite :
 * @map : #GogAxisMap
 * @value : value to test
 *
 * Returns TRUE if value means something in this map
 **/

gboolean 
gog_axis_map_finite (GogAxisMap *map, double value)
{
	return map->desc->map_finite (value);
}

/**
 * gog_axis_map_get_baseline :
 * @map : #GogAxisMap
 *
 * Returns the baseline for the given map, in view coordinates,
 * clipped to offset and offset+length, where offset and length
 * are the parameters of gog_axis_map_new.
 **/

double
gog_axis_map_get_baseline (GogAxisMap *map)
{
	return map->desc->map_baseline (map);
}

/**
 * gog_axis_map_get_extents:
 * @map : #GogAxisMap
 * @start : start for this axis
 * @stop : stop for this axis
 *
 * Returns start and stop for the given axis map in data coordinates. If
 * axis is not inverted, start = minimum and stop = maximum. If axis is invalid, 
 * it'll return arbitrary bounds. For example, an non inverted invalid X axis 
 * will have start set to 0.0 and stop set to 1.0.
 *
 * minimum or maximum can be NULL.
 * */

void
gog_axis_map_get_extents (GogAxisMap *map, double *start, double *stop)
{
	if (map->axis->inverted)
		return map->desc->map_bounds (map, stop, start);
	else
		return map->desc->map_bounds (map, start, stop);
}

/**
 * gog_axis_map_get_bounds:
 * @map : #GogAxisMap
 * @minimum : minimum for this axis
 * @maximum : maximum for this axis
 *
 * Returns bounds for the given axis map in data coordinates. If axis is invalid, 
 * it'll return arbitrary bounds. For example, for an invalid x axis, minimum = 0.0
 * and maximum = 1.0. 
 *
 * minimum or maximum can be NULL.
 * */

void
gog_axis_map_get_bounds (GogAxisMap *map, double *minimum, double *maximum)
{
	return map->desc->map_bounds (map, minimum, maximum);
}
/**
 * gog_axis_map_free :
 * @map : #GogAxisMap
 *
 * Free GogAxisMap object.
 **/

void
gog_axis_map_free (GogAxisMap *map)
{
	g_return_if_fail (map != NULL);

	if (map->desc->destroy != NULL)
		map->desc->destroy (map);

	g_object_unref (map->axis);
	g_free (map->data);
	g_free (map);
}

static void
gog_axis_auto_bound (GogAxis *axis)
{
	double maximum, minimum;
	double tmp;
	gboolean user_defined;
	
	g_return_if_fail (GOG_AXIS (axis) != NULL);

	minimum = axis->min_val;
	maximum = axis->max_val;

	tmp = gog_axis_get_entry (axis, GOG_AXIS_ELEM_MIN, &user_defined);
	if (user_defined) minimum = tmp;

	tmp = gog_axis_get_entry (axis, GOG_AXIS_ELEM_MAX, &user_defined);
	if (user_defined) maximum = tmp;

	if (axis->is_discrete)
		map_desc_discrete.auto_bound (axis, minimum, maximum, axis->auto_bound);
	else
		if (axis->map_desc->auto_bound)
			axis->map_desc->auto_bound (axis, minimum, maximum, axis->auto_bound);
}

static void
gog_axis_set_ticks (GogAxis *axis, int tick_nbr, GogAxisTick *ticks)
{
	unsigned i;

	g_return_if_fail (GOG_AXIS (axis) != NULL);

	if (axis->ticks != NULL) {
		for (i = 0; i < axis->tick_nbr; i++)
			g_free (axis->ticks[i].label);

		g_free (axis->ticks);
	}

	axis->tick_nbr = tick_nbr;
	axis->ticks = ticks;
}

static void 
gog_axis_calc_ticks (GogAxis *axis)
{
	g_return_if_fail (GOG_AXIS (axis) != NULL);

	if (axis->is_discrete)
		map_desc_discrete.calc_ticks (axis);
	else
		if (axis->map_desc->calc_ticks)
			axis->map_desc->calc_ticks (axis);

	if (axis->type == GOG_AXIS_PSEUDO_3D || axis->type == GOG_AXIS_Z) {
		GSList *l = axis->contributors;
		while (l) {
			gog_plot_update_3d (GOG_PLOT (l->data));
			l = l->next;
		}
	}
}

/************************************************************************/

typedef GogAxisBaseClass GogAxisClass;

static GType gog_axis_view_get_type (void);

static GObjectClass *parent_klass;

enum {
	AXIS_PROP_0,
	AXIS_PROP_TYPE,
	AXIS_PROP_INVERT,
	AXIS_PROP_MAP,
	AXIS_PROP_ASSIGNED_FORMAT_STR_XL
};

/*****************************************************************************/

static gboolean
role_grid_line_major_can_add (GogObject const *parent)
{
	GogAxis *axis = GOG_AXIS (parent);
	GogAxisType type = gog_axis_get_atype (axis);
	
	return ((type == GOG_AXIS_X || type == GOG_AXIS_Y || type == GOG_AXIS_RADIAL || 
		 (type == GOG_AXIS_CIRCULAR && !gog_axis_is_discrete (axis))) &&
		gog_axis_get_grid_line (GOG_AXIS (parent), TRUE) == NULL);
}

static gboolean
role_grid_line_minor_can_add (GogObject const *parent)
{
	GogAxis *axis = GOG_AXIS (parent);
	GogAxisType type = gog_axis_get_atype (axis);
	
	return (!gog_axis_is_discrete (GOG_AXIS (parent)) &&
		(type == GOG_AXIS_X || type == GOG_AXIS_Y || 
		 type == GOG_AXIS_RADIAL || type == GOG_AXIS_CIRCULAR) &&
		gog_axis_get_grid_line (GOG_AXIS (parent), FALSE) == NULL);
}

static void 
role_grid_line_major_post_add (GogObject *parent, GogObject *child)  
{
	g_object_set (G_OBJECT (child), "is-minor", (gboolean)FALSE, NULL);
}

static void 
role_grid_line_minor_post_add (GogObject *parent, GogObject *child)  
{ 
	g_object_set (G_OBJECT (child), "is-minor", (gboolean)TRUE, NULL);
}

static gboolean
role_axis_line_can_add (GogObject const *parent)
{
	GogChart *chart = GOG_AXIS_BASE (parent)->chart;
	GogAxisSet axis_set = gog_chart_get_axis_set (chart);
	
	if (axis_set == GOG_AXIS_SET_XY ||
	    (axis_set == GOG_AXIS_SET_RADAR && 
	     gog_axis_get_atype (GOG_AXIS (parent)) == GOG_AXIS_RADIAL))  
		return TRUE;

	return FALSE;
}

static void
role_axis_line_post_add (GogObject *parent, GogObject *child)
{
	gog_axis_base_set_position (GOG_AXIS_BASE (child), GOG_AXIS_AUTO);
}

static gboolean 
role_label_can_add (GogObject const *parent)
{
	GogAxisType type = gog_axis_get_atype (GOG_AXIS (parent));
	
	return (type == GOG_AXIS_X ||
		type == GOG_AXIS_Y);
}

/**
 * gog_axis_set_format :
 * @axis : #GogAxis
 * @fmt  : #GOFormat
 *
 * Absorbs a reference to @fmt, and accepts NULL.
 * returns TRUE if things changed
 **/
static gboolean
gog_axis_set_format (GogAxis *axis, GOFormat *fmt)
{
	if (go_format_eq (fmt, axis->assigned_format)) {
		go_format_unref (fmt);
		return FALSE;
	}
	if (axis->assigned_format != NULL)
		go_format_unref (axis->assigned_format);
	axis->assigned_format = fmt;
	return TRUE;
}

static void
gog_axis_set_property (GObject *obj, guint param_id,
		       GValue const *value, GParamSpec *pspec)
{
	GogAxis *axis = GOG_AXIS (obj);
	gboolean resized = FALSE;
	gboolean calc_ticks = FALSE;
	gboolean request_update = FALSE;
	int itmp;

	switch (param_id) {
	case AXIS_PROP_TYPE:
		itmp = g_value_get_int (value);
		if (axis->type != itmp) {
			axis->type = itmp;
			resized = TRUE;
			if (axis->type == GOG_AXIS_PSEUDO_3D)
				g_object_set (obj,
						"major-tick-labeled", FALSE,
						"major-tick-in", FALSE,
						"major-tick-out", FALSE,
						"minor-tick-in", FALSE,
						"minor-tick-out", FALSE,
						NULL);
		}
		break;
	case AXIS_PROP_INVERT:
		axis->inverted = g_value_get_boolean (value);
		resized = calc_ticks = TRUE;
		break;
	case AXIS_PROP_MAP :
		gog_axis_map_set (axis, g_value_get_string (value));
		request_update = TRUE;
		break;
	case AXIS_PROP_ASSIGNED_FORMAT_STR_XL : {
		char const *str = g_value_get_string (value);
		resized = gog_axis_set_format (axis, (str != NULL)
			? go_format_new_from_XL (str, FALSE)
			: NULL);
		calc_ticks = resized;
		break;
	}

	default: G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, param_id, pspec);
		 return; /* NOTE : RETURN */
	}

	if (request_update)
		gog_object_request_update (GOG_OBJECT (axis));
	else {
		if (calc_ticks)
			gog_axis_calc_ticks (axis);
		gog_object_emit_changed (GOG_OBJECT (obj), resized);
	}
}

static void
gog_axis_get_property (GObject *obj, guint param_id,
		       GValue *value, GParamSpec *pspec)
{
	GogAxis const *axis = GOG_AXIS (obj);

	switch (param_id) {
	case AXIS_PROP_TYPE:
		g_value_set_int (value, axis->type);
		break;
	case AXIS_PROP_INVERT:
		g_value_set_boolean (value, axis->inverted);
		break;
	case AXIS_PROP_MAP:
		g_value_set_string (value, axis->map_desc->name);
		break;
	case AXIS_PROP_ASSIGNED_FORMAT_STR_XL :
		if (axis->assigned_format != NULL)
			g_value_take_string (value,
				go_format_as_XL	(axis->assigned_format, FALSE));
		else
			g_value_set_static_string (value, NULL);
		break;

	default: G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, param_id, pspec);
		 break;
	}
}

static void
gog_axis_finalize (GObject *obj)
{
	GogAxis *axis = GOG_AXIS (obj);

	gog_axis_clear_contributors (axis);

	g_slist_free (axis->contributors);	axis->contributors = NULL;
	if (axis->labels != NULL) {
		g_object_unref (axis->labels);
		axis->labels   = NULL;
		/* this is for information only, no ref */
		axis->plot_that_supplied_labels = NULL;
	}
	if (axis->assigned_format != NULL) {
		go_format_unref (axis->assigned_format);
		axis->assigned_format = NULL;
	}
	if (axis->format != NULL) {
		go_format_unref (axis->format);
		axis->format = NULL;
	}

	gog_axis_set_ticks (axis, 0, NULL);

	gog_dataset_finalize (GOG_DATASET (axis));
	(parent_klass->finalize) (obj);
}

/**
 * gog_axis_get_entry :
 * @axis : #GogAxis
 * @i :
 * @user_defined : an optionally NULL pointr to gboolean
 *
 * Returns the value of axis element @i and sets @user_defined or
 * 	NaN on error
 **/
double
gog_axis_get_entry (GogAxis const *axis, GogAxisElemType i, gboolean *user_defined)
{
	GOData *dat;

	if (user_defined)
		*user_defined = FALSE;

	g_return_val_if_fail (GOG_AXIS (axis) != NULL, go_nan);
	g_return_val_if_fail (i >= GOG_AXIS_ELEM_MIN && i < GOG_AXIS_ELEM_MAX_ENTRY, go_nan);

	if (i != GOG_AXIS_ELEM_CROSS_POINT)
		dat = axis->source [i].data;
	else 
		dat = GOG_AXIS_BASE (axis)->cross_location.data;

	if (dat != NULL && IS_GO_DATA_SCALAR (dat)) {
		double tmp = go_data_scalar_get_value (GO_DATA_SCALAR (dat));
		if (go_finite (tmp)) {
			if (user_defined)
				*user_defined = TRUE;
			return tmp;
		}
	}

	if (i != GOG_AXIS_ELEM_CROSS_POINT)
		return axis->auto_bound [i];
	else
		return 0.;
}

static void
gog_axis_update (GogObject *obj)
{
	GSList *ptr;
	GogAxis *axis = GOG_AXIS (obj);
	double old_min = axis->auto_bound [GOG_AXIS_ELEM_MIN];
	double old_max = axis->auto_bound [GOG_AXIS_ELEM_MAX];
	GOData *labels;
	GogPlotBoundInfo bounds;

	gog_debug (0, g_warning ("axis::update"););

	if (axis->labels != NULL) {
		g_object_unref (axis->labels);
		axis->labels   = NULL;
		axis->plot_that_supplied_labels = NULL;
	}
	axis->is_discrete = TRUE;
	axis->min_val  =  DBL_MAX;
	axis->max_val  = -DBL_MAX;
	axis->min_contrib = axis->max_contrib = NULL;
	if (axis->format != NULL) {
		go_format_unref (axis->format);
		axis->format = NULL;
	}

	/* everything else is initialized in gog_plot_get_axis_bounds */
	bounds.fmt = NULL;
	for (ptr = axis->contributors ; ptr != NULL ; ptr = ptr->next) {
		labels = gog_plot_get_axis_bounds (GOG_PLOT (ptr->data),
						   axis->type, &bounds);

		/* value dimensions have more information than index dimensions.
		 * At least thats what I am guessing today*/
		if (!bounds.is_discrete)
			axis->is_discrete = FALSE;
		else if (axis->labels == NULL && labels != NULL) {
			g_object_ref (labels);
			axis->labels = GO_DATA_VECTOR (labels);
			axis->plot_that_supplied_labels = GOG_PLOT (ptr->data);
		}
		axis->center_on_ticks = bounds.center_on_ticks;

		if (axis->min_val > bounds.val.minima) {
			axis->min_val = bounds.val.minima;
			axis->logical_min_val = bounds.logical.minima;
			axis->min_contrib = ptr->data;
		} else if (axis->min_contrib == ptr->data) {
			axis->min_contrib = NULL;
			axis->min_val = bounds.val.minima;
		}

		if (axis->max_val < bounds.val.maxima) {
			axis->max_val = bounds.val.maxima;
			axis->logical_max_val = bounds.logical.maxima;
			axis->max_contrib = ptr->data;
		} else if (axis->max_contrib == ptr->data) {
			axis->max_contrib = NULL;
			axis->max_val = bounds.val.maxima;
		}
	}
	axis->format = bounds.fmt; /* just absorb the ref if it exists */

	gog_axis_auto_bound (axis);

	if (go_finite (axis->logical_min_val) &&
	    axis->auto_bound [GOG_AXIS_ELEM_MIN] < axis->logical_min_val)
		axis->auto_bound [GOG_AXIS_ELEM_MIN] = axis->logical_min_val;
	if (go_finite (axis->logical_max_val) &&
	    axis->auto_bound [GOG_AXIS_ELEM_MAX] > axis->logical_max_val)
		axis->auto_bound [GOG_AXIS_ELEM_MAX] = axis->logical_max_val;

	gog_axis_calc_ticks (axis);

	if (old_min != axis->auto_bound [GOG_AXIS_ELEM_MIN] ||
	    old_max != axis->auto_bound [GOG_AXIS_ELEM_MAX])
		gog_object_emit_changed (GOG_OBJECT (obj), TRUE);
}

static void
cb_axis_toggle_changed (GtkToggleButton *toggle_button, GObject *axis)
{
	g_object_set (axis,
		gtk_widget_get_name (GTK_WIDGET (toggle_button)),
		gtk_toggle_button_get_active (toggle_button),
		NULL);
}

typedef struct {
	GtkWidget *editor;
	GtkToggleButton *toggle;
	GogDataset *set;
	unsigned dim;
} ElemToggleData;

static void
cb_enable_dim (GtkToggleButton *toggle_button, ElemToggleData *closure)
{
	gboolean is_auto = gtk_toggle_button_get_active (toggle_button);
	double bound = GOG_AXIS (closure->set)->auto_bound [closure->dim];

	gtk_widget_set_sensitive (closure->editor, !is_auto);

	if (is_auto) /* clear the data */
		gog_dataset_set_dim (closure->set, closure->dim, NULL, NULL);

	if (go_finite (bound) && DBL_MAX > bound && bound > -DBL_MAX) {
		char *str = g_strdup_printf ("%g", bound);
		g_object_set (closure->editor, "text", str, NULL);
		g_free (str);
	} else
		g_object_set (closure->editor, "text", "", NULL);
}

static void
cb_axis_bound_changed (GogObject *axis, gboolean resize, ElemToggleData *closure)
{
	if (gtk_toggle_button_get_active (closure->toggle)) {
		double bound = GOG_AXIS (closure->set)->auto_bound [closure->dim];
		if (go_finite (bound) && DBL_MAX > bound && bound > -DBL_MAX) {
			char *str = g_strdup_printf ("%g", bound);
			g_object_set (closure->editor, "text", str, NULL);
			g_free (str);
		} else
			g_object_set (closure->editor, "text", "", NULL);
	}
}

static void
make_dim_editor (GogDataset *set, GtkTable *table, unsigned dim,
		 GogDataAllocator *dalloc, char const * const *dim_name)
{
	ElemToggleData *info;
	GClosure *closure;
	GtkWidget *editor = gog_data_allocator_editor (dalloc, set, dim, GOG_DATA_SCALAR);
	char *txt = g_strconcat (_(dim_name [dim]), ":", NULL);
	GtkWidget *toggle = gtk_check_button_new_with_mnemonic (txt);
	g_free (txt);

	info = g_new0 (ElemToggleData, 1);
	info->editor = editor;
	info->set = set;
	info->dim = dim;
	info->toggle = GTK_TOGGLE_BUTTON (toggle);
	g_signal_connect (G_OBJECT (toggle),
		"toggled",
		G_CALLBACK (cb_enable_dim), info);
	gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (toggle),
		gog_dataset_get_dim (set, dim) == NULL);

	closure = g_cclosure_new (G_CALLBACK (cb_axis_bound_changed),
				  info, (GClosureNotify)g_free);
	g_object_watch_closure (G_OBJECT (toggle), closure);
	g_signal_connect_closure (G_OBJECT (set),
		"changed",
		closure, FALSE);

	gtk_table_attach (table, toggle,
		0, 1, dim + 1, dim + 2, GTK_FILL, 0, 0, 0);
	gtk_table_attach (table, editor,
		1, 2, dim + 1, dim + 2, GTK_FILL | GTK_EXPAND, 0, 0, 0);
}

static void
cb_axis_fmt_changed (G_GNUC_UNUSED GtkWidget *widget,
		     char *fmt,
		     GObject *axis)
{
	g_object_set (axis, "assigned-format-string-XL", fmt, NULL);
}

static void
cb_map_combo_changed (GtkComboBox *combo,
		      GogAxis *axis)
{
	gog_axis_map_set_by_num (axis, gtk_combo_box_get_active (combo));
}

#if 0
static void
cb_axis_fmt_assignment_toggled (GtkToggleButton *toggle_button, GtkNotebook *notebook)
{
	/* any time the toggle changes assume the user wanted to select the page too */
	gtk_notebook_set_current_page (notebook, 0); /* assume it is the first page */ 
}
#endif

static void
gog_axis_populate_editor (GogObject *gobj, 
			  GogEditor *editor, 
			  GogDataAllocator *dalloc, 
			  GOCmdContext *cc)
{
	static guint axis_pref_page = 0;
	static char const *toggle_props[] = {
		"invert-axis"
	};
	GtkWidget *w;
	GtkTable  *table;
	unsigned i = 0;
	GogAxis *axis = GOG_AXIS (gobj);
	GogDataset *set = GOG_DATASET (gobj);
	GladeXML *gui;

	gui = go_libglade_new ("gog-axis-prefs.glade", "axis_pref_box", NULL, cc);
	if (gui == NULL)
		return;

	/* Bounds Page */
	table = GTK_TABLE (glade_xml_get_widget (gui, "bound_table"));
	if (axis->is_discrete) {
		static char const * const dim_names[] = {
			N_("M_inimum"),
			N_("M_aximum"),
			N_("Categories between _ticks"),
			N_("Categories between _labels")
		};
		for (i = GOG_AXIS_ELEM_MIN; i < GOG_AXIS_ELEM_CROSS_POINT ; i++)
			make_dim_editor (set, table, i, dalloc, dim_names);
	} else {
		static char const * const dim_names[] = {
			N_("M_inimum"),
			N_("M_aximum"),
			N_("Ma_jor ticks"),
			N_("Mi_nor ticks")
		};

		for (i = GOG_AXIS_ELEM_MIN; i < GOG_AXIS_ELEM_CROSS_POINT ; i++)
			make_dim_editor (set, table, i, dalloc, dim_names);
	}
	gtk_widget_show_all (GTK_WIDGET (table));

	/* Details page */
	if (!axis->is_discrete && gog_axis_get_atype (axis) != GOG_AXIS_CIRCULAR) {
		GtkWidget *w = glade_xml_get_widget (gui, "map_type_combo");
		gog_axis_map_populate_combo (axis, GTK_COMBO_BOX (w));
		g_signal_connect_object (G_OBJECT (w),
					 "changed",
					 G_CALLBACK (cb_map_combo_changed),
					 axis, 0);
	} else {
		GtkWidget *w = glade_xml_get_widget (gui, "map_type_box");
		gtk_widget_hide (w);
	}

	for (i = 0; i < G_N_ELEMENTS (toggle_props) ; i++) {
		gboolean cur_val;
		GtkWidget *w = glade_xml_get_widget (gui, toggle_props[i]);

		g_object_get (G_OBJECT (gobj), toggle_props[i], &cur_val, NULL);
		gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (w), cur_val);
		g_signal_connect_object (G_OBJECT (w),
					 "toggled",
					 G_CALLBACK (cb_axis_toggle_changed), axis, 0);
	}

	gog_editor_add_page (editor, 
			     glade_xml_get_widget (gui, "axis_pref_box"),
			     _("Scale"));

	/* Style page */
	(GOG_OBJECT_CLASS(parent_klass)->populate_editor) (gobj, editor, dalloc, cc);

	/* Format page */
	if (!axis->is_discrete && gog_axis_get_atype (axis) != GOG_AXIS_PSEUDO_3D) {
		w = go_format_sel_new ();
		if (axis->assigned_format != NULL && !go_format_is_general (axis->assigned_format))
			go_format_sel_set_style_format (GO_FORMAT_SEL (w),
				axis->assigned_format);
		else if (axis->format != NULL)
			go_format_sel_set_style_format (GO_FORMAT_SEL (w),
				axis->format);

#if 0
		/* TOO CHEESY to go into production
		 * We need a way to toggle auto vs user formats
		 * but the selector is too tall already
		 * disable for now */
		cbox = gtk_check_button_new_with_label (_("Format"));
		g_signal_connect (G_OBJECT (cbox),
			"toggled",
			G_CALLBACK (cb_axis_fmt_assignment_toggled), notebook);
		gtk_notebook_prepend_page (GTK_NOTEBOOK (notebook), w, cbox);
#else
		gog_editor_add_page (editor, w, _("Format"));
#endif

		gtk_widget_show (w);
		g_signal_connect (G_OBJECT (w),
			"format_changed", G_CALLBACK (cb_axis_fmt_changed), axis);
	}

	w = glade_xml_get_widget (gui, "axis_pref_box");
	g_object_set_data_full (G_OBJECT (w), "gui", gui,
				(GDestroyNotify)g_object_unref);

	gog_editor_set_store_page (editor, &axis_pref_page);
}

static void
gog_axis_init_style (GogStyledObject *gso, GogStyle *style)
{
	if (gog_axis_get_atype (GOG_AXIS (gso)) != GOG_AXIS_PSEUDO_3D)
		style->interesting_fields = GOG_STYLE_LINE | GOG_STYLE_FONT |
			GOG_STYLE_TEXT_LAYOUT;
	else
		style->interesting_fields = 0;
	gog_theme_fillin_style (gog_object_get_theme (GOG_OBJECT (gso)),
				style, GOG_OBJECT (gso), 0, FALSE);
}

static void
gog_axis_class_init (GObjectClass *gobject_klass)
{
	static GogObjectRole const roles[] = { 
		{ N_("MajorGrid"), "GogGridLine", 0,
		  GOG_POSITION_SPECIAL, GOG_POSITION_SPECIAL, GOG_OBJECT_NAME_BY_ROLE,
		  role_grid_line_major_can_add, NULL, NULL, role_grid_line_major_post_add, NULL, NULL, { -1 } },
		{ N_("MinorGrid"), "GogGridLine", 1,
		  GOG_POSITION_SPECIAL, GOG_POSITION_SPECIAL, GOG_OBJECT_NAME_BY_ROLE,
		  role_grid_line_minor_can_add, NULL, NULL, role_grid_line_minor_post_add, NULL, NULL, { -1 } },
		{ N_("AxisLine"), "GogAxisLine", 2,
		  GOG_POSITION_PADDING, GOG_POSITION_PADDING, GOG_OBJECT_NAME_BY_ROLE,
		  role_axis_line_can_add, NULL, NULL, role_axis_line_post_add, NULL, NULL, { -1 } },
		{ N_("Label"), "GogLabel", 3,
		  GOG_POSITION_SPECIAL|GOG_POSITION_ANY_MANUAL, GOG_POSITION_SPECIAL, GOG_OBJECT_NAME_BY_ROLE,
		  role_label_can_add, NULL, NULL, NULL, NULL, NULL, { -1 } }
	};

	GogObjectClass *gog_klass = (GogObjectClass *) gobject_klass;
	GogStyledObjectClass *style_klass = (GogStyledObjectClass *) gog_klass;

	parent_klass = g_type_class_peek_parent (gobject_klass);
	gobject_klass->set_property = gog_axis_set_property;
	gobject_klass->get_property = gog_axis_get_property;
	gobject_klass->finalize	    = gog_axis_finalize;

	/* no need to persist, the role handles that */
	g_object_class_install_property (gobject_klass, AXIS_PROP_TYPE,
		g_param_spec_int ("type", "Type",
			"GogAxisType",
			GOG_AXIS_UNKNOWN, GOG_AXIS_TYPES, GOG_AXIS_UNKNOWN, G_PARAM_READWRITE));
	g_object_class_install_property (gobject_klass, AXIS_PROP_INVERT,
		g_param_spec_boolean ("invert-axis", NULL,
			"Scale from high to low rather than low to high",
			FALSE, G_PARAM_READWRITE | GOG_PARAM_PERSISTENT));
	g_object_class_install_property (gobject_klass, AXIS_PROP_MAP,
		g_param_spec_string ("map-name", "MapName",
			"The name of the map for scaling",
			"linear", G_PARAM_READWRITE|GOG_PARAM_PERSISTENT));
	g_object_class_install_property (gobject_klass, AXIS_PROP_ASSIGNED_FORMAT_STR_XL,
		g_param_spec_string ("assigned-format-string-XL", NULL,
			"The user assigned format to use for non-discrete axis labels (XL format)",
			"General", G_PARAM_READWRITE | GOG_PARAM_PERSISTENT));

	gog_object_register_roles (gog_klass, roles, G_N_ELEMENTS (roles));

	gog_klass->update	= gog_axis_update;
	gog_klass->populate_editor	= gog_axis_populate_editor;
	gog_klass->view_type	= gog_axis_view_get_type ();
	style_klass->init_style = gog_axis_init_style;
}

static void
gog_axis_init (GogAxis *axis)
{
	axis->type	 = GOG_AXIS_UNKNOWN;
	axis->contributors = NULL;
	axis->inverted = FALSE;

	/* yes we want min = MAX */
	axis->min_val =  DBL_MAX;
	axis->max_val = -DBL_MAX;
	axis->min_contrib = axis->max_contrib = NULL;
	axis->is_discrete = FALSE;
	axis->center_on_ticks = FALSE;
	axis->labels = NULL;
	axis->plot_that_supplied_labels = NULL;
	axis->format = axis->assigned_format = NULL;

	gog_axis_map_set (axis, NULL);

	axis->ticks = NULL;
	axis->tick_nbr = 0;
}

static void
gog_axis_dataset_dims (GogDataset const *set, int *first, int *last)
{
	*first = GOG_AXIS_ELEM_MIN;
	*last  = GOG_AXIS_ELEM_CROSS_POINT;
}

static GogDatasetElement *
gog_axis_dataset_get_elem (GogDataset const *set, int dim_i)
{
	GogAxis *axis = GOG_AXIS (set);
	if (GOG_AXIS_ELEM_MIN <= dim_i && dim_i < GOG_AXIS_ELEM_CROSS_POINT)
		return &axis->source[dim_i];
	if (dim_i == GOG_AXIS_ELEM_CROSS_POINT) {
		return &(axis->base.cross_location);
	}
	return NULL;
}

static void
gog_axis_dim_changed (GogDataset *set, int dim_i)
{
	gog_axis_update (GOG_OBJECT (set));
	gog_object_emit_changed (GOG_OBJECT (set), TRUE);
}

static void
gog_axis_dataset_init (GogDatasetClass *iface)
{
	iface->dims	   = gog_axis_dataset_dims;
	iface->get_elem	   = gog_axis_dataset_get_elem;
	iface->dim_changed = gog_axis_dim_changed;
}

GSF_CLASS_FULL (GogAxis, gog_axis,
		NULL, NULL, gog_axis_class_init, NULL,
		gog_axis_init, GOG_AXIS_BASE_TYPE, 0,
		GSF_INTERFACE (gog_axis_dataset_init, GOG_DATASET_TYPE))


GogAxisType
gog_axis_get_atype (GogAxis const *axis)
{
	g_return_val_if_fail (GOG_AXIS (axis) != NULL, GOG_AXIS_UNKNOWN);
	return axis->type;
}

/**
 * gog_axis_is_center_on_ticks :
 * @axis : #GogAxis
 * 
 * Returns TRUE if labels are centered on ticks when @axis is discrete
 **/ 
gboolean
gog_axis_is_center_on_ticks (GogAxis const *axis)
{
	g_return_val_if_fail (GOG_AXIS (axis) != NULL, FALSE);
	return axis->center_on_ticks;
}

/**
 * gog_axis_is_discrete :
 * @axis : #GogAxis
 * 
 * Returns TRUE if @axis enumerates a set of discrete items, rather than a
 * continuous value
 **/ 
gboolean
gog_axis_is_discrete (GogAxis const *axis)
{
	g_return_val_if_fail (GOG_AXIS (axis) != NULL, FALSE);
	return axis->is_discrete;
}

/**
 * gog_axis_is_inverted :
 * @axis : #GogAxis
 * 
 * Returns TRUE if @axis is inverted.
 **/ 
gboolean
gog_axis_is_inverted (GogAxis const *axis)
{
	g_return_val_if_fail (GOG_AXIS (axis) != NULL, FALSE);
	return axis->inverted;
}

/**
 * gog_axis_get_bounds :
 * @axis : #GogAxis
 * @minima : result
 * @maxima : result
 *
 * return TRUE if the bounds stored in @minima and @maxima are sane
 **/
gboolean
gog_axis_get_bounds (GogAxis const *axis, double *minima, double *maxima)
{
	g_return_val_if_fail (GOG_AXIS (axis) != NULL, FALSE);
	g_return_val_if_fail (minima != NULL, FALSE);
	g_return_val_if_fail (maxima != NULL, FALSE);

	*minima = gog_axis_get_entry (axis, GOG_AXIS_ELEM_MIN, NULL);
	*maxima = gog_axis_get_entry (axis, GOG_AXIS_ELEM_MAX, NULL);

	return go_finite (*minima) && go_finite (*maxima) && *minima < *maxima;
}

/**
 * gog_axis_get_ticks :
 * @axis : #GogAxis
 * @ticks : result
 *
 * Retrieve an array of tick descriptions, and return the number of ticks.
 **/
unsigned 
gog_axis_get_ticks (GogAxis *axis, GogAxisTick **ticks)
{
	g_return_val_if_fail (GOG_AXIS (axis) != NULL, 0);
	g_return_val_if_fail (ticks != NULL, 0);

	*ticks = axis->ticks;
	return axis->tick_nbr;
}

/**
 * gog_axis_get_labels :
 * @axi : #GogAxis
 * @plot_that_labeled_axis : #GogPlot
 *
 * Return the possibly NULL #GOData used as a label for this axis
 * along with the plot that it was associated with
 **/
GOData *
gog_axis_get_labels (GogAxis const *axis, GogPlot **plot_that_labeled_axis)
{
	g_return_val_if_fail (GOG_AXIS (axis) != NULL, NULL);

	if (axis->is_discrete) {
		if (plot_that_labeled_axis != NULL)
			*plot_that_labeled_axis = axis->plot_that_supplied_labels;
		return GO_DATA (axis->labels);
	}
	if (plot_that_labeled_axis != NULL)
		*plot_that_labeled_axis = NULL;
	return NULL;
}

/**
 * gog_axis_add_contributor :
 * @axis : #GogAxis
 * @contrib : #GogObject (can we relax this to use an interface ?)
 *
 * Register @contrib as taking part in the negotiation of @axis's bounds.
 **/
void
gog_axis_add_contributor (GogAxis *axis, GogObject *contrib)
{
	g_return_if_fail (GOG_AXIS (axis) != NULL);
	g_return_if_fail (g_slist_find (axis->contributors, contrib) == NULL);

	axis->contributors = g_slist_prepend (axis->contributors, contrib);

	gog_object_request_update (GOG_OBJECT (axis));
}

/**
 * gog_axis_del_contributor :
 * @axis : #GogAxis
 * @contrib : #GogObject (can we relax this to use an interface ?)
 *
 * @contrib no longer takes part in the negotiation of @axis's bounds.
 **/
void
gog_axis_del_contributor (GogAxis *axis, GogObject *contrib)
{
	gboolean update = FALSE;

	g_return_if_fail (GOG_AXIS (axis) != NULL);
	g_return_if_fail (g_slist_find (axis->contributors, contrib) != NULL);

	if (axis->min_contrib == contrib) {
		axis->min_contrib = NULL;
		update = TRUE;
	}
	if (axis->max_contrib == contrib) {
		axis->max_contrib = NULL;
		update = TRUE;
	}
	axis->contributors = g_slist_remove (axis->contributors, contrib);

	if (update)
		gog_object_request_update (GOG_OBJECT (axis));
}

void
gog_axis_clear_contributors (GogAxis *axis)
{
	GSList *ptr, *list;
	GogAxisSet filter;

	g_return_if_fail (GOG_AXIS (axis) != NULL);

	filter = 1 << axis->type;
	list = g_slist_copy (axis->contributors);
	for (ptr = list; ptr != NULL ; ptr = ptr->next)
		gog_plot_axis_clear (GOG_PLOT (ptr->data), filter);
	g_slist_free (list);
}

GSList const *
gog_axis_contributors (GogAxis *axis)
{
	g_return_val_if_fail (GOG_AXIS (axis) != NULL, NULL);

	return axis->contributors;
}

/**
 * gog_axis_bound_changed :
 * @axis : #GogAxis
 * @contrib : #GogObject
**/
void
gog_axis_bound_changed (GogAxis *axis, GogObject *contrib)
{
	g_return_if_fail (GOG_AXIS (axis) != NULL);

	gog_object_request_update (GOG_OBJECT (axis));
}

/* gog_axis_get_grid_line:
 * @axis: #GogAxis
 * @major: whether to retrieve major or minor grid line.
 *
 * Returns a pointer to GridLine object associated to given axis, NULL
 * if it doesn't exists.
 **/
GogGridLine *
gog_axis_get_grid_line (GogAxis *axis, gboolean major)
{
	GogGridLine *grid_line;
	GSList *children;

	children = gog_object_get_children (GOG_OBJECT (axis), 
		gog_object_find_role_by_name (GOG_OBJECT (axis), 
			major ? "MajorGrid" : "MinorGrid"));
	if (children != NULL) {
		grid_line = GOG_GRID_LINE (children->data);
		g_slist_free (children);
		return grid_line;
	}
	return NULL;
}

/****************************************************************************/

typedef GogAxisBaseView		GogAxisView;
typedef GogAxisBaseViewClass	GogAxisViewClass;

#define GOG_AXIS_VIEW_TYPE	(gog_axis_view_get_type ())
#define GOG_AXIS_VIEW(o)	(G_TYPE_CHECK_INSTANCE_CAST ((o), GOG_AXIS_VIEW_TYPE, GogAxisView))
#define IS_GOG_AXIS_VIEW(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOG_AXIS_VIEW_TYPE))

static GogViewClass *aview_parent_klass;

static void
gog_axis_view_padding_request (GogView *view, 
			       GogViewAllocation const *bbox,
			       GogViewPadding *padding) 
{
	GogView *child;
	GogAxis *axis = GOG_AXIS (view->model);
	GogAxisType type = gog_axis_get_atype (axis);
	GogObjectPosition pos;
	GogViewAllocation tmp = *bbox;
	GogViewRequisition req;
	GogViewPadding label_padding, child_padding;
	GSList *ptr;
	double const pad_h = gog_renderer_pt2r_y (view->renderer, PAD_HACK);
	double const pad_w = gog_renderer_pt2r_x (view->renderer, PAD_HACK);

	label_padding.wr = label_padding.wl = label_padding.ht = label_padding.hb = 0;

	for (ptr = view->children; ptr != NULL ; ptr = ptr->next) {
		child = ptr->data;
		pos = child->model->position;
		if (IS_GOG_LABEL (child->model) && !(pos & GOG_POSITION_MANUAL)) {
			gog_view_size_request (child, &req);
			if (type == GOG_AXIS_X) 
				label_padding.hb += req.h + pad_h;
			else  
				label_padding.wl += req.w + pad_w;
		}
	}

	tmp.x += label_padding.wl;
	tmp.w -= label_padding.wl + label_padding.wr;
	tmp.y += label_padding.hb;
	tmp.h -= label_padding.hb + label_padding.ht;

	(aview_parent_klass->padding_request) (view, &tmp, padding);

	for (ptr = view->children; ptr != NULL ; ptr = ptr->next) {
		child = ptr->data;
		if (GOG_POSITION_IS_PADDING (child->model->position)) {
			gog_view_padding_request (child, &tmp, &child_padding);
			padding->wr = MAX (padding->wr, child_padding.wr);
			padding->wl = MAX (padding->wl, child_padding.wl);
			padding->hb = MAX (padding->hb, child_padding.hb);
			padding->ht = MAX (padding->ht, child_padding.ht);
		}
	}

	padding->wr += label_padding.wr;
	padding->wl += label_padding.wl;
	padding->ht += label_padding.ht;
	padding->hb += label_padding.hb;
}

static void
gog_axis_view_size_allocate (GogView *view, GogViewAllocation const *bbox)
{
	GSList *ptr;
	GogView *child;
	GogAxis *axis = GOG_AXIS (view->model);
	GogAxisType type = gog_axis_get_atype (axis);
	GogViewAllocation tmp = *bbox;
	GogViewAllocation const *plot_area = gog_chart_view_get_plot_area (view->parent);
	GogViewAllocation child_bbox;
	GogViewRequisition req;
	GogObjectPosition pos;
	double const pad_h = gog_renderer_pt2r_y (view->renderer, PAD_HACK);
	double const pad_w = gog_renderer_pt2r_x (view->renderer, PAD_HACK);

	for (ptr = view->children; ptr != NULL ; ptr = ptr->next) {
		child = ptr->data;
		pos = child->model->position;
		if (IS_GOG_LABEL (child->model) && (pos & GOG_POSITION_MANUAL)) {
			gog_view_size_request (child, &req);
			child_bbox = gog_object_get_manual_allocation (gog_view_get_model (child), 
								       plot_area, &req);
			gog_view_size_allocate (child, &child_bbox);
		} else {
			if (GOG_POSITION_IS_SPECIAL (pos)) {
				if (IS_GOG_LABEL (child->model)) {
					gog_view_size_request (child, &req);
					if (type == GOG_AXIS_X) {
						child_bbox.x = plot_area->x + (plot_area->w - req.w) / 2.0;
						child_bbox.w = plot_area->w;
						child_bbox.y = tmp.y + tmp.h - req.h;
						child_bbox.h = req.h;
						tmp.h -= req.h + pad_h;
					} else {
						child_bbox.x = tmp.x;
						child_bbox.w = req.w;
						child_bbox.y = plot_area->y + (plot_area->h - req.h) / 2.0;
						child_bbox.h = plot_area->h;
						tmp.x += req.w + pad_w;
						tmp.w -= req.w + pad_w;
					}
					gog_view_size_allocate (child, &child_bbox);
				} else {
					gog_view_size_allocate (child, plot_area);
				}
			}
		}	
	}
}

static void
gog_axis_view_render (GogView *view, GogViewAllocation const *bbox)
{
	GSList *ptr;

	(aview_parent_klass->render) (view, bbox);	

	/* Render every child except grid lines. Those are rendered
	 * before in gog_chart_view since we don't want to render them
	 * over axis. */
	for (ptr = view->children ; ptr != NULL ; ptr = ptr->next) {
		if (!IS_GOG_GRID_LINE (GOG_VIEW (ptr->data)->model))
			gog_view_render	(ptr->data, bbox);
	}
}

static void
gog_axis_view_class_init (GogAxisViewClass *gview_klass)
{
	GogViewClass *view_klass    = (GogViewClass *) gview_klass;

	aview_parent_klass = g_type_class_peek_parent (gview_klass);
	view_klass->size_allocate = gog_axis_view_size_allocate;
	view_klass->padding_request = gog_axis_view_padding_request;
	view_klass->render	    = gog_axis_view_render;
}

static GSF_CLASS (GogAxisView, gog_axis_view,
		  gog_axis_view_class_init, NULL,
		  GOG_AXIS_BASE_VIEW_TYPE)
