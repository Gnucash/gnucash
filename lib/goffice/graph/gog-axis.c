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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
 * USA
 */

#include <goffice/goffice-config.h>
#include <goffice/graph/gog-axis.h>
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
#include <goffice/graph/go-data.h>
#include <goffice/utils/go-format.h>
#include <goffice/utils/go-math.h>

#include <gsf/gsf-impl-utils.h>
#include <glib/gi18n.h>
//#include <src/gui-util.h>
//#include <src/format.h>
//#include <src/widgets/widget-format-selector.h>
#include <gui-util.h>
#include <format.h>
#include <widgets/widget-format-selector.h>
#include <gtk/gtktable.h>
#include <gtk/gtkcheckbutton.h>
#include <gtk/gtknotebook.h>
#include <gtk/gtklabel.h>
#include <gtk/gtkcombobox.h>
#include <gtk/gtkmisc.h>
#include <glade/glade-xml.h>

#include <string.h>

struct _GogAxis {
	GogStyledObject	 base;

	GogAxisType	 type;
	GogAxisPosition	 pos;
	GSList		*contributors;

	GogDatasetElement source [AXIS_ELEM_MAX_ENTRY];
	double		  auto_bound [AXIS_ELEM_MAX_ENTRY];
	struct {
		gboolean tick_in, tick_out;
		int size_pts;
	} major, minor;
	gboolean major_tick_labeled;
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

typedef GogStyledObjectClass GogAxisClass;

static GType gog_axis_view_get_type (void);

static GObjectClass *parent_klass;

enum {
	AXIS_PROP_0,
	AXIS_PROP_TYPE,
	AXIS_PROP_POS,
	AXIS_PROP_POS_STR,
	AXIS_PROP_INVERT,
	AXIS_PROP_MAP,
	AXIS_PROP_MAJOR_TICK_LABELED,
	AXIS_PROP_MAJOR_TICK_IN,
	AXIS_PROP_MAJOR_TICK_OUT,
	AXIS_PROP_MAJOR_TICK_SIZE_PTS,
	AXIS_PROP_MINOR_TICK_IN,
	AXIS_PROP_MINOR_TICK_OUT,
	AXIS_PROP_MINOR_TICK_SIZE_PTS,
	AXIS_PROP_ASSIGNED_FORMAT_STR_XL
};

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
create_invalid_axis_ticks (double min, double max, gboolean draw_labels) {
	GogAxisTick *ticks;
	
	ticks = g_new (GogAxisTick, 2);
	ticks[0].position = min;
	ticks[1].position = max;
	ticks[0].type = ticks[1].type = GOG_AXIS_TICK_MAJOR;
	ticks[0].label = draw_labels ? g_strdup ("##") : NULL;
	ticks[1].label = draw_labels ? g_strdup ("##") : NULL;

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
		data->scale = (map->axis->center_on_ticks)?
				1.0 / (data->max - data->min - 1):
				1.0 / (data->max - data->min);
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
map_discrete_to_canvas (GogAxisMap *map, double value, gboolean inverted)
{
	MapData *data = map->data;

	return inverted ? 
		((map->axis->center_on_ticks)?
			(data->min + data->max - 1 - value) * data->a + data->b :
			(data->min + data->max - value) * data->a + data->b) :
		value * data->a + data->b;
}

static void
map_discrete_auto_bound (GogAxis *axis, 
			 double minimum, 
			 double maximum, 
			 double *bound)
{
	if ((maximum - minimum) > GOG_AXIS_DISCRETE_AUTO_MAX_MAJOR_TICK_NBR) 
		bound [AXIS_ELEM_MAJOR_TICK] = 
		bound [AXIS_ELEM_MINOR_TICK] =
			ceil ((maximum - minimum + 1.0) / 
			      (double) GOG_AXIS_DISCRETE_AUTO_MAX_MAJOR_TICK_NBR); 
	else
		bound [AXIS_ELEM_MAJOR_TICK] = 
		bound [AXIS_ELEM_MINOR_TICK] = 1.;

	bound [AXIS_ELEM_CROSS_POINT] = 1.;
	bound [AXIS_ELEM_MIN] = minimum;
	bound [AXIS_ELEM_MAX] = maximum;
}

static void
map_discrete_calc_ticks (GogAxis *axis, 
			 gboolean draw_labels)
{
	GogAxisTick *ticks;
	gboolean valid;
	double maximum, minimum;
	int tick_nbr;
	int i, count;
	int major_tick, major_label;

	major_tick = rint (gog_axis_get_entry (axis, AXIS_ELEM_MAJOR_TICK, NULL));
	major_label = rint (gog_axis_get_entry (axis, AXIS_ELEM_MINOR_TICK, NULL));
	if (major_tick < 1)
		major_tick = 1;
	if (major_label < 1)
		major_label = 1;
	
	valid = gog_axis_get_bounds (axis, &minimum, &maximum);
	if (!valid) {
		gog_axis_set_ticks (axis, 2, create_invalid_axis_ticks (0.0, 1.0, draw_labels));
		return;
	}
		
	tick_nbr = rint (maximum -minimum) + 1;
	if (axis->center_on_ticks)
		tick_nbr--;
	if (tick_nbr < 1) {
		gog_axis_set_ticks (axis, 0, NULL);
		return;
	}
	ticks = g_new (GogAxisTick, tick_nbr);
	
	count = 0;
	for (i = 0; i < tick_nbr; i++) {
		ticks[count].position = (double) (i);
		ticks[count].type = GOG_AXIS_TICK_MINOR;
		ticks[count].label = NULL;
		
		if (i % major_tick == 0) 
			ticks[count].type = GOG_AXIS_TICK_MAJOR;

		/* Minimum >= .0 test is a trick to know if it's a barcol or an area/line plot */
		if (i == 0 && minimum >=.0 && !axis->center_on_ticks)
			ticks[count].type = GOG_AXIS_TICK_NONE;
		if ((i % major_label == 0) && draw_labels && 
		    (i < tick_nbr - 1 || minimum >= .0)) {
			if (axis->labels != NULL) {
				if (i < go_data_vector_get_len (axis->labels))
					ticks[count].label = go_data_vector_get_str (axis->labels, i);
				else
					ticks[count].label = NULL;
			}
			else
				ticks[count].label = g_strdup_printf ("%d", i + 1);
		}

		count++;
	}
	
	ticks = get_adjusted_tick_array (ticks, tick_nbr, count);
	gog_axis_set_ticks (axis, count, ticks);
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
map_linear_to_canvas (GogAxisMap *map, double value, gboolean inverted)
{
	MapData *data = map->data;

	return inverted ? 
		(data->min + data->max - value) * data->a + data->b :
		value * data->a + data->b;
}

static void
map_linear_auto_bound (GogAxis *axis, double minimum, double maximum, double *bound)
{
	double step, range, mant;
	int expon;

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
	bound [AXIS_ELEM_MIN] = step * floor (ldexp (mant - DBL_EPSILON, expon));
	mant = frexp (maximum / step, &expon);
	bound [AXIS_ELEM_MAX] = step * ceil (ldexp (mant + DBL_EPSILON, expon));
	bound [AXIS_ELEM_MAJOR_TICK] = step;
	bound [AXIS_ELEM_MINOR_TICK] = step / 5.;

	/* pull to zero if its nearby (do not pull both directions to 0) */
	if (bound [AXIS_ELEM_MIN] > 0 &&
	    (bound [AXIS_ELEM_MIN] - 10. * step) < 0)
		bound [AXIS_ELEM_MIN] = 0;
	else if (bound [AXIS_ELEM_MAX] < 0 &&
	    (bound [AXIS_ELEM_MAX] + 10. * step) < 0)
		bound [AXIS_ELEM_MAX] = 0;

	/* The epsilon shift can pull us away from a zero we want to
	 * keep (eg percentage bars withno negative elements) */
	if (bound [AXIS_ELEM_MIN] < 0 && minimum >= 0.)
		bound [AXIS_ELEM_MIN] = 0;
	else if (bound [AXIS_ELEM_MAX] > 0 && maximum <= 0.)
		bound [AXIS_ELEM_MAX] = 0;
}

static void
map_linear_calc_ticks (GogAxis *axis, 
		       gboolean draw_labels)
{
	GogAxisTick *ticks;
	double maximum, minimum;
	double tick_step;
	double major_tick, minor_tick;
	int tick_nbr, ratio, i;

	major_tick = gog_axis_get_entry (axis, AXIS_ELEM_MAJOR_TICK, NULL);
	minor_tick = gog_axis_get_entry (axis, AXIS_ELEM_MINOR_TICK, NULL);
	if (minor_tick < major_tick)
		tick_step = minor_tick;
	else
		tick_step = major_tick;
	ratio = rint (major_tick / tick_step);
	
	if (!gog_axis_get_bounds (axis, &minimum, &maximum)) {
		gog_axis_set_ticks (axis, 2, create_invalid_axis_ticks (0.0, 1.0, draw_labels));
		return;
	}

	tick_nbr = floor (go_add_epsilon ((maximum - minimum) / tick_step + 1.0));
	if (tick_nbr < 1 || tick_nbr > GOG_AXIS_MAX_TICK_NBR) {
		gog_axis_set_ticks (axis, 0, NULL);
		return;
	}
	ticks = g_new0 (GogAxisTick, tick_nbr);

	for (i = 0; i < tick_nbr; i++) {
		ticks[i].position = minimum + (double) i * tick_step;
		if (fabs (ticks[i].position) < tick_step / 1E10)
			ticks[i].position = 0.0;
		if (rint (fmod (i, ratio)) == 0) {
			ticks[i].type = GOG_AXIS_TICK_MAJOR;
			if (draw_labels) {
				if (axis->assigned_format == NULL || 
				    style_format_is_general (axis->assigned_format))
					ticks[i].label = go_format_value (axis->format, ticks[i].position);
				else
					ticks[i].label = go_format_value (axis->assigned_format, ticks[i].position);
			}
			else
				ticks[i].label = NULL;
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
map_log_to_canvas (GogAxisMap *map, double value, gboolean inverted) 
{
	MapLogData *data = map->data;
	double result;
	
	if (value <= 0.) 
		/* Make libart happy */
		result = inverted ? -DBL_MAX : DBL_MAX; 
	else
		result = inverted ? 
			log (value) * data->a_inv + data->b_inv :
			log (value) * data->a + data->b;

	return result;
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

	bound [AXIS_ELEM_MIN] = pow ( 10.0, minimum);
	bound [AXIS_ELEM_MAX] = pow ( 10.0, maximum);
	bound [AXIS_ELEM_MAJOR_TICK] = step;
	bound [AXIS_ELEM_MINOR_TICK] = 8;
}

static void
map_log_calc_ticks (GogAxis *axis, 
		    gboolean draw_labels)
{
	GogAxisTick *ticks;
	double maximum, minimum;
	double position;
	int major_tick, minor_tick, major_label, start_tick;
	int tick_nbr, i, j;
	int count;

	major_label = rint (gog_axis_get_entry (axis, AXIS_ELEM_MAJOR_TICK, NULL));
	minor_tick = rint (gog_axis_get_entry (axis, AXIS_ELEM_MINOR_TICK, NULL) + 1.0);

	if (!gog_axis_get_bounds (axis, &minimum, &maximum) || major_label < 1) {
		gog_axis_set_ticks (axis, 2, create_invalid_axis_ticks (1.0, 10.0, draw_labels));
		return;
	}
	if (minimum <= 0.0) {
		gog_axis_set_ticks (axis, 2, create_invalid_axis_ticks (1.0, 10.0, draw_labels));
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
			if ((i) % major_label == 0 && draw_labels) {
				ticks[count].type = GOG_AXIS_TICK_MAJOR;
				if (axis->assigned_format == NULL || 
				    style_format_is_general (axis->assigned_format))
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
	map_discrete,			map_discrete_to_canvas,
	map_discrete_init,		NULL,
	map_discrete_auto_bound,	map_discrete_calc_ticks,
	N_("Discrete"),			N_("Discrete mapping")
};

static const GogAxisMapDesc map_descs[] = 
{
	{
		map_linear,		map_linear_to_canvas,
		map_linear_init, 	NULL,	
		map_linear_auto_bound, 	map_linear_calc_ticks,	
		N_("Linear"),		N_("Linear mapping")
	},
	{
		map_log,		map_log_to_canvas,
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
 * gog_axis_map_to_canvas in order to translates data coordinates 
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
 * gog_axis_map_to_canvas :
 * @map : #GogAxisMap
 * @value : value to map to canvas space.
 *
 * Return a value in canvas coordinates, where
 * [offset,offset+length] means a data within plot bounds.
 **/

double 
gog_axis_map_to_canvas (GogAxisMap *map,
			double value)
{
	return map->desc->map_to_canvas (map, value, map->axis->inverted);
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

	tmp = gog_axis_get_entry (axis, AXIS_ELEM_MIN, &user_defined);
	if (user_defined) minimum = tmp;

	tmp = gog_axis_get_entry (axis, AXIS_ELEM_MAX, &user_defined);
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
		map_desc_discrete.calc_ticks (axis, 
					      axis->major_tick_labeled);
	else
		if (axis->map_desc->calc_ticks)
			axis->map_desc->calc_ticks (axis,
						    axis->major_tick_labeled);
}

/*****************************************************************************/

static void
role_label_post_add (GogObject *parent, GogObject *label)
{
	GogAxis const *axis = GOG_AXIS (parent);
	if (axis->pos == GOG_AXIS_AT_LOW)
		label->position = (axis->type == GOG_AXIS_X)
			? (GOG_POSITION_S|GOG_POSITION_ALIGN_CENTER)
			: (GOG_POSITION_W|GOG_POSITION_ALIGN_CENTER);
	else
		label->position = (axis->type == GOG_AXIS_X)
			? (GOG_POSITION_N|GOG_POSITION_ALIGN_CENTER)
			: (GOG_POSITION_E|GOG_POSITION_ALIGN_CENTER);
}

static gboolean
role_grid_line_can_add (GogObject const *parent, char const *type)
{
	GSList *children;

	children = gog_object_get_children (parent, 
		gog_object_find_role_by_name (GOG_OBJECT (parent), type));
	if (children != NULL) {
		g_slist_free (children);
		return FALSE;
	}

	return TRUE;
}

static gboolean
role_grid_line_major_can_add (GogObject const *parent)
{
	GogAxis *axis = GOG_AXIS (parent);
	GogAxisType type = gog_axis_get_atype (axis);
	
	return ((type == GOG_AXIS_X || type == GOG_AXIS_Y || type == GOG_AXIS_RADIAL) &&
		role_grid_line_can_add (parent, "MajorGrid"));
}

static gboolean
role_grid_line_minor_can_add (GogObject const *parent)
{
	GogAxis *axis = GOG_AXIS (parent);
	GogAxisType type = gog_axis_get_atype (axis);
	
	return (!gog_axis_is_discrete (GOG_AXIS (parent)) &&
		(type == GOG_AXIS_X || type == GOG_AXIS_Y || type == GOG_AXIS_RADIAL) &&
		role_grid_line_can_add (parent, "MinorGrid"));
}

static void 
role_grid_line_major_post_add (GogObject *parent, GogObject *child)  
{
	g_object_set (G_OBJECT (child), "is_minor", (gboolean)FALSE, NULL);
}

static void 
role_grid_line_minor_post_add (GogObject *parent, GogObject *child)  
{ 
	g_object_set (G_OBJECT (child), "is_minor", (gboolean)TRUE, NULL);
}

static gboolean
gog_axis_set_pos (GogAxis *axis, GogAxisPosition pos)
{
	GSList *ptr;
	if (axis->pos == pos)
		return FALSE;
	axis->pos = pos;

	for (ptr = GOG_OBJECT (axis)->children ; ptr != NULL ; ptr = ptr->next)
		if (IS_GOG_LABEL (ptr->data))
			role_label_post_add (GOG_OBJECT (axis), ptr->data);
	return TRUE;
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
		}
		break;
	case AXIS_PROP_POS:
		resized = gog_axis_set_pos (axis, g_value_get_int (value));
		break;
	case AXIS_PROP_POS_STR: {
		char const *str = g_value_get_string (value);
		if (str == NULL)
			return;
		else if (!g_ascii_strcasecmp (str, "low"))
			itmp = GOG_AXIS_AT_LOW;
		else if (!g_ascii_strcasecmp (str, "middle"))
			itmp = GOG_AXIS_IN_MIDDLE;
		else if (!g_ascii_strcasecmp (str, "high"))
			itmp = GOG_AXIS_AT_HIGH;
		else
			return;
		resized = gog_axis_set_pos (axis, itmp);
		break;
	}
	case AXIS_PROP_INVERT:
		axis->inverted = g_value_get_boolean (value);
		calc_ticks = TRUE;
		break;
	case AXIS_PROP_MAP :
		gog_axis_map_set (axis, g_value_get_string (value));
		request_update = TRUE;
		break;

	case AXIS_PROP_MAJOR_TICK_LABELED:
		itmp = g_value_get_boolean (value);
		if (axis->major_tick_labeled != itmp) {
			axis->major_tick_labeled = itmp;
			resized = TRUE;
			calc_ticks = TRUE;
		}
		break;
	case AXIS_PROP_MAJOR_TICK_IN :
		itmp = g_value_get_boolean (value);
		if (axis->major.tick_in != itmp) {
			axis->major.tick_in = itmp;
			if (itmp != axis->major.tick_out)
				calc_ticks = TRUE;
		}
		break;
	case AXIS_PROP_MAJOR_TICK_OUT :
		itmp = g_value_get_boolean (value);
		if (axis->major.tick_out != itmp) {
			axis->major.tick_out = itmp;
			resized = axis->major.size_pts > 0;
			if (itmp != axis->major.tick_in)
				calc_ticks = TRUE;
		}
		break;
	case AXIS_PROP_MAJOR_TICK_SIZE_PTS:
		itmp = g_value_get_int (value);
		if (axis->major.size_pts != itmp) {
			axis->major.size_pts = itmp;
			resized = axis->major.tick_out;
		}
		break;

	case AXIS_PROP_MINOR_TICK_IN :
		itmp = g_value_get_boolean (value);
		if (axis->minor.tick_in != itmp) {
			axis->minor.tick_in = itmp;
			if (itmp != axis->minor.tick_out)
				calc_ticks = TRUE;
		}
		break;
	case AXIS_PROP_MINOR_TICK_OUT :
		itmp = g_value_get_boolean (value);
		if (axis->minor.tick_out != itmp) {
			axis->minor.tick_out = itmp;
			resized = axis->minor.size_pts > 0;
			if (itmp != axis->minor.tick_in)
				calc_ticks = TRUE;
		}
		break;
	case AXIS_PROP_MINOR_TICK_SIZE_PTS:
		itmp = g_value_get_int (value);
		if (axis->minor.size_pts != itmp) {
			axis->minor.size_pts = itmp;
			resized = axis->minor.tick_out;
		}
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
	case AXIS_PROP_POS:
		g_value_set_int (value, axis->pos);
		break;
	case AXIS_PROP_POS_STR:
		switch (axis->pos) {
		case GOG_AXIS_AT_LOW:
			g_value_set_static_string (value, "low");
			break;
		case GOG_AXIS_IN_MIDDLE:
			g_value_set_static_string (value, "middle");
			break;
		case GOG_AXIS_AT_HIGH:
			g_value_set_static_string (value, "high");
			break;
		}
		break;
	case AXIS_PROP_INVERT:
		g_value_set_boolean (value, axis->inverted);
		break;
	case AXIS_PROP_MAP:
		g_value_set_string (value, axis->map_desc->name);
		break;

	case AXIS_PROP_MAJOR_TICK_LABELED:
		g_value_set_boolean (value, axis->major_tick_labeled);
		break;
	case AXIS_PROP_MAJOR_TICK_IN:
		g_value_set_boolean (value, axis->major.tick_in);
		break;
	case AXIS_PROP_MAJOR_TICK_OUT:
		g_value_set_boolean (value, axis->major.tick_out);
		break;
	case AXIS_PROP_MAJOR_TICK_SIZE_PTS:
		g_value_set_int (value, axis->major.size_pts);
		break;

	case AXIS_PROP_MINOR_TICK_IN:
		g_value_set_boolean (value, axis->minor.tick_in);
		break;
	case AXIS_PROP_MINOR_TICK_OUT:
		g_value_set_boolean (value, axis->minor.tick_out);
		break;
	case AXIS_PROP_MINOR_TICK_SIZE_PTS:
		g_value_set_int (value, axis->minor.size_pts);
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
	g_return_val_if_fail (i >= AXIS_ELEM_MIN && i < AXIS_ELEM_MAX_ENTRY, go_nan);

	dat = axis->source [i].data;
	if (dat != NULL && IS_GO_DATA_SCALAR (dat)) {
		double tmp = go_data_scalar_get_value (GO_DATA_SCALAR (dat));
		if (go_finite (tmp)) {
			if (user_defined)
				*user_defined = TRUE;
			return tmp;
		}
	}

	return axis->auto_bound [i];
}

static void
gog_axis_update (GogObject *obj)
{
	GSList *ptr;
	GogAxis *axis = GOG_AXIS (obj);
	double old_min = axis->auto_bound [AXIS_ELEM_MIN];
	double old_max = axis->auto_bound [AXIS_ELEM_MAX];
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
			axis->center_on_ticks = bounds.center_on_ticks;
		}

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
	    axis->auto_bound [AXIS_ELEM_MIN] < axis->logical_min_val)
		axis->auto_bound [AXIS_ELEM_MIN] = axis->logical_min_val;
	if (go_finite (axis->logical_max_val) &&
	    axis->auto_bound [AXIS_ELEM_MAX] > axis->logical_max_val)
		axis->auto_bound [AXIS_ELEM_MAX] = axis->logical_max_val;

	gog_axis_calc_ticks (axis);

	/* FIXME: there isn't an easy way to know if a discrete axis
	 * needs to emit a changed signal. So allways emit changed signal if
	 * axis is discrete */
	if (old_min != axis->auto_bound [AXIS_ELEM_MIN] ||
	    old_max != axis->auto_bound [AXIS_ELEM_MAX] ||
	    axis->is_discrete)
		gog_object_emit_changed (GOG_OBJECT (obj), TRUE);
}

static void
cb_pos_changed (GtkToggleButton *toggle_button, GObject *axis)
{
	g_object_set (axis,
		"pos_str", gtk_toggle_button_get_active (toggle_button) ? "low" : "high",
		NULL);
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

static gpointer
gog_axis_editor (GogObject *gobj, GogDataAllocator *dalloc, GnmCmdContext *cc)
{
	static guint axis_pref_page = 0;
	static char const *toggle_props[] = {
		"invert-axis",
		"major-tick-labeled",
		"major-tick-out",
		"major-tick-in",
		"minor-tick-out",
		"minor-tick-in"
	};
	GtkWidget *w, *notebook; /* , *cbox; */
	GtkTable  *table;
	unsigned i = 0;
	GogAxis *axis = GOG_AXIS (gobj);
	GogDataset *set = GOG_DATASET (gobj);
	GladeXML *gui;

	/* No preferences for circular axis */
	if (axis->type == GOG_AXIS_CIRCULAR)
		return NULL;

	gui = gnm_glade_xml_new (cc, "gog-axis-prefs.glade", "axis_pref_box", NULL);
	if (gui == NULL)
		return NULL;
	notebook = gtk_notebook_new ();

	gtk_notebook_prepend_page (GTK_NOTEBOOK (notebook),
		glade_xml_get_widget (gui, "axis_pref_box"),
		gtk_label_new (_("Details")));
	gog_styled_object_editor (GOG_STYLED_OBJECT (gobj), cc, notebook);

	if (!axis->is_discrete) {
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

	w = glade_xml_get_widget (gui, "axis_low");
	if (axis->pos == GOG_AXIS_AT_LOW)
		gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (w), TRUE);
	else
		gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (
			glade_xml_get_widget (gui, "axis_high")), TRUE);
	g_signal_connect_object (G_OBJECT (w),
		"toggled",
		G_CALLBACK (cb_pos_changed), axis, 0);

	for (i = 0; i < G_N_ELEMENTS (toggle_props) ; i++) {
		gboolean cur_val;
		GtkWidget *w = glade_xml_get_widget (gui, toggle_props[i]);

		g_object_get (G_OBJECT (gobj), toggle_props[i], &cur_val, NULL);
		gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (w), cur_val);
		g_signal_connect_object (G_OBJECT (w),
					 "toggled",
					 G_CALLBACK (cb_axis_toggle_changed), axis, 0);
	}
	if (axis->is_discrete) {
		/* Hide minor tick properties */
		GtkWidget *w = glade_xml_get_widget (gui, "minor_tick_box");
		gtk_widget_hide (w);
	}

	/* Bounds Page */
	w = gtk_table_new (1, 2, FALSE);
	table = GTK_TABLE (w);
	w = gtk_label_new (_("Automatic"));
	gtk_misc_set_alignment (GTK_MISC (w), 0., .5);
	gtk_table_set_row_spacings (table, 6);
	gtk_table_set_col_spacings (table, 12);
	gtk_container_set_border_width (GTK_CONTAINER (table), 12);
	gtk_table_attach (table, w, 0, 1, 0, 1, GTK_FILL, 0, 0, 0);
	if (axis->is_discrete) {
		static char const * const dim_names[] = {
			NULL,
			NULL,
			N_("Categories between _ticks"),
			N_("Categories between _labels"),
			N_("_Cross at category #")
		};
		for (i = AXIS_ELEM_MAJOR_TICK; i < AXIS_ELEM_MAX_ENTRY ; i++)
			make_dim_editor (set, table, i, dalloc, dim_names);
		gtk_widget_show_all (GTK_WIDGET (table));
		gtk_notebook_prepend_page (GTK_NOTEBOOK (notebook), GTK_WIDGET (table),
			gtk_label_new (_("Bounds")));
	} else {
		static char const * const dim_names[] = {
			N_("M_in"),
			N_("M_ax"),
			N_("Ma_jor Ticks"),
			N_("Mi_nor Ticks"),
			N_("_Cross")
		};

		for (i = AXIS_ELEM_MIN; i < AXIS_ELEM_MAX_ENTRY ; i++)
			make_dim_editor (set, table, i, dalloc, dim_names);
		gtk_widget_show_all (GTK_WIDGET (table));
		gtk_notebook_prepend_page (GTK_NOTEBOOK (notebook), GTK_WIDGET (table),
			gtk_label_new (_("Bounds")));

		w = number_format_selector_new ();
		if (axis->assigned_format != NULL && !style_format_is_general (axis->assigned_format))
			number_format_selector_set_style_format (NUMBER_FORMAT_SELECTOR (w),
				axis->assigned_format);
		else if (axis->format != NULL)
			number_format_selector_set_style_format (NUMBER_FORMAT_SELECTOR (w),
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
		gtk_notebook_append_page (GTK_NOTEBOOK (notebook), w,
			gtk_label_new (_("Format")));
#endif

		gtk_widget_show (w);
		g_signal_connect (G_OBJECT (w),
			"number_format_changed",
			G_CALLBACK (cb_axis_fmt_changed), axis);
	}

	g_object_set_data_full (G_OBJECT (notebook), "gui", gui,
				(GDestroyNotify)g_object_unref);

	gog_style_handle_notebook (notebook, &axis_pref_page);
	gtk_widget_show (GTK_WIDGET (notebook));
	return notebook;
}

static void
gog_axis_init_style (GogStyledObject *gso, GogStyle *style)
{
	style->interesting_fields = GOG_STYLE_LINE | GOG_STYLE_FONT;
	gog_theme_fillin_style (gog_object_get_theme (GOG_OBJECT (gso)),
		style, GOG_OBJECT (gso), 0, FALSE);
}

static void
gog_axis_class_init (GObjectClass *gobject_klass)
{
	static GogObjectRole const roles[] = { 
		{ N_("Label"), "GogLabel", 0,
		  GOG_POSITION_COMPASS, GOG_POSITION_S|GOG_POSITION_ALIGN_CENTER, GOG_OBJECT_NAME_BY_ROLE,
		  NULL, NULL, NULL, role_label_post_add, NULL, NULL, { -1 } },
		{ N_("MinorGrid"), "GogGridLine", 0,
		  GOG_POSITION_SPECIAL, GOG_POSITION_SPECIAL, GOG_OBJECT_NAME_BY_ROLE,
		  role_grid_line_minor_can_add, NULL, NULL, role_grid_line_minor_post_add, NULL, NULL, { -1 } },
		{ N_("MajorGrid"), "GogGridLine", 1,
		  GOG_POSITION_SPECIAL, GOG_POSITION_SPECIAL, GOG_OBJECT_NAME_BY_ROLE,
		  role_grid_line_major_can_add, NULL, NULL, role_grid_line_major_post_add, NULL, NULL, { -1 } }
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
	g_object_class_install_property (gobject_klass, AXIS_PROP_POS,
		g_param_spec_int ("pos", "pos",
			"GogAxisPosition",
			GOG_AXIS_AT_LOW, GOG_AXIS_AT_HIGH, GOG_AXIS_AT_LOW, G_PARAM_READWRITE));
	g_object_class_install_property (gobject_klass, AXIS_PROP_POS_STR,
		g_param_spec_string ("pos_str", "pos_str",
			"Where to position an axis low, high, or crossing",
			"low", G_PARAM_READWRITE | GOG_PARAM_PERSISTENT));

	g_object_class_install_property (gobject_klass, AXIS_PROP_INVERT,
		g_param_spec_boolean ("invert-axis", NULL,
			"Scale from high to low rather than low to high",
			FALSE, G_PARAM_READWRITE | GOG_PARAM_PERSISTENT));
	g_object_class_install_property (gobject_klass, AXIS_PROP_MAP,
		g_param_spec_string ("map-name", "MapName",
			"The name of the map for scaling",
			"linear", G_PARAM_READWRITE|GOG_PARAM_PERSISTENT));
	g_object_class_install_property (gobject_klass, AXIS_PROP_MAJOR_TICK_LABELED,
		g_param_spec_boolean ("major-tick-labeled", NULL,
			"Show labels for major ticks",
			TRUE, G_PARAM_READWRITE | GOG_PARAM_PERSISTENT));
	g_object_class_install_property (gobject_klass, AXIS_PROP_MAJOR_TICK_IN,
		g_param_spec_boolean ("major-tick-in", NULL,
			"Major tick marks inside the axis",
			FALSE, G_PARAM_READWRITE | GOG_PARAM_PERSISTENT));
	g_object_class_install_property (gobject_klass, AXIS_PROP_MAJOR_TICK_OUT,
		g_param_spec_boolean ("major-tick-out", NULL,
			"Major tick marks outside the axis",
			TRUE, G_PARAM_READWRITE | GOG_PARAM_PERSISTENT));
	g_object_class_install_property (gobject_klass, AXIS_PROP_MAJOR_TICK_SIZE_PTS,
		g_param_spec_int ("major-tick-size-pts", "major-tick-size-pts",
			"Size of the major tick marks in pts",
			0, 20, 4, G_PARAM_READWRITE | GOG_PARAM_PERSISTENT));

	g_object_class_install_property (gobject_klass, AXIS_PROP_MINOR_TICK_IN,
		g_param_spec_boolean ("minor-tick-in", NULL,
			"Minor tick marks inside the axis",
			FALSE, G_PARAM_READWRITE | GOG_PARAM_PERSISTENT));
	g_object_class_install_property (gobject_klass, AXIS_PROP_MINOR_TICK_OUT,
		g_param_spec_boolean ("minor-tick-out", NULL,
			"Minor tick marks outside the axis",
			FALSE, G_PARAM_READWRITE | GOG_PARAM_PERSISTENT));
	g_object_class_install_property (gobject_klass, AXIS_PROP_MINOR_TICK_SIZE_PTS,
		g_param_spec_int ("minor-tick-size-pts", NULL,
			"Size of the minor tick marks in pts",
			0, 15, 2, G_PARAM_READWRITE | GOG_PARAM_PERSISTENT));
	g_object_class_install_property (gobject_klass, AXIS_PROP_ASSIGNED_FORMAT_STR_XL,
		g_param_spec_string ("assigned-format-string-XL", NULL,
			"The user assigned format to use for non-discrete axis labels (XL format)",
			"General", G_PARAM_READWRITE | GOG_PARAM_PERSISTENT));

	gog_object_register_roles (gog_klass, roles, G_N_ELEMENTS (roles));
	gog_klass->update	= gog_axis_update;
	gog_klass->editor	= gog_axis_editor;
	gog_klass->view_type	= gog_axis_view_get_type ();
	style_klass->init_style = gog_axis_init_style;
}

static void
gog_axis_init (GogAxis *axis)
{
	axis->type	 = GOG_AXIS_UNKNOWN;
	axis->pos	 = GOG_AXIS_AT_LOW;
	axis->contributors = NULL;
	axis->minor.tick_in = axis->minor.tick_out = axis->major.tick_in = FALSE;
	axis->major.tick_out = TRUE;
	axis->major_tick_labeled = TRUE;
	axis->inverted = FALSE;
	axis->major.size_pts = 4;
	axis->minor.size_pts = 2;

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
	*first = AXIS_ELEM_MIN;
	*last  = AXIS_ELEM_CROSS_POINT;
}

static GogDatasetElement *
gog_axis_dataset_get_elem (GogDataset const *set, int dim_i)
{
	GogAxis *axis = GOG_AXIS (set);
	if (AXIS_ELEM_MIN <= dim_i && dim_i <= AXIS_ELEM_CROSS_POINT)
		return &axis->source[dim_i];
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
		gog_axis_class_init, gog_axis_init,
		GOG_STYLED_OBJECT_TYPE, 0,
		GSF_INTERFACE (gog_axis_dataset_init, GOG_DATASET_TYPE))


GogAxisType
gog_axis_get_atype (GogAxis const *axis)
{
	g_return_val_if_fail (GOG_AXIS (axis) != NULL, GOG_AXIS_UNKNOWN);
	return axis->type;
}

GogAxisPosition
gog_axis_get_pos (GogAxis const *axis)
{
	g_return_val_if_fail (GOG_AXIS (axis) != NULL, GOG_AXIS_IN_MIDDLE);
	return axis->pos;
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

	*minima = gog_axis_get_entry (axis, AXIS_ELEM_MIN, NULL);
	*maxima = gog_axis_get_entry (axis, AXIS_ELEM_MAX, NULL);

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

/****************************************************************************/

typedef GogView		GogAxisView;
typedef GogViewClass	GogAxisViewClass;

#define GOG_AXIS_VIEW_TYPE	(gog_axis_view_get_type ())
#define GOG_AXIS_VIEW(o)	(G_TYPE_CHECK_INSTANCE_CAST ((o), GOG_AXIS_VIEW_TYPE, GogAxisView))
#define IS_GOG_AXIS_VIEW(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOG_AXIS_VIEW_TYPE))

static GogViewClass *aview_parent_klass;

void
gog_axis_view_padding_request (GogView *v, GogViewPadding *padding,
			       GogViewAllocation *bbox)
{
	GogAxis *axis = GOG_AXIS (v->model);
	GogViewRequisition txt_size;
	gboolean const is_horiz = axis->type == GOG_AXIS_X;
	unsigned i;
	double tick_major = 0., tick_minor = 0.;
	double txt_max_h, txt_max_w;
	double line_width = gog_renderer_line_size (
		v->renderer, axis->base.style->line.width);
	
	double Xl, Xr, wl, wr, xm;
	gboolean label_found = FALSE;
	double position;
	char *label = NULL;
	double padding_l, padding_r, padding_label;
	gboolean is_l_valid, is_r_valid;

	GogAxisMap *map;

	padding->wr = padding->wl = padding->ht = padding->hb = 0.;

	if ((axis->type != GOG_AXIS_X && axis->type != GOG_AXIS_Y))
		return;

	map = gog_axis_map_new (axis, 0., 1.);

	if (is_horiz) {
		xm = bbox->w - bbox->x;
	} else {
		xm = bbox->h - bbox->y;
	}


	padding_l = padding_r = padding_label = 0.;
	Xl = DBL_MAX;
	Xr = -DBL_MAX;
	wl = wr = 0.;
	txt_max_w = txt_max_h = 0.;
	gog_renderer_push_style (v->renderer, axis->base.style);
	for (i = 0; i < axis->tick_nbr; i++) {
		label = axis->ticks[i].label;
		if (label != NULL) {
			label_found = TRUE;
			position = gog_axis_map (map, axis->ticks[i].position);
			gog_renderer_measure_text (v->renderer, label,
						   &txt_size);
			
			if (txt_max_h < txt_size.h)
				txt_max_h = txt_size.h;
			if (txt_max_w < txt_size.w)
				txt_max_w = txt_size.w;
			
			if (position < Xl) {
				Xl = position;
				wl = (is_horiz ? txt_size.w : txt_size.h) / 2.;
			}
			if (position > Xr) {
				Xr = position;
				wr = (is_horiz ? txt_size.w : txt_size.h) / 2.;
			}
		}
	}
	gog_renderer_pop_style (v->renderer);

	if (label_found) { 
		if (Xl != Xr) {
			padding_l = (Xr * wl + Xl * (wr - xm)) / (Xr - Xl);
			padding_r = xm - (((Xr - 1.0) * wl + (Xl - 1.0) * (wr - xm)) / (Xr - Xl));
			is_l_valid = padding_l > 0.;
			is_r_valid = padding_r > 0.;
		} else {
			is_l_valid = Xl > .5;
			is_r_valid = ! is_l_valid;
		}
		
		if (!is_l_valid) {
			padding_l = 0.;
			if (Xr > 0.)
				padding_r = MAX (xm - ((xm - wr)  / Xr), 0.);
			else
				padding_r = 0.;
		} else if (!is_r_valid) {
			padding_r = 0.;
			if (Xl < 1.0) 
				padding_l = MAX ((wl -Xl * xm) / (1 - Xl), 0.);
			else
				padding_l = 0.;
		}
	}

	if (is_horiz) {
		if (line_width > 0) {
			if (axis->major.tick_out)
				tick_major = gog_renderer_pt2r_y (v->renderer,
					axis->major.size_pts);
			if (axis->minor.tick_out)
				tick_minor = gog_renderer_pt2r_y (v->renderer,
							  axis->minor.size_pts);
		}
		padding_label = (axis->is_discrete ?
			MAX (txt_max_h, MAX (tick_minor, tick_major)):
			MAX (tick_major + txt_max_h, tick_minor)) +
			line_width;	
	} else {
		if (line_width > 0) {
			if (axis->major.tick_out)
				tick_major = gog_renderer_pt2r_x (v->renderer,
					axis->major.size_pts);
			if (axis->minor.tick_out)
				tick_minor = gog_renderer_pt2r_x (v->renderer,
								  axis->minor.size_pts);
		}
		padding_label = (axis->is_discrete ?
			MAX (txt_max_w, MAX (tick_major, tick_minor)):
			MAX (tick_major + txt_max_w, tick_minor)) +
			line_width;	
	}

	if (is_horiz) {
		padding->wl = padding_l;
		padding->wr = padding_r;
		if (axis->pos == GOG_AXIS_AT_LOW) {
			padding->hb = padding_label;
			padding->ht = line_width;
		} else {
			padding->hb = line_width;
			padding->ht = padding_label;
		}
	} else {
		padding->ht = padding_r;
		padding->hb = padding_l;
		if (axis->pos == GOG_AXIS_AT_LOW) {
			padding->wl = padding_label;
			padding->wr = line_width;
		} else {
			padding->wl = line_width;
			padding->wr = padding_label;
		}
	}

	gog_axis_map_free (map);
}

static void
gog_axis_view_size_request (GogView *v, GogViewRequisition *req)
{
	gog_view_size_child_request (v, req, req);
}

static gboolean
overlap (GogViewAllocation const *bbox1, GogViewAllocation const *bbox2) {
	return (!((MAX (bbox2->x, bbox2->x + bbox2->w) < MIN (bbox1->x, bbox1->x + bbox1->w)) ||
		  (MAX (bbox2->y, bbox2->y + bbox2->h) < MIN (bbox1->y, bbox1->y + bbox1->h)) ||
		  (MIN (bbox2->x, bbox2->x + bbox2->w) > MAX (bbox1->x, bbox1->x + bbox1->w)) ||
		  (MIN (bbox2->y, bbox2->y + bbox2->h) > MAX (bbox1->y, bbox1->y + bbox1->h))));
}

static void
draw_axis_from_a_to_b (GogView *v, GogAxis *axis, double ax, double ay, double bx, double by,
		       gboolean draw_label) 
{
	GogAxisMap *map = NULL;
	ArtVpath path[3];
	double line_width;
	double axis_length, axis_angle, label_angle;
	double tick_len;
	double major_out_x = 0., major_out_y= 0., major_in_x = 0., major_in_y = 0.;
	double minor_out_x = 0., minor_out_y= 0., minor_in_x = 0., minor_in_y = 0.;
	double cos_alpha, sin_alpha;
	double pos, pos_x, pos_y, offset, label_offset;
	unsigned i;
	GogViewRequisition txt_size;
	GogViewAllocation label_pos, label_result, label_old = {0., 0., 0., 0.};
	gboolean draw_major, draw_minor;
	gboolean is_line_visible;

	axis_length = sqrt ((ax-bx)*(ax-bx)+(ay-by)*(ay-by));
	if (bx - ax != 0) {
		axis_angle = atan ((double)(by-ay)/(double)(bx-ax));
		if (bx < ax) {
			axis_angle += M_PI;
		}
	} else {
		if (ay > by) {
			axis_angle = - M_PI/2.0;
		} else {
			axis_angle = M_PI/2.0;
		}
	}
	label_angle = fmod (axis_angle + 2.0 * M_PI, M_PI);
	if (label_angle > M_PI / 2.0)
		label_angle = M_PI - label_angle;
	cos_alpha = cos (axis_angle + M_PI / 2.0);
	sin_alpha = sin (axis_angle + M_PI / 2.0);

	is_line_visible = gog_style_is_line_visible (axis->base.style);
	line_width = gog_renderer_line_size (v->renderer, axis->base.style->line.width) / 2;
	if (is_line_visible)
	{
		path[0].code = ART_MOVETO;
		path[1].code = ART_LINETO;
		path[2].code = ART_END;

		path[0].x = ax;
		path[0].y = ay;
		path[1].x = bx;
		path[1].y = by;
		gog_renderer_draw_path (v->renderer, path, NULL);

		map = gog_axis_map_new (axis, 0., axis_length);
	}

	draw_major = axis->major.tick_in || axis->major.tick_out;
	draw_minor = axis->minor.tick_in || axis->minor.tick_out;

	tick_len = gog_renderer_pt2r_x (v->renderer, axis->minor.size_pts) + line_width;
	minor_out_x = axis->minor.tick_out ? - tick_len * cos_alpha : 0.;
	minor_out_y = axis->minor.tick_out ? - tick_len * sin_alpha : 0.;
	minor_in_x = axis->minor.tick_in ? tick_len * cos_alpha : 0.;
	minor_in_y = axis->minor.tick_in ? tick_len * sin_alpha : 0.;
	tick_len = gog_renderer_pt2r_x (v->renderer, axis->major.size_pts) + line_width;
	major_out_x = axis->major.tick_out ? - tick_len * cos_alpha : 0.;
	major_out_y = axis->major.tick_out ? - tick_len * sin_alpha : 0.;
	major_in_x = axis->major.tick_in ? tick_len * cos_alpha : 0.;
	major_in_y = axis->major.tick_in ? tick_len * sin_alpha : 0.;
	label_offset = gog_renderer_pt2r_x (v->renderer, TICK_LABEL_PAD_HORIZ) + 
		axis->major.tick_in ? tick_len : 0.;

	for (i = 0; i < axis->tick_nbr; i++) {
		pos = gog_axis_map_to_canvas (map, axis->ticks[i].position);
		pos_x = ax + pos * cos (axis_angle);
		pos_y = ay + pos * sin (axis_angle);

		if (is_line_visible) {
			switch (axis->ticks[i].type) {
				case GOG_AXIS_TICK_MAJOR:
					if (draw_major) {
						path[0].x = major_out_x + pos_x;
						path[1].x = major_in_x + pos_x;
						path[0].y = major_out_y + pos_y;
						path[1].y = major_in_y + pos_y;
						gog_renderer_draw_path (v->renderer, path, NULL);
					}
					break;

				case GOG_AXIS_TICK_MINOR:
					if (draw_minor) {
						path[0].x = minor_out_x + pos_x;
						path[1].x = minor_in_x + pos_x;
						path[0].y = minor_out_y + pos_y;
						path[1].y = minor_in_y + pos_y;
						gog_renderer_draw_path (v->renderer, path, NULL);
					}
					break;

				default:
					break;
			}
		}

		if (axis->ticks[i].label != NULL && 
		    gog_axis_map (map, axis->ticks[i].position) >= 0.1 &&
		    draw_label) {
			gog_renderer_measure_text (v->renderer, axis->ticks[i].label, &txt_size);
			offset = (txt_size.h * cos (label_angle) + txt_size.w * sin (label_angle)) / 2.0 + label_offset;
			label_pos.x = pos_x + offset * cos_alpha;
			label_pos.y = pos_y + offset * sin_alpha;
			label_pos.w = txt_size.w;
			label_pos.h = txt_size.h;
			if (!overlap (&label_pos, &label_old)) {
				gog_renderer_draw_text (v->renderer, axis->ticks[i].label,
							&label_pos, GTK_ANCHOR_CENTER, &label_result);
				label_old = label_pos;
			}
		}	
	}

	if (is_line_visible)
		gog_axis_map_free (map);
}

static void
gog_axis_view_render_children (GogView *view, GogViewAllocation const *bbox)
{
	GSList *ptr;

	/* Render every child except grid lines. Those are rendered
	 * before in gog_chart_view since we don't want to render them
	 * over axis. */
	for (ptr = view->children ; ptr != NULL ; ptr = ptr->next) {
		if (!IS_GOG_GRID_LINE (GOG_VIEW (ptr->data)->model))
			gog_view_render	(ptr->data, bbox);
	}
}

static void
gog_axis_view_render (GogView *v, GogViewAllocation const *bbox)
{
	GtkAnchorType anchor;
	GogViewAllocation const *area;
	GogViewAllocation label_pos, label_result;
	GogViewRequisition txt_size;
	double last_label_pos = .0, last_label_size = -DBL_MAX;
	ArtVpath path[3];
	GogAxis *axis = GOG_AXIS (v->model);
	GogStyle *style = axis->base.style;
	unsigned i;
	double tick_len, label_pad, dir, center, label_spacing = 0.;
	double line_width = gog_renderer_line_size (v->renderer,
						    style->line.width) / 2.;
	double pos, offset;
	double major_in = 0., major_out = 0.;
	double minor_in = 0., minor_out = 0.;
	gboolean draw_major, draw_minor;
	gboolean is_line_visible = gog_style_is_line_visible (style);

	gog_axis_view_render_children (v, bbox);

	g_return_if_fail (axis->pos != GOG_AXIS_IN_MIDDLE);
	g_return_if_fail (v->parent != NULL);
	area = gog_chart_view_get_plot_area (v->parent);
	g_return_if_fail (area != NULL);

	gog_renderer_push_style (v->renderer, style);

	draw_major = (axis->major.tick_in || axis->major.tick_out) && is_line_visible; 
	draw_minor = (axis->minor.tick_in || axis->minor.tick_out) && is_line_visible;

	path[0].code = ART_MOVETO;
	path[1].code = ART_LINETO;
	path[2].code = ART_END;

	switch (axis->type) {
	case GOG_AXIS_X:
		switch (axis->pos) {
			default :
			case GOG_AXIS_AT_LOW:
				anchor = GTK_ANCHOR_N;
				dir = 1.;
				center = area->y + area->h;
				break;

			case GOG_AXIS_AT_HIGH:
				anchor = GTK_ANCHOR_S;
				dir = -1.;
				center = area->y;
				break;
		}

		tick_len = gog_renderer_pt2r_y (v->renderer, axis->major.size_pts);
		major_out = axis->major.tick_out ? center + dir * (line_width + tick_len) : center;
		major_in  = axis->major.tick_in  ? center - dir * (line_width + tick_len) : center;
		tick_len = gog_renderer_pt2r_y (v->renderer, axis->minor.size_pts);
		minor_out = axis->minor.tick_out ? center + dir * (line_width + tick_len) : center;
		minor_in  = axis->minor.tick_in  ? center - dir * (line_width + tick_len) : center;

		if (is_line_visible) {
			path[0].y = path[1].y = center;
			path[0].x = area->x - line_width;
			path[1].x = area->x + area->w + line_width;
			gog_renderer_draw_sharp_path (v->renderer, path, NULL);
		}

		if (axis->major_tick_labeled) {
			label_pad = gog_renderer_pt2r_y (v->renderer, TICK_LABEL_PAD_VERT);
			label_pos.y = (axis->major.tick_out && (!axis->is_discrete || axis->center_on_ticks))
				? major_out + dir * label_pad
				: center + dir * (line_width + label_pad);
			label_pos.h  = area->h - line_width;
			label_pos.w  = -1;
			gog_renderer_measure_text (v->renderer, "0", &txt_size);
			label_spacing = txt_size.w;
		}

		if (axis->tick_nbr > 0) {
			GogAxisMap *map = gog_axis_map_new (axis, area->x, area->w);
			offset = (axis->is_discrete && !axis->center_on_ticks)? -0.5 : 0.0;
			for (i = 0; i < axis->tick_nbr; i++) {

				if (is_line_visible) {
					pos = gog_axis_map_to_canvas (map, axis->ticks[i].position + offset);
					switch (axis->ticks[i].type) {
						case GOG_AXIS_TICK_MAJOR:
							if (draw_major) {
								path[0].x = path[1].x = pos;
								path[0].y = major_out;
								path[1].y = major_in;
								gog_renderer_draw_sharp_path 
									(v->renderer, path, NULL);
							}
							break;

						case GOG_AXIS_TICK_MINOR:
							if (draw_minor) {
								path[0].x = path[1].x = pos;
								path[0].y = minor_out;
								path[1].y = minor_in;
								gog_renderer_draw_sharp_path 
									(v->renderer, path, NULL);
							}
							break;

						default:
							break;
					}
				}
				if (axis->ticks[i].label != NULL) {
					label_pos.x = gog_axis_map_to_canvas (map, axis->ticks[i].position);
					if (fabs (last_label_pos - label_pos.x) > last_label_size + label_spacing) {
						gog_renderer_measure_text (v->renderer, axis->ticks[i].label, &txt_size);
						txt_size.w /= 2.0;
						if (fabs (last_label_pos - label_pos.x) > last_label_size + txt_size.w + label_spacing) {
							gog_renderer_draw_text (v->renderer, axis->ticks[i].label,
										&label_pos, anchor, &label_result);
							last_label_pos = label_pos.x;
							last_label_size = txt_size.w;
						}
					}
				}
			}
			gog_axis_map_free (map);
		}

		break;

	case GOG_AXIS_Y:
		switch (axis->pos) {
			default :
			case GOG_AXIS_AT_LOW:
				anchor = GTK_ANCHOR_E;
				dir = -1.;
				center = area->x;
				break;
			case GOG_AXIS_AT_HIGH:
				anchor = GTK_ANCHOR_W;
				dir = 1.;
				center = area->x + area->w;
				break;
		}

		tick_len = gog_renderer_pt2r_x (v->renderer, axis->major.size_pts);
		major_out = axis->major.tick_out ? center + dir * (line_width + tick_len) : center;
		major_in  = axis->major.tick_in  ? center - dir * (line_width + tick_len) : center;
		tick_len = gog_renderer_pt2r_x (v->renderer, axis->minor.size_pts);
		minor_out = axis->minor.tick_out ? center + dir * (line_width + tick_len) : center;
		minor_in  = axis->minor.tick_in  ? center - dir * (line_width + tick_len) : center;

		if (is_line_visible) {
			path[0].x = path[1].x = center;
			path[0].y = area->y + area->h + line_width;
			path[1].y = area->y - line_width;
			gog_renderer_draw_sharp_path (v->renderer, path, NULL);
		}

		if (axis->major_tick_labeled) {
			label_pad = gog_renderer_pt2r_x (v->renderer, TICK_LABEL_PAD_HORIZ);
			label_pos.x = (axis->major.tick_out && (!axis->is_discrete || axis->center_on_ticks))
				? major_out + dir * label_pad
				: center + dir * (line_width + label_pad);
			label_pos.w  = area->w - line_width;
			label_pos.h  = -1;
		}

		if (axis->tick_nbr > 0) {
			GogAxisMap *map = gog_axis_map_new (axis, area->h + area->y, -area->h);
			offset = (axis->is_discrete && !axis->center_on_ticks)? -0.5 : 0.0;
			for (i = 0; i < axis->tick_nbr; i++) {
				
				if (is_line_visible) {
					pos = gog_axis_map_to_canvas (map, axis->ticks[i].position + offset);
					switch (axis->ticks[i].type) {
						case GOG_AXIS_TICK_MAJOR:
							if (draw_major) {
								path[0].y = path[1].y = pos;
								path[0].x = major_out;
								path[1].x = major_in;
								gog_renderer_draw_sharp_path 
									(v->renderer, path, NULL);
							}
							break;

						case GOG_AXIS_TICK_MINOR:
							if (draw_minor) {
								path[0].y = path[1].y = pos;
								path[0].x = minor_out;
								path[1].x = minor_in;
								gog_renderer_draw_sharp_path 
									(v->renderer, path, NULL);
							}
							break;

						default:
							break;
					}
				}

				if (axis->ticks[i].label != NULL) {
					label_pos.y = gog_axis_map_to_canvas (map, axis->ticks[i].position);
					if (fabs (last_label_pos - label_pos.y) > last_label_size) {
						gog_renderer_measure_text (v->renderer, axis->ticks[i].label, &txt_size);
						txt_size.h /= 2.0;
						if (fabs (last_label_pos - label_pos.y) > last_label_size + txt_size.h) {
							gog_renderer_draw_text (v->renderer, axis->ticks[i].label,
										&label_pos, anchor, &label_result);
							last_label_pos = label_pos.y;
							last_label_size = txt_size.h;
						}
					}
				}
			}
			gog_axis_map_free (map);
		}

		break;

	case GOG_AXIS_CIRCULAR:
		break;

	case GOG_AXIS_RADIAL: {
		double    center_x, center_y, radius;
		unsigned  i, num_radii;
		double    circular_min, circular_max;
		GogAxis  *circular_axis;
		GogChart *chart;
		GSList   *axis_list;

		center_x = area->x + (area->w/2);
		center_y = area->y + (area->h/2);
		radius = v->allocation.h > v->allocation.w 
			? v->allocation.w / 2.0 
			: v->allocation.h / 2.0;

		g_return_if_fail (v->parent != NULL);
		g_return_if_fail (v->parent->model != NULL);
		g_return_if_fail (IS_GOG_CHART(v->parent->model));
		chart = GOG_CHART(v->parent->model);
		axis_list = gog_chart_get_axis (chart, GOG_AXIS_CIRCULAR);
		g_return_if_fail (axis_list != NULL);
		g_return_if_fail (axis_list->data != NULL);
		g_return_if_fail (IS_GOG_AXIS(axis_list->data));
		circular_axis = GOG_AXIS(axis_list->data);
		g_slist_free (axis_list);
		gog_axis_get_bounds (circular_axis, &circular_min, &circular_max);
		num_radii = rint (circular_max);

		for (i = 0; i < num_radii; i++) {
			double angle_rad = i * (2.0 * M_PI / num_radii);
			draw_axis_from_a_to_b (v, axis,
					       center_x, center_y,
					       center_x + radius * sin (angle_rad),
					       center_y - radius * cos (angle_rad),
					       i == 0);
		}
		break;
	}
	default :
		break;
	}

	gog_renderer_pop_style (v->renderer);
}

static void
gog_axis_view_class_init (GogAxisViewClass *gview_klass)
{
	GogViewClass *view_klass    = (GogViewClass *) gview_klass;

	aview_parent_klass = g_type_class_peek_parent (gview_klass);
	view_klass->size_request  = gog_axis_view_size_request;
	view_klass->render	  = gog_axis_view_render;
}

static GSF_CLASS (GogAxisView, gog_axis_view,
		  gog_axis_view_class_init, NULL,
		  GOG_VIEW_TYPE)
