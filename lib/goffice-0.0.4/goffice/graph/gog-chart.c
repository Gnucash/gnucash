/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-chart.c :
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
#include <goffice/graph/gog-chart-impl.h>
#include <goffice/graph/gog-plot-impl.h>
#include <goffice/graph/gog-graph-impl.h>
#include <goffice/graph/gog-style.h>
#include <goffice/graph/gog-view.h>
#include <goffice/graph/gog-axis.h>
#include <goffice/graph/gog-axis-line-impl.h>
#include <goffice/graph/gog-grid.h>
#include <goffice/graph/gog-grid-line.h>
#include <goffice/graph/gog-renderer.h>
#include <goffice/gtk/goffice-gtk.h>
#include <goffice/utils/go-math.h>

#include <gsf/gsf-impl-utils.h>
#include <glib/gi18n.h>
#include <string.h>
#include <math.h>

#include <gtk/gtkspinbutton.h>
#include <gtk/gtktogglebutton.h>

const struct {
	char const *name;
	GogAxisSet const axis_set;
} axis_set_desc[] = {
	{ "none",	GOG_AXIS_SET_NONE},
	{ "x",		GOG_AXIS_SET_X},
	{ "xy", 	GOG_AXIS_SET_XY},
	{ "xyz",	GOG_AXIS_SET_XYZ},
	{ "radar",	GOG_AXIS_SET_RADAR},
	{ "pseudo-3d",	GOG_AXIS_SET_XY_pseudo_3d}
};
	
GogAxisSet
gog_axis_set_from_str (char const *str)
{
	GogAxisSet axis_set = GOG_AXIS_SET_NONE;
	unsigned i;
	gboolean found = FALSE;

	if (str == NULL)
		return GOG_AXIS_SET_NONE;

	for (i = 0; i < G_N_ELEMENTS (axis_set_desc); i++) 
		if (strcmp (axis_set_desc[i].name, str) == 0) {
			axis_set = axis_set_desc[i].axis_set;
			found = TRUE;
			break;
		}
	if (!found) 
		g_warning ("[GogAxisSet::from_str] unknown axis set (%s)", str);
	return axis_set;
}

static void
calc_polygon_parameters (GogViewAllocation const *area, GogChartMapPolarData *data, gboolean fill_area)
{
	double edges = data->th1 - data->th0 + 1.0;
	double width = 2.0 * sin (2.0 * M_PI * go_rint (edges / 4.0) / edges);
	double height = 1.0 - cos (2.0 * M_PI * go_rint (edges / 2.0) / edges);

	data->rx = area->w / width;
	data->ry = area->h / height;

	if (!fill_area) {
		data->rx = MIN (data->rx, data->ry);
		data->ry = data->rx;
	}

	data->cx = area->x + area->w / 2.0;
	data->cy = area->y + data->ry + (area->h - data->ry * height) / 2.0;
}

static void
calc_circle_parameters (GogViewAllocation const *area, GogChartMapPolarData *data, gboolean fill_area)
{
	double x_min, x_max, y_min, y_max;
	
	if (data->th0 >= data->th1) {
		x_min = y_min = -1.0;
		x_max = y_max = 1.0;
	} else {
		double x;
		if (data->th0 > 2.0 * M_PI) {
			x = data->th0 - fmod (data->th0, 2.0 * M_PI);
			data->th0 -= x;
			data->th1 -= x;
		} else if (data->th1 < - 2.0 * M_PI) {
			x = data->th1 - fmod (data->th1, 2.0 * M_PI);
			data->th0 -= x;
			data->th1 -= x;
		}
		if (data->th1 - data->th0 > go_add_epsilon (2 * M_PI)) 
			data->th1 = data->th0 + 
				fmod (data->th1 - data->th0, 2.0 * M_PI);

		x_min = x_max = y_min = y_max = 0;
		x = cos (-data->th0);
		x_min = MIN (x_min, x); x_max = MAX (x_max, x);
		x = sin (-data->th0);
		y_min = MIN (y_min, x); y_max = MAX (y_max, x);
		x = cos (-data->th1);
		x_min = MIN (x_min, x); x_max = MAX (x_max, x);
		x = sin (-data->th1);
		y_min = MIN (y_min, x); y_max = MAX (y_max, x);

		if (0 > data->th0 && 0 < data->th1)
			x_max = 1.0;
		if (M_PI / 2.0 > data->th0 && M_PI / 2.0 < data->th1)
			y_min = -1.0;
		if (M_PI > data->th0 && M_PI < data->th1)
			x_min = -1.0;
		if (3.0 * M_PI / 2.0 > data->th0 && 3.0 * M_PI / 2.0 < data->th1)
			y_max = 1.0;
	}
	data->rx = area->w / (x_max - x_min);
	data->ry = area->h / (y_max - y_min);
	if (!fill_area) {
		data->rx = MIN (data->rx, data->ry);
		data->ry = data->rx;
	}	
	data->cx = -x_min * data->rx + area->x + (area->w - data->rx * (x_max - x_min)) / 2.0;
	data->cy = -y_min * data->ry + area->y + (area->h - data->ry * (y_max - y_min)) / 2.0;
}

static void 
null_map_2D (GogChartMap *map, double x, double y, double *u, double *v)
{
	g_warning ("[GogChartMap::map_2D] not implemented");
}

typedef struct {
	double a, b;
} XMapData;

static void 
x_map_2D_to_view (GogChartMap *map, double x, double y, double *u, double *v)
{
	XMapData *data = map->data;

	*u = gog_axis_map_to_view (map->axis_map[0], x);
	*v = data->a * y + data->b;
}

typedef struct {
	double		a[2][2];
	double		b[2];
} XYMapData;

static void
xy_map_2D_to_view (GogChartMap *map, double x, double y, double *u, double *v)
{
	*u = gog_axis_map_to_view (map->axis_map[0], x);
	*v = gog_axis_map_to_view (map->axis_map[1], y);
}

static void
polar_map_2D_to_view (GogChartMap *map, double x, double y, double *u, double *v)
{
	GogChartMapPolarData *data = (GogChartMapPolarData *) map->data;
	double r = gog_axis_map_to_view (map->axis_map[1], y);
	double t = gog_axis_map_to_view (map->axis_map[0], x);
	
	*u = data->cx + r * data->rx * cos (t);
	*v = data->cy + r * data->ry * sin (t);		
}

GogChartMapPolarData *
gog_chart_map_get_polar_parms (GogChartMap *map)
{
	return (GogChartMapPolarData *) map->data;
}	

GogChartMap *
gog_chart_map_new (GogChart *chart, GogViewAllocation const *area, 
		   GogAxis *axis0, GogAxis *axis1, GogAxis *axis2,
		   gboolean fill_area)
{
	GogChartMap *map;
	GogAxisSet axis_set;

	g_return_val_if_fail (GOG_CHART (chart) != NULL, NULL);

	map = g_new (GogChartMap, 1);

	g_object_ref (chart);
	map->chart = chart;
	map->area = *area;
	map->data = NULL;
	map->is_valid = FALSE;

	axis_set = gog_chart_get_axis_set (chart);
	switch (axis_set) {
		case GOG_AXIS_SET_X:
			{
				XMapData *data = g_new (XMapData, 1);

				map->axis_map[0] = gog_axis_map_new (axis0, map->area.x, map->area.w);
				map->axis_map[1] = map->axis_map[2] = NULL;

				data->b = area->y + area->h;
				data->a = - area->h;

				map->map_2D_to_view = x_map_2D_to_view;
				map->data = data;

				map->is_valid = gog_axis_map_is_valid (map->axis_map [0]);
				break;
			}
		case GOG_AXIS_SET_XY:
		case GOG_AXIS_SET_XY_pseudo_3d:
			{
				map->axis_map[0] = gog_axis_map_new (axis0, map->area.x, map->area.w);
				map->axis_map[1] = gog_axis_map_new (axis1, map->area.y + map->area.h, 
								     -map->area.h);
				map->axis_map[2] = NULL;

				map->data = NULL;
				map->map_2D_to_view = xy_map_2D_to_view;

				map->is_valid = gog_axis_map_is_valid (map->axis_map[0]) &&
					gog_axis_map_is_valid (map->axis_map[1]);
				break;
			}
		case GOG_AXIS_SET_RADAR:
			{
				double minimum, maximum;
				GogChartMapPolarData *data = g_new (GogChartMapPolarData, 1);
				
				map->axis_map[0] = gog_axis_map_new (axis0, 0.0, 1.0);
				gog_axis_map_get_bounds (map->axis_map[0], &minimum, &maximum);
				if (gog_axis_is_discrete (axis0)) {
					data->th0 = go_rint (minimum);
					data->th1 = go_rint (maximum);
					calc_polygon_parameters (area, data, fill_area);
					gog_axis_map_free (map->axis_map[0]);
					map->axis_map[0] = gog_axis_map_new (axis0, 
						- M_PI / 2.0,
						2.0 * M_PI * (maximum - minimum) / (maximum - minimum + 1));
				} else {
					minimum *= 2.0 * M_PI / 360.0;
					maximum *= 2.0 * M_PI / 360.0;
					data->th0 = minimum;
					data->th1 = maximum;
					calc_circle_parameters (area, data, fill_area);
					gog_axis_map_free (map->axis_map[0]);
					map->axis_map[0] = gog_axis_map_new (axis0, -minimum,
									     minimum - maximum);
				}
				map->axis_map[1] = gog_axis_map_new (axis1, 0.0, 1.0);
				map->axis_map[2] = NULL;

				map->data = data;
				map->map_2D_to_view = polar_map_2D_to_view;

				map->is_valid = gog_axis_map_is_valid (map->axis_map[0]) &&
					gog_axis_map_is_valid (map->axis_map[1]);
				break;
			}
		case GOG_AXIS_SET_XYZ:
		case GOG_AXIS_SET_ALL:
		case GOG_AXIS_SET_NONE:
		case GOG_AXIS_SET_UNKNOWN:
			g_warning ("[Chart::map_new] not implemented for this axis set (%i)",
				   axis_set);
			map->map_2D_to_view = null_map_2D;
			break;
	}

	return map;
}

void
gog_chart_map_2D_to_view (GogChartMap *map, double x, double y, double *u, double *v)
{
	return (map->map_2D_to_view) (map, x, y, u, v);
}

GogAxisMap *
gog_chart_map_get_axis_map (GogChartMap *map, unsigned i)
{
	g_return_val_if_fail (map != NULL, NULL);
	g_return_val_if_fail (i < 3, NULL);

	return map->axis_map[i];
}

gboolean
gog_chart_map_is_valid (GogChartMap *map)
{
	g_return_val_if_fail (map != NULL, FALSE);

	return map->is_valid;
}

void
gog_chart_map_free (GogChartMap *map)
{
	int i;

	g_return_if_fail (map != NULL);

	for (i = 0; i < 3; i++)
		if (map->axis_map[i] != NULL)
			gog_axis_map_free (map->axis_map[i]);

	g_free (map->data);
	g_object_unref (map->chart);
	g_free (map);
}

enum {
	CHART_PROP_0,
	CHART_PROP_CARDINALITY_VALID,
	CHART_PROP_PLOT_AREA,
	CHART_PROP_PLOT_AREA_IS_MANUAL
};

static GType gog_chart_view_get_type (void);
static GObjectClass *chart_parent_klass;

static void
gog_chart_update (GogObject *obj)
{
	GogChart *chart = GOG_CHART (obj);
	unsigned full = chart->full_cardinality;
	unsigned visible = chart->visible_cardinality;

	gog_chart_get_cardinality (chart, NULL, NULL);

	if (full != chart->full_cardinality ||
	    visible != chart->visible_cardinality)
		g_object_notify (G_OBJECT (chart), "cardinality-valid");
}

static void
gog_chart_finalize (GObject *obj)
{
	GogChart *chart = GOG_CHART (obj);

	/* on exit the role remove routines are not called */
	g_slist_free (chart->plots);
	g_slist_free (chart->axes);

	(chart_parent_klass->finalize) (obj);
}

static void
gog_chart_set_property (GObject *obj, guint param_id,
			 GValue const *value, GParamSpec *pspec)
{
	GogChart *chart = GOG_CHART (obj);
	char **str_doubles;
	char const *str;

	switch (param_id) {
	case CHART_PROP_PLOT_AREA:
		str = g_value_get_string (value);
		str_doubles = g_strsplit (str, " ", 4);
		if (g_strv_length (str_doubles) != 4) {
			g_strfreev (str_doubles);
			break;
		}
		chart->plot_area.x = g_ascii_strtod (str_doubles[0], NULL);
		chart->plot_area.y = g_ascii_strtod (str_doubles[1], NULL);
		chart->plot_area.w = g_ascii_strtod (str_doubles[2], NULL);
		chart->plot_area.h = g_ascii_strtod (str_doubles[3], NULL);
		g_strfreev (str_doubles);
		break;
	case CHART_PROP_PLOT_AREA_IS_MANUAL:
		chart->is_plot_area_manual = g_value_get_boolean (value);
		break;
	default: G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, param_id, pspec);
		 return; /* NOTE : RETURN */
	}
}

static void
gog_chart_get_property (GObject *obj, guint param_id,
			GValue *value, GParamSpec *pspec)
{
	GogChart *chart = GOG_CHART (obj);
	GString *string;
	char buffer[G_ASCII_DTOSTR_BUF_SIZE];

	switch (param_id) {
	case CHART_PROP_CARDINALITY_VALID:
		g_value_set_boolean (value, chart->cardinality_valid);
		break;
	case CHART_PROP_PLOT_AREA:
		string = g_string_new ("");
		g_string_append (string, g_ascii_dtostr (buffer, sizeof (buffer), chart->plot_area.x));
		g_string_append_c (string, ' ');
		g_string_append (string, g_ascii_dtostr (buffer, sizeof (buffer), chart->plot_area.y));
		g_string_append_c (string, ' ');
		g_string_append (string, g_ascii_dtostr (buffer, sizeof (buffer), chart->plot_area.w));
		g_string_append_c (string, ' ');
		g_string_append (string, g_ascii_dtostr (buffer, sizeof (buffer), chart->plot_area.h));
		g_value_set_string (value, string->str);
		g_string_free (string, TRUE);
		break;
	case CHART_PROP_PLOT_AREA_IS_MANUAL:
		g_value_set_boolean (value, chart->is_plot_area_manual);
		break;

	default: G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, param_id, pspec);
		 break;
	}
}

typedef struct {
	GtkWidget	*x_spin, *y_spin, *w_spin, *h_spin;
	gulong		w_spin_signal, h_spin_signal;
	GtkWidget	*manual_toggle;
	GogChart 	*chart;
	GladeXML	*gui;
} ChartPrefState;

static void
chart_pref_state_free (ChartPrefState *state) 
{
	g_object_unref (state->chart);
	g_object_unref (state->gui);
}

static void
cb_plot_area_changed (GtkWidget *spin, ChartPrefState *state)
{
	GogViewAllocation pos;
	double value;
	double max;

       	value = gtk_spin_button_get_value (GTK_SPIN_BUTTON (spin)) / 100.0;

       	gog_chart_get_plot_area (state->chart, &pos);
	if (spin == state->x_spin) {
		pos.x = value;
		max = 1.0 - pos.x;
		g_signal_handler_block (state->w_spin, state->w_spin_signal);
		gtk_spin_button_set_range (GTK_SPIN_BUTTON (state->w_spin), 0.0, max * 100.0);
		if (pos.w > max) pos.w = max;
		gtk_spin_button_set_value (GTK_SPIN_BUTTON (state->w_spin), pos.w * 100.0);
		g_signal_handler_unblock (state->w_spin, state->w_spin_signal);
	}
	else if (spin == state->y_spin) {
		pos.y = value;
		max = 1.0 - pos.y;
		g_signal_handler_block (state->h_spin, state->h_spin_signal);
		gtk_spin_button_set_range (GTK_SPIN_BUTTON (state->h_spin), 0.0, max * 100.0);
		if (pos.h > max) pos.h = max;
		gtk_spin_button_set_value (GTK_SPIN_BUTTON (state->h_spin), pos.w * 100.0);
		g_signal_handler_unblock (state->h_spin, state->h_spin_signal);
	}
	else if (spin == state->w_spin) {
		pos.w = value;
	}
	else if (spin == state->h_spin) {
		pos.h = value;
	}
	gog_chart_set_plot_area (state->chart, &pos);
	gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (state->manual_toggle), TRUE);
}

static void
cb_manual_toggle_changed (GtkToggleButton *button, ChartPrefState *state)
{
	gog_chart_set_plot_area (state->chart, 
		gtk_toggle_button_get_active (button) ?
		&state->chart->plot_area : NULL);
}

static void
gog_chart_populate_editor (GogObject *gobj, 
			   GogEditor *editor, 
			   G_GNUC_UNUSED GogDataAllocator *dalloc, 
			   GOCmdContext *cc)
{
	GogChart *chart = GOG_CHART (gobj);
	GtkWidget *w;
	GladeXML *gui;
	ChartPrefState *state;
	static guint chart_pref_page = 0;
	
	gui = go_libglade_new ("gog-chart-prefs.glade", "gog_chart_prefs", NULL, cc);
	if (gui == NULL)
		return;

	state = g_new (ChartPrefState, 1);
	state->chart = chart;
	state->gui = gui;
	g_object_ref (G_OBJECT (gobj));

	state->x_spin = glade_xml_get_widget (gui, "x_spin");
	gtk_spin_button_set_value (GTK_SPIN_BUTTON (state->x_spin), 
				   chart->plot_area.x * 100.0); 
	g_signal_connect (G_OBJECT (state->x_spin), "value-changed", 
			  G_CALLBACK (cb_plot_area_changed), state);

	state->y_spin = glade_xml_get_widget (gui, "y_spin");
	gtk_spin_button_set_value (GTK_SPIN_BUTTON (state->y_spin), 
				   chart->plot_area.y * 100.0); 
	g_signal_connect (G_OBJECT (state->y_spin), "value-changed", 
			  G_CALLBACK (cb_plot_area_changed), state);

	state->w_spin = glade_xml_get_widget (gui, "w_spin");
	gtk_spin_button_set_range (GTK_SPIN_BUTTON (state->w_spin), 
				   0.0, (1.0 - chart->plot_area.x) * 100.0);
	gtk_spin_button_set_value (GTK_SPIN_BUTTON (state->w_spin), 
				   100.0 * chart->plot_area.w); 
	state->w_spin_signal = g_signal_connect (G_OBJECT (state->w_spin), "value-changed", 
						 G_CALLBACK (cb_plot_area_changed), state);

	state->h_spin = glade_xml_get_widget (gui, "h_spin");
	gtk_spin_button_set_range (GTK_SPIN_BUTTON (state->h_spin), 
				   0.0, (1.0 - chart->plot_area.y) * 100.0);
	gtk_spin_button_set_value (GTK_SPIN_BUTTON (state->h_spin), 
				   100.0 * chart->plot_area.h); 
	state->h_spin_signal = g_signal_connect (G_OBJECT (state->h_spin), "value-changed", 
						 G_CALLBACK (cb_plot_area_changed), state);

	state->manual_toggle = glade_xml_get_widget (gui, "manual_toggle");
	gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (state->manual_toggle), 
				      chart->is_plot_area_manual);
	g_signal_connect (G_OBJECT (state->manual_toggle), "toggled", 
			  G_CALLBACK (cb_manual_toggle_changed), state);

	(GOG_OBJECT_CLASS(chart_parent_klass)->populate_editor) (gobj, editor, dalloc, cc);

	w = glade_xml_get_widget (gui, "gog_chart_prefs");
	g_object_set_data_full (G_OBJECT (w), "state", state, 
				(GDestroyNotify) chart_pref_state_free);  
	gog_editor_add_page (editor, w, _("Plot area"));
	
	gog_editor_set_store_page (editor, &chart_pref_page);
}

static void
gog_chart_children_reordered (GogObject *obj)
{
	GSList *ptr, *accum = NULL;
	GogChart *chart = GOG_CHART (obj);

	for (ptr = obj->children; ptr != NULL ; ptr = ptr->next)
		if (IS_GOG_PLOT (ptr->data))
			accum = g_slist_prepend (accum, ptr->data);
	g_slist_free (chart->plots);
	chart->plots = g_slist_reverse (accum);

	gog_chart_request_cardinality_update (chart);
}

static void
role_plot_post_add (GogObject *parent, GogObject *plot)
{
	GogChart *chart = GOG_CHART (parent);
	gboolean ok = TRUE;

	/* APPEND to keep order, there won't be that many */
	chart->plots = g_slist_append (chart->plots, plot);
	gog_chart_request_cardinality_update (chart);
	
	if (chart->plots->next == NULL)
		ok = gog_chart_axis_set_assign (chart,
			gog_plot_axis_set_pref (GOG_PLOT (plot)));
	ok |= gog_plot_axis_set_assign (GOG_PLOT (plot),
		chart->axis_set);

	/* a quick post condition to keep us on our toes */
	g_return_if_fail (ok);
}

static void
role_plot_pre_remove (GogObject *parent, GogObject *plot)
{
	GogChart *chart = GOG_CHART (parent);
	gog_plot_axis_clear (GOG_PLOT (plot), GOG_AXIS_SET_ALL);
	chart->plots = g_slist_remove (chart->plots, plot);
	gog_chart_request_cardinality_update (chart);
	
	if (chart->plots == NULL)
		gog_chart_axis_set_assign (chart, GOG_AXIS_SET_UNKNOWN);

	if (chart->grid != NULL && 
	    chart->axis_set != GOG_AXIS_SET_XY &&
	    chart->axis_set != GOG_AXIS_SET_X && 
	    chart->axis_set != GOG_AXIS_SET_XY_pseudo_3d &&
	    chart->axis_set != GOG_AXIS_SET_RADAR) {
		GogObject *grid = chart->grid; /* clear_parent clears ::grid */
		gog_object_clear_parent (GOG_OBJECT (grid));
		g_object_unref (grid);
	}
}

static gboolean
role_grid_can_add (GogObject const *parent)
{
	GogChart const *chart = GOG_CHART (parent);
	return chart->grid == NULL &&
		(chart->axis_set == GOG_AXIS_SET_XY ||
		 chart->axis_set == GOG_AXIS_SET_X ||
		 chart->axis_set == GOG_AXIS_SET_XY_pseudo_3d ||
		 chart->axis_set == GOG_AXIS_SET_RADAR);
}

static void
role_grid_post_add (GogObject *parent, GogObject *child)
{
	GogChart *chart = GOG_CHART (parent);
	g_return_if_fail (chart->grid == NULL);
	chart->grid = child;
}

static void
role_grid_pre_remove (GogObject *parent, GogObject *grid)
{
	GogChart *chart = GOG_CHART (parent);
	g_return_if_fail (chart->grid == grid);
	chart->grid = NULL;
}

static gboolean
axis_can_add (GogObject const *parent, GogAxisType t)
{
	GogChart *chart = GOG_CHART (parent);
	if (chart->axis_set == GOG_AXIS_SET_UNKNOWN)
		return FALSE;
	return (chart->axis_set & (1 << t)) != 0;
}

static gboolean
axis_can_remove (GogObject const *child)
{
	return NULL == gog_axis_contributors (GOG_AXIS (child));
}

static void
axis_post_add (GogObject *axis, GogAxisType t)
{
	GogChart *chart = GOG_CHART (axis->parent);
	g_object_set (G_OBJECT (axis), "type", (int)t, NULL);
	chart->axes = g_slist_prepend (chart->axes, axis);

	gog_axis_base_set_position (GOG_AXIS_BASE (axis), GOG_AXIS_AUTO);
}

static void
axis_pre_remove (GogObject *parent, GogObject *axis)
{
	GogChart *chart = GOG_CHART (parent);
	gog_axis_clear_contributors (GOG_AXIS (axis));
	chart->axes = g_slist_remove (chart->axes, axis);
}

static gboolean x_axis_can_add (GogObject const *parent) { return axis_can_add (parent, GOG_AXIS_X); }
static void x_axis_post_add    (GogObject *parent, GogObject *child)  { axis_post_add   (child, GOG_AXIS_X); }
static gboolean y_axis_can_add (GogObject const *parent) { return axis_can_add (parent, GOG_AXIS_Y); }
static void y_axis_post_add    (GogObject *parent, GogObject *child)  { axis_post_add   (child, GOG_AXIS_Y); }
static gboolean z_axis_can_add (GogObject const *parent) { return axis_can_add (parent, GOG_AXIS_Z); }
static void z_axis_post_add    (GogObject *parent, GogObject *child)  { axis_post_add   (child, GOG_AXIS_Z); }
static gboolean circular_axis_can_add (GogObject const *parent) { return axis_can_add (parent, GOG_AXIS_CIRCULAR); }
static void circular_axis_post_add    (GogObject *parent, GogObject *child)  { axis_post_add   (child, GOG_AXIS_CIRCULAR); }
static gboolean radial_axis_can_add (GogObject const *parent) { return axis_can_add (parent, GOG_AXIS_RADIAL); }
static void radial_axis_post_add    (GogObject *parent, GogObject *child)  { axis_post_add   (child, GOG_AXIS_RADIAL); }
static gboolean pseudo_3d_axis_can_add (GogObject const *parent) { return axis_can_add (parent, GOG_AXIS_PSEUDO_3D); }
static void pseudo_3d_axis_post_add    (GogObject *parent, GogObject *child)  { axis_post_add   (child, GOG_AXIS_PSEUDO_3D); }

static GogObjectRole const roles[] = {
	{ N_("Grid"), "GogGrid",	0,
	  GOG_POSITION_SPECIAL, GOG_POSITION_SPECIAL, GOG_OBJECT_NAME_BY_ROLE,
	  role_grid_can_add, NULL, NULL, role_grid_post_add, role_grid_pre_remove, NULL, { -1 } },
	{ N_("X-Axis"), "GogAxis",	1,
	  GOG_POSITION_PADDING, GOG_POSITION_PADDING, GOG_OBJECT_NAME_BY_ROLE,
	  x_axis_can_add, axis_can_remove, NULL, x_axis_post_add, axis_pre_remove, NULL,
	  { GOG_AXIS_X } },
	{ N_("Y-Axis"), "GogAxis",	2,
	  GOG_POSITION_PADDING, GOG_POSITION_PADDING, GOG_OBJECT_NAME_BY_ROLE,
	  y_axis_can_add, axis_can_remove, NULL, y_axis_post_add, axis_pre_remove, NULL,
	  { GOG_AXIS_Y } },
	{ N_("Z-Axis"), "GogAxis",	3,
	  GOG_POSITION_PADDING, GOG_POSITION_PADDING, GOG_OBJECT_NAME_BY_ROLE,
	  z_axis_can_add, axis_can_remove, NULL, z_axis_post_add, axis_pre_remove, NULL,
	  { GOG_AXIS_Z } },
	{ N_("Circular-Axis"), "GogAxis", 1,
	  GOG_POSITION_PADDING, GOG_POSITION_PADDING, GOG_OBJECT_NAME_BY_ROLE,
	  circular_axis_can_add, axis_can_remove, NULL, circular_axis_post_add, axis_pre_remove, NULL,
	  { GOG_AXIS_CIRCULAR } },
	{ N_("Radial-Axis"), "GogAxis",	2,
	  GOG_POSITION_PADDING, GOG_POSITION_PADDING, GOG_OBJECT_NAME_BY_ROLE,
	  radial_axis_can_add, axis_can_remove, NULL, radial_axis_post_add, axis_pre_remove, NULL,
	  { GOG_AXIS_RADIAL } },
	{ N_("Pseudo-3D-Axis"), "GogAxis", 3,
	  GOG_POSITION_SPECIAL, GOG_POSITION_SPECIAL, GOG_OBJECT_NAME_BY_ROLE,
	  pseudo_3d_axis_can_add, axis_can_remove, NULL, pseudo_3d_axis_post_add, axis_pre_remove, NULL,
	  { GOG_AXIS_PSEUDO_3D } },
	{ N_("Plot"), "GogPlot",	4,	/* keep the axis before the plots */
	  GOG_POSITION_SPECIAL, GOG_POSITION_SPECIAL, GOG_OBJECT_NAME_BY_TYPE,
	  NULL, NULL, NULL, role_plot_post_add, role_plot_pre_remove, NULL, { -1 } },
	{ N_("Title"), "GogLabel",	10,
	  GOG_POSITION_COMPASS|GOG_POSITION_ANY_MANUAL, 
	  GOG_POSITION_N|GOG_POSITION_ALIGN_CENTER, 
	  GOG_OBJECT_NAME_BY_ROLE,
	  NULL, NULL, NULL, NULL, NULL, NULL, { -1 } },
	{ N_("Legend"), "GogLegend",	11,
	  GOG_POSITION_COMPASS|GOG_POSITION_ANY_MANUAL, 
	  GOG_POSITION_E|GOG_POSITION_ALIGN_CENTER, 
	  GOG_OBJECT_NAME_BY_ROLE,
	  NULL, NULL, NULL, NULL, NULL, NULL, { -1 } }
};

static void
gog_chart_class_init (GogObjectClass *gog_klass)
{
	GObjectClass *gobject_klass = (GObjectClass *)gog_klass;

	chart_parent_klass = g_type_class_peek_parent (gog_klass);
	gobject_klass->finalize = gog_chart_finalize;
	gobject_klass->set_property = gog_chart_set_property;
	gobject_klass->get_property = gog_chart_get_property;

	gog_klass->populate_editor = gog_chart_populate_editor;

	gog_klass->can_manual_size = TRUE;

	g_object_class_install_property (gobject_klass, CHART_PROP_CARDINALITY_VALID,
		g_param_spec_boolean ("cardinality-valid", "cardinality-valid",
				      "Is the charts cardinality currently vaid",
				      FALSE, G_PARAM_READABLE));
	g_object_class_install_property (gobject_klass, CHART_PROP_PLOT_AREA,
		g_param_spec_string ("plot-area", "Plot area",
				     "Position and size of plot area, in percentage of chart size",
				     "0 0 1 1", G_PARAM_READWRITE|GOG_PARAM_PERSISTENT));
	g_object_class_install_property (gobject_klass, CHART_PROP_PLOT_AREA_IS_MANUAL,
		g_param_spec_boolean ("is-plot-area-manual", "Is plot area manual", 
				      "Is plot area manual",
				      FALSE, G_PARAM_READWRITE|GOG_PARAM_PERSISTENT));

	gog_klass->view_type = gog_chart_view_get_type ();
	gog_klass->update    = gog_chart_update;
	gog_klass->children_reordered = gog_chart_children_reordered;
	gog_object_register_roles (gog_klass, roles, G_N_ELEMENTS (roles));
}

static void
gog_chart_init (GogChart *chart)
{
	chart->x     = 0;
	chart->y     = 0;
	chart->cols  = 0;
	chart->rows  = 0;

	/* start as true so that we can queue an update when it changes */
	chart->cardinality_valid = TRUE;
	chart->axis_set = GOG_AXIS_SET_UNKNOWN;

	chart->is_plot_area_manual = FALSE;
	chart->plot_area.x =
	chart->plot_area.y = 0.0;
	chart->plot_area.w = 
	chart->plot_area.h = 1.0;
}

GSF_CLASS (GogChart, gog_chart,
	   gog_chart_class_init, gog_chart_init,
	   GOG_OUTLINED_OBJECT_TYPE)

/**
 * gog_chart_get_position :
 * @chart : const #GogChart
 * @x :
 * @y :
 * @cols :
 * @rows :
 *
 * Returns TRUE if the chart has been positioned.
 **/
gboolean
gog_chart_get_position (GogChart const *chart,
			unsigned *x, unsigned *y, unsigned *cols, unsigned *rows)
{
	g_return_val_if_fail (GOG_CHART (chart), FALSE);

	if (chart->cols <= 0 || chart->rows <= 0)
		return FALSE;

	if (x != NULL)	  *x	= chart->x;
	if (y != NULL)	  *y	= chart->y;
	if (cols != NULL) *cols	= chart->cols;
	if (rows != NULL) *rows	= chart->rows;

	return TRUE;
}

/**
 * gog_chart_set_position :
 * @chart : #GogChart
 * @x :
 * @y :
 * @cols :
 * @rows :
 *
 **/
void
gog_chart_set_position (GogChart *chart,
			unsigned x, unsigned y, unsigned cols, unsigned rows)
{
	g_return_if_fail (GOG_CHART (chart) != NULL);

	if (chart->x == x && chart->y == y &&
	    chart->cols == cols && chart->rows == rows)
		return;

	chart->x = x;
	chart->y = y;
	chart->cols = cols;
	chart->rows = rows;

	gog_graph_validate_chart_layout (GOG_GRAPH (GOG_OBJECT (chart)->parent));
	gog_object_emit_changed (GOG_OBJECT (chart), TRUE);
}

/**
 * gog_chart_get_plot_area :
 * @chart : #GogChart
 * @plot_area  : #GogViewAllocation
 *
 * Stores plot area in plot_area, in fraction of chart size, and returns
 * TRUE if plot area position is manual.
 **/
gboolean
gog_chart_get_plot_area (GogChart *chart, GogViewAllocation *plot_area)
{
	if (plot_area != NULL)
		*plot_area = chart->plot_area;
	
	return chart->is_plot_area_manual;
}

/**
 * gog_chart_set_plot_area :
 * @chart : #GogChart
 * @plot_area  : #GogViewAllocation
 *
 * If plot_area != NULL, sets plot area size and location, in fraction
 * of chart size, and sets GogChart::is_plot_area_manual flag to TRUE.
 * If plot_area == NULL, sets GogChart::is_plot_area_manual to FALSE.
 **/
void
gog_chart_set_plot_area (GogChart *chart, GogViewAllocation const *plot_area)
{
	if (plot_area == NULL) {
		chart->is_plot_area_manual = FALSE;
	} else {
		chart->plot_area = *plot_area;
		chart->is_plot_area_manual = TRUE;
	}
	gog_object_emit_changed (GOG_OBJECT (chart), TRUE);
}

/* FIXME: function description here */
void
gog_chart_get_cardinality (GogChart *chart, unsigned *full, unsigned *visible)
{
	GSList *ptr;
	unsigned tmp_full, tmp_visible;

	g_return_if_fail (GOG_CHART (chart) != NULL);

	if (!chart->cardinality_valid) {
		chart->cardinality_valid = TRUE;
		chart->full_cardinality = chart->visible_cardinality = 0;
		for (ptr = chart->plots ; ptr != NULL ; ptr = ptr->next) {
			gog_plot_get_cardinality (ptr->data, &tmp_full, &tmp_visible);
			chart->full_cardinality += tmp_full;
			chart->visible_cardinality += tmp_visible;
		}
	}

	if (full != NULL)
		*full = chart->full_cardinality;
	if (visible != NULL)
		*visible = chart->visible_cardinality;
}

void
gog_chart_request_cardinality_update (GogChart *chart)
{
	g_return_if_fail (GOG_CHART (chart) != NULL);
	
	if (chart->cardinality_valid) {
		chart->cardinality_valid = FALSE;
		gog_object_request_update (GOG_OBJECT (chart));
	}
}

void
gog_chart_foreach_elem (GogChart *chart, gboolean only_visible,
			GogEnumFunc handler, gpointer data)
{
	GSList *ptr;

	g_return_if_fail (GOG_CHART (chart) != NULL);
	g_return_if_fail (chart->cardinality_valid);

	for (ptr = chart->plots ; ptr != NULL ; ptr = ptr->next)
		gog_plot_foreach_elem (ptr->data, only_visible, handler, data);
}

GSList *
gog_chart_get_plots (GogChart const *chart)
{
	g_return_val_if_fail (GOG_CHART (chart) != NULL, NULL);
	return chart->plots;
}

GogAxisSet
gog_chart_get_axis_set (GogChart const *chart)
{
	g_return_val_if_fail (GOG_CHART (chart) != NULL, GOG_AXIS_SET_UNKNOWN);
	return chart->axis_set;
}

gboolean
gog_chart_axis_set_is_valid (GogChart const *chart, GogAxisSet type)
{
	GSList *ptr;

	g_return_val_if_fail (GOG_CHART (chart) != NULL, FALSE);

	for (ptr = chart->plots ; ptr != NULL ; ptr = ptr->next)
		if (!gog_plot_axis_set_is_valid (ptr->data, type))
			return FALSE;
	return TRUE;
}

static void
gog_chart_add_axis (GogChart *chart, GogAxisType type)
{
	unsigned i = G_N_ELEMENTS (roles);
	while (i-- > 0)
		if (roles[i].user.i == (int)type) {
			gog_object_add_by_role (GOG_OBJECT (chart), roles + i, NULL);
			return;
		}
	g_warning ("unknown axis type %d", type);
}

gboolean
gog_chart_axis_set_assign (GogChart *chart, GogAxisSet axis_set)
{
	GogAxis *axis;
	GSList  *ptr;
	GogAxisType type;

	g_return_val_if_fail (GOG_CHART (chart) != NULL, FALSE);

	if (chart->axis_set == axis_set)
		return TRUE;
	chart->axis_set = axis_set;

	if (axis_set == GOG_AXIS_SET_UNKNOWN)
		return TRUE;

	/* Add at least 1 instance of any required axis */
	for (type = 0 ; type < GOG_AXIS_TYPES ; type++)
		if ((axis_set & (1 << type))) {
			GSList *tmp = gog_chart_get_axes (chart, type);
			if (tmp == NULL)
				gog_chart_add_axis (chart, type);
			else
				g_slist_free (tmp);
		}

	/* link the plots */
	for (ptr = chart->plots ; ptr != NULL ; ptr = ptr->next)
		if (!gog_plot_axis_set_assign (ptr->data, axis_set))
			return FALSE;

	/* remove any existing axis that do not fit this scheme */
	for (ptr = GOG_OBJECT (chart)->children ; ptr != NULL ; ) {
		axis = ptr->data;
		ptr = ptr->next; /* list may change under us */
		if (IS_GOG_AXIS (axis)) {
			type = -1;
			g_object_get (G_OBJECT (axis), "type", &type, NULL);
			if (type < 0 || type >= GOG_AXIS_TYPES) {
				g_warning ("Invalid axis");
				continue;
			}

			if (0 == (axis_set & (1 << type))) {
				gog_object_clear_parent (GOG_OBJECT (axis));
				g_object_unref (axis);
			}
		}
	}

	return TRUE;
}

/**
 * gog_chart_get_axes :
 * @chart : #GogChart
 * @target  : #GogAxisType
 *
 * Return a list which the caller must free of all axis of type @target
 * associated with @chart.
 **/
GSList *
gog_chart_get_axes (GogChart const *chart, GogAxisType target)
{
	GSList *ptr, *res = NULL;
	GogAxis *axis;
	int type;

	g_return_val_if_fail (GOG_CHART (chart) != NULL, NULL);

	for (ptr = GOG_OBJECT (chart)->children ; ptr != NULL ; ptr = ptr->next) {
		axis = ptr->data;
		if (IS_GOG_AXIS (axis)) {
			type = -1;
			g_object_get (G_OBJECT (axis), "type", &type, NULL);
			if (type < 0 || type >= GOG_AXIS_TYPES) {
				g_warning ("Invalid axis");
				continue;
			}
			if (type == target)
				res = g_slist_prepend (res, axis);
		}
	}

	return res;
}

/**
 * gog_chart_get_grid :
 * @chart : #GogChart
 *
 * Returns the grid associated with @chart if one exists
 * otherwise NULL.
 **/
GogGrid  *
gog_chart_get_grid (GogChart const *chart)
{
	g_return_val_if_fail (GOG_CHART (chart) != NULL, NULL);
	return GOG_GRID (chart->grid);
}

/*********************************************************************/

typedef struct {
	GogOutlinedView base;

	GogViewAllocation	plot_area;
} GogChartView;
typedef GogOutlinedViewClass	GogChartViewClass;

#define GOG_CHART_VIEW_TYPE	(gog_chart_view_get_type ())
#define GOG_CHART_VIEW(o)	(G_TYPE_CHECK_INSTANCE_CAST ((o), GOG_CHART_VIEW_TYPE, GogChartView))
#define IS_GOG_CHART_VIEW(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOG_CHART_VIEW_TYPE))

static GogViewClass *cview_parent_klass;

GogViewAllocation const *
gog_chart_view_get_plot_area (GogView const *view)
{
	GogChartView *chart_view = GOG_CHART_VIEW (view);

	g_return_val_if_fail ((GOG_CHART_VIEW (view) != NULL), NULL);

	return & (chart_view->plot_area);
}

static void
gog_chart_view_size_allocate (GogView *view, GogViewAllocation const *bbox)
{
	GSList *ptr;
	GogView *child;
	GogChartView *chart_view = GOG_CHART_VIEW (view);
	GogViewAllocation tmp, *plot_area = &chart_view->plot_area;
	GogViewPadding padding;
	GogChart *chart = GOG_CHART (gog_view_get_model (view));

	(cview_parent_klass->size_allocate) (view, bbox);

	if (chart->is_plot_area_manual) {
		plot_area->x = bbox->x + chart->plot_area.x * bbox->w;
		plot_area->y = bbox->y + chart->plot_area.y * bbox->h;
		plot_area->w = chart->plot_area.w * bbox->w;
		plot_area->h = chart->plot_area.h * bbox->h;
	} else
		*plot_area = view->residual;

	tmp = *plot_area;
	gog_view_padding_request (view, plot_area, &padding);
	
	if (!chart->is_plot_area_manual) {
		plot_area->x += padding.wl;
		plot_area->w -= padding.wl + padding.wr;
		plot_area->y += padding.ht;
		plot_area->h -= padding.ht + padding.hb;
	} else {
		tmp.x -= padding.wl;
		tmp.w += padding.wl + padding.wr;
		tmp.y -= padding.ht;
		tmp.h += padding.ht + padding.hb;
	}

	for (ptr = view->children; ptr != NULL ; ptr = ptr->next) {
		child = ptr->data;
		if (GOG_POSITION_IS_PADDING (child->model->position)) {
			gog_view_size_allocate (child, &tmp);
		}
	}

	/* by default, overlay all GOG_POSITION_SPECIAL children in residual */
	for (ptr = view->children; ptr != NULL ; ptr = ptr->next) {
		child = ptr->data;
		if (GOG_POSITION_IS_SPECIAL (child->model->position))
			gog_view_size_allocate (child, plot_area);
	}
}

static void
gog_chart_view_init (GogChartView *cview)
{
}

static void
grid_line_render (GSList *start_ptr, GogViewAllocation const *bbox) 
{
	GSList *ptr, *child_ptr;
	GogView *child_view, *axis_child_view;

	/* Render minor lines first */
	for (ptr = start_ptr; ptr != NULL; ptr = ptr->next) {
		child_view = ptr->data;
		if (IS_GOG_AXIS (child_view->model)) {
			for (child_ptr = child_view->children; child_ptr != NULL; child_ptr = child_ptr->next) {
				axis_child_view = child_ptr->data;
				if (IS_GOG_GRID_LINE (axis_child_view->model) &&
				    gog_grid_line_is_minor (GOG_GRID_LINE (axis_child_view->model)))
					gog_view_render (axis_child_view, bbox);
			}
		}
	}	    
	/* then render major lines */
	for (ptr = start_ptr; ptr != NULL; ptr = ptr->next) {
		child_view = ptr->data;
		if (IS_GOG_AXIS (child_view->model)) {
			for (child_ptr = child_view->children; child_ptr != NULL; child_ptr = child_ptr->next) {
				axis_child_view = child_ptr->data;
				if (IS_GOG_GRID_LINE (axis_child_view->model) &&
				    !gog_grid_line_is_minor (GOG_GRID_LINE (axis_child_view->model)))
					gog_view_render (axis_child_view, bbox);
			}
		}
	}	    
}

static void
plot_render (GogView *view, GogViewAllocation const *bbox) 
{
	GSList *ptr;
	GogView *child_view;

	/* Render some plots before axes */
	for (ptr = view->children ; ptr != NULL ; ptr = ptr->next) {
		child_view = ptr->data;
		if (IS_GOG_PLOT (child_view->model) && 
		    GOG_PLOT (child_view->model)->render_before_axes)
			gog_view_render	(ptr->data, bbox);
	}
}

static void
gog_chart_view_render (GogView *view, GogViewAllocation const *bbox)
{
	GSList *ptr;
	GogView *child_view;
	gboolean grid_line_rendered = FALSE;

	cview_parent_klass->render (view, bbox);

	/* KLUDGE: render grid lines before axis */
	for (ptr = view->children ; ptr != NULL ; ptr = ptr->next) {
		child_view = ptr->data;
		if (!grid_line_rendered && IS_GOG_AXIS (child_view->model)) {
			grid_line_render (ptr, bbox);
			plot_render (view, bbox);
			grid_line_rendered = TRUE;
		}
		if (IS_GOG_PLOT (child_view->model)) {
		    if (!GOG_PLOT (child_view->model)->render_before_axes)
			gog_view_render	(ptr->data, bbox);
		} else
			gog_view_render	(ptr->data, bbox);
	}

}

static void
gog_chart_view_class_init (GogChartViewClass *gview_klass)
{
	GogViewClass *view_klass = (GogViewClass *) gview_klass;
	GogOutlinedViewClass *oview_klass = (GogOutlinedViewClass *) gview_klass;

	cview_parent_klass = g_type_class_peek_parent (gview_klass);
	view_klass->size_allocate   = gog_chart_view_size_allocate;
	view_klass->clip = FALSE;
	view_klass->render = gog_chart_view_render;
	oview_klass->call_parent_render = FALSE;
}

static GSF_CLASS (GogChartView, gog_chart_view,
		  gog_chart_view_class_init, gog_chart_view_init,
		  GOG_OUTLINED_VIEW_TYPE)
