/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-plot.c :
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
#include <goffice/graph/gog-plot-impl.h>
#include <goffice/graph/gog-plot-engine.h>
#include <goffice/graph/gog-series-impl.h>
#include <goffice/graph/gog-chart.h>
#include <goffice/graph/gog-axis.h>
#include <goffice/graph/gog-style.h>
#include <goffice/graph/gog-theme.h>
#include <goffice/graph/gog-graph.h>
#include <goffice/graph/gog-object-xml.h>
#include <goffice/data/go-data.h>
#include <goffice/utils/go-math.h>
#include <glib/gi18n.h>

#include <gtk/gtktable.h>
#include <gtk/gtkcombobox.h>
#include <gtk/gtklabel.h>
#include <gtk/gtkliststore.h>
#include <gtk/gtkcellrenderertext.h>
#include <gtk/gtkcelllayout.h>

#include <gsf/gsf-impl-utils.h>
#include <string.h>

#define GOG_PLOT_GET_CLASS(o)	(G_TYPE_INSTANCE_GET_CLASS ((o), GOG_PLOT_TYPE, GogPlotClass))

enum {
	PLOT_PROP_0,
	PLOT_PROP_VARY_STYLE_BY_ELEMENT,
	PLOT_PROP_AXIS_X,
	PLOT_PROP_AXIS_Y,
	PLOT_PROP_GROUP,
	PLOT_PROP_GURU_HINTS
};

static GObjectClass *plot_parent_klass;

static gboolean gog_plot_set_axis_by_id (GogPlot *plot, GogAxisType type, unsigned id);
static unsigned gog_plot_get_axis_id (GogPlot const *plot, GogAxisType type);
	
static void
gog_plot_finalize (GObject *obj)
{
	GogPlot *plot = GOG_PLOT (obj);

	g_slist_free (plot->series); /* GogObject does the unref */

	gog_plot_axis_clear (plot, GOG_AXIS_SET_ALL); /* just in case */
	if (plot->plot_group)
		g_free (plot->plot_group);
	if (plot->guru_hints != NULL)
		g_free (plot->guru_hints);

	(*plot_parent_klass->finalize) (obj);
}

static gboolean
role_series_can_add (GogObject const *parent)
{
	GogPlot *plot = GOG_PLOT (parent);
	return g_slist_length (plot->series) < plot->desc.num_series_max;
}
static gboolean
role_series_can_remove (GogObject const *child)
{
	GogPlot const *plot = GOG_PLOT (child->parent);
	return g_slist_length (plot->series) > plot->desc.num_series_min;
}

static GogObject *
role_series_allocate (GogObject *plot)
{
	GogPlotClass *klass = GOG_PLOT_GET_CLASS (plot);
	GType type = klass->series_type;

	if (type == 0)
		type = GOG_SERIES_TYPE;
	return g_object_new (type, NULL);
}

static void
role_series_post_add (GogObject *parent, GogObject *child)
{
	GogPlot *plot = GOG_PLOT (parent);
	GogSeries *series = GOG_SERIES (child);
	unsigned num_dim;
	num_dim = plot->desc.series.num_dim;

	/* Alias things so that dim -1 is valid */
	series->values = g_new0 (GogDatasetElement, num_dim+1) + 1;
	series->plot = plot;

	/* if there are other series associated with the plot, and there are 
	 * shared dimensions, clone them over.  */
	if (series->plot->series != NULL) {
		GogGraph *graph = gog_object_get_graph (GOG_OBJECT (plot));
		GogSeriesDesc const *desc = &plot->desc.series;
		GogSeries const *src = plot->series->data;
		unsigned i;

		for (i = num_dim; i-- > 0 ; ) /* name is never shared */
			if (desc->dim[i].is_shared)
				gog_dataset_set_dim_internal (GOG_DATASET (series),
					i, src->values[i].data, graph);

		gog_series_check_validity (series);
	}

	/* APPEND to keep order, there won't be that many */
	plot->series = g_slist_append (plot->series, series);
	gog_plot_request_cardinality_update (plot);
}

static void
role_series_pre_remove (GogObject *parent, GogObject *series)
{
	GogPlot *plot = GOG_PLOT (parent);
	plot->series = g_slist_remove (plot->series, series);
	gog_plot_request_cardinality_update (plot);
}

typedef struct {
	GogPlot		*plot;
	GogAxisType	type;
} PlotPrefState;

static void
cb_axis_changed (GtkComboBox *combo, PlotPrefState *state)
{
	GtkTreeIter iter;
	GValue value;
	GtkTreeModel *model = gtk_combo_box_get_model (combo);

	memset (&value, 0, sizeof (GValue));
	gtk_combo_box_get_active_iter (combo, &iter);
	gtk_tree_model_get_value (model, &iter, 1, &value);
	gog_plot_set_axis_by_id (state->plot, state->type, g_value_get_uint (&value));
}

static void
gog_plot_populate_editor (GogObject *obj,
			  GogEditor *editor,
			  G_GNUC_UNUSED GogDataAllocator *dalloc,
			  GOCmdContext *cc)
{
	static const char *axis_labels[7] = { 
		N_("X axis:"), 
		N_("Y axis:"), 
		N_("Z axis:"), 
		N_("Circular axis:"), 
		N_("Radial axis:"), 
		N_("Type axis:"), 
		N_("Pseudo 3D axis:")
	};
	
	GtkWidget *table, *combo;
	GogAxisType type;
	GogPlot *plot = GOG_PLOT (obj);
	unsigned count = 0, axis_count;
	GSList *axes, *ptr;
	GogChart *chart = GOG_CHART (gog_object_get_parent (obj));
	GogAxis *axis;
	GtkListStore *store;
	GtkTreeIter iter;
	GtkCellRenderer *cell;
	PlotPrefState *state;

	g_return_if_fail (chart != NULL);

	if (gog_chart_get_axis_set (chart) != GOG_AXIS_SET_XY) {
		(GOG_OBJECT_CLASS(plot_parent_klass)->populate_editor) (obj, editor, dalloc, cc);
		return;
	}

	table = gtk_table_new (0, 1, FALSE);
	for (type = 0 ; type < GOG_AXIS_TYPES ; type++) {
		if (plot->axis[type] != NULL) {
			count++;
			gtk_table_resize (GTK_TABLE (table), count, 1);
			gtk_table_attach (GTK_TABLE (table), gtk_label_new (_(axis_labels[type])),
					  0, 1, count - 1, count, 0, 0, 0, 0);

			store = gtk_list_store_new (2, G_TYPE_STRING, G_TYPE_UINT);
			combo = gtk_combo_box_new_with_model (GTK_TREE_MODEL (store));

			cell = gtk_cell_renderer_text_new ();
			gtk_cell_layout_pack_start (GTK_CELL_LAYOUT (combo), cell, TRUE);
			gtk_cell_layout_set_attributes (GTK_CELL_LAYOUT (combo), cell,
							"text", 0,
							NULL);
			
			axes = gog_chart_get_axes (chart, type);
			axis_count = 0;
			for (ptr = axes; ptr != NULL; ptr = ptr->next) {
				axis = GOG_AXIS (ptr->data);
				gtk_list_store_prepend (store, &iter);
				gtk_list_store_set (store, &iter, 
					0, gog_object_get_name (GOG_OBJECT (axis)), 
					1, gog_object_get_id (GOG_OBJECT (axis)),
					-1);
				if (axis == plot->axis[type])
					gtk_combo_box_set_active_iter (GTK_COMBO_BOX (combo), &iter); 
				axis_count++;
			}
			if (axis_count < 2)
				gtk_widget_set_sensitive (GTK_WIDGET (combo), FALSE);
			g_slist_free (axes);
			gtk_table_attach (GTK_TABLE (table), combo,
					  1, 2, count - 1, count, 0, 0, 0, 0);
			state = g_new (PlotPrefState, 1);
			state->plot = plot;
			state->type = type;
			g_signal_connect (G_OBJECT (combo), "changed",
					  G_CALLBACK (cb_axis_changed), state);
			g_object_set_data_full (G_OBJECT (combo),
						"state", state, (GDestroyNotify) g_free);
		}
	}

	if (count > 0) {
		gtk_table_set_col_spacings (GTK_TABLE (table), 12);
		gtk_table_set_row_spacings (GTK_TABLE (table), 6);
		gtk_container_set_border_width (GTK_CONTAINER (table), 12);
		gtk_widget_show_all (table);
		gog_editor_add_page (editor, table, _("Axes"));
	}
	else
		g_object_unref (G_OBJECT (table));
	
	(GOG_OBJECT_CLASS(plot_parent_klass)->populate_editor) (obj, editor, dalloc, cc);
}

static void
gog_plot_set_property (GObject *obj, guint param_id,
		       GValue const *value, GParamSpec *pspec)
{
	GogPlot *plot = GOG_PLOT (obj);
	gboolean b_tmp;

	switch (param_id) {
	case PLOT_PROP_VARY_STYLE_BY_ELEMENT:
		b_tmp = g_value_get_boolean (value) &&
			gog_plot_supports_vary_style_by_element (plot);
		if (plot->vary_style_by_element ^ b_tmp) {
			plot->vary_style_by_element = b_tmp;
			gog_plot_request_cardinality_update (plot);
		}
		break;
	case PLOT_PROP_AXIS_X:
		gog_plot_set_axis_by_id (plot, GOG_AXIS_X, g_value_get_uint (value));
		break;
	case PLOT_PROP_AXIS_Y:
		gog_plot_set_axis_by_id (plot, GOG_AXIS_Y, g_value_get_uint (value));
		break;
	case PLOT_PROP_GROUP: {
		char const *group = g_value_get_string (value);
		if (plot->plot_group)
			g_free (plot->plot_group);
		plot->plot_group = (group)? g_strdup (g_value_get_string (value)): NULL;
		break;
	}
	case PLOT_PROP_GURU_HINTS:
		if (plot->guru_hints != NULL)
			g_free (plot->guru_hints);
		plot->guru_hints = g_strdup (g_value_get_string (value));
		break;

	default: G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, param_id, pspec);
		 return; /* NOTE : RETURN */
	}
}

static void
gog_plot_get_property (GObject *obj, guint param_id,
		       GValue *value, GParamSpec *pspec)
{
	GogPlot *plot = GOG_PLOT (obj);
	switch (param_id) {
	case PLOT_PROP_VARY_STYLE_BY_ELEMENT:
		g_value_set_boolean (value,
			plot->vary_style_by_element &&
			gog_plot_supports_vary_style_by_element (plot));
		break;
	case PLOT_PROP_AXIS_X:
		g_value_set_uint (value, gog_plot_get_axis_id (plot, GOG_AXIS_X));
		break;
	case PLOT_PROP_AXIS_Y:
		g_value_set_uint (value, gog_plot_get_axis_id (plot, GOG_AXIS_Y));
		break;
	case PLOT_PROP_GROUP:
		g_value_set_string (value, plot->plot_group);
		break;
	case PLOT_PROP_GURU_HINTS:
		g_value_set_string (value, plot->guru_hints);
		break;

	default: G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, param_id, pspec);
		 break;
	}
}

static void
gog_plot_children_reordered (GogObject *obj)
{
	GSList *ptr, *accum = NULL;
	GogPlot *plot = GOG_PLOT (obj);

	for (ptr = obj->children; ptr != NULL ; ptr = ptr->next)
		if (IS_GOG_SERIES (ptr->data))
			accum = g_slist_prepend (accum, ptr->data);
	g_slist_free (plot->series);
	plot->series = g_slist_reverse (accum);

	gog_plot_request_cardinality_update (plot);
}

static void
gog_plot_class_init (GogObjectClass *gog_klass)
{
	static GogObjectRole const roles[] = {
		{ N_("Series"), "GogSeries",	0,
		  GOG_POSITION_SPECIAL, GOG_POSITION_SPECIAL, GOG_OBJECT_NAME_BY_ROLE,
		  role_series_can_add, role_series_can_remove,
		  role_series_allocate,
		  role_series_post_add, role_series_pre_remove, NULL },
	};
	GObjectClass *gobject_klass = (GObjectClass *) gog_klass;
	GogPlotClass *plot_klass = (GogPlotClass *) gog_klass;

	plot_parent_klass = g_type_class_peek_parent (gog_klass);
	gobject_klass->finalize		= gog_plot_finalize;
	gobject_klass->set_property	= gog_plot_set_property;
	gobject_klass->get_property	= gog_plot_get_property;
	gog_klass->populate_editor	= gog_plot_populate_editor;
	plot_klass->axis_set 		= GOG_AXIS_SET_NONE;
	plot_klass->guru_helper		= NULL;

	g_object_class_install_property (gobject_klass, PLOT_PROP_VARY_STYLE_BY_ELEMENT,
		g_param_spec_boolean ("vary-style-by-element", "vary-style-by-element",
			"Use a different style for each segments",
			FALSE,
			G_PARAM_READWRITE|GOG_PARAM_PERSISTENT|GOG_PARAM_FORCE_SAVE));
	g_object_class_install_property (gobject_klass, PLOT_PROP_AXIS_X,
		g_param_spec_uint ("x_axis", "x_axis", "Reference to X axis",
			0, G_MAXINT, 0,
			G_PARAM_READWRITE|GOG_PARAM_PERSISTENT));
	g_object_class_install_property (gobject_klass, PLOT_PROP_AXIS_Y,
		g_param_spec_uint ("y_axis", "y_axis", "Reference to Y axis",
			0, G_MAXINT, 0,
			G_PARAM_READWRITE|GOG_PARAM_PERSISTENT));
	g_object_class_install_property (gobject_klass, PLOT_PROP_GROUP,
		g_param_spec_string ("plot-group", _("Plot group"), 
			_("Name of plot group if any"),
			NULL, G_PARAM_READWRITE|GOG_PARAM_PERSISTENT));
	g_object_class_install_property (gobject_klass, PLOT_PROP_GURU_HINTS,
		g_param_spec_string ("guru-hints", _("Guru hints"), 
			_("Semicolon separated list of hints for automatic addition of objects in"
			  "guru dialog"),
			NULL, G_PARAM_READWRITE));

	gog_klass->children_reordered = gog_plot_children_reordered;
	gog_object_register_roles (gog_klass, roles, G_N_ELEMENTS (roles));
	GOG_PLOT_CLASS (gog_klass)->update_3d = NULL;
}

static void
gog_plot_init (GogPlot *plot, GogPlotClass const *derived_plot_klass)
{
	/* keep a local copy so that we can over-ride things if desired */
	plot->desc = derived_plot_klass->desc;
	/* start as true so that we can queue an update when it changes */
	plot->cardinality_valid = TRUE;
	plot->render_before_axes = FALSE;
	plot->plot_group = NULL;
	plot->guru_hints = NULL;
}

GSF_CLASS_ABSTRACT (GogPlot, gog_plot,
		    gog_plot_class_init, gog_plot_init,
		    GOG_OBJECT_TYPE)

GogPlot *
gog_plot_new_by_type (GogPlotType const *type)
{
	GogPlot *res;

	g_return_val_if_fail (type != NULL, NULL);

	res = gog_plot_new_by_name (type->engine);
	if (res != NULL && type->properties != NULL)
		g_hash_table_foreach (type->properties,
			(GHFunc) gog_object_set_arg, res);
	return res;
}

/**
 * gog_plot_make_similar :
 * @dst :
 * @src :
 *
 * As much as possible have @dst use similar formatting and data allocation to
 * @src.
 *
 * return TRUE on failue
 **/
gboolean
gog_plot_make_similar (GogPlot *dst, GogPlot const *src)
{
	g_return_val_if_fail (GOG_PLOT (dst) != NULL, TRUE);
	g_return_val_if_fail (GOG_PLOT (src) != NULL, TRUE);

	return FALSE;
}

/* convenience routines */
GogSeries *
gog_plot_new_series (GogPlot *plot)
{
	GogObject *res;

	g_return_val_if_fail (GOG_PLOT (plot) != NULL, NULL);

	res = gog_object_add_by_name (GOG_OBJECT (plot), "Series", NULL);
	return res ? GOG_SERIES (res) : NULL;
}
GogPlotDesc const *
gog_plot_description (GogPlot const *plot)
{
	g_return_val_if_fail (GOG_PLOT (plot) != NULL, NULL);
	return &plot->desc;
}

static GogChart *
gog_plot_get_chart (GogPlot const *plot)
{
	return GOG_CHART (GOG_OBJECT (plot)->parent);
}

/* protected */
void
gog_plot_request_cardinality_update (GogPlot *plot)
{
	g_return_if_fail (GOG_PLOT (plot) != NULL);

	if (plot->cardinality_valid) {
		GogChart *chart = gog_plot_get_chart (plot);
		plot->cardinality_valid = FALSE;
		gog_object_request_update (GOG_OBJECT (plot));
		if (chart != NULL)
			gog_chart_request_cardinality_update (chart);
	}
}

/**
 * gog_plot_get_cardinality :
 * @plot : #GogPlot
 *
 * Return the number of logical elements in the plot, updating the cache if
 * necessary
 **/
void
gog_plot_get_cardinality (GogPlot *plot, unsigned *full, unsigned *visible)
{
	g_return_if_fail (GOG_PLOT (plot) != NULL);

	if (!plot->cardinality_valid) {
		GogSeries *series;
		GSList	  *ptr;
		gboolean   is_valid;
		unsigned   size = 0, no_legend = 0, i;

		plot->cardinality_valid = TRUE;
		gog_chart_get_cardinality (gog_plot_get_chart (plot), NULL, &i);
		plot->index_num = i;

		for (ptr = plot->series; ptr != NULL ; ptr = ptr->next) {
			series = GOG_SERIES (ptr->data);
			is_valid = gog_series_is_valid (GOG_SERIES (series));
			if (plot->vary_style_by_element) {
				if (is_valid && size < series->num_elements)
					size = series->num_elements;
				gog_series_set_index (series, plot->index_num, FALSE);
			} else {
				gog_series_set_index (series, i++, FALSE);
				if (!gog_series_has_legend (series))
					no_legend++;
			}
		}

		plot->full_cardinality =
			plot->vary_style_by_element ? size : (i - plot->index_num);
		plot->visible_cardinality = plot->full_cardinality - no_legend;
	}

	if (full != NULL)
		*full = plot->full_cardinality;
	if (visible != NULL)
		*visible = plot->visible_cardinality;
}

void
gog_plot_foreach_elem (GogPlot *plot, gboolean only_visible,
		       GogEnumFunc func, gpointer data)
{
	GSList *ptr;
	GogSeries const *series;
	GogStyle *style, *tmp_style;
	GODataVector *labels;
	unsigned i, n, num_labels = 0;
	char *label = NULL;
	GogTheme *theme = gog_object_get_theme (GOG_OBJECT (plot));
	GogPlotClass *klass = GOG_PLOT_GET_CLASS (plot);
	GList *overrides;

	g_return_if_fail (GOG_PLOT (plot) != NULL);
	if (!plot->cardinality_valid)
		gog_plot_get_cardinality (plot, NULL, NULL);

	if (klass->foreach_elem) {
		klass->foreach_elem (plot, only_visible, func, data);
		return;
	}

	ptr = plot->series;
	if (ptr == NULL)
		return;

	if (!plot->vary_style_by_element) {
		unsigned i = plot->index_num;
		for (; ptr != NULL ; ptr = ptr->next)
			if (!only_visible || gog_series_has_legend (ptr->data)) {
				func (i, gog_styled_object_get_style (ptr->data),
				      gog_object_get_name (ptr->data), data);
				i++;
			}
		return;
	}

	series = ptr->data; /* start with the first */
	labels = NULL;
	if (series->values[0].data != NULL) {
		labels = GO_DATA_VECTOR (series->values[0].data);
		num_labels = go_data_vector_get_len (labels);
	}
	style = gog_style_dup (series->base.style);
	n = only_visible ? plot->visible_cardinality : plot->full_cardinality;
	for (overrides = series->overrides, i = 0; i < n ; i++) {
		if (overrides != NULL &&
		    (GOG_SERIES_ELEMENT (overrides->data)->index == i)) {
			tmp_style = GOG_STYLED_OBJECT (overrides->data)->style;
			overrides = overrides->next;
		} else
			tmp_style = style;

		gog_theme_fillin_style (theme, tmp_style, GOG_OBJECT (series),
			plot->index_num + i, FALSE);
		if (labels != NULL)
 			label = (i < num_labels)
 				? go_data_vector_get_str (labels, i) : g_strdup ("");
		else
			label = NULL;
		if (label == NULL)
			label = g_strdup_printf ("%d", i);
		(func) (i, tmp_style, label, data);
		g_free (label);
	}
	g_object_unref (style);
}

/**
 * gog_plot_get_series :
 * @plot : #GogPlot
 *
 * A list of the series in @plot.  Do not modify or free the list.
 **/
GSList const *
gog_plot_get_series (GogPlot const *plot)
{
	g_return_val_if_fail (GOG_PLOT (plot) != NULL, NULL);
	return plot->series;
}

/**
 * gog_plot_get_axis_bounds :
 * @plot : #GogPlot
 * @axis : #GogAxisType
 * @bounds : #GogPlotBoundInfo
 *
 * Queries @plot for its axis preferences for @axis and stores the results in
 * @bounds.  All elements of @bounds are initialized to sane values before the
 * query _ACCEPT_ ::fmt.  The caller is responsible for initializing it.  This
 * is done so that once on plot has selected a format the others need not do
 * the lookup too if so desired.
 *
 * Caller is responsible for unrefing ::fmt.
 **/
GOData *
gog_plot_get_axis_bounds (GogPlot *plot, GogAxisType axis,
			  GogPlotBoundInfo *bounds)
{
	GogPlotClass *klass = GOG_PLOT_GET_CLASS (plot);

	g_return_val_if_fail (klass != NULL, NULL);
	g_return_val_if_fail (bounds != NULL, NULL);

	bounds->val.minima =  DBL_MAX;
	bounds->val.maxima = -DBL_MAX;
	bounds->logical.maxima = go_nan;
	bounds->logical.minima = go_nan;
	bounds->is_discrete = FALSE;
	bounds->center_on_ticks = TRUE;
	if (klass->axis_get_bounds == NULL)
		return NULL;
	return (klass->axis_get_bounds) (plot, axis, bounds);
}

gboolean
gog_plot_supports_vary_style_by_element (GogPlot const *plot)
{
	GogPlotClass *klass = GOG_PLOT_GET_CLASS (plot);

	g_return_val_if_fail (klass != NULL, FALSE);

	if (klass->supports_vary_style_by_element)
		return (klass->supports_vary_style_by_element) (plot);
	return TRUE; /* default */
}

GogAxisSet
gog_plot_axis_set_pref (GogPlot const *plot)
{
	GogPlotClass *klass = GOG_PLOT_GET_CLASS (plot);

	g_return_val_if_fail (klass != NULL, GOG_AXIS_SET_UNKNOWN);
	return klass->axis_set;
}

gboolean
gog_plot_axis_set_is_valid (GogPlot const *plot, GogAxisSet axis_set)
{
	GogPlotClass *klass = GOG_PLOT_GET_CLASS (plot);

	g_return_val_if_fail (klass != NULL, FALSE);
	return (axis_set == klass->axis_set);
}

static gboolean
gog_plot_set_axis_by_id (GogPlot *plot, GogAxisType type, unsigned id)
{
	GogChart const *chart;
	GogAxis *axis;
	GSList *axes, *ptr;
	gboolean found = FALSE;

	if (id == 0)
		return FALSE;

	g_return_val_if_fail (GOG_PLOT (plot) != NULL, FALSE);
	g_return_val_if_fail (GOG_OBJECT (plot)->parent != NULL, FALSE);

	chart = gog_plot_get_chart (plot);
	g_return_val_if_fail (GOG_CHART (chart) != NULL, FALSE);

	axes = gog_chart_get_axes (chart, type);
	g_return_val_if_fail (axes != NULL, FALSE);

	for (ptr = axes; ptr != NULL && !found; ptr = ptr->next) {
		axis = GOG_AXIS (ptr->data);
		if (gog_object_get_id (GOG_OBJECT (axis)) == id) {
			if (plot->axis[type] != NULL)
				gog_axis_del_contributor (plot->axis[type], GOG_OBJECT (plot));
			plot->axis[type] = axis;
			gog_axis_add_contributor (axis, GOG_OBJECT (plot));
			found = TRUE;
		}
	}		
	g_slist_free (axes);
	return found;
}

gboolean
gog_plot_axis_set_assign (GogPlot *plot, GogAxisSet axis_set)
{
	GogPlotClass *klass = GOG_PLOT_GET_CLASS (plot);
	GogChart const *chart;
	GogAxisType type;

	g_return_val_if_fail (klass != NULL, FALSE);

	chart = gog_plot_get_chart (plot);
	for (type = 0 ; type < GOG_AXIS_TYPES ; type++) {
		if (plot->axis[type] != NULL) {
			if (!(axis_set & (1 << type))) {
				gog_axis_del_contributor (plot->axis[type], GOG_OBJECT (plot));
				plot->axis[type] = NULL;
			}
		} else if (axis_set & (1 << type)) {
			GSList *axes = gog_chart_get_axes (chart, type);
			if (axes != NULL) {
				gog_axis_add_contributor (axes->data, GOG_OBJECT (plot));
				plot->axis[type] = axes->data;
				g_slist_free (axes);
			}
		}
	}

	return (axis_set == klass->axis_set);
}

/**
 * gog_plot_axis_clear : 
 * @plot : #GogPlot
 * @filter : #GogAxisSet
 * 
 * A utility method to clear all existing axis associations flagged by @filter
 **/
void
gog_plot_axis_clear (GogPlot *plot, GogAxisSet filter)
{
	GogAxisType type;

	g_return_if_fail (GOG_PLOT (plot) != NULL);

	for (type = 0 ; type < GOG_AXIS_TYPES ; type++)
		if (plot->axis[type] != NULL && ((1 << type) & filter)) {
			gog_axis_del_contributor (plot->axis[type], GOG_OBJECT (plot));
			plot->axis[type] = NULL;
		}
}

static unsigned
gog_plot_get_axis_id (GogPlot const *plot, GogAxisType type)
{
	GogAxis *axis = gog_plot_get_axis (plot, type);
	
	return axis != NULL ? gog_object_get_id (GOG_OBJECT (axis)) : 0;
}

GogAxis	*
gog_plot_get_axis (GogPlot const *plot, GogAxisType type)
{
	g_return_val_if_fail (GOG_PLOT (plot) != NULL, NULL);
	g_return_val_if_fail (type < GOG_AXIS_TYPES, NULL);
	g_return_val_if_fail (GOG_AXIS_UNKNOWN < type, NULL);
	return plot->axis[type];
}

void
gog_plot_update_3d (GogPlot *plot)
{
	GogPlotClass *klass = GOG_PLOT_GET_CLASS (plot);
	g_return_if_fail (GOG_PLOT (plot) != NULL);

	if (klass->update_3d)
		klass->update_3d (plot);
}

static void
gog_plot_guru_helper_add_grid_line (GogPlot *plot, gboolean major) 
{
	GogAxisType type;

	for (type = 0; type < GOG_AXIS_TYPES; type++) {
		if (((type & (GOG_AXIS_X | 
			    GOG_AXIS_Y | 
			    GOG_AXIS_CIRCULAR | 
			    GOG_AXIS_RADIAL)) != 0) &&
		    plot->axis[type] != NULL &&
		    gog_axis_get_grid_line (plot->axis[type], major) == NULL)
		{
			gog_object_add_by_name (GOG_OBJECT (plot->axis[type]), 
						major ? "MajorGrid": "MinorGrid", NULL);
		}
	}
}

void
gog_plot_guru_helper (GogPlot *plot)
{
	GogPlotClass *klass;
	char **hints;
	char *hint;
	unsigned i;

 	g_return_if_fail (GOG_PLOT (plot) != NULL);	
	klass = GOG_PLOT_GET_CLASS (plot);

	if (plot->guru_hints == NULL)
		return;

	hints = g_strsplit (plot->guru_hints, ";", 0);
	
	for (i = 0; i < g_strv_length (hints); i++) {
		hint = g_strstrip (hints[i]);
		if (strcmp (hints[i], "backplane") == 0) {
			GogChart *chart = GOG_CHART (gog_object_get_parent (GOG_OBJECT (plot)));
			if (chart != NULL && gog_chart_get_grid (chart) == NULL)
				gog_object_add_by_name (GOG_OBJECT (chart), "Grid", NULL);
		} else if (strcmp (hints[i], "major-grid") == 0) {
			gog_plot_guru_helper_add_grid_line (plot, TRUE);
		} else if (strcmp (hints[i], "minor-grid") == 0) {
			gog_plot_guru_helper_add_grid_line (plot, FALSE);
		} else if (klass->guru_helper)
			klass->guru_helper (plot, hint);
	}

	g_strfreev (hints);
}

/****************************************************************************/
/* a placeholder.  It seems likely that we will want this eventually        */
GSF_CLASS_ABSTRACT (GogPlotView, gog_plot_view,
		    NULL, NULL,
		    GOG_VIEW_TYPE)
