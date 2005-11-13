/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-axis-line.c :
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

#include <goffice/goffice-config.h>

#include <goffice/graph/gog-axis-line-impl.h>
#include <goffice/graph/gog-axis.h>
#include <goffice/graph/gog-chart.h>
#include <goffice/graph/gog-data-allocator.h>
#include <goffice/graph/gog-renderer.h>
#include <goffice/graph/gog-style.h>
#include <goffice/graph/gog-theme.h>

#include <goffice/gtk/goffice-gtk.h>

#include <goffice/utils/go-math.h>

#include <gsf/gsf-impl-utils.h>

#include <gtk/gtkcellrenderertext.h>
#include <gtk/gtkcelllayout.h>
#include <gtk/gtkcombobox.h>
#include <gtk/gtkliststore.h>
#include <gtk/gtkspinbutton.h>
#include <gtk/gtktogglebutton.h>

#include <glib/gi18n.h>

static GogViewClass *gab_view_parent_klass;
static GObjectClass *gab_parent_klass;

enum {
	AXIS_BASE_PROP_0,
	AXIS_BASE_PROP_POSITION,
	AXIS_BASE_PROP_MAJOR_TICK_LABELED,
	AXIS_BASE_PROP_MAJOR_TICK_IN,
	AXIS_BASE_PROP_MAJOR_TICK_OUT,
	AXIS_BASE_PROP_MAJOR_TICK_SIZE_PTS,
	AXIS_BASE_PROP_MINOR_TICK_IN,
	AXIS_BASE_PROP_MINOR_TICK_OUT,
	AXIS_BASE_PROP_MINOR_TICK_SIZE_PTS,
	AXIS_BASE_PROP_CROSS_AXIS_ID,
	AXIS_BASE_PROP_CROSS_LOCATION
};

static double gog_axis_base_get_cross_location (GogAxisBase *axis_base);

static void
gog_axis_base_set_property (GObject *obj, guint param_id,
			    GValue const *value, GParamSpec *pspec)
{
	gboolean resized = FALSE;
	char const *str;
	GogAxisBase *axis_base = GOG_AXIS_BASE (obj);
	int itmp;
	unsigned position;

	switch (param_id) {
		case AXIS_BASE_PROP_POSITION:
			str = g_value_get_string (value);
			if (str == NULL)
				return;
			else if (!g_ascii_strcasecmp (str, "low"))
				position = GOG_AXIS_AT_LOW;
			else if (!g_ascii_strcasecmp (str, "cross"))
				position = GOG_AXIS_CROSS;
			else if (!g_ascii_strcasecmp (str, "high"))
				position = GOG_AXIS_AT_HIGH;
			else
				return;
			resized = (position != axis_base->position);
			axis_base->position = position;
			break;
		case AXIS_BASE_PROP_CROSS_AXIS_ID:
			axis_base->crossed_axis_id = g_value_get_uint (value);
			break;

		case AXIS_BASE_PROP_MAJOR_TICK_LABELED:
			itmp = g_value_get_boolean (value);
			if (axis_base->major_tick_labeled != itmp) {
				axis_base->major_tick_labeled = itmp;
				resized = TRUE;
			}
			break;
		case AXIS_BASE_PROP_MAJOR_TICK_IN :
			axis_base->major.tick_in = g_value_get_boolean (value);
			break;
		case AXIS_BASE_PROP_MAJOR_TICK_OUT :
			itmp = g_value_get_boolean (value);
			if (axis_base->major.tick_out != itmp) {
				axis_base->major.tick_out = itmp;
				resized = axis_base->major.size_pts > 0;
			}
			break;
		case AXIS_BASE_PROP_MAJOR_TICK_SIZE_PTS:
			itmp = g_value_get_int (value);
			if (axis_base->major.size_pts != itmp) {
				axis_base->major.size_pts = itmp;
				resized = axis_base->major.tick_out;
			}
			break;

		case AXIS_BASE_PROP_MINOR_TICK_IN :
			axis_base->minor.tick_in = g_value_get_boolean (value);
			break;
		case AXIS_BASE_PROP_MINOR_TICK_OUT :
			itmp = g_value_get_boolean (value);
			if (axis_base->minor.tick_out != itmp) {
				axis_base->minor.tick_out = itmp;
				resized = axis_base->minor.size_pts > 0;
			}
			break;
		case AXIS_BASE_PROP_MINOR_TICK_SIZE_PTS:
			itmp = g_value_get_int (value);
			if (axis_base->minor.size_pts != itmp) {
				axis_base->minor.size_pts = itmp;
				resized = axis_base->minor.tick_out;
		}
			break;

		default: G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, param_id, pspec);
			 return; /* NOTE : RETURN */
	}

	gog_object_emit_changed (GOG_OBJECT (obj), resized);
}

static void
gog_axis_base_get_property (GObject *obj, guint param_id,
			    GValue *value, GParamSpec *pspec)
{
	GogAxisBase *axis_base = GOG_AXIS_BASE (obj);

	switch (param_id) {
		case AXIS_BASE_PROP_POSITION:
			switch (axis_base->position) {
				case GOG_AXIS_AT_LOW:
					g_value_set_static_string (value, "low");
					break;
				case GOG_AXIS_CROSS:
					g_value_set_static_string (value, "cross");
					break;
				case GOG_AXIS_AT_HIGH:
					g_value_set_static_string (value, "high");
					break;
				default:
					g_warning ("[GogAxisBase::set_property] invalid axis position");
				break;
			}
			break;
		case AXIS_BASE_PROP_CROSS_AXIS_ID:
			g_value_set_uint (value, axis_base->crossed_axis_id);
			break;

		case AXIS_BASE_PROP_MAJOR_TICK_LABELED:
			g_value_set_boolean (value, axis_base->major_tick_labeled);
			break;
		case AXIS_BASE_PROP_MAJOR_TICK_IN:
			g_value_set_boolean (value, axis_base->major.tick_in);
			break;
		case AXIS_BASE_PROP_MAJOR_TICK_OUT:
			g_value_set_boolean (value, axis_base->major.tick_out);
			break;
		case AXIS_BASE_PROP_MAJOR_TICK_SIZE_PTS:
			g_value_set_int (value, axis_base->major.size_pts);
			break;

		case AXIS_BASE_PROP_MINOR_TICK_IN:
			g_value_set_boolean (value, axis_base->minor.tick_in);
			break;
		case AXIS_BASE_PROP_MINOR_TICK_OUT:
			g_value_set_boolean (value, axis_base->minor.tick_out);
			break;
		case AXIS_BASE_PROP_MINOR_TICK_SIZE_PTS:
			g_value_set_int (value, axis_base->minor.size_pts);
			break;

		default: G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, param_id, pspec);
			 break;
	}
}

static void
gog_axis_base_parent_changed (GogObject *child, gboolean was_set)
{
	GogAxisBase *axis_base = GOG_AXIS_BASE (child);

	if (was_set) {
		if (IS_GOG_AXIS (child))
			axis_base->axis = GOG_AXIS (child);
		else
			axis_base->axis = GOG_AXIS (child->parent);
		axis_base->chart = GOG_CHART (GOG_OBJECT (axis_base->axis)->parent);

	} else {
		axis_base->axis = NULL;
		axis_base->chart = NULL;
	}
	(GOG_OBJECT_CLASS (gab_parent_klass)->parent_changed) (child, was_set);
}

static void
gog_axis_base_finalize (GObject *obj)
{
	gog_dataset_finalize (GOG_DATASET (obj));
	(gab_parent_klass->finalize) (obj);
}

static GogAxisType
gog_axis_base_get_crossed_axis_type (GogAxisBase *axis_base)
{
	GogAxisType axis_type, crossed_type;
	GogAxisSet axis_set;

	axis_type = gog_axis_get_atype (axis_base->axis);
	axis_set = gog_chart_get_axis_set (axis_base->chart);

	crossed_type = GOG_AXIS_UNKNOWN;
	switch (axis_set) {
		case GOG_AXIS_SET_XY:
		case GOG_AXIS_SET_XY_pseudo_3d:
			if (axis_type == GOG_AXIS_X)
				crossed_type = GOG_AXIS_Y;
			else
				crossed_type = GOG_AXIS_X;
			break;
		case GOG_AXIS_SET_RADAR:
			if (axis_type == GOG_AXIS_RADIAL)
				crossed_type = GOG_AXIS_CIRCULAR;
			else
				crossed_type = GOG_AXIS_RADIAL;
			break;
		case GOG_AXIS_SET_X:
		case GOG_AXIS_SET_UNKNOWN:
			break;
		case GOG_AXIS_SET_XYZ:
		case GOG_AXIS_SET_ALL:
		case GOG_AXIS_SET_NONE:
			g_message ("[GogAxisBase::get_crossed_axis_type] unimplemented for this axis set (%i)",
				   axis_set);
			break;
	}
	return crossed_type;
}

static GogAxis *
gog_axis_base_get_crossed_axis (GogAxisBase *axis_base)
{
	GogAxis *crossed_axis = NULL;
	GSList *axes, *ptr;
	gboolean found = FALSE;

	axes = gog_chart_get_axes (axis_base->chart,
		gog_axis_base_get_crossed_axis_type (axis_base));
	g_return_val_if_fail (axes != NULL, NULL);

	for (ptr = axes; ptr != NULL && !found; ptr = ptr->next) {
		crossed_axis = GOG_AXIS (ptr->data);
		if (gog_object_get_id (GOG_OBJECT (crossed_axis)) == axis_base->crossed_axis_id)
			found = TRUE;
	}

	if (!found)
		crossed_axis = GOG_AXIS (axes->data);

	g_slist_free (axes);
	return crossed_axis;
}

void
gog_axis_base_set_position (GogAxisBase *axis_base, GogAxisPosition position)
{
	GogAxis *axis;
	GogChart *chart;
	GSList *lines, *axes = NULL, *lptr, *aptr;
	gboolean can_at_low = TRUE, can_at_high = TRUE;

	g_return_if_fail (GOG_AXIS_BASE (axis_base) != NULL);

	if (position == GOG_AXIS_AUTO) {
		if (IS_GOG_AXIS (axis_base))
			axis = GOG_AXIS (axis_base);
		else
			axis = GOG_AXIS (gog_object_get_parent (GOG_OBJECT (axis_base)));

		chart = GOG_CHART (gog_object_get_parent (GOG_OBJECT (axis)));
		if (chart != NULL)
			axes = gog_chart_get_axes (chart, gog_axis_get_atype (axis));
		else
			axes = g_slist_prepend (axes, axis);

		for (aptr = axes; aptr != NULL; aptr = aptr->next) {
			lines = gog_object_get_children (GOG_OBJECT (aptr->data), NULL);
			lines = g_slist_prepend (lines, aptr->data);
			for (lptr = lines; lptr != NULL; lptr = lptr->next) {
				if (lptr->data == axis_base || !IS_GOG_AXIS_BASE (lptr->data))
					continue;
				position = gog_axis_base_get_position (GOG_AXIS_BASE (lptr->data));
				if (position == GOG_AXIS_AT_HIGH )
					can_at_high = FALSE;
				else if (position == GOG_AXIS_AT_LOW)
					can_at_low = FALSE;
			}
			g_slist_free (lines);
		}
		g_slist_free (axes);

		if (can_at_low)
			position = GOG_AXIS_AT_LOW;
		else if (can_at_high)
			position = GOG_AXIS_AT_HIGH;
		else
			position = GOG_AXIS_CROSS;
	}

	axis_base->position = position;
}

typedef struct {
	GogAxisBase 	*axis_base;
	GladeXML 	*gui;
} AxisBasePrefs;

static void
axis_base_pref_free (AxisBasePrefs *state)
{
	g_object_unref (state->gui);
	g_free (state);
}

static void
cb_cross_location_changed (GtkWidget *editor, AxisBasePrefs *state)
{
	gtk_toggle_button_set_active
		(GTK_TOGGLE_BUTTON (glade_xml_get_widget (state->gui, "axis_cross")),
		 TRUE);
}

static void
cb_cross_axis_changed (GtkComboBox *combo, AxisBasePrefs *state)
{
	GtkTreeIter iter;
	GValue value;
	GtkTreeModel *model = gtk_combo_box_get_model (combo);

	gtk_combo_box_get_active_iter (combo, &iter);
	gtk_tree_model_get_value (model, &iter, 1, &value);
	state->axis_base->crossed_axis_id = g_value_get_uint (&value);

	gtk_toggle_button_set_active
		(GTK_TOGGLE_BUTTON (glade_xml_get_widget (state->gui, "axis_cross")),
		 TRUE);
}

static void
cb_position_toggled (GtkWidget *button, GogAxisBase *axis_base)
{
	GogAxisPosition position;
	char const *widget_name = gtk_widget_get_name (button);
	GSList *lines, *axes, *aptr, *lptr;

	if (!gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (button)))
		return;

	if (g_ascii_strcasecmp ("axis_high", widget_name) == 0)
		position = GOG_AXIS_AT_HIGH;
	else if (g_ascii_strcasecmp ("axis_cross", widget_name) == 0)
		position = GOG_AXIS_CROSS;
	else
		position = GOG_AXIS_AT_LOW;

	if (position != GOG_AXIS_CROSS) {
		axes = gog_chart_get_axes (axis_base->chart, gog_axis_get_atype (axis_base->axis));
		for (aptr = axes; aptr != NULL; aptr = aptr->next) {
			lines = gog_object_get_children (GOG_OBJECT (aptr->data), NULL);
			lines = g_slist_prepend (lines, aptr->data);
			for (lptr = lines; lptr != NULL; lptr = lptr->next) {
				if (lptr->data == axis_base || !IS_GOG_AXIS_BASE (lptr->data))
					continue;
				if (position == gog_axis_base_get_position (GOG_AXIS_BASE (lptr->data))) {
					gog_axis_base_set_position (GOG_AXIS_BASE (lptr->data),
								    gog_axis_base_get_position (axis_base));
					break;
				}
			}
			g_slist_free (lines);
		}
		g_slist_free (axes);
	}
	gog_axis_base_set_position (axis_base, position);
	gog_object_emit_changed (GOG_OBJECT (axis_base), TRUE);
}

static void
cb_tick_toggle_changed (GtkToggleButton *toggle_button, GObject *axis_base)
{
	g_object_set (axis_base,
		gtk_widget_get_name (GTK_WIDGET (toggle_button)),
		gtk_toggle_button_get_active (toggle_button),
		NULL);
}

static void
gog_axis_base_populate_editor (GogObject *gobj,
			       GogEditor *editor,
			       GogDataAllocator *dalloc,
			       GOCmdContext *cc)
{
	static char const *toggle_props[] = {
		"major-tick-labeled",
		"major-tick-out",
		"major-tick-in",
		"minor-tick-out",
		"minor-tick-in"
	};
	GogAxis *crossed_axis;
	GogAxisBase *axis_base;
	GladeXML *gui;
	GtkListStore *store;
	GtkTreeIter iter;
	GtkWidget *combo, *data_editor, *container, *w;
	GtkCellRenderer *cell;
	GSList *axes, *ptr;
	AxisBasePrefs *state;
	GogAxisType crossed_axis_type;
	static guint axis_base_pref_page = 0;
	unsigned axis_count;
	unsigned crossed_axis_id;
	unsigned i;

	axis_base = GOG_AXIS_BASE (gobj);
	g_return_if_fail (GOG_AXIS_BASE (axis_base) != NULL);

	gog_editor_set_store_page (editor, &axis_base_pref_page);
	
	if (gog_axis_get_atype (axis_base->axis) == GOG_AXIS_PSEUDO_3D) {
		(GOG_OBJECT_CLASS(gab_parent_klass)->populate_editor) (gobj, editor, dalloc, cc);
		return;
	}

	gui = go_libglade_new ("gog-axis-prefs.glade", "axis_base_pref_box", NULL, cc);
	if (gui == NULL) {
		(GOG_OBJECT_CLASS(gab_parent_klass)->populate_editor) (gobj, editor, dalloc, cc);
		return;
	}

	crossed_axis_type = gog_axis_base_get_crossed_axis_type (axis_base);
	if (crossed_axis_type != GOG_AXIS_UNKNOWN) {
		store = gtk_list_store_new (2, G_TYPE_STRING, G_TYPE_UINT);
		combo = glade_xml_get_widget (gui, "cross_axis_combo");
		gtk_combo_box_set_model (GTK_COMBO_BOX (combo), GTK_TREE_MODEL (store));

		cell = gtk_cell_renderer_text_new ();
		gtk_cell_layout_pack_start (GTK_CELL_LAYOUT (combo), cell, TRUE);
		gtk_cell_layout_set_attributes (GTK_CELL_LAYOUT (combo), cell,
						"text", 0,
						NULL);

		axes = gog_chart_get_axes (axis_base->chart, crossed_axis_type);
		axis_count = 0;
		for (ptr = axes; ptr != NULL; ptr = ptr->next) {
			crossed_axis = GOG_AXIS (ptr->data);
			crossed_axis_id = gog_object_get_id (GOG_OBJECT (crossed_axis));
			gtk_list_store_prepend (store, &iter);
			gtk_list_store_set (store, &iter,
					    0, gog_object_get_name (GOG_OBJECT (crossed_axis)),
					    1, crossed_axis_id,
					    -1);
			if (axis_base->crossed_axis_id == crossed_axis_id || axis_count == 0)
				gtk_combo_box_set_active_iter (GTK_COMBO_BOX (combo), &iter);
			axis_count++;
		}
		if (axis_count < 2)
			gtk_widget_set_sensitive (GTK_WIDGET (combo), FALSE);
		g_slist_free (axes);

		data_editor = gog_data_allocator_editor (dalloc, GOG_DATASET (axis_base),
							 GOG_AXIS_ELEM_CROSS_POINT, GOG_DATA_SCALAR);
		container = glade_xml_get_widget (gui, "cross_location_alignment");
		gtk_container_add (GTK_CONTAINER (container), data_editor);
		gtk_widget_show_all (container);

		state = g_new (AxisBasePrefs, 1);
		state->axis_base = axis_base;
		state->gui = gui;
		g_signal_connect (G_OBJECT (combo), "changed",
				  G_CALLBACK (cb_cross_axis_changed), state);
		g_signal_connect (G_OBJECT (data_editor), "changed",
				  G_CALLBACK (cb_cross_location_changed), state);
		g_object_set_data_full (G_OBJECT (combo),
					"state", state, (GDestroyNotify) axis_base_pref_free);

		w = glade_xml_get_widget (gui, "axis_low");
		if (axis_base->position == GOG_AXIS_AT_LOW)
			gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (w), TRUE);
		g_signal_connect (G_OBJECT (w), "toggled",
				  G_CALLBACK (cb_position_toggled), axis_base);
		w = glade_xml_get_widget (gui, "axis_cross");
		if (axis_base->position == GOG_AXIS_CROSS)
			gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (w), TRUE);
		g_signal_connect (G_OBJECT (w), "toggled",
				  G_CALLBACK (cb_position_toggled), axis_base);
		w = glade_xml_get_widget (gui, "axis_high");
		if (axis_base->position == GOG_AXIS_AT_HIGH)
			gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (w), TRUE);
		g_signal_connect (G_OBJECT (w), "toggled",
				  G_CALLBACK (cb_position_toggled), axis_base);
	}
	else {
		w = glade_xml_get_widget (gui, "position_box");
		gtk_widget_hide (w);
	}

	for (i = 0; i < G_N_ELEMENTS (toggle_props) ; i++) {
		gboolean cur_val;

		w = glade_xml_get_widget (gui, toggle_props[i]);
		g_object_get (G_OBJECT (gobj), toggle_props[i], &cur_val, NULL);
		gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (w), cur_val);
		g_signal_connect_object (G_OBJECT (w),
					 "toggled",
					 G_CALLBACK (cb_tick_toggle_changed), axis_base, 0);
	}

	if (gog_axis_is_discrete (axis_base->axis)) {
		/* Hide minor tick properties */
		GtkWidget *w = glade_xml_get_widget (gui, "minor_tick_box");
		gtk_widget_hide (w);
	}

	gog_editor_add_page (editor,
			     glade_xml_get_widget (gui, "axis_base_pref_box"),
			     _("Layout"));

	(GOG_OBJECT_CLASS(gab_parent_klass)->populate_editor) (gobj, editor, dalloc, cc);
}

static void
gog_axis_base_init_style (GogStyledObject *gso, GogStyle *style)
{
	style->interesting_fields = GOG_STYLE_LINE | GOG_STYLE_FONT;
	gog_theme_fillin_style (gog_object_get_theme (GOG_OBJECT (gso)),
		style, GOG_OBJECT (gso), 0, FALSE);
}

static void
gog_axis_base_class_init (GObjectClass *gobject_klass)
{
	GogObjectClass *gog_klass = (GogObjectClass *) gobject_klass;
	GogStyledObjectClass *gso_klass = (GogStyledObjectClass *) gobject_klass;

	gab_parent_klass = g_type_class_peek_parent (gobject_klass);
	gobject_klass->set_property 	= gog_axis_base_set_property;
	gobject_klass->get_property 	= gog_axis_base_get_property;
	gobject_klass->finalize	    	= gog_axis_base_finalize;
	gog_klass->parent_changed 	= gog_axis_base_parent_changed;

	g_object_class_install_property (gobject_klass, AXIS_BASE_PROP_POSITION,
		g_param_spec_string ("pos_str", "pos_str",
			"Where to position an axis low, high, or crossing",
			"low", G_PARAM_READWRITE | GOG_PARAM_PERSISTENT));
	g_object_class_install_property (gobject_klass, AXIS_BASE_PROP_MAJOR_TICK_LABELED,
		g_param_spec_boolean ("major-tick-labeled", NULL,
			"Show labels for major ticks",
			TRUE, G_PARAM_READWRITE | GOG_PARAM_PERSISTENT));
	g_object_class_install_property (gobject_klass, AXIS_BASE_PROP_MAJOR_TICK_IN,
		g_param_spec_boolean ("major-tick-in", NULL,
			"Major tick marks inside the axis",
			FALSE, G_PARAM_READWRITE | GOG_PARAM_PERSISTENT));
	g_object_class_install_property (gobject_klass, AXIS_BASE_PROP_MAJOR_TICK_OUT,
		g_param_spec_boolean ("major-tick-out", NULL,
			"Major tick marks outside the axis",
			TRUE, G_PARAM_READWRITE | GOG_PARAM_PERSISTENT));
	g_object_class_install_property (gobject_klass, AXIS_BASE_PROP_MAJOR_TICK_SIZE_PTS,
		g_param_spec_int ("major-tick-size-pts", "major-tick-size-pts",
			"Size of the major tick marks in pts",
			0, 20, 4, G_PARAM_READWRITE | GOG_PARAM_PERSISTENT));

	g_object_class_install_property (gobject_klass, AXIS_BASE_PROP_MINOR_TICK_IN,
		g_param_spec_boolean ("minor-tick-in", NULL,
			"Minor tick marks inside the axis",
			FALSE, G_PARAM_READWRITE | GOG_PARAM_PERSISTENT));
	g_object_class_install_property (gobject_klass, AXIS_BASE_PROP_MINOR_TICK_OUT,
		g_param_spec_boolean ("minor-tick-out", NULL,
			"Minor tick marks outside the axis",
			FALSE, G_PARAM_READWRITE | GOG_PARAM_PERSISTENT));
	g_object_class_install_property (gobject_klass, AXIS_BASE_PROP_MINOR_TICK_SIZE_PTS,
		g_param_spec_int ("minor-tick-size-pts", NULL,
			"Size of the minor tick marks in pts",
			0, 15, 2, G_PARAM_READWRITE | GOG_PARAM_PERSISTENT));

	g_object_class_install_property (gobject_klass, AXIS_BASE_PROP_CROSS_AXIS_ID,
		g_param_spec_uint ("cross_axis_id", NULL,
			"Which axis to cross",
			0, G_MAXUINT, 0, G_PARAM_READWRITE | GOG_PARAM_PERSISTENT));

	gog_klass->populate_editor	= gog_axis_base_populate_editor;
	gog_klass->view_type		= gog_axis_base_view_get_type ();
	gso_klass->init_style 		= gog_axis_base_init_style;
}

static void
gog_axis_base_init (GogAxisBase *gab)
{
	gab->chart = NULL;
	gab->axis = NULL;

	gab->position = GOG_AXIS_AT_LOW;
	gab->crossed_axis_id = 0;

	gab->minor.tick_in = gab->minor.tick_out = gab->major.tick_in = FALSE;
	gab->major.tick_out = TRUE;
	gab->major_tick_labeled = TRUE;
	gab->major.size_pts = 4;
	gab->minor.size_pts = 2;
}

static double
gog_axis_base_get_cross_location (GogAxisBase *axis_base)
{
	GOData *data;

	g_return_val_if_fail (GOG_AXIS_BASE (axis_base) != NULL, 0.);

	data = axis_base->cross_location.data;
	if (data != NULL && IS_GO_DATA_SCALAR (data))
		return go_data_scalar_get_value (GO_DATA_SCALAR (data));

	return 0.;
}

GogAxisPosition
gog_axis_base_get_position (GogAxisBase *axis_base)
{
	g_return_val_if_fail (GOG_AXIS_BASE (axis_base) != NULL, GOG_AXIS_AT_LOW);

	return axis_base->position;
}

GSF_CLASS_ABSTRACT (GogAxisBase, gog_axis_base,
		    gog_axis_base_class_init, gog_axis_base_init,
		    GOG_STYLED_OBJECT_TYPE);

/************************************************************************/

#define POINT_MIN_DISTANCE 	5 	/* distance minimum between point and axis for point = TRUE, in pixels */

typedef enum {
	GOG_AXIS_BASE_RENDER,
	GOG_AXIS_BASE_POINT,
	GOG_AXIS_BASE_PADDING_REQUEST
} GogAxisBaseAction;

static gboolean
axis_line_point (double x, double y,
		 double xa, double ya, double wa, double ha)
{
	return go_geometry_point_to_segment (x, y, xa, ya, wa, ha) <= POINT_MIN_DISTANCE;
}

static GogViewAllocation
axis_line_get_bbox (GogAxisBase *axis_base, GogRenderer *renderer,
		    double x, double y, double w, double h,
		    GOGeometrySide side, double start_at, gboolean draw_labels)
{
	GogAxisMap *map = NULL;
	GogAxisTick *ticks;
	GOGeometryAABR total_bbox, bbox;
	GogStyle *style = axis_base->base.style;
	GOGeometryAABR txt_aabr;
	GOGeometryOBR txt_obr;
	double line_width;
	double axis_length, axis_angle, label_padding;
	double cos_alpha, sin_alpha;
	double pos;
	unsigned i, tick_nbr;
	gboolean is_line_visible;
	double minor_tick_len, major_tick_len, tick_len;

	go_geometry_cartesian_to_polar (w, h, &axis_length, &axis_angle);
	cos_alpha = side == GO_SIDE_LEFT ? - sin (axis_angle) : + sin (axis_angle);
	sin_alpha = side == GO_SIDE_LEFT ? + cos (axis_angle) : - cos (axis_angle);

	is_line_visible = gog_style_is_line_visible (style);
	line_width = gog_renderer_line_size (renderer, style->line.width) / 2;

	minor_tick_len = gog_renderer_pt2r (renderer, axis_base->minor.size_pts);
	major_tick_len = gog_renderer_pt2r (renderer, axis_base->major.size_pts);
	tick_len = axis_base->major.tick_out ? major_tick_len :
		(axis_base->minor.tick_out ? minor_tick_len : 0.);
	gog_renderer_get_text_OBR (renderer, "0", &txt_obr);
	label_padding = txt_obr.w;

	total_bbox.x = x; total_bbox.y = y; 
	total_bbox.w = w; total_bbox.h = h;

	if (is_line_visible) {
		double out_len, in_len;

		out_len = line_width;
		if (axis_base->major.tick_out)
			out_len += major_tick_len;
		else if (axis_base->minor.tick_out)
			out_len += minor_tick_len;
		in_len  = line_width;
		if (axis_base->major.tick_in)
			in_len += major_tick_len;
		else if (axis_base->minor.tick_in)
			in_len += minor_tick_len;

		bbox.x = x - out_len * cos_alpha;
		bbox.y = y - out_len * sin_alpha;
		bbox.w = (out_len + in_len) * cos_alpha;
		bbox.h = (out_len + in_len) * sin_alpha;
		go_geometry_AABR_add (&total_bbox, &bbox);
		bbox.x += w;
		bbox.y += h;
		go_geometry_AABR_add (&total_bbox, &bbox);
	}

	tick_nbr = gog_axis_get_ticks (axis_base->axis, &ticks);

	if (!draw_labels)
		return total_bbox;

	map = gog_axis_map_new (axis_base->axis, 0., axis_length);

	for (i = 0; i < tick_nbr; i++) {
		if (ticks[i].label != NULL) {
			pos = gog_axis_map_to_view (map, ticks[i].position);
			gog_renderer_get_text_OBR (renderer, ticks[i].label, &txt_obr);
			txt_obr.w += label_padding;
			go_geometry_calc_label_position (&txt_obr, axis_angle, tick_len, side);
			txt_obr.x += x + pos * cos (axis_angle);
			txt_obr.y += y + pos * sin (axis_angle);
			go_geometry_OBR_to_AABR (&txt_obr, &txt_aabr);
			go_geometry_AABR_add (&total_bbox, &txt_aabr);
		}
	}
	gog_axis_map_free (map);

	return total_bbox;
}

static void
axis_line_render (GogAxisBase *axis_base, GogRenderer *renderer,
		  double x, double y, double w, double h,
		  GOGeometrySide side,
		  double start_at,
		  gboolean draw_labels,
		  gboolean sharp)
{
	GogAxisMap *map = NULL;
	GogAxisTick *ticks;
	GogViewAllocation label_pos;
	GogStyle *style = axis_base->base.style;
	GOGeometryOBR txt_obr, txt_obr_old = {0., 0., 0., 0., 0.};
	ArtVpath path[3];
	double line_width;
	double axis_length, axis_angle, label_padding;
	double major_tick_len, minor_tick_len, tick_len;
	double major_out_x = 0., major_out_y= 0., major_in_x = 0., major_in_y = 0.;
	double minor_out_x = 0., minor_out_y= 0., minor_in_x = 0., minor_in_y = 0.;
	double cos_alpha, sin_alpha;
	double pos, pos_x, pos_y;
	unsigned i, tick_nbr;
	gboolean draw_major, draw_minor;
	gboolean is_line_visible;

	go_geometry_cartesian_to_polar (w, h, &axis_length, &axis_angle);
	cos_alpha = side == GO_SIDE_LEFT ? - sin (axis_angle) : + sin (axis_angle);
	sin_alpha = side == GO_SIDE_LEFT ? + cos (axis_angle) : - cos (axis_angle);
	
	is_line_visible = gog_style_is_line_visible (style);
	line_width = gog_renderer_line_size (renderer, style->line.width) / 2;
	if (is_line_visible)
	{
		path[0].code = ART_MOVETO;
		path[1].code = ART_LINETO;
		path[2].code = ART_END;

		path[0].x = x;
		path[0].y = y;
		path[1].x = path[0].x + w;
		path[1].y = path[0].y + h;
		if (sharp)
			gog_renderer_draw_sharp_path (renderer, path);
		else
			gog_renderer_draw_path (renderer, path);
	}

	map = gog_axis_map_new (axis_base->axis, 0., axis_length);

	draw_major = axis_base->major.tick_in || axis_base->major.tick_out;
	draw_minor = axis_base->minor.tick_in || axis_base->minor.tick_out;

	minor_tick_len = gog_renderer_pt2r (renderer, axis_base->minor.size_pts) + line_width;
	minor_out_x = axis_base->minor.tick_out ? - minor_tick_len * cos_alpha : 0.;
	minor_out_y = axis_base->minor.tick_out ? - minor_tick_len * sin_alpha : 0.;
	minor_in_x = axis_base->minor.tick_in ? minor_tick_len * cos_alpha : 0.;
	minor_in_y = axis_base->minor.tick_in ? minor_tick_len * sin_alpha : 0.;

	major_tick_len = gog_renderer_pt2r (renderer, axis_base->major.size_pts) + line_width;
	major_out_x = axis_base->major.tick_out ? - major_tick_len * cos_alpha : 0.;
	major_out_y = axis_base->major.tick_out ? - major_tick_len * sin_alpha : 0.;
	major_in_x = axis_base->major.tick_in ? major_tick_len * cos_alpha : 0.;
	major_in_y = axis_base->major.tick_in ? major_tick_len * sin_alpha : 0.;

	tick_len = axis_base->major.tick_out ? major_tick_len :
		(axis_base->minor.tick_out ? minor_tick_len : 0.);
	gog_renderer_get_text_OBR (renderer, "0", &txt_obr);
	label_padding = txt_obr.w;

	tick_nbr = gog_axis_get_ticks (axis_base->axis, &ticks);

	for (i = 0; i < tick_nbr; i++) {
		if (gog_axis_map (map, ticks[i].position) < start_at)
			continue;

		pos = gog_axis_map_to_view (map, ticks[i].position);
		pos_x = x + pos * cos (axis_angle);
		pos_y = y + pos * sin (axis_angle);

		if (is_line_visible) {
			switch (ticks[i].type) {
				case GOG_AXIS_TICK_MAJOR:
					if (draw_major) {
						path[0].x = major_out_x + pos_x;
						path[1].x = major_in_x + pos_x;
						path[0].y = major_out_y + pos_y;
						path[1].y = major_in_y + pos_y;
						if (sharp)
							gog_renderer_draw_sharp_path (renderer, path);
						else
							gog_renderer_draw_path (renderer, path);
					}
					break;

				case GOG_AXIS_TICK_MINOR:
					if (draw_minor) {
						path[0].x = minor_out_x + pos_x;
						path[1].x = minor_in_x + pos_x;
						path[0].y = minor_out_y + pos_y;
						path[1].y = minor_in_y + pos_y;
						if (sharp)
							gog_renderer_draw_sharp_path (renderer, path);
						else
							gog_renderer_draw_path (renderer, path);
					}
					break;

				default:
					break;
			}
		}

		if (ticks[i].label != NULL && draw_labels) {
			pos = gog_axis_map_to_view (map, ticks[i].position);
			gog_renderer_get_text_OBR (renderer, ticks[i].label, &txt_obr);
			txt_obr.w += label_padding;
			go_geometry_calc_label_position (&txt_obr, axis_angle, tick_len, side);
			txt_obr.x += x + pos * cos (axis_angle);
			txt_obr.y += y + pos * sin (axis_angle);
			if (!go_geometry_test_OBR_overlap (&txt_obr, &txt_obr_old)) {
				label_pos.x = txt_obr.x;
				label_pos.y = txt_obr.y;
				gog_renderer_draw_text (renderer, ticks[i].label,
							&label_pos, GTK_ANCHOR_CENTER, NULL);
				txt_obr_old = txt_obr;
			}
		}
	}

	gog_axis_map_free (map);
}

static gboolean
axis_circle_point (double x, double y, double center_x, double center_y, double radius, int num_radii)
{

	if (num_radii > 0.0) {
		int i;
		double x0 = center_x;
		double y0 = center_y;
		double x1, y1;
		double angle_rad = 0;

		for (i = 1; i <= num_radii; i++) {
			x1 = x0;
			y1 = y0;
			angle_rad = 2.0 * M_PI * (double) i / (double) num_radii;
			x0 = center_x + radius * sin (angle_rad);
			y0 = center_y - radius * cos (angle_rad);
			if (go_geometry_point_to_segment (x, y, x0, y0, x1 - x0, y1 - y0) < POINT_MIN_DISTANCE)
				return TRUE;
		}
	}

	return (radius - sqrt ((x - center_x) * (x - center_x) + (y - center_y) * (y - center_y))) < POINT_MIN_DISTANCE;
}

static GogViewAllocation
axis_circle_get_bbox (GogAxisBase *axis_base, GogRenderer *renderer,
		      GogChartMap *c_map, gboolean draw_labels)
{
	GogAxisMap *map;
	GogAxisTick *ticks;
	GogViewAllocation total_bbox;
	GogChartMapPolarData *parms = gog_chart_map_get_polar_parms (c_map);
	GOGeometryOBR txt_obr;
	GOGeometryAABR txt_aabr;
	double angle, offset, position, label_padding;
	double major_tick_len, minor_tick_len, tick_len, x, y;
	unsigned i, tick_nbr;
	gboolean draw_ticks;

	total_bbox.x = parms->cx; total_bbox.y = parms->cy; total_bbox.w = 0.; total_bbox.h = 0.;

	minor_tick_len = gog_renderer_pt2r (renderer, axis_base->minor.size_pts);
	major_tick_len = gog_renderer_pt2r (renderer, axis_base->major.size_pts);
	tick_len = axis_base->major.tick_out ? major_tick_len :
		(axis_base->minor.tick_out ? minor_tick_len : 0.);
	gog_renderer_get_text_OBR (renderer, "0", &txt_obr);
	label_padding = txt_obr.w;

	draw_ticks = gog_style_is_line_visible (axis_base->base.style) &&
		(axis_base->major.tick_out || axis_base->minor.tick_out);

	map = gog_chart_map_get_axis_map (c_map, 1);
	gog_axis_map_get_extents (map, &offset , &position);
	map = gog_chart_map_get_axis_map (c_map, 0);
	tick_nbr = gog_axis_get_ticks (axis_base->axis, &ticks);
	for (i = 0; i < tick_nbr; i++) {
		angle = gog_axis_map_to_view (map, ticks[i].position);
		gog_chart_map_2D_to_view (c_map, ticks[i].position, position, &x, &y);

		if (ticks[i].label != NULL && draw_labels) {
			gog_renderer_get_text_OBR (renderer, ticks[i].label, &txt_obr);
			txt_obr.w += label_padding;
			go_geometry_calc_label_position (&txt_obr, angle + M_PI / 2.0, tick_len, GO_SIDE_LEFT);
			txt_obr.x += x;
			txt_obr.y += y;
			go_geometry_OBR_to_AABR (&txt_obr, &txt_aabr);
			go_geometry_AABR_add (&total_bbox, &txt_aabr);
		} else
			if (draw_ticks) {
				txt_aabr.x = x + cos (angle) * tick_len;
				txt_aabr.y = y + sin (angle) * tick_len;
				txt_aabr.w = txt_aabr.h = 0.;
				go_geometry_AABR_add (&total_bbox, &txt_aabr);
			}
	}

	return total_bbox;
}

static void
axis_circle_render (GogAxisBase *axis_base, GogRenderer *renderer,
		    GogChartMap *c_map, gboolean is_discrete, gboolean draw_labels)
{
	GogAxisMap *map;
	GogAxisTick *ticks;
	GogViewAllocation label_pos;
	GogChartMapPolarData *parms = gog_chart_map_get_polar_parms (c_map);
	GOGeometryOBR txt_obr, txt_obr_old = {0., 0., 0., 0., 0.};
	GOGeometryOBR txt_obr_first;
	ArtVpath *cpath, path[3];
	double angle, offset, position, label_padding;
	double start, stop;
	double major_tick_len, minor_tick_len, tick_len;
	unsigned i, step_nbr, tick_nbr;
	gboolean draw_major, draw_minor;
	gboolean is_line_visible;
	gboolean first_label_done = FALSE;

	map = gog_chart_map_get_axis_map (c_map, 1);
	gog_axis_map_get_extents (map, &offset , &position);
	map = gog_chart_map_get_axis_map (c_map, 0);

	if (is_discrete) {
		gog_axis_map_get_extents (map, &start, &stop);
		step_nbr = go_rint (parms->th1 - parms->th0) + 1;
		cpath = art_new (ArtVpath, step_nbr + 2);
		for (i = 0; i <= step_nbr; i++) {
			gog_chart_map_2D_to_view (c_map, i + parms->th0, 
						  position, &cpath[i].x, &cpath[i].y);
			cpath[i].code = ART_LINETO;
		}
		cpath[0].code = ART_MOVETO;
		cpath[step_nbr + 1].code = ART_END;
		gog_renderer_draw_path (renderer, cpath);
		g_free (cpath);
	} else {
		gog_renderer_draw_arc (renderer, parms->cx, parms->cy, parms->rx, parms->ry,
				      -parms->th1, -parms->th0);
	}

	is_line_visible = gog_style_is_line_visible (axis_base->base.style);
	draw_major = axis_base->major.tick_in || axis_base->major.tick_out;
	draw_minor = axis_base->minor.tick_in || axis_base->minor.tick_out;

	if (is_line_visible) {
		path[0].code = ART_MOVETO;
		path[1].code = ART_LINETO;
		path[2].code = ART_END;
	}

	minor_tick_len = gog_renderer_pt2r (renderer, axis_base->minor.size_pts);
	major_tick_len = gog_renderer_pt2r (renderer, axis_base->major.size_pts);
	tick_len = axis_base->major.tick_out ? major_tick_len :
		(axis_base->minor.tick_out ? minor_tick_len : 0.);
	gog_renderer_get_text_OBR (renderer, "0", &txt_obr);
	label_padding = txt_obr.w;

	tick_nbr = gog_axis_get_ticks (axis_base->axis, &ticks);
	for (i = 0; i < tick_nbr; i++) {
		angle = gog_axis_map_to_view (map, ticks[i].position);
		if (is_line_visible) {
			switch (ticks[i].type) {
				case GOG_AXIS_TICK_MAJOR:
					if (draw_major) {
						gog_chart_map_2D_to_view (c_map, ticks[i].position, position,
									  &path[0].x, &path[0].y);
						if (axis_base->major.tick_in) {
							path[1].x = path[0].x - major_tick_len * cos (angle);
							path[1].y = path[0].y - major_tick_len * sin (angle);
						} else {
							path[1].x = path[0].x;
							path[1].y = path[0].y;
						}
						if (axis_base->major.tick_out) {
							path[0].x += major_tick_len * cos (angle);
							path[0].y += major_tick_len * sin (angle);
						}
						gog_renderer_draw_path (renderer, path);
					}
					break;

				case GOG_AXIS_TICK_MINOR:
					if (draw_minor) {
						gog_chart_map_2D_to_view (c_map, ticks[i].position, position,
									  &path[0].x, &path[0].y);
						if (axis_base->minor.tick_in) {
							path[1].x = path[0].x - minor_tick_len * cos (angle);
							path[1].y = path[0].y - minor_tick_len * sin (angle);
						} else {
							path[1].x = path[0].x;
							path[1].y = path[0].y;
						}
						if (axis_base->minor.tick_out) {
							path[0].x += minor_tick_len * cos (angle);
							path[0].y += minor_tick_len * sin (angle);
						}
						gog_renderer_draw_path (renderer, path);
					}
					break;

				default:
					break;
			}
		}

		if (ticks[i].label != NULL && draw_labels) {
			gog_chart_map_2D_to_view (c_map, ticks[i].position, position,
						  &label_pos.x, &label_pos.y);
			gog_renderer_get_text_OBR (renderer, ticks[i].label, &txt_obr);
			txt_obr.w += label_padding;
			go_geometry_calc_label_position (&txt_obr, angle + M_PI / 2.0, tick_len, GO_SIDE_LEFT);
			label_pos.x += txt_obr.x;
			label_pos.y += txt_obr.y;
			txt_obr.x = label_pos.x;
			txt_obr.y = label_pos.y;
			if (!first_label_done || 
			    (!go_geometry_test_OBR_overlap (&txt_obr, &txt_obr_old) &&
			     !go_geometry_test_OBR_overlap (&txt_obr, &txt_obr_first))) {
				gog_renderer_draw_text (renderer, ticks[i].label,
							&label_pos, GTK_ANCHOR_CENTER, NULL);
				txt_obr_old = txt_obr;
			}
			if (!first_label_done) {
				txt_obr_first = txt_obr;
				first_label_done = TRUE;
			}
		}
	}
}

static gboolean
x_process (GogAxisBaseAction action, GogView *view, GogViewPadding *padding,
	   GogViewAllocation const *plot_area, double x, double y)
{
	GogAxisBase *axis_base = GOG_AXIS_BASE (view->model);
	GogAxisType axis_type = gog_axis_get_atype (axis_base->axis);
	GogChartMap *c_map;
	GogAxisMap *a_map;
	GogViewAllocation tmp = *plot_area;
	GogViewAllocation axis_line_bbox;
	double ax, ay, bx, by;
	double start, stop;

	g_return_val_if_fail (axis_type == GOG_AXIS_X, FALSE);

	c_map = gog_chart_map_new (axis_base->chart, plot_area, axis_base->axis, NULL, NULL, TRUE);
	a_map = gog_chart_map_get_axis_map (c_map, 0);

	gog_axis_map_get_extents (a_map, &start, &stop);
	gog_chart_map_2D_to_view (c_map, start, 0, &ax, &ay);
	gog_chart_map_2D_to_view (c_map, stop, 0, &bx, &by);

	gog_chart_map_free (c_map);

	switch (action) {
		case GOG_AXIS_BASE_RENDER:
			axis_line_render (GOG_AXIS_BASE (view->model),
				view->renderer, ax, ay, bx - ax , by - ay, 
				GO_SIDE_RIGHT, -1.,
				axis_base->major_tick_labeled, TRUE);
			break;

		case GOG_AXIS_BASE_PADDING_REQUEST:
			axis_line_bbox = axis_line_get_bbox (GOG_AXIS_BASE (view->model),
							     view->renderer, ax, ay, bx - ax, by - ay, 
							     GO_SIDE_RIGHT, -1.,
							     axis_base->major_tick_labeled);
			padding->wl = MAX (0., tmp.x - axis_line_bbox.x);
			padding->ht = MAX (0., tmp.y - axis_line_bbox.y);
			padding->wr = MAX (0., axis_line_bbox.x + axis_line_bbox.w - tmp.x - tmp.w);
			padding->hb = MAX (0., axis_line_bbox.y + axis_line_bbox.h - tmp.y - tmp.h);
			break;

		case GOG_AXIS_BASE_POINT:
			return axis_line_point (x, y, ax, ay, bx - ax, by - ay);
			break;
	}

	return FALSE;
}

static gboolean
xy_process (GogAxisBaseAction action, GogView *view, GogViewPadding *padding,
	    GogViewAllocation const *plot_area, double x, double y)
{
	GogAxisBase *axis_base = GOG_AXIS_BASE (view->model);
	GogAxis *cross_axis;
	GogChartMap *c_map;
	GogAxisMap *a_map;
	GogViewAllocation tmp = *plot_area;
	GogViewAllocation axis_line_bbox;
	double ax, ay, bx, by;
	GogAxisType axis_type = gog_axis_get_atype (axis_base->axis);
	double position;
	double minimum, maximum, start, stop;
	GOGeometrySide side;

	g_return_val_if_fail (axis_type == GOG_AXIS_X ||
			      axis_type == GOG_AXIS_Y, FALSE);

	cross_axis = gog_axis_base_get_crossed_axis (axis_base);
	if (axis_type == GOG_AXIS_X) {
		c_map = gog_chart_map_new (axis_base->chart, plot_area, axis_base->axis, cross_axis, NULL, TRUE);
		a_map = gog_chart_map_get_axis_map (c_map, 1);
	} else {
		c_map = gog_chart_map_new (axis_base->chart, plot_area, cross_axis, axis_base->axis, NULL, TRUE);
		a_map = gog_chart_map_get_axis_map (c_map, 0);
	}

	gog_axis_map_get_extents (a_map, &start, &stop);
	gog_axis_map_get_bounds (a_map, &minimum, &maximum);
	if (axis_base->position == GOG_AXIS_CROSS) {
		position = gog_axis_base_get_cross_location (axis_base);
		if (position < minimum || position > maximum) {
			gog_chart_map_free (c_map);
			return FALSE;
		}
	} else
		position = axis_base->position == GOG_AXIS_AT_LOW ?  start : stop;

	side = axis_base->position == GOG_AXIS_AT_LOW ? GO_SIDE_RIGHT : GO_SIDE_LEFT;

	if (axis_type == GOG_AXIS_X) {
		a_map = gog_chart_map_get_axis_map (c_map, 0);
		gog_axis_map_get_extents (a_map, &start, &stop);
		gog_chart_map_2D_to_view (c_map, start, position, &ax, &ay);
		gog_chart_map_2D_to_view (c_map, stop, position, &bx, &by);
	} else {
		a_map = gog_chart_map_get_axis_map (c_map, 1);
		gog_axis_map_get_extents (a_map, &start, &stop);
		gog_chart_map_2D_to_view (c_map, position, start, &ax, &ay);
		gog_chart_map_2D_to_view (c_map, position, stop, &bx, &by);
		side = (side == GO_SIDE_LEFT) ? GO_SIDE_RIGHT : GO_SIDE_LEFT;
	}
	gog_chart_map_free (c_map);

	switch (action) {
		case GOG_AXIS_BASE_RENDER:
			axis_line_render (GOG_AXIS_BASE (view->model),
				view->renderer, ax, ay, bx - ax , by - ay, side, -1.,
				axis_base->major_tick_labeled, TRUE);
			break;

		case GOG_AXIS_BASE_PADDING_REQUEST:
			axis_line_bbox = axis_line_get_bbox (GOG_AXIS_BASE (view->model),
							     view->renderer, ax, ay, bx - ax, by - ay, side, -1.,
							     axis_base->major_tick_labeled);
			padding->wl = MAX (0., tmp.x - axis_line_bbox.x);
			padding->ht = MAX (0., tmp.y - axis_line_bbox.y);
			padding->wr = MAX (0., axis_line_bbox.x + axis_line_bbox.w - tmp.x - tmp.w);
			padding->hb = MAX (0., axis_line_bbox.y + axis_line_bbox.h - tmp.y - tmp.h);
			break;

		case GOG_AXIS_BASE_POINT:
			return axis_line_point (x, y, ax, ay, bx - ax, by - ay);
			break;
	}

	return FALSE;
}

static gboolean
radar_process (GogAxisBaseAction action, GogView *view, GogViewPadding *padding,
	       GogViewAllocation const *area, double x, double y)
{
	GogAxisBase *axis_base = GOG_AXIS_BASE (view->model);
	GogAxis *cross_axis;
	GogChartMap *c_map;
	GogAxisMap *a_map;
	GogAxisType axis_type = gog_axis_get_atype (axis_base->axis);
	GogChartMapPolarData *parms;
	GogViewAllocation tmp = *area;
	GogViewAllocation bbox;
	GOGeometrySide side;
	double start, stop, minimum, maximum;
	double bx, by, position;
	unsigned i;
	gboolean point = FALSE;

	g_return_val_if_fail (axis_type == GOG_AXIS_CIRCULAR ||
			      axis_type == GOG_AXIS_RADIAL, FALSE);

	cross_axis = gog_axis_base_get_crossed_axis (axis_base);

	if (axis_type == GOG_AXIS_RADIAL) {
		c_map = gog_chart_map_new (axis_base->chart, area, cross_axis, axis_base->axis, NULL,
					   action == GOG_AXIS_BASE_PADDING_REQUEST);
		parms = gog_chart_map_get_polar_parms (c_map);
		a_map = gog_chart_map_get_axis_map (c_map, 0);
		gog_axis_map_get_bounds (a_map, &minimum, &maximum);
		gog_axis_map_get_extents (a_map, &start, &stop);
		if (axis_base->position == GOG_AXIS_CROSS) {
			position = gog_axis_base_get_cross_location (axis_base);
			if (position < minimum || position > maximum) {
				gog_chart_map_free (c_map);
				return FALSE;
			}
		} else
			position = axis_base->position == GOG_AXIS_AT_LOW ?  start : stop;
		side = axis_base->position == GOG_AXIS_AT_LOW ? GO_SIDE_RIGHT : GO_SIDE_LEFT;

		a_map = gog_chart_map_get_axis_map (c_map, 1);
		gog_axis_map_get_extents (a_map, &start, &stop);

		switch (action) {
			case GOG_AXIS_BASE_RENDER:
				if (gog_axis_is_discrete (cross_axis))
					for (i = parms->th0; i <= parms->th1; i++) {
					       	gog_chart_map_2D_to_view (c_map, i, stop, &bx, &by);
						axis_line_render (axis_base, view->renderer,
								  parms->cx, parms->cy,
								  bx - parms->cx, by - parms->cy,
								  side, 0.1, i == parms->th0 && axis_base->major_tick_labeled,
								  FALSE);
					} else {
					       	gog_chart_map_2D_to_view (c_map, position, stop, &bx, &by);
						axis_line_render (axis_base, view->renderer,
								  parms->cx, parms->cy,
								  bx - parms->cx, by - parms->cy,
								  side, 0., axis_base->major_tick_labeled,
								  FALSE);
					}
				break;
			case GOG_AXIS_BASE_PADDING_REQUEST:
				if (gog_axis_is_discrete (cross_axis)) break;

				gog_chart_map_2D_to_view (c_map, position, stop, &bx, &by);
				bbox = axis_line_get_bbox (axis_base,
					view->renderer, parms->cx, parms->cy,
					bx - parms->cx, by - parms->cy, side, -1.,
					axis_base->major_tick_labeled);
				padding->wl = MAX (0., tmp.x - bbox.x);
				padding->ht = MAX (0., tmp.y - bbox.y);
				padding->wr = MAX (0., bbox.x + bbox.w - tmp.x - tmp.w);
				padding->hb = MAX (0., bbox.y + bbox.h - tmp.y - tmp.h);
				break;
			case GOG_AXIS_BASE_POINT:
				if (gog_axis_is_discrete (cross_axis))
					for (i = parms->th0; i <= parms->th1; i++) {
						gog_chart_map_2D_to_view (c_map, i, stop, &bx, &by);
						point = axis_line_point (x, y, parms->cx, parms->cy,
								     bx - parms->cx, by - parms->cy);
						if (point)
							break;
					}
				else {
					gog_chart_map_2D_to_view (c_map, position, stop, &bx, &by);
					point = axis_line_point (x, y, parms->cx, parms->cy,
								 bx - parms->cx, by - parms->cy);
				}
				break;
		}
		gog_chart_map_free (c_map);
	} else {
		c_map = gog_chart_map_new (axis_base->chart, area, axis_base->axis, cross_axis, NULL,
					   action == GOG_AXIS_BASE_PADDING_REQUEST);
		parms = gog_chart_map_get_polar_parms (c_map);

		switch (action) {
			case GOG_AXIS_BASE_RENDER:
				axis_circle_render (GOG_AXIS_BASE (view->model), view->renderer,
						    c_map, gog_axis_is_discrete (axis_base->axis),
						    axis_base->major_tick_labeled);
				break;
			case GOG_AXIS_BASE_PADDING_REQUEST:
				bbox = axis_circle_get_bbox (axis_base, view->renderer, c_map,
							     axis_base->major_tick_labeled);
				padding->wl = MAX (0., tmp.x - bbox.x);
				padding->ht = MAX (0., tmp.y - bbox.y);
				padding->wr = MAX (0., bbox.x + bbox.w - tmp.x - tmp.w);
				padding->hb = MAX (0., bbox.y + bbox.h - tmp.y - tmp.h);
				break;
			case GOG_AXIS_BASE_POINT:
				point = axis_circle_point (x, y, parms->cx, parms->cy, parms->rx, parms->th1);
				break;
		}
		gog_chart_map_free (c_map);
	}
	return point;
}

static gboolean
gog_axis_base_view_info_at_point (GogView *view, double x, double y,
				  GogObject const *cur_selection,
				  GogObject **obj, char **name)
{
	GogAxisBase *axis_base = GOG_AXIS_BASE (view->model);
	GogAxisSet axis_set = gog_chart_get_axis_set (axis_base->chart);
	gboolean pointed = FALSE;
	GogViewAllocation const *plot_area;

	/* FIXME: not nice */
	if (IS_GOG_AXIS (view->model))
		plot_area = gog_chart_view_get_plot_area (view->parent);
	else
		plot_area = gog_chart_view_get_plot_area (view->parent->parent);

	switch (axis_set) {
		case GOG_AXIS_SET_X:
			pointed = x_process (GOG_AXIS_BASE_POINT, view, NULL, plot_area, x, y);
			break;
		case GOG_AXIS_SET_XY:
			pointed = xy_process (GOG_AXIS_BASE_POINT, view, NULL, plot_area, x, y);
			break;
		case GOG_AXIS_SET_XY_pseudo_3d:
			if (gog_axis_get_atype (axis_base->axis) != GOG_AXIS_PSEUDO_3D)
				pointed = xy_process (GOG_AXIS_BASE_POINT, view, NULL, plot_area, x, y);
			break;
		case GOG_AXIS_SET_RADAR:
			pointed = radar_process (GOG_AXIS_BASE_POINT, view, NULL, plot_area, x, y);
			break;
		case GOG_AXIS_SET_UNKNOWN:
			break;
		default:
			g_warning ("[AxisBaseView::info_at_point] not implemented for this axis set (%i)",
				   axis_set);
			break;
	}

	if (pointed) {
		if (obj != NULL)
			*obj = view->model;
		if (name != NULL)
			*name = NULL;
		return TRUE;
	}

	return FALSE;
}

static void
gog_axis_base_view_padding_request (GogView *view, GogViewAllocation const *bbox, GogViewPadding *padding)
{
	GogAxisSet axis_set;
	GogAxisBase *axis_base = GOG_AXIS_BASE (view->model);
	GogStyle *style = axis_base->base.style;

	axis_set = gog_chart_get_axis_set (axis_base->chart);

	gog_renderer_push_style (view->renderer, style);

	switch (axis_set) {
		case GOG_AXIS_SET_X:
			x_process (GOG_AXIS_BASE_PADDING_REQUEST, view, padding, bbox, 0., 0.);
			break;
		case GOG_AXIS_SET_XY:
			xy_process (GOG_AXIS_BASE_PADDING_REQUEST, view, padding, bbox, 0., 0.);
			break;
		case GOG_AXIS_SET_XY_pseudo_3d:
			if (gog_axis_get_atype (axis_base->axis) != GOG_AXIS_PSEUDO_3D)
				xy_process (GOG_AXIS_BASE_PADDING_REQUEST, view, padding, bbox, 0., 0.);
			break;
		case GOG_AXIS_SET_RADAR:
			radar_process (GOG_AXIS_BASE_PADDING_REQUEST, view, padding, bbox, 0., 0.);
			break;
		case GOG_AXIS_SET_UNKNOWN:
			break;
		default:
			g_warning ("[AxisBaseView::padding_request] not implemented for this axis set (%i)",
				   axis_set);
			break;
	}

	gog_renderer_pop_style (view->renderer);
}

static void
gog_axis_base_view_render (GogView *view, GogViewAllocation const *bbox)
{
	GogAxisSet axis_set;
	GogAxisBase *axis_base = GOG_AXIS_BASE (view->model);
	GogStyle *style = axis_base->base.style;
	GogViewAllocation const *plot_area;

	axis_set = gog_chart_get_axis_set (axis_base->chart);
	/* FIXME: not nice */
	if (IS_GOG_AXIS (view->model))
		plot_area = gog_chart_view_get_plot_area (view->parent);
	else
		plot_area = gog_chart_view_get_plot_area (view->parent->parent);

	gog_renderer_push_style (view->renderer, style);

	switch (axis_set) {
		case GOG_AXIS_SET_X:
			x_process (GOG_AXIS_BASE_RENDER, view, NULL, plot_area, 0., 0.);
			break;
		case GOG_AXIS_SET_XY:
			xy_process (GOG_AXIS_BASE_RENDER, view, NULL, plot_area, 0., 0.);
			break;
		case GOG_AXIS_SET_XY_pseudo_3d:
			if (gog_axis_get_atype (axis_base->axis) != GOG_AXIS_PSEUDO_3D)
				xy_process (GOG_AXIS_BASE_RENDER, view, NULL, plot_area, 0., 0.);
			break;
		case GOG_AXIS_SET_RADAR:
			radar_process (GOG_AXIS_BASE_RENDER, view, NULL, plot_area, 0., 0.);
			break;
		case GOG_AXIS_SET_UNKNOWN:
			break;
		default:
			g_warning ("[AxisBaseView::render] not implemented for this axis set (%i)",
				   axis_set);
			break;
	}

	gog_renderer_pop_style (view->renderer);
}

static void
gog_axis_base_view_class_init (GogAxisBaseViewClass *gview_klass)
{
	GogViewClass *view_klass = (GogViewClass *) gview_klass;

	gab_view_parent_klass = g_type_class_peek_parent (gview_klass);

	view_klass->info_at_point	= gog_axis_base_view_info_at_point;
	view_klass->padding_request 	= gog_axis_base_view_padding_request;
	view_klass->render 		= gog_axis_base_view_render;
}

GSF_CLASS (GogAxisBaseView, gog_axis_base_view,
	   gog_axis_base_view_class_init, NULL,
	   GOG_VIEW_TYPE)

/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/

struct _GogAxisLine {
	GogAxisBase	base;
};

typedef GogAxisBaseClass GogAxisLineClass;

static GObjectClass *gal_parent_klass;

static void
gog_axis_line_class_init (GObjectClass *gobject_klass)
{
	gal_parent_klass = g_type_class_peek_parent (gobject_klass);
}

static void
gog_axis_line_dataset_dims (GogDataset const *set, int *first, int *last)
{
	*first = GOG_AXIS_ELEM_CROSS_POINT;
	*last  = GOG_AXIS_ELEM_CROSS_POINT;
}

static GogDatasetElement *
gog_axis_line_dataset_get_elem (GogDataset const *set, int dim_i)
{
	GogAxisBase *axis_base = GOG_AXIS_BASE (set);

	g_return_val_if_fail (dim_i == GOG_AXIS_ELEM_CROSS_POINT, NULL);

	return &axis_base->cross_location;
}

static void
gog_axis_line_dim_changed (GogDataset *set, int dim_i)
{
	gog_object_emit_changed (GOG_OBJECT (set), TRUE);
}

static void
gog_axis_line_dataset_init (GogDatasetClass *iface)
{
	iface->dims	   = gog_axis_line_dataset_dims;
	iface->get_elem	   = gog_axis_line_dataset_get_elem;
	iface->dim_changed = gog_axis_line_dim_changed;
}

GSF_CLASS_FULL (GogAxisLine, gog_axis_line,
		NULL, NULL, gog_axis_line_class_init, NULL,
		NULL /*init*/, GOG_AXIS_BASE_TYPE, 0,
		GSF_INTERFACE (gog_axis_line_dataset_init, GOG_DATASET_TYPE))
