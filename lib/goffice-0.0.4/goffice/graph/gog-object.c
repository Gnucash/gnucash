/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-object.c :
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
#include <goffice/graph/gog-object.h>
#include <goffice/graph/gog-graph-impl.h> /* for gog_graph_request_update */
#include <goffice/graph/gog-data-set.h>
#include <goffice/data/go-data.h>
#include <goffice/gtk/goffice-gtk.h>

#include <gsf/gsf-impl-utils.h>
#include <glib/gi18n.h>
#include <string.h>
#include <stdlib.h>

#include <gtk/gtkcombobox.h>
#include <gtk/gtklabel.h>
#include <gtk/gtknotebook.h>
#include <gtk/gtksizegroup.h>
#include <gtk/gtkspinbutton.h>
#include <gtk/gtktogglebutton.h>
#include <gtk/gtkwidget.h>

GogEditor *
gog_editor_new (void)
{
	GogEditor *editor = g_new (GogEditor, 1);

	editor->store_page = NULL;
	editor->pages = NULL;

	return editor;
}

void
gog_editor_add_page (GogEditor *editor, gpointer widget, char const *label)
{
	GogEditorPage *page;

	g_return_if_fail (editor != NULL);
	page = g_new (GogEditorPage, 1);

	page->widget = widget;
	page->label = label;

	editor->pages = g_slist_prepend (editor->pages, page);
}

void
gog_editor_set_store_page (GogEditor *editor, unsigned *store_page)
{
	g_return_if_fail (editor != NULL);

	editor->store_page = store_page;
}

static void
cb_switch_page (G_GNUC_UNUSED GtkNotebook *n, G_GNUC_UNUSED GtkNotebookPage *p,
		guint page_num, guint *store_page)
{
		*store_page = page_num;
}

gpointer
gog_editor_get_notebook (GogEditor *editor)
{
	GtkWidget *notebook;
	GogEditorPage *page;
	GSList *ptr;
	unsigned page_count = 0;

	notebook = gtk_notebook_new ();
	if (editor->pages != NULL) {
		for (ptr = editor->pages; ptr != NULL; ptr = ptr->next) {
			page = (GogEditorPage *) ptr->data;
			gtk_notebook_prepend_page (GTK_NOTEBOOK (notebook),
						   GTK_WIDGET (page->widget),
						   gtk_label_new (page->label));
			gtk_widget_show (page->widget);
			page_count ++;
		}
	} else {
		/* Display a blank page */
		GtkWidget *label =  gtk_label_new (NULL);
		gtk_notebook_prepend_page (GTK_NOTEBOOK (notebook),
					   label, NULL);
		gtk_widget_show (label);
		page_count = 1;
	}

	if (page_count == 1)
		gtk_notebook_set_show_tabs (GTK_NOTEBOOK (notebook), FALSE);

	if (editor->store_page != NULL) {
		gtk_notebook_set_current_page (GTK_NOTEBOOK (notebook), *editor->store_page);
		g_signal_connect (G_OBJECT (notebook),
				  "switch_page",
				  G_CALLBACK (cb_switch_page), editor->store_page);
	} else
		gtk_notebook_set_current_page (GTK_NOTEBOOK (notebook), 0);

	return notebook;
}

void
gog_editor_free (GogEditor *editor)
{
	g_slist_foreach (editor->pages, (GFunc) g_free, NULL);
	g_slist_free (editor->pages);

	g_free (editor);
}

typedef struct {
	char const *label;
	char const *value;
	unsigned const flags; 
} GogPositionFlagDesc;

static GogPositionFlagDesc const position_compass[] = {
	{N_("Top"), 		"top",		GOG_POSITION_N},
	{N_("Top right"), 	"top-righ",	GOG_POSITION_N|GOG_POSITION_E},
	{N_("Right"), 		"right",	GOG_POSITION_E},
	{N_("Bottom right"), 	"bottom-righ",	GOG_POSITION_E|GOG_POSITION_S},
	{N_("Bottom"),		"bottom",	GOG_POSITION_S},
	{N_("Bottom left"),	"bottom-left",	GOG_POSITION_S|GOG_POSITION_W},
	{N_("Left"),		"left",		GOG_POSITION_W},
	{N_("Top left"),	"top-left",	GOG_POSITION_W|GOG_POSITION_N}
};

static GogPositionFlagDesc const position_alignment[] = {
	{N_("Fill"), 	"fill",		GOG_POSITION_ALIGN_FILL},
	{N_("Start"), 	"start",	GOG_POSITION_ALIGN_START},
	{N_("End"), 	"end",		GOG_POSITION_ALIGN_END},
	{N_("Center"), 	"center",	GOG_POSITION_ALIGN_CENTER}
};

static GogPositionFlagDesc const position_anchor[] = {
	{N_("Top left"), 	"top-left",	GOG_POSITION_ANCHOR_NW},
	{N_("Top"), 		"top",		GOG_POSITION_ANCHOR_N},
	{N_("Top right"), 	"top-right",	GOG_POSITION_ANCHOR_NE},
	{N_("Left"), 		"left",		GOG_POSITION_ANCHOR_W},
	{N_("Center"), 		"center",	GOG_POSITION_ANCHOR_CENTER},
	{N_("Right"), 		"right",	GOG_POSITION_ANCHOR_E},
	{N_("Bottom left"), 	"bottom-left",	GOG_POSITION_ANCHOR_SW},
	{N_("Bottom"), 		"bottom",	GOG_POSITION_ANCHOR_S},
	{N_("Bottom right"),	"bottom-right",	GOG_POSITION_ANCHOR_SE}
};

enum {
	OBJECT_PROP_0,
	OBJECT_PROP_ID,
	OBJECT_PROP_POSITION,
	OBJECT_PROP_POSITION_COMPASS,
	OBJECT_PROP_POSITION_ALIGNMENT,
	OBJECT_PROP_POSITION_IS_MANUAL,
	OBJECT_PROP_POSITION_ANCHOR,
};

enum {
	CHILD_ADDED,
	CHILD_REMOVED,
	CHILD_NAME_CHANGED,
	CHILDREN_REORDERED,
	NAME_CHANGED,
	CHANGED,
	LAST_SIGNAL
};
static gulong gog_object_signals [LAST_SIGNAL] = { 0, };

static GObjectClass *parent_klass;

static void gog_object_set_id (GogObject *obj, unsigned id);

static void
gog_object_finalize (GObject *gobj)
{
	GogObject *obj = GOG_OBJECT (gobj);

	g_free (obj->user_name); obj->user_name = NULL;
	g_free (obj->auto_name); obj->auto_name = NULL;

	g_slist_foreach (obj->children, (GFunc) g_object_unref, NULL);
	g_slist_free (obj->children);
	obj->children = NULL;

	(parent_klass->finalize) (gobj);
}

static void
gog_object_parent_changed (GogObject *child, gboolean was_set)
{
	GSList *ptr = child->children;
	for (; ptr != NULL ; ptr = ptr->next) {
		GogObjectClass *klass = GOG_OBJECT_GET_CLASS (ptr->data);
		(*klass->parent_changed) (ptr->data, was_set);
	}

	if (IS_GOG_DATASET (child))
		gog_dataset_parent_changed (GOG_DATASET (child), was_set);
}

static void
gog_object_set_property (GObject *obj, guint param_id,
			 GValue const *value, GParamSpec *pspec)
{
	GogObject *gobj = GOG_OBJECT (obj);
	char const *str;
	char **str_doubles;
	unsigned id;

	switch (param_id) {
	case OBJECT_PROP_ID:
		id = g_value_get_uint (value);
		gog_object_set_id (gobj, id);
		break;
	case OBJECT_PROP_POSITION:
		str = g_value_get_string (value);
		str_doubles = g_strsplit (str, " ", 4);
		if (g_strv_length (str_doubles) != 4) {
			g_strfreev (str_doubles);
			break;
		}
		gobj->manual_position.x = g_ascii_strtod (str_doubles[0], NULL);
		gobj->manual_position.y = g_ascii_strtod (str_doubles[1], NULL);
		gobj->manual_position.w = g_ascii_strtod (str_doubles[2], NULL);
		gobj->manual_position.h = g_ascii_strtod (str_doubles[3], NULL);
		g_strfreev (str_doubles);
		break;
	case OBJECT_PROP_POSITION_COMPASS:
		str = g_value_get_string (value);
		if (str == NULL)
			break;
		for (id = 0; id < G_N_ELEMENTS (position_compass); id++)
			if (strcmp (str, position_compass[id].value) == 0) 
				break;
		if (id < G_N_ELEMENTS (position_compass))
			gog_object_set_position_flags (gobj, 
						       position_compass[id].flags, 
						       GOG_POSITION_COMPASS);
		break;
	case OBJECT_PROP_POSITION_ALIGNMENT:
		str = g_value_get_string (value);
		if (str == NULL)
			break;
		for (id = 0; id < G_N_ELEMENTS (position_alignment); id++)
			if (strcmp (str, position_alignment[id].value) == 0) 
				break;
		if (id < G_N_ELEMENTS (position_alignment))
			gog_object_set_position_flags (gobj, 
						       position_alignment[id].flags, 
						       GOG_POSITION_ALIGNMENT);
		break;
	case OBJECT_PROP_POSITION_IS_MANUAL:
		gog_object_set_position_flags (gobj,
			g_value_get_boolean (value) ? GOG_POSITION_MANUAL : 0,
			GOG_POSITION_MANUAL);
		break;
	case OBJECT_PROP_POSITION_ANCHOR:
		str = g_value_get_string (value);
		if (str == NULL) 
			break;
		for (id = 0; id < G_N_ELEMENTS (position_anchor); id++)
			if (strcmp (str, position_anchor[id].value) == 0) 
				break;
		if (id < G_N_ELEMENTS (position_anchor))
			gog_object_set_position_flags (gobj, 
						       position_anchor[id].flags, 
						       GOG_POSITION_ANCHOR);
		break;

	default: G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, param_id, pspec);
		 return; /* NOTE : RETURN */
	}
}

static void
gog_object_get_property (GObject *obj, guint param_id,
		       GValue *value, GParamSpec *pspec)
{
	GogObject *gobj = GOG_OBJECT (obj);
	GogObjectPosition flags;
	GString *string;
	char buffer[G_ASCII_DTOSTR_BUF_SIZE];
	unsigned i;

	switch (param_id) {
	case OBJECT_PROP_ID:
		g_value_set_uint (value, GOG_OBJECT (obj)->id);
		break;
	case OBJECT_PROP_POSITION:
		string = g_string_new ("");
		g_string_append (string, g_ascii_dtostr (buffer, sizeof (buffer), gobj->manual_position.x));
		g_string_append_c (string, ' ');
		g_string_append (string, g_ascii_dtostr (buffer, sizeof (buffer), gobj->manual_position.y));
		g_string_append_c (string, ' ');
		g_string_append (string, g_ascii_dtostr (buffer, sizeof (buffer), gobj->manual_position.w));
		g_string_append_c (string, ' ');
		g_string_append (string, g_ascii_dtostr (buffer, sizeof (buffer), gobj->manual_position.h));
		g_value_set_string (value, string->str);
		g_string_free (string, TRUE);
		break;
	case OBJECT_PROP_POSITION_COMPASS:
		flags = gog_object_get_position_flags (GOG_OBJECT (obj), GOG_POSITION_COMPASS);
		for (i = 0; i < G_N_ELEMENTS (position_compass); i++)
			if (position_compass[i].flags == flags) {
				g_value_set_string (value, position_compass[i].value);
				break;
			}
		break;	
	case OBJECT_PROP_POSITION_ALIGNMENT:
		flags = gog_object_get_position_flags (GOG_OBJECT (obj), GOG_POSITION_ALIGNMENT);
		for (i = 0; i < G_N_ELEMENTS (position_alignment); i++)
			if (position_alignment[i].flags == flags) {
				g_value_set_string (value, position_alignment[i].value);
				break;
			}
		break;	
	case OBJECT_PROP_POSITION_IS_MANUAL:
		g_value_set_boolean (value, (gobj->position & GOG_POSITION_MANUAL) != 0);
		break;
	case OBJECT_PROP_POSITION_ANCHOR:
		flags = gog_object_get_position_flags (GOG_OBJECT (obj), GOG_POSITION_ANCHOR);
		for (i = 0; i < G_N_ELEMENTS (position_anchor); i++)
			if (position_anchor[i].flags == flags) {
				g_value_set_string (value, position_anchor[i].value);
				break;
			}
		break;

	default: G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, param_id, pspec);
		 break;
	}
}

typedef struct {
	GtkWidget	*x_spin, *y_spin, *w_spin, *h_spin;
	GtkWidget	*manual_toggle;
	GogObject	*gobj;
	GladeXML	*gui;
} ObjectPrefState;

static void
object_pref_state_free (ObjectPrefState *state) 
{
	g_object_unref (state->gobj);
	g_object_unref (state->gui);
}


static void
cb_compass_changed (GtkComboBox *combo, ObjectPrefState *state)
{
	GogObjectPosition position = position_compass[gtk_combo_box_get_active (combo)].flags;

	gog_object_set_position_flags (state->gobj, position, GOG_POSITION_COMPASS);
}

static void
cb_alignment_changed (GtkComboBox *combo, ObjectPrefState *state)
{
	GogObjectPosition position = position_alignment[gtk_combo_box_get_active (combo)].flags;

	gog_object_set_position_flags (state->gobj, position, GOG_POSITION_ALIGNMENT);
}

static void
cb_position_changed (GtkWidget *spin, ObjectPrefState *state)
{
	GogViewAllocation pos;
	double value = gtk_spin_button_get_value (GTK_SPIN_BUTTON (spin)) / 100.0;

       	gog_object_get_manual_position (state->gobj, &pos);
	if (spin == state->x_spin)
		pos.x = value;
	else if (spin == state->y_spin)
		pos.y = value;
	else if (spin == state->w_spin)
		pos.w = value;
	else if (spin == state->h_spin)
		pos.h = value;
	gog_object_set_manual_position (state->gobj, &pos);
	if (state->manual_toggle != NULL)
		gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (state->manual_toggle), TRUE);
}

static void
cb_manual_position_changed (GtkToggleButton *button, ObjectPrefState *state)
{
	gog_object_set_position_flags (state->gobj, 
		gtk_toggle_button_get_active (button) ? GOG_POSITION_MANUAL : 0, 
		GOG_POSITION_MANUAL);
}

static void
cb_anchor_changed (GtkComboBox *combo, ObjectPrefState *state)
{
	GogObjectPosition position = position_anchor[gtk_combo_box_get_active (combo)].flags;

	gog_object_set_position_flags (state->gobj, position, GOG_POSITION_ANCHOR);
	if (state->manual_toggle != NULL)
		gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (state->manual_toggle), TRUE);
}

static void
gog_object_populate_editor (GogObject *gobj, 
			    GogEditor *editor, 
			    G_GNUC_UNUSED GogDataAllocator *dalloc, 
			    GOCmdContext *cc)
{
	GtkWidget *w;
	GtkSizeGroup *widget_size_group, *label_size_group;
	GladeXML *gui;
	GogObjectClass *gog_klass;
	GogObjectPosition allowable_positions, flags;
	ObjectPrefState *state;
	unsigned i;
	
	if (gobj->role == NULL) 
		return;

	gog_klass = GOG_OBJECT_GET_CLASS (gobj);
	
       	allowable_positions = gobj->role->allowable_positions;
	if (!(allowable_positions & (GOG_POSITION_MANUAL | GOG_POSITION_COMPASS)))
		return;	

	gui = go_libglade_new ("gog-object-prefs.glade", "gog_object_prefs", NULL, cc);
	if (gui == NULL)
		return;

	state = g_new (ObjectPrefState, 1);
	state->gobj = gobj;
	state->gui = gui;
	state->manual_toggle = NULL;
	g_object_ref (G_OBJECT (gobj));

	widget_size_group = gtk_size_group_new (GTK_SIZE_GROUP_HORIZONTAL);
	label_size_group = gtk_size_group_new (GTK_SIZE_GROUP_HORIZONTAL);

	if (allowable_positions & GOG_POSITION_COMPASS) {
		w = glade_xml_get_widget (gui, "position_combo");
		gtk_size_group_add_widget (widget_size_group, w);
		flags = gog_object_get_position_flags (gobj, GOG_POSITION_COMPASS);
		for (i = 0; i < G_N_ELEMENTS (position_compass); i++) { 
			gtk_combo_box_append_text (GTK_COMBO_BOX (w), _(position_compass[i].label));
			if (position_compass[i].flags == flags)
				gtk_combo_box_set_active (GTK_COMBO_BOX (w), i);
		}
		g_signal_connect (G_OBJECT (w), "changed", G_CALLBACK (cb_compass_changed), state);
		w = glade_xml_get_widget (gui, "position_label");
		gtk_size_group_add_widget (label_size_group, w);
	} else {
		w = glade_xml_get_widget (gui, "compass_position");
		gtk_widget_hide (w);
	}


	if (allowable_positions & GOG_POSITION_COMPASS) {
		w = glade_xml_get_widget (gui, "alignment_combo");
		gtk_size_group_add_widget (widget_size_group, w);
		flags = gog_object_get_position_flags (gobj, GOG_POSITION_ALIGNMENT);
		for (i = 0; i < G_N_ELEMENTS (position_alignment); i++) { 
			gtk_combo_box_append_text (GTK_COMBO_BOX (w), _(position_alignment[i].label));
			if (position_alignment[i].flags == flags)
				gtk_combo_box_set_active (GTK_COMBO_BOX (w), i);
		}
		g_signal_connect (G_OBJECT (w), "changed", G_CALLBACK (cb_alignment_changed), state);
		w = glade_xml_get_widget (gui, "alignment_label");
		gtk_size_group_add_widget (label_size_group, w);
	} else {
		w = glade_xml_get_widget (gui, "compass_alignment");
		gtk_widget_hide (w);
	}

	if (!(allowable_positions & GOG_POSITION_COMPASS)) {
		w =glade_xml_get_widget (gui, "automatic_position_box");
		gtk_widget_hide (w);
	}
	
	g_object_unref (G_OBJECT (widget_size_group));
	g_object_unref (G_OBJECT (label_size_group));

	widget_size_group = gtk_size_group_new (GTK_SIZE_GROUP_HORIZONTAL);
	label_size_group = gtk_size_group_new (GTK_SIZE_GROUP_HORIZONTAL);
	
	if (allowable_positions & GOG_POSITION_MANUAL) {
		w = glade_xml_get_widget (gui, "x_label");
		gtk_size_group_add_widget (label_size_group, w);
		w = glade_xml_get_widget (gui, "x_spin");
		gtk_size_group_add_widget (widget_size_group, w);
		gtk_spin_button_set_value (GTK_SPIN_BUTTON (w), gobj->manual_position.x * 100.0);
		g_signal_connect (G_OBJECT (w), "value-changed", G_CALLBACK (cb_position_changed), state);
		state->x_spin = w;
		
		w = glade_xml_get_widget (gui, "y_label");
		gtk_size_group_add_widget (label_size_group, w);
		w = glade_xml_get_widget (gui, "y_spin");
		gtk_size_group_add_widget (widget_size_group, w);
		gtk_spin_button_set_value (GTK_SPIN_BUTTON (w), gobj->manual_position.y * 100.0);
		g_signal_connect (G_OBJECT (w), "value-changed", G_CALLBACK (cb_position_changed), state);
		state->y_spin = w;

		w = glade_xml_get_widget (gui, "anchor_label");
		gtk_size_group_add_widget (label_size_group, w);
		w =  glade_xml_get_widget (gui, "anchor_combo");
		flags = gog_object_get_position_flags (gobj, GOG_POSITION_ANCHOR);
		for (i = 0; i < G_N_ELEMENTS (position_anchor); i++) {
			gtk_combo_box_append_text (GTK_COMBO_BOX (w), _(position_anchor[i].label));
			if (i == 0 || position_anchor[i].flags == flags)
				gtk_combo_box_set_active (GTK_COMBO_BOX (w), i);
		}
		g_signal_connect (G_OBJECT (w), "changed", G_CALLBACK (cb_anchor_changed), state);
		gtk_combo_box_set_wrap_width (GTK_COMBO_BOX (w), 3);
		
		if (gog_klass->can_manual_size) {
			w = glade_xml_get_widget (gui, "width_label");
			gtk_size_group_add_widget (label_size_group, w);
			w = glade_xml_get_widget (gui, "width_spin");
			gtk_size_group_add_widget (widget_size_group, w);
			gtk_spin_button_set_value (GTK_SPIN_BUTTON (w), gobj->manual_position.w * 100.0);
			g_signal_connect (G_OBJECT (w), "value-changed", 
					  G_CALLBACK (cb_position_changed), state);
			state->w_spin = w;

			w = glade_xml_get_widget (gui, "height_label");
			gtk_size_group_add_widget (label_size_group, w);
			w = glade_xml_get_widget (gui, "height_spin");
			gtk_size_group_add_widget (widget_size_group, w);
			gtk_spin_button_set_value (GTK_SPIN_BUTTON (w), gobj->manual_position.h * 100.0);
			g_signal_connect (G_OBJECT (w), "value-changed", 
					  G_CALLBACK (cb_position_changed), state);
			state->h_spin = w;
		} else {
			w = glade_xml_get_widget (gui, "manual_sizes");
			gtk_widget_hide (w);
		}
	}

	g_object_unref (G_OBJECT (widget_size_group));
	g_object_unref (G_OBJECT (label_size_group));

	w = glade_xml_get_widget (gui, "manual_position_button");
	if ((allowable_positions & GOG_POSITION_MANUAL) &&
	    ((allowable_positions & (GOG_POSITION_COMPASS | GOG_POSITION_ALIGNMENT)) ||
	     (allowable_positions & GOG_POSITION_SPECIAL))) {
		gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (w), 
				       gog_object_get_position_flags (gobj, GOG_POSITION_MANUAL) != 0);
		g_signal_connect (G_OBJECT (w), "toggled", G_CALLBACK (cb_manual_position_changed), state);
		state->manual_toggle = w;
	} else {
		gtk_widget_hide (w);
	}

	w = glade_xml_get_widget (gui, "gog_object_prefs");
	g_object_set_data_full (G_OBJECT (w), "state", state, 
				(GDestroyNotify) object_pref_state_free);  
	gog_editor_add_page (editor, w, _("Position"));
}

static void
gog_object_base_init (GogObjectClass *klass)
{
	klass->roles_allocated = FALSE;
	/* klass->roles might be non-NULL; in that case, it points to
	   the roles hash of the superclass. */
}

static void
gog_object_base_finalize (GogObjectClass *klass)
{
	if (klass->roles_allocated)
		g_hash_table_destroy (klass->roles);
}

static void
gog_object_class_init (GObjectClass *klass)
{
	GogObjectClass *gog_klass = (GogObjectClass *)klass;
	parent_klass = g_type_class_peek_parent (klass);

	klass->finalize = gog_object_finalize;
	klass->set_property	= gog_object_set_property;
	klass->get_property	= gog_object_get_property;

	gog_klass->parent_changed  = gog_object_parent_changed;
	gog_klass->populate_editor = gog_object_populate_editor;

	gog_klass->can_manual_size = FALSE;
	gog_klass->use_parent_as_proxy = FALSE;

	g_object_class_install_property (klass, OBJECT_PROP_ID,
		g_param_spec_uint ("id", "id", "Object ID",
				   0, G_MAXINT, 0,
				   G_PARAM_READWRITE|GOG_PARAM_PERSISTENT));
	g_object_class_install_property (klass, OBJECT_PROP_POSITION,
		g_param_spec_string ("position", "Position", 
				     "Position and size of object, in percentage of parent size",
				     "0 0 1 1", G_PARAM_READWRITE|GOG_PARAM_PERSISTENT));
	g_object_class_install_property (klass, OBJECT_PROP_POSITION_COMPASS,
		g_param_spec_string ("compass", "Compass",
				     "Compass auto position flags",
				     "top", G_PARAM_READWRITE|GOG_PARAM_PERSISTENT));
	g_object_class_install_property (klass, OBJECT_PROP_POSITION_ALIGNMENT,
		g_param_spec_string ("alignment", "Alignment",
				     "Alignment flag",
				     "fill", G_PARAM_READWRITE|GOG_PARAM_PERSISTENT));
	g_object_class_install_property (klass, OBJECT_PROP_POSITION_IS_MANUAL,
		g_param_spec_boolean ("is-position-manual", "Is position manual", 
				      "Is position manual",
				      FALSE, G_PARAM_READWRITE|GOG_PARAM_PERSISTENT));
	g_object_class_install_property (klass, OBJECT_PROP_POSITION_ANCHOR,
		g_param_spec_string ("anchor", "Anchor",
				     "Anchor for manual position",
				     "top-left", G_PARAM_READWRITE|GOG_PARAM_PERSISTENT));

	gog_object_signals [CHILD_ADDED] = g_signal_new ("child-added",
		G_TYPE_FROM_CLASS (klass),
		G_SIGNAL_RUN_LAST,
		G_STRUCT_OFFSET (GogObjectClass, child_added),
		NULL, NULL,
		g_cclosure_marshal_VOID__OBJECT,
		G_TYPE_NONE,	1, G_TYPE_OBJECT);
	gog_object_signals [CHILD_REMOVED] = g_signal_new ("child-removed",
		G_TYPE_FROM_CLASS (klass),
		G_SIGNAL_RUN_LAST,
		G_STRUCT_OFFSET (GogObjectClass, child_removed),
		NULL, NULL,
		g_cclosure_marshal_VOID__OBJECT,
		G_TYPE_NONE,	1, G_TYPE_OBJECT);
	gog_object_signals [CHILD_NAME_CHANGED] = g_signal_new ("child-name-changed",
		G_TYPE_FROM_CLASS (gog_klass),
		G_SIGNAL_RUN_LAST,
		G_STRUCT_OFFSET (GogObjectClass, child_name_changed),
		NULL, NULL,
		g_cclosure_marshal_VOID__OBJECT,
		G_TYPE_NONE,	1, G_TYPE_OBJECT);
	gog_object_signals [CHILDREN_REORDERED] = g_signal_new ("children-reordered",
		G_TYPE_FROM_CLASS (klass),
		G_SIGNAL_RUN_LAST,
		G_STRUCT_OFFSET (GogObjectClass, children_reordered),
		NULL, NULL,
		g_cclosure_marshal_VOID__VOID,
		G_TYPE_NONE, 0);
	gog_object_signals [NAME_CHANGED] = g_signal_new ("name-changed",
		G_TYPE_FROM_CLASS (klass),
		G_SIGNAL_RUN_LAST,
		G_STRUCT_OFFSET (GogObjectClass, name_changed),
		NULL, NULL,
		g_cclosure_marshal_VOID__VOID,
		G_TYPE_NONE, 0);
	gog_object_signals [CHANGED] = g_signal_new ("changed",
		G_TYPE_FROM_CLASS (klass),
		G_SIGNAL_RUN_LAST,
		G_STRUCT_OFFSET (GogObjectClass, changed),
		NULL, NULL,
		g_cclosure_marshal_VOID__BOOLEAN,
		G_TYPE_NONE, 1, G_TYPE_BOOLEAN);
}

static void
gog_object_init (GogObject *obj)
{
	obj->children = NULL;
	obj->user_name = NULL;
	obj->auto_name = NULL;
	obj->id = 0;
	obj->needs_update = FALSE;
	obj->being_updated = FALSE;
	obj->manual_position.x =
	obj->manual_position.y = 0.0;
	obj->manual_position.w =
	obj->manual_position.h = 1.0;
}

GSF_CLASS_FULL (GogObject, gog_object,
		gog_object_base_init, gog_object_base_finalize,
		gog_object_class_init, NULL, gog_object_init,
		G_TYPE_OBJECT, 0, {})

static gboolean
gog_object_is_same_type (GogObject *obj_a, GogObject *obj_b)
{
	g_return_val_if_fail (obj_a->role != NULL, FALSE);
	g_return_val_if_fail (obj_b->role != NULL, FALSE);

	if (obj_a->role->naming_conv != obj_b->role->naming_conv)
		return FALSE;

	if (obj_a->role->naming_conv == GOG_OBJECT_NAME_BY_ROLE)
		return (obj_a->role == obj_b->role);

	return (G_OBJECT_TYPE (obj_a) == G_OBJECT_TYPE (obj_b));
}

static void
gog_object_generate_name (GogObject *obj)
{
	GogObjectClass *klass = GOG_OBJECT_GET_CLASS (obj);
	char const *type_name;

	g_return_if_fail (klass != NULL);
	g_return_if_fail (obj->role != NULL);

	switch (obj->role->naming_conv) {
	default :
	case GOG_OBJECT_NAME_MANUALLY :
		g_warning ("Role %s should not be autogenerating names",
			   obj->role->id);

	case GOG_OBJECT_NAME_BY_ROLE :
		g_return_if_fail (obj->role != NULL);
		type_name = _(obj->role->id);
		break;

	case GOG_OBJECT_NAME_BY_TYPE :
		g_return_if_fail (klass->type_name != NULL);
		type_name = _((*klass->type_name) (obj));
		break;
	}

	if (type_name == NULL)
		type_name =  "BROKEN";

	g_free (obj->auto_name);
	obj->auto_name =  g_strdup_printf ("%s%d", type_name, obj->id);
}

unsigned
gog_object_get_id (GogObject const *obj)
{
	g_return_val_if_fail (GOG_OBJECT (obj) != NULL, 0);
	g_return_val_if_fail (obj != 0, 0);

	return obj->id;
}

static void
gog_object_generate_id (GogObject *obj)
{
	GSList *ptr;
	unsigned id_max = 0;
	GogObject *child;

	obj->id = 0;

	if (obj->parent == NULL)
		return;

	for (ptr = obj->parent->children; ptr != NULL ; ptr = ptr->next) {
		child = GOG_OBJECT (ptr->data);
		if (gog_object_is_same_type (obj, child))
		    id_max = MAX (child->id, id_max);
	}
	obj->id = id_max + 1;

	gog_object_generate_name (obj);
}

static void
gog_object_set_id (GogObject *obj, unsigned id)
{
	gboolean found = FALSE;
	GSList *ptr;
	GogObject *child;

	g_return_if_fail (GOG_OBJECT (obj) != NULL);

	if (id == 0)
		return gog_object_generate_id (obj);

	g_return_if_fail (GOG_OBJECT (obj)->parent != NULL);

	for (ptr = obj->parent->children; ptr != NULL && !found; ptr = ptr->next) {
		child = GOG_OBJECT (ptr->data);
		found = child->id == id &&
			gog_object_is_same_type (obj, child) &&
			ptr->data != obj;
		}

	if (found) {
		g_warning ("id %u already exists", id);
		gog_object_generate_id (obj);
		return;
	}

	if (id == obj->id)
		return;

	obj->id = id;
	gog_object_generate_name (obj);
}

static void
dataset_dup (GogDataset const *src, GogDataset *dst)
{
	gint	     n, last;
	gog_dataset_dims (src, &n, &last);
	for ( ; n <= last ; n++)
		gog_dataset_set_dim (dst, n,
			go_data_dup (gog_dataset_get_dim (src, n)),
			NULL);
}

/**
 * gog_object_dup :
 * @src : #GogObject
 * @new_parent : #GogObject the parent tree for the object (can be NULL)
 * @datadup : a function to duplicate the data (a default one is used if NULL) 
 *
 * Create a deep copy of @obj using @new_parent as its parent.
 **/

GogObject *
gog_object_dup (GogObject const *src, GogObject *new_parent, GogDataDuplicator datadup)
{
	gint	     n;
	GParamSpec **props;
	GogObject   *dst = NULL;
	GSList      *ptr;
	GValue	     val = { 0 };

	if (src == NULL)
		return NULL;

	g_return_val_if_fail (GOG_OBJECT (src) != NULL, NULL);

	if (src->role == NULL || src->explicitly_typed_role)
		dst = g_object_new (G_OBJECT_TYPE (src), NULL);
	if (new_parent)
		dst = gog_object_add_by_role (new_parent, src->role, dst);

	dst->position = src->position;
	/* properties */
	props = g_object_class_list_properties (G_OBJECT_GET_CLASS (src), &n);
	while (n-- > 0)
		if (props[n]->flags & GOG_PARAM_PERSISTENT) {
			g_value_init (&val, props[n]->value_type);
			g_object_get_property (G_OBJECT (src), props[n]->name, &val);
			g_object_set_property (G_OBJECT (dst), props[n]->name, &val);
			g_value_unset (&val);
		}
	g_free (props);

	if (IS_GOG_DATASET (src)) {	/* convenience to save data */
		if (datadup)
			datadup (GOG_DATASET (src), GOG_DATASET (dst));
		else
			dataset_dup (GOG_DATASET (src), GOG_DATASET (dst));
	}

	for (ptr = src->children; ptr != NULL ; ptr = ptr->next)
		/* children added directly to new parent, no need to use the
		 * function result */
		gog_object_dup (ptr->data, dst, datadup);

	return dst;
}

/**
 * gog_object_get_parent :
 * @obj : a #GogObject
 *
 * Returns @obj's parent, potentially NULL if it has not been added to a
 * heirarchy yet.  does not change ref-count in any way.
 **/
GogObject *
gog_object_get_parent (GogObject const *obj)
{
	g_return_val_if_fail (GOG_OBJECT (obj) != NULL, NULL);
	return obj->parent;
}

/**
 * gog_object_get_parent_typed :
 * @obj : a #GogObject
 * @type : a #GType
 *
 * Returns @obj's parent of type @type, potentially NULL if it has not been
 * added to a heirarchy yet or none of the parents are of type @type.
 **/
GogObject *
gog_object_get_parent_typed (GogObject const *obj, GType t)
{
	g_return_val_if_fail (GOG_OBJECT (obj) != NULL, NULL);

	for (; obj != NULL ; obj = obj->parent)
		if (G_TYPE_CHECK_INSTANCE_TYPE (obj, t))
			return GOG_OBJECT (obj); /* const cast */
	return NULL;
}

/**
 * gog_object_get_graph :
 * @obj : const * #GogObject
 *
 * Returns the parent graph.
 **/
GogGraph *
gog_object_get_graph (GogObject const *obj)
{
	g_return_val_if_fail (GOG_OBJECT (obj) != NULL, NULL);

	for (; obj != NULL ; obj = obj->parent)
		if (IS_GOG_GRAPH (obj))
			return GOG_GRAPH (obj);
	return NULL;
}

GogTheme *
gog_object_get_theme (GogObject const *obj)
{
	GogGraph *graph = gog_object_get_graph (obj);

	return (graph != NULL) ? gog_graph_get_theme (graph) : NULL;
}

/**
 * gog_object_get_name :
 * @obj : a #GogObject
 *
 * No need to free the result
 **/
char const *
gog_object_get_name (GogObject const *obj)
{
	g_return_val_if_fail (GOG_OBJECT (obj) != NULL, NULL);
	return (obj->user_name != NULL && *obj->user_name != '\0') ? obj->user_name : obj->auto_name;
}

/**
 * gog_object_set_name :
 * @obj : #GogObject
 * @name :
 * @err : #GError
 *
 * Assign the new name and signals that it has changed.
 * NOTE : it _absorbs_ @name rather than copying it, and generates a new name
 * if @name == NULL
 **/
void
gog_object_set_name (GogObject *obj, char *name, GError **err)
{
	GogObject *tmp;

	g_return_if_fail (GOG_OBJECT (obj) != NULL);

	if (obj->user_name == name)
		return;
	g_free (obj->user_name);
	obj->user_name = name;

	g_signal_emit (G_OBJECT (obj),
		gog_object_signals [NAME_CHANGED], 0);

	for (tmp = obj; tmp != NULL ; tmp = tmp->parent)
		g_signal_emit (G_OBJECT (tmp),
			gog_object_signals [CHILD_NAME_CHANGED], 0, obj);
}

/**
 * gog_object_get_children :
 * @obj : a #GogObject
 * @filter : an optional #GogObjectRole to use as a filter
 *
 * The list needs to be Freed
 **/
GSList *
gog_object_get_children (GogObject const *obj, GogObjectRole const *filter)
{
	GSList *ptr, *res = NULL;

	g_return_val_if_fail (GOG_OBJECT (obj) != NULL, NULL);

	if (filter == NULL)
		return g_slist_copy (obj->children);

	for (ptr = obj->children ; ptr != NULL ; ptr = ptr->next)
		if (GOG_OBJECT (ptr->data)->role == filter)
			res = g_slist_prepend (res, ptr->data);
	return g_slist_reverse (res);
}

/**
 * gog_object_get_child_by_role :
 * @obj : a #GogObject
 * @role : a #GogObjectRole to use as a filter
 *
 * A convenience routine to handle a unique child
 * Returns NULL and spews an error if there is more than one.
 **/
GogObject *
gog_object_get_child_by_role (GogObject const *obj, GogObjectRole const *role)
{
	GogObject *res = NULL;
	GSList *children = gog_object_get_children (obj, role);

	if (children != NULL && children->next == NULL)
		res = children->data;
	g_slist_free (children);
	return res;
}

/**
 * gog_object_is_deletable :
 * @obj : a #GogObject
 *
 * Can the specified @obj be deleted ?
 **/
gboolean
gog_object_is_deletable (GogObject const *obj)
{
	g_return_val_if_fail (GOG_OBJECT (obj) != NULL, FALSE);

	if (IS_GOG_GRAPH (obj))
		return FALSE;

	return obj->role == NULL || obj->role->can_remove == NULL ||
		(obj->role->can_remove) (obj);
}

struct possible_add_closure {
	GSList *res;
	GogObject const *parent;
};

static void
cb_collect_possible_additions (char const *name, GogObjectRole const *role,
			       struct possible_add_closure *data)
{
	if (role->can_add == NULL || (role->can_add) (data->parent))
		data->res = g_slist_prepend (data->res, (gpointer)role);
}

static int
gog_object_position_cmp (GogObjectPosition pos)
{
	if (pos & GOG_POSITION_COMPASS)
		return 0;
	if (GOG_POSITION_IS_SPECIAL (pos) ||
	    GOG_POSITION_IS_PADDING (pos))
		return 2;
	return 1; /* GOG_POSITION_MANUAL */
}

static int
gog_role_cmp (GogObjectRole const *a, GogObjectRole const *b)
{
	int index_a = gog_object_position_cmp (a->allowable_positions);
	int index_b = gog_object_position_cmp (b->allowable_positions);

	if (b->priority != a->priority)
		return b->priority - a->priority;

	/* intentionally reverse to put SPECIAL at the top */
	if (index_a < index_b)
		return 1;
	else if (index_a > index_b)
		return -1;
	return 0;
}

static int
gog_role_cmp_full (GogObjectRole const *a, GogObjectRole const *b)
{
	int res = gog_role_cmp (a, b);
	if (res != 0)
		return res;
	return g_utf8_collate (a->id, b->id);
}

/**
 * gog_object_possible_additions :
 * @parent : a #GogObject
 *
 * returns a list of GogObjectRoles that could be added
 *
 * The resulting list needs to be freed
 **/
GSList *
gog_object_possible_additions (GogObject const *parent)
{
	GogObjectClass *klass = GOG_OBJECT_GET_CLASS (parent);
	g_return_val_if_fail (klass != NULL, NULL);

	if (klass->roles != NULL) {
		struct possible_add_closure data;
		data.res = NULL;
		data.parent = parent;

		g_hash_table_foreach (klass->roles,
			(GHFunc) cb_collect_possible_additions, &data);

		return g_slist_sort (data.res, (GCompareFunc) gog_role_cmp_full);
	}

	return NULL;
}

/**
 * gog_object_can_reorder :
 * @obj : #GogObject
 * @inc_ok : possibly NULL pointer.
 * @dec_ok : possibly NULL pointer.
 *
 * If @obj can move forward or backward in its parents child list
 **/
void
gog_object_can_reorder (GogObject const *obj, gboolean *inc_ok, gboolean *dec_ok)
{
	GogObject const *parent;
	GSList *ptr;

	g_return_if_fail (GOG_OBJECT (obj) != NULL);

	if (inc_ok != NULL)
		*inc_ok = FALSE;
	if (dec_ok != NULL)
		*dec_ok = FALSE;

	if (obj->parent == NULL || gog_object_get_graph (obj) == NULL)
		return;
	parent = obj->parent;
	ptr = parent->children;

	g_return_if_fail (ptr != NULL);

	/* find a pointer to the previous sibling */
	if (ptr->data != obj) {
		while (ptr->next != NULL && ptr->next->data != obj)
			ptr = ptr->next;

		g_return_if_fail (ptr->next != NULL);

		if (inc_ok != NULL &&
		    !gog_role_cmp (((GogObject *)ptr->data)->role, obj->role))
			*inc_ok = TRUE;

		ptr = ptr->next;
	}

	/* ptr now points at @obj */
	if (dec_ok != NULL && ptr->next != NULL &&
	    !gog_role_cmp (obj->role, ((GogObject *)ptr->next->data)->role))
		*dec_ok = TRUE;
}

/**
 * gog_object_reorder :
 * @obj : #GogObject
 * @inc :
 * @goto_max :
 *
 * Returns the object just before @obj in the new ordering.
 **/
GogObject *
gog_object_reorder (GogObject const *obj, gboolean inc, gboolean goto_max)
{
	GogObject *parent, *obj_follows;
	GSList **ptr, *tmp;

	g_return_val_if_fail (GOG_OBJECT (obj) != NULL, NULL);

	if (obj->parent == NULL || gog_object_get_graph (obj) == NULL)
		return NULL;
	parent = obj->parent;

	if (inc)
		parent->children = g_slist_reverse (parent->children);

	for (ptr = &parent->children; *ptr != NULL && (*ptr)->data != obj ;)
		ptr = &(*ptr)->next;

	g_return_val_if_fail (*ptr != NULL, NULL);
	g_return_val_if_fail ((*ptr)->next != NULL, NULL);

	tmp = *ptr;
	*ptr = tmp->next;
	ptr = &(*ptr)->next;

	while (goto_max && *ptr != NULL &&
	       !gog_role_cmp (obj->role, ((GogObject *)((*ptr)->data))->role))
		ptr = &(*ptr)->next;

	tmp->next = *ptr;
	*ptr = tmp;

	if (inc)
		parent->children = g_slist_reverse (parent->children);

	if (parent->children->data != obj) {
		for (tmp = parent->children ; tmp->next->data != obj ; )
			tmp = tmp->next;
		obj_follows = tmp->data;
	} else
		obj_follows = NULL;

	/* Pass the sibling that precedes obj, or NULL if is the head */
	g_signal_emit (G_OBJECT (parent),
		gog_object_signals [CHILDREN_REORDERED], 0);
	gog_object_emit_changed (parent, TRUE);

	return obj_follows;
}

/**
 * gog_object_get_editor :
 * @obj   : #GogObject
 * @dalloc : #GogDataAllocator
 * @cc     : #GOCmdContext
 *
 **/

gpointer
gog_object_get_editor (GogObject *obj, GogDataAllocator *dalloc,
		       GOCmdContext *cc)
{
	GtkWidget *notebook;
	GogEditor *editor;
	GogObjectClass *klass = GOG_OBJECT_GET_CLASS (obj);

	g_return_val_if_fail (klass != NULL, NULL);

	editor = gog_editor_new ();
	if (klass->populate_editor) {
		/* If there are pending updates do them before creating the editor
		 * to avoid expensive widget changes later */
		gog_graph_force_update (gog_object_get_graph (obj));
		(*klass->populate_editor) (obj, editor, dalloc, cc);
	}

	notebook = gog_editor_get_notebook (editor);

	gog_editor_free (editor);

	return notebook;
}

/**
 * gog_object_new_view :
 * @obj : a #GogObject
 * @data :
 **/
GogView *
gog_object_new_view (GogObject const *obj, GogView *parent)
{
	GogObjectClass *klass = GOG_OBJECT_GET_CLASS (obj);

	g_return_val_if_fail (klass != NULL, NULL);

	if (klass->view_type != 0)
		/* set model before parent */
		return g_object_new (klass->view_type,
			"model", obj,
			"parent", parent,
			NULL);

	return NULL;
}

void
gog_object_update (GogObject *obj)
{
	GogObjectClass *klass = GOG_OBJECT_GET_CLASS (obj);
	GSList *ptr;

	g_return_if_fail (klass != NULL);

	ptr = obj->children; /* depth first */
	for (; ptr != NULL ; ptr = ptr->next)
		gog_object_update (ptr->data);

	if (obj->needs_update) {
		obj->needs_update = FALSE;
		obj->being_updated = TRUE;
		gog_debug (0, g_warning ("updating %s (%p)", G_OBJECT_TYPE_NAME (obj), obj););
		if (klass->update != NULL)
			(*klass->update) (obj);
		obj->being_updated = FALSE;
	}
}

gboolean
gog_object_request_update (GogObject *obj)
{
	GogGraph *graph;
	g_return_val_if_fail (GOG_OBJECT (obj), FALSE);
	g_return_val_if_fail (!obj->being_updated, FALSE);

	if (obj->needs_update)
		return FALSE;

	graph = gog_object_get_graph (obj);
	if (graph == NULL) /* we are not linked into a graph yet */
		return FALSE;

	gog_graph_request_update (graph);
	obj->needs_update = TRUE;

	return TRUE;
}

void
gog_object_emit_changed (GogObject *obj, gboolean resize)
{
	GogObjectClass *gog_klass;

	g_return_if_fail (GOG_OBJECT (obj));

	gog_klass = GOG_OBJECT_GET_CLASS (obj);

	if (gog_klass->use_parent_as_proxy) {
		obj = obj->parent;
		if (obj != NULL) {
			g_return_if_fail (IS_GOG_OBJECT (obj));
			gog_object_emit_changed (obj, resize);
		}
		return;
	}
	g_signal_emit (G_OBJECT (obj),
		gog_object_signals [CHANGED], 0, resize);
}

/******************************************************************************/

/**
 * gog_object_clear_parent :
 * @obj : #GogObject
 *
 * Does _not_ unref the child, which in effect adds a ref by freeing up the ref
 * previously associated with the parent.
 **/
gboolean
gog_object_clear_parent (GogObject *obj)
{
	GogObjectClass *klass;
	GogObject *parent;

	g_return_val_if_fail (GOG_OBJECT (obj), FALSE);
	g_return_val_if_fail (obj->parent != NULL, FALSE);
	g_return_val_if_fail (gog_object_is_deletable (obj), FALSE);

	klass = GOG_OBJECT_GET_CLASS (obj);
	parent = obj->parent;
	g_signal_emit (G_OBJECT (parent),
		gog_object_signals [CHILD_REMOVED], 0, obj);
	(*klass->parent_changed) (obj, FALSE);

	if (obj->role != NULL && obj->role->pre_remove != NULL)
		(obj->role->pre_remove) (parent, obj);

	parent->children = g_slist_remove (parent->children, obj);
	obj->parent = NULL;

	if (obj->role != NULL && obj->role->post_remove != NULL)
		(obj->role->post_remove) (parent, obj);

	obj->role = NULL;

	return TRUE;
}

/**
 * gog_object_set_parent :
 * @child  : #GogObject.
 * @parent : #GogObject.
 * @id : optionally %NULL.
 * @role : a static string that can be sent to @parent::add
 *
 * Absorbs a ref to @child
 **/
gboolean
gog_object_set_parent (GogObject *child, GogObject *parent,
		       GogObjectRole const *role, unsigned id)
{
	GogObjectClass *klass;
	GSList **step;

	g_return_val_if_fail (GOG_OBJECT (child), FALSE);
	g_return_val_if_fail (child->parent == NULL, FALSE);
	g_return_val_if_fail (role != NULL, FALSE);

	klass = GOG_OBJECT_GET_CLASS (child);
	child->parent	= parent;
	child->role	= role;
	child->position = role->default_position;

	/* Insert sorted based on hokey little ordering */
	step = &parent->children;
	while (*step != NULL &&
	       gog_role_cmp_full (GOG_OBJECT ((*step)->data)->role, role) >= 0)
		step = &((*step)->next);
	*step = g_slist_prepend (*step, child);

	if (id != 0)
		gog_object_set_id (child, id);
	else
		gog_object_generate_id (child);

	if (role->post_add != NULL)
		(role->post_add) (parent, child);
	(*klass->parent_changed) (child, TRUE);

	g_signal_emit (G_OBJECT (parent),
		gog_object_signals [CHILD_ADDED], 0, child);

	return TRUE;
}

GogObject *
gog_object_add_by_role (GogObject *parent, GogObjectRole const *role, GogObject *child)
{
	GType is_a;
	gboolean const explicitly_typed_role = (child != NULL);

	g_return_val_if_fail (role != NULL, NULL);
	g_return_val_if_fail (GOG_OBJECT (parent) != NULL, NULL);

	is_a = g_type_from_name (role->is_a_typename);

	g_return_val_if_fail (is_a != 0, NULL);

	if (child == NULL)
		child = (role->allocate)
			? (role->allocate) (parent)
			: g_object_new (is_a, NULL);

	g_return_val_if_fail (G_TYPE_CHECK_INSTANCE_TYPE (child, is_a), NULL);
	child->explicitly_typed_role = explicitly_typed_role;
	if (gog_object_set_parent (child, parent, role, 0))
		return child;
	g_object_unref (child);
	return NULL;
}

/**
 * gog_object_add_by_name :
 * @parent : #GogObject
 * @role :
 * @child : optionally null #GogObject
 *
 * Returns a newly created child of @parent in @role.  If @child is provided,
 * it is assumed to be an unaffiliated object that will be assigned in @role.
 * On failure return NULL.
 **/
GogObject *
gog_object_add_by_name (GogObject *parent,
			char const *role, GogObject *child)
{
	return gog_object_add_by_role (parent,
		gog_object_find_role_by_name (parent, role), child);
}

/**
 * gog_object_get_position_flags :
 * @obj : #GogObject
 * @mask : #GogObjectPosition
 *
 * Retrieve position flags of GogObject @obj, masked by @mask.
 */
GogObjectPosition
gog_object_get_position_flags (GogObject const *obj, GogObjectPosition mask)
{
	g_return_val_if_fail (GOG_OBJECT (obj) != NULL, GOG_POSITION_SPECIAL & mask);
	return obj->position & mask;
}

/**
 * gog_object_set_position_flags :
 * @obj : #GogObject
 * @flags : #GogObjectPosition
 * @mask : #GogObjectPosition
 *
 * Attempts to set the position flags of @obj to @flags.
 * Returns TRUE the new flags are permitted.
 **/
gboolean
gog_object_set_position_flags (GogObject *obj, GogObjectPosition flags, GogObjectPosition mask)
{
	g_return_val_if_fail (GOG_OBJECT (obj) != NULL, FALSE);

	if (obj->role == NULL)
		return FALSE;

	if ((obj->position & mask) == flags)
		return TRUE;

	if ((flags & obj->role->allowable_positions) !=
	    (flags & (GOG_POSITION_COMPASS | GOG_POSITION_ANY_MANUAL))) {
		g_warning ("[GogObject::set_position_flags] Invalid flags (%s)",
			   gog_object_get_name (obj));
		return FALSE;
	}
	obj->position = (obj->position & ~mask) | (flags & mask);
	gog_object_emit_changed (obj, TRUE);
	return TRUE;
}

/**
 * gog_object_get_manual_position:
 * @obj : #GogObject
 *
 * returns manual position of this object, in points.
 **/
void
gog_object_get_manual_position (GogObject *gobj, GogViewAllocation *pos)
{
	g_return_if_fail (GOG_OBJECT (gobj) != NULL);

	if (pos != NULL)
		*pos = gobj->manual_position;
}

/**
 * gog_object_set_manual_position:
 * @obj : #GogObject
 * @pos : #GogViewAllocation
 *
 * set manual position of given object, in points. 
 **/
void
gog_object_set_manual_position (GogObject *gobj, GogViewAllocation const *pos)
{
	g_return_if_fail (GOG_OBJECT (gobj) != NULL);

	if (gobj->manual_position.x == pos->x &&
	    gobj->manual_position.y == pos->y &&
	    gobj->manual_position.w == pos->w &&
	    gobj->manual_position.h == pos->h)
		return;

	gobj->manual_position = *pos;
	gog_object_emit_changed (gobj, TRUE);
}

/**
 * gog_object_get_manual_allocation:
 * @gobj : #GogObject
 * @parent_allocation : #GogViewAllocation
 * @requisition : #GogViewRequisition
 *
 * Returns manual allocation of a GogObject given its parent allocation
 * and its size request.
 **/
GogViewAllocation
gog_object_get_manual_allocation (GogObject *gobj,
				  GogViewAllocation const *parent_allocation,
				  GogViewRequisition const *requisition)
{
	GogViewAllocation pos;
	unsigned anchor;

	pos.x = parent_allocation->x + gobj->manual_position.x * parent_allocation->w;
	pos.y = parent_allocation->y + gobj->manual_position.y * parent_allocation->h;

	if (GOG_OBJECT_GET_CLASS (gobj)->can_manual_size) {
		pos.w = gobj->manual_position.w * parent_allocation->w;
		pos.h = gobj->manual_position.h * parent_allocation->h;
	} else {
		pos.w = requisition->w;
		pos.h = requisition->h;
	}
	
	anchor = gog_object_get_position_flags (gobj, GOG_POSITION_ANCHOR);
			   
	switch (anchor) {
		case GOG_POSITION_ANCHOR_N:
		case GOG_POSITION_ANCHOR_CENTER:
		case GOG_POSITION_ANCHOR_S:
			pos.x -= pos.w / 2.0;
			break;
		case GOG_POSITION_ANCHOR_SE:
		case GOG_POSITION_ANCHOR_E:
		case GOG_POSITION_ANCHOR_NE:
			pos.x -= pos.w;
			break;
		default:
			break;
	}
	switch (anchor) {
		case GOG_POSITION_ANCHOR_E:
		case GOG_POSITION_ANCHOR_CENTER:
		case GOG_POSITION_ANCHOR_W:
			pos.y -= pos.h / 2.0;
			break;
		case GOG_POSITION_ANCHOR_SE:
		case GOG_POSITION_ANCHOR_S:
		case GOG_POSITION_ANCHOR_SW:
			pos.y -= pos.h;
			break;
		default:
			break;
	}

	return pos;
}

GogObjectRole const *
gog_object_find_role_by_name (GogObject const *obj, char const *role)
{
	GogObjectClass *klass = GOG_OBJECT_GET_CLASS (obj);

	g_return_val_if_fail (klass != NULL, NULL);

	return g_hash_table_lookup (klass->roles, role);
}

static void
cb_copy_hash_table (gpointer key, gpointer value, GHashTable *hash_table)
{
	g_hash_table_insert (hash_table, key, value);
}

static void
gog_object_allocate_roles (GogObjectClass *klass)
{
	GHashTable *roles = g_hash_table_new (g_str_hash, g_str_equal);

	if (klass->roles != NULL)
		g_hash_table_foreach (klass->roles,
			(GHFunc) cb_copy_hash_table, roles);
	klass->roles = roles;
	klass->roles_allocated = TRUE;
}

void
gog_object_register_roles (GogObjectClass *klass,
			   GogObjectRole const *roles, unsigned n_roles)
{
	unsigned i;

	if (!klass->roles_allocated)
		gog_object_allocate_roles (klass);

	for (i = 0 ; i < n_roles ; i++) {
		g_return_if_fail (g_hash_table_lookup (klass->roles,
			(gpointer )roles[i].id) == NULL);
		g_hash_table_replace (klass->roles,
			(gpointer )roles[i].id, (gpointer) (roles + i));
	}
}
