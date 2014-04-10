/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-error-bar.c :  
 *
 * Copyright (C) 2004 Jean Brefort (jean.brefort@ac-dijon.fr)
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
//#include <src/gui-util.h>
#include <gui-util.h>
#include "gog-error-bar.h"
#include "gog-series-impl.h"
#include "gog-plot-impl.h"
#include "gog-object-xml.h"
#include "gog-data-allocator.h"
#include "gog-style.h"
#include "gog-renderer.h"
#include "go-data-impl.h"
#include "go-data.h"
#include <goffice/gui-utils/go-color-palette.h>
#include <goffice/gui-utils/go-combo-color.h>
#include <goffice/gui-utils/go-combo-pixmaps.h>
#include <goffice/utils/go-math.h>
#include <gsf/gsf-impl-utils.h>
#include <gtk/gtkspinbutton.h>
#include <gtk/gtkcombobox.h>
#include <gtk/gtktable.h>
#include <gtk/gtklabel.h>
#include <glade/glade-xml.h>
#include <glib/gi18n.h>

#include <string.h>

#define CC2XML(s) ((const xmlChar *)(s))

typedef GObjectClass GogErrorBarClass;
static GObjectClass *error_bar_parent_klass;

#ifdef WITH_GTK
typedef struct {
	GogSeries *series;
	GogErrorBar *bar;
	char const* property;
	GogErrorBarDisplay display;
	GOColor color;
	double width, line_width;
} GogErrorBarEditor;

static void
cb_destroy (G_GNUC_UNUSED GtkWidget *w, GogErrorBarEditor *editor)
{
	g_free (editor);
}

static void
cb_width_changed (GtkAdjustment *adj, GogErrorBarEditor *editor)
{
	editor->width = adj->value;
	if (editor->bar) {
		editor->bar->width = adj->value;
		gog_object_request_update (GOG_OBJECT (editor->series));
	}
}

static void
cb_line_width_changed (GtkAdjustment *adj, GogErrorBarEditor *editor)
{
	editor->line_width = adj->value;
	if (editor->bar) {
		editor->bar->style->line.width = adj->value;
		gog_object_request_update (GOG_OBJECT (editor->series));
	}
}

static void
cb_color_changed (G_GNUC_UNUSED GOComboColor *cc, GOColor color,
		     G_GNUC_UNUSED gboolean is_custom,
		     G_GNUC_UNUSED gboolean by_user,
		     G_GNUC_UNUSED gboolean is_default, GogErrorBarEditor *editor)
{
	editor->color = color;
	if (editor->bar) {
		editor->bar->style->line.color = color;
		gog_object_request_update (GOG_OBJECT (editor->series));
	}
}

static void
cb_display_changed (G_GNUC_UNUSED GOComboPixmaps *combo, GogErrorBarDisplay display, GogErrorBarEditor *editor)
{
	editor->display = display;
	if (editor->bar) {
		editor->bar->display = display;
		gog_object_request_update (GOG_OBJECT (editor->series));
	}
}

static void
cb_type_changed (GtkWidget *w, GogErrorBarEditor *editor)
{
	GladeXML *gui = GLADE_XML (g_object_get_data (G_OBJECT (w), "gui"));
	gpointer data;
	GogDataset *set;
	GogDataAllocator *dalloc;
	int type = gtk_combo_box_get_active (GTK_COMBO_BOX (w));
	dalloc = GOG_DATA_ALLOCATOR (g_object_get_data (G_OBJECT (w), "allocator"));
	if (type == GOG_ERROR_BAR_TYPE_NONE) {
		set = GOG_DATASET (editor->bar->series);
		gog_dataset_set_dim (set, editor->bar->error_i, NULL, NULL);
		gog_dataset_set_dim (set, editor->bar->error_i + 1, NULL, NULL);
		g_object_set (editor->series, editor->property, NULL, NULL);
		editor->bar = NULL;
		data = g_object_get_data (G_OBJECT (w), "plus");
		if (GTK_IS_WIDGET (data))
			gtk_widget_destroy (GTK_WIDGET(data));
		data = g_object_get_data (G_OBJECT (w), "minus");
		if (GTK_IS_WIDGET (data))
			gtk_widget_destroy (GTK_WIDGET(data));
		g_object_set_data (G_OBJECT (w), "plus", NULL);
		g_object_set_data (G_OBJECT (w), "minus", NULL);
		gtk_widget_hide (glade_xml_get_widget (gui, "values_box"));
		gtk_widget_hide (glade_xml_get_widget (gui, "style_box"));
	} else {
		GtkWidget *table = glade_xml_get_widget (gui, "values_table");
		if (!editor->bar) {
			editor->bar = g_object_new (GOG_ERROR_BAR_TYPE, NULL);
			editor->bar->style->line.color = editor->color;
			editor->bar->style->line.width = editor->line_width;
			editor->bar->width = editor->width;
			editor->bar->display = editor->display;
			editor->bar->type = type;
			g_object_set (editor->series, editor->property, editor->bar, NULL);
			g_object_unref (editor->bar);
			g_object_get (editor->series, editor->property, &editor->bar, NULL);
		}
		editor->bar->type = type;
		set = GOG_DATASET (editor->bar->series);
		data = g_object_get_data (G_OBJECT (w), "plus");
		if (!data) {
			GtkWidget* al = GTK_WIDGET (gog_data_allocator_editor (dalloc, set, editor->bar->error_i, GOG_DATA_VECTOR));
			gtk_table_attach (GTK_TABLE (table), al, 1, 2, 0, 1, GTK_FILL | GTK_EXPAND, 0, 0, 0);
			g_object_set_data (G_OBJECT (w), "plus", al);
		}
		data = g_object_get_data (G_OBJECT (w), "minus");
		if (!data) {
			GtkWidget* al = GTK_WIDGET (gog_data_allocator_editor (dalloc, set, editor->bar->error_i + 1, GOG_DATA_VECTOR));
			gtk_table_attach (GTK_TABLE (table), al, 1, 2, 1, 2, GTK_FILL | GTK_EXPAND, 0, 0, 0);
			g_object_set_data (G_OBJECT (w), "minus", al);
		}
		gtk_widget_show_all (glade_xml_get_widget (gui, "values_box"));
		gtk_widget_show (glade_xml_get_widget (gui, "style_box"));
	}
	gog_object_request_update (GOG_OBJECT (editor->series));
}

gpointer
gog_error_bar_prefs (GogSeries *series,
			char const* property,
			gboolean horizontal,
			GogDataAllocator *dalloc,
			GnmCmdContext *cc)
{
	GladeXML *gui;
	GtkWidget *w, *bar_prefs;
	GOComboPixmaps *cpx;
	GtkTable *style_table, *values_table;
	GogDataset *set;
	GdkPixbuf *pixbuf;
	GogErrorBarEditor *editor;
	
	g_return_val_if_fail (IS_GOG_SERIES (series), NULL);
	
	editor = g_new0 (GogErrorBarEditor, 1);
	editor->series = series;
	editor->property = property;
	g_object_get (series, property, &editor->bar, NULL);
	if (editor->bar) {
		editor->color = editor->bar->style->line.color;
		editor->line_width = editor->bar->style->line.width;
		editor->width = editor->bar->width;
		editor->display = editor->bar->display;
	} else {
		editor->color = RGBA_BLACK;
		editor->line_width = 1.;
		editor->width = 5.;
		editor->display = GOG_ERROR_BAR_DISPLAY_BOTH;
	}
	set = GOG_DATASET (series);

	gui = gnm_glade_xml_new (cc, "gog-error-bar-prefs.glade", "gog_error_bar_prefs", NULL);

	/* Style properties */

	/* Width */
	w = glade_xml_get_widget (gui, "width");
	gtk_spin_button_set_value (GTK_SPIN_BUTTON (w), editor->width);
	g_signal_connect (gtk_spin_button_get_adjustment (GTK_SPIN_BUTTON (w)),
		"value_changed",
		G_CALLBACK (cb_width_changed), editor);
	
	/* Line width */
	w = glade_xml_get_widget (gui, "line_width");
	gtk_spin_button_set_value (GTK_SPIN_BUTTON (w), editor->line_width);
	g_signal_connect (gtk_spin_button_get_adjustment (GTK_SPIN_BUTTON (w)),
		"value_changed",
		G_CALLBACK (cb_line_width_changed), editor);

	style_table = GTK_TABLE (glade_xml_get_widget (gui, "style_table"));

	/* Color */
	w = go_combo_color_new (NULL, _("Automatic"), RGBA_BLACK,
		go_color_group_fetch ("color", NULL));
	go_combo_color_set_instant_apply (GO_COMBO_COLOR (w), FALSE);
	go_combo_color_set_allow_alpha (GO_COMBO_COLOR (w), TRUE);
	gtk_label_set_mnemonic_widget (
		GTK_LABEL (glade_xml_get_widget (gui, "color_label")), w);
	go_combo_color_set_color (GO_COMBO_COLOR (w), editor->color);
	g_signal_connect (G_OBJECT (w),
		"color_changed",
		G_CALLBACK (cb_color_changed), editor);
	gtk_table_attach (GTK_TABLE (style_table), w, 1, 2, 3, 4, 0, 0, 0, 0);
	
	/* Display style */
	cpx = go_combo_pixmaps_new (4);
	pixbuf = gnumeric_load_pixbuf ("bar-none.png");
	go_combo_pixmaps_add_element  (cpx,
				       pixbuf,
				       GOG_ERROR_BAR_DISPLAY_NONE,
				       _("No error bar displayed"));
	if (horizontal) {
		pixbuf = gnumeric_load_pixbuf ("bar-hplus.png");
		go_combo_pixmaps_add_element  (cpx,
					       pixbuf,
					       GOG_ERROR_BAR_DISPLAY_POSITIVE,
					       _("Positive error bar displayed"));
		pixbuf = gnumeric_load_pixbuf ("bar-hminus.png");
		go_combo_pixmaps_add_element  (cpx,
					       pixbuf,
					       GOG_ERROR_BAR_DISPLAY_NEGATIVE,
					       _("Negative error bar displayed"));
		pixbuf = gnumeric_load_pixbuf ("bar-hboth.png");
		go_combo_pixmaps_add_element  (cpx,
					       pixbuf,
					       GOG_ERROR_BAR_DISPLAY_BOTH,
					       _("Full error bar displayed"));
	} else {
		pixbuf = gnumeric_load_pixbuf ("bar-vplus.png");
		go_combo_pixmaps_add_element  (cpx,
					       pixbuf,
					       GOG_ERROR_BAR_DISPLAY_POSITIVE,
					       _("Positive error bar displayed"));
		pixbuf = gnumeric_load_pixbuf ("bar-vminus.png");
		go_combo_pixmaps_add_element  (cpx,
					       pixbuf,
					       GOG_ERROR_BAR_DISPLAY_NEGATIVE,
					       _("Negative error bar displayed"));
		pixbuf = gnumeric_load_pixbuf ("bar-vboth.png");
		go_combo_pixmaps_add_element  (cpx,
					       pixbuf,
					       GOG_ERROR_BAR_DISPLAY_BOTH,
					       _("Full error bar displayed"));
	}
	gtk_table_attach (GTK_TABLE (style_table), GTK_WIDGET(cpx), 1, 2, 0, 1, 0, 0, 0, 0);
	go_combo_pixmaps_select_id (cpx, editor->display);
	g_signal_connect (G_OBJECT (cpx), "changed", G_CALLBACK (cb_display_changed), editor);

	/* Category property*/
	w = glade_xml_get_widget (gui, "category_combo");
	gtk_combo_box_set_active (GTK_COMBO_BOX (w), (editor->bar)? (int) editor->bar->type: 0);
	g_object_set_data_full (G_OBJECT (w), "gui", gui, (GDestroyNotify)g_object_unref);
	g_object_set_data (G_OBJECT (w), "allocator", dalloc);
	g_signal_connect (G_OBJECT (w), "changed", G_CALLBACK (cb_type_changed), editor);

	/* Value properties */
	bar_prefs = glade_xml_get_widget (gui, "gog_error_bar_prefs");
	g_signal_connect (bar_prefs, "destroy", G_CALLBACK (cb_destroy), editor);
	gtk_widget_show_all (bar_prefs);

	values_table = GTK_TABLE (glade_xml_get_widget (gui, "values_table"));
	if (editor->bar) {
		GtkWidget* al = GTK_WIDGET (gog_data_allocator_editor (dalloc, set, editor->bar->error_i, GOG_DATA_VECTOR));
		gtk_widget_show (al);
		gtk_table_attach (values_table, al, 1, 2, 0, 1, GTK_FILL | GTK_EXPAND, 0, 0, 0);
		g_object_set_data (G_OBJECT (w), "plus", al);
		al = GTK_WIDGET (gog_data_allocator_editor (dalloc, set, editor->bar->error_i + 1, GOG_DATA_VECTOR));
		gtk_widget_show (al);
		gtk_table_attach (values_table, al, 1, 2, 1, 2, GTK_FILL | GTK_EXPAND, 0, 0, 0);
		g_object_set_data (G_OBJECT (w), "minus", al);
	} else {
		gtk_widget_hide (glade_xml_get_widget (gui, "values_box"));
		gtk_widget_hide (glade_xml_get_widget (gui, "style_box"));
	}

	return GTK_WIDGET(bar_prefs);
}
#endif

static void
gog_error_bar_init (GogErrorBar* bar)
{
	bar->type = GOG_ERROR_BAR_TYPE_NONE;
	bar->display = GOG_ERROR_BAR_DISPLAY_BOTH;
	bar->width = 5.;
	bar->style = gog_style_new ();
	bar->style->line.color = RGBA_BLACK;
	bar->style->line.width = 1.;
}

static void
gog_error_bar_finalize (GObject *obj)
{
	GogErrorBar *bar = GOG_ERROR_BAR (obj);
	if (bar->style) {
		g_object_unref (bar->style);
		bar->style = NULL;
	}
	(error_bar_parent_klass->finalize) (obj);
}

static void
gog_error_bar_class_init (GogErrorBarClass *klass)
{
	GObjectClass *gobject_klass = (GObjectClass *) klass;
	error_bar_parent_klass = g_type_class_peek_parent (klass);

	gobject_klass->finalize		= gog_error_bar_finalize;
}

static gboolean
gog_error_bar_persist_dom_load (GogPersist *gp, xmlNode *node)
{
	GogErrorBar *bar = GOG_ERROR_BAR (gp);

	gchar* str;
	str = xmlGetProp (node, CC2XML ("error_type"));
	if (str) {
		if (!strcmp (str, "absolute"))
			bar->type = GOG_ERROR_BAR_TYPE_ABSOLUTE;
		else if (!strcmp (str, "relative"))
			bar->type = GOG_ERROR_BAR_TYPE_RELATIVE;
		else if (!strcmp (str, "percent"))
			bar->type = GOG_ERROR_BAR_TYPE_PERCENT;
		xmlFree (str);
	}
	str = xmlGetProp (node, CC2XML ("display"));
	if (str) {
		if (!strcmp (str, "none"))
			bar->display = GOG_ERROR_BAR_DISPLAY_NONE;
		else if (!strcmp (str, "positive"))
			bar->display = GOG_ERROR_BAR_DISPLAY_POSITIVE;
		else if (!strcmp (str, "negative"))
			bar->display = GOG_ERROR_BAR_DISPLAY_NEGATIVE;
		xmlFree (str);
	}
	str = xmlGetProp (node, CC2XML ("width"));
	if (str) {
		bar->width = g_strtod (str, NULL);
		xmlFree (str);
	}
	str = xmlGetProp (node, CC2XML ("line_width"));
	if (str) {
		bar->style->line.width = g_strtod (str, NULL);
		xmlFree (str);
	}
	str = xmlGetProp (node, CC2XML ("color"));
	if (str != NULL) {
		bar->style->line.color = go_color_from_str (str);
		xmlFree (str);
	}

	return TRUE;
}

static void
gog_error_bar_persist_dom_save (GogPersist const *gp, xmlNode *parent)
{
	GogErrorBar const *bar = GOG_ERROR_BAR (gp);

	{
		const char *str = NULL;
		xmlSetProp (parent, CC2XML ("type"), CC2XML ("GogErrorBar"));
		switch (bar->type) {
		case GOG_ERROR_BAR_TYPE_ABSOLUTE:
			str = "absolute";
			break;
		case GOG_ERROR_BAR_TYPE_RELATIVE:
			str = "relative";
			break;
		case GOG_ERROR_BAR_TYPE_PERCENT:
			str = "percent";
			break;
		default:
			break;
		}
		if (str)
			xmlSetProp (parent, CC2XML ("error_type"), CC2XML (str));
	}

	{
		const char *str = NULL;
		switch (bar->display) {
		case GOG_ERROR_BAR_DISPLAY_NONE:
			str = "none";
			break;
		case GOG_ERROR_BAR_DISPLAY_POSITIVE:
			str = "positive";
			break;
		case GOG_ERROR_BAR_DISPLAY_NEGATIVE:
			str = "negative";
			break;
		default:
			break;
		}
		if (str)
			xmlSetProp (parent, CC2XML ("display"), CC2XML (str));
	}

	if (bar->width != 5.) {
		char *str = g_strdup_printf ("%f",  bar->width);
		xmlSetProp (parent, CC2XML ("width"), CC2XML (str));
		g_free (str);
	}

	if (bar->style->line.width != 1.) {
		char *str = g_strdup_printf ("%f",  bar->style->line.width);
		xmlSetProp (parent, CC2XML ("line_width"), CC2XML (str));
		g_free (str);
	}
	if (bar->style->line.color != RGBA_BLACK) {
		char *str = go_color_as_str (bar->style->line.color);
		xmlSetProp (parent, CC2XML ("color"), CC2XML (str));
		g_free (str);
	}
}

static void
gog_error_bar_persist_sax_save (GogPersist const *gp, GsfXMLOut *output)
{
	GogErrorBar *bar = GOG_ERROR_BAR (gp);
	char const *str;

	gsf_xml_out_add_cstr_unchecked (output, "type", "GogErrorBar");
	switch (bar->type) {
	case GOG_ERROR_BAR_TYPE_ABSOLUTE: str = "absolute"; break;
	case GOG_ERROR_BAR_TYPE_RELATIVE: str = "relative"; break;
	case GOG_ERROR_BAR_TYPE_PERCENT:  str = "percent"; break;
	default: str = NULL; break;
	}
	if (str != NULL)
		gsf_xml_out_add_cstr_unchecked (output, "error_type", str);

	switch (bar->display) {
	case GOG_ERROR_BAR_DISPLAY_NONE:	str = "none"; break;
	case GOG_ERROR_BAR_DISPLAY_POSITIVE:	str = "positive"; break;
	case GOG_ERROR_BAR_DISPLAY_NEGATIVE:	str = "negative"; break;
	default: str = NULL; break;
	}
	if (str != NULL)
		gsf_xml_out_add_cstr_unchecked (output, "display", str);
#ifdef GOG_WARN_TODO
#warning Why 5.0 and why 1.0 ?
#endif
	if (bar->width != 5.)
		gsf_xml_out_add_float (output, "width", bar->width, 2);
	if (bar->style->line.width != 1.)
		gsf_xml_out_add_float (output, "line_width", bar->style->line.width, 2);
	if (bar->style->line.color != RGBA_BLACK)
		go_xml_out_add_color (output, "color", bar->style->line.color);
}

static void
gog_error_bar_persist_init (GogPersistClass *iface)
{
	iface->dom_load = gog_error_bar_persist_dom_load;
	iface->dom_save = gog_error_bar_persist_dom_save;
	iface->sax_save = gog_error_bar_persist_sax_save;
}

GSF_CLASS_FULL (GogErrorBar, gog_error_bar,
		gog_error_bar_class_init, gog_error_bar_init,
		G_TYPE_OBJECT, 0,
		GSF_INTERFACE (gog_error_bar_persist_init, GOG_PERSIST_TYPE))


/**
 * gog_error_bar_get_bounds :
 * @bar : A GogErrorBar
 * @index : the index corresponding to the value which error limits are 
 * @min : where the minimum value will be stored 
 * @max : where the maximum value will be stored
 *
 * If the value correponding to @index is valid, fills min and max with the error values:
 * -> positive_error in @max.
 * -> negative_error in @min.
 * If one of the errors is not valid or not defined, its value is set to -1.0.
 *
 * Return value : FALSE if the @bar->type is GOG_ERROR_BAR_TYPE_NONE or if the value is not valid,
 * TRUE otherwise.
 **/
gboolean
gog_error_bar_get_bounds (GogErrorBar const *bar, int index, double *min, double *max)
{
	double value;
	GOData *data;
	int length;
	
	/* -1 ensures that the bar will not be displayed if the error is not a correct one.
		With a 0 value, it might be, because of rounding errors */
	*min = *max = -1.; 

	g_return_val_if_fail (GOG_ERROR_BAR (bar) != NULL, FALSE);
	if (!gog_series_is_valid (bar->series))
		return FALSE;
	value = go_data_vector_get_value (GO_DATA_VECTOR (bar->series->values[bar->dim_i].data), index);
	data = bar->series->values[bar->error_i].data;
	length = (IS_GO_DATA (data)) ? go_data_vector_get_len (GO_DATA_VECTOR (data)) : 0;
	
	if ((bar->type == GOG_ERROR_BAR_TYPE_NONE) || isnan (value) || !go_finite (value))
		return FALSE;
	
	if (length == 1) 
		*max = go_data_vector_get_value (GO_DATA_VECTOR (data), 0);
	else if (length > index)
		*max = go_data_vector_get_value (GO_DATA_VECTOR (data), index);
	
	data = bar->series->values[bar->error_i + 1].data;
	length = (IS_GO_DATA (data))? go_data_vector_get_len (GO_DATA_VECTOR (data)): 0;
	if (length == 0)
		*min = *max; /* use same values for + and - */
	else if (length == 1)
		*min = go_data_vector_get_value (GO_DATA_VECTOR (data), 0);
	else if (length > index)
		*min = go_data_vector_get_value (GO_DATA_VECTOR (data), index);
	
	if (isnan (*min) || !go_finite (*min) || (*min <= 0)) {
		*min = -1.;
	}
	if (isnan (*max) || !go_finite (*max) || (*max <= 0)) {
		*max = -1.;
	}
	
	switch (bar->type)
	{
	case GOG_ERROR_BAR_TYPE_RELATIVE:
		*min *= fabs (value);
		*max *= fabs (value);
		break;
	case GOG_ERROR_BAR_TYPE_PERCENT:
		*min *= fabs (value) / 100;
		*max *= fabs (value) / 100;
		break;
	default:
		break;
	}
	return TRUE;
}

void
gog_error_bar_get_minmax (const GogErrorBar *bar, double *min, double *max)
{
	double *values;
	int i, imax;
	double tmp_min, tmp_max, plus, minus;
	
	g_return_if_fail (GOG_ERROR_BAR (bar) != NULL);

	if (!gog_series_is_valid (bar->series)) {
		*min = DBL_MAX;
		*max = -DBL_MAX;
		return;
	}

	imax = go_data_vector_get_len (GO_DATA_VECTOR (bar->series->values[bar->dim_i].data));
	go_data_vector_get_minmax (GO_DATA_VECTOR (bar->series->values[bar->dim_i].data), min, max);
	values = go_data_vector_get_values (GO_DATA_VECTOR (bar->series->values[bar->dim_i].data));

	for (i = 0; i < imax; i++) {
		if  (gog_error_bar_get_bounds (bar, i, &minus, &plus)) {
			tmp_min = values[i] - minus;
			tmp_max = values[i] + plus;
			if (tmp_min < *min)
				*min = tmp_min;
			if (tmp_max > *max)
				*max = tmp_max;
		}
	}
}

GogErrorBar  *
gog_error_bar_dup		(GogErrorBar const *bar)
{
	GogErrorBar* dbar;

	g_return_val_if_fail (IS_GOG_ERROR_BAR (bar), NULL);

	dbar = g_object_new (GOG_ERROR_BAR_TYPE, NULL);
	dbar->type = bar->type;
	dbar->series = bar->series;
	dbar->dim_i = bar->dim_i;
	dbar->error_i = bar->error_i;
	dbar->display = bar->display;
	dbar->width = bar->width;
	if (dbar->style) g_object_unref (dbar->style);
	dbar->style = gog_style_dup (bar->style);
	return dbar;
}

/**
 * gog_error_bar_render :
 * @bar : A GogErrorBar
 * @rend : A GogRenderer 
 * @x_map :  A GogAxisMap for the x axis
 * @y_map :  A GogAxisMap for the y axis
 * @x : x coordinate of the origin of the bar 
 * @y : y coordinate of the origin of the bar
 * @plus : distance from the origin to the positive end of the bar 
 * @minus : distance from the origin to the negative end of the bar 
 * @horizontal : whether the bar is horizontal or not.
 *
 * Displays the error bar. If @plus is negative, the positive side of the bar is not displayed,
 * and if @minus is negative, the negative side of the bar is not displayed.
 * x_map and y_map are used to convert coordinates from data space to canvas coordinates.
 * This function must not be called if #gog_error_bar_get_bounds returned FALSE.
 **/
void gog_error_bar_render (const GogErrorBar *bar,
			   GogRenderer *rend,
			   GogAxisMap *x_map, GogAxisMap *y_map,
			   double x, double y,
			   double minus,
			   double plus,
			   gboolean horizontal)
{
	ArtVpath path [7];
	int n;
	double x_start, y_start, x_end, y_end;
	double line_width, width;
	gboolean start = plus > .0 && bar ->display & GOG_ERROR_BAR_DISPLAY_POSITIVE,
		 end = minus > 0. && bar ->display & GOG_ERROR_BAR_DISPLAY_NEGATIVE;

	if (!start && !end) return;

	if (horizontal) {
		x_start = start ? 
			gog_axis_map_to_canvas (x_map, x + plus) : 
			gog_axis_map_to_canvas (x_map, x);
		x_end =  end ? 
			gog_axis_map_to_canvas (x_map , x - minus) :
			gog_axis_map_to_canvas (x_map , x);
		y_start = y_end = gog_axis_map_to_canvas (y_map, y);
	} else {
		x_start = x_end = gog_axis_map_to_canvas (x_map ,x);
		y_start = start ? 
			gog_axis_map_to_canvas (y_map, y + plus) :
			gog_axis_map_to_canvas (y_map, y);
		y_end =  end ? 
			gog_axis_map_to_canvas (y_map, y - minus) :
			gog_axis_map_to_canvas (y_map, y);
	}
	x = gog_axis_map_to_canvas (x_map, x);
	y = gog_axis_map_to_canvas (y_map, y);

	path[0].code = ART_MOVETO;
	path[1].code = ART_LINETO;
	path[0].x = x_start;
	path[1].x = x_end;
	path[0].y = path[1].y = y_start;
	path[0].y = y_start;
	path[1].y = y_end;

	if (horizontal) {
		width = gog_renderer_pt2r_y (rend, bar->width) / 2.;
		line_width = gog_renderer_pt2r_x (rend, bar->style->line.width);
	} else {
		width = gog_renderer_pt2r_x (rend, bar->width) / 2.;
		line_width = gog_renderer_pt2r_y (rend, bar->style->line.width);
	}

	if ((2. * width) > line_width) {
		if (start && end) {
			path[2].code = ART_MOVETO;
			path[3].code = ART_LINETO;
			n = 4;
		} else
		n = 2;
		path[n].code = ART_MOVETO;
		path[n + 1].code = ART_LINETO;
		path[n + 2].code = ART_END;
		if (horizontal) {
			if (start) {
				path[2].x =path[3].x = x_start;
				path[2].y = y - width;
				path[3].y = y + width;
			}
			if (end) {
				path[n].x =path[n+1].x = x_end;
				path[n].y = y - width;
				path[n+1].y = y + width;
			}
		} else {
			if (start) {
				path[2].x = x - width;
				path[3].x = x + width;
				path[2].y =path[3].y = y_start;
			}
			if (end) {
				path[n].x = x - width;
				path[n+1].x = x + width;
				path[n].y =path[n+1].y = y_end;
			}
		}
	} else
		path[2].code = ART_END;

	gog_renderer_push_style (rend, bar->style);
	gog_renderer_draw_sharp_path (rend, path, NULL);
	gog_renderer_pop_style (rend);
}

gboolean
gog_error_bar_is_visible (GogErrorBar *bar)
{
	return (bar != NULL) &&
		(bar->type != GOG_ERROR_BAR_TYPE_NONE) &&
		(bar->display != GOG_ERROR_BAR_DISPLAY_NONE);
}
