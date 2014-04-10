/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-pie-prefs.c
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
#include "gog-pie.h"
#include <plugin.h>
#include <gui-util.h>

#include <glade/glade-xml.h>
#include <gtk/gtkspinbutton.h>
#include <gtk/gtktogglebutton.h>

GtkWidget *gog_pie_series_element_pref   (GogPieSeriesElement *element, GnmCmdContext *cc);

static void
cb_element_separation_changed (GtkAdjustment *adj, GObject *element)
{
	g_object_set (element, "separation", adj->value / 100., NULL);
}

GtkWidget *
gog_pie_series_element_pref (GogPieSeriesElement *element, GnmCmdContext *cc)
{
	GtkWidget  *w;
	char const *dir = gnm_plugin_get_dir_name (
		plugins_get_plugin_by_id ("GOffice_plot_pie"));
	char	 *path = g_build_filename (dir, "gog-pie-series.glade", NULL);
	GladeXML *gui = gnm_glade_xml_new (cc, path, "gog_pie_series_element_prefs", NULL);

	g_free (path);
        if (gui == NULL)
                return NULL;
	
	w = glade_xml_get_widget (gui, "separation_spinner");
	gtk_spin_button_set_value (GTK_SPIN_BUTTON (w), element->separation * 100.);
	g_signal_connect (G_OBJECT (gtk_spin_button_get_adjustment (GTK_SPIN_BUTTON (w))),
		"value_changed",
		G_CALLBACK (cb_element_separation_changed), element);

	w = glade_xml_get_widget (gui, "gog_pie_series_element_prefs");
	g_object_set_data_full (G_OBJECT (w),
		"state", gui, (GDestroyNotify)g_object_unref);

	return w;
}

/****************************************************************************/
GtkWidget *gog_pie_plot_pref   (GogPiePlot *plot, GnmCmdContext *cc);

static void
cb_default_separation_changed (GtkAdjustment *adj, GObject *pie)
{
	g_object_set (pie, "default_separation", adj->value / 100., NULL);
}

static void
cb_rotation_changed (GtkAdjustment *adj, GObject *pie)
{
	g_object_set (pie, "initial_angle", adj->value, NULL);
}


static void
cb_use_style_toggled (GtkToggleButton *button, GObject *series)
{
	g_object_set (series, "vary_style_by_element",
		gtk_toggle_button_get_active (button), NULL);
}

static void
gog_pie_plot_pref_signal_connect (GogPiePlot *pie, GladeXML *gui)
{
	GtkWidget *w;
	
	w = glade_xml_get_widget (gui, "rotation_spinner");
	gtk_spin_button_set_value (GTK_SPIN_BUTTON (w), pie->initial_angle);
	g_signal_connect (G_OBJECT (gtk_spin_button_get_adjustment (GTK_SPIN_BUTTON (w))),
		"value_changed",
		G_CALLBACK (cb_rotation_changed), pie);

	w = glade_xml_get_widget (gui, "separation_spinner");
	gtk_spin_button_set_value (GTK_SPIN_BUTTON (w), pie->default_separation * 100.);
	g_signal_connect (G_OBJECT (gtk_spin_button_get_adjustment (GTK_SPIN_BUTTON (w))),
		"value_changed",
		G_CALLBACK (cb_default_separation_changed), pie);

	w = glade_xml_get_widget (gui, "vary_style_by_element");
	gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (w), pie->base.vary_style_by_element);
	g_signal_connect (G_OBJECT (w),
		"toggled",
		G_CALLBACK (cb_use_style_toggled), pie);
}

GtkWidget *
gog_pie_plot_pref (GogPiePlot *pie, GnmCmdContext *cc)
{
	GtkWidget  *w;
	char const *dir = gnm_plugin_get_dir_name (
		plugins_get_plugin_by_id ("GOffice_plot_pie"));
	char	 *path = g_build_filename (dir, "gog-pie-prefs.glade", NULL);
	GladeXML *gui = gnm_glade_xml_new (cc, path, "gog_pie_prefs", NULL);

	g_free (path);
        if (gui == NULL)
                return NULL;

	gog_pie_plot_pref_signal_connect (pie, gui);

	w = glade_xml_get_widget (gui, "gog_pie_prefs");
	g_object_set_data_full (G_OBJECT (w),
		"state", gui, (GDestroyNotify)g_object_unref);

	return w;
}

/****************************************************************************/

GtkWidget *gog_ring_plot_pref   (GogRingPlot *ring, GnmCmdContext *cc);

static void
cb_center_size_changed (GtkAdjustment *adj, GObject *ring)
{
	g_object_set (ring, "center_size", adj->value/100., NULL);
}


GtkWidget *
gog_ring_plot_pref (GogRingPlot *ring, GnmCmdContext *cc)
{
	GtkWidget  *w;
	char const *dir = gnm_plugin_get_dir_name (
		plugins_get_plugin_by_id ("GOffice_plot_pie"));
	char	 *path = g_build_filename (dir, "gog-ring-prefs.glade", NULL);
	GladeXML *gui = gnm_glade_xml_new (cc, path, "gog_ring_prefs", NULL);

	g_free (path);
        if (gui == NULL)
                return NULL;

	gog_pie_plot_pref_signal_connect (GOG_PIE_PLOT (ring), gui);

	w = glade_xml_get_widget (gui, "center_size_spinner");
	gtk_spin_button_set_value (GTK_SPIN_BUTTON (w), ring->center_size * 100);
	g_signal_connect (G_OBJECT (gtk_spin_button_get_adjustment (GTK_SPIN_BUTTON (w))),
		"value_changed",
		G_CALLBACK (cb_center_size_changed), ring);

	w = glade_xml_get_widget (gui, "gog_ring_prefs");
	g_object_set_data_full (G_OBJECT (w),
		"state", gui, (GDestroyNotify)g_object_unref);

	return w;
}

/****************************************************************************/

GtkWidget *gog_pie_series_pref (GogPieSeries *series, GnmCmdContext *cc);

static void
cb_separation_changed (GtkAdjustment *adj, GObject *pie)
{
	g_object_set (pie, "separation", adj->value, NULL);
}

GtkWidget *
gog_pie_series_pref (GogPieSeries *pie, GnmCmdContext *cc)
{
	GtkWidget  *w;
	char const *dir = gnm_plugin_get_dir_name (
		plugins_get_plugin_by_id ("GOffice_plot_pie"));
	char	 *path = g_build_filename (dir, "gog-pie-prefs.glade", NULL);
	GladeXML *gui = gnm_glade_xml_new (cc, path, "gog_pie_prefs", NULL);

	g_free (path);
        if (gui == NULL)
                return NULL;

	w = glade_xml_get_widget (gui, "rotation_spinner");
	gtk_spin_button_set_value (GTK_SPIN_BUTTON (w), pie->initial_angle);
	g_signal_connect (G_OBJECT (gtk_spin_button_get_adjustment (GTK_SPIN_BUTTON (w))),
		"value_changed",
		G_CALLBACK (cb_rotation_changed), pie);

	w = glade_xml_get_widget (gui, "separation_spinner");
	gtk_spin_button_set_value (GTK_SPIN_BUTTON (w), pie->separation);
	g_signal_connect (G_OBJECT (gtk_spin_button_get_adjustment (GTK_SPIN_BUTTON (w))),
		"value_changed",
		G_CALLBACK (cb_separation_changed), pie);

	gtk_widget_hide (glade_xml_get_widget (gui, "vary_style_by_element"));

	w = glade_xml_get_widget (gui, "gog_pie_prefs");
	g_object_set_data_full (G_OBJECT (w),
		"state", gui, (GDestroyNotify)g_object_unref);

	return w;
}
