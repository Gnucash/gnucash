/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * pie-demo.c : 
 *
 * Copyright (C) 2003-2005 Jean Brefort (jean.brefort@normalesup.org)
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Library General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301, USA.
 */

#include <gtk/gtk.h>
#include <goffice/goffice.h>
#include <goffice/app/go-plugin.h>
#include <goffice/app/go-plugin-loader-module.h>
#include <goffice/data/go-data-simple.h>
#include <goffice/graph/gog-data-set.h>
#include <goffice/graph/gog-label.h>
#include <goffice/graph/gog-object.h>
#include <goffice/graph/gog-plot.h>
#include <goffice/graph/gog-series.h>
#include <goffice/graph/gog-style.h>
#include <goffice/gtk/go-graph-widget.h>

static void
on_quit (GtkObject *object)
{
	gtk_object_destroy (object);
	gtk_main_quit ();
}

int
main (int argc, char *argv[])
{
	GtkWidget *window, *box, *w;
	GogChart *chart;
	GogGraph *graph;
	GogLabel *label;
	GogPlot *pie;
	GogSeries *series;
	GogStyle *style;
	GOData *data;
	GError *error;
	PangoFontDescription *desc;
	char const *title = "Some statistics";
	char const * const legends[] = {"first", "second", "third", "fourth"};
	double values[] = {10., 20., 30., 40.};

	gtk_init (&argc, &argv);
	/* Initialize libgoffice */
	libgoffice_init ();
	/* Initialize plugins manager */
	go_plugins_init (NULL, NULL, NULL, NULL, TRUE, GO_PLUGIN_LOADER_MODULE_TYPE);

	window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
	gtk_window_resize (GTK_WINDOW (window), 300, 340);
	gtk_window_set_title (GTK_WINDOW (window), "pie demo");
	g_signal_connect (window, "destroy", gtk_main_quit, NULL);

	box = gtk_vbox_new (FALSE, 0);
	w = gtk_button_new_from_stock (GTK_STOCK_QUIT);
	g_signal_connect_swapped (w, "clicked", G_CALLBACK (on_quit), window);
	gtk_box_pack_end (GTK_BOX (box), w, FALSE, FALSE, 0);

	w = gtk_hseparator_new ();
	gtk_box_pack_end (GTK_BOX (box), w, FALSE, FALSE, 2);

	/* Create a graph widget and add it to the GtkVBox */
	w = go_graph_widget_new ();
	gtk_box_pack_end (GTK_BOX (box), w, TRUE, TRUE, 0);
	/* Get the embedded graph */
	graph = go_graph_widget_get_graph (GO_GRAPH_WIDGET (w));
	/* Add a title */
	label = (GogLabel *) g_object_new (GOG_LABEL_TYPE, NULL);
	data = go_data_scalar_str_new (title, FALSE);
	gog_dataset_set_dim (GOG_DATASET (label), 0, data, NULL);
	gog_object_add_by_name (GOG_OBJECT (graph), "Title", GOG_OBJECT (label));
	/* Change the title font */
	style = gog_styled_object_get_style (GOG_STYLED_OBJECT (label));
	desc = pango_font_description_from_string ("Sans bold 16");
	gog_style_set_font_desc (style, desc);
	/* Get the chart created by the widget initialization */
	chart = go_graph_widget_get_chart (GO_GRAPH_WIDGET (w));
	/* Create a pie plot and add it to the chart */
	pie = (GogPlot *) gog_plot_new_by_name ("GogPiePlot");
	gog_object_add_by_name (GOG_OBJECT (chart), "Plot", GOG_OBJECT (pie));
	/* Create a series for the plot and populate it with some simple data */
	series = gog_plot_new_series (pie);
	data = go_data_vector_str_new (legends, 4, NULL);
	gog_series_set_dim (series, 0, data, &error);
	data = go_data_vector_val_new (values, 4, NULL);
	gog_series_set_dim (series, 1, data, &error);
	/* Add a legend to the chart */
	gog_object_add_by_name (GOG_OBJECT (chart), "Legend", NULL);

	gtk_container_add (GTK_CONTAINER (window), box);
	gtk_widget_show_all (GTK_WIDGET (window));

	w = gtk_hseparator_new ();
	gtk_box_pack_start (GTK_BOX (box), w, FALSE, FALSE, 0);

	gtk_main ();

	/* Clean libgoffice stuff */
	libgoffice_shutdown ();
	return 0;
}
