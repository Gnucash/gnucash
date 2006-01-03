/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-bubble-prefs.c
 *
 * Copyright (C) 2004 Jean Brefort (jean.brefort@normalesup.org)
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
#include "gog-surface.h"
#include <goffice/gtk/goffice-gtk.h>
#include <goffice/app/go-plugin.h>

#include <gtk/gtktogglebutton.h>

#include <string.h>

GtkWidget *gog_contour_plot_pref   (GogContourPlot *plot, GOCmdContext *cc);

static void
cb_transpose (GtkToggleButton *btn, GObject *plot)
{
	g_object_set (plot, "transposed", gtk_toggle_button_get_active (btn), NULL);
}

GtkWidget *
gog_contour_plot_pref (GogContourPlot *plot, GOCmdContext *cc)
{
	GtkWidget  *w;
	char const *dir = go_plugin_get_dir_name (
		go_plugins_get_plugin_by_id ("GOffice_plot_surface"));
	char	 *path = g_build_filename (dir, "gog-contour-prefs.glade", NULL);
	GladeXML *gui = go_libglade_new (path, "gog_contour_prefs", NULL, cc);

	g_free (path);
        if (gui == NULL)
                return NULL;


	w = glade_xml_get_widget (gui, "transpose");
	gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (w), plot->transposed);
	g_signal_connect (G_OBJECT (w),
		"toggled",
		G_CALLBACK (cb_transpose), plot);

	w = glade_xml_get_widget (gui, "gog_contour_prefs");
	g_object_set_data_full (G_OBJECT (w),
		"state", gui, (GDestroyNotify)g_object_unref);

	return w;
}
