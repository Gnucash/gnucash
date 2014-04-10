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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
 * USA
 */

#include <goffice/goffice-config.h>
#include "gog-surface.h"
//#include <src/plugin.h>
//#include <src/gui-util.h>
#include <plugin.h>
#include <gui-util.h>

#include <glade/glade-xml.h>
#include <gtk/gtkspinbutton.h>

#include <string.h>

GtkWidget *gog_contour_plot_pref   (GogContourPlot *plot, GnmCmdContext *cc);

static void
cb_levels_changed (GtkSpinButton *btn, GObject *plot)
{
	g_object_set (plot, "levels", gtk_spin_button_get_value_as_int (btn), NULL);
}

GtkWidget *
gog_contour_plot_pref (GogContourPlot *plot, GnmCmdContext *cc)
{
	GtkWidget  *w;
	char const *dir = gnm_plugin_get_dir_name (
		plugins_get_plugin_by_id ("GOffice_plot_surface"));
	char	 *path = g_build_filename (dir, "gog-contour-prefs.glade", NULL);
	GladeXML *gui = gnm_glade_xml_new (cc, path, "gog_contour_prefs", NULL);

	g_free (path);
        if (gui == NULL)
                return NULL;


	w = glade_xml_get_widget (gui, "levels");
	gtk_spin_button_set_value (GTK_SPIN_BUTTON (w), plot->levels);
	g_signal_connect (G_OBJECT (w),
		"value_changed",
		G_CALLBACK (cb_levels_changed), plot);

	w = glade_xml_get_widget (gui, "gog_contour_prefs");
	g_object_set_data_full (G_OBJECT (w),
		"state", gui, (GDestroyNotify)g_object_unref);

	return w;
}
