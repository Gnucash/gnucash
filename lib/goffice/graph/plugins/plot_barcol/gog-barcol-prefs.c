/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-barcol-prefs.c
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
#include "gog-barcol.h"
//#include <src/plugin.h>
//#include <src/gui-util.h>
#include <plugin.h>
#include <gui-util.h>

#include <glade/glade-xml.h>
#include <gtk/gtkspinbutton.h>

GtkWidget *gog_barcol_plot_pref (GogBarColPlot *plot, GnmCmdContext *cc);

static void
cb_gap_changed (GtkAdjustment *adj, GObject *barcal)
{
	g_object_set (barcal, "gap_percentage", (int)adj->value, NULL);
}

static void
cb_overlap_changed (GtkAdjustment *adj, GObject *barcol)
{
	g_object_set (barcol, "overlap_percentage", (int)adj->value, NULL);
}

GtkWidget *
gog_barcol_plot_pref (GogBarColPlot *barcol, GnmCmdContext *cc)
{
	GtkWidget  *w;
	char const *dir = gnm_plugin_get_dir_name (
		plugins_get_plugin_by_id ("GOffice_plot_barcol"));
	char	 *path = g_build_filename (dir, "gog-barcol-prefs.glade", NULL);
	GladeXML *gui = gnm_glade_xml_new (cc, path, "gog_barcol_prefs", NULL);

	g_free (path);
        if (gui == NULL)
                return NULL;

	w = glade_xml_get_widget (gui, "gap_spinner");
	gtk_spin_button_set_value (GTK_SPIN_BUTTON (w), barcol->gap_percentage);
	g_signal_connect (G_OBJECT (gtk_spin_button_get_adjustment (GTK_SPIN_BUTTON (w))),
		"value_changed",
		G_CALLBACK (cb_gap_changed), barcol);

	w = glade_xml_get_widget (gui, "overlap_spinner");
	gtk_spin_button_set_value (GTK_SPIN_BUTTON (w), barcol->overlap_percentage);
	g_signal_connect (G_OBJECT (gtk_spin_button_get_adjustment (GTK_SPIN_BUTTON (w))),
		"value_changed",
		G_CALLBACK (cb_overlap_changed), barcol);

	w = glade_xml_get_widget (gui, "gog_barcol_prefs");
	g_object_set_data_full (G_OBJECT (w),
		"state", gui, (GDestroyNotify)g_object_unref);

	return w;
}
