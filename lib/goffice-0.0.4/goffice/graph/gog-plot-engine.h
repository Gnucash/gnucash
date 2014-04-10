/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-plot-engine.h : 
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
#ifndef GOG_PLOT_ENGINE_H
#define GOG_PLOT_ENGINE_H

#include <goffice/graph/goffice-graph.h>
#include <glib-object.h>

G_BEGIN_DECLS

struct _GogPlotType {
	GogPlotFamily 	*family;
	char 		*engine;

	char 		*name, *sample_image_file;
	char 		*description; /* untranslated */
	int 		 col, row;

	GHashTable 	*properties;
}; 

struct _GogPlotFamily {
	char *name, *sample_image_file;
	int priority;

	GogAxisSet	 axis_set;

	GHashTable *types;
}; 

/* GogPlotFamily hashed by name */
GHashTable const *gog_plot_families (void);
GogPlotFamily *gog_plot_family_by_name  (char const *name);
GogPlotFamily *gog_plot_family_register (char const *name, char const *sample_image_file,
					 int priority, GogAxisSet axis_set);
GogPlotType   *gog_plot_type_register   (GogPlotFamily *famlily, int col, int row,
					 char const *name, char const *sample_image_file,
					 char const *description, char const *engine);

struct _GogRegCurveType {
	char *engine;

	char *name;
	char *description; /* untranslated */

	GHashTable *properties;
};

GHashTable const *gog_reg_curve_types (void);

void gog_plugin_services_init (void);
void gog_plugin_services_shutdown (void);

G_END_DECLS

#endif /* GOG_PLOT_ENGINE_H */
