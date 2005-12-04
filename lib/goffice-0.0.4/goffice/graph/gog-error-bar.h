/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-error-bar.h :  
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

#ifndef GOG_ERROR_BAR_H
#define GOG_ERROR_BAR_H

#include <goffice/utils/go-color.h>
#include "gog-series.h"
#include "gog-data-set.h"
#include "gog-axis.h"

G_BEGIN_DECLS

typedef enum {
	GOG_ERROR_BAR_TYPE_NONE,
	GOG_ERROR_BAR_TYPE_ABSOLUTE,
	GOG_ERROR_BAR_TYPE_RELATIVE,
	GOG_ERROR_BAR_TYPE_PERCENT
} GogErrorBarType;

typedef enum {
	GOG_ERROR_BAR_DISPLAY_NONE,
	GOG_ERROR_BAR_DISPLAY_POSITIVE,
	GOG_ERROR_BAR_DISPLAY_NEGATIVE,
	GOG_ERROR_BAR_DISPLAY_BOTH
} GogErrorBarDisplay;

struct  _GogErrorBar{
	GObject	base;
	GogErrorBarType type;
	GogSeries *series;
	int dim_i;
	int error_i;
	GogErrorBarDisplay display;
	float width;
	GogStyle* style;
};

#define GOG_ERROR_BAR_TYPE		(gog_error_bar_get_type ())
#define GOG_ERROR_BAR(o)		(G_TYPE_CHECK_INSTANCE_CAST ((o), GOG_ERROR_BAR_TYPE, GogErrorBar))
#define IS_GOG_ERROR_BAR(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOG_ERROR_BAR_TYPE))

GType gog_error_bar_get_type (void);

GogErrorBar  	*gog_error_bar_dup		(GogErrorBar const *bar);

#ifdef WITH_GTK
gpointer 	 gog_error_bar_prefs (GogSeries *series, char const* property, 
				      gboolean horizontal, GogDataAllocator *dalloc, 
				      GOCmdContext *cc);
#endif

gboolean 	 gog_error_bar_get_bounds (const GogErrorBar *bar, int index, 
					   double *min, double *max);
void 		 gog_error_bar_get_minmax (const GogErrorBar *bar, 
					   double *min, double *max);
void 		 gog_error_bar_render (const GogErrorBar *bar, GogRenderer *rend, 
				       GogAxisMap *x_map, GogAxisMap *y_map,
				       double x, double y, 
				       double minus,
				       double plus,
				       gboolean horizontal);
gboolean 	 gog_error_bar_is_visible (GogErrorBar *bar);

G_END_DECLS

#endif
