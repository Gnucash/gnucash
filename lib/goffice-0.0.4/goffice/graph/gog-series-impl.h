/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-series-impl.h :  
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

#ifndef GO_SERIES_IMPL_H
#define GO_SERIES_IMPL_H

#include <goffice/graph/goffice-graph.h>
#include <goffice/graph/gog-styled-object.h>
#include <goffice/graph/gog-series.h>
#include <goffice/graph/gog-data-set.h>
#include <goffice/graph/gog-style.h>
#include <glib-object.h>
#include <gtk/gtknotebook.h>

G_BEGIN_DECLS

typedef struct {
	GogStyledObject base;

	unsigned index;
} GogSeriesElement;

typedef struct {
	GogStyledObjectClass base;
	
	/* Virtuals */
	gpointer (*gse_populate_editor) (GogObject *gobj, 
					 GogEditor *editor, 
					 GOCmdContext *cc);
} GogSeriesElementClass;

#define GOG_SERIES_ELEMENT_GET_CLASS(o)	(G_TYPE_INSTANCE_GET_CLASS ((o), GOG_SERIES_ELEMENT_TYPE, GogSeriesElementClass))

typedef enum {
	GOG_SERIES_REQUIRED,  /* it must be there */
	GOG_SERIES_SUGGESTED, /* allocator will fill it in, but use need not */
	GOG_SERIES_OPTIONAL,
	GOG_SERIES_ERRORS
} GogSeriesPriority;

#define GOG_SERIES_ACCEPT_REGRESSION_CURVE	1

struct _GogSeriesDimDesc {
	char const *name;
	GogSeriesPriority	priority;
	gboolean		is_shared;
	GogDimType		val_type;
	GogMSDimType		ms_type;
};

struct _GogSeriesDesc {
	unsigned style_fields;
	unsigned num_dim;
	GogSeriesDimDesc const *dim;
};

struct _GogSeries {
	GogStyledObject base;

	int index;
	unsigned manual_index : 1;
	unsigned is_valid     : 1;
	unsigned needs_recalc : 1;
	unsigned acceptable_children : 1;

	GogPlot	  	  *plot;
	GogDatasetElement *values;
	gboolean	   has_legend;
	unsigned   	   num_elements;
	GList		  *overrides;  /* GogSeriesElement (individual points) */
};

typedef struct {
	GogStyledObjectClass base;

	GType		series_element_type;

	/* Virtuals */
	void (*dim_changed) (GogSeries *series, int dim_i);
} GogSeriesClass;

#define GOG_SERIES_CLASS(k)	(G_TYPE_CHECK_CLASS_CAST ((k), GOG_SERIES_TYPE, GogSeriesClass))
#define IS_GOG_SERIES_CLASS(k)	(G_TYPE_CHECK_CLASS_TYPE ((k), GOG_SERIES_TYPE))
#define GOG_SERIES_GET_CLASS(o)	(G_TYPE_INSTANCE_GET_CLASS ((o), GOG_SERIES_TYPE, GogSeriesClass))

/* protected */
void gog_series_check_validity   (GogSeries *series);
GogSeriesElement *gog_series_get_element (GogSeries const *series, int index);

G_END_DECLS

#endif /* GO_SERIES_IMPL_H */
