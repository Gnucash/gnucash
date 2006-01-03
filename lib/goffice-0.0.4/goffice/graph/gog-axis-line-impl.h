/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-axis-line-impl.h :
 *
 * Copyright (C) 2005 Emmanuel Pacaud (emmanuel.pacaud@univ-poitiers.fr)
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

#ifndef GOG_AXIS_LINE_IMPL_H
#define GOG_AXIS_LINE_IMPL_H

#include <goffice/graph/gog-axis-line.h>
#include <goffice/graph/gog-data-set.h>
#include <goffice/graph/gog-styled-object.h>
#include <goffice/graph/gog-view.h>
#include <glib-object.h>

G_BEGIN_DECLS

typedef struct {
	GogStyledObject	 base;

	GogChart	*chart;
	GogAxis		*axis;

	GogAxisPosition    position;
	unsigned 	   crossed_axis_id;
	GogDatasetElement  cross_location;
	
	struct {
		gboolean tick_in, tick_out;
		int size_pts;
	} major, minor;
	gboolean major_tick_labeled;
} GogAxisBase;

typedef GogStyledObjectClass GogAxisBaseClass;

#define GOG_AXIS_BASE_TYPE	(gog_axis_base_get_type ())
#define GOG_AXIS_BASE(o)	(G_TYPE_CHECK_INSTANCE_CAST ((o), GOG_AXIS_BASE_TYPE, GogAxisBase))
#define IS_GOG_AXIS_BASE(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOG_AXIS_BASE_TYPE))

GType gog_axis_base_get_type (void);

GogAxisPosition 	gog_axis_base_get_position (GogAxisBase *axis_base);
void		    	gog_axis_base_set_position (GogAxisBase *axis_base, GogAxisPosition position);

void			gog_axis_base_set_label_angle (GogAxisBase *axis_base, double angle);

typedef GogView		GogAxisBaseView;
typedef GogViewClass	GogAxisBaseViewClass;

#define GOG_AXIS_BASE_VIEW_TYPE		(gog_axis_base_view_get_type ())
#define GOG_AXIS_BASE_VIEW(o)		(G_TYPE_CHECK_INSTANCE_CAST ((o), GOG_AXIS_BASE_VIEW_TYPE, GogAxisBaseView))
#define IS_GOG_AXIS_BASE_VIEW(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOG_AXIS_BASE_VIEW_TYPE))

GType gog_axis_base_view_get_type (void);

G_END_DECLS

#endif /*GOG_AXIS_LINE_IMPL_H*/

