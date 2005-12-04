/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-axis-line.h :
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

#ifndef GOG_AXIS_BASE_H
#define GOG_AXIS_BASE_H

#include <glib-object.h>

G_BEGIN_DECLS

typedef enum {
	GOG_AXIS_AT_LOW,
	GOG_AXIS_CROSS,
	GOG_AXIS_AT_HIGH,
	GOG_AXIS_AUTO
} GogAxisPosition;

typedef enum {
	GOG_AXIS_TICK_NONE,
	GOG_AXIS_TICK_MAJOR,
	GOG_AXIS_TICK_MINOR
} GogAxisTickTypes;

#define GOG_AXIS_LINE_TYPE	(gog_axis_line_get_type ())
#define GOG_AXIS_LINE(o)	(G_TYPE_CHECK_INSTANCE_CAST ((o), GOG_AXIS_LINE_TYPE, GogAxisLine))
#define IS_GOG_AXIS_LINE(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOG_AXIS_LINE_TYPE))

GType gog_axis_line_get_type (void);

G_END_DECLS

#endif /*GOG_AXIS_BASE_H*/

