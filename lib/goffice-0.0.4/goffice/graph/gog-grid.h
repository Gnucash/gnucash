/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-grid.h : 
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
#ifndef GOG_GRID_H
#define GOG_GRID_H

#include <goffice/graph/goffice-graph.h>
#include <glib-object.h>

G_BEGIN_DECLS

#define GOG_GRID_TYPE	(gog_grid_get_type ())
#define GOG_GRID(o)	(G_TYPE_CHECK_INSTANCE_CAST ((o), GOG_GRID_TYPE, GogGrid))
#define IS_GOG_GRID(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOG_GRID_TYPE))

GType gog_grid_get_type (void);

G_END_DECLS

#endif /* GOG_GRID_H */
