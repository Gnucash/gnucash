/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-label.h : 
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
#ifndef GOG_LABEL_H
#define GOG_LABEL_H

#include <goffice/graph/goffice-graph.h>
#include <glib-object.h>

G_BEGIN_DECLS

#define GOG_LABEL_TYPE	(gog_label_get_type ())
#define GOG_LABEL(o)	(G_TYPE_CHECK_INSTANCE_CAST ((o), GOG_LABEL_TYPE, GogLabel))
#define IS_GOG_LABEL(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOG_LABEL_TYPE))

GType gog_label_get_type (void);

G_END_DECLS

#endif /* GOG_LABEL_H */
