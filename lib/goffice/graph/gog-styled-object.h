/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-styled-object.h : 
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
#ifndef GOG_STYLED_OBJECT_H
#define GOG_STYLED_OBJECT_H

#include <goffice/graph/goffice-graph.h>
#include <goffice/graph/gog-object.h>

G_BEGIN_DECLS

struct _GogStyledObject {
	GogObject	base;

	GogStyle	*style;
};

typedef struct {
	GogObjectClass base;

	/* virtual */
	void	  (*init_style)     	(GogStyledObject *obj, GogStyle *style);

	/* signal */
	void (*style_changed) (GogStyledObject *obj, GogStyle const *new_style);
} GogStyledObjectClass;

#define GOG_STYLED_OBJECT_TYPE	(gog_styled_object_get_type ())
#define GOG_STYLED_OBJECT(o)	(G_TYPE_CHECK_INSTANCE_CAST ((o), GOG_STYLED_OBJECT_TYPE, GogStyledObject))
#define IS_GOG_STYLED_OBJECT(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOG_STYLED_OBJECT_TYPE))
#define GOG_STYLED_OBJECT_GET_CLASS(o)	(G_TYPE_INSTANCE_GET_CLASS ((o), GOG_STYLED_OBJECT_TYPE, GogStyledObjectClass))

GType     gog_styled_object_get_type (void);
gboolean gog_styled_object_set_style 	   (GogStyledObject *gso, GogStyle *style);
GogStyle *gog_styled_object_get_style	   (GogStyledObject *gso);
GogStyle *gog_styled_object_get_auto_style (GogStyledObject *gso);
void	  gog_styled_object_style_changed  (GogStyledObject *gso);
void	  gog_styled_object_apply_theme	   (GogStyledObject *gso, GogStyle *style);

G_END_DECLS

#endif /* GOG_STYLED_OBJECT_H */
