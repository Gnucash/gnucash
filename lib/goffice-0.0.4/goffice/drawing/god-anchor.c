/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * god-anchor.c: MS Office Graphic Object support
 *
 * Copyright (C) 2000-2002
 *	Jody Goldberg (jody@gnome.org)
 *	Michael Meeks (mmeeks@gnu.org)
 *      Christopher James Lahey <clahey@ximian.com>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
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

#include <goffice/goffice-config.h>
#include <goffice/drawing/god-anchor.h>
#include <gsf/gsf-impl-utils.h>
#include <string.h>

static GObjectClass *parent_class;

struct GodAnchorPrivate_ {
	GoRect rect;
};

GodAnchor *
god_anchor_new (void)
{
	GodAnchor *anchor;

	anchor = g_object_new (GOD_ANCHOR_TYPE, NULL);

	return anchor;
}

void
god_anchor_get_rect  (GodAnchor *anchor,
		      GoRect    *rect)
{
	if (GOD_ANCHOR_GET_CLASS (anchor)->get_rect) {
		GOD_ANCHOR_GET_CLASS (anchor)->get_rect (anchor, rect);
	} else {
		rect->top    = 0;
		rect->left   = 0;
		rect->bottom = 0;
		rect->right  = 0;
	}
}

void
god_anchor_set_rect  (GodAnchor    *anchor,
		      const GoRect *rect)
{
	if (GOD_ANCHOR_GET_CLASS (anchor)->set_rect)
		GOD_ANCHOR_GET_CLASS (anchor)->set_rect (anchor, rect);
}

static void
god_anchor_init (GObject *object)
{
	GodAnchor *anchor          = GOD_ANCHOR (object);
	anchor->priv               = g_new0 (GodAnchorPrivate, 1);
	anchor->priv->rect.top    = 0.0;
	anchor->priv->rect.left   = 0.0;
	anchor->priv->rect.bottom = 0.0;
	anchor->priv->rect.right  = 0.0;
}

static void
god_anchor_finalize (GObject *object)
{
	GodAnchor *anchor = GOD_ANCHOR (object);

	g_free (anchor->priv);
	anchor->priv = NULL;

	G_OBJECT_CLASS (parent_class)->finalize (object);
}

static void
real_god_anchor_get_rect (GodAnchor *anchor,
			  GoRect *rect)
{
	*rect = anchor->priv->rect;
}

static void
real_god_anchor_set_rect (GodAnchor *anchor,
			  const GoRect    *rect)
{
	anchor->priv->rect = *rect;
}

static void
god_anchor_class_init (GodAnchorClass *class)
{
	GObjectClass *object_class;

	object_class           = (GObjectClass *) class;

	parent_class           = g_type_class_peek_parent (class);

	object_class->finalize = god_anchor_finalize;

	class->get_rect        = real_god_anchor_get_rect;
	class->set_rect        = real_god_anchor_set_rect;
}

GSF_CLASS (GodAnchor, god_anchor,
	   god_anchor_class_init, god_anchor_init,
	   G_TYPE_OBJECT)
