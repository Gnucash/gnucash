/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * god-drawing.c: MS Office Graphic Object support
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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
 * USA
 */

#include <goffice/goffice-config.h>
#include "drawing/god-drawing.h"
#include <gsf/gsf-impl-utils.h>

static GObjectClass *parent_class;

struct GodDrawingPrivate_ {
	GodShape *root_shape;
	GodShape *background;
	GodDrawingGroup *drawing_group;
};

GodDrawing *
god_drawing_new (void)
{
	GodDrawing *drawing;

	drawing = g_object_new (GOD_DRAWING_TYPE, NULL);

	return drawing;
}

GodShape *
god_drawing_get_root_shape  (GodDrawing *drawing)
{
	if (drawing->priv->root_shape)
		g_object_ref (drawing->priv->root_shape);
	return drawing->priv->root_shape;
}

void
god_drawing_set_root_shape  (GodDrawing *drawing,
			     GodShape *root_shape)
{
	if (drawing->priv->root_shape)
		g_object_unref (drawing->priv->root_shape);
	drawing->priv->root_shape = root_shape;
	if (drawing->priv->root_shape)
		g_object_ref (drawing->priv->root_shape);
}

GodShape *
god_drawing_get_background  (GodDrawing *drawing)
{
	if (drawing->priv->background)
		g_object_ref (drawing->priv->background);
	return drawing->priv->background;
}

void
god_drawing_set_background  (GodDrawing *drawing,
			     GodShape *background)
{
	if (drawing->priv->background)
		g_object_unref (drawing->priv->background);
	drawing->priv->background = background;
	if (drawing->priv->background)
		g_object_ref (drawing->priv->background);
}

GodDrawingGroup *
god_drawing_get_drawing_group  (GodDrawing *drawing)
{
	if (drawing->priv->drawing_group)
		g_object_ref (drawing->priv->drawing_group);
	return drawing->priv->drawing_group;
}

void
god_drawing_set_drawing_group  (GodDrawing *drawing,
				GodDrawingGroup *drawing_group)
{
	if (drawing->priv->drawing_group)
		g_object_unref (drawing->priv->drawing_group);
	drawing->priv->drawing_group = drawing_group;
	if (drawing->priv->drawing_group)
		g_object_ref (drawing->priv->drawing_group);
}

static void
god_drawing_init (GObject *object)
{
	GodDrawing *drawing = GOD_DRAWING (object);

	drawing->priv = g_new0 (GodDrawingPrivate, 1);
}

static void
god_drawing_dispose (GObject *object)
{
	GodDrawing *drawing = GOD_DRAWING (object);

	if (drawing->priv == NULL)
		return;

	if (drawing->priv->root_shape)
		g_object_unref (drawing->priv->root_shape);
	if (drawing->priv->background)
		g_object_unref (drawing->priv->background);
	if (drawing->priv->drawing_group)
		g_object_unref (drawing->priv->drawing_group);
	g_free (drawing->priv);
	drawing->priv = NULL;

	G_OBJECT_CLASS (parent_class)->dispose (object);
}

static void
god_drawing_class_init (GodDrawingClass *class)
{
	GObjectClass *object_class;

	object_class           = (GObjectClass *) class;

	parent_class           = g_type_class_peek_parent (class);

	object_class->dispose = god_drawing_dispose;
}

GSF_CLASS (GodDrawing, god_drawing,
	   god_drawing_class_init, god_drawing_init,
	   G_TYPE_OBJECT)
