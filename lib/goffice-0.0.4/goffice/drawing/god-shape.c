/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * god-shape.c: MS Office Graphic Object support
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
#include <goffice/drawing/god-shape.h>
#include <gsf/gsf-impl-utils.h>
#include <string.h>

static GObjectClass *parent_class;

struct GodShapePrivate_ {
	GodShape *parent;
	GPtrArray *children; /* Of type GodShape. */
	GodPropertyTable *prop_table;
	GodAnchor *anchor;
	GodTextModel *text_model;
};

static GPtrArray*
g_ptr_array_insert_val (GPtrArray        *array,
			guint             index,
			gpointer          data) 
{
	g_ptr_array_add (array, data);
	memmove (array->pdata + index + 1,
		 array->pdata + index,
		 array->len - index - 1);
	g_ptr_array_index (array, index) = data;
	return array;
}

static void
ensure_prop_table (GodShape *shape)
{
	if (shape->priv->prop_table == NULL)
		shape->priv->prop_table =
			god_property_table_new();
}

static void
ensure_text_model (GodShape *shape)
{
	if (shape->priv->text_model == NULL)
		shape->priv->text_model =
			god_text_model_new();
}

GodShape *
god_shape_new (void)
{
	GodShape *shape;

	shape = g_object_new (GOD_SHAPE_TYPE, NULL);

	return shape;
}

void
god_shape_append_child   (GodShape       *parent,
				 GodShape       *child)
{
	god_shape_insert_child (parent, child, -1);
}

/* pos can be -1 to represent the end. */
void
god_shape_insert_child   (GodShape       *parent,
				 GodShape       *child,
				 int                   pos)
{
	g_return_if_fail (parent != NULL);
	g_return_if_fail (child != NULL);
	g_return_if_fail (child->priv->parent == NULL);

	if (pos == -1)
		pos = parent->priv->children->len;

	g_ptr_array_insert_val (parent->priv->children, pos, child);
	g_object_ref (child);
	child->priv->parent = parent;
}

/* pos must be an actual position */
void
god_shape_delete_child   (GodShape       *parent,
				 int                   pos)
{
	GodShape *child = g_ptr_array_remove_index (parent->priv->children, pos);
	g_object_unref (child);
}

/* old_pos must be an actual position.  new_pos can be -1 to represent the end. */
void
god_shape_reorder_child  (GodShape       *parent,
				 int                   old_pos,
				 int                   new_pos)
{
	GodShape *child = g_ptr_array_remove_index (parent->priv->children, old_pos);
	child->priv->parent = NULL;
	god_shape_insert_child (parent, child, new_pos);
	g_object_unref (child);
}

int
god_shape_get_child_count  (GodShape       *parent)
{
	return parent->priv->children->len;
}

GodShape *
god_shape_get_child  (GodShape       *parent,
		      int pos)
{
	GodShape *child;

	g_return_val_if_fail (pos < god_shape_get_child_count (parent), NULL);

	child = g_ptr_array_index (parent->priv->children, pos);

	g_return_val_if_fail (child, NULL);

	g_object_ref (child);
	return child;
}

GodPropertyTable *
god_shape_get_prop_table  (GodShape *shape)
{
	ensure_prop_table (shape);
	g_object_ref (shape->priv->prop_table);
	return shape->priv->prop_table;
}

void
god_shape_set_prop_table  (GodShape *shape,
				  GodPropertyTable *prop_table)
{
	if (shape->priv->prop_table)
		g_object_unref (shape->priv->prop_table);
	shape->priv->prop_table = prop_table;
	if (shape->priv->prop_table)
		g_object_ref (shape->priv->prop_table);
}

GodAnchor *
god_shape_get_anchor  (GodShape *shape)
{
	if (shape->priv->anchor)
		g_object_ref (shape->priv->anchor);
	return shape->priv->anchor;
}

void
god_shape_set_anchor  (GodShape  *shape,
		       GodAnchor *anchor)
{
	if (shape->priv->anchor)
		g_object_unref (shape->priv->anchor);
	shape->priv->anchor = anchor;
	if (shape->priv->anchor)
		g_object_ref (shape->priv->anchor);
}

GodTextModel *
god_shape_get_text_model  (GodShape *shape)
{
	ensure_text_model (shape);
	g_object_ref (shape->priv->text_model);
	return shape->priv->text_model;
}

void
god_shape_set_text_model  (GodShape *shape,
			   GodTextModel *text_model)
{
	if (shape->priv->text_model)
		g_object_unref (shape->priv->text_model);
	shape->priv->text_model = text_model;
	if (shape->priv->text_model)
		g_object_ref (shape->priv->text_model);
}

const char *
god_shape_get_text  (GodShape *shape)
{
	if (shape->priv->text_model)
		return god_text_model_get_text (shape->priv->text_model);
	else
		return NULL;
}

void
god_shape_set_text  (GodShape *shape,
			    const char     *text)
{
	ensure_text_model (shape);
	god_text_model_set_text (shape->priv->text_model, text);
}

static void
god_shape_init (GObject *object)
{
	GodShape *shape = GOD_SHAPE (object);
	shape->priv = g_new0 (GodShapePrivate, 1);
	shape->priv->children = g_ptr_array_new ();
	shape->priv->parent = NULL;
}

static void
god_shape_dispose (GObject *object)
{
	GodShape *shape = GOD_SHAPE (object);
	guint i;

	if (shape->priv == NULL)
		return;

	for (i = 0; i < shape->priv->children->len; i++)
		g_object_unref (g_ptr_array_index (shape->priv->children, i));
	g_ptr_array_free (shape->priv->children, TRUE);
	if (shape->priv->prop_table)
		g_object_unref (shape->priv->prop_table);
	if (shape->priv->anchor)
		g_object_unref (shape->priv->anchor);
	if (shape->priv->text_model)
		g_object_unref (shape->priv->text_model);
	g_free (shape->priv);
	shape->priv = NULL;

	G_OBJECT_CLASS (parent_class)->dispose (object);
}

static void
god_shape_class_init (GodShapeClass *class)
{
	GObjectClass *object_class;

	object_class          = (GObjectClass *) class;

	parent_class          = g_type_class_peek_parent (class);

	object_class->dispose = god_shape_dispose;
}

GSF_CLASS (GodShape, god_shape,
	   god_shape_class_init, god_shape_init,
	   G_TYPE_OBJECT)
