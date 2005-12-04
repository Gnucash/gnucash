/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * god-image-store.c: MS Office Graphic Object support
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
#include <goffice/drawing/god-image-store.h>
#include <gsf/gsf-impl-utils.h>
#include <string.h>

static GObjectClass *parent_class;

struct GodImageStorePrivate_ {
	GPtrArray *images; /* Of type GodImage. */
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

GodImageStore *
god_image_store_new (void)
{
	GodImageStore *shape;

	shape = g_object_new (GOD_IMAGE_STORE_TYPE, NULL);

	return shape;
}

void
god_image_store_append_image   (GodImageStore       *store,
				GodImage       *image)
{
	god_image_store_insert_image (store, image, -1);
}

/* pos can be -1 to represent the end. */
void
god_image_store_insert_image   (GodImageStore       *store,
				GodImage       *image,
				int                   pos)
{
	g_return_if_fail (store != NULL);
	g_return_if_fail (image != NULL);

	if (pos == -1)
		pos = store->priv->images->len;

	g_ptr_array_insert_val (store->priv->images, pos, image);
	g_object_ref (image);
}

/* pos must be an actual position */
void
god_image_store_delete_image   (GodImageStore       *store,
				int                   pos)
{
	GodImage *image = g_ptr_array_remove_index (store->priv->images, pos);
	g_object_unref (image);
}

/* old_pos must be an actual position.  new_pos can be -1 to represent the end. */
void
god_image_store_reorder_image  (GodImageStore       *store,
				int                   old_pos,
				int                   new_pos)
{
	GodImage *image = g_ptr_array_remove_index (store->priv->images, old_pos);
	god_image_store_insert_image (store, image, new_pos);
	g_object_unref (image);
}

int
god_image_store_get_image_count  (GodImageStore       *store)
{
	return store->priv->images->len;
}

GodImage *
god_image_store_get_image  (GodImageStore       *store,
			    int pos)
{
	GodImage *image;

	g_return_val_if_fail (pos < god_image_store_get_image_count (store), NULL);

	image = g_ptr_array_index (store->priv->images, pos);

	g_return_val_if_fail (image, NULL);

	g_object_ref (image);
	return image;
}

static void
god_image_store_init (GObject *object)
{
	GodImageStore *shape = GOD_IMAGE_STORE (object);
	shape->priv = g_new0 (GodImageStorePrivate, 1);
	shape->priv->images = g_ptr_array_new ();
}

static void
god_image_store_dispose (GObject *object)
{
	GodImageStore *shape = GOD_IMAGE_STORE (object);
	guint i;

	if (shape->priv == NULL)
		return;

	for (i = 0; i < shape->priv->images->len; i++)
		g_object_unref (g_ptr_array_index (shape->priv->images, i));
	g_ptr_array_free (shape->priv->images, TRUE);
	g_free (shape->priv);
	shape->priv = NULL;

	G_OBJECT_CLASS (parent_class)->dispose (object);
}

static void
god_image_store_class_init (GodImageStoreClass *class)
{
	GObjectClass *object_class;

	object_class          = (GObjectClass *) class;

	parent_class          = g_type_class_peek_parent (class);

	object_class->dispose = god_image_store_dispose;
}

GSF_CLASS (GodImageStore, god_image_store,
	   god_image_store_class_init, god_image_store_init,
	   G_TYPE_OBJECT)
