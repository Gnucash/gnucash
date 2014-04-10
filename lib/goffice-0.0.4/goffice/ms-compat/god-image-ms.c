/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * god-image-ms.c: MS Office Graphic Object support
 *
 * Copyright (C) 2000-2004
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
#include <goffice/ms-compat/god-image-ms.h>
#include <gsf/gsf-impl-utils.h>
#include <string.h>

static GObjectClass *parent_class;

struct GodImageMsPrivate_ {
	guint8 *hash;
};

GodImage *
god_image_ms_new (void)
{
	GodImage *image;

	image = g_object_new (GOD_IMAGE_MS_TYPE, NULL);

	return image;
}

const guint8 *
god_image_ms_get_hash  (GodImageMs     *image)
{
	return image->priv->hash;
}

void
god_image_ms_set_hash  (GodImageMs     *image,
			const guint8 *hash)
{
	g_free (image->priv->hash);
	image->priv->hash = g_memdup (hash, 16);
}

static void
god_image_ms_init (GObject *object)
{
	GodImageMs *image = GOD_IMAGE_MS (object);
	image->priv = g_new0 (GodImageMsPrivate, 1);
}

static void
god_image_ms_finalize (GObject *object)
{
	GodImageMs *image = GOD_IMAGE_MS (object);

	g_free (image->priv->hash);
	g_free (image->priv);
	image->priv = NULL;

	G_OBJECT_CLASS (parent_class)->finalize (object);
}

static void
god_image_ms_class_init (GodImageMsClass *class)
{
	GObjectClass *object_class;

	object_class           = (GObjectClass *) class;

	parent_class           = g_type_class_peek_parent (class);

	object_class->finalize = god_image_ms_finalize;
}

GSF_CLASS (GodImageMs, god_image_ms,
	   god_image_ms_class_init, god_image_ms_init,
	   GOD_IMAGE_TYPE)
