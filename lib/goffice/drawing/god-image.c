/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * god-image.c: MS Office Graphic Object support
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
#include "drawing/god-image.h"
#include <gsf/gsf-impl-utils.h>
#include <string.h>

static GObjectClass *parent_class;

struct GodImagePrivate_ {
	char *format;
	guint8 *data;
	guint32 length;
	GdkPixbuf *pixbuf;
};

static void
ensure_pixbuf (GodImage *image)
{
	GdkPixbufLoader *loader;

	if (image->priv->pixbuf)
		return;

	if (image->priv->format)
		loader = gdk_pixbuf_loader_new_with_type (image->priv->format, NULL);
	else
		loader = gdk_pixbuf_loader_new ();
	if (loader) {
		if (gdk_pixbuf_loader_write (loader, image->priv->data, image->priv->length, NULL)) {
			image->priv->pixbuf = gdk_pixbuf_loader_get_pixbuf (loader);
			if (image->priv->pixbuf) {
				g_object_ref (image->priv->pixbuf);
			}
		}
		gdk_pixbuf_loader_close (loader, NULL);
		g_object_unref (loader);
	}
}

GodImage *
god_image_new (void)
{
	GodImage *image;

	image = g_object_new (GOD_IMAGE_TYPE, NULL);

	return image;
}

GdkPixbuf *
god_image_get_pixbuf  (GodImage *image)
{
	ensure_pixbuf (image);

	if (image->priv->pixbuf)
		g_object_ref (image->priv->pixbuf);
	return image->priv->pixbuf;
}

void
god_image_set_image_data  (GodImage *image,
			   const char   *format,
			   const guint8 *data,
			   guint32       length)
{
	g_free (image->priv->data);
	g_free (image->priv->format);
	image->priv->format = g_strdup (format);
	image->priv->length = length;
	image->priv->data = g_memdup (data, length);

	if (image->priv->pixbuf)
		g_object_unref (image->priv->pixbuf);
	image->priv->pixbuf = NULL;
}

static void
god_image_init (GObject *object)
{
	GodImage *image = GOD_IMAGE (object);
	image->priv = g_new0 (GodImagePrivate, 1);
}

static void
god_image_dispose (GObject *object)
{
	GodImage *image = GOD_IMAGE (object);

	if (image->priv == NULL)
		return;

	if (image->priv->pixbuf)
		g_object_unref (image->priv->pixbuf);
	g_free (image->priv->data);
	g_free (image->priv->format);
	g_free (image->priv);
	image->priv = NULL;

	G_OBJECT_CLASS (parent_class)->dispose (object);
}

static void
god_image_class_init (GodImageClass *class)
{
	GObjectClass *object_class;

	object_class          = (GObjectClass *) class;

	parent_class          = g_type_class_peek_parent (class);

	object_class->dispose = god_image_dispose;
}

GSF_CLASS (GodImage, god_image,
	   god_image_class_init, god_image_init,
	   G_TYPE_OBJECT)
