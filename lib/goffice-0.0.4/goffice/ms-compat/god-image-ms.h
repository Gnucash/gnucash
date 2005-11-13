/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/**
 * god-image-ms.h: MS Office Graphic Object support
 *
 * Author:
 *    Michael Meeks (michael@ximian.com)
 *    Jody Goldberg (jody@gnome.org)
 *    Christopher James Lahey <clahey@ximian.com>
 *
 * (C) 1998-2003 Michael Meeks, Jody Goldberg, Chris Lahey
 **/

#ifndef GOD_IMAGE_MS_H
#define GOD_IMAGE_MS_H

#include <glib-object.h>
#include <glib.h>
#include <goffice/drawing/god-image.h>

G_BEGIN_DECLS

#define GOD_IMAGE_MS_TYPE		(god_image_ms_get_type ())
#define GOD_IMAGE_MS(o)		(G_TYPE_CHECK_INSTANCE_CAST ((o), GOD_IMAGE_MS_TYPE, GodImageMs))
#define GOD_IMAGE_MS_CLASS(k)		(G_TYPE_CHECK_CLASS_CAST ((k), GOD_IMAGE_MS_TYPE, GodImageMsClass))
#define GOD_IMAGE_MS_GET_CLASS(o)	(G_TYPE_INSTANCE_GET_CLASS((o), GOD_IMAGE_MS_TYPE, GodImageMsClass))
#define IS_GOD_IMAGE_MS(o)		(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOD_IMAGE_MS_TYPE))
#define IS_GOD_IMAGE_MS_CLASS(k)	(G_TYPE_CHECK_CLASS_TYPE ((k), GOD_IMAGE_MS_TYPE))

typedef struct GodImageMsPrivate_ GodImageMsPrivate;

typedef struct {
	GodImage parent;
	GodImageMsPrivate *priv;
} GodImageMs;

typedef struct {
	GodImageClass parent_class;
} GodImageMsClass;

GType         god_image_ms_get_type  (void);
GodImage     *god_image_ms_new       (void);

/* hash is a 16 byte id */
const guint8 *god_image_ms_get_hash  (GodImageMs   *image);
void          god_image_ms_set_hash  (GodImageMs   *image,
				      const guint8 *hash);



G_END_DECLS

#endif /* GOD_IMAGE_MS_H */
