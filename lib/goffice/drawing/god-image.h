/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/**
 * god-image.h: MS Office Graphic Object support
 *
 * Author:
 *    Michael Meeks (michael@ximian.com)
 *    Jody Goldberg (jody@gnome.org)
 *    Christopher James Lahey <clahey@ximian.com>
 *
 * (C) 1998-2003 Michael Meeks, Jody Goldberg, Chris Lahey
 **/
#ifndef GOD_IMAGE_H
#define GOD_IMAGE_H

#include <glib-object.h>
#include <glib.h>
#include <drawing/god-property-table.h>
#include <drawing/god-anchor.h>
#include <drawing/god-text-model.h>
#include <gdk-pixbuf/gdk-pixbuf.h>

G_BEGIN_DECLS

#define GOD_IMAGE_TYPE		(god_image_get_type ())
#define GOD_IMAGE(o)		(G_TYPE_CHECK_INSTANCE_CAST ((o), GOD_IMAGE_TYPE, GodImage))
#define GOD_IMAGE_CLASS(k)	(G_TYPE_CHECK_CLASS_CAST ((k), GOD_IMAGE_TYPE, GodImageClass))
#define IS_GOD_IMAGE(o)		(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOD_IMAGE_TYPE))
#define IS_GOD_IMAGE_CLASS(k)	(G_TYPE_CHECK_CLASS_TYPE ((k), GOD_IMAGE_TYPE))

typedef struct GodImagePrivate_ GodImagePrivate;

typedef struct {
	GObject parent;
	GodImagePrivate *priv;
} GodImage;

typedef struct {
	GObjectClass parent_class;
} GodImageClass;

GType      god_image_get_type        (void);
GodImage  *god_image_new             (void);

GdkPixbuf *god_image_get_pixbuf      (GodImage     *image);
/* Instead of setting the pixbuf, you set the image data.  */
void       god_image_set_image_data  (GodImage     *image,
				      const char   *format,
				      const guint8 *data,
				      guint32       length);

G_END_DECLS

#endif /* GOD_IMAGE_H */
