/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/**
 * god-image-store.h: MS Office Graphic Object support
 *
 * Author:
 *    Michael Meeks (michael@ximian.com)
 *    Jody Goldberg (jody@gnome.org)
 *    Christopher James Lahey <clahey@ximian.com>
 *
 * (C) 1998-2003 Michael Meeks, Jody Goldberg, Chris Lahey
 **/
#ifndef GOD_IMAGE_STORE_H
#define GOD_IMAGE_STORE_H

#include <glib-object.h>
#include <glib.h>
#include <goffice/drawing/god-image.h>

G_BEGIN_DECLS

#define GOD_IMAGE_STORE_TYPE		(god_image_store_get_type ())
#define GOD_IMAGE_STORE(o)		(G_TYPE_CHECK_INSTANCE_CAST ((o), GOD_IMAGE_STORE_TYPE, GodImageStore))
#define GOD_IMAGE_STORE_CLASS(k)	(G_TYPE_CHECK_CLASS_CAST ((k), GOD_IMAGE_STORE_TYPE, GodImageStoreClass))
#define IS_GOD_IMAGE_STORE(o)		(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOD_IMAGE_STORE_TYPE))
#define IS_GOD_IMAGE_STORE_CLASS(k)	(G_TYPE_CHECK_CLASS_TYPE ((k), GOD_IMAGE_STORE_TYPE))

typedef struct GodImageStorePrivate_ GodImageStorePrivate;

typedef struct {
	GObject parent;
	GodImageStorePrivate *priv;
} GodImageStore;

typedef struct {
	GObjectClass parent_class;
} GodImageStoreClass;

GType          god_image_store_get_type         (void);
GodImageStore *god_image_store_new              (void);

/* Tree functions */
void           god_image_store_append_image     (GodImageStore *store,
						 GodImage      *image);
void           god_image_store_insert_image     (GodImageStore *store,
						 GodImage      *image,
						 int            pos);
void           god_image_store_delete_image     (GodImageStore *store,
						 int            pos);
void           god_image_store_reorder_image    (GodImageStore *store,
						 int            old_pos,
						 int            new_pos);
int            god_image_store_get_image_count  (GodImageStore *store);
/* Return value is reffed. */
GodImage      *god_image_store_get_image        (GodImageStore *store,
						 int            pos);

G_END_DECLS

#endif /* GOD_IMAGE_STORE_H */
