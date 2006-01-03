/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/**
 * god-drawing-group.h: MS Office Graphic Object support
 *
 * Author:
 *    Michael Meeks (michael@ximian.com)
 *    Jody Goldberg (jody@gnome.org)
 *    Christopher James Lahey <clahey@ximian.com>
 *
 * (C) 1998-2003 Michael Meeks, Jody Goldberg, Chris Lahey
 **/

#ifndef GOD_DRAWING_GROUP_H
#define GOD_DRAWING_GROUP_H

#include <glib-object.h>
#include <glib.h>
#include <goffice/drawing/god-image-store.h>

G_BEGIN_DECLS

#define GOD_DRAWING_GROUP_TYPE		(god_drawing_group_get_type ())
#define GOD_DRAWING_GROUP(o)		(G_TYPE_CHECK_INSTANCE_CAST ((o), GOD_DRAWING_GROUP_TYPE, GodDrawingGroup))
#define GOD_DRAWING_GROUP_CLASS(k)	(G_TYPE_CHECK_CLASS_CAST ((k), GOD_DRAWING_GROUP_TYPE, GodDrawingGroupClass))
#define GOD_DRAWING_GROUP_GET_CLASS(o)	(G_TYPE_INSTANCE_GET_CLASS((o), GOD_DRAWING_GROUP_TYPE, GodDrawingGroupClass))
#define IS_GOD_DRAWING_GROUP(o)		(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOD_DRAWING_GROUP_TYPE))
#define IS_GOD_DRAWING_GROUP_CLASS(k)	(G_TYPE_CHECK_CLASS_TYPE ((k), GOD_DRAWING_GROUP_TYPE))

typedef struct GodDrawingGroupPrivate_ GodDrawingGroupPrivate;

typedef struct {
	GObject parent;
	GodDrawingGroupPrivate *priv;
} GodDrawingGroup;

typedef struct {
	GObjectClass parent_class;
} GodDrawingGroupClass;

GType            god_drawing_group_get_type  (void);

GodDrawingGroup *god_drawing_group_new       (void);

GodImageStore   *god_drawing_group_get_image_store (GodDrawingGroup *drawing_group);

G_END_DECLS

#endif /* GOD_DRAWING_GROUP_H */
