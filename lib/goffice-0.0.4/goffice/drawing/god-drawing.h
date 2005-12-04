/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/**
 * god-drawing.h: MS Office Graphic Object support
 *
 * Author:
 *    Michael Meeks (michael@ximian.com)
 *    Jody Goldberg (jody@gnome.org)
 *    Christopher James Lahey <clahey@ximian.com>
 *
 * (C) 1998-2003 Michael Meeks, Jody Goldberg, Chris Lahey
 **/
#ifndef GOD_DRAWING_H
#define GOD_DRAWING_H

#include <glib-object.h>
#include <glib.h>
#include <goffice/drawing/god-shape.h>
#include <goffice/drawing/god-drawing-group.h>

G_BEGIN_DECLS

#define GOD_DRAWING_TYPE	(god_drawing_get_type ())
#define GOD_DRAWING(o)	(G_TYPE_CHECK_INSTANCE_CAST ((o), GOD_DRAWING_TYPE, GodDrawing))
#define GOD_DRAWING_CLASS(k)	(G_TYPE_CHECK_CLASS_CAST ((k), GOD_DRAWING_TYPE, GodDrawingClass))
#define IS_GOD_DRAWING(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOD_DRAWING_TYPE))
#define IS_GOD_DRAWING_CLASS(k)	(G_TYPE_CHECK_CLASS_TYPE ((k), GOD_DRAWING_TYPE))

typedef struct GodDrawingPrivate_ GodDrawingPrivate;

typedef struct {
	GObject parent;
	GodDrawingPrivate *priv;
} GodDrawing;

typedef struct {
	GObjectClass parent_class;
} GodDrawingClass;

GType            god_drawing_get_type           (void);
GodDrawing      *god_drawing_new                (void);

/* Return value is reffed. */
GodShape        *god_drawing_get_root_shape     (GodDrawing      *drawing);
void             god_drawing_set_root_shape     (GodDrawing      *drawing,
						 GodShape        *root_shape);

/* Return value is reffed. */
GodShape        *god_drawing_get_background     (GodDrawing      *drawing);
void             god_drawing_set_background     (GodDrawing      *drawing,
						 GodShape        *root_shape);

/* Return value is reffed. */
GodDrawingGroup *god_drawing_get_drawing_group  (GodDrawing      *drawing);
void             god_drawing_set_drawing_group  (GodDrawing      *drawing,
						 GodDrawingGroup *drawing_group);


G_END_DECLS

#endif /* GOD_DRAWING_H */
