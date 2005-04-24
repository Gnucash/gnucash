/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/**
 * god-shape.h: MS Office Graphic Object support
 *
 * Author:
 *    Michael Meeks (michael@ximian.com)
 *    Jody Goldberg (jody@gnome.org)
 *    Christopher James Lahey <clahey@ximian.com>
 *
 * (C) 1998-2003 Michael Meeks, Jody Goldberg, Chris Lahey
 **/
#ifndef GOD_SHAPE_H
#define GOD_SHAPE_H

#include <glib-object.h>
#include <glib.h>
#include <drawing/god-property-table.h>
#include <drawing/god-anchor.h>
#include <drawing/god-text-model.h>

G_BEGIN_DECLS

#define GOD_SHAPE_TYPE		(god_shape_get_type ())
#define GOD_SHAPE(o)		(G_TYPE_CHECK_INSTANCE_CAST ((o), GOD_SHAPE_TYPE, GodShape))
#define GOD_SHAPE_CLASS(k)	(G_TYPE_CHECK_CLASS_CAST ((k), GOD_SHAPE_TYPE, GodShapeClass))
#define IS_GOD_SHAPE(o)		(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOD_SHAPE_TYPE))
#define IS_GOD_SHAPE_CLASS(k)	(G_TYPE_CHECK_CLASS_TYPE ((k), GOD_SHAPE_TYPE))

typedef struct GodShapePrivate_ GodShapePrivate;

typedef struct {
	GObject parent;
	GodShapePrivate *priv;
} GodShape;

typedef struct {
	GObjectClass parent_class;
} GodShapeClass;

GType             god_shape_get_type         (void);
GodShape         *god_shape_new              (void);

/* Tree functions */
void              god_shape_append_child     (GodShape         *parent,
					      GodShape         *child);
void              god_shape_insert_child     (GodShape         *parent,
					      GodShape         *child,
					      int               pos);
void              god_shape_delete_child     (GodShape         *parent,
					      int               pos);
void              god_shape_reorder_child    (GodShape         *parent,
					      int               old_pos,
					      int               new_pos);
int               god_shape_get_child_count  (GodShape         *parent);
/* Return value is reffed. */
GodShape         *god_shape_get_child        (GodShape         *parent,
					      int               pos);

/* Return value is reffed. */
GodPropertyTable *god_shape_get_prop_table   (GodShape         *shape);
void              god_shape_set_prop_table   (GodShape         *shape,
					      GodPropertyTable *prop_table);

/* Return value is reffed. */
GodAnchor        *god_shape_get_anchor       (GodShape         *shape);
void              god_shape_set_anchor       (GodShape         *shape,
					      GodAnchor        *anchor);

/* Return value is reffed. */
GodTextModel     *god_shape_get_text_model   (GodShape         *shape);
void              god_shape_set_text_model   (GodShape         *shape,
					      GodTextModel     *text);

const char       *god_shape_get_text         (GodShape         *shape);
void              god_shape_set_text         (GodShape         *shape,
					      const char       *text_value);

G_END_DECLS

#endif /* GOD_SHAPE_H */
