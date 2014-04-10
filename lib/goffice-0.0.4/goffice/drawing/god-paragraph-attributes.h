/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/**
 * god-paragraph-attributes.h: MS Office Graphic Object support
 *
 * Author:
 *    Michael Meeks (michael@ximian.com)
 *    Jody Goldberg (jody@gnome.org)
 *    Christopher James Lahey <clahey@ximian.com>
 *
 * (C) 1998-2003 Michael Meeks, Jody Goldberg, Chris Lahey
 **/
#ifndef GOD_PARAGRAPH_ATTRIBUTES_H
#define GOD_PARAGRAPH_ATTRIBUTES_H

#include <glib-object.h>
#include <glib.h>

G_BEGIN_DECLS

#define GOD_PARAGRAPH_ATTRIBUTES_TYPE		(god_paragraph_attributes_get_type ())
#define GOD_PARAGRAPH_ATTRIBUTES(o)		(G_TYPE_CHECK_INSTANCE_CAST ((o), GOD_PARAGRAPH_ATTRIBUTES_TYPE, GodParagraphAttributes))
#define GOD_PARAGRAPH_ATTRIBUTES_CLASS(k)	(G_TYPE_CHECK_CLASS_CAST ((k), GOD_PARAGRAPH_ATTRIBUTES_TYPE, GodParagraphAttributesClass))
#define GOD_PARAGRAPH_ATTRIBUTES_GET_CLASS(o)	(G_TYPE_INSTANCE_GET_CLASS((o), GOD_PARAGRAPH_ATTRIBUTES_TYPE, GodParagraphAttributesClass))
#define IS_GOD_PARAGRAPH_ATTRIBUTES(o)		(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOD_PARAGRAPH_ATTRIBUTES_TYPE))
#define IS_GOD_PARAGRAPH_ATTRIBUTES_CLASS(k)	(G_TYPE_CHECK_CLASS_TYPE ((k), GOD_PARAGRAPH_ATTRIBUTES_TYPE))

typedef struct GodParagraphAttributesPrivate_ GodParagraphAttributesPrivate;

typedef struct {
	GObject parent;
	GodParagraphAttributesPrivate *priv;
} GodParagraphAttributes;

typedef struct {
	GObjectClass parent_class;
} GodParagraphAttributesClass;

typedef enum {
	GOD_PARAGRAPH_ATTRIBUTES_FLAGS_INDENT           = 1 << 0,
	GOD_PARAGRAPH_ATTRIBUTES_FLAGS_SPACE_BEFORE     = 1 << 1,
	GOD_PARAGRAPH_ATTRIBUTES_FLAGS_SPACE_AFTER      = 1 << 2,
	GOD_PARAGRAPH_ATTRIBUTES_FLAGS_ALIGNMENT        = 1 << 3,
	GOD_PARAGRAPH_ATTRIBUTES_FLAGS_BULLET_CHARACTER = 1 << 4,
	GOD_PARAGRAPH_ATTRIBUTES_FLAGS_BULLET_INDENT    = 1 << 5,
	GOD_PARAGRAPH_ATTRIBUTES_FLAGS_BULLET_SIZE      = 1 << 6,
	GOD_PARAGRAPH_ATTRIBUTES_FLAGS_BULLET_FAMILY    = 1 << 7,
	GOD_PARAGRAPH_ATTRIBUTES_FLAGS_BULLET_ON        = 1 << 8,
	GOD_PARAGRAPH_ATTRIBUTES_FLAGS_ALL              = ((1 << 9) - 1),
} GodParagraphAttributesFlags;

typedef enum {
	GOD_PARAGRAPH_ALIGNMENT_LEFT = 0,
	GOD_PARAGRAPH_ALIGNMENT_CENTER = 1,
	GOD_PARAGRAPH_ALIGNMENT_RIGHT = 2,
	GOD_PARAGRAPH_ALIGNMENT_JUSTIFY = 3
} GodParagraphAlignment;

GType                   god_paragraph_attributes_get_type       (void);
GodParagraphAttributes *god_paragraph_attributes_new            (void);


G_END_DECLS

#endif /* GOD_PARAGRAPH_ATTRIBUTES_H */
