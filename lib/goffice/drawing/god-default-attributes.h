/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/**
 * god-default-attributes.h: MS Office Graphic Object support
 *
 * Author:
 *    Michael Meeks (michael@ximian.com)
 *    Jody Goldberg (jody@gnome.org)
 *    Christopher James Lahey <clahey@ximian.com>
 *
 * (C) 1998-2003 Michael Meeks, Jody Goldberg, Chris Lahey
 **/
#ifndef GOD_DEFAULT_ATTRIBUTES_H
#define GOD_DEFAULT_ATTRIBUTES_H

#include <glib-object.h>
#include <glib.h>
#include <drawing/god-paragraph-attributes.h>

G_BEGIN_DECLS

#define GOD_DEFAULT_ATTRIBUTES_TYPE		(god_default_attributes_get_type ())
#define GOD_DEFAULT_ATTRIBUTES(o)		(G_TYPE_CHECK_INSTANCE_CAST ((o), GOD_DEFAULT_ATTRIBUTES_TYPE, GodDefaultAttributes))
#define GOD_DEFAULT_ATTRIBUTES_CLASS(k)		(G_TYPE_CHECK_CLASS_CAST ((k), GOD_DEFAULT_ATTRIBUTES_TYPE, GodDefaultAttributesClass))
#define GOD_DEFAULT_ATTRIBUTES_GET_CLASS(o)	(G_TYPE_INSTANCE_GET_CLASS((o), GOD_DEFAULT_ATTRIBUTES_TYPE, GodDefaultAttributesClass))
#define IS_GOD_DEFAULT_ATTRIBUTES(o)		(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOD_DEFAULT_ATTRIBUTES_TYPE))
#define IS_GOD_DEFAULT_ATTRIBUTES_CLASS(k)	(G_TYPE_CHECK_CLASS_TYPE ((k), GOD_DEFAULT_ATTRIBUTES_TYPE))

typedef struct GodDefaultAttributesPrivate_ GodDefaultAttributesPrivate;

typedef struct {
	GObject parent;
	GodDefaultAttributesPrivate *priv;
} GodDefaultAttributes;

typedef struct {
	GObjectClass parent_class;
} GodDefaultAttributesClass;

GType                         god_default_attributes_get_type                             (void);
GodDefaultAttributes         *god_default_attributes_new                                  (void);

void                          god_default_attributes_set_paragraph_attributes             (GodDefaultAttributes   *attributes,
											   GodParagraphAttributes *paragraph_attributes);
void                          god_default_attributes_set_paragraph_attributes_for_indent  (GodDefaultAttributes   *attributes,
											   guint                   indent,
											   GodParagraphAttributes *paragraph_attributes);
void                          god_default_attributes_set_pango_attributes                 (GodDefaultAttributes   *attributes,
											   GList                  *pango_attributes);
void                          god_default_attributes_set_pango_attributes_for_indent      (GodDefaultAttributes   *attributes,
											   guint                   indent,
											   GList                  *pango_attributes);

const GodParagraphAttributes *god_default_attributes_get_paragraph_attributes             (GodDefaultAttributes   *attributes);
const GodParagraphAttributes *god_default_attributes_get_paragraph_attributes_for_indent  (GodDefaultAttributes   *attributes,
											   guint                   indent);
const GList                  *god_default_attributes_get_pango_attributes                 (GodDefaultAttributes   *attributes);
const GList                  *god_default_attributes_get_pango_attributes_for_indent      (GodDefaultAttributes   *attributes,
											   guint                   indent);


G_END_DECLS

#endif /* GOD_DEFAULT_ATTRIBUTES_H */
