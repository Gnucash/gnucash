/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/**
 * god-text-model.h: MS Office Graphic Object support
 *
 * Author:
 *    Michael Meeks (michael@ximian.com)
 *    Jody Goldberg (jody@gnome.org)
 *    Christopher James Lahey <clahey@ximian.com>
 *
 * (C) 1998-2003 Michael Meeks, Jody Goldberg, Chris Lahey
 **/

#ifndef GOD_TEXT_MODEL_H
#define GOD_TEXT_MODEL_H

#include <glib-object.h>
#include <glib.h>
#include <goffice/drawing/god-paragraph-attributes.h>
#include <goffice/drawing/god-default-attributes.h>
#include <pango/pango-attributes.h>

G_BEGIN_DECLS

#define GOD_TEXT_MODEL_TYPE		(god_text_model_get_type ())
#define GOD_TEXT_MODEL(o)		(G_TYPE_CHECK_INSTANCE_CAST ((o), GOD_TEXT_MODEL_TYPE, GodTextModel))
#define GOD_TEXT_MODEL_CLASS(k)		(G_TYPE_CHECK_CLASS_CAST ((k), GOD_TEXT_MODEL_TYPE, GodTextModelClass))
#define GOD_TEXT_MODEL_GET_CLASS(o)	(G_TYPE_INSTANCE_GET_CLASS((o), GOD_TEXT_MODEL_TYPE, GodTextModelClass))
#define IS_GOD_TEXT_MODEL(o)		(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOD_TEXT_MODEL_TYPE))
#define IS_GOD_TEXT_MODEL_CLASS(k)	(G_TYPE_CHECK_CLASS_TYPE ((k), GOD_TEXT_MODEL_TYPE))

typedef struct GodTextModelPrivate GodTextModelPrivate;
typedef struct GodTextModel GodTextModel;
typedef struct GodTextModelClass GodTextModelClass;

typedef struct {
	char *text;
	PangoAttrList *char_attributes;
	GodParagraphAttributes *para_attributes;
	int indent;
} GodTextModelParagraph;

typedef void (*GodTextModelParagraphForeachCallback) (GodTextModel *text,
						      GodTextModelParagraph *paragraph,
						      gpointer user_data);

struct GodTextModel {
	GObject parent;
	GodTextModelPrivate *priv;
};

struct GodTextModelClass {
	GObjectClass parent_class;

	const char    *(*get_text)  (GodTextModel *text);
	void           (*set_text)  (GodTextModel *text, const char    *text_value);
	void           (*set_paragraph_attributes) (GodTextModel *text, int start, int end, GodParagraphAttributes *attributes);
	void           (*set_pango_attributes) (GodTextModel *text, int start, int end, GList *attributes);
	void           (*set_indent) (GodTextModel *text, int start, int end, int indent);
	const GodDefaultAttributes *           (*get_default_attributes) (GodTextModel *text);
	void           (*paragraph_foreach) (GodTextModel *text, GodTextModelParagraphForeachCallback callback, gpointer user_data);
};

GType                       god_text_model_get_type                  (void);

/* Set routines*/
GodTextModel               *god_text_model_new                       (void);
void                        god_text_model_set_text                  (GodTextModel                         *text,
								      const char                           *text_value);
#if 0
void                       *god_text_model_append_paragraph          (GodTextModel                         *text,
								      PangoLayout                          *layout,
								      GodParagraphAttributes               *para_attr);
#endif
void                        god_text_model_set_paragraph_attributes  (GodTextModel                         *text,
								      int                                   start,
								      int                                   end,
								      GodParagraphAttributes               *para_attr);
void                        god_text_model_set_pango_attributes      (GodTextModel                         *text,
								      int                                   start,
								      int                                   end,
								      GList                                *char_attrs);
void                        god_text_model_set_indent                (GodTextModel                         *text,
								      int                                   start,
								      int                                   end,
								      int                                   indent);
const GodDefaultAttributes *god_text_model_get_default_attributes    (GodTextModel                         *text);

/* Get routines */
const char                 *god_text_model_get_text                  (GodTextModel                         *text);
int                         god_text_model_get_length                (GodTextModel                         *text);
void                        god_text_model_paragraph_foreach         (GodTextModel                         *text,
								      GodTextModelParagraphForeachCallback  callback,
								      gpointer                              user_data);


G_END_DECLS

#endif /* GOD_TEXT_MODEL_H */
