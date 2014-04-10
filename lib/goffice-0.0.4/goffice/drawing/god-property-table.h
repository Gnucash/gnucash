/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/**
 * god-property-table.h: MS Office Graphic Object support
 *
 * Author:
 *    Michael Meeks (michael@ximian.com)
 *    Jody Goldberg (jody@gnome.org)
 *    Christopher James Lahey <clahey@ximian.com>
 *
 * (C) 1998-2003 Michael Meeks, Jody Goldberg, Chris Lahey
 **/

#ifndef GOD_PROPERTY_TABLE_H
#define GOD_PROPERTY_TABLE_H

#include <glib-object.h>
#include <glib.h>
#include <goffice/utils/go-units.h>
#include <pango/pango-attributes.h>

G_BEGIN_DECLS

#define GOD_PROPERTY_TABLE_TYPE	(god_property_table_get_type ())
#define GOD_PROPERTY_TABLE(o)	(G_TYPE_CHECK_INSTANCE_CAST ((o), GOD_PROPERTY_TABLE_TYPE, GodPropertyTable))
#define GOD_PROPERTY_TABLE_CLASS(k)	(G_TYPE_CHECK_CLASS_CAST ((k), GOD_PROPERTY_TABLE_TYPE, GodClassPropertyTable))
#define IS_GOD_PROPERTY_TABLE(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOD_PROPERTY_TABLE_TYPE))
#define IS_GOD_PROPERTY_TABLE_CLASS(k)	(G_TYPE_CHECK_CLASS_TYPE ((k), GOD_PROPERTY_TABLE_TYPE))

typedef struct GodPropertyTablePrivate_ GodPropertyTablePrivate;

typedef struct {
	GObject parent;
	GodPropertyTablePrivate *priv;
} GodPropertyTable;

typedef struct {
	GObjectClass parent_class;
} GodPropertyTableClass;

/* Flags */
#define GOD_PROPERTY_FLIP_H	         "flip-h"
#define GOD_PROPERTY_FLIP_V		 "flip-v"
#define GOD_PROPERTY_FILLED		 "filled"
#define GOD_PROPERTY_BACKGROUND		 "background"

/* will be enums when we support multiple "arrow shapes */
#define GOD_PROPERTY_ARROW_START	 "arrow-start"
#define GOD_PROPERTY_ARROW_END		 "arrow-end"

/* Integers & Enums */
#define GOD_PROPERTY_BLIP_ID		 "blip-id"
#define GOD_PROPERTY_FONT_COLOR		 "font-color"
#define GOD_PROPERTY_FILL_TYPE		 "fill-type"
#define GOD_PROPERTY_FILL_SHADE_TYPE	 "fill-shade_type"
#define GOD_PROPERTY_FILL_ANGLE	 	 "fill-angle"
#define GOD_PROPERTY_FILL_FOCUS	 	 "fill-focus"
#define GOD_PROPERTY_FILL_COLOR		 "fill-color"
#define GOD_PROPERTY_FILL_ALPHA		 "fill-alpha"
#define GOD_PROPERTY_FILL_PRESET	 "fill-preset"
#define GOD_PROPERTY_FILL_BACKGROUND	 "fill-background"
#define GOD_PROPERTY_FILL_BACKGROUND_ALPHA "fill-background-alpha"
#define GOD_PROPERTY_OUTLINE_COLOR	 "outline-color"
#define GOD_PROPERTY_OUTLINE_WIDTH	 "outline-width"
#define GOD_PROPERTY_OUTLINE_STYLE	 "outline-style"
#define GOD_PROPERTY_SCROLLBAR_VALUE	 "scrollbar-value"
#define GOD_PROPERTY_SCROLLBAR_MIN	 "scrollbar-min"
#define GOD_PROPERTY_SCROLLBAR_MAX	 "scrollbar-max"
#define GOD_PROPERTY_SCROLLBAR_INC	 "scrollbar-inc"
#define GOD_PROPERTY_SCROLLBAR_PAGE	 "scrollbar-page"
#define GOD_PROPERTY_BLIP_CROP_TOP	 "blip-crop-top"
#define GOD_PROPERTY_BLIP_CROP_BOTTOM	 "blip-crop-bottom"
#define GOD_PROPERTY_BLIP_CROP_LEFT	 "blip-crop-left"
#define GOD_PROPERTY_BLIP_CROP_RIGHT	 "blip-crop-right"

#define GOD_PROPERTY_LTXID                "ltxid"
#define GOD_PROPERTY_DX_TEXT_LEFT         "dx-text-left"
#define GOD_PROPERTY_DX_TEXT_TOP          "dx-text-top"
#define GOD_PROPERTY_DX_TEXT_RIGHT        "dx-text-right"
#define GOD_PROPERTY_DX_TEXT_BOTTOM       "dx-text-bottom"
#define GOD_PROPERTY_FILL_RECT_LEFT       "fill-rect-left"
#define GOD_PROPERTY_FILL_RECT_TOP        "fill-rect-top"
#define GOD_PROPERTY_FILL_RECT_RIGHT      "fill-rect-right"
#define GOD_PROPERTY_FILL_RECT_BOTTOM     "fill-rect-bottom"

/* Ptrs */
#define GOD_PROPERTY_ANCHOR		  "anchor"
#define GOD_PROPERTY_TEXT		  "text"

/* GArrays */
#define GOD_PROPERTY_POLYGON_COORDS	  "polygon-coords"

/* Expressions */
#define GOD_PROPERTY_CHECKBOX_LINK	  "checkbox-link"
#define GOD_PROPERTY_SCROLLBAR_LINK	  "scrollbar-link"

/* PangoAttrList */
#define GOD_PROPERTY_MARKUP	  	  "markup"

typedef enum {
	GOD_FILL_TYPE_SOLID,
	GOD_FILL_TYPE_PATTERN,
	GOD_FILL_TYPE_TEXTURE,
	GOD_FILL_TYPE_PICTURE,
	GOD_FILL_TYPE_SHADE,
	GOD_FILL_TYPE_SHADE_CENTER,
	GOD_FILL_TYPE_SHADE_SHAPE,
	GOD_FILL_TYPE_SHADE_SCALE,
	GOD_FILL_TYPE_SHADE_TITLE,
	GOD_FILL_TYPE_SHADE_BACKGROUND
} GodFillType;

typedef const char *GodPropertyID;

GType             god_property_table_get_type     (void);

/* Base methods */
void              god_property_table_set          (GodPropertyTable *attrs,
						   GodPropertyID     id,
						   GValue           *attr);
GValue           *god_property_table_get          (GodPropertyTable *table,
						   GodPropertyID     id);

/* Set methods */
void              god_property_table_set_flag     (GodPropertyTable *table,
						   GodPropertyID     id,
						   gboolean          val);
void              god_property_table_set_uint     (GodPropertyTable *table,
						   GodPropertyID     id,
						   guint32           val);
void              god_property_table_set_int      (GodPropertyTable *table,
						   GodPropertyID     id,
						   gint32            val);
void              god_property_table_set_length   (GodPropertyTable *table,
						   GodPropertyID     id,
						   go_unit_t         val);
void              god_property_table_set_pointer  (GodPropertyTable *table,
						   GodPropertyID     id,
						   gpointer          val);
void              god_property_table_set_array    (GodPropertyTable *table,
						   GodPropertyID     id,
						   GArray           *array);
void              god_property_table_set_markup   (GodPropertyTable *table,
						   GodPropertyID     id,
						   PangoAttrList    *list);

/* Get methods */
gboolean          god_property_table_get_flag     (GodPropertyTable *table,
						   GodPropertyID     id,
						   gboolean          default_value);
guint32           god_property_table_get_uint     (GodPropertyTable *table,
						   GodPropertyID     id,
						   guint32           default_value);
gint32            god_property_table_get_int      (GodPropertyTable *table,
						   GodPropertyID     id,
						   gint32            default_value);
go_unit_t         god_property_table_get_length   (GodPropertyTable *table,
						   GodPropertyID     id,
						   go_unit_t         default_value);
gpointer          god_property_table_get_pointer  (GodPropertyTable *table,
						   GodPropertyID     id,
						   gpointer          default_value);
GArray           *god_property_table_get_array    (GodPropertyTable *table,
						   GodPropertyID     id,
						   GArray           *default_value);
PangoAttrList	 *god_property_table_get_markup	  (GodPropertyTable *table,
						   GodPropertyID     id,
						   PangoAttrList *default_value);

/* Allocation */
GodPropertyTable *god_property_table_new          (void);

G_END_DECLS

#endif /* GOD_PROPERTY_TABLE_H */
