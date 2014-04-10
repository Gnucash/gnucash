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
#include <utils/go-units.h>
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
#define GOD_PROPERTY_FLIP_H	         "flip_h"
#define GOD_PROPERTY_FLIP_V		 "flip_v"
#define GOD_PROPERTY_FILLED		 "filled"
#define GOD_PROPERTY_BACKGROUND		 "background"

/* will be enums when we support multiple "arrow shapes */
#define GOD_PROPERTY_ARROW_START	 "arrow_start"
#define GOD_PROPERTY_ARROW_END		 "arrow_end"

/* Integers & Enums */
#define GOD_PROPERTY_BLIP_ID		 "blip_id"
#define GOD_PROPERTY_FONT_COLOR		 "font_color"
#define GOD_PROPERTY_FILL_TYPE		 "fill_type"
#define GOD_PROPERTY_FILL_SHADE_TYPE	 "fill_shade_type"
#define GOD_PROPERTY_FILL_ANGLE	 	 "fill_angle"
#define GOD_PROPERTY_FILL_FOCUS	 	 "fill_focus"
#define GOD_PROPERTY_FILL_COLOR		 "fill_color"
#define GOD_PROPERTY_FILL_ALPHA		 "fill_alpha"
#define GOD_PROPERTY_FILL_PRESET	 "fill_preset"
#define GOD_PROPERTY_FILL_BACKGROUND	 "fill_background"
#define GOD_PROPERTY_FILL_BACKGROUND_ALPHA "fill_background_alpha"
#define GOD_PROPERTY_OUTLINE_COLOR	 "outline_color"
#define GOD_PROPERTY_OUTLINE_WIDTH	 "outline_width"
#define GOD_PROPERTY_OUTLINE_STYLE	 "outline_style"
#define GOD_PROPERTY_SCROLLBAR_VALUE	 "scrollbar_value"
#define GOD_PROPERTY_SCROLLBAR_MIN	 "scrollbar_min"
#define GOD_PROPERTY_SCROLLBAR_MAX	 "scrollbar_max"
#define GOD_PROPERTY_SCROLLBAR_INC	 "scrollbar_inc"
#define GOD_PROPERTY_SCROLLBAR_PAGE	 "scrollbar_page"
#define GOD_PROPERTY_BLIP_CROP_TOP	 "blip_crop_top"
#define GOD_PROPERTY_BLIP_CROP_BOTTOM	 "blip_crop_bottom"
#define GOD_PROPERTY_BLIP_CROP_LEFT	 "blip_crop_left"
#define GOD_PROPERTY_BLIP_CROP_RIGHT	 "blip_crop_right"

#define GOD_PROPERTY_LTXID                "ltxid"
#define GOD_PROPERTY_DX_TEXT_LEFT         "dx_text_left"
#define GOD_PROPERTY_DX_TEXT_TOP          "dx_text_top"
#define GOD_PROPERTY_DX_TEXT_RIGHT        "dx_text_right"
#define GOD_PROPERTY_DX_TEXT_BOTTOM       "dx_text_bottom"
#define GOD_PROPERTY_FILL_RECT_LEFT       "fill_rect_left"
#define GOD_PROPERTY_FILL_RECT_TOP        "fill_rect_top"
#define GOD_PROPERTY_FILL_RECT_RIGHT      "fill_rect_right"
#define GOD_PROPERTY_FILL_RECT_BOTTOM     "fill_rect_bottom"

/* Ptrs */
#define GOD_PROPERTY_ANCHOR		  "anchor"
#define GOD_PROPERTY_TEXT		  "text"

/* GArrays */
#define GOD_PROPERTY_POLYGON_COORDS	  "polygon_coords"

/* Expressions */
#define GOD_PROPERTY_CHECKBOX_LINK	  "checkbox_link"
#define GOD_PROPERTY_SCROLLBAR_LINK	  "scrollbar_link"

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
