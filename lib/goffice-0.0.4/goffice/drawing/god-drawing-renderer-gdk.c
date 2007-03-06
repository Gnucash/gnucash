/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * god-drawing-renderer-gdk.c: MS Office Graphic Object support
 *
 * Copyright (C) 2000-2002
 *	Jody Goldberg (jody@gnome.org)
 *	Michael Meeks (mmeeks@gnu.org)
 *      Christopher James Lahey <clahey@ximian.com>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301
 * USA
 */

#include <goffice/goffice-config.h>
#include <goffice/drawing/god-drawing-renderer-gdk.h>
#include <gsf/gsf-impl-utils.h>
#include <gdk/gdk.h>
#include <string.h>
#include <math.h>

static GObjectClass *parent_class;

struct GodDrawingRendererGdkPrivate_ {
	GodDrawing *drawing;
	GdkDrawable *drawable;
	GdkGC *gc;
	GodAnchor *extents;
	go_unit_t x_units_per_pixel;
	go_unit_t y_units_per_pixel;
};

GodDrawingRendererGdk *
god_drawing_renderer_gdk_new (void)
{
	GodDrawingRendererGdk *renderer;

	renderer = g_object_new (GOD_DRAWING_RENDERER_GDK_TYPE, NULL);

	return renderer;
}

static void
update_units_per_pixel (GodDrawingRendererGdk *renderer)
{
	if (renderer->priv->drawable && renderer->priv->extents) {
		gint pixel_width, pixel_height;
		GoRect extent_rect;
		gdk_drawable_get_size (renderer->priv->drawable, &pixel_width, &pixel_height);
		god_anchor_get_rect (renderer->priv->extents, &extent_rect);

		renderer->priv->x_units_per_pixel = (extent_rect.right - extent_rect.left) / pixel_width;
		renderer->priv->y_units_per_pixel = (extent_rect.bottom - extent_rect.top) / pixel_height;
	} else {
		renderer->priv->x_units_per_pixel = 0;
		renderer->priv->y_units_per_pixel = 0;
	}
}

GodDrawing *
god_drawing_renderer_gdk_get_drawing  (GodDrawingRendererGdk *renderer)
{
	if (renderer->priv->drawing)
		g_object_ref (renderer->priv->drawing);
	return renderer->priv->drawing;
}

void
god_drawing_renderer_gdk_set_drawing  (GodDrawingRendererGdk *renderer,
					  GodDrawing *drawing)
{
	if (renderer->priv->drawing)
		g_object_unref (renderer->priv->drawing);
	renderer->priv->drawing = drawing;
	if (renderer->priv->drawing)
		g_object_ref (renderer->priv->drawing);
}

GdkDrawable *
god_drawing_renderer_gdk_get_drawable  (GodDrawingRendererGdk *renderer)
{
	if (renderer->priv->drawable)
		g_object_ref (renderer->priv->drawable);
	return renderer->priv->drawable;
}

void
god_drawing_renderer_gdk_set_drawable  (GodDrawingRendererGdk *renderer,
					GdkDrawable *drawable)
{
	if (renderer->priv->drawable)
		g_object_unref (renderer->priv->drawable);
	renderer->priv->drawable = drawable;
	if (renderer->priv->drawable)
		g_object_ref (renderer->priv->drawable);
	update_units_per_pixel (renderer);
}

GdkGC *
god_drawing_renderer_gdk_get_gc  (GodDrawingRendererGdk *renderer)
{
	if (renderer->priv->gc)
		g_object_ref (renderer->priv->gc);
	return renderer->priv->gc;
}

void
god_drawing_renderer_gdk_set_gc  (GodDrawingRendererGdk *renderer,
				  GdkGC *gc)
{
	if (renderer->priv->gc)
		g_object_unref (renderer->priv->gc);
	renderer->priv->gc = gc;
	if (renderer->priv->gc)
		g_object_ref (renderer->priv->gc);
	update_units_per_pixel (renderer);
}

GodAnchor *
god_drawing_renderer_gdk_get_extents  (GodDrawingRendererGdk *renderer)
{
	if (renderer->priv->extents)
		g_object_ref (renderer->priv->extents);
	return renderer->priv->extents;
}

void
god_drawing_renderer_gdk_set_extents  (GodDrawingRendererGdk *renderer,
					  GodAnchor *extents)
{
	if (renderer->priv->extents)
		g_object_unref (renderer->priv->extents);
	renderer->priv->extents = extents;
	if (renderer->priv->extents)
		g_object_ref (renderer->priv->extents);
	update_units_per_pixel (renderer);
}

static GdkPixbuf *
get_pixbuf (GodDrawingRendererGdk *renderer,
	    int which_pic)
{
	GodDrawingGroup *drawing_group;
	GdkPixbuf *ret_val = NULL;

	if (which_pic < 0)
		return NULL;

	drawing_group = god_drawing_get_drawing_group (renderer->priv->drawing);
	if (drawing_group) {
		GodImageStore *image_store = god_drawing_group_get_image_store (drawing_group);
		if (which_pic < god_image_store_get_image_count (image_store)) {
			GodImage *image = god_image_store_get_image (image_store, which_pic);
			ret_val = god_image_get_pixbuf (image);
			g_object_unref (image);
		}
		g_object_unref (image_store);
		g_object_unref (drawing_group);
	}
	return ret_val;
}

typedef struct {
	GodDrawingRendererGdk *renderer;
	GdkRectangle *rect;
	long long y_ofs;
	const GodDefaultAttributes *default_attributes;
} DrawTextContext;

static gboolean
make_absolute (PangoAttribute *attr, gpointer user_data)
{
	DrawTextContext *draw_context = user_data;
	if (attr->klass->type == PANGO_ATTR_SIZE &&
	    ! ((PangoAttrSize *) attr)->absolute) {
		PangoAttrSize *size_attr = (PangoAttrSize *) attr;
		size_attr->size = GO_PT_TO_UN ((long long) size_attr->size) / draw_context->renderer->priv->y_units_per_pixel;
		size_attr->absolute = TRUE;
	}
	return FALSE;
}

static void
draw_text (GodTextModel *text,
	   GodTextModelParagraph *paragraph,
	   gpointer user_data)
{
	int height;
	PangoLayout *layout;
	DrawTextContext *draw_context = user_data;
	double space_before = 0;
	double space_after = 0;
	double indent = 0;
	const GList *iterator;
	PangoAttrList *attributes;
	GodParagraphAlignment alignment = GOD_PARAGRAPH_ALIGNMENT_LEFT;
	const GodParagraphAttributes *default_para_attributes;
	gunichar bullet_character = 0;
	double bullet_indent = 0;
	double bullet_size = 0;
	char *bullet_family = NULL;
	gboolean bullet_on = FALSE;
	PangoFontDescription *bullet_desc;
	PangoAttrIterator *attr_iterator;

	if (draw_context->default_attributes) {
		default_para_attributes = god_default_attributes_get_paragraph_attributes_for_indent ((GodDefaultAttributes *)draw_context->default_attributes, paragraph->indent);
		if (default_para_attributes) {
			g_object_get ((GodParagraphAttributes *) default_para_attributes,
				      "space_before", &space_before,
				      "space_after", &space_after,
				      "indent", &indent,
				      "alignment", &alignment,
				      "bullet_character", &bullet_character,
				      "bullet_indent", &bullet_indent,
				      "bullet_size", &bullet_size,
				      "bullet_family", &bullet_family,
				      "bullet_on", &bullet_on,
				      NULL);
		}
	}
	if (paragraph->para_attributes) {
		GodParagraphAttributesFlags flags;
		g_object_get (paragraph->para_attributes,
			      "flags", &flags,
			      NULL);
		if (flags & GOD_PARAGRAPH_ATTRIBUTES_FLAGS_SPACE_BEFORE)
			g_object_get (paragraph->para_attributes,
				      "space_before", &space_before,
				      NULL);
		if (flags & GOD_PARAGRAPH_ATTRIBUTES_FLAGS_SPACE_AFTER)
			g_object_get (paragraph->para_attributes,
				      "space_after", &space_after,
				      NULL);
		if (flags & GOD_PARAGRAPH_ATTRIBUTES_FLAGS_INDENT)
			g_object_get (paragraph->para_attributes,
				      "indent", &indent,
				      NULL);
		if (flags & GOD_PARAGRAPH_ATTRIBUTES_FLAGS_ALIGNMENT)
			g_object_get (paragraph->para_attributes,
				      "alignment", &alignment,
				      NULL);
		if (flags & GOD_PARAGRAPH_ATTRIBUTES_FLAGS_BULLET_CHARACTER)
			g_object_get (paragraph->para_attributes,
				      "bullet_character", &bullet_character,
				      NULL);
		if (flags & GOD_PARAGRAPH_ATTRIBUTES_FLAGS_BULLET_INDENT)
			g_object_get (paragraph->para_attributes,
				      "bullet_indent", &bullet_indent,
				      NULL);
		if (flags & GOD_PARAGRAPH_ATTRIBUTES_FLAGS_BULLET_SIZE)
			g_object_get (paragraph->para_attributes,
				      "bullet_size", &bullet_size,
				      NULL);
		if (flags & GOD_PARAGRAPH_ATTRIBUTES_FLAGS_BULLET_FAMILY) {
			g_free (bullet_family);
			bullet_family = NULL;
			g_object_get (paragraph->para_attributes,
				      "bullet_family", &bullet_family,
				      NULL);
		}
		if (flags & GOD_PARAGRAPH_ATTRIBUTES_FLAGS_BULLET_ON) {
			g_object_get (paragraph->para_attributes,
				      "bullet_on", &bullet_on,
				      NULL);
		}
	}
	draw_context->y_ofs += space_before;

	layout = pango_layout_new (gdk_pango_context_get_for_screen (gdk_screen_get_default()));
	pango_layout_set_alignment (layout, alignment == GOD_PARAGRAPH_ALIGNMENT_JUSTIFY ? PANGO_ALIGN_LEFT : alignment);
	pango_layout_set_width (layout, draw_context->rect->width * PANGO_SCALE);
	if (strchr (paragraph->text, 0xb)) {
		int i;
		char *paragraph_text = g_strdup (paragraph->text);
		for (i = 0; paragraph_text[i]; i++) {
			if (paragraph_text[i] == 0xb)
				paragraph_text[i] = '\r';
		}
		pango_layout_set_text (layout, paragraph_text, -1);
		g_free (paragraph_text);
	} else {
		pango_layout_set_text (layout, paragraph->text, -1);
	}
	pango_layout_set_auto_dir (layout, FALSE);
	if (paragraph->char_attributes)
		attributes = pango_attr_list_copy (paragraph->char_attributes);
	else
		attributes = pango_attr_list_new ();
	if (draw_context->default_attributes) {
		iterator = god_default_attributes_get_pango_attributes_for_indent ((GodDefaultAttributes *)draw_context->default_attributes, paragraph->indent);
		for (; iterator; iterator = iterator->next) {
			PangoAttribute *attr = pango_attribute_copy (iterator->data);
			attr->start_index = 0;
			attr->end_index = -1;
			pango_attr_list_insert_before (attributes, attr);
		}
	}
	pango_attr_list_filter (attributes, make_absolute, draw_context);
	pango_layout_set_attributes (layout, attributes);
	attr_iterator = pango_attr_list_get_iterator (attributes);
	bullet_desc = pango_font_description_new ();
	pango_attr_iterator_get_font (attr_iterator, bullet_desc, NULL, NULL);
	pango_attr_iterator_destroy (attr_iterator);
	pango_attr_list_unref (attributes);
	gdk_draw_layout (draw_context->renderer->priv->drawable,
			 draw_context->renderer->priv->gc,
			 draw_context->rect->x + indent / draw_context->renderer->priv->x_units_per_pixel,
			 draw_context->rect->y + draw_context->y_ofs / draw_context->renderer->priv->y_units_per_pixel,
			 layout);

	pango_layout_get_size (layout, NULL, &height);

	g_object_unref (layout);
	layout = NULL;

	if (bullet_character != 0 &&
	    bullet_character != 0xe011 &&
	    bullet_size != 0 &&
	    bullet_family != NULL &&
	    bullet_on) {
		char utf8[7];
		int length;
		layout = pango_layout_new (gdk_pango_context_get_for_screen (gdk_screen_get_default()));
		pango_layout_set_alignment (layout, PANGO_ALIGN_LEFT);
		length = g_unichar_to_utf8 (bullet_character, utf8);
		pango_layout_set_text (layout, utf8, length);
		pango_layout_set_auto_dir (layout, FALSE);
#if 0
		pango_font_description_set_absolute_size (bullet_desc, 
							  GO_PT_TO_UN ((go_unit_t) bullet_size * PANGO_SCALE) /
							  draw_context->renderer->priv->y_units_per_pixel);
#endif
		pango_font_description_set_size (bullet_desc, pango_font_description_get_size (bullet_desc) * sqrt (bullet_size));
		pango_font_description_set_family (bullet_desc, bullet_family);
		pango_layout_set_font_description (layout, bullet_desc);

		gdk_draw_layout (draw_context->renderer->priv->drawable,
				 draw_context->renderer->priv->gc,
				 draw_context->rect->x + bullet_indent / draw_context->renderer->priv->x_units_per_pixel,
				 draw_context->rect->y + draw_context->y_ofs / draw_context->renderer->priv->y_units_per_pixel,
				 layout);
		pango_font_description_free (bullet_desc);
		g_object_unref (layout);
		layout = NULL;
	}

	draw_context->y_ofs += height * draw_context->renderer->priv->y_units_per_pixel / PANGO_SCALE;
	draw_context->y_ofs += space_after;
}

static void
god_drawing_renderer_gdk_render_shape (GodDrawingRendererGdk *renderer,
				       GdkRectangle          *area,
				       GodShape              *shape)
{
	GodAnchor *anchor;
	GdkRectangle rect;
	GdkRectangle intersection;
	GoRect anchor_rect;

	anchor = god_shape_get_anchor (shape);
	if (anchor) {
		god_anchor_get_rect (anchor, &anchor_rect);
		rect.x = anchor_rect.left / renderer->priv->x_units_per_pixel;
		rect.width = anchor_rect.right / renderer->priv->x_units_per_pixel - rect.x;
		rect.y = anchor_rect.top / renderer->priv->y_units_per_pixel;
		rect.height = anchor_rect.bottom / renderer->priv->y_units_per_pixel - rect.y;
		g_object_unref (anchor);
	} else {
		rect.x = 0;
		rect.y = 0;
		gdk_drawable_get_size (renderer->priv->drawable, &(rect.width), &(rect.height));
	}

	if (!gdk_rectangle_intersect (area, &rect, &intersection))
		return;

	{
		GodPropertyTable *prop_table;
		gboolean filled;
		GodFillType fill_type;
		GodTextModel *text_model;
		DrawTextContext *draw_context;

		prop_table = god_shape_get_prop_table (shape);
		filled = god_property_table_get_flag (prop_table,
						      GOD_PROPERTY_FILLED,
						      TRUE);
		fill_type = god_property_table_get_int (prop_table,
							GOD_PROPERTY_FILL_TYPE,
							GOD_FILL_TYPE_SOLID);
		if (filled && fill_type == GOD_FILL_TYPE_PICTURE) {
			int which_pic = god_property_table_get_int (prop_table,
								    GOD_PROPERTY_BLIP_ID,
								    -1);
			GdkPixbuf *pixbuf = get_pixbuf (renderer, which_pic);
			if (pixbuf) {
				GdkPixbuf *to_blit = gdk_pixbuf_new (gdk_pixbuf_get_colorspace (pixbuf),
								     gdk_pixbuf_get_has_alpha (pixbuf),
								     gdk_pixbuf_get_bits_per_sample (pixbuf),
								     intersection.width, intersection.height);
				double offset_x, offset_y;
				double scale_x, scale_y;

				scale_x = (double) rect.width / (double) gdk_pixbuf_get_width (pixbuf);
				scale_y = (double) rect.height / (double) gdk_pixbuf_get_height (pixbuf);
				offset_x = rect.x - intersection.x;
				offset_y = rect.y - intersection.y;

				gdk_pixbuf_scale (pixbuf,
						  to_blit,
						  0,
						  0,
						  intersection.width,
						  intersection.height,
						  offset_x,
						  offset_y,
						  scale_x,
						  scale_y,
						  GDK_INTERP_HYPER);
			

				gdk_draw_pixbuf (renderer->priv->drawable,
						 renderer->priv->gc,
						 to_blit,
						 0,
						 0,
						 intersection.x,
						 intersection.y,
						 intersection.width,
						 intersection.height,
						 GDK_RGB_DITHER_NORMAL,
						 intersection.x,
						 intersection.y);

				g_object_unref (to_blit);
				g_object_unref (pixbuf);
			}
		}
		if (filled && fill_type == GOD_FILL_TYPE_SOLID) {
			GdkColor old_color, color;
			guint32 colornum = god_property_table_get_uint (prop_table,
								     GOD_PROPERTY_FILL_COLOR,
								     0xffffff);
			GdkGCValues values;

			gdk_gc_get_values (renderer->priv->gc,
					   &values);
			old_color = values.foreground;

			color.red = (colornum & 0xff0000) >> 16;
			color.blue = (colornum & 0xff00) >> 8;
			color.green = colornum & 0xff;

			color.red = color.red | (color.red << 8);
			color.blue = color.blue | (color.blue << 8);
			color.green = color.green + (color.green << 8);

			gdk_gc_set_rgb_fg_color (renderer->priv->gc, &color);

			gdk_draw_rectangle (renderer->priv->drawable,
					    renderer->priv->gc,
					    TRUE,
					    intersection.x,
					    intersection.y,
					    intersection.width,
					    intersection.height);

			gdk_gc_set_foreground (renderer->priv->gc, &old_color);
		}
		text_model = god_shape_get_text_model (shape);
		draw_context = g_new (DrawTextContext, 1);
		draw_context->renderer = renderer;
		draw_context->rect = &rect;
		draw_context->y_ofs = 0;
		draw_context->default_attributes = god_text_model_get_default_attributes (text_model);
		god_text_model_paragraph_foreach (text_model, draw_text, draw_context);
		g_object_unref (prop_table);
	}

	{
		int i, child_count;
		GodShape *child;
		child_count = god_shape_get_child_count (shape);
		for (i = 0; i < child_count; i++) {
			child = god_shape_get_child (shape, i);
			god_drawing_renderer_gdk_render_shape (renderer, area, child);
		}
	}
}

void
god_drawing_renderer_gdk_render        (GodDrawingRendererGdk *renderer,
					GdkRectangle          *area)
{
	GodShape *shape;

	update_units_per_pixel (renderer);
	shape = god_drawing_get_background (renderer->priv->drawing);
	if (shape) {
		god_drawing_renderer_gdk_render_shape (renderer,
						       area,
						       shape);
		g_object_unref (shape);
	}

	shape = god_drawing_get_root_shape (renderer->priv->drawing);
	if (shape) {
		god_drawing_renderer_gdk_render_shape (renderer,
						       area,
						       shape);
		g_object_unref (shape);
	}
}

static void
god_drawing_renderer_gdk_init (GObject *object)
{
	GodDrawingRendererGdk *renderer = GOD_DRAWING_RENDERER_GDK (object);

	renderer->priv = g_new0 (GodDrawingRendererGdkPrivate, 1);
}

static void
god_drawing_renderer_gdk_dispose (GObject *object)
{
	GodDrawingRendererGdk *renderer = GOD_DRAWING_RENDERER_GDK (object);

	if (renderer->priv == NULL)
		return;

	if (renderer->priv->drawing)
		g_object_unref (renderer->priv->drawing);
	if (renderer->priv->drawable)
		g_object_unref (renderer->priv->drawable);
	if (renderer->priv->gc)
		g_object_unref (renderer->priv->gc);
	if (renderer->priv->extents)
		g_object_unref (renderer->priv->extents);
	g_free (renderer->priv);
	renderer->priv = NULL;

	G_OBJECT_CLASS (parent_class)->dispose (object);
}

static void
god_drawing_renderer_gdk_class_init (GodDrawingRendererGdkClass *class)
{
	GObjectClass *object_class;

	object_class           = (GObjectClass *) class;

	parent_class           = g_type_class_peek_parent (class);

	object_class->dispose = god_drawing_renderer_gdk_dispose;
}

GSF_CLASS (GodDrawingRendererGdk, god_drawing_renderer_gdk,
	   god_drawing_renderer_gdk_class_init, god_drawing_renderer_gdk_init,
	   G_TYPE_OBJECT)
