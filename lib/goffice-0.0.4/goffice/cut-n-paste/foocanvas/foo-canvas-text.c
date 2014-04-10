/* File import from foocanvas to libgoffice by import-foocanvas.  Do not edit.  */

#undef GTK_DISABLE_DEPRECATED
#include <goffice/goffice-config.h>
#include <glib/gi18n.h>
#include <gsf/gsf-impl-utils.h>
/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * Copyright (C) 1997, 1998, 1999, 2000 Free Software Foundation
 * All rights reserved.
 *
 * This file is part of the Gnome Library.
 *
 * The Gnome Library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * The Gnome Library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with the Gnome Library; see the file COPYING.LIB.  If
 * not, write to the Free Software Foundation, Inc., 51 Franklin St,
 * Fifth Floor, Boston, MA  02110-1301 USA.
 */
/*
  @NOTATION@
 */
/* Text item type for FooCanvas widget
 *
 * FooCanvas is basically a port of the Tk toolkit's most excellent canvas
 * widget.  Tk is copyrighted by the Regents of the University of California,
 * Sun Microsystems, and other parties.
 *
 *
 * Author: Federico Mena <federico@nuclecu.unam.mx>
 * Port to Pango co-done by Gergõ Érdi <cactus@cactus.rulez.org>
 */

#include <math.h>
#include <string.h>
#include "foo-canvas-text.h"

#include "foo-canvas-util.h"
#include "foo-canvas-i18n.h"



/* Object argument IDs */
enum {
	PROP_0,

	/* Text contents */
	PROP_TEXT,
	PROP_MARKUP,

	/* Position */
	PROP_X,
	PROP_Y,

	/* Font */
	PROP_FONT,
	PROP_FONT_DESC,
	PROP_FAMILY, PROP_FAMILY_SET,

	/* Style */
	PROP_ATTRIBUTES,
	PROP_STYLE,         PROP_STYLE_SET,
	PROP_VARIANT,       PROP_VARIANT_SET,
	PROP_WEIGHT,        PROP_WEIGHT_SET,
	PROP_STRETCH,	    PROP_STRETCH_SET,
	PROP_SIZE,          PROP_SIZE_SET,
	PROP_SIZE_POINTS,
	PROP_STRIKETHROUGH, PROP_STRIKETHROUGH_SET,
	PROP_UNDERLINE,     PROP_UNDERLINE_SET,
	PROP_RISE,          PROP_RISE_SET,
	PROP_SCALE,         PROP_SCALE_SET,

	/* Clipping */
	PROP_ANCHOR,
	PROP_JUSTIFICATION,
	PROP_CLIP_WIDTH,
	PROP_CLIP_HEIGHT,
	PROP_CLIP,
	PROP_WRAP_WIDTH,
	PROP_X_OFFSET,
	PROP_Y_OFFSET,

	/* Coloring */
	PROP_FILL_COLOR,
	PROP_FILL_COLOR_GDK,
	PROP_FILL_COLOR_RGBA,
	PROP_FILL_STIPPLE,

	/* Rendered size accessors */
	PROP_TEXT_WIDTH,
	PROP_TEXT_HEIGHT
};

struct _FooCanvasTextPrivate {
	gint placeholder;
};

static void foo_canvas_text_class_init (FooCanvasTextClass *class);
static void foo_canvas_text_init (FooCanvasText *text);
static void foo_canvas_text_destroy (GtkObject *object);
static void foo_canvas_text_set_property (GObject            *object,
					    guint               param_id,
					    const GValue       *value,
					    GParamSpec         *pspec);
static void foo_canvas_text_get_property (GObject            *object,
					    guint               param_id,
					    GValue             *value,
					    GParamSpec         *pspec);

static void   foo_canvas_text_update    (FooCanvasItem  *item,
					   double            i2w_dx,
					   double            i2w_dy,
					   int               flags);
static void   foo_canvas_text_realize   (FooCanvasItem  *item);
static void   foo_canvas_text_unrealize (FooCanvasItem  *item);
static void   foo_canvas_text_draw      (FooCanvasItem  *item,
					   GdkDrawable      *drawable,
					   GdkEventExpose   *expose);
static double foo_canvas_text_point     (FooCanvasItem  *item,
					   double            x,
					   double            y,
					   int               cx,
					   int               cy,
					   FooCanvasItem **actual_item);
static void   foo_canvas_text_translate (FooCanvasItem  *item,
					   double            dx,
					   double            dy);
static void   foo_canvas_text_bounds    (FooCanvasItem  *item,
					   double           *x1,
					   double           *y1,
					   double           *x2,
					   double           *y2);

static void foo_canvas_text_set_markup (FooCanvasText *textitem,
					  const gchar     *markup);

static void foo_canvas_text_set_font_desc    (FooCanvasText *textitem,
					        PangoFontDescription *font_desc);

static void foo_canvas_text_apply_font_desc  (FooCanvasText *textitem);
static void foo_canvas_text_apply_attributes (FooCanvasText *textitem);

static void add_attr (PangoAttrList  *attr_list,
		      PangoAttribute *attr);

static FooCanvasItemClass *parent_class;



/**
 * foo_canvas_text_get_type:
 * @void:
 *
 * Registers the &FooCanvasText class if necessary, and returns the type ID
 * associated to it.
 *
 * Return value: The type ID of the &FooCanvasText class.
 **/
GtkType
foo_canvas_text_get_type (void)
{
	static GtkType text_type = 0;

	if (!text_type) {
		/* FIXME: Convert to gobject style.  */
		static const GtkTypeInfo text_info = {
			(char *)"FooCanvasText",
			sizeof (FooCanvasText),
			sizeof (FooCanvasTextClass),
			(GtkClassInitFunc) foo_canvas_text_class_init,
			(GtkObjectInitFunc) foo_canvas_text_init,
			NULL, /* reserved_1 */
			NULL, /* reserved_2 */
			(GtkClassInitFunc) NULL
		};

		text_type = gtk_type_unique (foo_canvas_item_get_type (), &text_info);
	}

	return text_type;
}

/* Class initialization function for the text item */
static void
foo_canvas_text_class_init (FooCanvasTextClass *class)
{
	GObjectClass *gobject_class;
	GtkObjectClass *object_class;
	FooCanvasItemClass *item_class;

	gobject_class = (GObjectClass *) class;
	object_class = (GtkObjectClass *) class;
	item_class = (FooCanvasItemClass *) class;

	parent_class = gtk_type_class (foo_canvas_item_get_type ());

	gobject_class->set_property = foo_canvas_text_set_property;
	gobject_class->get_property = foo_canvas_text_get_property;

	/* Text */
        g_object_class_install_property
                (gobject_class,
                 PROP_TEXT,
                 g_param_spec_string ("text",
				      _("Text"),
				      _("Text to render"),
                                      NULL,
                                      GSF_PARAM_STATIC | G_PARAM_READWRITE));

        g_object_class_install_property
                (gobject_class,
                 PROP_MARKUP,
                 g_param_spec_string ("markup",
				      _("Markup"),
				      _("Marked up text to render"),
				      NULL,
                                      (G_PARAM_WRITABLE)));

	/* Position */
        g_object_class_install_property
                (gobject_class,
                 PROP_X,
                 g_param_spec_double ("x", NULL, NULL,
				      -G_MAXDOUBLE, G_MAXDOUBLE, 0.0,
				      GSF_PARAM_STATIC | G_PARAM_READWRITE));

        g_object_class_install_property
                (gobject_class,
                 PROP_Y,
                 g_param_spec_double ("y", NULL, NULL,
				      -G_MAXDOUBLE, G_MAXDOUBLE, 0.0,
				      GSF_PARAM_STATIC | G_PARAM_READWRITE));


	/* Font */
	g_object_class_install_property
                (gobject_class,
                 PROP_FONT,
                 g_param_spec_string ("font",
				      _("Font"),
				      _("Font description as a string"),
                                      NULL,
                                      GSF_PARAM_STATIC | G_PARAM_READWRITE));

        g_object_class_install_property
		(gobject_class,
		 PROP_FONT_DESC,
		 g_param_spec_boxed ("font-desc",
				     _("Font description"),
				     _("Font description as a PangoFontDescription struct"),
				     PANGO_TYPE_FONT_DESCRIPTION,
				     GSF_PARAM_STATIC | G_PARAM_READWRITE));

	g_object_class_install_property
		(gobject_class,
		 PROP_FAMILY,
		 g_param_spec_string ("family",
				      _("Font family"),
				      _("Name of the font family, e.g. Sans, Helvetica, Times, Monospace"),
				      NULL,
				      GSF_PARAM_STATIC | G_PARAM_READWRITE));

	/* Style */
        g_object_class_install_property
                (gobject_class,
                 PROP_ATTRIBUTES,
                 g_param_spec_boxed ("attributes", NULL, NULL,
				     PANGO_TYPE_ATTR_LIST,
				     GSF_PARAM_STATIC | G_PARAM_READWRITE));

	g_object_class_install_property
		(gobject_class,
		 PROP_STYLE,
		 g_param_spec_enum ("style",
				    _("Font style"),
				    _("Font style"),
				    PANGO_TYPE_STYLE,
				    PANGO_STYLE_NORMAL,
				    GSF_PARAM_STATIC | G_PARAM_READWRITE));

	g_object_class_install_property
		(gobject_class,
		 PROP_VARIANT,
		 g_param_spec_enum ("variant",
				    _("Font variant"),
				    _("Font variant"),
				    PANGO_TYPE_VARIANT,
				    PANGO_VARIANT_NORMAL,
				    GSF_PARAM_STATIC | G_PARAM_READWRITE));

	g_object_class_install_property
		(gobject_class,
		 PROP_WEIGHT,
		 g_param_spec_int ("weight",
				   _("Font weight"),
				   _("Font weight"),
				   0,
				   G_MAXINT,
				   PANGO_WEIGHT_NORMAL,
				   GSF_PARAM_STATIC | G_PARAM_READWRITE));


	g_object_class_install_property
		(gobject_class,
		 PROP_STRETCH,
		 g_param_spec_enum ("stretch",
				    _("Font stretch"),
				    _("Font stretch"),
				    PANGO_TYPE_STRETCH,
				    PANGO_STRETCH_NORMAL,
				    GSF_PARAM_STATIC | G_PARAM_READWRITE));

	g_object_class_install_property
		(gobject_class,
		 PROP_SIZE,
		 g_param_spec_int ("size",
				   _("Font size"),
				   _("Font size"),
				   0,
				   G_MAXINT,
				   0,
				   GSF_PARAM_STATIC | G_PARAM_READWRITE));

	g_object_class_install_property
		(gobject_class,
		PROP_SIZE_POINTS,
		g_param_spec_double ("size-points",
				     _("Font points"),
				     _("Font size in points"),
				     0.0,
				     G_MAXDOUBLE,
				     0.0,
				     GSF_PARAM_STATIC | G_PARAM_READWRITE));

	g_object_class_install_property
		(gobject_class,
		 PROP_RISE,
		 g_param_spec_int ("rise",
				   _("Rise"),
				   _("Offset of text above the baseline (below the baseline if rise is negative)"),
				   -G_MAXINT,
				   G_MAXINT,
				   0,
				   GSF_PARAM_STATIC | G_PARAM_READWRITE));

	g_object_class_install_property
		(gobject_class,
		 PROP_STRIKETHROUGH,
		 g_param_spec_boolean ("strikethrough",
				       _("Strikethrough"),
				       _("Whether to strike through the text"),
				       FALSE,
				       GSF_PARAM_STATIC | G_PARAM_READWRITE));

	g_object_class_install_property
		(gobject_class,
		 PROP_UNDERLINE,
		 g_param_spec_enum ("underline",
				    _("Underline"),
				    _("Style of underline for this text"),
				    PANGO_TYPE_UNDERLINE,
				    PANGO_UNDERLINE_NONE,
				    GSF_PARAM_STATIC | G_PARAM_READWRITE));

	g_object_class_install_property
		(gobject_class,
		 PROP_SCALE,
		 g_param_spec_double ("scale",
				      _("Scale"),
				      _("Size of font, relative to default size"),
				      0.0,
				      G_MAXDOUBLE,
				      1.0,
				      GSF_PARAM_STATIC | G_PARAM_READWRITE));

        g_object_class_install_property
		(gobject_class,
                 PROP_ANCHOR,
                 g_param_spec_enum ("anchor", NULL, NULL,
                                    GTK_TYPE_ANCHOR_TYPE,
                                    GTK_ANCHOR_CENTER,
                                    GSF_PARAM_STATIC | G_PARAM_READWRITE));
        g_object_class_install_property
                (gobject_class,
                 PROP_JUSTIFICATION,
                 g_param_spec_enum ("justification", NULL, NULL,
                                    GTK_TYPE_JUSTIFICATION,
                                    GTK_JUSTIFY_LEFT,
                                    GSF_PARAM_STATIC | G_PARAM_READWRITE));
        g_object_class_install_property
                (gobject_class,
                 PROP_CLIP_WIDTH,
                 g_param_spec_double ("clip-width", NULL, NULL,
				      -G_MAXDOUBLE, G_MAXDOUBLE, 0.0,
				      GSF_PARAM_STATIC | G_PARAM_READWRITE));
        g_object_class_install_property
                (gobject_class,
                 PROP_CLIP_HEIGHT,
                 g_param_spec_double ("clip-height", NULL, NULL,
				      -G_MAXDOUBLE, G_MAXDOUBLE, 0.0,
				      GSF_PARAM_STATIC | G_PARAM_READWRITE));
        g_object_class_install_property
                (gobject_class,
                 PROP_CLIP,
                 g_param_spec_boolean ("clip", NULL, NULL,
				       FALSE,
				       GSF_PARAM_STATIC | G_PARAM_READWRITE));
        g_object_class_install_property
                (gobject_class,
                 PROP_WRAP_WIDTH,
                 g_param_spec_double ("wrap-width", NULL, NULL,
				      -G_MAXDOUBLE, G_MAXDOUBLE, 0.0,
				      GSF_PARAM_STATIC | G_PARAM_READWRITE));
        g_object_class_install_property
                (gobject_class,
                 PROP_X_OFFSET,
                 g_param_spec_double ("x-offset", NULL, NULL,
				      -G_MAXDOUBLE, G_MAXDOUBLE, 0.0,
				      GSF_PARAM_STATIC | G_PARAM_READWRITE));
        g_object_class_install_property
                (gobject_class,
                 PROP_Y_OFFSET,
                 g_param_spec_double ("y-offset", NULL, NULL,
				      -G_MAXDOUBLE, G_MAXDOUBLE, 0.0,
				      GSF_PARAM_STATIC | G_PARAM_READWRITE));
        g_object_class_install_property
                (gobject_class,
                 PROP_FILL_COLOR,
                 g_param_spec_string ("fill-color",
				      _("Color"),
				      _("Text color, as string"),
                                      NULL,
                                      GSF_PARAM_STATIC | G_PARAM_READWRITE));
        g_object_class_install_property
                (gobject_class,
                 PROP_FILL_COLOR_GDK,
                 g_param_spec_boxed ("fill-color-gdk",
				     _("Color"),
				     _("Text color, as a GdkColor"),
				     GDK_TYPE_COLOR,
				     GSF_PARAM_STATIC | G_PARAM_READWRITE));
        g_object_class_install_property
                (gobject_class,
                 PROP_FILL_COLOR_RGBA,
                 g_param_spec_uint ("fill-color-rgba",
				    _("Color"),
				    _("Text color, as an R/G/B/A combined integer"),
				    0, G_MAXUINT, 0,
				    GSF_PARAM_STATIC | G_PARAM_READWRITE));
        g_object_class_install_property
                (gobject_class,
                 PROP_FILL_STIPPLE,
                 g_param_spec_object ("fill-stipple", NULL, NULL,
                                      GDK_TYPE_DRAWABLE,
                                      GSF_PARAM_STATIC | G_PARAM_READWRITE));
        g_object_class_install_property
                (gobject_class,
                 PROP_TEXT_WIDTH,
                 g_param_spec_double ("text-width",
				      _("Text width"),
				      _("Width of the rendered text"),
				      0.0, G_MAXDOUBLE, 0.0,
				      GSF_PARAM_STATIC | G_PARAM_READWRITE));
        g_object_class_install_property
                (gobject_class,
                 PROP_TEXT_HEIGHT,
                 g_param_spec_double ("text-height",
				      _("Text height"),
				      _("Height of the rendered text"),
				      0.0, G_MAXDOUBLE, 0.0,
				      GSF_PARAM_STATIC | G_PARAM_READWRITE));

	/* Style props are set (explicitly applied) or not */
#define ADD_SET_PROP(propname, propval, nick, blurb) g_object_class_install_property (gobject_class, propval, g_param_spec_boolean (propname, nick, blurb, FALSE, GSF_PARAM_STATIC | G_PARAM_READWRITE))

	ADD_SET_PROP ("family-set", PROP_FAMILY_SET,
		      _("Font family set"),
		      _("Whether this tag affects the font family"));

	ADD_SET_PROP ("style-set", PROP_STYLE_SET,
		      _("Font style set"),
		      _("Whether this tag affects the font style"));

	ADD_SET_PROP ("variant-set", PROP_VARIANT_SET,
		      _("Font variant set"),
		      _("Whether this tag affects the font variant"));

	ADD_SET_PROP ("weight-set", PROP_WEIGHT_SET,
		      _("Font weight set"),
		      _("Whether this tag affects the font weight"));

	ADD_SET_PROP ("stretch-set", PROP_STRETCH_SET,
		      _("Font stretch set"),
		      _("Whether this tag affects the font stretch"));

	ADD_SET_PROP ("size-set", PROP_SIZE_SET,
		      _("Font size set"),
		      _("Whether this tag affects the font size"));

	ADD_SET_PROP ("rise-set", PROP_RISE_SET,
		      _("Rise set"),
		      _("Whether this tag affects the rise"));

	ADD_SET_PROP ("strikethrough-set", PROP_STRIKETHROUGH_SET,
		      _("Strikethrough set"),
		      _("Whether this tag affects strikethrough"));

	ADD_SET_PROP ("underline-set", PROP_UNDERLINE_SET,
		      _("Underline set"),
		      _("Whether this tag affects underlining"));

	ADD_SET_PROP ("scale-set", PROP_SCALE_SET,
		      _("Scale set"),
		      _("Whether this tag affects font scaling"));
#undef ADD_SET_PROP

	object_class->destroy = foo_canvas_text_destroy;

	item_class->update = foo_canvas_text_update;
	item_class->realize = foo_canvas_text_realize;
	item_class->unrealize = foo_canvas_text_unrealize;
	item_class->draw = foo_canvas_text_draw;
	item_class->point = foo_canvas_text_point;
	item_class->translate = foo_canvas_text_translate;
	item_class->bounds = foo_canvas_text_bounds;
}

/* Object initialization function for the text item */
static void
foo_canvas_text_init (FooCanvasText *text)
{
	text->x = 0.0;
	text->y = 0.0;
	text->anchor = GTK_ANCHOR_CENTER;
	text->justification = GTK_JUSTIFY_LEFT;
	text->clip_width = 0.0;
	text->clip_height = 0.0;
	text->xofs = 0.0;
	text->yofs = 0.0;
	text->layout = NULL;

	text->font_desc = NULL;

	text->underline     = PANGO_UNDERLINE_NONE;
	text->strikethrough = FALSE;
	text->rise          = 0;

	text->underline_set = FALSE;
	text->strike_set    = FALSE;
	text->rise_set      = FALSE;

	text->priv = g_new (FooCanvasTextPrivate, 1);
}

/* Destroy handler for the text item */
static void
foo_canvas_text_destroy (GtkObject *object)
{
	FooCanvasText *text;

	g_return_if_fail (FOO_IS_CANVAS_TEXT (object));

	text = FOO_CANVAS_TEXT (object);

	/* remember, destroy can be run multiple times! */

	g_free (text->text);
	text->text = NULL;

	if (text->layout)
	    g_object_unref (G_OBJECT (text->layout));
	text->layout = NULL;

	if (text->font_desc) {
		pango_font_description_free (text->font_desc);
		text->font_desc = NULL;
	}

	if (text->attr_list)
		pango_attr_list_unref (text->attr_list);
	text->attr_list = NULL;

	if (text->stipple)
		g_object_unref (text->stipple);
	text->stipple = NULL;

	g_free (text->priv);
	text->priv = NULL;

	if (GTK_OBJECT_CLASS (parent_class)->destroy)
		(* GTK_OBJECT_CLASS (parent_class)->destroy) (object);
}

static void
get_bounds (FooCanvasText *text, double *px1, double *py1, double *px2, double *py2)
{
	FooCanvasItem *item;
	double wx, wy;

	item = FOO_CANVAS_ITEM (text);

	/* Get canvas pixel coordinates for text position */


	wx = text->x;
	wy = text->y;
	foo_canvas_item_i2w (item, &wx, &wy);
	foo_canvas_w2c (item->canvas, wx + text->xofs, wy + text->yofs, &text->cx, &text->cy);

	/* Get canvas pixel coordinates for clip rectangle position */

	foo_canvas_w2c (item->canvas, wx, wy, &text->clip_cx, &text->clip_cy);
	text->clip_cwidth = text->clip_width * item->canvas->pixels_per_unit;
	text->clip_cheight = text->clip_height * item->canvas->pixels_per_unit;

	/* Anchor text */

	switch (text->anchor) {
	case GTK_ANCHOR_NW:
	case GTK_ANCHOR_W:
	case GTK_ANCHOR_SW:
		break;

	case GTK_ANCHOR_N:
	case GTK_ANCHOR_CENTER:
	case GTK_ANCHOR_S:
		text->cx -= text->max_width / 2;
		text->clip_cx -= text->clip_cwidth / 2;
		break;

	case GTK_ANCHOR_NE:
	case GTK_ANCHOR_E:
	case GTK_ANCHOR_SE:
		text->cx -= text->max_width;
		text->clip_cx -= text->clip_cwidth;
		break;

	default:
		break;
	}

	switch (text->anchor) {
	case GTK_ANCHOR_NW:
	case GTK_ANCHOR_N:
	case GTK_ANCHOR_NE:
		break;

	case GTK_ANCHOR_W:
	case GTK_ANCHOR_CENTER:
	case GTK_ANCHOR_E:
		text->cy -= text->height / 2;
		text->clip_cy -= text->clip_cheight / 2;
		break;

	case GTK_ANCHOR_SW:
	case GTK_ANCHOR_S:
	case GTK_ANCHOR_SE:
		text->cy -= text->height;
		text->clip_cy -= text->clip_cheight;
		break;

	default:
		break;
	}

	/* Bounds */

	if (text->clip) {
		*px1 = text->clip_cx;
		*py1 = text->clip_cy;
		*px2 = text->clip_cx + text->clip_cwidth;
		*py2 = text->clip_cy + text->clip_cheight;
	} else {
		*px1 = text->cx;
		*py1 = text->cy;
		*px2 = text->cx + text->max_width;
		*py2 = text->cy + text->height;
	}
}

/* Convenience function to set the text's GC's foreground color */
static void
set_text_gc_foreground (FooCanvasText *text)
{
	GdkColor c;

	if (!text->gc)
		return;

	c.pixel = text->pixel;
	gdk_gc_set_foreground (text->gc, &c);
}

/* Sets the stipple pattern for the text */
static void
set_stipple (FooCanvasText *text, GdkBitmap *stipple, int reconfigure)
{
	if (text->stipple && !reconfigure)
		g_object_unref (text->stipple);

	text->stipple = stipple;
	if (stipple && !reconfigure)
		g_object_ref (stipple);

	if (text->gc) {
		if (stipple) {
			gdk_gc_set_stipple (text->gc, stipple);
			gdk_gc_set_fill (text->gc, GDK_STIPPLED);
		} else
			gdk_gc_set_fill (text->gc, GDK_SOLID);
	}
}

static PangoFontMask
get_property_font_set_mask (guint prop_id)
{
  switch (prop_id)
    {
    case PROP_FAMILY_SET:
      return PANGO_FONT_MASK_FAMILY;
    case PROP_STYLE_SET:
      return PANGO_FONT_MASK_STYLE;
    case PROP_VARIANT_SET:
      return PANGO_FONT_MASK_VARIANT;
    case PROP_WEIGHT_SET:
      return PANGO_FONT_MASK_WEIGHT;
    case PROP_STRETCH_SET:
      return PANGO_FONT_MASK_STRETCH;
    case PROP_SIZE_SET:
      return PANGO_FONT_MASK_SIZE;
    }

  return 0;
}

static void
ensure_font (FooCanvasText *text)
{
	if (!text->font_desc)
		text->font_desc = pango_font_description_new ();
}

/* Set_arg handler for the text item */
static void
foo_canvas_text_set_property (GObject            *object,
				guint               param_id,
				const GValue       *value,
				GParamSpec         *pspec)
{
	FooCanvasItem *item;
	FooCanvasText *text;
	GdkColor color = { 0, 0, 0, 0, };
	GdkColor *pcolor;
	gboolean color_changed;
	int have_pixel;
	PangoAlignment align;

	g_return_if_fail (object != NULL);
	g_return_if_fail (FOO_IS_CANVAS_TEXT (object));

	item = FOO_CANVAS_ITEM (object);
	text = FOO_CANVAS_TEXT (object);

	color_changed = FALSE;
	have_pixel = FALSE;


	if (!text->layout) {
		text->layout = gtk_widget_create_pango_layout  (GTK_WIDGET (item->canvas), NULL);
	}

	switch (param_id) {
	case PROP_TEXT:
		if (text->text)
			g_free (text->text);

		text->text = g_value_dup_string (value);
		pango_layout_set_text (text->layout, text->text, -1);

		break;

	case PROP_MARKUP:
		foo_canvas_text_set_markup (text,
					      g_value_get_string (value));
		break;

	case PROP_X:
		text->x = g_value_get_double (value);
		break;

	case PROP_Y:
		text->y = g_value_get_double (value);
		break;

	case PROP_FONT: {
		const char *font_name;
		PangoFontDescription *font_desc;

		font_name = g_value_get_string (value);
		if (font_name)
			font_desc = pango_font_description_from_string (font_name);
		else
			font_desc = NULL;

		foo_canvas_text_set_font_desc (text, font_desc);
		if (font_desc)
			pango_font_description_free (font_desc);

		break;
	}

	case PROP_FONT_DESC:
		foo_canvas_text_set_font_desc (text, g_value_peek_pointer (value));
		break;

	case PROP_FAMILY:
	case PROP_STYLE:
	case PROP_VARIANT:
	case PROP_WEIGHT:
	case PROP_STRETCH:
	case PROP_SIZE:
	case PROP_SIZE_POINTS:
		ensure_font (text);

		switch (param_id) {
		case PROP_FAMILY:
			pango_font_description_set_family (text->font_desc,
							   g_value_get_string (value));
			break;
		case PROP_STYLE:
			pango_font_description_set_style (text->font_desc,
							  g_value_get_enum (value));
			break;
		case PROP_VARIANT:
			pango_font_description_set_variant (text->font_desc,
							    g_value_get_enum (value));
			break;
		case PROP_WEIGHT:
			pango_font_description_set_weight (text->font_desc,
							   g_value_get_int (value));
			break;
		case PROP_STRETCH:
			pango_font_description_set_stretch (text->font_desc,
							    g_value_get_enum (value));
			break;
		case PROP_SIZE:
			/* FIXME: This is bogus! It should be pixels, not points/PANGO_SCALE! */
			pango_font_description_set_size (text->font_desc,
							 g_value_get_int (value));
			break;
		case PROP_SIZE_POINTS:
			pango_font_description_set_size (text->font_desc,
							 g_value_get_double (value) * PANGO_SCALE);
			break;
		}

		foo_canvas_text_apply_font_desc (text);
		break;

	case PROP_FAMILY_SET:
	case PROP_STYLE_SET:
	case PROP_VARIANT_SET:
	case PROP_WEIGHT_SET:
	case PROP_STRETCH_SET:
	case PROP_SIZE_SET:
		if (!g_value_get_boolean (value) && text->font_desc)
			pango_font_description_unset_fields (text->font_desc,
							     get_property_font_set_mask (param_id));
		break;

	case PROP_SCALE:
		text->scale = g_value_get_double (value);
		text->scale_set = TRUE;

		foo_canvas_text_apply_font_desc (text);
		break;

	case PROP_SCALE_SET:
		text->scale_set = g_value_get_boolean (value);

		foo_canvas_text_apply_font_desc (text);
		break;

	case PROP_UNDERLINE:
		text->underline = g_value_get_enum (value);
		text->underline_set = TRUE;

		foo_canvas_text_apply_attributes (text);
		break;

	case PROP_UNDERLINE_SET:
		text->underline_set = g_value_get_boolean (value);

		foo_canvas_text_apply_attributes (text);
		break;

	case PROP_STRIKETHROUGH:
		text->strikethrough = g_value_get_boolean (value);
		text->strike_set = TRUE;

		foo_canvas_text_apply_attributes (text);
		break;

	case PROP_STRIKETHROUGH_SET:
		text->strike_set = g_value_get_boolean (value);

		foo_canvas_text_apply_attributes (text);
		break;

	case PROP_RISE:
		text->rise = g_value_get_int (value);
		text->rise_set = TRUE;

		foo_canvas_text_apply_attributes (text);
		break;

	case PROP_RISE_SET:
		text->rise_set = TRUE;

		foo_canvas_text_apply_attributes (text);
		break;

	case PROP_ATTRIBUTES:
		if (text->attr_list)
			pango_attr_list_unref (text->attr_list);

		text->attr_list = g_value_peek_pointer (value);
		if (text->attr_list)
			pango_attr_list_ref (text->attr_list);

		foo_canvas_text_apply_attributes (text);
		break;

	case PROP_ANCHOR:
		text->anchor = g_value_get_enum (value);
		break;

	case PROP_JUSTIFICATION:
		text->justification = g_value_get_enum (value);

		switch (text->justification) {
		case GTK_JUSTIFY_LEFT:
		        align = PANGO_ALIGN_LEFT;
			break;
		case GTK_JUSTIFY_CENTER:
		        align = PANGO_ALIGN_CENTER;
			break;
		case GTK_JUSTIFY_RIGHT:
		        align = PANGO_ALIGN_RIGHT;
			break;
		default:
		        /* GTK_JUSTIFY_FILL isn't supported yet. */
		        align = PANGO_ALIGN_LEFT;
			break;
		}
		pango_layout_set_alignment (text->layout, align);
		break;

	case PROP_CLIP_WIDTH:
		text->clip_width = fabs (g_value_get_double (value));
		break;

	case PROP_CLIP_HEIGHT:
		text->clip_height = fabs (g_value_get_double (value));
		break;

	case PROP_CLIP:
		text->clip = g_value_get_boolean (value);
		break;

	case PROP_WRAP_WIDTH: {
		double w = fabs (g_value_get_double (value));
		pango_layout_set_width (text->layout,
			w * text->item.canvas->pixels_per_unit * PANGO_SCALE);

		break;
	}

	case PROP_X_OFFSET:
		text->xofs = g_value_get_double (value);
		break;

	case PROP_Y_OFFSET:
		text->yofs = g_value_get_double (value);
		break;

        case PROP_FILL_COLOR: {
		const char *color_name;

		color_name = g_value_get_string (value);
		if (color_name) {
			gdk_color_parse (color_name, &color);

			text->rgba = ((color.red & 0xff00) << 16 |
				      (color.green & 0xff00) << 8 |
				      (color.blue & 0xff00) |
				      0xff);
			color_changed = TRUE;
		}
		break;
	}

	case PROP_FILL_COLOR_GDK:
		pcolor = g_value_get_boxed (value);
		if (pcolor) {
		    GdkColormap *colormap;

		    color = *pcolor;
		    colormap = gtk_widget_get_colormap (GTK_WIDGET (item->canvas));
		    gdk_rgb_find_color (colormap, &color);
		    have_pixel = TRUE;
		}

		text->rgba = ((color.red & 0xff00) << 16 |
			      (color.green & 0xff00) << 8|
			      (color.blue & 0xff00) |
			      0xff);
		color_changed = TRUE;
		break;

        case PROP_FILL_COLOR_RGBA:
		text->rgba = g_value_get_uint (value);
		color_changed = TRUE;
		break;

	case PROP_FILL_STIPPLE:
		set_stipple (text, (GdkBitmap *)g_value_get_object (value), FALSE);
		break;

	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
		break;
	}

	if (color_changed) {
		if (have_pixel)
			text->pixel = color.pixel;
		else
			text->pixel = foo_canvas_get_color_pixel (item->canvas, text->rgba);

		set_text_gc_foreground (text);
	}

	/* Calculate text dimensions */

	if (text->layout)
	        pango_layout_get_pixel_size (text->layout,
					     &text->max_width,
					     &text->height);
	else {
		text->max_width = 0;
		text->height = 0;
	}

	foo_canvas_item_request_update (item);
}

/* Get_arg handler for the text item */
static void
foo_canvas_text_get_property (GObject            *object,
				guint               param_id,
				GValue             *value,
				GParamSpec         *pspec)
{
	FooCanvasText *text;

	g_return_if_fail (object != NULL);
	g_return_if_fail (FOO_IS_CANVAS_TEXT (object));

	text = FOO_CANVAS_TEXT (object);

	switch (param_id) {
	case PROP_TEXT:
		g_value_set_string (value, text->text);
		break;

	case PROP_X:
		g_value_set_double (value, text->x);
		break;

	case PROP_Y:
		g_value_set_double (value, text->y);
		break;

	case PROP_FONT:
	case PROP_FONT_DESC:
	case PROP_FAMILY:
	case PROP_STYLE:
	case PROP_VARIANT:
	case PROP_WEIGHT:
	case PROP_STRETCH:
	case PROP_SIZE:
	case PROP_SIZE_POINTS:
		ensure_font (text);

		switch (param_id) {
		case PROP_FONT:
		{
			/* FIXME GValue imposes a totally gratuitous string copy
			 * here, we could just hand off string ownership
			 */
			gchar *str;

			str = pango_font_description_to_string (text->font_desc);
			g_value_set_string (value, str);
			g_free (str);

			break;
		}

		case PROP_FONT_DESC:
			g_value_set_boxed (value, text->font_desc);
			break;

		case PROP_FAMILY:
			g_value_set_string (value, pango_font_description_get_family (text->font_desc));
			break;

		case PROP_STYLE:
			g_value_set_enum (value, pango_font_description_get_style (text->font_desc));
			break;

		case PROP_VARIANT:
			g_value_set_enum (value, pango_font_description_get_variant (text->font_desc));
			break;

		case PROP_WEIGHT:
			g_value_set_int (value, pango_font_description_get_weight (text->font_desc));
			break;

		case PROP_STRETCH:
			g_value_set_enum (value, pango_font_description_get_stretch (text->font_desc));
			break;

		case PROP_SIZE:
			g_value_set_int (value, pango_font_description_get_size (text->font_desc));
			break;

		case PROP_SIZE_POINTS:
			g_value_set_double (value, ((double)pango_font_description_get_size (text->font_desc)) / (double)PANGO_SCALE);
			break;
		}
		break;

	case PROP_FAMILY_SET:
	case PROP_STYLE_SET:
	case PROP_VARIANT_SET:
	case PROP_WEIGHT_SET:
	case PROP_STRETCH_SET:
	case PROP_SIZE_SET:
	{
		PangoFontMask set_mask = text->font_desc ? pango_font_description_get_set_fields (text->font_desc) : 0;
		PangoFontMask test_mask = get_property_font_set_mask (param_id);
		g_value_set_boolean (value, (set_mask & test_mask) != 0);

		break;
	}

	case PROP_SCALE:
		g_value_set_double (value, text->scale);
		break;
	case PROP_SCALE_SET:
		g_value_set_boolean (value, text->scale_set);
		break;

	case PROP_UNDERLINE:
		g_value_set_enum (value, text->underline);
		break;
	case PROP_UNDERLINE_SET:
		g_value_set_boolean (value, text->underline_set);
		break;

	case PROP_STRIKETHROUGH:
		g_value_set_boolean (value, text->strikethrough);
		break;
	case PROP_STRIKETHROUGH_SET:
		g_value_set_boolean (value, text->strike_set);
		break;

	case PROP_RISE:
		g_value_set_int (value, text->rise);
		break;
	case PROP_RISE_SET:
		g_value_set_boolean (value, text->rise_set);
		break;

	case PROP_ATTRIBUTES:
		g_value_set_boxed (value, text->attr_list);
		break;

	case PROP_ANCHOR:
		g_value_set_enum (value, text->anchor);
		break;

	case PROP_JUSTIFICATION:
		g_value_set_enum (value, text->justification);
		break;

	case PROP_CLIP_WIDTH:
		g_value_set_double (value, text->clip_width);
		break;

	case PROP_CLIP_HEIGHT:
		g_value_set_double (value, text->clip_height);
		break;

	case PROP_CLIP:
		g_value_set_boolean (value, text->clip);
		break;

	case PROP_WRAP_WIDTH:
		g_value_set_double (value,
			pango_layout_get_width (text->layout) / PANGO_SCALE);
		break;

	case PROP_X_OFFSET:
		g_value_set_double (value, text->xofs);
		break;

	case PROP_Y_OFFSET:
		g_value_set_double (value, text->yofs);
		break;

	case PROP_FILL_COLOR:
                g_value_take_string (value,
				     g_strdup_printf ("#%02x%02x%02x",
						      text->rgba >> 24,
						      (text->rgba >> 16) & 0xff,
						      (text->rgba >> 8) & 0xff));
		break;

	case PROP_FILL_COLOR_GDK: {
		FooCanvas *canvas = FOO_CANVAS_ITEM (text)->canvas;
		GdkColormap *colormap = gtk_widget_get_colormap (GTK_WIDGET (canvas));
		GdkColor color;

		gdk_colormap_query_color (colormap, text->pixel, &color);
		g_value_set_boxed (value, &color);
		break;
	}
	case PROP_FILL_COLOR_RGBA:
		g_value_set_uint (value, text->rgba);
		break;

	case PROP_FILL_STIPPLE:
		g_value_set_object (value, text->stipple);
		break;

	case PROP_TEXT_WIDTH:
		g_value_set_double (value, text->max_width / text->item.canvas->pixels_per_unit);
		break;

	case PROP_TEXT_HEIGHT:
		g_value_set_double (value, text->height / text->item.canvas->pixels_per_unit);
		break;

	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
		break;
	}
}

/* */
static void
foo_canvas_text_apply_font_desc (FooCanvasText *text)
{
	PangoFontDescription *font_desc =
		pango_font_description_copy (
			GTK_WIDGET (FOO_CANVAS_ITEM (text)->canvas)->style->font_desc);

	if (text->font_desc)
		pango_font_description_merge (font_desc, text->font_desc, TRUE);

	pango_layout_set_font_description (text->layout, font_desc);
	pango_font_description_free (font_desc);
}

static void
add_attr (PangoAttrList  *attr_list,
	  PangoAttribute *attr)
{
	attr->start_index = 0;
	attr->end_index = G_MAXINT;

	pango_attr_list_insert (attr_list, attr);
}

/* */
static void
foo_canvas_text_apply_attributes (FooCanvasText *text)
{
	PangoAttrList *attr_list;
	double zoom;

	if (text->attr_list)
		attr_list = pango_attr_list_copy (text->attr_list);
	else
		attr_list = pango_attr_list_new ();

	if (text->underline_set)
		add_attr (attr_list, pango_attr_underline_new (text->underline));
	if (text->strike_set)
		add_attr (attr_list, pango_attr_strikethrough_new (text->strikethrough));
	if (text->rise_set)
		add_attr (attr_list, pango_attr_rise_new (text->rise));

	zoom = text->item.canvas->pixels_per_unit;
	if (fabs (zoom - 1.) > 1e-4) {
		PangoAttribute *attr = pango_attr_scale_new (zoom);
		attr->start_index = 0;
		attr->end_index = -1;
		pango_attr_list_insert_before (attr_list, attr);
	}

	pango_layout_set_attributes (text->layout, attr_list);
	pango_attr_list_unref (attr_list);
}

static void
foo_canvas_text_set_font_desc (FooCanvasText      *text,
				 PangoFontDescription *font_desc)
{
	if (text->font_desc)
		pango_font_description_free (text->font_desc);

	if (font_desc)
		text->font_desc = pango_font_description_copy (font_desc);
	else
		text->font_desc = NULL;

	foo_canvas_text_apply_font_desc (text);
}

/* Setting the text from a Pango markup string */
static void
foo_canvas_text_set_markup (FooCanvasText *textitem,
			      const gchar     *markup)
{
	PangoAttrList *attr_list = NULL;
	gchar         *text = NULL;
	GError        *error = NULL;

	if (textitem->text)
		g_free (textitem->text);
	if (textitem->attr_list)
		pango_attr_list_unref (textitem->attr_list);

	if (markup && !pango_parse_markup (markup, -1,
					   0,
					   &attr_list, &text, NULL,
					   &error))
	{
		g_warning ("Failed to set cell text from markup due to error parsing markup: %s",
			   error->message);
		g_error_free (error);
		return;
	}

	textitem->text = text;
	textitem->attr_list = attr_list;

	pango_layout_set_text (textitem->layout, text, -1);

	foo_canvas_text_apply_attributes (textitem);
}

/* Update handler for the text item */
static void
foo_canvas_text_update (FooCanvasItem *item, double i2w_dx, double i2w_dy, int flags)
{
	FooCanvasText *text;
	double x1, y1, x2, y2;

	text = FOO_CANVAS_TEXT (item);

	if (parent_class->update)
		(* parent_class->update) (item, i2w_dx, i2w_dy, flags);

	set_text_gc_foreground (text);
	set_stipple (text, text->stipple, TRUE);
	get_bounds (text, &x1, &y1, &x2, &y2);

	foo_canvas_update_bbox (item,
				  floor (x1), floor (y1),
				  ceil (x2), ceil (y2));
}

/* Realize handler for the text item */
static void
foo_canvas_text_realize (FooCanvasItem *item)
{
	FooCanvasText *text;

	text = FOO_CANVAS_TEXT (item);

	if (parent_class->realize)
		(* parent_class->realize) (item);

	text->gc = gdk_gc_new (item->canvas->layout.bin_window);
}

/* Unrealize handler for the text item */
static void
foo_canvas_text_unrealize (FooCanvasItem *item)
{
	FooCanvasText *text;

	text = FOO_CANVAS_TEXT (item);

	g_object_unref (text->gc);
	text->gc = NULL;

	if (parent_class->unrealize)
		(* parent_class->unrealize) (item);
}

/* Draw handler for the text item */
static void
foo_canvas_text_draw (FooCanvasItem *item, GdkDrawable *drawable,
			GdkEventExpose   *expose)
{
	FooCanvasText *text;
	GdkRectangle rect;

	text = FOO_CANVAS_TEXT (item);

	if (!text->text)
		return;

	if (text->clip) {
		rect.x = text->clip_cx;
		rect.y = text->clip_cy;
		rect.width = text->clip_cwidth;
		rect.height = text->clip_cheight;

		gdk_gc_set_clip_rectangle (text->gc, &rect);
	}

	if (text->stipple)
		foo_canvas_set_stipple_origin (item->canvas, text->gc);


	gdk_draw_layout (drawable, text->gc, text->cx, text->cy, text->layout);

	if (text->clip)
		gdk_gc_set_clip_rectangle (text->gc, NULL);
}

/* Point handler for the text item */
static double
foo_canvas_text_point (FooCanvasItem *item, double x, double y,
			 int cx, int cy, FooCanvasItem **actual_item)
{
	FooCanvasText *text;
	PangoLayoutIter *iter;
	int x1, y1, x2, y2;
	int dx, dy;
	double dist, best;

	text = FOO_CANVAS_TEXT (item);

	*actual_item = item;

	/* The idea is to build bounding rectangles for each of the lines of
	 * text (clipped by the clipping rectangle, if it is activated) and see
	 * whether the point is inside any of these.  If it is, we are done.
	 * Otherwise, calculate the distance to the nearest rectangle.
	 */

	best = 1.0e36;

	iter = pango_layout_get_iter (text->layout);
	do {
 	        PangoRectangle log_rect;

		pango_layout_iter_get_line_extents (iter, NULL, &log_rect);

		if (text->clip) {
			x1 = PANGO_PIXELS (log_rect.x);
			y1 = PANGO_PIXELS (log_rect.y);
			x2 = PANGO_PIXELS (log_rect.x+log_rect.width);
			y2 = PANGO_PIXELS (log_rect.y+log_rect.height);


			if (x1 < text->clip_cx)
				x1 = text->clip_cx;

			if (y1 < text->clip_cy)
				y1 = text->clip_cy;

			if (x2 > (text->clip_cx + text->clip_width))
				x2 = text->clip_cx + text->clip_width;

			if (y2 > (text->clip_cy + text->clip_height))
				y2 = text->clip_cy + text->clip_height;

			if ((x1 >= x2) || (y1 >= y2))
				continue;
		} else {
			x1 = text->x;
			y1 = text->y;
			x2 = log_rect.width;
			y2 = log_rect.height;
		}

		/* Calculate distance from point to rectangle */

		if (cx < x1)
			dx = x1 - cx;
		else if (cx >= x2)
			dx = cx - x2 + 1;
		else
			dx = 0;

		if (cy < y1)
			dy = y1 - cy;
		else if (cy >= y2)
			dy = cy - y2 + 1;
		else
			dy = 0;

		if ((dx == 0) && (dy == 0)) {
			pango_layout_iter_free(iter);
			return 0.0;
		}

		dist = sqrt (dx * dx + dy * dy);
		if (dist < best)
			best = dist;

	} while (pango_layout_iter_next_line(iter));

	pango_layout_iter_free(iter);

	return best / item->canvas->pixels_per_unit;
}

static void
foo_canvas_text_translate (FooCanvasItem *item, double dx, double dy)
{
	FooCanvasText *text;

	text = FOO_CANVAS_TEXT (item);

	text->x += dx;
	text->y += dy;
}

/* Bounds handler for the text item */
static void
foo_canvas_text_bounds (FooCanvasItem *item, double *x1, double *y1, double *x2, double *y2)
{
	FooCanvasText *text;
	double width, height;

	text = FOO_CANVAS_TEXT (item);

	*x1 = text->x;
	*y1 = text->y;

	if (text->clip) {
		width = text->clip_width;
		height = text->clip_height;
	} else {
		width = text->max_width / item->canvas->pixels_per_unit;
		height = text->height / item->canvas->pixels_per_unit;
	}

	switch (text->anchor) {
	case GTK_ANCHOR_NW:
	case GTK_ANCHOR_W:
	case GTK_ANCHOR_SW:
		break;

	case GTK_ANCHOR_N:
	case GTK_ANCHOR_CENTER:
	case GTK_ANCHOR_S:
		*x1 -= width / 2.0;
		break;

	case GTK_ANCHOR_NE:
	case GTK_ANCHOR_E:
	case GTK_ANCHOR_SE:
		*x1 -= width;
		break;

	default:
		break;
	}

	switch (text->anchor) {
	case GTK_ANCHOR_NW:
	case GTK_ANCHOR_N:
	case GTK_ANCHOR_NE:
		break;

	case GTK_ANCHOR_W:
	case GTK_ANCHOR_CENTER:
	case GTK_ANCHOR_E:
		*y1 -= height / 2.0;
		break;

	case GTK_ANCHOR_SW:
	case GTK_ANCHOR_S:
	case GTK_ANCHOR_SE:
		*y1 -= height;
		break;

	default:
		break;
	}

	*x2 = *x1 + width;
	*y2 = *y1 + height;
}
