/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * color.c: Color allocation on the Gnumeric spreadsheet
 *
 * Author:
 *  Miguel de Icaza (miguel@kernel.org)
 *
 */
#include <config.h>
#include "gnumeric.h"
#include "style-color.h"
#include "style-border.h"
#include <gtk/gtk.h>
#include <stdio.h>

/* Public _unallocated_ colours, i.e., no valid .pixel.  */
GdkColor gs_black      = { 0, 0x0000, 0x0000, 0x0000 };    /* "Black" */
GdkColor gs_white      = { 0, 0xffff, 0xffff, 0xffff };    /* "White" */
GdkColor gs_yellow     = { 0, 0xffff, 0xffff, 0xe0e0 };    /* "LightYellow" */
GdkColor gs_lavender   = { 0, 0xe6e6, 0xe6e6, 0xfafa };    /* "lavender" */
GdkColor gs_dark_gray  = { 0, 0x3333, 0x3333, 0x3333 };    /* "gray20" */
GdkColor gs_light_gray = { 0, 0xc7c7, 0xc7c7, 0xc7c7 };    /* "gray78" */

static GHashTable *style_color_hash;
static GnmColor *sc_black;
static GnmColor *sc_white;
static GnmColor *sc_grid;

GnmColor *
style_color_new_name (char const *name)
{
	GdkColor c;

	gdk_color_parse (name, &c);
	return style_color_new (c.red, c.green, c.blue);
}

static GnmColor *
style_color_new_uninterned (gushort red, gushort green, gushort blue,
			    gboolean is_auto)
{
	GnmColor *sc = g_new (GnmColor, 1);

	sc->color.red = red;
	sc->color.green = green;
	sc->color.blue = blue;
	sc->color.pixel = gs_white.pixel;
	sc->name = NULL;
	sc->is_auto = is_auto;

	/* Make a contrasting selection color with an alpha of .5 */
	red   += (gs_lavender.red   - red)/2;
	green += (gs_lavender.green - green)/2;
	blue  += (gs_lavender.blue  - blue)/2;
	sc->selected_color.red = red;
	sc->selected_color.green = green;
	sc->selected_color.blue = blue;
	sc->selected_color.pixel = gs_white.pixel;

	sc->ref_count = 1;

	return sc;
}

GnmColor *
style_color_new (gushort red, gushort green, gushort blue)
{
	GnmColor *sc;
	GnmColor key;

	key.color.red   = red;
	key.color.green = green;
	key.color.blue  = blue;
	key.is_auto = FALSE;

	sc = g_hash_table_lookup (style_color_hash, &key);
	if (!sc) {
		sc = style_color_new_uninterned (red, green, blue, FALSE);
		g_hash_table_insert (style_color_hash, sc, sc);
	} else
		sc->ref_count++;

	return sc;
}

GnmColor *
style_color_new_pango (PangoColor *c)
{
	return style_color_new (c->red, c->green, c->blue);
}

/* scale 8 bit/color ->  16 bit/color by cloning */
GnmColor *
style_color_new_i8 (guint8 red, guint8 green, guint8 blue)
{
	gushort red16, green16, blue16;

	red16 =   ((gushort) red) << 8   | red;
	green16 = ((gushort) green) << 8 | green;
	blue16 =  ((gushort) blue) << 8  | blue;

	return style_color_new (red16, green16, blue16);
}
GnmColor *
style_color_new_go (GOColor c)
{
	return style_color_new_i8 (
		UINT_RGBA_R (c), UINT_RGBA_G (c), UINT_RGBA_B (c));
}

GnmColor *
style_color_black (void)
{
	if (!sc_black)
		sc_black = style_color_new (0, 0, 0);
	return style_color_ref (sc_black);
}

GnmColor *
style_color_white (void)
{
	if (!sc_white)
		sc_white = style_color_new (0xffff, 0xffff, 0xffff);
	return style_color_ref (sc_white);
}

GnmColor *
style_color_grid (void)
{
	if (!sc_grid)
		sc_grid = style_color_new (0xc7c7, 0xc7c7, 0xc7c7);
	return style_color_ref (sc_grid);
}

/**
 * Support for Excel auto-colors.
 */

/**
 * Always black, as far as we know.
 */
GnmColor *
style_color_auto_font (void)
{
	static GnmColor *color = NULL;

	if (!color)
		color = style_color_new_uninterned (0, 0, 0, TRUE);
	return style_color_ref (color);
}

/**
 * Always white, as far as we know.
 */
GnmColor *
style_color_auto_back (void)
{
	static GnmColor *color = NULL;

	if (!color)
		color = style_color_new_uninterned (0xffff, 0xffff, 0xffff,
						    TRUE);
	return style_color_ref (color);
}

/**
 * Normally black, but follows grid color if so told.
 */
GnmColor *
style_color_auto_pattern (void)
{
	static GnmColor *color = NULL;

	if (!color)
		color = style_color_new_uninterned (0, 0, 0, TRUE);
	return style_color_ref (color);
}

GnmColor *
style_color_ref (GnmColor *sc)
{
	if (sc != NULL)
		sc->ref_count++;

	return sc;
}

void
style_color_unref (GnmColor *sc)
{
	if (sc == NULL)
		return;

	g_return_if_fail (sc->ref_count > 0);

	sc->ref_count--;
	if (sc->ref_count != 0)
		return;

	/*
	 * There is no need to deallocate colors, as they come from
	 * the GDK Color Context
	 */
	g_hash_table_remove (style_color_hash, sc);
	g_free (sc);
}

gint
style_color_equal (const GnmColor *k1, const GnmColor *k2)
{
	if (k1->color.red   == k2->color.red &&
	    k1->color.green == k2->color.green &&
	    k1->color.blue  == k2->color.blue &&
	    k1->is_auto == k2->is_auto)
		return 1;

	return 0;
}

static guint
color_hash (gconstpointer v)
{
	const GnmColor *k = (const GnmColor *)v;

	return (k->color.red << 16) ^ (k->color.green << 8) ^ (k->color.blue << 0) ^
		(k->is_auto);
}

void
gnumeric_color_init (void)
{
	GdkColor error;

	gdk_color_parse ("cyan", &error);
	if (gdk_screen_get_default () != NULL) {
		/*
		 * Make sure we can see bogus attempt at getting the pixel
		 * value.  This is, by nature, not multi-head safe.
		 */
		gdk_rgb_find_color (
			gdk_screen_get_default_colormap (
				    gdk_screen_get_default ()),
			&error);
	} else
		error.pixel = 0;

	gs_black.pixel = error.pixel;
	gs_white.pixel = error.pixel;
	gs_yellow.pixel =  error.pixel;
	gs_lavender.pixel =  error.pixel;
	gs_dark_gray.pixel =  error.pixel;
	gs_light_gray.pixel =  error.pixel;

	style_color_hash = g_hash_table_new (color_hash,
					     (GEqualFunc) style_color_equal);
}

static void
cb_color_leak (gpointer key, gpointer value, gpointer user_data)
{
	GnmColor *color = value;

	fprintf (stderr, "Leaking style-color at %p [%04x:%04x:%04x].\n",
		 color, color->color.red, color->color.green, color->color.blue);
}

void
gnumeric_color_shutdown (void)
{
	/*
	 * FIXME: this doesn't really belong here, but style-border.c isn't
	 * able to clean itself up yet.
	 */	   
	GnmBorder *none = style_border_none ();
	style_color_unref (none->color);
	none->color = NULL;

	if (sc_black) {
		style_color_unref (sc_black);
		sc_black = NULL;
	}

	if (sc_white) {
		style_color_unref (sc_white);
		sc_white = NULL;
	}

	if (sc_grid) {
		style_color_unref (sc_grid);
		sc_grid = NULL;
	}

	g_hash_table_foreach (style_color_hash, cb_color_leak, NULL);
	g_hash_table_destroy (style_color_hash);
	style_color_hash = NULL;
}
