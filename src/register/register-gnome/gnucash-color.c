/********************************************************************\
 * gnucash-color.c -- color handling for table cells                *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

/*
 *  Shamelessly stolen from Gnumeric and modified
 *
 *   Heath Martin <martinh@pegasus.cc.ucf.edu>
 *
 * color.c: Color allocation on the Gnumeric spreadsheet
 *
 * Author:
 *  Miguel de Icaza (miguel@kernel.org)
 *
 * We keep our own color context, as the color allocation might take place
 * before any of our Canvases are realized.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <gnome.h>

#include "gnucash-color.h"

static int color_inited;

/* Public Colors */
GdkColor gn_white, gn_black, gn_light_gray;
GdkColor gn_dark_gray, gn_blue, gn_red, gn_yellow;

static GHashTable *color_hash_table = NULL;

static guint
color_hash (gconstpointer v)
{
    const guint32 *c = (guint32 *) v;

    return *c;
}


static gint
color_equal (gconstpointer v, gconstpointer w)
{
    const guint32 *c1 = (guint32 *) v;
    const guint32 *c2 = (guint32 *) w;

    return (*c1 == *c2);
}


gulong
gnucash_color_alloc (gushort red, gushort green, gushort blue)
{
    GdkColormap *colormap = gtk_widget_get_default_colormap ();
    GdkColor *c;

    if (!color_inited)
        gnucash_color_init ();

    c = g_new0 (GdkColor, 1);
    c->red = red;
    c->green = green;
    c->blue = blue;

    g_return_val_if_fail (gdk_colormap_alloc_color (colormap, c, FALSE, TRUE), 0);

    return c->pixel;
}


void
gnucash_color_alloc_gdk (GdkColor *c)
{
    GdkColormap *colormap = gtk_widget_get_default_colormap ();

    g_return_if_fail (c != NULL);

    g_assert (gdk_colormap_alloc_color (colormap, c,
                                        FALSE, TRUE));
}


void
gnucash_color_alloc_name (const char *name, GdkColor *c)
{
    GdkColormap *colormap = gtk_widget_get_default_colormap ();

    g_return_if_fail (name != NULL);
    g_return_if_fail (c != NULL);

    gdk_color_parse (name, c);
    c->pixel = 0;
    g_assert (gdk_colormap_alloc_color (colormap, c,
                                        FALSE, TRUE));
}


/* This function takes an argb spec for a color and returns an
 *  allocated GdkColor.  We take care of allocating and managing
 *  the colors.  Caller must not touch the returned color.
 */
GdkColor *
gnucash_color_argb_to_gdk (guint32 argb)
{
    GdkColor *color;
    const guint32 key = argb;
    guint32 *newkey;

    color = g_hash_table_lookup (color_hash_table, &key);

    if (color)
        return color;

    color = g_new0(GdkColor, 1);
    newkey = g_new0(guint32, 1);

    *newkey = key;

    color->red = (argb & 0xff0000) >> 8;
    color->green = argb & 0xff00;
    color->blue = (argb & 0xff) << 8;

    gnucash_color_alloc_gdk(color);

    g_hash_table_insert (color_hash_table, newkey, color);

    return color;
}


void
gnucash_color_init (void)
{
    /* Allocate the default colors */
    gnucash_color_alloc_name ("white",  &gn_white);
    gnucash_color_alloc_name ("black",  &gn_black);

    gnucash_color_alloc_name ("gray60", &gn_light_gray);
    gnucash_color_alloc_name ("gray40", &gn_dark_gray);
    gnucash_color_alloc_name ("blue",   &gn_blue);
    gnucash_color_alloc_name ("red",    &gn_red);
    gnucash_color_alloc_name ("yellow", &gn_yellow);

    if (!color_hash_table)
        color_hash_table = g_hash_table_new (color_hash, color_equal);

    color_inited = 1;
}


/*
  Local Variables:
  c-basic-offset: 8
  End:
*/
