/********************************************************************\
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
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

#include <gnome.h>

#include "gnucash-color.h"

static int color_inited;
static GdkColorContext *gnucash_color_context;

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
        int failed;

        if (!color_inited)
                gnucash_color_init ();

        return gdk_color_context_get_pixel (gnucash_color_context,
                                            red, green, blue, &failed);
}


void
gnucash_color_alloc_gdk (GdkColor *c)
{
        int failed;

        g_return_if_fail (c != NULL);

        c->pixel = gdk_color_context_get_pixel (gnucash_color_context, c->red,
						c->green, c->blue, &failed);
}


void
gnucash_color_alloc_name (const char *name, GdkColor *c)
{
        int failed;

        g_return_if_fail (name != NULL);
        g_return_if_fail (c != NULL);

        gdk_color_parse (name, c);
        c->pixel = 0;
        c->pixel = gdk_color_context_get_pixel (gnucash_color_context, c->red,
						c->green, c->blue, &failed);
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

        color->pixel = gnucash_color_alloc(color->red,
                                           color->green,
                                           color->blue);

        g_hash_table_insert (color_hash_table, newkey, color);

        return color;
}


void
gnucash_color_init (void)
{
        GdkColormap *colormap = gtk_widget_get_default_colormap ();

        /* Initialize the color context */
        gnucash_color_context =
                gdk_color_context_new (gtk_widget_get_default_visual (),
                                       colormap);

        /* Allocate the default colors */
        gdk_color_white (colormap, &gn_white);
        gdk_color_black (colormap, &gn_black);

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
