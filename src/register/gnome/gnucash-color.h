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

#ifndef GNUCASH_COLOR_H
#define GNUCASH_COLOR_H

#include <gnome.h>

void      gnucash_color_init        (void);

/* Return the pixel value for the given red, green and blue */
gulong    gnucash_color_alloc       (gushort red, gushort green, gushort blue);
void      gnucash_color_alloc_name  (const char *name, GdkColor *color);
void      gnucash_color_alloc_gdk   (GdkColor *color);
GdkColor *gnucash_color_argb_to_gdk (guint32 argb);

extern GdkColor gn_white, gn_light_gray, gn_dark_gray;
extern GdkColor gn_black, gn_blue, gn_red, gn_yellow;

#endif /* GNUCASH_COLOR_H */
