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
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
\********************************************************************/

#ifndef GNUCASH_COLOR_H
#define GNUCASH_COLOR_H

#include "basiccell.h"  /* for uint32 */

void     gnucash_color_init      (void);

/* Return the pixel value for the given red, green and blue */
gulong   gnucash_color_alloc      (gushort red, gushort green, gushort blue);
void     gnucash_color_alloc_name (const char *name, GdkColor *color);
void     gnucash_color_alloc_gdk  (GdkColor *color);
GdkColor *gnucash_color_argb_to_gdk (uint32 argb);


extern GdkColor gn_white, gn_light_gray, gn_dark_gray, gn_black, gn_red;

#endif /* GNUCASH_COLOR_H */
