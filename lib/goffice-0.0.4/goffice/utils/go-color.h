/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * go-color.h
 *
 * Copyright (C) 1999, 2000 EMC Capital Management, Inc.
 *
 * Developed by Jon Trowbridge <trow@gnu.org> and
 * Havoc Pennington <hp@pobox.com>.
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

#ifndef GO_COLOR_H
#define GO_COLOR_H

#include <glib.h>
#include <goffice/utils/goffice-utils.h>
#include <libart_lgpl/art_render.h>
#include <libart_lgpl/art_svp.h>
#include <pango/pango.h>

#ifdef WITH_GTK
#include <gdk/gdktypes.h>
#endif

G_BEGIN_DECLS

/*
  Some convenient macros for drawing into an RGB buffer.
  Beware of side effects, code-bloat, and all of the other classic
  cpp-perils...
*/

#define GDK_TO_UINT(c)	RGBA_TO_UINT(((c).red>>8), ((c).green>>8), ((c).blue>>8), 0xff)

#define RGB_TO_UINT(r,g,b)	((((guint)(r))<<16)|(((guint)(g))<<8)|((guint)(b)))
#define RGB_TO_RGBA(x,a)	(((x) << 8) | ((((guint)a) & 0xff)))
#define RGB_WHITE   RGB_TO_UINT(0xff, 0xff, 0xff)
#define RGB_BLACK   RGB_TO_UINT(0x00, 0x00, 0x00)
#define RGB_RED     RGB_TO_UINT(0xff, 0x00, 0x00)
#define RGB_GREEN   RGB_TO_UINT(0x00, 0xff, 0x00)
#define RGB_BLUE    RGB_TO_UINT(0x00, 0x00, 0xff)
#define RGB_YELLOW  RGB_TO_UINT(0xff, 0xff, 0x00)
#define RGB_VIOLET  RGB_TO_UINT(0xff, 0x00, 0xff)
#define RGB_CYAN    RGB_TO_UINT(0x00, 0xff, 0xff)
#define RGB_GREY(x) RGB_TO_UINT(x,x,x)

#define RGBA_TO_UINT(r,g,b,a)	((((guint)(r))<<24)|(((guint)(g))<<16)|(((guint)(b))<<8)|(guint)(a))
#define RGBA_WHITE  RGB_TO_RGBA(RGB_WHITE, 0xff)
#define RGBA_BLACK  RGB_TO_RGBA(RGB_BLACK, 0xff)
#define RGBA_RED    RGB_TO_RGBA(RGB_RED, 0xff)
#define RGBA_GREEN  RGB_TO_RGBA(RGB_GREEN, 0xff)
#define RGBA_BLUE   RGB_TO_RGBA(RGB_BLUE, 0xff)
#define RGBA_YELLOW RGB_TO_RGBA(RGB_YELLOW, 0xff)
#define RGBA_VIOLET RGB_TO_RGBA(RGB_VIOLET, 0xff)
#define RGBA_CYAN   RGB_TO_RGBA(RGB_CYAN, 0xff)
#define RGBA_GREY(x) RGB_TO_RGBA(RGB_GREY(x), 0xff)

#define UINT_RGBA_R(x) (((guint)(x))>>24)
#define UINT_RGBA_G(x) ((((guint)(x))>>16)&0xff)
#define UINT_RGBA_B(x) ((((guint)(x))>>8)&0xff)
#define UINT_RGBA_A(x) (((guint)(x))&0xff)
#define UINT_RGBA_CHANGE_R(x, r) (((x)&(~(0xff<<24)))|(((r)&0xff)<<24))
#define UINT_RGBA_CHANGE_G(x, g) (((x)&(~(0xff<<16)))|(((g)&0xff)<<16))
#define UINT_RGBA_CHANGE_B(x, b) (((x)&(~(0xff<<8)))|(((b)&0xff)<<8))
#define UINT_RGBA_CHANGE_A(x, a) (((x)&(~0xff))|((a)&0xff))
#define UINT_TO_RGB(u,r,g,b) \
{ (*(r)) = ((u)>>16)&0xff; (*(g)) = ((u)>>8)&0xff; (*(b)) = (u)&0xff; }
#define UINT_TO_RGBA(u,r,g,b,a) \
{ UINT_TO_RGB(((u)>>8),r,g,b); (*(a)) = (u)&0xff; }
#define MONO_INTERPOLATE(v1, v2, t) ((gint)rint((v2)*(t)+(v1)*(1-(t))))
#define UINT_INTERPOLATE(c1, c2, t) \
  RGBA_TO_UINT( MONO_INTERPOLATE(UINT_RGBA_R(c1), UINT_RGBA_R(c2), t), \
		MONO_INTERPOLATE(UINT_RGBA_G(c1), UINT_RGBA_G(c2), t), \
		MONO_INTERPOLATE(UINT_RGBA_B(c1), UINT_RGBA_B(c2), t), \
		MONO_INTERPOLATE(UINT_RGBA_A(c1), UINT_RGBA_A(c2), t) )
#define PIXEL_RGB(p, r, g, b) \
{((guchar*)(p))[0]=(r); ((guchar*)(p))[1]=(g); ((guchar*)(p))[2]=(b);}
#define PIXEL_RGBA(p, r, g, b, a) \
{ if ((a)>=0xff) { PIXEL_RGB(p,r,g,b) } \
  else if ((a)>0) { \
    guint pixel_tmp; \
    pixel_tmp = ((guchar*)(p))[0]; \
    ((guchar*)(p))[0] = pixel_tmp + ((((r)-pixel_tmp)*(a)+0x80) >> 8); \
    pixel_tmp = ((guchar*)(p))[1]; \
    ((guchar*)(p))[1] = pixel_tmp + ((((g)-pixel_tmp)*(a)+0x80) >> 8); \
    pixel_tmp = ((guchar*)(p))[2]; \
    ((guchar*)(p))[2] = pixel_tmp + ((((b)-pixel_tmp)*(a)+0x80) >> 8); }}
#define PIXEL_RGB_UINT(p, i) \
UINT_TO_RGB((i), ((guchar*)p), ((guchar*)p)+1, ((guchar*)p)+2)
#define PIXEL_RGBA_UINT(p, i) \
  PIXEL_RGBA((p), ((i)>>24)&0xff, ((i)>>16)&0xff, ((i)>>8)&0xff, (i)&0xff)
#define PIXEL_BLACK(p) PIXEL_RGB(p,0,0,0)
#define PIXEL_WHITE(p) PIXEL_RGB(p,0xff,0xff,0xff)
#define PIXEL_GREY(p,g) PIXEL_RGB(p,g,g,g)
#define PIXEL_GREYA(p,g,a) PIXEL_RGBA(p,g,g,g,a)

void go_color_to_artpix  (ArtPixMaxDepth *res, GOColor rgba);
void go_color_render_svp (GOColor color, ArtSVP const *svp,
			  int x0, int y0, int x1, int y1,
			  art_u8 *buf, int rowstride);

GOColor   go_color_from_str (char const *string);
gchar    *go_color_as_str   (GOColor color);
PangoAttribute *go_color_to_pango (GOColor color, gboolean is_fore);
#ifdef WITH_GTK
GdkColor *go_color_to_gdk   (GOColor color, GdkColor *res);
#endif

G_END_DECLS

#endif /* GO_COLOR_H */
