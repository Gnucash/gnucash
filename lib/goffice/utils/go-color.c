/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-color.c :
 *
 * Copyright (C) 2003-2004 Jody Goldberg (jody@gnome.org)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of version 2 of the GNU General Public
 * License as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
 * USA
 */

#include <goffice/goffice-config.h>
#include "go-color.h"

#include <stdio.h>

void
go_color_to_artpix (ArtPixMaxDepth *res, GOColor rgba)
{
	guint8 r = UINT_RGBA_R (rgba);
	guint8 g = UINT_RGBA_G (rgba);
	guint8 b = UINT_RGBA_B (rgba);
	guint8 a = UINT_RGBA_A (rgba);
	res[0] = ART_PIX_MAX_FROM_8 (r);
	res[1] = ART_PIX_MAX_FROM_8 (g);
	res[2] = ART_PIX_MAX_FROM_8 (b);
	res[3] = ART_PIX_MAX_FROM_8 (a);
}

/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 *  A slightly modified version of art_rgb_svp to render into rgba buffer
 *
 *  This program is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Library General Public License
 *  as published by the Free Software Foundation; either version 2 of
 *  the License, or (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Library General Public License for more details.
 *
 *  You should have received a copy of the GNU Library General Public
 *  License along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *  Authors:
 *    Raph Levien <raph@acm.org>
 *    Lauris Kaplinski <lauris@ariman.ee>
 *
 *  Copyright (C) 1998 Raph Levien
 *
 */

/* Render a sorted vector path into an RGBA buffer. */
#include <libart_lgpl/art_misc.h>
#include <libart_lgpl/art_svp_render_aa.h>
#include <libart_lgpl/art_rgb.h>

typedef struct {
	int const *alphatab;
	art_u8 r, g, b;
	art_u8 *buf;
	int rowstride;
	int x0, x1;
} solid_data;

static void
fill_solid (art_u8 * buf, art_u8 r, art_u8 g, art_u8 b, int n)
{
	while (n-- > 0) {
		* buf++ = r;
		* buf++ = g;
		* buf++ = b;
		* buf++ = 255;
	}
}

static void
fill_blend (art_u8 * buf, art_u8 r, art_u8 g, art_u8 b, int alpha, int n)
{
	int br, bg, bb, ba;
	int cr, cg, cb;

	while (n-- > 0) {
		br = * (buf + 0);
		bg = * (buf + 1);
		bb = * (buf + 2);
		ba = * (buf + 3);

		cr = (br * ba + 0x80) >> 8;
		cg = (bg * ba + 0x80) >> 8;
		cb = (bb * ba + 0x80) >> 8;

		* buf++ = cr + (((r - cr) * alpha + 0x80) >> 8);
		* buf++ = cg + (((g - cg) * alpha + 0x80) >> 8);
		* buf++ = cb + (((b - cb) * alpha + 0x80) >> 8);
		* buf++ = ba + (((255 - ba) * alpha + 0x80) >> 8);
	}
}

static void
cb_fill_alpha (void *callback_data, int y, int start,
	       ArtSVPRenderAAStep *steps, int n_steps)
{
	solid_data *data = callback_data;
	art_u8 *linebuf;
	int run_x0, run_x1;
	art_u32 running_sum = start;
	int x0, x1;
	int k;
	art_u8 r, g, b;
	int const *alphatab;
	int alpha;

	linebuf = data->buf;
	x0 = data->x0;
	x1 = data->x1;

	r = data->r;
	g = data->g;
	b = data->b;
	alphatab = data->alphatab;

	if (n_steps > 0) {
		run_x1 = steps[0].x;
		if (run_x1 > x0) {
			alpha = (running_sum >> 16) & 0xff;
			if (alpha)
				fill_blend (linebuf, r, g, b, alphatab[alpha],
					    run_x1 - x0);
		}

		/* render the steps into tmpbuf */
		for (k = 0; k < n_steps - 1; k++) {
			running_sum += steps[k].delta;
			run_x0 = run_x1;
			run_x1 = steps[k + 1].x;
			if (run_x1 > run_x0) {
				alpha = (running_sum >> 16) & 0xff;
				if (alpha)
					fill_blend (linebuf + (run_x0 - x0) * 4, r, g, b, alphatab[alpha],
						    run_x1 - run_x0);
			}
		}
		running_sum += steps[k].delta;
		if (x1 > run_x1) {
			alpha = (running_sum >> 16) & 0xff;
			if (alpha)
				fill_blend (linebuf + (run_x1 - x0) * 4, r, g, b, alphatab[alpha],
					    x1 - run_x1);
		}
	} else {
		alpha = (running_sum >> 16) & 0xff;
		if (alpha)
			fill_blend (linebuf, r, g, b, alphatab[alpha],
				    x1 - x0);
	}

	data->buf += data->rowstride;
}

static void
cb_fill_opaque (void *callback_data, int y, int start,
		ArtSVPRenderAAStep *steps, int n_steps)
{
	solid_data *data = callback_data;
	art_u8 *linebuf;
	int run_x0, run_x1;
	art_u32 running_sum = start;
	int x0, x1;
	int k;
	art_u8 r, g, b;
	int const *alphatab;
	int alpha;

	linebuf = data->buf;
	x0 = data->x0;
	x1 = data->x1;

	r = data->r;
	g = data->g;
	b = data->b;
	alphatab = data->alphatab;

	if (n_steps > 0) {
		run_x1 = steps[0].x;
		if (run_x1 > x0) {
			alpha = running_sum >> 16;
			if (alpha) {
				if (alpha >= 255)
					fill_solid (linebuf, r, g, b,
						    run_x1 - x0);
				else
					fill_blend (linebuf, r, g, b, alphatab[alpha],
						    run_x1 - x0);
			}
		}

		/* render the steps into tmpbuf */
		for (k = 0; k < n_steps - 1; k++) {
			running_sum += steps[k].delta;
			run_x0 = run_x1;
			run_x1 = steps[k + 1].x;
			if (run_x1 > run_x0) {
				alpha = running_sum >> 16;
				if (alpha) {
					if (alpha >= 255)
						fill_solid (linebuf + (run_x0 - x0) * 4, r, g, b,
							    run_x1 - run_x0);
					else
						fill_blend (linebuf + (run_x0 - x0) * 4, r, g, b, alphatab[alpha],
							    run_x1 - run_x0);
				}
			}
		}
		running_sum += steps[k].delta;
		if (x1 > run_x1) {
			alpha = running_sum >> 16;
			if (alpha) {
				if (alpha >= 255)
					fill_solid (linebuf + (run_x1 - x0) * 4, r, g, b,
						    x1 - run_x1);
				else
					fill_blend (linebuf + (run_x1 - x0) * 4, r, g, b, alphatab[alpha],
						    x1 - run_x1);
			}
		}
	} else {
		alpha = running_sum >> 16;
		if (alpha) {
			if (alpha >= 255)
				fill_solid (linebuf, r, g, b, x1 - x0);
			else
				fill_blend (linebuf, r, g, b, alphatab[alpha], x1 - x0);
		}
	}

	data->buf += data->rowstride;
}

/**
 * go_color_render_svp: Alpha-composite sorted vector path over RGBA buffer.
 * @color : Color in 0xRRGGBBAA format.
 * @svp   : The source sorted vector path.
 * @x0    : Left coordinate of destination rectangle.
 * @y0    : Top coordinate of destination rectangle.
 * @x1    : Right coordinate of destination rectangle.
 * @y1    : Bottom coordinate of destination rectangle.
 * @buf   : Destination RGB buffer.
 * @rowstride: Rowstride of @buf buffer.
 *
 * Renders the shape specified with @svp over the @buf RGB buffer.
 * @x1 - @x0 specifies the width, and @y1 - @y0 specifies the height,
 * of the rectangle rendered. The new pixels are stored starting at
 * the first byte of @buf. Thus, the @x0 and @y0 parameters specify
 * an offset within @svp, and may be tweaked as a way of doing
 * integer-pixel translations without fiddling with @svp itself.
 *
 * The @color argument specifies the color for the rendering. Pixels of
 * entirely 0 winding number are left untouched. Pixels of entirely
 * 1 winding number have the color @color composited over them (ie,
 * are replaced by the red, green, blue components of @color if the alpha
 * component is 0xff). Pixels of intermediate coverage are linearly
 * interpolated.
 **/
void
go_color_render_svp (GOColor color, ArtSVP const *svp,
		     int x0, int y0, int x1, int y1,
		     art_u8 *buf, int rowstride)
{
	solid_data data;
	int alpha, i, a, da;
	int alphatab[256];

	data.rowstride = rowstride;
	data.buf = buf;
	data.x0 = x0;
	data.x1 = x1;
	data.r = UINT_RGBA_R (color);
	data.g = UINT_RGBA_G (color);
	data.b = UINT_RGBA_B (color);
	alpha  = UINT_RGBA_A (color);

	a = 0x8000;
	da = (alpha * 66051 + 0x80) >> 8; /* 66051 equals 2 ^ 32 / (255 * 255) */

	if (alpha != 0xff) {
		for (i = 0; i < 256; i++) {
			alphatab[i] = a >> 16;
			a += da;
		}
		data.alphatab = alphatab;
		art_svp_render_aa (svp, x0, y0, x1, y1, &cb_fill_alpha, &data);
	} else {
		/* Hard code the most common table */
		static int const opaque[] = {
			0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,
			18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33,
			34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49,
			50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65,
			66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81,
			82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97,
			98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110,
			111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122,
			123, 124, 125, 126, 127, 129, 130, 131, 132, 133, 134, 135,
			136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147,
			148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159,
			160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171,
			172, 173, 174, 175, 176, 177, 178, 179, 180, 181, 182, 183,
			184, 185, 186, 187, 188, 189, 190, 191, 192, 193, 194, 195,
			196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207,
			208, 209, 210, 211, 212, 213, 214, 215, 216, 217, 218, 219,
			220, 221, 222, 223, 224, 225, 226, 227, 228, 229, 230, 231,
			232, 233, 234, 235, 236, 237, 238, 239, 240, 241, 242, 243,
			244, 245, 246, 247, 248, 249, 250, 251, 252, 253, 254, 255, 256
		};
		data.alphatab = opaque;
		art_svp_render_aa (svp, x0, y0, x1, y1, &cb_fill_opaque, &data);
	}
}

GOColor
go_color_from_str (gchar const *string)
{
	unsigned r, g, b, a;
	GOColor color = 0;

	if (sscanf ((char const *) string, "%X:%X:%X:%X", &r, &g, &b, &a) == 4)
		color = RGBA_TO_UINT (r, g, b, a);
	return color;
}

gchar *
go_color_as_str (GOColor color)
{
	unsigned r, g, b, a;

	UINT_TO_RGBA (color, &r, &g, &b, &a);
	return g_strdup_printf ("%X:%X:%X:%X", r, g, b, a);
}

PangoAttribute *
go_color_to_pango (GOColor color, gboolean is_fore)
{
	guint16 r, g, b;
	r  = UINT_RGBA_R (color);
	r |= (r << 8);
	g  = UINT_RGBA_G (color);
	g |= (g << 8);
	b  = UINT_RGBA_B (color);
	b |= (b << 8);

	if (is_fore)
		return pango_attr_foreground_new (r, g, b);
	else
		return pango_attr_background_new (r, g, b);
}

#ifdef WITH_GTK
#include <gdk/gdkcolor.h>

GdkColor *
go_color_to_gdk	(GOColor color, GdkColor *res)
{
	res->red    = UINT_RGBA_R (color);
	res->red   |= (res->red << 8);
	res->green  = UINT_RGBA_G (color);
	res->green |= (res->green << 8);
	res->blue   = UINT_RGBA_B (color);
	res->blue  |= (res->blue << 8);

	return res;
}
#endif /* WITH_GTK */
