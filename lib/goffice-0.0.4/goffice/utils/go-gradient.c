/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-gradient.c :
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
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301
 * USA
 */

#include <goffice/goffice-config.h>
#include "go-gradient.h"
#include "go-color.h"

#ifdef WITH_GTK
#include <goffice/gtk/go-combo-pixmaps.h>
#include <gdk-pixbuf/gdk-pixdata.h>
#endif

#include <glib/gi18n.h>
#include <string.h>


char const *grad_dir_names[] = {
	"n-s",
	"s-n",
	"n-s-mirrored",
	"s-n-mirrored",
	"w-e",
	"e-w",
	"w-e-mirrored",
	"e-w-mirrored",
	"nw-se",
	"se-nw",
	"nw-se-mirrored",
	"se-nw-mirrored",
	"ne-sw",
	"sw-ne",
	"sw-ne-mirrored",
	"ne-sw-mirrored"
};

GOGradientDirection
go_gradient_dir_from_str (char const *name)
{
	unsigned i;
	for (i = 0; i < GO_GRADIENT_MAX; i++)
		if (strcmp (grad_dir_names[i], name) == 0)
			return i;
	return GO_GRADIENT_N_TO_S;
}

char const *
go_gradient_dir_as_str (GOGradientDirection dir)
{
	return (dir < 0 || dir >= GO_GRADIENT_MAX) ? "gradient"
		: grad_dir_names[dir];
}

#ifdef WITH_GTK
GtkWidget *
go_gradient_selector (GOColor start, GOColor end)
{
	int const W = 20, H = 20;
	unsigned	 i;
	GOComboPixmaps	*w;
	GdkPixbuf	*pixbuf;
	ArtRender	*render;
	ArtGradientLinear gradient;
	ArtGradientStop	  stops[2];

	w = go_combo_pixmaps_new (4);
	for (i = 0; i < GO_GRADIENT_MAX; i++) {
		GOGradientDirection dir = (GOGradientDirection)i;
		pixbuf = gdk_pixbuf_new (GDK_COLORSPACE_RGB, TRUE, 8, W, H);
		gdk_pixbuf_fill (pixbuf, 0); /* in case the fill colours have alpha = 0 */
		render = art_render_new (0, 0, W, H,
			gdk_pixbuf_get_pixels (pixbuf),
			gdk_pixbuf_get_rowstride (pixbuf),
			gdk_pixbuf_get_n_channels (pixbuf) - 1,
			8, ART_ALPHA_SEPARATE, NULL);
		go_gradient_setup (&gradient, dir, start, end, 0, 0,
			W, H, stops);
		art_render_gradient_linear (render,
			&gradient, ART_FILTER_NEAREST);
		art_render_invoke (render);
		go_combo_pixmaps_add_element (w, pixbuf, dir, NULL);
	}

	return GTK_WIDGET (w);
}
#endif /* WITH_GTK */

void
go_gradient_setup (ArtGradientLinear *gradient,
		   GOGradientDirection dir, GOColor col0, GOColor col1,
		   double x0, double y0, double x1, double y1,
		   ArtGradientStop *stops)
{
	double dx = x1 - x0;
	double dy = y1 - y0;

	if (dir < 4) {
		gradient->a = 0.;
		gradient->b = 1. / (dy ? dy : 1);
		gradient->c = - 1.e-10 - (gradient->a * x0 + gradient->b * y0);
	} else if (dir < 8) {
		gradient->a = 1. / (dx ? dx : 1);
		gradient->b = 0.;
		gradient->c = -(gradient->a * x0 + gradient->b * y0);
	} else if (dir < 12) {
		gradient->a = .5 / (dx ? dx : 1);
		gradient->b = .5 / (dy ? dy : 1);
		gradient->c = -(gradient->a * x0 + gradient->b * y0);
	} else {
		gradient->a = -.5 / (dx ? dx : 1);
		gradient->b = .5 / (dy ? dy : 1);
		/* Note: this gradient is anchored at (x1,y0).  */
		gradient->c = -(gradient->a * x1 + gradient->b * y0);
	}

	gradient->stops = stops;
	gradient->n_stops = 2;
	stops[0].offset = 0;
	stops[1].offset = 1;

	switch (dir % 4) {
	case 0:
		gradient->spread = ART_GRADIENT_PAD;
		go_color_to_artpix (stops[0].color, col0);
		go_color_to_artpix (stops[1].color, col1);
		break;
	case 1:
		gradient->spread = ART_GRADIENT_PAD;
		go_color_to_artpix (stops[0].color, col1);
		go_color_to_artpix (stops[1].color, col0);
		break;
	case 2:
		gradient->spread = ART_GRADIENT_REFLECT;
		go_color_to_artpix (stops[0].color, col0);
		go_color_to_artpix (stops[1].color, col1);
		gradient->a *= 2;
		gradient->b *= 2;
		gradient->c *= 2;
		break;
	case 3:
		gradient->spread = ART_GRADIENT_REFLECT;
		go_color_to_artpix (stops[0].color, col1);
		go_color_to_artpix (stops[1].color, col0);
		gradient->a *= 2;
		gradient->b *= 2;
		gradient->c *= 2;
		break;
	}
}
