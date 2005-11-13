/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-style.h : 
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
#ifndef GO_GRAPH_STYLE_H
#define GO_GRAPH_STYLE_H

#include <goffice/graph/gog-object.h>
#include <goffice/graph/gog-object-xml.h>
#include <goffice/graph/goffice-graph.h>
#include <goffice/utils/goffice-utils.h>
#include <goffice/app/goffice-app.h>
#include <goffice/utils/go-gradient.h>
#include <goffice/utils/go-line.h>
#include <goffice/utils/go-pattern.h>
#include <glib-object.h>

G_BEGIN_DECLS

#define GOG_STYLE_TYPE	(gog_style_get_type ())
#define GOG_STYLE(o)	(G_TYPE_CHECK_INSTANCE_CAST ((o), GOG_STYLE_TYPE, GogStyle))
#define IS_GOG_STYLE(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOG_STYLE_TYPE))

GType gog_style_get_type (void);

typedef enum {
	GOG_STYLE_OUTLINE	= 1 << 0,
	GOG_STYLE_FILL		= 1 << 1,
	GOG_STYLE_LINE		= 1 << 2,
	GOG_STYLE_MARKER	= 1 << 3,
	GOG_STYLE_FONT		= 1 << 4,
	GOG_STYLE_TEXT_LAYOUT	= 1 << 5,
	GOG_STYLE_ALL		= 0x1F
} GogStyleFlag;

typedef enum {
	GOG_FILL_STYLE_NONE	= 0,
	GOG_FILL_STYLE_PATTERN	= 1,
	GOG_FILL_STYLE_GRADIENT	= 2,
	GOG_FILL_STYLE_IMAGE	= 3
} GogFillStyle;

typedef enum {
	GOG_IMAGE_STRETCHED,
	GOG_IMAGE_WALLPAPER,
	GOG_IMAGE_CENTERED
} GogImageType;

typedef struct {
	/* <0 == no outline,
	 * =0 == hairline : unscaled, minimum useful (can be bigger than visible) size.
	 * >0 in pts */
	float	 	 width;
	GOLineDashType 	 dash_type;
	gboolean	 auto_dash;
	GOColor	 	 color;
	gboolean 	 auto_color;
	unsigned 	 pattern; /* TODO border type from gnumeric */
} GogStyleLine;

typedef struct {
	GOMarker *mark;
	gboolean auto_shape;
	gboolean auto_outline_color;
	gboolean auto_fill_color;
} GogStyleMark;

struct _GogStyle {
	GObject	base;

	GogStyleFlag	interesting_fields;
	GogStyleFlag	disable_theming;

	GogStyleLine	outline, line;
	struct {
		GogFillStyle	type;
		gboolean	auto_fore, auto_back;	/* share between pattern and gradient */
		gboolean	invert_if_negative;	/* placeholder for XL */

		/* This could all be a union but why bother ? */
		GOPattern pattern;
		struct {
			GOGradientDirection dir;
			float   brightness; /* < 0 => 2 color */
		} gradient;
		struct {
			GogImageType type;
			GdkPixbuf *image;
			char      *filename;
		} image;
	} fill;
	GogStyleMark marker;
	struct {
		GOColor		 color;
		GOFont const 	*font;
		gboolean 	 auto_scale;
	} font;
	struct {
		double		 angle;
		gboolean	 auto_angle; 
	} text_layout;	
};

GogStyle  *gog_style_new		(void);
GogStyle  *gog_style_dup		(GogStyle const *style);
void	   gog_style_assign		(GogStyle *dst, GogStyle const *src);
void	   gog_style_apply_theme	(GogStyle *dst, GogStyle const *src);

void	   gog_style_set_marker			(GogStyle *style, GOMarker *marker);
void	   gog_style_set_font_desc		(GogStyle *style,
						 PangoFontDescription *desc);
void	   gog_style_set_font			(GogStyle *style, GOFont const *font);
void	   gog_style_set_fill_brightness	(GogStyle *style, float brightness);
void	   gog_style_set_fill_image_filename	(GogStyle *style, char *filename);
void	   gog_style_set_text_angle     	(GogStyle *style, double angle);

gboolean   gog_style_is_different_size	(GogStyle const *a, GogStyle const *b);
gboolean   gog_style_is_marker_visible	(GogStyle const *style);
gboolean   gog_style_is_line_visible	(GogStyle const *style);
gboolean   gog_style_is_outline_visible	(GogStyle const *style);
void	   gog_style_force_auto		(GogStyle *style);

void 	   gog_style_populate_editor 	(GogStyle *style,
					 GogEditor *editor,
					 GogStyle *default_style,
					 GOCmdContext *cc,
					 GObject *object_with_style,
					 gboolean watch_for_external_change);
gpointer   gog_style_get_editor	     	(GogStyle *style,
					 GogStyle *default_style,
					 GOCmdContext *cc,
					 GObject *object_with_style);

G_END_DECLS

#endif /* GO_GRAPH_STYLE_H */
