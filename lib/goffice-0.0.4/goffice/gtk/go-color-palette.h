/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * go-color-palette.h - A color selector palette
 * Copyright 2000-2004, Ximian, Inc.
 *
 * Authors:
 * This code was extracted from widget-color-combo.c
 *   written by Miguel de Icaza (miguel@kernel.org) and
 *   Dom Lachowicz (dominicl@seas.upenn.edu). The extracted
 *   code was re-packaged into a separate object by
 *   Michael Levy (mlevy@genoscope.cns.fr)
 *   And later revised and polished by
 *   Almer S. Tigelaar (almer@gnome.org)
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License, version 2, as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301
 * USA.
 */

#ifndef _GO_COLOR_PALETTE_H_
#define _GO_COLOR_PALETTE_H_

#include <goffice/gtk/go-color-group.h>
#include <gtk/gtkwidget.h>

G_BEGIN_DECLS

typedef struct _GOColorPalette  GOColorPalette;

#define GO_COLOR_PALETTE_TYPE     (go_color_palette_get_type ())
#define GO_COLOR_PALETTE(obj)     (G_TYPE_CHECK_INSTANCE_CAST((obj), GO_COLOR_PALETTE_TYPE, GOColorPalette))
#define GO_COLOR_PALETTE_CLASS(k) (G_TYPE_CHECK_CLASS_CAST(k), GO_COLOR_PALETTE_TYPE)
#define IS_GO_COLOR_PALETTE(obj)  (G_TYPE_CHECK_INSTANCE_TYPE((obj), GO_COLOR_PALETTE_TYPE))

GType      go_color_palette_get_type (void);

GtkWidget *go_color_palette_new	      (char const *no_color_label,
				       GOColor default_color,
				       GOColorGroup *color_group);
GtkWidget *go_color_palette_make_menu (char const *no_color_label,
				       GOColor default_color,
				       GOColorGroup *color_group,
				       char const *custom_dialog_title,
				       GOColor current_color);

void	go_color_palette_set_title		(GOColorPalette *p, char const *title);
void	go_color_palette_set_group		(GOColorPalette *p, GOColorGroup *cg);
void    go_color_palette_set_current_color	(GOColorPalette *p, GOColor color);
void    go_color_palette_set_color_to_default	(GOColorPalette *p);
GOColor go_color_palette_get_current_color	(GOColorPalette *p,
						 gboolean *is_default, gboolean *is_custom);
void	go_color_palette_set_allow_alpha	(GOColorPalette *p, gboolean allow_alpha);

G_END_DECLS

#endif /* __GO_COLOR_PALETTE_H__ */
