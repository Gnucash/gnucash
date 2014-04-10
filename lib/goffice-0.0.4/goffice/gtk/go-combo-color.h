/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * go-combo-color.h - A color selector combo box
 * Copyright 2000, 2001, Ximian, Inc.
 *
 * Authors:
 *   Miguel de Icaza (miguel@kernel.org)
 *   Dom Lachowicz (dominicl@seas.upenn.edu)
 *
 * Reworked and split up into a separate ColorPalette object:
 *   Michael Levy (mlevy@genoscope.cns.fr)
 *
 * And later revised and polished by:
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
#ifndef _GO_COMBO_COLOR_H_
#define _GO_COMBO_COLOR_H_

#include <glib-object.h>
#include <goffice/gtk/go-color-group.h>
#include <goffice/utils/go-color.h>
#include <gtk/gtkwidget.h>

G_BEGIN_DECLS

#define GO_COMBO_COLOR_TYPE	(go_combo_color_get_type ())
#define GO_COMBO_COLOR(o)	(G_TYPE_CHECK_INSTANCE_CAST((o), GO_COMBO_COLOR_TYPE, GOComboColor))
#define IS_GO_COMBO_COLOR(o)	(G_TYPE_CHECK_INSTANCE_TYPE((o), GO_COMBO_COLOR_TYPE))
#define GO_COMBO_COLOR_CLASS(k)	(G_TYPE_CHECK_CLASS_CAST(k), GO_COMBO_COLOR_TYPE)

typedef struct _GOComboColor GOComboColor;

GType      go_combo_color_get_type   (void);
GtkWidget *go_combo_color_new        (GdkPixbuf   *icon,
				      char const *no_color_label,
				      GOColor default_color,
				      GOColorGroup  *color_group);
GOColor go_combo_color_get_color (GOComboColor  *cc, gboolean *is_default);
void    go_combo_color_set_color (GOComboColor  *cc, GOColor   color);
void    go_combo_color_set_color_to_default (GOComboColor *cc);
void    go_combo_color_set_color_gdk (GOComboColor *cc, GdkColor *color);
GOColor go_combo_color_get_default   (GOComboColor *cc);
void	go_combo_color_set_default   (GOComboColor *cc, GOColor color);

void go_combo_color_set_allow_alpha    (GOComboColor *cc, gboolean allow_alpha);
void go_combo_color_set_instant_apply  (GOComboColor *cc, gboolean active);

G_END_DECLS

#endif /* _GO_COMBO_COLOR_H_ */
