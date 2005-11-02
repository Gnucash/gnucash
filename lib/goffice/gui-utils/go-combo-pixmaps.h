/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * widget-pixmap-combo.h - A pixmap selector combo box
 * Copyright 2000-2003, Ximian, Inc.
 *
 * Authors:
 *   Jody Goldberg <jody@gnome.org>
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
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#ifndef GO_COMBO_PIXMAPS_H
#define GO_COMBO_PIXMAPS_H

#include <gtk/gtktooltips.h>

G_BEGIN_DECLS

#define GO_COMBO_PIXMAPS_TYPE	(go_combo_pixmaps_get_type ())
#define GO_COMBO_PIXMAPS(o)	(G_TYPE_CHECK_INSTANCE_CAST((o), GO_COMBO_PIXMAPS_TYPE, GOComboPixmaps))
#define IS_GO_COMBO_PIXMAPS(o)	(G_TYPE_CHECK_INSTANCE_TYPE((o), GO_COMBO_PIXMAPS_TYPE))

typedef struct _GOComboPixmaps	GOComboPixmaps;
typedef struct _GOMenuPixmaps	GOMenuPixmaps;

GType      go_combo_pixmaps_get_type	 (void);
GOComboPixmaps *go_combo_pixmaps_new	 (int ncols);
void       go_combo_pixmaps_add_element  (GOComboPixmaps *combo,
					  GdkPixbuf const *pixbuf, int id,
					  char const *tooltip);
gboolean   go_combo_pixmaps_select_index (GOComboPixmaps *combo, int index);
gboolean   go_combo_pixmaps_select_id    (GOComboPixmaps *combo, int id);
int        go_combo_pixmaps_get_selected (GOComboPixmaps const *combo, int *index);
GtkWidget *go_combo_pixmaps_get_preview	 (GOComboPixmaps const *combo);

GOMenuPixmaps *go_menu_pixmaps_new	 (int ncols);
void       go_menu_pixmaps_add_element   (GOMenuPixmaps *menu,
					  GdkPixbuf const *pixbuf, int id);

G_END_DECLS

#endif /* GO_COMBO_PIXMAPS_H */
