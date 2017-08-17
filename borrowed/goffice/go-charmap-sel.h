/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 *  Copyright (C) 2003 Andreas J. Guelzow
 *
 *  based on code by:
 *  Copyright (C) 2000 Marco Pesenti Gritti
 *  from the galeon code base
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2, or (at your option)
 *  any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301, USA.
 */

#ifndef _GO_CHARMAP_SEL_H_
#define _GO_CHARMAP_SEL_H_

#include <glib-object.h>
#include <gtk/gtk.h>

G_BEGIN_DECLS

#define GO_TYPE_CHARMAP_SEL        (go_charmap_sel_get_type ())
#define GO_CHARMAP_SEL(obj)        (G_TYPE_CHECK_INSTANCE_CAST((obj), GO_TYPE_CHARMAP_SEL, GOCharmapSel))
#define GO_IS_CHARMAP_SEL(obj)     (G_TYPE_CHECK_INSTANCE_TYPE((obj), GO_TYPE_CHARMAP_SEL))

typedef struct _GOCharmapSel GOCharmapSel;

typedef enum
{
    GO_CHARMAP_SEL_TO_UTF8 = 0,
    GO_CHARMAP_SEL_FROM_UTF8
} GOCharmapSelTestDirection;

GType go_charmap_sel_get_type(void);
GtkWidget * go_charmap_sel_new(GOCharmapSelTestDirection test);

gchar const *go_charmap_sel_get_encoding(GOCharmapSel *cs);
gboolean go_charmap_sel_set_encoding(GOCharmapSel *cs, const char *enc);

const char *go_charmap_sel_get_encoding_name(GOCharmapSel *cs, const char *enc);

G_END_DECLS

#endif /* _GO_CHARMAP_SEL_H_ */
