/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * go-font-sel.h - Misc GTK+ utilities
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
#ifndef _GO_FONT_SEL_H_
#define _GO_FONT_SEL_H_

#include <gtk/gtkwindow.h>
#include <goffice/utils/goffice-utils.h>

G_BEGIN_DECLS

#define GO_FONT_SEL_TYPE	(go_font_sel_get_type ())
#define GO_FONT_SEL(obj)	(G_TYPE_CHECK_INSTANCE_CAST((obj), GO_FONT_SEL_TYPE, GOFontSel))
#define IS_GO_FONT_SEL(obj)	(G_TYPE_CHECK_INSTANCE_TYPE((obj), GO_FONT_SEL_TYPE))

typedef struct _GOFontSel GOFontSel;

GType         go_font_sel_get_type (void);
GtkWidget    *go_font_sel_new      (void);
void	      go_font_sel_set_font (GOFontSel *fs, GOFont const *font);
GOFont const *go_font_sel_get_font (GOFontSel const *fs);
void go_font_sel_editable_enters   (GOFontSel *fs, GtkWindow *dialog);
void go_font_sel_set_sample_text   (GOFontSel *fs, char const *text);

G_END_DECLS

#endif /* _GO_FONT_SEL_H_ */
