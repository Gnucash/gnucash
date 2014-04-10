/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * go-gui-utils.h - Misc GTK+ utilities
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
#ifndef GO_GUI_UTILS_H
#define GO_GUI_UTILS_H

G_BEGIN_DECLS

#include <gtk/gtkwidget.h>
#include <glade/glade-xml.h>
#include <goffice/app/goffice-app.h>

void	   go_editable_enters (GtkWindow *window, GtkWidget *w);

GtkWidget *go_gtk_button_new_with_stock_image (char const *text,
					       char const *stock_id);

GladeXML  *go_libglade_new (char const *gladefile, char const *root,
			    char const *domain, GOCmdContext *cc);

GdkPixbuf *go_pixbuf_intelligent_scale (GdkPixbuf *pixbuf, 
					guint width, guint height);

G_END_DECLS

#endif /* GO_GUI_UTILS_H */
