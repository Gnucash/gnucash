/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-marker.h :
 *
 * Copyright (C) 2003-2004 Emmanuel Pacaud (emmanuel.pacaud@univ-poitiers.fr)
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

#ifndef GO_MARKER_H
#define GO_MARKER_H

#include <glib-object.h>
#include <goffice/utils/goffice-utils.h>
#include <libart_lgpl/art_vpath.h>

#ifdef WITH_GTK
#include <gdk-pixbuf/gdk-pixbuf.h>
#endif

G_BEGIN_DECLS

#define GO_MARKER_TYPE	  	(go_marker_get_type ())
#define GO_MARKER(o)		(G_TYPE_CHECK_INSTANCE_CAST((o), GO_MARKER_TYPE, GOMarker))
#define IS_GO_MARKER(o)		(G_TYPE_CHECK_INSTANCE_TYPE((o), GO_MARKER_TYPE))

typedef enum {
	GO_MARKER_NONE,
	GO_MARKER_SQUARE,
	GO_MARKER_DIAMOND,
	GO_MARKER_TRIANGLE_DOWN,
	GO_MARKER_TRIANGLE_UP,
	GO_MARKER_TRIANGLE_RIGHT,
	GO_MARKER_TRIANGLE_LEFT,
	GO_MARKER_CIRCLE,
	GO_MARKER_X,
	GO_MARKER_CROSS,
	GO_MARKER_ASTERISK,
	GO_MARKER_BAR,
	GO_MARKER_HALF_BAR,
	GO_MARKER_BUTTERFLY,
	GO_MARKER_HOURGLASS,
	GO_MARKER_MAX
} GOMarkerShape;

struct _GOMarker {
	GObject 	base;

	int		size;
	double		scale;
	GOMarkerShape	shape;
	GOColor		outline_color;
	GOColor		fill_color;
#ifdef WITH_GTK
	GdkPixbuf	*pixbuf;
#else
	gpointer	 pixbuf;
#endif
};

GType go_marker_get_type (void); 


GOMarkerShape    go_marker_shape_from_str       (char const *name);
char const      *go_marker_shape_as_str         (GOMarkerShape shape);
void		 go_marker_get_paths		(GOMarker * marker,
						 ArtVpath const **outline_path,
						 ArtVpath const **fill_path);
GOMarkerShape 	 go_marker_get_shape		(GOMarker *m);
void 		 go_marker_set_shape 		(GOMarker *m, GOMarkerShape shape);
GOColor 	 go_marker_get_outline_color	(GOMarker *m);
void		 go_marker_set_outline_color	(GOMarker *m, GOColor color);
GOColor		 go_marker_get_fill_color	(GOMarker *m);
void		 go_marker_set_fill_color	(GOMarker *m, GOColor color);
int		 go_marker_get_size		(GOMarker *m);
void		 go_marker_set_size		(GOMarker *m, int size);
double		 go_marker_get_outline_width	(GOMarker *m);

void		 go_marker_assign 		(GOMarker *dst, GOMarker const *src);
GOMarker *	 go_marker_dup 			(GOMarker *src);
GOMarker * 	 go_marker_new 			(void);

#ifdef WITH_GTK
GdkPixbuf const *go_marker_get_pixbuf		(GOMarker *m, double scale);
GdkPixbuf const	*go_marker_get_pixbuf_with_size (GOMarker *m, guint size);

gpointer 	 go_marker_selector 		(GOColor outline_color, 
						 GOColor fill_color,
						 GOMarkerShape default_shape);
GOMarkerShape	 go_marker_selector_get_shape	(gpointer selector,
						 int index, gboolean *is_auto);
#endif
	
G_END_DECLS

#endif /* GO_MARKER_H */
