/* File import from foocanvas to libgoffice by import-foocanvas.  Do not edit.  */

/* GNOME libraries - GdkPixbuf item for the GNOME canvas
 *
 * Copyright (C) 1999 The Free Software Foundation
 *
 * Author: Federico Mena-Quintero <federico@gimp.org>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301
 * USA.
 */

#ifndef FOO_CANVAS_PIXBUF_H
#define FOO_CANVAS_PIXBUF_H


#include <goffice/cut-n-paste/foocanvas/foo-canvas.h>

G_BEGIN_DECLS



#define FOO_TYPE_CANVAS_PIXBUF            (foo_canvas_pixbuf_get_type ())
#define FOO_CANVAS_PIXBUF(obj)            (GTK_CHECK_CAST ((obj), FOO_TYPE_CANVAS_PIXBUF, FooCanvasPixbuf))
#define FOO_CANVAS_PIXBUF_CLASS(klass)    (GTK_CHECK_CLASS_CAST ((klass), FOO_TYPE_CANVAS_PIXBUF, FooCanvasPixbufClass))
#define FOO_IS_CANVAS_PIXBUF(obj)         (GTK_CHECK_TYPE ((obj), FOO_TYPE_CANVAS_PIXBUF))
#define FOO_IS_CANVAS_PIXBUF_CLASS(klass) (GTK_CHECK_CLASS_TYPE ((klass), FOO_TYPE_CANVAS_PIXBUF))
#define FOO_CANVAS_PIXBUF_GET_CLASS(obj)  (GTK_CHECK_GET_CLASS ((obj), FOO_TYPE_CANVAS_PIXBUF, FooCanvasPixbufClass))


typedef struct _FooCanvasPixbuf FooCanvasPixbuf;
typedef struct _FooCanvasPixbufClass FooCanvasPixbufClass;

struct _FooCanvasPixbuf {
	FooCanvasItem item;

	/* Private data */
	gpointer priv;
};

struct _FooCanvasPixbufClass {
	FooCanvasItemClass parent_class;
};


GtkType foo_canvas_pixbuf_get_type (void) G_GNUC_CONST;



G_END_DECLS

#endif
