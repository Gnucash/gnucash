/* File import from foocanvas to libgoffice by import-foocanvas.  Do not edit.  */

/*
 * Copyright (C) 1997, 1998, 1999, 2000 Free Software Foundation
 * All rights reserved.
 *
 * This file is part of the Gnome Library.
 *
 * The Gnome Library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * The Gnome Library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with the Gnome Library; see the file COPYING.LIB.  If
 * not, write to the Free Software Foundation, Inc., 51 Franklin St,
 * Fifth Floor, Boston, MA  02110-1301 USA.
 */
/*
  @NOTATION@
 */

#ifndef LIBFOOCANVAS_H
#define LIBFOOCANVAS_H

#include <goffice/cut-n-paste/foocanvas/foo-canvas.h>
#include <goffice/cut-n-paste/foocanvas/foo-canvas-line.h>
#include <goffice/cut-n-paste/foocanvas/foo-canvas-text.h>
#include <goffice/cut-n-paste/foocanvas/foo-canvas-polygon.h>
#include <goffice/cut-n-paste/foocanvas/foo-canvas-pixbuf.h>
#include <goffice/cut-n-paste/foocanvas/foo-canvas-widget.h>
#include <goffice/cut-n-paste/foocanvas/foo-canvas-rect-ellipse.h>
#include <goffice/cut-n-paste/foocanvas/foo-canvas-util.h>

G_BEGIN_DECLS

GType foo_canvas_points_get_type (void);
#define FOO_TYPE_CANVAS_POINTS foo_canvas_points_get_type()

G_END_DECLS

#endif /* LIBFOOCANVAS_H */
