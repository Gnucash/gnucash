/* File import from foocanvas to libgoffice by import-foocanvas.  Do not edit.  */

#include <goffice/goffice-config.h>
#include <glib/gi18n.h>
#include <gsf/gsf-impl-utils.h>
/*
 * Copyright (C) 1999, 2000 Red Hat, Inc.
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

#include <glib-object.h>

#include <goffice/cut-n-paste/foocanvas/libfoocanvas.h>

GType
foo_canvas_points_get_type (void)
{
    static GType type_canvas_points = 0;

    if (!type_canvas_points)
	type_canvas_points = g_boxed_type_register_static
	    ("FooCanvasPoints",
	     (GBoxedCopyFunc) foo_canvas_points_ref,
	     (GBoxedFreeFunc) foo_canvas_points_unref);

    return type_canvas_points;
}
