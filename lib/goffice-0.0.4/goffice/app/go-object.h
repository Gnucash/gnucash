/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * go-object.h : A GOPlugin aware wrapper for g_object_new
 *
 * Copyright (C) 2004 Jody Goldberg (jody@gnome.org)
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
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301
 * USA
 */
#ifndef GO_OBJECT_H
#define GO_OBJECT_H

#include <glib-object.h>

G_BEGIN_DECLS

gpointer go_object_new        (char const *type, char const *first_prop, ...);
gpointer go_object_new_valist (char const *type, char const *first_prop,
			       va_list vargs);

/*****************************************************************************/

#define GO_SERVICE_OBJECT_TYPE  (go_service_object_get_type ())
#define GO_SERVICE_OBJECT(o)    (G_TYPE_CHECK_INSTANCE_CAST ((o), GO_SERVICE_OBJECT_TYPE, GOServiceObject))
#define IS_GO_SERVICE_OBJECT(o) (G_TYPE_CHECK_INSTANCE_TYPE ((o), GO_SERVICE_OBJECT_TYPE))
typedef struct _GOServiceObject	GOServiceObject;
GType go_service_object_get_type (void);

char const   *go_service_object_primary_type (GOServiceObject const *service);
GSList const *go_service_object_interfaces   (GOServiceObject const *service);

G_END_DECLS

#endif /* GO_OBJECT_H */
