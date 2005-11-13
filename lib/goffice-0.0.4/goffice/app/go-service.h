/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * go-service.h : A GOffice plugin
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
#ifndef GO_SERVICE_H
#define GO_SERVICE_H

#include <goffice/app/goffice-app.h>
#include <glib-object.h>

G_BEGIN_DECLS

#define GO_SERVICE_TYPE         (go_service_get_type ())
#define GO_SERVICE(o)           (G_TYPE_CHECK_INSTANCE_CAST ((o), GO_SERVICE_TYPE, GOService))
#define IS_GO_SERVICE(o)        (G_TYPE_CHECK_INSTANCE_TYPE ((o), GO_SERVICE_TYPE))

GType     go_service_get_type   (void);
GOPlugin *go_service_get_plugin (GOService const *service);

#define GO_SERVICE_SIMPLE_TYPE	(go_service_simple_get_type ())
#define GO_SERVICE_SIMPLE(o)	(G_TYPE_CHECK_INSTANCE_CAST ((o), GO_SERVICE_SIMPLE_TYPE, GOServiceSimple))
#define IS_GO_SERVICE_SIMPLE(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), GO_SERVICE_SIMPLE_TYPE))
typedef struct _GOServiceSimple GOServiceSimple;
GType go_service_simple_get_type (void);

G_END_DECLS

#endif /* GO_SERVICE_H */
