/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * go-service-impl.h :  Implementation details for the abstract GOService
 *			 interface
 *
 * Copyright (C) 2001-2004 Zbigniew Chyla (cyba@gnome.pl)
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

#ifndef GO_SERVICE_IMPL_H
#define GO_SERVICE_IMPL_H

#include <goffice/app/go-service.h>
#include <glib-object.h>
#include <libxml/tree.h>

G_BEGIN_DECLS

struct _GOService {
	GObject base;
	GOPlugin *plugin;
};
typedef struct {
	GObjectClass base;
	GOErrorStack *(*read_details) (GOService *service, xmlNode *tree);
	GOErrorStack *(*activate)     (GOService *service);
	GOErrorStack *(*deactivate)   (GOService *service);
	char 	     *(*description)  (GOService *service);
} GOServiceClass;

struct _GOServiceSimple {
	GOService	base;
};
typedef GOServiceClass GOServiceSimpleClass;

G_END_DECLS

#endif /* GO_GRAPH_ITEM_IMPL_H */

