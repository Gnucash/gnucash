/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * go-graph-item-impl.h :  Implementation details for the abstract graph-item
 * 			interface
 *
 * Copyright (C) 2001 Zbigniew Chyla (cyba@gnome.pl)
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

#ifndef GO_PLUGIN_SERVICE_IMPL_H
#define GO_PLUGIN_SERVICE_IMPL_H

#include <goffice/app/goffice-app.h>
#include <glib-object.h>
#include <libxml/tree.h>

G_BEGIN_DECLS

struct _GOPluginService {
	GObject   g_object;

	char   *id;
	GOPlugin *plugin;
	gboolean is_loaded;

	/* protected */
	gpointer cbs_ptr;
	gboolean is_active;

	/* private */
	char *saved_description;
};

typedef struct{
	GObjectClass g_object_class;

	void (*read_xml) (GOPluginService *service, xmlNode *tree, ErrorInfo **ret_error);
	void (*activate) (GOPluginService *service, ErrorInfo **ret_error);
	void (*deactivate) (GOPluginService *service, ErrorInfo **ret_error);
	char *(*get_description) (GOPluginService *service);
} GOPluginServiceClass;

#define GPS_CLASS(klass) (G_TYPE_CHECK_CLASS_CAST ((klass), GO_PLUGIN_SERVICE_TYPE, GOPluginServiceClass))
#define GPS_GET_CLASS(o) GPS_CLASS (G_OBJECT_GET_CLASS (o))

typedef struct{
	GOPluginServiceClass plugin_service_class;
	GHashTable *pending; /* has service instances by type names */
} PluginServiceGObjectLoaderClass;

struct _PluginServiceGObjectLoader {
	GOPluginService plugin_service;
};

#define GPS_GOBJECT_LOADER_CLASS(klass) (G_TYPE_CHECK_CLASS_CAST ((klass), GO_PLUGIN_SERVICE_TYPE, PluginServiceGObjectLoaderClass))
#define GPS_GOBJECT_LOADER_GET_CLASS(o) GPS_GOBJECT_LOADER_CLASS (G_OBJECT_GET_CLASS (o))

typedef struct{
	GOPluginServiceClass plugin_service_class;
} PluginServiceSimpleClass;

struct _PluginServiceSimple {
	GOPluginService plugin_service;
};

G_END_DECLS

#endif /* GO_PLUGIN_SERVICE_IMPL_H */

