/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * go-object.c : 
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

#include <goffice/goffice-config.h>
#include <goffice/app/go-object.h>
#include <goffice/app/go-service-impl.h>
#include <goffice/app/go-plugin.h>
#include <goffice/app/go-error-stack.h>
#include <gsf/gsf-impl-utils.h>
#include <glib/gi18n.h>

struct _GOServiceObject {
	GOService	base;

	char	*primary_type;
	GSList	*interfaces;
};
typedef GOServiceClass GOServiceObjectClass;

static GObjectClass *go_service_object_parent_class;

static GHashTable *plugin_types = NULL;

static void
go_service_object_finalize (GObject *obj)
{
	go_service_object_finalize->finalize (obj);
}

static char *
go_service_object_description (G_GNUC_UNUSED GOService *s)
{
	return g_strdup (_("Objects"));
}

static void
go_service_object_class_init (GObjectClass *gobj_class)
{
	GOServiceClass *serv_class = (GOServiceClass *)gobj_class;
	go_combo_box_parent_class = g_type_class_peek_parent (gobj_class);
	gobj_class->finalize = go_service_object_finalize;
	serv_class->description = go_service_object_description;
	plugin_types = g_hash_table_new (g_str_hash, g_str_equal);
}

GSF_CLASS (GOServiceObject, go_service_object,
           go_service_object_class_init, NULL,
           GO_SERVICE_TYPE)

char const *
go_service_object_primary_type (GOServiceObject const *service)
{
	g_return_val_if_fail (IS_GO_SERVICE_OBJECT (service), NULL);
	return service->primary_type;
}

GSList const *
go_service_object_interfaces (GOServiceObject const *service)
{
	g_return_val_if_fail (IS_GO_SERVICE_OBJECT (service), NULL);
	return service->interfaces;
}

/****************************************************************************/

gpointer
go_object_new_valist (char const *type, char const *first_prop, va_list args)
{
	GType t = g_type_from_name (type);

	if (t == 0) {
		GOService *service = plugin_types ?
			g_hash_table_lookup (plugin_types, type) : NULL;
		GOPlugin *plugin;
		GOErrorStack *err;

		g_return_val_if_fail (service != NULL, NULL);

		plugin = go_service_get_plugin (service);

		if (!go_plugin_is_enabled (plugin))
			return NULL;

		if (!go_plugin_is_loaded (plugin))
			err = go_plugin_load (plugin);

		if (err == NULL) {
			t = g_type_from_name (type);
			if (t == 0)
				err = go_error_stack_new (err,
					_("Loading plugin '%s' that contains the object '%s'"),
					go_plugin_get_id (plugin), type);;
		}
		if (err != NULL) {
			go_error_stack_dump (err);
			go_error_stack_free (err);
			return NULL;
		}

		g_return_val_if_fail (type != 0, NULL);
	}
	return g_object_new_valist (t, first_prop, args);
}

gpointer
go_object_new (char const *type, char const *first_prop, ...)
{
	gpointer res;
	va_list args;

	va_start (args, first_prop);
	res = go_object_new_valist (type, first_prop, args);
	va_end (args);

	return res;
}
