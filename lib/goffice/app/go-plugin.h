/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * go-plugin.h : A GOffice plugin
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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
 * USA
 */
#ifndef GO_PLUGIN_H
#define GO_PLUGIN_H

#include <goffice/app/goffice-app.h>
#include <glib-object.h>

G_BEGIN_DECLS

#define GO_PLUGIN_TYPE	    (go_plugin_get_type ())
#define GO_PLUGIN(o)	    (G_TYPE_CHECK_INSTANCE_CAST ((o), GO_PLUGIN_TYPE, GOPlugin))
#define IS_GO_PLUGIN(o)	    (G_TYPE_CHECK_INSTANCE_TYPE ((o), GO_PLUGIN_TYPE))

/**
 * NOTE NOTE NOTE
 * Inherits from GTypeModule
 **/
GType go_plugin_get_type (void);

/* Methods */
GOErrorStack *go_plugin_load		(GOPlugin *plugin);

/* Info */
gboolean    go_plugin_is_enabled	(GOPlugin *plugin);
gboolean    go_plugin_is_loaded		(GOPlugin *plugin);
char const *go_plugin_get_dir		(GOPlugin *plugin);
char const *go_plugin_get_id		(GOPlugin *plugin);
char const *go_plugin_get_name		(GOPlugin *plugin);
char const *go_plugin_get_description	(GOPlugin *plugin);
char const *go_plugin_get_textdomain	(GOPlugin *plugin);
GSList     *go_plugin_get_dependencies	(GOPlugin *plugin);
GSList     *go_plugin_get_services	(GOPlugin *plugin);

/* Utilities */
char	   *go_plugin_build_filename	(GOPlugin *plugin,
					 char const *first_element, ...);

G_END_DECLS

#endif /* GO_PLUGIN_H */
