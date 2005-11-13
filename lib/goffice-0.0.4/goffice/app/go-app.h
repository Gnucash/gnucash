/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * go-app.h :  A GOffice appument
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
#ifndef GO_APP_H
#define GO_APP_H

#include <goffice/app/goffice-app.h>
#include <glib-object.h>

G_BEGIN_DECLS

#define GO_APP_TYPE	    (go_app_get_type ())
#define GO_APP(o)	    (G_TYPE_CHECK_INSTANCE_CAST ((o), GO_APP_TYPE, GOApp))
#define IS_GO_APP(o)	    (G_TYPE_CHECK_INSTANCE_TYPE ((o), GO_APP_TYPE))

GType go_app_get_type (void);

/* FIXME : should be in GOContext which App inherits from */
GOPlugin *go_app_get_plugin (char const *id);

/* TODO : I suspect these should be lookups of some sort
 *  eg go_app_find_in_lib_dir (GOApp const *app, subdir);
 *  or possibly
 *     go_app_foreach_lib_dir (GOApp const *app, gboolean (*handler)(path, userdata));
 **/
char	 *go_app_sys_lib_dir    (GOApp const *app, char const *subdir);
char	 *go_app_sys_data_dir   (GOApp const *app, char const *subdir);
char	 *go_app_sys_plugin_dir (GOApp const *app);

/* FIXME : Seems gui specific, move to gui-utils */
char	 *go_app_sys_glade_dir  (GOApp const *app);

G_END_DECLS

#endif /* GO_APP_H */
