/* 
 * gnc-plugin.h -- A module or plugin which can add more
 *	functionality to gnucash.
 * Copyright (C) 2003 Jan Arne Petersen
 * Author: Jan Arne Petersen <jpetersen@uni-bonn.de>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
 * Boston, MA  02111-1307,  USA       gnu@gnu.org
 */

#ifndef __GNC_PLUGIN_H
#define __GNC_PLUGIN_H

#include "gnc-main-window.h"
#include "gnc-plugin-page.h"

G_BEGIN_DECLS

/* type macros */
#define GNC_TYPE_PLUGIN            (gnc_plugin_get_type ())
#define GNC_PLUGIN(o)              (G_TYPE_CHECK_INSTANCE_CAST ((o), GNC_TYPE_PLUGIN, GncPlugin))
#define GNC_PLUGIN_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_PLUGIN, GncPluginClass))
#define GNC_IS_PLUGIN(o)           (G_TYPE_CHECK_INSTANCE_TYPE ((o), GNC_TYPE_PLUGIN))
#define GNC_IS_PLUGIN_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_PLUGIN))
#define GNC_PLUGIN_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_PLUGIN, GncPluginClass))

/* typedefs & structures */
typedef struct GncPluginPrivate GncPluginPrivate;

typedef struct {
	GObject parent;
	GncPluginPrivate *priv;

	GncMainWindow *window;
} GncPlugin;

typedef struct {
	GObjectClass parent;
	const gchar *plugin_name;

	const gchar *actions_name;
	EggActionEntry *actions;
	guint n_actions; 
	const gchar *ui_filename;
 
	/* Virtual Table */
	void (* add_to_window) (GncPlugin *plugin, GncMainWindow *window, GQuark type);
	void (* remove_from_window) (GncPlugin *plugin, GncMainWindow *window, GQuark type);

	GncPluginPage *(* create_page) (GncPlugin *plugin, const gchar *uri);
} GncPluginClass;

/* function prototypes */
GType                 gnc_plugin_get_type        (void);

void                  gnc_plugin_add_to_window   (GncPlugin *plugin,
					          GncMainWindow *window,
					          GQuark type);
void                  gnc_plugin_remove_from_window (GncPlugin *plugin,
					             GncMainWindow *window,
					             GQuark type);

GncPluginPage        *gnc_plugin_create_page     (GncPlugin *plugin,
						  const gchar *uri);

G_END_DECLS

#endif /* __GNC_PLUGIN_H */
