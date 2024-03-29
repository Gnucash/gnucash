/*
 * gnc-plugin-ofx.h --
 * Copyright (C) 2003 David Hampton <hampton@employees.org>
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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

#ifndef __GNC_PLUGIN_OFX_H
#define __GNC_PLUGIN_OFX_H

#include <gtk/gtk.h>

#include "gnc-plugin.h"

G_BEGIN_DECLS

/* type macros */
#define GNC_TYPE_PLUGIN_OFX            (gnc_plugin_ofx_get_type ())
G_DECLARE_FINAL_TYPE (GncPluginOfx, gnc_plugin_ofx, GNC, PLUGIN_OFX, GncPlugin)

#define GNC_PLUGIN_OFX_NAME "gnc-plugin-ofx"

/* function prototypes */
GncPlugin *gnc_plugin_ofx_new      (void);

void       gnc_plugin_ofx_create_plugin (void);

G_END_DECLS

#endif /* __GNC_PLUGIN_OFX_H */
