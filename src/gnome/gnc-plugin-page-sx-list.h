/* 
 * gnc-plugin-page-sx-list.h
 *
 * Copyright (C) 2006 Josh Sled <jsled@asynchronous.org>
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
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

/** @addtogroup ContentPlugins
    @{ */
/** @addtogroup GncPluginPageSxList A SX List Plugin Page
    @{ */
/** @brief Functions providing a list of scheduled transactions as a plugin page.
    @author Josh Sled <jsled@asynchronous.org>
*/

#ifndef __GNC_PLUGIN_PAGE_SX_LIST_H
#define __GNC_PLUGIN_PAGE_SX_LIST_H

#include "config.h"
#include <glib/gi18n.h>
#include <gtk/gtkwindow.h>
#include "SchedXaction.h"
#include "gnc-plugin-page.h"


G_BEGIN_DECLS

/* type macros */
#define GNC_TYPE_PLUGIN_PAGE_SX_LIST            (gnc_plugin_page_sx_list_get_type ())
#define GNC_PLUGIN_PAGE_SX_LIST(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_PLUGIN_PAGE_SX_LIST, GncPluginPageSxList))
#define GNC_PLUGIN_PAGE_SX_LIST_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_PLUGIN_PAGE_SX_LIST, GncPluginPageSxListClass))
#define GNC_IS_PLUGIN_PAGE_SX_LIST(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_PLUGIN_PAGE_SX_LIST))
#define GNC_IS_PLUGIN_PAGE_SX_LIST_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_PLUGIN_PAGE_SX_LIST))
#define GNC_PLUGIN_PAGE_SX_LIST_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_PLUGIN_PAGE_SX_LIST, GncPluginPageSxListClass))

#define GNC_PLUGIN_PAGE_SX_LIST_NAME "GncPluginPageSxList"

/* typedefs & structures */
typedef struct
{
    GncPluginPage gnc_plugin_page;
} GncPluginPageSxList;

typedef struct
{
    GncPluginPageClass gnc_plugin_page;
} GncPluginPageSxListClass;

/* function prototypes */

/**
 * Retrieve the type number for an "sx list" plugin page.
 * @return The type number.
 */
GType gnc_plugin_page_sx_list_get_type(void);

/**
 * @return The newly created plugin page.
 **/
GncPluginPage *gnc_plugin_page_sx_list_new(void);

G_END_DECLS

#endif /* __GNC_PLUGIN_PAGE_SX_LIST_H */
/** @} */
/** @} */
