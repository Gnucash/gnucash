/*
 * gnc-plugin_page-owner-tree.h --
 *
 * Copyright (C) 2011 Geert Janssens <geert@kobaltwit.be>
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

/** @addtogroup ContentPlugins
    @{ */
/** @addtogroup GncPluginPageOwnerTree An Owner Tree Plugin
    @{ */
/** @file gnc-plugin-page-owner-tree.h
    @brief Functions providing a page which lists owners of one type. This type
           can be vendors, customers or employees.
    @author Copyright (C) 2011 Geert Janssens <geert@kobaltwit.be>
*/

#ifndef __GNC_PLUGIN_PAGE_OWNER_TREE_H
#define __GNC_PLUGIN_PAGE_OWNER_TREE_H

#include <gtk/gtk.h>

#include "gnc-plugin-page.h"
#include "gncOwner.h"

G_BEGIN_DECLS

/* type macros */
#define GNC_TYPE_PLUGIN_PAGE_OWNER_TREE            (gnc_plugin_page_owner_tree_get_type ())
#define GNC_PLUGIN_PAGE_OWNER_TREE(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_PLUGIN_PAGE_OWNER_TREE, GncPluginPageOwnerTree))
#define GNC_PLUGIN_PAGE_OWNER_TREE_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_PLUGIN_PAGE_OWNER_TREE, GncPluginPageOwnerTreeClass))
#define GNC_IS_PLUGIN_PAGE_OWNER_TREE(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_PLUGIN_PAGE_OWNER_TREE))
#define GNC_IS_PLUGIN_PAGE_OWNER_TREE_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_PLUGIN_PAGE_OWNER_TREE))
#define GNC_PLUGIN_PAGE_OWNER_TREE_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_PLUGIN_PAGE_OWNER_TREE, GncPluginPageOwnerTreeClass))

#define GNC_PLUGIN_PAGE_OWNER_TREE_NAME "GncPluginPageOwnerTree"

/* typedefs & structures */
typedef struct
{
    GncPluginPage gnc_plugin_page;
} GncPluginPageOwnerTree;

typedef struct
{
    GncPluginPageClass gnc_plugin_page;

    /* callbacks */
    void (*owner_selected) (GncPluginPage *page,
                            GncOwner      *owner);

} GncPluginPageOwnerTreeClass;

/* function prototypes */

/** Retrieve the type number for an "owner tree" plugin page.
 *
 *  @return The type number.
 */
GType gnc_plugin_page_owner_tree_get_type (void);


/** Create a new "owner tree" plugin page.
 *
 *  @param owner_type The owner type to create a page for. Can be any of
 *         the owner types defined in GnuCash, like vendor, customer,...
 *
 *  @return The newly created plugin page.
 */
GncPluginPage *gnc_plugin_page_owner_tree_new  (GncOwnerType owner_type);


/** Given a pointer to an owner tree plugin page, return the
 *  selected owner (if any).
 *
 *  @param page The "owner tree" page.
 *
 *  @return The currently selected owner.  NULL if no owner is
 *  selected.
 */
GncOwner * gnc_plugin_page_owner_tree_get_current_owner (GncPluginPageOwnerTree *page);

G_END_DECLS

#endif /* __GNC_PLUGIN_PAGE_OWNER_TREE_H */
/** @} */
/** @} */
