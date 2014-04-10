/********************************************************************\
 * gnc-menu-extensions.h -- functions to build dynamic menus        *
 * Copyright (C) 1999 Rob Browning         	                    *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
\********************************************************************/

#ifndef GNC_MENU_EXTENSIONS_H
#define GNC_MENU_EXTENSIONS_H

#include <guile/gh.h>
#include <gnome.h>

#define WINDOW_NAME_MAIN     "Main"
#define WINDOW_NAME_REGISTER "Register"
#define WINDOW_NAME_INVOICE  "Invoice"
#define WINDOW_NAME_ALL      "All"

void gnc_add_c_extension(GnomeUIInfo *info, gchar *path);
void gnc_add_scm_extension(SCM extension);

/* Replacement for gnome_app_insert_menus, since the original one will
 * fail for i18n. In particular, as soon as the gnome stock menus
 * (created through the macros in gnome-app-helper) have a different
 * translation in the original gnome libs compared to the gnucash
 * message catalog, then the gnome_app_insert_menus will
 * fail. Therefore this function looks up the translation of each 'path'
 * element in the gettext domain "gnome-libs" first. 
 *
 * This function should be used in all places where the 'path'
 * contains a stock gnome menu, created through the macros in
 * gnome-app-helper. */
void
gnc_gnome_app_insert_menus (GnomeApp *app, const gchar *path, 
			    GnomeUIInfo *menuinfo);

/* This is called from the window initializing code, when the actual
 * menu items stored by the above functions should now be inserted in
 * the menu of the GnomeApp app.
 *
 * app - The GnomeApp to add the stored menu items
 * prefix - The prefix of the window that is currently being set up.
 */
void gnc_extensions_menu_setup(GnomeApp * app, gchar *prefix);
/* This is called from the window initializing code, when the actual
 * menu items stored by the above functions should now be inserted in
 * the menu of the GnomeApp app.
 *
 * Use this function when your menu callbacks needs some user_data
 * pointer in order to access window-related data.
 *
 * app - The GnomeApp to add the stored menu items
 * prefix - The prefix of the window that is currently being set up.
 * user_data - The user data to be passed on to menu item's callback functions.
 */
void gnc_extensions_menu_setup_with_data(GnomeApp * app, 
					 gchar *prefix, 
					 gpointer user_data);
void gnc_extensions_shutdown(void);

#endif
