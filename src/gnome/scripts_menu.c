/********************************************************************\
 * scripts_menu.h -- functions to build the dynamic scripts menu    *
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
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
\********************************************************************/

#include "scripts_menu.h"

#include <guile/gh.h>

#include "top-level.h"
#include "util.h"

/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_GUI;

/* FIXME: is this always kosher?  Will SCM's always fit in a gpointer? */

static void
gnc_extensions_menu_cb( GtkWidget *w, gpointer p)
{
  SCM closure = (SCM) p;

  if (!p)
    return;

  gh_call0(closure);
}

void
gnc_extensions_menu_add_item(char name[],
                             char hint[],
                             gpointer data)
{
  GnomeUIInfo item_info[2];
  GnomeUIInfo tmpi;
  
  tmpi.type = GNOME_APP_UI_ITEM;
  tmpi.label = N_(name);
  tmpi.hint = N_(hint);
  tmpi.moreinfo = gnc_extensions_menu_cb;
  tmpi.user_data = data;
  tmpi.unused_data = NULL;
  tmpi.pixmap_type = GNOME_APP_PIXMAP_NONE;
  tmpi.pixmap_info = NULL;
  tmpi.accelerator_key = 0;
  tmpi.ac_mods = (GdkModifierType) 0;
  tmpi.widget = NULL;
  item_info[0] = tmpi;
  
  tmpi.type = GNOME_APP_UI_ENDOFINFO;
  tmpi.label = NULL;
  tmpi.moreinfo = NULL;
  item_info[1] = tmpi;
  
  PINFO ("gnc_extensions_menu_add_item(): %s %s %p\n", name, hint, data);
  gnome_app_insert_menus(GNOME_APP(gnc_get_ui_data()), "Extensions/",
			 item_info);
  gnome_app_install_menu_hints(GNOME_APP(gnc_get_ui_data()), item_info);
}
