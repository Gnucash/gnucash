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

#include <gnome.h>

#include "scripts_menu.h"

#include "top-level.h"

#include "guile-util.h"
#include "util.h"


typedef struct _ScriptInfo ScriptInfo;
struct _ScriptInfo
{
  SCM script;
  SCM script_id;

  GnomeUIInfo info[2];
};

/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_GUI;

static GSList *script_list = NULL;


static void
gnc_extensions_menu_cb( GtkWidget *w, gpointer p)
{
  ScriptInfo *si = (ScriptInfo *) p;

  if (si == NULL)
    return;

  gh_call0(si->script);
}


static ScriptInfo *
gnc_extensions_create_script_info(char *name, char *hint, SCM script)
{
  ScriptInfo *si;

  si = g_new0(ScriptInfo, 1);
  si->script = script;
  si->script_id = gnc_register_c_side_scheme_ptr(script);

  script_list = g_slist_prepend(script_list, si);

  si->info[0].type = GNOME_APP_UI_ITEM;
  si->info[0].label = g_strdup(name);
  si->info[0].hint = g_strdup(hint);
  si->info[0].moreinfo = gnc_extensions_menu_cb;
  si->info[0].user_data = si;
  si->info[0].unused_data = NULL;
  si->info[0].pixmap_type = GNOME_APP_PIXMAP_NONE;
  si->info[0].pixmap_info = NULL;
  si->info[0].accelerator_key = 0;
  si->info[0].ac_mods = (GdkModifierType) 0;
  si->info[0].widget = NULL;
  
  si->info[1].type = GNOME_APP_UI_ENDOFINFO;
  si->info[1].label = NULL;
  si->info[1].moreinfo = NULL;

  return si;
}


void
gnc_extensions_menu_add_item(char *name, char *hint, SCM script)
{
  ScriptInfo *si;

  g_return_if_fail(gh_procedure_p(script));

  si = gnc_extensions_create_script_info(name, hint, script);
  
  PINFO ("gnc_extensions_menu_add_item(): %s %s %p\n", name, hint, si);

  gnome_app_insert_menus(GNOME_APP(gnc_get_ui_data()),
                         "Extensions/", si->info);
  gnome_app_install_menu_hints(GNOME_APP(gnc_get_ui_data()), si->info);
}


static void
cleanup_script_info(gpointer script_info, gpointer not_used)
{
  ScriptInfo *si = (ScriptInfo *) script_info;

  gnc_unregister_c_side_scheme_ptr_id(si->script_id);

  g_free(si->info[0].label);
  g_free(si->info[0].hint);
  g_free(si);
}


void
gnc_extensions_shutdown()
{
  g_slist_foreach(script_list, cleanup_script_info, NULL);

  g_slist_free(script_list);

  script_list = NULL;
}
