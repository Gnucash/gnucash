/********************************************************************\
 * gnc-menu-extensions.c -- functions to build dynamic menus        *
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

#include "config.h"

#include <gnome.h>

#include "guile-util.h"
#include "gnc-engine-util.h"
#include "gnc-menu-extensions.h"
#include "gnc-ui.h"

typedef struct _ExtensionInfo ExtensionInfo;
struct _ExtensionInfo
{
  SCM extension;
  gchar *path;

  GnomeUIInfo info[2];

  gpointer extra_info;
};

typedef struct _Getters Getters;
struct _Getters
{
  SCM type;
  SCM name;
  SCM documentation;
  SCM path;
  SCM script;
};

/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_GUI;

static GSList *extension_list = NULL;
static Getters getters = {0, 0, 0, 0, 0};

static void
initialize_getters()
{
  static gboolean getters_initialized = FALSE;

  if (getters_initialized)
    return;

  getters.type = gh_eval_str("gnc:extension-type");
  getters.name = gh_eval_str("gnc:extension-name");
  getters.documentation = gh_eval_str("gnc:extension-documentation");
  getters.path = gh_eval_str("gnc:extension-path");
  getters.script = gh_eval_str("gnc:extension-script");

  getters_initialized = TRUE;
}


static GnomeUIInfoType
gnc_extension_type(ExtensionInfo *ext_info)
{
  GnomeUIInfoType type;
  char *string;

  initialize_getters();

  string = gnc_guile_call1_symbol_to_string(getters.type, ext_info->extension);
  if (string == NULL)
  {
    PERR("bad type");
    return GNOME_APP_UI_ENDOFINFO;
  }

  if (safe_strcmp(string, "menu-item") == 0)
    type = GNOME_APP_UI_ITEM;
  else if (safe_strcmp(string, "menu") == 0)
    type = GNOME_APP_UI_SUBTREE;
  else if (safe_strcmp(string, "separator") == 0)
    type = GNOME_APP_UI_SEPARATOR;
  else
  {
    PERR("bad type");
    type = GNOME_APP_UI_ENDOFINFO;
  }

  free(string);

  return type;
}


/* returns malloc'd name */
static char *
gnc_extension_name(ExtensionInfo *ext_info)
{
  initialize_getters();

  return gnc_guile_call1_to_string(getters.name, ext_info->extension);
}


/* returns malloc'd docs */
static char *
gnc_extension_documentation(ExtensionInfo *ext_info)
{
  initialize_getters();

  return gnc_guile_call1_to_string(getters.documentation, ext_info->extension);
}


/* returns g_malloc'd path */
static char *
gnc_extension_path(SCM extension)
{
  SCM path;
  gchar **strings;
  gchar *fullpath;
  gint i;

  initialize_getters();

  path = gnc_guile_call1_to_list(getters.path, extension);
  if (path == SCM_UNDEFINED)
    return g_strdup("");

  if (gh_null_p(path))
    return g_strdup("");

  strings = g_new0(gchar *, gh_length(path) + 1);

  i = 0;
  while (!gh_null_p(path))
  {
    SCM item;

    item = gh_car(path);
    path = gh_cdr(path);

    if (gh_string_p(item))
      strings[i] = gh_scm2newstr(item, NULL);
    else
    {
      while (i > 0)
        free(strings[--i]);
      g_free(strings);

      PERR("not a string");

      return NULL;
    }

    i++;
  }

  fullpath = g_strjoinv("/", strings);

  i = 0;
  while (strings[i] != NULL)
    free(strings[i++]);
  g_free(strings);

  return fullpath;
}


static void
gnc_extension_run_script(ExtensionInfo *ext_info)
{
  SCM script;

  initialize_getters();

  script = gnc_guile_call1_to_procedure(getters.script, ext_info->extension);
  if (script == SCM_UNDEFINED)
  {
    PERR("not a procedure.");
    return;
  }

  gh_call0(script);
}


static void
gnc_extension_cb(GtkWidget *w, gpointer data)
{
  ExtensionInfo *ext_info = data;

  if (ext_info == NULL)
    return;

  gnc_extension_run_script(ext_info);
}


static ExtensionInfo *
gnc_create_extension_info(SCM extension)
{
  GnomeUIInfo *info;
  ExtensionInfo *ext_info;
  char *string;

  ext_info = g_new0(ExtensionInfo, 1);
  ext_info->extension = extension;
  ext_info->path = gnc_extension_path(extension);

  ext_info->info[0].type = gnc_extension_type(ext_info);

  switch (ext_info->info[0].type)
  {
    case GNOME_APP_UI_ITEM:
      ext_info->info[0].moreinfo = gnc_extension_cb;

      string = gnc_extension_documentation(ext_info);
      ext_info->info[0].hint = g_strdup(string);
      if (string != NULL)
        free(string);

      string = gnc_extension_name(ext_info);
      ext_info->info[0].label = g_strdup(string);
      if (string != NULL)
        free(string);

      break;

    case GNOME_APP_UI_SUBTREE:
      info = g_new(GnomeUIInfo, 1);
      info->type = GNOME_APP_UI_ENDOFINFO;
      ext_info->info[0].moreinfo = info;
      ext_info->extra_info = info;

      string = gnc_extension_name(ext_info);
      ext_info->info[0].label = g_strdup(string);
      if (string != NULL)
        free(string);

      break;

    case GNOME_APP_UI_SEPARATOR:
      ext_info->info[0].type = GNOME_APP_UI_SEPARATOR;
      break;

    default:
      PERR("bad item type");
      g_free(ext_info);
      return NULL;
  }

  ext_info->info[0].user_data = ext_info;
  ext_info->info[0].pixmap_type = GNOME_APP_PIXMAP_NONE;
  ext_info->info[1].type = GNOME_APP_UI_ENDOFINFO;

  scm_protect_object(extension);
  
  /* need to append so we can run them in order */
  extension_list = g_slist_append(extension_list, ext_info);

  return ext_info;
}


static void
cleanup_extension_info(gpointer extension_info, gpointer not_used)
{
  ExtensionInfo *ext_info = extension_info;

  if (ext_info->extension)
    scm_unprotect_object(ext_info->extension);

  g_free(ext_info->info[0].label);
  g_free(ext_info->info[0].hint);
  g_free(ext_info->extra_info);
  g_free(ext_info->path);
  g_free(ext_info);
}


void
gnc_add_scm_extension(SCM extension)
{
  ExtensionInfo *ext_info;
  ext_info = gnc_create_extension_info(extension);
  if (ext_info == NULL)
  {
    PERR("bad extension");
    return;
  }
}

void
gnc_add_c_extension(GnomeUIInfo *info, gchar *path)
{
  ExtensionInfo *ext_info;

  ext_info = g_new0(ExtensionInfo, 1);
  ext_info->path = g_strdup(path);

  ext_info->info[0] = *info;
  ext_info->info[0].label = g_strdup(info->label);
  ext_info->info[0].hint  = g_strdup(info->hint);
  ext_info->info[1].type  = GNOME_APP_UI_ENDOFINFO;

  /* need to append so we can run them in order */
  extension_list = g_slist_append(extension_list, ext_info);
}

void
gnc_extensions_menu_setup(GnomeApp * app)
{
  GSList        * l = NULL;
  ExtensionInfo * info;
  
  for(l=extension_list; l; l=l->next) {
    info = l->data;
    gnome_app_insert_menus(app, info->path, info->info);
    gnome_app_install_menu_hints(app, info->info); 
  }
}

void
gnc_extensions_shutdown(void)
{
  g_slist_foreach(extension_list, cleanup_extension_info, NULL);

  g_slist_free(extension_list);

  extension_list = NULL;
}
