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
  gchar *window;
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

  getters.type = scm_c_eval_string("gnc:extension-type");
  getters.name = scm_c_eval_string("gnc:extension-name");
  getters.documentation = scm_c_eval_string("gnc:extension-documentation");
  getters.path = scm_c_eval_string("gnc:extension-path");
  getters.script = scm_c_eval_string("gnc:extension-script");

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
static void
gnc_extension_path(SCM extension, char **window, char **fullpath)
{
  SCM path;
  gchar **strings;
  gint i;

  initialize_getters();

  path = gnc_guile_call1_to_list(getters.path, extension);
  if (path == SCM_UNDEFINED) {
    *window = g_strdup("");
    *fullpath = g_strdup("");
    return;
  }

  if (SCM_NULLP(path)) {
    *window = g_strdup("");
    *fullpath = g_strdup("");
    return;
  }

  strings = g_new0(gchar *, scm_ilength(path) + 1);

  i = 0;
  while (!SCM_NULLP(path))
  {
    SCM item;

    item = SCM_CAR(path);
    path = SCM_CDR(path);

    if (SCM_STRINGP(item))
    {
      /* strings[i] = gh_scm2newstr(item, NULL); */
      strings[i] = SCM_STRING_CHARS (item);
    }
    else
    {
      g_free(strings);

      PERR("not a string");

      *window = g_strdup("");
      *fullpath = g_strdup("");
      return;
    }

    i++;
  }

  if (i > 0) {
    *window = g_strdup(strings[0]);
    *fullpath = g_strjoinv("/", strings+1);
  } else {
    *window = g_strdup(WINDOW_NAME_MAIN);
    *fullpath = g_strjoinv("/", strings);
  }

  g_free(strings);
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

  scm_call_0(script);
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
  gnc_extension_path(extension, &ext_info->window, &ext_info->path);

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

  scm_gc_protect_object(extension);
  
  /* need to append so we can run them in order */
  extension_list = g_slist_append(extension_list, ext_info);

  return ext_info;
}


static void
cleanup_extension_info(gpointer extension_info, gpointer not_used)
{
  ExtensionInfo *ext_info = extension_info;

  if (ext_info->extension)
    scm_gc_unprotect_object(ext_info->extension);

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
  char *separator;

  ext_info = g_new0(ExtensionInfo, 1);
  separator = index(path, '/');
  if (separator) {
    ext_info->window = g_strndup(path, separator-path);
    ext_info->path = g_strdup(separator+1);
  } else {
    ext_info->window = g_strdup(WINDOW_NAME_MAIN);
    ext_info->path = g_strdup(path);
  }

  ext_info->info[0] = *info;
  ext_info->info[0].label = g_strdup(info->label);
  ext_info->info[0].hint  = g_strdup(info->hint);
  ext_info->info[1].type  = GNOME_APP_UI_ENDOFINFO;

  /* need to append so we can run them in order */
  extension_list = g_slist_append(extension_list, ext_info);
}


/* This code is directly copied from libgnomeui's gnome-app-helper.c
 * without modifications. */
static gint
g_strncmp_ignore_char( const gchar *first, const gchar *second, gint length, gchar ignored )
{
    gint i, j;
    for ( i = 0, j = 0; i < length; i++, j++ )
	{
	    while ( first[i] == ignored && i < length ) i++;
	    while ( second[j] == ignored ) j++;
	    if ( i == length )
		return 0;
	    if ( first[i] < second[j] )
		return -1;
	    if ( first[i] > second[j] )
		return 1;
	}
    return 0;
}

/* This code is copied from libgnomeui's gnome-app-helper.c
 * originally. */
static const gchar *
gnc_gnome_app_helper_gettext (const gchar *str)
{
    const char *s;

    /* First try to look up the string in gettext domain gnome-libs,
     * since this is where the original gnome stock labels have been
     * translated. */
    s = dgettext ("gnome-libs", str);
    if ( s == str )
	s = gettext (str);

    return s;
}

/* This code is directly copied from libgnomeui's gnome-app-helper.c,
 * except for the call to the translation lookup . */
/**
 * gnome_app_find_menu_pos
 * @parent: Root menu shell widget containing menu items to be searched
 * @path: Specifies the target menu item by menu path
 * @pos: (output) returned item position
 *
 * Description:
 * finds menu item described by path starting
 * in the GtkMenuShell top and returns its parent GtkMenuShell and the
 * position after this item in pos:  gtk_menu_shell_insert(p, w, pos)
 * would then insert widget w in GtkMenuShell p right after the menu item
 * described by path.
 **/
static GtkWidget *
gnc_gnome_app_find_menu_pos (GtkWidget *parent, const gchar *path, gint *pos)
{
    GtkBin *item;
    gchar *label = NULL;
    GList *children;
    gchar *name_end;
    gchar *part;
    const gchar *transl;
    gint p;
    int  path_len;

    g_return_val_if_fail (parent != NULL, NULL);
    g_return_val_if_fail (path != NULL, NULL);
    g_return_val_if_fail (pos != NULL, NULL);

    children = GTK_MENU_SHELL (parent)->children;

    name_end = strchr(path, '/');
    if(name_end == NULL)
	path_len = strlen(path);
    else
	path_len = name_end - path;

    if (path_len == 0) {

	if (children && GTK_IS_TEAROFF_MENU_ITEM(children->data))
	    /* consider the position after the tear off item as the topmost one. */
	    *pos = 1;
	else
	    *pos = 0;
	return parent;
    }

    /* this ugly thing should fix the localization problems */
    part = g_malloc(path_len + 1);
    if(!part)
	return NULL;
    strncpy(part, path, path_len);
    part[path_len] = '\0';
    transl = gnc_gnome_app_helper_gettext (part);
    path_len = strlen(transl);

    p = 0;

    while (children){
	item = GTK_BIN (children->data);
	children = children->next;
	label = NULL;
	p++;

	if (GTK_IS_TEAROFF_MENU_ITEM(item))
	    label = NULL;
	else if (!item->child)          /* this is a separator, right? */
	    label = "<Separator>";
	else if (GTK_IS_LABEL (item->child))  /* a simple item with a label */
	    label = GTK_LABEL (item->child)->label;
	else
	    label = NULL; /* something that we just can't handle */
	/*fprintf(stderr, "Transl '%s', Label '%s'\n", transl, label);*/
	if (label && (g_strncmp_ignore_char (transl, label, path_len, '_') == 0)){
	    if (name_end == NULL) {
		*pos = p;
		g_free(part);
		return parent;
	    }
	    else if (GTK_MENU_ITEM (item)->submenu) {
		g_free(part);
		return gnc_gnome_app_find_menu_pos
		    (GTK_MENU_ITEM (item)->submenu,
		     (gchar *)(name_end + 1), pos);
	    }
	    else {
		g_free(part);
		return NULL;
	    }
	}
    }

    g_free(part);
    return NULL;
}

/* This code is more or less copied from libgnomeui's gnome-app-helper.c . */
void
gnc_gnome_app_insert_menus (GnomeApp *app, const gchar *path, GnomeUIInfo *menuinfo)
{
    GtkWidget *menu_shell;
    gint pos;

    menu_shell = gnc_gnome_app_find_menu_pos(app->menubar, path, &pos);
    if(menu_shell == NULL) {
	g_warning("gnc_gnome_app_insert_menus: couldn't find "
		  "insertion point for menus!");
	return;
    }

    /* create menus and insert them */
    gnome_app_fill_menu (GTK_MENU_SHELL (menu_shell), menuinfo, 
			 app->accel_group, TRUE, pos);
}

void
gnc_extensions_menu_setup(GnomeApp * app, gchar *window)
{
  GSList        * l = NULL;
  ExtensionInfo * info;

  for(l=extension_list; l; l=l->next) {
    info = l->data;
    if ((strcmp(info->window, window) != 0) &&
	(strcmp(info->window, WINDOW_NAME_ALL) != 0))
      continue;
    /* fprintf(stderr, "Inserting extension menu at path '%s'\n", info->path); */
    gnc_gnome_app_insert_menus(app, info->path, info->info);
    gnome_app_install_menu_hints(app, info->info); 
  }
}

void
gnc_extensions_menu_setup_with_data(GnomeApp * app, 
				    gchar *window, gpointer user_data)
{
  GSList        * l = NULL;
  ExtensionInfo * info;

  for(l=extension_list; l; l=l->next) {
    info = l->data;
    if ((strcmp(info->window, window) != 0) &&
	(strcmp(info->window, WINDOW_NAME_ALL) != 0))
      continue;
    /* fprintf(stderr, "Inserting extension menu/w/d at path '%s'\n", info->path); */
    gnome_app_insert_menus_with_data(app, info->path, info->info, user_data);
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
