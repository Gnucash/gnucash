/* 
 * gnc-plugin-file-history.c -- 
 * Copyright (C) 2003,2005 David Hampton <hampton@employees.org>
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

/** @addtogroup MenuPlugins
    @{ */
/** @addtogroup PluginFileHistory File History Menu Items
    @{ */
/** @file gnc-plugin-file-history.c
    @brief Functions providing the file history menu.
    @author Copyright (C) 2003,2005 David Hampton <hampton@employees.org>
*/

#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <glib/gprintf.h>
#include <string.h>

#include "gnc-gkeyfile-utils.h"
#include "gnc-file.h"
#include "gnc-main-window.h"
#include "gnc-plugin-file-history.h"
#include "gnc-window.h"
#include "gnc-engine.h"
#include "gnc-gconf-utils.h"

static GObjectClass *parent_class = NULL;

#define FILENAME_STRING "filename"

static void gnc_plugin_file_history_class_init (GncPluginFileHistoryClass *klass);
static void gnc_plugin_file_history_init (GncPluginFileHistory *plugin);
static void gnc_plugin_file_history_finalize (GObject *object);

static void gnc_plugin_file_history_add_to_window (GncPlugin *plugin, GncMainWindow *window, GQuark type);
static void gnc_plugin_file_history_remove_from_window (GncPlugin *plugin, GncMainWindow *window, GQuark type);


/** The debugging module used by this file. */
static QofLogModule log_module = GNC_MOD_GUI;

/* Command callbacks */
static void gnc_plugin_file_history_cmd_open_file (GtkAction *action, GncMainWindowActionData *data);


/** The label given to the main window for this plugin. */
#define PLUGIN_ACTIONS_NAME "gnc-plugin-file-history-actions"
/** The name of the UI description file for this plugin. */
#define PLUGIN_UI_FILENAME  "gnc-plugin-file-history-ui.xml"

#define GNOME1_HISTORY "History"
#define GNOME1_MAXFILES "MaxFiles"

/** A placeholder set of actions that are filled in by this plugin.
 *  As the user opens files, the names and visibility of these actions
 *  will be updated to reflect the users recent choices.  This list is
 *  limited to ten actions, although there may be a smaller limit set
 *  by the user.  The typical limit is four. */
static GtkActionEntry gnc_plugin_actions [] = {
	{ "RecentFile0Action", NULL, "", NULL, NULL, G_CALLBACK (gnc_plugin_file_history_cmd_open_file) },
	{ "RecentFile1Action", NULL, "", NULL, NULL, G_CALLBACK (gnc_plugin_file_history_cmd_open_file) },
	{ "RecentFile2Action", NULL, "", NULL, NULL, G_CALLBACK (gnc_plugin_file_history_cmd_open_file) },
	{ "RecentFile3Action", NULL, "", NULL, NULL, G_CALLBACK (gnc_plugin_file_history_cmd_open_file) },
	{ "RecentFile4Action", NULL, "", NULL, NULL, G_CALLBACK (gnc_plugin_file_history_cmd_open_file) },
	{ "RecentFile5Action", NULL, "", NULL, NULL, G_CALLBACK (gnc_plugin_file_history_cmd_open_file) },
	{ "RecentFile6Action", NULL, "", NULL, NULL, G_CALLBACK (gnc_plugin_file_history_cmd_open_file) },
	{ "RecentFile7Action", NULL, "", NULL, NULL, G_CALLBACK (gnc_plugin_file_history_cmd_open_file) },
	{ "RecentFile8Action", NULL, "", NULL, NULL, G_CALLBACK (gnc_plugin_file_history_cmd_open_file) },
	{ "RecentFile9Action", NULL, "", NULL, NULL, G_CALLBACK (gnc_plugin_file_history_cmd_open_file) },
};
/** The number of actions provided by this plugin. */
static guint gnc_plugin_n_actions = G_N_ELEMENTS (gnc_plugin_actions);


/** The instance private data for a file history plugin.  This data
 *  structure is unused. */
typedef struct GncPluginFileHistoryPrivate
{
	gpointer dummy;
} GncPluginFileHistoryPrivate;


#define GNC_PLUGIN_FILE_HISTORY_GET_PRIVATE(o)  \
   (G_TYPE_INSTANCE_GET_PRIVATE ((o), GNC_TYPE_PLUGIN_FILE_HISTORY, GncPluginFileHistoryPrivate))

/************************************************************
 *                     Other Functions                      *
 ************************************************************/

/** Convert an array index into a gconf key string.
 *
 *  @param index An index number that can be used with the
 *  gnc_plugin_actions array.
 *
 *  @return The partial gconf key associated with this array entry. It
 *  is the callers responsibility to free this string when
 *  finished.  */
static gchar *
gnc_history_gconf_index_to_key (guint index)
{
  return g_strdup_printf(HISTORY_STRING_FILE_N, index);
}


/** Convert a gconf key string into an array index.  This function
 *  uses sscanf to pull the number off the end of the key and convert
 *  it to an integer.
 *
 *  @param key The last part of the gconf key.
 *
 *  @return An index number that can be used with the
 *  gnc_plugin_actions array. */
static gint
gnc_history_gconf_key_to_index (const gchar *key)
{
  gint index, result;

  result = sscanf(key, HISTORY_STRING_FILE_N, &index);
  if (result != 1)
    return -1;
  if ((index < 0) || (index >= gnc_plugin_n_actions))
      return -1;
  return index;
}


/*  Add a file name to the front of the file "history list".  If the
 *  name already exist on the list, then it is moved from its current
 *  location to the front of the list.  The "list" is actually a
 *  sequence of up to ten gconf keys.
 */
void
gnc_history_add_file (const char *newfile)
{
  gchar *filename, *from, *to;
  gint i, last;

  if (newfile == NULL)
    return;
  if (!g_utf8_validate(newfile, -1, NULL))
    return;

  /*
   * Look for the filename in gconf.
   */
  last = MAX_HISTORY_FILES - 1;
  for (i = 0; i < MAX_HISTORY_FILES; i++) {
    from = gnc_history_gconf_index_to_key(i);
    filename = gnc_gconf_get_string(HISTORY_STRING_SECTION, from, NULL);
    g_free(from);

    if (!filename) {
      last = i;
      break;
    }
    if (g_utf8_collate(newfile, filename) == 0) {
      g_free(filename);
      last = i;
      break;
    }
    g_free(filename);
  }

  /*
   * Shuffle filenames upward through gconf.
   */
  to = gnc_history_gconf_index_to_key(last);
  for (i = last - 1; i >= 0; i--) {
    from = gnc_history_gconf_index_to_key(i);
    filename = gnc_gconf_get_string(HISTORY_STRING_SECTION, from, NULL);
    if (filename) {
      gnc_gconf_set_string(HISTORY_STRING_SECTION, to, filename, NULL);
      g_free(filename);
    } else {
      gnc_gconf_unset(HISTORY_STRING_SECTION, to, NULL);
    }
    g_free(to);
    to = from;
  }

  /*
   * Store the new zero entry.
   */
  gnc_gconf_set_string(HISTORY_STRING_SECTION, to, newfile, NULL);
  g_free(to);
}


/** Remove all occurences of a file name from the history list.  Move
 *  the other key values up in the list to fill the gaps.
 *
 *  @param filename The name of the file to remove from the list.
 */
void
gnc_history_remove_file (const char *oldfile)
{
  gchar *filename, *from, *to;
  gint i, j;

  if (!oldfile)
    return;
  if (!g_utf8_validate(oldfile, -1, NULL))
    return;

  for (i=0, j=0; i<MAX_HISTORY_FILES; i++) {
    from = gnc_history_gconf_index_to_key(i);
    filename = gnc_gconf_get_string(HISTORY_STRING_SECTION, from, NULL);

    if (filename) {
      if (g_utf8_collate(oldfile, filename) == 0) {
        gnc_gconf_unset(HISTORY_STRING_SECTION, from, NULL);
      } else {
        if (i != j) {
          to = gnc_history_gconf_index_to_key(j);
          gnc_gconf_set_string(HISTORY_STRING_SECTION, to, filename, NULL);
          gnc_gconf_unset(HISTORY_STRING_SECTION, from, NULL);
          g_free(to);
        }
        j++;
      }
    }
    g_free(from);
  }
}

/*  Retrieve the name of the file most recently accessed.  This is the
 *  name at the front of the list.  Since the "list" is actually a
 *  sequence of up to ten gconf keys, this is the value of key zero.
 */
char *
gnc_history_get_last (void)
{
  char *filename, *key;

  key = gnc_history_gconf_index_to_key(0);
  filename = gnc_gconf_get_string(HISTORY_STRING_SECTION, key, NULL);
  g_free(key);

  return filename;
}


/************************************************************
 *                     Other Functions                      *
 ************************************************************/

/** This routine takes a filename and modifies it so that it will
 *  display correctly in a GtkLabel.  It also adds a mnemonic to the
 *  start of the menu item.
 *
 *  @param filename A pointer to the filename to mangle.
 *
 *  @return A pointer to the mangled filename.  The Caller is
 *  responsible for freeing this memory.
 */
static gchar *
gnc_history_generate_label (int index, const gchar *filename)
{
	const gchar *src;
	gchar *result, *dst;
	gunichar  unichar;

	/* raw byte length, not num characters */
	result = g_malloc(strlen(filename) * 2);

	dst = result;
	if (index < 10)
	  dst += g_sprintf(result, "_%d ", (index + 1) % 10);

	/* If the filename begins with "mysql://" or "postgres://", hide the
	user name and password.  Otherwise, it is a filename - hide everything
	except the file name. */

	if (g_ascii_strncasecmp(filename, "mysql://", 8) == 0 ||
		g_ascii_strncasecmp(filename, "postgres://", 11) == 0 ) {
	  gint num_colons = 0;

	  /* Loop for all chars and copy from 'src' to 'dst'.  While doing this,
	     convert username and password (after 3rd ':') to asterisks. */
	  src = filename;
	  for( ; *src; src = g_utf8_next_char(src)) {
	    if (num_colons < 3 || *src == ':') {
	      unichar = g_utf8_get_char(src);
		} else {
		  unichar = '*';
		}
		dst += g_unichar_to_utf8 (unichar, dst);
	    if (unichar == '_') {
	      dst += g_unichar_to_utf8 ('_', dst);
		} else if (unichar == ':') {
		  num_colons++;
		}
	  }
	} else {
	  /* Find the filename portion of the path */
	  src = g_utf8_strrchr(filename, -1, G_DIR_SEPARATOR);
	  if (src) {
	    src = g_utf8_next_char(src);

	    /* Fix up any underline characters so they aren't mistaken as
	     * command accelerator keys. */
	    for ( ; *src; src = g_utf8_next_char(src)) {
	      unichar = g_utf8_get_char(src);
	      dst += g_unichar_to_utf8 (unichar, dst);

	      if (unichar == '_')
	        dst += g_unichar_to_utf8 ('_', dst);
	    }
	  }
	}

	*dst = '\0';
	return result;
}


/** Update one entry in the file history menu.  This function is
 *  called by either the gnc_plugin_history_list_changed function or
 *  the gnc_history_update_menus function.  It updates the specified
 *  file history item in the specified window.
 *
 *  This routine attaches the actual filename to the menu_item (via
 *  g_object_set_data) for later retrieval.  It also massages the
 *  filename so that it will display correctly in the menu, and also
 *  add a mnemonic for the menu item.
 *
 *  @param window A pointer to window whose file history should be
 *  updated.
 *
 *  @param index Update this item in the menu (base-0).
 *
 *  @param filename The new filename to associate with this menu item.
 */
static void
gnc_history_update_action (GncMainWindow *window,
			   gint index,
			   const gchar *filename)
{
	GtkActionGroup *action_group;
	GtkAction *action;
	gchar *action_name, *label_name, *old_filename;
	gint limit;

	ENTER("window %p, index %d, filename %s", window, index,
	      filename ? filename : "(null)");
	/* Get the action group */
	action_group =
	  gnc_main_window_get_action_group(window, PLUGIN_ACTIONS_NAME);

	action_name = g_strdup_printf("RecentFile%dAction", index);
	action = gtk_action_group_get_action (action_group, action_name);

	limit = gnc_gconf_get_int (HISTORY_STRING_SECTION,
				   HISTORY_STRING_MAXFILES,
				   NULL);

	if (filename && (strlen(filename) > 0) && (index < limit)) {
	  /* set the menu label (w/accelerator) */
	  label_name = gnc_history_generate_label(index, filename);
	  g_object_set(G_OBJECT(action), "label", label_name, "visible", TRUE, NULL);
	  g_free(label_name);

	  /* set the filename for the callback function */
	  old_filename = g_object_get_data(G_OBJECT(action), FILENAME_STRING);
	  if (old_filename)
	    g_free(old_filename);
	  g_object_set_data(G_OBJECT(action), FILENAME_STRING, g_strdup(filename));
	} else {
	  gtk_action_set_visible(action, FALSE);
	}
	g_free(action_name);
	LEAVE("");
}


/** Update the file history menu for a window.  This function walks
 *  the list of all possible gconf keys for the file history and
 *  forces a read/menu update on each key.  It should only be called
 *  once when the window is created.
 *
 *  @param window A pointer to the window whose file history menu
 *  should be updated.
 */
static void
gnc_history_update_menus (GncMainWindow *window)
{
	gchar *filename, *key;
	guint i;

	ENTER("");
	for (i = 0; i < MAX_HISTORY_FILES; i++) {
	  key = gnc_history_gconf_index_to_key(i);
	  filename = gnc_gconf_get_string(HISTORY_STRING_SECTION, key, NULL);
	  gnc_history_update_action(window, i, filename);
	  g_free(filename);
	  g_free(key);
	}
	LEAVE("");
}


/** Update an entry in the file history menu because a gconf entry
 *  changed.  This function is called whenever an item in the gconf
 *  history section is changed.  It is responsible for updating the
 *  menu item that corresponds to that key.
 *
 *  @param client A pointer to gconf client that noticed an entry
 *  change.
 *
 *  @param cnxn_id Unused.
 *
 *  @param entry A pointer to gconf entry that changed.
 *
 *  @param user_data A pointer to the window that this gconf client is
 *  associated with.
 */
static void
gnc_plugin_history_list_changed (GConfClient *client,
				 guint cnxn_id,
				 GConfEntry *entry,
				 gpointer user_data)
{
	GncMainWindow *window;
	GConfValue *value;
	const gchar *fullkey, *key, *filename;
	gint index;

	ENTER("");
	window = GNC_MAIN_WINDOW(user_data);

	fullkey = gconf_entry_get_key(entry);
	key = strrchr(fullkey, '/') + 1;
	if (strcmp(key, HISTORY_STRING_MAXFILES) == 0) {
	  gnc_history_update_menus (window);
	  LEAVE("updated maxfiles");
	  return;
	}
	index = gnc_history_gconf_key_to_index(key);
	if (index < 0) {
	  LEAVE("bad index");
	  return;
	}

	value = gconf_entry_get_value(entry);
	if (!value) {
	  LEAVE("No gconf value");
	  return;
	}
	filename = gconf_value_get_string(value);
	gnc_history_update_action (window, index, filename);

	gnc_main_window_actions_updated (window);
	LEAVE("");
}


/* This routine copies the gnucash 1.x file history list over to
 * gnucash 2.0. */
static void
gnc_plugin_history_list_from_gnucash1 (void)
{
  GKeyFile *keyfile;
  const gchar *home;
  gchar *mdi_file, *value;
  gchar **keys, **key, *new_key;
  gint file_id, max;

  /* First test if there are already files in the gconf file history.
   * If so, then bail out now. */
  value = gnc_gconf_get_string(HISTORY_STRING_SECTION, "file0", NULL);
  if (value) {
    g_free(value);
    return;
  }

  home = g_get_home_dir();
  if (!home)
    return;

  /* Copy the old values from the gnucash 1.x/gnome1 settings file to
   * the gnucash 2.x/gconf settings area.  */
  mdi_file = g_build_filename(home, ".gnome", "GnuCash", (gchar *)NULL);
  keyfile = gnc_key_file_load_from_file (mdi_file, FALSE, FALSE, NULL);
  if (keyfile) {
    keys = g_key_file_get_keys(keyfile, GNOME1_HISTORY, NULL, NULL);
    if (keys) {
      for (key = keys; *key; key++) {
	if (!strcmp(*key, GNOME1_MAXFILES)) {
	  max = g_key_file_get_integer(keyfile, GNOME1_HISTORY,
				       GNOME1_MAXFILES, NULL);
	  printf("Found old maxfiles: %d\n", max);
	  if ((max > 0) && (max < MAX_HISTORY_FILES))
	    printf("Setting maxfiles: %d\n\n", max);
	    gnc_gconf_set_int(HISTORY_STRING_SECTION, HISTORY_STRING_MAXFILES,
			      max, NULL);
	  continue;
	}

	if (sscanf(*key, "File%d", &file_id) == 1) {
	  value = g_key_file_get_string(keyfile, GNOME1_HISTORY, *key, NULL);
	  if (!value)
	    continue;
	  printf("Found old file %d: %s\n", file_id, value);
	  new_key = g_strdup_printf(HISTORY_STRING_FILE_N, file_id);
	  gnc_gconf_set_string (HISTORY_STRING_SECTION, new_key, value, NULL);
	  printf("Setting %s: %s\n\n", new_key, value);
	  g_free(new_key);
	  g_free(value);
	}
      }
      g_strfreev(keys);
    }
    g_key_file_free(keyfile);
  }

  g_free(mdi_file);
}

/************************************************************
 *                  Object Implementation                   *
 ************************************************************/

/*  Get the type of a file history plugin.  */
GType
gnc_plugin_file_history_get_type (void)
{
	static GType gnc_plugin_file_history_type = 0;

	if (gnc_plugin_file_history_type == 0) {
		static const GTypeInfo our_info = {
			sizeof (GncPluginFileHistoryClass),
			NULL,		/* base_init */
			NULL,		/* base_finalize */
			(GClassInitFunc) gnc_plugin_file_history_class_init,
			NULL,		/* class_finalize */
			NULL,		/* class_data */
			sizeof (GncPluginFileHistory),
			0,
			(GInstanceInitFunc) gnc_plugin_file_history_init
		};

		gnc_plugin_file_history_type =
		  g_type_register_static (GNC_TYPE_PLUGIN,
					  "GncPluginFileHistory",
					  &our_info, 0);
	}

	return gnc_plugin_file_history_type;
}


/** Initialize the file history plugin class. */
static void
gnc_plugin_file_history_class_init (GncPluginFileHistoryClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	GncPluginClass *plugin_class = GNC_PLUGIN_CLASS (klass);

	parent_class = g_type_class_peek_parent (klass);

	object_class->finalize = gnc_plugin_file_history_finalize;

	/* plugin info */
	plugin_class->plugin_name   = GNC_PLUGIN_FILE_HISTORY_NAME;

	/* function overrides */
	plugin_class->add_to_window = gnc_plugin_file_history_add_to_window;
	plugin_class->remove_from_window =
	  gnc_plugin_file_history_remove_from_window;

	/* widget addition/removal */
	plugin_class->actions_name  = PLUGIN_ACTIONS_NAME;
	plugin_class->actions       = gnc_plugin_actions;
	plugin_class->n_actions     = gnc_plugin_n_actions;
	plugin_class->ui_filename   = PLUGIN_UI_FILENAME;

	plugin_class->gconf_section = HISTORY_STRING_SECTION;
	plugin_class->gconf_notifications = gnc_plugin_history_list_changed;

	g_type_class_add_private(klass, sizeof(GncPluginFileHistoryPrivate));

	gnc_plugin_history_list_from_gnucash1();
}


/** Initialize an instance of the file history plugin. */
static void
gnc_plugin_file_history_init (GncPluginFileHistory *plugin)
{
	ENTER("plugin %p", plugin);
	LEAVE("");
}


/** Finalize an instance of the file history plugin. */
static void
gnc_plugin_file_history_finalize (GObject *object)
{
	GncPluginFileHistory *plugin;
	GncPluginFileHistoryPrivate *priv;

	g_return_if_fail (GNC_IS_PLUGIN_FILE_HISTORY (object));

	ENTER("plugin %p", object);
	plugin = GNC_PLUGIN_FILE_HISTORY (object);
	priv = GNC_PLUGIN_FILE_HISTORY_GET_PRIVATE (plugin);

	G_OBJECT_CLASS (parent_class)->finalize (object);
	LEAVE("");
}


/*  Create a new file history plugin.  This plugin attaches the file
 *  history menu to any window that is opened.
 */
GncPlugin *
gnc_plugin_file_history_new (void)
{
	GncPlugin *plugin_page = NULL;

	ENTER("");
	plugin_page = GNC_PLUGIN (g_object_new (GNC_TYPE_PLUGIN_FILE_HISTORY, NULL));
	LEAVE("plugin %p", plugin_page);
	return plugin_page;
}

/************************************************************
 *              Plugin Function Implementation              *
 ************************************************************/

/** Initialize the file history menu for a window.  This function is
 *  called as part of the initialization of a window, after all the
 *  plugin menu items have been added to the menu structure.  Its job
 *  is to correctly initialize the file history menu.  It does this by
 *  first calling a function that initializes the menu to the current
 *  as maintained in gconf.  It then creates a gconf client that will
 *  listens for any changes to the file history menu, and will update
 *  the menu when they are signaled.
 *
 *  @param plugin A pointer to the gnc-plugin object responsible for
 *  adding/removing the file history menu.
 *
 *  @param window A pointer the gnc-main-window that is being initialized.
 *
 *  @param type Unused
 */
static void
gnc_plugin_file_history_add_to_window (GncPlugin *plugin,
				       GncMainWindow *window,
				       GQuark type)
{
	gnc_history_update_menus(window);
}


/** Finalize the file history menu for this window.  This function is
 *  called as part of the destruction of a window.
 *
 *  @param plugin A pointer to the gnc-plugin object responsible for
 *  adding/removing the file history menu.  It stops the gconf
 *  notifications for this window, and destroys the gconf client
 *  object.
 *
 *  @param window A pointer the gnc-main-window that is being destroyed.
 *
 *  @param type Unused
 */
static void
gnc_plugin_file_history_remove_from_window (GncPlugin *plugin,
					    GncMainWindow *window,
					    GQuark type)
{
}

/************************************************************
 *                    Command Callbacks                     *
 ************************************************************/

/** The user has selected one of the items in the File History menu.
 *  Close down the current session and start up a new one with the
 *  requested file.
 *
 *  @param action A pointer to the action selected by the user.  This
 *  action represents one of the items in the file history menu.
 *
 *  @param data A pointer to the gnc-main-window data to be used by
 *  this function.  This is mainly to find out which window it was
 *  that had a menu selected.  That's not really important for this
 *  function and we're about to close all the windows anyway.
 */
static void
gnc_plugin_file_history_cmd_open_file (GtkAction *action,
				       GncMainWindowActionData *data)
{
	gchar *filename;

	g_return_if_fail(GTK_IS_ACTION(action));
	g_return_if_fail(data != NULL);

	/* DRH - Do we need to close all open windows but the first?
	 * Which progress bar should we be using? One in a window, or
	 * in a new "file loading" dialog???
	 */
	filename = g_object_get_data(G_OBJECT(action), FILENAME_STRING);
	gnc_window_set_progressbar_window (GNC_WINDOW(data->window));
	/* also opens new account page */
	if (!gnc_file_open_file (filename))
	  gnc_history_remove_file (filename);
	gnc_window_set_progressbar_window (NULL);
}

/** @} */
/** @} */
