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

#include <config.h>

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
#include "gnc-prefs.h"
#include "gnc-uri-utils.h"
#include "gnc-gtk-utils.h"

static GObjectClass *parent_class = NULL;

#define FILENAME_STRING "filename"
#define MAX_HISTORY_FILES 10    /* May be any number up to 10 */
#define GNC_PREFS_GROUP_HISTORY   "history"
#define GNC_PREF_HISTORY_MAXFILES "maxfiles"
#define HISTORY_STRING_FILE_N   "file%d"

static void gnc_plugin_file_history_class_init (GncPluginFileHistoryClass *klass);
static void gnc_plugin_file_history_init (GncPluginFileHistory *plugin);
static void gnc_plugin_file_history_finalize (GObject *object);

static void gnc_plugin_file_history_add_to_window (GncPlugin *plugin, GncMainWindow *window, GQuark type);
static void gnc_plugin_file_history_remove_from_window (GncPlugin *plugin, GncMainWindow *window, GQuark type);


/** The debugging module used by this file. */
static QofLogModule log_module = GNC_MOD_GUI;

/* Command callbacks */
static void gnc_plugin_file_history_cmd_open_file (GSimpleAction *simple, GVariant *parameter, gpointer user_data);

/** The label given to the main window for this plugin. */
#define PLUGIN_ACTIONS_NAME "gnc-plugin-file-history-actions"
/** The name of the UI description file for this plugin. */
#define PLUGIN_UI_FILENAME  "gnc-plugin-file-history.ui"

#define GNOME1_HISTORY "History"
#define GNOME1_MAXFILES "MaxFiles"

/** A placeholder set of actions that are filled in by this plugin.
 *  As the user opens files, the names and visibility of these actions
 *  will be updated to reflect the users recent choices.  This list is
 *  limited to ten actions, although there may be a smaller limit set
 *  by the user.  The typical limit is four. */
static GActionEntry gnc_plugin_actions [] =
{
    { "RecentFile0Action", gnc_plugin_file_history_cmd_open_file, NULL, NULL, NULL },
    { "RecentFile1Action", gnc_plugin_file_history_cmd_open_file, NULL, NULL, NULL },
    { "RecentFile2Action", gnc_plugin_file_history_cmd_open_file, NULL, NULL, NULL },
    { "RecentFile3Action", gnc_plugin_file_history_cmd_open_file, NULL, NULL, NULL },
    { "RecentFile4Action", gnc_plugin_file_history_cmd_open_file, NULL, NULL, NULL },
    { "RecentFile5Action", gnc_plugin_file_history_cmd_open_file, NULL, NULL, NULL },
    { "RecentFile6Action", gnc_plugin_file_history_cmd_open_file, NULL, NULL, NULL },
    { "RecentFile7Action", gnc_plugin_file_history_cmd_open_file, NULL, NULL, NULL },
    { "RecentFile8Action", gnc_plugin_file_history_cmd_open_file, NULL, NULL, NULL },
    { "RecentFile9Action", gnc_plugin_file_history_cmd_open_file, NULL, NULL, NULL },
};
/** The number of actions provided by this plugin. */
static guint gnc_plugin_n_actions = G_N_ELEMENTS(gnc_plugin_actions);

/** The default menu items that need to be add to the menu */
static const gchar *gnc_plugin_load_ui_items [] =
{
    NULL,
};

/** The instance private data for a file history plugin.  This data
 *  structure is unused. */
typedef struct GncPluginFileHistoryPrivate
{
    gpointer dummy;
} GncPluginFileHistoryPrivate;


#define GNC_PLUGIN_FILE_HISTORY_GET_PRIVATE(o)  \
   ((GncPluginFileHistoryPrivate*)gnc_plugin_file_history_get_instance_private((GncPluginFileHistory*)o))

/************************************************************
 *                     Other Functions                      *
 ************************************************************/

/** Convert an array index into a preference name.
 *
 *  @param index An index number that can be used with the
 *  gnc_plugin_actions array.
 *
 *  @return The preference name associated with this array entry. It
 *  is the callers responsibility to free this string when
 *  finished.  */
static gchar *
gnc_history_index_to_pref_name (guint index)
{
    return g_strdup_printf(HISTORY_STRING_FILE_N, index);
}


/** Convert a preference name into an array index.  This function
 *  uses sscanf to pull the number off the end of the key and convert
 *  it to an integer.
 *
 *  @param key The preference name.
 *
 *  @return An index number that can be used with the
 *  gnc_plugin_actions array. */
static gint
gnc_history_pref_name_to_index (const gchar *pref)
{
    gint index, result;

    result = sscanf(pref, HISTORY_STRING_FILE_N, &index);
    if (result != 1)
        return -1;
    if ((index < 0) || (index >= gnc_plugin_n_actions))
        return -1;
    return index;
}


/*  Add a file name to the front of the file "history list".  If the
 *  name already exist on the list, then it is moved from its current
 *  location to the front of the list.  The "list" is actually a
 *  sequence of up to ten preferences.
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
     * Look for the filename in preferences.
     */
    last = MAX_HISTORY_FILES - 1;
    for (i = 0; i < MAX_HISTORY_FILES; i++)
    {
        from = gnc_history_index_to_pref_name(i);
        filename = gnc_prefs_get_string(GNC_PREFS_GROUP_HISTORY, from);
        g_free(from);

        if (!filename)
        {
            last = i;
            break;
        }
        if (g_utf8_collate(newfile, filename) == 0)
        {
            g_free(filename);
            last = i;
            break;
        }
        g_free(filename);
    }

    /*
     * Shuffle filenames upward through preferences.
     */
    to = gnc_history_index_to_pref_name(last);
    for (i = last - 1; i >= 0; i--)
    {
        from = gnc_history_index_to_pref_name(i);
        filename = gnc_prefs_get_string(GNC_PREFS_GROUP_HISTORY, from);
        if (filename && *filename)
        {
            gnc_prefs_set_string(GNC_PREFS_GROUP_HISTORY, to, filename);
        }
        else
        {
            gnc_prefs_reset(GNC_PREFS_GROUP_HISTORY, to);
        }
        g_free(filename);
        g_free(to);
        to = from;
    }

    /*
     * Store the new zero entry.
     */
    gnc_prefs_set_string(GNC_PREFS_GROUP_HISTORY, to, newfile);
    g_free(to);
}


/** Remove all occurrences of a file name from the history list.  Move
 *  the other file names up in the list to fill the gaps.
 *
 *  @param oldfile The name of the file to remove from the list.
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

    for (i = 0, j = 0; i < MAX_HISTORY_FILES; i++)
    {
        from = gnc_history_index_to_pref_name(i);
        filename = gnc_prefs_get_string(GNC_PREFS_GROUP_HISTORY, from);

        if (filename)
        {
            if (g_utf8_collate(oldfile, filename) == 0)
            {
                gnc_prefs_reset(GNC_PREFS_GROUP_HISTORY, from);
            }
            else
            {
                if (i != j)
                {
                    to = gnc_history_index_to_pref_name(j);
                    gnc_prefs_set_string(GNC_PREFS_GROUP_HISTORY, to, filename);
                    gnc_prefs_reset(GNC_PREFS_GROUP_HISTORY, from);
                    g_free(to);
                }
                j++;
            }
            g_free (filename);
        }
        g_free(from);
    }
}


/** Test for a file name existing in the history list.
 *
 *  @param oldfile The name of the file to test for in the list.
 */
gboolean gnc_history_test_for_file (const char *oldfile)
{
    gchar *filename, *from;
    gint i;
    gboolean found = FALSE;

    if (!oldfile)
        return FALSE;
    if (!g_utf8_validate(oldfile, -1, NULL))
        return FALSE;

    for (i = 0; i < MAX_HISTORY_FILES; i++)
    {
        from = gnc_history_index_to_pref_name(i);
        filename = gnc_prefs_get_string(GNC_PREFS_GROUP_HISTORY, from);
        g_free(from);

        if (!filename)
            continue;

        if (g_utf8_collate(oldfile, filename) == 0)
        {
            found = TRUE;
            g_free (filename);
            break;
        }
        g_free (filename);
    }

    return found;
}


/*  Retrieve the name of the file most recently accessed.  This is the
 *  name at the front of the list.  Since the "list" is actually a
 *  sequence of up to ten preference names, this is the value of the first preference.
 */
char *
gnc_history_get_last (void)
{
    char *filename, *pref;

    pref = gnc_history_index_to_pref_name(0);
    filename = gnc_prefs_get_string(GNC_PREFS_GROUP_HISTORY, pref);
    g_free(pref);

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
    gchar *label, *result;
    gchar **splitlabel;

    if (gnc_uri_targets_local_fs (filename))
    {
        /* for file paths, only display the file name */
        gchar *filepath = gnc_uri_get_path ( filename );
        label = g_path_get_basename ( filepath );
        g_free ( filepath );
    }
    else
    {
        /* for databases, display the full uri, except for the password */
        label = gnc_uri_normalize_uri ( filename, FALSE );
    }

    /* Escape '_' characters */
    splitlabel = g_strsplit ( label, "_", 0);
    g_free (label);
    label = g_strjoinv ( "__", splitlabel);
    g_strfreev (splitlabel);

    result = g_strdup_printf ( "_%d %s", (index + 1) % 10, label);
    g_free ( label );
    return result;

}

/** This routine takes a filename and modifies it so that can be
 *  used as a tooltip for a GtkLabel.  For true filenames it just
 *  returns the file name. For database backed data files the
 *  password will be stripped from the uri.
 *
 *  @param filename A pointer to the filename to mangle.
 *
 *  @return A pointer to the mangled filename.  The Caller is
 *  responsible for freeing this memory.
 */
static gchar *
gnc_history_generate_tooltip (int index, const gchar *filename)
{

    if (gnc_uri_targets_local_fs (filename))
        /* for file paths, display the full file path */
        return gnc_uri_get_path ( filename );
    else
        /* for databases, display the full uri, except for the password */
        return gnc_uri_normalize_uri ( filename, FALSE );

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
    GncMenuModelSearch *gsm = g_new0 (GncMenuModelSearch, 1);
    gchar *action_name;
    gint limit;
    gboolean add_item = FALSE;
    gint pos;

    ENTER("window %p, index %d, filename %s", window, index,
          filename ? filename : "(null)");

    action_name = g_strdup_printf ("RecentFile%dAction", index);

    gsm->search_action_label = NULL;
    gsm->search_action_name = action_name;

    if (!gnc_menubar_model_find_item (gnc_main_window_get_menu_model(window), gsm)) // could not find action_name
    {
        add_item = TRUE;
        gsm->search_action_name = "FilePlaceholder6"; // placeholder

        if (!gnc_menubar_model_find_item (gnc_main_window_get_menu_model(window), gsm))
        {
            LEAVE("Could not find 'menu_item' with action name '%s'", action_name);
            g_free (gsm);
            g_free (action_name);
            return;
        }
        else
            pos = gsm->index + index;
    }
    else
        pos = gsm->index;

    limit = gnc_prefs_get_int (GNC_PREFS_GROUP_HISTORY,
                               GNC_PREF_HISTORY_MAXFILES);

    if (filename && (strlen(filename) > 0) && (index < limit))
    {
        GMenuItem *item;
        gchar *label_name = gnc_history_generate_label (index, filename);
        gchar *tooltip = gnc_history_generate_tooltip (index, filename);
        gchar *full_action_name = g_strconcat (PLUGIN_ACTIONS_NAME, ".",
                                               action_name, NULL);

        item = g_menu_item_new (label_name, full_action_name);

        g_menu_item_set_attribute (item, GNC_MENU_ATTRIBUTE_TOOLTIP, "s", tooltip);

        if (!add_item)
            g_menu_remove (G_MENU(gsm->model), pos);

        g_menu_insert_item (G_MENU(gsm->model), pos, item);

        g_free (full_action_name);
        g_free (label_name);
        g_free (tooltip);
    }
    g_free (gsm);
    g_free (action_name);
    LEAVE("");
}


/** Update the file history menu for a window.  This function walks
 *  the list of all possible preference names for the file history and
 *  forces a read/menu update on each preference.  It should only be called
 *  once when the window is created.
 *
 *  @param window A pointer to the window whose file history menu
 *  should be updated.
 */
static void
gnc_history_update_menus (GncMainWindow *window)
{
    gchar *filename, *pref;
    guint i;

    ENTER("");

    for (i = 0; i < MAX_HISTORY_FILES; i++)
    {
        pref = gnc_history_index_to_pref_name(i);
        filename = gnc_prefs_get_string(GNC_PREFS_GROUP_HISTORY, pref);
        gnc_history_update_action(window, i, filename);
        g_free(filename);
        g_free(pref);
    }
    LEAVE("");
}


/** Update an entry in the file history menu because a preference
 *  changed.  This function is called whenever an item in the preferences
 *  history group is changed.  It is responsible for updating the
 *  menu item that corresponds to that key.
 *
 *  @param prefs Unused.
 *
 *  @param pref The name of the preference that changed.
 *
 *  @param user_data A pointer to the window that notice the preference change.
 */
static void
gnc_plugin_history_list_changed (gpointer prefs,
                                 gchar *pref,
                                 gpointer user_data)
{
    GncMainWindow *window;
    gchar *filename;
    gint index;

    ENTER("");
    window = GNC_MAIN_WINDOW(user_data);

    if (strcmp(pref, GNC_PREF_HISTORY_MAXFILES) == 0)
    {
        gnc_history_update_menus (window);
        LEAVE("updated maxfiles");
        return;
    }
    index = gnc_history_pref_name_to_index(pref);
    if (index < 0)
    {
        LEAVE("bad index");
        return;
    }

    filename = gnc_prefs_get_string (GNC_PREFS_GROUP_HISTORY, pref);
    gnc_history_update_action (window, index, filename);
    g_free (filename);

    LEAVE("");
}

/************************************************************
 *                  Object Implementation                   *
 ************************************************************/

/** Initialize the file history plugin class. */
static void
gnc_plugin_file_history_class_init (GncPluginFileHistoryClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS (klass);
    GncPluginClass *plugin_class = GNC_PLUGIN_CLASS (klass);

    parent_class = g_type_class_peek_parent (klass);

    object_class->finalize = gnc_plugin_file_history_finalize;

    /* plugin info */
    plugin_class->plugin_name = GNC_PLUGIN_FILE_HISTORY_NAME;

    /* function overrides */
    plugin_class->add_to_window = gnc_plugin_file_history_add_to_window;
    plugin_class->remove_from_window = gnc_plugin_file_history_remove_from_window;

    /* widget addition/removal */
    plugin_class->actions_name  = PLUGIN_ACTIONS_NAME;
    plugin_class->actions       = gnc_plugin_actions;
    plugin_class->n_actions     = gnc_plugin_n_actions;
    plugin_class->ui_filename   = PLUGIN_UI_FILENAME;
    plugin_class->ui_updates    = gnc_plugin_load_ui_items;
}

G_DEFINE_TYPE_WITH_PRIVATE(GncPluginFileHistory, gnc_plugin_file_history, GNC_TYPE_PLUGIN)

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
    g_return_if_fail (GNC_IS_PLUGIN_FILE_HISTORY (object));

    ENTER("plugin %p", object);
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
 *  as maintained in the preferences database.  It will then
 *  listens for any changes to the history preferences, and will update
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
    gnc_prefs_register_cb (GNC_PREFS_GROUP_HISTORY, NULL,
                           gnc_plugin_history_list_changed, window);
    gnc_history_update_menus(window);
}


/** Finalize the file history menu for this window.  This function is
 *  called as part of the destruction of a window.
 *
 *  @param plugin A pointer to the gnc-plugin object responsible for
 *  adding/removing the file history menu.  It stops listening for
 *  changes in the history preferences.
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
    gnc_prefs_remove_cb_by_func (GNC_PREFS_GROUP_HISTORY, NULL,
                                 gnc_plugin_history_list_changed, window);
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
gnc_plugin_file_history_cmd_open_file (GSimpleAction *simple,
                                       GVariant      *parameter,
                                       gpointer       user_data)

{
    GncMainWindowActionData *data = user_data;
    gchar *filename, *pref, *index;
    const gchar *action_name;

    g_return_if_fail (G_IS_SIMPLE_ACTION(simple));
    g_return_if_fail (data != NULL);

    // action name will be of the form 'RecentFile1Action'
    action_name =  g_action_get_name (G_ACTION(simple));

    index = g_utf8_substring (action_name, 10, 11);

    pref = gnc_history_index_to_pref_name (atoi (index));
    filename = gnc_prefs_get_string (GNC_PREFS_GROUP_HISTORY, pref);

    PINFO("File to open is '%s' on action '%s'", filename, action_name);

    gnc_window_set_progressbar_window (GNC_WINDOW(data->window));
    /* also opens new account page */
    gnc_file_open_file (GTK_WINDOW (data->window),
                        filename, /*open_readonly*/ FALSE);
    gnc_window_set_progressbar_window (NULL);

    g_free (pref);
    g_free (filename);
    g_free (index);
}

/** @} */
/** @} */
