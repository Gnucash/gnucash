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
#include "gnc-prefs.h"
#include "gnc-uri-utils.h"

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
static GtkActionEntry gnc_plugin_actions [] =
{
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
        if (filename)
        {
            gnc_prefs_set_string(GNC_PREFS_GROUP_HISTORY, to, filename);
            g_free(filename);
        }
        else
        {
            gnc_prefs_reset(GNC_PREFS_GROUP_HISTORY, to);
        }
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
        }
        g_free(from);
    }
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

    if ( gnc_uri_is_file_uri ( filename ) )
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

    if ( gnc_uri_is_file_uri ( filename ) )
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
    GtkActionGroup *action_group;
    GtkAction *action;
    gchar *action_name, *label_name, *tooltip, *old_filename;
    gint limit;

    ENTER("window %p, index %d, filename %s", window, index,
          filename ? filename : "(null)");
    /* Get the action group */
    action_group =
        gnc_main_window_get_action_group(window, PLUGIN_ACTIONS_NAME);

    action_name = g_strdup_printf("RecentFile%dAction", index);
    action = gtk_action_group_get_action (action_group, action_name);

    limit = gnc_prefs_get_int (GNC_PREFS_GROUP_HISTORY,
                               GNC_PREF_HISTORY_MAXFILES);

    if (filename && (strlen(filename) > 0) && (index < limit))
    {
        /* set the menu label (w/accelerator) */
        label_name = gnc_history_generate_label(index, filename);
        tooltip    = gnc_history_generate_tooltip(index, filename);
        g_object_set(G_OBJECT(action), "label", label_name,
                                       "tooltip", tooltip,
                                       "visible", TRUE,
                                       NULL);
        g_free(label_name);
        g_free(tooltip);

        /* set the filename for the callback function */
        old_filename = g_object_get_data(G_OBJECT(action), FILENAME_STRING);
        if (old_filename)
            g_free(old_filename);
        g_object_set_data(G_OBJECT(action), FILENAME_STRING, g_strdup(filename));
    }
    else
    {
        gtk_action_set_visible(action, FALSE);
    }
    g_free(action_name);
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
    const gchar *filename;
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

    gnc_main_window_actions_updated (window);
    LEAVE("");
}

/************************************************************
 *                  Object Implementation                   *
 ************************************************************/

/*  Get the type of a file history plugin.  */
GType
gnc_plugin_file_history_get_type (void)
{
    static GType gnc_plugin_file_history_type = 0;

    if (gnc_plugin_file_history_type == 0)
    {
        static const GTypeInfo our_info =
        {
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

    g_type_class_add_private(klass, sizeof(GncPluginFileHistoryPrivate));
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
    gnc_file_open_file (filename, /*open_readonly*/ FALSE);
    gnc_window_set_progressbar_window (NULL);
}

/** @} */
/** @} */
