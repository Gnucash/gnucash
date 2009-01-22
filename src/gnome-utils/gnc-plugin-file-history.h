/* 
 * gnc-plugin-file-history.h -- 
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
/** @file gnc-plugin-file-history.h
    @brief Functions providing the file history menu.
    @author Copyright (C) 2003,2005 David Hampton <hampton@employees.org>

    This plugin handles the file history information that appears in
    the application menus.
*/

#ifndef __GNC_PLUGIN_FILE_HISTORY_H
#define __GNC_PLUGIN_FILE_HISTORY_H

#include "gnc-plugin.h"

G_BEGIN_DECLS

/* type macros */
#define GNC_TYPE_PLUGIN_FILE_HISTORY            (gnc_plugin_file_history_get_type ())
#define GNC_PLUGIN_FILE_HISTORY(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_PLUGIN_FILE_HISTORY, GncPluginFileHistory))
#define GNC_PLUGIN_FILE_HISTORY_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_PLUGIN_FILE_HISTORY, GncPluginFileHistoryClass))
#define GNC_IS_PLUGIN_FILE_HISTORY(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_PLUGIN_FILE_HISTORY))
#define GNC_IS_PLUGIN_FILE_HISTORY_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_PLUGIN_FILE_HISTORY))
#define GNC_PLUGIN_FILE_HISTORY_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_PLUGIN_FILE_HISTORY, GncPluginFileHistoryClass))

#define GNC_PLUGIN_FILE_HISTORY_NAME "gnc-plugin-file-history"
#define MAX_HISTORY_FILES 10	/* May be any number up to 10 */
#define HISTORY_STRING_SECTION  "history"
#define HISTORY_STRING_MAXFILES "maxfiles"
#define HISTORY_STRING_FILE_N   "file%d"

/* typedefs & structures */

/** The instance data structure for a file history plugin. */
typedef struct {
	GncPlugin gnc_plugin;
} GncPluginFileHistory;


/** The class data structure for a file history plugin. */
typedef struct {
	GncPluginClass gnc_plugin;
} GncPluginFileHistoryClass;

/* function prototypes */


/** Get the type of a file history plugin.
 *
 *  @return A GType.
 */
GType gnc_plugin_file_history_get_type (void);


/** Create a new file history plugin.  This plugin attaches the file
 *  history menu to any window that is opened.
 *
 *  @return A pointer to the new object.
 */
GncPlugin *gnc_plugin_file_history_new (void);


/** Add a file name to the front of the file "history list".  If the
 *  name already exist on the list, then it is moved from its current
 *  location to the front of the list.  The "list" is actually a
 *  sequence of up to ten gconf keys.
 *
 *  @param filename The name of the file to add to the list.
 */
void gnc_history_add_file (const char *filename);

/** Remove all occurences of a file name from the history list.  Move
 *  the other key values up in the list to fill the gaps.
 *
 *  @param oldfile The name of the file to remove from the list.
 */
void gnc_history_remove_file (const char *oldfile);


/** Retrieve the name of the file most recently accessed.  This is the
 *  name at the front of the list.  Since the "list" is actually a
 *  sequence of up to ten gconf keys, this is the value of key zero.
 *
 *  @return This function returns an allocated string containing the
 *  name of the most recently accessed file.  The caller is
 *  responsible for freeing this string.
 */
char * gnc_history_get_last (void);

G_END_DECLS

#endif /* __GNC_PLUGIN_FILE_HISTORY_H */

/** @} */
/** @} */
