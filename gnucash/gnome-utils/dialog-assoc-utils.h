/********************************************************************\
 * dialog-assoc-utils.h -- Associations dialog Utils                *
 * Copyright (C) 2020 Robert Fewell                                 *
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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
\********************************************************************/

#ifndef DIALOG_ASSOC_UTILS_H
#define DIALOG_ASSOC_UTILS_H

/** Return the current associate path head uri.
 *
 *  This function will get the current associate path head from pref's.
 *  If it is not set then a default path head is set based on either
 *  the home directory or the user data directory.
 *
 *  The calling function should free the returned value with g_free when
 *  the it is no longer needed.
 *
 *  @return The current associate path head.
 */
gchar * gnc_assoc_get_path_head (void);

/** Return an unescaped uri for display use.
 *
 *  The function allocates memory for the uri. The calling function should
 *  free this memory with g_free when the unescaped uri is no longer needed.

 *  Return an unesacped uri for displaying and if OS is windows change the
 *  '/' to '\' to look like a traditional windows path
 *
 *  @param path_head The starting common path head
 *  @param uri The association
 *  @param uri_scheme
 *
 *  @return The unescaped uri used for display purposes.
 */
gchar * gnc_assoc_get_unescape_uri (const gchar *path_head, const gchar *uri, gchar *uri_scheme);

/** Presents a dialog when the path head is changed.
 *
 *  When the path head is changed a dialog is raised that allows for
 *  existing relative file associations to be made absolute based on the
 *  old_path_head_uri and existing absolute file associations to be made
 *  relative based on the new_path_head_uri.
 *
 *  @param parent The GtkWindow for the parent widget
 *  @param old_path_head_uri The old path head uri
 */
void gnc_assoc_pref_path_head_changed (GtkWindow *parent, const gchar *old_path_head_uri);

#endif
