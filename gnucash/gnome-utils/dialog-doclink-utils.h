/********************************************************************\
 * dialog-doclink-utils.h -- Document link dialog Utils             *
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

#ifndef DIALOG_DOCLINK_UTILS_H
#define DIALOG_DOCLINK_UTILS_H

#ifdef __cplusplus
extern "C" {
#endif

#define GNC_DOC_LINK_PATH_HEAD "assoc-head"
/* Note, assoc-head is the old name for the document link head which has been
   kept for compatibility */


/** Return the current documentation-link file path head uri.
 *
 *  This function will get the current documentation-link path head from prefs.
 *  If it is not set then a default path head is set based on either
 *  the home directory or the user data directory.
 *
 *  The calling function should free the returned value with g_free when
 *  the it is no longer needed.
 *
 *  @return The current documentation-link file path head.
 */
gchar * gnc_doclink_get_path_head (void);

/** Sets the label text for displaying the path head in a dialog.
 *
 *  @param path_head_label The GtkLabel Widget
 *  @param incoming_path_head The starting common path head
 *  @param prefix A text string to place infront of the path head text
 */
void gnc_doclink_set_path_head_label (GtkWidget *path_head_label,
                                      const gchar *incoming_path_head,
                                      const gchar *prefix);

/** Return a uri that can be used for opening it.
 *
 *  The function allocates memory for the uri. The calling function should
 *  free this memory with g_free when uri is no longer needed.
 *
 *  If the uri scheme is 'file' or NULL, an absolute path is created and returned
 *  otherwise the uri is returned.
 *
 *  @param path_head The starting common path head
 *  @param uri The document link URI
 *  @param uri_scheme http:, https:, or file:
 *
 *  @return The uri used for opening the link.
 */
gchar * gnc_doclink_get_use_uri (const gchar *path_head, const gchar *uri, gchar *uri_scheme);

/** Corrects an earlier relative file documentation link uri forrmat.
 *
 *  Prior to version 3.5, relative paths were stored starting as 'file:'
 *  or 'file:/' depending on OS. This function changes them so that
 *  relative paths are stored without a leading "/" and in native form.
 *
 *  @param trans The Transaction holding the document link
 *  @param book_ro TRUE if the book is read only
 */
gchar * gnc_doclink_convert_trans_link_uri (gpointer trans, gboolean book_ro);

/** Return an unescaped uri for display use.
 *
 *  The function allocates memory for the uri. The calling function should
 *  free this memory with g_free when the unescaped uri is no longer needed.

 *  Return an unescaped uri for displaying and if OS is windows change the
 *  '/' to '\' to look like a traditional windows path
 *
 *  @param path_head The starting common path head
 *  @param uri The document link
 *  @param uri_scheme
 *
 *  @return The unescaped uri used for display purposes.
 */
gchar * gnc_doclink_get_unescape_uri (const gchar *path_head, const gchar *uri, gchar *uri_scheme);

/** Return an unescaped uri for display use just based on the uri.
 *
 *  The function allocates memory for the uri. The calling function should
 *  free this memory with g_free when the unescaped uri is no longer needed.

 *  Return an unescaped uri for displaying and if OS is windows change the
 *  '/' to '\' to look like a traditional windows path
 *
 *  @param uri The document link
 *
 *  @return The unescaped uri used for display purposes.
 */
gchar * gnc_doclink_get_unescaped_just_uri (const gchar *uri);

/** Presents a dialog when the path head is changed.
 *
 *  When the path head is changed a dialog is raised that allows for
 *  existing relative file document links to be made absolute based on the
 *  old_path_head_uri and existing absolute file document links to be made
 *  relative based on the new_path_head_uri.
 *
 *  @param parent The GtkWindow for the parent widget
 *  @param old_path_head_uri The old path head uri
 */
void gnc_doclink_pref_path_head_changed (GtkWindow *parent, const gchar *old_path_head_uri);

#ifdef __cplusplus
}
#endif

#endif
