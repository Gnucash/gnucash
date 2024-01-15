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

/** Sets the label text for displaying the path head in a dialog.
 *
 *  @param path_head_label The GtkLabel Widget
 *  @param incoming_path_head The starting common path head
 *  @param prefix A text string to place infront of the path head text
 */
void gnc_doclink_set_path_head_label (GtkWidget *path_head_label,
                                      const gchar *incoming_path_head,
                                      const gchar *prefix);

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
