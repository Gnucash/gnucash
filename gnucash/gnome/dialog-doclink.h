/********************************************************************\
 * dialog-doclink.h -- Document links dialog                        *
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

#ifndef DIALOG_DOCLINK_H
#define DIALOG_DOCLINK_H

#ifdef __cplusplus
extern "C" {
#endif

/** Completion receives the resulting URI, which it owns and must free. */
typedef void (*GncDoclinkUriCallback) (GtkWindow *parent, gchar *uri,
                                       gpointer user_data);

/** Present the URI editor without starting a nested GTK main loop. The
 * callback receives the old URI on cancellation, an empty string on removal,
 * or the edited URI on acceptance. */
void gnc_doclink_get_uri_dialog_async (GtkWindow *parent, const gchar *title,
                                       const gchar *uri,
                                       GncDoclinkUriCallback callback,
                                       gpointer user_data);

/** Open the doclink uri.
 *
 *  A check is made for the uri being valid and then gnc_launch_doclink is used
 *
 *  @param parent The GtkWindow for the parent widget
 *  @param uri The doclink
 */
void gnc_doclink_open_uri (GtkWindow *parent, const gchar *uri);

/** Present a dialog to list all the Invoice linked documents.
 *
 *  A query is run to return all the invoice linked documents which
 *  are then added to a tree view. From this tree view the invoice
 *  and linked document can be opened along with a dialog to edit the
 *  document link.
 *
 *  @param parent The GtkWindow for the parent widget
 */
void gnc_doclink_business_dialog (GtkWindow *parent);

/** Present a dialog to list all the Transaction linked documents.
 *
 *  A query is run to return all the transaction linked documents which
 *  are then added to a tree view. From this tree view the transaction
 *  and linked document can be opened along with a dialog to edit the
 *  document link.
 *
 *  @param parent The GtkWindow for the parent widget
 */
void gnc_doclink_trans_dialog (GtkWindow *parent);

#ifdef __cplusplus
}
#endif

#endif
