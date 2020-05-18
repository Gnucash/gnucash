/********************************************************************\
 * dialog-assoc.h -- Associations dialog                            *
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

#ifndef DIALOG_ASSOC_H
#define DIALOG_ASSOC_H

/** Present the right edit dialog for the uri.
 *
 *  The function allocates memory for the uri. The calling function should
 *  free this memory with g_free when uri is no longer needed.
 *
 *  @param parent The GtkWindow for the parent widget
 *  @param title The dialog title to be used for the dialog
 *  @param uri The old uri to be ammended in the dialog
 *
 *  @return The ammeded uri, can be NULL if deletion required.
 */
gchar * gnc_assoc_get_uri_dialog (GtkWindow *parent, const gchar *title, const gchar *uri);

/** Open the association uri.
 *
 *  A check is made for the uri being valid and then gnc_launch_assoc is used
 *
 *  @param parent The GtkWindow for the parent widget
 *  @param uri The association
 */
void gnc_assoc_open_uri (GtkWindow *parent, const gchar *uri);

/** Present a dialog to list all the Invoice associations.
 *
 *  A query is run to return all the invoice associations which
 *  are then added to a tree view. From this tree view the invoice
 *  and association can be opened along with a dialog to edit the
 *  association.
 *
 *  @param parent The GtkWindow for the parent widget
 */
void gnc_assoc_business_dialog (GtkWindow *parent);

/** Present a dialog to list all the Transaction associations.
 *
 *  A query is run to return all the transaction associations which
 *  are then added to a tree view. From this tree view the transaction
 *  and association can be opened along with a dialog to edit the
 *  association.
 *
 *  @param parent The GtkWindow for the parent widget
 */
void gnc_assoc_trans_dialog (GtkWindow *parent);

#endif
