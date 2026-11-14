/********************************************************************\
 * dialog-object-references.h -- dialog for displaying a list of    *
 *                               objects which refer to a specific  *
 *                               object                             *
 *                                                                  *
 * Copyright (C) 2010 Phil Longstaff (plongstaff@rogers.com)        *
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
 * You should have received a copy of the GNU General Public License *
 * along with this program; if not, see                            *
 * <https://www.gnu.org/licenses/>.                                *
\********************************************************************/

#ifndef DIALOG_OBJECT_REFERENCES_H
#define DIALOG_OBJECT_REFERENCES_H

#include <gtk/gtk.h>

G_BEGIN_DECLS

/** @addtogroup GUI
    @{ */
/** @file dialog-object-references.h
 *
 *  This file contains the functions to present a dialog box with a
 *  list of object references and an explanation that these objects
 *  must be modified to not refer to another specific object so that
 *  that object may be deleted.
 */

#define GNC_TYPE_OBJECT_REFERENCES_DIALOG (gnc_object_references_dialog_get_type ())
G_DECLARE_FINAL_TYPE (GncObjectReferencesDialog, gnc_object_references_dialog,
                      GNC, OBJECT_REFERENCES_DIALOG, GtkDialog)

/** Copy the display names of a borrowed list of QofInstance objects.
 * @param objects Objects in display order; neither list nor objects are retained.
 * @return An owned NULL-terminated string vector, freed with g_strfreev().
 */
GStrv gnc_object_references_get_display_names (const GList *objects);

void gnc_ui_object_references_show( const gchar* explanation, GList* objlist );

/** @} */

G_END_DECLS

#endif /* DIALOG_OBJECT_REFERENCES_H */
