/*
 * dialog-properties.h -- properties dialog
 * Copyright (C) 2019 Robert Fewell
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

/** @addtogroup Dialogs
    @{ */
/** @addtogroup PropDialog Properteies Dialog
    @{ */
/** @file dialog-properties.h
    @brief Dialog for handling Gnucash file properties.
    @author Copyright (c) 2019 Robert Fewell

    These functions are the external API available for the
    properties dialog. This dialog allows a user to modify
    several Gnucash properties.
    Any module may add a page (or partial page) of properties
    to the dialog.  These additions are done by providing
    the name of a glade file and the content to load from that
    file along with a widget in that file.  If a partial
    page is added, the widget name provided must be that of
    a GtkGrid containing four columns. If a full page is added,
    the widget name provided to this code can be any kind of
    widget, but for consistency it should probably be the same.

    The argument *is* a glade file, so if your code has special
    requirements (e.g. make one widget insensitive until another is
    selected) feel free to go ahead and add your own callbacks to the
    glade file.  This code will connect any callbacks that exist in
    the glade file.
*/

#ifndef GNC_DIALOG_PROPERTIES_H
#define GNC_DIALOG_PROPERTIES_H

#include <gtk/gtk.h>

/** This function adds a full page of properties to the properties
 *  dialog.  When the dialog is created, the specified widget will be
 *  pulled from the specified glade file and added to the properties
 *  dialog with the specified tab name.  The tab name may not be
 *  duplicated.  For example, the Business code might have a full page
 *  of its own properties.
 *
 *  @param filename The name of a glade file.
 *
 *  @param widgetname The name of the widget to extract from the glade file.
 *
 *  @param tabname The (translated!) name this page of properties should have in
 *  the dialog notebook. */
void gnc_properties_add_page (const gchar *filename,
                               const gchar *widgetname,
                               const gchar *tabname);


/** This function adds a partial page of properties to the
 *  properties dialog.  When the dialog is created, the specified
 *  widget will be pulled from the specified glade file and added to
 *  the properties dialog with the specified tab name.  The tab name
 *  may be duplicated.  For example, the HBCI properties may share a
 *  "Data Import" page with QIF and other methods.
 *
 *  @param filename The name of a glade file.
 *
 *  @param widgetname The name of the widget to extract from the glade file.
 *
 *  @param tabname The (translated!) name this page of properties should have in
 *  the dialog notebook. */
void gnc_properties_add_to_page (const gchar *filename,
                                  const gchar *widgetname,
                                  const gchar *tabname);


/** This function creates the properties dialog and presents it to
 *  the user.  The properties dialog is a singleton, so if a
 *  properties dialog already exists it will be raised to the top of
 *  the window stack instead of creating a new dialog. */
void gnc_properties_dialog (GtkWindow *parent);

#endif
/** @} */
/** @} */
