/*
 * dialog-preferences.h -- preferences dialog
 * Copyright (C) 2005 David Hampton
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
/** @addtogroup PrefDialog Preferences Dialog
    @{ */
/** @file dialog-preferences.h
    @brief Dialog for handling user preferences.
    @author Copyright (c) 2005 David Hampton <hampton@employees.org>

    These functions are the external API available for the user
    preference dialog. This dialog allows a user to modify
    several user preferences in the gnucash preferences database.
    Any module may add a page (or partial page) of preferences
    to the dialog.  These additions are done by providing
    the name of a glade file and the content to load from that
    file along with a widget in that file.  If a partial
    page is added, the widget name provided must be that of
    a GtkTable containing four columns. If a full page is added,
    the widget name provided to this code can be any kind of
    widget, but for consistency it should probably be the same.

    If a widget name is in the form pref/aaa.bbb/ccc... and it is a type
    of widget this code knows how to handle, then the widget is bound
    to the preference named ccc in group aaa.bbb. This means that if
    the widget's value changes, the preference is automatically updated.
    The same goes the other way around. This code currently knows about
    font buttons, radio buttons, check buttons, spin boxes, combo boxes,
    gnucash currency select widgets, gnucash accounting period widgets,
    and a gnucash date edit widget. (Combo boxes should not be used for
    less than five choices. Use a radio button group instead.)

    The argument *is* a glade file, so if your code has special
    requirements (e.g. make one widget insensitive until another is
    selected) feel free to go ahead and add your own callbacks to the
    glade file.  This code will connect any callbacks that exist in
    the glade file.
*/

#ifndef GNC_DIALOG_PREFERENCES_H
#define GNC_DIALOG_PREFERENCES_H

/** This function adds a full page of preferences to the preferences
 *  dialog.  When the dialog is created, the specified widget will be
 *  pulled from the specified glade file and added to the preferences
 *  dialog with the specified tab name.  The tab name may not be
 *  duplicated.  For example, the Business code might have a full page
 *  of its own preferences.
 *
 *  @param filename The name of a glade file.
 *
 *  @param widgetname The name of the widget to extract from the glade file.
 *
 *  @param tabname The (translated!) name this page of preferences should have in
 *  the dialog notebook. */
void gnc_preferences_add_page (const gchar *filename,
                               const gchar *widgetname,
                               const gchar *tabname);


/** This function adds a partial page of preferences to the
 *  preferences dialog.  When the dialog is created, the specified
 *  widget will be pulled from the specified glade file and added to
 *  the preferences dialog with the specified tab name.  The tab name
 *  may be duplicated.  For example, the HBCI preferences may share a
 *  "Data Import" page with QIF and other methods.
 *
 *  @param filename The name of a glade file.
 *
 *  @param widgetname The name of the widget to extract from the glade file.
 *
 *  @param tabname The (translated!) name this page of preferences should have in
 *  the dialog notebook. */
void gnc_preferences_add_to_page (const gchar *filename,
                                  const gchar *widgetname,
                                  const gchar *tabname);


/** This function creates the preferences dialog and presents it to
 *  the user.  The preferences dialog is a singleton, so if a
 *  preferences dialog already exists it will be raised to the top of
 *  the window stack instead of creating a new dialog. */
void gnc_preferences_dialog (void);

#endif
/** @} */
/** @} */
