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
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
\********************************************************************/

#ifndef DIALOG_OBJECT_REFERENCES_H
#define DIALOG_OBJECT_REFERENCES_H

/** @addtogroup GUI
    @{ */
/** @file dialog-object-references.h
 *
 *  This file contains the functions to present a dialog box with a
 *  list of object references and an explanation that these objects
 *  must be modified to not refer to another specific object so that
 *  that object may be deleted.
 */

void gnc_ui_object_references_show( const gchar* explanation, GList* objlist );

/** @} */

#endif /* DIALOG_OBJECT_REFERENCES_H */
