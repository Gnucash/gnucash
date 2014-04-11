/********************************************************************\
 * dialog-file-access.h -- dialog for opening a file or making a    *
 *                        connection to a libdbi database           *
 *                                                                  *
 * Copyright (C) 2009 Phil Longstaff (plongstaff@rogers.com)        *
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

#ifndef DIALOG_FILE_ACCESS_H
#define DIALOG_FILE_ACCESS_H

/** @addtogroup GUI
    @{ */
/** @file dialog-file-access.h
 *
 *  This file contains the functions to present a GUI to select
 *  a file or a database connection.  Separate functions exist for
 *  loading/open and for saving.
 */

void gnc_ui_file_access_for_open( void );
void gnc_ui_file_access_for_save_as( void );

/** @} */

#endif /* DIALOG_FILE_ACCESS_H */
