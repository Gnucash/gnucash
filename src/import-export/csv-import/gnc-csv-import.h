/********************************************************************\
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
/** @file
    @brief CSV import GUI
    *
    gnc-csv-import.h
    @author Copyright (c) 2007 Benny Sperisen <lasindi@gmail.com>
*/
#ifndef CSV_IMPORT_H
#define CSV_IMPORT_H

/** The gnc_file_csv_import() will let the user select a
 * CSV/Fixed-Width file to open, select an account to import it to,
 * and import the transactions into the account. It also allows the
 * user to configure how the file is parsed. */
void              gnc_file_csv_import (void);
#endif
