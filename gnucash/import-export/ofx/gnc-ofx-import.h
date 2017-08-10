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
    @brief Ofx import module interface
    *
    gnc-ofx-import.h
    @author Copyright (c) 2002 Benoit Grégoire <bock@step.polymtl.ca>
*/
#ifndef OFX_IMPORT_H
#define OFX_IMPORT_H

/** The gnc_file_ofx_import() routine will pop up a standard file
 *     selection dialogue asking the user to pick a OFX/QFX file. If one
 *     is selected the the OFX file is opened and read. It's contents
 *     are merged into the existing session (if any). The current
 *     session continues to remain open for editing. */
void              gnc_file_ofx_import (void);
#endif
