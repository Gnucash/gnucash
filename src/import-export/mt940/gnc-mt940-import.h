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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
\********************************************************************/
 /** @file
     @brief mt940 import module interface
     *
     gnc-mt940-import.h
     @author Copyright (c) 2002 Benoit Grégoire <bock@step.polymtl.ca>
 */
#ifndef MT940_IMPORT_H
#define MT940_IMPORT_H

/** The gnc_file_mt940_import() routine will pop up a standard file
 *     selection dialogue asking the user to pick an MT940 file. If one
 *     is selected then the MT940 file is opened and read. Its contents
 *     are merged into the existing session (if any). The current
 *     session continues to remain open for editing. */
void              gnc_file_mt940_import (void);
SCM  scm_gnc_file_mt940_import (void);
#endif
