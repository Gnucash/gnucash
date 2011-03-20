/*
 * gnc-gkeyfile-utils.h -- utility functions for working
 *              with GKeyFile data structures from GLib
 * Copyright (C) 2005 David Hampton <hampton@employees.org>
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

/** @addtogroup GLib
    @{ */
/** @addtogroup GKeyFile GKeyfile Utilities

    This file provides routines that help make it easier to use the
    GKeyFile functions from within Gnucash.

    @{ */
/** @file gnc-gkeyfile-utils.h
 *  @brief GKeyFile helper routines.
 *  @author Copyright (C) 2005 David Hampton <hampton@employees.org>
 */

#ifndef GNC_GKEYFILE_UTILS_H
#define GNC_GKEYFILE_UTILS_H


/** Open and read a key/value file from disk into memory.
 *
 *  @param file The name of the file to load.  This should be a fully
 *  qualified path.
 *
 *  @param ignore_error If true this function will ignore any problems
 *  reading the an existing file from disk.
 *
 *  @param return_empty_struct If TRUE this function will always
 *  return a GKeyFile structure.  Set to TRUE if performing a
 *  read/modify/write on a file that may or may not already exist.
 *
 *  @return A pointer to a GKeyFile data structure, or NULL.
 */
GKeyFile *gnc_key_file_load_from_file (const gchar *file,
                                       gboolean ignore_error,
                                       gboolean return_empty_struct,
                                       GError **caller_error);


/** Write a key/value file from memory to disk.  If there is no data
 * to be written, this function will not create a file and will remove
 * any exiting file.
 *
 *  @param file The name of the file to write.  This should be a fully
 *  qualified path.
 *
 *  @param key_file The data to be written.
 *
 *  @return A TRUE if the data was successfully written to disk.
 *  FALSE if there was an error.
 */
gboolean gnc_key_file_save_to_file (const gchar *file,
                                    GKeyFile *key_file,
                                    GError **error);

#endif /* GNC_GKEYFILE_UTILS_H */
/** @} */
/** @} */
