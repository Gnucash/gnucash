/*
 * BinReloc - a library for creating relocatable executables
 * Written by: Hongli Lai <h.lai@chello.nl>
 * http://autopackage.org/
 *
 * This source code is public domain. You can relicense this code
 * under whatever license you want.
 *
 * See http://autopackage.org/docs/binreloc/ for
 * more information and how to use this.
 */
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
 *                                                                  *
\********************************************************************/


#ifndef __BINRELOC_H__
#define __BINRELOC_H__

#include <glib.h>

G_BEGIN_DECLS


/** These error codes can be returned by gnc_gbr_init(). */
typedef enum
{
    /** Cannot allocate memory. */
    GNC_GBR_INIT_ERROR_NOMEM,
    /** Unable to open /proc/self/maps; see errno for details. */
    GNC_GBR_INIT_ERROR_OPEN_MAPS,
    /** Unable to read from /proc/self/maps; see errno for details. */
    GNC_GBR_INIT_ERROR_READ_MAPS,
    /** The file format of /proc/self/maps is invalid; kernel bug? */
    GNC_GBR_INIT_ERROR_INVALID_MAPS,
    /** BinReloc determined that gnucash is not running from a bundle */
    GNC_GBR_INIT_ERROR_MAC_NOT_BUNDLE,
    /** Binreloc determined that the bundle is not an app bundle */
    GNC_GBR_INIT_ERROR_MAC_NOT_APP_BUNDLE,
    /** BinReloc is disabled (the ENABLE_BINRELOC macro is not defined). */
    GNC_GBR_INIT_ERROR_DISABLED,
    /** Binreloc was unable to determine the location of gnucash.exe. */
    GNC_GBR_INIT_WIN32_NO_EXE_DIR
} Gnc_GbrInitError;


gboolean gnc_gbr_init             (GError **error);

gchar   *gnc_gbr_find_exe         (const gchar *default_exe);
gchar   *gnc_gbr_find_exe_dir     (const gchar *default_dir);
gchar   *gnc_gbr_find_prefix      (const gchar *default_prefix);
gchar   *gnc_gbr_find_bin_dir     (const gchar *default_bin_dir);
gchar   *gnc_gbr_find_sbin_dir    (const gchar *default_sbin_dir);
gchar   *gnc_gbr_find_data_dir    (const gchar *default_data_dir);
gchar   *gnc_gbr_find_lib_dir     (const gchar *default_lib_dir);
gchar   *gnc_gbr_find_libexec_dir (const gchar *default_libexec_dir);
gchar   *gnc_gbr_find_etc_dir     (const gchar *default_etc_dir);

/** Sets the executable path to the given value. This is useful if the
 * binreloc lookup code will not be used, but instead the executable
 * location is obtained from somewhere else (e.g. qt) but the gnucash
 * code should nevertheless use this path internally. */
void gnc_gbr_set_exe (const gchar* default_exe);


G_END_DECLS

#endif /* __BINRELOC_H__ */
