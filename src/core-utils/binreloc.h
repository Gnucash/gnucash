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
    GNC_GBR_INIT_ERROR_DISABLED
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
