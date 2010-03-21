/********************************************************************\
 * gnc-path.c -- Path lookup of gnucash installation locations      *
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

#include "config.h"
#include "gnc-path.h"
#include "gncla-dir.h"
#include <stdio.h>
#include "binreloc.h"

gchar *gnc_path_get_prefix()
{
    //printf("Returning prefix %s\n", gnc_gbr_find_prefix (PREFIX));
    return gnc_gbr_find_prefix (PREFIX);
}

/** Returns the libdir path, usually
 * "$prefix/lib". Needed for gnome_program_init().
 *
 * @returns A newly allocated string. */
gchar *gnc_path_get_libdir()
{
    //printf("Returning libdir %s\n", gnc_gbr_find_lib_dir (LIBDIR));
    return gnc_gbr_find_lib_dir (LIBDIR);
}

/** Returns the datadir path, usually
 * "$prefix/share/gnucash". Needed for gnome_program_init().
 *
 * @returns A newly allocated string. */
gchar *gnc_path_get_pkgdatadir()
{
    gchar *datadir = gnc_gbr_find_data_dir (DATADIR);
    gchar *result = g_build_filename (datadir, "gnucash", (char*)NULL);
    g_free (datadir);
    //printf("Returning pkgdatadir %s\n", result);
    return result;
}

/** Returns the sysconfdir path, usually
 * "$prefix/etc/gnucash". Needed for gnome_program_init().
 *
 * @returns A newly allocated string. */
gchar *gnc_path_get_pkgsysconfdir()
{
    gchar *sysconfdir = gnc_gbr_find_etc_dir (SYSCONFDIR);
    gchar *result = g_build_filename (sysconfdir, "gnucash", (char*)NULL);
    g_free (sysconfdir);
    //printf("Returning pkgsysconfdir %s\n", result);
    return result;
}


/** Returns the pkglibdir path, usually
 * "$prefix/lib/gnucash".
 *
 * @returns A newly allocated string. */
gchar *gnc_path_get_pkglibdir()
{
    gchar *libdir = gnc_path_get_libdir ();
    gchar *result = g_build_filename (libdir, "gnucash", (char*)NULL);
    g_free (libdir);
    //printf("Returning pkglibdir %s\n", result);
    return result;
}

/** Returns the glade file path, usually
 * "$prefix/share/gnucash/glade".
 *
 * @returns A newly allocated string. */
gchar *gnc_path_get_gladedir()
{
    gchar *pkgdatadir = gnc_path_get_pkgdatadir ();
    gchar *result = g_build_filename (pkgdatadir, "glade", (char*)NULL);
    g_free (pkgdatadir);
    //printf("Returning gladedir %s\n", result);
    return result;
}

/** Returns the localedir path, usually
 * "$prefix/share/locale".
 *
 * @returns A newly allocated string. */
gchar *gnc_path_get_localedir()
{
    gchar *prefix = gnc_path_get_prefix();
    gchar *result = g_build_filename (prefix, LOCALE_DATADIRNAME, "locale", (char*)NULL);
    g_free (prefix);
    //printf("Returning localedir %s\n", result);
    return result;
}

/** Returns the glade file path, usually
 * "$prefix/share/gnucash/accounts".
 *
 * @returns A newly allocated string. */
gchar *gnc_path_get_accountsdir()
{
    gchar *pkgdatadir = gnc_path_get_pkgdatadir ();
    gchar *result = g_build_filename (pkgdatadir, "accounts", (char*)NULL);
    g_free (pkgdatadir);
    //printf("Returning accountsdir %s\n", result);
    return result;
}

/** Returns the gconf schema config source path, usually
 * "$prefix/etc/gconf/gconf.xml.defaults".
 *
 * @returns A newly allocated string. */
gchar *gnc_path_get_gconfdir(gboolean force_slashes)
{
    gchar *sysconfdir = gnc_gbr_find_etc_dir (SYSCONFDIR);
    gchar *separator = G_DIR_SEPARATOR_S;
    gchar *result;

    if (force_slashes)
    {
        gchar **splitted;
        splitted = g_strsplit (sysconfdir, "\\", -1);
        g_free (sysconfdir);
        sysconfdir = g_strjoinv ("/", splitted);
        g_strfreev (splitted);
        separator = "/";
    }

    result = g_build_path (separator, sysconfdir, "gconf", "gconf.xml.defaults",
                           (gchar*)NULL);
    g_free (sysconfdir);
    //printf("Returning gconfdir %s\n", result);
    return result;
}

