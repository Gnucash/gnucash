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

#include <config.h>
#include "gnc-path.h"
#include "gncla-dir.h"
#include <stdio.h>
#include "binreloc.h"

gchar *gnc_path_get_prefix()
{
    //printf("Returning prefix %s\n", gnc_gbr_find_prefix (PREFIX));
    return gnc_gbr_find_prefix (PREFIX);
}

/** Returns the bindir path, usually
 * "$prefix/bin".
 *
 * @returns A newly allocated string. */
gchar *gnc_path_get_bindir()
{
    //printf("Returning bindir %s\n", gnc_gbr_find_bin_dir (BINDIR));
    return gnc_gbr_find_bin_dir (BINDIR);
}

/** Returns the libdir path, usually
 * "$prefix/lib".
 *
 * @returns A newly allocated string. */
gchar *gnc_path_get_libdir()
{
    //printf("Returning libdir %s\n", gnc_gbr_find_lib_dir (LIBDIR));
    return gnc_gbr_find_lib_dir (LIBDIR);
}

/** Returns the libdir path, usually
 * "$prefix/lib".
 *
 * @returns A newly allocated string. */
gchar *gnc_path_get_datadir()
{
    //printf("Returning libdir %s\n", gnc_gbr_find_lib_dir (LIBDIR));
    return gnc_gbr_find_data_dir (DATADIR);
}

/** Returns the datadir path, usually
 * "$prefix/share/gnucash". Needed for gnc_gnome_locate_*().
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

/** Returns the docdir path, usually
 * "$prefix/share/doc/gnucash".
 *
 * @returns A newly allocated string. */
gchar *gnc_path_get_pkgdocdir()
{
    gchar *docdir = gnc_gbr_find_data_dir (DATADIR);
    gchar *result = g_build_filename (docdir, "doc", "gnucash", (char*)NULL);
    g_free (docdir);
    //printf("Returning pkgdocdir %s\n", result);
    return result;
}

/** Returns the sysconfdir path, usually
 * "$prefix/etc/gnucash".
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
#ifdef G_OS_WIN32
    /* Workaround for Bug 618646, {pkglibdir} will be bin/ on Windows */
    gchar *result = gnc_gbr_find_bin_dir(libdir);
#else
    gchar *result = g_build_filename (libdir, "gnucash", (char*)NULL);
#endif
    g_free (libdir);
    //printf("Returning pkglibdir %s\n", result);
    return result;
}

/** Returns the gtkbuilder file path, usually
 * "$prefix/share/gnucash/gtkbuilder".
 *
 * @returns A newly allocated string. */
gchar *gnc_path_get_gtkbuilderdir()
{
    gchar *pkgdatadir = gnc_path_get_pkgdatadir ();
    gchar *result = g_build_filename (pkgdatadir, "gtkbuilder", (char*)NULL);
    g_free (pkgdatadir);
    //printf("Returning gtkbuilderdir %s\n", result);
    return result;
}

/** Returns the localedir path, usually
 * "$prefix/share/locale".
 *
 * @returns A newly allocated string. */
gchar *gnc_path_get_localedir()
{
    if (g_path_is_absolute (LOCALEDIR))
        return g_strdup(LOCALEDIR);
    else
    {
        gchar *prefix = gnc_path_get_prefix();
        gchar *result = g_build_filename (prefix, LOCALEDIR, (char*)NULL);
        g_free (prefix);
        //printf("Returning localedir %s\n", result);
        return result;
    }
}

/** Returns the accounts file path, usually
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

/** Returns the file path to the report directory, usually
 * "$prefix/share/gnucash/scm/gnucash/report".
 *
 * @returns A newly allocated string. */
gchar *gnc_path_get_reportdir()
{
    /* Careful: if the cmake variable SCHEME_INSTALLED_SOURCE_DIR gets changed
     * in toplevel CMakeLists.txt, this path should probably change as well.
     * Currently this code assumes SCHEME_INSTALLED_SOURCE_DIR is set to
     * pkgdatadir/scm
     * We can't use GNC_SCM_INSTALL_DIR directly at build time to
     * get this information, because on Windows and OS X
     * the final path may get installed in a different location
     * than assumed during build, invalidating the build path at
     * runtime.
     */
    gchar *pkgdatadir = gnc_path_get_pkgdatadir ();
    gchar *result = g_build_filename (pkgdatadir, "scm",
                                      "gnucash", "report", (char*)NULL);
    g_free (pkgdatadir);

    return result;
}

/** Returns the file path to the standard
 * reports, usually
 * "$prefix/share/gnucash/scm/gnucash/report/standard-reports".
 *
 * @returns A newly allocated string. */
gchar *gnc_path_get_stdreportsdir()
{
    gchar *reportdir = gnc_path_get_reportdir ();
    gchar *result = g_build_filename (reportdir, "standard-reports", NULL);
    g_free (reportdir);
    //printf("Returning stdreportsdir %s\n", result);
    return result;
}

