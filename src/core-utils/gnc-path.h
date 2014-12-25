/********************************************************************\
 * gnc-path.h -- Path lookup of gnucash installation locations      *
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


#ifndef GNC_PATH_H
#define GNC_PATH_H

#include <glib.h>

/** Returns the installation prefix path, usually
 * "$prefix".
 *
 * @returns A newly allocated string. */
gchar *gnc_path_get_prefix(void);

/** Returns the bindir path, usually
 * "$prefix/bin".
 *
 * @returns A newly allocated string. */
gchar *gnc_path_get_bindir(void);

/** Returns the libdir path, usually
 * "$prefix/lib".
 *
 * @returns A newly allocated string. */
gchar *gnc_path_get_libdir(void);

/** Returns the pkgdatadir path, usually
 * "$prefix/share/gnucash". Needed for gnc_gnome_locate_*().
 *
 * @returns A newly allocated string. */
gchar *gnc_path_get_pkgdatadir(void);

/** Returns the pkgdocdir path, usually
 * "$prefix/share/doc/gnucash".
 *
 * @returns A newly allocated string. */
gchar *gnc_path_get_pkgdocdir(void);

/** Returns the pkgsysconfdir path, usually
 * "$prefix/etc/gnucash".
 *
 * @returns A newly allocated string. */
gchar *gnc_path_get_pkgsysconfdir(void);


/** Returns the pkglibdir path, usually
 * "$prefix/lib/gnucash".
 *
 * @returns A newly allocated string. */
gchar *gnc_path_get_pkglibdir(void);

/** Returns the gtkbuilder file path, usually
 * "$prefix/share/gnucash/gktbuilder".
 *
 * @returns A newly allocated string. */
gchar *gnc_path_get_gtkbuilderdir(void);

/** Returns the localedir path, usually
 * "$prefix/share/locale".
 *
 * @returns A newly allocated string. */
gchar *gnc_path_get_localedir(void);

/** Returns the accounts file path, usually
 * "$prefix/share/gnucash/accounts".
 *
 * @returns A newly allocated string. */
gchar *gnc_path_get_accountsdir(void);

/** Returns the file path to the report directory, usually
 * "$prefix/share/gnucash/scm/gnucash/report".
 *
 * @returns A newly allocated string. */
gchar *gnc_path_get_reportdir(void);

/** Returns the file path to the standard
 * reports, usually
 * "$prefix/share/gnucash/scm/gnucash/report/standard-reports".
 *
 * @returns A newly allocated string. */
gchar *gnc_path_get_stdreportsdir(void);



#endif /* GNC_PATH_H */
