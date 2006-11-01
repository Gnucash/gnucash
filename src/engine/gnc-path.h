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
 * "$prefix". Needed for gnome_program_init().
 *
 * @returns A newly allocated string. */
gchar *gnc_path_get_prefix(void);

/** Returns the libdir path, usually
 * "$prefix/lib". Needed for gnome_program_init(void).
 *
 * @returns A newly allocated string. */
gchar *gnc_path_get_libdir(void);

/** Returns the pkgdatadir path, usually
 * "$prefix/share/gnucash". Needed for gnome_program_init(void).
 *
 * @returns A newly allocated string. */
gchar *gnc_path_get_pkgdatadir(void);

/** Returns the pkgsysconfdir path, usually
 * "$prefix/etc/gnucash". Needed for gnome_program_init(void).
 *
 * @returns A newly allocated string. */
gchar *gnc_path_get_pkgsysconfdir(void);


/** Returns the pkglibdir path, usually
 * "$prefix/lib/gnucash".
 *
 * @returns A newly allocated string. */
gchar *gnc_path_get_pkglibdir(void);

/** Returns the glade file path, usually
 * "$prefix/share/gnucash/glade".
 *
 * @returns A newly allocated string. */
gchar *gnc_path_get_gladedir(void);

/** Returns the localedir path, usually
 * "$prefix/share/locale".
 *
 * @returns A newly allocated string. */
gchar *gnc_path_get_localedir(void);

/** Returns the glade file path, usually
 * "$prefix/share/gnucash/accounts".
 *
 * @returns A newly allocated string. */
gchar *gnc_path_get_accountsdir(void);

/** Returns the gconf schema config source path, usually
 * "$prefix/etc/gconf/gconf.xml.defaults".
 *
 * @param force_slashes Use slashes as separator of the elements
 * of the path.
 *
 * @returns A newly allocated string. */
gchar *gnc_path_get_gconfdir(gboolean force_slashes);



#endif /* GNC_PATH_H */
