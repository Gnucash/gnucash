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

gchar *gnc_path_get_prefix()
{
  return g_strdup (PREFIX);
}

/** Returns the libdir path, usually
 * "$prefix/lib". Needed for gnome_program_init().
 *
 * @returns A newly allocated string. */
gchar *gnc_path_get_libdir()
{
  return g_strdup (LIBDIR);
}

/** Returns the datadir path, usually
 * "$prefix/share". Needed for gnome_program_init().
 *
 * @returns A newly allocated string. */
gchar *gnc_path_get_datadir()
{
  return g_strdup (DATADIR);
}

/** Returns the sysconfdir path, usually
 * "$prefix/etc". Needed for gnome_program_init().
 *
 * @returns A newly allocated string. */
gchar *gnc_path_get_sysconfdir()
{
  return g_strdup (SYSCONFDIR);
}


/** Returns the pkglibdir path, usually
 * "$prefix/lib/gnucash".
 *
 * @returns A newly allocated string. */
gchar *gnc_path_get_pkglibdir()
{
  return g_strdup (GNC_LIBDIR);
}

/** Returns the glade file path, usually
 * "$prefix/share/gnucash/glade".
 *
 * @returns A newly allocated string. */
gchar *gnc_path_get_gladedir()
{
  gchar *result;
#ifdef G_OS_WIN32
  result = g_win32_get_package_installation_subdirectory
    (GETTEXT_PACKAGE, NULL, "share\\gnucash\\glade");
#else
  result = g_strdup (GNC_GLADE_DIR);
#endif
  return result;
}

/** Returns the localedir path, usually
 * "$prefix/share/locale".
 *
 * @returns A newly allocated string. */
gchar *gnc_path_get_localedir()
{
  return g_strdup (LOCALE_DIR);
}

/** Returns the glade file path, usually
 * "$prefix/share/gnucash/accounts".
 *
 * @returns A newly allocated string. */
gchar *gnc_path_get_accountsdir()
{
  gchar *result;
#ifdef G_OS_WIN32
  result = 
    g_win32_get_package_installation_subdirectory
    (GETTEXT_PACKAGE, NULL, "share\\gnucash\\accounts");
#else
  result = g_strdup (GNC_ACCOUNTS_DIR);
#endif
  return result;
}

/** Returns the gconf schema config source path, usually
 * "$prefix/etc/gconf/gconf.xml.defaults".
 *
 * @returns A newly allocated string. */
gchar *gnc_path_get_gconfdir()
{
  return g_strdup (GNC_GCONF_DIR);
}

