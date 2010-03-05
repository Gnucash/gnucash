/*
 * gnc-keyring.h -- utility functions to store and retrieve passwords.
 *
 * Copyright (C) 2010 Geert Janssens <janssens.geert@telenet.be>
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

/** @addtogroup GUI
    @{ */
/** @addtogroup GUIUtility
    @{ */
/** @file gnc-keyring.h
    @brief Functions to save and retrieve passwords.
    @author Copyright (C) 2010 Geert Janssens <janssens-geert@telenet.be>

    GnuCash needs passwords for some of the services it uses, like
    connecting to a remote database, or bank. For security these passwords
    shouldn't be stored unless in an encrypted password store. This is
    implemented for example in Gnome's keyring or Mac OS X' keychain.
    This file defines some convenience functions to store a password
    or to retrieve one.
*/
#ifndef KEYRING_H_
#define KEYRING_H_

#include <glib.h>

#include "gnc-ui.h"

/** Attempt to store a password in some trusted keystore. At this point
 *  that can be Gnome's keyring or Mac OS X' keychain. If no
 *  such keystore is available, this function does nothing.
 *
 *  All the parameters passed (except for the password) will be
 *  used to create a unique key, so the password can later be
 *  retrieved again with the same parameters.
 *
 *  @param parent Used to transition from in case the user is prompted
 *                for a password.
 *  @param access_method Service type the user attempts to access. Can
 *                things like 'mysql', 'postgres' and so on.
 *  @param server Server the user wishes to connect to.
 *  @param port   Port the service listens on. If set to 0, it will
 *                be ignored in the search for a password.
 *  @param service The service the user wishes to access on the server.
 *                This can be a database name or a path.
 *  @param user   The username to access the service. Remember, although
 *                you pass it to search for the password, it can have
 *                changed when the function returns.
 *  @param password The password to access the service.
 */
void gnc_keyring_set_password ( const gchar *access_method,
                                const gchar *server,
                                guint32 port,
                                const gchar *service,
                                const gchar *user,
                                const gchar* password );

/** Attempt to retrieve a password to connect to
 *  a remote service. This is deliberately generic: the remote
 *  service can be a database, website, anything.
 *
 *  If a trusted keystore infrastructure is found (such as the
 *  Gnome's keyring or Mac OS X' keychain) this infrastructure
 *  will be queried first.
 *
 *  If no such infrastructure is available or the query didn't
 *  return a valid result, the user will be prompted for his
 *  password.
 *
 *  @warning When the user is prompted for a password, he can also
 *  change the username. So whenever you call this function, read
 *  both the username and password values before you continue !
 *
 *  @param parent Used to transition from in case the user is prompted
 *                for a password.
 *  @param access_method Service type the user attempts to access. Can
 *                things like 'mysql', 'postgres' and so on.
 *  @param server Server the user wishes to connect to.
 *  @param port   Port the service listens on. If set to 0, it will
 *                be ignored in the search for a password.
 *  @param service The service the user wishes to access on the server.
 *                This can be a database name or a path.
 *  @param user   The user name to access the service. Remember, although
 *                you pass it to search for the password, it can have
 *                changed when the function returns.
 *  @param password The password to access the service.
 *  @return       a boolean indicating whether or not a valid password
 *                has been retrieved. The function will return FALSE
 *                when the user explicitly cancels the password dialog or
 *                if it wasn't called properly. Otherwise it wil return
 *                TRUE.
 *
 *  access_method, server, port, service and user will be the parameters
 *  passed to the trusted keystore (if available) to find the unique
 *  password for this service.
 */

gboolean gnc_keyring_get_password ( GtkWidget *parent,
                                    const gchar *access_method,
                                    const gchar *server,
                                    guint32 port,
                                    const gchar *service,
                                    gchar **user,
                                    gchar **password );

/* @} */
/* @} */


#endif /* KEYRING_H_ */
