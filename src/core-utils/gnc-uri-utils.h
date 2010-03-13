/*
 * gnc-uri-utils.h -- utility functions to convert uri in separate
 *                    components and back.
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

/** @addtogroup Utils Utility functions
    @{ */
/** @addtogroup UtilUri Uri conversion
 * @{ */
/** @file gnc-uri-utils.h
 *  @brief  Utility functions for convert uri in separate components and back
 *  @author Copyright (C) 2010 Geert Janssens <janssens-geert@telenet.be>
 *
 *  These functions help you convert a uri into its separate components
 *  (being protocol, host name, port, user name, password and path) or
 *  to compose a uri from these separate components.
 *
 */

#ifndef GNCURIUTILS_H_
#define GNCURIUTILS_H_

/** Converts a uri in separate components.
 *
 *  Uri's can take any of the following forms:
 *
 *  @li @c /some/filesystem/path A simple file system path (unix style)
 *  @li @c c:\\some\\windows\\path A simple file system path (Windows style)
 *  @li @c proto://[[username[:password]@]hostname[:port]]/path (universal uri)
 *
 *  In the last form, anything in square brackets is optional.
 *
 *  The function allocates memory for each of the components that it finds
 *  in the uri. The calling function should free this memory with g_free
 *  if the items are no longer needed.
 *
 *  @param uri The uri to convert
 *
 *  @param protocol The protocol for this uri. If the uri didn't have an
 *  explicit protocol, 'file' will be the assumed protocol and hence what
 *  will be returned.
 *  @param hostname The host name of the server to connect to. In case of
 *  the 'file' protocol, this will be NULL
 *  @param port An optional port to connect to or 0 if the default port is to
 *  be used. For the 'file' protocol this is always 0 as well.
 *  @param username Optional user name found in this uri or NULL if none is found.
 *  @param password Optional password found in this uri or NULL if none is found.
 *  @param path The path found in this uri. Note that if the protocol is a file based
 *  protocol, the path will be converted to an absolute path.
 *
 */

void gnc_uri_get_components (const gchar *uri,
                             gchar **protocol,
                             gchar **hostname,
                             guint32 port,
                             gchar **username,
                             gchar **password,
                             gchar **path);

/** Extracts the protocol from a uri
 *
 *  Uri's can take any of the following forms:
 *
 *  @li @c /some/filesystem/path A simple file system path (unix style)
 *  @li @c c:\\some\\windows\\path A simple file system path (Windows style)
 *  @li @c proto://[[username[:password]@]hostname[:port]]/path (universal uri)
 *
 *  In the last form, anything in square brackets is optional.
 *
 *  The function allocates memory for the protocol. The calling function should
 *  free this memory with g_free if it no longer needs the string.
 *
 *  @param uri The uri to extract the protocol from
 *
 *  @return The protocol for this uri. If the uri didn't have an
 *  explicit protocol, 'file' will be returned as protocol.
 */

gchar *gnc_uri_get_protocol (const gchar *uri);

/** Extracts the path part from a uri
 *
 *  Uri's can take any of the following forms:
 *
 *  @li @c /some/filesystem/path A simple file system path (unix style)
 *  @li @c c:\\some\\windows\\path A simple file system path (Windows style)
 *  @li @c proto://[[username[:password]@]hostname[:port]]/path (universal uri)
 *
 *  In the last form, anything in square brackets is optional.
 *
 *  The function allocates memory for the path. The calling function should
 *  free this memory with g_free if it no longer needs the string.
 *
 *  @param uri The uri to extract the path part from
 *
 *  @return The protocol for this uri, or NULL if no path could be extracted.
 */

gchar *gnc_uri_get_path (const gchar *uri);

/** Composes a normalized uri starting from its separate components.
 *
 *  The resulting uri will take either of these forms:
 *  @li @c file:///some/absolute/path (file could also be xml or sqlite)
 *  @li @c file://c:\\some\\windows\\path (file could also be xml or sqlite)
 *  @li @c protocol://[user[:password]@]hostname[:port]/path
 *
 *  Only the components that are provided will be inserted in the uri. However
 *  if no protocol has been provided, 'file' will be used as default protocol.
 *
 *  The function allocates memory for for the uri. The calling function should
 *  free this memory with g_free the uri is no longer needed.
 *
*  @param protocol The protocol for this uri. If NULL,, 'file' will be used
*   in the uri.
 *  @param hostname The host name of the server to connect to. This will be
 *  ignored for the 'file' type protocols ('file', 'xml', 'sqlite').
 *  @param port An optional port to set o, the uri, or 0 if no port is to be
 *  set. This will be ignored for the 'file' type protocols ('file', 'xml',
 *  'sqlite').
 *  @param username Optional user name to set in the uri or NULL otherwise. This will
 *  be ignored for the 'file' type protocols ('file', 'xml', 'sqlite').
 *  @param password Optional password to set in the uri or NULL otherwise. This will
 *  be ignored for the 'file' type protocols ('file', 'xml', 'sqlite').
 *  @param path The path to set in the uri.
 *
 *  @return The normalized uri.
 */

gchar *gnc_uri_create_uri (const gchar *protocol,
                           const gchar *hostname,
                           guint32 port,
                           const gchar *username,
                           const gchar *password,
                           const gchar *path);

/** Composes a normalized uri starting from any uri (filename, db spec,...).
 *
 *  The resulting uri will take either of these forms:
 *  @li @c file:///some/absolute/path (file could also be xml or sqlite)
 *  @li @c file://c:\\some\\windows\\path (file could also be xml or sqlite)
 *  @li @c protocol://[user[:password]@]hostname[:port]/path
 *
 *  Only the components that are provided will be inserted in the uri. The
 *  allow_password parameter controls if the password should be added to the
 *  returned uri when available.
 *  If no protocol has been provided, 'file' will be used as default protocol.
 *
 *  The function allocates memory for for the uri. The calling function should
 *  free this memory with g_free the uri is no longer needed.
 *
*  @param uri The uri that schould be converted into a normalized uri
 *  @param allow_password If set to TRUE, the normalized uri and the input uri
 *  has a password, this passworld will also be set in the normalized uri.
 *  Otherwise no password will be set in the normalized uri.
 *
 *  @return The normalized uri.
 */
gchar *gnc_uri_normalize_uri (const gchar *uri, gboolean allow_password);


/** Checks if the given protocol is used to refer to a file
 *  (as opposed to a network service like a database or web url)
 *
 *  @param protocol The protocol to check
 *
 *  @return TRUE if the protocol is used with files, FALSE of the protocol
 *  is normally used with network services (database, web url,...)
 */
gboolean gnc_uri_is_file_protocol (const gchar *protocol);

/** Checks if the given uri defines a file
 *  (as opposed to a network service like a database or web url)
 *
 *  @param uri The uri to check
 *
 *  @return TRUE if the uri is a files, FALSE of the protocol
 *  is normally used with network services (database, web url,...)
 */
gboolean gnc_uri_is_file_uri (const gchar *uri);

#endif /* GNCURIUTILS_H_ */
/** @} */
/** @} */

