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
 *  (being scheme, host name, port, user name, password and path) or
 *  to compose a uri from these separate components.
 *
 *  For GnuCash' purposes a full uri can be described as:
 *
 *  @li @c scheme://[[username[:password]@]hostname[:port]]/path (universal uri)
 *  @li @c file://[localhost]/path (uri refering to a file on the local file system)
 *
 *  Anything in square brackets is optional.
 *
 *  While not strictly uris this function will also accept input in the form
 *  of a local file system path for convenience:
 *
 *  @li @c some/filesystem/path A simple relative file system path (unix style)
 *  @li @c /some/filesystem/path A simple absolute file system path (unix style)
 *  @li @c some\\windows\\path A simple relative file system path (Windows style)
 *  @li @c c:\\some\\windows\\path A simple file system path (Windows style)
 *
 *  The local path will then be considered as the path component of a uri where
 *  appropriate. In case of conversion from a file system path to a uri the relative
 *  paths will be converted to absolute paths as uris don't allow relative paths.
 *
 */

#ifndef GNCURIUTILS_H_
#define GNCURIUTILS_H_

#define GNC_DATAFILE_EXT ".gnucash"
#define GNC_LOGFILE_EXT  ".log"

#include "platform.h"

/** Checks if the given uri is a valid uri
 *
 *  A valid uri is defined by having at least a scheme and a path.
 *  If the uri is not referring to a file on the local file system
 *  a hostname should be set as well.
 *
 *  @param uri The uri to check
 *
 *  @return TRUE if the input is a valid uri, FALSE otherwise
 */
gboolean gnc_uri_is_uri (const gchar *uri);

/** Converts a uri in separate components.
  *
 *  The function allocates memory for each of the components that it finds
 *  in the uri. The calling function should free this memory with g_free
 *  when the items are no longer needed.
 *
 *  @param uri The uri to convert
 *
 *  @param scheme The scheme for this uri. If the uri doesn't have an
 *  explicit scheme, NULL will be returned.
 *  @param hostname The host name of the server to connect to. In case of
 *  the local file system path, NULL will be returned
 *  @param port An optional port to connect to or 0 if the default port is to
 *  be used. For local filesystem path this is always 0 as well.
 *  @param username Optional user name found in this uri or NULL if none is found.
 *  @param password Optional password found in this uri or NULL if none is found.
 *  @param path The path found in this uri.
 *
 */

void gnc_uri_get_components (const gchar *uri,
                             gchar **scheme,
                             gchar **hostname,
                             gint32 *port,
                             gchar **username,
                             gchar **password,
                             gchar **path);

/** Extracts the scheme from a uri
 *
 *  The function allocates memory for the scheme. The calling function should
 *  free this memory with g_free if it no longer needs the string.
 *
 *  @param uri The uri to extract the scheme from
 *
 *  @return The scheme for this uri. If the uri didn't have an
 *  explicit scheme, NULL will be returned.
 */

gchar *gnc_uri_get_scheme (const gchar *uri);

/** Extracts the path part from a uri
 *
 *  The function allocates memory for the path. The calling function should
 *  free this memory with g_free if it no longer needs the string.
 *
 *  @param uri The uri to extract the path part from
 *
 *  @return The path for this uri, or NULL if no path could be extracted.
 */

gchar *gnc_uri_get_path (const gchar *uri);

/** Composes a normalized uri starting from its separate components.
 *
 *  The resulting uri will take either of these forms:
 *  @li @c file:///some/absolute/path (file could also be xml or sqlite)
 *  @li @c file://c:\\some\\windows\\path (file could also be xml or sqlite)
 *  @li @c scheme://[user[:password]@]hostname[:port]/path
 *
 *  Only the components that are provided will be inserted in the uri. However
 *  if no scheme has been provided, 'file' will be used as default scheme.
 *
 *  The function allocates memory for the uri. The calling function should
 *  free this memory with g_free the uri is no longer needed.
 *
*  @param scheme The scheme for this uri. If NULL,, 'file' will be used
*   in the uri.
 *  @param hostname The host name of the server to connect to. This will be
 *  ignored for the 'file' type schemes ('file', 'xml', 'sqlite').
 *  @param port An optional port to set o, the uri, or 0 if no port is to be
 *  set. This will be ignored for the 'file' type schemes ('file', 'xml',
 *  'sqlite').
 *  @param username Optional user name to set in the uri or NULL otherwise. This will
 *  be ignored for the 'file' type schemes ('file', 'xml', 'sqlite').
 *  @param password Optional password to set in the uri or NULL otherwise. This will
 *  be ignored for the 'file' type schemes ('file', 'xml', 'sqlite').
 *  @param path The path to set in the uri.
 *
 *  @return The normalized uri.
 */

gchar *gnc_uri_create_uri (const gchar *scheme,
                           const gchar *hostname,
                           gint32 port,
                           const gchar *username,
                           const gchar *password,
                           const gchar *path);

/** Composes a normalized uri starting from any uri (filename, db spec,...).
 *
 *  The resulting uri will take either of these forms:
 *  @li @c file:///some/absolute/path ('file' can also be xml or sqlite)
 *  @li @c file://c:\\some\\windows\\path ('file' can also be xml or sqlite)
 *  @li @c scheme://[user[:password]@]hostname[:port]/path
 *
 *  Only the components that are provided will be inserted in the uri. The
 *  allow_password parameter controls if the password should be added to the
 *  returned uri when available.
 *  If no scheme has been provided, 'file' will be used as default scheme.
 *
 *  The function allocates memory for the uri. The calling function should
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


/** Checks if the given uri is a valid uri
 *
 *  A valid uri is defined by having at least a scheme and a path.
 *  If the uri is not referring to a file on the local file system
 *  a hostname should be set as well.
 *
 *  @param uri The uri to check
 *
 *  @return TRUE if the input is a valid uri, FALSE otherwise
 */
gboolean gnc_uri_is_uri (const gchar *uri);


/** Checks if there is a backend that explicitly stated to handle the given scheme.
 *
 *  @param scheme The scheme to check
 *
 *  @return TRUE if at least one backend explicitly handles this scheme, otherwise FALSE
 */
gboolean gnc_uri_is_known_scheme (const gchar *scheme);


/** Checks if the given scheme is used to refer to a file
 *  (as opposed to a network service like a database or web url)
 *
 *  @param scheme The scheme to check
 *
 *  @return TRUE if the scheme is used with files, FALSE if the scheme
 *  is normally used with network services (database, web url,...).
 *  It will also return FALSE if scheme is NULL.
 */
gboolean gnc_uri_is_file_scheme (const gchar *scheme);


/** Checks if the given uri defines a file
 *  (as opposed to a network service like a database or web url)
 *
 *  @param uri The uri to check
 *
 *  @return TRUE if the uri is a files, FALSE of the scheme
 *  is normally used with network services (database, web url,...)
 */
gboolean gnc_uri_is_file_uri (const gchar *uri);


/** Checks if the given uri is either a valid file uri or a local filesystem path
 *
 *  A valid file uri is defined by having a file targeting scheme
 *  ('file', 'xml' or 'sqlite3' are accepted) and a non-NULL path.
 *
 *  @param uri The uri to check
 *
 *  @return TRUE if the input is a valid file uri or a local filesystem path.
 *  FALSE otherwise
 */
gboolean gnc_uri_targets_local_fs (const gchar *uri);


/** Adds an extension to the uri if:
 *  * the uri is not empty and file based
 *  * doesn't already have the extension
 *
 *  @param uri The uri to process
 *  @param extension The extension to add if missing. Note that the extension
 *                   is added verbatim, so if a dot should be added, this
 *                   should be part of the extension.
 *
 *  @return The uri, but garanteed to end with extension if the uri is file
 *          based. Otherwise the uri is returned unmodified. Note that the
 *          returned value should be freed with g_free when no longer needed.
 */
gchar *gnc_uri_add_extension ( const gchar *uri, const gchar *extension );


/** @name Deprecated functions
 * @{
 */

/** Extracts the protocol from a uri
 *
 *  @deprecated This function has been deprecated in gnucash 3.4. Please use gnc_uri_get_scheme instead.
 *
 *  The function allocates memory for the protocol. The calling function should
 *  free this memory with g_free if it no longer needs the string.
 *
 *  @param uri The uri to extract the protocol from
 *
 *  @return The protocol for this uri. If the uri didn't have an
 *  explicit protocol, NULL will be returned.
 */

gchar *gnc_uri_get_protocol (const gchar *uri)
    GNC_DEPRECATED("Please use gnc_uri_get_scheme instead (since 3.4)");

/** Checks if there is a backend that explicitly stated to handle the given protocol.
 *
 *  @deprecated This function has been deprecated in gnucash 3.4. Please use gnc_uri_is_known_scheme instead.
 *
 *  @param protocol The protocol to check
 *
 *  @return TRUE if at least one backend explicitly handles this protocol, otherwise FALSE
 */
gboolean gnc_uri_is_known_protocol (const gchar *protocol)
    GNC_DEPRECATED("Please use gnc_uri_known_scheme instead (since 3.4)");


/** Checks if the given protocol is used to refer to a file
 *  (as opposed to a network service like a database or web url)
 *
 *  @deprecated This function has been deprecated in gnucash 3.4. Please use gnc_uri_is_file_scheme instead.
 *
 *  @param protocol The protocol to check
 *
 *  @return TRUE if the protocol is used with files, FALSE of the protocol
 *  is normally used with network services (database, web url,...)
 */
gboolean gnc_uri_is_file_protocol (const gchar *protocol)
    GNC_DEPRECATED("Please use gnc_uri_is_file_scheme instead (since 3.4)");

/** @} */

#endif /* GNCURIUTILS_H_ */
/** @} */
/** @} */

