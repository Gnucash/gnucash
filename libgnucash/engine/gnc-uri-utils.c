/*
 * gnc-uri-utils.c -- utility functions to convert uri in separate
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

#include <glib.h>
#include "gnc-uri-utils.h"
#include "gnc-filepath-utils.h"
#include "qofsession.h"

/* Checks if the given uri is a valid uri
 */
gboolean gnc_uri_is_uri (const gchar *uri)
{

    gchar *scheme = NULL, *hostname = NULL;
    gchar *username = NULL, *password = NULL;
    gchar *path     = NULL;
    gint   port = 0;
    gboolean is_uri = FALSE;

    gnc_uri_get_components ( uri, &scheme, &hostname, &port,
                             &username, &password, &path );

    /* For gnucash to consider a uri valid the following must be true:
     * - scheme and path must not be NULL
     * - for anything but local filesystem uris, hostname must be valid as well */
    is_uri = (scheme && path && (gnc_uri_is_file_scheme(scheme) || hostname));

    g_free (scheme);
    g_free (hostname);
    g_free (username);
    g_free (password);
    g_free (path);

    return is_uri;
}

/* Checks if the given scheme is used to refer to a file
 * (as opposed to a network service)
 */
gboolean gnc_uri_is_known_scheme (const gchar *scheme)
{
    gboolean is_known_scheme = FALSE;
    GList *node;
    GList *known_scheme_list = qof_backend_get_registered_access_method_list();

    for ( node = known_scheme_list; node != NULL; node = node->next )
    {
        gchar *known_scheme = node->data;
        if ( !g_ascii_strcasecmp (scheme, known_scheme) )
        {
            is_known_scheme = TRUE;
            break;
        }
    }

    g_list_free (known_scheme_list);
    return is_known_scheme;
}

/* Checks if the given scheme is used to refer to a file
 * (as opposed to a network service)
 * Note unknown schemes are always considered network schemes.
 *
 * *Compatibility note:*
 * This used to be the other way around before gnucash 3.4. Before
 * that unknown schemes were always considered local file system
 * uri schemes.
 */
gboolean gnc_uri_is_file_scheme (const gchar *scheme)
{
    return (scheme &&
            (!g_ascii_strcasecmp (scheme, "file") ||
             !g_ascii_strcasecmp (scheme, "xml") ||
             !g_ascii_strcasecmp (scheme, "sqlite3")));
}

/* Checks if the given uri defines a file
 * (as opposed to a network service)
 */
gboolean gnc_uri_is_file_uri (const gchar *uri)
{
    gchar *scheme = gnc_uri_get_scheme ( uri );
    gboolean result = gnc_uri_is_file_scheme ( scheme );

    g_free ( scheme );

    return result;
}

/* Checks if the given uri is a valid uri
 */
gboolean gnc_uri_targets_local_fs (const gchar *uri)
{

    gchar *scheme = NULL, *hostname = NULL;
    gchar *username = NULL, *password = NULL;
    gchar *path     = NULL;
    gint   port = 0;
    gboolean is_local_fs = FALSE;

    gnc_uri_get_components ( uri, &scheme, &hostname, &port,
                             &username, &password, &path );

    /* For gnucash to consider a uri to target the local fs:
     * path must not be NULL
     * AND
     *   scheme should be NULL
     *   OR
     *   scheme must be file type scheme (file, xml, sqlite) */
    is_local_fs = (path && (!scheme || gnc_uri_is_file_scheme(scheme)));

    g_free (scheme);
    g_free (hostname);
    g_free (username);
    g_free (password);
    g_free (path);

    return is_local_fs;
}

/* Splits a uri into its separate components */
void gnc_uri_get_components (const gchar *uri,
                             gchar **scheme,
                             gchar **hostname,
                             gint32 *port,
                             gchar **username,
                             gchar **password,
                             gchar **path)
{
    gchar **splituri;
    gchar *url = NULL, *tmpusername = NULL, *tmphostname = NULL;
    gchar *delimiter = NULL;

    *scheme   = NULL;
    *hostname = NULL;
    *port     = 0;
    *username = NULL;
    *password = NULL;
    *path     = NULL;

    g_return_if_fail( uri != NULL && strlen (uri) > 0);

    splituri = g_strsplit ( uri, "://", 2 );
    if ( splituri[1] == NULL )
    {
        /* No scheme means simple file path.
           Set path to copy of the input. */
        *path     = g_strdup ( uri );
        g_strfreev ( splituri );
        return;
    }

    /* At least a scheme was found, set it here */
    *scheme = g_strdup ( splituri[0] );

    if ( gnc_uri_is_file_scheme ( *scheme ) )
    {
        /* a true file uri on windows can start file:///N:/
           so we come here with /N:/, it could also be /N:\
        */
        if (g_str_has_prefix (splituri[1], "/") &&
           ((g_strstr_len (splituri[1], -1,  ":/") != NULL) || (g_strstr_len (splituri[1], -1,  ":\\") != NULL)))
        {
            gchar *ptr = splituri[1];
            *path = gnc_resolve_file_path ( ptr + 1 );
        }
        else
            *path = gnc_resolve_file_path ( splituri[1] );
        g_strfreev ( splituri );
        return;
    }

    /* Protocol indicates full network style uri, let's see if it
     * has a username and/or password
     */
    url = g_strdup (splituri[1]);
    g_strfreev ( splituri );

    /* Check for "@" sign, but start from the end - the password may contain
     * this sign as well
     */
    delimiter = g_strrstr ( url, "@" );
    if ( delimiter != NULL )
    {
        /* There is at least a username in the url */
        delimiter[0] = '\0';
        tmpusername = url;
        tmphostname = delimiter + 1;

        /* Check if there's a password too by looking for a :
         * Start from the beginning this time to avoid possible :
         * in the password */
        delimiter = g_strstr_len ( tmpusername, -1, ":" );
        if ( delimiter != NULL )
        {
            /* There is password in the url */
            delimiter[0] = '\0';
            *password = g_strdup ( (const gchar*)(delimiter + 1) );
        }
        *username = g_strdup ( (const gchar*)tmpusername );
    }
    else
    {
        /* No username and password were given */
        tmphostname = url;
    }

    /* Find the path part */
    delimiter = g_strstr_len ( tmphostname, -1, "/" );
    if ( delimiter != NULL )
    {
        delimiter[0] = '\0';
        if ( gnc_uri_is_file_scheme ( *scheme ) ) /* always return absolute file paths */
            *path = gnc_resolve_file_path ( (const gchar*)(delimiter + 1) );
        else /* path is no file path, so copy it as is */
            *path = g_strdup ( (const gchar*)(delimiter + 1) );
    }

    /* Check for a port specifier */
    delimiter = g_strstr_len ( tmphostname, -1, ":" );
    if ( delimiter != NULL )
    {
        delimiter[0] = '\0';
        *port = g_ascii_strtoll ( delimiter + 1, NULL, 0 );
    }

    *hostname = g_strdup ( (const gchar*)tmphostname );

    g_free ( url );

    return;

}

gchar *gnc_uri_get_scheme (const gchar *uri)
{
    gchar *scheme   = NULL;
    gchar *hostname = NULL;
    gint32 port     = 0;
    gchar *username = NULL;
    gchar *password = NULL;
    gchar *path     = NULL;

    gnc_uri_get_components ( uri, &scheme, &hostname, &port,
                             &username, &password, &path );

    g_free (hostname);
    g_free (username);
    g_free (password);
    g_free (path);

    return scheme;
}

gchar *gnc_uri_get_path (const gchar *uri)
{
    gchar *scheme   = NULL;
    gchar *hostname = NULL;
    gint32 port = 0;
    gchar *username = NULL;
    gchar *password = NULL;
    gchar *path     = NULL;

    gnc_uri_get_components ( uri, &scheme, &hostname, &port,
                             &username, &password, &path );

    g_free (scheme);
    g_free (hostname);
    g_free (username);
    g_free (password);

    return path;
}

/* Generates a normalized uri from the separate components */
gchar *gnc_uri_create_uri (const gchar *scheme,
                           const gchar *hostname,
                           gint32 port,
                           const gchar *username,
                           const gchar *password,
                           const gchar *path)
{
    gchar *userpass = NULL, *portstr = NULL, *uri = NULL;

    g_return_val_if_fail( path != 0, NULL );

    if (!scheme || gnc_uri_is_file_scheme (scheme))
    {
        /* Compose a file based uri, which means ignore everything but
         * the scheme and the path
         * We return an absolute pathname if the scheme is known or
         * no scheme was given. For an unknown scheme, we return the
         * path info as is.
         */
        gchar *abs_path;
        gchar *uri_scheme;
        if (scheme && (!gnc_uri_is_known_scheme (scheme)) )
            abs_path = g_strdup ( path );
        else
            abs_path = gnc_resolve_file_path ( path );

        if (!scheme)
            uri_scheme = g_strdup ("file");
        else
            uri_scheme = g_strdup (scheme);

        /* Arrive here with...
         *
         * /my/path/to/file with space.txt
         * becomes file:///my/path/to/file with space.txt
         *
         * c:\my\path\to\file with space.txt
         * becomes file:///c:\my\path\to\file with space.txt
         *
         * \\myserver\share\path\to\file with space.txt
         * becomes file://\\myserver\share\path\to\file with space.txt
         *
         * probably they should all be forward slashs and spaces escaped
         * also with UNC it could be file://myserver/share/path/to/file with space.txt
         */

        if (g_str_has_prefix (abs_path, "/") || g_str_has_prefix (abs_path, "\\"))
            uri = g_strdup_printf ( "%s://%s", uri_scheme, abs_path );
        else // for windows add an extra "/"
            uri = g_strdup_printf ( "%s:///%s", uri_scheme, abs_path );

        g_free (uri_scheme);
        g_free (abs_path);

        return uri;
    }

    /* Not a file based uri, we need to setup all components that are not NULL
     * For this scenario, hostname is mandatory.
     */
    g_return_val_if_fail( hostname != 0, NULL );

    if ( username != NULL && *username )
    {
        if ( password != NULL && *password )
            userpass = g_strdup_printf ( "%s:%s@", username, password );
        else
            userpass = g_strdup_printf ( "%s@", username );
    }
    else
        userpass = g_strdup ( "" );

    if ( port != 0 )
        portstr = g_strdup_printf ( ":%d", port );
    else
        portstr = g_strdup ( "" );

    // XXX Do I have to add the slash always or are there situations
    //     it is in the path already ?
    uri = g_strconcat ( scheme, "://", userpass, hostname, portstr, "/", path, NULL );

    g_free ( userpass );
    g_free ( portstr );

    return uri;

}

gchar *gnc_uri_normalize_uri (const gchar *uri, gboolean allow_password)
{
    gchar *scheme   = NULL;
    gchar *hostname = NULL;
    gint32 port = 0;
    gchar *username = NULL;
    gchar *password = NULL;
    gchar *path     = NULL;
    gchar *newuri   = NULL;

    gnc_uri_get_components ( uri, &scheme, &hostname, &port,
                             &username, &password, &path );
    if (allow_password)
        newuri = gnc_uri_create_uri ( scheme, hostname, port,
                                      username, password, path);
    else
        newuri = gnc_uri_create_uri ( scheme, hostname, port,
                                      username, /* no password */ NULL, path);

    g_free (scheme);
    g_free (hostname);
    g_free (username);
    g_free (password);
    g_free (path);

    return newuri;
}

gchar *gnc_uri_add_extension ( const gchar *uri, const gchar *extension )
{
    g_return_val_if_fail( uri != 0, NULL );

    /* Only add extension if the user provided the extension and the uri is
     * file based.
     */
    if ( !extension || !gnc_uri_is_file_uri( uri ) )
        return g_strdup( uri );

    /* Don't add extension if it's already there */
    if ( g_str_has_suffix( uri, extension ) )
        return g_strdup( uri );

    /* Ok, all tests passed, let's add the extension */
    return g_strconcat( uri, extension, NULL );
}


/* Deprecated functions
 * ********************/

/* replaced with gnc_uri_get_scheme */
gchar *gnc_uri_get_protocol (const gchar *uri)
{
    return gnc_uri_get_scheme (uri);
}

/* replaced with gnc_uri_is_known_scheme */
gboolean gnc_uri_is_known_protocol (const gchar *protocol)
{
    return gnc_uri_is_known_scheme(protocol);
}

/* replaced with gnc_uri_is_file_scheme */
gboolean gnc_uri_is_file_protocol (const gchar *protocol)
{
    return gnc_uri_is_file_scheme (protocol);
}
