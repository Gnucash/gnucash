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

/* Checks if the given protocol is used to refer to a file
 * (as opposed to a network service)
 */
gboolean gnc_uri_is_file_protocol (const gchar *protocol)
{
    if ( !g_ascii_strcasecmp (protocol, "file") ||
         !g_ascii_strcasecmp (protocol, "xml") ||
         !g_ascii_strcasecmp (protocol, "sqlite3")
         )
        return TRUE;
    else
        return FALSE;
}

/* Checks if the given uri defines a file
 * (as opposed to a network service)
 */
gboolean gnc_uri_is_file_uri (const gchar *uri)
{
    gchar *protocol = gnc_uri_get_protocol ( uri );
    gboolean result = gnc_uri_is_file_protocol ( protocol );

    g_free ( protocol );

    return result;
}

/* Splits a uri into its separate components */
void gnc_uri_get_components (const gchar *uri,
                             gchar **protocol,
                             gchar **hostname,
                             guint32 port,
                             gchar **username,
                             gchar **password,
                             gchar **path)
{
    gchar **splituri, **spliturl;
    gchar *url = NULL, *tmpusername = NULL, *tmphostname = NULL;
    gchar *delimiter = NULL;

    *protocol = NULL;
    *hostname = NULL;
    port      = 0;
    *username = NULL;
    *password = NULL;
    *path     = NULL;

    g_return_if_fail( uri != 0 );

    splituri = g_strsplit ( uri, "://", 2 );
    if ( splituri[1] == NULL )
    {
        /* No protocol means simple file uri */
        *protocol = g_strdup ( "file" );
        *path     = g_strdup ( splituri[0] );
        g_strfreev ( splituri );
        return;
    }

    /* At least a protocol was found, set it here */
    *protocol = g_strdup ( splituri[0] );

    if ( gnc_uri_is_file_protocol ( *protocol ) )
    {
        /* Protocol indicates file based uri */
        *path     = gnc_resolve_file_path ( splituri[1] );
        g_strfreev ( splituri );
        return;
    }

    /* Protocol indicates full network style uri, let's see if it
     * has a username and/or password
     */
    url = g_strdup (splituri[1]);
    g_strfreev ( splituri );

    delimiter = g_strrstr ( url, "@" );
    if ( delimiter != NULL )
    {
        /* There is at least a username in the url */
        delimiter[0] = '\0';
        tmpusername = url;
        tmphostname = delimiter + 1;

        /* Check if there's a password too */
        delimiter = g_strstr_len ( tmpusername, -1, ":" );
        if ( delimiter != NULL )
        {
            /* There is password in the url */
            delimiter[0] = '\0';
            *username = g_strdup ( (const gchar*)tmpusername );
            *password = g_strdup ( (const gchar*)(delimiter+1) );
        }
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
         if ( gnc_uri_is_file_protocol ( *protocol ) ) /* always return absolute file paths */
             *path = gnc_resolve_file_path ( (const gchar*)(delimiter+1) );
         else /* path is no file path, so copy it as is */
             *path = g_strdup ( (const gchar*)(delimiter+1) );
    }

    /* Check for a port specifier */
    delimiter = g_strstr_len ( tmphostname, -1, ":" );
    if ( delimiter != NULL )
    {
         delimiter[0] = '\0';
         port = g_ascii_strtoll ( (const gchar*)(delimiter+1), NULL, 0 );
    }

    *hostname = g_strdup ( (const gchar*)tmphostname );

    g_free ( url );

    return;

}

gchar *gnc_uri_get_protocol (const gchar *uri)
{
    gchar *protocol = NULL;
    gchar *hostname = NULL;
    guint32 port = 0;
    gchar *username = NULL;
    gchar *password = NULL;
    gchar *path     = NULL;

    gnc_uri_get_components ( uri, &protocol, &hostname, port,
                             &username, &password, &path );

    g_free (hostname);
    g_free (username);
    g_free (password);
    g_free (path);

    return protocol;
}

gchar *gnc_uri_get_path (const gchar *uri)
{
    gchar *protocol = NULL;
    gchar *hostname = NULL;
    guint32 port = 0;
    gchar *username = NULL;
    gchar *password = NULL;
    gchar *path     = NULL;

    gnc_uri_get_components ( uri, &protocol, &hostname, port,
                             &username, &password, &path );

    g_free (protocol);
    g_free (hostname);
    g_free (username);
    g_free (password);

    return path;
}

/* Generates a normalized uri from the separate components */
gchar *gnc_uri_create_uri (const gchar *protocol,
                           const gchar *hostname,
                           guint32 port,
                           const gchar *username,
                           const gchar *password,
                           const gchar *path)
{
    gchar *userpass=NULL, *uri=NULL;

    g_return_val_if_fail( path != 0, NULL );

    if ( (protocol == NULL) || gnc_uri_is_file_protocol ( protocol ) )
    {
        /* Compose a file based uri, which means ignore everything but
         * the protocol and the path
         * We always return absolute pathnames
         */
        gchar *abs_path = gnc_resolve_file_path ( path );
        if ( protocol == NULL )
            uri = g_strdup_printf ( "file://%s", abs_path );
        else
            uri = g_strdup_printf ( "%s://%s", protocol, abs_path );
        g_free (abs_path);
        return uri;
    }

    /* Not a file based uri, we need to setup all components that are not NULL
     * For this scenario, hostname is mandatory.
     */
    g_return_val_if_fail( hostname != 0, NULL );

    if ( username != NULL )
    {
        if ( password != NULL )
            userpass = g_strdup_printf ( "%s:%s@", username, password);
        else
            userpass = g_strdup_printf ( "%s@", username);
    }

    // XXX Do I have to add the slash always or are there situations
    //     it is in the path already ?
    uri = g_strconcat ( protocol, "://", userpass, hostname, "/", path, NULL );

    g_free ( userpass );

    return uri;

}

gchar *gnc_uri_normalize_uri (const gchar *uri, gboolean allow_password)
{
    gchar *protocol = NULL;
    gchar *hostname = NULL;
    guint32 port = 0;
    gchar *username = NULL;
    gchar *password = NULL;
    gchar *path     = NULL;
    gchar *newuri   = NULL;

    gnc_uri_get_components ( uri, &protocol, &hostname, port,
                             &username, &password, &path );
    if (allow_password)
        newuri = gnc_uri_create_uri ( protocol, hostname, port,
                                      username, password, path);
    else
        newuri = gnc_uri_create_uri ( protocol, hostname, port,
                                      username, /* no password */ NULL, path);

    g_free (protocol);
    g_free (hostname);
    g_free (username);
    g_free (password);
    g_free (path);

    return newuri;
}
