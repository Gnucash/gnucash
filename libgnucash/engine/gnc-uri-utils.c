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

/* Checks if the given protocol is used to refer to a file
 * (as opposed to a network service)
 */
gboolean gnc_uri_is_known_protocol (const gchar *protocol)
{
    gboolean is_known_proto = FALSE;
    GList *node;
    GList *known_proto_list = qof_backend_get_registered_access_method_list();

    for ( node = known_proto_list; node != NULL; node = node->next )
    {
        gchar *known_proto = node->data;
        if ( !g_ascii_strcasecmp (protocol, known_proto) )
        {
            is_known_proto = TRUE;
            break;
        }
    }

    g_list_free (known_proto_list);
    return is_known_proto;
}

/* Checks if the given protocol is used to refer to a file
 * (as opposed to a network service)
 * For simplicity, handle all unknown protocols as if it were
 * file based protocols. This will avoid password lookups and such.
 */
gboolean gnc_uri_is_file_protocol (const gchar *protocol)
{
    if ( !g_ascii_strcasecmp (protocol, "mysql") ||
            !g_ascii_strcasecmp (protocol, "postgres")
       )
        return FALSE;
    else
        return TRUE;
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
                             gint32 *port,
                             gchar **username,
                             gchar **password,
                             gchar **path)
{
    gchar **splituri;
    gchar *url = NULL, *tmpusername = NULL, *tmphostname = NULL;
    gchar *delimiter = NULL;

    *protocol = NULL;
    *hostname = NULL;
    *port      = 0;
    *username = NULL;
    *password = NULL;
    *path     = NULL;

    g_return_if_fail( uri != NULL && strlen (uri) > 0);

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
        /* Protocol indicates file based uri.
         * Note that unknown protocols are treated as if they are
         * file-based protocols. This is done to prevent password
         * lookups on unknown protocols.
         * On the other hand, since we don't know the specifics of
         * unknown protocols, we don't attempt to return an absolute
         * pathname for them, just whatever was there.
         */
        if ( gnc_uri_is_known_protocol ( *protocol ) )
            *path     = gnc_resolve_file_path ( splituri[1] );
        else
            *path     = g_strdup ( splituri[1] );
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
        if ( gnc_uri_is_file_protocol ( *protocol ) ) /* always return absolute file paths */
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

gchar *gnc_uri_get_protocol (const gchar *uri)
{
    gchar *protocol = NULL;
    gchar *hostname = NULL;
    gint32 port     = 0;
    gchar *username = NULL;
    gchar *password = NULL;
    gchar *path     = NULL;

    gnc_uri_get_components ( uri, &protocol, &hostname, &port,
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
    gint32 port = 0;
    gchar *username = NULL;
    gchar *password = NULL;
    gchar *path     = NULL;

    gnc_uri_get_components ( uri, &protocol, &hostname, &port,
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
                           gint32 port,
                           const gchar *username,
                           const gchar *password,
                           const gchar *path)
{
    gchar *userpass = NULL, *portstr = NULL, *uri = NULL;

    g_return_val_if_fail( path != 0, NULL );

    if ( (protocol == NULL) || gnc_uri_is_file_protocol ( protocol ) )
    {
        /* Compose a file based uri, which means ignore everything but
         * the protocol and the path
         * We return an absolute pathname if the protocol is known or
         * no protocol was given. For an unknown protocol, we return the
         * path info as is.
         */
        gchar *abs_path;
        if ( protocol && (!gnc_uri_is_known_protocol (protocol)) )
            abs_path = g_strdup ( path );
        else
            abs_path = gnc_resolve_file_path ( path );
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
    uri = g_strconcat ( protocol, "://", userpass, hostname, portstr, "/", path, NULL );

    g_free ( userpass );
    g_free ( portstr );

    return uri;

}

gchar *gnc_uri_normalize_uri (const gchar *uri, gboolean allow_password)
{
    gchar *protocol = NULL;
    gchar *hostname = NULL;
    gint32 port = 0;
    gchar *username = NULL;
    gchar *password = NULL;
    gchar *path     = NULL;
    gchar *newuri   = NULL;

    gnc_uri_get_components ( uri, &protocol, &hostname, &port,
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
