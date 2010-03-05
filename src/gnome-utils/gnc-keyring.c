/*
 * gnc-keyring.c -- utility functions to store and retrieve passwords.
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

#include "config.h"
#include <glib/gi18n.h>
#include "qof.h"
#include "gnc-ui.h"
#include "gnc-keyring.h"
#ifdef HAVE_GNOME_KEYRING
#include <gnome-keyring.h>
#endif
#ifdef HAVE_OSX_KEYCHAIN
#include <SecKeychain.h>
#endif

/* This static indicates the debugging module that this .o belongs to. */
static QofLogModule log_module = GNC_MOD_GUI;

void gnc_keyring_set_password (const gchar *access_method,
                               const gchar *server,
                               guint32 port,
                               const gchar *service,
                               const gchar *user,
                               const gchar* password)
{

#ifdef HAVE_GNOME_KEYRING
    GnomeKeyringResult  gkr_result;
    guint32 *item_id = NULL;

    gkr_result = gnome_keyring_set_network_password_sync
                       (NULL, user, NULL, server, service,
                        access_method, NULL, port, password, item_id);

    if (gkr_result != GNOME_KEYRING_RESULT_OK)
    {
        PWARN ("Gnome-keyring error: %s",
                gnome_keyring_result_to_message(gkr_result));
        PWARN ("The user will be prompted for a password again next time.");
    }
#endif /* HAVE_GNOME_KEYRING */
#ifdef HAVE_OSX_KEYCHAIN
#  ifdef 0
    /* FIXME The OSX part hasn't been tested yet */
    OSStatus status;
    SecKeychainItemRef *itemRef = NULL;

    /* mysql and postgres aren't valid protocols on Mac OS X.
     * So we use the security domain parameter to allow us to
     * distinguish between these two.
     */
    // FIXME I'm not sure this works if a password was already in the keychain
    //       I may have to do a lookup first and if it exists, run some update
    //       update function instead
    g_set_application_name(PACKAGE);
    status = SecKeychainAddInternetPassword ( NULL, /* keychain */
               strlen(server), server,              /* servername */
               strlen(access_method), access_method,/* securitydomain */
               strlen(*user), *user,                /* acountname */
               strlen(service), service,            /* path */
               port,                                /* port */
               kSecProtocolTypeAny,                 /* protocol */
               kSecAuthenticationTypeDefault,       /* auth type */
               strlen(password), password,          /* passworddata */
               SecKeychainItemRef *itemRef );

    if ( status != noErr )
    {
        CFStringRef resultstring = SecCopyErrorMessageString( status, NULL );
        PWARN ( "OS X keychain error: %s", resultstring );
        PWARN ( "The user will be prompted for a password again next time." );
        CFRelease ( resultstring );
    }


#  endif /* 0 */
#endif /* HAVE_OSX_KEYCHAIN */
}


gboolean gnc_keyring_get_password ( GtkWidget *parent,
                                    const gchar *access_method,
                                    const gchar *server,
                                    guint32 port,
                                    const gchar *service,
                                    gchar **user,
                                    gchar **password)
{
    gboolean password_found = FALSE;
#ifdef HAVE_GNOME_KEYRING
    GnomeKeyringResult  gkr_result;
    GList *found_list = NULL;
    GnomeKeyringNetworkPasswordData *found;
#endif
#ifdef HAVE_OSX_KEYCHAIN
#  ifdef 0
    /* FIXME The OSX part hasn't been tested yet */
    void *password_data;
    UInt32 password_length;
    OSStatus status;
#  endif /* 0 */
#endif

    g_return_val_if_fail (user != NULL, FALSE);
    g_return_val_if_fail (password != NULL, FALSE);

    *password = NULL;

#ifdef HAVE_GNOME_KEYRING
    g_set_application_name(PACKAGE);
    gkr_result = gnome_keyring_find_network_password_sync
                       ( *user, NULL, server, service,
                        access_method, NULL, port, &found_list );

    if (gkr_result == GNOME_KEYRING_RESULT_OK)
    {
        found = (GnomeKeyringNetworkPasswordData *) found_list->data;
        if (found->password)
            *password = g_strdup(found->password);
        password_found = TRUE;
    }
    else
        PWARN ("Gnome-keyring access failed: %s.",
                gnome_keyring_result_to_message(gkr_result));

    gnome_keyring_network_password_list_free(found_list);
#endif /* HAVE_GNOME_KEYRING */
#ifdef HAVE_OSX_KEYCHAIN
#  ifdef 0
    /* FIXME The OSX part hasn't been tested yet */
    void *password_data;
    UInt32 password_length;
    OSStatus status;

    /* mysql and postgres aren't valid protocols on Mac OS X.
     * So we use the security domain parameter to allow us to
     * distinguish between these two.
     */
    status = SecKeychainFindInternetPassword( NULL,
                strlen(server), server,
                strlen(access_method), access_method,
                strlen(*user), *user,
                strlen(service), service,
                port,
                kSecProtocolTypeAny,
                kSecAuthenticationTypeDefault,
                &password_length, &password_data,
                NULL);

    if ( status == noErr )
    {
        *password = xmalloc((password_length + 1) * sizeof(char));
        strncpy(*password, password_data, (size_t)password_length);
        (*password)[password_length] = '\0';
        password_found = TRUE;
        SecKeychainItemFreeContent(NULL, password_data);
    }
    else
    {
        CFStringRef resultstring = SecCopyErrorMessageString( status, NULL );
        PWARN ( "OS X keychain error: %s", resultstring );
        CFRelease ( resultstring );
    }

#  endif /* 0 */
#endif /* HAVE_OSX_KEYCHAIN */

    if ( !password_found )
    {
        /* If we got here, either no proper password store is
         * available on this system, or we couldn't retrieve
         * a password from it. In both cases, just ask the user
         * to enter one
         */
        gchar *db_path, *heading;

        if ( port == 0 )
            db_path=g_strdup_printf ( "%s://%s/%s", access_method, server, service );
        else
            db_path=g_strdup_printf ( "%s://%s:%d/%s", access_method, server, port, service );
        heading = g_strdup_printf ( /* Translators: %s is a path to a database or any other url,
                 like mysql://user@server.somewhere/somedb, http://www.somequotes.com/thequotes */
                _("Enter a user name and password to connect to: %s"),
                             db_path );

        password_found = gnc_get_username_password ( parent, heading,
                                                     *user, NULL,
                                                     user, password );
        g_free ( db_path );
        g_free ( heading );

        if ( password_found )
        {
            /* User entered new user/password information
             * Let's try to add it to a password store.
             */
            gchar *newuser = g_strdup( *user );
            gchar *newpassword = g_strdup( *password );
            gnc_keyring_set_password ( access_method,
                                       server,
                                       port,
                                       service,
                                       newuser,
                                       newpassword );
            g_free ( newuser );
            g_free ( newpassword );
        }
    }

    return password_found;
}
