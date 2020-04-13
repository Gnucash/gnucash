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

#include <config.h>
#include <glib/gi18n.h>
#include "qof.h"
#include "gnc-ui.h"
#include "gnc-keyring.h"
#ifdef HAVE_LIBSECRET
#include <libsecret/secret.h>
#elif HAVE_GNOME_KEYRING
#define GNOME_KEYRING_DEPRECATED
#define GNOME_KEYRING_DEPRECATED_FOR(x)
#include <gnome-keyring.h>
#endif
#ifdef HAVE_OSX_KEYCHAIN
#include <Security/Security.h>
#include <CoreFoundation/CoreFoundation.h>
#include <Carbon/Carbon.h>
#endif

/* This static indicates the debugging module that this .o belongs to. */
G_GNUC_UNUSED static QofLogModule log_module = GNC_MOD_GUI;

#ifdef HAVE_LIBSECRET
const SecretSchema* gnucash_get_secret_schema(void) G_GNUC_CONST;
const SecretSchema* gnucash_get_secret_schema(void)
{
    static const SecretSchema secret_schema = {
        "org.gnucash.password", SECRET_SCHEMA_NONE,
        {
            { "protocol", SECRET_SCHEMA_ATTRIBUTE_STRING },
            { "server", SECRET_SCHEMA_ATTRIBUTE_STRING },
            { "port", SECRET_SCHEMA_ATTRIBUTE_INTEGER },
            { "user", SECRET_SCHEMA_ATTRIBUTE_STRING },
            { "NULL", 0 },
        }
    };

    return &secret_schema;
}

#define SECRET_SCHEMA_GNUCASH gnucash_get_secret_schema()
#endif

void gnc_keyring_set_password (const gchar *access_method,
                               const gchar *server,
                               guint32 port,
                               const gchar *service,
                               const gchar *user,
                               const gchar* password)
{
#ifdef HAVE_LIBSECRET
    GError* error = NULL;
    gchar* label = NULL;

    g_return_if_fail(access_method != NULL && server != NULL &&
                     service != NULL && user != NULL && password != NULL);

    label = g_strdup_printf("GnuCash password for %s://%s@%s", access_method, user, server);

    if (port == 0)
        secret_password_store_sync (SECRET_SCHEMA_GNUCASH, SECRET_COLLECTION_DEFAULT,
                                    label, password, NULL, &error,
                                    "protocol", access_method,
                                    "server", server,
                                    "user", user,
                                    NULL);
    else
        secret_password_store_sync (SECRET_SCHEMA_GNUCASH, SECRET_COLLECTION_DEFAULT,
                                    label, password, NULL, &error,
                                    "protocol", access_method,
                                    "server", server,
                                    "port", port,
                                    "user", user,
                                    NULL);

    g_free(label);

    if (error != NULL)
    {
        PWARN ("libsecret error: %s", error->message);
        PWARN ("The user will be prompted for a password again next time.");
        g_error_free(error);
    }
#elif HAVE_GNOME_KEYRING
    GnomeKeyringResult  gkr_result;
    guint32 item_id = 0;

    g_return_if_fail(access_method != NULL && server != NULL &&
                     service != NULL && user != NULL && password != NULL);

    gkr_result = gnome_keyring_set_network_password_sync
        (NULL, user, NULL, server, service,
         access_method, NULL, port, password, &item_id);

    if (gkr_result != GNOME_KEYRING_RESULT_OK)
    {
        PWARN ("Gnome-keyring error: %s",
               gnome_keyring_result_to_message(gkr_result));
        PWARN ("The user will be prompted for a password again next time.");
    }
#endif /* HAVE_GNOME_KEYRING */
#ifdef HAVE_OSX_KEYCHAIN
    OSStatus status;
    SecKeychainItemRef *itemRef = NULL;

    g_return_if_fail(access_method != NULL && server != NULL &&
                     service != NULL && user != NULL && password != NULL);
    /* mysql and postgres aren't valid protocols on Mac OS X.
     * So we use the security domain parameter to allow us to
     * distinguish between these two.
     */
    // FIXME I'm not sure this works if a password was already in the keychain
    //       I may have to do a lookup first and if it exists, run some
    //       update function instead
    status =
        SecKeychainAddInternetPassword (NULL, /* keychain */
                                        strlen(server), server, /* servername */
                                        strlen(access_method),
                                        access_method,  /* securitydomain */
                                        strlen(user), user, /* accountname */
                                        strlen(service), service, /* path */
                                        port, /* port */
                                        kSecProtocolTypeAny, /* protocol */
                                        kSecAuthenticationTypeDefault, /* auth type */
                                        strlen(password),
                                        password, /* passworddata */
                                        itemRef );

    if ( status != noErr )
    {
        CFStringRef osx_resultstring = SecCopyErrorMessageString( status, NULL );
        const gchar *resultstring =
            CFStringGetCStringPtr(osx_resultstring,
                                  GetApplicationTextEncoding());
        PWARN ( "OS X keychain error: %s", resultstring );
        PWARN ( "The user will be prompted for a password again next time." );
        CFRelease ( osx_resultstring );
    }
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
    gchar *db_path, *heading;
#ifdef HAVE_LIBSECRET
    GError* error = NULL;
    char* libsecret_password;
#elif HAVE_GNOME_KEYRING
    GnomeKeyringResult  gkr_result;
    GList *found_list = NULL;
    GnomeKeyringNetworkPasswordData *found;
#endif
#ifdef HAVE_OSX_KEYCHAIN
    void *password_data;
    UInt32 password_length;
    OSStatus status;
#endif

    g_return_val_if_fail (user != NULL, FALSE);
    g_return_val_if_fail (password != NULL, FALSE);

    *password = NULL;

#ifdef HAVE_LIBSECRET
    /* Workaround for https://bugs.gnucash.org/show_bug.cgi?id=746873
     * and by extension for https://bugs.gnucash.org/show_bug.cgi?id=748625
     * Store a dummy password and delete it again. This forces libsecret
     * to open the keychain, where only a call to secret_password_lookup_sync
     * sometimes fails to do so. More details can be found in the bug reports
     * referenced above. */
    secret_password_store_sync (SECRET_SCHEMA_GNUCASH, SECRET_COLLECTION_DEFAULT,
                                "Dummy password", "dummy", NULL, &error,
                                "protocol", "gnucash",
                                "server", "gnucash",
                                "user", "gnucash",
                                NULL);
    secret_password_clear_sync (SECRET_SCHEMA_GNUCASH, NULL, &error,
                                "protocol", "gnucash",
                                "server", "gnucash",
                                "user", "gnucash",
                                NULL);

    /* Note: only use the port attribute if it  was set by the user. */
    if (port == 0)
        libsecret_password = secret_password_lookup_sync (SECRET_SCHEMA_GNUCASH, NULL, &error,
                                                          "protocol", access_method,
                                                          "server", server,
                                                          "user", *user,
                                                          NULL);
    else
        libsecret_password = secret_password_lookup_sync (SECRET_SCHEMA_GNUCASH, NULL, &error,
                                                          "protocol", access_method,
                                                          "server", server,
                                                          "port", port,
                                                          "user", *user,
                                                          NULL);

    if (libsecret_password != NULL) {
        *password = g_strdup (libsecret_password);
        secret_password_free (libsecret_password);
        return TRUE;
    }

    /* No password found yet. Perhaps it was written with a port equal to 0.
     * Gnucash versions prior to 2.6.7 did this unfortunately... */
    libsecret_password = secret_password_lookup_sync (SECRET_SCHEMA_GNUCASH, NULL, &error,
                                                      "protocol", access_method,
                                                      "server", server,
                                                      "port", 0,
                                                      "user", *user,
                                                      NULL);

    if (libsecret_password != NULL) {
        *password = g_strdup (libsecret_password);
        secret_password_free (libsecret_password);

        /* Ok, got an password with 0 port.
           Store a copy in a more recent gnucash style. */
        gnc_keyring_set_password(access_method, server, port, service, *user, *password);
        return TRUE;
    }

    /* No password was found while querying libsecret using the gnucash schema,
       Look for a password stored via gnome-keyring instead */
    if (port == 0)
        libsecret_password = secret_password_lookup_sync (SECRET_SCHEMA_COMPAT_NETWORK, NULL, &error,
                                                          "protocol", access_method,
                                                          "server", server,
                                                          "object", service,
                                                          "user", *user,
                                                          NULL);
    else
        libsecret_password = secret_password_lookup_sync (SECRET_SCHEMA_COMPAT_NETWORK, NULL, &error,
                                                          "protocol", access_method,
                                                          "server", server,
                                                          "port", port,
                                                          "object", service,
                                                          "user", *user,
                                                          NULL);

    if (libsecret_password != NULL) {
        *password = g_strdup (libsecret_password);
        secret_password_free (libsecret_password);

        /* Ok, got an old gnome-keyring password.
         * Store a copy of it in a libsecret compatible format. */
        gnc_keyring_set_password(access_method, server, port, service, *user, *password);
        return TRUE;
    }

    /* Something went wrong while attempting to access libsecret
     * Log the error message and carry on... */
    if (error != NULL) {
        PWARN ("libsecret access failed: %s.", error->message);
        g_error_free(error);
    }

#elif HAVE_GNOME_KEYRING
    gkr_result = gnome_keyring_find_network_password_sync
        ( *user, NULL, server, service,
          access_method, NULL, port, &found_list );

    if (gkr_result == GNOME_KEYRING_RESULT_OK)
    {
        found = (GnomeKeyringNetworkPasswordData *) found_list->data;
        if (found->password)
            *password = g_strdup(found->password);
        gnome_keyring_network_password_list_free(found_list);
        return TRUE;
    }

    /* Something went wrong while attempting to access libsecret
     * Log the error message and carry on... */
    PWARN ("Gnome-keyring access failed: %s.",
           gnome_keyring_result_to_message(gkr_result));
    gnome_keyring_network_password_list_free(found_list);
#endif /* HAVE_LIBSECRET or HAVE_GNOME_KEYRING */

#ifdef HAVE_OSX_KEYCHAIN
    /* mysql and postgres aren't valid protocols on Mac OS X.
     * So we use the security domain parameter to allow us to
     * distinguish between these two.
     */
    if (*user != NULL)
    {
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
            *password = g_strndup(password_data, password_length);
            SecKeychainItemFreeContent(NULL, password_data);
            return TRUE;
        }
        else
        {
            CFStringRef osx_resultstring = SecCopyErrorMessageString( status, NULL );
            const gchar *resultstring = CFStringGetCStringPtr(osx_resultstring,
                                                              GetApplicationTextEncoding());
            PWARN ( "OS X keychain error: %s", resultstring );
            CFRelease ( osx_resultstring );
        }
    }
#endif /* HAVE_OSX_KEYCHAIN */

    /* If we got here, either no proper password store is
     * available on this system, or we couldn't retrieve
     * a password from it. In both cases, just ask the user
     * to enter one
     */

    if ( port == 0 )
        db_path = g_strdup_printf ( "%s://%s/%s", access_method, server, service );
    else
        db_path = g_strdup_printf ( "%s://%s:%d/%s", access_method, server, port, service );
    heading = g_strdup_printf ( /* Translators: %s is a path to a database or any other url,
                                   like mysql://user@server.somewhere/somedb, https://www.somequotes.com/thequotes */
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

    return password_found;
}
