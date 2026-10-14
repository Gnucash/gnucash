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
// SecKeychain* are deprecated
#pragma GCC diagnostic warning "-Wdeprecated-declarations"
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

#ifdef HAVE_OSX_KEYCHAIN
/* Log an OSStatus from the macOS keychain as a readable message. */
static void
log_osx_keychain_error (const gchar *context, OSStatus status)
{
    CFStringRef osx_resultstring = SecCopyErrorMessageString (status, NULL);
    char resultstring[256] = "";
    if (osx_resultstring)
    {
        CFStringGetCString (osx_resultstring, resultstring,
                            sizeof resultstring, kCFStringEncodingUTF8);
        CFRelease (osx_resultstring);
    }
    PWARN ("OS X keychain error in %s (status %d): %s",
           context, (int) status, resultstring);
}
#endif

#if defined(HAVE_LIBSECRET) || defined(HAVE_GNOME_KEYRING) || defined(HAVE_OSX_KEYCHAIN)
/* Return a copy of the password currently stored in the keyring for these
 * coordinates, or NULL if none is stored; the caller frees it with g_free().
 * Unlike gnc_keyring_get_password() this performs no user interaction. */
static gchar *
keyring_lookup_password (const gchar *access_method, const gchar *server,
                             guint32 port, const gchar *service,
                             const gchar *user)
{
    gchar *stored = NULL;
#ifdef HAVE_LIBSECRET
    GError *error = NULL;
    gchar *found;
    if (port == 0)
        found = secret_password_lookup_sync (SECRET_SCHEMA_GNUCASH, NULL, &error,
                                             "protocol", access_method,
                                             "server", server,
                                             "user", user,
                                             NULL);
    else
        found = secret_password_lookup_sync (SECRET_SCHEMA_GNUCASH, NULL, &error,
                                             "protocol", access_method,
                                             "server", server,
                                             "port", port,
                                             "user", user,
                                             NULL);
    if (found)
    {
        stored = g_strdup (found);
        secret_password_free (found);
    }
    if (error)
        g_error_free (error);
#elif HAVE_GNOME_KEYRING
    GList *found_list = NULL;
    if (gnome_keyring_find_network_password_sync
            (user, NULL, server, service, access_method, NULL, port,
             &found_list) == GNOME_KEYRING_RESULT_OK && found_list)
    {
        GnomeKeyringNetworkPasswordData *found = found_list->data;
        if (found->password)
            stored = g_strdup (found->password);
    }
    if (found_list)
        gnome_keyring_network_password_list_free (found_list);
#endif
#ifdef HAVE_OSX_KEYCHAIN
    void *password_data = NULL;
    UInt32 password_length = 0;
    if (SecKeychainFindInternetPassword (NULL,
                                         strlen (server), server,
                                         strlen (access_method), access_method,
                                         strlen (user), user,
                                         strlen (service), service,
                                         port, kSecProtocolTypeAny,
                                         kSecAuthenticationTypeDefault,
                                         &password_length, &password_data,
                                         NULL) == noErr)
    {
        stored = g_strndup (password_data, password_length);
        SecKeychainItemFreeContent (NULL, password_data);
    }
#endif
    return stored;
}
#endif /* any keyring backend */

#ifdef HAVE_OSX_KEYCHAIN
/* Store password for these coordinates in the macOS keychain, updating the item
 * if it already exists; a plain SecKeychainAddInternetPassword would fail with
 * errSecDuplicateItem. */
static OSStatus
store_osx_keychain_password (const gchar *access_method, const gchar *server,
                             guint32 port, const gchar *service,
                             const gchar *user, const gchar *password)
{
    SecKeychainItemRef itemRef = NULL;
    OSStatus status =
        SecKeychainFindInternetPassword (NULL,
                                         strlen (server), server,
                                         strlen (access_method), access_method,
                                         strlen (user), user,
                                         strlen (service), service,
                                         port, kSecProtocolTypeAny,
                                         kSecAuthenticationTypeDefault,
                                         NULL, NULL, &itemRef);
    if (status == errSecItemNotFound)
        status =
            SecKeychainAddInternetPassword (NULL,
                                            strlen (server), server,
                                            strlen (access_method),
                                            access_method,
                                            strlen (user), user,
                                            strlen (service), service,
                                            port, kSecProtocolTypeAny,
                                            kSecAuthenticationTypeDefault,
                                            strlen (password), password, NULL);
    else if (status == noErr)
        status =
            SecKeychainItemModifyAttributesAndData (itemRef, NULL,
                                                    strlen (password),
                                                    password);
    if (itemRef)
        CFRelease (itemRef);
    return status;
}
#endif /* HAVE_OSX_KEYCHAIN */

void gnc_keyring_set_password (const gchar *access_method,
                               const gchar *server,
                               guint32 port,
                               const gchar *service,
                               const gchar *user,
                               const gchar* password)
{
#if defined(HAVE_LIBSECRET) || defined(HAVE_GNOME_KEYRING) || defined(HAVE_OSX_KEYCHAIN)
    gchar *stored;
    gboolean unchanged;

    g_return_if_fail(access_method != NULL && server != NULL &&
                     service != NULL && user != NULL && password != NULL);

    /* Only (re)write the keyring when the stored password is missing or has
     * actually changed. */
    stored = keyring_lookup_password (access_method, server, port, service, user);
    unchanged = (g_strcmp0 (stored, password) == 0);
    g_free (stored);
    if (unchanged)
        return;
#endif

#ifdef HAVE_LIBSECRET
    {
        GError *error = NULL;
        gchar *label = g_strdup_printf ("GnuCash password for %s://%s@%s",
                                        access_method, user, server);
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
        g_free (label);
        if (error != NULL)
        {
            PWARN ("libsecret error: %s", error->message);
            PWARN ("The user will be prompted for a password again next time.");
            g_error_free (error);
        }
    }
#elif HAVE_GNOME_KEYRING
    {
        guint32 item_id = 0;
        GnomeKeyringResult gkr_result = gnome_keyring_set_network_password_sync
            (NULL, user, NULL, server, service,
             access_method, NULL, port, password, &item_id);
        if (gkr_result != GNOME_KEYRING_RESULT_OK)
        {
            PWARN ("Gnome-keyring error: %s",
                   gnome_keyring_result_to_message (gkr_result));
            PWARN ("The user will be prompted for a password again next time.");
        }
    }
#endif /* HAVE_GNOME_KEYRING */
#ifdef HAVE_OSX_KEYCHAIN
    {
        OSStatus status =
            store_osx_keychain_password (access_method, server, port, service,
                                         user, password);
        if (status != noErr)
        {
            log_osx_keychain_error ("gnc_keyring_set_password", status);
            PWARN ("The user will be prompted for a password again next time.");
        }
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
                                "protocol", PROJECT_NAME,
                                "server", PROJECT_NAME,
                                "user", PROJECT_NAME,
                                NULL);
    secret_password_clear_sync (SECRET_SCHEMA_GNUCASH, NULL, &error,
                                "protocol", PROJECT_NAME,
                                "server", PROJECT_NAME,
                                "user", PROJECT_NAME,
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
            log_osx_keychain_error ("gnc_keyring_get_password", status);
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
