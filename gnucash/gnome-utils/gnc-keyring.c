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


static gboolean
keyring_lookup_password (const gchar *access_method,
                                    const gchar *server,
                                    guint32 port,
                                    const gchar *service,
                                    gchar **user,
                                    gchar **password)
{
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
            CFStringRef osx_resultstring = SecCopyErrorMessageString( status, NULL );
            const gchar *resultstring = CFStringGetCStringPtr(osx_resultstring,
                                                              GetApplicationTextEncoding());
            PWARN ( "OS X keychain error: %s", resultstring );
            CFRelease ( osx_resultstring );
        }
    }
#endif /* HAVE_OSX_KEYCHAIN */

    return FALSE;
}


typedef struct
{
    gchar *user;
    gchar *password;
} GncKeyringCredentials;

typedef struct
{
    GTask *task;
    gchar *access_method;
    gchar *server;
    guint32 port;
    gchar *service;
    gchar *user;
    gboolean completed;
} GncKeyringRequest;

static void
gnc_keyring_credentials_free (GncKeyringCredentials *credentials)
{
    if (!credentials)
        return;

    g_free (credentials->user);
    g_free (credentials->password);
    g_free (credentials);
}

static void
gnc_keyring_request_free (GncKeyringRequest *request)
{
    g_clear_object (&request->task);
    g_free (request->access_method);
    g_free (request->server);
    g_free (request->service);
    g_free (request->user);
    g_free (request);
}

static void
gnc_keyring_request_complete (GncKeyringRequest *request,
                              GncKeyringCredentials *credentials)
{
    if (request->completed)
    {
        gnc_keyring_credentials_free (credentials);
        return;
    }
    request->completed = TRUE;
    g_task_return_pointer (request->task, credentials,
                           (GDestroyNotify)gnc_keyring_credentials_free);
    gnc_keyring_request_free (request);
}

static void
gnc_keyring_request_fail (GncKeyringRequest *request, GError *error)
{
    if (request->completed)
    {
        g_clear_error (&error);
        return;
    }
    request->completed = TRUE;
    if (error)
        g_task_return_error (request->task, error);
    else
        g_task_return_new_error (request->task, G_IO_ERROR,
                                 G_IO_ERROR_FAILED,
                                 "%s", _("Password retrieval failed."));
    gnc_keyring_request_free (request);
}

static void
gnc_keyring_password_entered (GObject *source, GAsyncResult *result,
                              gpointer user_data)
{
    GncKeyringRequest *request = user_data;
    GncKeyringCredentials *credentials;
    GError *error = NULL;

    (void)source;
    credentials = g_new0 (GncKeyringCredentials, 1);
    if (!gnc_get_username_password_finish (result, &credentials->user,
                                           &credentials->password, &error))
    {
        gnc_keyring_credentials_free (credentials);
        gnc_keyring_request_fail (request, error);
        return;
    }

    gnc_keyring_set_password (request->access_method, request->server,
                              request->port, request->service,
                              credentials->user, credentials->password);
    gnc_keyring_request_complete (request, credentials);
}

void
gnc_keyring_get_password_async (GtkWindow *parent,
                                const gchar *access_method,
                                const gchar *server,
                                guint32 port,
                                const gchar *service,
                                const gchar *user,
                                GCancellable *cancellable,
                                GAsyncReadyCallback callback,
                                gpointer user_data)
{
    GncKeyringRequest *request;
    GncKeyringCredentials *credentials;
    gchar *db_path;
    gchar *heading;

    g_return_if_fail (!parent || GTK_IS_WINDOW (parent));
    g_return_if_fail (access_method != NULL);
    g_return_if_fail (server != NULL);
    g_return_if_fail (service != NULL);

    request = g_new0 (GncKeyringRequest, 1);
    request->task = g_task_new (NULL, cancellable, callback, user_data);
    request->access_method = g_strdup (access_method);
    request->server = g_strdup (server);
    request->port = port;
    request->service = g_strdup (service);
    request->user = g_strdup (user);

    credentials = g_new0 (GncKeyringCredentials, 1);
    credentials->user = g_strdup (request->user);
    if (keyring_lookup_password (request->access_method, request->server,
                                 request->port, request->service,
                                 &credentials->user, &credentials->password))
    {
        gnc_keyring_request_complete (request, credentials);
        return;
    }
    gnc_keyring_credentials_free (credentials);

    if (port == 0)
        db_path = g_strdup_printf ("%s://%s/%s", access_method, server, service);
    else
        db_path = g_strdup_printf ("%s://%s:%d/%s", access_method, server,
                                   port, service);
    heading = g_strdup_printf (
        _("Enter a user name and password to connect to: %s"), db_path);
    gnc_get_username_password_async (parent, heading, request->user, NULL,
                                     cancellable, gnc_keyring_password_entered,
                                     request);
    g_free (heading);
    g_free (db_path);
}

gboolean
gnc_keyring_get_password_finish (GAsyncResult *result, gchar **user,
                                 gchar **password, GError **error)
{
    GncKeyringCredentials *credentials;

    g_return_val_if_fail (user != NULL, FALSE);
    g_return_val_if_fail (password != NULL, FALSE);
    g_return_val_if_fail (g_task_is_valid (result, NULL), FALSE);

    *user = NULL;
    *password = NULL;
    credentials = g_task_propagate_pointer (G_TASK (result), error);
    if (!credentials)
        return FALSE;

    *user = g_steal_pointer (&credentials->user);
    *password = g_steal_pointer (&credentials->password);
    gnc_keyring_credentials_free (credentials);
    return TRUE;
}
