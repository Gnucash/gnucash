/********************************************************************\
 * file-utils.c -- simple file utilities                            *
 * Copyright (C) 1997 Robin D. Clark <rclark@cs.hmc.edu>            *
 * Copyright (C) 1998 Rob Browning                                  *
 * Copyright (C) 1998-2000 Linas Vepstas <linas@linas.org>          *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
\********************************************************************/

#include "config.h"

#include <glib.h>
//#include <glib/gstdio.h>
//#include <errno.h>

#include "gnc-state.h"
//#include "gnc-engine.h"
#include "gnc-filepath-utils.h"
#include "gnc-gkeyfile-utils.h"
#include "gnc-uri-utils.h"
#include "qof.h"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = G_LOG_DOMAIN;

/* Absolute path to the state file for the current book
 * Before 2.4.1, this file didn't have an extension.
 * The code will look for such pre 2.4.0 file if no post 2.4.1
 * version is found. If there is an old style file, save the
 * name here as well. The old style state file will then be
 * converted into a new style one the next time state is saved.
 */
static gchar* state_file_name = NULL;
static gchar* state_file_name_pre_241 = NULL;
/* State file data for current book */
static GKeyFile *state_file = NULL;

/* Determine which file name to use for the state file. This name is based
 * the current book's uri and guid.
 *
 * The state files will be searched for in the books directory in GnuCash'
 * private configuration directory. This configuration directory is
 * platform dependent and can be overridden with environment variable
 * DOT_GNUCASH_DIR. On linux for example this is ~/.gnucash by default.
 *
 * The URL is used to compute the base name of the state file and the
 * guid is used to differentiate when the user has multiple data files
 * with the same name.
 *
 * As of GnuCash 2.4.1 state files will have their own extension to
 * differentiate them from data files saved by the user. New state
 * files will always be created with such an extension. But GnuCash
 * will continue to search for state files without an extension if
 * no proper state file with extension is found. */


static void
gnc_state_set_base (const QofSession *session)
{
    gchar *basename, *original = NULL, *filename, *file_guid;
    gchar *sf_extension = NULL, *newstyle_filename = NULL;
    const gchar *uri;
    gchar guid_string[GUID_ENCODING_LENGTH+1];
    QofBook *book;
    const GncGUID *guid;
    GKeyFile *key_file = NULL;
    gint i;

    /* Reset filenames possibly found in a previous run */
    g_free (state_file_name);
    g_free (state_file_name_pre_241);
    state_file_name = NULL;
    state_file_name_pre_241 = NULL;

    uri = qof_session_get_url(session);
    ENTER("session %p (%s)", session, uri ? uri : "(null)");
    if (!uri)
    {
        LEAVE("no uri, nothing to do");
        return;
    }

    /* Get the book GncGUID */
    book = qof_session_get_book(session);
    guid = qof_entity_get_guid(QOF_INSTANCE(book));
    guid_to_string_buff(guid, guid_string);

    if (gnc_uri_is_file_uri (uri))
    {
        /* The book_uri is a true file, use its basename. */
        gchar *path = gnc_uri_get_path (uri);
        basename = g_path_get_basename (path);
        g_free (path);
    }
    else
    {
        /* The book_uri is composed of database connection parameters. */
        gchar* protocol = NULL;
        gchar* host = NULL;
        gchar* dbname = NULL;
        gchar* username = NULL;
        gchar* password = NULL;
        gint portnum = 0;
        gnc_uri_get_components (uri, &protocol, &host, &portnum,
                                &username, &password, &dbname);

        basename = g_strjoin ("_", protocol, host, username, dbname, NULL);
        g_free (protocol);
        g_free (host);
        g_free (username);
        g_free (password);
        g_free (dbname);
    }

    DEBUG ("Basename %s", basename);
    original = gnc_build_book_path (basename);
    g_free (basename);
    DEBUG ("Original %s", original);

    sf_extension = g_strdup (STATE_FILE_EXT);
    i = 1;
    while (1)
    {
        if (i == 1)
            filename = g_strconcat (original, sf_extension, NULL);
        else
            filename = g_strdup_printf ("%s_%d%s", original, i, sf_extension);
        DEBUG ("Trying %s", filename);
        key_file = gnc_key_file_load_from_file (filename, TRUE, FALSE, NULL);
        DEBUG ("Result %p", key_file);

        if (!key_file)
        {
            DEBUG ("No key file by that name");
            if (g_strcmp0 (sf_extension, STATE_FILE_EXT) == 0)
            {
                DEBUG ("Trying old state file names for compatibility");
                i = 1;
                g_free ( sf_extension);
                sf_extension = g_strdup ("");

                /* Regardless of whether or not an old state file is found,
                 * the currently tested name should be used for the future
                 * state file.
                 */
                state_file_name = filename;
                continue;
            }

            /* No old style file found. We'll return with the new file name
             * we set earlier, and no existing key file. */
            g_free (filename);
            break;
        }

        file_guid = g_key_file_get_string (key_file,
                                           STATE_FILE_TOP, STATE_FILE_BOOK_GUID,
                                           NULL);
        DEBUG ("File GncGUID is %s", file_guid ? file_guid : "<not found>");
        if (g_strcmp0 (guid_string, file_guid) == 0)
        {
            DEBUG ("Matched !!!");
            /* Save the found file for later use. Which name to save to
             * depends on whether it was an old or new style file name
             */
            if (g_strcmp0 (sf_extension, STATE_FILE_EXT) == 0)
                state_file_name = filename;
            else
                state_file_name_pre_241 = filename;

            g_free (file_guid);
            break;
        }
        DEBUG ("Clean up this pass");
        g_free (file_guid);
        g_key_file_free (key_file);
        g_free (filename);
        i++;
    }

    DEBUG("Clean up");
    g_free(original);
    g_key_file_free (key_file);

    LEAVE ();
}

GKeyFile *gnc_state_load (const QofSession *session)
{

    GKeyFile *keyfile = NULL;
    GError *error = NULL;

    /* Drop possible previous state_file first */
    if (state_file)
    {
        g_key_file_free (state_file);
        state_file = NULL;
    }

    gnc_state_set_base (session);

    if (state_file_name_pre_241)
        state_file = gnc_key_file_load_from_file (state_file_name_pre_241,
                     TRUE, TRUE, NULL);
    else if (state_file_name)
        state_file = gnc_key_file_load_from_file (state_file_name,
                     TRUE, TRUE, NULL);

    return gnc_state_get_current ();

}

void gnc_state_save (const QofSession *session)
{
    GError *error = NULL;

    if (!qof_session_get_url(session))
    {
        DEBUG("No file associated with session - skip state saving");
        return;
    }

    gnc_state_set_base (session);

    /* Write it all out to disk */
    if (state_file_name)
        gnc_key_file_save_to_file(state_file_name, state_file, &error);
    else
        PWARN ("No state file name set, can't save state");

    if (error)
    {
        PERR ("Error: Failure saving state file.\n  %s",
              error->message);
        g_error_free (error);
    }
}

GKeyFile *gnc_state_get_current (void)
{
    if (!state_file)
    {
        PINFO ("No pre-existing state found, creating new one");
        state_file = g_key_file_new ();
    }

    return state_file;

}

gint gnc_state_drop_sections_for (const gchar *partial_name)
{
    gchar **groups;
    gint found_count = 0, dropped_count = 0;
    gsize i, num_groups;
    GError *error = NULL;

    if (!state_file)
    {
        PWARN ("No pre-existing state found, ignoring drop request");
        return 0;
    }

    ENTER("");

    groups = g_key_file_get_groups (state_file, &num_groups);
    for (i = 0; i < num_groups; i++)
    {
        if (g_strstr_len (groups[i], -1, partial_name))
        {
            DEBUG ("Section \"%s\" matches \"%s\", removing", groups[i], partial_name);
            found_count++;
            if (!g_key_file_remove_group (state_file, groups[i], &error))
            {
                PWARN ("Warning: unable to remove section %s.\n  %s",
                        groups[i],
                        error->message);
                g_error_free (error);
            }
            else
                dropped_count++;

        }
    }
    g_strfreev (groups);

    LEAVE("Found %i sections matching \"%s\", successfully removed %i",
            found_count, partial_name, dropped_count);
    return dropped_count;

}

