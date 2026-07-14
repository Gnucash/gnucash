/********************************************************************\
 * gnc-state.cpp -- functions to manage gui state                   *
 * Copyright (C) 1997 Robin D. Clark <rclark@cs.hmc.edu>            *
 * Copyright (C) 1998 Rob Browning                                  *
 * Copyright (C) 1998-2000 Linas Vepstas <linas@linas.org>          *
 * Copyright (C) 2026 Brent McBride <mcbridebt@hotmail.com>         *
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

#include <config.h>

#include <glib.h>
#include <filesystem>
#include <format>
#include <memory>
#include <optional>
#include <span>
#include <string>
#include <string_view>

#include "gnc-state.h"
#include "gnc-filepath-utils.h"
#include "gnc-gkeyfile-utils.h"
#include "gnc-uri.hpp"
#include <guid.hpp>
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
static std::optional<std::string> s_state_file_name;
static std::optional<std::string> s_state_file_name_pre_241;
/* State file data for current book */
static GKeyFile *s_state_file = nullptr;

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
    /* Reset filenames possibly found in a previous run */
    s_state_file_name.reset ();
    s_state_file_name_pre_241.reset ();

    const char *uri = qof_session_get_url (session);
    ENTER("session %p (%s)", session, uri ? uri : "(null)");
    if (!uri || !*uri)
    {
        LEAVE("no uri, nothing to do");
        return;
    }

    /* Get the book GncGUID */
    QofBook *book = qof_session_get_book (session);
    const GncGUID *guid = qof_entity_get_guid (QOF_INSTANCE (book));

    std::string basename;
    GncUri parsed_uri {uri};
    if (parsed_uri.targets_local_fs ())
    {
        /* The book_uri is a true file, use its basename. */
        basename = std::filesystem::path {*parsed_uri.path ()}
                       .filename ().string ();
    }
    else
    {
        /* The book_uri is composed of database connection parameters.
         * Join the scheme, host, username and dbname with underscores. As
         * with the historical g_strjoin, an absent component terminates the
         * name, so anything following a missing component is dropped. */
        for (const auto& component : {parsed_uri.scheme (),
                                      parsed_uri.hostname (),
                                      parsed_uri.username (),
                                      parsed_uri.path ()})
        {
            if (!component)
                break;
            if (!basename.empty ())
                basename += '_';
            basename += *component;
        }
    }

    DEBUG ("Basename %s", basename.c_str ());
    std::unique_ptr<char, decltype (&g_free)> raw_original {
        gnc_build_book_path (basename.c_str ()), g_free};
    std::string original {raw_original.get ()};
    DEBUG ("Original %s", original.c_str ());

    std::string sf_extension {STATE_FILE_EXT};
    std::unique_ptr<GKeyFile, decltype (&g_key_file_free)> key_file {
        nullptr, g_key_file_free};
    int i = 1;
    while (true)
    {
        std::string filename;
        if (i == 1)
            filename = original + sf_extension;
        else
            filename = std::format ("{}_{}{}", original, i, sf_extension);
        DEBUG ("Trying %s", filename.c_str ());
        key_file.reset (gnc_key_file_load_from_file (filename.c_str (), true,
                                                     false, nullptr));
        DEBUG ("Result %p", key_file.get ());

        if (!key_file)
        {
            DEBUG ("No key file by that name");
            if (sf_extension == STATE_FILE_EXT)
            {
                DEBUG ("Trying old state file names for compatibility");
                i = 1;
                sf_extension.clear ();

                /* Regardless of whether or not an old state file is found,
                 * the currently tested name should be used for the future
                 * state file.
                 */
                s_state_file_name = filename;
                continue;
            }

            /* No old style file found. We'll return with the new file name
             * we set earlier, and no existing key file. */
            break;
        }

        std::unique_ptr<char, decltype (&g_free)> raw_file_guid {
            g_key_file_get_string (key_file.get (), STATE_FILE_TOP,
                                   STATE_FILE_BOOK_GUID, nullptr),
            g_free};
        DEBUG ("File GncGUID is %s",
               raw_file_guid ? raw_file_guid.get () : "<not found>");
        bool matched = false;
        if (raw_file_guid)
        {
            try
            {
                gnc::GUID file_guid =
                    gnc::GUID::from_string (raw_file_guid.get ());
                matched = file_guid == *guid;
            }
            catch (const gnc::guid_syntax_exception&)
            {
                /* Malformed guid in the state file; treat as no match. */
            }
        }
        if (matched)
        {
            DEBUG ("Matched !!!");
            /* Save the found file for later use. Which name to save to
             * depends on whether it was an old or new style file name
             */
            if (sf_extension == STATE_FILE_EXT)
                s_state_file_name = filename;
            else
                s_state_file_name_pre_241 = filename;

            break;
        }
        i++;
    }

    LEAVE ();
}

GKeyFile *
gnc_state_load (const QofSession *session)
{
    /* Drop possible previous state_file first */
    if (s_state_file)
    {
        g_key_file_free (s_state_file);
        s_state_file = nullptr;
    }

    gnc_state_set_base (session);

    if (s_state_file_name_pre_241)
        s_state_file = gnc_key_file_load_from_file (
            s_state_file_name_pre_241->c_str (), true, true, nullptr);
    else if (s_state_file_name)
        s_state_file = gnc_key_file_load_from_file (
            s_state_file_name->c_str (), true, true, nullptr);

    return gnc_state_get_current ();
}

void
gnc_state_save (const QofSession *session)
{
    GError *error = nullptr;

    const char *uri = qof_session_get_url (session);
    if (!uri || !*uri)
    {
        DEBUG("No file associated with session - skip state saving");
        return;
    }

    gnc_state_set_base (session);

    /* Write it all out to disk */
    if (s_state_file_name)
        gnc_key_file_save_to_file (s_state_file_name->c_str (), s_state_file,
                                   &error);
    else
        PWARN ("No state file name set, can't save state");

    if (error)
    {
        PERR ("Error: Cannot open state file %s", error->message);
        g_error_free (error);
    }
}

GKeyFile *
gnc_state_get_current (void)
{
    if (!s_state_file)
    {
        PINFO ("No pre-existing state found, creating new one");
        s_state_file = g_key_file_new ();
    }

    return s_state_file;

}

int
gnc_state_drop_sections_for (const char *partial_name)
{
    if (!s_state_file)
    {
        PWARN ("No pre-existing state found, ignoring drop request");
        return 0;
    }

    ENTER("");

    int found_count = 0, dropped_count = 0;
    gsize num_groups;
    std::unique_ptr<char *, decltype (&g_strfreev)> groups {
        g_key_file_get_groups (s_state_file, &num_groups), g_strfreev};
    for (char *group : std::span {groups.get (), num_groups})
    {
        if (std::string_view {group}.find (partial_name)
            != std::string_view::npos)
        {
            DEBUG ("Section \"%s\" matches \"%s\", removing", group,
                   partial_name);
            found_count++;
            GError *error = nullptr;
            if (!g_key_file_remove_group (s_state_file, group, &error))
            {
                PWARN ("Warning: unable to remove section %s.\n  %s",
                        group,
                        error->message);
                g_error_free (error);
            }
            else
                dropped_count++;

        }
    }

    LEAVE("Found %i sections matching \"%s\", successfully removed %i",
            found_count, partial_name, dropped_count);
    return dropped_count;

}

