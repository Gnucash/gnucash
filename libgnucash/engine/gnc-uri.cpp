/*
 * gnc-uri.cpp -- utility functions to convert uri in separate
 *                    components and back.
 *
 * Copyright (C) 2010 Geert Janssens <janssens.geert@telenet.be>
 * Copyright (C) 2026 Brent McBride <mcbridebt@hotmail.com>
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
#include <cstdint>
#include <optional>
#include <stdexcept>
#include <string>
#include <utility>
#include "gnc-uri-utils.h"
#include "gnc-uri.hpp"
#include "gnc-filepath-utils.h"
#include "qofsession.h"

/* Duplicates an optional component into a freshly allocated C string, or
 * returns nullptr when the component is absent. This preserves the public C
 * API's contract that returned strings (or NULL) are released with g_free(). */
static gchar *
dup_or_null (const std::optional<std::string>& component)
{
    return component ? g_strdup (component->c_str()) : nullptr;
}

/* Wraps a possibly-null C string as an optional, mapping NULL to nullopt so
 * the historical "absent vs empty" distinction is preserved when crossing
 * from the C veneers into the GncUri class. */
static std::optional<std::string>
to_opt (const gchar *s)
{
    return s ? std::optional<std::string> { s } : std::nullopt;
}

/* True when the scheme is one of the registered backend access methods. */
static bool
scheme_is_known (const gchar *scheme)
{
    bool is_known_scheme = false;
    GList *known_scheme_list = qof_backend_get_registered_access_method_list();

    for ( GList *node = known_scheme_list; node != nullptr; node = node->next )
    {
        gchar *known_scheme = static_cast<gchar*>(node->data);
        if ( !g_ascii_strcasecmp (scheme, known_scheme) )
        {
            is_known_scheme = true;
            break;
        }
    }

    g_list_free (known_scheme_list);
    return is_known_scheme;
}

/* ---------------------------------------------------------------------------
 * GncUri - C++ interface
 *
 * The class below carries all of the parsing and composition logic. The
 * extern "C" functions further down are thin veneers over it, preserving the
 * historical gchar* / g_free contract for C (and not-yet-migrated C++)
 * callers.
 * ------------------------------------------------------------------------- */

GncUri::GncUri (std::optional<std::string> scheme,
                std::optional<std::string> hostname,
                int32_t port,
                std::optional<std::string> username,
                std::optional<std::string> password,
                std::optional<std::string> path)
    : m_scheme   (std::move (scheme))
    , m_hostname (std::move (hostname))
    , m_username (std::move (username))
    , m_password (std::move (password))
    , m_path     (std::move (path))
    , m_port     (port)
{
    /* A GncUri built from components must be able to form a valid uri: it
     * needs a path, and a non-file scheme also needs a hostname. Failing here
     * keeps every constructed GncUri valid, so str()/try_str() never have to
     * re-check. (The parsing ctor below is deliberately permissive instead.) */
    if (!m_path)
        throw std::invalid_argument ("GncUri: a path is required");
    if (m_scheme && !scheme_is_file (*m_scheme) && !m_hostname)
        throw std::invalid_argument (
            "GncUri: a hostname is required for a non-file scheme");
}

GncUri::GncUri (const std::string& uri)
{
    if (uri.empty())
        return;

    auto sep = uri.find ("://");
    if (sep == std::string::npos)
    {
        /* No scheme means a simple file path; the path is a copy of the input. */
        m_path = uri;
        return;
    }

    std::string scheme = uri.substr (0, sep);
    std::string rest   = uri.substr (sep + 3);
    m_scheme = scheme;

    if (scheme_is_file (scheme))
    {
        /* A true file uri on windows can start with file:///N:/, so we arrive
         * here with /N:/ (it could also be /N:\). Strip the leading slash in
         * that case before resolving. */
        const gchar *file_path = rest.c_str();
        if (!rest.empty() && rest.front() == '/' &&
            (rest.find (":/") != std::string::npos ||
             rest.find (":\\") != std::string::npos))
            file_path = rest.c_str() + 1;

        gchar *resolved = gnc_resolve_file_path (file_path);
        m_path = std::string { resolved };
        g_free (resolved);
        return;
    }

    /* Network style uri: [user[:password]@]hostname[:port][/path].
     * Look for the '@' from the end, as the password may contain one too. */
    std::string hostpart;
    auto at = rest.rfind ('@');
    if (at != std::string::npos)
    {
        std::string userinfo = rest.substr (0, at);
        hostpart = rest.substr (at + 1);

        /* Look for a password, searching from the start so a ':' in the
         * password doesn't confuse the split. */
        auto colon = userinfo.find (':');
        if (colon != std::string::npos)
        {
            m_username = userinfo.substr (0, colon);
            m_password = userinfo.substr (colon + 1);
        }
        else
            m_username = userinfo;
    }
    else
        hostpart = rest;

    /* Split off the path part. */
    auto slash = hostpart.find ('/');
    if (slash != std::string::npos)
    {
        m_path = hostpart.substr (slash + 1);
        hostpart.erase (slash);
    }

    /* Split off an optional port specifier. */
    auto port_colon = hostpart.find (':');
    if (port_colon != std::string::npos)
    {
        m_port = static_cast<int32_t> (
            g_ascii_strtoll (hostpart.c_str() + port_colon + 1, nullptr, 0));
        hostpart.erase (port_colon);
    }

    m_hostname = hostpart;
}

bool
GncUri::scheme_is_file (const std::string& scheme) noexcept
{
    return (!g_ascii_strcasecmp (scheme.c_str(), "file") ||
            !g_ascii_strcasecmp (scheme.c_str(), "xml") ||
            !g_ascii_strcasecmp (scheme.c_str(), "sqlite3"));
}

bool
GncUri::is_file_uri () const noexcept
{
    return m_scheme && scheme_is_file (*m_scheme);
}

bool
GncUri::targets_local_fs () const noexcept
{
    /* Targets the local fs when it has a path and either no scheme or a
     * file-type scheme (file, xml, sqlite). */
    return m_path && (!m_scheme || scheme_is_file (*m_scheme));
}

std::optional<std::string>
GncUri::try_str (bool allow_password) const
{
    if (!m_path)
        return std::nullopt;

    const std::string& path = *m_path;

    if (!m_scheme || scheme_is_file (*m_scheme))
    {
        /* File based uri: ignore everything but the scheme and the path. The
         * path is resolved to an absolute name for a known (or absent) scheme;
         * for an unknown scheme it is used as is. */
        gchar *resolved = (m_scheme && !scheme_is_known (m_scheme->c_str()))
                          ? g_strdup (path.c_str())
                          : gnc_resolve_file_path (path.c_str());
        std::string abs_path { resolved };
        g_free (resolved);

        std::string scheme = m_scheme ? *m_scheme : std::string { "file" };

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
         */
        bool absolute = !abs_path.empty() &&
                        (abs_path.front() == '/' || abs_path.front() == '\\');
        return absolute ? scheme + "://" + abs_path
                        : scheme + ":///" + abs_path; // extra "/" for windows
    }

    std::string userpass;
    if (m_username && !m_username->empty())
    {
        userpass = *m_username;
        if (allow_password && m_password && !m_password->empty())
        {
            userpass += ':';
            userpass += *m_password;
        }
        userpass += '@';
    }

    std::string portstr;
    if (m_port != 0)
        portstr = ":" + std::to_string (m_port);

    return *m_scheme + "://" + userpass + *m_hostname + portstr + "/" + path;
}

std::string
GncUri::str (bool allow_password) const
{
    if (std::optional<std::string> result = try_str (allow_password))
        return std::move (*result);

    /* try_str only fails when there is no path; a non-file scheme without a
     * hostname is already rejected by the component constructor. */
    throw std::invalid_argument ("GncUri::str: a path is required");
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
gboolean
gnc_uri_is_file_scheme (const gchar *scheme)
{
    return scheme && GncUri::scheme_is_file (scheme);
}

/* Checks if the given uri defines a file
 * (as opposed to a network service)
 */
gboolean
gnc_uri_is_file_uri (const gchar *uri)
{
    g_return_val_if_fail (uri != nullptr, FALSE);
    return GncUri { uri }.is_file_uri();
}

/* Checks if the given uri is a valid uri
 */
gboolean
gnc_uri_targets_local_fs (const gchar *uri)
{
    g_return_val_if_fail (uri != nullptr, FALSE);
    return GncUri { uri }.targets_local_fs();
}

/* Splits a uri into its separate components. Thin C veneer over the GncUri
 * class; each component is handed back as a freshly allocated string (or NULL
 * when absent) that the caller frees with g_free(). */
void
gnc_uri_get_components (const gchar *uri,
                        gchar **scheme,
                        gchar **hostname,
                        gint32 *port,
                        gchar **username,
                        gchar **password,
                        gchar **path)
{
    *scheme   = nullptr;
    *hostname = nullptr;
    *port     = 0;
    *username = nullptr;
    *password = nullptr;
    *path     = nullptr;

    g_return_if_fail( uri != nullptr && strlen (uri) > 0);

    GncUri parsed { uri };

    *scheme   = dup_or_null (parsed.scheme());
    *hostname = dup_or_null (parsed.hostname());
    *username = dup_or_null (parsed.username());
    *password = dup_or_null (parsed.password());
    *path     = dup_or_null (parsed.path());
    *port     = parsed.port();
}

gchar *
gnc_uri_get_scheme (const gchar *uri)
{
    g_return_val_if_fail (uri != nullptr, nullptr);
    return dup_or_null (GncUri { uri }.scheme());
}

gchar *
gnc_uri_get_path (const gchar *uri)
{
    g_return_val_if_fail (uri != nullptr, nullptr);
    return dup_or_null (GncUri { uri }.path());
}

/* Generates a normalized uri from the separate components */
gchar *
gnc_uri_create_uri (const gchar *scheme,
                    const gchar *hostname,
                    gint32 port,
                    const gchar *username,
                    const gchar *password,
                    const gchar *path)
{
    g_return_val_if_fail( path != nullptr, nullptr );

    /* For a non-file scheme a hostname is mandatory. (A missing scheme is
     * treated as a file scheme, which needs no hostname.) */
    if (scheme && !GncUri::scheme_is_file (scheme))
        g_return_val_if_fail( hostname != nullptr, nullptr );

    GncUri uri { to_opt (scheme), to_opt (hostname), port,
                 to_opt (username), to_opt (password), to_opt (path) };
    return g_strdup (uri.str().c_str());
}

gchar *
gnc_uri_normalize_uri (const gchar *uri, gboolean allow_password)
{
    g_return_val_if_fail (uri != nullptr, nullptr);

    GncUri parsed { uri };
    return gnc_uri_create_uri (
        parsed.scheme()   ? parsed.scheme()->c_str()   : nullptr,
        parsed.hostname() ? parsed.hostname()->c_str() : nullptr,
        parsed.port(),
        parsed.username() ? parsed.username()->c_str() : nullptr,
        (allow_password && parsed.password()) ? parsed.password()->c_str() : nullptr,
        parsed.path()     ? parsed.path()->c_str()     : nullptr );
}

gchar *
gnc_uri_add_extension ( const gchar *uri, const gchar *extension )
{
    g_return_val_if_fail( uri != nullptr, nullptr );

    /* Only add extension if the user provided the extension and the uri is
     * file based.
     */
    if ( !extension || !gnc_uri_is_file_uri( uri ) )
        return g_strdup( uri );

    /* Don't add extension if it's already there */
    if ( g_str_has_suffix( uri, extension ) )
        return g_strdup( uri );

    /* Ok, all tests passed, let's add the extension */
    return g_strconcat( uri, extension, nullptr );
}
