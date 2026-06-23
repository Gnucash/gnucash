/********************************************************************\
 * gnc-uri.hpp -- C++ interface to parse and compose uris.          *
 *                                                                  *
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
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#ifndef GNC_URI_HPP
#define GNC_URI_HPP

#include <cstdint>
#include <optional>
#include <string>

/** @addtogroup Engine
    @{ */
/** @file gnc-uri.hpp
 *  @brief C++ interface to parse and compose GnuCash resource locators.
 *  @author Copyright (C) 2026 Brent McBride <mcbridebt@hotmail.com>
 *
 *  GnuCash refers to the books it stores by a uri, which may be a network
 *  service (such as a database) or a local filesystem path. This is the C++
 *  face of that utility; the C functions in gnc-uri-utils.h are thin veneers
 *  over the class below and remain available for not-yet-migrated callers.
 */

/** A parsed GnuCash resource locator.
 *
 *  Construct one from a uri (or a bare local filesystem path) to inspect its
 *  components, or from individual components to compose a normalized uri with
 *  str(). Components that are absent from a uri are reported as std::nullopt,
 *  preserving the historical distinction between a missing component and one
 *  that is present but empty.
 */
class GncUri
{
public:
    /** Parse a uri, or a bare local filesystem path, into its components.
     *  An empty string yields an empty GncUri (all components absent). */
    explicit GncUri (const std::string& uri);

    /** Construct directly from individual components, typically to compose a
     *  uri with str(). Absent components are represented by std::nullopt. */
    GncUri (std::optional<std::string> scheme,
            std::optional<std::string> hostname,
            int32_t port,
            std::optional<std::string> username,
            std::optional<std::string> password,
            std::optional<std::string> path);

    const std::optional<std::string>& scheme()   const noexcept { return m_scheme; }
    const std::optional<std::string>& hostname() const noexcept { return m_hostname; }
    const std::optional<std::string>& username() const noexcept { return m_username; }
    const std::optional<std::string>& password() const noexcept { return m_password; }
    const std::optional<std::string>& path()     const noexcept { return m_path; }
    int32_t port() const noexcept { return m_port; }

    /** True if this uri uses a file-type scheme (file, xml, sqlite3). A uri
     *  without a scheme is not considered a file uri (matching the historical
     *  gnc_uri_is_file_uri behaviour). */
    bool is_file_uri() const noexcept;

    /** True if the uri refers to the local filesystem: it has a path and
     *  either no scheme or a file-type scheme. */
    bool targets_local_fs() const noexcept;

    /** Compose a normalized uri string from the components.
     *
     *  @param allow_password When false, any password is omitted from the
     *         result.
     *  @return The composed uri. For a file-type (or absent) scheme the path
     *          is resolved to an absolute name.
     *  @throws std::invalid_argument when no path is present, or when a
     *          non-file scheme is missing its hostname.
     */
    std::string str (bool allow_password = true) const;

    /** Like str(), but returns std::nullopt instead of throwing when the
     *  components cannot form a valid uri (no path is present, or a non-file
     *  scheme is missing its hostname). Intended for callers that historically
     *  tolerated a NULL result from gnc_uri_normalize_uri / gnc_uri_create_uri.
     *
     *  @param allow_password When false, any password is omitted from the
     *         result.
     */
    std::optional<std::string> try_str (bool allow_password = true) const;

    /** True if @a scheme is a file-type scheme (file, xml, sqlite3). */
    static bool scheme_is_file (const std::string& scheme) noexcept;

private:
    std::optional<std::string> m_scheme;
    std::optional<std::string> m_hostname;
    std::optional<std::string> m_username;
    std::optional<std::string> m_password;
    std::optional<std::string> m_path;
    int32_t m_port = 0;
};

/** @} */

#endif /* GNC_URI_HPP */
