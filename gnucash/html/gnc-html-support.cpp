/********************************************************************
 * gnc-html-support.cpp -- platform-neutral helpers for GncHtml      *
 * Copyright (C) 2026 Christopher Lam                               *
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
\********************************************************************/

#include "gnc-html-support.hpp"

#include <algorithm>
#include <array>
#include <cctype>
#include <format>
#include <utility>

namespace gnc::html
{

namespace
{

constexpr std::string_view OBJECT_OPEN {"<object classid="};
constexpr std::string_view OBJECT_CLOSE {"</object>"};

/* The URLType values that carry a "//machine" authority. Spelled out
 * here rather than included from gnc-html-extras.h so that this
 * translation unit stays free of GnuCash headers and can be compiled
 * straight into the unit test. The values are asserted against the real
 * URL_TYPE_* macros in the test. */
constexpr std::array MACHINE_SCHEMES {
    std::string_view {"http"},
    std::string_view {"secure"},
    std::string_view {"ftp"},
};

/* Characters that never need percent-encoding in a path: RFC 3986's
 * unreserved set, plus '/' (the separator itself) and ':' (so a Windows
 * drive letter survives). */
[[nodiscard]] constexpr bool
is_uri_path_safe (unsigned char c) noexcept
{
    return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') ||
           (c >= '0' && c <= '9') ||
           c == '-' || c == '.' || c == '_' || c == '~' ||
           c == '/' || c == ':';
}

/* Fragments may additionally carry the sub-delims and a few gen-delims
 * unescaped; anchors GnuCash reports generate are plain identifiers, but
 * a report title used as an anchor can contain anything. */
[[nodiscard]] constexpr bool
is_uri_fragment_safe (unsigned char c) noexcept
{
    return is_uri_path_safe (c) || c == '!' || c == '$' || c == '&' ||
           c == '\'' || c == '(' || c == ')' || c == '*' || c == '+' ||
           c == ',' || c == ';' || c == '=' || c == '@' || c == '?';
}

[[nodiscard]] std::string
percent_encode (std::string_view text, bool (*is_safe) (unsigned char) noexcept)
{
    constexpr std::string_view hex {"0123456789ABCDEF"};
    std::string out;
    out.reserve (text.size ());
    for (auto ch : text)
    {
        auto c = static_cast<unsigned char> (ch);
        if (is_safe (c))
            out.push_back (ch);
        else
        {
            out.push_back ('%');
            out.push_back (hex[c >> 4]);
            out.push_back (hex[c & 0x0F]);
        }
    }
    return out;
}

[[nodiscard]] constexpr int
hex_value (char c) noexcept
{
    if (c >= '0' && c <= '9') return c - '0';
    if (c >= 'a' && c <= 'f') return c - 'a' + 10;
    if (c >= 'A' && c <= 'F') return c - 'A' + 10;
    return -1;
}

[[nodiscard]] std::string
percent_decode (std::string_view text)
{
    std::string out;
    out.reserve (text.size ());
    for (std::size_t i = 0; i < text.size (); ++i)
    {
        if (text[i] == '%' && i + 2 < text.size ())
        {
            auto hi = hex_value (text[i + 1]);
            auto lo = hex_value (text[i + 2]);
            if (hi >= 0 && lo >= 0)
            {
                out.push_back (static_cast<char> (hi * 16 + lo));
                i += 2;
                continue;
            }
        }
        out.push_back (text[i]);
    }
    return out;
}

/* True for "/C:/..." and "C:/...", the shapes a Windows path takes once
 * its separators have been normalised. */
[[nodiscard]] constexpr bool
has_drive_letter (std::string_view path) noexcept
{
    return path.size () >= 2 && std::isalpha (static_cast<unsigned char> (path[0])) &&
           path[1] == ':';
}

/** Split @a location into ($1, $2) of the POSIX ERE "^(//[^/]*)[/]*(/.*)?$".
 *
 *  POSIX matching is leftmost-longest for the match as a whole before it
 *  is longest-first for subexpressions, which is why the "[/]*" in the
 *  middle ends up matching nothing whenever there is a path left to
 *  assign to $2: letting it eat the separator would strand the rest of
 *  the string outside the match.
 */
struct MachineSplit
{
    std::optional<std::string_view> machine;
    std::optional<std::string_view> location;
};

[[nodiscard]] MachineSplit
split_machine (std::string_view path)
{
    if (!path.starts_with ("//"))
        return {};              // the regex doesn't match at all

    auto host_end = path.find ('/', 2);
    if (host_end == std::string_view::npos)
        return {path, std::nullopt};  // "//example.com", no path part

    return {path.substr (0, host_end), path.substr (host_end)};
}

/** $1 of the POSIX ERE "^[/]*(.*)[/]+([^/]*)$" applied to @a location.
 *
 *  "[/]+" is pinned to the run of slashes that ends at the last slash in
 *  the string, because $2 ("[^/]*$") cannot contain one. "[/]*" then takes
 *  as many leading slashes as it can without overrunning the start of
 *  that run -- which is what keeps "/" and "///" matching with an empty
 *  $1 instead of failing.
 */
[[nodiscard]] std::optional<std::string_view>
split_directory (std::string_view location)
{
    auto last_slash = location.rfind ('/');
    if (last_slash == std::string_view::npos)
        return std::nullopt;    // the regex doesn't match at all

    auto run_start = last_slash;
    while (run_start > 0 && location[run_start - 1] == '/')
        --run_start;

    std::size_t leading = 0;
    while (leading < location.size () && location[leading] == '/')
        ++leading;

    auto star = std::min (leading, run_start);
    return location.substr (star, run_start - star);
}

} // namespace

bool
has_machine_name (std::string_view url_type) noexcept
{
    return std::ranges::find (MACHINE_SCHEMES, url_type) != MACHINE_SCHEMES.end ();
}

std::optional<std::string>
extract_base_name (std::string_view url_type, std::string_view path)
{
    std::optional<std::string_view> machine;
    std::optional<std::string_view> location;

    if (has_machine_name (url_type))
    {
        auto split = split_machine (path);
        machine = split.machine;
        location = split.location;
    }
    else
        location = path;

    auto base = location ? split_directory (*location) : std::nullopt;

    if (machine)
    {
        if (base && !base->empty ())
            return std::format ("{}/{}/", *machine, *base);
        return std::format ("{}/", *machine);
    }

    if (base && !base->empty ())
        return std::string {*base};

    return std::nullopt;
}

std::expected<std::optional<EmbeddedObject>, RewriteError>
find_embedded_object (std::string_view html, std::size_t from)
{
    if (from >= html.size ())
        return std::optional<EmbeddedObject>{};

    auto begin = html.find (OBJECT_OPEN, from);
    if (begin == std::string_view::npos)
        return std::optional<EmbeddedObject>{};

    auto quote_pos = begin + OBJECT_OPEN.size ();
    if (quote_pos >= html.size ())
        return std::unexpected (RewriteError::unterminated_classid);

    auto quote = html[quote_pos];
    if (quote != '"' && quote != '\'')
        return std::unexpected (RewriteError::unterminated_classid);

    auto classid_begin = quote_pos + 1;
    auto classid_end = html.find (quote, classid_begin);
    if (classid_end == std::string_view::npos)
        return std::unexpected (RewriteError::unterminated_classid);

    auto close = html.find (OBJECT_CLOSE, classid_end);
    if (close == std::string_view::npos)
        return std::unexpected (RewriteError::unterminated_object);

    auto end = close + OBJECT_CLOSE.size ();
    return std::optional<EmbeddedObject>{
        EmbeddedObject {html.substr (classid_begin, classid_end - classid_begin),
                        html.substr (begin, end - begin), begin, end}};
}

std::expected<std::string, RewriteError>
rewrite_embedded_objects (std::string_view html, const EmbeddedObjectHandler& handler)
{
    std::string out;
    std::size_t pos = 0;

    while (true)
    {
        auto found = find_embedded_object (html, pos);
        if (!found)
            return std::unexpected (found.error ());
        if (!*found)
            break;

        const auto& object = **found;
        out.append (html.substr (pos, object.begin - pos));
        if (handler)
            out.append (handler (object.classid, object.element));
        pos = object.end;
    }

    if (pos == 0)
        return std::string {html};  // nothing to rewrite; avoid the copy churn

    out.append (html.substr (pos));
    return out;
}

std::string
make_error_page (std::string_view title, std::string_view body)
{
    return std::format ("<html><body><h3>{}</h3><p>{}</body></html>", title, body);
}

std::string
file_uri_from_path (std::string_view path)
{
    std::string normalized {path};
    std::ranges::replace (normalized, '\\', '/');

    /* "file://" already supplies two of the three slashes an absolute
     * URI needs; the third is the one that starts the path. A Windows
     * path ("C:/...") and a relative path have to borrow it. */
    if (!normalized.starts_with ('/'))
        normalized.insert (normalized.begin (), '/');

    return std::format ("file://{}", percent_encode (normalized, is_uri_path_safe));
}

std::optional<std::string>
path_from_file_uri (std::string_view uri)
{
    constexpr std::string_view scheme {"file://"};
    if (!uri.starts_with (scheme))
        return std::nullopt;

    auto rest = uri.substr (scheme.size ());
    if (auto hash = rest.find ('#'); hash != std::string_view::npos)
        rest = rest.substr (0, hash);

    auto path = percent_decode (rest);

    /* file:///C:/x came from C:\x, so hand back C:/x rather than a path
     * with a spurious root slash in front of the drive letter. */
    if (path.size () > 1 && path[0] == '/' && has_drive_letter (std::string_view {path}.substr (1)))
        path.erase (path.begin ());

    return path;
}

std::string
with_fragment (std::string_view uri, std::string_view label)
{
    if (auto hash = uri.find ('#'); hash != std::string_view::npos)
        uri = uri.substr (0, hash);

    if (label.empty ())
        return std::string {uri};

    return std::format ("{}#{}", uri, percent_encode (label, is_uri_fragment_safe));
}

void
PendingNavigation::set_uri (std::string uri)
{
    m_request = UriRequest {std::move (uri)};
}

void
PendingNavigation::set_html (std::string html)
{
    m_request = HtmlRequest {std::move (html)};
}

bool
PendingNavigation::empty () const noexcept
{
    return std::holds_alternative<std::monostate> (m_request);
}

Request
PendingNavigation::take () noexcept
{
    return std::exchange (m_request, Request {});
}

void
PendingNavigation::clear () noexcept
{
    m_request = Request {};
}

} // namespace gnc::html
