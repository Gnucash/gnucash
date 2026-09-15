/********************************************************************
 * gnc-html-support.hpp -- platform-neutral helpers for GncHtml      *
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

/** @file gnc-html-support.hpp
 *  @brief Platform-neutral logic shared by the GncHtml rendering backends.
 *
 *  Every GncHtml backend (webkit2 on Unix, WebView2 on Windows, WKWebView
 *  on macOS) has to do the same handful of string manipulations before it
 *  can hand anything to its browser engine: work out the base location of
 *  a URL so relative links resolve, splice the results of GnuCash's
 *  \<object classid="..."\> handlers into the report HTML, turn a temporary
 *  file path into a URI the engine will accept, and remember a navigation
 *  request that arrived before the engine finished initialising.
 *
 *  None of that needs a browser, a display, GLib or GTK, so it lives here
 *  as ordinary C++23 that can be unit-tested anywhere -- see
 *  gnucash/html/test/test-gnc-html-support.cpp.
 */

#ifndef GNC_HTML_SUPPORT_HPP
#define GNC_HTML_SUPPORT_HPP

#include <expected>
#include <functional>
#include <optional>
#include <string>
#include <string_view>
#include <variant>

namespace gnc::html
{

/** URL schemes for which extract_base_name() splits a machine name off the
 *  front of the path. Kept as a free predicate rather than an enum because
 *  the URLType values it compares against are plain string constants from
 *  gnc-html-extras.h, and this header deliberately doesn't include that.
 */
[[nodiscard]] bool has_machine_name (std::string_view url_type) noexcept;

/** Derive the "base location" of a URL: the part a relative link in the
 *  rendered document should be resolved against.
 *
 *  This reproduces, exactly, the behaviour of the POSIX-regex
 *  implementation the webkit2 and WebView2 backends carry, so that all
 *  three backends resolve relative links identically. Two consequences of
 *  that are worth spelling out, because they look like oversights and are
 *  in fact load-bearing:
 *
 *  - For a machine-ful scheme the returned string keeps the leading "//"
 *    of the authority and gains a trailing "/", e.g.
 *    ("http", "//example.com/a/b/c.html") -> "//example.com/a/b/".
 *  - For every other scheme the leading slash of an absolute path is
 *    *dropped* and there is no trailing slash, e.g.
 *    ("file", "/home/me/report.html") -> "home/me".
 *
 *  @param url_type A URLType string such as URL_TYPE_FILE or URL_TYPE_HTTP.
 *  @param path The location part of the URL, without any "#label" suffix.
 *  @return The base location, or std::nullopt when the path has no
 *          directory component at all (e.g. "report.html").
 */
[[nodiscard]] std::optional<std::string>
extract_base_name (std::string_view url_type, std::string_view path);

/** One \<object classid="..."\>...\</object\> element found in report HTML. */
struct EmbeddedObject
{
    /** The classid attribute value, without the surrounding quotes. */
    std::string_view classid;
    /** The whole element, from "<object" through "</object>" inclusive. */
    std::string_view element;
    /** Offset of the element within the string it was found in. */
    std::size_t begin;
    /** Offset one past the element's last character. */
    std::size_t end;
};

/** Why rewrite_embedded_objects() gave up on a document. */
enum class RewriteError
{
    /** An "<object classid=" was found with no "</object>" after it. The
     *  document is malformed and callers should render it unchanged. */
    unterminated_object,
    /** An "<object classid=" was found whose quoted classid never closes. */
    unterminated_classid,
};

/** Locate the first embedded object at or after @a from.
 *
 *  @return The object, std::nullopt if there is none, or a RewriteError if
 *          one starts but is never terminated.
 */
[[nodiscard]] std::expected<std::optional<EmbeddedObject>, RewriteError>
find_embedded_object (std::string_view html, std::size_t from = 0);

/** Called once per embedded object to produce its replacement text.
 *  @param classid The object's classid attribute.
 *  @param element The whole \<object\>...\</object\> element.
 */
using EmbeddedObjectHandler =
    std::function<std::string (std::string_view classid, std::string_view element)>;

/** Replace every \<object classid="..."\> element in @a html with whatever
 *  @a handler returns for it, leaving all other text untouched.
 *
 *  @return The rewritten document, or a RewriteError if @a html is
 *          malformed -- in which case the caller should render @a html
 *          unchanged, which is what the other backends do.
 */
[[nodiscard]] std::expected<std::string, RewriteError>
rewrite_embedded_objects (std::string_view html, const EmbeddedObjectHandler& handler);

/** Build the "Not found" page shown when a URL can't be loaded.
 *  @param title Already-translated heading.
 *  @param body Already-translated body text.
 */
[[nodiscard]] std::string make_error_page (std::string_view title, std::string_view body);

/** Convert a local filesystem path to a file: URI.
 *
 *  Handles both POSIX paths ("/tmp/x.html" -> "file:///tmp/x.html") and
 *  Windows paths ("C:\\a\\b.html" -> "file:///C:/a/b.html"), and
 *  percent-encodes everything outside the unreserved set so that a
 *  temporary directory containing spaces -- normal on macOS, where
 *  TMPDIR lives under /var/folders -- doesn't produce a URI the engine
 *  silently refuses to load.
 */
[[nodiscard]] std::string file_uri_from_path (std::string_view path);

/** Inverse of file_uri_from_path(), including percent-decoding.
 *  @return std::nullopt if @a uri is not a file: URI.
 */
[[nodiscard]] std::optional<std::string> path_from_file_uri (std::string_view uri);

/** Append "#label" to @a uri, replacing any fragment already present.
 *  An empty @a label returns @a uri with its fragment stripped.
 */
[[nodiscard]] std::string with_fragment (std::string_view uri, std::string_view label);

/** A navigation request that a backend was asked to perform. */
struct UriRequest
{
    std::string uri;
    auto operator<=> (const UriRequest&) const = default;
};

/** A request to render a literal HTML string. */
struct HtmlRequest
{
    std::string html;
    auto operator<=> (const HtmlRequest&) const = default;
};

/** Either kind of navigation request, or none. */
using Request = std::variant<std::monostate, UriRequest, HtmlRequest>;

/** Holds the most recent navigation request made before the browser
 *  engine was ready to serve it.
 *
 *  Engine creation is asynchronous on both WebView2 and WKWebView, but
 *  callers routinely call gnc_html_show_url()/gnc_html_show_data()
 *  synchronously right after constructing the widget. Only the most
 *  recent request matters -- an earlier one would be immediately
 *  overwritten on screen anyway -- so setting a new one discards the
 *  previous one rather than queueing behind it.
 */
class PendingNavigation
{
public:
    PendingNavigation () = default;

    /** Remember a URI to navigate to, replacing any pending request. */
    void set_uri (std::string uri);
    /** Remember an HTML string to render, replacing any pending request. */
    void set_html (std::string html);

    [[nodiscard]] bool empty () const noexcept;
    /** Peek at the pending request without consuming it. */
    [[nodiscard]] const Request& peek () const noexcept { return m_request; }

    /** Consume the pending request, leaving the object empty. */
    [[nodiscard]] Request take () noexcept;

    void clear () noexcept;

private:
    Request m_request;
};

} // namespace gnc::html

#endif // GNC_HTML_SUPPORT_HPP
