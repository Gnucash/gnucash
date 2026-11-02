/********************************************************************
 * test-gnc-html-support.cpp -- tests for the GncHtml helpers        *
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

#include <glib.h>

#include "gnc-html-extras.h"
#include "gnc-html-support.hpp"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcpp"
#include <gtest/gtest.h>
#pragma GCC diagnostic pop

#include <string>
#include <string_view>
#include <vector>

using namespace std::string_view_literals;

using gnc::html::EmbeddedObject;
using gnc::html::HtmlRequest;
using gnc::html::PendingNavigation;
using gnc::html::Request;
using gnc::html::RewriteError;
using gnc::html::UriRequest;

namespace
{

/* The legacy format string the webkit2 and WebView2 backends build their
 * 404 page with. make_error_page() has to keep producing exactly this. */
constexpr auto LEGACY_404_FORMAT = "<html><body><h3>%s</h3><p>%s</body></html>";

/* Convenience: unwrap a successful find_embedded_object() result. */
EmbeddedObject
require_object (std::string_view html, std::size_t from = 0)
{
    auto found = gnc::html::find_embedded_object (html, from);
    EXPECT_TRUE (found.has_value ());
    EXPECT_TRUE (found.value ().has_value ());
    return **found;
}

} // namespace

/* ------------------------------------------------------------------ */
/* has_machine_name                                                    */
/* ------------------------------------------------------------------ */

TEST (GncHtmlSupport, MachineSchemesMatchTheUrlTypeMacros)
{
    /* gnc-html-support.cpp spells these out rather than including
     * gnc-html-extras.h, so pin them to the real macros here. */
    EXPECT_TRUE (gnc::html::has_machine_name (URL_TYPE_HTTP));
    EXPECT_TRUE (gnc::html::has_machine_name (URL_TYPE_SECURE));
    EXPECT_TRUE (gnc::html::has_machine_name (URL_TYPE_FTP));
}

TEST (GncHtmlSupport, NonMachineSchemes)
{
    EXPECT_FALSE (gnc::html::has_machine_name (URL_TYPE_FILE));
    EXPECT_FALSE (gnc::html::has_machine_name (URL_TYPE_JUMP));
    EXPECT_FALSE (gnc::html::has_machine_name (URL_TYPE_REPORT));
    EXPECT_FALSE (gnc::html::has_machine_name (URL_TYPE_REGISTER));
    EXPECT_FALSE (gnc::html::has_machine_name (""sv));
    /* Not a prefix match: "http" must not be found inside "https". */
    EXPECT_FALSE (gnc::html::has_machine_name ("https"sv));
}

/* ------------------------------------------------------------------ */
/* extract_base_name                                                   */
/* ------------------------------------------------------------------ */

TEST (GncHtmlSupport, BaseNameOfHttpUrl)
{
    EXPECT_EQ (gnc::html::extract_base_name (URL_TYPE_HTTP, "//example.com/a/b/c.html"),
               "//example.com/a/b/");
}

TEST (GncHtmlSupport, BaseNameOfSecureAndFtpUrls)
{
    EXPECT_EQ (gnc::html::extract_base_name (URL_TYPE_SECURE, "//example.com/reports/x.html"),
               "//example.com/reports/");
    EXPECT_EQ (gnc::html::extract_base_name (URL_TYPE_FTP, "//ftp.example.com/pub/x"),
               "//ftp.example.com/pub/");
}

TEST (GncHtmlSupport, BaseNameOfHttpUrlWithFileAtRoot)
{
    /* "//host/x.html": the path part is "/x.html", whose directory
     * component is empty, so only the machine and a separator remain. */
    EXPECT_EQ (gnc::html::extract_base_name (URL_TYPE_HTTP, "//example.com/x.html"),
               "//example.com/");
}

TEST (GncHtmlSupport, BaseNameOfMachineOnlyUrl)
{
    EXPECT_EQ (gnc::html::extract_base_name (URL_TYPE_HTTP, "//example.com"),
               "//example.com/");
}

TEST (GncHtmlSupport, BaseNameOfHttpUrlWithoutAuthorityHasNoMatch)
{
    /* The machine regex is anchored on "//", so a path-only http
     * location matches nothing at all and yields no base. */
    EXPECT_FALSE (gnc::html::extract_base_name (URL_TYPE_HTTP, "example.com/a/b.html").has_value ());
}

TEST (GncHtmlSupport, BaseNameOfFilePathDropsTheLeadingSlash)
{
    /* Documented quirk inherited from the POSIX-regex implementation:
     * for non-machine schemes the root slash is consumed and no
     * trailing slash is added. All three backends agree on this. */
    EXPECT_EQ (gnc::html::extract_base_name (URL_TYPE_FILE, "/home/me/report.html"),
               "home/me");
}

TEST (GncHtmlSupport, BaseNameOfRelativeFilePath)
{
    EXPECT_EQ (gnc::html::extract_base_name (URL_TYPE_FILE, "reports/x.html"), "reports");
}

TEST (GncHtmlSupport, BaseNameOfBareFileNameHasNoMatch)
{
    EXPECT_FALSE (gnc::html::extract_base_name (URL_TYPE_FILE, "report.html").has_value ());
    EXPECT_FALSE (gnc::html::extract_base_name (URL_TYPE_FILE, "").has_value ());
}

TEST (GncHtmlSupport, BaseNameOfRootIsEmptyAndThereforeAbsent)
{
    /* "/" matches the regex with an empty $1; the legacy code treats an
     * empty base the same as no base at all. */
    EXPECT_FALSE (gnc::html::extract_base_name (URL_TYPE_FILE, "/").has_value ());
    EXPECT_FALSE (gnc::html::extract_base_name (URL_TYPE_FILE, "///").has_value ());
}

TEST (GncHtmlSupport, BaseNameKeepsInteriorDuplicateSlashes)
{
    /* Only the run of slashes immediately before the last component is
     * consumed; earlier duplicates stay in the base. */
    EXPECT_EQ (gnc::html::extract_base_name (URL_TYPE_FILE, "//a//b//c"), "a//b");
    EXPECT_EQ (gnc::html::extract_base_name (URL_TYPE_HTTP, "//host//a//b//c"),
               "//host/a//b/");
}

TEST (GncHtmlSupport, BaseNameOfDirectoryPath)
{
    /* A trailing slash means the last component is empty, so the base is
     * the whole directory path. */
    EXPECT_EQ (gnc::html::extract_base_name (URL_TYPE_FILE, "/home/me/"), "home/me");
}

/* ------------------------------------------------------------------ */
/* find_embedded_object                                                */
/* ------------------------------------------------------------------ */

TEST (GncHtmlSupport, NoEmbeddedObject)
{
    auto found = gnc::html::find_embedded_object ("<html><body>plain</body></html>"sv);
    ASSERT_TRUE (found.has_value ());
    EXPECT_FALSE (found->has_value ());
}

TEST (GncHtmlSupport, FindsEmbeddedObject)
{
    constexpr auto html = "<p>before</p><object classid=\"gnc-guppi-pie\">data</object><p>after</p>"sv;
    auto object = require_object (html);

    EXPECT_EQ (object.classid, "gnc-guppi-pie");
    EXPECT_EQ (object.element, "<object classid=\"gnc-guppi-pie\">data</object>");
    EXPECT_EQ (object.begin, html.find ("<object"));
    EXPECT_EQ (object.end, html.find ("<p>after"));
}

TEST (GncHtmlSupport, FindsEmbeddedObjectWithSingleQuotedClassid)
{
    auto object = require_object ("<object classid='gnc-chart'></object>"sv);
    EXPECT_EQ (object.classid, "gnc-chart");
}

TEST (GncHtmlSupport, FindsEmbeddedObjectFromOffset)
{
    constexpr auto html = "<object classid=\"a\">1</object><object classid=\"b\">2</object>"sv;
    auto first = require_object (html);
    EXPECT_EQ (first.classid, "a");

    auto second = require_object (html, first.end);
    EXPECT_EQ (second.classid, "b");
    EXPECT_EQ (second.end, html.size ());

    auto third = gnc::html::find_embedded_object (html, second.end);
    ASSERT_TRUE (third.has_value ());
    EXPECT_FALSE (third->has_value ());
}

TEST (GncHtmlSupport, UnterminatedObjectIsAnError)
{
    auto found = gnc::html::find_embedded_object ("<object classid=\"a\">no end tag"sv);
    ASSERT_FALSE (found.has_value ());
    EXPECT_EQ (found.error (), RewriteError::unterminated_object);
}

TEST (GncHtmlSupport, UnterminatedClassidIsAnError)
{
    auto unclosed_quote = gnc::html::find_embedded_object ("<object classid=\"a</object>"sv);
    ASSERT_FALSE (unclosed_quote.has_value ());
    EXPECT_EQ (unclosed_quote.error (), RewriteError::unterminated_classid);

    auto no_quote = gnc::html::find_embedded_object ("<object classid=a\"></object>"sv);
    ASSERT_FALSE (no_quote.has_value ());
    EXPECT_EQ (no_quote.error (), RewriteError::unterminated_classid);

    auto truncated = gnc::html::find_embedded_object ("<object classid="sv);
    ASSERT_FALSE (truncated.has_value ());
    EXPECT_EQ (truncated.error (), RewriteError::unterminated_classid);
}

/* ------------------------------------------------------------------ */
/* rewrite_embedded_objects                                            */
/* ------------------------------------------------------------------ */

TEST (GncHtmlSupport, RewriteLeavesPlainHtmlAlone)
{
    constexpr auto html = "<html><body>no objects here</body></html>"sv;
    auto result = gnc::html::rewrite_embedded_objects (
        html, [] (std::string_view, std::string_view) { return std::string {"X"}; });
    ASSERT_TRUE (result.has_value ());
    EXPECT_EQ (*result, html);
}

TEST (GncHtmlSupport, RewriteReplacesEveryObject)
{
    constexpr auto html =
        "A<object classid=\"one\">a</object>B<object classid=\"two\">b</object>C"sv;

    std::vector<std::string> seen_classids;
    std::vector<std::string> seen_elements;
    auto result = gnc::html::rewrite_embedded_objects (
        html, [&] (std::string_view classid, std::string_view element)
        {
            seen_classids.emplace_back (classid);
            seen_elements.emplace_back (element);
            return std::string {"<img src=\""} + std::string {classid} + "\">";
        });

    ASSERT_TRUE (result.has_value ());
    EXPECT_EQ (*result, "A<img src=\"one\">B<img src=\"two\">C");
    EXPECT_EQ (seen_classids, (std::vector<std::string> {"one", "two"}));
    EXPECT_EQ (seen_elements,
               (std::vector<std::string> {"<object classid=\"one\">a</object>",
                                          "<object classid=\"two\">b</object>"}));
}

TEST (GncHtmlSupport, RewriteHandlesObjectsAtBothEnds)
{
    auto result = gnc::html::rewrite_embedded_objects (
        "<object classid=\"x\"></object>"sv,
        [] (std::string_view, std::string_view) { return std::string {"!"}; });
    ASSERT_TRUE (result.has_value ());
    EXPECT_EQ (*result, "!");
}

TEST (GncHtmlSupport, RewriteWithEmptyReplacementDropsTheObject)
{
    auto result = gnc::html::rewrite_embedded_objects (
        "A<object classid=\"x\">q</object>B"sv,
        [] (std::string_view, std::string_view) { return std::string {}; });
    ASSERT_TRUE (result.has_value ());
    EXPECT_EQ (*result, "AB");
}

TEST (GncHtmlSupport, RewriteWithNoHandlerDropsEveryObject)
{
    auto result = gnc::html::rewrite_embedded_objects ("A<object classid=\"x\">q</object>B"sv, {});
    ASSERT_TRUE (result.has_value ());
    EXPECT_EQ (*result, "AB");
}

TEST (GncHtmlSupport, RewriteReportsMalformedDocuments)
{
    auto result = gnc::html::rewrite_embedded_objects (
        "A<object classid=\"x\">never closed"sv,
        [] (std::string_view, std::string_view) { return std::string {"!"}; });
    ASSERT_FALSE (result.has_value ());
    EXPECT_EQ (result.error (), RewriteError::unterminated_object);
}

TEST (GncHtmlSupport, RewriteReportsMalformationAfterAGoodObject)
{
    /* The legacy implementation abandons its partial result and renders
     * the original document; returning an error lets the caller do the
     * same without having to detect it. */
    auto result = gnc::html::rewrite_embedded_objects (
        "<object classid=\"good\">g</object><object classid=\"bad\">"sv,
        [] (std::string_view, std::string_view) { return std::string {"!"}; });
    ASSERT_FALSE (result.has_value ());
    EXPECT_EQ (result.error (), RewriteError::unterminated_object);
}

/* ------------------------------------------------------------------ */
/* make_error_page                                                     */
/* ------------------------------------------------------------------ */

TEST (GncHtmlSupport, ErrorPageMatchesTheLegacyFormat)
{
    auto page = gnc::html::make_error_page ("Not found", "The specified URL could not be loaded.");

    auto* legacy = g_strdup_printf (LEGACY_404_FORMAT, "Not found",
                                    "The specified URL could not be loaded.");
    EXPECT_EQ (page, legacy);
    g_free (legacy);
}

/* ------------------------------------------------------------------ */
/* file_uri_from_path / path_from_file_uri                             */
/* ------------------------------------------------------------------ */

TEST (GncHtmlSupport, FileUriFromPosixPath)
{
    EXPECT_EQ (gnc::html::file_uri_from_path ("/tmp/gnc-report-abc123.html"),
               "file:///tmp/gnc-report-abc123.html");
}

TEST (GncHtmlSupport, FileUriFromWindowsPath)
{
    EXPECT_EQ (gnc::html::file_uri_from_path ("C:\\Users\\me\\report.html"),
               "file:///C:/Users/me/report.html");
}

TEST (GncHtmlSupport, FileUriFromRelativePathIsRooted)
{
    EXPECT_EQ (gnc::html::file_uri_from_path ("reports/x.html"), "file:///reports/x.html");
}

TEST (GncHtmlSupport, FileUriEncodesUnsafeCharacters)
{
    /* macOS puts temporary files under a per-session directory, and a
     * user's own export path can contain anything at all. */
    EXPECT_EQ (gnc::html::file_uri_from_path ("/tmp/my report.html"),
               "file:///tmp/my%20report.html");
    EXPECT_EQ (gnc::html::file_uri_from_path ("/tmp/a#b.html"), "file:///tmp/a%23b.html");
    EXPECT_EQ (gnc::html::file_uri_from_path ("/tmp/100%.html"), "file:///tmp/100%25.html");
    EXPECT_EQ (gnc::html::file_uri_from_path ("/tmp/a?b.html"), "file:///tmp/a%3Fb.html");
}

TEST (GncHtmlSupport, FileUriEncodesUtf8PerByte)
{
    EXPECT_EQ (gnc::html::file_uri_from_path ("/tmp/caf\xc3\xa9.html"),
               "file:///tmp/caf%C3%A9.html");
}

TEST (GncHtmlSupport, PathFromFileUriRejectsOtherSchemes)
{
    EXPECT_FALSE (gnc::html::path_from_file_uri ("https://example.com/x").has_value ());
    EXPECT_FALSE (gnc::html::path_from_file_uri ("gnc-report:id=42").has_value ());
    EXPECT_FALSE (gnc::html::path_from_file_uri ("").has_value ());
}

TEST (GncHtmlSupport, PathFromFileUriDropsTheFragment)
{
    EXPECT_EQ (gnc::html::path_from_file_uri ("file:///tmp/x.html#anchor"), "/tmp/x.html");
}

TEST (GncHtmlSupport, PathFromFileUriUnwrapsTheDriveLetter)
{
    EXPECT_EQ (gnc::html::path_from_file_uri ("file:///C:/Users/me/report.html"),
               "C:/Users/me/report.html");
}

TEST (GncHtmlSupport, FileUriRoundTrips)
{
    for (auto path : {"/tmp/plain.html"sv, "/tmp/with space.html"sv, "/tmp/caf\xc3\xa9.html"sv,
                      "/tmp/100%.html"sv, "/tmp/a#b.html"sv, "/var/folders/xy/T/gnc.html"sv})
    {
        EXPECT_EQ (gnc::html::path_from_file_uri (gnc::html::file_uri_from_path (path)), path);
    }
}

/* ------------------------------------------------------------------ */
/* with_fragment                                                       */
/* ------------------------------------------------------------------ */

TEST (GncHtmlSupport, FragmentIsAppended)
{
    EXPECT_EQ (gnc::html::with_fragment ("file:///tmp/x.html", "section-2"),
               "file:///tmp/x.html#section-2");
}

TEST (GncHtmlSupport, FragmentReplacesAnExistingOne)
{
    EXPECT_EQ (gnc::html::with_fragment ("file:///tmp/x.html#old", "new"),
               "file:///tmp/x.html#new");
}

TEST (GncHtmlSupport, EmptyFragmentStripsAnExistingOne)
{
    EXPECT_EQ (gnc::html::with_fragment ("file:///tmp/x.html#old", ""), "file:///tmp/x.html");
    EXPECT_EQ (gnc::html::with_fragment ("file:///tmp/x.html", ""), "file:///tmp/x.html");
}

TEST (GncHtmlSupport, FragmentIsEncoded)
{
    EXPECT_EQ (gnc::html::with_fragment ("file:///tmp/x.html", "Assets:Current Assets"),
               "file:///tmp/x.html#Assets:Current%20Assets");
    EXPECT_EQ (gnc::html::with_fragment ("file:///tmp/x.html", "a#b"), "file:///tmp/x.html#a%23b");
}

/* ------------------------------------------------------------------ */
/* PendingNavigation                                                   */
/* ------------------------------------------------------------------ */

TEST (GncHtmlPendingNavigation, StartsEmpty)
{
    PendingNavigation pending;
    EXPECT_TRUE (pending.empty ());
    EXPECT_TRUE (std::holds_alternative<std::monostate> (pending.peek ()));
}

TEST (GncHtmlPendingNavigation, TakeOnEmptyYieldsNothing)
{
    PendingNavigation pending;
    EXPECT_TRUE (std::holds_alternative<std::monostate> (pending.take ()));
    EXPECT_TRUE (pending.empty ());
}

TEST (GncHtmlPendingNavigation, RemembersAUri)
{
    PendingNavigation pending;
    pending.set_uri ("file:///tmp/x.html");

    EXPECT_FALSE (pending.empty ());
    auto request = pending.take ();
    ASSERT_TRUE (std::holds_alternative<UriRequest> (request));
    EXPECT_EQ (std::get<UriRequest> (request).uri, "file:///tmp/x.html");
    EXPECT_TRUE (pending.empty ());
}

TEST (GncHtmlPendingNavigation, RemembersAnHtmlString)
{
    PendingNavigation pending;
    pending.set_html ("<html>hi</html>");

    auto request = pending.take ();
    ASSERT_TRUE (std::holds_alternative<HtmlRequest> (request));
    EXPECT_EQ (std::get<HtmlRequest> (request).html, "<html>hi</html>");
    EXPECT_TRUE (pending.empty ());
}

TEST (GncHtmlPendingNavigation, OnlyTheLatestRequestSurvives)
{
    /* An earlier request would be painted over instantly, so it is
     * dropped rather than queued. */
    PendingNavigation pending;
    pending.set_uri ("file:///tmp/first.html");
    pending.set_html ("<html>second</html>");
    pending.set_uri ("file:///tmp/third.html");

    auto request = pending.take ();
    ASSERT_TRUE (std::holds_alternative<UriRequest> (request));
    EXPECT_EQ (std::get<UriRequest> (request).uri, "file:///tmp/third.html");
}

TEST (GncHtmlPendingNavigation, PeekDoesNotConsume)
{
    PendingNavigation pending;
    pending.set_uri ("file:///tmp/x.html");

    ASSERT_TRUE (std::holds_alternative<UriRequest> (pending.peek ()));
    EXPECT_FALSE (pending.empty ());
    EXPECT_TRUE (std::holds_alternative<UriRequest> (pending.peek ()));
}

TEST (GncHtmlPendingNavigation, ClearDiscardsTheRequest)
{
    PendingNavigation pending;
    pending.set_html ("<html>dropped</html>");
    pending.clear ();

    EXPECT_TRUE (pending.empty ());
    EXPECT_TRUE (std::holds_alternative<std::monostate> (pending.take ()));
}
