#include "config.h"
#include <glib.h>
#include "../gnc-html-webkit2.cpp"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcpp"
#include <gtest/gtest.h>
#pragma GCC diagnostic pop

/* extract_base_name() (defined statically in gnc-html-webkit2.cpp) parses
 * the "location" of a page being loaded into the html viewer, and returns
 * the directory portion of that location so that further relative links
 * on the page can be resolved against it.
 *
 * For URL_TYPE_HTTP/URL_TYPE_SECURE/URL_TYPE_FTP locations, "path" is a
 * schema-relative URL of the form "//host[:port][/path...]"; for every
 * other type (URL_TYPE_FILE and friends) it is treated as a plain
 * filesystem path, absolute or relative.
 */

class ExtractBaseNameTest : public ::testing::TestWithParam<
    std::tuple<const char*, const char*, const char*>>
{
};

TEST_P (ExtractBaseNameTest, Cases)
{
    const auto& [type, path, expected] = GetParam();
    char *result = extract_base_name (type, path);
    if (expected)
        EXPECT_STREQ (result, expected);
    else
        EXPECT_EQ (result, nullptr);
    g_free (result);
}

INSTANTIATE_TEST_SUITE_P (Http, ExtractBaseNameTest, ::testing::Values(
    std::make_tuple (URL_TYPE_HTTP, "//www.gnucash.org/docs/help/index.html", "//www.gnucash.org/docs/help/"),
    /* already a directory (trailing slash) is left untouched */
    std::make_tuple (URL_TYPE_HTTP, "//www.gnucash.org/docs/help/", "//www.gnucash.org/docs/help/"),
    /* single path segment: filename directly under the host root */
    std::make_tuple (URL_TYPE_HTTP, "//www.gnucash.org/index.html", "//www.gnucash.org/"),
    /* host root, explicit trailing slash */
    std::make_tuple (URL_TYPE_HTTP, "//www.gnucash.org/", "//www.gnucash.org/"),
    /* host only, no path at all */
    std::make_tuple (URL_TYPE_HTTP, "//www.gnucash.org", "//www.gnucash.org"),
    /* empty path never matches the "//host" prefix */
    std::make_tuple (URL_TYPE_HTTP, "", nullptr),
    /* missing the leading "//" required for a schema-relative URL */
    std::make_tuple (URL_TYPE_HTTP, "www.gnucash.org/a/b.html", nullptr)
));

INSTANTIATE_TEST_SUITE_P (Secure, ExtractBaseNameTest, ::testing::Values(
    /* URL_TYPE_SECURE is "secure", not "https" - make sure that string is
     * what actually selects the http-style machine/path parsing branch */
    std::make_tuple (URL_TYPE_SECURE, "//example.com/a/b/report.html", "//example.com/a/b/"),
    std::make_tuple (URL_TYPE_SECURE, "//example.com:8443/report.html", "//example.com:8443/")
));

INSTANTIATE_TEST_SUITE_P (Ftp, ExtractBaseNameTest, ::testing::Values(
    std::make_tuple (URL_TYPE_FTP, "//ftp.gnu.org/gnu/gnucash/file.tar.gz", "//ftp.gnu.org/gnu/gnucash/"),
    std::make_tuple (URL_TYPE_FTP, "//ftp.gnu.org/", "//ftp.gnu.org/")
));

INSTANTIATE_TEST_SUITE_P (FileAndOtherTypes, ExtractBaseNameTest, ::testing::Values(
    std::make_tuple (URL_TYPE_FILE, "/usr/share/gnucash/report.html", "/usr/share/gnucash/"),
    std::make_tuple (URL_TYPE_FILE, "/usr/share/gnucash/", "/usr/share/gnucash/"),
    /* relative filesystem paths */
    std::make_tuple (URL_TYPE_FILE, "reports/foo.html", "reports/"),
    std::make_tuple (URL_TYPE_FILE, "reports/", "reports/"),
    /* no directory component at all -> nothing to extract */
    std::make_tuple (URL_TYPE_FILE, "report.html", nullptr),
    std::make_tuple (URL_TYPE_FILE, "", nullptr),
    /* any type other than http/secure/ftp is treated the same as a
     * plain filesystem path */
    std::make_tuple (URL_TYPE_XMLDATA, "/data/accounts/foo.xml", "/data/accounts/"),
    std::make_tuple (URL_TYPE_JUMP, "somewhere/on/the/page.html", "somewhere/on/the/")
));

INSTANTIATE_TEST_SUITE_P (NullPath, ExtractBaseNameTest, ::testing::Values(
    /* a null path must short-circuit before any regex is even attempted,
     * regardless of type */
    std::make_tuple (URL_TYPE_HTTP, nullptr, nullptr),
    std::make_tuple (URL_TYPE_FILE, nullptr, nullptr)
));
