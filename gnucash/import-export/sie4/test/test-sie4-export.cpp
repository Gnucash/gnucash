/********************************************************************\
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
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#include <config.h>

#include <algorithm>
#include <iomanip>
#include <iterator>
#include <sstream>
#include <string>

#include <glib.h>
#include <glib/gstdio.h>

#include <cashobjects.h>
#include <gnc-date.h>
#include <gnc-session.h>
#include <gnc-uri-utils.h>
#include <qofbackend.h>
#include <TransLog.h>

#include "sie4-export.h"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcpp"
#include <gtest/gtest.h>
#pragma GCC diagnostic pop

#define GNC_LIB_NAME "gncmod-backend-xml"
#define GNC_LIB_REL_PATH "xml"

namespace
{
bool backend_loaded = false;

std::string
error_message (GError *error)
{
    return error ? error->message : "";
}

std::string
test_file_path (const gchar *filename)
{
    auto location = g_getenv ("GNC_TEST_FILES");
    if (!location)
        location = ".";

    auto full_path = g_build_filename (location, filename,
                                       static_cast<gchar*> (nullptr));
    std::string result{full_path};
    g_free (full_path);
    return result;
}

bool
load_book_as_current_session (const gchar *filename)
{
    auto session = qof_session_new (qof_book_new ());
    auto url = gnc_uri_normalize_uri (filename, FALSE);

    qof_session_begin (session, url, SESSION_READ_ONLY);
    g_free (url);
    if (qof_session_get_error (session) != ERR_BACKEND_NO_ERR)
    {
        ADD_FAILURE () << "qof_session_begin: "
                       << qof_session_get_error_message (session);
        qof_session_destroy (session);
        return false;
    }

    qof_session_load (session, nullptr);
    if (qof_session_get_error (session) != ERR_BACKEND_NO_ERR)
    {
        ADD_FAILURE () << "qof_session_load: "
                       << qof_session_get_error_message (session);
        qof_session_destroy (session);
        return false;
    }

    gnc_clear_current_session ();
    gnc_set_current_session (session);
    return true;
}

std::string
read_file (const gchar *filename)
{
    gchar *contents = nullptr;
    gsize length = 0;
    GError *raw_error = nullptr;

    if (!g_file_get_contents (filename, &contents, &length, &raw_error))
    {
        ADD_FAILURE () << "Unable to read " << filename << ": "
                       << error_message (raw_error);
        g_clear_error (&raw_error);
        return {};
    }

    std::string result{contents, length};
    g_free (contents);
    return result;
}

std::string
canonicalize_generation_time (std::string contents)
{
    auto gen_pos = contents.find ("#GEN ");
    if (gen_pos == std::string::npos)
        return contents;

    auto gen_end = contents.find ('\n', gen_pos);
    contents.replace (gen_pos,
                      gen_end == std::string::npos ?
                          std::string::npos : gen_end - gen_pos,
                      "#GEN <generated>");
    return contents;
}

std::string
escaped_text (const std::string& text)
{
    std::ostringstream ss;

    for (auto byte : text)
    {
        auto ch = static_cast<unsigned char> (byte);
        if (ch == '\t')
            ss << "\\t";
        else if (ch == '\r')
            ss << "\\r";
        else if (ch >= 0x20 && ch <= 0x7e)
            ss << ch;
        else
            ss << "\\x" << std::hex << std::setw (2)
               << std::setfill ('0') << static_cast<unsigned> (ch)
               << std::dec << std::setfill (' ');
    }

    return ss.str ();
}

size_t
line_number_at (const std::string& contents, size_t offset)
{
    return static_cast<size_t> (std::count (contents.begin (),
                                            contents.begin () + offset,
                                            '\n')) + 1;
}

std::string
line_at (const std::string& contents, size_t offset)
{
    auto line_begin = offset == 0 ? std::string::npos :
        contents.rfind ('\n', offset - 1);
    line_begin = line_begin == std::string::npos ? 0 : line_begin + 1;

    auto line_end = contents.find ('\n', offset);
    if (line_end == std::string::npos)
        line_end = contents.size ();

    return contents.substr (line_begin, line_end - line_begin);
}

std::string
byte_at (const std::string& contents, size_t offset)
{
    if (offset >= contents.size ())
        return "<EOF>";

    std::ostringstream ss;
    ss << "0x" << std::hex << std::setw (2) << std::setfill ('0')
       << static_cast<unsigned> (
              static_cast<unsigned char> (contents[offset]));
    return ss.str ();
}

bool
compare_files (const gchar *expected_file, const gchar *actual_file,
               std::string& failure_message)
{
    auto expected = canonicalize_generation_time (read_file (expected_file));
    auto actual = canonicalize_generation_time (read_file (actual_file));
    auto compare_size = std::min (expected.size (), actual.size ());

    auto mismatch = std::mismatch (expected.begin (),
                                   expected.begin () + compare_size,
                                   actual.begin ());
    auto offset = static_cast<size_t> (
        std::distance (expected.begin (), mismatch.first));
    auto files_match = offset == compare_size && expected.size () == actual.size ();

    if (!files_match)
    {
        std::ostringstream ss;
        ss << expected_file << " and " << actual_file
           << " differ at byte " << offset
           << " on line " << line_number_at (expected, offset)
           << " (expected size " << expected.size ()
           << ", actual size " << actual.size ()
           << "): expected " << byte_at (expected, offset)
           << ", actual " << byte_at (actual, offset)
           << "\nExpected: "
           << escaped_text (line_at (expected, offset))
           << "\nActual:   "
           << escaped_text (line_at (actual, offset));
        failure_message = ss.str ();
        return false;
    }

    failure_message.clear ();
    return true;
}

void
expect_files_equal (const gchar *expected_file, const gchar *actual_file)
{
    std::string failure_message;

    if (!compare_files (expected_file, actual_file, failure_message))
        ADD_FAILURE () << failure_message;
}

class Sie4Export : public testing::Test
{
public:
    static void SetUpTestSuite ()
    {
        g_setenv ("GNC_UNINSTALLED", "1", TRUE);
        qof_init ();
        cashobjects_register ();
        backend_loaded = qof_load_backend_library (GNC_LIB_REL_PATH, GNC_LIB_NAME);
        EXPECT_TRUE (backend_loaded)
            << "loading gnc-backend-xml GModule failed";
        xaccLogDisable ();
    }

    static void TearDownTestSuite ()
    {
        gnc_clear_current_session ();
        qof_close ();
    }

    void TearDown () override
    {
        gnc_clear_current_session ();
    }
};

bool
export_book (const std::string& input_file, const std::string& output_file)
{
    if (!load_book_as_current_session (input_file.c_str ()))
        return false;

    GncSie4ExportSettings settings{};
    settings.file_name = output_file.c_str ();
    settings.company_name = "Göteborgs Åkeri AB";
    settings.organization_number = "556677-8899";
    settings.contact = "";
    settings.street_address = "Åsa Andersson";
    settings.postal_address = "Östra vägen 1 123 45 Göteborg";
    settings.phone = "";
    settings.account_plan = "EUBAS97";
    settings.currency_code = "SEK";
    settings.voucher_series = "A";
    settings.current_start = gnc_dmy2time64 (1, 1, 2024);
    settings.current_end = gnc_dmy2time64_end (31, 12, 2024);
    settings.previous_start = gnc_dmy2time64 (1, 1, 2023);
    settings.previous_end = gnc_dmy2time64_end (31, 12, 2023);
    settings.include_business_dimensions = TRUE;
    settings.include_zero_balances = FALSE;
    settings.use_transaction_numbers = TRUE;

    GncSie4ExportResult result{};
    GError *export_raw_error = nullptr;
    auto export_ok = gnc_sie4_export (&settings, &result, &export_raw_error);
    if (!export_ok)
    {
        ADD_FAILURE () << error_message (export_raw_error);
        g_clear_error (&export_raw_error);
        g_unlink (output_file.c_str ());
        return false;
    }
    EXPECT_EQ (0u, result.generated_voucher_numbers);
    return true;
}

bool
make_export_path (gchar **tmp_dir, std::string& output_file)
{
    GError *raw_error = nullptr;
    *tmp_dir = g_dir_make_tmp ("gnucash-sie4-export-XXXXXX", &raw_error);
    if (!*tmp_dir)
    {
        ADD_FAILURE () << error_message (raw_error);
        g_clear_error (&raw_error);
        return false;
    }

    auto full_path = g_build_filename (*tmp_dir, "test-books.se",
                                       static_cast<gchar*> (nullptr));
    output_file = full_path;
    g_free (full_path);
    return true;
}

void
remove_export_path (gchar *tmp_dir, const std::string& output_file)
{
    g_unlink (output_file.c_str ());
    g_rmdir (tmp_dir);
    g_free (tmp_dir);
}
}

TEST_F (Sie4Export, ExportsTestBooks)
{
    /* Reference-output test for the known-good SIE4 fixture. The comparison
     * canonicalizes #GEN because that line is intentionally time-dependent. */
    if (!backend_loaded)
        return;

    auto input_file = test_file_path ("test-books.gnucash");
    auto expected_file = test_file_path ("expected-test-books.se");

    if (!g_file_test (input_file.c_str (), G_FILE_TEST_EXISTS))
    {
        ADD_FAILURE () << input_file << " does not exist";
        return;
    }

    if (!g_file_test (expected_file.c_str (), G_FILE_TEST_EXISTS))
    {
        ADD_FAILURE () << expected_file << " does not exist";
        return;
    }

    gchar *tmp_dir = nullptr;
    std::string output_file;
    if (!make_export_path (&tmp_dir, output_file))
        return;

    if (!export_book (input_file, output_file))
    {
        remove_export_path (tmp_dir, output_file);
        return;
    }

    expect_files_equal (expected_file.c_str (), output_file.c_str ());
    remove_export_path (tmp_dir, output_file);
}

TEST_F (Sie4Export, DetectsExportMismatch)
{
    /* The fail-books fixture differs from test-books only in one exported
     * split memo. This verifies that the reference-output comparison catches the
     * mismatch and reports the expected and actual SIE lines. */
    if (!backend_loaded)
        return;

    auto input_file = test_file_path ("fail-books.gnucash");
    auto expected_file = test_file_path ("expected-test-books.se");

    if (!g_file_test (input_file.c_str (), G_FILE_TEST_EXISTS))
    {
        ADD_FAILURE () << input_file << " does not exist";
        return;
    }

    if (!g_file_test (expected_file.c_str (), G_FILE_TEST_EXISTS))
    {
        ADD_FAILURE () << expected_file << " does not exist";
        return;
    }

    gchar *tmp_dir = nullptr;
    std::string output_file;
    if (!make_export_path (&tmp_dir, output_file))
        return;

    if (!export_book (input_file, output_file))
    {
        remove_export_path (tmp_dir, output_file);
        return;
    }

    std::string failure_message;
    EXPECT_FALSE (compare_files (expected_file.c_str (), output_file.c_str (),
                                 failure_message));
    EXPECT_NE (std::string::npos,
               failure_message.find (
                   "Expected: #TRANS 1510 {} 1250.00 20240315 \"kundfaktura\""));
    EXPECT_NE (std::string::npos,
               failure_message.find (
                   "Actual:   #TRANS 1510 {} 1250.00 20240315 \"UNIT TEST CHECK\""));

    remove_export_path (tmp_dir, output_file);
}
