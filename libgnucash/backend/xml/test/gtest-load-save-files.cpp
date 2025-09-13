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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/
#include <glib.h>
#include <glib/gfileutils.h>
#include <glib/gstdio.h>

#include <config.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <errno.h>
#include <string.h>
#include <zlib.h>

#include <cstdlib>
#include <functional>
#include <memory>
#include <string>
#include <vector>

#include <cashobjects.h>
#include <TransLog.h>
#include <gnc-engine.h>
#include <gnc-prefs.h>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcpp"
#include <gtest/gtest.h>
#pragma GCC diagnostic pop
#include <unittest-support.h>

#include "../gnc-backend-xml.h"
#include "../io-gncxml-v2.h"

#define GNC_LIB_NAME "gncmod-backend-xml"
#define GNC_LIB_REL_PATH "xml"

static std::vector<unsigned char> read_file (std::string filename)
{
    gchar *contents;
    gsize length = 0;

    if (g_file_get_contents (filename.c_str (), &contents, &length, nullptr))
    {
        std::vector<unsigned char> data(length);

        memcpy (data.data (), contents, length);
        g_free (contents);
        return data;
    }
    else
    {
        return {};
    }
}

static bool
compare_files (std::string filename1, std::string filename2)
{
    auto contents1 = read_file (filename1);
    auto contents2 = read_file (filename2);

    if (contents1.size () > 0 && contents1.size () == contents2.size ()
            && !memcmp(contents1.data (), contents2.data (), contents1.size ())) {
        return true;
    } else {
        ADD_FAILURE() << "compare_files: " << filename1 << " and " << filename2 << " are different";
        return false;
    }
}

static bool
decompress_file (std::string filename, const std::vector<unsigned char> &in, std::vector<unsigned char> &out)
{
    /* 037 0213 are the header id bytes for a gzipped file. */
    if (in.size () < 2 || in[0] != 037 || in[1] != 0213)
    {
        ADD_FAILURE() << "decompress_file: " << filename << " is not compressed";
        return false;
    }

    z_stream stream{};
    stream.next_in = const_cast<unsigned char*>(in.data ());
    stream.avail_in = in.size ();
    stream.zalloc = Z_NULL;
    stream.zfree = Z_NULL;
    stream.opaque = Z_NULL;
    stream.next_out = out.data ();
    stream.avail_out = out.size ();

    /* "add 16 to decode only the gzip format" */
    int ret = inflateInit2 (&stream, 16 + MAX_WBITS);
    if (ret != Z_OK)
    {
        ADD_FAILURE() << "decompress_file: " << filename << " could not be uncompressed (inflateInit): " << ret;
        return false;
    }

    ret = inflate (&stream, Z_NO_FLUSH);
    if (ret != Z_STREAM_END)
    {
        ADD_FAILURE() << "decompress_file: " << filename << " could not be uncompressed (inflate "
            << in.size () << " into " << out.size() << "): " << ret << " " << stream.msg
            << " (avail_in " << stream.avail_in << " avail_out " << stream.avail_out << ")";
        inflateEnd (&stream);
        return false;
    }

    ret = inflateEnd (&stream);
    if (ret != Z_OK)
    {
        ADD_FAILURE() << "decompress_file: " << filename << " could not be uncompressed (inflateEnd): " << ret;
        return false;
    }

    if (stream.avail_in)
    {
        ADD_FAILURE() << "decompress_file: " << filename << " has unused compressed data: " << stream.avail_in;
        return false;
    }

    out.resize (stream.avail_out);

    return true;
}

static bool
compare_compressed_files (std::string uncompressed_filename1, std::string compressed_filename2)
{
    auto uncompressed_contents1 = read_file (uncompressed_filename1);
    auto compressed_contents2 = read_file (compressed_filename2);
    /* Allow some space to grow beyond the expected size */
    std::vector<unsigned char> uncompressed_contents2(uncompressed_contents1.size () * 2);

    if (!decompress_file (compressed_filename2, compressed_contents2, uncompressed_contents2))
        return false;

    if (uncompressed_contents1.size () > 0
        && uncompressed_contents1.size () != uncompressed_contents2.size ())
    {
        ADD_FAILURE() << "compare_compressed_files: " << uncompressed_filename1
            << " and " << compressed_filename2 << " are different sizes or empty ("
            << uncompressed_contents1.size () << " and "
            << uncompressed_contents2.size () << ")";
        return false;
    }

    if (!memcmp(uncompressed_contents1.data (), uncompressed_contents2.data (), uncompressed_contents1.size ())) {
        return true;
    } else {
        ADD_FAILURE() << "compare_compressed_files: " << uncompressed_filename1
            << " and " << compressed_filename2 << " are different";
        return false;
    }
}

/* The original file is used for comparisons. The file will be different when
 * there are future changes in the GnuCash output and needs to be updated if
 * that happens.
 *
 * Using the same file each time also checks that nothing in the file will swap
 * between two stable states (bug 746937).
 */
class LoadSaveFiles : public testing::TestWithParam<std::string>
{
public:
    static void SetUpTestSuite ()
    {
        g_setenv ("GNC_UNINSTALLED", "1", TRUE);
        qof_init ();
        cashobjects_register ();
        ASSERT_TRUE(qof_load_backend_library (GNC_LIB_REL_PATH, GNC_LIB_NAME)) << "loading gnc-backend-xml GModule failed";
        xaccLogDisable ();
    }

    static void TearDownTestSuite ()
    {
        qof_close ();
    }
};

#define QOF_SESSION_CHECKED_CALL(_function, _session, ...) \
    do { \
        _function (_session.get (), ## __VA_ARGS__); \
        ASSERT_EQ (qof_session_get_error (_session.get ()), 0) << #_function \
            << " (" << #_session << ".get (), " << #__VA_ARGS__ << "): " << qof_session_get_error (_session.get ()) \
            << " \"" << qof_session_get_error_message (_session.get ()) << "\""; \
    } while (0)

TEST_P(LoadSaveFiles, test_file)
{
    auto filename = GetParam();
    /* Verify that we can write a compressed version of the original file that
     * has the original content when uncompressed.
     */
    auto new_compressed_file = filename + "-test-compressed~";
    /* Verify that we can read a compressed file and write an uncompressed file
     * that has the original content.
     */
    auto new_uncompressed_file = filename + "-test-uncompressed~";
    const char *logdomain = "backend.xml";
    GLogLevelFlags loglevel = static_cast<decltype (loglevel)>
                              (G_LOG_LEVEL_WARNING);
    TestErrorStruct check = { loglevel, const_cast<char*> (logdomain), nullptr };
    g_log_set_handler (logdomain, loglevel,
                       (GLogFunc)test_checked_handler, &check);

    {
        auto load_uncompressed_session = std::shared_ptr<QofSession>{qof_session_new (qof_book_new ()), qof_session_destroy};

        QOF_SESSION_CHECKED_CALL(qof_session_begin, load_uncompressed_session, filename.c_str (), SESSION_READ_ONLY);
        QOF_SESSION_CHECKED_CALL(qof_session_load, load_uncompressed_session, nullptr);

        auto save_compressed_session = std::shared_ptr<QofSession>{qof_session_new (nullptr), qof_session_destroy};

        g_unlink (new_compressed_file.c_str ());
        g_unlink ((new_compressed_file + ".LCK").c_str ());
        QOF_SESSION_CHECKED_CALL(qof_session_begin, save_compressed_session, new_compressed_file.c_str (), SESSION_NEW_OVERWRITE);

        qof_event_suspend ();
        qof_session_swap_data (load_uncompressed_session.get (), save_compressed_session.get ());
        qof_book_mark_session_dirty (qof_session_get_book (save_compressed_session.get ()));
        qof_event_resume ();

        qof_session_end (load_uncompressed_session.get ());

        gnc_prefs_set_file_save_compressed (TRUE);
        QOF_SESSION_CHECKED_CALL(qof_session_save, save_compressed_session, nullptr);

        qof_session_end (save_compressed_session.get ());
    }

    if (!compare_compressed_files (filename, new_compressed_file))
        return;

    {
        auto load_compressed_session = std::shared_ptr<QofSession>{qof_session_new (qof_book_new ()), qof_session_destroy};

        QOF_SESSION_CHECKED_CALL(qof_session_begin, load_compressed_session, new_compressed_file.c_str (), SESSION_READ_ONLY);
        QOF_SESSION_CHECKED_CALL(qof_session_load, load_compressed_session, nullptr);

        auto save_uncompressed_session = std::shared_ptr<QofSession>{qof_session_new (nullptr), qof_session_destroy};

        g_unlink (new_uncompressed_file.c_str ());
        g_unlink ((new_uncompressed_file + ".LCK").c_str ());
        QOF_SESSION_CHECKED_CALL(qof_session_begin, save_uncompressed_session, new_uncompressed_file.c_str (), SESSION_NEW_OVERWRITE);

        qof_event_suspend ();
        qof_session_swap_data (load_compressed_session.get (), save_uncompressed_session.get ());
        qof_book_mark_session_dirty (qof_session_get_book (save_uncompressed_session.get ()));
        qof_event_resume ();

        qof_session_end (load_compressed_session.get ());

        gnc_prefs_set_file_save_compressed (FALSE);
        QOF_SESSION_CHECKED_CALL(qof_session_save, save_uncompressed_session, nullptr);

        qof_session_end (save_uncompressed_session.get ());
    }

    if (!compare_files (filename, new_uncompressed_file))
        return;
}

std::vector<std::string> ListTestCases ();

INSTANTIATE_TEST_SUITE_P(
    LoadSaveFilesDir,
    LoadSaveFiles,
    testing::ValuesIn (ListTestCases ()));

std::vector<std::string> ListTestCases ()
{
    std::vector<std::string> files;
    const char *location = g_getenv ("GNC_TEST_FILES");

    if (!location)
        location = "test-files/load-save";

    std::shared_ptr<GDir> dir{g_dir_open (location, 0, nullptr), g_dir_close};
    if (dir)
    {
        const gchar *entry;

        while ((entry = g_dir_read_name (dir.get ())) != nullptr)
        {
            if (g_str_has_suffix (entry, ".gnucash"))
            {
                std::shared_ptr<gchar> to_open{g_build_filename (location, entry, (gchar*)nullptr), g_free};

                if (!g_file_test (to_open.get (), G_FILE_TEST_IS_DIR))
                    files.push_back (to_open.get());
            }
        }
    }
    else
    {
        ADD_FAILURE() << "unable to open directory " << location;
    }

    EXPECT_FALSE(files.empty()) << "no files found in " << location;
    return files;
}
