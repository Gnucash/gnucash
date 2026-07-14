/********************************************************************
 * gtest-gnc-state.cpp: test suite for gnc-state                    *
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
 * along with this program; if not, you can retrieve it from        *
 * https://www.gnu.org/licenses/old-licenses/gpl-2.0.html           *
 * or contact:                                                      *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 ********************************************************************/
#include "config.h"
#include <glib.h>
#include "../gnc-state.h"
#include <qof.h>
#include "gnc-filepath-utils.h"
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcpp"
#include <gtest/gtest.h>
#pragma GCC diagnostic pop

/* Test seam exported (without a public header) by qofsession.cpp: set a
 * session's URI directly, without opening a backend. Used the same way as
 * libgnucash/engine/test/test-qofsession-old.cpp. */
extern void (*p_qof_session_set_uri) (QofSession *, char const *);
void init_static_qofsession_pointers (void);

/* gnc_state_get_current() lazily creates a process-wide singleton GKeyFile
 * and always returns the same pointer thereafter. */
TEST(GncState, GetCurrentCreatesSingleton)
{
    GKeyFile *kf = gnc_state_get_current();
    ASSERT_NE(kf, nullptr);

    GKeyFile *kf2 = gnc_state_get_current();
    EXPECT_EQ(kf, kf2);
}

/* gnc_state_drop_sections_for() removes every group whose name contains the
 * given substring and returns the number of groups removed. */
TEST(GncState, DropSectionsForMatchesSubstring)
{
    GKeyFile *kf = gnc_state_get_current();

    g_key_file_set_string(kf, "Account_abc", "key", "value");
    g_key_file_set_string(kf, "Register_abc", "key", "value");
    g_key_file_set_string(kf, "Window_def", "key", "value");

    gint dropped = gnc_state_drop_sections_for("abc");

    EXPECT_EQ(dropped, 2);
    EXPECT_FALSE(g_key_file_has_group(kf, "Account_abc"));
    EXPECT_FALSE(g_key_file_has_group(kf, "Register_abc"));
    EXPECT_TRUE(g_key_file_has_group(kf, "Window_def"));

    /* clean up the group we left behind so tests stay independent */
    g_key_file_remove_group(kf, "Window_def", nullptr);
}

/* When nothing matches, no groups are removed and the count is zero. */
TEST(GncState, DropSectionsForNoMatchReturnsZero)
{
    gint dropped = gnc_state_drop_sections_for("no-such-section-xyz");
    EXPECT_EQ(dropped, 0);
}

/* Fixture that drives the on-disk save/load path. It redirects the state
 * directory to a throwaway temp dir via GNC_DATA_HOME and gives the session a
 * URI without opening a real backend. */
class GncStateFile : public ::testing::Test
{
protected:
    gchar      *m_data_home      = nullptr;
    gchar      *m_saved_builddir = nullptr;
    QofSession *m_session        = nullptr;

    void SetUp() override
    {
        qof_init();
        m_data_home = g_dir_make_tmp("gnc-state-XXXXXX", nullptr);
        ASSERT_NE(m_data_home, nullptr);
        /* Under ctest the harness sets GNC_UNINSTALLED=YES and GNC_BUILDDIR,
         * and gnc-filepath roots user data at $GNC_BUILDDIR/gnc_data_home,
         * ignoring GNC_DATA_HOME. Point both at our throwaway dir so each test
         * gets an isolated books/ directory whichever way it is launched, and
         * so book_path() (via gnc_build_book_path) matches where the code under
         * test actually reads and writes. */
        g_setenv("GNC_DATA_HOME", m_data_home, TRUE);
        m_saved_builddir = g_strdup(g_getenv("GNC_BUILDDIR"));
        g_setenv("GNC_BUILDDIR", m_data_home, TRUE);
        gnc_filepath_init();

        m_session = qof_session_new(qof_book_new());
        init_static_qofsession_pointers();
    }

    void TearDown() override
    {
        qof_session_destroy(m_session);
        g_unsetenv("GNC_DATA_HOME");
        if (m_saved_builddir)
        {
            g_setenv("GNC_BUILDDIR", m_saved_builddir, TRUE);
            g_free(m_saved_builddir);
            m_saved_builddir = nullptr;
        }
        else
            g_unsetenv("GNC_BUILDDIR");
        gnc_filepath_init();
        g_free(m_data_home);
        qof_close();
    }

    /* Path of a state file as the code under test computes it. Use the
     * production builder rather than duplicating the layout, which is rooted
     * at a platform- and environment-dependent user-data directory, not
     * directly under GNC_DATA_HOME. */
    gchar *book_path(const char *basename)
    {
        return gnc_build_book_path(basename);
    }

    /* The session book's guid in the same encoding gnc-state uses. */
    void book_guid_string(gchar *buf)
    {
        QofBook *book = qof_session_get_book(m_session);
        const GncGUID *guid = qof_entity_get_guid(QOF_INSTANCE(book));
        guid_to_string_buff(guid, buf);
    }

    /* Write a state file into the books/ dir with the given book guid and a
     * recognisable seeded section, so set_base() finds it on disk. */
    void write_state_file(const char *basename, const char *book_guid)
    {
        GKeyFile *kf = g_key_file_new();
        g_key_file_set_string(kf, STATE_FILE_TOP, STATE_FILE_BOOK_GUID, book_guid);
        g_key_file_set_string(kf, "Seeded", "seedkey", "seedval");

        gchar *path = book_path(basename);
        gchar *dir  = g_path_get_dirname(path);
        g_mkdir_with_parents(dir, 0700);
        g_free(dir);

        gchar *data = g_key_file_to_data(kf, nullptr, nullptr);
        g_file_set_contents(path, data, -1, nullptr);
        g_free(data);
        g_free(path);
        g_key_file_free(kf);
    }
};

/* A session with no URI must not attempt to write a state file. */
TEST_F(GncStateFile, SaveWithEmptyUriDoesNothing)
{
    p_qof_session_set_uri(m_session, "");
    gnc_state_save(m_session);
    SUCCEED();
}

/* A file:// URI uses the file's basename; state survives a save/load round
 * trip once the book guid has been stamped into the state (as the gui does). */
TEST_F(GncStateFile, FileUriSaveLoadRoundTrip)
{
    p_qof_session_set_uri(m_session, "file:///MyTestBook.gnucash");

    QofBook *book = qof_session_get_book(m_session);
    const GncGUID *guid = qof_entity_get_guid(QOF_INSTANCE(book));
    gchar guid_string[GUID_ENCODING_LENGTH + 1];
    guid_to_string_buff(guid, guid_string);

    GKeyFile *state = gnc_state_get_current();
    g_key_file_set_string(state, STATE_FILE_TOP, STATE_FILE_BOOK_GUID, guid_string);
    g_key_file_set_string(state, "TestSection", "testkey", "testval");

    gnc_state_save(m_session);

    gchar *path = book_path("MyTestBook.gnucash" STATE_FILE_EXT);
    EXPECT_TRUE(g_file_test(path, G_FILE_TEST_EXISTS));
    g_free(path);

    GKeyFile *loaded = gnc_state_load(m_session);
    gchar *val = g_key_file_get_string(loaded, "TestSection", "testkey", nullptr);
    EXPECT_STREQ(val, "testval");
    g_free(val);
}

/* For a database URI with no user name, g_strjoin() treats the missing
 * component as a terminator, so the basename is truncated and the database
 * name is dropped. This pins that long-standing contract. */
TEST_F(GncStateFile, DbUriBasenameTruncatesAtMissingComponent)
{
    p_qof_session_set_uri(m_session, "postgres://www.gnucash.org/gnucash");
    gnc_state_save(m_session);

    gchar *truncated = book_path("postgres_www.gnucash.org" STATE_FILE_EXT);
    EXPECT_TRUE(g_file_test(truncated, G_FILE_TEST_EXISTS));
    g_free(truncated);

    /* The database name must NOT have made it into the file name. */
    gchar *with_dbname = book_path("postgres_www.gnucash.org_gnucash" STATE_FILE_EXT);
    EXPECT_FALSE(g_file_test(with_dbname, G_FILE_TEST_EXISTS));
    g_free(with_dbname);
}

/* When every component is present the basename joins scheme, host, user and
 * database name with underscores. */
TEST_F(GncStateFile, DbUriBasenameJoinsAllComponents)
{
    p_qof_session_set_uri(m_session, "postgres://dbuser@www.gnucash.org/gnucash");
    gnc_state_save(m_session);

    gchar *path = book_path("postgres_www.gnucash.org_dbuser_gnucash" STATE_FILE_EXT);
    EXPECT_TRUE(g_file_test(path, G_FILE_TEST_EXISTS));
    g_free(path);
}

/* Loading a session with no URI yields an empty (non-NULL) state. */
TEST_F(GncStateFile, LoadWithEmptyUriReturnsEmptyState)
{
    p_qof_session_set_uri(m_session, "");
    GKeyFile *loaded = gnc_state_load(m_session);
    EXPECT_NE(loaded, nullptr);
}

/* A pre-existing .gcm file owned by a different book is skipped, and a new
 * numbered file name is chosen instead. */
TEST_F(GncStateFile, FileUriDisambiguatesOnGuidMismatch)
{
    p_qof_session_set_uri(m_session, "file:///MyBook.gnucash");

    /* Seed a state file belonging to some OTHER book. */
    write_state_file("MyBook.gnucash" STATE_FILE_EXT,
                     "00000000000000000000000000000000");

    gnc_state_save(m_session);

    gchar *path = book_path("MyBook.gnucash_2" STATE_FILE_EXT);
    EXPECT_TRUE(g_file_test(path, G_FILE_TEST_EXISTS));
    g_free(path);
}

/* A pre-existing .gcm file whose stored guid can't be parsed is treated as a
 * non-match rather than aborting, so a new numbered file name is chosen. */
TEST_F(GncStateFile, FileUriDisambiguatesOnMalformedGuid)
{
    p_qof_session_set_uri(m_session, "file:///MyBook.gnucash");

    /* Seed a state file whose guid is not a valid guid string. */
    write_state_file("MyBook.gnucash" STATE_FILE_EXT, "not-a-valid-guid");

    gnc_state_save(m_session);

    gchar *path = book_path("MyBook.gnucash_2" STATE_FILE_EXT);
    EXPECT_TRUE(g_file_test(path, G_FILE_TEST_EXISTS));
    g_free(path);
}

/* When no new-style .gcm file exists, an old (pre-2.4.1) extension-less file
 * with a matching guid is found and loaded. */
TEST_F(GncStateFile, FileUriLoadsPre241StyleFile)
{
    p_qof_session_set_uri(m_session, "file:///MyBook.gnucash");

    gchar guid_string[GUID_ENCODING_LENGTH + 1];
    book_guid_string(guid_string);
    write_state_file("MyBook.gnucash", guid_string);

    GKeyFile *loaded = gnc_state_load(m_session);
    gchar *val = g_key_file_get_string(loaded, "Seeded", "seedkey", nullptr);
    EXPECT_STREQ(val, "seedval");
    g_free(val);
}
