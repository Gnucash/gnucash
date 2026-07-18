/********************************************************************
 * gtest-gnc-string-utils.cpp: Unit tests for gnc-string-utils.     *
 * Copyright 2011 John Ralls <jralls@ceridwen.us>                   *
 * Copyright 2026 Brent McBride <mcbridebt@hotmail.com>             *
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


#include <config.h>
#include <cstring>
#include <glib.h>
#include <gnc-string-utils.h>
#include <gnc-locale-utils.hpp>
#include <unittest-support.h>
#include <gtest/gtest.h>

/* safe_utf8_collate compares with boost::locale::collator via
 * gnc_get_boost_locale(), which returns a usable locale only after
 * gnc_init_boost_locale() has run (done in main() for the application).
 * Initialize it once for the whole test run.
 *
 * Pin the encoding to US-ASCII (LC_ALL=C) so that gnc_locale_from_utf8 and
 * gnc_locale_to_utf8 exercise their conversion (and failure) path rather
 * than the UTF-8 no-op short-circuit, deterministically on any host. */
class BoostLocaleEnvironment : public ::testing::Environment
{
public:
    void SetUp () override
    {
        g_setenv ("LC_ALL", "C", TRUE);
        gnc_init_boost_locale ("");
    }
};

static auto* const boost_locale_env =
    ::testing::AddGlobalTestEnvironment (new BoostLocaleEnvironment);

TEST(GncGlibUtils, safe_utf8_collate)
{
    EXPECT_EQ (0, safe_utf8_collate ("abc", "abc"));
    EXPECT_LT (safe_utf8_collate ("abc", "abd"), 0);
    EXPECT_GT (safe_utf8_collate ("abd", "abc"), 0);

    /* Empty strings are treated the same as nullptr. */
    EXPECT_EQ (0, safe_utf8_collate ("", ""));
    EXPECT_EQ (0, safe_utf8_collate (nullptr, nullptr));
    EXPECT_EQ (0, safe_utf8_collate ("", nullptr));
    EXPECT_EQ (0, safe_utf8_collate (nullptr, ""));

    /* When only one side is empty/nullptr the non-empty side sorts later. */
    EXPECT_EQ (1, safe_utf8_collate ("abc", nullptr));
    EXPECT_EQ (1, safe_utf8_collate ("abc", ""));
    EXPECT_EQ (-1, safe_utf8_collate (nullptr, "abc"));
    EXPECT_EQ (-1, safe_utf8_collate ("", "abc"));
}

TEST(GncGlibUtils, gnc_utf8_validate)
{
    const gchar *valid = "Hello, world";
    const gchar *end = nullptr;

    EXPECT_TRUE (gnc_utf8_validate (valid, -1, &end));
    EXPECT_EQ (valid + strlen (valid), end);

    /* max_len path: validate only a prefix of the string. */
    EXPECT_TRUE (gnc_utf8_validate (valid, 5, &end));
    EXPECT_EQ (valid + 5, end);

    /* Invalid input: end is left pointing at the first bad byte. */
    const gchar *invalid = "abc\xb2\xf3xyz";
    EXPECT_FALSE (gnc_utf8_validate (invalid, -1, &end));
    EXPECT_EQ (invalid + 3, end);

    /* A multi-byte character (here "é" == 0xc3 0xa9) validates when whole,
     * but fails when max_len splits it, with end left at the split point. */
    const gchar *multibyte = "a\xc3\xa9";
    EXPECT_TRUE (gnc_utf8_validate (multibyte, 3, &end));
    EXPECT_EQ (multibyte + 3, end);
    EXPECT_FALSE (gnc_utf8_validate (multibyte, 2, &end));
    EXPECT_EQ (multibyte + 1, end);

    /* A well-formed 6-byte lead byte (0xFC) followed by an invalid
     * continuation byte decodes to (gunichar)-1 while UTF8_LENGTH still
     * matches, exercising the result == -1 rejection rather than the
     * overlong-length check. */
    const gchar *bad_seq = "\xFC\x20";
    EXPECT_FALSE (gnc_utf8_validate (bad_seq, -1, &end));
    EXPECT_EQ (bad_seq, end);
}

TEST(GncGlibUtils, gnc_utf8_strip_invalid_strdup)
{
    /* Valid input yields an equal, independently-allocated copy. */
    gchar *result = gnc_utf8_strip_invalid_strdup ("valid string");
    EXPECT_STREQ ("valid string", result);
    g_free (result);
}

static void
check_strip_invalid_and_controls (const gchar *input)
{
    gchar *str = g_strdup (input);
    const gchar *controls = "\b\f\n\r\t\v\x01\x02\x03\x04\x05\x06\x07"
        "\x08\x09\xa\xb\xc\xd\xe\xf\x10\x11\x12\x13\x14\x15\x16"
        "\x17\x18\x19\x1a\x1b\x1c\x1d\x1e\x1f";
    gchar *msg = g_strdup_printf ("Invalid utf8 string: %s", input);
    const GLogLevelFlags level = static_cast<GLogLevelFlags>
        (G_LOG_LEVEL_WARNING | G_LOG_FLAG_FATAL);
    TestErrorStruct check = {level, nullptr, msg, 0};
    guint handler = g_log_set_handler (nullptr, level,
                                       (GLogFunc)test_null_handler, &check);

    gnc_utf8_strip_invalid_and_controls (str);
    EXPECT_TRUE (g_utf8_validate (str, -1, nullptr));
    EXPECT_EQ (nullptr, strpbrk (str, controls));
    EXPECT_GT (g_utf8_strlen (str, -1), 0);

    g_log_remove_handler (nullptr, handler);
    g_free (str);
    g_free (msg);
}

TEST(GncGlibUtils, gnc_utf8_strip_invalid_and_controls)
{
    check_strip_invalid_and_controls
        ("Η γρήγορη καφέ αλεπού πήδηξε πάνω από την \xb2\xf3ργή σκύλο.");
    check_strip_invalid_and_controls
        ("Η γρήγορη καφέ αλεπού\bπήδηξε\nπάνω από\tτην αργή σκύλο.");
}

TEST(GncGlibUtils, gnc_locale_utf8_roundtrip)
{
    /* Plain ASCII round-trips through any locale encoding. */
    const gchar *ascii = "Plain ASCII text 12345";
    gchar *locale_str = gnc_locale_from_utf8 (ascii);
    ASSERT_NE (nullptr, locale_str);
    gchar *utf8_str = gnc_locale_to_utf8 (locale_str);
    ASSERT_NE (nullptr, utf8_str);
    EXPECT_STREQ (ascii, utf8_str);
    g_free (locale_str);
    g_free (utf8_str);
}

TEST(GncGlibUtils, gnc_locale_conversion_failure)
{
    /* Input that cannot be converted makes boost::locale::conv throw with
     * the stop method; the functions catch it, warn, and return nullptr.
     * 0xff is invalid UTF-8 and non-ASCII, so both directions fail in any
     * locale encoding. */
    const gchar *invalid = "\xff\xfe";
    const GLogLevelFlags level = static_cast<GLogLevelFlags>
        (G_LOG_LEVEL_WARNING | G_LOG_FLAG_FATAL);
    TestErrorStruct check = {level, nullptr, nullptr, 0};
    guint handler = g_log_set_handler (nullptr, level,
                                       (GLogFunc)test_null_handler, &check);

    EXPECT_EQ (nullptr, gnc_locale_from_utf8 (invalid));
    EXPECT_EQ (nullptr, gnc_locale_to_utf8 (invalid));

    g_log_remove_handler (nullptr, handler);
}

static gpointer
add_offset_map_fn (gpointer data, gpointer user_data)
{
    return GINT_TO_POINTER (GPOINTER_TO_INT (data) +
                            GPOINTER_TO_INT (user_data));
}

TEST(GncGlibUtils, gnc_g_list_map)
{
    GList *list = nullptr;
    list = g_list_append (list, GINT_TO_POINTER (1));
    list = g_list_append (list, GINT_TO_POINTER (2));
    list = g_list_append (list, GINT_TO_POINTER (3));

    GList *mapped = gnc_g_list_map (list, add_offset_map_fn,
                                    GINT_TO_POINTER (10));
    ASSERT_EQ (3u, g_list_length (mapped));
    EXPECT_EQ (11, GPOINTER_TO_INT (g_list_nth_data (mapped, 0)));
    EXPECT_EQ (12, GPOINTER_TO_INT (g_list_nth_data (mapped, 1)));
    EXPECT_EQ (13, GPOINTER_TO_INT (g_list_nth_data (mapped, 2)));

    g_list_free (list);
    g_list_free (mapped);
}

TEST(GncGlibUtils, gnc_g_list_cut)
{
    /* A null or empty list is left untouched. */
    GList *empty = nullptr;
    gnc_g_list_cut (&empty, nullptr);
    EXPECT_EQ (nullptr, empty);

    GList *list = nullptr;
    list = g_list_append (list, GINT_TO_POINTER (1));
    list = g_list_append (list, GINT_TO_POINTER (2));
    list = g_list_append (list, GINT_TO_POINTER (3));
    list = g_list_append (list, GINT_TO_POINTER (4));

    /* Cut at the third element: head keeps [1, 2], tail becomes [3, 4]. */
    GList *tail = g_list_nth (list, 2);
    gnc_g_list_cut (&list, tail);

    ASSERT_EQ (2u, g_list_length (list));
    EXPECT_EQ (1, GPOINTER_TO_INT (g_list_nth_data (list, 0)));
    EXPECT_EQ (2, GPOINTER_TO_INT (g_list_nth_data (list, 1)));

    ASSERT_EQ (2u, g_list_length (tail));
    EXPECT_EQ (3, GPOINTER_TO_INT (g_list_nth_data (tail, 0)));
    EXPECT_EQ (4, GPOINTER_TO_INT (g_list_nth_data (tail, 1)));

    g_list_free (list);
    g_list_free (tail);

    /* Cutting at the first element clears the caller's list pointer. */
    GList *head = g_list_append (nullptr, GINT_TO_POINTER (1));
    GList *single = head;
    gnc_g_list_cut (&single, head);
    EXPECT_EQ (nullptr, single);
    g_list_free (head);
}

TEST(GncGlibUtils, gnc_g_list_stringjoin)
{
    GList *test = nullptr;
    gchar *ret;

    EXPECT_EQ (nullptr, gnc_g_list_stringjoin (nullptr, nullptr));
    EXPECT_EQ (nullptr, gnc_g_list_stringjoin (nullptr, ":"));

    test = g_list_prepend (test, (gpointer)"one");

    ret = gnc_g_list_stringjoin (test, nullptr);
    EXPECT_STREQ ("one", ret);
    g_free (ret);

    ret = gnc_g_list_stringjoin (test, "");
    EXPECT_STREQ ("one", ret);
    g_free (ret);

    ret = gnc_g_list_stringjoin (test, ":");
    EXPECT_STREQ ("one", ret);
    g_free (ret);

    /* The following inserts a nullptr between "two" and "one". As a
       result, the stringjoin effectively skips a step, i.e. it does
       not insert separator repeatedly between NULL strings */
    test = g_list_prepend (test, nullptr);

    test = g_list_prepend (test, (gpointer)"two");

    ret = gnc_g_list_stringjoin (test, nullptr);
    EXPECT_STREQ ("twoone", ret);
    g_free (ret);

    ret = gnc_g_list_stringjoin (test, "");
    EXPECT_STREQ ("twoone", ret);
    g_free (ret);

    ret = gnc_g_list_stringjoin (test, ":");
    EXPECT_STREQ ("two:one", ret);
    g_free (ret);

    test = g_list_prepend (test, (gpointer)"three");

    ret = gnc_g_list_stringjoin (test, nullptr);
    EXPECT_STREQ ("threetwoone", ret);
    g_free (ret);

    ret = gnc_g_list_stringjoin (test, "");
    EXPECT_STREQ ("threetwoone", ret);
    g_free (ret);

    ret = gnc_g_list_stringjoin (test, ":");
    EXPECT_STREQ ("three:two:one", ret);
    g_free (ret);

    g_list_free (test);
}

TEST(GncGlibUtils, gnc_g_list_stringjoin_nodups)
{
    GList *test = nullptr;
    gchar *ret;

    test = g_list_prepend (test, (gpointer)"one");
    test = g_list_prepend (test, (gpointer)"two");
    test = g_list_prepend (test, (gpointer)"two");
    test = g_list_prepend (test, (gpointer)"three");
    test = g_list_prepend (test, (gpointer)"one:two");
    test = g_list_prepend (test, (gpointer)"four");
    test = g_list_reverse (test);
    ret = gnc_g_list_stringjoin_nodups (test, ":");
    EXPECT_STREQ ("one:two:three:four", ret);
    g_free (ret);
    g_list_free (test);
}

TEST(GncGlibUtils, gnc_list_length_cmp)
{
    GList *lst = nullptr;

    EXPECT_EQ (0, gnc_list_length_cmp (lst, 0));
    EXPECT_EQ (-1, gnc_list_length_cmp (lst, 1));

    lst = g_list_prepend (lst, GINT_TO_POINTER (1));
    EXPECT_EQ (1, gnc_list_length_cmp (lst, 0));
    EXPECT_EQ (0, gnc_list_length_cmp (lst, 1));
    EXPECT_EQ (-1, gnc_list_length_cmp (lst, 2));

    lst = g_list_prepend (lst, GINT_TO_POINTER (2));
    EXPECT_EQ (1, gnc_list_length_cmp (lst, 1));
    EXPECT_EQ (0, gnc_list_length_cmp (lst, 2));
    EXPECT_EQ (-1, gnc_list_length_cmp (lst, 3));

    g_list_free (lst);
}
