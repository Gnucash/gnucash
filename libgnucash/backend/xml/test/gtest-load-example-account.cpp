/********************************************************************\
 * gtest-load-example-account.cpp -- Load the account hierarchy     *
 *                                   templates of every locale.     *
 *                                                                  *
 * Copyright (C) 2026 Isak Adamssvärd                               *
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
 * along with this program; if not, see                             *
 * <https://www.gnu.org/licenses/>                                  *
 *                                                                  *
\********************************************************************/
#include <glib.h>

#include <config.h>

#include <algorithm>
#include <filesystem>
#include <fstream>
#include <string>
#include <vector>

#include <gnc-engine.h>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcpp"
#include <gtest/gtest.h>
#pragma GCC diagnostic pop

#include "../io-example-account.h"

namespace fs = std::filesystem;

static const char* k_template_suffix = ".gnucash-xea";

/* Warnings and errors logged while a template is being read. */
static std::vector<std::string> s_log_messages;

static void
collect_log_message (const char* log_domain, GLogLevelFlags log_level,
                     const char* message, gpointer user_data)
{
    if (!(log_level & (G_LOG_LEVEL_WARNING | G_LOG_LEVEL_CRITICAL | G_LOG_LEVEL_ERROR)))
        return;

    std::string entry;
    if (log_domain)
        entry.append (log_domain).append (": ");
    if (message)
        entry.append (message);
    s_log_messages.push_back (entry);
}

static std::string
logged_messages ()
{
    std::string result;
    for (const auto& message : s_log_messages)
        result.append ("\n    ").append (message);
    return result;
}

/* The directories below the accounts directory, one for each locale. */
static std::vector<fs::path>
locale_directories (const fs::path& accounts_dir)
{
    std::vector<fs::path> result;
    for (const auto& entry : fs::directory_iterator (accounts_dir))
        if (entry.is_directory ())
            result.push_back (entry.path ());
    std::sort (result.begin (), result.end ());
    return result;
}

static std::vector<fs::path>
template_files (const fs::path& locale_dir)
{
    std::vector<fs::path> result;
    for (const auto& entry : fs::directory_iterator (locale_dir))
        if (entry.is_regular_file () && entry.path ().extension () == k_template_suffix)
            result.push_back (entry.path ());
    std::sort (result.begin (), result.end ());
    return result;
}

class ExampleAccountTest : public testing::Test
{
public:
    static void SetUpTestSuite ()
    {
        g_setenv ("GNC_UNINSTALLED", "1", TRUE);
        gnc_engine_init (0, nullptr);
        g_log_set_default_handler (collect_log_message, nullptr);
    }

protected:
    void SetUp () override
    {
        auto accounts_path = g_getenv ("GNC_ACCOUNT_PATH");
        ASSERT_NE (accounts_path, nullptr) << "GNC_ACCOUNT_PATH is not set.";
        m_accounts_dir = accounts_path;
        ASSERT_TRUE (fs::is_directory (m_accounts_dir)) << m_accounts_dir << " is not a directory.";
        m_locale_dirs = locale_directories (m_accounts_dir);
        ASSERT_FALSE (m_locale_dirs.empty ()) << "No locale directories in " << m_accounts_dir;
        s_log_messages.clear ();
    }

    fs::path m_accounts_dir;
    std::vector<fs::path> m_locale_dirs;
};

/* Every template must be readable without any complaint from the parser.
 * The parser logs a message and carries on when it can't make sense of a
 * part of an account, so the template still loads but isn't what its
 * author intended.
 */
TEST_F (ExampleAccountTest, read_every_template)
{
    for (const auto& locale_dir : m_locale_dirs)
    {
        auto files = template_files (locale_dir);
        EXPECT_FALSE (files.empty ()) << "No account templates in " << locale_dir;

        for (const auto& file : files)
        {
            SCOPED_TRACE (file.string ());
            s_log_messages.clear ();

            auto example = gnc_read_example_account (file.string ().c_str ());
            EXPECT_NE (example, nullptr);
            EXPECT_TRUE (s_log_messages.empty ()) << "Messages logged while reading:" << logged_messages ();
            if (!example)
                continue;

            EXPECT_TRUE (example->title && *example->title) << "The template has no title.";
            EXPECT_NE (example->root, nullptr) << "The template has no root account.";
            gnc_destroy_example_account (example);
        }
    }
}

/* The hierarchy assistant reads a whole locale directory at once. */
TEST_F (ExampleAccountTest, load_example_account_list)
{
    for (const auto& locale_dir : m_locale_dirs)
    {
        SCOPED_TRACE (locale_dir.string ());
        auto files = template_files (locale_dir);

        auto list = gnc_load_example_account_list (locale_dir.string ().c_str ());
        EXPECT_EQ (g_slist_length (list), files.size ());
        gnc_free_example_account_list (list);
    }
}

/* Bug 568659: a template that can't be read must not hide the other
 * templates in its directory.
 */
TEST_F (ExampleAccountTest, unreadable_template_is_skipped)
{
    auto tmp_name = g_dir_make_tmp ("gnc-example-accounts-XXXXXX", nullptr);
    ASSERT_NE (tmp_name, nullptr);
    fs::path tmp_dir {tmp_name};
    g_free (tmp_name);

    auto good_templates = template_files (m_accounts_dir / "C");
    ASSERT_FALSE (good_templates.empty ());
    fs::copy_file (good_templates.front (), tmp_dir / good_templates.front ().filename ());
    /* An empty file isn't a valid template. */
    std::ofstream {tmp_dir / "acctchrt_broken.gnucash-xea"};

    auto list = gnc_load_example_account_list (tmp_dir.string ().c_str ());
    EXPECT_EQ (g_slist_length (list), 1u);
    gnc_free_example_account_list (list);

    fs::remove_all (tmp_dir);
}

/* Write a template with the given title and descriptions and read it. */
static GncExampleAccount*
read_template_with_descriptions (const fs::path& dir, const std::string& title,
                                 const std::string& short_description,
                                 const std::string& long_description)
{
    auto file = dir / "acctchrt_test.gnucash-xea";
    {
        std::ofstream out {file};
        out << R"(<?xml version="1.0" encoding="utf-8"?>
<gnc-account-example
  xmlns="http://www.gnucash.org/XML/"
  xmlns:act="http://www.gnucash.org/XML/act"
  xmlns:gnc="http://www.gnucash.org/XML/gnc"
  xmlns:gnc-act="http://www.gnucash.org/XML/gnc-act">
<gnc-act:title>)" << title << R"(</gnc-act:title>
<gnc-act:short-description>)" << short_description << R"(</gnc-act:short-description>
<gnc-act:long-description>)" << long_description << R"(</gnc-act:long-description>
<gnc:account version="2.0.0">
  <act:name>Root Account</act:name>
  <act:id type="new">1972cce2e2364f95b2b0bc014502661d</act:id>
  <act:type>ROOT</act:type>
  <act:commodity-scu>0</act:commodity-scu>
</gnc:account>
</gnc-account-example>
)";
    }
    return gnc_read_example_account (file.string ().c_str ());
}

/* The title and the descriptions are stripped, and every run of whitespace
 * in them is reduced to its first character.
 */
TEST_F (ExampleAccountTest, whitespace_in_descriptions_is_squashed)
{
    auto tmp_name = g_dir_make_tmp ("gnc-example-accounts-XXXXXX", nullptr);
    ASSERT_NE (tmp_name, nullptr);
    fs::path tmp_dir {tmp_name};
    g_free (tmp_name);

    auto example = read_template_with_descriptions (
        tmp_dir,
        "Two  spaces,   three spaces,    four spaces",
        "\n  Surrounded by whitespace\n",
        "First line.\n    Second line.  Next sentence åäö.\n\n    New paragraph.");
    ASSERT_NE (example, nullptr);

    EXPECT_STREQ (example->title, "Two spaces, three spaces, four spaces");
    EXPECT_STREQ (example->short_description, "Surrounded by whitespace");
    EXPECT_STREQ (example->long_description,
                  "First line.\nSecond line. Next sentence åäö.\nNew paragraph.");

    gnc_destroy_example_account (example);
    fs::remove_all (tmp_dir);
}
