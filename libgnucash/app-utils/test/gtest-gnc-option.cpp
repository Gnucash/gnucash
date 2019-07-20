/********************************************************************
 * gtest-gnc-option.cpp -- unit tests for GncOption class.          *
 * Copyright (C) 2019 John Ralls <jralls@ceridwen.us>               *
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
 *******************************************************************/

#include <gtest/gtest.h>
#include <gnc-option.hpp>

TEST(GncOption, test_string_ctor)
{
    EXPECT_NO_THROW({
            auto option = gnc_make_string_option("foo", "bar", "baz",
                                                 "Phony Option",
                                                 std::string{"waldo"});
        });
}

TEST(GncOption, test_text_ctor)
{
    EXPECT_NO_THROW({
            auto option = gnc_make_text_option("foo", "bar", "baz",
                                               "Phony Option",
                                               std::string{"waldo"});
        });
}

TEST(GncOption, test_string_default_value)
{
    auto option = gnc_make_string_option("foo", "bar", "baz", "Phony Option",
                                         std::string{"waldo"});
    EXPECT_STREQ("waldo", option.get_default_value<std::string>().c_str());
    EXPECT_STREQ("waldo", option.get_value<std::string>().c_str());
}

TEST(GncOption, test_string_value)
{
    auto option = gnc_make_string_option("foo", "bar", "baz", "Phony Option",
                                         std::string{"waldo"});
    option.set_value(std::string{"pepper"});
    EXPECT_STREQ("waldo", option.get_default_value<std::string>().c_str());
    EXPECT_NO_THROW({
            EXPECT_STREQ("pepper", option.get_value<std::string>().c_str());
        });
}

TEST(GncOption, test_string_scm_functions)
{
    auto option = gnc_make_string_option("foo", "bar", "baz", "Phony Option",
                                         std::string{"waldo"});
    auto scm_value = option.get_scm_value();
    auto str_value = scm_to_utf8_string(scm_value);
    EXPECT_STREQ("waldo", str_value);
    g_free(str_value);
    scm_value = option.get_scm_default_value();
    str_value = scm_to_utf8_string(scm_value);
    EXPECT_STREQ("waldo", str_value);
    g_free(str_value);
}

TEST(GNCOption, test_budget_ctor)
{
    auto book = qof_book_new();
    auto budget = gnc_budget_new(book);
    EXPECT_NO_THROW({
            auto option = gnc_make_budget_option("foo", "bar", "baz",
                                                 "Phony Option", budget);
        });
    gnc_budget_destroy(budget);
    qof_book_destroy(book);
}

TEST(GNCOption, test_budget_scm_functions)
{
    auto book = qof_book_new();
    auto budget = gnc_budget_new(book);
    auto option = gnc_make_budget_option("foo", "bar", "baz",
                                         "Phony Option", budget);
    auto scm_budget = option.get_scm_value();
    auto str_value = scm_to_utf8_string(scm_budget);
    auto guid = guid_to_string(qof_instance_get_guid(budget));
    EXPECT_STREQ(guid, str_value);
    g_free(guid);
    gnc_budget_destroy(budget);
    qof_book_destroy(book);
}

TEST(GNCOption, test_commodity_ctor)
{
    auto book = qof_book_new();
    auto hpe = gnc_commodity_new(book, "Hewlett Packard Enterprise, Inc.",
                                    "NYSE", "HPE", NULL, 1);
    EXPECT_NO_THROW({
            auto option = gnc_make_commodity_option("foo", "bar", "baz",
                                                 "Phony Option", hpe);
        });
    gnc_commodity_destroy(hpe);
    qof_book_destroy(book);
}

TEST(GNCOption, test_currency_ctor)
{
    auto book = qof_book_new();
    auto table = gnc_commodity_table_new();
    qof_book_set_data(book, GNC_COMMODITY_TABLE, table);
    auto hpe = gnc_commodity_new(book, "Hewlett Packard Enterprise, Inc.",
                                    "NYSE", "HPE", NULL, 1);
    EXPECT_THROW({
            auto option = gnc_make_currency_option("foo", "bar", "baz",
                                                 "Phony Option", hpe);
        }, std::invalid_argument);
    gnc_commodity_destroy(hpe);
    auto eur = gnc_commodity_new(book, "Euro", "ISO4217", "EUR", NULL, 100);
    EXPECT_NO_THROW({
            auto option = gnc_make_currency_option("foo", "bar", "baz",
                                                 "Phony Option", eur);
        });
    gnc_commodity_destroy(eur);
    auto usd = gnc_commodity_new(book, "United States Dollar",
                                 "CURRENCY", "USD", NULL, 100);
    EXPECT_NO_THROW({
            auto option = gnc_make_currency_option("foo", "bar", "baz",
                                                 "Phony Option", usd);
        });
    gnc_commodity_destroy(usd);
    qof_book_set_data(book, GNC_COMMODITY_TABLE, nullptr);
    gnc_commodity_table_destroy(table);
    qof_book_destroy(book);
}

TEST(GNCOption, test_currency_setter)
{
    auto book = qof_book_new();
    auto table = gnc_commodity_table_new();
    qof_book_set_data(book, GNC_COMMODITY_TABLE, table);
    auto hpe = gnc_commodity_new(book, "Hewlett Packard Enterprise, Inc.",
                                    "NYSE", "HPE", NULL, 1);
    auto eur = gnc_commodity_new(book, "Euro", "ISO4217", "EUR", NULL, 100);
    auto option = gnc_make_currency_option("foo", "bar", "baz",
                                                 "Phony Option", eur);
    auto usd = gnc_commodity_new(book, "United States Dollar",
                                 "CURRENCY", "USD", NULL, 100);
    EXPECT_NO_THROW({
            option.set_value(QOF_INSTANCE(usd));
        });
    EXPECT_PRED2(gnc_commodity_equal, usd, GNC_COMMODITY(option.get_value<QofInstance*>()));
    EXPECT_THROW({
            option.set_value(QOF_INSTANCE(hpe));
        }, std::invalid_argument);
    EXPECT_PRED2(gnc_commodity_equal, usd, GNC_COMMODITY(option.get_value<QofInstance*>()));
    gnc_commodity_destroy(hpe);
    gnc_commodity_destroy(usd);
    gnc_commodity_destroy(eur);
    qof_book_set_data(book, GNC_COMMODITY_TABLE, nullptr);
    gnc_commodity_table_destroy(table);
    qof_book_destroy(book);
}
