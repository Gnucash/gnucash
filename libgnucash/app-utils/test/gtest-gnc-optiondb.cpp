/********************************************************************
 * gtest-gnc-optiondb.cpp -- unit tests for GncOption class.        *
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
#include <gnc-optiondb.hpp>

TEST(GncOptionDB, test_ctor)
{
    EXPECT_NO_THROW ({ GncOptionDB optiondb; });
}

TEST(GncOptionDB, test_register_option)
{
    GncOptionDB optiondb;
    GncOption option1{"foo", "bar", "baz", "Phony Option",
                      std::string{"waldo"}};
    optiondb.register_option("foo", std::move(option1));
    EXPECT_EQ(optiondb.num_sections(), 1);
}

TEST(GncOptionDB, test_lookup_string_option)
{
    GncOptionDB optiondb;
    GncOption option1{"foo", "bar", "baz", "Phony Option",
                      std::string{"waldo"}};
    optiondb.register_option("foo", std::move(option1));
    EXPECT_STREQ("waldo", optiondb.lookup_string_option("foo", "bar").c_str());
}

TEST(GncOptionDB, test_unregister_option)
{
    GncOptionDB optiondb;
    GncOption option1{"foo", "bar", "baz", "Phony Option",
                      std::string{"waldo"}};
    optiondb.register_option("foo", std::move(option1));
    optiondb.unregister_option("foo", "bar");
    EXPECT_TRUE(optiondb.lookup_string_option("foo", "bar").empty());
}
