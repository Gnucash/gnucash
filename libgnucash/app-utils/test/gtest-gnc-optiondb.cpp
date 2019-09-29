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

class GncOptionDBTest : public ::testing::Test
{
protected:
    GncOptionDBTest() : m_db{gnc_option_db_new()} {}

    GncOptionDBPtr m_db;
};

TEST_F(GncOptionDBTest, test_ctor)
{
    EXPECT_NO_THROW ({ GncOptionDB optiondb; });
}

TEST_F(GncOptionDBTest, test_register_option)
{
    GncOption option1{"foo", "bar", "baz", "Phony Option",
                      std::string{"waldo"}};
    m_db->register_option("foo", std::move(option1));
    EXPECT_EQ(m_db->num_sections(), 1);
}

TEST_F(GncOptionDBTest, test_lookup_string_option)
{
    GncOption option1{"foo", "bar", "baz", "Phony Option",
                      std::string{"waldo"}};
    m_db->register_option("foo", std::move(option1));
    EXPECT_STREQ("waldo", m_db->lookup_string_option("foo", "bar").c_str());
}

TEST_F(GncOptionDBTest, test_unregister_option)
{
    GncOption option1{"foo", "bar", "baz", "Phony Option",
                      std::string{"waldo"}};
    m_db->register_option("foo", std::move(option1));
    m_db->unregister_option("foo", "bar");
    EXPECT_TRUE(m_db->lookup_string_option("foo", "bar").empty());
}

TEST_F(GncOptionDBTest, test_register_string_option)
{
    gnc_register_string_option(m_db, "foo", "bar", "baz", "Phony Option",
                               std::string{"waldo"});
    EXPECT_STREQ("waldo", m_db->lookup_string_option("foo", "bar").c_str());
}

class GncUIType
{
public:
    void set_value(const std::string& value) { m_value = value; }
    const std::string& get_value() const { return m_value; }
private:
    std::string m_value;
};

class GncOptionDBUITest : public ::testing::Test
{
protected:
    GncOptionDBUITest() : m_db{gnc_option_db_new()}
    {
        gnc_register_string_option(m_db, "foo", "bar", "baz", "Phony Option",
                                   std::string{"waldo"});
        gnc_register_text_option(m_db, "foo", "sausage", "links",
                                 "Phony Option", std::string{"waldo"});
        gnc_register_string_option(m_db, "qux", "grault", "baz", "Phony Option",
                                   std::string{""});
        gnc_register_text_option(m_db, "qux", "garply", "fred",
                                   "Phony Option", std::string{"waldo"});
    }

    GncOptionDBPtr m_db;
};

TEST_F(GncOptionDBUITest, test_set_ui_item)
{
    GncUIType entry;
    m_db->set_ui_item("foo", "bar", &entry);
    EXPECT_EQ(&entry, static_cast<GncUIType*>(m_db->get_ui_item("foo", "bar")));
}

TEST_F(GncOptionDBUITest, test_ui_value_from_option)
{
    GncUIType entry;
    const char* value{"waldo"};
    m_db->set_ui_item("foo", "bar", &entry);
    m_db->set_ui_from_option("foo", "bar", [](GncOption& option){
            auto ui_item = static_cast<GncUIType* const>(option.get_ui_item());
            ui_item->set_value(option.get_value<std::string>());
        });
    EXPECT_STREQ(value, entry.get_value().c_str());
}

TEST_F(GncOptionDBUITest, test_option_value_from_ui)
{
    GncUIType entry;
    const char* value{"pepper"};
    m_db->set_ui_item("foo", "bar", &entry);
    entry.set_value(value);
    m_db->set_option_from_ui("foo", "bar", [](GncOption& option){
            auto ui_item = static_cast<GncUIType* const>(option.get_ui_item());
            option.set_value(ui_item->get_value());
        });
    EXPECT_STREQ(value, m_db->lookup_string_option("foo", "bar").c_str());
}
