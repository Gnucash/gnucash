/********************************************************************
 * test-tree-container.cpp:                                         *
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
 * https://www.gnu.org/licenses/old-licenses/gpl-2.0.html            *
 * or contact:                                                      *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 ********************************************************************/

#include "config.h"
#include <glib.h>
#include <gtk/gtk.h>
#include "../gnc-list-model-container.hpp"
#include <gtest/gtest.h>
#include <string>

enum {
  COLUMN_STRING,
  COLUMN_INT,
  COLUMN_BOOLEAN,
  N_COLUMNS
};


TEST(GncListModelContainer, Equality)
{
    auto store1 = gtk_list_store_new (N_COLUMNS, G_TYPE_STRING, G_TYPE_INT, G_TYPE_BOOLEAN);
    auto store2 = gtk_list_store_new (N_COLUMNS, G_TYPE_STRING, G_TYPE_INT, G_TYPE_BOOLEAN);

    GncListModelContainer container1{GTK_TREE_MODEL(store1)};
    GncListModelContainer container2{GTK_TREE_MODEL(store2)};

    // these are null tests
    EXPECT_TRUE (container1.begin() == container1.begin());
    EXPECT_TRUE (container1.end() == container1.end());

    EXPECT_TRUE (container1.begin() == container1.end());
    EXPECT_TRUE (container1.size() == 0);
    EXPECT_TRUE (container2.size() == 0);
    EXPECT_TRUE (container1.empty());
    EXPECT_TRUE (container1.empty());

    EXPECT_FALSE (container1.begin() == container2.begin());
    EXPECT_FALSE (container1.end() == container2.end());

    // both containers have identical contents
    container1.append()->set_column (COLUMN_STRING, "1");
    container2.append()->set_column (COLUMN_STRING, "1");

    // the containers are now no longer empty
    EXPECT_FALSE (container1.begin() == container1.end());
    EXPECT_FALSE (container2.begin() == container2.end());
    EXPECT_TRUE (container1.size() == 1);
    EXPECT_TRUE (container2.size() == 1);

    // however the iterators behave as expected -- iterators from
    // store1 must differ from iterators from store2
    EXPECT_FALSE (container1.begin() == container2.begin());
    EXPECT_FALSE (container1.end() == container2.end());

    g_object_unref (store1);
    g_object_unref (store2);
}

TEST(GncListModelContainer, Basic)
{
    auto store = gtk_list_store_new (N_COLUMNS, G_TYPE_STRING, G_TYPE_INT, G_TYPE_BOOLEAN);
    GncListModelContainer container{GTK_TREE_MODEL(store)};

    // test empty
    EXPECT_TRUE (container.empty());

    for (size_t i = 0; i < 10; i++)
    {
        auto str = std::string("string ") + std::to_string(i);
        auto iter = container.append ();
        iter->set_columns (0,
                           COLUMN_STRING, str.c_str(),
                           COLUMN_INT, i,
                           COLUMN_BOOLEAN, (i % 2) == 0,
                           -1);
    }

    // test non-empty
    EXPECT_FALSE (container.empty());

    // test size
    EXPECT_TRUE (10 == container.size());

    auto int_is_five = [](auto it){ return it.get_column_int(COLUMN_INT) == 5; };
    auto iter_found = std::find_if (container.begin(), container.end(), int_is_five);
    EXPECT_TRUE (iter_found.has_value());
    EXPECT_EQ ("string 5", iter_found->get_column_string (COLUMN_STRING));

    g_object_unref (store);
}

int main(int argc, char** argv)
{
    if (gtk_init_check (nullptr, nullptr))
        std::cout << "gtk init completed!" << std::endl;
    else
        std::cout << "no display present!" << std::endl;

    // Initialize the Google Test framework
    ::testing::InitGoogleTest(&argc, argv);

    // Run tests
    return RUN_ALL_TESTS();
}
