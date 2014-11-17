/********************************************************************
 * test-kvp-frame.cpp: A Google Test suite for kvp-frame impl.      *
 * Copyright 2014 Aaron Laws <dartme18@gmail.com>                   *
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
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html            *
 * or contact:                                                      *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 ********************************************************************/

#include "../kvp-value.hpp"
#include "../kvp_frame.hpp"
#include <gtest/gtest.h>
#include <algorithm>

template <typename A, typename B> void
assert_contains (std::vector<A> vec, B const & b)
{
    auto val = std::find (vec.begin (), vec.end (), b);
    EXPECT_NE (val, vec.end ());
}

TEST (KvpFrameTest, Replace)
{
    auto f1 = new KvpFrameImpl;
    auto v1 = new KvpValueImpl {15.0};
    auto v2 = new KvpValueImpl { (int64_t)52};
    std::string k1 {"first key"};

    EXPECT_EQ (nullptr, f1->replace_nc (k1.c_str (), v1));
    EXPECT_EQ (v1, f1->replace_nc (k1.c_str (), v2));

    delete f1; //this should also delete v2.
    delete v1;
}

TEST (KvpFrameTest, GetKeys)
{
    auto k1 = "first key";
    auto k2 = "second key";
    auto k3 = "first key/third key";
    auto f1 = new KvpFrameImpl;
    auto v1 = new KvpValueImpl {15.2};
    auto v2 = new KvpValueImpl { (int64_t)12};
    auto v3 = new KvpValueImpl {strdup ("Never again")};

    f1->replace_nc (k1, v1);
    f1->replace_nc (k2, v2);
    f1->replace_nc (k3, v3);

    auto keys = f1->get_keys ();
    EXPECT_EQ (keys.size (), 3);

    assert_contains (keys, k1);
    assert_contains (keys, k2);
    assert_contains (keys, k3);

    delete f1;//This should delete our KvpValueImpls, and our string above, too.
}

TEST (KvpFrameTest, GetSlot)
{
    auto f1 = new KvpFrameImpl;
    auto v1 = new KvpValueImpl {2.2};
    auto v2 = new KvpValueImpl { (int64_t)4};
    auto k1 = "first key";
    auto k2 = "first key/second key";

    f1->replace_nc (k1, v1);
    EXPECT_EQ (v1, f1->get_slot(k1));
    f1->replace_nc (k2, v2);
    EXPECT_EQ (v2, f1->get_slot(k2));
    EXPECT_EQ (v1, f1->get_slot(k1));

    delete f1;//which will delete both kvpvalues, too.
}
