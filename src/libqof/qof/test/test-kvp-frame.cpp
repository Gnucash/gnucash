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

class KvpFrameTest : public ::testing::Test
{
public:
    KvpFrameTest() :
        t_int_val{new KvpValue {INT64_C(15)}},
        t_str_val{new KvpValue{"a value"}}     {
        auto f1 = new KvpFrame;
        t_root.set("top", new KvpValue{f1});
        f1->set("first", t_int_val);
        f1->set("second", new KvpValue{new KvpFrame});
        f1->set("third", t_str_val);
    }
protected:
    KvpFrameImpl t_root;
    KvpValue *t_int_val;
    KvpValue *t_str_val;
};

template <typename A, typename B> void
assert_contains (std::vector<A> vec, B const & b)
{
    auto val = std::find (vec.begin (), vec.end (), b);
    EXPECT_NE (val, vec.end ());
}

TEST_F (KvpFrameTest, SetLocal)
{
    auto f1 = new KvpFrameImpl;
    auto v1 = new KvpValueImpl {15.0};
    auto v2 = new KvpValueImpl { (int64_t)52};
    std::string k1 {"first key"};

    EXPECT_EQ (nullptr, f1->set (k1.c_str (), v1));
    EXPECT_EQ (v1, f1->set (k1.c_str (), v2));

    delete f1; //this should also delete v2.
    delete v1;
}

TEST_F (KvpFrameTest, SetPath)
{
    Path path1 {"top", "second", "twenty-first"};
    Path path2 {"top", "third", "thirty-first"};
    auto v1 = new KvpValueImpl {15.0};
    auto v2 = new KvpValueImpl { (int64_t)52};

    EXPECT_EQ (nullptr, t_root.set(path1, v1));
    EXPECT_EQ (v1, t_root.set(path1, v2));
    EXPECT_EQ (nullptr, t_root.set(path2, v1));
    EXPECT_EQ (v2, t_root.get_slot(path1));
    EXPECT_EQ (nullptr, t_root.get_slot(path2));
    delete v1;
}

TEST_F (KvpFrameTest, SetPathWithCreate)
{
    Path path1 {"top", "second", "twenty-first"};
    Path path2 {"top", "third", "thirty-first"};
    auto v1 = new KvpValueImpl {15.0};
    auto v2 = new KvpValueImpl { (int64_t)52};

    EXPECT_EQ (nullptr, t_root.set_path(path1, v1));
    EXPECT_EQ (v1, t_root.set_path(path1, v2));
    EXPECT_EQ (nullptr, t_root.set_path(path2, v1));
    EXPECT_EQ (v2, t_root.get_slot(path1));
    EXPECT_EQ (v1, t_root.get_slot(path2));
}

TEST_F (KvpFrameTest, GetKeys)
{
    auto k1 = "top";
    auto k2 = "first";
    auto k3 = "second";

    auto keys = t_root.get_keys ();
    EXPECT_EQ (keys.size (), 1);

    assert_contains (keys, k1);
    auto frameval = t_root.get_slot(k1);
    ASSERT_EQ(frameval->get_type(), KVP_TYPE_FRAME);
    keys = frameval->get<KvpFrame*>()->get_keys();
    assert_contains (keys, k2);
    assert_contains (keys, k3);
}

TEST_F (KvpFrameTest, GetLocalSlot)
{
    auto k1 = "first";
    auto k2 = "third";
    auto k3 = "doesn't exist";

    auto frameval = t_root.get_slot("top");
    ASSERT_EQ(frameval->get_type(), KVP_TYPE_FRAME);
    auto f1 = frameval->get<KvpFrame*>();
    EXPECT_EQ (t_int_val, f1->get_slot(k1));
    EXPECT_EQ (t_str_val, f1->get_slot(k2));
    EXPECT_EQ (nullptr, f1->get_slot(k3));
}

TEST_F (KvpFrameTest, GetSlotPath)
{
    Path path1 {"top", "second", "twenty-first"};
    Path path2 {"top", "third", "thirty-first"};
    auto v1 = new KvpValueImpl {15.0};
    auto v2 = new KvpValueImpl { (int64_t)52};

    EXPECT_EQ (nullptr, t_root.set(path1, v1));
    EXPECT_EQ (nullptr, t_root.set(path2, v2));
    EXPECT_EQ (v1, t_root.get_slot(path1));
    EXPECT_EQ (nullptr, t_root.get_slot(path2));
    delete v2;
}

TEST_F (KvpFrameTest, Empty)
{
    KvpFrameImpl f1, f2;
    f2.set("value", new KvpValue {2.2});
    EXPECT_TRUE(f1.empty());
    EXPECT_FALSE(f2.empty());
}
