/********************************************************************
 * test-kvp-value.cpp: A Google Test suite for kvp-value impl.      *
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
#include "../guid.h"
#include "../kvp_frame.hpp"
#include "../gnc-date.h"
#include <memory>
#include <gtest/gtest.h>

TEST (KvpValueTest, Replace_Frame)
{
    auto f1 = new KvpFrameImpl;
    std::unique_ptr<KvpValueImpl> v1 {new KvpValueImpl {f1}};
    auto f2 = new KvpFrameImpl;
    EXPECT_EQ (f1, v1->replace_frame_nc (f2));
    v1->set (5.2);
    /*f1 and f2 should be deleted now*/
    f1 = new KvpFrameImpl;
    EXPECT_EQ (nullptr, v1->replace_frame_nc (f1));
    delete f1;
}

TEST (KvpValueTest, Equality)
{
    std::unique_ptr<KvpValueImpl> v1 {new KvpValueImpl { (int64_t)1}};
    std::unique_ptr<KvpValueImpl> v2 {new KvpValueImpl { (int64_t)2}};
    EXPECT_LT (compare (*v1, *v2), 0);
    // This is deleted in the kvpvalue destructor!
    auto guid = guid_new ();
    v1 = std::unique_ptr<KvpValueImpl> {new KvpValueImpl {guid}};
    //this guid is also deleted in kvp value destructor.
    v2 = std::unique_ptr<KvpValueImpl> {new KvpValueImpl {guid_copy (guid)}};
    EXPECT_EQ (compare (*v1, *v2), 0);

    v1 = std::unique_ptr<KvpValueImpl> {new KvpValueImpl {timespec_now ()}};
    v2 = std::unique_ptr<KvpValueImpl> {new KvpValueImpl {*v1}};
    EXPECT_EQ (compare (*v1, *v2), 0);
}

TEST (KvpValueTest, Add)
{
    auto v3 = new KvpValueImpl {timespec_now ()};
    auto v4 = new KvpValueImpl {*v3};
    auto new_one = v3->add (v4);
    EXPECT_NE (new_one, v3);
    EXPECT_EQ (new_one->get_type (), KvpValueType::KVP_TYPE_GLIST);
    EXPECT_NE (new_one->get<GList*> (), nullptr);
    /* also deletes v3 and v4 because they're "in" new_one */
    delete new_one;
}

TEST (KvpValueTest, Copy)
{
    std::unique_ptr<KvpValueImpl> v1 {new KvpValueImpl {5.2}};
    std::unique_ptr<KvpValueImpl> v2 {new KvpValueImpl {*v1}};
    EXPECT_EQ (compare (*v1, *v2), 0);

    auto guid = guid_new ();
    v1 = std::unique_ptr<KvpValueImpl> {new KvpValueImpl {guid}};
    v2 = std::unique_ptr<KvpValueImpl> {new KvpValueImpl {*v1}};
    EXPECT_EQ (compare (*v1, *v2), 0);

    v1 = std::unique_ptr<KvpValueImpl> {new KvpValueImpl {guid}};
    v2 = std::unique_ptr<KvpValueImpl> {new KvpValueImpl {*v1}};
    /*This should delete the guid*/
    v1->set(5.2);
    EXPECT_NE (compare (*v1, *v2), 0);
}

TEST (KvpValueTest, Stack)
{
    KvpValueImpl v1 {5.2};
    auto guid = guid_new ();
    v1 = KvpValueImpl {guid};

    EXPECT_NE (nullptr, guid);
}
