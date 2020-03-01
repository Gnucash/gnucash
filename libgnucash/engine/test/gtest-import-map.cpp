/********************************************************************
 * gtest-import-map.cpp: Test import match maps.                    *
 * Copyright 2015 John Ralls <jralls@ceridwen.us>		    *
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

extern "C"
{
#include <config.h>
#include "../Account.h"
#include <qof.h>
}

#include <qofinstance-p.h>
#include <kvp-frame.hpp>
#include <gtest/gtest.h>
#include <string>

class ImapTest : public testing::Test
{
protected:
    void SetUp() {
        QofBook *book = qof_book_new();
        Account *root = gnc_account_create_root(book);

        t_asset_account1 = xaccMallocAccount(book);
        xaccAccountSetName(t_asset_account1, "Asset");
        gnc_account_append_child(root, t_asset_account1);

        t_bank_account = xaccMallocAccount(book);
        xaccAccountSetName(t_bank_account, "Bank");
        gnc_account_append_child(t_asset_account1, t_bank_account);

        t_asset_account2 = xaccMallocAccount(book);
        xaccAccountSetName(t_asset_account2, "Asset-Bank");
        gnc_account_append_child(root, t_asset_account2);

        t_sav_account = xaccMallocAccount(book);
        xaccAccountSetName(t_sav_account, "Bank");
        gnc_account_append_child(t_asset_account2, t_sav_account);

        t_expense_account = xaccMallocAccount(book);
        xaccAccountSetName(t_expense_account, "Expense");
        gnc_account_append_child(root, t_expense_account);

        t_expense_account1 = xaccMallocAccount(book);
        xaccAccountSetName(t_expense_account1, "Food");
        gnc_account_append_child(t_expense_account, t_expense_account1);

        t_expense_account2 = xaccMallocAccount(book);
        xaccAccountSetName(t_expense_account2, "Drink");
        gnc_account_append_child(t_expense_account, t_expense_account2);
    }
    void TearDown() {
        auto root = gnc_account_get_root (t_bank_account);
        auto book = gnc_account_get_book (root);
        xaccAccountBeginEdit (root);
        xaccAccountDestroy (root);
        qof_book_destroy (book);
    }
    Account *t_bank_account {};
    Account *t_sav_account {};
    Account *t_expense_account1 {};
    Account *t_expense_account2 {};

    Account *t_asset_account1 {};
    Account *t_asset_account2 {};
    Account *t_expense_account {};
};

TEST_F(ImapTest, CreateImap) {
    GncImportMatchMap *imap = gnc_account_imap_create_imap (t_bank_account);
    EXPECT_NE(nullptr, imap);
    EXPECT_EQ(t_bank_account, imap->acc);
    EXPECT_EQ(gnc_account_get_book(t_bank_account), imap->book);

    g_free(imap);
}

static const char* IMAP_FRAME = "import-map";
static const char* IMAP_FRAME_BAYES = "import-map-bayes";

class ImapPlainTest : public ImapTest
{
protected:
    void SetUp() {
        ImapTest::SetUp();
        t_imap = gnc_account_imap_create_imap (t_bank_account);
    }

    void TearDown() {
        g_free(t_imap);
        ImapTest::TearDown();
    }

    GncImportMatchMap *t_imap {};
};

TEST_F(ImapPlainTest, FindAccount)
{
    auto root = qof_instance_get_slots(QOF_INSTANCE(t_bank_account));
    auto acc1_val = new KvpValue(guid_copy(xaccAccountGetGUID(t_expense_account1)));
    auto acc2_val = new KvpValue(guid_copy(xaccAccountGetGUID(t_expense_account2)));
    root->set_path({IMAP_FRAME, "foo", "bar"}, acc1_val);
    root->set_path({IMAP_FRAME, "baz", "waldo"}, acc2_val);
    root->set_path({IMAP_FRAME, "pepper"}, new KvpValue{*acc1_val});
    root->set_path({IMAP_FRAME, "salt"}, new KvpValue{*acc2_val});

    EXPECT_EQ(t_expense_account1, gnc_account_imap_find_account(t_imap, "foo", "bar"));
    EXPECT_EQ(t_expense_account2, gnc_account_imap_find_account(t_imap, "baz", "waldo"));
    EXPECT_EQ(t_expense_account1, gnc_account_imap_find_account(t_imap, NULL, "pepper"));
    EXPECT_EQ(t_expense_account2, gnc_account_imap_find_account(t_imap, NULL, "salt"));
    EXPECT_EQ(nullptr, gnc_account_imap_find_account(t_imap, "salt", NULL));
}

TEST_F(ImapPlainTest, AddAccount)
{
// prevent the embedded beginedit/committedit from doing anything
    qof_instance_increase_editlevel(QOF_INSTANCE(t_bank_account));
    qof_instance_mark_clean(QOF_INSTANCE(t_bank_account));
    gnc_account_imap_add_account(t_imap, "foo", "bar", t_expense_account1);
    gnc_account_imap_add_account(t_imap, "baz", "waldo", t_expense_account2);
    gnc_account_imap_add_account(t_imap, NULL, "pepper", t_expense_account1);
    gnc_account_imap_add_account(t_imap, NULL, "salt", t_expense_account2);
    EXPECT_EQ(1, qof_instance_get_editlevel(QOF_INSTANCE(t_bank_account)));
    EXPECT_TRUE(qof_instance_get_dirty_flag(QOF_INSTANCE(t_bank_account)));
    qof_instance_mark_clean(QOF_INSTANCE(t_bank_account));
    gnc_account_imap_add_account(t_imap, NULL, NULL, t_expense_account2);
    EXPECT_FALSE(qof_instance_get_dirty_flag(QOF_INSTANCE(t_bank_account)));
    gnc_account_imap_add_account(t_imap, "pork", "sausage", NULL);
    EXPECT_FALSE(qof_instance_get_dirty_flag(QOF_INSTANCE(t_bank_account)));
    qof_instance_reset_editlevel(QOF_INSTANCE(t_bank_account));

    auto root = qof_instance_get_slots(QOF_INSTANCE(t_bank_account));
    auto value = root->get_slot({IMAP_FRAME, "foo", "bar"});
    auto check_account = [this](KvpValue* v) {
        return xaccAccountLookup(v->get<GncGUID*>(), this->t_imap->book); };
    EXPECT_EQ(t_expense_account1, check_account(value));
    value = root->get_slot({IMAP_FRAME, "baz", "waldo"});
    EXPECT_EQ(t_expense_account2, check_account(value));
    value = root->get_slot({IMAP_FRAME, "pepper"});
    EXPECT_EQ(t_expense_account1, check_account(value));
    value = root->get_slot({IMAP_FRAME, "salt"});
    EXPECT_EQ(t_expense_account2, check_account(value));
    value = root->get_slot({IMAP_FRAME, "pork", "sausage"});
    EXPECT_EQ(nullptr, value);
}

TEST_F(ImapPlainTest, DeleteAccount)
{
    Path path1 {IMAP_FRAME, "foo", "waldo"};
    Path path2 {IMAP_FRAME, "foo"};
    Path path3 {IMAP_FRAME};

// prevent the embedded beginedit/committedit from doing anything
    qof_instance_increase_editlevel(QOF_INSTANCE(t_bank_account));
    qof_instance_mark_clean(QOF_INSTANCE(t_bank_account));
    gnc_account_imap_add_account(t_imap, "foo", "bar", t_expense_account1);
    gnc_account_imap_add_account(t_imap, "foo", "waldo", t_expense_account2);
    gnc_account_imap_add_account(t_imap, NULL, "pepper", t_expense_account1);
    EXPECT_EQ(1, qof_instance_get_editlevel(QOF_INSTANCE(t_bank_account)));
    EXPECT_TRUE(qof_instance_get_dirty_flag(QOF_INSTANCE(t_bank_account)));
    qof_instance_mark_clean(QOF_INSTANCE(t_bank_account));

    gnc_account_imap_delete_account(t_imap, NULL, NULL);
    EXPECT_FALSE(qof_instance_get_dirty_flag(QOF_INSTANCE(t_bank_account)));

    gnc_account_imap_delete_account(t_imap, "foo", "waldo");
    EXPECT_TRUE(qof_instance_get_dirty_flag(QOF_INSTANCE(t_bank_account)));
    qof_instance_mark_clean(QOF_INSTANCE(t_bank_account));
    EXPECT_EQ(t_expense_account1, gnc_account_imap_find_account(t_imap, "foo", "bar"));
    EXPECT_EQ(nullptr, gnc_account_imap_find_account(t_imap, "foo", "waldo"));
    auto root = qof_instance_get_slots(QOF_INSTANCE(t_bank_account));
    EXPECT_EQ(nullptr, root->get_slot(path1));

    gnc_account_imap_delete_account(t_imap, "foo", "bar");
    EXPECT_TRUE(qof_instance_get_dirty_flag(QOF_INSTANCE(t_bank_account)));
    qof_instance_mark_clean(QOF_INSTANCE(t_bank_account));
    EXPECT_EQ(nullptr, root->get_slot(path2));

    gnc_account_imap_delete_account(t_imap, NULL, "pepper");
    EXPECT_TRUE(qof_instance_get_dirty_flag(QOF_INSTANCE(t_bank_account)));
    qof_instance_mark_clean(QOF_INSTANCE(t_bank_account));
    EXPECT_EQ(nullptr, root->get_slot(path3));
    qof_instance_reset_editlevel(QOF_INSTANCE(t_bank_account));
}

static const char* foo = "foo";
static const char* bar = "bar";
static const char* baz = "baz";
static const char* waldo = "waldo";
static const char* pepper = "pepper";
static const char* salt = "salt";
static const char* pork = "pork";
static const char* sausage = "sausage";



class ImapBayesTest : public ImapPlainTest
{
protected:
    void SetUp() {
        ImapPlainTest::SetUp();
        t_list1 = g_list_prepend(t_list1, const_cast<char*>(foo));
        t_list1 = g_list_prepend(t_list1, const_cast<char*>(bar));
        t_list2 = g_list_prepend(t_list2, const_cast<char*>(baz));
        t_list2 = g_list_prepend(t_list2, const_cast<char*>(waldo));
        t_list3 = g_list_prepend(t_list3, const_cast<char*>(pepper));
        t_list4 = g_list_prepend(t_list4, const_cast<char*>(salt));
        t_list5 = g_list_prepend(t_list5, const_cast<char*>(pork));
        t_list5 = g_list_prepend(t_list5, const_cast<char*>(sausage));
    }
    void TearDown() {
        g_list_free(t_list1);
        g_list_free(t_list2);
        g_list_free(t_list3);
        g_list_free(t_list4);
        g_list_free(t_list5);
        t_list1 = nullptr;
        t_list2 = nullptr;
        t_list3 = nullptr;
        t_list4 = nullptr;
        t_list5 = nullptr;
        ImapPlainTest::TearDown();
    }

    GList *t_list1 {};
    GList *t_list2 {};
    GList *t_list3 {};
    GList *t_list4 {};
    GList *t_list5 {};
};

TEST_F(ImapBayesTest, FindAccountBayes)
{
    auto root = qof_instance_get_slots(QOF_INSTANCE(t_bank_account));
    auto acct1_guid = guid_to_string (xaccAccountGetGUID(t_expense_account1));
    auto acct2_guid = guid_to_string (xaccAccountGetGUID(t_expense_account2));
    auto value = new KvpValue(INT64_C(42));

    root->set_path({std::string{IMAP_FRAME_BAYES} + "/" + foo + "/" + acct1_guid}, value);
    root->set_path({std::string{IMAP_FRAME_BAYES} + "/" + bar + "/" + acct1_guid}, new KvpValue{*value});
    root->set_path({std::string{IMAP_FRAME_BAYES} + "/" + baz + "/" + acct2_guid}, new KvpValue{*value});
    root->set_path({std::string{IMAP_FRAME_BAYES} + "/" + waldo + "/" + acct2_guid}, new KvpValue{*value});
    root->set_path({std::string{IMAP_FRAME_BAYES} + "/" + pepper + "/" + acct1_guid}, new KvpValue{*value});
    root->set_path({std::string{IMAP_FRAME_BAYES} + "/" + salt + "/" + acct2_guid}, new KvpValue{*value});

    auto account = gnc_account_imap_find_account_bayes(t_imap, t_list1);
    EXPECT_EQ(t_expense_account1, account);
    account = gnc_account_imap_find_account_bayes(t_imap, t_list2);
    EXPECT_EQ(t_expense_account2, account);
    account = gnc_account_imap_find_account_bayes(t_imap, t_list3);
    EXPECT_EQ(t_expense_account1, account);
    account = gnc_account_imap_find_account_bayes(t_imap, t_list4);
    EXPECT_EQ(t_expense_account2, account);
    account = gnc_account_imap_find_account_bayes(t_imap, t_list5);
    EXPECT_EQ(nullptr, account);

    // only imap entries with exact token matching should be considered
    root->set_path({std::string{IMAP_FRAME_BAYES} + "/" + pepper + waldo + "/" + acct2_guid}, new KvpValue{*value});
    account = gnc_account_imap_find_account_bayes(t_imap, t_list3);
    EXPECT_EQ(t_expense_account1, account);
    root->set_path({std::string{IMAP_FRAME_BAYES} + "/" + pepper + "/" + waldo + "/" + acct2_guid}, new KvpValue{*value});
    account = gnc_account_imap_find_account_bayes(t_imap, t_list3);
    EXPECT_EQ(t_expense_account1, account);
}

TEST_F(ImapBayesTest, AddAccountBayes)
{
    // prevent the embedded beginedit/committedit from doing anything
    qof_instance_increase_editlevel(QOF_INSTANCE(t_bank_account));
    qof_instance_mark_clean(QOF_INSTANCE(t_bank_account));
    gnc_account_imap_add_account_bayes(t_imap, t_list1, t_expense_account1);
    gnc_account_imap_add_account_bayes(t_imap, t_list2, t_expense_account2);
    gnc_account_imap_add_account_bayes(t_imap, t_list3, t_expense_account1);
    gnc_account_imap_add_account_bayes(t_imap, t_list4, t_expense_account2);
    EXPECT_EQ(1, qof_instance_get_editlevel(QOF_INSTANCE(t_bank_account)));
    EXPECT_TRUE(qof_instance_get_dirty_flag(QOF_INSTANCE(t_bank_account)));
    qof_instance_mark_clean(QOF_INSTANCE(t_bank_account));
    gnc_account_imap_add_account_bayes(t_imap, t_list5, NULL);
    EXPECT_FALSE(qof_instance_get_dirty_flag(QOF_INSTANCE(t_bank_account)));
    qof_instance_reset_editlevel(QOF_INSTANCE(t_bank_account));

    auto root = qof_instance_get_slots(QOF_INSTANCE(t_bank_account));
    auto acct1_guid = guid_to_string (xaccAccountGetGUID(t_expense_account1));
    auto acct2_guid = guid_to_string (xaccAccountGetGUID(t_expense_account2));
    auto value = root->get_slot({std::string{IMAP_FRAME_BAYES} + "/foo/bar"});
    auto check_account = [this](KvpValue* v) {
        return (v->get<const char*>(), this->t_imap->book); };
    value = root->get_slot({std::string{IMAP_FRAME_BAYES} + "/" + foo + "/" + acct1_guid});
    EXPECT_EQ(1, value->get<int64_t>());
    value = root->get_slot({std::string{IMAP_FRAME_BAYES} + "/" + bar + "/" + acct1_guid});
    EXPECT_EQ(1, value->get<int64_t>());
    value = root->get_slot({std::string{IMAP_FRAME_BAYES} + "/" + baz + "/" + acct2_guid});
    EXPECT_EQ(1, value->get<int64_t>());
    value = root->get_slot({std::string{IMAP_FRAME_BAYES} + "/" + waldo + "/" + acct2_guid});
    EXPECT_EQ(1, value->get<int64_t>());
    value = root->get_slot({std::string{IMAP_FRAME_BAYES} + "/" + pepper + "/" + acct1_guid});
    EXPECT_EQ(1, value->get<int64_t>());
    value = root->get_slot({std::string{IMAP_FRAME_BAYES} + "/" + salt + "/" + acct2_guid});
    EXPECT_EQ(1, value->get<int64_t>());
    value = root->get_slot({std::string{IMAP_FRAME_BAYES} + "/" + baz + "/" + acct1_guid});
    EXPECT_EQ(nullptr, value);

    qof_instance_increase_editlevel(QOF_INSTANCE(t_bank_account));
    gnc_account_imap_add_account_bayes(t_imap, t_list2, t_expense_account2);
    qof_instance_mark_clean(QOF_INSTANCE(t_bank_account));
    qof_instance_reset_editlevel(QOF_INSTANCE(t_bank_account));
    value = root->get_slot({std::string{IMAP_FRAME_BAYES} + "/" + baz + "/" + acct2_guid});
    EXPECT_EQ(2, value->get<int64_t>());
}

TEST_F(ImapBayesTest, ConvertBayesData)
{
    auto root = qof_instance_get_slots(QOF_INSTANCE(t_bank_account));
    auto book = qof_instance_get_slots(QOF_INSTANCE(t_imap->book));
    auto acct1_guid = guid_to_string (xaccAccountGetGUID(t_expense_account1)); //Food
    auto acct2_guid = guid_to_string (xaccAccountGetGUID(t_expense_account2)); //Drink
    auto acct3_guid = guid_to_string (xaccAccountGetGUID(t_asset_account2)); //Asset-Bank
    auto acct4_guid = guid_to_string (xaccAccountGetGUID(t_sav_account)); //Sav Bank
    auto val1 = new KvpValue(static_cast<int64_t>(10));
    auto val2 = new KvpValue(static_cast<int64_t>(5));
    auto val3 = new KvpValue(static_cast<int64_t>(2));
    // Set up some old entries
    root->set_path({IMAP_FRAME_BAYES, "severely", "divided", "token", "Asset-Bank"}, val1);
    root->set_path({IMAP_FRAME_BAYES, salt, "Asset-Bank#Bank"}, new KvpValue{*val1});
    root->set_path({IMAP_FRAME_BAYES, salt, "Asset>Bank#Bank"}, val2);
    root->set_path({IMAP_FRAME_BAYES, pork, "Expense#Food"}, new KvpValue{*val2});
    root->set_path({IMAP_FRAME_BAYES, sausage, "Expense#Drink"}, val3);
    root->set_path({IMAP_FRAME_BAYES, foo, "Expense#Food"}, new KvpValue{*val2});
    root->set_path({IMAP_FRAME_BAYES, salt, acct1_guid}, new KvpValue{*val1});
    /*Calling into the imap functions should trigger a conversion.*/
    gnc_account_imap_add_account_bayes(t_imap, t_list5, t_expense_account2); //pork and sausage; account Food
    // convert from 'Asset-Bank' to 'Asset-Bank' guid
    auto value = root->get_slot({std::string{IMAP_FRAME_BAYES} + "/severely/divided/token/" + acct3_guid});
    EXPECT_EQ(10, value->get<int64_t>());
    // convert from 'Asset-Bank#Bank' to 'Sav Bank' guid
    value = root->get_slot({std::string{IMAP_FRAME_BAYES} + "/" + salt + "/" + acct4_guid});
    EXPECT_EQ(10, value->get<int64_t>());
    // convert from 'Expense#Food' to 'Food' guid
    value = root->get_slot({std::string{IMAP_FRAME_BAYES} + "/" + pork + "/" + acct1_guid});
    EXPECT_EQ(5, value->get<int64_t>());
    // convert from 'Expense#Drink' to 'Drink' guid
    value = root->get_slot({std::string{IMAP_FRAME_BAYES} + "/" + sausage + "/" + acct2_guid});
    /*We put in 2, then called it once to bring it up to 3.*/
    EXPECT_EQ(3, value->get<int64_t>());
    // convert from 'Expense#Food' to 'Food' guid but add to original value
    value = root->get_slot({std::string{IMAP_FRAME_BAYES} + "/" + foo + "/" + acct1_guid});
    EXPECT_EQ(5, value->get<int64_t>());
    // Keep GUID value from original
    value = root->get_slot({std::string{IMAP_FRAME_BAYES} + "/" + salt + "/" + acct1_guid});
    EXPECT_EQ(10, value->get<int64_t>());
    EXPECT_TRUE(qof_instance_get_dirty_flag(QOF_INSTANCE(t_bank_account)));
}

/* Tests the import map's handling of KVP delimiters */
TEST_F (ImapBayesTest, import_map_with_delimiters)
{
    GList * tokens {nullptr};
    tokens = g_list_prepend (tokens, const_cast<char*> ("one/two/three"));
    gnc_account_imap_add_account_bayes (t_imap, tokens, t_expense_account1);
    gnc_account_imap_add_account_bayes (t_imap, tokens, t_expense_account1);
    gnc_account_imap_add_account_bayes (t_imap, tokens, t_expense_account1);
    auto account = gnc_account_imap_find_account_bayes (t_imap, tokens);
    EXPECT_EQ (account, t_expense_account1);
}

TEST_F (ImapBayesTest, get_bayes_info)
{
    GList * tokens {nullptr};
    tokens = g_list_prepend (tokens, const_cast <char*> ("one/two/three"));
    gnc_account_imap_add_account_bayes(t_imap, tokens, t_expense_account1);
    auto account = gnc_account_imap_find_account_bayes (t_imap, tokens);
    EXPECT_EQ (account, t_expense_account1);
    auto infos = gnc_account_imap_get_info_bayes (t_bank_account);
    EXPECT_EQ (g_list_first (infos), g_list_last (infos));
    auto info = static_cast <imap_info*> (g_list_first (infos)->data);
    EXPECT_EQ (info->source_account, t_bank_account);
    EXPECT_EQ (info->map_account, t_expense_account1);
    auto acct1_guid = guid_to_string (xaccAccountGetGUID(t_expense_account1)); //Food
    EXPECT_STREQ (info->head, (std::string {IMAP_FRAME_BAYES} + "/one/two/three/" + acct1_guid).c_str ());
    EXPECT_STREQ (info->match_string, "one/two/three");
    EXPECT_STREQ (info->count, "1");
}

