/********************************************************************
 * test-import-map.cpp: Test import match maps.                     *
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
#include <qofinstance-p.h>
#include <kvp_frame.h>

struct GncImportMatchMap
{
    Account *acc;
    QofBook *book;
};

extern GncImportMatchMap * gnc_account_create_imap (Account *acc);
extern Account* gnc_imap_find_account(GncImportMatchMap *imap,
				      const char* category,
				      const char *key);
extern void gnc_imap_add_account (GncImportMatchMap *imap,
				  const char *category,
				  const char *key, Account *acc);
extern Account* gnc_imap_find_account_bayes (GncImportMatchMap *imap,
					     GList* tokens);
extern void gnc_imap_add_account_bayes (GncImportMatchMap *imap,
					GList* tokens,
					Account *acc);
}

#include <gtest/gtest.h>

class ImapTest : public testing::Test
{
protected:
    void SetUp() {
        QofBook *book = qof_book_new();
        Account *root = gnc_account_create_root(book);
        t_bank_account = xaccMallocAccount(book);
        t_expense_account1 = xaccMallocAccount(book);
        xaccAccountSetName(t_expense_account1, "Food");
        gnc_account_append_child(root, t_expense_account1);
        t_expense_account2 = xaccMallocAccount(book);
        xaccAccountSetName(t_expense_account2, "Drink");
        gnc_account_append_child(root, t_expense_account2);
    }
    void TearDown() {
        qof_book_destroy (gnc_account_get_book (t_bank_account));
    }
    Account *t_bank_account {};
    Account *t_expense_account1 {};
    Account *t_expense_account2 {};
};

TEST_F(ImapTest, CreateImap) {
    GncImportMatchMap *imap = gnc_account_create_imap (t_bank_account);
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
        t_imap = gnc_account_create_imap (t_bank_account);
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
    auto acc1_val = kvp_value_new_guid(xaccAccountGetGUID(t_expense_account1));
    auto acc2_val = kvp_value_new_guid(xaccAccountGetGUID(t_expense_account2));
    kvp_frame_set_slot_path(root, acc1_val, IMAP_FRAME, "foo", "bar", NULL);
    kvp_frame_set_slot_path(root, acc2_val, IMAP_FRAME, "baz", "waldo", NULL);
    kvp_frame_set_slot_path(root, acc1_val, IMAP_FRAME, "pepper", NULL);
    kvp_frame_set_slot_path(root, acc2_val, IMAP_FRAME, "salt", NULL);
    kvp_value_delete(acc1_val);
    kvp_value_delete(acc2_val);

    EXPECT_EQ(t_expense_account1, gnc_imap_find_account(t_imap, "foo", "bar"));
    EXPECT_EQ(t_expense_account2,
              gnc_imap_find_account(t_imap, "baz", "waldo"));
    EXPECT_EQ(t_expense_account1,
              gnc_imap_find_account(t_imap, NULL, "pepper"));
    EXPECT_EQ(t_expense_account2, gnc_imap_find_account(t_imap, NULL, "salt"));
    EXPECT_EQ(nullptr, gnc_imap_find_account(t_imap, "salt", NULL));
}

TEST_F(ImapPlainTest, AddAccount)
{
// prevent the embedded beginedit/commitedit from doing anything
    qof_instance_increase_editlevel(QOF_INSTANCE(t_bank_account));
    qof_instance_mark_clean(QOF_INSTANCE(t_bank_account));
    gnc_imap_add_account(t_imap, "foo", "bar", t_expense_account1);
    gnc_imap_add_account(t_imap, "baz", "waldo", t_expense_account2);
    gnc_imap_add_account(t_imap, NULL, "pepper", t_expense_account1);
    gnc_imap_add_account(t_imap, NULL, "salt", t_expense_account2);
    EXPECT_EQ(1, qof_instance_get_editlevel(QOF_INSTANCE(t_bank_account)));
    EXPECT_TRUE(qof_instance_get_dirty_flag(QOF_INSTANCE(t_bank_account)));
    qof_instance_mark_clean(QOF_INSTANCE(t_bank_account));
    gnc_imap_add_account(t_imap, NULL, NULL, t_expense_account2);
    EXPECT_FALSE(qof_instance_get_dirty_flag(QOF_INSTANCE(t_bank_account)));
    gnc_imap_add_account(t_imap, "pork", "sausage", NULL);
    EXPECT_FALSE(qof_instance_get_dirty_flag(QOF_INSTANCE(t_bank_account)));
    qof_instance_reset_editlevel(QOF_INSTANCE(t_bank_account));

    auto root = qof_instance_get_slots(QOF_INSTANCE(t_bank_account));
    auto value = kvp_frame_get_slot_path(root, IMAP_FRAME, "foo", "bar", NULL);
    auto check_account = [this](KvpValue* v) {
        return xaccAccountLookup(kvp_value_get_guid(v), this->t_imap->book); };
    EXPECT_EQ(t_expense_account1, check_account(value));
    value = kvp_frame_get_slot_path(root, IMAP_FRAME, "baz", "waldo", NULL);
    EXPECT_EQ(t_expense_account2, check_account(value));
    value = kvp_frame_get_slot_path(root, IMAP_FRAME, "pepper", NULL);
    EXPECT_EQ(t_expense_account1, check_account(value));
    value = kvp_frame_get_slot_path(root, IMAP_FRAME, "salt", NULL);
    EXPECT_EQ(t_expense_account2, check_account(value));
    value = kvp_frame_get_slot_path(root, IMAP_FRAME, "pork", "sausage", NULL);
    EXPECT_EQ(nullptr, value);
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
    auto acct1_name = gnc_account_get_full_name(t_expense_account1);
    auto acct2_name = gnc_account_get_full_name(t_expense_account2);
    auto value = kvp_value_new_gint64(42);

    kvp_frame_set_slot_path(root, value, IMAP_FRAME_BAYES,
                            foo, acct1_name, NULL);
    kvp_frame_set_slot_path(root, value, IMAP_FRAME_BAYES,
                            bar, acct1_name, NULL);
    kvp_frame_set_slot_path(root, value, IMAP_FRAME_BAYES,
                            baz, acct2_name, NULL);
    kvp_frame_set_slot_path(root, value, IMAP_FRAME_BAYES,
                            waldo, acct2_name, NULL);
    kvp_frame_set_slot_path(root, value, IMAP_FRAME_BAYES,
                            pepper, acct1_name, NULL);
    kvp_frame_set_slot_path(root, value, IMAP_FRAME_BAYES,
                            salt, acct2_name, NULL);
    kvp_value_delete(value);

    auto account = gnc_imap_find_account_bayes(t_imap, t_list1);
    EXPECT_EQ(t_expense_account1, account);
    account = gnc_imap_find_account_bayes(t_imap, t_list2);
    EXPECT_EQ(t_expense_account2, account);
    account = gnc_imap_find_account_bayes(t_imap, t_list3);
    EXPECT_EQ(t_expense_account1, account);
    account = gnc_imap_find_account_bayes(t_imap, t_list4);
    EXPECT_EQ(t_expense_account2, account);
    account = gnc_imap_find_account_bayes(t_imap, t_list5);
    EXPECT_EQ(nullptr, account);
}

TEST_F(ImapBayesTest, AddAccountBayes)
{
    // prevent the embedded beginedit/commitedit from doing anything
    qof_instance_increase_editlevel(QOF_INSTANCE(t_bank_account));
    qof_instance_mark_clean(QOF_INSTANCE(t_bank_account));
    gnc_imap_add_account_bayes(t_imap, t_list1, t_expense_account1);
    gnc_imap_add_account_bayes(t_imap, t_list2, t_expense_account2);
    gnc_imap_add_account_bayes(t_imap, t_list3, t_expense_account1);
    gnc_imap_add_account_bayes(t_imap, t_list4, t_expense_account2);
    EXPECT_EQ(1, qof_instance_get_editlevel(QOF_INSTANCE(t_bank_account)));
    EXPECT_TRUE(qof_instance_get_dirty_flag(QOF_INSTANCE(t_bank_account)));
    qof_instance_mark_clean(QOF_INSTANCE(t_bank_account));
    gnc_imap_add_account_bayes(t_imap, t_list5, NULL);
    EXPECT_FALSE(qof_instance_get_dirty_flag(QOF_INSTANCE(t_bank_account)));
    qof_instance_reset_editlevel(QOF_INSTANCE(t_bank_account));

    auto root = qof_instance_get_slots(QOF_INSTANCE(t_bank_account));
    auto acct1_name = gnc_account_get_full_name(t_expense_account1);
    auto acct2_name = gnc_account_get_full_name(t_expense_account2);
    auto value = kvp_frame_get_slot_path(root, IMAP_FRAME_BAYES, "foo", "bar", NULL);
    auto check_account = [this](KvpValue* v) {
        return (kvp_value_get_string(v),
                                 this->t_imap->book); };
    value = kvp_frame_get_slot_path(root, IMAP_FRAME_BAYES, foo, acct1_name,
                                    NULL);
    EXPECT_EQ(1, kvp_value_get_gint64(value));
    value = kvp_frame_get_slot_path(root, IMAP_FRAME_BAYES, bar, acct1_name,
                                    NULL);
    EXPECT_EQ(1, kvp_value_get_gint64(value));
    value = kvp_frame_get_slot_path(root, IMAP_FRAME_BAYES, baz, acct2_name,
                                    NULL);
    EXPECT_EQ(1, kvp_value_get_gint64(value));
    value = kvp_frame_get_slot_path(root, IMAP_FRAME_BAYES, waldo, acct2_name,
                                    NULL);
    EXPECT_EQ(1, kvp_value_get_gint64(value));
    value = kvp_frame_get_slot_path(root, IMAP_FRAME_BAYES, pepper, acct1_name,
                                    NULL);
    EXPECT_EQ(1, kvp_value_get_gint64(value));
    value = kvp_frame_get_slot_path(root, IMAP_FRAME_BAYES, salt, acct2_name,
                                    NULL);
    EXPECT_EQ(1, kvp_value_get_gint64(value));
    value = kvp_frame_get_slot_path(root, IMAP_FRAME_BAYES, baz, acct1_name,
                                    NULL);
    EXPECT_EQ(0, kvp_value_get_gint64(value));

    qof_instance_increase_editlevel(QOF_INSTANCE(t_bank_account));
    gnc_imap_add_account_bayes(t_imap, t_list2, t_expense_account2);
    qof_instance_mark_clean(QOF_INSTANCE(t_bank_account));
    qof_instance_reset_editlevel(QOF_INSTANCE(t_bank_account));
    value = kvp_frame_get_slot_path(root, IMAP_FRAME_BAYES, baz, acct2_name,
                                    NULL);
    EXPECT_EQ(2, kvp_value_get_gint64(value));
}
