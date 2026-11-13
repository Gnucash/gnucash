/********************************************************************
 * gtest-search-account.cpp: Unit tests for GNCSearchAccount        *
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
 * <https://www.gnu.org/licenses/>.                                 *
\********************************************************************/

#include <config.h>
#include <gtk/gtk.h>
#include <glib.h>
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcpp"
#include <gtest/gtest.h>
#pragma GCC diagnostic pop

extern "C"
{
#include "Account.h"
#include "search-account.h"
}

TEST(SearchAccountTest, InitialSelectionEmpty)
{
    GNCSearchAccount *fi = gnc_search_account_new();
    ASSERT_TRUE(GNC_IS_SEARCH_ACCOUNT(fi));
    EXPECT_EQ(gnc_search_account_get_selected_accounts(fi), nullptr);
    g_object_unref(fi);
}

TEST(SearchAccountTest, SetAndClearSelectedAccounts)
{
    GNCSearchAccount *fi = gnc_search_account_new();
    ASSERT_TRUE(GNC_IS_SEARCH_ACCOUNT(fi));

    QofBook *book = qof_book_new();
    Account *acc1 = xaccMallocAccount(book);
    Account *acc2 = xaccMallocAccount(book);

    GList *list = nullptr;
    list = g_list_append(list, acc1);
    list = g_list_append(list, acc2);

    gnc_search_account_set_selected_accounts(fi, list);
    g_list_free(list);

    GList *selected = gnc_search_account_get_selected_accounts(fi);
    ASSERT_NE(selected, nullptr);
    EXPECT_EQ(g_list_length(selected), 2u);
    EXPECT_EQ(selected->data, acc1);
    EXPECT_EQ(selected->next->data, acc2);

    /* Clearing the selection */
    gnc_search_account_set_selected_accounts(fi, nullptr);
    EXPECT_EQ(gnc_search_account_get_selected_accounts(fi), nullptr);

    xaccAccountDestroy(acc1);
    xaccAccountDestroy(acc2);
    qof_book_destroy(book);
    g_object_unref(fi);
}
