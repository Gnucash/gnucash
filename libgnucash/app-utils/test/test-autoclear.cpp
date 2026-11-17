/********************************************************************
 * test-autoclear.c: test suite for Auto-Clear          	    *
 * Copyright 2020 Cristian Klein <cristian@kleinlabs.eu>            *
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
 * <https://www.gnu.org/licenses/>                                  *
 ********************************************************************/
#include "config.h"
#include <glib.h>
// GoogleTest is written in C++, however, the function we test in C.
#include "../gnc-autoclear.h"
#include <memory>
#include <cstdint>
#include <Account.h>
#include <Split.h>
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcpp"
#include <gtest/gtest.h>
#pragma GCC diagnostic pop


static const int64_t DENOM = 100; //< Denominator is always 100 for simplicity.

struct SplitDatum {
    const char *memo;
    gint64 amount; //< Numerator of amount.
    bool cleared;
};

struct Tests {
    gint64 amount;
    const char *expectedErr;
};

struct TestCase {
    std::vector<SplitDatum> splits;
    std::vector<Tests> tests;
};

TestCase easyTestCase = {
    .splits = {
        { "Memo 01", -  8234, true },
        { "Memo 02", -156326, true },
        { "Memo 03", -  4500, true },
        { "Memo 04", -694056, true },
        { "Memo 05", -  7358, true },
        { "Memo 06", - 11700, true },
        { "Memo 07", - 20497, true },
        { "Memo 08", - 11900, true },
        { "Memo 09", -  8275, true },
        { "Memo 10", - 58700, true },
        { "Memo 11", +100000, true },
        { "Memo 12", - 13881, true },
        { "Memo 13", -  5000, true },
        { "Memo 14", +200000, true },
        { "Memo 15", - 16800, true },
        { "Memo 16", -152000, true },
        { "Memo 17", +160000, false },
        { "Memo 18", - 63610, false },
        { "Memo 19", -  2702, false },
        { "Memo 20", - 15400, false },
        { "Memo 21", -  3900, false },
        { "Memo 22", - 22042, false },
        { "Memo 23", -  2900, false },
        { "Memo 24", - 10900, false },
        { "Memo 25", - 44400, false },
        { "Memo 26", -  9200, false },
        { "Memo 27", -  7900, false },
        { "Memo 28", -  1990, false },
        { "Memo 29", -  7901, false },
        { "Memo 30", - 61200, false },
    },
    .tests = {
        { 0, "The selected amount cannot be cleared.", },
        { -869227, "Account is already at Auto-Clear Balance." }, // No splits need to be cleared.
        { -869300, "The selected amount cannot be cleared." },
        { -869230, NULL },
        { -963272, NULL }, // All splits need to be cleared.
    },
};

TestCase ambiguousTestCase = {
    .splits = {
        { "Memo 01", -10, false },
        { "Memo 02", -10, false },
        { "Memo 03", -10, false },
    },
    .tests = {
        { -10, "Cannot uniquely clear splits. Found multiple possibilities." },
        { -20, "Cannot uniquely clear splits. Found multiple possibilities." },

        // -30 can be cleared by returning all three -10 splits
        { -30, nullptr },
    },
};


TestCase sequentialTestCase1 =
{
    .splits = {
        { "Memo 01", 2, false },
        { "Memo 02", 3, false },
        { "Memo 03", 5, false },
    },
    .tests = {
        { 1, "The selected amount cannot be cleared." },
        { 4, "The selected amount cannot be cleared." },
        { 5, "Cannot uniquely clear splits. Found multiple possibilities." },
        { 6, "The selected amount cannot be cleared." },
        { 9, "The selected amount cannot be cleared." },
        { 11, "The selected amount cannot be cleared." },
        { 12, "The selected amount cannot be cleared." },
        { 2, nullptr },
        { 7, nullptr },
        { 10, nullptr },
    },
};

TestCase sequentialTestCase2 =
{
    .splits = {
        { "Memo 01", 2, false },
        { "Memo 02", 3, false },
        { "Memo 03", 5, false },
    },
    .tests = {
        { 3, nullptr },
        { 8, nullptr },
        { 10, nullptr },
    },
};


TestCase sequentialTestCase3 =
{
    .splits = {
        { "Memo 01", 2, false },
        { "Memo 02", 3, false },
        { "Memo 03", 5, false },
        { "Memo 04", 5, false },
    },
    .tests = {
        { 5, "Cannot uniquely clear splits. Found multiple possibilities." },
        { 7, "Cannot uniquely clear splits. Found multiple possibilities." },
        { 10, "Cannot uniquely clear splits. Found multiple possibilities." },
        { 2, nullptr },
        { 12, nullptr },
        { 15, nullptr },
    },
};

TestCase sequentialTestCase4 =
{
    .splits = {
        { "Memo 01", 2, false },
        { "Memo 02", 3, false },
        { "Memo 03", 5, false },
        { "Memo 04", 5, false },
    },
    .tests = {
        { 3, nullptr },
        { 13, nullptr },
        { 15, nullptr },
    },
};

TestCase slowTestCase =
{
    .splits = {
        { "Memo 01", 1, false },
        { "Memo 02", 2, false },
        { "Memo 03", 4, false },
        { "Memo 03", 8, false },
        { "Memo 03", 16, false },
        { "Memo 03", 32, false },
        { "Memo 03", 64, false },
        { "Memo 03", 128, false },
        { "Memo 03", 256, false },
        { "Memo 03", 512, false },
        { "Memo 03", 1024, false },
        { "Memo 03", 2048, false },
        { "Memo 03", 4096, false },
        { "Memo 03", 8192, false },
        { "Memo 03", 16384, false },
        { "Memo 03", 32768, false },
        { "Memo 03", 65536, false },
        { "Memo 03", 131072, false },
        { "Memo 03", 262144, false },
        { "Memo 03", 524288, false },
        { "Memo 03", 1048576, false },
    },
    .tests = {
        { 6000000, "The selected amount cannot be cleared." },
        { 5000000, "The selected amount cannot be cleared." },
        { 4000000, "The selected amount cannot be cleared." },
        { 3000000, "The selected amount cannot be cleared." },
        { 2000000, nullptr },
    },
};

TestCase mixedSignsTestCase =
{
    .splits = {

        { "memo 01", 30000 },
        { "memo 01", -20000 },
        { "memo 01", 10000 },
        { "memo 01", 500 },
        { "memo 01", -400 },
        { "memo 01", 300 }
    },
    .tests = {
        { 30300, nullptr },
    },
};

TestCase testwithzerosTestCase =
{
    .splits = {
        { "memo 01", 10000 },
        { "memo 01", 0 },
        { "memo 01", 10000 },
        { "memo 01", 5000 },
    },
    .tests = {
        { 19999, "The selected amount cannot be cleared." },
        { 20001, "The selected amount cannot be cleared." },
        { 20000, nullptr },
    },
};


class AutoClearTest : public ::testing::TestWithParam<TestCase *> {
protected:
    std::shared_ptr<QofBook> m_book;
    Account *m_account; // owned by m_book
    TestCase &m_testCase;

public:
    AutoClearTest() :
        m_book(qof_book_new(), qof_book_destroy),
        m_account(xaccMallocAccount(m_book.get())),
        m_testCase(*GetParam())
    {
        std::cout << "\n\ncreating new account with splits";
        xaccAccountSetName(m_account, "Test Account");
        xaccAccountBeginEdit(m_account);
        for (auto &d : m_testCase.splits) {
            Split *split = xaccMallocSplit(m_book.get());
            xaccSplitSetMemo(split, d.memo);
            xaccSplitSetAmount(split, gnc_numeric_create(d.amount, DENOM));
            xaccSplitSetReconcile(split, d.cleared ? CREC : NREC);
            xaccSplitSetAccount(split, m_account);
            std::cout << ' ' << d.amount;

            gnc_account_insert_split(m_account, split);
        }
        xaccAccountCommitEdit(m_account);
        std::cout << std::endl;
    }
};

TEST_P(AutoClearTest, DoesAutoClear) {
    for (auto &t : m_testCase.tests) {
        gnc_numeric amount_to_clear = gnc_numeric_create(t.amount, DENOM);
        GError *error = nullptr;

        GList *splits_to_clear = gnc_account_get_autoclear_splits
            (m_account, amount_to_clear, INT64_MAX, &error, 0);

        auto err = error ? error->message : nullptr;

        std::cout << "testing clearing " << t.amount
                  << " should be [" << std::string (err ? err : "ok") << ']'
                  << std::endl;

        // Actually clear splits
        for (GList *node = splits_to_clear; node; node = node->next) {
            Split *split = (Split *)node->data;
            xaccSplitSetReconcile(split, CREC);
        }

        g_list_free (splits_to_clear);

        EXPECT_STREQ(err, t.expectedErr);
        if (t.expectedErr == NULL) {
            gnc_numeric c = xaccAccountGetClearedBalance(m_account);
            EXPECT_EQ(c.num, t.amount);
            EXPECT_EQ(c.denom, DENOM);
        }
        if (error)
            g_error_free (error);
    }
}

#ifndef INSTANTIATE_TEST_SUITE_P
// Silence "no previous declaration for" (treated as error due to -Werror) when building with GoogleTest < 1.8.1
static testing::internal::ParamGenerator<TestCase*> gtest_InstantiationAutoClearTestAutoClearTest_EvalGenerator_();
static std::string gtest_InstantiationAutoClearTestAutoClearTest_EvalGenerateName_(const testing::TestParamInfo<TestCase*>&);

INSTANTIATE_TEST_CASE_P(
#else // INSTANTIATE_TEST_SUITE_P
INSTANTIATE_TEST_SUITE_P(
#endif // INSTANTIATE_TEST_SUITE_P
    InstantiationAutoClearTest,
    AutoClearTest,
    ::testing::Values(
        &easyTestCase,
        &sequentialTestCase1,
        &sequentialTestCase2,
        &sequentialTestCase3,
        &sequentialTestCase4,
        &slowTestCase,
        &mixedSignsTestCase,
        &testwithzerosTestCase,
        &ambiguousTestCase
    )
);
