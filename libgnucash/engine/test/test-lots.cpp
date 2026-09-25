/***************************************************************************
 *            test-lots.c
 *
 *  Copyright (C) 2003 Linas Vepstas <linas@linas.org>
 ****************************************************************************/
/*
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301, USA.
 */
/**
 * @file test-lots.c
 * @brief Minimal test to see if automatic lot scrubbing works.
 * @author Linas Vepstas <linas@linas.org>
 */
#include <glib.h>

#include <config.h>
#include <ctype.h>
#include "qofinstance-p.h"
#include "qof.h"
#include "Account.h"
#include "AccountP.hpp"
#include "gnc-lot.h"
#include "Scrub3.h"
#include "cashobjects.h"
#include "test-stuff.h"
#include "test-engine-stuff.h"
#include "Transaction.h"
#include "cap-gains.h"
#include "policy-p.h"
#include "TransactionP.hpp"
#include "Scrub.h"
#include "Scrub2.h"
#include "gnc-session.h"

class ScopedEnvironment
{
public:
    ScopedEnvironment (const char *name, const char *value) : m_name (name),
        m_old_value (g_strdup (g_getenv (name)))
    {
        if (value)
            g_setenv (name, value, TRUE);
        else
            g_unsetenv (name);
    }

    ~ScopedEnvironment ()
    {
        if (m_old_value)
            g_setenv (m_name, m_old_value, TRUE);
        else
            g_unsetenv (m_name);
        g_free (m_old_value);
    }

private:
    const char *m_name;
    gchar *m_old_value;
};

struct TransactionModifyCounter
{
    Transaction *transaction;
    guint count;
};

static void
count_transaction_modifies (QofInstance *instance, QofEventId event_type,
                            gpointer handler_data, gpointer)
{
    auto counter = static_cast<TransactionModifyCounter *> (handler_data);
    if (counter && instance == QOF_INSTANCE (counter->transaction) &&
        (event_type & QOF_EVENT_MODIFY))
        ++counter->count;
}

static gint transaction_num = 32;
static gint	max_iterate = 1;


static void
test_lot_kvp ()
{
    QofSession *sess = get_random_session ();
    QofBook *book = qof_session_get_book (sess);
    GNCLot *lot = gnc_lot_new (book);

    // title
    g_assert_cmpstr (gnc_lot_get_title (lot), ==, NULL);

    gnc_lot_set_title (lot, "");
    g_assert_cmpstr (gnc_lot_get_title (lot), ==, "");

    gnc_lot_set_title (lot, "doc");
    g_assert_cmpstr (gnc_lot_get_title (lot), ==, "doc");

    gnc_lot_set_title (lot, "unset");
    g_assert_cmpstr (gnc_lot_get_title (lot), ==, "unset");

    gnc_lot_set_title (lot, NULL);
    g_assert_cmpstr (gnc_lot_get_title (lot), ==, NULL);

    // notes
    g_assert_cmpstr (gnc_lot_get_notes (lot), ==, NULL);

    gnc_lot_set_notes (lot, "");
    g_assert_cmpstr (gnc_lot_get_notes (lot), ==, "");

    gnc_lot_set_notes (lot, "doc");
    g_assert_cmpstr (gnc_lot_get_notes (lot), ==, "doc");

    gnc_lot_set_notes (lot, "unset");
    g_assert_cmpstr (gnc_lot_get_notes (lot), ==, "unset");

    gnc_lot_set_notes (lot, NULL);
    g_assert_cmpstr (gnc_lot_get_notes (lot), ==, NULL);

    gnc_lot_destroy (lot);
    qof_session_destroy (sess);
}

struct LotAssignmentFixture
{
    QofSession *session;
    QofBook *book;
    Account *account;
    gnc_commodity *currency;
    Transaction *first_transaction;
    Transaction *second_transaction;
    Transaction *overflow_transaction;
};

static Split *
add_lot_assignment_split_with_currency (LotAssignmentFixture *fixture,
                                        gint64 amount, time64 date,
                                        gnc_commodity *currency)
{
    auto transaction = xaccMallocTransaction (fixture->book);
    auto split = xaccMallocSplit (fixture->book);
    auto numeric = gnc_numeric_create (amount, 1);

    xaccTransBeginEdit (transaction);
    xaccTransSetCurrency (transaction, currency);
    xaccTransSetDatePostedSecsNormalized (transaction, date);
    xaccSplitSetParent (split, transaction);
    xaccSplitSetAccount (split, fixture->account);
    xaccSplitSetAmount (split, numeric);
    xaccSplitSetValue (split, numeric);
    xaccTransCommitEdit (transaction);
    return split;
}

static Split *
add_lot_assignment_split (LotAssignmentFixture *fixture, gint64 amount,
                          time64 date)
{
    return add_lot_assignment_split_with_currency (
        fixture, amount, date, fixture->currency);
}

static LotAssignmentFixture
make_lot_assignment_fixture (void)
{
    LotAssignmentFixture fixture {};
    fixture.session = qof_session_new (qof_book_new ());
    fixture.book = qof_session_get_book (fixture.session);
    fixture.currency = gnc_commodity_new (fixture.book, "Lot Test Currency",
                                          "CURRENCY", "LTC", "", 100);
    auto stock = gnc_commodity_new (fixture.book, "Lot Test Stock", "NYSE",
                                     "LTS", "", 1000);
    auto root = gnc_account_create_root (fixture.book);
    fixture.account = xaccMallocAccount (fixture.book);

    xaccAccountBeginEdit (fixture.account);
    xaccAccountSetName (fixture.account, "Lot assignment account");
    xaccAccountSetType (fixture.account, ACCT_TYPE_STOCK);
    xaccAccountSetCommodity (fixture.account, stock);
    gnc_account_append_child (root, fixture.account);
    xaccAccountCommitEdit (fixture.account);

    fixture.first_transaction = xaccSplitGetParent (
        add_lot_assignment_split (&fixture, 10, 86400));
    fixture.second_transaction = xaccSplitGetParent (
        add_lot_assignment_split (&fixture, 10, 172800));
    fixture.overflow_transaction = xaccSplitGetParent (
        add_lot_assignment_split (&fixture, -25, 259200));
    return fixture;
}

static gboolean
lot_assignment_matches (const Account *actual, const Account *expected)
{
    auto actual_lots = xaccAccountGetLotList (actual);
    auto expected_lots = xaccAccountGetLotList (expected);
    gboolean matches = g_list_length (actual_lots) == g_list_length (expected_lots) &&
                       xaccAccountGetSplitsSize (actual) ==
                       xaccAccountGetSplitsSize (expected);

    for (auto actual_node = actual_lots, expected_node = expected_lots;
         matches && actual_node && expected_node;
         actual_node = actual_node->next, expected_node = expected_node->next)
    {
        auto actual_lot = GNC_LOT (actual_node->data);
        auto expected_lot = GNC_LOT (expected_node->data);
        matches = gnc_lot_is_closed (actual_lot) == gnc_lot_is_closed (expected_lot) &&
                  gnc_numeric_equal (gnc_lot_get_balance (actual_lot),
                                     gnc_lot_get_balance (expected_lot)) &&
                  gnc_lot_count_splits (actual_lot) ==
                  gnc_lot_count_splits (expected_lot);

        for (auto actual_split = gnc_lot_get_split_list (actual_lot),
                  expected_split = gnc_lot_get_split_list (expected_lot);
             matches && actual_split && expected_split;
             actual_split = actual_split->next, expected_split = expected_split->next)
            matches = gnc_numeric_equal (
                          xaccSplitGetAmount (GNC_SPLIT (actual_split->data)),
                          xaccSplitGetAmount (GNC_SPLIT (expected_split->data))) &&
                      gnc_numeric_equal (
                          xaccSplitGetValue (GNC_SPLIT (actual_split->data)),
                          xaccSplitGetValue (GNC_SPLIT (expected_split->data)));
    }

    g_list_free (actual_lots);
    g_list_free (expected_lots);
    return matches;
}

static void
destroy_lot_assignment_fixture (LotAssignmentFixture *fixture)
{
    qof_session_destroy (fixture->session);
}

static LotAssignmentFixture
make_empty_lot_assignment_fixture (void)
{
    LotAssignmentFixture fixture {};
    fixture.session = qof_session_new (qof_book_new ());
    fixture.book = qof_session_get_book (fixture.session);
    fixture.currency = gnc_commodity_new (fixture.book, "Lot Test Currency",
                                          "CURRENCY", "LTC", "", 100);
    auto stock = gnc_commodity_new (fixture.book, "Lot Test Stock", "NYSE",
                                     "LTS", "", 1000);
    auto root = gnc_account_create_root (fixture.book);
    fixture.account = xaccMallocAccount (fixture.book);

    xaccAccountBeginEdit (fixture.account);
    xaccAccountSetName (fixture.account, "Lot work budget account");
    xaccAccountSetType (fixture.account, ACCT_TYPE_STOCK);
    xaccAccountSetCommodity (fixture.account, stock);
    gnc_account_append_child (root, fixture.account);
    xaccAccountCommitEdit (fixture.account);
    return fixture;
}

struct GainsRelationshipFixture
{
    Split *source;
    Split *lot_split;
    Split *income_split;
    Transaction *source_transaction;
    Transaction *gains_transaction;
};

static GainsRelationshipFixture
add_gains_relationship (LotAssignmentFixture *fixture, time64 source_date,
                        time64 gains_date)
{
    auto source = add_lot_assignment_split (fixture, 10, source_date);
    auto source_transaction = xaccSplitGetParent (source);
    auto gains_transaction = xaccMallocTransaction (fixture->book);
    auto lot_split = xaccMallocSplit (fixture->book);
    auto income_split = xaccMallocSplit (fixture->book);
    auto income_account = xaccMallocAccount (fixture->book);
    auto root = gnc_book_get_root_account (fixture->book);
    auto zero = gnc_numeric_zero ();
    auto gain = gnc_numeric_create (-10, 1);
    auto income = gnc_numeric_neg (gain);

    xaccAccountBeginEdit (income_account);
    xaccAccountSetName (income_account, "Lot gains income account");
    xaccAccountSetType (income_account, ACCT_TYPE_INCOME);
    xaccAccountSetCommodity (income_account, fixture->currency);
    gnc_account_append_child (root, income_account);
    xaccAccountCommitEdit (income_account);

    xaccTransBeginEdit (gains_transaction);
    xaccTransSetCurrency (gains_transaction, fixture->currency);
    xaccTransSetDatePostedSecsNormalized (gains_transaction, gains_date);
    xaccSplitSetParent (lot_split, gains_transaction);
    xaccSplitSetAccount (lot_split, fixture->account);
    xaccSplitSetAmount (lot_split, zero);
    xaccSplitSetValue (lot_split, gain);
    xaccSplitSetParent (income_split, gains_transaction);
    xaccSplitSetAccount (income_split, income_account);
    xaccSplitSetAmount (income_split, income);
    xaccSplitSetValue (income_split, income);
    xaccTransCommitEdit (gains_transaction);

    xaccTransBeginEdit (source_transaction);
    qof_instance_set (QOF_INSTANCE (source), "gains-split",
                      xaccSplitGetGUID (lot_split), nullptr);
    xaccTransCommitEdit (source_transaction);
    xaccTransBeginEdit (gains_transaction);
    qof_instance_set (QOF_INSTANCE (lot_split), "gains-source",
                      xaccSplitGetGUID (source), nullptr);
    xaccTransCommitEdit (gains_transaction);

    return {source, lot_split, income_split, source_transaction,
            gains_transaction};
}

static void
populate_lot_assignment_noop_workload (LotAssignmentFixture *fixture)
{
    for (guint i = 0; i < 6; ++i)
    {
        auto split = add_lot_assignment_split (fixture, 0, (i + 1) * 86400);
        if (i % 2 == 0)
            xaccTransVoid (xaccSplitGetParent (split), "bounded no-op");
    }

    auto assigned = add_lot_assignment_split (fixture, 10, 7 * 86400);
    xaccSplitAssign (assigned);
    add_lot_assignment_split (fixture, 10, 8 * 86400);
    add_lot_assignment_split (fixture, -25, 9 * 86400);
}

static void
test_incremental_lot_assignment_work_budget (void)
{
    if (gnc_current_session_exist ())
        gnc_clear_current_session ();

    auto incremental = make_empty_lot_assignment_fixture ();
    auto synchronous = make_empty_lot_assignment_fixture ();
    populate_lot_assignment_noop_workload (&incremental);
    populate_lot_assignment_noop_workload (&synchronous);
    gnc_set_current_session (incremental.session);
    auto context = gnc_scrub_context_begin (incremental.book);
    auto plan = gnc_lot_assignment_plan_begin (incremental.account, context);
    do_test (plan != NULL, "start bounded no-op lot assignment");

    guint steps = 0;
    while (plan && gnc_lot_assignment_plan_get_state (plan) ==
           GNC_LOT_ASSIGNMENT_PLAN_RUNNING && steps < 32)
    {
        auto before_examined = gnc_lot_assignment_plan_get_examined (plan);
        auto before_completed = gnc_lot_assignment_plan_get_completed (plan);
        gnc_lot_assignment_plan_step (plan, 2);
        do_test (gnc_lot_assignment_plan_get_examined (plan) - before_examined <= 2,
                 "lot assignment step dequeues at most max work entries");
        ++steps;
        if (steps <= 3)
            do_test (gnc_lot_assignment_plan_get_completed (plan) == before_completed,
                     "no-op entries consume bounded work without mutation");
    }

    do_test (steps > 3, "no-op queue reaches done only after multiple work turns");
    do_test (gnc_lot_assignment_plan_get_state (plan) ==
             GNC_LOT_ASSIGNMENT_PLAN_DONE,
             "no-op work budget plan reaches done state");
    xaccAccountAssignLots (synchronous.account);
    do_test (lot_assignment_matches (incremental.account, synchronous.account),
             "no-op work budget plan matches synchronous result");

    gnc_lot_assignment_plan_free (plan);
    gnc_scrub_context_end (context);
    gnc_scrub_context_unref (context);
    gnc_clear_current_session ();
    destroy_lot_assignment_fixture (&synchronous);
}

static void
test_incremental_lot_assignment (void)
{
    if (gnc_current_session_exist ())
        gnc_clear_current_session ();

    auto incremental = make_lot_assignment_fixture ();
    auto synchronous = make_lot_assignment_fixture ();
    gnc_set_current_session (incremental.session);
    auto context = gnc_scrub_context_begin (incremental.book);
    auto plan = gnc_lot_assignment_plan_begin (incremental.account, context);
    do_test (context != NULL && plan != NULL,
             "start bounded lot assignment with active scrub context");

    guint steps = 0;
    while (plan && gnc_lot_assignment_plan_get_state (plan) ==
           GNC_LOT_ASSIGNMENT_PLAN_RUNNING && steps < 16)
    {
        auto before = gnc_lot_assignment_plan_get_completed (plan);
        auto state = gnc_lot_assignment_plan_step (plan, 1);
        auto after = gnc_lot_assignment_plan_get_completed (plan);
        do_test (after - before <= 1, "lot assignment step respects max mutations");
        ++steps;
        if (steps == 3)
            do_test (xaccAccountGetSplitsSize (incremental.account) == 4,
                     "first sale assignment queues one remainder split");
        if (steps == 4)
            do_test (xaccAccountGetSplitsSize (incremental.account) == 5,
                     "remainder is assigned before later input and splits again");
        if (state != GNC_LOT_ASSIGNMENT_PLAN_RUNNING)
            break;
    }
    do_test (steps == 5, "lot assignment completes across five bounded mutations");
    do_test (gnc_lot_assignment_plan_get_state (plan) ==
             GNC_LOT_ASSIGNMENT_PLAN_DONE,
             "bounded lot assignment reaches done state");

    xaccAccountAssignLots (synchronous.account);
    do_test (lot_assignment_matches (incremental.account, synchronous.account),
             "bounded lot assignment matches synchronous FIFO result");

    gnc_lot_assignment_plan_free (plan);
    gnc_scrub_context_end (context);
    gnc_scrub_context_unref (context);
    gnc_clear_current_session ();
    destroy_lot_assignment_fixture (&synchronous);
}

static void
test_incremental_lot_assignment_cancel_and_stale (void)
{
    if (gnc_current_session_exist ())
        gnc_clear_current_session ();

    auto cancelled = make_lot_assignment_fixture ();
    gnc_set_current_session (cancelled.session);
    auto cancelled_context = gnc_scrub_context_begin (cancelled.book);
    auto cancelled_plan = gnc_lot_assignment_plan_begin (cancelled.account,
                                                          cancelled_context);
    do_test (cancelled_plan != NULL, "start cancellable lot assignment");
    gnc_lot_assignment_plan_step (cancelled_plan, 1);
    auto completed = gnc_lot_assignment_plan_get_completed (cancelled_plan);
    gnc_lot_assignment_plan_cancel (cancelled_plan);
    do_test (gnc_lot_assignment_plan_step (cancelled_plan, 1) ==
             GNC_LOT_ASSIGNMENT_PLAN_CANCELLED &&
             gnc_lot_assignment_plan_get_completed (cancelled_plan) == completed,
             "cancelled plan performs no follow-up mutation");
    gnc_lot_assignment_plan_free (cancelled_plan);
    gnc_scrub_context_end (cancelled_context);
    gnc_scrub_context_unref (cancelled_context);
    gnc_clear_current_session ();

    auto stale = make_lot_assignment_fixture ();
    gnc_set_current_session (stale.session);
    auto stale_context = gnc_scrub_context_begin (stale.book);
    auto stale_plan = gnc_lot_assignment_plan_begin (stale.account, stale_context);
    do_test (stale_plan != NULL, "start stale-context lot assignment");
    gnc_lot_assignment_plan_step (stale_plan, 1);
    completed = gnc_lot_assignment_plan_get_completed (stale_plan);
    gnc_scrub_context_end (stale_context);
    do_test (gnc_lot_assignment_plan_step (stale_plan, 1) ==
             GNC_LOT_ASSIGNMENT_PLAN_STALE &&
             gnc_lot_assignment_plan_get_completed (stale_plan) == completed,
             "stale context prevents a follow-up mutation");
    gnc_lot_assignment_plan_free (stale_plan);
    gnc_scrub_context_unref (stale_context);
    gnc_clear_current_session ();
}

static void
test_incremental_lot_assignment_defers_auto_gains (void)
{
    if (gnc_current_session_exist ())
        gnc_clear_current_session ();

    LotAssignmentFixture fixture {};
    {
        ScopedEnvironment lots_off {"GNC_AUTO_SCRUB_LOTS", nullptr};
        fixture = make_lot_assignment_fixture ();
    }
    gnc_set_current_session (fixture.session);
    auto context = gnc_scrub_context_begin (fixture.book);
    xaccEnableDataScrubbing ();
    do_test (context != nullptr, "start lot assignment deferral context");

    {
        ScopedEnvironment lots_on {"GNC_AUTO_SCRUB_LOTS", "1"};
        auto blocked = gnc_lot_assignment_plan_begin (fixture.account, context);
        do_test (blocked == nullptr,
                 "auto-lot bounded plan requires active gains deferral");

        do_test (gnc_scrub_context_enable_commit_deferral (
                     context, GNC_SCRUB_DEFERRED_COMMIT_GAINS),
                 "enable gains deferral for bounded lot assignment");
        auto plan = gnc_lot_assignment_plan_begin (fixture.account, context);
        do_test (plan != nullptr,
                 "start auto-lot plan with active gains deferral");

        for (guint step = 0; plan && step < 3; ++step)
        {
            gnc_lot_assignment_plan_step (plan, 1);
            do_test (gnc_scrub_deferred_commit_pending_count (
                         context, GNC_SCRUB_DEFERRED_COMMIT_GAINS) == step + 1,
                     "each bounded lot assignment queues one gains transaction");
        }
        do_test (plan && gnc_lot_assignment_plan_get_state (plan) ==
                 GNC_LOT_ASSIGNMENT_PLAN_RUNNING,
                 "overflow assignment remains resumable after bounded work");
        do_test (xaccAccountGetSplitsSize (fixture.account) == 4,
                 "third bounded assignment splits the overflowing sale");

        auto first_guid = *xaccTransGetGUID (fixture.first_transaction);
        GncGUID guid;
        do_test (gnc_scrub_deferred_commit_peek (
                     context, GNC_SCRUB_DEFERRED_COMMIT_GAINS, &guid) &&
                 guid_equal (&guid, &first_guid),
                 "split commits queue gains work in transaction FIFO order");

        xaccTransBeginEdit (fixture.second_transaction);
        xaccTransCommitEdit (fixture.second_transaction);
        do_test (gnc_scrub_deferred_commit_pending_count (
                     context, GNC_SCRUB_DEFERRED_COMMIT_GAINS) == 3,
                 "repeated second-transaction commit is deduplicated");

        xaccTransBeginEdit (fixture.overflow_transaction);
        xaccTransCommitEdit (fixture.overflow_transaction);
        do_test (gnc_scrub_deferred_commit_pending_count (
                     context, GNC_SCRUB_DEFERRED_COMMIT_GAINS) == 3,
                 "repeated overflow commit is deduplicated");

        gnc_lot_assignment_plan_cancel (plan);
        gnc_lot_assignment_plan_free (plan);
    }

    gnc_scrub_context_cancel (context);
    gnc_scrub_context_end (context);
    gnc_scrub_context_unref (context);

    auto later_context = gnc_scrub_context_begin (fixture.book);
    GncGUID first_head, repeated_head;
    do_test (later_context != nullptr &&
             gnc_scrub_deferred_commit_pending_count (
                 later_context, GNC_SCRUB_DEFERRED_COMMIT_GAINS) == 3 &&
             gnc_scrub_deferred_commit_peek (
                 later_context, GNC_SCRUB_DEFERRED_COMMIT_GAINS, &first_head) &&
             guid_equal (&first_head, xaccTransGetGUID (fixture.first_transaction)) &&
             gnc_scrub_deferred_commit_peek (
                 later_context, GNC_SCRUB_DEFERRED_COMMIT_GAINS, &repeated_head) &&
             guid_equal (&repeated_head, &first_head) &&
             gnc_scrub_deferred_commit_pending_count (
                 later_context, GNC_SCRUB_DEFERRED_COMMIT_GAINS) == 3,
             "cancelled plan hands an unchanged gains head to a later context");

    gnc_scrub_context_end (later_context);
    gnc_scrub_context_unref (later_context);
    gnc_clear_current_session ();
}

static void
set_overflow_value (LotAssignmentFixture *fixture, gint64 value)
{
    auto split = xaccTransFindSplitByAccount (fixture->overflow_transaction,
                                               fixture->account);
    xaccTransBeginEdit (fixture->overflow_transaction);
    xaccSplitSetValue (split, gnc_numeric_create (value, 1));
    xaccTransCommitEdit (fixture->overflow_transaction);
}

static guint
count_nonzero_gain_splits (Account *account)
{
    guint count = 0;
    auto splits = xaccAccountGetSplitList (account);
    for (auto node = splits; node; node = node->next)
    {
        auto split = GNC_SPLIT (node->data);
        if (gnc_numeric_zero_p (xaccSplitGetAmount (split)) &&
            !gnc_numeric_zero_p (xaccSplitGetValue (split)))
            ++count;
    }
    g_list_free (splits);
    return count;
}

static void
test_account_trades_collector_is_bounded_and_generation_safe (void)
{
    if (gnc_current_session_exist ())
        gnc_clear_current_session ();
    ScopedEnvironment lots_off {"GNC_AUTO_SCRUB_LOTS", nullptr};
    auto fixture = make_empty_lot_assignment_fixture ();
    xaccAccountSetType (fixture.account, ACCT_TYPE_ASSET);
    xaccAccountSetCommodity (fixture.account, fixture.currency);

    Split *target = nullptr;
    Transaction *non_lot_transaction = nullptr;
    for (guint i = 0; i < 64; ++i)
    {
        auto split = add_lot_assignment_split (&fixture, 10,
                                                (i + 1) * 86400);
        if (!target) target = split;
        if (i == 1) non_lot_transaction = xaccSplitGetParent (split);
    }
    gnc_set_current_session (fixture.session);
    auto context = gnc_scrub_context_begin (fixture.book);
    auto collector = gnc_account_trades_plan_begin (fixture.account, context);
    guint steps = 0;
    while (collector && gnc_account_trades_plan_get_state (collector) ==
           GNC_ACCOUNT_TRADES_PLAN_RUNNING && steps++ < 128)
        gnc_account_trades_plan_step (collector, 1);
    gboolean has_trades = TRUE;
    do_test (collector && gnc_account_trades_plan_get_state (collector) ==
             GNC_ACCOUNT_TRADES_PLAN_DONE && steps > 64 &&
             gnc_account_trades_plan_get_result (collector, &has_trades) &&
             !has_trades,
             "many non-trade splits require many bounded collector turns");
    gnc_account_trades_plan_free (collector);

    auto other_currency = gnc_commodity_new (
        fixture.book, "Other Trade Currency", "CURRENCY", "OTC", "", 100);
    add_lot_assignment_split_with_currency (
        &fixture, -5, 65 * 86400, other_currency);
    collector = gnc_account_trades_plan_begin (fixture.account, context);
    steps = 0;
    while (collector && gnc_account_trades_plan_get_state (collector) ==
           GNC_ACCOUNT_TRADES_PLAN_RUNNING && steps++ < 128)
        gnc_account_trades_plan_step (collector, 1);
    has_trades = FALSE;
    do_test (collector && gnc_account_trades_plan_get_state (collector) ==
             GNC_ACCOUNT_TRADES_PLAN_DONE && steps >= 65 &&
             gnc_account_trades_plan_get_result (collector, &has_trades) &&
             has_trades,
             "trade eligibility finds a late trade without a hidden full scan");
    gnc_account_trades_plan_free (collector);

    auto assign = gnc_split_assign_plan_begin (target, context);
    steps = 0;
    while (assign && gnc_split_assign_plan_step (assign, 1) ==
           GNC_SPLIT_ASSIGN_PLAN_RUNNING && steps++ < 512)
        ;
    do_test (assign && gnc_split_assign_plan_get_state (assign) ==
             GNC_SPLIT_ASSIGN_PLAN_DONE && steps > 64 && xaccSplitGetLot (target),
             "split assignment delegates trade eligibility over many step(1) turns");
    auto target_lot = xaccSplitGetLot (target);
    gnc_split_assign_plan_free (assign);

    auto generation = gnc_account_get_scrub_generation (fixture.account);
    xaccAccountSetType (fixture.account, xaccAccountGetType (fixture.account));
    xaccAccountSetCommodity (fixture.account,
                             xaccAccountGetCommodity (fixture.account));
    gnc_account_set_policy (fixture.account,
                            gnc_account_get_policy (fixture.account));
    auto target_transaction = xaccSplitGetParent (target);
    xaccTransSetDatePostedSecs (target_transaction,
                                xaccTransRetDatePosted (target_transaction));
    do_test (gnc_account_get_scrub_generation (fixture.account) == generation,
             "semantically equal selectors do not invalidate collectors");

    collector = gnc_account_trades_plan_begin (fixture.account, context);
    do_test (collector && gnc_account_trades_plan_step (collector, 1) ==
             GNC_ACCOUNT_TRADES_PLAN_RUNNING,
             "trade collector remains active before a no-op date setter");
    xaccTransSetDatePostedSecs (target_transaction,
                                xaccTransRetDatePosted (target_transaction));
    do_test (gnc_account_trades_plan_step (collector, 1) ==
             GNC_ACCOUNT_TRADES_PLAN_RUNNING,
             "no-op posted date does not invalidate an active collector");
    gnc_account_trades_plan_free (collector);

    collector = gnc_account_trades_plan_begin (fixture.account, context);
    gnc_account_trades_plan_step (collector, 1);
    GNCPolicy alternate_policy = *xaccGetFIFOPolicy ();
    gnc_account_set_policy (fixture.account, &alternate_policy);
    do_test (gnc_account_trades_plan_step (collector, 1) ==
             GNC_ACCOUNT_TRADES_PLAN_STALE,
             "policy mutation invalidates an active trade collector");
    gnc_account_trades_plan_free (collector);
    gnc_account_set_policy (fixture.account, xaccGetFIFOPolicy ());

    collector = gnc_account_trades_plan_begin (fixture.account, context);
    gnc_account_trades_plan_step (collector, 1);
    xaccAccountSetType (fixture.account, ACCT_TYPE_BANK);
    do_test (gnc_account_trades_plan_step (collector, 1) ==
             GNC_ACCOUNT_TRADES_PLAN_STALE,
             "account type mutation invalidates trade eligibility");
    gnc_account_trades_plan_free (collector);
    xaccAccountSetType (fixture.account, ACCT_TYPE_ASSET);

    auto lot_plan = gnc_lot_scrub_plan_begin (target_lot, context);
    do_test (lot_plan && gnc_lot_scrub_plan_step (lot_plan, 1) ==
             GNC_LOT_SCRUB_PLAN_RUNNING,
             "lot collector captures its generation before date mutation");
    xaccTransSetDatePostedSecs (
        target_transaction, xaccTransRetDatePosted (target_transaction) + 86400);
    do_test (lot_plan && gnc_lot_scrub_plan_step (lot_plan, 1) ==
             GNC_LOT_SCRUB_PLAN_STALE,
             "posted-date mutation centrally invalidates lot collectors");
    gnc_lot_scrub_plan_free (lot_plan);

    collector = gnc_account_trades_plan_begin (fixture.account, context);
    gnc_account_trades_plan_step (collector, 1);
    xaccTransSetCurrency (non_lot_transaction, other_currency);
    do_test (gnc_account_trades_plan_step (collector, 1) ==
             GNC_ACCOUNT_TRADES_PLAN_STALE,
             "transaction currency mutation invalidates trade eligibility");
    gnc_account_trades_plan_free (collector);

    auto root = gnc_account_get_root (fixture.account);
    auto empty = xaccMallocAccount (fixture.book);
    xaccAccountSetType (empty, ACCT_TYPE_ASSET);
    xaccAccountSetCommodity (empty, fixture.currency);
    gnc_account_append_child (root, empty);
    collector = gnc_account_trades_plan_begin (empty, context);
    xaccAccountSetCommodity (empty, other_currency);
    do_test (gnc_account_trades_plan_step (collector, 1) ==
             GNC_ACCOUNT_TRADES_PLAN_STALE,
             "account commodity mutation invalidates trade eligibility");
    gnc_account_trades_plan_free (collector);

    gnc_scrub_context_end (context);
    gnc_scrub_context_unref (context);
    gnc_clear_current_session ();
}

static gboolean
latest_policy_candidate_is_better (GNCPolicy *, Split *candidate,
                                   Split *current_best)
{
    return candidate && (!current_best ||
           xaccSplitOrderDateOnly (candidate, current_best) > 0);
}

static void
test_bounded_assignment_uses_account_policy (void)
{
    if (gnc_current_session_exist ())
        gnc_clear_current_session ();
    ScopedEnvironment lots_off {"GNC_AUTO_SCRUB_LOTS", nullptr};
    auto fixture = make_lot_assignment_fixture ();
    auto first = xaccTransFindSplitByAccount (fixture.first_transaction,
                                               fixture.account);
    auto second = xaccTransFindSplitByAccount (fixture.second_transaction,
                                                fixture.account);
    auto closing = xaccTransFindSplitByAccount (fixture.overflow_transaction,
                                                 fixture.account);
    xaccSplitAssign (first);
    xaccSplitAssign (second);
    auto latest_lot = xaccSplitGetLot (second);
    GNCPolicy latest_policy = *xaccGetFIFOPolicy ();
    latest_policy.PolicyCandidateIsBetter = latest_policy_candidate_is_better;
    gnc_account_set_policy (fixture.account, &latest_policy);

    gnc_set_current_session (fixture.session);
    auto context = gnc_scrub_context_begin (fixture.book);
    auto plan = gnc_split_assign_plan_begin (closing, context);
    guint steps = 0;
    while (plan && gnc_split_assign_plan_step (plan, 1) ==
           GNC_SPLIT_ASSIGN_PLAN_RUNNING && steps++ < 256)
        ;
    do_test (plan && gnc_split_assign_plan_get_state (plan) ==
             GNC_SPLIT_ASSIGN_PLAN_DONE && xaccSplitGetLot (closing) == latest_lot,
             "bounded lot selection honors the account policy comparator");
    gnc_split_assign_plan_free (plan);
    gnc_account_set_policy (fixture.account, xaccGetFIFOPolicy ());
    gnc_scrub_context_end (context);
    gnc_scrub_context_unref (context);
    gnc_clear_current_session ();
}

static void
test_account_lots_plan_completes_unassigned_zero_noop (void)
{
    if (gnc_current_session_exist ())
        gnc_clear_current_session ();
    ScopedEnvironment lots_off {"GNC_AUTO_SCRUB_LOTS", nullptr};
    auto fixture = make_lot_assignment_fixture ();
    auto zero = add_lot_assignment_split (&fixture, 0, 4 * 86400);
    gnc_set_current_session (fixture.session);
    auto context = gnc_scrub_context_begin (fixture.book);
    auto plan = gnc_account_lots_plan_begin (fixture.account, FALSE, context);
    guint steps = 0;
    auto state = GNC_ACCOUNT_LOTS_PLAN_RUNNING;
    while (plan && state == GNC_ACCOUNT_LOTS_PLAN_RUNNING && steps++ < 4096)
        state = gnc_account_lots_plan_step (plan, 1);
    do_test (plan && state == GNC_ACCOUNT_LOTS_PLAN_DONE && steps > 20,
             "account lots plan completes across bounded no-op and mutation turns");
    do_test (!xaccSplitGetLot (zero),
             "completed zero-amount no-op is not selected forever");
    gnc_account_lots_plan_free (plan);
    gnc_scrub_context_end (context);
    gnc_scrub_context_unref (context);
    gnc_clear_current_session ();
}

static void
test_lot_sort_relationship_and_rollback_generations (void)
{
    if (gnc_current_session_exist ())
        gnc_clear_current_session ();
    ScopedEnvironment lots_off {"GNC_AUTO_SCRUB_LOTS", nullptr};
    auto fixture = make_empty_lot_assignment_fixture ();
    auto first = add_lot_assignment_split (&fixture, 10, 86400);
    auto third = add_lot_assignment_split (&fixture, -5, 3 * 86400);
    auto second = add_lot_assignment_split (&fixture, -5, 2 * 86400);
    auto lot = gnc_lot_make_default (fixture.account);
    gnc_lot_add_split (lot, first);
    gnc_lot_add_split (lot, third);
    gnc_lot_add_split (lot, second);

    gnc_set_current_session (fixture.session);
    auto context = gnc_scrub_context_begin (fixture.book);
    auto stats = gnc_lot_stats_plan_begin (lot, context);
    do_test (stats && gnc_lot_stats_plan_step (stats, 1) ==
             GNC_LOT_STATS_RUNNING,
             "lot collector captures same-head unsorted order");
    auto generation = gnc_lot_get_scrub_generation (lot);
    do_test (gnc_lot_get_earliest_split (lot) == first &&
             gnc_lot_get_scrub_generation (lot) > generation,
             "same-head interior lot reorder advances generation");
    do_test (gnc_lot_stats_plan_step (stats, 1) == GNC_LOT_STATS_STALE,
             "same-head interior reorder invalidates active collector");
    gnc_lot_stats_plan_free (stats);

    generation = gnc_lot_get_scrub_generation (lot);
    gnc_lot_get_earliest_split (lot);
    do_test (gnc_lot_get_scrub_generation (lot) == generation,
             "already sorted lot does not spuriously advance generation");

    auto expect_stale = [&] (auto mutate, const char *message)
    {
        auto active = gnc_lot_stats_plan_begin (lot, context);
        do_test (active && gnc_lot_stats_plan_step (active, 1) ==
                 GNC_LOT_STATS_RUNNING, "start mutation-guarded lot collector");
        mutate ();
        do_test (active && gnc_lot_stats_plan_step (active, 1) ==
                 GNC_LOT_STATS_STALE, message);
        gnc_lot_stats_plan_free (active);
    };

    auto transaction = xaccSplitGetParent (third);
    expect_stale ([&] { xaccTransSetDateEnteredSecs (
                            transaction,
                            xaccTransRetDateEntered (transaction) + 1); },
                  "entered-date order mutation invalidates lot collectors");
    expect_stale ([&] { xaccTransSetNum (transaction, "generation-num"); },
                  "transaction number mutation invalidates lot collectors");
    expect_stale ([&] { xaccTransSetDescription (
                            transaction, "generation-description"); },
                  "description mutation invalidates lot collectors");
    expect_stale ([&] { xaccSplitSetAction (third, "generation-action"); },
                  "split action mutation invalidates lot collectors");
    expect_stale ([&] { xaccTransSetIsClosingTxn (transaction, TRUE); },
                  "closing-order mutation invalidates lot collectors");

    generation = gnc_lot_get_scrub_generation (lot);
    xaccTransSetDateEnteredSecs (transaction,
                                 xaccTransRetDateEntered (transaction));
    xaccTransSetNum (transaction, xaccTransGetNum (transaction));
    xaccTransSetDescription (transaction,
                             xaccTransGetDescription (transaction));
    xaccSplitSetAction (third, xaccSplitGetAction (third));
    xaccTransSetIsClosingTxn (transaction,
                              xaccTransGetIsClosingTxn (transaction));
    do_test (gnc_lot_get_scrub_generation (lot) == generation,
             "semantically equal order setters do not invalidate collectors");

    xaccTransBeginEdit (transaction);
    auto transaction_cursor = gnc_transaction_split_cursor_begin (transaction,
                                                                   context);
    do_test (transaction_cursor != nullptr,
             "transaction cursor starts after BeginEdit generation bump");
    xaccTransCommitEdit (transaction);
    GncGUID ignored_split;
    do_test (gnc_transaction_split_cursor_next (transaction_cursor,
                                                 &ignored_split) ==
             GNC_TRANSACTION_SPLIT_CURSOR_STALE,
             "no-op commit invalidates cursor before replacing split nodes");
    gnc_transaction_split_cursor_free (transaction_cursor);

    auto populated_entered = xaccSplitGetParent (
        add_lot_assignment_split (&fixture, 1, 4 * 86400));
    xaccTransBeginEdit (populated_entered);
    populated_entered->date_entered = 1;
    qof_instance_set_dirty (QOF_INSTANCE (populated_entered));
    auto populated_generation = populated_entered->split_list_generation;
    xaccTransCommitEdit (populated_entered);
    auto populated_delta = populated_entered->split_list_generation -
                           populated_generation;
    auto autofilled_entered = xaccSplitGetParent (
        add_lot_assignment_split (&fixture, 1, 5 * 86400));
    xaccTransBeginEdit (autofilled_entered);
    autofilled_entered->date_entered = 0;
    qof_instance_set_dirty (QOF_INSTANCE (autofilled_entered));
    auto autofilled_generation = autofilled_entered->split_list_generation;
    xaccTransCommitEdit (autofilled_entered);
    auto autofilled_delta = autofilled_entered->split_list_generation -
                            autofilled_generation;
    do_test (xaccTransRetDateEntered (autofilled_entered) != 0 &&
             autofilled_delta == populated_delta + 1,
             "commit date-entered autofill adds its own central invalidation");

    auto posted = xaccTransGetDatePostedGDate (transaction);
    generation = gnc_lot_get_scrub_generation (lot);
    TransactionModifyCounter modify_counter {transaction, 0};
    auto event_handler = qof_event_register_handler (
        count_transaction_modifies, &modify_counter);
    xaccTransSetDatePostedGDate (transaction, posted);
    do_test (gnc_lot_get_scrub_generation (lot) == generation &&
             modify_counter.count == 0,
             "equal posted scalar and GDate KVP are a semantic no-op");

    auto wrong_posted = posted;
    g_date_add_days (&wrong_posted, 1);
    xaccTransBeginEdit (transaction);
    qof_instance_set_path_kvp<GDate> (
        QOF_INSTANCE (transaction), wrong_posted, {TRANS_DATE_POSTED});
    qof_instance_set_dirty (QOF_INSTANCE (transaction));
    xaccTransCommitEdit (transaction);
    modify_counter.count = 0;
    stats = gnc_lot_stats_plan_begin (lot, context);
    do_test (stats && gnc_lot_stats_plan_step (stats, 1) ==
             GNC_LOT_STATS_RUNNING,
             "lot collector starts before KVP-only posted-date repair");
    xaccTransSetDatePostedGDate (transaction, posted);
    auto repaired = xaccTransGetDatePostedGDate (transaction);
    do_test (modify_counter.count == 1 &&
             qof_instance_get_editlevel (QOF_INSTANCE (transaction)) == 0 &&
             g_date_compare (&repaired, &posted) == 0,
             "KVP-only posted-date repair uses exactly one edit and commit");
    do_test (gnc_lot_stats_plan_step (stats, 1) == GNC_LOT_STATS_STALE,
             "KVP-only posted-date repair invalidates lot collectors");
    gnc_lot_stats_plan_free (stats);
    qof_event_unregister_handler (event_handler);

    xaccTransBeginEdit (transaction);
    stats = gnc_lot_stats_plan_begin (lot, context);
    gnc_lot_stats_plan_step (stats, 1);
    qof_instance_set (QOF_INSTANCE (third), "gains-split",
                      xaccSplitGetGUID (second), nullptr);
    do_test (qof_instance_is_dirty (QOF_INSTANCE (third)),
             "gains relationship mutation participates in rollback");
    do_test (gnc_lot_stats_plan_step (stats, 1) == GNC_LOT_STATS_STALE,
             "gains relationship mutation invalidates all endpoints");
    gnc_lot_stats_plan_free (stats);
    xaccTransRollbackEdit (transaction);
    do_test (xaccSplitGetCapGainsSplit (third) == nullptr,
             "rollback restores the gains relationship KVP");

    xaccTransBeginEdit (transaction);
    xaccTransSetNum (transaction, "rolled-back-number");
    stats = gnc_lot_stats_plan_begin (lot, context);
    gnc_lot_stats_plan_step (stats, 1);
    xaccTransRollbackEdit (transaction);
    do_test (gnc_lot_stats_plan_step (stats, 1) == GNC_LOT_STATS_STALE,
             "rollback invalidates collectors started on edited topology");
    gnc_lot_stats_plan_free (stats);

    auto cursor = gnc_account_lot_cursor_begin (fixture.account, context);
    do_test (cursor != nullptr, "private account lot cursor retains authority");
    gnc_scrub_context_cancel (context);
    GncGUID ignored;
    do_test (gnc_account_lot_cursor_next (cursor, &ignored) ==
             GNC_ACCOUNT_LOT_CURSOR_CANCELLED,
             "account lot cursor observes retained context cancellation");
    gnc_account_lot_cursor_free (cursor);
    gnc_scrub_context_end (context);
    gnc_scrub_context_unref (context);
    gnc_clear_current_session ();
}

static void
test_rollback_gains_relationship_endpoint_collectors (const char *property,
                                                       gboolean gains_split)
{
    if (gnc_current_session_exist ())
        gnc_clear_current_session ();
    ScopedEnvironment lots_off {"GNC_AUTO_SCRUB_LOTS", nullptr};
    auto fixture = make_empty_lot_assignment_fixture ();
    auto source = add_lot_assignment_split (&fixture, 10, 86400);
    auto old_endpoint = add_lot_assignment_split (&fixture, -3, 2 * 86400);
    auto new_endpoint = add_lot_assignment_split (&fixture, -4, 3 * 86400);
    auto source_filler = add_lot_assignment_split (&fixture, -1, 4 * 86400);
    auto old_filler = add_lot_assignment_split (&fixture, 1, 5 * 86400);
    auto new_filler = add_lot_assignment_split (&fixture, 1, 6 * 86400);
    auto source_lot = gnc_lot_make_default (fixture.account);
    auto old_lot = gnc_lot_make_default (fixture.account);
    auto new_lot = gnc_lot_make_default (fixture.account);
    gnc_lot_add_split (source_lot, source);
    gnc_lot_add_split (source_lot, source_filler);
    gnc_lot_add_split (old_lot, old_endpoint);
    gnc_lot_add_split (old_lot, old_filler);
    gnc_lot_add_split (new_lot, new_endpoint);
    gnc_lot_add_split (new_lot, new_filler);

    auto transaction = xaccSplitGetParent (source);
    xaccTransBeginEdit (transaction);
    qof_instance_set (QOF_INSTANCE (source), property,
                      xaccSplitGetGUID (old_endpoint), nullptr);
    xaccTransCommitEdit (transaction);

    gnc_set_current_session (fixture.session);
    auto context = gnc_scrub_context_begin (fixture.book);
    xaccTransBeginEdit (transaction);
    qof_instance_set (QOF_INSTANCE (source), property,
                      xaccSplitGetGUID (new_endpoint), nullptr);
    auto source_stats = gnc_lot_stats_plan_begin (source_lot, context);
    auto old_stats = gnc_lot_stats_plan_begin (old_lot, context);
    auto new_stats = gnc_lot_stats_plan_begin (new_lot, context);
    do_test (source_stats && old_stats && new_stats &&
             gnc_lot_stats_plan_step (source_stats, 1) ==
                 GNC_LOT_STATS_RUNNING &&
             gnc_lot_stats_plan_step (old_stats, 1) ==
                 GNC_LOT_STATS_RUNNING &&
             gnc_lot_stats_plan_step (new_stats, 1) ==
                 GNC_LOT_STATS_RUNNING,
             gains_split
                 ? "gains-split rollback collectors start on source, old and new lots"
                 : "gains-source rollback collectors start on source, old and new lots");

    source->gains = GAINS_STATUS_ADIRTY;
    source->gains_split = new_endpoint;
    old_endpoint->gains = GAINS_STATUS_ADIRTY;
    old_endpoint->gains_split = source;
    new_endpoint->gains = GAINS_STATUS_ADIRTY;
    new_endpoint->gains_split = source;
    xaccTransRollbackEdit (transaction);

    do_test (gnc_lot_stats_plan_step (source_stats, 1) ==
                 GNC_LOT_STATS_STALE &&
             gnc_lot_stats_plan_step (old_stats, 1) ==
                 GNC_LOT_STATS_STALE &&
             gnc_lot_stats_plan_step (new_stats, 1) ==
                 GNC_LOT_STATS_STALE,
             gains_split
                 ? "gains-split rollback invalidates source, removed and restored endpoints"
                 : "gains-source rollback invalidates source, removed and restored endpoints");
    auto restored = gains_split
        ? xaccSplitGetCapGainsSplit (source)
        : xaccSplitGetGainsSourceSplit (source);
    do_test (restored == old_endpoint,
             gains_split
                 ? "gains-split rollback restores the snapshot endpoint"
                 : "gains-source rollback restores the snapshot endpoint");
    do_test (source->gains == GAINS_STATUS_UNKNOWN &&
             source->gains_split == nullptr &&
             old_endpoint->gains == GAINS_STATUS_UNKNOWN &&
             old_endpoint->gains_split == nullptr &&
             new_endpoint->gains == GAINS_STATUS_UNKNOWN &&
             new_endpoint->gains_split == nullptr,
             gains_split
                 ? "gains-split rollback clears all endpoint runtime caches"
                 : "gains-source rollback clears all endpoint runtime caches");

    gnc_lot_stats_plan_free (source_stats);
    gnc_lot_stats_plan_free (old_stats);
    gnc_lot_stats_plan_free (new_stats);
    gnc_scrub_context_end (context);
    gnc_scrub_context_unref (context);
    gnc_clear_current_session ();
}

static void
test_rollback_gains_split_endpoint_collectors (void)
{
    test_rollback_gains_relationship_endpoint_collectors ("gains-split", TRUE);
}

static void
test_rollback_gains_source_endpoint_collectors (void)
{
    test_rollback_gains_relationship_endpoint_collectors ("gains-source", FALSE);
}

static void
test_lot_scrub_cancel_preserves_vdirty_terminal_marker (void)
{
    if (gnc_current_session_exist ())
        gnc_clear_current_session ();
    ScopedEnvironment lots_off {"GNC_AUTO_SCRUB_LOTS", nullptr};
    auto fixture = make_empty_lot_assignment_fixture ();
    auto opening = add_lot_assignment_split (&fixture, 10, 86400);
    auto closing = add_lot_assignment_split (&fixture, -10, 2 * 86400);
    auto lot = gnc_lot_make_default (fixture.account);
    gnc_lot_add_split (lot, opening);
    gnc_lot_add_split (lot, closing);
    opening->gains = GAINS_STATUS_VDIRTY;
    closing->gains = GAINS_STATUS_VDIRTY;

    gnc_set_current_session (fixture.session);
    auto context = gnc_scrub_context_begin (fixture.book);
    auto plan = gnc_lot_scrub_plan_begin (lot, context);
    guint steps = 0;
    while (plan && gnc_lot_scrub_plan_get_state (plan) ==
           GNC_LOT_SCRUB_PLAN_RUNNING &&
           (closing->gains & GAINS_STATUS_VDIRTY) && steps++ < 2048)
        gnc_lot_scrub_plan_step (plan, 1);
    do_test (plan && gnc_lot_scrub_plan_get_state (plan) ==
             GNC_LOT_SCRUB_PLAN_RUNNING && steps > 1 &&
             (opening->gains & GAINS_STATUS_VDIRTY) &&
             !(closing->gains & GAINS_STATUS_VDIRTY),
             "cap child completes while opening VDIRTY remains terminal marker");
    gnc_scrub_context_cancel (context);
    do_test (gnc_lot_scrub_plan_step (plan, 1) ==
             GNC_LOT_SCRUB_PLAN_CANCELLED &&
             (opening->gains & GAINS_STATUS_VDIRTY),
             "cancellation before FINALIZE preserves durable VDIRTY work");
    gnc_lot_scrub_plan_free (plan);
    gnc_scrub_context_end (context);
    gnc_scrub_context_unref (context);

    context = gnc_scrub_context_begin (fixture.book);
    plan = gnc_lot_scrub_plan_begin (lot, context);
    steps = 0;
    while (plan && gnc_lot_scrub_plan_get_state (plan) ==
           GNC_LOT_SCRUB_PLAN_RUNNING && steps++ < 4096)
        gnc_lot_scrub_plan_step (plan, 1);
    do_test (plan && gnc_lot_scrub_plan_get_state (plan) ==
             GNC_LOT_SCRUB_PLAN_DONE && steps > 1 &&
             !(opening->gains & (GAINS_STATUS_ADIRTY |
                                  GAINS_STATUS_VDIRTY)),
             "handoff reaches FINALIZE and clears marker only after proof");
    gnc_lot_scrub_plan_free (plan);
    gnc_scrub_context_end (context);
    gnc_scrub_context_unref (context);
    gnc_clear_current_session ();
}

static void
test_legacy_gains_scrub_uses_transaction_book (void)
{
    if (gnc_current_session_exist ())
        gnc_clear_current_session ();
    ScopedEnvironment lots_off {"GNC_AUTO_SCRUB_LOTS", nullptr};
    auto current = qof_session_new (qof_book_new ());
    auto foreign = make_empty_lot_assignment_fixture ();
    auto relationship = add_gains_relationship (&foreign, 86400, 2 * 86400);
    xaccSplitDetermineGainStatus (relationship.lot_split);
    relationship.lot_split->gains |= GAINS_STATUS_DATE_DIRTY;

    do_test (relationship.source->gains_split == nullptr &&
                 relationship.lot_split->gains_split == relationship.source,
             "loaded gains relationship may have a one-sided runtime cache");

    gnc_set_current_session (current);
    xaccTransScrubGains (relationship.gains_transaction, nullptr);

    do_test (gnc_get_current_session () == current,
             "legacy gains scrub preserves an unrelated current session");
    do_test (xaccTransRetDatePosted (relationship.gains_transaction) ==
                 xaccTransRetDatePosted (relationship.source_transaction) &&
             !(relationship.lot_split->gains & GAINS_STATUS_DATE_DIRTY),
             "legacy gains scrub synchronously processes a foreign book");

    gnc_clear_current_session ();
    destroy_lot_assignment_fixture (&foreign);
}

static void
test_book_shutdown_with_asymmetric_gains_cache (gboolean source_first)
{
    auto fixture = make_empty_lot_assignment_fixture ();
    auto source_date = source_first ? 2 * 86400 : 86400;
    auto gains_date = source_first ? 86400 : 2 * 86400;
    auto relationship = add_gains_relationship (&fixture, source_date,
                                                gains_date);

    xaccSplitDetermineGainStatus (relationship.lot_split);
    do_test (relationship.source->gains_split == nullptr &&
                 relationship.lot_split->gains_split == relationship.source,
             source_first
                 ? "source-first shutdown starts with a one-sided gains cache"
                 : "gains-first shutdown starts with a one-sided gains cache");

    destroy_lot_assignment_fixture (&fixture);
    success (source_first
                 ? "source-first shutdown does not follow freed gains caches"
                 : "gains-first shutdown handles one-sided gains caches");
}

static void
test_non_shutdown_split_destroy_clears_gains_caches (void)
{
    auto fixture = make_empty_lot_assignment_fixture ();
    auto relationship = add_gains_relationship (&fixture, 86400, 2 * 86400);

    xaccSplitDetermineGainStatus (relationship.source);
    xaccSplitDetermineGainStatus (relationship.lot_split);
    relationship.income_split->gains = GAINS_STATUS_GAINS;
    relationship.income_split->gains_split = relationship.source;
    do_test (relationship.source->gains_split == relationship.lot_split &&
                 relationship.lot_split->gains_split == relationship.source &&
                 relationship.income_split->gains_split == relationship.source,
             "normal split destroy starts with the complete gains cache");

    do_test (xaccSplitDestroy (relationship.source),
             "normal source split destroy succeeds");
    do_test (relationship.lot_split->gains_split == nullptr &&
                 relationship.income_split->gains_split == nullptr,
             "normal source split destroy clears both gains transaction caches");
    destroy_lot_assignment_fixture (&fixture);
}

static void
test_legacy_lot_scrub_uses_account_book (void)
{
    if (gnc_current_session_exist ())
        gnc_clear_current_session ();
    ScopedEnvironment lots_off {"GNC_AUTO_SCRUB_LOTS", nullptr};
    auto current = qof_session_new (qof_book_new ());
    auto foreign = make_lot_assignment_fixture ();

    gnc_set_current_session (current);
    xaccAccountScrubLots (foreign.account);
    auto lots = xaccAccountGetLotList (foreign.account);

    do_test (gnc_get_current_session () == current,
             "legacy lot scrub preserves an unrelated current session");
    do_test (lots != nullptr,
             "legacy lot scrub synchronously processes a foreign book");
    g_list_free (lots);

    gnc_clear_current_session ();
    destroy_lot_assignment_fixture (&foreign);
}

static void
test_lot_scrub_plan_external_mutation_is_stale (void)
{
    if (gnc_current_session_exist ())
        gnc_clear_current_session ();
    ScopedEnvironment lots_off {"GNC_AUTO_SCRUB_LOTS", nullptr};
    auto fixture = make_lot_assignment_fixture ();
    auto split = xaccTransFindSplitByAccount (fixture.first_transaction,
                                               fixture.account);
    xaccSplitAssign (split);
    auto lot = xaccSplitGetLot (split);
    gnc_set_current_session (fixture.session);
    auto context = gnc_scrub_context_begin (fixture.book);
    auto plan = gnc_lot_scrub_plan_begin (lot, context);
    do_test (plan != nullptr, "start generation-guarded lot scrub plan");
    do_test (gnc_lot_scrub_plan_step (plan, 1) ==
             GNC_LOT_SCRUB_PLAN_RUNNING,
             "first lot scrub step captures a bounded raw cursor");

    xaccTransBeginEdit (fixture.first_transaction);
    xaccSplitSetAmount (split, gnc_numeric_create (11, 1));
    do_test (gnc_lot_scrub_plan_step (plan, 1) ==
             GNC_LOT_SCRUB_PLAN_STALE,
             "external split mutation invalidates the lot cursor");
    xaccTransCommitEdit (fixture.first_transaction);

    gnc_lot_scrub_plan_free (plan);
    gnc_scrub_context_end (context);
    gnc_scrub_context_unref (context);
    gnc_clear_current_session ();
}

static guint
lot_scrub_terminal_fixture_steps (void)
{
    auto fixture = make_empty_lot_assignment_fixture ();
    auto opening = add_lot_assignment_split (&fixture, 10, 86400);
    auto closing = add_lot_assignment_split (&fixture, -10, 2 * 86400);
    auto lot = gnc_lot_make_default (fixture.account);
    gnc_lot_add_split (lot, opening);
    gnc_lot_add_split (lot, closing);
    opening->gains = GAINS_STATUS_VDIRTY;
    closing->gains = GAINS_STATUS_VDIRTY;
    gnc_set_current_session (fixture.session);
    auto context = gnc_scrub_context_begin (fixture.book);
    auto plan = gnc_lot_scrub_plan_begin (lot, context);
    guint steps = 0;
    while (plan && gnc_lot_scrub_plan_get_state (plan) ==
           GNC_LOT_SCRUB_PLAN_RUNNING && steps++ < 4096)
        gnc_lot_scrub_plan_step (plan, 1);
    auto done = plan && gnc_lot_scrub_plan_get_state (plan) ==
                        GNC_LOT_SCRUB_PLAN_DONE;
    gnc_lot_scrub_plan_free (plan);
    gnc_scrub_context_end (context);
    gnc_scrub_context_unref (context);
    gnc_clear_current_session ();
    return done ? steps : 0;
}

static void
test_lot_scrub_external_mutation_after_every_step_is_stale (void)
{
    if (gnc_current_session_exist ())
        gnc_clear_current_session ();
    ScopedEnvironment lots_off {"GNC_AUTO_SCRUB_LOTS", nullptr};
    auto terminal_steps = lot_scrub_terminal_fixture_steps ();
    do_test (terminal_steps > 3,
             "terminal lot scrub traverses multiple generation-guarded phases");

    for (guint offset = 1; offset < terminal_steps; ++offset)
    {
        auto fixture = make_empty_lot_assignment_fixture ();
        auto opening = add_lot_assignment_split (&fixture, 10, 86400);
        auto closing = add_lot_assignment_split (&fixture, -10, 2 * 86400);
        auto lot = gnc_lot_make_default (fixture.account);
        gnc_lot_add_split (lot, opening);
        gnc_lot_add_split (lot, closing);
        opening->gains = GAINS_STATUS_VDIRTY;
        closing->gains = GAINS_STATUS_VDIRTY;
        gnc_set_current_session (fixture.session);
        auto context = gnc_scrub_context_begin (fixture.book);
        auto plan = gnc_lot_scrub_plan_begin (lot, context);
        guint advanced = 0;
        while (plan && gnc_lot_scrub_plan_get_state (plan) ==
               GNC_LOT_SCRUB_PLAN_RUNNING && advanced++ < offset)
            gnc_lot_scrub_plan_step (plan, 1);
        if (!plan || gnc_lot_scrub_plan_get_state (plan) !=
                         GNC_LOT_SCRUB_PLAN_RUNNING)
        {
            auto message = g_strdup_printf (
                "lot scrub remains running before external mutation at offset %u",
                offset);
            do_test (FALSE, message);
            g_free (message);
        }
        else
        {
            auto description = g_strdup_printf ("external-drift-%u", offset);
            xaccTransSetDescription (xaccSplitGetParent (opening), description);
            g_free (description);
            auto stale_message = g_strdup_printf (
                "external mutation after step offset %u is stale", offset);
            do_test (gnc_lot_scrub_plan_step (plan, 1) ==
                     GNC_LOT_SCRUB_PLAN_STALE, stale_message);
            g_free (stale_message);
            auto dirty_message = g_strdup_printf (
                "external mutation at step offset %u preserves VDIRTY", offset);
            do_test (opening->gains & GAINS_STATUS_VDIRTY, dirty_message);
            g_free (dirty_message);
        }
        gnc_lot_scrub_plan_free (plan);
        gnc_scrub_context_end (context);
        gnc_scrub_context_unref (context);
        gnc_clear_current_session ();
    }
}

static void
test_composite_lots_job_cancel_handoff_and_drain (void)
{
    if (gnc_current_session_exist ())
        gnc_clear_current_session ();
    LotAssignmentFixture fixture {};
    {
        ScopedEnvironment lots_off {"GNC_AUTO_SCRUB_LOTS", nullptr};
        fixture = make_lot_assignment_fixture ();
    }
    gnc_set_current_session (fixture.session);
    xaccEnableDataScrubbing ();
    ScopedEnvironment lots_on {"GNC_AUTO_SCRUB_LOTS", "1"};
    auto job = gnc_scrub_lots_job_begin (fixture.account, FALSE);
    do_test (job != nullptr, "start composite lot and gains job");

    guint steps = 0;
    gboolean saw_deferred_head = FALSE;
    while (job && gnc_scrub_job_get_state (job) == GNC_SCRUB_JOB_RUNNING &&
           gnc_scrub_job_get_phase (job) == GNC_SCRUB_JOB_PHASE_LOTS &&
           steps++ < 512)
    {
        gnc_scrub_job_step (job, 1);
        if (gnc_scrub_job_get_total (job) >
            gnc_scrub_job_get_completed (job) + 1)
        {
            saw_deferred_head = TRUE;
            break;
        }
    }
    do_test (steps > 1 && saw_deferred_head,
             "step(1) exposes queued gains only after multiple bounded units");
    gnc_scrub_job_cancel (job);
    gnc_scrub_job_free (job);

    auto handoff = gnc_scrub_context_begin (fixture.book);
    GncGUID first_head;
    do_test (handoff && gnc_scrub_deferred_commit_pending_count (
                 handoff, GNC_SCRUB_DEFERRED_COMMIT_GAINS) > 0 &&
             gnc_scrub_deferred_commit_peek (
                 handoff, GNC_SCRUB_DEFERRED_COMMIT_GAINS, &first_head) &&
             guid_equal (&first_head,
                         xaccTransGetGUID (fixture.first_transaction)),
             "cancel preserves the unacknowledged FIFO head for handoff");
    gnc_scrub_context_end (handoff);
    gnc_scrub_context_unref (handoff);

    auto consumer = gnc_scrub_deferred_gains_job_begin (fixture.book);
    do_test (consumer != nullptr, "real gains consumer acquires handed-off FIFO");
    guint drain_steps = 0;
    while (consumer && gnc_scrub_job_get_state (consumer) ==
           GNC_SCRUB_JOB_RUNNING && drain_steps++ < 4096)
        gnc_scrub_job_step (consumer, 1);
    do_test (consumer && gnc_scrub_job_get_state (consumer) ==
             GNC_SCRUB_JOB_DONE && drain_steps > 1,
             "real gains consumer drains the FIFO over many step(1) turns");
    auto opening = xaccTransFindSplitByAccount (fixture.first_transaction,
                                                 fixture.account);
    do_test (opening && xaccSplitGetLot (opening) &&
             gnc_lot_get_earliest_split (xaccSplitGetLot (opening)) == opening,
             "drained head includes a fully checked no-op opening split");
    do_test (opening &&
             !(opening->gains & (GAINS_STATUS_ADIRTY |
                                  GAINS_STATUS_VDIRTY)),
             "no-op opening split completes its dirty state before FIFO ACK");
    gnc_scrub_job_free (consumer);

    auto verify = gnc_scrub_context_begin (fixture.book);
    do_test (verify && gnc_scrub_deferred_commit_pending_count (
                 verify, GNC_SCRUB_DEFERRED_COMMIT_GAINS) == 0,
             "consumer acknowledges every head only after completion");
    gnc_scrub_context_end (verify);
    gnc_scrub_context_unref (verify);
    gnc_clear_current_session ();
}

static void
test_composite_lots_job_matches_legacy_multicommodity_gain (void)
{
    if (gnc_current_session_exist ())
        gnc_clear_current_session ();
    LotAssignmentFixture incremental {}, synchronous {};
    {
        ScopedEnvironment lots_off {"GNC_AUTO_SCRUB_LOTS", nullptr};
        incremental = make_lot_assignment_fixture ();
        synchronous = make_lot_assignment_fixture ();
        set_overflow_value (&incremental, -50);
        set_overflow_value (&synchronous, -50);
        if (gnc_current_session_exist ())
            gnc_clear_current_session ();
        do_test (!gnc_current_session_exist (),
                 "legacy comparison runs without a current session");
        xaccAccountScrubLots (synchronous.account);
    }

    gnc_set_current_session (incremental.session);
    xaccEnableDataScrubbing ();
    ScopedEnvironment lots_on {"GNC_AUTO_SCRUB_LOTS", "1"};
    auto job = gnc_scrub_lots_job_begin (incremental.account, FALSE);
    guint steps = 0;
    while (job && gnc_scrub_job_get_state (job) == GNC_SCRUB_JOB_RUNNING &&
           steps++ < 8192)
        gnc_scrub_job_step (job, 1);
    do_test (job && gnc_scrub_job_get_state (job) == GNC_SCRUB_JOB_DONE &&
             steps > 20,
             "multi-commodity gain job requires many bounded step(1) turns");
    do_test (lot_assignment_matches (incremental.account,
                                     synchronous.account),
             "composite job matches legacy FIFO amounts and values");
    do_test (count_nonzero_gain_splits (incremental.account) ==
             count_nonzero_gain_splits (synchronous.account) &&
             count_nonzero_gain_splits (incremental.account) > 0,
             "composite job matches legacy realized gain splits");
    gnc_scrub_job_free (job);

    auto verify = gnc_scrub_context_begin (incremental.book);
    do_test (verify && gnc_scrub_deferred_commit_pending_count (
                 verify, GNC_SCRUB_DEFERRED_COMMIT_GAINS) == 0,
             "successful composite job leaves no deferred gains work");
    gnc_scrub_context_end (verify);
    gnc_scrub_context_unref (verify);
    gnc_clear_current_session ();
    destroy_lot_assignment_fixture (&synchronous);
}

static void
run_test (void)
{
    QofSession *sess;
    QofBook *book;
    Account *root;

    /* --------------------------------------------------------- */
    /* In the first test, we will merely try to see if we can run
     * without crashing.  We don't check to see if data is good. */
    sess = get_random_session ();
    book = qof_session_get_book (sess);
    do_test ((NULL != book), "create random data");

    add_random_transactions_to_book (book, transaction_num);

    root = gnc_book_get_root_account (book);
    xaccAccountTreeScrubLots (root);

    /* --------------------------------------------------------- */
    /* In the second test, we create an account with unrealized gains,
     * and see if that gets fixed correctly, with the correct balances,
     * and etc.
     * XXX not implemented
     */
    success ("automatic lot scrubbing lightly tested and seem to work");
    qof_session_destroy (sess);

}

int
main (int argc, char **argv)
{
    gint i;

    qof_init();
    if (!cashobjects_register())
        exit(1);

    /* Any tests that cause an error or warning to be printed
     * automatically fail! */
    g_log_set_always_fatal((GLogLevelFlags)(G_LOG_LEVEL_CRITICAL | G_LOG_LEVEL_WARNING));
    /* Set up a reproducible test-case */
    srand(0);
    /* Iterate the test a number of times */
    for (i = 0; i < max_iterate; i++)
    {
        fprintf(stdout, " Lots: %d of %d paired tests . . . \r",
                (i + 1) * 2, max_iterate * 2);
        fflush(stdout);
        run_test ();
    }

    test_lot_kvp ();
    test_incremental_lot_assignment ();
    test_incremental_lot_assignment_work_budget ();
    test_incremental_lot_assignment_cancel_and_stale ();
    test_incremental_lot_assignment_defers_auto_gains ();
    test_account_trades_collector_is_bounded_and_generation_safe ();
    test_bounded_assignment_uses_account_policy ();
    test_account_lots_plan_completes_unassigned_zero_noop ();
    test_lot_sort_relationship_and_rollback_generations ();
    test_rollback_gains_split_endpoint_collectors ();
    test_rollback_gains_source_endpoint_collectors ();
    test_lot_scrub_cancel_preserves_vdirty_terminal_marker ();
    test_legacy_gains_scrub_uses_transaction_book ();
    test_book_shutdown_with_asymmetric_gains_cache (TRUE);
    test_book_shutdown_with_asymmetric_gains_cache (FALSE);
    test_non_shutdown_split_destroy_clears_gains_caches ();
    test_legacy_lot_scrub_uses_account_book ();
    test_lot_scrub_plan_external_mutation_is_stale ();
    test_lot_scrub_external_mutation_after_every_step_is_stale ();
    test_composite_lots_job_cancel_handoff_and_drain ();
    test_composite_lots_job_matches_legacy_multicommodity_gain ();

    /* 'erase' the recurring tag line with dummy spaces. */
    fprintf(stdout, "Lots: Test series complete.\n");
    fflush(stdout);
    print_test_results();

    qof_close();
    return get_rv();
}
