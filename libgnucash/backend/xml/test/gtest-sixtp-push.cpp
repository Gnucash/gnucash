/*
 * This program is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 */

#include <glib.h>
#include <config.h>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcpp"
#include <gtest/gtest.h>
#pragma GCC diagnostic pop

#include "sixtp.h"
#include "sixtp-utils.h"

namespace
{
struct PushPlanCounts
{
    guint end_calls {0};
    guint fail_calls {0};
};

gboolean
count_end (gpointer children, GSList* results, GSList* siblings,
           gpointer parent, gpointer global_data, gpointer* result,
           const gchar* tag)
{
    auto counts = static_cast<PushPlanCounts*> (global_data);
    ++counts->end_calls;
    if (result)
        *result = counts;
    return TRUE;
}

void
count_failure (gpointer children, GSList* results, GSList* siblings,
               gpointer parent, gpointer global_data, gpointer* result,
               const gchar* tag)
{
    ++static_cast<PushPlanCounts*> (global_data)->fail_calls;
}

sixtp*
push_test_parser ()
{
    auto top = sixtp_new ();
    auto root = sixtp_new ();
    g_assert_nonnull (top);
    g_assert_nonnull (root);
    sixtp_set_chars (top, allow_and_ignore_only_whitespace);
    sixtp_set_end (top, count_end);
    sixtp_set_fail (top, count_failure);
    sixtp_set_chars (root, allow_and_ignore_only_whitespace);
    sixtp_set_fail (root, count_failure);
    g_assert_true (sixtp_add_sub_parser (top, "root", root));
    return top;
}

struct ReentrantPlanState
{
    sixtp_push_plan *plan {nullptr};
    sixtp_push_plan_status cancel_status {SIXTP_PUSH_PLAN_ACTIVE};
    guint callbacks {0};
};

gboolean
cancel_from_sax (GSList *siblings, gpointer parent, gpointer global_data,
                 gpointer *result, const char *text, int length)
{
    auto state = static_cast<ReentrantPlanState *> (global_data);
    ++state->callbacks;
    state->cancel_status = sixtp_push_plan_cancel (state->plan);
    return TRUE;
}

gboolean
free_from_sax (GSList *siblings, gpointer parent, gpointer global_data,
               gpointer *result, const char *text, int length)
{
    auto state = static_cast<ReentrantPlanState *> (global_data);
    ++state->callbacks;
    sixtp_push_plan_free (state->plan);
    state->plan = nullptr;
    return TRUE;
}

gboolean
cancel_from_top_end (gpointer children, GSList *results, GSList *siblings,
                     gpointer parent, gpointer global_data, gpointer *result,
                     const gchar *tag)
{
    auto state = static_cast<ReentrantPlanState *> (global_data);
    ++state->callbacks;
    state->cancel_status = sixtp_push_plan_cancel (state->plan);
    return TRUE;
}

gboolean
free_from_top_end (gpointer children, GSList *results, GSList *siblings,
                   gpointer parent, gpointer global_data, gpointer *result,
                   const gchar *tag)
{
    auto state = static_cast<ReentrantPlanState *> (global_data);
    ++state->callbacks;
    sixtp_push_plan_free (state->plan);
    state->plan = nullptr;
    return TRUE;
}

void
cancel_from_failure (gpointer children, GSList *results, GSList *siblings,
                     gpointer parent, gpointer global_data, gpointer *result,
                     const gchar *tag)
{
    auto state = static_cast<ReentrantPlanState *> (global_data);
    ++state->callbacks;
    state->cancel_status = sixtp_push_plan_cancel (state->plan);
}

void
free_from_failure (gpointer children, GSList *results, GSList *siblings,
                   gpointer parent, gpointer global_data, gpointer *result,
                   const gchar *tag)
{
    auto state = static_cast<ReentrantPlanState *> (global_data);
    ++state->callbacks;
    sixtp_push_plan_free (state->plan);
    state->plan = nullptr;
}

sixtp *
reentrant_push_test_parser (sixtp_characters_handler chars,
                            sixtp_end_handler end,
                            sixtp_fail_handler failure = nullptr)
{
    auto top = sixtp_new ();
    auto root = sixtp_new ();
    g_assert_nonnull (top);
    g_assert_nonnull (root);
    sixtp_set_chars (top, allow_and_ignore_only_whitespace);
    sixtp_set_end (top, end);
    sixtp_set_chars (root, chars);
    if (failure)
        sixtp_set_fail (root, failure);
    g_assert_true (sixtp_add_sub_parser (top, "root", root));
    return top;
}

TEST(SixtpPushPlan, ValidInputMayBeSplitAcrossChunks)
{
    PushPlanCounts counts;
    auto plan = sixtp_push_plan_new (push_test_parser (), NULL, &counts);
    gpointer result = NULL;
    ASSERT_NE (plan, nullptr);
    EXPECT_EQ (sixtp_push_plan_feed (plan, "<ro", 3), SIXTP_PUSH_PLAN_ACTIVE);
    EXPECT_EQ (sixtp_push_plan_feed (plan, "ot", 2), SIXTP_PUSH_PLAN_ACTIVE);
    EXPECT_EQ (sixtp_push_plan_feed (plan, "/>", 2), SIXTP_PUSH_PLAN_ACTIVE);
    EXPECT_EQ (sixtp_push_plan_finish (plan, &result), SIXTP_PUSH_PLAN_FINISHED);
    EXPECT_EQ (result, &counts);
    EXPECT_EQ (counts.end_calls, 1u);
    EXPECT_EQ (counts.fail_calls, 0u);
    sixtp_push_plan_free (plan);
}

TEST(SixtpPushPlan, ZeroLengthFeedIsOneLegalChunk)
{
    PushPlanCounts counts;
    auto plan = sixtp_push_plan_new (push_test_parser (), NULL, &counts);
    ASSERT_NE (plan, nullptr);
    EXPECT_EQ (sixtp_push_plan_feed (plan, NULL, 0), SIXTP_PUSH_PLAN_ACTIVE);
    EXPECT_EQ (sixtp_push_plan_feed (plan, "<root/>", 7), SIXTP_PUSH_PLAN_ACTIVE);
    EXPECT_EQ (sixtp_push_plan_finish (plan, NULL), SIXTP_PUSH_PLAN_FINISHED);
    EXPECT_EQ (counts.end_calls, 1u);
    sixtp_push_plan_free (plan);
}

TEST(SixtpPushPlan, MalformedInputFailsAndFinalizesOnce)
{
    PushPlanCounts counts;
    auto plan = sixtp_push_plan_new (push_test_parser (), NULL, &counts);
    gpointer result = &counts;
    ASSERT_NE (plan, nullptr);
    EXPECT_EQ (sixtp_push_plan_feed (plan, "<root>", 6), SIXTP_PUSH_PLAN_ACTIVE);
    g_test_expect_message ("gnc.backend.file.sixtp", G_LOG_LEVEL_CRITICAL,
                           "parse failed at:");
    EXPECT_EQ (sixtp_push_plan_finish (plan, &result), SIXTP_PUSH_PLAN_ERROR);
    g_test_assert_expected_messages ();
    EXPECT_EQ (result, nullptr);
    EXPECT_EQ (counts.end_calls, 1u);
    EXPECT_GT (counts.fail_calls, 0u);
    auto failures = counts.fail_calls;
    EXPECT_EQ (sixtp_push_plan_finish (plan, NULL), SIXTP_PUSH_PLAN_ERROR);
    EXPECT_EQ (sixtp_push_plan_feed (plan, "", 0), SIXTP_PUSH_PLAN_ERROR);
    EXPECT_EQ (counts.end_calls, 1u);
    EXPECT_EQ (counts.fail_calls, failures);
    sixtp_push_plan_free (plan);
}

TEST(SixtpPushPlan, CancelCleansActiveStackOnlyOnce)
{
    PushPlanCounts counts;
    auto plan = sixtp_push_plan_new (push_test_parser (), NULL, &counts);
    gpointer result = &counts;
    ASSERT_NE (plan, nullptr);
    EXPECT_EQ (sixtp_push_plan_feed (plan, "<root>", 6), SIXTP_PUSH_PLAN_ACTIVE);
    EXPECT_EQ (sixtp_push_plan_cancel (plan), SIXTP_PUSH_PLAN_CANCELLED);
    EXPECT_GT (counts.fail_calls, 0u);
    auto failures = counts.fail_calls;
    EXPECT_EQ (sixtp_push_plan_cancel (plan), SIXTP_PUSH_PLAN_CANCELLED);
    EXPECT_EQ (sixtp_push_plan_feed (plan, "</root>", 7), SIXTP_PUSH_PLAN_CANCELLED);
    EXPECT_EQ (sixtp_push_plan_finish (plan, &result), SIXTP_PUSH_PLAN_CANCELLED);
    EXPECT_EQ (result, nullptr);
    EXPECT_EQ (counts.end_calls, 0u);
    EXPECT_EQ (counts.fail_calls, failures);
    sixtp_push_plan_free (plan);
}

TEST(SixtpPushPlan, DoubleFinishAndCancelAreTerminalNoOps)
{
    PushPlanCounts counts;
    auto plan = sixtp_push_plan_new (push_test_parser (), NULL, &counts);
    ASSERT_NE (plan, nullptr);
    EXPECT_EQ (sixtp_push_plan_feed (plan, "<root/>", 7), SIXTP_PUSH_PLAN_ACTIVE);
    EXPECT_EQ (sixtp_push_plan_finish (plan, NULL), SIXTP_PUSH_PLAN_FINISHED);
    EXPECT_EQ (sixtp_push_plan_finish (plan, NULL), SIXTP_PUSH_PLAN_FINISHED);
    EXPECT_EQ (sixtp_push_plan_cancel (plan), SIXTP_PUSH_PLAN_FINISHED);
    EXPECT_EQ (counts.end_calls, 1u);
    EXPECT_EQ (counts.fail_calls, 0u);
    sixtp_push_plan_free (plan);
}

TEST(SixtpPushPlan, CancelFromSaxHandlerDefersTerminalization)
{
    ReentrantPlanState state;
    auto plan = sixtp_push_plan_new (
        reentrant_push_test_parser (cancel_from_sax, nullptr), NULL, &state);
    ASSERT_NE (plan, nullptr);
    state.plan = plan;

    EXPECT_EQ (sixtp_push_plan_feed (plan, "<root>x</root>", 14),
               SIXTP_PUSH_PLAN_CANCELLED);
    EXPECT_EQ (state.cancel_status, SIXTP_PUSH_PLAN_CANCELLED);
    EXPECT_EQ (state.callbacks, 1u);
    sixtp_push_plan_free (plan);
}

TEST(SixtpPushPlan, CancelFromTopEndHandlerDefersTerminalization)
{
    ReentrantPlanState state;
    auto plan = sixtp_push_plan_new (
        reentrant_push_test_parser (allow_and_ignore_only_whitespace,
                                    cancel_from_top_end), NULL, &state);
    gpointer result = &state;
    ASSERT_NE (plan, nullptr);
    state.plan = plan;
    EXPECT_EQ (sixtp_push_plan_feed (plan, "<root/>", 7),
               SIXTP_PUSH_PLAN_ACTIVE);

    EXPECT_EQ (sixtp_push_plan_finish (plan, &result),
               SIXTP_PUSH_PLAN_CANCELLED);
    EXPECT_EQ (result, nullptr);
    EXPECT_EQ (state.cancel_status, SIXTP_PUSH_PLAN_CANCELLED);
    EXPECT_EQ (state.callbacks, 1u);
    sixtp_push_plan_free (plan);
}

TEST(SixtpPushPlan, FreeFromTopEndHandlerDefersDestructionUntilFinishReturns)
{
    ReentrantPlanState state;
    auto plan = sixtp_push_plan_new (
        reentrant_push_test_parser (allow_and_ignore_only_whitespace,
                                    free_from_top_end), NULL, &state);
    ASSERT_NE (plan, nullptr);
    state.plan = plan;
    EXPECT_EQ (sixtp_push_plan_feed (plan, "<root/>", 7),
               SIXTP_PUSH_PLAN_ACTIVE);

    EXPECT_EQ (sixtp_push_plan_finish (plan, nullptr),
               SIXTP_PUSH_PLAN_CANCELLED);
    EXPECT_EQ (state.plan, nullptr);
    EXPECT_EQ (state.callbacks, 1u);
}

TEST(SixtpPushPlan, FreeFromSaxHandlerDefersDestructionUntilFeedReturns)
{
    ReentrantPlanState state;
    auto plan = sixtp_push_plan_new (
        reentrant_push_test_parser (free_from_sax, nullptr), NULL, &state);
    ASSERT_NE (plan, nullptr);
    state.plan = plan;

    EXPECT_EQ (sixtp_push_plan_feed (plan, "<root>x</root>", 14),
               SIXTP_PUSH_PLAN_CANCELLED);
    EXPECT_EQ (state.plan, nullptr);
    EXPECT_EQ (state.callbacks, 1u);
}

TEST(SixtpPushPlan, CancelFromFailureHandlerDefersTerminalization)
{
    ReentrantPlanState state;
    auto plan = sixtp_push_plan_new (
        reentrant_push_test_parser (allow_and_ignore_only_whitespace, nullptr,
                                    cancel_from_failure), NULL, &state);
    ASSERT_NE (plan, nullptr);
    state.plan = plan;
    EXPECT_EQ (sixtp_push_plan_feed (plan, "<root>", 6),
               SIXTP_PUSH_PLAN_ACTIVE);

    g_test_expect_message ("gnc.backend.file.sixtp", G_LOG_LEVEL_CRITICAL,
                           "parse failed at:");
    EXPECT_EQ (sixtp_push_plan_finish (plan, nullptr),
               SIXTP_PUSH_PLAN_ERROR);
    g_test_assert_expected_messages ();
    EXPECT_EQ (state.cancel_status, SIXTP_PUSH_PLAN_ERROR);
    EXPECT_EQ (state.callbacks, 1u);
    sixtp_push_plan_free (plan);
}

TEST(SixtpPushPlan, FreeFromFailureHandlerDefersDestructionUntilFinishReturns)
{
    ReentrantPlanState state;
    auto plan = sixtp_push_plan_new (
        reentrant_push_test_parser (allow_and_ignore_only_whitespace, nullptr,
                                    free_from_failure), NULL, &state);
    ASSERT_NE (plan, nullptr);
    state.plan = plan;
    EXPECT_EQ (sixtp_push_plan_feed (plan, "<root>", 6),
               SIXTP_PUSH_PLAN_ACTIVE);

    g_test_expect_message ("gnc.backend.file.sixtp", G_LOG_LEVEL_CRITICAL,
                           "parse failed at:");
    EXPECT_EQ (sixtp_push_plan_finish (plan, nullptr),
               SIXTP_PUSH_PLAN_ERROR);
    g_test_assert_expected_messages ();
    EXPECT_EQ (state.plan, nullptr);
    EXPECT_EQ (state.callbacks, 1u);
}
} // namespace
