/* Copyright (C) 2026 GnuCash contributors
 * SPDX-License-Identifier: GPL-2.0-or-later
 */
#include <config.h>
#include <gtest/gtest-spi.h>
#include "test-logging.hpp"

TEST (TestLogging, WarningIsDiagnosticAndDoesNotFailTheCase)
{
    g_log ("logging-test", G_LOG_LEVEL_WARNING, "diagnostic warning");
    EXPECT_FALSE (::testing::Test::HasFailure ());
}

TEST (TestLogging, ExpectedWarningAllowsTheOperationToFinish)
{
    g_test_expect_message ("logging-test", G_LOG_LEVEL_WARNING, "expected warning");
    g_log ("logging-test", G_LOG_LEVEL_WARNING, "expected warning");
    g_test_assert_expected_messages ();
}

TEST (TestLogging, ExpectedCriticalAllowsTheOperationToFinish)
{
    g_test_expect_message ("logging-test", G_LOG_LEVEL_CRITICAL, "expected critical");
    g_log ("logging-test", G_LOG_LEVEL_CRITICAL, "expected critical");
    g_test_assert_expected_messages ();
}

TEST (TestLogging, UnexpectedCriticalFailsTheCaseWithoutAborting)
{
    EXPECT_NONFATAL_FAILURE (
        g_log ("logging-test", G_LOG_LEVEL_CRITICAL, "unexpected critical"),
        "unexpected critical");
}

TEST (TestLogging, StructuredWarningIsDiagnostic)
{
    g_log_structured ("logging-test", G_LOG_LEVEL_WARNING,
                      "MESSAGE", "structured diagnostic warning");
    EXPECT_FALSE (::testing::Test::HasFailure ());
}

TEST (TestLogging, UnexpectedStructuredCriticalFailsTheCaseWithoutAborting)
{
    EXPECT_NONFATAL_FAILURE (
        g_log_structured ("logging-test", G_LOG_LEVEL_CRITICAL,
                          "MESSAGE", "structured unexpected critical"),
        "structured unexpected critical");
}

int main (int argc, char **argv)
{
    ::testing::InitGoogleTest (&argc, argv);
    gnc::test::initialize_logging ();
    return RUN_ALL_TESTS ();
}
