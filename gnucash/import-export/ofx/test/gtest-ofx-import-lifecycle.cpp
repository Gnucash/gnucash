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

/********************************************************************
 * gtest-ofx-import-lifecycle.cpp -- real OFX workflow lifecycle    *
 ********************************************************************/
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcpp"
#include <gtest/gtest.h>
#pragma GCC diagnostic pop

#include <config.h>

#include "gnc-ofx-import-test-seam.h"
#include "gnc-session.h"
#include "gnc-ui-util.h"
#include <gtk/gtk.h>

class OfxImportLifecycleTest : public ::testing::Test
{
protected:
    static void SetUpTestSuite ()
    {
        gtk_init ();
        ASSERT_TRUE (gtk_is_initialized ());
        g_log_set_always_fatal (static_cast<GLogLevelFlags> (
            G_LOG_FATAL_MASK | G_LOG_LEVEL_CRITICAL));
    }

    OfxImportLifecycleTest () : m_book {gnc_get_current_book ()}
    {
    }

    ~OfxImportLifecycleTest () override
    {
        if (gnc_current_session_exist ())
            gnc_clear_current_session ();
    }

    QofBook *m_book;
};

static void
run_parent_abort_with_async_state (gboolean commodity)
{
    auto application = g_application_new (nullptr, G_APPLICATION_NON_UNIQUE);
    auto seam = gnc_ofx_import_test_seam_new (application);
    ASSERT_NE (seam, nullptr);
    ASSERT_TRUE (commodity
                     ? gnc_ofx_import_test_begin_commodity_state (seam)
                     : gnc_ofx_import_test_begin_account_state (seam));

    gnc_ofx_import_test_parent_destroy (seam);

    EXPECT_EQ (gnc_ofx_import_test_metadata_cleanup_calls (seam), 1u);
    EXPECT_EQ (gnc_ofx_import_test_payload_destroy_calls (seam), 0u);
    EXPECT_EQ (gnc_ofx_import_test_cleanup_result (seam),
               GNC_IMPORT_OPERATION_TEARDOWN_MUTATION_ALLOWED);

    if (commodity)
        gnc_ofx_import_test_complete_commodity_cancel (seam);
    else
        gnc_ofx_import_test_complete_account_cancel (seam);

    EXPECT_EQ (gnc_ofx_import_test_metadata_cleanup_calls (seam), 1u);
    EXPECT_EQ (gnc_ofx_import_test_payload_destroy_calls (seam), 1u);
    gnc_ofx_import_test_seam_free (seam);
    g_object_unref (application);
}

TEST_F(OfxImportLifecycleTest, parent_abort_during_real_account_async_state)
{
    ASSERT_NE (m_book, nullptr);
    run_parent_abort_with_async_state (FALSE);
}

TEST_F(OfxImportLifecycleTest, parent_abort_during_real_commodity_async_state)
{
    ASSERT_NE (m_book, nullptr);
    run_parent_abort_with_async_state (TRUE);
}

TEST_F(OfxImportLifecycleTest,
       parent_abort_cleans_real_slots_and_disconnects_reconcile_destroy)
{
    ASSERT_NE (m_book, nullptr);
    auto application = g_application_new (nullptr, G_APPLICATION_NON_UNIQUE);
    auto seam = gnc_ofx_import_test_seam_new (application);
    ASSERT_NE (seam, nullptr);
    ASSERT_TRUE (gnc_ofx_import_test_create_matcher (seam));
    ASSERT_TRUE (gnc_ofx_import_test_add_open_transaction (seam));
    auto reconcile = gnc_ofx_import_test_attach_reconcile (seam);
    ASSERT_NE (reconcile, nullptr);

    gnc_ofx_import_test_parent_destroy (seam);

    EXPECT_EQ (gnc_ofx_import_test_metadata_cleanup_calls (seam), 1u);
    EXPECT_EQ (gnc_ofx_import_test_payload_destroy_calls (seam), 1u);
    EXPECT_EQ (gnc_ofx_import_test_reconcile_calls (seam), 0u);
    EXPECT_EQ (gnc_ofx_import_test_cleanup_result (seam),
               GNC_IMPORT_OPERATION_TEARDOWN_MUTATION_ALLOWED);

    gtk_window_destroy (reconcile);
    EXPECT_EQ (gnc_ofx_import_test_reconcile_calls (seam), 0u);
    EXPECT_EQ (gnc_ofx_import_test_payload_destroy_calls (seam), 1u);
    g_object_unref (reconcile);
    gnc_ofx_import_test_seam_free (seam);
    g_object_unref (application);
}
