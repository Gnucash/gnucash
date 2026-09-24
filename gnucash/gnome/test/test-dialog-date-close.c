/*
 * test-dialog-date-close.c -- async date dialog contract tests
 *
 * Copyright (C) 2026
 *
 * This program is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 */

#include <config.h>

#include <glib.h>

#include "dialog-date-close.h"

typedef enum
{
    DATE_CLOSE,
    DATE_ACCOUNT,
    DATES_ACCOUNT_QUESTION
} DateCloseTestKind;

typedef struct
{
    GMainLoop *loop;
    DateCloseTestKind kind;
    GError *error;
} DateCloseTest;

static void
date_close_invalid_finished (GObject *source, GAsyncResult *result,
                             gpointer user_data)
{
    DateCloseTest *test = user_data;
    gboolean accepted;

    (void)source;
    switch (test->kind)
    {
    case DATE_CLOSE:
    {
        time64 date;

        accepted = gnc_dialog_date_close_parented_finish (result, &date,
                                                           &test->error);
        break;
    }
    case DATE_ACCOUNT:
    {
        time64 date;
        Account *account;

        accepted = gnc_dialog_date_acct_parented_finish (result, &date,
                                                          &account,
                                                          &test->error);
        break;
    }
    case DATES_ACCOUNT_QUESTION:
    {
        time64 due_date;
        time64 post_date;
        char *memo = NULL;
        Account *account;
        gboolean answer;

        accepted = gnc_dialog_dates_acct_question_parented_finish (
            result, &due_date, &post_date, &memo, &account, &answer,
            &test->error);
        g_free (memo);
        break;
    }
    default:
        g_assert_not_reached ();
    }

    g_assert_false (accepted);
    g_main_loop_quit (test->loop);
}

static void
date_close_test_clear (DateCloseTest *test)
{
    g_clear_error (&test->error);
    g_main_loop_unref (test->loop);
}

static void
test_date_close_rejects_incomplete_request (void)
{
    DateCloseTest test = { 0 };

    test.loop = g_main_loop_new (NULL, FALSE);
    test.kind = DATE_CLOSE;
    gnc_dialog_date_close_parented_async (
        NULL, NULL, "Close Date", FALSE, 0, NULL,
        date_close_invalid_finished, &test);
    g_main_loop_run (test.loop);

    g_assert_error (test.error, G_IO_ERROR, G_IO_ERROR_INVALID_ARGUMENT);
    date_close_test_clear (&test);
}

static void
test_date_close_reports_cancelled_request (void)
{
    DateCloseTest test = { 0 };
    GCancellable *cancellable = g_cancellable_new ();

    test.loop = g_main_loop_new (NULL, FALSE);
    test.kind = DATE_CLOSE;
    g_cancellable_cancel (cancellable);
    gnc_dialog_date_close_parented_async (
        NULL, "Close order", "Close Date", FALSE, 0, cancellable,
        date_close_invalid_finished, &test);
    g_main_loop_run (test.loop);

    g_assert_error (test.error, G_IO_ERROR, G_IO_ERROR_CANCELLED);
    g_object_unref (cancellable);
    date_close_test_clear (&test);
}

static void
test_date_account_rejects_incomplete_request (void)
{
    DateCloseTest test = { 0 };

    test.loop = g_main_loop_new (NULL, FALSE);
    test.kind = DATE_ACCOUNT;
    gnc_dialog_date_acct_parented_async (
        NULL, NULL, "Date", "Account", FALSE, NULL, NULL, 0, NULL, NULL,
        date_close_invalid_finished, &test);
    g_main_loop_run (test.loop);

    g_assert_error (test.error, G_IO_ERROR, G_IO_ERROR_INVALID_ARGUMENT);
    date_close_test_clear (&test);
}

static void
test_dates_account_question_rejects_incomplete_request (void)
{
    DateCloseTest test = { 0 };

    test.loop = g_main_loop_new (NULL, FALSE);
    test.kind = DATES_ACCOUNT_QUESTION;
    gnc_dialog_dates_acct_question_parented_async (
        NULL, NULL, "Due Date", "Post Date", "Account", NULL, FALSE, FALSE,
        NULL, NULL, NULL, NULL, 0, 0, NULL, NULL, FALSE, NULL,
        date_close_invalid_finished, &test);
    g_main_loop_run (test.loop);

    g_assert_error (test.error, G_IO_ERROR, G_IO_ERROR_INVALID_ARGUMENT);
    date_close_test_clear (&test);
}

int
main (int argc, char **argv)
{
    g_test_init (&argc, &argv, NULL);
    g_test_add_func ("/gnome/dialog-date-close/incomplete-date",
                     test_date_close_rejects_incomplete_request);
    g_test_add_func ("/gnome/dialog-date-close/cancelled-date",
                     test_date_close_reports_cancelled_request);
    g_test_add_func ("/gnome/dialog-date-close/incomplete-date-account",
                     test_date_account_rejects_incomplete_request);
    g_test_add_func ("/gnome/dialog-date-close/incomplete-posting",
                     test_dates_account_question_rejects_incomplete_request);
    return g_test_run ();
}
