/* Copyright (C) 2026 GnuCash Developers
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 */

#include <config.h>

#include <glib.h>

#include "gnc-session-transition.h"

typedef struct
{
    gchar marker;
    GString *events;
    GncSessionTransition *transition;
} TestTransition;

typedef struct
{
    GString *events;
    TestTransition *nested;
    GncSessionTransitionDisposition nested_disposition;
} EnqueueFromStart;

typedef struct
{
    GString *events;
    TestTransition rejected;
    GncSessionTransitionDisposition rejected_disposition;
} EnqueueFromCancel;

static void
transition_started (GncSessionTransition *transition, gpointer user_data)
{
    TestTransition *test = user_data;

    test->transition = transition;
    g_string_append_c (test->events, test->marker);
}

static void
complete_transition (TestTransition *test)
{
    GncSessionTransition *transition = test->transition;

    g_assert_nonnull (transition);
    test->transition = NULL;
    gnc_session_transition_complete (transition);
}

static void
synchronous_transition_started (GncSessionTransition *transition,
                                gpointer user_data)
{
    TestTransition *test = user_data;
    GString *events = test->events;
    gchar marker = test->marker;

    g_string_append_c (events, marker);
    g_free (test);
    gnc_session_transition_complete (transition);
}

static void
enqueue_from_start (GncSessionTransition *transition, gpointer user_data)
{
    EnqueueFromStart *test = user_data;

    g_string_append_c (test->events, 'A');
    test->nested_disposition = gnc_session_transition_enqueue (
        GNC_SESSION_TRANSITION_OPEN, transition_started, NULL, test->nested);
    gnc_session_transition_complete (transition);
}

static void
enqueue_from_cancel (gpointer user_data)
{
    EnqueueFromCancel *test = user_data;

    g_string_append_c (test->events, 'C');
    test->rejected_disposition = gnc_session_transition_enqueue (
        GNC_SESSION_TRANSITION_OPEN, transition_started, NULL,
        &test->rejected);
}

static void
test_ordered_opens_and_quit (void)
{
    GString *events = g_string_new (NULL);
    TestTransition first_open = { '1', events, NULL };
    TestTransition second_open = { '2', events, NULL };
    TestTransition quit = { 'Q', events, NULL };
    TestTransition rejected_open = { 'X', events, NULL };
    TestTransition nested = { 'B', events, NULL };
    TestTransition blocker = { 'D', events, NULL };
    EnqueueFromStart enqueue_start = {
        events, &nested, GNC_SESSION_TRANSITION_REJECTED
    };
    EnqueueFromCancel enqueue_cancel = {
        events, { 'Z', events, NULL }, GNC_SESSION_TRANSITION_STARTED
    };
    TestTransition *synchronous = g_new0 (TestTransition, 1);

    g_assert_cmpint (gnc_session_transition_enqueue (
                         GNC_SESSION_TRANSITION_OPEN, transition_started,
                         NULL, &first_open),
                     ==, GNC_SESSION_TRANSITION_STARTED);
    g_assert_cmpint (gnc_session_transition_enqueue (
                         GNC_SESSION_TRANSITION_OPEN, transition_started,
                         NULL, &second_open),
                     ==, GNC_SESSION_TRANSITION_QUEUED);
    g_assert_cmpint (gnc_session_transition_enqueue (
                         GNC_SESSION_TRANSITION_QUIT, transition_started,
                         NULL, &quit),
                     ==, GNC_SESSION_TRANSITION_QUEUED);
    g_assert_cmpint (gnc_session_transition_enqueue (
                         GNC_SESSION_TRANSITION_OPEN, transition_started,
                         NULL, &rejected_open),
                     ==, GNC_SESSION_TRANSITION_REJECTED);
    g_assert_cmpstr (events->str, ==, "1");
    g_assert_true (gnc_session_transition_quit_pending ());

    complete_transition (&first_open);
    g_assert_cmpstr (events->str, ==, "12");
    complete_transition (&second_open);
    g_assert_cmpstr (events->str, ==, "12Q");

    /* Cancelling the admitted quit reopens admission. */
    complete_transition (&quit);
    g_assert_false (gnc_session_transition_quit_pending ());
    g_assert_cmpint (gnc_session_transition_enqueue (
                         GNC_SESSION_TRANSITION_OPEN, transition_started,
                         NULL, &rejected_open),
                     ==, GNC_SESSION_TRANSITION_STARTED);
    g_assert_cmpstr (events->str, ==, "12QX");
    complete_transition (&rejected_open);

    /* A STARTED callback may complete and release its request before enqueue
     * returns. */
    synchronous->marker = 'S';
    synchronous->events = events;
    g_assert_cmpint (gnc_session_transition_enqueue (
                         GNC_SESSION_TRANSITION_SAVE,
                         synchronous_transition_started, NULL, synchronous),
                     ==, GNC_SESSION_TRANSITION_STARTED);
    g_assert_cmpstr (events->str, ==, "12QXS");

    /* Enqueuing from start remains FIFO even when the outer request completes
     * synchronously. */
    g_assert_cmpint (gnc_session_transition_enqueue (
                         GNC_SESSION_TRANSITION_OPEN, enqueue_from_start, NULL,
                         &enqueue_start),
                     ==, GNC_SESSION_TRANSITION_STARTED);
    g_assert_cmpint (enqueue_start.nested_disposition,
                     ==, GNC_SESSION_TRANSITION_QUEUED);
    g_assert_cmpstr (events->str, ==, "12QXSAB");
    complete_transition (&nested);

    /* Shutdown cancels only queued requests. Cancellation may try to enqueue,
     * but admission is already sealed. The active operation remains valid
     * until its terminal callback completes it. */
    g_assert_cmpint (gnc_session_transition_enqueue (
                         GNC_SESSION_TRANSITION_OPEN, transition_started,
                         NULL, &blocker),
                     ==, GNC_SESSION_TRANSITION_STARTED);
    g_assert_cmpint (gnc_session_transition_enqueue (
                         GNC_SESSION_TRANSITION_OPEN, transition_started,
                         enqueue_from_cancel, &enqueue_cancel),
                     ==, GNC_SESSION_TRANSITION_QUEUED);
    gnc_session_transition_begin_shutdown (NULL);
    g_assert_cmpstr (events->str, ==, "12QXSABDC");
    g_assert_cmpint (enqueue_cancel.rejected_disposition,
                     ==, GNC_SESSION_TRANSITION_REJECTED);
    g_assert_null (enqueue_cancel.rejected.transition);
    g_assert_true (gnc_session_transition_quit_pending ());
    complete_transition (&blocker);

    g_string_free (events, TRUE);
}

int
main (int argc, char **argv)
{
    g_test_init (&argc, &argv, NULL);
    g_test_add_func ("/gnucash/session-transition/ordered-opens-and-quit",
                     test_ordered_opens_and_quit);
    return g_test_run ();
}
