/* Copyright (C) 2026 GnuCash Developers
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 */

#include <config.h>

#include "gnc-session-transition.h"

struct _GncSessionTransition
{
    GncSessionTransitionKind kind;
    GncSessionTransitionStartFunc start;
    GncSessionTransitionCancelFunc cancel;
    gpointer user_data;
};

static GQueue transition_queue = G_QUEUE_INIT;
static GncSessionTransition *active_transition;
static GncSessionTransition *quit_transition;
static gboolean transition_dispatching;
static gboolean transition_shutting_down;

static void gnc_session_transition_dispatch (void);

static void
gnc_session_transition_cancel (GncSessionTransition *transition)
{
    if (transition->cancel)
        transition->cancel (transition->user_data);
    g_free (transition);
}

static void
gnc_session_transition_dispatch (void)
{
    if (transition_dispatching || transition_shutting_down)
        return;

    transition_dispatching = TRUE;
    while (!active_transition && !g_queue_is_empty (&transition_queue) &&
           !transition_shutting_down)
    {
        active_transition = g_queue_pop_head (&transition_queue);
        active_transition->start (active_transition,
                                  active_transition->user_data);
    }
    transition_dispatching = FALSE;
}

GncSessionTransitionDisposition
gnc_session_transition_enqueue (GncSessionTransitionKind kind,
                                GncSessionTransitionStartFunc start,
                                GncSessionTransitionCancelFunc cancel,
                                gpointer user_data)
{
    GncSessionTransition *transition;
    GncSessionTransitionDisposition disposition;

    g_return_val_if_fail (start != NULL, GNC_SESSION_TRANSITION_REJECTED);

    /* Once a quit request is admitted, accepting work behind it would either
     * strand that work or restart session activity during shutdown. */
    if (transition_shutting_down || quit_transition)
        return GNC_SESSION_TRANSITION_REJECTED;

    transition = g_new0 (GncSessionTransition, 1);
    transition->kind = kind;
    transition->start = start;
    transition->cancel = cancel;
    transition->user_data = user_data;
    if (kind == GNC_SESSION_TRANSITION_QUIT)
        quit_transition = transition;

    disposition = active_transition || !g_queue_is_empty (&transition_queue) ?
        GNC_SESSION_TRANSITION_QUEUED : GNC_SESSION_TRANSITION_STARTED;
    g_queue_push_tail (&transition_queue, transition);
    gnc_session_transition_dispatch ();
    return disposition;
}

void
gnc_session_transition_complete (GncSessionTransition *transition)
{
    g_return_if_fail (transition != NULL);
    g_return_if_fail (transition == active_transition);

    active_transition = NULL;
    if (transition == quit_transition)
        quit_transition = NULL;
    g_free (transition);
    gnc_session_transition_dispatch ();
}

void
gnc_session_transition_begin_shutdown (GncSessionTransition *transition)
{
    g_return_if_fail (transition == NULL || transition == active_transition);

    transition_shutting_down = TRUE;

    while (!g_queue_is_empty (&transition_queue))
        gnc_session_transition_cancel (g_queue_pop_head (&transition_queue));

    if (transition)
    {
        active_transition = NULL;
        g_free (transition);
    }
    quit_transition = NULL;
}

gboolean
gnc_session_transition_quit_pending (void)
{
    return quit_transition != NULL || transition_shutting_down;
}
