/*
 * gnc-session.c -- GnuCash's session handling
 *
 * Copyright (C) 2006 Chris Shoemaker <c.shoemaker@cox.net>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

#include <config.h>
#include "qof.h"
#include "gnc-session.h"
#include "gnc-engine.h"
#include "TransLog.h"

static QofSession * current_session = NULL;
static guint64 current_session_generation = 1;
static QofLogModule log_module = GNC_MOD_ENGINE;

struct GncSessionOperationContext
{
    gatomicrefcount ref_count;
    QofSession *session;
    QofBook *book;
    guint64 current_generation;
    guint64 session_generation;
    guint64 active_operation_id;
    guint64 last_operation_id;
    QofSessionOperationKind kind;
    QofSessionOperationLease *lease;
    guint lease_depth;
    gboolean valid;
    gboolean terminal;
};

static void
advance_current_session_generation (void)
{
    current_session_generation++;
    if (current_session_generation == 0)
        current_session_generation = 1;
}

QofSession *
gnc_get_current_session (void)
{
    if (!current_session)
    {
        QofBook* book = qof_book_new ();
        qof_event_suspend();
        current_session = qof_session_new (book);
        advance_current_session_generation ();
        qof_event_resume();
    }

    return current_session;
}

gboolean
gnc_current_session_exist(void)
{
    return (current_session != NULL);
}

guint64
gnc_current_session_get_generation(void)
{
    return current_session_generation;
}

static gboolean
operation_context_matches_current (
    const GncSessionOperationContext *context,
    gboolean check_session_generation)
{
    if (!context || !context->valid || !current_session ||
        context->current_generation != current_session_generation)
        return FALSE;
    if (context->session != current_session ||
        context->book != qof_session_get_book (current_session))
        return FALSE;
    return !check_session_generation ||
           context->session_generation ==
               qof_session_get_operation_generation (current_session);
}

GncSessionOperationContext *
gnc_session_operation_context_new (QofBook *book,
                                   QofSessionOperationKind kind)
{
    GncSessionOperationContext *context;

    if (!book || !current_session ||
        qof_session_get_book (current_session) != book ||
        qof_session_has_active_operation_lease (current_session))
        return NULL;

    context = g_new0 (GncSessionOperationContext, 1);
    g_atomic_ref_count_init (&context->ref_count);
    context->session = current_session;
    context->book = book;
    context->current_generation = current_session_generation;
    context->session_generation =
        qof_session_get_operation_generation (current_session);
    context->kind = kind;
    context->valid = TRUE;
    return context;
}

GncSessionOperationContext *
gnc_session_operation_context_ref (GncSessionOperationContext *context)
{
    if (context)
        g_atomic_ref_count_inc (&context->ref_count);
    return context;
}

gboolean
gnc_session_operation_context_begin (GncSessionOperationContext *context)
{
    QofSessionOperationLease *lease;

    if (!context || context->terminal)
        return FALSE;
    if (context->lease)
    {
        if (!operation_context_matches_current (context, TRUE) ||
            !qof_session_operation_lease_is_valid (context->lease,
                                                   context->session) ||
            qof_session_operation_lease_get_id (context->lease) !=
                context->active_operation_id ||
            qof_session_operation_lease_get_kind (context->lease) !=
                context->kind)
            return FALSE;
        context->lease_depth++;
        return TRUE;
    }
    if (!operation_context_matches_current (context, TRUE))
        return FALSE;

    lease = qof_session_operation_lease_acquire_for (context->session,
                                                     context->kind);
    if (!lease)
        return FALSE;
    context->active_operation_id =
        qof_session_operation_lease_get_id (lease);
    if (!context->active_operation_id)
    {
        qof_session_operation_lease_release (lease);
        context->valid = FALSE;
        return FALSE;
    }
    context->lease = lease;
    context->lease_depth = 1;
    return TRUE;
}

gboolean
gnc_session_operation_context_begin_cleanup (
    GncSessionOperationContext *context)
{
    QofSessionOperationLease *lease;

    if (!context || context->kind != QOF_SESSION_OPERATION_IMPORT)
        return FALSE;
    if (context->lease)
    {
        if (!operation_context_matches_current (context, TRUE) ||
            !qof_session_operation_lease_is_valid (context->lease,
                                                   context->session) ||
            qof_session_operation_lease_get_id (context->lease) !=
                context->active_operation_id ||
            qof_session_operation_lease_get_kind (context->lease) !=
                context->kind)
            return FALSE;
        context->terminal = TRUE;
        context->lease_depth++;
        return TRUE;
    }
    if (!operation_context_matches_current (context, FALSE))
        return FALSE;

    context->terminal = TRUE;
    /* Cleanup may rebase only the operation generation. Session identity,
     * current-slot generation, and book identity were checked above. */
    context->session_generation =
        qof_session_get_operation_generation (context->session);
    lease = qof_session_operation_lease_acquire_for (context->session,
                                                     context->kind);
    if (!lease)
        return FALSE;
    context->active_operation_id =
        qof_session_operation_lease_get_id (lease);
    if (!context->active_operation_id)
    {
        qof_session_operation_lease_release (lease);
        context->valid = FALSE;
        return FALSE;
    }
    context->lease = lease;
    context->lease_depth = 1;
    return TRUE;
}

void
gnc_session_operation_context_end (GncSessionOperationContext *context)
{
    QofSessionOperationLease *lease;
    guint64 operation_id;
    gboolean owned;

    if (!context || !context->lease || !context->lease_depth)
        return;
    context->lease_depth--;
    if (context->lease_depth)
        return;

    lease = context->lease;
    operation_id = context->active_operation_id;
    owned = operation_context_matches_current (context, TRUE) &&
            qof_session_operation_lease_is_valid (lease,
                                                  context->session) &&
            qof_session_operation_lease_get_id (lease) == operation_id &&
            qof_session_operation_lease_get_kind (lease) == context->kind;
    context->lease = NULL;
    context->active_operation_id = 0;
    qof_session_operation_lease_release (lease);

    if (owned && operation_context_matches_current (context, FALSE))
    {
        context->session_generation =
            qof_session_get_operation_generation (context->session);
        context->last_operation_id = operation_id;
    }
    else
        context->valid = FALSE;
}

gboolean
gnc_session_operation_context_is_current (
    const GncSessionOperationContext *context)
{
    return context && !context->terminal &&
           operation_context_matches_current (context, TRUE);
}

gboolean
gnc_session_operation_context_identifies_current_book (
    const GncSessionOperationContext *context)
{
    return operation_context_matches_current (context, FALSE);
}

gboolean
gnc_session_operation_context_has_lease (
    const GncSessionOperationContext *context)
{
    return context && context->lease && context->lease_depth &&
           operation_context_matches_current (context, TRUE) &&
           qof_session_operation_lease_is_valid (context->lease,
                                                 context->session) &&
           qof_session_operation_lease_get_id (context->lease) ==
               context->active_operation_id;
}

guint64
gnc_session_operation_context_get_last_id (
    const GncSessionOperationContext *context)
{
    return context ? context->last_operation_id : 0;
}

void
gnc_session_operation_context_unref (GncSessionOperationContext *context)
{
    if (!context || !g_atomic_ref_count_dec (&context->ref_count))
        return;
    if (context->lease)
    {
        context->lease_depth = 1;
        gnc_session_operation_context_end (context);
    }
    g_free (context);
}

void
gnc_set_current_session (QofSession *session)
{
    if (current_session == session)
        return;

    if (current_session &&
        qof_session_has_active_operation_lease (current_session))
    {
        PWARN ("Refusing to replace the current session while it is leased");
        return;
    }
    if (session && qof_session_has_active_operation_lease (session))
    {
        PWARN ("Refusing to install a leased session as current");
        return;
    }

    if (current_session)
        PINFO("Leak of current session.");
    current_session = session;
    advance_current_session_generation ();
}

void gnc_clear_current_session()
{
    if (current_session)
    {
        if (qof_session_has_active_operation_lease (current_session))
        {
            PWARN ("Refusing to clear a leased current session without its token");
            return;
        }

        QofSession *session = current_session;
        current_session = NULL;
        advance_current_session_generation ();
        xaccLogDisable();
        qof_session_destroy(session);
        xaccLogEnable();
    }
}

gboolean
gnc_clear_current_session_with_lease (QofSessionOperationLease *lease)
{
    QofSession *session;

    if (!current_session ||
        !qof_session_operation_lease_is_valid (lease, current_session))
        return FALSE;

    session = current_session;
    current_session = NULL;
    xaccLogDisable ();
    if (!qof_session_destroy_with_lease (session, lease))
    {
        current_session = session;
        xaccLogEnable ();
        return FALSE;
    }
    advance_current_session_generation ();
    xaccLogEnable ();
    return TRUE;
}
