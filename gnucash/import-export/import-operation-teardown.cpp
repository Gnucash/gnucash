/********************************************************************\
 * import-operation-teardown.cpp -- asynchronous import teardown owner*
 * Copyright (C) 2026                                               *
 ********************************************************************/
#include <config.h>

#include "import-operation-teardown.h"

#include "qoflog.h"

static constexpr guint RETRY_INTERVAL_MSEC = 50;

struct GncImportOperationTeardown
{
    gatomicrefcount ref_count;
    GncSessionOperationContext *context;
    GApplication *application;
    gulong shutdown_handler;
    guint source_id;
    gboolean application_held;
    gboolean requested;
    gboolean complete;
    gboolean finishing;
    gboolean in_source;
    gboolean in_shutdown;
    GncImportOperationTeardownFunc callback;
    gpointer user_data;
    GDestroyNotify user_data_destroy;
};

static gboolean teardown_attempt (GncImportOperationTeardown *owner,
                                  gboolean application_shutdown);

GncImportOperationTeardown *
gnc_import_operation_teardown_ref (GncImportOperationTeardown *owner)
{
    g_return_val_if_fail (owner, nullptr);
    g_atomic_ref_count_inc (&owner->ref_count);
    return owner;
}

static void
teardown_release_application_hold (GncImportOperationTeardown *owner)
{
    if (!owner->application_held)
        return;
    owner->application_held = FALSE;
    g_application_release (owner->application);
}

static void
teardown_dispose (GncImportOperationTeardown *owner)
{
    g_assert (!owner->source_id);
    teardown_release_application_hold (owner);
    if (owner->shutdown_handler)
        g_signal_handler_disconnect (owner->application, owner->shutdown_handler);
    if (owner->user_data_destroy)
        owner->user_data_destroy (owner->user_data);
    gnc_session_operation_context_unref (owner->context);
    g_object_unref (owner->application);
    g_free (owner);
}

void
gnc_import_operation_teardown_unref (GncImportOperationTeardown *owner)
{
    if (!owner)
        return;
    if (g_atomic_ref_count_dec (&owner->ref_count))
        teardown_dispose (owner);
}

static void
teardown_source_destroyed (gpointer user_data)
{
    auto owner = static_cast<GncImportOperationTeardown *> (user_data);
    owner->source_id = 0;

    /* A destroyed main context is a terminal application boundary. Never
     * discard current-book objects as stale merely because the retry source
     * disappeared. */
    if (owner->requested && !owner->complete && !owner->finishing)
        teardown_attempt (owner, TRUE);
    gnc_import_operation_teardown_unref (owner);
}

static void
teardown_complete (GncImportOperationTeardown *owner,
                   GncImportOperationTeardownResult result)
{
    guint source_id = 0;

    if (owner->complete || owner->finishing)
        return;

    gnc_import_operation_teardown_ref (owner);
    owner->finishing = TRUE;
    owner->complete = TRUE;
    if (!owner->in_source && owner->source_id)
    {
        source_id = owner->source_id;
        owner->source_id = 0;
    }
    teardown_release_application_hold (owner);
    if (source_id)
        g_source_remove (source_id);

    owner->callback (owner, result, owner->user_data);
    owner->finishing = FALSE;
    gnc_import_operation_teardown_unref (owner);
}

static gboolean
teardown_attempt (GncImportOperationTeardown *owner,
                  gboolean application_shutdown)
{
    if (owner->complete)
        return TRUE;

    if (gnc_session_operation_context_begin_cleanup (owner->context))
    {
        gnc_import_operation_teardown_ref (owner);
        teardown_complete (owner,
                           GNC_IMPORT_OPERATION_TEARDOWN_MUTATION_ALLOWED);
        gnc_session_operation_context_end (owner->context);
        gnc_import_operation_teardown_unref (owner);
        return TRUE;
    }

    if (!gnc_session_operation_context_identifies_current_book (owner->context))
    {
        teardown_complete (owner, GNC_IMPORT_OPERATION_TEARDOWN_STALE);
        return TRUE;
    }

    if (application_shutdown)
    {
        teardown_complete (owner,
                           GNC_IMPORT_OPERATION_TEARDOWN_BOOK_SHUTDOWN);
        return TRUE;
    }
    return FALSE;
}

static gboolean
teardown_retry (gpointer user_data)
{
    auto owner = static_cast<GncImportOperationTeardown *> (user_data);
    owner->in_source = TRUE;
    auto finished = teardown_attempt (owner, FALSE);
    owner->in_source = FALSE;

    return finished ? G_SOURCE_REMOVE : G_SOURCE_CONTINUE;
}

static void
teardown_application_shutdown (GApplication *application, gpointer user_data)
{
    auto owner = static_cast<GncImportOperationTeardown *> (user_data);
    gnc_import_operation_teardown_ref (owner);
    if (owner->complete)
    {
        gnc_import_operation_teardown_unref (owner);
        return;
    }
    owner->requested = TRUE;
    owner->in_shutdown = TRUE;
    auto shutdown_handler = owner->shutdown_handler;
    owner->shutdown_handler = 0;
    if (shutdown_handler)
        g_signal_handler_disconnect (application, shutdown_handler);
    teardown_attempt (owner, TRUE);
    owner->in_shutdown = FALSE;
    gnc_import_operation_teardown_unref (owner);
}

GncImportOperationTeardown *
gnc_import_operation_teardown_new (
    GncSessionOperationContext *context,
    GApplication *application,
    GncImportOperationTeardownFunc callback,
    gpointer user_data,
    GDestroyNotify user_data_destroy)
{
    g_return_val_if_fail (context, nullptr);
    g_return_val_if_fail (G_IS_APPLICATION (application), nullptr);
    g_return_val_if_fail (callback, nullptr);

    auto owner = g_new0 (GncImportOperationTeardown, 1);
    g_atomic_ref_count_init (&owner->ref_count);
    owner->context = gnc_session_operation_context_ref (context);
    owner->application = G_APPLICATION (g_object_ref (application));
    owner->callback = callback;
    owner->user_data = user_data;
    owner->user_data_destroy = user_data_destroy;
    owner->application_held = TRUE;
    g_application_hold (owner->application);
    owner->shutdown_handler = g_signal_connect (
        owner->application, "shutdown",
        G_CALLBACK (teardown_application_shutdown), owner);
    return owner;
}

gboolean
gnc_import_operation_teardown_request (GncImportOperationTeardown *owner)
{
    g_return_val_if_fail (owner, TRUE);
    if (owner->complete)
        return TRUE;
    if (owner->requested)
        return FALSE;

    owner->requested = TRUE;
    if (teardown_attempt (owner, FALSE))
        return TRUE;

    owner->source_id = g_timeout_add_full (
        G_PRIORITY_DEFAULT_IDLE, RETRY_INTERVAL_MSEC, teardown_retry,
        gnc_import_operation_teardown_ref (owner), teardown_source_destroyed);
    g_assert (owner->source_id);
    return FALSE;
}

gboolean
gnc_import_operation_teardown_is_requested (
    const GncImportOperationTeardown *owner)
{
    return owner && owner->requested;
}

gboolean
gnc_import_operation_teardown_is_complete (
    const GncImportOperationTeardown *owner)
{
    return !owner || owner->complete;
}

gboolean
gnc_import_operation_teardown_has_pending_retry (
    const GncImportOperationTeardown *owner)
{
    return owner && owner->source_id && !owner->complete;
}

GncSessionOperationContext *
gnc_import_operation_teardown_get_context (GncImportOperationTeardown *owner)
{
    return owner ? owner->context : nullptr;
}
