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

/********************************************************************\
 * gnc-ofx-import-teardown.cpp -- OFX workflow lifecycle controller  *
 ********************************************************************/
#include <config.h>

#include <gtk/gtk.h>

#include "gnc-ofx-import-teardown.h"

#include "Transaction.h"
#include "gnc-session.h"
#include "gnc-ui-util.h"
#include "import-main-matcher.h"
#include "qofbook.h"

struct GncOfxImportDestroyContinuation;

struct GncOfxImportLifecycle
{
    GncImportOperationTeardown *teardown; /* Borrowed: owns this payload. */
    GNCImportMainMatcher **matcher_slot;
    GList **transaction_slot;
    GncOfxImportMetadataCleanupFunc metadata_cleanup;
    gpointer user_data;
    GDestroyNotify user_data_destroy;
    GList *continuations; /* Non-owning registrations. */
    gboolean operation_ref_held;
};

struct GncOfxImportAsyncState
{
    GncOfxImportLifecycle *lifecycle;
};

struct GncOfxImportDestroyContinuation
{
    GncOfxImportLifecycle *lifecycle;
    GWeakRef source;
    gulong handler;
    GncOfxImportDestroyContinuationFunc callback;
    gpointer user_data;
    gboolean registered;
    gboolean cancelled;
    gboolean emitting;
};

static void
ofx_import_lifecycle_release_operation (GncOfxImportLifecycle *lifecycle)
{
    if (!lifecycle || !lifecycle->operation_ref_held)
        return;
    lifecycle->operation_ref_held = FALSE;
    gnc_import_operation_teardown_unref (lifecycle->teardown);
}

static void
ofx_import_destroy_continuation_closed (gpointer user_data, GClosure *closure)
{
    auto continuation = static_cast<GncOfxImportDestroyContinuation *> (user_data);
    auto lifecycle = continuation->lifecycle;

    if (continuation->registered)
    {
        lifecycle->continuations = g_list_remove (lifecycle->continuations,
                                                   continuation);
        continuation->registered = FALSE;
    }
    continuation->handler = 0;
    g_weak_ref_clear (&continuation->source);
    g_free (continuation);
    gnc_ofx_import_lifecycle_unref (lifecycle);
    (void)closure;
}

static void
ofx_import_destroy_continuation_called (
    GObject *source, GncOfxImportDestroyContinuation *continuation)
{
    auto lifecycle = gnc_ofx_import_lifecycle_ref (continuation->lifecycle);
    if (continuation->registered)
    {
        lifecycle->continuations = g_list_remove (lifecycle->continuations,
                                                   continuation);
        continuation->registered = FALSE;
    }
    continuation->emitting = TRUE;
    if (!continuation->cancelled &&
        !gnc_ofx_import_lifecycle_is_terminal (lifecycle))
        continuation->callback (source, continuation->user_data);
    continuation->emitting = FALSE;
    auto handler = continuation->handler;
    continuation->handler = 0;
    if (handler)
        g_signal_handler_disconnect (source, handler);
    gnc_ofx_import_lifecycle_unref (lifecycle);
}

static void
ofx_import_lifecycle_cancel_continuations (
    GncOfxImportLifecycle *lifecycle)
{
    while (lifecycle->continuations)
    {
        auto continuation = static_cast<GncOfxImportDestroyContinuation *> (
            lifecycle->continuations->data);
        continuation->cancelled = TRUE;
        auto source = G_OBJECT (g_weak_ref_get (&continuation->source));
        if (continuation->emitting)
        {
            lifecycle->continuations = g_list_remove (
                lifecycle->continuations, continuation);
            continuation->registered = FALSE;
        }
        else if (source && continuation->handler)
        {
            auto handler = continuation->handler;
            continuation->handler = 0;
            g_signal_handler_disconnect (source, handler);
        }
        else
        {
            /* A live registration always has either a source/handler or is
             * currently emitting. Its GClosure owns the final lifecycle ref. */
            g_assert_not_reached ();
        }
        g_clear_object (&source);
    }
}

static void
ofx_import_lifecycle_assert_book_owns_transaction (Transaction *transaction)
{
    auto book = qof_instance_get_book (QOF_INSTANCE (transaction));
    g_assert (book);
    auto collection = qof_book_get_collection (book, GNC_ID_TRANS);
    g_assert (qof_collection_lookup_entity (
                  collection,
                  qof_instance_get_guid (QOF_INSTANCE (transaction))) ==
              QOF_INSTANCE (transaction));
}

static void
ofx_import_lifecycle_cleanup (
    GncImportOperationTeardown *teardown,
    GncImportOperationTeardownResult result,
    gpointer user_data)
{
    auto lifecycle = static_cast<GncOfxImportLifecycle *> (user_data);
    auto mutation_allowed =
        result == GNC_IMPORT_OPERATION_TEARDOWN_MUTATION_ALLOWED;

    g_assert (teardown == lifecycle->teardown);
    ofx_import_lifecycle_cancel_continuations (lifecycle);

    if (lifecycle->matcher_slot && *lifecycle->matcher_slot)
    {
        auto matcher = *lifecycle->matcher_slot;
        *lifecycle->matcher_slot = nullptr;
        if (mutation_allowed)
            gnc_gen_trans_list_delete_with_cleanup_operation (matcher);
        else
            gnc_gen_trans_list_discard (matcher);
    }

    if (lifecycle->transaction_slot && *lifecycle->transaction_slot)
    {
        if (mutation_allowed)
        {
            for (auto node = *lifecycle->transaction_slot; node;
                 node = node->next)
            {
                auto transaction = static_cast<Transaction *> (node->data);
                g_assert (qof_instance_get_book (QOF_INSTANCE (transaction)) ==
                          gnc_get_current_book ());
                ofx_import_lifecycle_assert_book_owns_transaction (transaction);
                xaccTransDestroy (transaction);
                xaccTransCommitEdit (transaction);
            }
        }
        else if (result == GNC_IMPORT_OPERATION_TEARDOWN_BOOK_SHUTDOWN)
        {
            for (auto node = *lifecycle->transaction_slot; node;
                 node = node->next)
            {
                auto transaction = static_cast<Transaction *> (node->data);
                g_assert (qof_instance_get_book (QOF_INSTANCE (transaction)) ==
                          gnc_get_current_book ());
                ofx_import_lifecycle_assert_book_owns_transaction (transaction);
            }
            g_warning ("OFX teardown transferred open transaction ownership to the shutting-down current book");
        }
        else
            g_warning ("Discarding stale OFX transaction references without touching a foreign book");
        g_list_free (*lifecycle->transaction_slot);
        *lifecycle->transaction_slot = nullptr;
    }

    if (lifecycle->metadata_cleanup)
        lifecycle->metadata_cleanup (lifecycle, result,
                                     lifecycle->user_data);
    ofx_import_lifecycle_release_operation (lifecycle);
}

static void
ofx_import_lifecycle_free (gpointer user_data)
{
    auto lifecycle = static_cast<GncOfxImportLifecycle *> (user_data);
    g_assert (!lifecycle->continuations);
    g_assert (!lifecycle->operation_ref_held);
    if (lifecycle->user_data_destroy)
        lifecycle->user_data_destroy (lifecycle->user_data);
    g_free (lifecycle);
}

GncOfxImportLifecycle *
gnc_ofx_import_lifecycle_new (
    GncSessionOperationContext *context,
    GApplication *application,
    GNCImportMainMatcher **matcher_slot,
    GList **transaction_slot,
    GncOfxImportMetadataCleanupFunc metadata_cleanup,
    gpointer user_data,
    GDestroyNotify user_data_destroy)
{
    g_return_val_if_fail (context, nullptr);
    g_return_val_if_fail (G_IS_APPLICATION (application), nullptr);
    g_return_val_if_fail (matcher_slot, nullptr);
    g_return_val_if_fail (transaction_slot, nullptr);

    auto lifecycle = g_new0 (GncOfxImportLifecycle, 1);
    lifecycle->matcher_slot = matcher_slot;
    lifecycle->transaction_slot = transaction_slot;
    lifecycle->metadata_cleanup = metadata_cleanup;
    lifecycle->user_data = user_data;
    lifecycle->user_data_destroy = user_data_destroy;
    lifecycle->operation_ref_held = TRUE;
    lifecycle->teardown = gnc_import_operation_teardown_new (
        context, application, ofx_import_lifecycle_cleanup, lifecycle,
        ofx_import_lifecycle_free);
    if (!lifecycle->teardown)
    {
        g_free (lifecycle);
        return nullptr;
    }
    return lifecycle;
}

GncOfxImportLifecycle *
gnc_ofx_import_lifecycle_ref (GncOfxImportLifecycle *lifecycle)
{
    if (lifecycle)
        gnc_import_operation_teardown_ref (lifecycle->teardown);
    return lifecycle;
}

void
gnc_ofx_import_lifecycle_unref (GncOfxImportLifecycle *lifecycle)
{
    if (lifecycle)
        gnc_import_operation_teardown_unref (lifecycle->teardown);
}

gboolean
gnc_ofx_import_lifecycle_request (GncOfxImportLifecycle *lifecycle)
{
    return !lifecycle ||
           gnc_import_operation_teardown_request (lifecycle->teardown);
}

void
gnc_ofx_import_lifecycle_finish (GncOfxImportLifecycle *lifecycle)
{
    if (!lifecycle)
        return;
    g_return_if_fail (!gnc_ofx_import_lifecycle_is_terminal (lifecycle));
    g_return_if_fail (!lifecycle->continuations);
    ofx_import_lifecycle_release_operation (lifecycle);
}

gboolean
gnc_ofx_import_lifecycle_is_terminal (
    const GncOfxImportLifecycle *lifecycle)
{
    return !lifecycle ||
           gnc_import_operation_teardown_is_requested (lifecycle->teardown) ||
           gnc_import_operation_teardown_is_complete (lifecycle->teardown);
}

GncSessionOperationContext *
gnc_ofx_import_lifecycle_get_context (GncOfxImportLifecycle *lifecycle)
{
    return lifecycle ? gnc_import_operation_teardown_get_context (
                           lifecycle->teardown) : nullptr;
}

GncImportOperationTeardown *
gnc_ofx_import_lifecycle_get_teardown (GncOfxImportLifecycle *lifecycle)
{
    return lifecycle ? lifecycle->teardown : nullptr;
}

GncOfxImportAsyncState *
gnc_ofx_import_async_state_new (GncOfxImportLifecycle *lifecycle)
{
    g_return_val_if_fail (lifecycle, nullptr);
    if (gnc_ofx_import_lifecycle_is_terminal (lifecycle))
        return nullptr;
    auto state = g_new0 (GncOfxImportAsyncState, 1);
    state->lifecycle = gnc_ofx_import_lifecycle_ref (lifecycle);
    return state;
}

void
gnc_ofx_import_async_state_unref (GncOfxImportAsyncState *state)
{
    if (!state)
        return;
    auto lifecycle = state->lifecycle;
    state->lifecycle = nullptr;
    g_free (state);
    gnc_ofx_import_lifecycle_unref (lifecycle);
}

gboolean
gnc_ofx_import_async_state_is_active (const GncOfxImportAsyncState *state)
{
    return state && state->lifecycle &&
           !gnc_ofx_import_lifecycle_is_terminal (state->lifecycle);
}

GncOfxImportLifecycle *
gnc_ofx_import_async_state_get_lifecycle (GncOfxImportAsyncState *state)
{
    return state ? state->lifecycle : nullptr;
}

gboolean
gnc_ofx_import_async_state_request_teardown (GncOfxImportAsyncState *state)
{
    return !state || gnc_ofx_import_lifecycle_request (state->lifecycle);
}

gboolean
gnc_ofx_import_lifecycle_connect_destroy (
    GncOfxImportLifecycle *lifecycle,
    GObject *source,
    GncOfxImportDestroyContinuationFunc callback,
    gpointer user_data)
{
    g_return_val_if_fail (lifecycle, FALSE);
    g_return_val_if_fail (G_IS_OBJECT (source), FALSE);
    g_return_val_if_fail (callback, FALSE);
    if (gnc_ofx_import_lifecycle_is_terminal (lifecycle) ||
        !g_signal_lookup ("destroy", G_OBJECT_TYPE (source)))
        return FALSE;

    auto continuation = g_new0 (GncOfxImportDestroyContinuation, 1);
    continuation->lifecycle = gnc_ofx_import_lifecycle_ref (lifecycle);
    continuation->callback = callback;
    continuation->user_data = user_data;
    continuation->registered = TRUE;
    g_weak_ref_init (&continuation->source, source);
    lifecycle->continuations = g_list_prepend (lifecycle->continuations,
                                                continuation);
    continuation->handler = g_signal_connect_data (
        source, "destroy",
        G_CALLBACK (ofx_import_destroy_continuation_called), continuation,
        ofx_import_destroy_continuation_closed,
        static_cast<GConnectFlags> (0));
    if (!continuation->handler)
    {
        lifecycle->continuations = g_list_remove (lifecycle->continuations,
                                                   continuation);
        continuation->registered = FALSE;
        ofx_import_destroy_continuation_closed (continuation, nullptr);
        return FALSE;
    }
    return TRUE;
}
