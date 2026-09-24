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
 * gnc-ofx-import-teardown.h -- OFX workflow lifecycle controller   *
 ********************************************************************/
#ifndef GNC_OFX_IMPORT_TEARDOWN_H
#define GNC_OFX_IMPORT_TEARDOWN_H

#include "import-operation-teardown.h"

G_BEGIN_DECLS

typedef struct GncOfxImportLifecycle GncOfxImportLifecycle;
typedef struct GncOfxImportAsyncState GncOfxImportAsyncState;
typedef struct _main_matcher_info GNCImportMainMatcher;

typedef void (*GncOfxImportMetadataCleanupFunc) (
    GncOfxImportLifecycle *lifecycle,
    GncImportOperationTeardownResult result,
    gpointer user_data);

typedef void (*GncOfxImportDestroyContinuationFunc) (
    GObject *source,
    gpointer user_data);

/** Create the one lifecycle controller for an OFX workflow.
 *
 * The controller owns the asynchronous operation reference, the application
 * hold, and @a user_data. @a matcher_slot and @a transaction_slot remain part
 * of that payload but their book-owning contents are disposed centrally by
 * the controller's real terminal callback. Every asynchronous state or signal
 * continuation must hold a lifecycle reference.
 */
GncOfxImportLifecycle *gnc_ofx_import_lifecycle_new (
    GncSessionOperationContext *context,
    GApplication *application,
    GNCImportMainMatcher **matcher_slot,
    GList **transaction_slot,
    GncOfxImportMetadataCleanupFunc metadata_cleanup,
    gpointer user_data,
    GDestroyNotify user_data_destroy);

GncOfxImportLifecycle *gnc_ofx_import_lifecycle_ref (
    GncOfxImportLifecycle *lifecycle);
void gnc_ofx_import_lifecycle_unref (
    GncOfxImportLifecycle *lifecycle);

/** Coalesce matcher, parent, shutdown, and explicit abort requests. */
gboolean gnc_ofx_import_lifecycle_request (
    GncOfxImportLifecycle *lifecycle);

/** Release the initial workflow reference after a successful normal finish.
 * No asynchronous state or reconcile continuation may still be registered. */
void gnc_ofx_import_lifecycle_finish (
    GncOfxImportLifecycle *lifecycle);

gboolean gnc_ofx_import_lifecycle_is_terminal (
    const GncOfxImportLifecycle *lifecycle);

GncSessionOperationContext *gnc_ofx_import_lifecycle_get_context (
    GncOfxImportLifecycle *lifecycle);
GncImportOperationTeardown *gnc_ofx_import_lifecycle_get_teardown (
    GncOfxImportLifecycle *lifecycle);

/** Register one outstanding account/commodity/new-book/verify callback. The
 * registration keeps the OFX payload alive after terminal cleanup; callbacks
 * must check is_active() before touching it and release exactly once. */
GncOfxImportAsyncState *gnc_ofx_import_async_state_new (
    GncOfxImportLifecycle *lifecycle);
void gnc_ofx_import_async_state_unref (
    GncOfxImportAsyncState *state);
gboolean gnc_ofx_import_async_state_is_active (
    const GncOfxImportAsyncState *state);
GncOfxImportLifecycle *gnc_ofx_import_async_state_get_lifecycle (
    GncOfxImportAsyncState *state);
gboolean gnc_ofx_import_async_state_request_teardown (
    GncOfxImportAsyncState *state);

/** Attach an exactly-once continuation to a GObject destroy signal. The
 * controller holds the payload alive until the signal is disconnected or
 * emitted. Terminal cleanup disconnects all remaining continuations before
 * payload cleanup, so a later window destroy cannot call stale OFX data. */
gboolean gnc_ofx_import_lifecycle_connect_destroy (
    GncOfxImportLifecycle *lifecycle,
    GObject *source,
    GncOfxImportDestroyContinuationFunc callback,
    gpointer user_data);

G_END_DECLS

#endif
