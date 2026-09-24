/********************************************************************\
 * import-operation-teardown.h -- asynchronous import teardown owner *
 * Copyright (C) 2026                                               *
 ********************************************************************/
#ifndef GNC_IMPORT_OPERATION_TEARDOWN_H
#define GNC_IMPORT_OPERATION_TEARDOWN_H

#include <gio/gio.h>

#include "gnc-session.h"

G_BEGIN_DECLS

typedef struct GncImportOperationTeardown GncImportOperationTeardown;

typedef enum
{
    /** The callback owns a short IMPORT cleanup lease and may mutate the book. */
    GNC_IMPORT_OPERATION_TEARDOWN_MUTATION_ALLOWED,
    /** The captured session or book is no longer current; only references may be freed. */
    GNC_IMPORT_OPERATION_TEARDOWN_STALE,
    /** GApplication is shutting down while a current-book foreign lease remains.
     * Book objects remain owned by the current QofBook and must not be mutated;
     * the callback must release only its non-owning GUI/import metadata. */
    GNC_IMPORT_OPERATION_TEARDOWN_BOOK_SHUTDOWN
} GncImportOperationTeardownResult;

typedef void (*GncImportOperationTeardownFunc) (
    GncImportOperationTeardown *owner,
    GncImportOperationTeardownResult result,
    gpointer user_data);

/** Create the single terminal owner for one asynchronous import workflow.
 *
 * @a application is mandatory: a workflow that can cross main-loop turns must
 * have an application shutdown boundary. The owner takes references to the
 * application and operation context and owns @a user_data until final unref.
 * It holds the application from construction through normal release or
 * terminal completion; application shutdown always terminalizes the owner,
 * even if no explicit abort was requested first.
 */
GncImportOperationTeardown *gnc_import_operation_teardown_new (
    GncSessionOperationContext *context,
    GApplication *application,
    GncImportOperationTeardownFunc callback,
    gpointer user_data,
    GDestroyNotify user_data_destroy);

GncImportOperationTeardown *gnc_import_operation_teardown_ref (
    GncImportOperationTeardown *owner);
void gnc_import_operation_teardown_unref (
    GncImportOperationTeardown *owner);

/** Request terminal cleanup. Repeated requests are coalesced. The callback is
 * invoked exactly once, synchronously when possible or from the owner's sole
 * timeout source after a foreign synchronous lease is released. */
gboolean gnc_import_operation_teardown_request (
    GncImportOperationTeardown *owner);

gboolean gnc_import_operation_teardown_is_requested (
    const GncImportOperationTeardown *owner);
gboolean gnc_import_operation_teardown_is_complete (
    const GncImportOperationTeardown *owner);

/** Return whether the owner currently has its sole retry source registered. */
gboolean gnc_import_operation_teardown_has_pending_retry (
    const GncImportOperationTeardown *owner);

/** Borrow the context. The pointer remains valid while an owner reference is held. */
GncSessionOperationContext *gnc_import_operation_teardown_get_context (
    GncImportOperationTeardown *owner);

G_END_DECLS

#endif
