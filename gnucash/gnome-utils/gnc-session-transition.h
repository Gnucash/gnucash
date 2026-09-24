/* Copyright (C) 2026 GnuCash Developers
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 */

#ifndef GNC_SESSION_TRANSITION_H
#define GNC_SESSION_TRANSITION_H

#include <glib.h>

G_BEGIN_DECLS

typedef struct _GncSessionTransition GncSessionTransition;

typedef enum
{
    GNC_SESSION_TRANSITION_NEW,
    GNC_SESSION_TRANSITION_OPEN,
    GNC_SESSION_TRANSITION_SAVE,
    GNC_SESSION_TRANSITION_REVERT,
    GNC_SESSION_TRANSITION_QUIT
} GncSessionTransitionKind;

typedef enum
{
    GNC_SESSION_TRANSITION_REJECTED,
    GNC_SESSION_TRANSITION_STARTED,
    GNC_SESSION_TRANSITION_QUEUED
} GncSessionTransitionDisposition;

typedef void (*GncSessionTransitionStartFunc) (GncSessionTransition *transition,
                                               gpointer user_data);
typedef void (*GncSessionTransitionCancelFunc) (gpointer user_data);

/**
 * Queue a destructive session transition. This API is main-thread-only.
 *
 * @start may run synchronously before this function returns and may complete
 * @transition synchronously. Callers must therefore not rely on @user_data
 * remaining alive after a STARTED result. On REJECTED, ownership of @user_data
 * remains with the caller. @cancel is called only for an admitted, queued
 * request discarded by begin_shutdown().
 *
 * A queued quit rejects later requests until it is either completed (cancelled)
 * or begins shutdown. This prevents work from being stranded behind quit.
 */
GncSessionTransitionDisposition gnc_session_transition_enqueue (
    GncSessionTransitionKind kind,
    GncSessionTransitionStartFunc start,
    GncSessionTransitionCancelFunc cancel,
    gpointer user_data);

/** Complete the currently active transition exactly once.
 *
 * Completion may synchronously start the next queued request, so it must be
 * the caller's last action after releasing all request-local state.
 */
void gnc_session_transition_complete (GncSessionTransition *transition);

/** Seal admission and cancel queued work.
 *
 * If @transition is the active request it is consumed; this also supports a
 * file-open error decision that turns into Quit. If it is %NULL, an active
 * transition remains valid until its terminal callback completes it; this form
 * is used by unconditional UI shutdown.
 */
void gnc_session_transition_begin_shutdown (GncSessionTransition *transition);
gboolean gnc_session_transition_quit_pending (void);

G_END_DECLS

#endif /* GNC_SESSION_TRANSITION_H */
