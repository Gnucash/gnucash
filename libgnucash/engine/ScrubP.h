/********************************************************************\
 * ScrubP.h -- Internal scrub operation contracts                   *
 *                                                                  *
 * Copyright (C) 2026 GnuCash contributors                          *
\********************************************************************/

#ifndef XACC_SCRUB_P_H
#define XACC_SCRUB_P_H

#include "Scrub.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Validate explicit authority for a mutation of @a book. */
gboolean gnc_scrub_context_validate_for_book (const GncScrubContext *context,
                                               const QofBook *book,
                                               const char *operation);

/* Reject a legacy scrub only when a SCRUB lease owns the same current book. */
gboolean gnc_scrub_legacy_operation_allowed (const QofBook *book,
                                              const char *operation);

/* Central Transaction commit decision. TRUE means the GUID was accepted by
 * the active book-bound deferral queue and the synchronous hook must not run. */
gboolean gnc_scrub_defer_commit_hook (QofBook *book, const GncGUID *guid,
                                      GncScrubDeferredCommitKind kind);

/* Transaction commit calls this invariant directly; it isn't a public
 * operation entry and must not borrow a GUI context across re-entrant turns. */
void xaccTransScrubImbalanceInternal (Transaction *trans, Account *root,
                                      Account *parent,
                                      GncScrubContext *context);

/* Transaction commit invokes the lot invariant through this entry. */
gboolean xaccScrubLotInternal (GNCLot *lot,
                               GncScrubContext *context);

#ifdef __cplusplus
}
#endif

#endif /* XACC_SCRUB_P_H */
