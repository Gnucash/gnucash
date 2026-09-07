/********************************************************************\
 * gnc-reconciled-balance.h -- Reconciled balances public interface.   *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

/** @addtogroup Engine
    @{ */
/** @addtogroup ReconciledBalance Reconciled Balances
    @{ */
/** @file gnc-reconciled-balance.h
 *  @brief A seal on what the book said at a date, checked again as the
 *  book changes
 *
 *  Finishing a reconciliation tells you the past is right.  Nothing then
 *  watches it.  A transaction dated inside that period can be deleted,
 *  edited, entered late, or arrive twice from a CSV or OFX import, and
 *  the book will not mention it again.
 *
 *  A record seals it: account @a A had balance @a B as of the end of day
 *  @a D, where @a B is what the book itself computed at the moment the
 *  record was made.  One is written automatically when a reconciliation
 *  finishes; they can also be added by hand as checkpoints.
 *
 *  What it does and does not tell you:
 *
 *  - It detects *any* change to what the account held on or before @a D,
 *    whatever caused it -- the register, an import, a script, the python
 *    bindings, a hand-edited file, a restored backup.  It rests only on
 *    post dates and amounts, so no code path can slip past it and no
 *    metadata rewrite can falsify it.
 *
 *  - It cannot tell you whether the change was wanted.  A duplicate
 *    import, a back-dated typo and a cheque you wrote in January and
 *    entered in March are the same event to it: a split dated on or
 *    before @a D appeared after the seal was made.  Intent is not in the
 *    data.  So a broken record means "go and look", never "something is
 *    wrong".
 *
 *  - When the change was legitimate, the record is re-sealed at the new
 *    balance (gnc_reconciled_balance_reseal()).  One back-dated
 *    transaction breaks every record dated on or after it, all by the
 *    same amount -- which is itself the signature of a single late entry,
 *    as against the scattered deltas of real damage.
 *
 *  It enforces nothing.  It marks no split, locks nothing, and never
 *  stops a transaction being entered, edited or deleted.
 *
 *  Other semantics:
 *
 *  - The amount is in the account's own commodity and covers that
 *    account alone; sub-account balances are not included.
 *
 *  - The amount is stored with the engine's internal sign.
 *    Presentation layers that show balances with the "reversed balance"
 *    convention (income, credit card, liability, equity ...) must apply
 *    gnc_reverse_balance() themselves on the way in and out.
 *
 *  - The date is stored day-neutral (like a transaction post date) and
 *    the balance is taken at the end of that day.
 */

#ifndef GNC_RECONCILED_BALANCE_H
#define GNC_RECONCILED_BALANCE_H

#include <glib.h>

/** The reconciled balance data. */
typedef struct reconciled_balance_s GncReconciledBalance;
typedef struct _GncReconciledBalanceClass GncReconciledBalanceClass;

#include "qof.h"
#include "Account.h"

#ifdef __cplusplus
extern "C"
{
#endif

/* --- type macros --- */
#define GNC_TYPE_RECONCILED_BALANCE            (gnc_reconciled_balance_get_type ())
#define GNC_RECONCILED_BALANCE(o)              \
     (G_TYPE_CHECK_INSTANCE_CAST ((o), GNC_TYPE_RECONCILED_BALANCE, GncReconciledBalance))
#define GNC_RECONCILED_BALANCE_CLASS(k)        \
     (G_TYPE_CHECK_CLASS_CAST((k), GNC_TYPE_RECONCILED_BALANCE, GncReconciledBalanceClass))
#define GNC_IS_RECONCILED_BALANCE(o)           \
     (G_TYPE_CHECK_INSTANCE_TYPE ((o), GNC_TYPE_RECONCILED_BALANCE))
#define GNC_IS_RECONCILED_BALANCE_CLASS(k)     \
     (G_TYPE_CHECK_CLASS_TYPE ((k), GNC_TYPE_RECONCILED_BALANCE))
#define GNC_RECONCILED_BALANCE_GET_CLASS(o)    \
     (G_TYPE_INSTANCE_GET_CLASS ((o), GNC_TYPE_RECONCILED_BALANCE, GncReconciledBalanceClass))

GType gnc_reconciled_balance_get_type (void);

/** The outcome of checking a record against the book. */
typedef enum
{
    /** The record cannot be checked: it has no account, or the account
     *  it referred to has been deleted. */
    GNC_RECONCILED_BALANCE_UNKNOWN,
    /** The account still reconciles to the recorded figure. */
    GNC_RECONCILED_BALANCE_HOLDS,
    /** It no longer does: a split covered by that statement has been
     *  edited, deleted or un-reconciled since. */
    GNC_RECONCILED_BALANCE_BROKEN,
} GncReconciledBalanceStatus;

/** Register the reconciled balance object with the engine.  Called from
 *  cashobjects_register(). */
gboolean gnc_reconciled_balance_register (void);

/** Create a new, empty record in @a book.  The caller is expected to
 *  set an account, a date and an amount. */
GncReconciledBalance *gnc_reconciled_balance_new (QofBook *book);

/** Remove a record from its book and free it. */
void gnc_reconciled_balance_destroy (GncReconciledBalance *ba);

void gnc_reconciled_balance_begin_edit (GncReconciledBalance *ba);
void gnc_reconciled_balance_commit_edit (GncReconciledBalance *ba);

const GncGUID *gnc_reconciled_balance_get_guid (const GncReconciledBalance *ba);

/** The account the record is about, or NULL if it is unset or has
 *  since been deleted. */
Account *gnc_reconciled_balance_get_account (const GncReconciledBalance *ba);
void gnc_reconciled_balance_set_account (GncReconciledBalance *ba, Account *acc);

/** The statement date.  Stored day-neutral; the balance is taken at
 *  the end of that day. */
time64 gnc_reconciled_balance_get_date (const GncReconciledBalance *ba);
void gnc_reconciled_balance_set_date (GncReconciledBalance *ba, time64 date);

/** The recorded balance, in the account's commodity and with the
 *  engine's internal sign.  See the file comment. */
gnc_numeric gnc_reconciled_balance_get_amount (const GncReconciledBalance *ba);
void gnc_reconciled_balance_set_amount (GncReconciledBalance *ba, gnc_numeric amount);

/** A free-text note, e.g. the statement number the figure came from. */
const char *gnc_reconciled_balance_get_notes (const GncReconciledBalance *ba);
void gnc_reconciled_balance_set_notes (GncReconciledBalance *ba, const char *notes);

/** The reconciled balance the book actually has for the account as of
 *  the recorded date.  gnc_numeric_zero() if the account is gone. */
gnc_numeric gnc_reconciled_balance_get_actual (const GncReconciledBalance *ba);

/** actual - recorded.  Zero when the record still holds. */
gnc_numeric gnc_reconciled_balance_get_delta (const GncReconciledBalance *ba);

GncReconciledBalanceStatus gnc_reconciled_balance_get_status (const GncReconciledBalance *ba);

/** Convenience wrapper: TRUE only for GNC_RECONCILED_BALANCE_BROKEN, so
 *  a record whose account has been deleted does not raise an alarm. */
gboolean gnc_reconciled_balance_is_broken (const GncReconciledBalance *ba);

/** The balance @a acc actually has as of the end of the day of @a date:
 *  the quantity a record seals.  Exposed so that the GUI can propose a
 *  figure without duplicating the rule. */
gnc_numeric gnc_reconciled_balance_compute (Account *acc, time64 date);

/** Re-seal @a rb at the balance the book has now, for when the change
 *  that broke it was a legitimate one.  Returns the figure it used to
 *  hold, so a caller can mention it; the record itself keeps no history
 *  of having been re-sealed. */
gnc_numeric gnc_reconciled_balance_reseal (GncReconciledBalance *rb);

GncReconciledBalance *gnc_reconciled_balance_lookup (const GncGUID *guid,
                                                   const QofBook *book);

/** All records in the book, oldest date first.  Free the list with
 *  g_list_free(); the records themselves belong to the book. */
GList *gnc_reconciled_balance_get_all (const QofBook *book);

/** The records for one account, oldest date first.  Free the list
 *  with g_list_free(). */
GList *gnc_reconciled_balance_get_for_account (const Account *acc);

/** The broken records in the book, oldest date first.  Free the list
 *  with g_list_free(). */
GList *gnc_reconciled_balance_get_broken (const QofBook *book);

/** The number of broken records in the book.  Each record that has not
 *  been checked since the last change to the book walks its account's
 *  splits, so this is not free after an edit; results are memoised until
 *  the next change. */
guint gnc_reconciled_balance_count_broken (const QofBook *book);

/** The number of records for @a acc that are broken.  Same cost note
 *  as gnc_reconciled_balance_count_broken(). */
guint gnc_reconciled_balance_count_broken_for_account (const Account *acc);

#ifdef __cplusplus
}
#endif

#endif /* GNC_RECONCILED_BALANCE_H */
/** @} */
/** @} */
