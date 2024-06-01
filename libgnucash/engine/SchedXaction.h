/********************************************************************\
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
/**
 * @addtogroup Engine
 * @{ */
/**
   @addtogroup SchedXaction Scheduled/Periodic/Recurring Transactions

   Scheduled Transactions provide a framework for remembering
   information about a transactions that are set to occur in the
   future, either once or periodically.
 @{ */
/**
 * @file SchedXaction.h
 * @brief Scheduled Transactions public handling routines.
 * @author Copyright (C) 2001 Joshua Sled <jsled@asynchronous.org>
*/

#ifndef XACC_SCHEDXACTION_H
#define XACC_SCHEDXACTION_H

typedef struct _SchedXactionClass SchedXactionClass;

#include <time.h>
#include <glib.h>
#include "qof.h"
#include "Recurrence.h"
#include "gnc-engine.h"

#ifdef __cplusplus
extern "C" {
#endif

/* --- type macros --- */
#define GNC_TYPE_SCHEDXACTION            (gnc_schedxaction_get_type ())
#define GNC_SCHEDXACTION(o)              \
     (G_TYPE_CHECK_INSTANCE_CAST ((o), GNC_TYPE_SCHEDXACTION, SchedXaction))
#define GNC_SCHEDXACTION_CLASS(k)        \
     (G_TYPE_CHECK_CLASS_CAST((k), GNC_TYPE_SCHEDXACTION, SchedXactionClass))
#define GNC_IS_SCHEDXACTION(o)           \
     (G_TYPE_CHECK_INSTANCE_TYPE ((o), GNC_TYPE_SCHEDXACTION))
#define GNC_IS_SCHEDXACTION_CLASS(k)     \
     (G_TYPE_CHECK_CLASS_TYPE ((k), GNC_TYPE_SCHEDXACTION))
#define GNC_SCHEDXACTION_GET_CLASS(o)    \
     (G_TYPE_INSTANCE_GET_CLASS ((o), GNC_TYPE_SCHEDXACTION, SchedXactionClass))
GType gnc_schedxaction_get_type(void);

#define GNC_IS_SX(obj)  GNC_IS_SCHEDXACTION(obj)
#define GNC_SX(obj)     GNC_SCHEDXACTION(obj)

typedef struct _SchedXaction SchedXaction;

/**
 * A single scheduled transaction.
 *
 * Scheduled transactions have a list of transactions, and a frequency
 * [and associated date anchors] with which they are scheduled.
 *
 * Things that make sense to have in a template transaction:
 *   [not] Date [though eventually some/multiple template transactions
 *               might have relative dates].
 *   Memo
 *   Account
 *   Funds In/Out... or an expr involving 'amt' [A, x, y, a?] for
 *     variable expenses.
 *
 * Template transactions are instantiated by:
 *  . copying the fields of the template
 *  . setting the date to the calculated "due" date.
 *
 * We should be able to use the GeneralJournal [or, yet-another-subtype
 * of the internal ledger] for this editing.
 **/
struct _SchedXaction
{
    QofInstance     inst;
    gchar           *name;

    GList           *schedule;

    GDate           last_date;

    GDate           start_date;
    /* if end_date is invalid, then no end. */
    GDate           end_date;

    /* if num_occurances_total == 0, then no limit */
    gint            num_occurances_total;
    /* remaining occurrences are as-of the 'last_date'. */
    gint            num_occurances_remain;

    /* the current instance-count of the SX. */
    gint            instance_num;

    gboolean        enabled;
    gboolean        autoCreateOption;
    gboolean        autoCreateNotify;
    gint            advanceCreateDays;
    gint            advanceRemindDays;

    Account        *template_acct;

    /** The list of deferred SX instances.  This list is of SXTmpStateData
     * instances.  */
    GList /* <SXTmpStateData*> */ *deferredList;
};

struct _SchedXactionClass
{
    QofInstanceClass parent_class;
};

/** Just the variable temporal bits from the SX structure. */
typedef struct _SXTmpStateData
{
    GDate last_date;
    gint num_occur_rem;
    gint num_inst;
} SXTmpStateData;

#define xaccSchedXactionSetGUID(X,G) qof_instance_set_guid(QOF_INSTANCE(X),(G))

/**
 * Creates and initializes a scheduled transaction.
*/
SchedXaction *xaccSchedXactionMalloc(QofBook *book);

void sx_set_template_account (SchedXaction *sx, Account *account);

/**
 * Cleans up and frees a SchedXaction and its associated data.
*/
void xaccSchedXactionDestroy( SchedXaction *sx );

void gnc_sx_begin_edit (SchedXaction *sx);
void gnc_sx_commit_edit (SchedXaction *sx);

/** @return GList<Recurrence*> **/
/*@ dependent @*/
GList* gnc_sx_get_schedule(const SchedXaction *sx);
/** @param[in] schedule A GList<Recurrence*> **/
void gnc_sx_set_schedule(SchedXaction *sx, /*@ null @*//*@ only @*/ GList *schedule);

gchar *xaccSchedXactionGetName( const SchedXaction *sx );
/**
 * A copy of the name is made.
*/
void xaccSchedXactionSetName( SchedXaction *sx, const gchar *newName );

const GDate* xaccSchedXactionGetStartDate(const SchedXaction *sx );
time64 xaccSchedXactionGetStartDateTT(const SchedXaction *sx );
void xaccSchedXactionSetStartDate( SchedXaction *sx, const GDate* newStart );
void xaccSchedXactionSetStartDateTT( SchedXaction *sx, const time64 newStart );

int xaccSchedXactionHasEndDate( const SchedXaction *sx );
/**
 * Returns invalid date when there is no end-date specified.
*/
const GDate* xaccSchedXactionGetEndDate(const SchedXaction *sx );
/**
 * Set to an invalid GDate to turn off 'end-date' definition.
*/
void xaccSchedXactionSetEndDate( SchedXaction *sx, const GDate* newEnd );

const GDate* xaccSchedXactionGetLastOccurDate(const SchedXaction *sx );
time64 xaccSchedXactionGetLastOccurDateTT(const SchedXaction *sx );
void xaccSchedXactionSetLastOccurDate( SchedXaction *sx, const GDate* newLastOccur );
void xaccSchedXactionSetLastOccurDateTT( SchedXaction *sx, const time64 newLastOccur );

/**
 * Returns true if the scheduled transaction has a defined number of
 * occurrences, false if not.
*/
gboolean xaccSchedXactionHasOccurDef( const SchedXaction *sx );
gint xaccSchedXactionGetNumOccur( const SchedXaction *sx );
/**
 * Set to '0' to turn off number-of-occurrences definition.
*/
void xaccSchedXactionSetNumOccur( SchedXaction *sx, gint numNum );
gint xaccSchedXactionGetRemOccur( const SchedXaction *sx );
void xaccSchedXactionSetRemOccur( SchedXaction *sx, gint numRemain );

/** Calculates and returns the number of occurrences of the given SX
 * in the given date range (inclusive). */
gint gnc_sx_get_num_occur_daterange(const SchedXaction *sx, const GDate* start_date, const GDate* end_date);

/** \brief Get the instance count.
 *
 *   This is incremented by one for every created
 * instance of the SX.  Returns the instance num of the SX unless stateData
 * is non-null, in which case it returns the instance num from the state
 * data.
 * @param sx The instance whose state should be retrieved.
 * @param stateData may be NULL.
*/
gint gnc_sx_get_instance_count( const SchedXaction *sx, /*@ null @*/ SXTmpStateData *stateData );
/**
 * Sets the instance count to something other than the default.  As the
 * default is the incorrect value '0', callers should DTRT here.
*/
void gnc_sx_set_instance_count( SchedXaction *sx, gint instanceNum );

/* must be g_list_freed */
GList *xaccSchedXactionGetSplits( const SchedXaction *sx );
void xaccSchedXactionSetSplits( SchedXaction *sx, GList *newSplits );

gboolean xaccSchedXactionGetEnabled( const SchedXaction *sx );
void xaccSchedXactionSetEnabled( SchedXaction *sx, gboolean newEnabled );

void xaccSchedXactionGetAutoCreate( const SchedXaction *sx,
                                    /*@ out @*/ gboolean *outAutoCreate,
                                    /*@ out @*/ gboolean *outNotify );
void xaccSchedXactionSetAutoCreate( SchedXaction *sx,
                                    gboolean newAutoCreate,
                                    gboolean newNotify );

gint xaccSchedXactionGetAdvanceCreation( const SchedXaction *sx );
void xaccSchedXactionSetAdvanceCreation( SchedXaction *sx, gint createDays );

gint xaccSchedXactionGetAdvanceReminder( const SchedXaction *sx );
void xaccSchedXactionSetAdvanceReminder( SchedXaction *sx, gint reminderDays );

/** \name Temporal state data.
 *
 * These functions allow us to opaquely save the entire temporal state of
 * ScheduledTransactions.  This is used by the "since-last-run" dialog to
 * store the initial state of SXes before modification ... if it later
 * becomes necessary to revert an entire set of changes, we can 'revert' the
 * SX without having to rollback all the individual state changes.
@{
*/
/** Allocates a new SXTmpStateData object and fills it with the
 * current state of the given sx.
 */
SXTmpStateData *gnc_sx_create_temporal_state(const SchedXaction *sx );

/** Calculates the next occurrence of the given SX and stores that
 * occurrence in the remporalStateDate. The SX is unchanged. */
void gnc_sx_incr_temporal_state(const SchedXaction *sx, SXTmpStateData *stateData );

/** Frees the given stateDate object. */
void gnc_sx_destroy_temporal_state( SXTmpStateData *stateData );

/** \brief Allocates and returns a one-by-one copy of the given
 * temporal state.
 *
 * The caller must destroy the returned object with
 * gnc_sx_destroy_temporal_state() after usage.
*/
SXTmpStateData *gnc_sx_clone_temporal_state( SXTmpStateData *stateData );
/** @} */

/** \brief Returns the next occurrence of a scheduled transaction.
 *
 *   If the transaction hasn't occurred, then it's based off the start date.
 * Otherwise, it's based off the last-occurrence date.
 *
 * If state data is NULL, the current value of the SX is used for
 * computation.  Otherwise, the values in the state data are used.  This
 * allows the caller to correctly create a set of instances into the future
 * for possible action without modifying the SX state until action is
 * actually taken.
*/
GDate xaccSchedXactionGetNextInstance(const SchedXaction *sx,
                                      SXTmpStateData *stateData);

/** \brief Adds an instance to the deferred list of the SX.

Added instances are added in date-sorted order.
*/
void gnc_sx_add_defer_instance( SchedXaction *sx, void *deferStateData );

/** \brief Removes an instance from the deferred list.

If the instance is no longer useful; gnc_sx_destroy_temporal_state() it.
*/
void gnc_sx_remove_defer_instance( SchedXaction *sx, void *deferStateData );

/** \brief Returns the defer list from the SX.

 This is a date-sorted state-data instance list.
 The list should not be modified by the caller; use the
 gnc_sx_{add,remove}_defer_instance() functions to modify the list.
*/
GList *gnc_sx_get_defer_instances( SchedXaction *sx );

/* #defines for Properties and GncModule */
#define GNC_SX_SHARES                "shares"
#define GNC_SX_FREQ_SPEC             "scheduled-frequency"
#define GNC_SX_NAME                  "sched-xname"
#define GNC_SX_START_DATE            "sched-start-date"
#define GNC_SX_LAST_DATE             "sched-last-date"
#define GNC_SX_NUM_OCCUR             "sx-total-number"
#define GNC_SX_REM_OCCUR             "sx-remaining-num"

/** \brief QOF registration. */
gboolean SXRegister (void);

/** \deprecated */
#define xaccSchedXactionIsDirty(X) qof_instance_is_dirty (QOF_INSTANCE(X))
/** \deprecated */
#define xaccSchedXactionGetGUID(X) qof_entity_get_guid(QOF_INSTANCE(X))

#ifdef __cplusplus
}
#endif

#endif /* XACC_SCHEDXACTION_H */

/** @} */
/** @} */

/** \page loanhandling Handling loan repayment in GnuCash::Scheduled Transactions
 * \sa The original email thread at <https://lists.gnucash.org/pipermail/gnucash-devel/2002-July/006438.html>.
 *
 * July, 2002 - jsled@asynchronous.org
 *
 * API: \ref SchedXaction
 *
 * We define loan repayment values in the following terms:
 *
 * Identifiers:\n
 * P  : the original principal.  This is the overall principal afforded by the
 *     loan at the time of it's creation.\n
 * P' : The beginning principal.  This is the principal at the time of entry
 *     into GnuCash.\n
 * I :  The interest rate associated with the loan.  Note that this may change
 *     over time [based on an addition to the Prime rate, for instance], at
 *     various frequencies [yearly, monthly, quarterly...].  Ideally, we can
 *     use the FreqSpec mechanism to facilitate the interest rate adjustment.\n
 * N  : The length of the loan in periods.\n
 * m  : The minimum periodic payment.\n
 * n  : The current period of the repayment.
 *
 * Functions:\n
 * PMT  : Total equal periodic payment, as per Gnumeric/Excel's definitions
 *       [see end for more detail].\n
 * IPMT : Monthly payment interest portion,  ""\n
 * PPMT : Monthly payment principal portion, ""
 *
 * [ NOTE: 'PMT(I,N,P) = IPMT(I, n, N, P) + PPMT(I, n, N, P)' for 0 <= n < N ]
 *
 *
 * The formula entered into the SX template for a loan may then look like:
 *
 * Example 1:
 * \verbatim
 * Desc/Memo |                     Account |         Credit |           Debit
 * ----------+-----------------------------+----------------+-------------------
 * Repayment | Assets:Bank:Checking        |                | =PMT(I,n,N,P)
 *          |                             |                |  + fixed_amt
 * Interest  | Expenses:Loan_Name:Interest | =IPMT(I,n,N,P) |
 * PMI       | Expenses:Loan_Name:Misc     | fixed_amt      |
 * Principal | Liabilities:Loan_Name       | =PPMT(I,n,N,P) |
 * -----------------------------------------------------------------------------
 * \endverbatim
 *
 * Or, in the case where an escrow account is involved [with thanks to warlord
 * for the review and fixes]:
 *
 * Example 2:
 * \verbatim
 * Desc/Memo      |             Account         |       Credit   |       Debit
 * ---------------+-----------------------------+----------------+--------------
 * Repayment      | Assets:Bank:Checking        |                | =PMT(I,n,N,P)
 *               |                             |                | + escrow_amt
 *               |                             |                | + fixed_amt
 *               |                             |                | + pre_payment
 * Escrow         | Assets:Loan_Escrow_acct     | escrow_amt     |
 * Interest       | Expenses:Loan_Name:Interest | =IPMT(I,n,N,P) |
 * PMI            | Expenses:Loan_Name:Misc     | fixed_amt      |
 * Principal      | Liabilities:Loan_Name       | =PPMT(I,n,N,P) |
 *               |                             | + pre_payment  |
 * \endverbatim
 *
 * FreqSpec = 1 month
 * \verbatim
 * -----------------------------------------------------------------------------
 *
 * Desc/Memo      |             Account         |       Credit   |       Debit
 * ---------------+-----------------------------+----------------+--------------
 * Insurance      | Assets:Loan_Escrow_acct     |                | insurance_amt
 * Insurance      | Expenses:Home_Insurance     | insurance_amt  |
 * \endverbatim
 *
 * FreqSpec = 1 year
 * \verbatim
 * -----------------------------------------------------------------------------
 * Desc/Memo      |             Account         |       Credit   |       Debit
 * ---------------+-----------------------------+----------------+--------------
 * Taxes          | Assets:Loan_Escrow_acct     |                | taxes_amt
 * Taxes          | Expenses:Property_Taxes     | taxes_amt      |
 * FreqSpec = Quarterly
 * -----------------------------------------------------------------------------
 * \endverbatim
 *
 *
 * \section guidpractical Practical questions regarding the implementation of this facility are:
 *
 * | 1. The transactions as in Example 2 are not going to be scheduled for the\n
 * |    same day; are their values linked at all / do they need to share the\n
 * |    same var bindings?
 *
 * Yes, they would want to be linked.  More precisely, the insurance/tax amounts
 * are very likely linked to the escrow_amt in Ex.2.  Unfortunately, these are
 * very likely separate SXes...
 *
 * -----
 * | 2. How does this effect the SX implementation of variables?
 *
 * Vastly.
 *
 * It becomes clear that multiple SXes will be related.  While they'll have
 * separate FreqSpecs and template transactions, they'll share some state.  For
 * both visualization [i.e., the SX list] and processing [credit/debit cell
 * value computation] we'll want some manner of dealing with this.
 *
 * It becomes clear as well that the nature of variables and functions needs to
 * be more clearly defined with respect to these issues.  We probably want to
 * institute a clear policy for the scoping of variables.  As well, since the
 * SXes will have different instantiation dates, we'll need a method and
 * implementation for the relation of SXes to each other.
 *
 * A substantial hurdle is that if a set of SXes are [strongly] related, there
 * is no-longer a single instantiation date for a set of related SXes.  In fact,
 * there may be different frequencies of recurrence.
 *
 * One option -- on the surface -- to relate them would be to maintain an
 * instance variable-binding frame cache, which would store user-entered and
 * computed variable bindings.  The first instantiated SX of the set would create
 * the frame, and the "last" instance would clean it up.  First "last" instance
 * is defined by the last-occurring SX in a related set, in a given time range.
 *
 * For example: a loan SX-set is defined by two monthly SXes ["repayment" and
 * "insurance"], and a quarterly "tax" SX.  The first monthly SX would create a
 * frame, which would be passed two the second monthly SX.  This would occur for
 * the 3 months of interest.  The Quarterly SX would get all 3 frames for it's
 * creation, and use them in an /appropriate/ [read: to be defined through a lot
 * of pain] way.  As the time-based dependency relationship between the frames
 * plays out, the frame can be removed from the system.
 *
 * Another option is to toss this idea entirely and instead let the user DTRT
 * manually.
 *
 * A related option is to add the necessary grouping mechanism to the SX
 * storage/data structure: immediately allowing visual grouping of related SXes,
 * and potentially allowing a storage place for such frame data in the future
 * with less file-versioning headache.  This is the option that will be pursued.
 *
 * Another element implicit in the original requirements to support
 * loans/repayment calculations is implicit variables.  These are symbolic names
 * which can be used and are automagically bound to values.  The known implicit
 * variables to support loan/repayment are:
 *
 * P [loan principal amount], N [loan repayment periods], I [interest], m
 * [minimum payment] and n [current period].  Some of these [P, N, I, m] are
 * fixed over many instances; some [n] are rebound specific to the instance.
 * See the 'variable-scope-frame' below for a method of handling these
 * variables.
 *
 * And yet-another element implicit in the original requirement is support for
 * detecting and computing the result of functions in the template transaction's
 * credit/debit cells.  Changes to the src/app-utils/gnc-exp-parser.[hc] and
 * src/calculation/expression_parser.[ch] to support functions would be
 * necessitated.  It is conceivable that after parsing, the parsed expression
 * could be passed to scheme for evaluation.  Hopefully this would make it
 * easier to add support for new functions to the SX code via Scheme.
 *
 *
 * | 3. How do we deal with periodic [yearly, semi-yearly] updating of various\n
 * |    "fixed" variables?
 *
 * Another change in the way variables are used is that some SXes -- especially
 * loan-repayment -- may involve variables which are not tied to the instance of
 * the SX, but rather to variables which:
 * - are also involved in another SX
 * - change with a frequency different than the SX
 * - are represented by a relationship to the outside world ["prime + 1.7"]
 *
 * A partial fix for this problem is to provide multiple levels of scope for
 * variable bindings, and expose this to the user by a method of assigning
 * [perhaps time-dependent] values to these variables.  Variables bound in this
 * manner would absolve the user of the need to bind them at SX-creation time.
 *
 * An added benefit of this would be to allow some users [see Bug#85707] have
 * "fixed variable" values for a group of SXes.
 *
 * In combination with the SX Grouping, this would provide most of a fix for the
 * problem described in #2, above.  The variable_frame could be used to provide
 * the shared-state between related SXes, without imposing quite the same
 * burden.  This approach is slightly less flexible, but that allows it to be
 * implemented more readily, and understood more easily.
 *
 * A question which comes up when thinking about yearly-changing values such as
 * interest rates is if the historical information needs to be versioned.  For
 * now, we punt on this issue, but hopefully will provide enough of a framework
 * for this to be reasonably added in the future.
 *
 * We define four types of variables supported by this scheme:
 *
 * implicit  : provided only by the system
 *            e.g.: 'n', the current index of the repayment
 *
 * transient : have user-defined values, bound at instantiation time.
 *            e.g.: existing ad-hoc variables in SXes.
 *
 * static    : have a user-defined values, and are not expected to change with
 *            any measurable frequency.  The user may change these at their
 *            leisure, but no facility to assist or encourage this is
 *            provided.
 *            e.g.: paycheck amount, loan principal amount
 *
 * periodic  : have user-defined values which change at specific points in
 *            time [July 1, yearly].  After the expiration of a variable value,
 *            it's re-binding will prevent any dependent SXes from being
 *            created.
 *            e.g.: loan tax amount, loan interest rate
 *
 * | 4. From where do we get the dollar amount against which to do the [PI]PMT\n
 * |    calculation?
 *
 * The user will specify the parameters of the Loan via some UI... then where
 * does the data go?
 *
 * - KVP data for that account?
 * - KVP data for the SX?
 * - Do we have a different top-level "Loan" object?
 * - Present only in the SX template transactions/variable-frames?
 *
 * I believe that the only location of the data after Druid creation is in the
 * variable-binding frames and the formulae in the template transactions.  The
 * Druid would thus simply assist the user in creating the following SX-related
 * structures:
 *
 * - SXGroup: Loan Repayment
 *	- variable_frame
 *		 - P [static]
 *		 - N [static]
 *		 - n [implicit]
 *		 - I [periodic]
 *		 - pmi_amount [periodic]
 *		 - tax_amount [periodic]
 *		 - pre_payment [periodic]
 *		 - insurance_amount [periodic]
 * - SX: Payment
 *	 - Bank -> { Escrow,
 *                Liability:Loan:Principal,
 *                Expense:Loan:Interest,
 *                Expense:Loan:Insurance }
 * - SX: Tax
 *	 - Escrow -> Expense:Tax
 * - SX: Insurance
 *	 - Escrow -> Expense:Insurance
 *
 *
 * \section loansreference Reference
 *
 *
 * \subsection loanssoftware Other software:
 *
 * Gnumeric supports the following functions WRT payment calculation:
 *
 * - PMT( rate, nper, pv [, fv, type] )
 *  PMT returns the amount of payment for a loan based on a constant interest
 *  rate and constant payments (ea. payment equal).
 *  @rate : constant interest rate
 *  @nper : overall number of payments
 *  @pv   : present value
 *  @fv   : future value
 *  @type : payment type
 *	 - 0 : end of period
 *	 - 1 : beginning of period
 *
 * - IPMT( rate, per, nper, pv, fv, type )
 *  IPMT calculates the amount of a payment of an annuity going towards
 *  interest. Formula for IPMT is:
 *  IPMT(per) = - principal(per-1) * interest_rate
 *  where:
 *  principal(per-1) = amount of the remaining principal from last period.
 *
 * - ISPMT( rate, per, nper, pv )
 *  ISPMT returns the interest paid on a given period.
 *  If @per < 1 or @per > @nper, returns #NUM! err.
 *
 * - PPMT(rate, per, nper, pv [, fv, type] )
 *  PPMT calculates the amount of a payment of an annuity going towards
 *  principal.
 *  PPMT(per) = PMT - IPMT(per)
 *  where: PMT is payment
 *	- IPMT is interest for period per
 *
 * - PV( rate, nper, pmt [, fv, type] )
 *  Calculates the present value of an investment
 *  @rate : periodic interest rate
 *  @nper : number of compounding periods
 *  @pmt  : payment made each period
 *  @fv   : future value
 *
 *
 */

