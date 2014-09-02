/********************************************************************\
 * gncOwner.h -- Business Interface:  Object OWNERs                 *
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
/** @addtogroup Business
    @{ */
/** @addtogroup Owner
    @{ */
/** @file gncOwner.h
    @brief Business Interface:  Object OWNERs
    @author Copyright (C) 2001,2002 Derek Atkins <warlord@MIT.EDU>
    @author Copyright (c) 2005 Neil Williams <linux@codehelp.co.uk>
    @author Copyright (c) 2006 David Hampton <hampton@employees.org>
*/

#ifndef GNC_OWNER_H_
#define GNC_OWNER_H_

typedef struct _gncOwner GncOwner;

#define GNC_ID_OWNER   "gncOwner"

typedef enum
{
    GNC_OWNER_NONE ,
    GNC_OWNER_UNDEFINED ,
    GNC_OWNER_CUSTOMER ,
    GNC_OWNER_JOB ,
    GNC_OWNER_VENDOR ,
    GNC_OWNER_EMPLOYEE ,
} GncOwnerType;

#include "qof.h"
#include "gncCustomer.h"
#include "gncJob.h"
#include "gncVendor.h"
#include "gncEmployee.h"
#include "gncInvoice.h"
#include "Account.h"
#include "gnc-lot.h"

/** \name QOF handling

Whilst GncOwner is not a formal QOF object, these functions
are still expected to be useful in making GncOwner transparent
to QOF as they can be used by objects like GncInvoice.
@{
*/
/** return the type for the collection. */
QofIdTypeConst qofOwnerGetType(const GncOwner *owner);
/** return the owner itself as an entity. */
QofInstance* qofOwnerGetOwner (const GncOwner *owner);
/** set the owner from the entity. */
void qofOwnerSetEntity (GncOwner *owner, QofInstance *ent);
/** Check if entity is an owner kind. This function conveniently
 *  imitates the various GNC_IS_ checks on the other gnucash
 *  objects even though an owner is not really a true object. */
gboolean GNC_IS_OWNER (QofInstance *ent);

/** Returns the QofIdType of the given GncOwnerType, or NULL if no
 * suitable one exists. */
QofIdTypeConst gncOwnerTypeToQofIdType(GncOwnerType t);

gboolean
gncOwnerRegister(void);

/** @} */

#ifndef SWIG

/** \struct GncOwner */
struct _gncOwner
{
    GncOwnerType     type;      /**< Customer, Job, Vendor, Employee or Undefined. */
    union
    {
        gpointer       undefined;
        GncCustomer *  customer;
        GncJob *       job;
        GncVendor *    vendor;
        GncEmployee *  employee;
    } owner;                   /**< holds the pointer to the owner object. */
    gpointer         qof_temp; /**< Set type independently of the owner. */
};

#endif /* SWIG */

/** \name Setup routines
@{
*/
void gncOwnerInitUndefined (GncOwner *owner, gpointer obj);
void gncOwnerInitCustomer (GncOwner *owner, GncCustomer *customer);
void gncOwnerInitJob (GncOwner *owner, GncJob *job);
void gncOwnerInitVendor (GncOwner *owner, GncVendor *vendor);
void gncOwnerInitEmployee (GncOwner *owner, GncEmployee *employee);
/** @} */
/** \name Get routines.
@{
*/
/** Returns the GncOwnerType of this owner. (Not to be confused with qofOwnerGetType().) */
GncOwnerType gncOwnerGetType (const GncOwner *owner);
/** Returns TRUE if the given owner is one of the valid objects.
 * Returns FALSE if the owner is (still) undefined, or if it is NULL. */
gboolean gncOwnerIsValid (const GncOwner *owner);

/** If the given owner is of type GNC_OWNER_UNDEFINED, returns the undefined
 * pointer, which is usually NULL. Otherwise returns NULL. */
gpointer gncOwnerGetUndefined (const GncOwner *owner);
/** If the given owner is of type GNC_OWNER_CUSTOMER, returns the pointer
 * to the customer object. Otherwise returns NULL. */
GncCustomer * gncOwnerGetCustomer (const GncOwner *owner);
/** If the given owner is of type GNC_OWNER_JOB, returns the pointer
 * to the job object. Otherwise returns NULL. */
GncJob * gncOwnerGetJob (const GncOwner *owner);
/** If the given owner is of type GNC_OWNER_VENDOR, returns the pointer
 * to the vendor object. Otherwise returns NULL. */
GncVendor * gncOwnerGetVendor (const GncOwner *owner);
/** If the given owner is of type GNC_OWNER_EMPLOYEE, returns the pointer
 * to the employee object. Otherwise returns NULL. */
GncEmployee * gncOwnerGetEmployee (const GncOwner *owner);

const char * gncOwnerGetID (const GncOwner *owner);
const char * gncOwnerGetName (const GncOwner *owner);
GncAddress * gncOwnerGetAddr (const GncOwner *owner);
gboolean gncOwnerGetActive (const GncOwner *owner);
gnc_commodity * gncOwnerGetCurrency (const GncOwner *owner);
/** @} */

/** \name Set routines.
@{
*/
void gncOwnerSetActive (const GncOwner *owner, gboolean active);
/** @} */

void gncOwnerCopy (const GncOwner *src, GncOwner *dest);

/** \name Comparison routines.
 @{
 */
/** Assess equality by checking
 *  - if both owner objects refer to the same owner type
 *  - and if the owner reference points to the same
 *    {vendor/customer/employee} in memory */
gboolean gncOwnerEqual (const GncOwner *a, const GncOwner *b);
/** Same as gncOwnerEqual, but returns 0 if
    equal to be used as a GList custom compare function */
int gncOwnerGCompareFunc (const GncOwner *a, const GncOwner *b);
/** Sort on name */
int gncOwnerCompare (const GncOwner *a, const GncOwner *b);
/** @} */

/** Get the GncGUID of the immediate owner */
const GncGUID * gncOwnerGetGUID (const GncOwner *owner);
GncGUID gncOwnerRetGUID (GncOwner *owner);

/**
 * Get the "parent" Owner or GncGUID thereof.  The "parent" owner
 * is the Customer or Vendor, or the Owner of a Job
 */
const GncOwner * gncOwnerGetEndOwner (const GncOwner *owner);
const GncGUID * gncOwnerGetEndGUID (const GncOwner *owner);

/** Attach an owner to a lot */
void gncOwnerAttachToLot (const GncOwner *owner, GNCLot *lot);

/** Helper function used to filter a list of lots by owner.
 */
gboolean gncOwnerLotMatchOwnerFunc (GNCLot *lot, gpointer user_data);

/** Helper function used to sort lots by date. If the lot is
 * linked to an invoice, use the invoice posted date, otherwise
 * use the lot's opened date.
 */
gint gncOwnerLotsSortFunc (GNCLot *lotA, GNCLot *lotB);

/** Get the owner from the lot.  If an owner is found in the lot,
 * fill in "owner" and return TRUE.  Otherwise return FALSE.
 */
gboolean gncOwnerGetOwnerFromLot (GNCLot *lot, GncOwner *owner);

gboolean gncOwnerGetOwnerFromTypeGuid (QofBook *book, GncOwner *owner, QofIdType type, GncGUID *guid);

/**
 * Create a lot for a payment to the owner using the other
 * parameters passed in. If a transaction is set, this transaction will be
 * reused if possible (meaning, if the transaction currency matches
 * the owner's currency and if the transaction has (at least?) one
 * split in the transfer account).
 */
GNCLot *
gncOwnerCreatePaymentLot (const GncOwner *owner, Transaction *txn,
                          Account *posted_acc, Account *xfer_acc,
                          gnc_numeric amount, gnc_numeric exch, Timespec date,
                          const char *memo, const char *num);

/**
 * Given a list of lots, try to balance as many of them as possible
 * by creating balancing transactions between them. This can be used
 * to automatically link invoices to payments (to "mark" invoices as
 * paid) or to credit notes or the other way around.
 *
 * The function starts with the first lot in the list and tries to
 * create balancing transactions to the remainder of the lots in the
 * list. If it reaches the end of the list, it will find the next
 * still open lot in the list and tries to balance it with all lots
 * that follow it (the ones that precede it are either already closed
 * or not suitable or they would have been processed in a previous
 * iteration).
 *
 * By intelligently sorting the list of lots, you can play with the
 * order of precedence in which the lots should be processed. For
 * example, by sorting the oldest invoice lots first, the code will
 * attempt to balance these first.
 *
 * Some restrictions:
 * - the algorithm is lazy: it will create the smallest balancing
 *   transaction(s) possible, not the largest ones. Since the process
 *   is iterative, you will have balanced the maximum amount possible
 *   in the end, but it may be done in several transactions instead of
 *   only one big one.
 * - the balancing transactions only work within one account. If a
 *   balancing lot is from another account than the lot currently being
 *   balanced, it will be skipped during balance evaluation. However
 *   if there is a mix of lots from two different accounts, the algorithm
 *   will still attempt to match all lots per account.
 * - the calling function is responsible for the memory management
 *   of the lots list. If it created the list, it should properly free
 *   it as well.
 */
void gncOwnerAutoApplyPaymentsWithLots (const GncOwner *owner, GList *lots);

/**
 * A convenience function to apply a payment to the owner.
 * It creates a lot for a payment, optionally based on an existing
 * transaction and then tries to balance it with the list of
 * document/payment lots passed in. If not lots were given,
 * all open lots for the owner are considered.
 *
 * This code is actually a convenience wrapper around gncOwnerCreatePaymentLot
 * and gncOwnerAutoApplyPaymentsWithLots. See their descriptions for more
 * details on what happens exactly.
 */
void
gncOwnerApplyPayment (const GncOwner *owner, Transaction *txn, GList *lots,
                      Account *posted_acc, Account *xfer_acc,
                      gnc_numeric amount, gnc_numeric exch, Timespec date,
                      const char *memo, const char *num, gboolean auto_pay);

/** Helper function to find a split in lot that best offsets target_value
 *  Obviously it should be of opposite sign.
 * If there are more splits of opposite sign the following
 * criteria are used in order of preference:
 * 1. exact match in abs value is preferred over larger abs value
 * 2. larger abs value is preferred over smaller abs value
 * 3. if previous and new candidate are in the same value category,
 *    prefer real payment splits over lot link splits
 * 4. if previous and new candiate are of same split type
 *    prefer biggest abs value.
 */
Split *gncOwnerFindOffsettingSplit (GNCLot *pay_lot, gnc_numeric target_value);

/** Helper function to reduce the value of a split to target_value. To make
 *  sure the split's parent transaction remains balanced a second split
 *  will be created with the remainder. Similarly if the split was part of a
 *  (business) lot, the remainder split will be added to the same lot to
 *  keep the lot's balance unchanged.
 */
gboolean gncOwnerReduceSplitTo (Split *split, gnc_numeric target_value);

/** To help a user understand what a lot link transaction does,
 *  we set the memo to name all documents involved in the link.
 *  The function below calculates this memo and sets it for
 *  all splits in the lot link transaction.
 */
void gncOwnerSetLotLinkMemo (Transaction *ll_txn);

/** Returns a GList of account-types based on the owner type */
GList * gncOwnerGetAccountTypesList (const GncOwner *owner);

/** Returns a GList of currencies associated with the owner */
GList * gncOwnerGetCommoditiesList (const GncOwner *owner);


/** Given an owner, extract the open balance from the owner and then
 *  convert it to the desired currency.
 */
gnc_numeric
gncOwnerGetBalanceInCurrency (const GncOwner *owner,
                              const gnc_commodity *report_currency);

#define OWNER_TYPE        "type"
#define OWNER_TYPE_STRING "type-string"  /**< Allows the type to be handled externally. */
#define OWNER_CUSTOMER    "customer"
#define OWNER_JOB         "job"
#define OWNER_VENDOR      "vendor"
#define OWNER_EMPLOYEE    "employee"
#define OWNER_PARENT      "parent"
#define OWNER_PARENTG     "parent-guid"
#define OWNER_NAME        "name"

#define OWNER_FROM_LOT    "owner-from-lot"

/**
 * These two functions are mainly for the convenience of scheme code.
 * Normal C code has no need to ever use these two functions, and rather
 * can just use a GncOwner directly and just pass around a pointer to it.
 */
GncOwner * gncOwnerNew (void);
void gncOwnerFree (GncOwner *owner);


/**
 * These are convenience wrappers around gnc{Vender,Customer,Job,Employee}*
 * functions. This allows you to begin edit, destroy commit edit an owner
 * without knowing its type.
 */
void gncOwnerBeginEdit (GncOwner *owner);
void gncOwnerCommitEdit (GncOwner *owner);
void gncOwnerDestroy (GncOwner *owner);

#endif /* GNC_OWNER_H_ */
/** @} */
/** @} */
