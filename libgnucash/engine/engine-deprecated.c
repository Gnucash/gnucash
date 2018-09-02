/********************************************************************\
 * engine-deprecated.i: Guile wrappers for deleted C functions.     *
 * Copyright 2018 John Ralls <jralls@ceridwen.us>                   *
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

/* The functions in this file have been removed from the C library and are
 * provided here for backward compatibility. They will be removed completely
 * from the next major release of GnuCash.
 */

#include "engine-deprecated.h"

#include <libguile/deprecation.h>

void xaccSplitSetDateReconciledTS(Split* s, Timespec *ts)
{
    scm_c_issue_deprecation_warning("xaccSplitSetDateReconciled is deprecated. Use xaccSplitSetDateReconciledSecs instead.");
    xaccSplitSetDateReconciledSecs (s, ts->tv_sec);
}

void xaccSplitGetDateReconciledTS(Split* s, Timespec *ts)
{
    scm_c_issue_deprecation_warning("xaccSplitGetDateReconciled is deprecated. Use xaccSplitGetDateReconciledSecs instead.");
    ts->tv_sec = xaccSplitGetDateReconciled (s);
    ts->tv_nsec = 0;
}

Timespec xaccSplitRetDateReconciledTS(Split* s)
{
    Timespec ts = {0, 0};
    scm_c_issue_deprecation_warning("xaccSplitSetDateReconciled is deprecated. Use xaccSplitSetDateReconciledSecs instead.");
    ts.tv_sec = xaccSplitGetDateReconciled (s);
    return ts;
}

Timespec
gnc_transaction_get_date_posted(const Transaction *t)
{
    Timespec ts = {0, 0};
    scm_c_issue_deprecation_warning("gnc_transaction_get_date_posted is deprecated. Use xaccTransRetDatePosted instead.");
    ts.tv_sec = xaccTransRetDatePosted(t);
    return ts;
}

Timespec
gnc_transaction_get_date_entered(const Transaction *t)
{
    Timespec ts = {0, 0};
    scm_c_issue_deprecation_warning("gnc_transaction_get_date_entered is deprecated. Use xaccTransGetDateEntered instead.");
    ts.tv_sec = xaccTransRetDateEntered(t);
    return ts;
}

Timespec
gnc_split_get_date_reconciled(const Split *s)
{
    Timespec ts = {0, 0};
    scm_c_issue_deprecation_warning("gnc_split_get_date_reconciled is deprecated. Use xaccSplitGetDateReconciled instead.");
    ts.tv_sec = xaccSplitGetDateReconciled(s);
    return ts;
}

void
gnc_transaction_set_date(Transaction *t, Timespec ts)
{
    scm_c_issue_deprecation_warning("gnc_transaction_set_date is deprecated. Use xaccTransSetDatePostedSecs instead.");
    xaccTransSetDatePostedSecs(t, ts.tv_sec);
}

Timespec
gncTaxTableLastModified (const GncTaxTable *table)
{
    Timespec t = {0, 0};
    scm_c_issue_deprecation_warning("gncTaxTableLastModified is deprecated. Use gncTaxTableLastModifiedSecs instead.");
    t.tv_sec = gncTaxTableLastModifiedSecs (table);
    return t;
}

void
gncOwnerApplyPayment (const GncOwner *owner, Transaction **preset_txn,
                           GList *lots, Account *posted_acc, Account *xfer_acc,
                           gnc_numeric amount, gnc_numeric exch, Timespec date,
                           const char *memo, const char *num,
                           gboolean auto_pay)
{
    scm_c_issue_deprecation_warning("gncOwnerApplyPayment is deprecated. Use gncOwnerApplyPaymentSecs instead.");
    gncOwnerApplyPaymentSecs (owner, preset_txn, lots, posted_acc,
                              xfer_acc, amount, exch, date.tv_sec,
                              memo, num, auto_pay);
}

GNCLot*
gncOwnerCreatePaymentLot (const GncOwner *owner,
                                  Transaction **preset_txn, Account *posted_acc,
                                  Account *xfer_acc, gnc_numeric amount,
                                  gnc_numeric exch, Timespec date,
                                  const char *memo, const char *num)
{
    GNCLot* lot = NULL;
    scm_c_issue_deprecation_warning("gncOwnerCreatePaymentLot is deprecated. Use gncOwnerCreatePaymentLotSecs instead.");
    lot = gncOwnerCreatePaymentLotSecs (owner, preset_txn, posted_acc,
                                        xfer_acc, amount, exch, date.tv_sec,
                                        memo, num);
    return lot;
}

void
gnc_price_set_time (GNCPrice *p, Timespec t)
{
    scm_c_issue_deprecation_warning("gnc_price_set_time is deprecated. Use gnc_price_set_time64 instead.");
    gnc_price_set_time64(p, t.tv_sec);
}

Timespec
gnc_price_get_time (GNCPrice *p)
{
    Timespec t = {0, 0};
    scm_c_issue_deprecation_warning("gnc_price_get_time is deprecated. Use gnc_price_get_time64 instead.");
    t.tv_sec = gnc_price_get_time64(p);
    return t;
}

GNCPrice*
gnc_pricedb_lookup_at_time(GNCPriceDB *db,
                           const gnc_commodity *commodity,
                           const gnc_commodity *currency,
                           Timespec t)
{
    scm_c_issue_deprecation_warning("gnc_pricedb_lookup_at_time is deprecated. Use gnc_pricedb_lookup_at_time64 instead.");
    return gnc_pricedb_lookup_at_time64(db, commodity, currency, t.tv_sec);
}

GNCPrice*
gnc_pricedb_lookup_day(GNCPriceDB *db,
                       const gnc_commodity *commodity,
                       const gnc_commodity *currency,
                       Timespec t)
{
    scm_c_issue_deprecation_warning("gnc_pricedb_lookup_day is deprecated. Use gnc_pricedb_lookup_day_t64 instead.");
    return gnc_pricedb_lookup_day_t64(db, commodity, currency, t.tv_sec);
}

GNCPrice*
gnc_pricedb_lookup_nearest_in_time(GNCPriceDB *db,
                                   const gnc_commodity *c,
                                   const gnc_commodity *currency,
                                   Timespec t)
{
    scm_c_issue_deprecation_warning("gnc_pricedb_lookup_nearest_in_time is deprecated. Use gnc_pricedb_lookup_nearest_in_time64 instead.");
    return gnc_pricedb_lookup_nearest_in_time64(db, c, currency, t.tv_sec);
}

PriceList*
gnc_pricedb_lookup_nearest_in_time_any_currency(GNCPriceDB *db,
                                                const gnc_commodity *c,
                                                Timespec t)
{
    scm_c_issue_deprecation_warning("gnc_pricedb_lookup_nearest_in_time_any_currency is deprecated. Use gnc_pricedb_lookup_nearest_in_time_any_currency_t64 instead.");
    return gnc_pricedb_lookup_nearest_in_time_any_currency_t64(db, c, t.tv_sec);
}

GNCPrice*
gnc_pricedb_lookup_latest_before(GNCPriceDB *db,
                                 gnc_commodity *c,
                                 gnc_commodity *currency,
                                 Timespec t)
{
    scm_c_issue_deprecation_warning("gnc_pricedb_lookup_latest_before is deprecated. Use gnc_pricedb_lookup_latest_before_t64 instead.");
    return gnc_pricedb_lookup_latest_before_t64(db, c, currency, t.tv_sec);
}

PriceList*
gnc_pricedb_lookup_latest_before_any_currency(GNCPriceDB *db,
                                              const gnc_commodity *c,
                                              Timespec t)
{
    scm_c_issue_deprecation_warning("gnc_pricedb_lookup_latest_before_any_currency is deprecated. Use gnc_pricedb_lookup_latest_before_any_currency_t64 instead.");
    return gnc_pricedb_lookup_latest_before_any_currency_t64(db, c, t.tv_sec);
}

gnc_numeric
gnc_pricedb_convert_balance_nearest_price(GNCPriceDB *pdb,
                                          gnc_numeric balance,
                                          const gnc_commodity *balance_currency,
                                          const gnc_commodity *new_currency,
                                          Timespec t)
{
    scm_c_issue_deprecation_warning("gnc_pricedb_convert_balance_nearest_price is deprecated. Use gnc_pricedb_convert_balance_nearest_price_t64 instead.");
    return gnc_pricedb_convert_balance_nearest_price_t64(pdb, balance, balance_currency, new_currency, t.tv_sec);
}
