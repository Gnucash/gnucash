/********************************************************************\
 * engine-deprecated.h: Guile wrappers for deleted C functions.     *
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

#include "config.h"
#include "Split.h"
#include "Transaction.h"
#include "gncTaxTable.h"
#include "gncOwner.h"

typedef struct
{
    time64 tv_sec;
    time64 tv_nsec;
} Timespec;


void xaccSplitSetDateReconciledTS(Split* s, Timespec *ts);
void xaccSplitGetDateReconciledTS(Split* s, Timespec *ts);
Timespec xaccSplitRetDateReconciledTS(Split* s);

Timespec gnc_transaction_get_date_posted(const Transaction *t);
Timespec gnc_transaction_get_date_entered(const Transaction *t);
Timespec gnc_split_get_date_reconciled(const Split *s);
void gnc_transaction_set_date(Transaction *t, Timespec ts);

Timespec gncTaxTableLastModified (const GncTaxTable *table);

void gncOwnerApplyPayment (const GncOwner *owner, Transaction **preset_txn,
                           GList *lots, Account *posted_acc, Account *xfer_acc,
                           gnc_numeric amount, gnc_numeric exch, Timespec date,
                           const char *memo, const char *num,
                           gboolean auto_pay);

GNCLot* gncOwnerCreatePaymentLot (const GncOwner *owner,
                                  Transaction **preset_txn, Account *posted_acc,
                                  Account *xfer_acc, gnc_numeric amount,
                                  gnc_numeric exch, Timespec date,
                                  const char *memo, const char *num);

void gnc_price_set_time (GNCPrice *p, Timespec t);
Timespec gnc_price_get_time (GNCPrice *p);
GNCPrice* gnc_pricedb_lookup_at_time(GNCPriceDB *db,
                                     const gnc_commodity *commodity,
                                     const gnc_commodity *currency,
                                     Timespec t);
GNCPrice* gnc_pricedb_lookup_day(GNCPriceDB *db,
                                 const gnc_commodity *commodity,
                                 const gnc_commodity *currency,
                                 Timespec t);
GNCPrice* gnc_pricedb_lookup_nearest_in_time(GNCPriceDB *db,
                                             const gnc_commodity *c,
                                             const gnc_commodity *currency,
                                             Timespec t);
PriceList* gnc_pricedb_lookup_nearest_in_time_any_currency(GNCPriceDB *db,
                                                           const gnc_commodity *c,
                                                           Timespec t);
GNCPrice* gnc_pricedb_lookup_latest_before(GNCPriceDB *db,
                                           gnc_commodity *c,
                                           gnc_commodity *currency,
                                           Timespec t);
PriceList* gnc_pricedb_lookup_latest_before_any_currency(GNCPriceDB *db,
                                                         const gnc_commodity *c,
                                                         Timespec t);
gnc_numeric
gnc_pricedb_convert_balance_nearest_price(GNCPriceDB *pdb,
                                          gnc_numeric balance,
                                          const gnc_commodity *balance_currency,
                                          const gnc_commodity *new_currency,
                                          Timespec t);

