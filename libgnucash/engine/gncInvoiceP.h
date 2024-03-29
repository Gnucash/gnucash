/********************************************************************\
 * gncInvoiceP.h -- the Core Business Invoice Interface              *
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

/*
 * Copyright (C) 2001 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#ifndef GNC_INVOICEP_H_
#define GNC_INVOICEP_H_

#include "gncInvoice.h"
#include "Account.h"
#include "Transaction.h"
#include "gnc-lot.h"
#include "gncOwner.h"

#ifdef __cplusplus
extern "C" {
#endif

gboolean gncInvoiceRegister (void);
gchar *gncInvoiceNextID (QofBook *book, const GncOwner *owner);
void gncInvoiceSetPostedAcc (GncInvoice *invoice, Account *acc);
void gncInvoiceSetPostedTxn (GncInvoice *invoice, Transaction *txn);
void gncInvoiceSetPostedLot (GncInvoice *invoice, GNCLot *lot);
//void gncInvoiceSetPaidTxn (GncInvoice *invoice, Transaction *txn);

void gncInvoiceAttachToLot (GncInvoice *invoice, GNCLot *lot);
void gncInvoiceDetachFromLot (GNCLot *lot);
void gncInvoiceAttachToTxn (GncInvoice *invoice, Transaction *txn);

#define gncInvoiceSetGUID(I,G) qof_instance_set_guid(QOF_INSTANCE(I),(G))

#ifdef __cplusplus
}
#endif

#endif /* GNC_INVOICEP_H_ */
