/*
 * gncInvoiceP.h -- the Core Busines Invoice Interface
 * Copyright (C) 2001 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#ifndef GNC_INVOICEP_H_
#define GNC_INVOICEP_H_

#include "gncInvoice.h"
#include "Account.h"
#include "Transaction.h"

gboolean gncInvoiceRegister (void);
gint gncInvoiceNextID (GNCBook *book);
void gncInvoiceSetGUID (GncInvoice *invoice, const GUID *guid);
void gncInvoiceSetDirty (GncInvoice *invoice, gboolean dirty);
void gncInvoiceSetPostedAcc (GncInvoice *invoice, Account *acc);
void gncInvoiceSetPostedTxn (GncInvoice *invoice, Transaction *txn);
void gncInvoiceSetPaidTxn (GncInvoice *invoice, Transaction *txn);

#endif /* GNC_INVOICEP_H_ */
