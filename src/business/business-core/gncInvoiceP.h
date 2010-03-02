/********************************************************************\
 * gncInvoiceP.h -- the Core Busines Invoice Interface              *
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

gboolean gncInvoiceRegister (void);
gint64 gncInvoiceNextID (QofBook *book, GncOwner *owner);
void gncInvoiceSetPostedAcc (GncInvoice *invoice, Account *acc);
void gncInvoiceSetPostedTxn (GncInvoice *invoice, Transaction *txn);
void gncInvoiceSetPostedLot (GncInvoice *invoice, GNCLot *lot);
void gncInvoiceSetPaidTxn (GncInvoice *invoice, Transaction *txn);


/** The gncCloneInvoice() routine makes a copy of the indicated
 *  invoice, placing it in the indicated book.  It copies
 *  the name, description, type, due-days, discount, etc.
 *  It also copies (as needed) both parents and children, so that
 *  the parent-child relationship is correctly mirrored in the
 *  clone.
 *  It then adds a pair of 'gemini' kvp pointers so that each copy
 *  can be found from the other.
 */

GncInvoice * gncCloneInvoice (GncInvoice *from, QofBook *);

/** The gncInvoiceObtainTwin() will find the 'twin' of the
 *  indicated invoice in the indicated book.  If the twin doesn't
 *  yet exist in the book, it will be created (by calling
 *  gncCloneInvoice()) and placed into the book.
 *
 * We called this routine 'Obtain' instead of "Get" to distinguish
 * it from the other Get routines, which work in fundamentally
 * different ways.
 */
GncInvoice * gncInvoiceObtainTwin (GncInvoice *from, QofBook *book);
#define gncInvoiceSetGUID(I,G) qof_instance_set_guid(QOF_INSTANCE(I),(G))
#endif /* GNC_INVOICEP_H_ */
