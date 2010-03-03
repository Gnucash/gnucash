/*
 * dialog-payment.h -- Dialog to enter payments
 * Copyright (C) 2002,2006 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

#ifndef _DIALOG_PAYMENT_H
#define _DIALOG_PAYMENT_H

typedef struct _payment_window PaymentWindow;

#include "gncOwner.h"
#include "gncInvoice.h"

/* Create a payment window */
PaymentWindow * gnc_ui_payment_new (GncOwner *owner, QofBook *book);
PaymentWindow * gnc_ui_payment_new_with_invoice (GncOwner *owner,
        QofBook *book,
        GncInvoice *invoice);

/* Destroy a payment window */
void gnc_ui_payment_window_destroy (PaymentWindow *pw);

#endif /* _DIALOG_PAYMENT_H */
