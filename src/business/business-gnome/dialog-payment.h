/*
 * dialog-payment.h -- Dialog to enter payments
 * Copyright (C) 2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#ifndef _DIALOG_PAYMENT_H
#define _DIALOG_PAYMENT_H

typedef struct _payment_window PaymentWindow;

#include "gnc-book.h"
#include "gncOwner.h"

/* Create a payment window */
PaymentWindow * gnc_ui_payment_new (GncOwner *owner, GNCBook *book);

/* Destroy a payment window */
void gnc_ui_payment_window_destroy (PaymentWindow *pw);

#endif /* _DIALOG_PAYMENT_H */
