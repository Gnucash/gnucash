/*
 * dialog-billterms.h -- Dialog to create and edit billing terms
 * Copyright (C) 2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#ifndef _DIALOG_BILLTERMS_H
#define _DIALOG_BILLTERMS_H

typedef struct _billterms_window BillTermsWindow;

#include "gnc-book.h"

/* Create a billterms window */
BillTermsWindow * gnc_ui_billterms_window_new (GNCBook *book);

/* Destroy a billterms window */
void gnc_ui_billterms_window_destroy (BillTermsWindow *ttw);

#endif /* _DIALOG_BILLTERMS_H */
