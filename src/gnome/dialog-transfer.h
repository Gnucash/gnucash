/********************************************************************\
 * dialog-transfer.h -- transfer dialog for GnuCash                 *
 * Copyright (C) 1999 Linas Vepstas                                 *
 * Copyright (C) 2000 Dave Peticolas                                *
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
\********************************************************************/

#ifndef __DIALOG_TRANSFER_H__
#define __DIALOG_TRANSFER_H__

#include "Account.h"

typedef struct _xferDialog XferDialog;

XferDialog * gnc_xfer_dialog(GtkWidget * parent, Account *initial);

void gnc_xfer_dialog_select_from_account(XferDialog *xferData,
                                         Account *account);
void gnc_xfer_dialog_select_to_account(XferDialog *xferData,
                                       Account *account);

void gnc_xfer_dialog_set_amount(XferDialog *xferData, gnc_numeric amount);
void gnc_xfer_dialog_set_description(XferDialog *xferData,
                                     const char *description);

#endif
