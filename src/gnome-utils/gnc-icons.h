/*
 * gnc-icons.h -- Functions to create a GtkIconFactory for GnuCash
 * Copyright (C) 2003 Jan Arne Petersen
 * Author: Jan Arne Petersen <jpetersen@uni-bonn.de>
 */
/********************************************************************\
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


#ifndef __GNC_ICONS_H
#define __GNC_ICONS_H

G_BEGIN_DECLS

#define GNC_STOCK_ACCOUNT "gnc-account"
#define GNC_STOCK_ACCOUNT_REPORT "gnc-account-report"
#define GNC_STOCK_DELETE_ACCOUNT "gnc-delete-account"
#define GNC_STOCK_EDIT_ACCOUNT "gnc-edit-account"
#define GNC_STOCK_NEW_ACCOUNT "gnc-new-account"
#define GNC_STOCK_OPEN_ACCOUNT "gnc-open-account"
#define GNC_STOCK_SPLIT_TRANS "gnc-split-transaction"
#define GNC_STOCK_SCHEDULE "gnc-schedule-new"
#define GNC_STOCK_TRANSFER "gnc-transfer"
#define GNC_STOCK_JUMP_TO "gnc-jump-to"
#define GNC_STOCK_INVOICE "gnc-invoice-post"
#define GNC_STOCK_INVOICE_POST "gnc-invoice-post"
#define GNC_STOCK_INVOICE_UNPOST "gnc-invoice-unpost"
#define GNC_STOCK_INVOICE_PAY "gnc-invoice-pay"
#define GNC_STOCK_INVOICE_NEW "gnc-invoice-new"
#define GNC_STOCK_INVOICE_EDIT "gnc-invoice-edit"
#define GNC_STOCK_INVOICE_DUPLICATE "gnc-invoice-duplicate"
#define GNC_STOCK_PDF_EXPORT "gnc-pdf-export"

//FIXME: use own budget icons?
#define GNC_STOCK_BUDGET "gnc-budget"
#define GNC_STOCK_NEW_BUDGET "gnc-account"
#define GNC_STOCK_OPEN_BUDGET "gnc-open-account"
//#define GNC_STOCK_CLOSE_BUDGET "gnc-close-account"
//#define GNC_STOCK_EDIT_BUDGET "gnc-edit-account"
#define GNC_STOCK_DELETE_BUDGET "gnc-delete-account"

void gnc_load_stock_icons (void);

G_END_DECLS

#endif /* __GNC_ICONS_H */
