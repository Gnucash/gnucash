/*
 * gnc-icons.h -- Functions to add icons for GnuCash to use
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

#define GNC_ICON_ACCOUNT "gnc-account"
#define GNC_ICON_ACCOUNT_REPORT "gnc-account-report"
#define GNC_ICON_DELETE_ACCOUNT "gnc-account-delete"
#define GNC_ICON_EDIT_ACCOUNT "gnc-account-edit"
#define GNC_ICON_NEW_ACCOUNT "gnc-account-new"
#define GNC_ICON_OPEN_ACCOUNT "gnc-account-open"
#define GNC_ICON_SPLIT_TRANS "gnc-split-trans"
#define GNC_ICON_SCHEDULE "gnc-sx-new"
#define GNC_ICON_TRANSFER "gnc-transfer"
#define GNC_ICON_JUMP_TO "gnc-jumpto"
#define GNC_ICON_INVOICE "gnc-invoice"
#define GNC_ICON_INVOICE_POST "gnc-invoice-post"
#define GNC_ICON_INVOICE_UNPOST "gnc-invoice-unpost"
#define GNC_ICON_INVOICE_PAY "gnc-invoice-pay"
#define GNC_ICON_INVOICE_NEW "gnc-invoice-new"
#define GNC_ICON_INVOICE_EDIT "gnc-invoice-edit"
#define GNC_ICON_INVOICE_DUPLICATE "gnc-invoice-duplicate"
#define GNC_ICON_PDF_EXPORT "gnc-gnome-pdf"

//FIXME: use own budget icons?
#define GNC_ICON_BUDGET "gnc-account"
#define GNC_ICON_NEW_BUDGET "gnc-account"
#define GNC_ICON_OPEN_BUDGET "gnc-account-open"
//#define GNC_ICON_CLOSE_BUDGET "gnc-close-account"
//#define GNC_ICON_EDIT_BUDGET "gnc-edit-account"
#define GNC_ICON_DELETE_BUDGET "gnc-account-delete"
#define GNC_ICON_APP "gnucash-icon"

void gnc_load_app_icons (void);

G_END_DECLS

#endif /* __GNC_ICONS_H */
