/********************************************************************\
 * dialog-find-transactions.h : locate transactions/splits matching *
 *                              criteria.                           *
 * Copyright (C) 2000 Bill Gribble <grib@billgribble.com>           *
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

#ifndef __DIALOG_FIND_TRANSACTIONS_H_
#define __DIALOG_FIND_TRANSACTIONS_H_

#include <gnome.h>

#include "MultiLedger.h"


typedef struct _SelectDateDialog SelectDateDialog;
typedef struct _FindTransactionsDialog FindTransactionsDialog;


FindTransactionsDialog * 
gnc_ui_find_transactions_dialog_create(xaccLedgerDisplay * ledger);

void gnc_ui_find_transactions_dialog_destroy(FindTransactionsDialog * pcd);

#endif
