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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
\********************************************************************/

#ifndef DIALOG_FIND_TRANSACTIONS_H
#define DIALOG_FIND_TRANSACTIONS_H

#include "gnc-ledger-display.h"
#include "dialog-search.h"

GNCSearchWindow *
gnc_ui_find_transactions_dialog_create (GtkWindow *parent, GNCLedgerDisplay * ledger);

#endif
