/********************************************************************
 * dialog-exchange.h -- Dialog to enter exchange rates              *
 *                                                                  *
 * Copyright (C) 2002 Derek Atkins <derek@ihtfp.com>                *
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
 ********************************************************************/

#ifndef GNC_DIALOG_EXCHANGE_H
#define GNC_DIALOG_EXCHANGE_H

#include "gnc-pricedb.h"
#include "gnc-commodity.h"
#include "gnc-numeric.h"
#include "date.h"

typedef struct _exchange_window ExchangeDialog;

void gnc_ui_exchange_dialog_destroy (ExchangeDialog * w);

/* Create a dialog to enter an exchange rate.  Looks in the pricedb
 * for entries; stores entries in the pricedb.
 */
ExchangeDialog *
gnc_ui_exchange_dialog_full (GNCPriceDB * pricedb,
			     gnc_numeric amount,
			     gnc_commodity * from,
			     gnc_commodity * to,
			     Timespec date);

#endif /* GNC_DIALOG_EXCHANGE_H */
