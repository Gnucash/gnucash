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
#include "gnc-date.h"

/* Create a dialog to enter/modify an Split's exchange rate.  Look in
 * the pricedb for potential entries if exchange_rate points to a
 * 'zero' value; new/changed entries are stored in the pricedb
 * (overwriting existing pricedb entries if they exist).
 *
 * If the run was successful (user hit 'ok'), fill in the (new)
 * exchange rate, store it in the pricedb, and return TRUE.  Otherwise
 * return FALSE (and do not touch exchange_rate)
 */
gboolean
gnc_ui_exchange_dialog_new (GNCPriceDB * pricedb,
			    gnc_numeric amount,
			    gnc_commodity * from,
			    gnc_commodity * to,
			    Timespec date,
			    gnc_numeric * exchange_rate);

#endif /* GNC_DIALOG_EXCHANGE_H */
