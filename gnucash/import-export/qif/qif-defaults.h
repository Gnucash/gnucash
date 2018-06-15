/*
 * qif-defaults.h -- QIF Defaults -- default accounts...
 *
 * Created by:	Derek Atkins <derek@ihtfp.com>
 * Copyright (c) 2003 Derek Atkins <warlord@MIT.EDU>
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

#ifndef QIF_DEFAULTS_H
#define QIF_DEFAULTS_H

#include "qif-objects.h"
#include "qif-import.h"

QifAccount qif_default_equity_acct(QifContext ctx);
QifAccount qif_default_equity_holding(QifContext ctx, const char *security);

QifAccount qif_default_margin_interest_acct(QifContext ctx);
QifAccount qif_default_commission_acct(QifContext ctx);
QifAccount qif_default_stock_acct(QifContext ctx, const char *security);
QifAccount qif_default_cglong_acct(QifContext ctx, const char *security);
QifAccount qif_default_cgmid_acct(QifContext ctx, const char *security);
QifAccount qif_default_cgshort_acct(QifContext ctx, const char *security);
QifAccount qif_default_dividend_acct(QifContext ctx, const char *security);
QifAccount qif_default_interest_acct(QifContext ctx, const char *security);
QifAccount qif_default_capital_return_acct(QifContext ctx, const char *security);

#endif /* QIF_DEFAULTS_H */
