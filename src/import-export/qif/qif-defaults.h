/*
 * qif-defaults.h -- QIF Defaults -- default accounts...
 *
 * Created by:	Derek Atkins <derek@ihtfp.com>
 *
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
