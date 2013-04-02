/*
 * qif-defaults.c -- QIF Defaults -- default accounts...
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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <glib.h>
#include <glib/gi18n.h>

#include "gnc-helpers.h"
#include "qif-import-p.h"
#include "qif-objects-p.h"
#include "qif-defaults.h"


static GList *stock_list = NULL;
static GList *ext_stock_list = NULL;
static GList *income_list = NULL;
static GList *expense_list = NULL;
static GList *equity_list = NULL;

#define RETURN_ACCT(c,n,l) { if (stock_list == NULL) acct_type_init(); \
	return find_or_make_acct(c, n, l); \
}

static void
acct_type_init(void)
{
    stock_list = qif_parse_acct_type("__stock__", -1);
    ext_stock_list = qif_parse_acct_type("__extstock__", -1);
    income_list = qif_parse_acct_type("__income__", -1);
    expense_list = qif_parse_acct_type("__expense__", -1);
    equity_list = qif_parse_acct_type("__equity__", -1);
}

QifAccount qif_default_equity_acct(QifContext ctx)
{
    char *name = g_strdup(_("Retained Earnings"));
    RETURN_ACCT(ctx, name, equity_list);
}

QifAccount qif_default_margin_interest_acct(QifContext ctx)
{
    char *name = g_strdup_printf("%s%s%s", _("Margin Interest"),
                                 gnc_get_account_separator_string(),
                                 ctx->current_acct->name);
    RETURN_ACCT(ctx, name, expense_list);
}

QifAccount qif_default_commission_acct(QifContext ctx)
{
    char *name = g_strdup_printf("%s%s%s", _("Commissions"),
                                 gnc_get_account_separator_string(),
                                 ctx->current_acct->name);
    RETURN_ACCT(ctx, name, expense_list);
}

QifAccount qif_default_stock_acct(QifContext ctx, const char *security)
{
    char *name = g_strdup_printf("%s%s%s", ctx->current_acct->name,
                                 gnc_get_account_separator_string(),
                                 security);
    RETURN_ACCT(ctx, name, stock_list);
}

QifAccount qif_default_cglong_acct(QifContext ctx, const char *security)
{
    char *name = g_strdup_printf("%s%s%s%s%s", _("Cap. gain (long)"),
                                 gnc_get_account_separator_string(),
                                 ctx->current_acct->name,
                                 gnc_get_account_separator_string(),
                                 security);
    RETURN_ACCT(ctx, name, income_list);
}

QifAccount qif_default_cgmid_acct(QifContext ctx, const char *security)
{
    char *name = g_strdup_printf("%s%s%s%s%s", _("Cap. gain (mid)"),
                                 gnc_get_account_separator_string(),
                                 ctx->current_acct->name,
                                 gnc_get_account_separator_string(),
                                 security);
    RETURN_ACCT(ctx, name, income_list);
}

QifAccount qif_default_cgshort_acct(QifContext ctx, const char *security)
{
    char *name = g_strdup_printf("%s%s%s%s%s", _("Cap. gain (short)"),
                                 gnc_get_account_separator_string(),
                                 ctx->current_acct->name,
                                 gnc_get_account_separator_string(),
                                 security);
    RETURN_ACCT(ctx, name, income_list);
}

QifAccount qif_default_dividend_acct(QifContext ctx, const char *security)
{
    char *name = g_strdup_printf("%s%s%s%s%s", _("Dividends"),
                                 gnc_get_account_separator_string(),
                                 ctx->current_acct->name,
                                 gnc_get_account_separator_string(),
                                 security);
    RETURN_ACCT(ctx, name, income_list);
}

QifAccount qif_default_interest_acct(QifContext ctx, const char *security)
{
    char *name = g_strdup_printf("%s%s%s%s%s", _("Interest"),
                                 gnc_get_account_separator_string(),
                                 ctx->current_acct->name,
                                 gnc_get_account_separator_string(),
                                 security);
    RETURN_ACCT(ctx, name, income_list);
}

QifAccount qif_default_capital_return_acct(QifContext ctx, const char *security)
{
    char *name = g_strdup_printf("%s%s%s%s%s", _("Cap Return"),
                                 gnc_get_account_separator_string(),
                                 ctx->current_acct->name,
                                 gnc_get_account_separator_string(),
                                 security);
    RETURN_ACCT(ctx, name, income_list);
}

QifAccount qif_default_equity_holding(QifContext ctx, const char *security)
{
    return qif_default_equity_acct(ctx);
}

