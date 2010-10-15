/********************************************************************\
 * SX-ttinfo.c -- Template Transaction manipulation functions       *
 *               for scheduled transactions                         *
 * Copyright (C) 2001 Robert Merkel <rgmerk@mira.net>               *
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
 *                                                                  *
\********************************************************************/


#include "config.h"

#include "SX-ttinfo.h"

/* KvpFrame policy? */
struct TTInfo_s
{
    /* FIXME add notes field */
    char *description; /* owned by us */
    char *num;         /* owned  by us */
    gnc_commodity *common_currency; /* not freed */

    GList *splits; /* list of template splits, owned by us */
};

struct TTSplitInfo_s
{
    char *action; /* owned by us */
    /* FIXME: What about the split's KvpFrame */
    char *memo; /* owned by us */
    char *credit_formula, *debit_formula; /* owned by us */
    Account *acc;
};

TTInfo *
gnc_ttinfo_malloc(void)
{
    TTInfo *tti = g_new0(TTInfo, 1);
    return tti;
}

static void
delete_splitinfo(gpointer data, gpointer user_data)
{
    gnc_ttsplitinfo_free( (TTSplitInfo *) data);
    return;
}

void gnc_ttinfo_free(TTInfo *info)
{
    g_return_if_fail(info);

    g_free(info->description);
    g_free(info->num);
    g_list_foreach(info->splits,
                   delete_splitinfo,
                   NULL);

    g_list_free(info->splits);

    g_free(info);

    return;
}

void
gnc_ttinfo_set_description(TTInfo *tti, const char *description)
{
    g_return_if_fail(tti);

    if (tti->description)
    {
        g_free(tti->description);
    }

    tti->description = g_strdup(description);

    return;
}

const char *
gnc_ttinfo_get_description(TTInfo *tti)
{
    g_return_val_if_fail(tti, NULL);

    return tti->description;
}




void
gnc_ttinfo_set_num(TTInfo *tti, const char *num)
{
    g_return_if_fail(tti);

    if (tti->num)
    {
        g_free(tti->num);
    }

    tti->num = g_strdup(num);

    return;
}

const char*
gnc_ttinfo_get_num(TTInfo *tti)
{
    g_return_val_if_fail(tti, NULL);

    return tti->num;
}


void
gnc_ttinfo_set_currency(TTInfo *tti, gnc_commodity *common_currency)
{
    g_return_if_fail(tti);

    tti->common_currency = common_currency;
    return;
}

gnc_commodity *
gnc_ttinfo_get_currency(TTInfo *tti)
{
    g_return_val_if_fail(tti, NULL);

    return tti->common_currency;
}


void gnc_ttinfo_set_template_splits(TTInfo *tti, GList *splits)
{
    g_return_if_fail(tti);

    tti->splits = splits;
    return;
}

void gnc_ttinfo_append_template_split(TTInfo *tti, TTSplitInfo *split_i)
{
    g_return_if_fail(tti && split_i);

    tti->splits = g_list_append(tti->splits, split_i);

    return;
}

GList *
gnc_ttinfo_get_template_splits(TTInfo *tti)
{
    g_return_val_if_fail(tti, NULL);
    return tti->splits;
}

TTSplitInfo *
gnc_ttsplitinfo_malloc(void)
{
    TTSplitInfo *ttsi = g_new0(TTSplitInfo, 1);
    return ttsi;
}

void
gnc_ttsplitinfo_free(TTSplitInfo *ttsi)
{
    if ( ttsi->action )
        g_free(ttsi->action);
    if ( ttsi->memo )
        g_free(ttsi->memo);
    if ( ttsi->credit_formula )
        g_free(ttsi->credit_formula);
    if ( ttsi->debit_formula )
        g_free(ttsi->debit_formula);
    g_free(ttsi);
    return;
}

void
gnc_ttsplitinfo_set_action(TTSplitInfo *ttsi, const char *action)
{
    g_return_if_fail(ttsi);

    if (ttsi->action)
        g_free(ttsi->action);

    ttsi->action = g_strdup(action);
    return;
}

const char *
gnc_ttsplitinfo_get_action(TTSplitInfo *ttsi)
{
    g_return_val_if_fail(ttsi, NULL);

    return ttsi->action;
}

void
gnc_ttsplitinfo_set_memo(TTSplitInfo *ttsi, const char *memo)
{
    g_return_if_fail(ttsi);

    if (ttsi->memo)
        g_free(ttsi->memo);

    ttsi->memo = g_strdup(memo);
    return;
}

const char *
gnc_ttsplitinfo_get_memo(TTSplitInfo *ttsi)
{
    g_return_val_if_fail(ttsi, NULL);

    return ttsi->memo;
}

void
gnc_ttsplitinfo_set_credit_formula_numeric(TTSplitInfo *ttsi, gnc_numeric credit)
{
    g_return_if_fail(ttsi);

    if (ttsi->credit_formula)
        g_free(ttsi->credit_formula);

    ttsi->credit_formula = gnc_numeric_to_string(credit);

    if (ttsi->debit_formula)
    {
        g_free(ttsi->debit_formula);
        ttsi->debit_formula = NULL;
    }
}

void
gnc_ttsplitinfo_set_credit_formula(TTSplitInfo *ttsi, const char *credit_formula)
{
    g_return_if_fail(ttsi);

    if (ttsi->credit_formula)
        g_free(ttsi->credit_formula);

    ttsi->credit_formula = g_strdup(credit_formula);

    if (ttsi->debit_formula)
    {
        g_free(ttsi->debit_formula);
        ttsi->debit_formula = NULL;
    }
    return;
}

const char *
gnc_ttsplitinfo_get_credit_formula(TTSplitInfo *ttsi)
{
    g_return_val_if_fail(ttsi, NULL);
    return ttsi->credit_formula;
}


const char *
gnc_ttsplitinfo_get_debit_formula(TTSplitInfo *ttsi)
{
    g_return_val_if_fail(ttsi, NULL);
    return ttsi->debit_formula;
}

void
gnc_ttsplitinfo_set_debit_formula_numeric(TTSplitInfo *ttsi, gnc_numeric debit)
{
    g_return_if_fail(ttsi);

    if (ttsi->debit_formula)
    {
        g_free(ttsi->debit_formula);
    }
    ttsi->debit_formula = gnc_numeric_to_string(debit);

    if (ttsi->credit_formula)
    {
        g_free(ttsi->credit_formula);
        ttsi->credit_formula = NULL;
    }
    return;
}

void
gnc_ttsplitinfo_set_debit_formula(TTSplitInfo *ttsi, const char *debit_formula)
{
    g_return_if_fail(ttsi);

    if (ttsi->debit_formula)
        g_free(ttsi->debit_formula);

    ttsi->debit_formula = g_strdup(debit_formula);

    if (ttsi->credit_formula)
    {
        g_free(ttsi->credit_formula);
        ttsi->credit_formula = NULL;
    }
    return;
}

void
gnc_ttsplitinfo_set_account(TTSplitInfo *ttsi, Account *acc)
{
    g_return_if_fail(ttsi && acc);

    ttsi->acc = acc;
    return;
}

Account *
gnc_ttsplitinfo_get_account(TTSplitInfo *ttsi)
{
    g_return_val_if_fail(ttsi, NULL);

    return ttsi->acc;
}
