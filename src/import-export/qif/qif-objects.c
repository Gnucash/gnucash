/*
 * qif-objects.c -- Objects for the QIF Importer
 *
 * Written by:	Derek Atkins  <derek@ihtfp.com>
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
#include <string.h>
#include "Account.h"

#include "gnc-engine.h"

#include "qif-import-p.h"
#include "qif-objects-p.h"
#include "qif-defaults.h"

static QofLogModule log_module = GNC_MOD_IMPORT;

/* create a new object of type t, with type-string type and
 * destroy function dest.  Requires 'obj' to be set.
 */
#define qif_object_new(t,typ,dest) ({ \
	obj = (QifObject) g_new0(t, 1); \
	obj->type = typ; \
	obj->destroy = dest; \
	obj; \
})

/* Save the string from this "line".  Also:
 * - make sure we're not over-writing anything.
 * - make sure the 'line' object no longer references the string.
 */
#define qif_save_str(var) { \
	if (var) { \
		PERR("duplicate found at line %d: %s", line->lineno, line->line); \
		g_free(var); \
	} \
	(var) = line->line; \
	line->line = NULL; \
}

/* QIF Account */
static void
qif_account_destroy(QifObject obj)
{
    QifAccount acct = (QifAccount) obj;

    g_free(acct->name);
    g_free(acct->desc);
    g_free(acct->limitstr);
    g_free(acct->budgetstr);

    g_free(acct);
};

static QifAccount
qif_account_new(void)
{
    QifObject obj;
    QifAccount acct;

    obj = qif_object_new(struct _QifAccount, QIF_O_ACCOUNT, qif_account_destroy);

    acct = (QifAccount)obj;
    acct->type_list = qif_parse_acct_type("bank", -1);

    acct->limit = gnc_numeric_zero();
    acct->budget = gnc_numeric_zero();
    return acct;
}

/*
 * Merge acct into ctx.  If this account already exists in ctx then
 * merge in any new values from acct into the ctx version and return
 * the existing acct.  If the account does not already exist, then
 * insert it into the ctx and return it.
 */
QifAccount
qif_account_merge(QifContext ctx, QifAccount acct)
{
    QifAccount acct2 =
        (QifAccount)qif_object_map_lookup(ctx, acct->obj.type, acct->name);

    if (!acct2)
    {
        qif_object_map_insert(ctx, acct->obj.type, (QifObject)acct);
        return acct;
    }

    /* obviously the name is the same, so don't worry about that */

    if (!acct2->desc && acct->desc)
        acct2->desc = g_strdup(acct->desc);

    if (!acct2->type_list && acct->type_list)
        acct2->type_list = acct->type_list;

    if (!acct2->limitstr && acct->limitstr)
    {
        acct2->limitstr = g_strdup(acct->limitstr);
        acct2->limit = acct->limit;
    }

    if (!acct2->budgetstr && acct->budgetstr)
    {
        acct2->budgetstr = g_strdup(acct->budgetstr);
        acct2->budget = acct->budget;
    }

    return acct2;
}

static QifError
qif_account_parse(QifContext ctx, GList *record)
{
    QifAccount acct, temp;
    QifLine line;

    g_return_val_if_fail(ctx, QIF_E_INTERNAL);
    g_return_val_if_fail(record, QIF_E_BADSTATE);

    acct = qif_account_new();

    for (; record; record = record->next)
    {
        line = record->data;

        switch (line->type)
        {
        case 'N':			/* N : account name */
            qif_save_str(acct->name);
            break;
        case 'D': 			/* D : account description */
            qif_save_str(acct->desc);
            break;
        case 'T':			/* T : account type */
            acct->type_list = qif_parse_acct_type(line->line, line->lineno);
            break;
        case 'L':			/* L : account limit */
            qif_save_str(acct->limitstr);
            break;
        case 'B':			/* B : account budget */
            qif_save_str(acct->budgetstr);
            break;
        default:
            PERR("Unknown QIF account data at line %d: %s", line->lineno, line->line);
        }
    }

    /* Merge the account into the context */
    temp = qif_account_merge(ctx, acct);
    if (! (ctx->parse_flags & QIF_F_IGNORE_ACCOUNTS))
        ctx->current_acct = temp;
    if (temp != acct)
        qif_account_destroy((QifObject)acct);

    return QIF_E_OK;
}

/* QIF Category */
static void
qif_cat_destroy(QifObject obj)
{
    QifCategory cat = (QifCategory) obj;

    g_free(cat->name);
    g_free(cat->desc);
    g_free(cat->taxclass);
    g_free(cat->budgetstr);

    g_free(cat);
}

static QifCategory
qif_cat_new(void)
{
    QifObject obj;
    QifCategory cat;

    obj = qif_object_new(struct _QifCategory, QIF_O_CATEGORY, qif_cat_destroy);
    cat = (QifCategory)obj;
    cat->budget = gnc_numeric_zero();

    return cat;
}

/*
 * Merge cat into ctx.  If this category already exists in ctx then
 * merge in any new values from cat into the ctx version and return
 * the existing cat.  If the category does not already exist, then
 * insert it into the ctx and return it.
 */
QifCategory
qif_cat_merge(QifContext ctx, QifCategory cat)
{
    QifCategory cat2 =
        (QifCategory)qif_object_map_lookup(ctx, cat->obj.type, cat->name);

    if (!cat2)
    {
        qif_object_map_insert(ctx, cat->obj.type, (QifObject)cat);
        return cat;
    }

    /* obviously the name is the same, so don't worry about that */

    if (!cat2->desc && cat->desc)
        cat2->desc = g_strdup(cat->desc);

    if (!cat2->taxclass && cat->taxclass)
        cat2->taxclass = g_strdup(cat->taxclass);

    cat2->taxable = (cat2->taxable || cat->taxable);
    cat2->expense = (cat2->expense || cat->expense);
    cat2->income = (cat2->income || cat->income);

    if (!cat2->budgetstr && cat->budgetstr)
    {
        cat2->budgetstr = g_strdup(cat->budgetstr);
        cat2->budget = cat->budget;
    }

    return cat2;
}

static QifError
qif_cat_parse(QifContext ctx, GList *record)
{
    QifCategory cat;
    QifLine line;

    g_return_val_if_fail(ctx, QIF_E_INTERNAL);
    g_return_val_if_fail(record, QIF_E_BADSTATE);

    cat = qif_cat_new();

    for (; record; record = record->next)
    {
        line = record->data;

        switch (line->type)
        {
        case 'N':			/* N : category name */
            qif_save_str(cat->name);
            break;
        case 'D':			/* D : category description */
            qif_save_str(cat->desc);
            break;
        case 'T':			/* T : category is taxable? */
            cat->taxable = TRUE;
            break;
        case 'E':			/* E : category is expense? */
            cat->expense = TRUE;
            break;
        case 'I':			/* I : category is income? */
            cat->income = TRUE;
            break;
        case 'R':			/* R : category taxclass XXX */
            /* XXX: a number? */
            qif_save_str(cat->taxclass);
            break;
        case 'B':			/* B : category budget */
            qif_save_str(cat->budgetstr);
            break;
        default:
            PERR("Unknown QIF category data at line %d: %s", line->lineno, line->line);
        }
    }

    if (qif_cat_merge(ctx, cat) != cat)
        qif_cat_destroy((QifObject)cat);

    return QIF_E_OK;
}

/* QIF Class */
static void
qif_class_destroy(QifObject obj)
{
    QifClass qclass = (QifClass) obj;

    g_free(qclass->name);
    g_free(qclass->desc);
    g_free(qclass->taxdesig);

    g_free(qclass);
}

static QifClass
qif_class_new()
{
    QifObject obj;

    obj = qif_object_new(struct _QifClass, QIF_O_CLASS, qif_class_destroy);
    return (QifClass)obj;
}

/*
 * Merge qclass into ctx.  If this class already exists in ctx then
 * merge in any new values from qclass into the ctx version and return
 * the existing qclass.  If the class does not already exist, then
 * insert it into the ctx and return it.
 */
QifClass
qif_class_merge(QifContext ctx, QifClass qclass)
{
    QifClass qclass2 =
        (QifClass)qif_object_map_lookup(ctx, qclass->obj.type, qclass->name);

    if (!qclass2)
    {
        qif_object_map_insert(ctx, qclass->obj.type, (QifObject)qclass);
        return qclass;
    }

    /* obviously the name is the same, so don't worry about that */

    if (!qclass2->desc && qclass->desc)
        qclass2->desc = g_strdup(qclass->desc);

    if (!qclass2->taxdesig && qclass->taxdesig)
        qclass2->taxdesig = g_strdup(qclass->taxdesig);

    return qclass2;
}

static QifError
qif_class_parse(QifContext ctx, GList *record)
{
    QifClass qclass;
    QifLine line;

    g_return_val_if_fail(ctx, QIF_E_INTERNAL);
    g_return_val_if_fail(record, QIF_E_BADSTATE);

    qclass = qif_class_new();

    for (; record; record = record->next)
    {
        line = record->data;

        switch (line->type)
        {
        case 'N':			/* N : class name */
            qif_save_str(qclass->name);
            break;
        case 'D':			/* D : class description */
            qif_save_str(qclass->desc);
            break;
        case 'R':			/* R : Tax designator */
            qif_save_str(qclass->taxdesig);
            break;
        default:
            PERR("Unknown QIF class data at line %d: %s", line->lineno, line->line);
        }
    }

    if (qif_class_merge(ctx, qclass) != qclass)
        qif_class_destroy((QifObject)qclass);

    return QIF_E_OK;
}

/* QIF Security Symbol */
static void
qif_security_destroy(QifObject obj)
{
    QifSecurity security = (QifSecurity) obj;

    g_free(security->name);
    g_free(security->symbol);
    g_free(security->type);

    g_free(security);
}

static QifSecurity
qif_security_new()
{
    QifObject obj;

    obj = qif_object_new(struct _QifSecurity, QIF_O_SECURITY, qif_security_destroy);
    return (QifSecurity)obj;
}

/*
 * Merge security into ctx.  If this security already exists in ctx then
 * merge in any new values from security into the ctx version and return
 * the existing security.  If the security does not already exist, then
 * insert it into the ctx and return it.
 */
QifSecurity
qif_security_merge(QifContext ctx, QifSecurity security)
{
    QifSecurity security2 =
        (QifSecurity)qif_object_map_lookup(ctx, security->obj.type, security->name);

    if (!security2)
    {
        qif_object_map_insert(ctx, security->obj.type, (QifObject)security);
        return security;
    }

    /* obviously the name is the same, so don't worry about that */

    if (!security2->symbol && security->symbol)
        security2->symbol = g_strdup(security->symbol);

    if (!security2->type && security->type)
        security2->type = g_strdup(security->type);

    return security2;
}

static QifError
qif_security_parse(QifContext ctx, GList *record)
{
    QifSecurity security;
    QifLine line;

    g_return_val_if_fail(ctx, QIF_E_INTERNAL);
    g_return_val_if_fail(record, QIF_E_BADSTATE);

    security = qif_security_new();

    for (; record; record = record->next)
    {
        line = record->data;

        switch (line->type)
        {
        case 'N':			/* N : security name */
            qif_save_str(security->name);
            break;
        case 'S':			/* S : security symbol */
            qif_save_str(security->symbol);
            break;
        case 'T':			/* T : security type */
            qif_save_str(security->type);
            break;
        default:
            PERR("Unknown QIF security data at line %d: %s", line->lineno, line->line);
        }
    }

    if (qif_security_merge(ctx, security) != security)
        qif_security_destroy((QifObject)security);

    return QIF_E_OK;
}

/********************* TXN *********************/

static QifSplit
qif_split_new()
{
    QifSplit split = g_new0(struct _QifSplit, 1);

    /* Initialize to 'zero' (even though they are not valid) */
    split->amount = gnc_numeric_zero();
    split->value = gnc_numeric_zero();

    return split;
}

static void
qif_split_destroy(QifSplit split)
{
    if (!split) return;

    g_free(split->memo);
    g_free(split->catstr);
    g_free(split->amountstr);

    g_free(split);
}

static QifSplit
qif_split_copy(QifSplit split)
{
    QifSplit s = qif_split_new();

    memcpy(s, split, sizeof(*s));
    if (s->memo) s->memo = g_strdup(s->memo);
    if (s->amountstr) s->amountstr = g_strdup(s->amountstr);
    if (s->catstr) s->memo = g_strdup(s->catstr);

    return s;
}

/* Forward declarations */
static void qif_txn_invst_destroy(QifInvstTxn);

/* QIF Transaction */

static void
qif_split_parse_category(QifContext ctx, QifSplit split)
{
    char *cat = NULL;
    char *cat_class = NULL;
    char *miscx_cat = NULL;
    char *miscx_class = NULL;

    gboolean miscx_is_acct;

    static GList *types = NULL;

    g_return_if_fail(ctx);
    g_return_if_fail(split);
    g_return_if_fail(split->cat.cat == NULL && split->cat_class == NULL);

    if (qif_parse_split_category(split->catstr,
                                 &cat, &split->cat_is_acct, &cat_class,
                                 &miscx_cat, &miscx_is_acct, &miscx_class))
    {
        g_assert(cat);

        if (split->cat_is_acct)
        {
            if (types == NULL)
                types = qif_parse_acct_type("__any_bank__", -1);

            split->cat.acct = find_or_make_acct(ctx, cat, types);

        }
        else
            split->cat.cat = find_or_make_cat(ctx, cat);

        if (cat_class)
            split->cat_class = find_or_make_class(ctx, cat_class);

        /* miscx isn't used in a normal transaction, so just ignore it */
        if (miscx_cat)
            g_free(miscx_cat);
        if (miscx_class)
            g_free(miscx_class);

    }
    else
        PERR("Problem parsing split category: %s", split->catstr);
}

static void
qif_txn_destroy(QifObject obj)
{
    QifTxn txn = (QifTxn) obj;
    GList *node;
    QifSplit split;

    g_free(txn->datestr);
    g_free(txn->payee);
    g_free(txn->address);
    g_free(txn->num);

    if (txn->invst_info)
        qif_txn_invst_destroy(txn->invst_info);

    for (node = txn->splits; node; node = node->next)
    {
        split = node->data;
        if (split == txn->default_split)
            txn->default_split = NULL;
        if (split == txn->current_split)
            txn->current_split = NULL;

        qif_split_destroy(split);
    }

    g_list_free(txn->splits);
    qif_split_destroy(txn->default_split);
    qif_split_destroy(txn->current_split);

    g_free(txn);
}

static QifTxn
qif_txn_new(void)
{
    QifObject obj;
    QifTxn txn;

    obj = qif_object_new(struct _QifTxn, "qif-txn", qif_txn_destroy);
    txn = (QifTxn) obj;
    txn->default_split = qif_split_new();

    return txn;
}

static void
qif_txn_init(QifContext ctx)
{
    qif_clear_flag(ctx->parse_flags, QIF_F_IGNORE_ACCOUNTS);
    ctx->parse_state = NULL;
}

static gboolean
qif_is_bad_numeric_string(const char* line)
{
    return (strncmp(line, "...", 3) == 0);
}

/*
 * this is called for the first transaction after each !Type: tag.
 *
 * if the first transaction after a !Type: tag has a payee of "Opening
 * Balance" or "Initial Balance", we have to massage the transaction a
 * little.  The meaning of an OB transaction is "transfer from Equity
 * to the account specified in the L line."  Idiomatically, ms-money
 * and some others use this transaction instead of an Account record
 * to specify "this" account (the from-account for all following
 * transactions), so we have to allow for that.
 *
 * Even if the payee isn't "Opening Balance", we if we have no default
 * from-account by this time we need to set one.  In that case we set
 * the default account based on the file name.
 *
 * If we DO know the account already, and this is a tranfer to it,
 * it's also an opening balance regardless of the payee.
 *
 * In the end make sure that the context 'current account' is set.
 */
static void
qif_process_opening_balance_txn(QifContext ctx, QifTxn txn)
{
    QifSplit split = txn->default_split;
    QifAccount cur_acct = NULL;	/* We know that ctx->current_acct is NULL */

    g_return_if_fail(txn->invst_info == NULL);

    if ((!cur_acct && txn->payee &&
            (!strcasecmp(txn->payee, "Opening Balance") ||
             !strcasecmp(txn->payee, "Initial Balance")) && split->cat_is_acct) ||
            (cur_acct &&
             ((split->cat_is_acct && !strcasecmp(split->cat.acct->name, cur_acct->name))
              ||
              (!split->cat_is_acct && !strcasecmp(split->cat.cat->name, cur_acct->name))))
       )
    {

        /* This is an explicit "Opening Balance" transactions.  We need to
         * change the "from account" to point to the equity account that
         * the opening balance comes from...
         */
        if (split->cat_is_acct)
            cur_acct = split->cat.acct;
        else
        {
            g_assert(split->cat.cat);
            cur_acct = find_or_make_acct(ctx, g_strdup(split->cat.cat->name),
                                         qif_parse_acct_type_guess(txn->txn_type));
            split->cat_is_acct = TRUE;
        }
        split->cat.acct = qif_default_equity_acct(ctx);
    }

    /*
     * If we found an opening balance account then set up the context.
     * If we didn't actually succeed in finding an account then
     * set a flag so we can go back later and look for it.
     */

    if (cur_acct)
    {
        ctx->opening_bal_acct = cur_acct;
        ctx->current_acct = cur_acct;
    }
    else
        qif_set_flag(ctx->parse_flags, QIF_F_TXN_NEEDS_ACCT);
}

/* process all the splits in the transaction -- if this is a "split
 * transaction" then make sure the sum of all the amounts (including
 * the default split) does NOT equal zero -- if it does then we want
 * to reverse all the splits.  The "amount" should be the 'T' amount
 * from the txn.
 */
static void
qif_txn_fix_amounts(QifTxn txn, gnc_numeric amount)
{
    gnc_numeric sum = amount;
    QifSplit split;
    GList *node;

    g_return_if_fail(txn);

    /* No current_split, so this is NOT a split transaction. */
    if (!txn->current_split) return;

    /* Then add in every split in the split-list */
    for (node = txn->splits; node; node = node->next)
    {
        split = node->data;
        sum = gnc_numeric_add(sum, split->amount, GNC_DENOM_AUTO, GNC_DENOM_LCD);
    }

    /* if the sum is not zero then reverse all the amounts in the split list */
    if (!gnc_numeric_zero_p(sum))
        for (node = txn->splits; node; node = node->next)
        {
            split = node->data;
            split->amount = gnc_numeric_neg(split->amount);
        }
}

static QifError
qif_txn_parse(QifContext ctx, GList *record)
{
    QifTxn txn;
    QifLine line;
    GList *node;
    QifSplit split;

    g_return_val_if_fail(ctx, QIF_E_INTERNAL);
    g_return_val_if_fail(record, QIF_E_BADSTATE);

    txn = qif_txn_new();
    txn->txn_type = ctx->parse_type;

    for (; record; record = record->next)
    {
        line = record->data;

        switch (line->type)
        {
        case 'D':			/* D : transaction date */
            qif_save_str(txn->datestr);
            break;
        case 'P':			/* P : payee */
            qif_save_str(txn->payee);
            break;
        case 'A':			/* A : address */
            /* multiple 'A' lines are appended together with newlines */
            if (txn->address)
            {
                char *tmp = txn->address;
                txn->address = g_strconcat(tmp, "\n", line->line, NULL);
                g_free(tmp);
            }
            else
                qif_save_str(txn->address);
            break;
        case 'N':			/* N : check/transaction number */
            qif_save_str(txn->num);
            break;
        case 'C':			/* C : transaction cleared flag */
            txn->cleared = qif_parse_cleared(line);
            break;
        case 'L':			/* L : default split category */
            if (!txn->current_split) qif_save_str(txn->default_split->catstr);
            break;
        case 'M':			/* M : default split memo */
            if (!txn->current_split) qif_save_str(txn->default_split->memo);
            break;
        case 'T':			/* T : total transaction amount */
            if (!txn->current_split && !qif_is_bad_numeric_string(line->line))
                qif_save_str(txn->default_split->amountstr);
            break;
        case 'S':			/* S : split category */
            /* This implies a quicken-style "split transaction", so we're mostly
             * going to ignore the default_split except for internal verification.
             */
            txn->current_split = qif_split_new();
            txn->splits = g_list_prepend(txn->splits, txn->current_split);
            qif_save_str(txn->current_split->catstr);
            break;
        case 'E':			/* E : split memo */
            if (txn->current_split)
                qif_save_str(txn->current_split->memo);
            break;
        case '$':			/* split amount */
            if (txn->current_split && !qif_is_bad_numeric_string(line->line))
                qif_save_str(txn->current_split->amountstr);
            break;
        default:
            PERR("Unknown QIF transaction data at line %d: %s", line->lineno, line->line);
        }
    }

    /* If we have no date string then there is no reason to do anything else */
    if (txn->datestr)
    {
        /* We delay processing the date and amount strings until later.. */

        /* parse the category on each split */
        for (node = txn->splits; node; node = node->next)
        {
            split = node->data;
            if (split->catstr)
                qif_split_parse_category(ctx, split);
        }
        /* ... including the default split */
        if (txn->default_split->catstr)
            qif_split_parse_category(ctx, txn->default_split);

        /* if we don't have an account, then deal with the opening balance */
        if (!ctx->current_acct)
            qif_process_opening_balance_txn(ctx, txn);

        /* Set the transaction's from account */
        txn->from_acct = ctx->current_acct;

        /* And add it to the process list */
        ctx->parse_state = g_list_prepend(ctx->parse_state, txn);

    }
    else
        /* no date?  Ignore this txn */
        qif_txn_destroy((QifObject)txn);

    return QIF_E_OK;
}

/* after we parse the amounts, fix up the transaction splits */
void
qif_txn_setup_splits(QifTxn txn)
{
    QifSplit split, this_split;
    GList *node;
    gnc_numeric total;

    if (txn->splits)
    {
        /* We have a bunch of "far" splits -- maybe fix up the totals.. */
        qif_txn_fix_amounts(txn, txn->default_split->amount);

        /* Re-Compute the total for the "near" (default) split */
        total = gnc_numeric_zero();
        for (node = txn->splits; node; node = node->next)
        {
            split = node->data;
            split->value = split->amount;
            total = gnc_numeric_add(total, split->amount, 0, GNC_DENOM_LCD);
        }

        /* And re-set the default-split amount */
        txn->default_split->amount = gnc_numeric_neg(total);

    }
    else
    {
        /* not a split txn.  Compute the "far" split by copying the "near"
         *  split and then moving the 'near' split to the far split.
         */

        /* First make a copy of this transaction and move the copy to the 'near' */
        split = txn->default_split;
        this_split = qif_split_copy(split);
        txn->default_split = this_split;

        /* then adjust the 'far' txn */
        split->amount = gnc_numeric_neg(split->amount);
        split->value = split->amount;
        txn->splits = g_list_prepend(txn->splits, split);
    }

    /* Set the default-split value from the default-split amount */
    txn->default_split->value = txn->default_split->amount;
}

/* This is called when we're done processing an account.  We want
 * to merge the transactions in the "parse_state" into the Qif Context
 */
static QifError
qif_txn_end_acct(QifContext ctx)
{
    GList *node;
    QifTxn txn;
    gboolean txn_needs_acct;

    g_return_val_if_fail(ctx, QIF_E_INTERNAL);

    /* Return now if there is nothing to do. */
    if (!ctx->parse_state) return QIF_E_OK;

    /* Walk through the list of transactions.  First check if it
     * needs a from-account; then add it to the context.
     */

    txn_needs_acct = (ctx->parse_flags & QIF_F_TXN_NEEDS_ACCT);

    /* Invert the list so we're working in the right order */
    ctx->parse_state = g_list_reverse(ctx->parse_state);

    for (node = ctx->parse_state; node; node = node->next)
    {
        txn = node->data;

        /* If we need a from account, then set it.. */
        if (txn_needs_acct && ctx->opening_bal_acct && !txn->from_acct)
            txn->from_acct = ctx->opening_bal_acct;

        /* merge the txn into the context (prepends to the list) */
        qif_object_list_insert(ctx, (QifObject)txn);
    }

    if (txn_needs_acct && ctx->opening_bal_acct)
        qif_clear_flag(ctx->parse_flags, QIF_F_TXN_NEEDS_ACCT);

    /* clean up our state */
    g_list_free(ctx->parse_state);
    ctx->parse_state = NULL;

    return QIF_E_OK;
}

/* Extra info in an Investment Transaction */
static QifInvstTxn
qif_invst_txn_new(void)
{
    QifInvstTxn itxn = g_new0(struct _QifInvstTxn, 1);

    itxn->amount = gnc_numeric_zero();
    itxn->d_amount = gnc_numeric_zero();
    itxn->price = gnc_numeric_zero();
    itxn->shares = gnc_numeric_zero();
    itxn->commission = gnc_numeric_zero();

    return itxn;
}

static void
qif_txn_invst_destroy(QifInvstTxn itxn)
{
    if (!itxn) return;

    g_free(itxn->amountstr);
    g_free(itxn->d_amountstr);
    g_free(itxn->pricestr);
    g_free(itxn->sharesstr);
    g_free(itxn->commissionstr);
    g_free(itxn->security);

    g_free(itxn->catstr);

    g_free(itxn);
}

static QifError
qif_txn_invst_parse(QifContext ctx, GList *record)
{
    QifTxn txn;
    QifInvstTxn itxn;
    QifLine line;

    g_return_val_if_fail(ctx, QIF_E_INTERNAL);
    g_return_val_if_fail(record, QIF_E_BADSTATE);

    txn = qif_txn_new();
    txn->txn_type = ctx->parse_type;
    itxn = qif_invst_txn_new();
    txn->invst_info = itxn;

    for (; record; record = record->next)
    {
        line = record->data;

        switch (line->type)
        {
        case 'D':			/* D : transaction date */
            qif_save_str(txn->datestr);
            break;
        case 'P':			/* P : txn payee */
            qif_save_str(txn->payee);
            break;
        case 'N':			/* N : action */
            itxn->action = qif_parse_action(line);
            break;
        case 'C':			/* C : cleared flag */
            txn->cleared = qif_parse_cleared(line);
            break;
        case 'M':			/* M : memo */
            if (!txn->current_split)
                qif_save_str(txn->default_split->memo);
            break;
        case 'T':			/* T : total amount */
            if (!qif_is_bad_numeric_string(line->line))
                qif_save_str(itxn->amountstr);
            break;
        case '$':			/* $ : transfer amount */
            if (!qif_is_bad_numeric_string(line->line))
                qif_save_str(itxn->d_amountstr);
            break;
        case 'I':			/* I : share price */
            qif_save_str(itxn->pricestr);
            break;
        case 'Q':			/* Q : number of shares */
            qif_save_str(itxn->sharesstr);
            break;
        case 'Y':			/* Y : name of security */
            qif_save_str(itxn->security);
            break;
        case 'O':			/* O : commission */
            qif_save_str(itxn->commissionstr);
            break;
        case 'L':			/* L : category */
            qif_save_str(itxn->catstr);
            break;
        default:
            PERR("Unknown QIF Investment transaction data at line %d: %s",
                 line->lineno, line->line);
        }
    }

    /* If we have no date string then there is no reason to do anything else */
    if (txn->datestr && itxn->action != QIF_A_NONE)
    {

        /* Make sure we've got a security name */
        if (!itxn->security)
            itxn->security = g_strdup("");	/* XXX */

        /* if we don't have a from account, then mark the fact that
         * we'll need one later.
         */
        if (ctx->current_acct)
            txn->from_acct = ctx->current_acct;
        else
            qif_set_flag(ctx->parse_flags, QIF_F_ITXN_NEEDS_ACCT);

        /* Add this transaction to the parse state for later processing */
        ctx->parse_state = g_list_prepend(ctx->parse_state, txn);

    }
    else
    {
        /* no date?  Just destroy it */
        qif_txn_destroy((QifObject)txn);
    }

    return QIF_E_OK;
}


void
qif_invst_txn_setup_splits(QifContext ctx, QifTxn txn)
{
    QifInvstTxn itxn;
    QifSplit near_split, far_split, comm_split;
    QifAccount from_acct;

    char *cat = NULL;
    char *cat_class = NULL;
    gboolean cat_is_acct = FALSE;
    char *miscx = NULL;
    char *miscx_class = NULL;
    gboolean miscx_is_acct = FALSE;

    /* Cached account-type lists */
    static GList *bank_list = NULL;

    gnc_numeric split_value;

    g_return_if_fail(ctx);
    g_return_if_fail(txn);
    g_return_if_fail(txn->invst_info);

    itxn = txn->invst_info;

    /* Compute the share value, because we'll probably need it */
    split_value = gnc_numeric_mul(itxn->shares, itxn->price, 0, GNC_DENOM_REDUCE);

    /* Make sure that "amount" is a valid "transaction amount" */
    if (!itxn->amountstr && itxn->d_amountstr)
        itxn->amount = itxn->d_amount;

    /* near and far splits..  for simplicity */
    near_split = txn->default_split;
    far_split = qif_split_new();
    from_acct = txn->from_acct;

    /* Parse the category string */
    if (!qif_parse_split_category(itxn->catstr,
                                  &cat, &cat_is_acct, &cat_class,
                                  &miscx, &miscx_is_acct, &miscx_class))
        PERR("Failure parsing category: %s", itxn->catstr);

    /* Make sure we've got a cached list */
    if (bank_list == NULL)
        bank_list = qif_parse_acct_type("__any_bank__", -1);

    /* find the NEAR account */

    switch (itxn->action)
    {
    case QIF_A_BUY:
    case QIF_A_BUYX:
    case QIF_A_REINVDIV:
    case QIF_A_REINVINT:
    case QIF_A_REINVLG:
    case QIF_A_REINVMD:
    case QIF_A_REINVSG:
    case QIF_A_REINVSH:
    case QIF_A_SELL:
    case QIF_A_SELLX:
    case QIF_A_SHRSIN:
    case QIF_A_SHRSOUT:
    case QIF_A_STKSPLIT:
        txn->from_acct = qif_default_stock_acct(ctx, itxn->security);
        break;

    case QIF_A_CGLONG:
    case QIF_A_CGMID:
    case QIF_A_CGSHORT:
    case QIF_A_DIV:
    case QIF_A_INTINC:
    case QIF_A_MARGINT:
    case QIF_A_MISCEXP:
    case QIF_A_MISCINC:
    case QIF_A_RTRNCAP:
    case QIF_A_XIN:
    case QIF_A_XOUT:
        txn->from_acct = from_acct;
        break;

    case QIF_A_CGLONGX:
    case QIF_A_CGMIDX:
    case QIF_A_CGSHORTX:
    case QIF_A_DIVX:
    case QIF_A_INTINCX:
    case QIF_A_MARGINTX:
    case QIF_A_RTRNCAPX:
        txn->from_acct = find_or_make_acct(ctx, cat, bank_list);
        cat = NULL;
        break;

    case QIF_A_MISCEXPX:
    case QIF_A_MISCINCX:
        txn->from_acct = find_or_make_acct(ctx, miscx, bank_list);
        miscx = NULL;
        break;

    default:
        PERR("Unhandled Action: %d", itxn->action);
        break;
    }

    /* find the FAR account */

    itxn->far_cat_is_acct = TRUE;
    switch (itxn->action)
    {
    case QIF_A_BUY:
    case QIF_A_SELL:
        itxn->far_cat.acct = from_acct;
        break;

    case QIF_A_BUYX:
    case QIF_A_MISCEXP:
    case QIF_A_MISCEXPX:
    case QIF_A_MISCINC:
    case QIF_A_MISCINCX:
    case QIF_A_SELLX:
    case QIF_A_XIN:
    case QIF_A_XOUT:
        itxn->far_cat.cat = find_or_make_cat(ctx, cat);
        itxn->far_cat_is_acct = FALSE;
        cat = NULL;
        break;

    case QIF_A_CGLONG:
    case QIF_A_CGLONGX:
    case QIF_A_REINVLG:
        itxn->far_cat.acct = qif_default_cglong_acct(ctx, itxn->security);
        break;

    case QIF_A_CGMID:
    case QIF_A_CGMIDX:
    case QIF_A_REINVMD:
        itxn->far_cat.acct = qif_default_cgmid_acct(ctx, itxn->security);
        break;

    case QIF_A_CGSHORT:
    case QIF_A_CGSHORTX:
    case QIF_A_REINVSG:
    case QIF_A_REINVSH:
        itxn->far_cat.acct = qif_default_cgshort_acct(ctx, itxn->security);
        break;

    case QIF_A_DIV:
    case QIF_A_DIVX:
    case QIF_A_REINVDIV:
        itxn->far_cat.acct = qif_default_dividend_acct(ctx, itxn->security);
        break;

    case QIF_A_INTINC:
    case QIF_A_INTINCX:
    case QIF_A_REINVINT:
        itxn->far_cat.acct = qif_default_interest_acct(ctx, itxn->security);
        break;

    case QIF_A_MARGINT:
    case QIF_A_MARGINTX:
        itxn->far_cat.acct = qif_default_margin_interest_acct(ctx);
        break;

    case QIF_A_RTRNCAP:
    case QIF_A_RTRNCAPX:
        itxn->far_cat.acct = qif_default_capital_return_acct(ctx, itxn->security);
        break;

    case QIF_A_SHRSIN:
    case QIF_A_SHRSOUT:
        itxn->far_cat.acct = qif_default_equity_holding(ctx, itxn->security);
        break;

    case QIF_A_STKSPLIT:
        itxn->far_cat.acct = qif_default_stock_acct(ctx, itxn->security);
        break;

    default:
        break;
    }

    /* If we dont have a far acct (or far category) then reset the flag */
    if (!itxn->far_cat.obj)
        itxn->far_cat_is_acct = FALSE;

    /* And now fill in the "near" and "far" splits.  In particular we need
     *
     *	NEAR:	txn->from_acct, near_split->amount, value
     *	FAR:	cat, far_split->amount, value
     */
    switch (itxn->action)
    {
    case QIF_A_BUY:
    case QIF_A_BUYX:
    case QIF_A_REINVDIV:
    case QIF_A_REINVINT:
    case QIF_A_REINVLG:
    case QIF_A_REINVMD:
    case QIF_A_REINVSG:
    case QIF_A_REINVSH:
    case QIF_A_SHRSIN:
        near_split->amount = itxn->shares;
        near_split->value = split_value;
        far_split->amount = far_split->value = gnc_numeric_neg(itxn->amount);
        break;

    case QIF_A_SELL:
    case QIF_A_SELLX:
    case QIF_A_SHRSOUT:
        near_split->amount = gnc_numeric_neg(itxn->shares);
        near_split->value = gnc_numeric_neg(split_value);
        far_split->amount = far_split->value = itxn->amount;
        break;

    case QIF_A_CGLONG:
    case QIF_A_CGLONGX:
    case QIF_A_CGMID:
    case QIF_A_CGMIDX:
    case QIF_A_CGSHORT:
    case QIF_A_CGSHORTX:
    case QIF_A_DIV:
    case QIF_A_DIVX:
    case QIF_A_INTINC:
    case QIF_A_INTINCX:
    case QIF_A_MISCINC:
    case QIF_A_MISCINCX:
    case QIF_A_RTRNCAP:
    case QIF_A_RTRNCAPX:
    case QIF_A_XIN:
        near_split->amount = near_split->value = itxn->amount;
        far_split->amount = far_split->value = gnc_numeric_neg(itxn->amount);
        break;

    case QIF_A_MARGINT:
    case QIF_A_MARGINTX:
    case QIF_A_MISCEXP:
    case QIF_A_MISCEXPX:
    case QIF_A_XOUT:
        near_split->amount = near_split->value = gnc_numeric_neg(itxn->amount);
        far_split->amount = far_split->value = itxn->amount;
        break;

    case QIF_A_STKSPLIT:
        /* QIF just specifies the split ratio, not the number of shares
         * in and out, so we have to fetch the number of shares from the
         * security account..  FEH!
         */

        near_split->value = gnc_numeric_neg(split_value);
        far_split->value = split_value;

        /* XXX: FIXME: compute in-shares/out-shares based on ratio here:
         *
         * splitratio = num-shares / 10;
         * in_shares = gnc_account_get_balance(near_acct);
         * out_shares = in_shares * splitratio;
         *
         * near_split->amount = out_shares;
         * far_split->amount = gnc_numeric_neg(in_shares);
         *
         * We know (later) that near_split == txn->default_split and
         * far_split == txn->splits->data, so we'll just special-case this
         * kind of txn when we convert to GNC later.
         */

        break;

    default:
        break;
    }

    /* Just make sure to set that it's an account, not a category */
    far_split->cat.obj = itxn->far_cat.obj;
    if (itxn->far_cat_is_acct)
        far_split->cat_is_acct = TRUE;

    /* make the commission split if we need it, then add it to the split-list  */
    if (itxn->commissionstr)
    {
        comm_split = qif_split_new();
        comm_split->cat.acct = qif_default_commission_acct(ctx);
        comm_split->cat_is_acct = TRUE;
        comm_split->amount = itxn->commission;
        comm_split->value = itxn->commission;

        txn->splits = g_list_prepend(txn->splits, comm_split);
    }

    /* Push the "far split" into the txn split-list */
    txn->splits = g_list_prepend(txn->splits, far_split);

    /* Free parsed strings.. */
    g_free(cat);
    g_free(cat_class);
    g_free(miscx);
    g_free(miscx_class);
}


/* Other handlers */
static void
qif_autoswitch_set(QifContext ctx)
{
    qif_set_flag(ctx->parse_flags, QIF_F_IGNORE_ACCOUNTS);
}

static void
qif_autoswitch_clear(QifContext ctx)
{
    qif_clear_flag(ctx->parse_flags, QIF_F_IGNORE_ACCOUNTS);
}

/********************************************************************************
 * find or make ...
 */

QifAccount
find_or_make_acct(QifContext ctx, char *name, GList *types)
{
    QifAccount res;

    res = (QifAccount)qif_object_map_lookup(ctx, QIF_O_ACCOUNT, name);
    if (res)
        g_free(name);
    else
    {
        res = qif_account_new();
        res->name = name;
        res->type_list = types;

        qif_object_map_insert(ctx, name, (QifObject)res);
    }

    return res;
}

QifCategory
find_or_make_cat(QifContext ctx, char *name)
{
    QifCategory res;

    res = (QifCategory)qif_object_map_lookup(ctx, QIF_O_CATEGORY, name);
    if (res)
        g_free(name);
    else
    {
        res = qif_cat_new();

        res->name = name;

        qif_object_map_insert(ctx, name, (QifObject)res);
    }

    return res;
}

QifClass
find_or_make_class(QifContext ctx, char *name)
{
    QifClass res;

    res = (QifClass)qif_object_map_lookup(ctx, QIF_O_CLASS, name);
    if (res)
        g_free(name);
    else
    {
        res = qif_class_new();
        res->name = name;
        qif_object_map_insert(ctx, name, (QifObject)res);
    }
    return res;
}

/*****************************************************************************/

/*
 * initialize handlers
 */
void
qif_object_init(void)
{
    int i;
    static struct
    {
        QifType		type;
        struct _QifHandler	handler;
    } handlers[] =
    {
        { QIF_TYPE_BANK, { qif_txn_init, qif_txn_parse, qif_txn_end_acct } },
        { QIF_TYPE_CASH, { qif_txn_init, qif_txn_parse, qif_txn_end_acct } },
        { QIF_TYPE_CCARD, { qif_txn_init, qif_txn_parse, qif_txn_end_acct } },
        { QIF_TYPE_INVST, { qif_txn_init, qif_txn_invst_parse, qif_txn_end_acct } },
        { QIF_TYPE_PORT, { qif_txn_init, qif_txn_invst_parse, qif_txn_end_acct } },
        { QIF_TYPE_OTH_A, { qif_txn_init, qif_txn_parse, qif_txn_end_acct } },
        { QIF_TYPE_OTH_L, { qif_txn_init, qif_txn_parse, qif_txn_end_acct } },
        { QIF_TYPE_CLASS, { NULL, qif_class_parse, NULL } },
        { QIF_TYPE_CAT, { NULL, qif_cat_parse, NULL } },
        { QIF_TYPE_SECURITY, { NULL, qif_security_parse, NULL } },
        { QIF_ACCOUNT, { NULL, qif_account_parse, NULL } },
        { QIF_AUTOSWITCH, { qif_autoswitch_set, NULL, NULL } },
        { QIF_CLEAR_AUTOSWITCH, { qif_autoswitch_clear, NULL, NULL } },
        { 0, {NULL, NULL, NULL} }
    };

    for (i = 0; handlers[i].type > 0; i++)
    {
        if (handlers[i].type <= 0)
        {
            PERR("Invalid type?!?  (%d @ %d)", handlers[i].type, i);
        }
        else
            qif_register_handler(handlers[i].type, &(handlers[i].handler));
    }
}
