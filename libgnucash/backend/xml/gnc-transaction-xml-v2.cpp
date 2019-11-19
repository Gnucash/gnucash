/********************************************************************
 * gnc-transactions-xml-v2.c -- xml routines for transactions       *
 * Copyright (C) 2001 Rob Browning                                  *
 * Copyright (C) 2002 Linas Vepstas <linas@linas.org>               *
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
 *******************************************************************/
extern "C"
{
#include <config.h>

#include <glib.h>
#include <string.h>
#include "AccountP.h"
#include "Transaction.h"
#include "TransactionP.h"
#include "gnc-lot.h"
#include "gnc-lot-p.h"
}
#include "gnc-xml-helper.h"

#include "sixtp.h"
#include "sixtp-utils.h"
#include "sixtp-parsers.h"
#include "sixtp-utils.h"
#include "sixtp-dom-parsers.h"
#include "sixtp-dom-generators.h"

#include "gnc-xml.h"

#include "io-gncxml-gen.h"

#include "sixtp-dom-parsers.h"

const gchar* transaction_version_string = "2.0.0";

static void
add_gnc_num (xmlNodePtr node, const gchar* tag, gnc_numeric num)
{
    xmlAddChild (node, gnc_numeric_to_dom_tree (tag, &num));
}

static void
add_time64 (xmlNodePtr node, const gchar * tag, time64 time, gboolean always)
{
    if (always || time)
        xmlAddChild (node, time64_to_dom_tree (tag, time));
}

static xmlNodePtr
split_to_dom_tree (const gchar* tag, Split* spl)
{
    xmlNodePtr ret;

    ret = xmlNewNode (NULL, BAD_CAST tag);

    xmlAddChild (ret, guid_to_dom_tree ("split:id", xaccSplitGetGUID (spl)));

    {
        char* memo = g_strdup (xaccSplitGetMemo (spl));

        if (memo && g_strcmp0 (memo, "") != 0)
        {
            xmlNewTextChild (ret, NULL, BAD_CAST "split:memo",
                             checked_char_cast (memo));
        }
        g_free (memo);
    }

    {
        char* action = g_strdup (xaccSplitGetAction (spl));

        if (action && g_strcmp0 (action, "") != 0)
        {
            xmlNewTextChild (ret, NULL, BAD_CAST "split:action",
                             checked_char_cast (action));
        }
        g_free (action);
    }

    {
        char tmp[2];

        tmp[0] = xaccSplitGetReconcile (spl);
        tmp[1] = '\0';

        xmlNewTextChild (ret, NULL, BAD_CAST "split:reconciled-state",
                         BAD_CAST tmp);
    }

    add_time64 (ret, "split:reconcile-date",
                xaccSplitGetDateReconciled (spl), FALSE);

    add_gnc_num (ret, "split:value", xaccSplitGetValue (spl));

    add_gnc_num (ret, "split:quantity", xaccSplitGetAmount (spl));

    {
        Account* account = xaccSplitGetAccount (spl);

        xmlAddChild (ret, guid_to_dom_tree ("split:account",
                                            xaccAccountGetGUID (account)));
    }
    {
        GNCLot* lot = xaccSplitGetLot (spl);

        if (lot)
        {
            xmlAddChild (ret, guid_to_dom_tree ("split:lot",
                                                gnc_lot_get_guid (lot)));
        }
    }
    /* xmlAddChild won't do anything with a NULL, so tests are superfluous. */
    xmlAddChild (ret, qof_instance_slots_to_dom_tree ("split:slots",
                                                      QOF_INSTANCE (spl)));
    return ret;
}

static void
add_trans_splits (xmlNodePtr node, Transaction* trn)
{
    GList* n;
    xmlNodePtr toaddto;

    toaddto = xmlNewChild (node, NULL, BAD_CAST "trn:splits", NULL);

    for (n = xaccTransGetSplitList (trn); n; n = n->next)
    {
        Split* s = static_cast<decltype (s)> (n->data);
        xmlAddChild (toaddto, split_to_dom_tree ("trn:split", s));
    }
}

xmlNodePtr
gnc_transaction_dom_tree_create (Transaction* trn)
{
    xmlNodePtr ret;
    gchar* str = NULL;

    ret = xmlNewNode (NULL, BAD_CAST "gnc:transaction");

    xmlSetProp (ret, BAD_CAST "version",
                BAD_CAST transaction_version_string);

    xmlAddChild (ret, guid_to_dom_tree ("trn:id", xaccTransGetGUID (trn)));

    xmlAddChild (ret, commodity_ref_to_dom_tree ("trn:currency",
                                                 xaccTransGetCurrency (trn)));
    str = g_strdup (xaccTransGetNum (trn));
    if (str && (g_strcmp0 (str, "") != 0))
    {
        xmlNewTextChild (ret, NULL, BAD_CAST "trn:num",
                         checked_char_cast (str));
    }
    g_free (str);

    add_time64 (ret, "trn:date-posted", xaccTransRetDatePosted (trn), TRUE);

    add_time64 (ret, "trn:date-entered",
                  xaccTransRetDateEntered (trn), TRUE);

    str = g_strdup (xaccTransGetDescription (trn));
    if (str)
    {
        xmlNewTextChild (ret, NULL, BAD_CAST "trn:description",
                         checked_char_cast (str));
    }
    g_free (str);

    /* xmlAddChild won't do anything with a NULL, so tests are superfluous. */
    xmlAddChild (ret, qof_instance_slots_to_dom_tree ("trn:slots",
                                                      QOF_INSTANCE (trn)));

    add_trans_splits (ret, trn);

    return ret;
}

/***********************************************************************/

struct split_pdata
{
    Split* split;
    QofBook* book;
};

static inline gboolean
set_spl_string (xmlNodePtr node, Split* spl,
                void (*func) (Split* spl, const char* txt))
{
    gchar* tmp = dom_tree_to_text (node);
    g_return_val_if_fail (tmp, FALSE);

    func (spl, tmp);

    g_free (tmp);

    return TRUE;
}

static inline gboolean
set_spl_gnc_num (xmlNodePtr node, Split* spl,
                 void (*func) (Split* spl, gnc_numeric gn))
{
    gnc_numeric* num = dom_tree_to_gnc_numeric (node);
    g_return_val_if_fail (num, FALSE);

    func (spl, *num);

    g_free (num);

    return FALSE;
}

static gboolean
spl_id_handler (xmlNodePtr node, gpointer data)
{
    struct split_pdata* pdata = static_cast<decltype (pdata)> (data);
    GncGUID* tmp = dom_tree_to_guid (node);
    g_return_val_if_fail (tmp, FALSE);

    xaccSplitSetGUID (pdata->split, tmp);

    guid_free (tmp);
    return TRUE;
}

static gboolean
spl_memo_handler (xmlNodePtr node, gpointer data)
{
    struct split_pdata* pdata = static_cast<decltype (pdata)> (data);
    return set_spl_string (node, pdata->split, xaccSplitSetMemo);
}

static gboolean
spl_action_handler (xmlNodePtr node, gpointer data)
{
    struct split_pdata* pdata = static_cast<decltype (pdata)> (data);
    return set_spl_string (node, pdata->split, xaccSplitSetAction);
}

static gboolean
spl_reconciled_state_handler (xmlNodePtr node, gpointer data)
{
    struct split_pdata* pdata = static_cast<decltype (pdata)> (data);
    gchar* tmp = dom_tree_to_text (node);
    g_return_val_if_fail (tmp, FALSE);

    xaccSplitSetReconcile (pdata->split, tmp[0]);

    g_free (tmp);

    return TRUE;
}

static gboolean
spl_reconcile_date_handler (xmlNodePtr node, gpointer data)
{
    struct split_pdata* pdata = static_cast<decltype (pdata)> (data);
    time64 time  = dom_tree_to_time64 (node);
    if (!dom_tree_valid_time64 (time, node->name)) time = 0;
    xaccSplitSetDateReconciledSecs (pdata->split, time);
    return TRUE;
}

static gboolean
spl_value_handler (xmlNodePtr node, gpointer data)
{
    struct split_pdata* pdata = static_cast<decltype (pdata)> (data);
    return set_spl_gnc_num (node, pdata->split, xaccSplitSetValue);
}

static gboolean
spl_quantity_handler (xmlNodePtr node, gpointer data)
{
    struct split_pdata* pdata = static_cast<decltype (pdata)> (data);
    return set_spl_gnc_num (node, pdata->split, xaccSplitSetAmount);
}

gboolean gnc_transaction_xml_v2_testing = FALSE;

static gboolean
spl_account_handler (xmlNodePtr node, gpointer data)
{
    struct split_pdata* pdata = static_cast<decltype (pdata)> (data);
    GncGUID* id = dom_tree_to_guid (node);
    Account* account;

    g_return_val_if_fail (id, FALSE);

    account = xaccAccountLookup (id, pdata->book);
    if (!account && gnc_transaction_xml_v2_testing &&
        !guid_equal (id, guid_null ()))
    {
        account = xaccMallocAccount (pdata->book);
        xaccAccountSetGUID (account, id);
        xaccAccountSetCommoditySCU (account,
                                    xaccSplitGetAmount (pdata->split).denom);
    }

    xaccAccountInsertSplit (account, pdata->split);

    guid_free (id);

    return TRUE;
}

static gboolean
spl_lot_handler (xmlNodePtr node, gpointer data)
{
    struct split_pdata* pdata = static_cast<decltype (pdata)> (data);
    GncGUID* id = dom_tree_to_guid (node);
    GNCLot* lot;

    g_return_val_if_fail (id, FALSE);

    lot = gnc_lot_lookup (id, pdata->book);
    if (!lot && gnc_transaction_xml_v2_testing &&
        !guid_equal (id, guid_null ()))
    {
        lot = gnc_lot_new (pdata->book);
        gnc_lot_set_guid (lot, *id);
    }

    gnc_lot_add_split (lot, pdata->split);

    guid_free (id);

    return TRUE;
}

static gboolean
spl_slots_handler (xmlNodePtr node, gpointer data)
{
    struct split_pdata* pdata = static_cast<decltype (pdata)> (data);
    gboolean successful;

    successful = dom_tree_create_instance_slots (node,
                                                 QOF_INSTANCE (pdata->split));
    g_return_val_if_fail (successful, FALSE);

    return TRUE;
}

struct dom_tree_handler spl_dom_handlers[] =
{
    { "split:id", spl_id_handler, 1, 0 },
    { "split:memo", spl_memo_handler, 0, 0 },
    { "split:action", spl_action_handler, 0, 0 },
    { "split:reconciled-state", spl_reconciled_state_handler, 1, 0 },
    { "split:reconcile-date", spl_reconcile_date_handler, 0, 0 },
    { "split:value", spl_value_handler, 1, 0 },
    { "split:quantity", spl_quantity_handler, 1, 0 },
    { "split:account", spl_account_handler, 1, 0 },
    { "split:lot", spl_lot_handler, 0, 0 },
    { "split:slots", spl_slots_handler, 0, 0 },
    { NULL, NULL, 0, 0 },
};

static Split*
dom_tree_to_split (xmlNodePtr node, QofBook* book)
{
    struct split_pdata pdata;
    Split* ret;

    g_return_val_if_fail (book, NULL);

    ret = xaccMallocSplit (book);
    g_return_val_if_fail (ret, NULL);

    pdata.split = ret;
    pdata.book = book;

    /* this isn't going to work in a testing setup */
    if (dom_tree_generic_parse (node, spl_dom_handlers, &pdata))
    {
        return ret;
    }
    else
    {
        xaccSplitDestroy (ret);
        return NULL;
    }
}

/***********************************************************************/

struct trans_pdata
{
    Transaction* trans;
    QofBook* book;
};

static inline gboolean
set_tran_string (xmlNodePtr node, Transaction* trn,
                 void (*func) (Transaction* trn, const char* txt))
{
    gchar* tmp;

    tmp = dom_tree_to_text (node);

    g_return_val_if_fail (tmp, FALSE);

    func (trn, tmp);

    g_free (tmp);

    return TRUE;
}

static gboolean
set_tran_time64 (xmlNodePtr node, Transaction * trn,
        void (*func) (Transaction *, time64))
{
    time64 time = dom_tree_to_time64 (node);
    if (!dom_tree_valid_time64 (time, node->name)) time = 0;
    func (trn, time);
    return TRUE;
}

static inline gboolean
set_tran_date (xmlNodePtr node, Transaction* trn,
               void (*func) (Transaction* trn, const time64 tm))
{
    time64 time = dom_tree_to_time64 (node);
    if (!dom_tree_valid_time64 (time, node->name)) time = 0;
    func (trn, time);
    return TRUE;
}

static gboolean
trn_id_handler (xmlNodePtr node, gpointer trans_pdata)
{
    struct trans_pdata* pdata = static_cast<decltype (pdata)> (trans_pdata);
    Transaction* trn = pdata->trans;
    GncGUID* tmp = dom_tree_to_guid (node);

    g_return_val_if_fail (tmp, FALSE);

    xaccTransSetGUID ((Transaction*)trn, tmp);

    guid_free (tmp);

    return TRUE;
}

static gboolean
trn_currency_handler (xmlNodePtr node, gpointer trans_pdata)
{
    struct trans_pdata* pdata = static_cast<decltype (pdata)> (trans_pdata);
    Transaction* trn = pdata->trans;
    gnc_commodity* ref;

    ref = dom_tree_to_commodity_ref (node, pdata->book);
    xaccTransSetCurrency (trn, ref);

    return TRUE;
}

static gboolean
trn_num_handler (xmlNodePtr node, gpointer trans_pdata)
{
    struct trans_pdata* pdata = static_cast<decltype (pdata)> (trans_pdata);
    Transaction* trn = pdata->trans;

    return set_tran_string (node, trn, xaccTransSetNum);
}

static gboolean
trn_date_posted_handler (xmlNodePtr node, gpointer trans_pdata)
{
    struct trans_pdata* pdata = static_cast<decltype (pdata)> (trans_pdata);
    Transaction* trn = pdata->trans;

    return set_tran_time64 (node, trn, xaccTransSetDatePostedSecs);
}

static gboolean
trn_date_entered_handler (xmlNodePtr node, gpointer trans_pdata)
{
    struct trans_pdata* pdata = static_cast<decltype (pdata)> (trans_pdata);
    Transaction* trn = pdata->trans;

    return set_tran_time64 (node, trn, xaccTransSetDateEnteredSecs);
}

static gboolean
trn_description_handler (xmlNodePtr node, gpointer trans_pdata)
{
    struct trans_pdata* pdata = static_cast<decltype (pdata)> (trans_pdata);
    Transaction* trn = pdata->trans;

    return set_tran_string (node, trn, xaccTransSetDescription);
}

static gboolean
trn_slots_handler (xmlNodePtr node, gpointer trans_pdata)
{
    struct trans_pdata* pdata = static_cast<decltype (pdata)> (trans_pdata);
    Transaction* trn = pdata->trans;
    gboolean successful;

    successful = dom_tree_create_instance_slots (node, QOF_INSTANCE (trn));

    g_return_val_if_fail (successful, FALSE);

    return TRUE;
}

static gboolean
trn_splits_handler (xmlNodePtr node, gpointer trans_pdata)
{
    struct trans_pdata* pdata = static_cast<decltype (pdata)> (trans_pdata);
    Transaction* trn = pdata->trans;
    xmlNodePtr mark;

    g_return_val_if_fail (node, FALSE);
    g_return_val_if_fail (node->xmlChildrenNode, FALSE);

    for (mark = node->xmlChildrenNode; mark; mark = mark->next)
    {
        Split* spl;

        if (g_strcmp0 ("text", (char*)mark->name) == 0)
            continue;

        if (g_strcmp0 ("trn:split", (char*)mark->name))
        {
            return FALSE;
        }

        spl = dom_tree_to_split (mark, pdata->book);

        if (spl)
        {
            xaccTransAppendSplit (trn, spl);
        }
        else
        {
            return FALSE;
        }
    }
    return TRUE;
}

struct dom_tree_handler trn_dom_handlers[] =
{
    { "trn:id", trn_id_handler, 1, 0 },
    { "trn:currency", trn_currency_handler, 0, 0},
    { "trn:num", trn_num_handler, 0, 0 },
    { "trn:date-posted", trn_date_posted_handler, 1, 0 },
    { "trn:date-entered", trn_date_entered_handler, 1, 0 },
    { "trn:description", trn_description_handler, 0, 0 },
    { "trn:slots", trn_slots_handler, 0, 0 },
    { "trn:splits", trn_splits_handler, 1, 0 },
    { NULL, NULL, 0, 0 },
};

static gboolean
gnc_transaction_end_handler (gpointer data_for_children,
                             GSList* data_from_children, GSList* sibling_data,
                             gpointer parent_data, gpointer global_data,
                             gpointer* result, const gchar* tag)
{
    Transaction* trn = NULL;
    xmlNodePtr tree = (xmlNodePtr)data_for_children;
    gxpf_data* gdata = (gxpf_data*)global_data;

    if (parent_data)
    {
        return TRUE;
    }

    /* OK.  For some messed up reason this is getting called again with a
       NULL tag.  So we ignore those cases */
    if (!tag)
    {
        return TRUE;
    }

    g_return_val_if_fail (tree, FALSE);

    trn = dom_tree_to_transaction (tree,
                                   static_cast<QofBook*> (gdata->bookdata));
    if (trn != NULL)
    {
        gdata->cb (tag, gdata->parsedata, trn);
    }

    xmlFreeNode (tree);

    return trn != NULL;
}

Transaction*
dom_tree_to_transaction (xmlNodePtr node, QofBook* book)
{
    Transaction* trn;
    gboolean successful;
    struct trans_pdata pdata;

    g_return_val_if_fail (node, NULL);
    g_return_val_if_fail (book, NULL);

    trn = xaccMallocTransaction (book);
    g_return_val_if_fail (trn, NULL);
    xaccTransBeginEdit (trn);

    pdata.trans = trn;
    pdata.book = book;

    successful = dom_tree_generic_parse (node, trn_dom_handlers, &pdata);

    xaccTransCommitEdit (trn);

    if (!successful)
    {
        xmlElemDump (stdout, NULL, node);
        xaccTransBeginEdit (trn);
        xaccTransDestroy (trn);
        xaccTransCommitEdit (trn);
        trn = NULL;
    }

    return trn;
}

sixtp*
gnc_transaction_sixtp_parser_create (void)
{
    return sixtp_dom_parser_new (gnc_transaction_end_handler, NULL, NULL);
}
