/********************************************************************
 * gnc-transactions-xml-v2.c -- xml routines for transactions       *
 * Copyright (C) 2001 Rob Browning                                  *
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
 *                                                                  *
 *******************************************************************/

#include "config.h"

#include <glib.h>
#include <string.h>

#include "gnc-xml-helper.h"
#include "gnc-engine-util.h"

#include "sixtp.h"
#include "sixtp-utils.h"
#include "sixtp-parsers.h"
#include "sixtp-utils.h"
#include "sixtp-dom-parsers.h"
#include "sixtp-dom-generators.h"

#include "gnc-xml.h"

#include "io-gncxml-v2.h"

#include "sixtp-dom-parsers.h"
#include "Transaction.h"
#include "TransactionP.h"

const gchar *transaction_version_string = "2.0.0";

static void
add_gnc_num(xmlNodePtr node, const gchar *tag, gnc_numeric num)
{
    xmlAddChild(node, gnc_numeric_to_dom_tree(tag, &num));
}

static void
add_timespec(xmlNodePtr node, const gchar *tag, Timespec tms, gboolean always)
{
    if(always || !((tms.tv_sec == 0) && (tms.tv_nsec == 0)))
    {
        xmlAddChild(node, timespec_to_dom_tree(tag, &tms));
    }
}

xmlNodePtr
split_to_dom_tree(const gchar *tag, Split *spl)
{
    xmlNodePtr ret;

    ret = xmlNewNode(NULL, tag);

    xmlAddChild(ret, guid_to_dom_tree("split:id", xaccSplitGetGUID(spl)));
    
    {
        const char *memo = xaccSplitGetMemo(spl);
        
        if(memo && safe_strcmp(memo, "") != 0)
        {
            xmlNewTextChild(ret, NULL, "split:memo", memo);
        }
    }
    
    {
        char tmp[2];

        tmp[0] = xaccSplitGetReconcile(spl);
        tmp[1] = '\0';

        xmlNewTextChild(ret, NULL, "split:reconciled-state", tmp);
    }

    add_timespec(ret, "split:reconcile-date",
                 xaccSplitRetDateReconciledTS(spl), FALSE);

    add_gnc_num(ret, "split:value", xaccSplitGetValue(spl));

    add_gnc_num(ret, "split:quantity", xaccSplitGetShareAmount(spl));
    
    xmlAddChild(ret, guid_to_dom_tree(
                    "split:account",
                    xaccSplitGetAccountGUID(spl)));
    
    {
        xmlNodePtr kvpnode = kvp_frame_to_dom_tree("split:slots",
                                                   xaccSplitGetSlots(spl));
        if(kvpnode)
        {
            xmlAddChild(ret, kvpnode);
        }
    }
    
    return ret;
}

static void
add_trans_splits(xmlNodePtr node, Transaction *trn)
{
    Split *mark;
    int i;
    xmlNodePtr toaddto;

    toaddto = xmlNewChild(node, NULL, "trn:splits", NULL);
    
    for(i = 0, mark = xaccTransGetSplit(trn, i); mark;
        i++, mark = xaccTransGetSplit(trn, i))
    {
        xmlAddChild(toaddto, split_to_dom_tree("trn:split", mark));
    }
}

xmlNodePtr
gnc_transaction_dom_tree_create(Transaction *trn)
{
    xmlNodePtr ret;

    ret = xmlNewNode(NULL, "gnc:transaction");

    xmlSetProp(ret, "version", transaction_version_string);

    xmlAddChild(ret, guid_to_dom_tree("trn:id", xaccTransGetGUID(trn)));

    if(xaccTransGetNum(trn) && (safe_strcmp(xaccTransGetNum(trn), "") != 0))
    {
        xmlNewTextChild(ret, NULL, "trn:num", xaccTransGetNum(trn));
    }

    add_timespec(ret, "trn:date-posted", xaccTransRetDatePostedTS(trn), TRUE);

    add_timespec(ret, "trn:date-entered",
                 xaccTransRetDateEnteredTS(trn), TRUE);

    if(xaccTransGetDescription(trn))
    {
        xmlNewTextChild(ret, NULL, "trn:description",
                        xaccTransGetDescription(trn));
    }
    
    {
        xmlNodePtr kvpnode = kvp_frame_to_dom_tree("trn:slots",
                                                   xaccTransGetSlots(trn));
        if(kvpnode)
        {
            xmlAddChild(ret, kvpnode);
        }
    }

    add_trans_splits(ret, trn);
    
    return ret;
}

/***********************************************************************/

static gboolean
set_spl_string(xmlNodePtr node, Split *spl,
               void (*func)(Split *spl, const char *txt))
{
    gchar *tmp = dom_tree_to_text(node);
    g_return_val_if_fail(tmp, FALSE);
    
    func(spl, tmp);

    g_free(tmp);

    return TRUE;
}

static gboolean
set_spl_gnc_num(xmlNodePtr node, Split* spl,
                void (*func)(Split *spl, gnc_numeric gn))
{
    gnc_numeric *num = dom_tree_to_gnc_numeric(node);
    g_return_val_if_fail(num, FALSE);

    func(spl, *num);

    g_free(num);

    return FALSE;
}
    
static gboolean
spl_id_handler(xmlNodePtr node, gpointer spl)
{
    GUID *tmp = dom_tree_to_guid(node);
    g_return_val_if_fail(tmp, FALSE);

    xaccSplitSetGUID((Split*)spl, tmp);

    g_free(tmp);
    return TRUE;
}

static gboolean
spl_memo_handler(xmlNodePtr node, gpointer spl)
{
    return set_spl_string(node, (Split*)spl, xaccSplitSetMemo);
}

static gboolean
spl_reconciled_state_handler(xmlNodePtr node, gpointer spl)
{
    gchar *tmp = dom_tree_to_text(node);
    g_return_val_if_fail(tmp, FALSE);

    xaccSplitSetReconcile((Split*)spl, tmp[0]);

    g_free(tmp);
    
    return TRUE;
}

static gboolean
spl_reconcile_date_handler(xmlNodePtr node, gpointer spl)
{
    Timespec *ts;

    ts = dom_tree_to_timespec(node);
    g_return_val_if_fail(ts, FALSE);

    xaccSplitSetDateReconciledTS((Split*)spl, ts);

    g_free(ts);
    
    return TRUE;
}

static gboolean
spl_value_handler(xmlNodePtr node, gpointer spl)
{
    return set_spl_gnc_num(node, (Split*)spl, xaccSplitSetValue);
}

static gboolean
spl_quantity_handler(xmlNodePtr node, gpointer spl)
{
    return set_spl_gnc_num(node, (Split*)spl, xaccSplitSetShareAmount);
}

static gboolean
spl_account_handler(xmlNodePtr node, gpointer spl)
{
    GUID *id = dom_tree_to_guid(node);

    if(!id) return FALSE;
    
    xaccSplitSetAccountGUID((Split*)spl, *id);
    g_free(id);
    return TRUE;
}

static gboolean
spl_slots_handler(xmlNodePtr node, gpointer spl)
{
    kvp_frame *frm = dom_tree_to_kvp_frame(node);
    g_return_val_if_fail(frm, FALSE);

    xaccSplitSetSlots_nc((Split*)spl, frm);
    
    return TRUE;
}

struct dom_tree_handler spl_dom_handlers[] =
{
    { "split:id", spl_id_handler, 1, 0 },
    { "split:memo", spl_memo_handler, 0, 0 },
    { "split:reconciled-state", spl_reconciled_state_handler, 1, 0 },
    { "split:reconcile-date", spl_reconcile_date_handler, 0, 0 },
    { "split:value", spl_value_handler, 1, 0 },
    { "split:quantity", spl_quantity_handler, 1, 0 },
    { "split:account", spl_account_handler, 1, 0 },
    { "split:slots", spl_slots_handler, 0, 0 },
    { NULL, NULL, 0, 0 },
};

Split*
dom_tree_to_split(xmlNodePtr node)
{
    Split *ret;

    ret = xaccMallocSplit();
    g_return_val_if_fail(ret, NULL);

    /* this isn't going to work in a testing setup */
    if(dom_tree_generic_parse(node, spl_dom_handlers, ret))
    {
        return ret;
    }
    else
    {
        xaccSplitDestroy(ret);
        return NULL;
    }
}


static gboolean
trn_splits_handler(xmlNodePtr node, gpointer trn)
{
    xmlNodePtr mark;

    g_return_val_if_fail(node, FALSE);
    g_return_val_if_fail(node->xmlChildrenNode, FALSE);

    for(mark = node->xmlChildrenNode; mark; mark = mark->next)
    {
        Split *spl;
        
        if(safe_strcmp("trn:split", mark->name))
        {
            return FALSE;
        }
        
        spl = dom_tree_to_split(mark);

        if(spl)
        {
            xaccTransAppendSplit((Transaction*)trn, spl);
        }
        else
        {
            return FALSE;
        }
    }
    return TRUE;
}

/***********************************************************************/

static gboolean
set_tran_string(xmlNodePtr node, Transaction *trn,
           void (*func)(Transaction *trn, const char *txt))
{
    gchar *tmp;

    tmp = dom_tree_to_text(node);

    g_return_val_if_fail(tmp, FALSE);
    
    func(trn, tmp);

    g_free(tmp);

    return TRUE;
}

static gboolean
set_tran_date(xmlNodePtr node, Transaction *trn,
         void (*func)(Transaction *trn, const Timespec *tm))
{
    Timespec *tm;

    tm = dom_tree_to_timespec(node);

    g_return_val_if_fail(tm, FALSE);
    
    func(trn, tm);

    g_free(tm);

    return TRUE;
}

static gboolean
trn_id_handler(xmlNodePtr node, gpointer trn)
{
    GUID *tmp = dom_tree_to_guid(node);
    g_return_val_if_fail(tmp, FALSE);

    xaccTransSetGUID((Transaction*)trn, tmp);

    g_free(tmp);
    
    return TRUE;
}

static gboolean
trn_num_handler(xmlNodePtr node, gpointer trn)
{
    return set_tran_string(node, (Transaction*)trn, xaccTransSetNum);
}

static gboolean
trn_date_posted_handler(xmlNodePtr node, gpointer trn)
{
    return set_tran_date(node, (Transaction*)trn, xaccTransSetDatePostedTS);
}

static gboolean
trn_date_entered_handler(xmlNodePtr node, gpointer trn)
{
    return set_tran_date(node, (Transaction*)trn, xaccTransSetDateEnteredTS);
}

static gboolean
trn_description_handler(xmlNodePtr node, gpointer trn)
{
    return set_tran_string(node, (Transaction*)trn, xaccTransSetDescription);
}

static gboolean
trn_slots_handler(xmlNodePtr node, gpointer trn)
{
    kvp_frame *frm;

    frm = dom_tree_to_kvp_frame(node);

    xaccTransSetSlots_nc((Transaction*)trn, frm);
    
    return TRUE;
}


struct dom_tree_handler trn_dom_handlers[] =
{
    { "trn:id", trn_id_handler, 1, 0 },
    { "trn:num", trn_num_handler, 0, 0 },
    { "trn:date-posted", trn_date_posted_handler, 1, 0 },
    { "trn:date-entered", trn_date_entered_handler, 1, 0 },
    { "trn:description", trn_description_handler, 0, 0 },
    { "trn:slots", trn_slots_handler, 0, 0 },
    { "trn:splits", trn_splits_handler, 1, 0 },
    { NULL, NULL, 0, 0 },
};

static gboolean
gnc_transaction_end_handler(gpointer data_for_children,
                            GSList* data_from_children, GSList* sibling_data,
                            gpointer parent_data, gpointer global_data,
                            gpointer *result, const gchar *tag)
{
    Transaction *trn = NULL;
    gboolean successful = FALSE;
    xmlNodePtr achild;
    xmlNodePtr tree = (xmlNodePtr)data_for_children;
    sixtp_gdv2 *globaldata = (sixtp_gdv2*)global_data;

    if(parent_data)
    {
        return TRUE;
    }

    /* OK.  For some messed up reason this is getting called again with a
       NULL tag.  So we ignore those cases */
    if(!tag)
    {
        return TRUE;
    }
    
    g_return_val_if_fail(tree, FALSE);
    
    trn = xaccMallocTransaction();
    g_return_val_if_fail(trn, FALSE);
    xaccTransBeginEdit(trn);

    successful = dom_tree_generic_parse(tree, trn_dom_handlers, trn);
    
    xaccTransCommitEdit(trn);

    if(successful)
    {
        globaldata->addTransactionFunc(globaldata, trn);
        successful = TRUE;
    }
    else
    {
        xmlElemDump(stdout, NULL, tree);
        xaccTransBeginEdit(trn);
        xaccTransDestroy(trn);
    }

    xmlFreeNode(tree);
    
    return successful;
}


sixtp*
gnc_transaction_sixtp_parser_create(void)
{
    return sixtp_dom_parser_new(gnc_transaction_end_handler, NULL, NULL);
}
