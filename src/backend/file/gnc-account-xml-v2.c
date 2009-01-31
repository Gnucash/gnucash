/********************************************************************\
 * gnc-account-xml-v2.c -- account xml i/o implementation           *
 *                                                                  *
 * Copyright (C) 2001 James LewisMoss <dres@debian.org>             *
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
\********************************************************************/

#include "config.h"

#include <glib.h>
#include <stdlib.h>
#include <string.h>

#include "gnc-xml-helper.h"
#include "sixtp.h"
#include "sixtp-utils.h"
#include "sixtp-parsers.h"
#include "sixtp-utils.h"
#include "sixtp-dom-parsers.h"
#include "sixtp-dom-generators.h"

#include "gnc-xml.h"
#include "io-gncxml-gen.h"
#include "io-gncxml-v2.h"

#include "sixtp-dom-parsers.h"
#include "AccountP.h"
#include "Account.h"

static QofLogModule log_module = GNC_MOD_IO;

const gchar *account_version_string = "2.0.0";

/* ids */
#define gnc_account_string "gnc:account"
#define act_name_string "act:name"
#define act_id_string "act:id"
#define act_type_string "act:type"
#define act_commodity_string "act:commodity"
#define act_commodity_scu_string "act:commodity-scu"
#define act_non_standard_scu_string "act:non-standard-scu"
#define act_code_string "act:code"
#define act_description_string "act:description"
#define act_slots_string "act:slots"
#define act_parent_string "act:parent"
#define act_lots_string "act:lots"
/* The currency and security strings should not appear in newer
 * xml files (anything post-gnucash-1.6) */
#define act_currency_string "act:currency"
#define act_currency_scu_string "act:currency-scu"
#define act_security_string "act:security"
#define act_security_scu_string "act:security-scu"

xmlNodePtr
gnc_account_dom_tree_create(Account *act,
                            gboolean exporting,
                            gboolean allow_incompat)
{
    const char *str;
    kvp_frame *kf;
    xmlNodePtr ret;
    GList *lots, *n;
    Account *parent;
    gnc_commodity *acct_commodity;

    ENTER ("(account=%p)", act);

    ret = xmlNewNode(NULL, BAD_CAST gnc_account_string);
    xmlSetProp(ret, BAD_CAST "version", BAD_CAST account_version_string);

    xmlAddChild(ret, text_to_dom_tree(act_name_string,
                                      xaccAccountGetName(act)));
    
    xmlAddChild(ret, guid_to_dom_tree(act_id_string, xaccAccountGetGUID(act)));

    xmlAddChild(ret, text_to_dom_tree(
                    act_type_string,
                    xaccAccountTypeEnumAsString(xaccAccountGetType(act))));

    acct_commodity = xaccAccountGetCommodity(act);
    if (acct_commodity != NULL)
    {
        xmlAddChild(ret, commodity_ref_to_dom_tree(act_commodity_string,
                                                   acct_commodity));
    
        xmlAddChild(ret, int_to_dom_tree(act_commodity_scu_string,
                                         xaccAccountGetCommoditySCUi(act)));
        
        if (xaccAccountGetNonStdSCU(act))
            xmlNewChild(ret, NULL, BAD_CAST act_non_standard_scu_string, NULL);
    }
    
    str = xaccAccountGetCode(act);
    if (str && strlen(str) > 0)
    {
        xmlAddChild(ret, text_to_dom_tree(act_code_string, str));
    }

    str = xaccAccountGetDescription(act);
    if (str && strlen(str) > 0)
    {
        xmlAddChild(ret, text_to_dom_tree(act_description_string, str));
    }
       
    kf = xaccAccountGetSlots(act);
    if(kf)
    {
        xmlNodePtr kvpnode = kvp_frame_to_dom_tree(act_slots_string, kf);
        if(kvpnode)
        {
            xmlAddChild(ret, kvpnode);
        }
    }

    parent = gnc_account_get_parent(act);
    if (parent)
    {
      if (!gnc_account_is_root(parent) || allow_incompat)
        xmlAddChild(ret, guid_to_dom_tree(act_parent_string,
					  xaccAccountGetGUID(parent)));
    }

    lots = xaccAccountGetLotList (act);
    PINFO ("lot list=%p", lots);
    if (lots && !exporting)
    {
       xmlNodePtr toaddto = xmlNewChild(ret, NULL, BAD_CAST act_lots_string, NULL);

       for (n = lots; n; n=n->next)
       {
          GNCLot * lot = n->data;
          xmlAddChild(toaddto, gnc_lot_dom_tree_create(lot));
       }
    }
    g_list_free(lots);

    LEAVE("");
    return ret;
}

/***********************************************************************/

struct account_pdata
{
  Account *account;
  QofBook *book;
};

static inline gboolean
set_string(xmlNodePtr node, Account* act,
           void (*func)(Account *act, const gchar *txt))
{
    gchar* txt = dom_tree_to_text(node);
    g_return_val_if_fail(txt, FALSE);
    
    func(act, txt);

    g_free(txt);
    
    return TRUE;
}

static gboolean
account_name_handler (xmlNodePtr node, gpointer act_pdata)
{
    struct account_pdata *pdata = act_pdata;

    return set_string(node, pdata->account, xaccAccountSetName);
}

static gboolean
account_id_handler (xmlNodePtr node, gpointer act_pdata)
{
    struct account_pdata *pdata = act_pdata;
    GUID *guid;

    guid = dom_tree_to_guid(node);
    g_return_val_if_fail(guid, FALSE);

    xaccAccountSetGUID(pdata->account, guid);

    g_free(guid);
    
    return TRUE;
}

static gboolean
account_type_handler (xmlNodePtr node, gpointer act_pdata)
{
    struct account_pdata *pdata = act_pdata;
    GNCAccountType type = ACCT_TYPE_INVALID;
    char *string;

    string = (char*) xmlNodeGetContent (node->xmlChildrenNode);
    xaccAccountStringToType(string, &type);
    xmlFree (string);

    xaccAccountSetType(pdata->account, type);

    return TRUE;
}

static gboolean
account_commodity_handler (xmlNodePtr node, gpointer act_pdata)
{
    struct account_pdata *pdata = act_pdata;
    gnc_commodity *ref;

//    ref = dom_tree_to_commodity_ref_no_engine(node, pdata->book);
    ref = dom_tree_to_commodity_ref(node, pdata->book);
    xaccAccountSetCommodity(pdata->account, ref);

    return TRUE;
}

static gboolean
account_commodity_scu_handler (xmlNodePtr node, gpointer act_pdata)
{
    struct account_pdata *pdata = act_pdata;
    gint64 val;

    dom_tree_to_integer(node, &val);
    xaccAccountSetCommoditySCU(pdata->account, val);

    return TRUE;
}

static gboolean
account_non_standard_scu_handler (xmlNodePtr node, gpointer act_pdata)
{
    struct account_pdata *pdata = act_pdata;

    xaccAccountSetNonStdSCU(pdata->account, TRUE);

    return TRUE;
}

/* ============================================================== */
/* The following deprecated routines are here only to service 
 * older XML files. */

static gboolean
deprecated_account_currency_handler (xmlNodePtr node, gpointer act_pdata)
{
    struct account_pdata *pdata = act_pdata;
    gnc_commodity *ref;

    PWARN("Obsolete xml tag will not be preserved."); 
    ref = dom_tree_to_commodity_ref_no_engine(node, pdata->book);
    DxaccAccountSetCurrency(pdata->account, ref);

    return TRUE;
}

static gboolean
deprecated_account_currency_scu_handler (xmlNodePtr node, gpointer act_pdata)
{
    PWARN("Obsolete xml tag will not be preserved."); 
    return TRUE;
}

static gboolean
deprecated_account_security_handler (xmlNodePtr node, gpointer act_pdata)
{
    struct account_pdata *pdata = act_pdata;
    gnc_commodity *ref;

    PWARN("Obsolete xml tag will not be preserved."); 
    if (!xaccAccountGetCommodity(pdata->account)) {
        ref = dom_tree_to_commodity_ref_no_engine(node, pdata->book);
        xaccAccountSetCommodity(pdata->account, ref);
    }

    return TRUE;
}

static gboolean
deprecated_account_security_scu_handler (xmlNodePtr node, gpointer act_pdata)
{
    struct account_pdata *pdata = act_pdata;
    gint64 val;

    PWARN("Obsolete xml tag will not be preserved."); 
    if (!xaccAccountGetCommoditySCU(pdata->account)) {
        dom_tree_to_integer(node, &val);
        xaccAccountSetCommoditySCU(pdata->account, val);
    }

    return TRUE;
}

/* ============================================================== */

static gboolean
account_slots_handler (xmlNodePtr node, gpointer act_pdata)
{
    struct account_pdata *pdata = act_pdata;

    return dom_tree_to_kvp_frame_given
        (node, xaccAccountGetSlots (pdata->account));
}

static gboolean
account_parent_handler (xmlNodePtr node, gpointer act_pdata)
{
    struct account_pdata *pdata = act_pdata;
    Account *parent;
    GUID *gid;

    gid = dom_tree_to_guid(node);
    g_return_val_if_fail(gid, FALSE);

    parent = xaccAccountLookup(gid, pdata->book);
    if (!parent)
    {
      g_free (gid);
      g_return_val_if_fail(parent, FALSE);
    }

    gnc_account_append_child(parent, pdata->account);

    g_free (gid);

    return TRUE;
}

static gboolean
account_code_handler(xmlNodePtr node, gpointer act_pdata)
{
    struct account_pdata *pdata = act_pdata;

    return set_string(node, pdata->account, xaccAccountSetCode);
}

static gboolean
account_description_handler(xmlNodePtr node, gpointer act_pdata)
{
    struct account_pdata *pdata = act_pdata;

    return set_string(node, pdata->account, xaccAccountSetDescription);
}

static gboolean
account_lots_handler(xmlNodePtr node, gpointer act_pdata)
{
    struct account_pdata *pdata = act_pdata;
    xmlNodePtr mark;

    g_return_val_if_fail(node, FALSE);
    g_return_val_if_fail(node->xmlChildrenNode, FALSE);

    for(mark = node->xmlChildrenNode; mark; mark = mark->next)
    {
        GNCLot *lot;
        
        if(safe_strcmp("text", (char*) mark->name) == 0)
          continue;

        lot = dom_tree_to_lot(mark, pdata->book);

        if(lot)
        {
            xaccAccountInsertLot (pdata->account, lot);
        }
        else
        {
            return FALSE;
        }
    }
    return TRUE;
}

static struct dom_tree_handler account_handlers_v2[] = {
    { act_name_string, account_name_handler, 1, 0 },
    { act_id_string, account_id_handler, 1, 0 },
    { act_type_string, account_type_handler, 1, 0 },
    { act_commodity_string, account_commodity_handler, 0, 0 },
    { act_commodity_scu_string, account_commodity_scu_handler, 0, 0 },
    { act_non_standard_scu_string, account_non_standard_scu_handler, 0, 0 },
    { act_code_string, account_code_handler, 0, 0 },
    { act_description_string, account_description_handler, 0, 0},
    { act_slots_string, account_slots_handler, 0, 0 },
    { act_parent_string, account_parent_handler, 0, 0 },
    { act_lots_string, account_lots_handler, 0, 0 },
    
    /* These should not appear in  newer xml files; only in old
     * (circa gnucash-1.6) xml files. We maintain them for backward 
     * compatibility. */
    { act_currency_string, deprecated_account_currency_handler, 0, 0 },
    { act_currency_scu_string, deprecated_account_currency_scu_handler, 0, 0 },
    { act_security_string, deprecated_account_security_handler, 0, 0 },
    { act_security_scu_string, deprecated_account_security_scu_handler, 0, 0 },
    { NULL, 0, 0, 0 }
};

static gboolean
gnc_account_end_handler(gpointer data_for_children,
                        GSList* data_from_children, GSList* sibling_data,
                        gpointer parent_data, gpointer global_data,
                        gpointer *result, const gchar *tag)
{
    int successful;
    Account *acc, *parent, *root;
    xmlNodePtr tree = (xmlNodePtr)data_for_children;
    gxpf_data *gdata = (gxpf_data*)global_data;
    QofBook *book = gdata->bookdata;
    int type;

    successful = TRUE;

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

    acc = dom_tree_to_account(tree, book);
    if(acc != NULL)
    {
        gdata->cb(tag, gdata->parsedata, acc);
        /*
         * Now return the account to the "edit" state.  At the end of reading
         * all the transactions, we will Commit.  This replaces #splits
         * rebalances with #accounts rebalances at the end.  A BIG win!
         */
        xaccAccountBeginEdit(acc);

	/* Backwards compatability.  If there's no parent, see if this
	 * account is of type ROOT.  If not, find or create a ROOT
	 * account and make that the parent. */
        parent = gnc_account_get_parent(acc);
        if (parent == NULL) {
            type = xaccAccountGetType(acc);
            if (type != ACCT_TYPE_ROOT) {
                root = gnc_book_get_root_account(book);
                if (root == NULL) {
		  root = gnc_account_create_root(book);
		}
                gnc_account_append_child(root, acc);
            }
        }
    }

    xmlFreeNode(tree);

    return acc != NULL;
}

Account*
dom_tree_to_account (xmlNodePtr node, QofBook *book)
{
    struct account_pdata act_pdata;
    Account *accToRet;
    gboolean successful;

    accToRet = xaccMallocAccount(book);
    xaccAccountBeginEdit(accToRet);

    act_pdata.account = accToRet;
    act_pdata.book = book;

    successful = dom_tree_generic_parse (node, account_handlers_v2,
                                         &act_pdata);
    if (successful) {
      xaccAccountCommitEdit (accToRet);
    } else {
        PERR ("failed to parse account tree");
        xaccAccountDestroy (accToRet);
        accToRet = NULL;
    }

    return accToRet;
}

sixtp*
gnc_account_sixtp_parser_create(void)
{
    return sixtp_dom_parser_new(gnc_account_end_handler, NULL, NULL);
}

/* ======================  END OF FILE ===================*/
