/***************************************************************************
 *            test-xml-transaction.c
 *
 *  Fri Oct  7 21:26:59 2005
 *  Copyright  2005  Neil Williams
 *  linux@codehelp.co.uk
 ****************************************************************************/
/*
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301, USA.
 */
 
#include "config.h"

#include <glib.h>
#include <glib/gstdio.h>
#include <stdlib.h>
#include <unistd.h>

#include <sys/types.h>
#include <dirent.h>
#include <sys/stat.h>

#include "gnc-xml-helper.h"
#include "gnc-xml.h"
#include "gnc-engine.h"
#include "cashobjects.h"
#include "sixtp-parsers.h"

#include "sixtp-dom-parsers.h"
#include "TransLog.h"
#include "io-gncxml-gen.h"

#include "test-stuff.h"
#include "test-engine-stuff.h"
#include "test-file-stuff.h"

#include "AccountP.h"
#include "Transaction.h"
#include "TransactionP.h"

static QofBook *book;

extern gboolean gnc_transaction_xml_v2_testing;

static xmlNodePtr
find_appropriate_node(xmlNodePtr node, Split *spl)
{
    xmlNodePtr mark;

    for(mark = node->xmlChildrenNode; mark; mark = mark->next)
    {
        gboolean account_guid_good = FALSE;
        gboolean amount_good = FALSE;
        xmlNodePtr mark2;

        for(mark2 = mark->xmlChildrenNode; mark2; mark2 = mark2->next)
        {
            if(safe_strcmp((char*)mark2->name, "split:value") == 0)
            {
                gnc_numeric *num = dom_tree_to_gnc_numeric(mark2);

                if(gnc_numeric_equal(*num, xaccSplitGetValue(spl)))
                {
                    amount_good = TRUE;
                }

                g_free(num);
            }
            else if(safe_strcmp((char*)mark2->name, "split:account") == 0)
            {
                GUID *accid = dom_tree_to_guid(mark2);
                Account *account = xaccSplitGetAccount (spl);

                if(guid_equal(accid, xaccAccountGetGUID(account)))
                {
                    account_guid_good = TRUE;
                }
                g_free(accid);
            }

            if(account_guid_good && amount_good)
            {
                return mark;
            }
        }
    }
    
    return NULL;
}

static char *
equals_node_val_vs_split_internal(xmlNodePtr node, Split* spl)
{
    xmlNodePtr mark;
    
    for(mark = node->children; mark != NULL; mark = mark->next)
    {
        if(safe_strcmp((char*)mark->name, "split:id") == 0)
        {
            GUID *id = dom_tree_to_guid(mark);

            if(!guid_equal(id, xaccSplitGetGUID(spl)))
            {
                g_free(id);
                return "ids differ";
            }
            g_free(id);
        }
        else if(safe_strcmp((char*)mark->name, "split:memo") == 0)
        {
            char *memo = dom_tree_to_text(mark);

            if(safe_strcmp(memo, xaccSplitGetMemo(spl)) != 0)
            {
                g_free(memo);
                return "memos differ";
            }
            g_free(memo);
        }
        else if(safe_strcmp((char*)mark->name, "split:reconciled-state") == 0)
        {
            char *rs = dom_tree_to_text(mark);

            if(rs[0] != xaccSplitGetReconcile(spl))
            {
                g_free(rs);
                return "states differ";
            }
            g_free(rs);
        }
        else if(safe_strcmp((char*)mark->name, "split:value") == 0)
        {
            gnc_numeric *num = dom_tree_to_gnc_numeric(mark);
            gnc_numeric val = xaccSplitGetValue(spl);

            if(!gnc_numeric_equal(*num, val))
            {
                g_free(num);
                return g_strdup_printf ("values differ: %" G_GINT64_FORMAT "/%"
					G_GINT64_FORMAT " v %" G_GINT64_FORMAT
					"/%" G_GINT64_FORMAT,
                                        (*num).num, (*num).denom,
                                        val.num, val.denom);
            }
            g_free(num);
        }
        else if(safe_strcmp((char*)mark->name, "split:quantity") == 0)
        {
            gnc_numeric *num = dom_tree_to_gnc_numeric(mark);
            gnc_numeric val = xaccSplitGetAmount(spl);

            if (!gnc_numeric_equal(*num, val)) {
              return g_strdup_printf( "quantities differ under _equal: %"
				      G_GINT64_FORMAT "/%" G_GINT64_FORMAT
				      " v %" G_GINT64_FORMAT "/%"
				      G_GINT64_FORMAT,
                                      (*num).num, (*num).denom,
                                      val.num, val.denom );
            }
            if(!gnc_numeric_equal(*num, val))
            {
                g_free(num);
                return g_strdup_printf ("quantities differ: %" G_GINT64_FORMAT
					"/%" G_GINT64_FORMAT " v %"
					G_GINT64_FORMAT "/%" G_GINT64_FORMAT,
                                        (*num).num, (*num).denom,
                                        val.num, val.denom);
            }
            g_free(num);
        }
        else if(safe_strcmp((char*)mark->name, "split:account") == 0)
        {
            GUID *id = dom_tree_to_guid(mark);
            Account *account = xaccSplitGetAccount (spl);

            if(!guid_equal(id, xaccAccountGetGUID(account)))
            {
                g_free(id);
                return "accounts differ";
            }
            g_free(id);
        }
    }
    return NULL;
}

static char *
equals_node_val_vs_splits(xmlNodePtr node, const Transaction *trn)
{
    xmlNodePtr spl_node;
    Split *spl_mark;
    char *msg;
    int i;

    g_return_val_if_fail(node, FALSE);
    g_return_val_if_fail(node->xmlChildrenNode, FALSE);
    
    for(i = 0, spl_mark = xaccTransGetSplit((Transaction*)trn, i);
        spl_mark;
        i++, spl_mark = xaccTransGetSplit((Transaction*)trn, i))
    {
        spl_node = find_appropriate_node(node, spl_mark);

        if(!spl_node)
        {
            return "no matching split found";
        }

        msg = equals_node_val_vs_split_internal(spl_node, spl_mark);
        if(msg != NULL)
        {
            return msg;
        }
    }
    
    return NULL;
}

static gchar*
node_and_transaction_equal(xmlNodePtr node, Transaction *trn)
{
    xmlNodePtr mark;
    
    while (safe_strcmp ((char*)node->name, "text") == 0)
      node = node->next;

    if(!check_dom_tree_version(node, "2.0.0"))
    {
        return "version wrong.  Not 2.0.0 or not there";
    }

    if(!node->name || safe_strcmp((char*)node->name, "gnc:transaction"))
    {
        return "Name of toplevel node is bad";
    }

    for(mark = node->xmlChildrenNode; mark; mark = mark->next)
    {
        if(safe_strcmp((char*)mark->name, "text") == 0)
        {
        }
        else if(safe_strcmp((char*)mark->name, "trn:id") == 0)
        {
            if(!equals_node_val_vs_guid(mark, xaccTransGetGUID(trn)))
            {
                return "ids differ";
            }
        }
        else if(safe_strcmp((char*)mark->name, "trn:currency") == 0)
        {
            if(!equals_node_val_vs_commodity(
                   mark, xaccTransGetCurrency(trn), xaccTransGetBook(trn)))
            {
                return g_strdup("currencies differ");
            }
        }
        else if(safe_strcmp((char*)mark->name, "trn:num") == 0)
        {
            if(!equals_node_val_vs_string(mark, xaccTransGetNum(trn)))
            {
                return "nums differ";
            }
        }
        else if(safe_strcmp((char*)mark->name, "trn:date-posted") == 0)
        {
            if(!equals_node_val_vs_date(mark, xaccTransRetDatePostedTS(trn)))
            {
                return "posted dates differ";
            }
        }
        else if(safe_strcmp((char*)mark->name, "trn:date-entered") == 0)
        {
            if(!equals_node_val_vs_date(mark, xaccTransRetDateEnteredTS(trn)))
            {
                return "entered dates differ";
            }
        }
        else if(safe_strcmp((char*)mark->name, "trn:description") == 0)
        {
            if(!equals_node_val_vs_string(mark, xaccTransGetDescription(trn)))
            {
                return "descriptions differ";
            }
        }
        else if(safe_strcmp((char*)mark->name, "trn:slots") == 0)
        {
            if(!equals_node_val_vs_kvp_frame(mark, xaccTransGetSlots(trn)))
            {
                return "slots differ";
            }
        }
        else if(safe_strcmp((char*)mark->name, "trn:splits") == 0)
        {
            char *msg = equals_node_val_vs_splits (mark, trn);
            if(msg != NULL)
            {
                return msg;
            }
        }
        else
        {
            return "unknown node";
        }
    }
    
    return NULL;
}

static void
really_get_rid_of_transaction(Transaction *trn)
{
    xaccTransBeginEdit(trn);
    xaccTransDestroy(trn);
    xaccTransCommitEdit(trn);
}

struct tran_data_struct
{
    Transaction *trn;
    Transaction *new_trn;
    gnc_commodity *com;
    int value;
};
typedef struct tran_data_struct tran_data;

static gboolean
test_add_transaction(const char *tag, gpointer globaldata, gpointer data)
{
    Transaction *trans = data;
    tran_data *gdata = (tran_data*)globaldata;

    xaccTransBeginEdit (trans);
    xaccTransSetCurrency (trans, gdata->com);
    xaccTransCommitEdit (trans);

    if (!do_test_args(xaccTransEqual(gdata->trn, trans, TRUE, TRUE, TRUE, FALSE),
                      "gnc_transaction_sixtp_parser_create",
                      __FILE__, __LINE__,
                      "%d", gdata->value))
      return FALSE;

    gdata->new_trn = trans;

    return TRUE;
}

static void
test_transaction(void)
{
    int i;

    for(i = 0; i < 50; i++)
    {
        Transaction *ran_trn;
        Account *root;
        xmlNodePtr test_node;
        gnc_commodity *com;
        gchar *compare_msg;
        gchar *filename1;
        int fd;

	/* The next line exists for its side effect of creating the
	 * account tree. */
	root = get_random_account_tree(book);
        ran_trn = get_random_transaction(book);
        if(!ran_trn)
        {
            failure_args("transaction_xml", __FILE__, __LINE__,
                         "get_random_transaction returned NULL");
            return;
       }

        {
          /* xaccAccountInsertSplit can reorder the splits. */
          GList * list = g_list_copy(xaccTransGetSplitList (ran_trn));
          GList * node = list;
          for ( ; node; node = node->next)
          {
            Split * s = node->data;
            Account * a = xaccMallocAccount(book);

            xaccAccountBeginEdit (a);
            xaccAccountSetCommoditySCU (a, xaccSplitGetAmount (s).denom);
            xaccAccountInsertSplit (a, s);
            xaccAccountCommitEdit (a);
          }
          g_list_free(list);
        }

        com = xaccTransGetCurrency (ran_trn);

        test_node = gnc_transaction_dom_tree_create(ran_trn);
        if(!test_node)
        {
            failure_args("transaction_xml", __FILE__, __LINE__,
                         "gnc_transaction_dom_tree_create returned NULL");
            really_get_rid_of_transaction(ran_trn);
            continue;
        }

        if((compare_msg = node_and_transaction_equal(test_node, ran_trn)) !=
           NULL)
        {
            failure_args("transaction_xml", __FILE__, __LINE__,
                         "node and transaction were not equal: %s",
                         compare_msg);
            xmlElemDump(stdout, NULL, test_node);
            printf("\n");
            fflush(stdout);
            xmlFreeNode(test_node);
            really_get_rid_of_transaction(ran_trn);
            continue;
        }
        else
        {
            success_args("transaction_xml", __FILE__, __LINE__, "%d", i );
        }
        
        filename1 = g_strdup_printf("test_file_XXXXXX");
        
        fd = g_mkstemp(filename1);
        
        write_dom_node_to_file(test_node, fd);

        close(fd);
        
        {
          GList * node = xaccTransGetSplitList (ran_trn);
          for ( ; node; node = node->next)
          {
            Split * s = node->data;
            Account * a1 = xaccSplitGetAccount(s);
            Account * a2 = xaccMallocAccount(book);

            xaccAccountBeginEdit (a2);
            xaccAccountSetCommoditySCU (a2, xaccAccountGetCommoditySCU (a1));
            xaccAccountSetGUID (a2, xaccAccountGetGUID (a1));
            xaccAccountCommitEdit (a2);
          }
        }

        {
            sixtp *parser;
            tran_data data;

            data.trn = ran_trn;
            data.com = com;
            data.value = i;

            parser = gnc_transaction_sixtp_parser_create();
            
            if(!gnc_xml_parse_file(parser, filename1, test_add_transaction,
                                   (gpointer)&data, book))
            {
                failure_args("gnc_xml_parse_file returned FALSE", 
                             __FILE__, __LINE__, "%d", i);
            }
            else
              really_get_rid_of_transaction (data.new_trn);

            /* no handling of circular data structures.  We'll do that later */
            /* sixtp_destroy(parser); */
        }

        g_unlink(filename1);
        g_free(filename1);
        really_get_rid_of_transaction(ran_trn);
        xmlFreeNode(test_node);
    }
}

static gboolean
test_real_transaction(const char *tag, gpointer global_data, gpointer data)
{
    const char *msg;

    msg = node_and_transaction_equal((xmlNodePtr)global_data,
                                     (Transaction*)data);
    do_test_args(msg == NULL, "test_real_transaction",
                 __FILE__, __LINE__, msg);
    really_get_rid_of_transaction((Transaction*)data);
    return TRUE;
}

int
main (int argc, char ** argv)
{
    qof_init();
    cashobjects_register();
    xaccLogDisable();

    gnc_transaction_xml_v2_testing = TRUE;

    book = qof_book_new ();

    if(argc > 1)
    {
        test_files_in_dir(argc, argv, test_real_transaction,
                          gnc_transaction_sixtp_parser_create(),
                          "gnc:transaction", book);
    }
    else
    {
        test_transaction();
    }

    print_test_results();
    qof_close();
    exit(get_rv());
}
