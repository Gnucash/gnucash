#include "config.h"

#include <glib.h>
#include <guile/gh.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include <sys/types.h>
#include <dirent.h>
#include <sys/stat.h>

#include "gnc-module.h"
#include "gnc-xml-helper.h"
#include "gnc-xml.h"
#include "gnc-engine-util.h"
#include "gnc-engine.h"

#include "sixtp-parsers.h"

#include "sixtp-dom-parsers.h"
#include "TransLog.h"
#include "io-gncxml-gen.h"

#include "test-stuff.h"
#include "test-engine-stuff.h"
#include "test-file-stuff.h"

#include "AccountP.h"
#include "Transaction.h"
#include "GNCIdP.h"

static GNCBook *book;

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
            if(safe_strcmp(mark2->name, "split:value") == 0)
            {
                gnc_numeric *num = dom_tree_to_gnc_numeric(mark2);

                if(gnc_numeric_equal(*num, xaccSplitGetValue(spl)))
                {
                    amount_good = TRUE;
                }

                g_free(num);
            }
            else if(safe_strcmp(mark2->name, "split:account") == 0)
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

static gboolean
equals_node_val_vs_split_internal(xmlNodePtr node, Split* spl)
{
    xmlNodePtr mark;
    
    for(mark = node->childs; mark != NULL; mark = mark->next)
    {
        if(safe_strcmp(mark->name, "split:id") == 0)
        {
            GUID *id = dom_tree_to_guid(mark);

            if(!guid_equal(id, xaccSplitGetGUID(spl)))
            {
                printf("ids differ\n");
                g_free(id);
                return FALSE;
            }
            g_free(id);
        }
        else if(safe_strcmp(mark->name, "split:memo") == 0)
        {
            char *memo = dom_tree_to_text(mark);

            if(safe_strcmp(memo, xaccSplitGetMemo(spl)) != 0)
            {
                printf("memos differ\n");
                g_free(memo);
                return FALSE;
            }
            g_free(memo);
        }
        else if(safe_strcmp(mark->name, "split:reconciled-state") == 0)
        {
            char *rs = dom_tree_to_text(mark);

            if(rs[0] != xaccSplitGetReconcile(spl))
            {
                printf("states differ\n");
                g_free(rs);
                return FALSE;
            }
            g_free(rs);
        }
        else if(safe_strcmp(mark->name, "split:value") == 0)
        {
            gnc_numeric *num = dom_tree_to_gnc_numeric(mark);

            if(!gnc_numeric_equal(*num, xaccSplitGetValue(spl)))
            {
                printf("values differ\n");
                g_free(num);
                return FALSE;
            }
            g_free(num);
        }
        else if(safe_strcmp(mark->name, "split:quantity") == 0)
        {
            gnc_numeric *num = dom_tree_to_gnc_numeric(mark);

            if(!gnc_numeric_equal(*num, xaccSplitGetAmount(spl)))
            {
                printf("quantities differ\n");
                g_free(num);
                return FALSE;
            }
            g_free(num);
        }
        else if(safe_strcmp(mark->name, "split:account") == 0)
        {
            GUID *id = dom_tree_to_guid(mark);
            Account *account = xaccSplitGetAccount (spl);

            if(!guid_equal(id, xaccAccountGetGUID(account)))
            {
                printf("accounts differ\n");
                g_free(id);
                return FALSE;
            }
            g_free(id);
        }
    }
    return TRUE;
}

static gboolean
equals_node_val_vs_splits(xmlNodePtr node, const Transaction *trn)
{
    xmlNodePtr spl_node;
    Split *spl_mark;
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
            return FALSE;
        }

        if(!equals_node_val_vs_split_internal(spl_node, spl_mark))
        {
            return FALSE;
        }
    }
    
    return TRUE;
}

static GNCBook *
xaccTransGetBook (Transaction *trn)
{
   Split *s = xaccTransGetSplit (trn, 0);
   Account *acc = xaccSplitGetAccount(s);
   return xaccAccountGetBook (acc);
}

static gchar*
node_and_transaction_equal(xmlNodePtr node, Transaction *trn)
{
    xmlNodePtr mark;
    
    while (safe_strcmp (node->name, "text") == 0)
      node = node->next;

    if(!check_dom_tree_version(node, "2.0.0"))
    {
        return "version wrong.  Not 2.0.0 or not there";
    }

    if(!node->name || safe_strcmp(node->name, "gnc:transaction"))
    {
        return "Name of toplevel node is bad";
    }

    for(mark = node->xmlChildrenNode; mark; mark = mark->next)
    {
        if(safe_strcmp(mark->name, "text") == 0)
        {
        }
        else if(safe_strcmp(mark->name, "trn:id") == 0)
        {
            if(!equals_node_val_vs_guid(mark, xaccTransGetGUID(trn)))
            {
                return "ids differ";
            }
        }
        else if(safe_strcmp(mark->name, "trn:currency") == 0)
        {
            if(!equals_node_val_vs_commodity(
                   mark, xaccTransGetCurrency(trn), xaccTransGetBook(trn)))
            {
                return g_strdup("currencies differ");
            }
        }
        else if(safe_strcmp(mark->name, "trn:num") == 0)
        {
            if(!equals_node_val_vs_string(mark, xaccTransGetNum(trn)))
            {
                return "nums differ";
            }
        }
        else if(safe_strcmp(mark->name, "trn:date-posted") == 0)
        {
            if(!equals_node_val_vs_date(mark, xaccTransRetDatePostedTS(trn)))
            {
                return "posted dates differ";
            }
        }
        else if(safe_strcmp(mark->name, "trn:date-entered") == 0)
        {
            if(!equals_node_val_vs_date(mark, xaccTransRetDateEnteredTS(trn)))
            {
                return "entered dates differ";
            }
        }
        else if(safe_strcmp(mark->name, "trn:description") == 0)
        {
            if(!equals_node_val_vs_string(mark, xaccTransGetDescription(trn)))
            {
                return "descriptions differ";
            }
        }
        else if(safe_strcmp(mark->name, "trn:slots") == 0)
        {
            if(!equals_node_val_vs_kvp_frame(mark, xaccTransGetSlots(trn)))
            {
                return "slots differ";
            }
        }
        else if(safe_strcmp(mark->name, "trn:splits") == 0)
        {
            if(!equals_node_val_vs_splits(mark, trn))
            {
                return "splits differ";
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

    do_test_args(xaccTransEqual(gdata->trn, trans, TRUE, TRUE),
                 "gnc_transaction_sixtp_parser_create",
                 __FILE__, __LINE__,
                 "%d", gdata->value);

    gdata->new_trn = trans;

    return TRUE;
}

static void
test_transaction(void)
{
    int i;

    for(i = 0; i < 20; i++)
    {
        Transaction *ran_trn;
        xmlNodePtr test_node;
        gnc_commodity *com;
        gchar *compare_msg;
        gchar *filename1;
        int fd;

        ran_trn = get_random_transaction(book);

        {
          GList * node = xaccTransGetSplitList (ran_trn);
          for ( ; node; node = node->next)
          {
            Split * s = node->data;
            Account * a = xaccMallocAccount(book);

            xaccAccountBeginEdit (a);
            xaccAccountSetCommoditySCU (a, xaccSplitGetAmount (s).denom);
            xaccAccountInsertSplit (a, s);
            xaccAccountCommitEdit (a);
          }
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
        
        fd = mkstemp(filename1);
        
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

        unlink(filename1);
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

static void
guile_main(int argc, char **argv)
{
    gnc_module_system_init();
    gnc_module_load("gnucash/engine", 0);

    xaccLogDisable();

    gnc_transaction_xml_v2_testing = TRUE;

    book = gnc_book_new ();

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
    exit(get_rv());
}

int
main(int argc, char ** argv)
{
  gh_enter (argc, argv, guile_main);
  return 0;
}
