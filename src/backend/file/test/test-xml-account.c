#include "config.h"

#include <glib.h>
#include <guile/gh.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "gnc-module.h"
#include "gnc-xml-helper.h"
#include "gnc-xml.h"
#include "gnc-engine-util.h"
#include "gnc-engine.h"

#include "sixtp-parsers.h"
#include "sixtp-dom-parsers.h"

#include "test-stuff.h"
#include "test-engine-stuff.h"
#include "test-file-stuff.h"

#include "Account.h"
#include "GNCIdP.h"
#include "Group.h"
#include "Scrub.h"
#include "gnc-book.h"

static GNCBook *book;

static gchar*
node_and_account_equal(xmlNodePtr node, Account *act)
{
    xmlNodePtr mark;

    while (safe_strcmp (node->name, "text") == 0)
      node = node->next;

    if(!check_dom_tree_version(node, "2.0.0"))
    {
        return g_strdup("version wrong.  Not 2.0.0 or not there");
    }

    if(!node->name || safe_strcmp(node->name, "gnc:account"))
    {
        return g_strdup("Name of toplevel node is bad");
    }

    for(mark = node->xmlChildrenNode; mark; mark = mark->next)
    {
        if(safe_strcmp(mark->name, "text") == 0)
        {
        }
        else if(safe_strcmp(mark->name, "act:name") == 0)
        {
            if(!equals_node_val_vs_string(mark, xaccAccountGetName(act)))
            {
                return g_strdup("names differ");
            }
        }
        else if(safe_strcmp(mark->name, "act:id") == 0)
        {
            if(!equals_node_val_vs_guid(mark, xaccAccountGetGUID(act)))
            {
                return g_strdup("ids differ");
            }
        }
        else if(safe_strcmp(mark->name, "act:type") == 0)
        {
            gchar *txt;
            int type;
            
            txt = dom_tree_to_text(mark);

            if(!txt)
            {
                return g_strdup("couldn't get type string");
            }
            else if(!xaccAccountStringToType(txt, &type))
            {
                g_free(txt);
                return g_strdup("couldn't convert type string to int");
            }
            else if(type != xaccAccountGetType(act))
            {
                g_free(txt);
                return g_strdup("types differ");
            }
            else
            {
                g_free(txt);
            }
        }
        else if(safe_strcmp(mark->name, "act:commodity") == 0)
        {
            if(!equals_node_val_vs_commodity(
                   mark, xaccAccountGetCommodity(act)))
            {
                return g_strdup("commodities differ");
            }
        }
        else if(safe_strcmp(mark->name, "act:currency") == 0)
        {
            if(!equals_node_val_vs_commodity(
                   mark, DxaccAccountGetCurrency(act, book)))
            {
                return g_strdup("currencies differ");
            }
        }
        else if(safe_strcmp(mark->name, "act:code") == 0)
        {
            if(!equals_node_val_vs_string(mark, xaccAccountGetCode(act)))
            {
                return g_strdup("codes differ");
            }
        }
        else if(safe_strcmp(mark->name, "act:description") == 0)
        {
            if(!equals_node_val_vs_string(
                   mark, xaccAccountGetDescription(act)))
            {
                return g_strdup("descriptions differ");
            }
        }
        else if(safe_strcmp(mark->name, "act:security") == 0)
        {
            if(!equals_node_val_vs_commodity(
                   mark, DxaccAccountGetSecurity(act, book)))
            {
                return g_strdup("securities differ");
            }
        }
        else if(safe_strcmp(mark->name, "act:slots") == 0)
        {
            xaccAccountDeleteOldData (act);

            if(!equals_node_val_vs_kvp_frame(mark, xaccAccountGetSlots(act)))
            {
                return g_strdup("slots differ");
            }
        }
        else if(safe_strcmp(mark->name, "act:parent") == 0)
        {
            if(!equals_node_val_vs_guid(
                   mark, xaccAccountGetGUID(xaccGroupGetParentAccount(
                                                xaccAccountGetParent(act)))))
            {
                return g_strdup("parent ids differ");
            }
        }
        else if(safe_strcmp(mark->name, "act:commodity-scu") == 0)
        {
            if(!equals_node_val_vs_int(mark, xaccAccountGetCommoditySCU(act)))
            {
                return g_strdup("commodity scus differ");
            }
        }
        else if(safe_strcmp(mark->name, "act:currency-scu") == 0)
        {
            if(!equals_node_val_vs_int(mark, DxaccAccountGetCurrencySCU(act)))
            {
                return g_strdup("currency scus differ");
            }
        }
        else if(safe_strcmp(mark->name, "act:security-scu") == 0)
        {
            if(!equals_node_val_vs_int(mark, xaccAccountGetCommoditySCU(act)))
            {
                return g_strdup("commodity scus differ");
            }
        }
        else
        {
            return g_strdup_printf("unknown node in dom tree: %s", mark->name);
        }
    }
    
    return NULL;
}

static void
delete_random_account(Account *act)
{
    gnc_commodity_destroy(xaccAccountGetCommodity(act));

    xaccAccountBeginEdit(act);
    xaccAccountDestroy(act);
}

struct act_data_struct
{
    Account *act;
    int value;
};
typedef struct act_data_struct act_data;

static gboolean
test_add_account(const char *tag, gpointer globaldata, gpointer data)
{
    Account *account = data;
    act_data *gdata = (act_data*)globaldata;
    gnc_commodity * com;
    gnc_commodity * new_com;
    gnc_commodity_table *t;

    com = xaccAccountGetCommodity (account);

    t = gnc_book_get_commodity_table (book);

    new_com = gnc_commodity_table_lookup (t,
                                          gnc_commodity_get_namespace (com),
                                          gnc_commodity_get_mnemonic (com));

    if (new_com)
      xaccAccountSetCommodity (account, new_com);

    do_test_args(xaccAccountEqual((Account*)data, (Account*)(gdata->act),
                                  TRUE),
                        "gnc_account_sixtp_parser_create", 
		 __FILE__, __LINE__, "%d", gdata->value );

    return TRUE;
}

static void
test_account(int i, Account *test_act)
{
    xmlNodePtr test_node;
    gchar *filename1;
    gchar *compare_msg;
    int fd;

    test_node = gnc_account_dom_tree_create(test_act);

    if(!test_node)
    {
        failure_args("account_xml", __FILE__, __LINE__, 
                     "gnc_account_dom_tree_create returned NULL");
        return;
    }

    if((compare_msg = node_and_account_equal(test_node, test_act)) != NULL)
    {
        failure_args("account_xml", __FILE__, __LINE__, 
                     "node and account were not equal: %s", compare_msg);
        xmlElemDump(stdout, NULL, test_node);
        fprintf(stdout, "\n");
        xmlFreeNode(test_node);
        g_free(compare_msg);
        return;
    }
    else
    {
        success("account_xml");
    }
        
    filename1 = g_strdup_printf("test_file_XXXXXX");
        
    fd = mkstemp(filename1);
        
    write_dom_node_to_file(test_node, fd);

    close(fd);

    {
        sixtp *parser;
        act_data data;

        data.act = test_act;
        data.value = i;

        parser = gnc_account_sixtp_parser_create();
            
        if(!gnc_xml_parse_file(parser, filename1, test_add_account,
                               &data, book))
        {
            failure_args("gnc_xml_parse_file returned FALSE",
                         __FILE__, __LINE__, "%d", i);
        }

        /* no handling of circular data structures.  We'll do that later */
        /* sixtp_destroy(parser); */
    }
        

    unlink(filename1);
    g_free(filename1);
    xmlFreeNode(test_node);
}

static void
test_generation(void)
{
    int i;

    for(i = 0; i < 20; i++)
    {
        Account *ran_act;

        ran_act = get_random_account(book);

        test_account(i, ran_act);

        delete_random_account(ran_act);
    }
    
    {
        /* empty some things. */
        Account *act;

        act = get_random_account(book);

        xaccAccountSetCode(act, "");
        xaccAccountSetDescription(act, "");
        xaccAccountSetCommodity(act, NULL);

        test_account(-1, act);

        delete_random_account(act);
    }
    
/*     { */
/*         Account *act1; */
/*         Account *act2; */

/*         act1 = get_random_account(); */
/*         act2 = get_random_account(); */

/*         xaccAccountInsertSubAccount(act1, act2); */

/*         test_account(-1, act2); */
/*         test_account(-1, act1); */

/*         delete_random_account(act2); */
/*         delete_random_account(act1); */
/*     } */
    
}

static gboolean
test_real_account(const char *tag, gpointer global_data, gpointer data)
{
    char *msg;
    Account *act = (Account*)data;

    if(!xaccAccountGetParent(act))
    {
        xaccGroupInsertAccount(gnc_book_get_group(book), act);
    }

    msg = node_and_account_equal((xmlNodePtr)global_data, act);
    do_test_args(msg == NULL, "test_real_account",
                 __FILE__, __LINE__, msg);

    g_free(msg);
    return TRUE;
}

static void
guile_main(int argc, char **argv)
{
    gnc_module_system_init();
    gnc_module_load("gnucash/engine", 0);

    book = gnc_book_new ();

    if(argc > 1)
    {
        test_files_in_dir(argc, argv, test_real_account, 
                          gnc_account_sixtp_parser_create(),
                          "gnc:account", book);
    }
    else
    {
        test_generation();
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
