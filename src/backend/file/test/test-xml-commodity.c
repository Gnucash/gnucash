#include "config.h"

#include <glib.h>
#include <glib/gstdio.h>
#include <libguile.h>
#include <stdlib.h>
#include <unistd.h>

#include "gnc-module.h"
#include "gnc-xml-helper.h"
#include "gnc-xml.h"
#include "qof.h"

#include "sixtp-parsers.h"
#include "sixtp-utils.h"

#include "sixtp-dom-parsers.h"

#include "io-gncxml-gen.h"

#include "test-stuff.h"
#include "test-engine-stuff.h"
#include "test-file-stuff.h"

#include "Account.h"

static QofBook *book;

static gchar*
node_and_commodity_equal(xmlNodePtr node, const gnc_commodity *com)
{
    xmlNodePtr mark;

    while (safe_strcmp ((char*)node->name, "text") == 0)
      node = node->next;

    if(!check_dom_tree_version(node, "2.0.0"))
    {
        return "version wrong.  Not 2.0.0 or not there";
    }

    if(!node->name || safe_strcmp((char*)node->name, "gnc:commodity"))
    {
        return "Name of toplevel node is bad";
    }

    for(mark = node->xmlChildrenNode; mark; mark = mark->next)
    {
        if(safe_strcmp((char*)mark->name, "text") == 0)
        {
        }
        else if(safe_strcmp((char*)mark->name, "cmdty:space") == 0)
        {
            if(!equals_node_val_vs_string(
                   mark, gnc_commodity_get_namespace(com)))
            {
                return "namespaces differ";
            }
        }
        else if(safe_strcmp((char*)mark->name, "cmdty:id") == 0)
        {
            if(!equals_node_val_vs_string(
                   mark, gnc_commodity_get_mnemonic(com)))
            {
                return "mnemonic differ";
            }
        }
        else if(safe_strcmp((char*)mark->name, "cmdty:name") == 0)
        {
            if(!equals_node_val_vs_string(
                   mark, gnc_commodity_get_fullname(com)))
            {
                return "names differ";
            }
        }
        else if(safe_strcmp((char*)mark->name, "cmdty:xcode") == 0)
        {
            if(!equals_node_val_vs_string(
                   mark, gnc_commodity_get_cusip(com)))
            {
                return "exchange codes differ";
            }
        }
        else if(safe_strcmp((char*)mark->name, "cmdty:fraction") == 0)
        {
            gchar *txt;
            gint64 type;
            
            txt = dom_tree_to_text(mark);

            if(!txt)
            {
                return "couldn't get fraction string";
            }

            else if(!string_to_gint64(txt, &type))
            {
                g_free(txt);
                return "couldn't convert fraction string to int";
            }
            else if(type != gnc_commodity_get_fraction(com))
            {
                g_free(txt);
                return "fractions differ";
            }
            else
            {
                g_free(txt);
            }
        }
        else
        {
            return "unknown node";
        }
    }
    
    return NULL;
}

struct com_data_struct
{
    gnc_commodity *com;
    int value;
};
typedef struct com_data_struct com_data;

static gboolean
test_add_commodity(const char *tag, gpointer globaldata, gpointer data)
{
    com_data *gdata = (com_data*)globaldata;
    
    do_test_args(gnc_commodity_equiv((gnc_commodity*)data, gdata->com),
            "gnc_commodity_sixtp_parser_create",
            __FILE__, __LINE__, "%d", gdata->value );
    gnc_commodity_destroy((gnc_commodity*)data);
    
    return TRUE;
    
}

static void
test_generation(void)
{
    int i;
    for(i = 0; i < 20; i++)
    {
        gnc_commodity *ran_com;
        xmlNodePtr test_node;
        gchar *filename1;
        int fd;
        gchar *compare_msg;
        
        ran_com = get_random_commodity(book);

        test_node = gnc_commodity_dom_tree_create(ran_com);
        if(!test_node)
        {
            failure_args("commodity_xml", __FILE__, __LINE__,
                         "gnc_commodity_dom_tree_create returned NULL");
            gnc_commodity_destroy(ran_com);
            continue;
        }

        if((compare_msg = node_and_commodity_equal(test_node, ran_com)) !=
           NULL)
        {
            failure_args("commodity_xml", __FILE__, __LINE__,
                         "node and commodity were not equal: %s", compare_msg);
            xmlElemDump(stdout, NULL, test_node);
            xmlFreeNode(test_node);
            gnc_commodity_destroy(ran_com);
            continue;
        }
        else 
        {
            success_args("commodity_xml", __FILE__, __LINE__, "%d", i);
        }
        
        filename1 = g_strdup_printf("test_file_XXXXXX");
        
        fd = g_mkstemp(filename1);
        
        write_dom_node_to_file(test_node, fd);

        close(fd);
        
        {
            sixtp *parser;
            com_data data;

            data.com = ran_com;
            data.value = i;
            
            parser = gnc_commodity_sixtp_parser_create();
            
            if(!gnc_xml_parse_file(parser, filename1, test_add_commodity,
                                   (gpointer)&data, book))
            {
                failure_args("gnc_xml_parse_file returned FALSE",  
                             __FILE__, __LINE__, "%d", i);
            }

            /* no handling of circular data structures.  We'll do that later */
            /* sixtp_destroy(parser); */
        }

        g_unlink(filename1);
        g_free(filename1);
        gnc_commodity_destroy(ran_com);
        xmlFreeNode(test_node);
    }
}

static gboolean
test_real_commodity(const char *tag, gpointer globaldata, gpointer data)
{
    const char *msg = node_and_commodity_equal((xmlNodePtr)globaldata,
                                               (gnc_commodity*)data);
    do_test_args(msg == NULL, "test_real_commodity",
                 __FILE__, __LINE__, msg);
    gnc_commodity_destroy((gnc_commodity*)data);
    return TRUE;
}

int
main(int argc, char **argv)
{
    gnc_engine_init(argc, argv);

    book = qof_book_new ();

    if(argc > 1)
    {
        test_files_in_dir(argc, argv, test_real_commodity,
                          gnc_commodity_sixtp_parser_create(),
                          "gnc:commodity", book);
    }
    else
    {
        test_generation();
    }

    print_test_results();
    exit(get_rv());
}
