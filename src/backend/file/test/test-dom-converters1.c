#include "config.h"

#include <stdlib.h>
#include <string.h>

#include <glib.h>

#include "gnc-test-stuff.h"

#include "gnc-xml-helper.h"
#include "gnc-engine-util.h"
#include "sixtp.h"
#include "sixtp-parsers.h"
#include "sixtp-utils.h"
#include "sixtp-dom-parsers.h"
#include "sixtp-dom-generators.h"

#include "gnc-commodity.h"
#include "guid.h"

static void
test_dom_tree_to_commodity_ref(void)
{
    int i;
    for(i = 0; i < 20; i++)
    {
        gnc_commodity *test_com1;
        gchar *test_str1;
        gchar *test_str2;
        gnc_commodity *test_com2;
        xmlNodePtr test_node;

        test_str1 = get_random_string();
        test_str2 = get_random_string();
        
        test_com1 = gnc_commodity_new(NULL, test_str1, test_str2, NULL, 0);
        test_node = commodity_ref_to_dom_tree("test-com", test_com1);

        test_com2 = dom_tree_to_commodity_ref_no_engine(test_node);

        do_test(gnc_commodity_equiv(test_com1, test_com2),
                            "dom_tree_to_commodity_ref_no_engine");

        xmlFreeNode(test_node);
        gnc_commodity_destroy(test_com1);
        gnc_commodity_destroy(test_com2);
        g_free(test_str1);
        g_free(test_str2);
    }
}

static void
test_dom_tree_to_text(void)
{
    int i;

    for(i = 0; i < 20; i++)
    {
        gchar *test_string1;
        gchar *test_string2;
        xmlNodePtr test_node;

        test_node = xmlNewNode(NULL, "test-node");
        test_string1 = get_random_string();

        xmlNodeAddContent(test_node, test_string1);

        test_string2 = dom_tree_to_text(test_node);

        if(!test_string2)
        {
            failure_args("dom_tree_to_text", __FILE__, __LINE__, 
                         "null return from dom_tree_to_text");
            xmlElemDump(stdout, NULL, test_node);
        }
        else if(safe_strcmp(test_string1, test_string2) == 0)
        {
            success_args("dom_tree_to_text", __FILE__, __LINE__, "with string %s",
                         test_string1);
        }
        else
        {
	    failure_args("dom_tree_to_text", __FILE__, __LINE__,
                         "with string %s", test_string1);
        }

        xmlFreeNode(test_node);
        g_free(test_string1);
        if(test_string2) g_free(test_string2);
    }
}


static void
test_dom_tree_to_timespec(void)
{
    int i;
    for(i = 0; i < 20; i++)
    {
        Timespec *test_spec1;
        Timespec *test_spec2;
        xmlNodePtr test_node;

        test_spec1 = get_random_timespec();

        test_node = timespec_to_dom_tree("test-spec", test_spec1);

        test_spec2 = dom_tree_to_timespec(test_node);

        if(test_spec2 == NULL)
        {
            failure_args("dom_tree_to_timespec",
                         __FILE__, __LINE__, "NULL return");
            printf("Node looks like:\n");
            xmlElemDump(stdout, NULL, test_node);
            printf("\n");
        }
        
        else if(timespec_cmp(test_spec1, test_spec2) == 0)
        {
            success("dom_tree_to_timespec");
        }
        else
        {
            failure("dom_tree_to_timespec");
            printf("Node looks like:\n");
            xmlElemDump(stdout, NULL, test_node);
            printf("\n");
            printf("Secs are %lld vs %lld :: ",
                   test_spec1->tv_sec,
                   test_spec2->tv_sec);
            printf("NSecs are %ld vs %ld\n",
                   test_spec1->tv_nsec,
                   test_spec2->tv_nsec);
        }

        g_free(test_spec1);
        g_free(test_spec2);
        xmlFreeNode(test_node);
    }
}

static gchar *
test_gnc_nums_internal(gnc_numeric to_test)
{
    gchar *ret = NULL;
    gnc_numeric *to_compare = NULL;
    xmlNodePtr to_gen = NULL;

    to_gen = gnc_numeric_to_dom_tree("test-num", &to_test);
    if(!to_gen)
    {
        ret =  "no dom tree created";
    }
    else
    {
        to_compare = dom_tree_to_gnc_numeric(to_gen);
        if(!to_compare)
        {
            ret = "no gnc_numeric parsed";
        }
        else
        {
            if(!gnc_numeric_equal(to_test, *to_compare))
            {
                ret = "numerics compared different";
            }
        }
    }

    if(to_compare)
    {
        g_free(to_compare);
    }
    if(to_gen)
    {
        xmlFreeNode(to_gen);
    }
    
    return ret;
}

static void
test_dom_tree_to_gnc_numeric(void)
{
    int i;

    for(i = 0; i < 20; i++)
    {
        gchar *message = NULL;

        message = test_gnc_nums_internal(get_random_gnc_numeric());

        do_test_args(message == NULL, "dom_tree_to_gnc_numeric",
                     __FILE__, __LINE__, message);
    }

    {
        gchar *message = NULL;

        message = test_gnc_nums_internal
          (gnc_numeric_create(18768786810, 100000));

        do_test_args(message == NULL, "gnc_num 18768786810/100000",
                     __FILE__, __LINE__, message);
    }
}


static void
test_dom_tree_to_guid(void)
{
    int i;
    for(i = 0;i < 20; i++)
    {
        GUID *test_guid1;
        GUID *test_guid2;
        xmlNodePtr test_node;

        test_guid1 = get_random_guid();

        if (!(test_node = guid_to_dom_tree("test-guid", test_guid1)))
        {
	     failure_args("guid_to_dom_tree", __FILE__, __LINE__, 
                          "conversion to dom tree failed");
        }

        test_guid2 = dom_tree_to_guid(test_node);

        do_test(guid_equal(test_guid1, test_guid2),
                            "dom_tree_to_guid" );

        xmlFreeNode(test_node);
        g_free(test_guid1);
        g_free(test_guid2);
    }
}

int
main(int argc, char **argv)
{
    test_dom_tree_to_guid();
    fflush(stdout);
    test_dom_tree_to_commodity_ref();
    fflush(stdout);
    test_dom_tree_to_text();
    fflush(stdout);
    test_dom_tree_to_timespec();
    fflush(stdout);
    test_dom_tree_to_gnc_numeric();
    fflush(stdout);
    print_test_results();
    exit(get_rv());
}
