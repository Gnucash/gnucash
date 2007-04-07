#include "config.h"

#include <stdlib.h>

#include "test-stuff.h"
#include "test-engine-stuff.h"
#include "test-file-stuff.h"

#include "qof.h"

#include "sixtp-dom-generators.h"
#include "sixtp-dom-parsers.h"


static void
test_kvp_get_slot(int run,
                  kvp_frame *test_frame1, const kvp_value *test_val1,
                  const gchar *test_key)
{
    const kvp_value *test_val2;
    test_val2 = kvp_frame_get_slot(test_frame1, test_key);
    if(kvp_value_compare(test_val1, test_val2) == 0)
    {
        success_args("kvp_frame_get_slot", __FILE__, __LINE__, "run=%d", run);
    }
    else
    {
        gchar *tmp;
        failure_args("kvp_frame_get_slot", __FILE__, __LINE__, "run=%d", run);
        tmp = kvp_value_to_string(test_val2);
        printf("    Value is %s\n", tmp);
        g_free(tmp);
    }
}

static void
test_kvp_copy_compare(int run,
                      kvp_frame *test_frame1, const kvp_value *test_val1,
                      const gchar *test_key)
{
    kvp_frame *test_frame2;

    test_frame2 = kvp_frame_copy(test_frame1);

    do_test_args(test_frame2 != NULL, "kvp_frame_copy",
                 __FILE__, __LINE__, "run=%d", run);

    if(kvp_frame_compare(test_frame1, test_frame2) == 0)
    {
        success_args("kvp_frame_copy->kvp_frame_compare",
                     __FILE__, __LINE__, "run=%d",run);
    }
    else
    {
        gchar *tmp;
        failure_args("kvp_frame_copy->kvp_frame_compare",
                     __FILE__, __LINE__, "run=%d", run);
        tmp =  kvp_frame_to_string(test_frame1);
        printf("Frame1 is %s\n", tmp);
        g_free(tmp);
        tmp =  kvp_frame_to_string(test_frame2);
        printf("Frame2 is %s\n", tmp);
        g_free(tmp);
    }

    kvp_frame_delete(test_frame2);
}

static void
test_kvp_copy_get_slot(int run,
                       kvp_frame *test_frame1, const kvp_value *test_val1,
                       const gchar *test_key)
{
    kvp_frame *test_frame2;
    const kvp_value *test_val2;

    test_frame2 = kvp_frame_copy(test_frame1);
    test_val2 = kvp_frame_get_slot(test_frame2, test_key);
    if(kvp_value_compare(test_val1, test_val2) == 0)
    {
        success_args("kvp_frame_copy->kvp_frame_get_slot",
                     __FILE__, __LINE__, "run=%d", run);
    }
    else
    {
        gchar *tmp;
        failure_args("kvp_frame_copy->kvp_frame_get_slot",
                     __FILE__, __LINE__, "run=%d", run);
        tmp =  kvp_frame_to_string(test_frame1);
        printf("Frame1 is %s\n", tmp);
        g_free(tmp);
        tmp =  kvp_frame_to_string(test_frame2);
        printf("Frame2 is %s\n", tmp);
        g_free(tmp);
    }
    kvp_frame_delete(test_frame2); 
}

static void
test_kvp_create_delete(void)
{
    kvp_frame *test_frame;
    
    test_frame = kvp_frame_new();

    if(test_frame != NULL)
    {
        kvp_frame_delete(test_frame);
        test_frame = NULL;
        success("kvp_frame_new");
    }
    else
    {
        failure("kvp_frame_new");
    }
}

static void
test_kvp_frames1(void)
{
    int i;
    
    for(i = 0; i < 20; i++)
    {
        kvp_frame *test_frame1;
        gchar *test_key;
        kvp_value *test_val1;

        test_val1 = get_random_kvp_value(i % KVP_TYPE_FRAME);

        test_frame1 = kvp_frame_new();
        test_key = get_random_string();

        kvp_frame_set_slot(test_frame1, test_key, test_val1);

        test_kvp_get_slot(i, test_frame1, test_val1, test_key);
        test_kvp_copy_compare(i, test_frame1, test_val1, test_key);
        test_kvp_copy_get_slot(i, test_frame1, test_val1, test_key);

        kvp_value_delete(test_val1);
        g_free(test_key);
        kvp_frame_delete(test_frame1);
    }
}

static void
test_kvp_printing(void)
{
    int i;
    for(i = 0; i < 20; i++)
    {
        kvp_frame *ran_frame;
        gchar *char_rep;
        
        ran_frame = get_random_kvp_frame();
        char_rep = kvp_frame_to_string(ran_frame);

        /* if we don't crash it's good :) */
        /* puts(char_rep); */

        g_free(char_rep);
        kvp_frame_delete(ran_frame);
    }
}

static void
test_kvp_xml_stuff(void)
{
    int i;
    for(i = 0; i < 20; i++)
    {
        kvp_frame *test_frame1;
        kvp_frame *test_frame2;
        xmlNodePtr test_node;

        test_frame1 = get_random_kvp_frame();

        test_node = kvp_frame_to_dom_tree("test-kvp", test_frame1);

        if(!test_node)
        {
            failure_args("xml stuff",__FILE__, __LINE__, 
                         "kvp_frame_to_dom_tree produced NULL");
        }
        else
        {
            test_frame2 = dom_tree_to_kvp_frame(test_node);

            if(kvp_frame_compare(test_frame1, test_frame2) == 0)
            {
                success("xml stuff");
            }
            else
            {
                gchar *tmp;
                failure("xml stuff");
                tmp = kvp_frame_to_string(test_frame1);
                printf("   with kvp_frame 1:\n%s\n", tmp);
                g_free(tmp);
                printf("   and xml:\n");
                xmlElemDump(stdout, NULL, test_node);
                printf("\n");
                tmp = kvp_frame_to_string(test_frame2);
                printf("   and kvp_frame 2:\n%s\n", tmp);
                g_free(tmp);
            }
            kvp_frame_delete(test_frame2);
            xmlFreeNode(test_node);
        }
        
        kvp_frame_delete(test_frame1);
    }
}

int
main(int argc, char** argv)
{
    qof_init();
    test_kvp_create_delete();
    test_kvp_printing();
    test_kvp_frames1();
    test_kvp_xml_stuff();
    print_test_results();
    exit(get_rv());
}
