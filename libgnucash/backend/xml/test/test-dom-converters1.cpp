/***************************************************************************
 *            test-dom-converters1.c
 *
 *  Fri Oct  7 20:51:06 2005
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
extern "C"
{
#include <config.h>

#include <stdlib.h>
#include <string.h>

#include <glib.h>

#include "test-engine-stuff.h"
#include "cashobjects.h"
#include "gnc-engine.h"
#include "gnc-commodity.h"
}

#include "test-file-stuff.h"
#include "gnc-xml-helper.h"
#include "sixtp.h"
#include "sixtp-parsers.h"
#include "sixtp-utils.h"
#include "sixtp-dom-parsers.h"
#include "sixtp-dom-generators.h"
#include "test-stuff.h"

#define GNC_V2_STRING "gnc-v2"
const gchar* gnc_v2_xml_version_string = GNC_V2_STRING;

static void
test_dom_tree_to_commodity_ref (void)
{
    int i;
    for (i = 0; i < 20; i++)
    {
        gnc_commodity* test_com1;
        gchar* test_str1;
        gchar* test_str2;
        gnc_commodity* test_com2;
        xmlNodePtr test_node;
        QofBook* book;

        book = qof_book_new ();

        test_str1 = get_random_string ();
        test_str2 = get_random_string ();

        test_com1 = gnc_commodity_new (book, NULL, test_str1, test_str2, NULL, 0);
        test_node = commodity_ref_to_dom_tree ("test-com", test_com1);

        test_com2 = dom_tree_to_commodity_ref_no_engine (test_node, book);

        do_test (gnc_commodity_equiv (test_com1, test_com2),
                 "dom_tree_to_commodity_ref_no_engine");

        xmlFreeNode (test_node);
        gnc_commodity_destroy (test_com1);
        gnc_commodity_destroy (test_com2);
        g_free (test_str1);
        g_free (test_str2);

        qof_book_destroy (book);
    }
}

static void
test_dom_tree_to_text (void)
{
    int i;

    for (i = 0; i < 20; i++)
    {
        gchar* test_string1;
        gchar* test_string2;
        xmlNodePtr test_node;

        test_node = xmlNewNode (NULL, BAD_CAST "test-node");
        test_string1 = get_random_string ();

        xmlNodeAddContent (test_node, BAD_CAST test_string1);

        test_string2 = dom_tree_to_text (test_node);

        if (!test_string2)
        {
            failure_args ("dom_tree_to_text", __FILE__, __LINE__,
                          "null return from dom_tree_to_text");
            xmlElemDump (stdout, NULL, test_node);
        }
        else if (g_strcmp0 (test_string1, test_string2) == 0)
        {
            success_args ("dom_tree_to_text", __FILE__, __LINE__, "with string %s",
                          test_string1);
        }
        else
        {
            failure_args ("dom_tree_to_text", __FILE__, __LINE__,
                          "with string %s", test_string1);
        }

        xmlFreeNode (test_node);
        g_free (test_string1);
        if (test_string2) g_free (test_string2);
    }
}


static void
test_dom_tree_to_time64 (void)
{
    int i;
    for (i = 0; i < 20; i++)
    {
        xmlNodePtr test_node;
        time64 test_spec1 = get_random_time ();
        test_node = time64_to_dom_tree ("test-spec", test_spec1);
        time64 test_spec2 = dom_tree_to_time64 (test_node);
        if (!dom_tree_valid_time64 (test_spec2, (const xmlChar*)"test-spec"))
        {
            failure_args ("dom_tree_to_time64",
                          __FILE__, __LINE__, "NULL return");
            printf ("Node looks like:\n");
            xmlElemDump (stdout, NULL, test_node);
            printf ("\n");
        }
        else if (test_spec1 == test_spec2)
        {
            success ("dom_tree_to_time64");
        }
        else
        {
            failure ("dom_tree_to_time64");
            printf ("Node looks like:\n");
            xmlElemDump (stdout, NULL, test_node);
            printf ("\n");
            printf ("passed: %" G_GUINT64_FORMAT "  got: %" G_GUINT64_FORMAT ".\n",
                    test_spec1, test_spec2);
        }
        xmlFreeNode (test_node);
    }
}

static const char*
test_gnc_nums_internal (gnc_numeric to_test)
{
    const char* ret = NULL;
    gnc_numeric* to_compare = NULL;
    xmlNodePtr to_gen = NULL;

    to_gen = gnc_numeric_to_dom_tree ("test-num", &to_test);
    if (!to_gen)
    {
        ret =  "no dom tree created";
    }
    else
    {
        to_compare = dom_tree_to_gnc_numeric (to_gen);
        if (!to_compare)
        {
            ret = "no gnc_numeric parsed";
        }
        else
        {
            if (!gnc_numeric_equal (to_test, *to_compare))
            {
                ret = "numerics compared different";
            }
        }
    }

    if (to_compare)
    {
        g_free (to_compare);
    }
    if (to_gen)
    {
        xmlFreeNode (to_gen);
    }

    return ret;
}

static void
test_dom_tree_to_gnc_numeric (void)
{
    int i;

    for (i = 0; i < 20; i++)
    {

        const char* message =
            test_gnc_nums_internal (get_random_gnc_numeric (GNC_DENOM_AUTO));

        do_test_args (message == NULL, "dom_tree_to_gnc_numeric",
                      __FILE__, __LINE__, message);
    }

    {
        const char* message = test_gnc_nums_internal
                              (gnc_numeric_create (18768786810LL, 100000));

        do_test_args (message == NULL, "gnc_num 18768786810/100000",
                      __FILE__, __LINE__, message);
    }
}


static void
test_dom_tree_to_guid (void)
{
    int i;
    for (i = 0; i < 20; i++)
    {
        GncGUID* test_guid1;
        GncGUID* test_guid2;
        xmlNodePtr test_node;

        test_guid1 = get_random_guid ();

        if (! (test_node = guid_to_dom_tree ("test-guid", test_guid1)))
        {
            failure_args ("guid_to_dom_tree", __FILE__, __LINE__,
                          "conversion to dom tree failed");
        }

        test_guid2 = dom_tree_to_guid (test_node);

        do_test (guid_equal (test_guid1, test_guid2),
                 "dom_tree_to_guid");

        xmlFreeNode (test_node);
        g_free (test_guid1);
        guid_free (test_guid2);
    }
}

int
main (int argc, char** argv)
{
    qof_init ();
    cashobjects_register ();
    test_dom_tree_to_guid ();
    fflush (stdout);
    test_dom_tree_to_commodity_ref ();
    fflush (stdout);
    test_dom_tree_to_text ();
    fflush (stdout);
    test_dom_tree_to_time64 ();
    fflush (stdout);
    test_dom_tree_to_gnc_numeric ();
    fflush (stdout);
    print_test_results ();
    qof_close ();
    exit (get_rv ());
}
