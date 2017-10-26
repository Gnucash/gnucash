/********************************************************************\
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
extern "C"
{
#include "config.h"

#include <glib.h>
#include <glib/gstdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "gnc-module.h"
#include "qof.h"
#include "test-engine-stuff.h"

#include "Account.h"
}

#include "gnc-xml-helper.h"
#include "gnc-xml.h"
#include "sixtp-parsers.h"
#include "sixtp-utils.h"
#include "sixtp-dom-parsers.h"
#include "io-gncxml-gen.h"
#include "test-file-stuff.h"
#include "test-stuff.h"

static QofBook* book;

static const char*
node_and_commodity_equal (xmlNodePtr node, const gnc_commodity* com)
{
    xmlNodePtr mark;

    while (g_strcmp0 ((char*)node->name, "text") == 0)
        node = node->next;

    if (!check_dom_tree_version (node, "2.0.0"))
    {
        return "version wrong.  Not 2.0.0 or not there";
    }

    if (!node->name || g_strcmp0 ((char*)node->name, "gnc:commodity"))
    {
        return "Name of toplevel node is bad";
    }

    for (mark = node->xmlChildrenNode; mark; mark = mark->next)
    {
        if (g_strcmp0 ((char*)mark->name, "text") == 0)
        {
        }
        else if (g_strcmp0 ((char*)mark->name, "cmdty:space") == 0)
        {
            /* Currency namespace is now stored as GNC_COMMODITY_NS_CURRENCY
             * but used to be stored as GNC_COMMODITY_NS_ISO,
             * so check both to cater for (very) old testfiles. */
            if (!equals_node_val_vs_string (
                    mark, gnc_commodity_get_namespace (com)) &&
                !((g_strcmp0 (gnc_commodity_get_namespace (com), GNC_COMMODITY_NS_CURRENCY) == 0) &&
                    equals_node_val_vs_string (
                        mark, GNC_COMMODITY_NS_ISO)))
            {
                return "namespaces differ";
            }
        }
        else if (g_strcmp0 ((char*)mark->name, "cmdty:id") == 0)
        {
            if (!equals_node_val_vs_string (
                    mark, gnc_commodity_get_mnemonic (com)))
            {
                return "mnemonic differ";
            }
        }
        else if (g_strcmp0 ((char*)mark->name, "cmdty:name") == 0)
        {
            if (!equals_node_val_vs_string (
                    mark, gnc_commodity_get_fullname (com)))
            {
                return "names differ";
            }
        }
        else if (g_strcmp0 ((char*)mark->name, "cmdty:xcode") == 0)
        {
            if (!equals_node_val_vs_string (
                    mark, gnc_commodity_get_cusip (com)))
            {
                return "exchange codes differ";
            }
        }
        else if (g_strcmp0 ((char*)mark->name, "cmdty:fraction") == 0)
        {
            gchar* txt;
            gint64 type;

            txt = dom_tree_to_text (mark);

            if (!txt)
            {
                return "couldn't get fraction string";
            }

            else if (!string_to_gint64 (txt, &type))
            {
                g_free (txt);
                return "couldn't convert fraction string to int";
            }
            else if (type != gnc_commodity_get_fraction (com))
            {
                g_free (txt);
                return "fractions differ";
            }
            else
            {
                g_free (txt);
            }
        }
        else if (g_strcmp0 ((char*)mark->name, "cmdty:slots") == 0)
        {
            if (!equals_node_val_vs_kvp_frame (mark,
                                               gnc_commodity_get_kvp_frame (com)))
                return "slots differ";
        }
        /* Legitimate tags which we don't yet have tests */
        else if (g_strcmp0 ((char*)mark->name, "cmdty:get_quotes") == 0 ||
                 g_strcmp0 ((char*)mark->name, "cmdty:quote_source") == 0 ||
                 g_strcmp0 ((char*)mark->name, "cmdty:quote_tz") == 0)
        {
            continue;
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
    gnc_commodity* com;
    int value;
};
typedef struct com_data_struct com_data;

static gboolean
test_add_commodity (const char* tag, gpointer globaldata, gpointer data)
{
    com_data* gdata = (com_data*)globaldata;

    do_test_args (gnc_commodity_equiv ((gnc_commodity*)data, gdata->com),
                  "gnc_commodity_sixtp_parser_create",
                  __FILE__, __LINE__, "%d", gdata->value);
    gnc_commodity_destroy ((gnc_commodity*)data);

    return TRUE;

}

static void
test_generation (void)
{
    int i;
    for (i = 0; i < 20; i++)
    {
        gnc_commodity* ran_com;
        xmlNodePtr test_node;
        gchar* filename1;
        int fd;

        ran_com = get_random_commodity (book);

        test_node = gnc_commodity_dom_tree_create (ran_com);
        if (!test_node)
        {
            failure_args ("commodity_xml", __FILE__, __LINE__,
                          "gnc_commodity_dom_tree_create returned NULL");
            gnc_commodity_destroy (ran_com);
            continue;
        }
        auto compare_msg = node_and_commodity_equal (test_node, ran_com);
        if (compare_msg != nullptr)
        {
            failure_args ("commodity_xml", __FILE__, __LINE__,
                          "node and commodity were not equal: %s", compare_msg);
            xmlElemDump (stdout, NULL, test_node);
            xmlFreeNode (test_node);
            gnc_commodity_destroy (ran_com);
            continue;
        }
        else
        {
            success_args ("commodity_xml", __FILE__, __LINE__, "%d", i);
        }

        filename1 = g_strdup_printf ("test_file_XXXXXX");

        fd = g_mkstemp (filename1);

        write_dom_node_to_file (test_node, fd);

        close (fd);

        {
            sixtp* parser;
            com_data data;

            data.com = ran_com;
            data.value = i;

            parser = gnc_commodity_sixtp_parser_create ();

            if (!gnc_xml_parse_file (parser, filename1, test_add_commodity,
                                     (gpointer)&data, book))
            {
                failure_args ("gnc_xml_parse_file returned FALSE",
                              __FILE__, __LINE__, "%d", i);
            }

            /* no handling of circular data structures.  We'll do that later */
            /* sixtp_destroy(parser); */
        }

        g_unlink (filename1);
        g_free (filename1);
        gnc_commodity_destroy (ran_com);
        xmlFreeNode (test_node);
    }
}

static gboolean
test_real_commodity (const char* tag, gpointer globaldata, gpointer data)
{
    const char* msg = node_and_commodity_equal ((xmlNodePtr)globaldata,
                                                (gnc_commodity*)data);
    do_test_args (msg == NULL, "test_real_commodity",
                  __FILE__, __LINE__, msg);
    gnc_commodity_destroy ((gnc_commodity*)data);
    return TRUE;
}

int
main (int argc, char** argv)
{
    g_setenv ("GNC_UNINSTALLED", "1", TRUE);
    gnc_engine_init (argc, argv);

    book = qof_book_new ();

    if (argc > 1)
    {
        test_files_in_dir (argc, argv, test_real_commodity,
                           gnc_commodity_sixtp_parser_create (),
                           "gnc:commodity", book);
    }
    else
    {
        test_generation ();
    }

    print_test_results ();
    exit (get_rv ());
}
