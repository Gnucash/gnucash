/***************************************************************************
 *            test-xml-pricedb.c
 *
 *  Fri Oct  7 21:24:15 2005
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
#include "config.h"

#include <glib.h>
#include <glib/gstdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "cashobjects.h"
#include "gnc-engine.h"
#include "gnc-pricedb.h"

#include "test-engine-stuff.h"
}

#include "gnc-xml-helper.h"
#include "gnc-xml.h"
#include "sixtp.h"
#include "sixtp-parsers.h"
#include "sixtp-dom-parsers.h"
#include "io-gncxml-v2.h"
#include "test-file-stuff.h"
#include "test-stuff.h"

static QofSession* session;
static int iter;

static gboolean
test_add_pricedb (const char* tag, gpointer globaldata, gpointer data)
{
    sixtp_gdv2* gdata = static_cast<decltype (gdata)> (globaldata);
    GNCPriceDB* db1 = static_cast<decltype (db1)> (data);
    GNCPriceDB* db2 = gnc_pricedb_get_db (gdata->book);

    do_test_args (gnc_pricedb_equal (db1, db2),
                  "gnc_pricedb_sixtp_parser_create",
                  __FILE__, __LINE__, "%d", iter);

    return TRUE;
}

static void
test_db (GNCPriceDB* db)
{
    xmlNodePtr test_node;
    gchar* filename1;
    int fd;
    QofBook* book = qof_instance_get_book (QOF_INSTANCE (db));

    test_node = gnc_pricedb_dom_tree_create (db);

    if (!test_node && db)
    {
        failure_args ("pricedb_xml", __FILE__, __LINE__,
                      "gnc_pricedb_dom_tree_create returned NULL");
        return;
    }

    if (!db)
        return;

    filename1 = g_strdup_printf ("test_file_XXXXXX");

    fd = g_mkstemp (filename1);

    write_dom_node_to_file (test_node, fd);

    close (fd);

    {
        sixtp *parser;
	load_counter lc = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
        sixtp_gdv2 data = {book, lc, NULL, NULL, FALSE};

        parser = sixtp_new ();

        if (!sixtp_add_some_sub_parsers
            (parser, TRUE,
             "gnc:pricedb", gnc_pricedb_sixtp_parser_create (),
             NULL, NULL))
        {
            failure_args ("sixtp_add_some_sub_parsers failed",
                          __FILE__, __LINE__, "%d", iter);
        }
        else if (!gnc_xml_parse_file (parser, filename1, test_add_pricedb,
                                      (gpointer)&data,
                                      qof_session_get_book (session)))
        {
            failure_args ("gnc_xml_parse_file returned FALSE",
                          __FILE__, __LINE__, "%d", iter);
        }
    }

    g_unlink (filename1);
    g_free (filename1);
    xmlFreeNode (test_node);
}

static void
test_generation (void)
{
    for (iter = 0; iter < 20; iter++)
    {
        GNCPriceDB* db;
        g_message ("iter=%d", iter);
        session = qof_session_new ();
        db = get_random_pricedb (qof_session_get_book (session));
        if (!db)
        {
            failure_args ("gnc_random_price_db returned NULL",
                          __FILE__, __LINE__, "%d", iter);
            return;
        }
        if (gnc_pricedb_get_num_prices (db))
            test_db (db);

        gnc_pricedb_destroy (db);
        qof_session_end (session);
    }
}

int
main (int argc, char** argv)
{
    qof_init ();
    cashobjects_register ();
    //qof_log_init_filename("/tmp/gnctest.trace");
    //qof_log_set_default(QOF_LOG_DETAIL);
    //qof_log_set_level(GNC_MOD_PRICE, QOF_LOG_DETAIL);
    session = qof_session_new ();
    test_generation ();
    print_test_results ();
    qof_close ();
    exit (get_rv ());
}
