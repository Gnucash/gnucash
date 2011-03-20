/***************************************************************************
 *            test-commodities.c
 *
 *  Mon Aug 22 09:08:32 2005
 *  Original authors: Derek Atkins, Linas Vepstas.
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

#include "gnc-commodity.h"
#include "qof.h"
#include "test-engine-stuff.h"
#include "test-stuff.h"

static void
test_commodity(void)
{
    gnc_commodity *com;

    {
        QofBook *book;

        book = qof_book_new ();
        com = gnc_commodity_new(book, NULL, NULL, NULL, NULL, 0);

        gnc_commodity_destroy(com);
        qof_book_destroy (book);

        success("commodity new and destroy");
    }

    {
        char *fullname;
        const char *namespace;
        char *mnemonic;
        char *cusip;
        int fraction;
        gnc_commodity *com2;
        QofBook *book;

        book = qof_book_new ();
        fullname = get_random_string();
        namespace = get_random_commodity_namespace();
        mnemonic = get_random_string();
        cusip = get_random_string();
        fraction = get_random_int_in_range(0, 10000);

        com = gnc_commodity_new(book, fullname, namespace, mnemonic,
                                cusip, fraction);

        do_test(
            com != NULL, "commodity with data new and destroy");

        do_test(
            safe_strcmp(fullname, gnc_commodity_get_fullname(com)) == 0,
            "fullnames equal test");

        do_test(
            safe_strcmp(namespace, gnc_commodity_get_namespace(com)) == 0,
            "namespace equal test");

        do_test(
            safe_strcmp(mnemonic, gnc_commodity_get_mnemonic(com)) == 0,
            "mnemonic equal test");

        do_test(
            safe_strcmp(cusip, gnc_commodity_get_cusip(com)) == 0,
            "cusip equal test");

        do_test(
            gnc_commodity_get_fraction(com) == fraction,
            "fraction code equal test");

        fullname = get_random_string();
        gnc_commodity_set_fullname(com, fullname);
        do_test(
            safe_strcmp(fullname, gnc_commodity_get_fullname(com)) == 0,
            "reset fullnames equal test");

        namespace = get_random_commodity_namespace();
        gnc_commodity_set_namespace(com, namespace);
        do_test(
            safe_strcmp(namespace, gnc_commodity_get_namespace(com)) == 0,
            "reset namespace equal test");

        mnemonic = get_random_string();
        gnc_commodity_set_mnemonic(com, mnemonic);
        do_test(
            safe_strcmp(mnemonic, gnc_commodity_get_mnemonic(com)) == 0,
            "reset mnemonic equal test");

        cusip = get_random_string();
        gnc_commodity_set_cusip(com, cusip);
        do_test(
            safe_strcmp(cusip, gnc_commodity_get_cusip(com)) == 0,
            "reset cusip equal test");

        fraction = get_random_int_in_range(0, 10000);
        gnc_commodity_set_fraction(com, fraction);
        do_test(
            gnc_commodity_get_fraction(com) == fraction,
            "reset fraction code equal test");

        com2 = gnc_commodity_new(book, fullname, namespace, mnemonic,
                                 cusip, fraction);
        do_test(
            gnc_commodity_equiv(com, com2), "commodity equiv");

        qof_book_destroy (book);
    }

    {
        int i, j, num_total = 0;
        gnc_commodity_table *tbl;
        gnc_commodity *coms[20];
        QofBook *book;

        book = qof_book_new ();
        tbl = gnc_commodity_table_new ();

        do_test(gnc_commodity_table_get_size(tbl) == 0,
                "test size for 0 table");

        for (i = 0; i < 20; i++)
        {
            coms[i] = get_random_commodity(book);

            if (!gnc_commodity_table_lookup(
                        tbl, gnc_commodity_get_namespace(coms[i]),
                        gnc_commodity_get_mnemonic(coms[i])))
                num_total++;
            do_test(
                gnc_commodity_table_insert(tbl, coms[i]) != NULL,
                "insert test");

            do_test_args(
                (int)gnc_commodity_table_get_size(tbl) == num_total,
                "test next size table", __FILE__, __LINE__,
                "should be %d and is %d", num_total,
                gnc_commodity_table_get_size(tbl));

            for (j = 0; j <= i; j++)
            {
                gnc_commodity *testcom;

                do_test(
                    (testcom = gnc_commodity_table_lookup(
                                   tbl, gnc_commodity_get_namespace(coms[j]),
                                   gnc_commodity_get_mnemonic(coms[j]))) != NULL,
                    "lookup commodity");
                do_test(
                    gnc_commodity_equiv(testcom, coms[j]),
                    "lookup commodity and test equiv");
            }

            do_test(
                gnc_commodity_table_has_namespace(
                    tbl, gnc_commodity_get_namespace(coms[i])),
                "test have namespace");
        }
    }

}

int
main (int argc, char **argv)
{
    qof_init();

    qof_book_register ();
    gnc_commodity_table_register();

    test_commodity();

    print_test_results();

    qof_close();
    return get_rv();
}
