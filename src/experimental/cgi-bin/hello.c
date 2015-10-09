/*
 * FILE:
 * hello.c
 *
 * FUNCTION:
 * The first in a sequence of demos for cgi-bin programming
 * This demo shows how to intialize the gnc_engine, and how
 * to dump the entire contents of a gnucash file to stdout.
 *
 */
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


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gnc-commodity.h"
#include "gnc-engine.h"
#include "io-gncxml-v2.h"


int
main (int argc, char *argv[])
{
    int fake_argc = 1;
    char * fake_argv[] = {"hello", 0};
    QofBook *book;
    int rc;

    /* intitialize the engine */
    gnc_engine_init (fake_argc, fake_argv);

    /* dirty little hack to work around commodity borkeness */
    {
        gnc_commodity_table *t = gnc_engine_commodities ();
        gnc_commodity *cm = gnc_commodity_new ("US Dollar", "ISO4217", "USD",  "840", 100);
        gnc_commodity_table_insert (t, cm);
    }

    /* contact the database, which is a flat file for this demo */
    book = qof_book_new ();

    rc = gnc_book_begin (book, "file:/tmp/demo.gml", FALSE, FALSE);
    if (!rc)
    {
        GNCBackendError err = gnc_book_get_error (book);
        printf ("HTTP/1.1 500 Server Error\n");
        printf ("\n");
        printf ("err=%d \n", err);
        goto bookerrexit;
    }

    rc = gnc_book_load (book);
    if (!rc)
    {
        GNCBackendError err = gnc_book_get_error (book);
        printf ("HTTP/1.1 500 Server Error\n");
        printf ("\n");
        printf ("err=%d \n", err);
        goto bookerrexit;
    }

    /* print the HTTP header */
    printf ("HTTP/1.1 200 OK\n");
    printf ("Content-Type: text/gnc-xml\r\n");
    // the current write interfaces don't give us a length :-(
    // printf ("Content-Length: %d\r\n", sz);
    printf ("\r\n");

    gnc_book_write_to_xml_filehandle_v2 (book, stdout);

bookerrexit:
    /* close the book */
    qof_book_destroy (book);

    /* shut down the engine */
    gnc_engine_shutdown ();

    return 0;
}
