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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gnc-book.h"
#include "gnc-commodity.h"
#include "gnc-engine.h"
#include "Group.h"
#include "io-gncxml-v2.h"

 
int
main (int argc, char *argv[]) 
{
   int fake_argc =1;
   char * fake_argv[] = {"hello", 0};
   GNCBook *book;
   AccountGroup *grp;
   char *bufp = NULL;
   int rc, sz = 0;
   
   /* intitialize the engine */
   gnc_engine_init (fake_argc, fake_argv);

   /* dirty little hack to work around commodity borkeness */
   {
      gnc_commodity_table *t = gnc_engine_commodities ();
      gnc_commodity *cm = gnc_commodity_new ("US Dollar", "ISO4217", "USD",  "840", 100);
      gnc_commodity_table_insert (t, cm);
   }

   /* contact the database, which is a flat file for this demo */
   book = gnc_book_new ();

   rc = gnc_book_begin (book, "file:/tmp/demo.gml", FALSE, FALSE);
   if (!rc) {
      GNCBackendError err = gnc_book_get_error (book);
      printf ("HTTP/1.1 500 Server Error\n");
      printf ("\n");
      printf ("err=%d \n", err);
      goto bookerrexit;
   }

   rc = gnc_book_load (book);
   if (!rc) {
      GNCBackendError err = gnc_book_get_error (book);
      printf ("HTTP/1.1 500 Server Error\n");
      printf ("\n");
      printf ("err=%d \n", err);
      goto bookerrexit;
   }

   /* the grp pointer points to our local cache of the data */
   grp = gnc_book_get_group (book);
   
 //  gncxml_write_to_buf(grp, &bufp, &sz);
// write_pricedb_str (NULL, book);


   /* print the HTTP header */
   printf ("HTTP/1.1 200 OK\n");
   printf ("Content-Type: text/gnc-xml\r\n");
   printf ("Content-Length: %d\r\n", sz);
   printf ("\r\n");

   printf ("%s", bufp);
   free (bufp);

bookerrexit:
   /* close the book */
   gnc_book_destroy (book);

   /* shut down the engine */
   gnc_engine_shutdown ();

   return 0;
}
