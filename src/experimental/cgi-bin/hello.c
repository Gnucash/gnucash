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
#include "gnc-engine.h"
#include "Group.h"
#include "io-gncxml.h"
 
int
main (int argc, char *argv[]) 
{
   int fake_argc =1;
   char * fake_argv[] = {"hello", 0};
   GNCBook *book;
   AccountGroup *grp;
   char *bufp;
   int rc, sz;
   

   /* intitialize the engine */
   gnc_engine_init (fake_argc, fake_argv);

   /* contact the database, which is a flat file for this demo */
   book = gnc_book_new ();

   rc = gnc_book_begin (book, "file:/tmp/demo.xac", FALSE);
   if (!rc) {
      int err = gnc_book_get_error (book);
      printf ("HTTP/1.1 500 Server Error\n");
      printf ("\n");
      printf ("%d %s\n", err, strerror (err));
      goto bookerrexit;
   }

   rc = gnc_book_load (book);
   if (!rc) {
      int err = gnc_book_get_error (book);
      printf ("HTTP/1.1 500 Server Error\n");
      printf ("\n");
      printf ("%d %s\n", err, strerror (err));
      goto bookerrexit;
   }

   /* the grp pointer points to our local cache of the data */
   grp = gnc_book_get_group (book);
   
   gncxml_write_to_buf(grp, &bufp, &sz);

   /* print the HTTP header */
   printf ("HTTP/1.1 200 OK\n");
   printf ("Content-Type: text/xml\n");
   printf ("Content-Length: %d\n", sz);
   printf ("\n");

   printf ("%s", bufp);
   free (bufp);

bookerrexit:
   /* close the book */
   gnc_book_destroy (book);

   /* shut down the engine */
   gnc_engine_shutdown ();

   return 0;
}
