/*
 * FILE:
 * gnc-server.c
 *
 * FUNCTION:
 * experimental gnucash server
 * written as a demo, not real code.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gnc-book.h"
#include "gnc-engine.h"
#include "Group.h"
#include "io-gncxml.h"
 
#include "fcgi_stdio.h"


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
      /* 500 Server Error */
      FCGI_SetExitStatus (500);
      goto bookerrexit;
   }

   rc = gnc_book_load (book);
   if (!rc) {
      int err = gnc_book_get_error (book);
      /* 500 Server Error */
      FCGI_SetExitStatus (500);
      goto bookerrexit;
   }

   /* the grp pointer points to our local cache of the data */
   grp = gnc_book_get_group (book);
   
   /* --------------------------------------------------- */
   /* done with initialization, go into event loop */

   while(FCGI_Accept() >= 0) 
   {
      GList *split_list;
      Query *q = NULL;
      char *request_method;
      int read_len=0;

      /* get the METHOD=POST data */
      request_method = getenv ("REQUEST_METHOD");
      if (!strcmp ("POST", request_method)) {
         char * content_length = getenv("CONTENT_LENGTH");
         read_len = atoi (content_length);

         /* read 'read_len' bytes from stdin ... */
      }

#if 0
      /* conver the xml input into a gnucash query structure... */
      q = gncxml_read_query (bufp, read_len);
      xaccQuerySetGroup (q, grp);

      /* hack ... */
      xaccQuerySetMaxSplits (q, 30);
       
      split_list = xaccQueryGetSplits (q);
#endif

      /* generate the xml output */
      /* hack -- we need to use the split list to generate the xml ... */
      gncxml_write_to_buf(grp, &bufp, &sz);

      /* print the HTTP header */
      printf("Content-type: text/html\r\n"
             "Content-Length: %d\r\n"
            "\r\n", sz);


      printf ("%s", bufp);
      free (bufp);
      // xaccFreeQuery (q);
   }

bookerrexit:
   /* close the book */
   gnc_book_destroy (book);

   /* shut down the engine */
   gnc_engine_shutdown ();

   return 0;
}

