/********************************************************************\
 * book.c -- implements book handling for postgres backend          *
 * Copyright (c) 2000, 2001 Linas Vepstas                           *
 *                                                                  *
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
\********************************************************************/


#define _GNU_SOURCE

#include "config.h"

#include <glib.h>
#include <string.h>  

#include <libpq-fe.h>  
 
#include "Backend.h"
#include "BackendP.h"
#include "book.h"
#include "gnc-book-p.h"
#include "gnc-engine-util.h"
#include "guid.h"
#include "kvp-sql.h"
#include "PostgresBackend.h"

#include "gnc-engine.h"
#include "base-autogen.h"

#include "putil.h"

static short module = MOD_BACKEND; 

/* ============================================================= */
/* ============================================================= */
/*                        BOOK STUFF                             */
/*      (UTILITIES FIRST, THEN SETTERS, THEN GETTERS)            */
/* ============================================================= */
/* ============================================================= */

/* ============================================================= */
/* The pgendStoreBook() routine stores everything in the book 
 * to the database.  That is, the engine data is written out to the 
 * database.  
 *
 * If do_mark is set to TRUE, then this routine sets a mark
 * to terminate recursion.  That is,  it will only store the
 * account once; a second call on a marked account will simply 
 * return.  Be sure to clear the mark when done!
 *
 * If the do_check_version flag is set, then this routine
 * will compare the engine and sql db version numbrs, and
 * perform the store only if the engine version is equal 
 * or newer than the sql version.
 *
 */

void
pgendStoreBookNoLock (PGBackend *be, GNCBook *book,
                         gboolean do_check_version)
{
   if (!be || !book) return;

   ENTER ("book=%p", book);

   if (do_check_version)
   {
     if (0 < pgendBookCompareVersion (be, book)) return;
   }
   book->version ++;  /* be sure to update the version !! */

   if ((0 == book->idata) &&
       (FALSE == kvp_frame_is_empty (gnc_book_get_slots(book))))
   {
      book->idata = pgendNewGUIDidx(be);
   }

   pgendPutOneBookOnly (be, book);

   if (book->idata)
   {
      pgendKVPDelete (be, book->idata);
      pgendKVPStore (be, book->idata, book->kvp_data);
   }
   LEAVE(" ");
}

void
pgendStoreBook (PGBackend *be, GNCBook *book)
{
   char *p;
   ENTER ("be=%p, book=%p", be, book);
   if (!be || !book) return;

   /* lock it up so that we store atomically */
   p = "BEGIN;\n"
       "LOCK TABLE gncBook IN EXCLUSIVE MODE;\n";
   SEND_QUERY (be,p, );
   FINISH_QUERY(be->connection);

   pgendStoreBookNoLock (be, book, TRUE);

   p = "COMMIT;\n"
       "NOTIFY gncBook;";
   SEND_QUERY (be,p, );
   FINISH_QUERY(be->connection);
   LEAVE(" ");
}


/* ============================================================= */
/*           BOOK GETTERS (SETTERS ARE ABOVE)                    */
/* ============================================================= */

/* ============================================================= */
/* The pgendGetBook() routine restores the all book data,
 * including the account heirarchy, the price db, commodities, etc.
 */

static gpointer
get_book_cb (PGBackend *be, PGresult *result, int j, gpointer data)
{
   GNCBook *book = (GNCBook *) data;
   GUID guid;

   /* first, lets see if we've already got this one */
   PINFO ("book GUID=%s", DB_GET_VAL("bookGuid",j));
   guid = nullguid;  /* just in case the read fails ... */
   string_to_guid (DB_GET_VAL("bookGuid",j), &guid);

   gnc_book_set_guid (book, guid);

   book->book_open = (DB_GET_VAL("book_open",j))[0];
   book->version = atoi(DB_GET_VAL("version",j));
   book->idata = atoi(DB_GET_VAL("iguid",j));

   return book;
}

void
pgendGetBook (PGBackend *be, GNCBook *book)
{
   char * bufp;

   ENTER ("be=%p", be);
   if (!be) return;

   /* For right now, get only the currently open book 
    * In theory, we should pass a guid into this routine, 
    * and fetch books based on that. 
    */
   bufp = "SELECT * FROM gncBook WHERE book_open='y';";
   SEND_QUERY (be, bufp, );
   pgendGetResults (be, get_book_cb, book);

   if (0 != book->idata) 
   {
      book->kvp_data = pgendKVPFetch (be, book->idata, book->kvp_data);
   }

   LEAVE (" ");
}

/* ======================== END OF FILE ======================== */
