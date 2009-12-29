/********************************************************************\
 * book.c -- implements book handling for postgres backend          *
 * Copyright (c) 2000, 2001, 2002 Linas Vepstas <linas@linas.org>   *
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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
\********************************************************************/

#include "config.h"

#include <glib.h>
#include <stdlib.h>
#include <string.h>

#include <libpq-fe.h>

#include "qof.h"
#include "book.h"
#include "gnc-pricedb.h"
#include "kvp-sql.h"
#include "PostgresBackend.h"

#include "gnc-engine.h"
#include "base-autogen.h"

#include "putil.h"

static QofLogModule log_module = GNC_MOD_BACKEND;

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
pgendStoreBookNoLock (PGBackend *be, QofBook *book,
                      gboolean do_check_version)
{
    guint32 idata;
    if (!be || !book) return;

    ENTER ("book=%p", book);

    if (do_check_version)
    {
        if (0 < pgendBookCompareVersion (be, book)) return;
    }
    qof_book_set_version(book, (qof_book_get_version(book) + 1)); /* be sure to update the version !! */

    if ((0 == qof_instance_get_idata(book)) &&
            (FALSE == kvp_frame_is_empty (qof_book_get_slots(book))))
    {
        qof_instance_set_idata(book, pgendNewGUIDidx(be));
    }

    pgendPutOneBookOnly (be, book);
    idata = qof_instance_get_idata(book);
    if ( idata > 0)
    {
        pgendKVPDelete (be, idata);
        pgendKVPStore (be, idata, qof_instance_get_slots((QofInstance*)book));
    }
    LEAVE(" ");
}

void
pgendStoreBook (PGBackend *be, QofBook *book)
{
    char *p;
    ENTER ("be=%p, book=%p", be, book);
    if (!be || !book) return;

    /* lock it up so that we store atomically */
    p = "BEGIN;\n"
        "LOCK TABLE gncBook IN EXCLUSIVE MODE;\n";
    SEND_QUERY (be, p, );
    FINISH_QUERY(be->connection);

    pgendStoreBookNoLock (be, book, TRUE);

    p = "COMMIT;\n"
        "NOTIFY gncBook;";
    SEND_QUERY (be, p, );
    FINISH_QUERY(be->connection);
    LEAVE(" ");
}


/* ============================================================= */
/*           BOOK GETTERS (SETTERS ARE ABOVE)                    */
/* ============================================================= */

/* ============================================================= */
/* The pgendBookRestore() routine restores the all book data,
 * including the account heirarchy, the price db, commodities, etc.
 */

static gpointer
get_one_book_cb (PGBackend *be, PGresult *result, int j, gpointer data)
{
    QofBook *book = (QofBook *) data;
    GUID guid;

    /* first, lets see if we've already got this one */
    PINFO ("book GUID=%s", DB_GET_VAL("bookGuid", j));
    guid = nullguid;  /* just in case the read fails ... */
    string_to_guid (DB_GET_VAL("bookGuid", j), &guid);

    qof_instance_set_guid (QOF_INSTANCE(book), &guid);

    if ((DB_GET_VAL("book_open", j))[0] == 'n')
    {
        qof_book_mark_closed(book);
    }
    qof_book_set_version(book, atoi(DB_GET_VAL("version", j)));
    qof_instance_set_idata(book, atoi(DB_GET_VAL("iguid", j)));

    return book;
}

static void pg_kvp_helper (const char* key, KvpValue *value, gpointer data)
{
    QofBook *book = (QofBook*)data;
    kvp_frame_set_slot_nc(qof_instance_get_slots((QofInstance*)book),
                          key, value);
}

void
pgendBookRestore (PGBackend *be, QofBook *book)
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
    pgendGetResults (be, get_one_book_cb, book);

    if (0 != qof_instance_get_idata(book))
    {
        KvpFrame *pg_frame;

        pg_frame = pgendKVPFetch (be, qof_instance_get_idata(book),
                                  qof_instance_get_slots((QofInstance*)book));
        kvp_frame_for_each_slot(pg_frame, pg_kvp_helper, book);
    }

    LEAVE (" ");
}

/* ============================================================= */
/* The pgendGetAllBooks() routine creates an empty book
 * for each book in the database.
 */

static gpointer
get_book_cb (PGBackend *be, PGresult *result, int j, gpointer data)
{
    QofBookList *blist = (QofBookList *) data;
    QofBookList *node;
    QofBook *book;
    GUID guid;

    PINFO ("book GUID=%s", DB_GET_VAL("bookGUID", j));
    guid = nullguid;  /* just in case the read fails ... */
    string_to_guid (DB_GET_VAL("bookGUID", j), &guid);

    /* first, lets see if we've already got this one */
    book = NULL;
    for (node = blist; node; node = node->next)
    {
        book = node->data;
        if (guid_equal (qof_entity_get_guid(QOF_INSTANCE(book)), &guid)) break;
        book = NULL;
    }

    if (!book)
    {
        book = qof_book_new();
        qof_instance_set_guid (QOF_INSTANCE(book), &guid);
    }

    if ((DB_GET_VAL("book_open", j))[0] == 'n')
    {
        qof_book_mark_closed(book);
    }
//   book->book_open = (DB_GET_VAL("book_open",j))[0];
    qof_book_set_version(book, atoi(DB_GET_VAL("version", j)));
    qof_instance_set_idata(book, atoi(DB_GET_VAL("iguid", j)));

    return blist;
}

QofBookList *
pgendGetAllBooks (PGBackend *be, QofBookList *blist)
{
    QofBookList *node;
    char * bufp;

    ENTER ("be=%p", be);
    if (!be) return NULL;

    /* Get them ALL */
    bufp = "SELECT * FROM gncBook;";
    SEND_QUERY (be, bufp, NULL);
    blist = pgendGetResults (be, get_book_cb, blist);

    /* get the KVP data for each book too */
    for (node = blist; node; node = node->next)
    {
        QofBook *book = node->data;
        if (0 != qof_instance_get_idata(book))
        {
            KvpFrame *pg_frame;

            pg_frame = pgendKVPFetch (be, qof_instance_get_idata(book),
                                      qof_instance_get_slots((QofInstance*)book));
            kvp_frame_for_each_slot(pg_frame, pg_kvp_helper, book);
        }
        /*      if (0 != qof_instance_get_idata(book))
              {
                 book->inst.kvp_data = pgendKVPFetch(be, qof_instance_get_idata(book),
                                                     book->inst.kvp_data);
              }*/
    }

    LEAVE (" ");
    return blist;
}

/* ============================================================= */

void
pgend_book_transfer_begin(QofBackend *bend, QofBook *newbook)
{
    PGBackend *be = (PGBackend *) bend;

    ENTER (" ");

    /* first, store the new book */
    pgendStoreBook (be, newbook);

    LEAVE (" ");
}

void
pgend_book_transfer_commit(QofBackend *bend, QofBook *newbook)
{
    /* PGBackend *be = (PGBackend *) bend; */
    ENTER (" ");

    LEAVE (" ");
}

/* ======================== END OF FILE ======================== */
