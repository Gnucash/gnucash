/********************************************************************\
 * price.c -- implements price handling for the postgres backend    *
 * Copyright (c) 2001, 2002 Linas Vepstas <linas@linas.org>         *
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
#include "gnc-commodity.h"
#include "gnc-engine.h"
#include "gnc-pricedb.h"
#include "gnc-pricedb-p.h"

#include "PostgresBackend.h"
#include "base-autogen.h"
#include "escape.h"
#include "price.h"
#include "putil.h"

static QofLogModule log_module = GNC_MOD_BACKEND;

/* ============================================================= */
/* ============================================================= */
/*                COMMODITIES  STUFF                             */
/* ============================================================= */
/* ============================================================= */

/* This routine restores all commodities in the database.
 */

static gpointer
get_commodities_cb (PGBackend *be, PGresult *result, int j, gpointer data)
{
    GList *node;

    /* stick the commodity in every book ... */
    for (node = be->blist; node; node = node->next)
    {
        gnc_commodity *com;
        QofBook *book = node->data;
        gnc_commodity_table *comtab = gnc_book_get_commodity_table (book);

        if (!comtab) continue;

        /* first, lets see if we've already got this one */
        com = gnc_commodity_table_lookup(comtab,
                                         DB_GET_VAL("namespace", j),
                                         DB_GET_VAL("mnemonic", j));

        if (com) continue;

        /* no we don't ... restore it */
        com = gnc_commodity_new (book,
                                 DB_GET_VAL("fullname", j),
                                 DB_GET_VAL("namespace", j),
                                 DB_GET_VAL("mnemonic", j),
                                 DB_GET_VAL("code", j),
                                 atoi(DB_GET_VAL("fraction", j)));

        gnc_commodity_table_insert (comtab, com);
    }
    return NULL;
}

void
pgendGetAllCommodities (PGBackend *be)
{
    char * p;
    if (!be) return;

    ENTER ("be=%p, conn=%p", be, be->connection);

    /* Get them ALL */
    p = "SELECT * FROM gncCommodity;";
    SEND_QUERY (be, p, );
    pgendGetResults (be, get_commodities_cb, NULL);

    LEAVE (" ");
}

void
pgendGetCommodity (PGBackend *be, const char * unique_name)
{
    sqlEscape *escape;
    char *p;

    if (!be || !unique_name) return;

    ENTER ("be=%p, conn=%p", be, be->connection);

    escape = sqlEscape_new ();

    /* Get them ALL */
    p = be->buff;
    p = stpcpy (p, "SELECT * FROM gncCommodity WHERE gncCommodity.commodity='");
    p = stpcpy (p, sqlEscapeString (escape, unique_name));
    p = stpcpy (p, "';");

    SEND_QUERY (be, be->buff, );
    pgendGetResults (be, get_commodities_cb, NULL);

    sqlEscape_destroy (escape);

    LEAVE (" ");
}

/* ============================================================= */
/* ============================================================= */
/*                PRICE  STUFF                                   */
/* ============================================================= */
/* ============================================================= */
/* store just one price */

static void
pgendStorePriceNoLock (PGBackend *be, GNCPrice *pr,
                       gboolean do_check_version)
{
    gnc_commodity *modity;

    if (do_check_version)
    {
        if (0 < pgendPriceCompareVersion (be, pr)) return;
    }
    /* be sure to update the version !! */
    qof_instance_increment_version(pr, be->version_check);

    /* make sure that we've stored the commodity
     * and currency before we store the price.
     */
    modity = gnc_price_get_commodity (pr);
    pgendPutOneCommodityOnly (be, modity);

    modity = gnc_price_get_currency (pr);
    pgendPutOneCommodityOnly (be, modity);

    pgendPutOnePriceOnly (be, pr);
}

/* ============================================================= */
/* store entire price database */

static gboolean
foreach_price_cb (GNCPrice *pr, gpointer bend)
{
    PGBackend *be = (PGBackend *) bend;
    gnc_commodity *modity;
    gint16 mark;

    /* make sure that we've stored the commodity
     * and currency before we store the price.
     * We use marks to avoid redundant stores.
     */
    modity = gnc_price_get_commodity (pr);
    mark = gnc_commodity_get_mark (modity);
    if (!mark)
    {
        pgendPutOneCommodityOnly (be, modity);
        gnc_commodity_set_mark (modity, 1);
    }

    modity = gnc_price_get_currency (pr);
    mark = gnc_commodity_get_mark (modity);
    if (!mark)
    {
        pgendPutOneCommodityOnly (be, modity);
        gnc_commodity_set_mark (modity, 1);
    }

    pgendPutOnePriceOnly (be, pr);

    return TRUE;
}

static gboolean
commodity_mark_cb (gnc_commodity *cm, gpointer user_data)
{
    gint32 v = ((gint32) GPOINTER_TO_INT(user_data)) & 0xffff;
    gnc_commodity_set_mark (cm, (gint16) v);
    return TRUE;
}


void
pgendStorePriceDBNoLock (PGBackend *be, QofBook *book)
{
    GNCPriceDB *prdb;
    gnc_commodity_table *comtab;

    prdb = gnc_book_get_pricedb(book);
    comtab = gnc_book_get_commodity_table (book);

    /* Clear the marks on commodities -- we use this to mark
     * the thing as 'already stored', avoiding redundant stores */
    gnc_commodity_table_foreach_commodity (comtab, commodity_mark_cb, 0);

    gnc_pricedb_foreach_price (prdb, foreach_price_cb,
                               (gpointer) be, FALSE);

    gnc_commodity_table_foreach_commodity (comtab, commodity_mark_cb, 0);
}

void
pgendStorePriceDB (PGBackend *be, QofBook *book)
{
    char *p;
    ENTER ("be=%p, book=%p", be, book);
    if (!be || !book) return;

    /* Lock it up so that we store atomically */
    p = "BEGIN;\n"
        "LOCK TABLE gncPrice IN EXCLUSIVE MODE;\n";
    SEND_QUERY (be, p, );
    FINISH_QUERY(be->connection);

    pgendStorePriceDBNoLock (be, book);

    p = "COMMIT;\n"
        "NOTIFY gncPrice;";
    SEND_QUERY (be, p, );
    FINISH_QUERY(be->connection);
    LEAVE(" ");
}

/* ============================================================= */
/* The pgendGetAllPrices() routine sucks *all* of the
 *    prices out of the database.  This is a potential
 *    CPU and memory-burner; its use is not suggested for anything
 *    but single-user mode.
 */

static gpointer
get_price_cb (PGBackend *be, PGresult *result, int j, gpointer data)
{
    QofBook *book = data;
    GNCPriceDB *prdb;
    GNCPrice *pr;
    gint32 sql_vers, local_vers;
    Timespec ts;
    gint64 num, denom;
    gnc_numeric value;
    GUID guid = nullguid;
    int not_found = 0;

    gnc_commodity * modity;

    FIND_BOOK (book);

    prdb = gnc_book_get_pricedb(book);

    /* First, lets see if we've already got this one */
    string_to_guid (DB_GET_VAL ("priceGuid", j), &guid);
    pr = gnc_price_lookup (&guid, book);

    if (!pr)
    {
        pr = gnc_price_create(book);
        gnc_price_begin_edit (pr);
        gnc_price_set_guid (pr, &guid);
        not_found = 1;
    }
    else
    {
        gnc_price_ref (pr);
        gnc_price_begin_edit (pr);
        not_found = 0;
    }

    /* compare versions. Hack alert -- Not sure how to handle failures */
    sql_vers = atoi (DB_GET_VAL("version", j));
    local_vers = qof_instance_get_version(pr);
    if (sql_vers < local_vers)
    {
        PERR ("local price version is higher than db !!! local=%d sql=%d",
              local_vers, sql_vers);
        gnc_price_commit_edit (pr);
        gnc_price_unref (pr);
        return data;
    }
    qof_instance_set_version (pr, sql_vers);

    modity = gnc_string_to_commodity (DB_GET_VAL("commodity", j), book);
    gnc_price_set_commodity (pr, modity);

    modity = gnc_string_to_commodity (DB_GET_VAL("currency", j), book);
    gnc_price_set_currency (pr, modity);

    ts = gnc_iso8601_to_timespec_gmt (DB_GET_VAL("time", j));
    gnc_price_set_time (pr, ts);

    gnc_price_set_source (pr, DB_GET_VAL("source", j));
    gnc_price_set_typestr (pr, DB_GET_VAL("type", j));

    num = strtoll (DB_GET_VAL("valueNum", j), NULL, 0);
    denom = strtoll (DB_GET_VAL("valueDenom", j), NULL, 0);
    value = gnc_numeric_create (num, denom);
    gnc_price_set_value (pr, value);

    if (not_found) gnc_pricedb_add_price(prdb, pr);
    gnc_price_commit_edit (pr);
    gnc_price_unref (pr);

    return data;
}


void
pgendGetAllPricesInBook (PGBackend *be, QofBook *book)
{
    char buff[400], *p;

    if (!be) return;
    ENTER ("be=%p, conn=%p", be, be->connection);

    /* first, make sure commodities table is up to date */
    pgendGetAllCommodities (be);

    /* Get them ALL */
    p = buff;
    p = stpcpy (p, "SELECT * FROM gncPrice WHERE bookGuid='");
    p = guid_to_string_buff (qof_book_get_guid(book), p);
    p = stpcpy (p, "';");
    SEND_QUERY (be, buff, );
    pgendGetResults (be, get_price_cb, book);

    LEAVE (" ");
}

/* ============================================================= */

void
pgendPriceFind (QofBackend *bend, gpointer olook)
{
    PGBackend *be = (PGBackend *)bend;
    GNCPriceLookup *look = (GNCPriceLookup *)olook;
    const char * commodity_str;
    const char * currency_str;
    sqlEscape *escape;
    char * p;

    ENTER ("be=%p, lookup=%p", be, look);
    if (!be || !look)
    {
        LEAVE("(null) args");
        return;
    }

    /* special case the two-way search in terms of more basic primitives */
    if (LOOKUP_NEAREST_IN_TIME == look->type)
    {
        look->type = LOOKUP_LATEST_BEFORE;
        pgendPriceFind (bend, look);
        look->type = LOOKUP_EARLIEST_AFTER;
        pgendPriceFind (bend, look);
        LEAVE(" ");
        return;
    }

    escape = sqlEscape_new ();

    commodity_str = gnc_commodity_get_unique_name(look->commodity);
    currency_str  = gnc_commodity_get_unique_name(look->currency);

    /* don't send events  to GUI, don't accept callbacks to backend */
    qof_event_suspend();
    pgendDisable(be);

    /* set up the common part of the query */
    p = be->buff;
    *p = 0;
    p = stpcpy (p, "SELECT * FROM gncPrice"
                "  WHERE commodity='");
    p = stpcpy (p, sqlEscapeString (escape, commodity_str));
    p = stpcpy (p, "' ");

    if (currency_str)
    {
        p = stpcpy (p, "AND currency='");
        p = stpcpy (p, sqlEscapeString (escape, currency_str));
        p = stpcpy (p, "' ");
    }

    PINFO("query = %s", be->buff);

    sqlEscape_destroy (escape);
    escape = NULL;

    switch (look->type)
    {
    case LOOKUP_LATEST:
        p = stpcpy (p, "ORDER BY time DESC LIMIT 1;");
        break;
    case LOOKUP_ALL:
        /* Get all prices for this commodity and currency */
        p = stpcpy (p, ";");
        break;
    case LOOKUP_AT_TIME:
        p = stpcpy (p, "AND time='");
        p = gnc_timespec_to_iso8601_buff (look->date, p);
        p = stpcpy (p, "';");
        break;
    case LOOKUP_NEAREST_IN_TIME:
        PERR ("this can't possibly happen but it did!!!");
        p = stpcpy (p, ";");
        break;
    case LOOKUP_LATEST_BEFORE:
        p = stpcpy (p, "AND time <= '");
        p = gnc_timespec_to_iso8601_buff (look->date, p);
        p = stpcpy (p, "' ORDER BY time DESC LIMIT 1;");
        break;
    case LOOKUP_EARLIEST_AFTER:
        p = stpcpy (p, "AND time >= '");
        p = gnc_timespec_to_iso8601_buff (look->date, p);
        p = stpcpy (p, "' ORDER BY time ASC LIMIT 1;");
        break;
    default:
        PERR ("unknown lookup type %d", look->type);
        /* re-enable events */
        pgendEnable(be);
        qof_event_resume();
        LEAVE(" ");
        return;
    }

    SEND_QUERY (be, be->buff, );
    pgendGetResults (be, get_price_cb, NULL);

    /* re-enable events */
    pgendEnable(be);
    qof_event_resume();
    LEAVE(" ");
}

/* ============================================================= */
/* ============================================================= */
/*         HIGHER LEVEL ROUTINES AND BACKEND PROPER              */
/* ============================================================= */
/* ============================================================= */

void
pgend_price_begin_edit (QofBackend * bend, GNCPrice *pr)
{
    if (pr && pr->db && qof_instance_get_dirty_flag(pr->db))
    {
        PERR ("price db is unexpectedly dirty");
    }
    return;
}

void
pgend_price_commit_edit (QofBackend * bend, GNCPrice *pr)
{
    char * bufp;
    PGBackend *be = (PGBackend *)bend;

    ENTER ("be=%p, price=%p", be, pr);
    if (!be || !pr) return;

    /* lock it up so that we query and store atomically */
    bufp = "BEGIN;\n"
           "LOCK TABLE gncPrice IN EXCLUSIVE MODE;\n";
    SEND_QUERY (be, bufp,);
    FINISH_QUERY(be->connection);

    /* check to see that the engine version is equal or newer than
     * whats in the database.  It its not, then some other user has
     * made changes, and we must roll back. */
    if (0 < pgendPriceCompareVersion (be, pr))
    {
        qof_instance_set_destroying(pr, FALSE);
        bufp = "ROLLBACK;";
        SEND_QUERY (be, bufp,);
        FINISH_QUERY(be->connection);

        /* hack alert -- we should restore the price data from the
         * sql back end at this point ! !!! */
        PWARN(" price data in engine is newer\n"
              " price must be rolled back.  This function\n"
              " is not completely implemented !! \n");
        LEAVE ("rolled back");
        qof_backend_set_error (&be->be, ERR_BACKEND_MODIFIED);
        return;
    }
    /* be sure to update the version !! */
    qof_instance_increment_version(pr, be->version_check);

    if (qof_instance_get_destroying(pr))
    {
        pgendStoreAuditPrice (be, pr, SQL_DELETE);
        bufp = be->buff;
        *bufp = 0;
        bufp = stpcpy (bufp, "DELETE FROM gncPrice WHERE priceGuid='");
        bufp = guid_to_string_buff (gnc_price_get_guid(pr), bufp);
        bufp = stpcpy (bufp, "';");
        PINFO ("%s\n", be->buff ? be->buff : "(null)");
        SEND_QUERY (be, be->buff, );
        FINISH_QUERY(be->connection);
    }
    else
    {
        pgendStorePriceNoLock (be, pr, FALSE);
    }

    bufp = "COMMIT;\n"
           "NOTIFY gncPrice;";
    SEND_QUERY (be, bufp,);
    FINISH_QUERY(be->connection);

    if (pr->db)
        qof_instance_mark_clean(&pr->db->inst);

    LEAVE ("commited");
    return;
}

/* ======================== END OF FILE ======================== */
