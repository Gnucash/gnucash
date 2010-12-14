/********************************************************************\
 * gnc-entry-quickfill.c -- Create an entry description quick-fill  *
 * Copyright (C) 2010 Christian Stimming <christian@cstimming.de>   *
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
 *                                                                  *
\********************************************************************/

#include "config.h"
#include "gnc-entry-quickfill.h"

typedef struct
{
    QuickFill *qf;
    QuickFillSort qf_sort;
    QofBook *book;
} EntryQF;

static void
shared_quickfill_destroy (QofBook *book, gpointer key, gpointer user_data)
{
    EntryQF *qfb = user_data;
    gnc_quickfill_destroy (qfb->qf);
    /* qof_event_unregister_handler (qfb->listener); */
    g_free (qfb);
}

static void entry_cb(gpointer data, gpointer user_data)
{
    const GncEntry* entry = data;
    EntryQF *s = (EntryQF *) user_data;
    gnc_quickfill_insert (s->qf, gncEntryGetDescription(entry), s->qf_sort);
}

/** Creates a new query that searches for all GncEntry items in the
 * current book. */
static QofQuery *new_query_for_entrys(QofBook *book)
{
    QofQuery *query = qof_query_create_for (GNC_ID_ENTRY);
    g_assert(book);
    qof_query_set_book (query, book);
    return query;
}

static EntryQF* build_shared_quickfill (QofBook *book, const char * key)
{
    EntryQF *result;
    QofQuery *query = new_query_for_entrys(book);
    GList *entries = qof_query_run(query);

/*     g_warning("Found %d GncEntry items", g_list_length (entries)); */

    result = g_new0(EntryQF, 1);

    result->qf = gnc_quickfill_new();
    result->qf_sort = QUICKFILL_LIFO;
    result->book = book;

    g_list_foreach (entries, entry_cb, result);

    qof_query_destroy(query);

    qof_book_set_data_fin (book, key, result, shared_quickfill_destroy);

    return result;
}

QuickFill * gnc_get_shared_entry_desc_quickfill (QofBook *book,
        const char * key)
{
    EntryQF *qfb;

    g_assert(book);
    g_assert(key);

    qfb = qof_book_get_data (book, key);

    if (qfb)
        return qfb->qf;

    qfb = build_shared_quickfill(book, key);
    return qfb->qf;
}
