/********************************************************************\
 * gnc-addr-quickfill.c -- Create an address line quick-fill  *
 * Copyright (C) 2011 Christian Stimming <christian@cstimming.de>   *
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
#include "gnc-addr-quickfill.h"
#include "engine/gnc-event.h"
#include "engine/gnc-engine.h"
#include "engine/gncAddress.h"

/* This static indicates the debugging module that this .o belongs to. */
static QofLogModule log_module = GNC_MOD_REGISTER;

typedef struct
{
    QuickFill *qf_addr2;
    QuickFill *qf_addr3;
    QuickFillSort qf_sort;
    QofBook *book;
    gint  listener;
} AddressQF;

static void
listen_for_gncaddress_events(QofInstance *entity,  QofEventId event_type,
                             gpointer user_data, gpointer event_data)
{
    AddressQF *qfb = user_data;
    const char *addr2, *addr3;

    /* We only listen for GncAddress events */
    if (!GNC_IS_ADDRESS (entity))
        return;

    /* We listen for MODIFY (if the description was changed into
     * something non-empty, so we add the string to the quickfill) and
     * DESTROY (to remove the description from the quickfill). */
    if (0 == (event_type & (QOF_EVENT_MODIFY | QOF_EVENT_DESTROY)))
        return;

    /*     g_warning("entity %p, entity type %s, event type %s, user data %p, ecent data %p", */
    /*               entity, entity->e_type, qofeventid_to_string(event_type), user_data, event_data); */

    addr2 = gncAddressGetAddr2(GNC_ADDRESS(entity));
    addr3 = gncAddressGetAddr3(GNC_ADDRESS(entity));
    if (event_type & QOF_EVENT_MODIFY)
    {
        /* If the description was changed into something non-empty, so
         * we add the string to the quickfill */
        if (addr2 && strlen(addr2) > 0)
        {
            /* Add the new string to the quickfill */
            gnc_quickfill_insert (qfb->qf_addr2, addr2, QUICKFILL_LIFO);
        }
        if (addr3 && strlen(addr3) > 0)
        {
            /* Add the new string to the quickfill */
            gnc_quickfill_insert (qfb->qf_addr3, addr3, QUICKFILL_LIFO);
        }
    }
    else if (event_type & QOF_EVENT_DESTROY)
    {
        if (addr2 && strlen(addr2) > 0)
        {
            /* Remove the description from the quickfill */
            gnc_quickfill_insert (qfb->qf_addr2, addr2, QUICKFILL_LIFO);
        }
        if (addr3 && strlen(addr3) > 0)
        {
            /* Remove the description from the quickfill */
            gnc_quickfill_insert (qfb->qf_addr3, addr3, QUICKFILL_LIFO);
        }
    }
}

static void
shared_quickfill_destroy (QofBook *book, gpointer key, gpointer user_data)
{
    AddressQF *qfb = user_data;
    gnc_quickfill_destroy (qfb->qf_addr2);
    gnc_quickfill_destroy (qfb->qf_addr3);
    qof_event_unregister_handler (qfb->listener);
    g_free (qfb);
}

static void address_cb(gpointer data, gpointer user_data)
{
    const GncAddress* address = data;
    AddressQF *s = (AddressQF *) user_data;

    gnc_quickfill_insert (s->qf_addr2,
                          gncAddressGetAddr2(address),
                          s->qf_sort);

    gnc_quickfill_insert (s->qf_addr3,
                          gncAddressGetAddr3(address),
                          s->qf_sort);
}

/** Creates a new query that searches for all GncAddress items in the
 * current book. */
static QofQuery *new_query_for_addresss(QofBook *book)
{
    GSList *primary_sort_params = NULL;
    QofQuery *query = qof_query_create_for (GNC_ID_ADDRESS);
    g_assert(book);
    qof_query_set_book (query, book);

    /* No particular sort order here. */

    return query;
}

static AddressQF* build_shared_quickfill (QofBook *book, const char * key)
{
    AddressQF *result;
    QofQuery *query = new_query_for_addresss(book);
    GList *entries = qof_query_run(query);

    /*     g_warning("Found %d GncAddress items", g_list_length (entries)); */

    result = g_new0(AddressQF, 1);

    result->qf_addr2 = gnc_quickfill_new();
    result->qf_addr3 = gnc_quickfill_new();
    result->qf_sort = QUICKFILL_LIFO;
    result->book = book;

    g_list_foreach (entries, address_cb, result);

    qof_query_destroy(query);

    result->listener =
        qof_event_register_handler (listen_for_gncaddress_events,
                                    result);

    qof_book_set_data_fin (book, key, result, shared_quickfill_destroy);

    return result;
}

QuickFill * gnc_get_shared_address_addr2_quickfill (QofBook *book, const char * key)
{
    AddressQF *qfb;

    g_assert(book);
    g_assert(key);

    qfb = qof_book_get_data (book, key);

    if (!qfb)
    {
        qfb = build_shared_quickfill(book, key);
    }

    return qfb->qf_addr2;
}

QuickFill * gnc_get_shared_address_addr3_quickfill (QofBook *book, const char * key)
{
    AddressQF *qfb;

    g_assert(book);
    g_assert(key);

    qfb = qof_book_get_data (book, key);

    if (!qfb)
    {
        qfb = build_shared_quickfill(book, key);
    }

    return qfb->qf_addr3;
}
