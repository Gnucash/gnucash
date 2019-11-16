/********************************************************************\
 * gnc-book-xml-v2.c -- book xml i/o implementation                 *
 *                                                                  *
 * Copyright (C) 2001 James LewisMoss <dres@debian.org>             *
 * Copyright (C) 2001 Linas Vepstas <linas@linas.org>               *
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
extern "C"
{
#include <config.h>

#include <glib.h>
#include <stdlib.h>
#include <string.h>
#include "qof.h"
}

#include "gnc-xml-helper.h"

#include "sixtp.h"
#include "sixtp-utils.h"
#include "sixtp-parsers.h"
#include "sixtp-utils.h"
#include "sixtp-dom-parsers.h"
#include "sixtp-dom-generators.h"

#include "gnc-xml.h"
#include "io-gncxml-gen.h"
#include "io-gncxml-v2.h"
#include "io-utils.h"

#include "sixtp-dom-parsers.h"

/* non-static because it's used in io-gncxml-v2.c */
const gchar* gnc_v2_book_version_string = "2.0.0";

/* ids */
#define gnc_book_string "gnc:book"
#define book_id_string "book:id"
#define book_slots_string "book:slots"

static QofLogModule log_module = GNC_MOD_IO;

/* ================================================================ */

xmlNodePtr
gnc_book_dom_tree_create (QofBook* book)
{
    xmlNodePtr ret;
    G_GNUC_UNUSED gboolean allow_incompat = TRUE;

    ret = xmlNewNode (NULL, BAD_CAST gnc_book_string);
    xmlSetProp (ret, BAD_CAST "version", BAD_CAST gnc_v2_book_version_string);

    xmlAddChild (ret, guid_to_dom_tree (book_id_string,
                                        qof_book_get_guid (book)));

    /* xmlAddChild won't do anything with a NULL, so tests are superfluous. */
    xmlAddChild (ret, qof_instance_slots_to_dom_tree (book_slots_string,
                                                      QOF_INSTANCE (book)));

    return ret;
}

/* ================================================================ */
/* same as above, but we write out directly.  Only handle the guid
 * and slots, everything else is handled elsewhere */

gboolean
write_book_parts (FILE* out, QofBook* book)
{
    xmlNodePtr domnode, slotsnode;

    domnode = guid_to_dom_tree (book_id_string, qof_book_get_guid (book));
    xmlElemDump (out, NULL, domnode);
    xmlFreeNode (domnode);

    if (ferror (out) || fprintf (out, "\n") < 0)
        return FALSE;


    slotsnode = qof_instance_slots_to_dom_tree (book_slots_string,
                                                QOF_INSTANCE (book));
    if (slotsnode)
    {
        xmlElemDump (out, NULL, slotsnode);
        xmlFreeNode (slotsnode);

        if (ferror (out) || fprintf (out, "\n") < 0)
            return FALSE;
    }

    return TRUE;
}


/* ================================================================ */

static gboolean
book_id_handler (xmlNodePtr node, gpointer book_pdata)
{
    QofBook* book = static_cast<decltype (book)> (book_pdata);
    GncGUID* guid;

    guid = dom_tree_to_guid (node);
    qof_instance_set_guid (QOF_INSTANCE (book), guid);
    guid_free (guid);

    return TRUE;
}

static gboolean
book_slots_handler (xmlNodePtr node, gpointer book_pdata)
{
    QofBook* book = static_cast<decltype (book)> (book_pdata);
    gboolean success;

    /* the below works only because the get is gaurenteed to return
     * a frame, even if its empty */
    success = dom_tree_create_instance_slots (node, QOF_INSTANCE (book));

    g_return_val_if_fail (success, FALSE);

    return TRUE;
}


static struct dom_tree_handler book_handlers_v2[] =
{
    { book_id_string, book_id_handler, 1, 0 },
    { book_slots_string, book_slots_handler, 0, 0 },
    { NULL, 0, 0, 0 }
};

static gboolean
gnc_book_end_handler (gpointer data_for_children,
                      GSList* data_from_children, GSList* sibling_data,
                      gpointer parent_data, gpointer global_data,
                      gpointer* result, const gchar* tag)
{
    xmlNodePtr tree = (xmlNodePtr)data_for_children;
    gxpf_data* gdata = (gxpf_data*)global_data;
    QofBook* book = static_cast<decltype (book)> (gdata->bookdata);


    if (parent_data) return TRUE;

    /* OK.  For some messed up reason this is getting called again with a
       NULL tag.  So we ignore those cases */
    if (!tag) return TRUE;

    g_return_val_if_fail (tree, FALSE);

    book = dom_tree_to_book (tree, book);
    if (!book)
        gdata->cb (tag, gdata->parsedata, book);

    xmlFreeNode (tree);

    return book != NULL;
}

static gboolean
gnc_book_id_end_handler (gpointer data_for_children,
                         GSList* data_from_children, GSList* sibling_data,
                         gpointer parent_data, gpointer global_data,
                         gpointer* result, const gchar* tag)
{
    gboolean successful;
    xmlNodePtr tree = (xmlNodePtr)data_for_children;
    gxpf_data* gdata = (gxpf_data*)global_data;
    QofBook* book = static_cast<decltype (book)> (gdata->bookdata);

    if (parent_data) return TRUE;
    if (!tag) return TRUE;

    g_return_val_if_fail (tree, FALSE);

    successful = book_id_handler (tree, book);
    xmlFreeNode (tree);

    return successful;
}

static gboolean
gnc_book_slots_end_handler (gpointer data_for_children,
                            GSList* data_from_children, GSList* sibling_data,
                            gpointer parent_data, gpointer global_data,
                            gpointer* result, const gchar* tag)
{
    gboolean successful;
    xmlNodePtr tree = (xmlNodePtr)data_for_children;
    gxpf_data* gdata = (gxpf_data*)global_data;
    QofBook* book = static_cast<decltype (book)> (gdata->bookdata);

    if (parent_data) return TRUE;
    if (!tag) return TRUE;

    g_return_val_if_fail (tree, FALSE);

    successful = book_slots_handler (tree, book);
    xmlFreeNode (tree);

    return successful;
}

QofBook*
dom_tree_to_book (xmlNodePtr node, QofBook* book)
{
    gboolean successful;

    successful = dom_tree_generic_parse (node, book_handlers_v2,
                                         book);
    if (!successful)
    {
        PERR ("failed to parse book");
        book = NULL;
    }

    return book;
}

sixtp*
gnc_book_sixtp_parser_create (void)
{
    return sixtp_dom_parser_new (gnc_book_end_handler, NULL, NULL);
}

sixtp*
gnc_book_id_sixtp_parser_create (void)
{
    return sixtp_dom_parser_new (gnc_book_id_end_handler, NULL, NULL);
}

sixtp*
gnc_book_slots_sixtp_parser_create (void)
{
    return sixtp_dom_parser_new (gnc_book_slots_end_handler, NULL, NULL);
}
