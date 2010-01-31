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

#include "config.h"

#include <glib.h>
#include <stdlib.h>
#include <string.h>

#include "gnc-gconf-utils.h"
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
#include "qof.h"

/* non-static because it's used in io-gncxml-v2.c */
const gchar *gnc_v2_book_version_string = "2.0.0";

/* ids */
#define gnc_book_string "gnc:book"
#define book_id_string "book:id"
#define book_slots_string "book:slots"

static QofLogModule log_module = GNC_MOD_IO;

/* ================================================================ */

#ifdef IMPLEMENT_BOOK_DOM_TREES_LATER

static void
append_account_tree (xmlNodePtr parent,
                     Account *account,
                     gboolean allow_incompat)
{
    GList *children, *node;

    children = gnc_account_get_children(account);
    for (node = children; node; node = node->next)
    {
        xmlNodePtr accnode;
        Account *account;

        account = node->data;
        accnode = gnc_account_dom_tree_create(account, FALSE, allow_incompat);
        xmlAddChild (parent, accnode);
        append_account_tree(accnode, account);
    }
    g_list_free(children);
}

static int
traverse_txns (Transaction *txn, gpointer data)
{
    xmlNodePtr node;
    xmlNodePtr parent = data;

    node = gnc_transaction_dom_tree_create(txn);
    xmlAddChild (parent, node);

    return 0;
}
#endif

/* ================================================================ */

xmlNodePtr
gnc_book_dom_tree_create(QofBook *book)
{
    xmlNodePtr ret;
    gboolean allow_incompat = TRUE;

    ret = xmlNewNode(NULL, BAD_CAST gnc_book_string);
    xmlSetProp(ret, BAD_CAST "version", BAD_CAST gnc_v2_book_version_string);

    xmlAddChild(ret, guid_to_dom_tree(book_id_string,
                                      qof_book_get_guid(book)));

    if (qof_book_get_slots(book))
    {
        xmlNodePtr kvpnode = kvp_frame_to_dom_tree(book_slots_string,
                             qof_book_get_slots(book));
        if (kvpnode)
            xmlAddChild(ret, kvpnode);
    }

#ifdef IMPLEMENT_BOOK_DOM_TREES_LATER
    /* theoretically, we should be adding all the below to the book
     * but in fact, there's enough brain damage in the code already
     * that we are only going to hand-edit the file at a higher layer.
     * And that's OK, since its probably a performance boost anyway.
     */
    xmlAddChild(ret, gnc_commodity_dom_tree_create(
                    gnc_book_get_commodity_table(book)));
    xmlAddChild(ret, gnc_pricedb_dom_tree_create(gnc_book_get_pricedb(book)));
    if (allow_incompat)
    {
        accnode = gnc_account_dom_tree_create(account, FALSE);
        xmlAddChild (ret, rootAccNode);
    }
    append_account_tree (ret, gnc_book_get_root(book));

    xaccAccountTreeForEachTransaction (gnc_book_get_root_account(book),
                                       traverse_txns, ret);

    /* xxx FIXME hack alert how are we going to handle
     *  gnc_book_get_template_group handled ???   */
    xmlAddChild(ret, gnc_schedXaction_dom_tree_create(
                    gnc_book_get_schedxactions(book)));

#endif

    return ret;
}

/* ================================================================ */
/* same as above, but we write out directly.  Only handle the guid
 * and slots, everything else is handled elsewhere */

gboolean
write_book_parts(FILE *out, QofBook *book)
{
    xmlNodePtr domnode;

    domnode = guid_to_dom_tree(book_id_string, qof_book_get_guid(book));
    xmlElemDump(out, NULL, domnode);
    xmlFreeNode (domnode);

    if (ferror(out) || fprintf(out, "\n") < 0)
         return FALSE;

    if (qof_book_get_slots(book))
    {
        xmlNodePtr kvpnode = kvp_frame_to_dom_tree(book_slots_string,
                             qof_book_get_slots(book));
        if (kvpnode)
        {
            xmlElemDump(out, NULL, kvpnode);
            xmlFreeNode(kvpnode);

            if (ferror(out) || fprintf(out, "\n") < 0)
                 return FALSE;
        }
    }

    return TRUE;
}


/* ================================================================ */

static gboolean
book_id_handler(xmlNodePtr node, gpointer book_pdata)
{
    QofBook *book = book_pdata;
    GUID *guid;

    guid = dom_tree_to_guid(node);
    qof_instance_set_guid(QOF_INSTANCE(book), guid);
    g_free(guid);

    return TRUE;
}

static gboolean
book_slots_handler (xmlNodePtr node, gpointer book_pdata)
{
    QofBook *book = book_pdata;
    gboolean success;

    /* the below works only because the get is gaurenteed to return
     * a frame, even if its empty */
    success = dom_tree_to_kvp_frame_given (node, qof_book_get_slots (book));

    g_return_val_if_fail(success, FALSE);

    return TRUE;
}


static struct dom_tree_handler book_handlers_v2[] =
{
    { book_id_string, book_id_handler, 1, 0 },
    { book_slots_string, book_slots_handler, 0, 0 },
    { NULL, 0, 0, 0 }
};

static gboolean
gnc_book_end_handler(gpointer data_for_children,
                     GSList* data_from_children, GSList* sibling_data,
                     gpointer parent_data, gpointer global_data,
                     gpointer *result, const gchar *tag)
{
    int successful;
    xmlNodePtr tree = (xmlNodePtr)data_for_children;
    gxpf_data *gdata = (gxpf_data*)global_data;
    QofBook *book = gdata->bookdata;

    successful = TRUE;

    if (parent_data) return TRUE;

    /* OK.  For some messed up reason this is getting called again with a
       NULL tag.  So we ignore those cases */
    if (!tag) return TRUE;

    g_return_val_if_fail(tree, FALSE);

    book = dom_tree_to_book(tree, book);
    if (!book)
        gdata->cb(tag, gdata->parsedata, book);

    xmlFreeNode(tree);

    return book != NULL;
}

static gboolean
gnc_book_id_end_handler(gpointer data_for_children,
                        GSList* data_from_children, GSList* sibling_data,
                        gpointer parent_data, gpointer global_data,
                        gpointer *result, const gchar *tag)
{
    gboolean successful;
    xmlNodePtr tree = (xmlNodePtr)data_for_children;
    gxpf_data *gdata = (gxpf_data*)global_data;
    QofBook *book = gdata->bookdata;

    if (parent_data) return TRUE;
    if (!tag) return TRUE;

    g_return_val_if_fail(tree, FALSE);

    successful = book_id_handler(tree, book);
    xmlFreeNode(tree);

    return successful;
}

static gboolean
gnc_book_slots_end_handler(gpointer data_for_children,
                           GSList* data_from_children, GSList* sibling_data,
                           gpointer parent_data, gpointer global_data,
                           gpointer *result, const gchar *tag)
{
    gboolean successful;
    xmlNodePtr tree = (xmlNodePtr)data_for_children;
    gxpf_data *gdata = (gxpf_data*)global_data;
    QofBook *book = gdata->bookdata;

    if (parent_data) return TRUE;
    if (!tag) return TRUE;

    g_return_val_if_fail(tree, FALSE);

    successful = book_slots_handler (tree, book);
    xmlFreeNode(tree);

    return successful;
}

QofBook*
dom_tree_to_book (xmlNodePtr node, QofBook *book)
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
gnc_book_sixtp_parser_create(void)
{
    return sixtp_dom_parser_new(gnc_book_end_handler, NULL, NULL);
}

sixtp*
gnc_book_id_sixtp_parser_create(void)
{
    return sixtp_dom_parser_new(gnc_book_id_end_handler, NULL, NULL);
}

sixtp*
gnc_book_slots_sixtp_parser_create(void)
{
    return sixtp_dom_parser_new(gnc_book_slots_end_handler, NULL, NULL);
}
