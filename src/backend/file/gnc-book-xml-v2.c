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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#include "config.h"

#include <glib.h>
#include <stdlib.h>
#include <string.h>

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
#include "gnc-book-p.h"
#include "gnc-book.h"
#include "gnc-engine-util.h"
#include "Group.h"

const gchar *book_version_string = "2.0.0";

/* ids */
#define gnc_book_string "gnc:book"
#define book_id_string "book:id"
#define book_slots_string "book:slots"

static short module = MOD_IO;

/* ================================================================ */

#ifdef IMPLEMENT_BOOK_DOM_TREES_LATER

static void
append_group (xmlNodePtr parent, AccountGroup *grp)
{
    GList *list;
    GList *node;

    list = xaccGroupGetAccountList(grp);

    for (node = list; node; node = node->next) 
    {
        xmlNodePtr accnode;
        AccountGroup *newgrp;

        accnode = gnc_account_dom_tree_create((Account*)(node->data));
        xmlAddChild (parent, accnode);

        newgrp = xaccAccountGetChildren((Account*)(node->data));

        if (newgrp)
        {
            append_group(accnode, newgrp);
        }
    }
}

static gboolean 
traverse_txns (Transaction *txn, gpointer data)
{
    xmlNodePtr node;
    xmlNodePtr parent = data;

    node = gnc_transaction_dom_tree_create(txn);
    xmlAddChild (parent, node);

    return TRUE;
}
#endif

/* ================================================================ */

xmlNodePtr
gnc_book_dom_tree_create(GNCBook *book)
{
    xmlNodePtr ret;

    ret = xmlNewNode(NULL, gnc_book_string);
    xmlSetProp(ret, "version", book_version_string);

    xmlAddChild(ret, guid_to_dom_tree(book_id_string, gnc_book_get_guid(book)));

    if(gnc_book_get_slots(book))
    {
        xmlNodePtr kvpnode = kvp_frame_to_dom_tree(book_slots_string,
                                                   gnc_book_get_slots(book));
        if(kvpnode)
        {
            xmlAddChild(ret, kvpnode);
        }
    }

#ifdef IMPLEMENT_BOOK_DOM_TREES_LATER
    /* theoretically, we should be adding all the below to the book
     * but in fact, there's enough brain damage in the code already 
     * that we are only going to hand-edit the file at a higher layer.
     * And that's OK, since its probably a performance boost anyway.
     */
    xmlAddChild(ret, gnc_commodity_dom_tree_create (gnc_book_get_commodity_table(book)));
    xmlAddChild(ret, gnc_pricedb_dom_tree_create (gnc_book_get_pricedb(book)));
    append_group (ret, gnc_book_get_group(book));

    xaccGroupForEachTransaction (gnc_book_get_group(book),
                                traverse_txns, ret);

    xmlAddChild(ret, gnc_freqSpec_dom_tree_create (book));

    /* xxx FIXME hack alert how are we going to handle 
     *  gnc_book_get_template_group handled ???   */
    xmlAddChild(ret, gnc_schedXaction_dom_tree_create (gnc_book_get_schedxactions(book)));

#endif 

    return ret;
}

/* ================================================================ */
/* same as above, but we write out directly.  Only handle the guid 
 * and slots, everythign else is handled elsewehere */

void
write_book_parts(FILE *out, GNCBook *book)
{
    xmlNodePtr domnode;

    domnode = guid_to_dom_tree(book_id_string, gnc_book_get_guid(book));
    xmlElemDump(out, NULL, domnode);
    fprintf(out, "\n");
    xmlFreeNode (domnode);

    if(gnc_book_get_slots(book))
    {
        xmlNodePtr kvpnode = kvp_frame_to_dom_tree(book_slots_string,
                                                   gnc_book_get_slots(book));
        if(kvpnode)
        {
            xmlElemDump(out, NULL, kvpnode);
            fprintf(out, "\n");
            xmlFreeNode (kvpnode);
        }
    }
}

/* ================================================================ */

static gboolean
book_id_handler (xmlNodePtr node, gpointer book_pdata)
{
    GNCBook *book = book_pdata;
    GUID *guid;

    guid = dom_tree_to_guid(node);
    gnc_book_set_guid(book, *guid);

    g_free(guid);
    
    return TRUE;
}

static gboolean
book_slots_handler (xmlNodePtr node, gpointer book_pdata)
{
    GNCBook *book = book_pdata;
    gboolean success;

    /* the below works only because the get is gaurenteed to return 
     * a frame, even if its empty */
    success = dom_tree_to_kvp_frame_given (node, gnc_book_get_slots (book));

    g_return_val_if_fail(success, FALSE);
    
    return TRUE;
}


static struct dom_tree_handler book_handlers_v2[] = {
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
    GNCBook *book = gdata->bookdata;

    successful = TRUE;

    if(parent_data)
    {
        return TRUE;
    }

    /* OK.  For some messed up reason this is getting called again with a
       NULL tag.  So we ignore those cases */
    if(!tag)
    {
        return TRUE;
    }

    g_return_val_if_fail(tree, FALSE);

    book = dom_tree_to_book(tree, book);
    if (NULL != book)
    {
        gdata->cb(tag, gdata->parsedata, book);
    }

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
    GNCBook *book = gdata->bookdata;

    if(parent_data) { return TRUE; }
    if(!tag) { return TRUE; }

    g_return_val_if_fail(tree, FALSE);

    successful = book_id_handler (tree, book);
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
    GNCBook *book = gdata->bookdata;

    if(parent_data) { return TRUE; }
    if(!tag) { return TRUE; }

    g_return_val_if_fail(tree, FALSE);

    successful = book_slots_handler (tree, book);
    xmlFreeNode(tree);

    return successful;
}

GNCBook*
dom_tree_to_book (xmlNodePtr node, GNCBook *book)
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

