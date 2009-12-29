/********************************************************************
 * gnc-pricedb-xml-v2.c -- xml routines for price db                *
 * Copyright (C) 2001 Gnumatic, Inc.                                *
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
 *******************************************************************/

#include "config.h"

#include <string.h>

#include "gnc-xml.h"
#include "sixtp.h"
#include "sixtp-utils.h"
#include "sixtp-parsers.h"
#include "sixtp-dom-parsers.h"
#include "sixtp-dom-generators.h"
#include "io-gncxml-gen.h"

#include "gnc-pricedb.h"
#include "gnc-pricedb-p.h"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_IO;

/* Read and Write the pricedb as XML -- something like this:

  <pricedb>
    price-1
    price-2
    ...
  </pricedb>

  where each price should look roughly like this:

  <price>
    <price:id>
      00000000111111112222222233333333
    </price:id>
    <price:commodity>
      <cmdty:space>NASDAQ</cmdty:space>
      <cmdty:id>RHAT</cmdty:id>
    </price:commodity>
    <price:currency>
      <cmdty:space>ISO?</cmdty:space>
      <cmdty:id>USD</cmdty:id>
    </price:currency>
    <price:time><ts:date>Mon ...</ts:date><ts:ns>12</ts:ns></price:time>
    <price:source>Finance::Quote</price:source>
    <price:type>bid</price:type>
    <price:value>11011/100</price:value>
  </price>

*/

/***********************************************************************/
/* READING */
/***********************************************************************/

/****************************************************************************/
/* <price>

  restores a price.  Does so via a walk of the XML tree in memory.
  Returns a GNCPrice * in result.

  Right now, a price is legitimate even if all of it's fields are not
  set.  We may need to change that later, but at the moment.

*/

static gboolean
price_parse_xml_sub_node(GNCPrice *p, xmlNodePtr sub_node, QofBook *book)
{
    if (!p || !sub_node) return FALSE;

    gnc_price_begin_edit (p);
    if (safe_strcmp("price:id", (char*)sub_node->name) == 0)
    {
        GUID *c = dom_tree_to_guid(sub_node);
        if (!c) return FALSE;
        gnc_price_set_guid(p, c);
        g_free(c);
    }
    else if (safe_strcmp("price:commodity", (char*)sub_node->name) == 0)
    {
        gnc_commodity *c = dom_tree_to_commodity_ref(sub_node, book);
        if (!c) return FALSE;
        gnc_price_set_commodity(p, c);
    }
    else if (safe_strcmp("price:currency", (char*)sub_node->name) == 0)
    {
        gnc_commodity *c = dom_tree_to_commodity_ref(sub_node, book);
        if (!c) return FALSE;
        gnc_price_set_currency(p, c);
    }
    else if (safe_strcmp("price:time", (char*)sub_node->name) == 0)
    {
        Timespec t = dom_tree_to_timespec(sub_node);
        if (!dom_tree_valid_timespec(&t, sub_node->name)) return FALSE;
        gnc_price_set_time(p, t);
    }
    else if (safe_strcmp("price:source", (char*)sub_node->name) == 0)
    {
        char *text = dom_tree_to_text(sub_node);
        if (!text) return FALSE;
        gnc_price_set_source(p, text);
        g_free(text);
    }
    else if (safe_strcmp("price:type", (char*)sub_node->name) == 0)
    {
        char *text = dom_tree_to_text(sub_node);
        if (!text) return FALSE;
        gnc_price_set_typestr(p, text);
        g_free(text);
    }
    else if (safe_strcmp("price:value", (char*)sub_node->name) == 0)
    {
        gnc_numeric *value = dom_tree_to_gnc_numeric(sub_node);
        if (!value) return FALSE;
        gnc_price_set_value(p, *value);
        g_free(value);
    }
    gnc_price_commit_edit (p);
    return TRUE;
}

static gboolean
price_parse_xml_end_handler(gpointer data_for_children,
                            GSList* data_from_children,
                            GSList* sibling_data,
                            gpointer parent_data,
                            gpointer global_data,
                            gpointer *result,
                            const gchar *tag)
{
    gboolean ok = TRUE;
    xmlNodePtr price_xml = (xmlNodePtr) data_for_children;
    xmlNodePtr child;
    GNCPrice *p = NULL;
    gxpf_data *gdata = global_data;
    QofBook *book = gdata->bookdata;

    /* we haven't been handed the *top* level node yet... */
    if (parent_data) return TRUE;

    *result = NULL;

    if (!price_xml) return FALSE;
    if (price_xml->next)
    {
        ok = FALSE;
        goto cleanup_and_exit;
    }
    if (price_xml->prev)
    {
        ok = FALSE;
        goto cleanup_and_exit;
    }
    if (!price_xml->xmlChildrenNode)
    {
        ok = FALSE;
        goto cleanup_and_exit;
    }

    p = gnc_price_create(book);
    if (!p)
    {
        ok = FALSE;
        goto cleanup_and_exit;
    }

    for (child = price_xml->xmlChildrenNode; child; child = child->next)
    {
        switch (child->type)
        {
        case XML_COMMENT_NODE:
        case XML_TEXT_NODE:
            break;
        case XML_ELEMENT_NODE:
            if (!price_parse_xml_sub_node(p, child, book))
            {
                ok = FALSE;
                goto cleanup_and_exit;
            }
            break;
        default:
            PERR("Unknown node type (%d) while parsing gnc-price xml.", child->type);
            child = NULL;
            ok = FALSE;
            goto cleanup_and_exit;
            break;
        }
    }

cleanup_and_exit:
    if (ok)
    {
        *result = p;
    }
    else
    {
        *result = NULL;
        gnc_price_unref(p);
    }
    xmlFreeNode(price_xml);
    return ok;
}

static void
cleanup_gnc_price(sixtp_child_result *result)
{
    if (result->data) gnc_price_unref((GNCPrice *) result->data);
}

static sixtp *
gnc_price_parser_new (void)
{
    return sixtp_dom_parser_new(price_parse_xml_end_handler,
                                cleanup_gnc_price,
                                cleanup_gnc_price);
}


/****************************************************************************/
/* <pricedb> (lineage <ledger-data>)

   restores a pricedb.  We allocate the new db in the start block, the
   children add to it, and it gets returned in result.  Note that the
   cleanup handler will destroy the pricedb, so the parent needs to
   stop that if desired.

   result: GNCPriceDB*

   start: create new GNCPriceDB*, and leave in *data_for_children.
   cleanup-result: destroy GNCPriceDB*
   result-fail: destroy GNCPriceDB*

*/

static gboolean
pricedb_start_handler(GSList* sibling_data,
                      gpointer parent_data,
                      gpointer global_data,
                      gpointer *data_for_children,
                      gpointer *result,
                      const gchar *tag,
                      gchar **attrs)
{
    gxpf_data *gdata = global_data;
    QofBook *book = gdata->bookdata;
    GNCPriceDB *db = gnc_book_get_pricedb(book);
    g_return_val_if_fail(db, FALSE);
    gnc_pricedb_set_bulk_update(db, TRUE);
    *result = db;
    return(TRUE);
}

static gboolean
pricedb_after_child_handler(gpointer data_for_children,
                            GSList* data_from_children,
                            GSList* sibling_data,
                            gpointer parent_data,
                            gpointer global_data,
                            gpointer *result,
                            const gchar *tag,
                            const gchar *child_tag,
                            sixtp_child_result *child_result)
{
    GNCPriceDB *db = (GNCPriceDB *) * result;

    g_return_val_if_fail(db, FALSE);

    /* right now children have to produce results :> */
    if (!child_result) return(FALSE);
    if (child_result->type != SIXTP_CHILD_RESULT_NODE) return(FALSE);

    if (strcmp(child_result->tag, "price") == 0)
    {
        GNCPrice *p = (GNCPrice *) child_result->data;

        g_return_val_if_fail(p, FALSE);
        gnc_pricedb_add_price(db, p);
        /* can't do this because the v1 parser doesn't use this data
           structure as global data */
        /* ((sixtp_gdv2*)global_data)->counter.prices_loaded++; */
        return TRUE;
    }
    else
    {
        PERR ("unexpected tag %s\n", child_result->tag);
        return FALSE;
    }
    return FALSE;
}

static void
pricedb_cleanup_result_handler(sixtp_child_result *result)
{
    if (result->data)
    {
        GNCPriceDB *db = (GNCPriceDB *) result->data;
        if (db) gnc_pricedb_destroy(db);
        result->data = NULL;
    }
}

static gboolean
pricedb_v2_end_handler(
    gpointer data_for_children, GSList* data_from_children,
    GSList* sibling_data, gpointer parent_data, gpointer global_data,
    gpointer *result, const gchar *tag)
{
    GNCPriceDB *db = *result;
    gxpf_data *gdata = (gxpf_data*)global_data;

    if (parent_data)
    {
        return TRUE;
    }

    if (!tag)
    {
        return TRUE;
    }

    gdata->cb(tag, gdata->parsedata, db);
    *result = NULL;

    gnc_pricedb_set_bulk_update(db, FALSE);

    return TRUE;
}

static sixtp*
gnc_pricedb_parser_new(void)
{
    sixtp *top_level;
    sixtp *price_parser;

    top_level =
        sixtp_set_any(sixtp_new(), TRUE,
                      SIXTP_START_HANDLER_ID, pricedb_start_handler,
                      SIXTP_AFTER_CHILD_HANDLER_ID, pricedb_after_child_handler,
                      SIXTP_CHARACTERS_HANDLER_ID,
                      allow_and_ignore_only_whitespace,
                      SIXTP_RESULT_FAIL_ID, pricedb_cleanup_result_handler,
                      SIXTP_CLEANUP_RESULT_ID, pricedb_cleanup_result_handler,
                      SIXTP_NO_MORE_HANDLERS);

    if (!top_level) return NULL;

    price_parser = gnc_price_parser_new();

    if (!price_parser)
    {
        sixtp_destroy(top_level);
        return NULL;
    }

    sixtp_add_sub_parser(top_level, "price", price_parser);

    return top_level;
}

sixtp*
gnc_pricedb_sixtp_parser_create(void)
{
    sixtp *ret;
    ret = gnc_pricedb_parser_new();
    sixtp_set_end(ret, pricedb_v2_end_handler);
    return ret;
}


/***********************************************************************/
/* WRITING */
/***********************************************************************/

static gboolean
add_child_or_kill_parent(xmlNodePtr parent, xmlNodePtr child)
{
    if (!child)
    {
        xmlFreeNode(parent);
        return FALSE;
    }
    xmlAddChild(parent, child);
    return TRUE;
}

static xmlNodePtr
gnc_price_to_dom_tree(const xmlChar *tag, GNCPrice *price)
{
    xmlNodePtr price_xml;
    const gchar *typestr, *sourcestr;
    xmlNodePtr tmpnode;
    gnc_commodity *commodity;
    gnc_commodity *currency;
    Timespec timesp;
    gnc_numeric value;

    if (!(tag && price)) return NULL;

    price_xml = xmlNewNode(NULL, tag);
    if (!price_xml) return NULL;

    commodity = gnc_price_get_commodity(price);
    currency = gnc_price_get_currency(price);

    if (!(commodity && currency)) return NULL;

    tmpnode = guid_to_dom_tree("price:id", gnc_price_get_guid(price));
    if (!add_child_or_kill_parent(price_xml, tmpnode)) return NULL;

    tmpnode = commodity_ref_to_dom_tree("price:commodity", commodity);
    if (!add_child_or_kill_parent(price_xml, tmpnode)) return NULL;

    tmpnode = commodity_ref_to_dom_tree("price:currency", currency);
    if (!add_child_or_kill_parent(price_xml, tmpnode)) return NULL;

    timesp = gnc_price_get_time(price);
    tmpnode = timespec_to_dom_tree("price:time", &timesp);
    if (!add_child_or_kill_parent(price_xml, tmpnode)) return NULL;

    sourcestr = gnc_price_get_source(price);
    if (sourcestr && (strlen(sourcestr) != 0))
    {
        tmpnode = text_to_dom_tree("price:source", sourcestr);
        if (!add_child_or_kill_parent(price_xml, tmpnode)) return NULL;
    }

    typestr = gnc_price_get_typestr(price);
    if (typestr && (strlen(typestr) != 0))
    {
        tmpnode = text_to_dom_tree("price:type", typestr);
        if (!add_child_or_kill_parent(price_xml, tmpnode)) return NULL;
    }

    value = gnc_price_get_value(price);
    tmpnode = gnc_numeric_to_dom_tree("price:value", &value);
    if (!add_child_or_kill_parent(price_xml, tmpnode)) return NULL;

    return price_xml;
}

static gboolean
xml_add_gnc_price_adapter(GNCPrice *p, gpointer data)
{
    xmlNodePtr xml_node = (xmlNodePtr) data;

    if (p)
    {
        xmlNodePtr price_xml = gnc_price_to_dom_tree(BAD_CAST "price", p);
        if (!price_xml) return FALSE;
        xmlAddChild(xml_node, price_xml);
        return TRUE;
    }
    else
    {
        return TRUE;
    }
}

static xmlNodePtr
gnc_pricedb_to_dom_tree(const xmlChar *tag, GNCPriceDB *db)
{
    xmlNodePtr db_xml = NULL;

    if (!tag) return NULL;

    db_xml = xmlNewNode(NULL, tag);
    if (!db_xml) return NULL;

    xmlSetProp(db_xml, BAD_CAST "version", BAD_CAST "1");

    if (!gnc_pricedb_foreach_price(db, xml_add_gnc_price_adapter, db_xml, TRUE))
    {
        xmlFreeNode(db_xml);
        return NULL;
    }

    /* if no children have been added just return NULL */
    if (!db_xml->xmlChildrenNode)
    {
        xmlFreeNode(db_xml);
        return NULL;
    }

    return db_xml;
}

xmlNodePtr
gnc_pricedb_dom_tree_create(GNCPriceDB *db)
{
    return gnc_pricedb_to_dom_tree(BAD_CAST "gnc:pricedb", db);
}
