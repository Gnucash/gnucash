/********************************************************************\
 * gnc-commodity-xml-v2.c -- commodity xml i/o implementation       *
 *                                                                  *
 * Copyright (C) 2001 James LewisMoss <dres@debian.org>             *
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

#include "sixtp-dom-parsers.h"
#include "AccountP.h"
#include "Account.h"

static QofLogModule log_module = GNC_MOD_IO;

const gchar *commodity_version_string = "2.0.0";

/* ids */
#define gnc_commodity_string "gnc:commodity"
#define cmdty_namespace      "cmdty:space"
#define cmdty_id             "cmdty:id"
#define cmdty_name           "cmdty:name"
#define cmdty_xcode          "cmdty:xcode"
#define cmdty_fraction       "cmdty:fraction"
#define cmdty_get_quotes     "cmdty:get_quotes"
#define cmdty_quote_source   "cmdty:quote_source"
#define cmdty_quote_tz       "cmdty:quote_tz"
#define cmdty_slots          "cmdty:slots"

xmlNodePtr
gnc_commodity_dom_tree_create(const gnc_commodity *com)
{
    gnc_quote_source *source;
    const char *string;
    xmlNodePtr ret;
    gboolean currency = gnc_commodity_is_iso(com);
    xmlNodePtr kvpnode =
        kvp_frame_to_dom_tree(cmdty_slots,
                              qof_instance_get_slots(QOF_INSTANCE(com)));

    if (currency && !gnc_commodity_get_quote_flag(com) && !kvpnode)
        return NULL;

    ret = xmlNewNode(NULL, BAD_CAST gnc_commodity_string);

    xmlSetProp(ret, BAD_CAST "version", BAD_CAST commodity_version_string);

    xmlAddChild(ret, text_to_dom_tree(cmdty_namespace,
                                      gnc_commodity_get_namespace_compat(com)));
    xmlAddChild(ret, text_to_dom_tree(cmdty_id,
                                      gnc_commodity_get_mnemonic(com)));

    if (!currency)
    {
        if (gnc_commodity_get_fullname(com))
        {
            xmlAddChild(ret, text_to_dom_tree(cmdty_name,
                                              gnc_commodity_get_fullname(com)));
        }

        if (gnc_commodity_get_cusip(com) &&
                strlen(gnc_commodity_get_cusip(com)) > 0)
        {
            xmlAddChild(ret, text_to_dom_tree(
                            cmdty_xcode,
                            gnc_commodity_get_cusip(com)));
        }

        xmlAddChild(ret, int_to_dom_tree(cmdty_fraction,
                                         gnc_commodity_get_fraction(com)));
    }

    if (gnc_commodity_get_quote_flag(com))
    {
        xmlNewChild(ret, NULL, BAD_CAST cmdty_get_quotes, NULL);
        source = gnc_commodity_get_quote_source(com);
        if (source)
            xmlAddChild(ret, text_to_dom_tree(cmdty_quote_source,
                                              gnc_quote_source_get_internal_name(source)));
        string = gnc_commodity_get_quote_tz(com);
        if (string)
            xmlAddChild(ret, text_to_dom_tree(cmdty_quote_tz, string));
    }

    if (kvpnode)
        xmlAddChild(ret, kvpnode);

    return ret;
}

/***********************************************************************/

struct com_char_handler
{
    gchar *tag;
    void(*func)(gnc_commodity *com, const char*val);
};

struct com_char_handler com_handlers[] =
{
    { cmdty_namespace,    gnc_commodity_set_namespace },
    { cmdty_id,           gnc_commodity_set_mnemonic },
    { cmdty_name,         gnc_commodity_set_fullname },
    { cmdty_xcode,        gnc_commodity_set_cusip },
    { cmdty_quote_tz,     gnc_commodity_set_quote_tz },
    { 0, 0 }
};

static void
set_commodity_value(xmlNodePtr node, gnc_commodity* com)
{
    if (safe_strcmp((char*) node->name, cmdty_fraction) == 0)
    {
        gint64 val;
        char *string;

        string = (char*) xmlNodeGetContent (node->xmlChildrenNode);
        if (string_to_gint64(string, &val))
        {
            gnc_commodity_set_fraction(com, val);
        }
        xmlFree (string);
    }
    else if (safe_strcmp((char*)node->name, cmdty_get_quotes) == 0)
    {
        gnc_commodity_set_quote_flag(com, TRUE);
    }
    else if (safe_strcmp((char*)node->name, cmdty_quote_source) == 0)
    {
        gnc_quote_source *source;
        char *string;

        string = (char*) xmlNodeGetContent (node->xmlChildrenNode);
        source = gnc_quote_source_lookup_by_internal(string);
        if (!source)
            source = gnc_quote_source_add_new(string, FALSE);
        gnc_commodity_set_quote_source(com, source);
        xmlFree (string);
    }
    else if (safe_strcmp((char*)node->name, cmdty_slots) == 0)
    {
        /* We ignore the results here */
        dom_tree_to_kvp_frame_given(node,
                                    qof_instance_get_slots(QOF_INSTANCE(com)));
    }
    else
    {
        struct com_char_handler *mark;

        for (mark = com_handlers; mark->tag; mark++)
        {
            if (safe_strcmp(mark->tag, (char*)node->name) == 0)
            {
                gchar* val = dom_tree_to_text(node);
                g_strstrip(val);
                (mark->func)(com, val);
                g_free(val);
                break;
            }
        }
    }
}

static gboolean
valid_commodity(gnc_commodity *com)
{
    if (gnc_commodity_get_namespace(com) == NULL)
    {
        PWARN("Invalid commodity: no namespace");
        return FALSE;
    }
    if (gnc_commodity_get_mnemonic(com) == NULL)
    {
        PWARN("Invalid commodity: no mnemonic");
        return FALSE;
    }
    if (gnc_commodity_get_fraction(com) == 0)
    {
        PWARN("Invalid commodity: 0 fraction");
        return FALSE;
    }
    return TRUE;
}

static gnc_commodity *
gnc_commodity_find_currency (QofBook *book, xmlNodePtr tree)
{
    gnc_commodity_table * table;
    gnc_commodity *currency = NULL;
    gchar *exchange = NULL, *mnemonic = NULL;
    xmlNodePtr node;

    for (node = tree->xmlChildrenNode; node; node = node->next)
    {
        if (safe_strcmp((char*) node->name, cmdty_namespace) == 0)
            exchange = (gchar*) xmlNodeGetContent (node->xmlChildrenNode);
        if (safe_strcmp((char*) node->name, cmdty_id) == 0)
            mnemonic = (gchar*) xmlNodeGetContent (node->xmlChildrenNode);
    }

    if (exchange
            && gnc_commodity_namespace_is_iso(exchange)
            && mnemonic)
    {
        table = gnc_commodity_table_get_table(book);
        currency = gnc_commodity_table_lookup(table, exchange, mnemonic);
    }

    if (exchange)
        xmlFree(exchange);
    if (mnemonic)
        xmlFree(mnemonic);

    return currency;
}

static gboolean
gnc_commodity_end_handler(gpointer data_for_children,
                          GSList* data_from_children, GSList* sibling_data,
                          gpointer parent_data, gpointer global_data,
                          gpointer *result, const gchar *tag)
{
    gnc_commodity *com, *old_com;
    xmlNodePtr achild;
    xmlNodePtr tree = (xmlNodePtr)data_for_children;
    gxpf_data *gdata = (gxpf_data*)global_data;
    QofBook *book = gdata->bookdata;

    if (parent_data)
    {
        return TRUE;
    }

    /* OK.  For some messed up reason this is getting called again with a
       NULL tag.  So we ignore those cases */
    if (!tag)
    {
        return TRUE;
    }

    g_return_val_if_fail(tree, FALSE);

    com = gnc_commodity_new(book, NULL, NULL, NULL, NULL, 0);
    old_com = gnc_commodity_find_currency(book, tree);
    if (old_com)
        gnc_commodity_copy(com, old_com);

    for (achild = tree->xmlChildrenNode; achild; achild = achild->next)
    {
        set_commodity_value(achild, com);
    }

    if (!valid_commodity(com))
    {
        PWARN("Invalid commodity parsed");
        xmlElemDump(stdout, NULL, tree);
        printf("\n");
        fflush(stdout);
        gnc_commodity_destroy(com);
        return FALSE;
    }

    gdata->cb(tag, gdata->parsedata, com);

    xmlFreeNode(tree);

    return TRUE;
}


sixtp*
gnc_commodity_sixtp_parser_create(void)
{
    return sixtp_dom_parser_new(gnc_commodity_end_handler, NULL, NULL);
}
