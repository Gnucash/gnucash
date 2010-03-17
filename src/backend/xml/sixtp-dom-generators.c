/********************************************************************
 * sixtp-dom-generators.c                                           *
 * Copyright 2001 Gnumatic, Inc.                                    *
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
 ********************************************************************/

#define __EXTENSIONS__

#include "config.h"
#include <glib.h>

#include "gnc-xml-helper.h"

#include "sixtp-dom-generators.h"
#include "sixtp-utils.h"

static QofLogModule log_module = GNC_MOD_IO;

xmlNodePtr
boolean_to_dom_tree(const char* tag, gboolean val)
{
    return text_to_dom_tree(tag, val ? "TRUE" : "FALSE");
}

xmlNodePtr
text_to_dom_tree(const char *tag, const char *str)
{
    xmlNodePtr result;

    g_return_val_if_fail(tag, NULL);
    g_return_val_if_fail(str, NULL);
    result = xmlNewNode(NULL, BAD_CAST tag);
    g_return_val_if_fail(result, NULL);
    xmlNodeAddContent(result, BAD_CAST str);
    return result;
}

xmlNodePtr
int_to_dom_tree(const char *tag, gint64 val)
{
    gchar *text;
    xmlNodePtr result;

    text = g_strdup_printf("%" G_GINT64_FORMAT, val);
    g_return_val_if_fail(text, NULL);
    result = text_to_dom_tree(tag, text);
    g_free(text);
    return result;
}

xmlNodePtr
guint_to_dom_tree(const char *tag, guint an_int)
{
    gchar *text;
    xmlNodePtr result;

    text = g_strdup_printf("%u", an_int );
    g_return_val_if_fail(text, NULL);
    result = text_to_dom_tree(tag, text);
    g_free(text);
    return result;
}


xmlNodePtr
guid_to_dom_tree(const char *tag, const GUID* gid)
{
    char guid_str[GUID_ENCODING_LENGTH + 1];
    xmlNodePtr ret;

    ret = xmlNewNode(NULL, BAD_CAST tag);

    xmlSetProp(ret, BAD_CAST "type", BAD_CAST "guid");

    if (!guid_to_string_buff(gid, guid_str))
    {
        PERR("guid_to_string_buff failed\n");
        return NULL;
    }

    xmlNodeAddContent(ret, BAD_CAST guid_str);

    return ret;
}

xmlNodePtr
commodity_ref_to_dom_tree(const char *tag, const gnc_commodity *c)
{
    xmlNodePtr ret;

    g_return_val_if_fail(c, NULL);

    ret = xmlNewNode(NULL, BAD_CAST tag);

    if (!gnc_commodity_get_namespace(c) || !gnc_commodity_get_mnemonic(c))
    {
        return NULL;
    }

    xmlNewTextChild(ret, NULL, BAD_CAST "cmdty:space", BAD_CAST gnc_commodity_get_namespace_compat(c));
    xmlNewTextChild(ret, NULL, BAD_CAST "cmdty:id", BAD_CAST gnc_commodity_get_mnemonic(c));

    return ret;
}

gchar *
timespec_sec_to_string(const Timespec *ts)
{
    gchar *ret;

    ret = g_new(gchar, TIMESPEC_SEC_FORMAT_MAX);

    if (!timespec_secs_to_given_string (ts, ret))
    {
        g_free(ret);
        return NULL;
    }

    return ret;
}

gchar *
timespec_nsec_to_string(const Timespec *ts)
{
    return g_strdup_printf("%ld", ts->tv_nsec);
}

xmlNodePtr
timespec_to_dom_tree(const char *tag, const Timespec *spec)
{
    xmlNodePtr ret;
    gchar *date_str = NULL;
    gchar *ns_str = NULL;

    g_return_val_if_fail(spec, NULL);

    date_str = timespec_sec_to_string(spec);

    if (!date_str)
    {
        return NULL;
    }

    ret = xmlNewNode(NULL, BAD_CAST tag);

    xmlNewTextChild(ret, NULL, BAD_CAST "ts:date", BAD_CAST date_str);

    if (spec->tv_nsec > 0)
    {
        ns_str = timespec_nsec_to_string(spec);
        if (ns_str)
        {
            xmlNewTextChild(ret, NULL, BAD_CAST "ts:ns", BAD_CAST ns_str);
        }
    }

    g_free(date_str);
    if (ns_str)
    {
        g_free(ns_str);
    }

    return ret;
}

xmlNodePtr
gdate_to_dom_tree(const char *tag, const GDate *date)
{
    xmlNodePtr ret;
    gchar *date_str = NULL;

    g_return_val_if_fail(date, NULL);
    date_str = g_new( gchar, 512 );

    g_date_strftime( date_str, 512, "%Y-%m-%d", date );

    ret = xmlNewNode(NULL, BAD_CAST tag);

    xmlNewTextChild(ret, NULL, BAD_CAST "gdate", BAD_CAST date_str);

    g_free(date_str);

    return ret;
}

xmlNodePtr
gnc_numeric_to_dom_tree(const char *tag, const gnc_numeric *num)
{
    xmlNodePtr ret;
    gchar *numstr;

    g_return_val_if_fail(num, NULL);

    numstr = gnc_numeric_to_string(*num);
    g_return_val_if_fail(numstr, NULL);

    ret = xmlNewNode(NULL, BAD_CAST tag);

    xmlNodeAddContent(ret, BAD_CAST numstr);

    g_free(numstr);

    return ret;
}

gchar*
double_to_string(double value)
{
    gchar *numstr;
#ifdef USE_GUILE_FOR_DOUBLE_CONVERSION
    numstr = gh_scm2newstr(scm_call_1(scm_c_eval_string("number->string"),
                                      scm_make_real(value)),
                           NULL);

#else /* don't USE_GUILE_FOR_DOUBLE_CONVERSION */
    /*
     * we're just going to use plain-old libc for the double conversion.
     * There was some question as to whether libc is accurate enough
     * in its printf function for doubles, but I don't understand
     * how it couldn't be ...
     */
    numstr = g_strdup_printf ("%24.18g", value);

#endif /* USE_GUILE_FOR_DOUBLE_CONVERSION */
    if (!numstr)
    {
        return NULL;

    }
    else
    {
        return g_strstrip(numstr);
    }
}

static void
add_text_to_node(xmlNodePtr node, gchar *type, gchar *val)
{
    xmlSetProp(node, BAD_CAST "type", BAD_CAST type);
    xmlNodeSetContent(node, BAD_CAST val);
    g_free(val);
}



static void
add_kvp_slot(gpointer key, gpointer value, gpointer data);

static void
add_kvp_value_node(xmlNodePtr node, gchar *tag, kvp_value* val)
{
    xmlNodePtr val_node;
    gchar *tmp_str1;
    kvp_value_t kvp_type;

    kvp_type = kvp_value_get_type(val);

    if (kvp_type == KVP_TYPE_STRING)
        val_node = xmlNewTextChild(node, NULL, BAD_CAST tag, BAD_CAST kvp_value_get_string(val));
    else if (kvp_type == KVP_TYPE_TIMESPEC)
        val_node = NULL;
    else if (kvp_type == KVP_TYPE_GDATE)
    {
        GDate d = kvp_value_get_gdate(val);
        val_node = gdate_to_dom_tree(tag, &d);
        xmlAddChild (node, val_node);
    }
    else
        val_node = xmlNewTextChild(node, NULL, BAD_CAST tag, NULL);

    switch (kvp_value_get_type(val))
    {
    case KVP_TYPE_GINT64:
        add_text_to_node(val_node, "integer",
                         g_strdup_printf("%" G_GINT64_FORMAT,
                                         kvp_value_get_gint64(val)));
        break;
    case KVP_TYPE_DOUBLE:
        add_text_to_node(val_node, "double",
                         double_to_string(kvp_value_get_double(val)));
        break;
    case KVP_TYPE_NUMERIC:
        add_text_to_node(val_node, "numeric",
                         gnc_numeric_to_string(kvp_value_get_numeric(val)));
        break;
    case KVP_TYPE_STRING:
        xmlSetProp(val_node, BAD_CAST "type", BAD_CAST "string");
        break;
    case KVP_TYPE_GUID:
        /* THREAD-UNSAFE */
        add_text_to_node(val_node, "guid",
                         g_strdup(guid_to_string(kvp_value_get_guid(val))));
        break;
    case KVP_TYPE_TIMESPEC:
    {
        Timespec ts = kvp_value_get_timespec (val);

        val_node = timespec_to_dom_tree (tag, &ts);
        xmlSetProp (val_node, BAD_CAST "type", BAD_CAST "timespec");
        xmlAddChild (node, val_node);
    }
    break;
    case KVP_TYPE_GDATE:
        xmlSetProp(val_node, BAD_CAST "type", BAD_CAST "gdate");
        break;
    case KVP_TYPE_BINARY:
    {
        guint64 size;
        void *binary_data = kvp_value_get_binary(val, &size);
        xmlSetProp(val_node, BAD_CAST "type", BAD_CAST "binary");
        g_return_if_fail(binary_data);
        tmp_str1 = binary_to_string(binary_data, size);
        xmlNodeSetContent(val_node, BAD_CAST tmp_str1);
        g_free(tmp_str1);
    }
    break;
    case KVP_TYPE_GLIST:
    {
        GList *cursor;

        xmlSetProp(val_node, BAD_CAST "type", BAD_CAST "list");
        for (cursor = kvp_value_get_glist(val); cursor; cursor = cursor->next)
        {
            kvp_value *val = (kvp_value*)cursor->data;
            add_kvp_value_node(val_node, "slot:value", val);
        }
    }

    break;
    case KVP_TYPE_FRAME:
    {
        kvp_frame *frame;

        xmlSetProp(val_node, BAD_CAST "type", BAD_CAST "frame");

        frame = kvp_value_get_frame (val);
        if (!frame || !kvp_frame_get_hash (frame))
            break;

        g_hash_table_foreach(kvp_frame_get_hash(frame),
                             add_kvp_slot, val_node);
    }
    break;

    }
}

static void
add_kvp_slot(gpointer key, gpointer value, gpointer data)
{
    xmlNodePtr slot_node;
    xmlNodePtr node = (xmlNodePtr)data;

    slot_node = xmlNewChild(node, NULL, BAD_CAST "slot", NULL);

    xmlNewTextChild(slot_node, NULL, BAD_CAST "slot:key", (xmlChar*)key);

    add_kvp_value_node(slot_node, "slot:value", (kvp_value*)value);
}

xmlNodePtr
kvp_frame_to_dom_tree(const char *tag, const kvp_frame *frame)
{
    xmlNodePtr ret;

    if (!frame)
    {
        return NULL;
    }

    if (!kvp_frame_get_hash(frame))
    {
        return NULL;
    }

    if (g_hash_table_size(kvp_frame_get_hash(frame)) == 0)
    {
        return NULL;
    }

    ret = xmlNewNode(NULL, BAD_CAST tag);

    g_hash_table_foreach(kvp_frame_get_hash(frame), add_kvp_slot, ret);

    return ret;
}

