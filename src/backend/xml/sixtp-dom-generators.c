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
#include <gnc-date.h>

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
    gchar *newstr = g_strdup (str);
    g_return_val_if_fail(tag, NULL);
    g_return_val_if_fail(str, NULL);
    result = xmlNewNode(NULL, BAD_CAST tag);
    g_return_val_if_fail(result, NULL);
    xmlNodeAddContent(result, checked_char_cast (newstr));
    g_free (newstr);
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
guid_to_dom_tree(const char *tag, const GncGUID* gid)
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
    gchar *name_space, *mnemonic;

    g_return_val_if_fail(c, NULL);

    ret = xmlNewNode(NULL, BAD_CAST tag);

    if (!gnc_commodity_get_namespace(c) || !gnc_commodity_get_mnemonic(c))
    {
        return NULL;
    }
    name_space = g_strdup (gnc_commodity_get_namespace_compat(c));
    mnemonic = g_strdup (gnc_commodity_get_mnemonic(c));
    xmlNewTextChild(ret, NULL, BAD_CAST "cmdty:space",
		    checked_char_cast (name_space));
    xmlNewTextChild(ret, NULL, BAD_CAST "cmdty:id",
		    checked_char_cast (mnemonic));
    g_free (name_space);
    g_free (mnemonic);
    return ret;
}

/* gnc_g_date_time_new_from_timespec_local normalizes the timespec,
 * but we want to serialize it un-normalized, so we make a partial
 * copy.
 */
gchar *
timespec_sec_to_string(const Timespec *ts)
{
     gchar *time_string;
     GDateTime *gdt;
     Timespec sts = { ts->tv_sec, 0};
     gdt = gnc_g_date_time_new_from_timespec_local (sts);
     g_return_val_if_fail (gdt != NULL, NULL);
     time_string = g_date_time_format (gdt, "%Y-%m-%d %H:%M:%S %z");
     g_date_time_unref (gdt);
     return time_string;
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

    xmlNewTextChild(ret, NULL, BAD_CAST "ts:date",
		    checked_char_cast (date_str));

    if (spec->tv_nsec > 0)
    {
        ns_str = timespec_nsec_to_string(spec);
        if (ns_str)
        {
            xmlNewTextChild(ret, NULL, BAD_CAST "ts:ns",
			    checked_char_cast (ns_str));
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

    xmlNewTextChild(ret, NULL, BAD_CAST "gdate", checked_char_cast (date_str));

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

    xmlNodeAddContent(ret, checked_char_cast (numstr));

    g_free(numstr);

    return ret;
}

gchar*
double_to_string(double value)
{
    gchar *numstr;
    numstr = g_strdup_printf ("%24.18g", value);

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
    gchar *newtype = g_strdup (type);
    gchar *newval = g_strdup (val);
    xmlSetProp(node, BAD_CAST "type", checked_char_cast (type));
    xmlNodeSetContent(node, checked_char_cast (val));
    g_free (newtype);
    g_free(newval);
}



static void
add_kvp_slot(gpointer key, gpointer value, gpointer data);

static void
add_kvp_value_node(xmlNodePtr node, gchar *tag, KvpValue* val)
{
    xmlNodePtr val_node;
    kvp_value_t kvp_type;

    kvp_type = kvp_value_get_type(val);

    if (kvp_type == KVP_TYPE_STRING)
    {
	gchar *newstr = g_strdup (kvp_value_get_string(val));
        val_node = xmlNewTextChild(node, NULL, BAD_CAST tag,
				   checked_char_cast (newstr));
	g_free (newstr);
    }
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
    {
        gchar guidstr[GUID_ENCODING_LENGTH+1];
        guid_to_string_buff(kvp_value_get_guid(val), guidstr);
        add_text_to_node(val_node, "guid", guidstr);
        break;
    }
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
    case KVP_TYPE_GLIST:
    {
        GList *cursor;

        xmlSetProp(val_node, BAD_CAST "type", BAD_CAST "list");
        for (cursor = kvp_value_get_glist(val); cursor; cursor = cursor->next)
        {
            KvpValue *val = (KvpValue*)cursor->data;
            add_kvp_value_node(val_node, "slot:value", val);
        }
    }

    break;
    case KVP_TYPE_FRAME:
    {
        KvpFrame *frame;

        xmlSetProp(val_node, BAD_CAST "type", BAD_CAST "frame");

        frame = kvp_value_get_frame (val);
        if (!frame || !kvp_frame_get_hash (frame))
            break;

        g_hash_table_foreach_sorted(kvp_frame_get_hash(frame),
                                    add_kvp_slot, val_node, (GCompareFunc)strcmp);
    }
    break;
    default:
	break;
    }
}

static void
add_kvp_slot(gpointer key, gpointer value, gpointer data)
{
    xmlNodePtr slot_node;
    xmlNodePtr node = (xmlNodePtr)data;
    gchar *newkey = g_strdup ((gchar*)key);
    slot_node = xmlNewChild(node, NULL, BAD_CAST "slot", NULL);

    xmlNewTextChild(slot_node, NULL, BAD_CAST "slot:key",
		    checked_char_cast (newkey));
    g_free (newkey);
    add_kvp_value_node(slot_node, "slot:value", (KvpValue*)value);
}

xmlNodePtr
kvp_frame_to_dom_tree(const char *tag, const KvpFrame *frame)
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

    g_hash_table_foreach_sorted(kvp_frame_get_hash(frame),
                                add_kvp_slot, ret, (GCompareFunc)strcmp);

    return ret;
}

