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
extern "C"
{
#define __EXTENSIONS__

#include <config.h>
#include <glib.h>

#include <gnc-date.h>
}

#include "gnc-xml-helper.h"
#include "sixtp-dom-generators.h"
#include "sixtp-utils.h"

#include <kvp-frame.hpp>
#include <gnc-datetime.hpp>

static QofLogModule log_module = GNC_MOD_IO;

xmlNodePtr
boolean_to_dom_tree (const char* tag, gboolean val)
{
    return text_to_dom_tree (tag, val ? "TRUE" : "FALSE");
}

xmlNodePtr
text_to_dom_tree (const char* tag, const char* str)
{
    xmlNodePtr result;
    gchar* newstr = g_strdup (str);
    g_return_val_if_fail (tag, NULL);
    g_return_val_if_fail (str, NULL);
    result = xmlNewNode (NULL, BAD_CAST tag);
    g_return_val_if_fail (result, NULL);
    xmlNodeAddContent (result, checked_char_cast (newstr));
    g_free (newstr);
    return result;
}

xmlNodePtr
int_to_dom_tree (const char* tag, gint64 val)
{
    gchar* text;
    xmlNodePtr result;

    text = g_strdup_printf ("%" G_GINT64_FORMAT, val);
    g_return_val_if_fail (text, NULL);
    result = text_to_dom_tree (tag, text);
    g_free (text);
    return result;
}

xmlNodePtr
guint_to_dom_tree (const char* tag, guint an_int)
{
    gchar* text;
    xmlNodePtr result;

    text = g_strdup_printf ("%u", an_int);
    g_return_val_if_fail (text, NULL);
    result = text_to_dom_tree (tag, text);
    g_free (text);
    return result;
}


xmlNodePtr
guid_to_dom_tree (const char* tag, const GncGUID* gid)
{
    char guid_str[GUID_ENCODING_LENGTH + 1];
    xmlNodePtr ret;

    ret = xmlNewNode (NULL, BAD_CAST tag);

    xmlSetProp (ret, BAD_CAST "type", BAD_CAST "guid");

    if (!guid_to_string_buff (gid, guid_str))
    {
        PERR ("guid_to_string_buff failed\n");
        return NULL;
    }

    xmlNodeAddContent (ret, BAD_CAST guid_str);

    return ret;
}

xmlNodePtr
commodity_ref_to_dom_tree (const char* tag, const gnc_commodity* c)
{
    xmlNodePtr ret;
    gchar* name_space, *mnemonic;

    g_return_val_if_fail (c, NULL);

    ret = xmlNewNode (NULL, BAD_CAST tag);

    if (!gnc_commodity_get_namespace (c) || !gnc_commodity_get_mnemonic (c))
    {
        return NULL;
    }
    name_space = g_strdup (gnc_commodity_get_namespace (c));
    mnemonic = g_strdup (gnc_commodity_get_mnemonic (c));
    xmlNewTextChild (ret, NULL, BAD_CAST "cmdty:space",
                     checked_char_cast (name_space));
    xmlNewTextChild (ret, NULL, BAD_CAST "cmdty:id",
                     checked_char_cast (mnemonic));
    g_free (name_space);
    g_free (mnemonic);
    return ret;
}

xmlNodePtr
time64_to_dom_tree (const char* tag, const time64 time)
{
    xmlNodePtr ret;
    g_return_val_if_fail (time != INT64_MAX, NULL);
    auto date_str = GncDateTime(time).format_iso8601();
    if (date_str.empty())
        return NULL;
    date_str += " +0000"; //Tack on a UTC offset to mollify GnuCash for Android
    ret = xmlNewNode (NULL, BAD_CAST tag);
    xmlNewTextChild (ret, NULL, BAD_CAST "ts:date",
                     checked_char_cast (const_cast<char*>(date_str.c_str())));
    return ret;
}

xmlNodePtr
gdate_to_dom_tree (const char* tag, const GDate* date)
{
    xmlNodePtr ret;
    gchar* date_str = NULL;

    g_return_val_if_fail (date, NULL);
    date_str = g_new (gchar, 512);

    g_date_strftime (date_str, 512, "%Y-%m-%d", date);

    ret = xmlNewNode (NULL, BAD_CAST tag);

    xmlNewTextChild (ret, NULL, BAD_CAST "gdate", checked_char_cast (date_str));

    g_free (date_str);

    return ret;
}

xmlNodePtr
gnc_numeric_to_dom_tree (const char* tag, const gnc_numeric* num)
{
    xmlNodePtr ret;
    gchar* numstr;

    g_return_val_if_fail (num, NULL);

    numstr = gnc_numeric_to_string (*num);
    g_return_val_if_fail (numstr, NULL);

    ret = xmlNewNode (NULL, BAD_CAST tag);

    xmlNodeAddContent (ret, checked_char_cast (numstr));

    g_free (numstr);

    return ret;
}

gchar*
double_to_string (double value)
{
    gchar* numstr;
    numstr = g_strdup_printf ("%24.18g", value);

    if (!numstr)
    {
        return NULL;

    }
    else
    {
        return g_strstrip (numstr);
    }
}

static void
add_text_to_node (xmlNodePtr node, const gchar* type, gchar* val)
{
    gchar* newtype = g_strdup (type);
    gchar* newval = g_strdup (val);
    xmlSetProp (node, BAD_CAST "type", BAD_CAST type);
    xmlNodeSetContent (node, checked_char_cast (val));
    g_free (newtype);
    g_free (newval);
}

static void add_kvp_slot (const char* key, KvpValue* value, void* data);

static void
add_kvp_value_node (xmlNodePtr node, const gchar* tag, KvpValue* val)
{
    xmlNodePtr val_node;

    switch (val->get_type ())
    {
    case KvpValue::Type::STRING:
    {
        auto newstr = g_strdup (val->get<const char*> ());
        val_node = xmlNewTextChild (node, NULL, BAD_CAST tag,
                                    checked_char_cast (newstr));
        g_free (newstr);
        break;
    }
    case KvpValue::Type::TIME64:
        val_node = NULL;
        break;
    case KvpValue::Type::GDATE:
    {
        auto d = val->get<GDate> ();
        val_node = gdate_to_dom_tree (tag, &d);
        xmlAddChild (node, val_node);
        break;
    }
    default:
        val_node = xmlNewTextChild (node, NULL, BAD_CAST tag, NULL);
        break;
    }

    switch (val->get_type ())
    {
    case KvpValue::Type::INT64:
        add_text_to_node (val_node, "integer",
                          g_strdup_printf ("%" G_GINT64_FORMAT,
                                           val->get<int64_t> ()));
        break;
    case KvpValue::Type::DOUBLE:
        add_text_to_node (val_node, "double",
                          double_to_string (val->get<double> ()));
        break;
    case KvpValue::Type::NUMERIC:
        add_text_to_node (val_node, "numeric",
                          gnc_numeric_to_string (val->get<gnc_numeric> ()));
        break;
    case KvpValue::Type::STRING:
        xmlSetProp (val_node, BAD_CAST "type", BAD_CAST "string");
        break;
    case KvpValue::Type::GUID:
    {
        gchar guidstr[GUID_ENCODING_LENGTH + 1];
        guid_to_string_buff (val->get<GncGUID*> (), guidstr);
        add_text_to_node (val_node, "guid", guidstr);
        break;
    }
    /* Note: The type attribute must remain 'timespec' to maintain
     * compatibility.
     */
    case KvpValue::Type::TIME64:
    {
        auto t = val->get<Time64> ();
        val_node = time64_to_dom_tree (tag, t.t);
        xmlSetProp (val_node, BAD_CAST "type", BAD_CAST "timespec");
        xmlAddChild (node, val_node);
        break;
    }
    case KvpValue::Type::GDATE:
        xmlSetProp (val_node, BAD_CAST "type", BAD_CAST "gdate");
        break;
    case KvpValue::Type::GLIST:
        xmlSetProp (val_node, BAD_CAST "type", BAD_CAST "list");
        for (auto cursor = val->get<GList*> (); cursor; cursor = cursor->next)
        {
            auto val = static_cast<KvpValue*> (cursor->data);
            add_kvp_value_node (val_node, "slot:value", val);
        }
        break;
    case KvpValue::Type::FRAME:
    {
        xmlSetProp (val_node, BAD_CAST "type", BAD_CAST "frame");

        auto frame = val->get<KvpFrame*> ();
        if (!frame)
            break;
        frame->for_each_slot_temp (&add_kvp_slot, val_node);
        break;
    }
    default:
        break;
    }
}

static void
add_kvp_slot (const char* key, KvpValue* value, void* data)
{
    auto newkey = g_strdup ((gchar*)key);
    auto node = static_cast<xmlNodePtr> (data);
    auto slot_node = xmlNewChild (node, NULL, BAD_CAST "slot", NULL);

    xmlNewTextChild (slot_node, NULL, BAD_CAST "slot:key",
                     checked_char_cast (newkey));
    g_free (newkey);
    add_kvp_value_node (slot_node, "slot:value", value);
}

xmlNodePtr
qof_instance_slots_to_dom_tree (const char* tag, const QofInstance* inst)
{
    xmlNodePtr ret;
    const char** keys;
    unsigned int i;
    KvpFrame* frame = qof_instance_get_slots (inst);
    if (!frame || frame->empty())
        return nullptr;

    ret = xmlNewNode (nullptr, BAD_CAST tag);
    frame->for_each_slot_temp (&add_kvp_slot, ret);
    return ret;
}
