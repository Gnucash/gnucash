/********************************************************************
 * sixtp-dom-parsers.c                                              *
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
#include <glib.h>

#include <config.h>

#include <string.h>

#include <gnc-engine.h>

#include "gnc-xml-helper.h"
#include "sixtp-utils.h"
#include "sixtp-dom-parsers.h"
#include <kvp-frame.hpp>

static QofLogModule log_module = GNC_MOD_IO;

const char*
dom_node_to_text (xmlNodePtr node) noexcept
{
    if (node && node->children && node->children->type == XML_TEXT_NODE
        && !node->children->next)
        return reinterpret_cast<const char*>(node->children->content);
    return nullptr;
}

std::optional<GncGUID>
dom_tree_to_guid (xmlNodePtr node)
{
    auto type = xmlGetProp (node, BAD_CAST "type");
    if (!type)
        return {};

    bool ok = !g_strcmp0 ((char*)type, "guid") || !g_strcmp0 ((char*)type, "new");

    xmlFree (type);

    if (!ok)
        return {};

    auto extract_guid = [](auto str) -> std::optional<GncGUID>
    {
        if (GncGUID guid; string_to_guid (str, &guid))
            return guid;

        return {};
    };

    return apply_xmlnode_text<std::optional<GncGUID>>(extract_guid, node);
}

static KvpValue*
dom_tree_to_integer_kvp_value (xmlNodePtr node)
{
    auto node_to_int_kvp = [](auto txt) -> KvpValue*
    {
        if (gint64 daint; string_to_gint64 (txt, &daint))
            return new KvpValue{daint};

        return nullptr;
    };
    return apply_xmlnode_text<KvpValue*> (node_to_int_kvp, node, nullptr);
}

template <typename T>
static bool
dom_tree_to_num (xmlNodePtr node, std::function<bool(const char*, T*)>string_to_num, T* num_ptr)
{
    return apply_xmlnode_text<T>([&](auto txt){ return string_to_num (txt, num_ptr);}, node, false);
}

gboolean
dom_tree_to_integer (xmlNodePtr node, gint64* daint)
{
    return dom_tree_to_num<gint64>(node, string_to_gint64, daint);
}

gboolean
dom_tree_to_guint16 (xmlNodePtr node, guint16* i)
{
    return dom_tree_to_num<guint16>(node, string_to_guint16, i);
}

gboolean
dom_tree_to_guint (xmlNodePtr node, guint* i)
{
    return dom_tree_to_num<guint>(node, string_to_guint, i);
}

gboolean
dom_tree_to_boolean (xmlNodePtr node, gboolean* b)
{
    auto set_bool = [b](auto text) -> gboolean
    {
        if (g_ascii_strncasecmp (text, "true", 4) == 0)
        {
            *b = TRUE;
            return TRUE;
        }
        else if (g_ascii_strncasecmp (text, "false", 5) == 0)
        {
            *b = FALSE;
            return TRUE;
        }
        else
        {
            *b = FALSE;
            return FALSE;
        }
    };
    return apply_xmlnode_text<gboolean> (set_bool, node);
}

static KvpValue*
dom_tree_to_double_kvp_value (xmlNodePtr node)
{
    auto node_to_double_kvp = [](auto txt) -> KvpValue*
    {
        if (double dadoub; string_to_double (txt, &dadoub)) return new KvpValue{dadoub};
        return nullptr;
    };
    return apply_xmlnode_text<KvpValue*> (node_to_double_kvp, node, nullptr);
}

static KvpValue*
dom_tree_to_numeric_kvp_value (xmlNodePtr node)
{
    return new KvpValue {dom_tree_to_gnc_numeric (node)};
}

static KvpValue*
dom_tree_to_string_kvp_value (xmlNodePtr node)
{
    auto node_to_string_kvp = [](auto txt) -> KvpValue*
    {
        return new KvpValue {g_strdup (txt)};
    };
    return apply_xmlnode_text<KvpValue*> (node_to_string_kvp, node, nullptr);
}

static KvpValue*
dom_tree_to_guid_kvp_value (xmlNodePtr node)
{
    auto daguid = dom_tree_to_guid (node);
    return daguid ? new KvpValue {guid_copy (&*daguid)} : nullptr;
}

static KvpValue*
dom_tree_to_time64_kvp_value (xmlNodePtr node)
{
    Time64 t{dom_tree_to_time64 (node)};
    return new KvpValue {t};
}

static KvpValue*
dom_tree_to_gdate_kvp_value (xmlNodePtr node)
{
    auto date = dom_tree_to_gdate (node);
    if (!date) return nullptr;
    auto rv{new KvpValue {*date}};
    g_date_free (date);
    return rv;
}

gboolean
string_to_binary (const gchar* str,  void** v, guint64* data_len)
{
    guint64 str_len;
    guchar* data;
    unsigned int i, j;

    g_return_val_if_fail (v != NULL, FALSE);
    g_return_val_if_fail (data_len != NULL, FALSE);

    str_len = strlen (str);

    /* Since no whitespace is allowed and hex encoding is 2 text chars
       per binary char, the result must be half the input size and the
       input size must be even. */
    if ((str_len % 2) != 0)
        return (FALSE);
    *data_len = str_len / 2;
    data = g_new0 (guchar, *data_len);

    for (j = 0, i = 0; i < str_len; i += 2, j++)
    {
        gchar tmpstr[3];
        long int converted;

        tmpstr[0] = str[i];
        tmpstr[1] = str[i + 1];
        tmpstr[2] = '\0';

        converted = strtol (tmpstr, NULL, 16);

        data[j] = (unsigned char)converted;
    }

    *v = data;

    return (TRUE);
}

static KvpValue* dom_tree_to_kvp_value (xmlNodePtr node);
//needed for test access as well as internal use.
KvpFrame* dom_tree_to_kvp_frame (xmlNodePtr node);

static KvpValue*
dom_tree_to_list_kvp_value (xmlNodePtr node)
{
    GList* list = NULL;
    xmlNodePtr mark;
    KvpValue* ret = NULL;

    for (mark = node->xmlChildrenNode; mark; mark = mark->next)
    {
        KvpValue* new_val;

        if (g_strcmp0 ((char*)mark->name, "text") == 0)
            continue;

        new_val = dom_tree_to_kvp_value (mark);
        if (new_val)
        {
            list = g_list_prepend (list, (gpointer)new_val);
        }
    }

    list = g_list_reverse (list);

    ret = new KvpValue {list};

    return ret;
}

static KvpValue*
dom_tree_to_frame_kvp_value (xmlNodePtr node)
{
    KvpFrame* frame = dom_tree_to_kvp_frame (node);
    return frame ? new KvpValue {frame} : nullptr;
}


struct kvp_val_converter
{
    const gchar* tag;
    KvpValue* (*converter) (xmlNodePtr node);
};
/* Note: The type attribute must remain 'timespec' to maintain compatibility.
 */

struct kvp_val_converter val_converters[] =
{
    { "integer", dom_tree_to_integer_kvp_value },
    { "double", dom_tree_to_double_kvp_value },
    { "numeric", dom_tree_to_numeric_kvp_value },
    { "string", dom_tree_to_string_kvp_value },
    { "guid", dom_tree_to_guid_kvp_value },
    { "timespec", dom_tree_to_time64_kvp_value },
    { "gdate", dom_tree_to_gdate_kvp_value },
    { "list", dom_tree_to_list_kvp_value },
    { "frame", dom_tree_to_frame_kvp_value },
    { 0, 0 },
};

static KvpValue*
dom_tree_to_kvp_value (xmlNodePtr node)
{
    xmlChar* xml_type;
    struct kvp_val_converter* mark;
    KvpValue* ret = NULL;

    xml_type = xmlGetProp (node, BAD_CAST "type");

    for (mark = val_converters; mark->tag; mark++)
    {
        if (g_strcmp0 (reinterpret_cast<char*>(xml_type), mark->tag) == 0)
        {
            ret = (mark->converter) (node);
        }
    }

    if (!mark->tag)
    {
        /* FIXME: deal with unknown type tag here */
    }

    xmlFree (xml_type);

    return ret;
}

static gboolean
dom_tree_to_kvp_frame_given (xmlNodePtr node, KvpFrame* frame)
{
    xmlNodePtr mark;

    g_return_val_if_fail (node, FALSE);
    g_return_val_if_fail (frame, FALSE);

    for (mark = node->xmlChildrenNode; mark; mark = mark->next)
    {
        if (g_strcmp0 ((char*)mark->name, "slot") == 0)
        {
            xmlNodePtr mark2;
            const gchar* key = NULL;
            std::optional<std::string> maybe_key;
            KvpValue* val = NULL;

            for (mark2 = mark->xmlChildrenNode; mark2; mark2 = mark2->next)
            {
                if (g_strcmp0 ((char*)mark2->name, "slot:key") == 0)
                {
                    key = dom_node_to_text (mark2);
                    if (!key)
                    {
                        maybe_key = dom_tree_to_text (mark2);
                        key = maybe_key ? maybe_key->c_str() : nullptr;
                    }
                }
                else if (g_strcmp0 ((char*)mark2->name, "slot:value") == 0)
                {
                    val = dom_tree_to_kvp_value (mark2);
                }
                else
                {
                    /* FIXME: should put some error here.
                     *        But ignore text type! */
                }
            }

            if (key)
            {
                if (val)
                {
                    //We're deleting the old KvpValue returned by replace_nc().
                    delete frame->set ({key}, val);
                }
                else
                {
                    /* FIXME: should put some error here */
                }
            }
        }
    }

    return TRUE;
}


KvpFrame*
dom_tree_to_kvp_frame (xmlNodePtr node)
{
    g_return_val_if_fail (node, NULL);

    auto ret = new KvpFrame;

    if (dom_tree_to_kvp_frame_given (node, ret))
        return ret;

    delete ret;
    return NULL;
}

gboolean
dom_tree_create_instance_slots (xmlNodePtr node, QofInstance* inst)
{
    KvpFrame* frame = qof_instance_get_slots (inst);
    return dom_tree_to_kvp_frame_given (node, frame);
}

std::optional<std::string>
dom_tree_to_text (xmlNodePtr tree)
{
    /* Expect *only* text and comment sibling nodes in the given tree --
       which actually may only be a "list".  i.e. if you're trying to
       extract bar from <foo>bar</foo>, pass in <foo>->xmlChildrenNode
       to this function.  This expectation is different from the rest of
       the dom_tree_to_* converters...

       Ignores comment nodes and collapse text nodes into one string.
       Returns NULL if expectations are unsatisfied.
    */
    std::string rv;
    gchar* temp;

    g_return_val_if_fail (tree, std::nullopt);

    /* no nodes means it's an empty string text */
    if (!tree->xmlChildrenNode)
    {
        DEBUG ("No children");
        return "";
    }

    temp = (char*)xmlNodeListGetString (NULL, tree->xmlChildrenNode, TRUE);
    if (!temp)
    {
        DEBUG ("Null string");
        return std::nullopt;
    }

    DEBUG ("node string [%s]", (temp == NULL ? "(null)" : temp));
    rv = temp;
    xmlFree (temp);
    return rv;
}

gnc_numeric
dom_tree_to_gnc_numeric (xmlNodePtr node)
{
    auto node_to_numeric = [](auto txt)
    {
        gnc_numeric num = gnc_numeric_from_string(txt);
        return gnc_numeric_check (num) ? gnc_numeric_zero() : num;
    };
    return apply_xmlnode_text<gnc_numeric> (node_to_numeric, node, gnc_numeric_zero());
}


time64
dom_tree_to_time64 (xmlNodePtr node)
{
    /* Turn something like this

       <date-posted>
         <ts:date>Mon, 05 Jun 2000 23:16:19 -0500</ts:date>
       </date-posted>

       into a time64, returning INT64_MAX that we're using to flag an erroneous
       date if there's a problem. Only one ts:date element is permitted for any
       date attribute.
    */

    time64 ret {INT64_MAX};
    gboolean seen = FALSE;
    xmlNodePtr n;

    for (n = node->xmlChildrenNode; n; n = n->next)
    {
        switch (n->type)
        {
        case XML_COMMENT_NODE:
        case XML_TEXT_NODE:
            break;
        case XML_ELEMENT_NODE:
            if (g_strcmp0 ("ts:date", (char*)n->name) == 0)
            {
                if (seen)
                {
                    return INT64_MAX;
                }
                seen = TRUE;
                ret = apply_xmlnode_text<time64> (gnc_iso8601_to_time64_gmt, n, INT64_MAX);
            }
            break;
        default:
            PERR ("unexpected sub-node.");
            return INT64_MAX;
            break;
        }
    }

    if (!seen)
    {
        PERR ("no ts:date node found.");
        return INT64_MAX;
    }

    return ret;
}

GDate*
dom_tree_to_gdate (xmlNodePtr node)
{
    /* Turn something like this

       <sx:startdate>
           <gdate>2001-04-03</gdate>
       </sx:startdate>

       into a GDate.  If the xml is invalid, returns NULL. */

    GDate ret;
    gboolean seen_date = FALSE;
    xmlNodePtr n;

    auto try_setting_date = [&ret](const char *content) -> bool
    {
        gint year = 0, month = 0, day = 0;
        if (sscanf (content, "%d-%d-%d", &year, &month, &day) != 3) return false;
        g_date_set_dmy (&ret, day, static_cast<GDateMonth>(month), year);
        return (g_date_valid (&ret));
    };

    /* creates an invalid date */
    g_date_clear (&ret, 1);

    for (n = node->xmlChildrenNode; n; n = n->next)
    {
        switch (n->type)
        {
        case XML_COMMENT_NODE:
        case XML_TEXT_NODE:
            break;
        case XML_ELEMENT_NODE:
            if (g_strcmp0 ("gdate", (char*)n->name) == 0)
            {
                if (seen_date || !apply_xmlnode_text<bool> (try_setting_date, n))
                    return NULL;
                seen_date = TRUE;
            }
            break;
        default:
            PERR ("unexpected sub-node.");
            return NULL;
        }
    }

    if (!seen_date)
    {
        PWARN ("no gdate node found.");
        return NULL;
    }

    return g_date_copy (&ret);
}

struct CommodityRef
{
    std::string space;
    std::string id;
};

std::string
gnc_strstrip (std::string_view sv)
{
    while (!sv.empty () && g_ascii_isspace (sv.front())) sv.remove_prefix (1);
    while (!sv.empty () && g_ascii_isspace (sv.back())) sv.remove_suffix (1);
    return std::string (sv);
}

static std::optional<CommodityRef>
parse_commodity_ref (xmlNodePtr node, QofBook* book)
{
    /* Turn something like this

       <currency>
         <cmdty:space>NASDAQ</cmdty:space>
         <cmdty:id>LNUX</cmdty:space>
       </currency>

       into a gnc_commodity*, returning NULL on failure.  Both sub-nodes
       are required, though for now, order is irrelevant. */

    CommodityRef rv;
    bool space_set{false};
    bool id_set{false};
    xmlNodePtr n;

    if (!node) return {};
    if (!node->xmlChildrenNode) return {};

    for (n = node->xmlChildrenNode; n; n = n->next)
    {
        switch (n->type)
        {
        case XML_COMMENT_NODE:
        case XML_TEXT_NODE:
            break;
        case XML_ELEMENT_NODE:
            if (g_strcmp0 ("cmdty:space", (char*)n->name) == 0)
            {
                if (space_set)
                {
                    return {};
                }
                rv.space = apply_xmlnode_text<std::string> (gnc_strstrip, n);
                space_set = true;
            }
            else if (g_strcmp0 ("cmdty:id", (char*)n->name) == 0)
            {
                if (id_set)
                {
                    return {};
                }
                rv.id = apply_xmlnode_text<std::string> (gnc_strstrip, n);
                id_set = true;
            }
            break;
        default:
            PERR ("unexpected sub-node.");
            return {};
            break;
        }
    }
    if (space_set && id_set)
        return rv;

    return {};
}

gnc_commodity*
dom_tree_to_commodity_ref_no_engine (xmlNodePtr node, QofBook* book)
{
    auto ref = parse_commodity_ref (node, book);

    if (!ref)
        return nullptr;

    return gnc_commodity_new (book, nullptr, ref->space.c_str(), ref->id.c_str(),
                              nullptr, 0);
}

gnc_commodity*
dom_tree_to_commodity_ref (xmlNodePtr node, QofBook* book)
{
    gnc_commodity* ret;
    gnc_commodity_table* table;

    auto ref = parse_commodity_ref (node, book);

    if (!ref)
        return nullptr;

    table = gnc_commodity_table_get_table (book);

    g_return_val_if_fail (table != NULL, NULL);

    ret =  gnc_commodity_table_lookup (table, ref->space.c_str(), ref->id.c_str());

    g_return_val_if_fail (ret != NULL, NULL);

    return ret;
}

/***********************************************************************/
/* generic parser */

static inline void
dom_tree_handlers_reset (struct dom_tree_handler* handlers)
{
    for (; handlers->tag != NULL; handlers++)
    {
        handlers->gotten = 0;
    }
}

static inline gboolean
dom_tree_handlers_all_gotten_p (struct dom_tree_handler* handlers)
{
    gboolean ret = TRUE;
    for (; handlers->tag != NULL; handlers++)
    {
        if (handlers->required && ! handlers->gotten)
        {
            PERR ("Not defined and it should be: %s",
                  handlers->tag ? handlers->tag : "(null)");
            ret = FALSE;
        }
    }
    return ret;
}


static inline gboolean
gnc_xml_set_data (const gchar* tag, xmlNodePtr node, gpointer item,
                  struct dom_tree_handler* handlers)
{
    for (; handlers->tag != NULL; handlers++)
    {
        if (g_strcmp0 (tag, handlers->tag) == 0)
        {
            (handlers->handler) (node, item);
            handlers->gotten = TRUE;
            break;
        }
    }

    if (!handlers->tag)
    {
        PERR ("Unhandled tag: %s",
              tag ? tag : "(null)");
        return FALSE;
    }

    return TRUE;
}

gboolean
dom_tree_generic_parse (xmlNodePtr node, struct dom_tree_handler* handlers,
                        gpointer data)
{
    xmlNodePtr achild;
    gboolean successful = TRUE;

    dom_tree_handlers_reset (handlers);

    for (achild = node->xmlChildrenNode; achild; achild = achild->next)
    {
        /* ignore stray text nodes */
        if (g_strcmp0 ((char*)achild->name, "text") == 0)
            continue;

        if (!gnc_xml_set_data ((char*)achild->name, achild, data, handlers))
        {
            PERR ("gnc_xml_set_data failed");
            successful = FALSE;
            continue;
        }
    }

    if (!dom_tree_handlers_all_gotten_p (handlers))
    {
        PERR ("didn't find all of the expected tags in the input");
        successful = FALSE;
    }

    return successful;
}

gboolean
dom_tree_valid_time64 (time64 val, const xmlChar * name)
{
    if (val != INT64_MAX)
        return TRUE;
    g_warning ("Invalid timestamp in data file. Look for a '%s' entry "
            "with a year outside of the valid range: 1400..10000", name);
    return FALSE;
}
