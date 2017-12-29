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
extern "C"
{
#include <config.h>

#include <glib.h>
#include <string.h>

#include "gnc-xml-helper.h"
#include <gnc-engine.h>
}

#include "sixtp-utils.h"
#include "sixtp-dom-parsers.h"
#include <kvp-frame.hpp>

static QofLogModule log_module = GNC_MOD_IO;

GncGUID*
dom_tree_to_guid (xmlNodePtr node)
{
    if (!node->properties)
    {
        return NULL;
    }

    if (strcmp ((char*) node->properties->name, "type") != 0)
    {
        PERR ("Unknown attribute for id tag: %s",
              node->properties->name ?
              (char*) node->properties->name : "(null)");
        return NULL;
    }

    {
        char* type;

        type = (char*)xmlNodeGetContent (node->properties->xmlAttrPropertyValue);

        /* handle new and guid the same for the moment */
        if ((g_strcmp0 ("guid", type) == 0) || (g_strcmp0 ("new", type) == 0))
        {
            auto gid = guid_new ();
            char* guid_str;

            guid_str = (char*)xmlNodeGetContent (node->xmlChildrenNode);
            string_to_guid (guid_str, gid);
            xmlFree (guid_str);
            xmlFree (type);
            return gid;
        }
        else
        {
            PERR ("Unknown type %s for attribute type for tag %s",
                  type ? type : "(null)",
                  node->properties->name ?
                  (char*) node->properties->name : "(null)");
            xmlFree (type);
            return NULL;
        }
    }
}

static KvpValue*
dom_tree_to_integer_kvp_value (xmlNodePtr node)
{
    gchar* text;
    gint64 daint;
    KvpValue* ret = NULL;

    text = dom_tree_to_text (node);

    if (string_to_gint64 (text, &daint))
    {
        ret = new KvpValue {daint};
    }
    g_free (text);

    return ret;
}

gboolean
dom_tree_to_integer (xmlNodePtr node, gint64* daint)
{
    gchar* text;
    gboolean ret;

    text = dom_tree_to_text (node);

    ret = string_to_gint64 (text, daint);

    g_free (text);

    return ret;
}

gboolean
dom_tree_to_guint16 (xmlNodePtr node, guint16* i)
{
    gboolean ret;
    guint j = 0;

    ret = dom_tree_to_guint (node, &j);
    *i = (guint16) j;
    return ret;
}

gboolean
dom_tree_to_guint (xmlNodePtr node, guint* i)
{
    gchar* text, *endptr;
    gboolean ret;

    text = dom_tree_to_text (node);
    /* In spite of the strange string_to_gint64 function, I'm just
       going to use strtoul here until someone shows me the error of
       my ways. -CAS */
    *i = (guint) strtoul (text, &endptr, 0);
    ret = (endptr != text);
    g_free (text);
    return ret;
}

gboolean
dom_tree_to_boolean (xmlNodePtr node, gboolean* b)
{
    gchar* text;

    text = dom_tree_to_text (node);
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
}

static KvpValue*
dom_tree_to_double_kvp_value (xmlNodePtr node)
{
    gchar* text;
    double dadoub;
    KvpValue* ret = NULL;

    text = dom_tree_to_text (node);

    if (string_to_double (text, &dadoub))
    {
        ret = new KvpValue {dadoub};
    }

    g_free (text);

    return ret;
}

static KvpValue*
dom_tree_to_numeric_kvp_value (xmlNodePtr node)
{
    gnc_numeric* danum;
    KvpValue* ret = NULL;

    danum = dom_tree_to_gnc_numeric (node);

    if (danum)
    {
        ret = new KvpValue {*danum};
    }

    g_free (danum);

    return ret;
}

static KvpValue*
dom_tree_to_string_kvp_value (xmlNodePtr node)
{
    gchar* datext;
    KvpValue* ret = NULL;

    datext = dom_tree_to_text (node);
    if (datext)
    {
        ret = new KvpValue {datext};
    }

    return ret;
}

static KvpValue*
dom_tree_to_guid_kvp_value (xmlNodePtr node)
{
    GncGUID* daguid;
    KvpValue* ret = NULL;

    daguid = dom_tree_to_guid (node);
    if (daguid)
    {
        ret = new KvpValue {daguid};
    }

    return ret;
}

static KvpValue*
dom_tree_to_timespec_kvp_value (xmlNodePtr node)
{
    Timespec ts;
    KvpValue* ret = nullptr;

    ts = dom_tree_to_timespec (node);
    ret = new KvpValue {ts};
    return ret;
}

static KvpValue*
dom_tree_to_gdate_kvp_value (xmlNodePtr node)
{
    GDate* date;
    KvpValue* ret = NULL;

    date = dom_tree_to_gdate (node);

    if (date)
    {
        ret = new KvpValue {*date};
    }

    g_free (date);

    return ret;
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
            list = g_list_append (list, (gpointer)new_val);
        }
    }

    ret = new KvpValue {list};

    return ret;
}

static KvpValue*
dom_tree_to_frame_kvp_value (xmlNodePtr node)
{
    KvpFrame* frame;
    KvpValue* ret = NULL;

    frame = dom_tree_to_kvp_frame (node);

    if (frame)
    {
        ret = new KvpValue {frame};
    }

    return ret;
}


struct kvp_val_converter
{
    const gchar* tag;
    KvpValue* (*converter) (xmlNodePtr node);
};

struct kvp_val_converter val_converters[] =
{
    { "integer", dom_tree_to_integer_kvp_value },
    { "double", dom_tree_to_double_kvp_value },
    { "numeric", dom_tree_to_numeric_kvp_value },
    { "string", dom_tree_to_string_kvp_value },
    { "guid", dom_tree_to_guid_kvp_value },
    { "timespec", dom_tree_to_timespec_kvp_value },
    { "gdate", dom_tree_to_gdate_kvp_value },
    { "list", dom_tree_to_list_kvp_value },
    { "frame", dom_tree_to_frame_kvp_value },
    { 0, 0 },
};

static KvpValue*
dom_tree_to_kvp_value (xmlNodePtr node)
{
    xmlChar* xml_type;
    gchar* type;
    struct kvp_val_converter* mark;
    KvpValue* ret = NULL;

    xml_type = xmlGetProp (node, BAD_CAST "type");
    if (xml_type)
    {
        type = g_strdup ((char*) xml_type);
        xmlFree (xml_type);
    }
    else
        type = NULL;

    for (mark = val_converters; mark->tag; mark++)
    {
        if (g_strcmp0 (type, mark->tag) == 0)
        {
            ret = (mark->converter) (node);
        }
    }

    if (!mark->tag)
    {
        /* FIXME: deal with unknown type tag here */
    }

    g_free (type);

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
            gchar* key = NULL;
            KvpValue* val = NULL;

            for (mark2 = mark->xmlChildrenNode; mark2; mark2 = mark2->next)
            {
                if (g_strcmp0 ((char*)mark2->name, "slot:key") == 0)
                {
                    key = dom_tree_to_text (mark2);
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
                g_free (key);
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

gchar*
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
    gchar* result;
    gchar* temp;

    g_return_val_if_fail (tree, NULL);

    /* no nodes means it's an empty string text */
    if (!tree->xmlChildrenNode)
    {
        DEBUG ("No children");
        return g_strdup ("");
    }

    temp = (char*)xmlNodeListGetString (NULL, tree->xmlChildrenNode, TRUE);
    if (!temp)
    {
        DEBUG ("Null string");
        return NULL;
    }

    DEBUG ("node string [%s]", (temp == NULL ? "(null)" : temp));
    result = g_strdup (temp);
    xmlFree (temp);
    return result;
}

gnc_numeric*
dom_tree_to_gnc_numeric (xmlNodePtr node)
{
    gchar* content = dom_tree_to_text (node);
    if (!content)
        return NULL;

    gnc_numeric *ret = g_new (gnc_numeric, 1);

    if (!string_to_gnc_numeric (content, ret))
	*ret = gnc_numeric_zero ();
    g_free (content);
    return ret;
}

static inline Timespec
timespec_failure (Timespec ts)
{
    ts.tv_sec = 0;
    ts.tv_nsec = 0;
    return ts;
}


Timespec
dom_tree_to_timespec (xmlNodePtr node)
{
    /* Turn something like this

       <date-posted>
         <s>Mon, 05 Jun 2000 23:16:19 -0500</s>
         <ns>658864000</ns>
       </date-posted>

       into a Timespec.  If this returns FALSE, the effects on *ts are
       undefined.  The XML is valid if it has at least one of <s> or <ns>
       and no more than one of each.  Order is irrelevant. */

    Timespec ret;
    gboolean seen_s = FALSE;
    gboolean seen_ns = FALSE;
    xmlNodePtr n;


    ret.tv_sec = 0;
    ret.tv_nsec = 0;
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
                if (seen_s)
                {
                    return timespec_failure (ret);
                }
                else
                {
                    gchar* content = dom_tree_to_text (n);
                    if (!content)
                    {
                        return timespec_failure (ret);
                    }

                    if (!string_to_timespec_secs (content, &ret))
                    {
                        g_free (content);
                        return timespec_failure (ret);
                    }
                    g_free (content);
                    seen_s = TRUE;
                }
            }
            else if (g_strcmp0 ("ts:ns", (char*)n->name) == 0)
            {
                if (seen_ns)
                {
                    return timespec_failure (ret);
                }
                else
                {
                    gchar* content = dom_tree_to_text (n);
                    if (!content)
                    {
                        return timespec_failure (ret);
                    }

                    if (!string_to_timespec_nsecs (content, &ret))
                    {
                        g_free (content);
                        return timespec_failure (ret);
                    }
                    g_free (content);
                    seen_ns = TRUE;
                }
            }
            break;
        default:
            PERR ("unexpected sub-node.");
            return timespec_failure (ret);
            break;
        }
    }

    if (!seen_s)
    {
        PERR ("no ts:date node found.");
        return timespec_failure (ret);
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

    GDate* ret;
    gboolean seen_date = FALSE;
    xmlNodePtr n;

    /* creates an invalid date */
    ret = g_date_new ();

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
                if (seen_date)
                {
                    goto failure;
                }
                else
                {
                    gchar* content = dom_tree_to_text (n);
                    gint year, month, day;
                    if (!content)
                    {
                        goto failure;
                    }

                    if (sscanf (content, "%d-%d-%d", &year, &month, &day) != 3)
                    {
                        g_free (content);
                        goto failure;
                    }
                    g_free (content);
                    seen_date = TRUE;
                    g_date_set_dmy (ret, day, static_cast<GDateMonth> (month),
                                    year);
                    if (!g_date_valid (ret))
                    {
                        PWARN ("invalid date");
                        goto failure;
                    }
                }
            }
            break;
        default:
            PERR ("unexpected sub-node.");
            goto failure;
        }
    }

    if (!seen_date)
    {
        PWARN ("no gdate node found.");
        goto failure;
    }

    return ret;
failure:
    g_date_free (ret);
    return NULL;
}


gnc_commodity*
dom_tree_to_commodity_ref_no_engine (xmlNodePtr node, QofBook* book)
{
    /* Turn something like this

       <currency>
         <cmdty:space>NASDAQ</cmdty:space>
         <cmdty:id>LNUX</cmdty:space>
       </currency>

       into a gnc_commodity*, returning NULL on failure.  Both sub-nodes
       are required, though for now, order is irrelevant. */

    gnc_commodity* c = NULL;
    gchar* space_str = NULL;
    gchar* id_str = NULL;
    xmlNodePtr n;

    if (!node) return NULL;
    if (!node->xmlChildrenNode) return NULL;

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
                if (space_str)
                {
                    return NULL;
                }
                else
                {
                    gchar* content = dom_tree_to_text (n);
                    if (!content) return NULL;
                    space_str = content;
                }
            }
            else if (g_strcmp0 ("cmdty:id", (char*)n->name) == 0)
            {
                if (id_str)
                {
                    return NULL;
                }
                else
                {
                    gchar* content = dom_tree_to_text (n);
                    if (!content) return NULL;
                    id_str = content;
                }
            }
            break;
        default:
            PERR ("unexpected sub-node.");
            return NULL;
            break;
        }
    }
    if (! (space_str && id_str))
    {
        c = NULL;
    }
    else
    {
        g_strstrip (space_str);
        g_strstrip (id_str);
        c = gnc_commodity_new (book, NULL, space_str, id_str, NULL, 0);
    }

    g_free (space_str);
    g_free (id_str);

    return c;
}

gnc_commodity*
dom_tree_to_commodity_ref (xmlNodePtr node, QofBook* book)
{
    gnc_commodity* daref;
    gnc_commodity* ret;
    gnc_commodity_table* table;

    daref = dom_tree_to_commodity_ref_no_engine (node, book);

    table = gnc_commodity_table_get_table (book);

    g_return_val_if_fail (table != NULL, NULL);

    ret =  gnc_commodity_table_lookup (table,
                                       gnc_commodity_get_namespace (daref),
                                       gnc_commodity_get_mnemonic (daref));

    gnc_commodity_destroy (daref);

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
dom_tree_valid_timespec (Timespec* ts, const xmlChar* name)
{

    if (ts->tv_sec || ts->tv_nsec)
        return TRUE;

    g_warning ("Invalid timestamp in data file.  Look for a '%s' entry "
               "with a date of 1969-12-31 or 1970-01-01.", name);
    return FALSE;
}
