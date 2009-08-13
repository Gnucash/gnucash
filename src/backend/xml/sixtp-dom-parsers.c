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

#include "config.h"

#include <glib.h>
#include <string.h>

#include "gnc-xml-helper.h"
#include "gnc-engine.h"
#include "sixtp-utils.h"
#include "sixtp-dom-parsers.h"

static QofLogModule log_module = GNC_MOD_IO;

GUID*
dom_tree_to_guid(xmlNodePtr node)
{
    if(!node->properties)
    {
        return NULL;
    }

    if(strcmp((char*) node->properties->name, "type") != 0)
    {
        PERR("Unknown attribute for id tag: %s",
             node->properties->name ?
             (char *) node->properties->name : "(null)");
        return NULL;
    }

    {
        char *type;

        type = (char*)xmlNodeGetContent (node->properties->xmlAttrPropertyValue);

        /* handle new and guid the same for the moment */
        if((safe_strcmp("guid", type) == 0) || (safe_strcmp("new", type) == 0))
        {
            GUID *gid = g_new(GUID, 1);
            char *guid_str;

            guid_str = (char*)xmlNodeGetContent (node->xmlChildrenNode);
            string_to_guid(guid_str, gid);
            xmlFree (guid_str);
            xmlFree (type);
            return gid;
        }
        else 
        {
            PERR("Unknown type %s for attribute type for tag %s",
                 type ? type : "(null)",
                 node->properties->name ?
                 (char *) node->properties->name : "(null)");
            xmlFree (type);
            return NULL;
        }
    }
}

kvp_value*
dom_tree_to_integer_kvp_value(xmlNodePtr node)
{
    gchar *text;
    gint64 daint;
    kvp_value* ret = NULL;
    
    text = dom_tree_to_text(node);

    if(string_to_gint64(text, &daint))
    {
        ret = kvp_value_new_gint64(daint);
    }
    g_free(text);

    return ret;
}

gboolean
dom_tree_to_integer(xmlNodePtr node, gint64 *daint)
{
    gchar *text;
    gboolean ret;

    text = dom_tree_to_text(node);

    ret = string_to_gint64(text, daint);

    g_free (text);

    return ret;
}

gboolean
dom_tree_to_guint16(xmlNodePtr node, guint16 *i)
{
    gboolean ret;
    guint j = 0;

    ret = dom_tree_to_guint(node, &j);
    *i = (guint16) j;
    return ret;
}

gboolean
dom_tree_to_guint(xmlNodePtr node, guint *i)
{
    gchar *text, *endptr;
    gboolean ret;

    text = dom_tree_to_text(node);
    /* In spite of the strange string_to_gint64 function, I'm just
       going to use strtoul here until someone shows me the error of
       my ways. -CAS */
    *i = (guint) strtoul(text, &endptr, 0);
    ret = (endptr != text);
    g_free(text);
    return ret;
}

gboolean
dom_tree_to_boolean(xmlNodePtr node, gboolean* b)
{
    gchar* text;

	text = dom_tree_to_text(node);
	if (strcasecmp(text, "true") == 0) {
	    *b = TRUE;
		return TRUE;
	} else if(strcasecmp(text, "false") == 0) {
	    *b = FALSE;
		return TRUE;
	} else {
	    *b = FALSE;
		return FALSE;
	}
}

kvp_value*
dom_tree_to_double_kvp_value(xmlNodePtr node)
{
    gchar *text;
    double dadoub;
    kvp_value *ret = NULL;

    text = dom_tree_to_text(node);

    if(string_to_double(text, &dadoub))
    {
        ret = kvp_value_new_double(dadoub);
    }

    g_free(text);
    
    return ret;
}

kvp_value*
dom_tree_to_numeric_kvp_value(xmlNodePtr node)
{
    gnc_numeric *danum;
    kvp_value *ret = NULL;

    danum = dom_tree_to_gnc_numeric(node);

    if(danum)
    {
        ret = kvp_value_new_gnc_numeric(*danum);
    }

    g_free(danum);
    
    return ret;
}

kvp_value*
dom_tree_to_string_kvp_value(xmlNodePtr node)
{
    gchar *datext;
    kvp_value *ret = NULL;

    datext = dom_tree_to_text(node);
    if(datext)
    {
        ret = kvp_value_new_string(datext);
    }

    g_free(datext);
    
    return ret;
}

kvp_value*
dom_tree_to_guid_kvp_value(xmlNodePtr node)
{
    GUID *daguid;
    kvp_value *ret = NULL;

    daguid = dom_tree_to_guid(node);
    if(daguid)
    {
        ret = kvp_value_new_guid(daguid);
    }

    g_free(daguid);
    
    return ret;
}

kvp_value*
dom_tree_to_timespec_kvp_value (xmlNodePtr node)
{
  Timespec ts;
  kvp_value * ret = NULL;

  ts = dom_tree_to_timespec (node);
  if (ts.tv_sec || ts.tv_nsec)
  {
    ret = kvp_value_new_timespec (ts);
  }
  return ret;
}

gboolean
string_to_binary(const gchar *str,  void **v, guint64 *data_len)
{
  guint64 str_len;
  guchar *data;
  unsigned int i, j;

  g_return_val_if_fail(v != NULL, FALSE);
  g_return_val_if_fail(data_len != NULL, FALSE);

  str_len = strlen(str);

  /* Since no whitespace is allowed and hex encoding is 2 text chars
     per binary char, the result must be half the input size and the
     input size must be even. */
  if((str_len % 2) != 0)
      return(FALSE);
  *data_len = str_len / 2;
  data = g_new0(guchar, *data_len);

  for(j = 0, i = 0; i < str_len; i += 2, j++)
  {
      gchar tmpstr[3];
      long int converted;

      tmpstr[0] = str[i];
      tmpstr[1] = str[i + 1];
      tmpstr[2] = '\0';

      converted = strtol(tmpstr, NULL, 16);

      data[j] = (unsigned char)converted;
  }

  *v = data;

  return(TRUE);
}

kvp_value*
dom_tree_to_binary_kvp_value(xmlNodePtr node)
{
    gchar *text;
    void *val;
    guint64 len;
    kvp_value *ret = NULL;

    text = dom_tree_to_text(node);

    if(string_to_binary(text, &val, &len))
    {
        ret = kvp_value_new_binary_nc(val, len);
    }
    else
    {
        PERR("string_to_binary returned false");
    }
    
    g_free(text);

    return ret;
}

kvp_value*
dom_tree_to_list_kvp_value(xmlNodePtr node)
{
    GList *list = NULL;
    xmlNodePtr mark;
    kvp_value *ret = NULL;

    for(mark = node->xmlChildrenNode; mark; mark = mark->next)
    {
        kvp_value *new_val;

        if (safe_strcmp ((char*)mark->name, "text") == 0)
          continue;

        new_val = dom_tree_to_kvp_value(mark);
        if(new_val)
        {
            list = g_list_append(list, (gpointer)new_val);
        }
    }

    ret = kvp_value_new_glist_nc(list);

    return ret;
}

kvp_value*
dom_tree_to_frame_kvp_value(xmlNodePtr node)
{
    kvp_frame *frame;
    kvp_value *ret = NULL;

    frame = dom_tree_to_kvp_frame(node);

    if(frame)
    {
        ret = kvp_value_new_frame(frame);
    }

    kvp_frame_delete(frame);
    
    return ret;
}


struct kvp_val_converter
{
    gchar *tag;
    kvp_value* (*converter)(xmlNodePtr node);
};

struct kvp_val_converter val_converters[] = {
    { "integer", dom_tree_to_integer_kvp_value },
    { "double", dom_tree_to_double_kvp_value },
    { "numeric", dom_tree_to_numeric_kvp_value },
    { "string", dom_tree_to_string_kvp_value },
    { "guid", dom_tree_to_guid_kvp_value },
    { "timespec", dom_tree_to_timespec_kvp_value },
    { "binary", dom_tree_to_binary_kvp_value },
    { "list", dom_tree_to_list_kvp_value },
    { "frame", dom_tree_to_frame_kvp_value },
    { 0, 0 },
};
    
kvp_value*
dom_tree_to_kvp_value(xmlNodePtr node)
{
    xmlChar *xml_type;
    gchar *type;
    struct kvp_val_converter *mark;
    kvp_value *ret = NULL;

    xml_type = xmlGetProp(node, BAD_CAST "type");
    if(xml_type)
    {
        type = g_strdup ((char*) xml_type);
        xmlFree (xml_type);
    }
    else
      type = NULL;

    for(mark = val_converters; mark->tag; mark++)
    {
        if(safe_strcmp(type, mark->tag) == 0)
        {
            ret = (mark->converter)(node);
        }
    }

    if(!mark->tag)
    {
        /* FIXME: deal with unknown type tag here */
    }

    g_free(type);
    
    return ret;
}

gboolean
dom_tree_to_kvp_frame_given(xmlNodePtr node, kvp_frame *frame)
{
    xmlNodePtr mark;

    g_return_val_if_fail(node, FALSE);
    g_return_val_if_fail(frame, FALSE);

    for(mark = node->xmlChildrenNode; mark; mark = mark->next)
    {
        if(safe_strcmp((char*)mark->name, "slot") == 0)
        {
            xmlNodePtr mark2;
            gchar *key = NULL;
            kvp_value *val = NULL;

            for(mark2 = mark->xmlChildrenNode; mark2; mark2 = mark2->next)
            {
                if(safe_strcmp((char*)mark2->name, "slot:key") == 0)
                {
                    key = dom_tree_to_text(mark2);
                }
                else if(safe_strcmp((char*)mark2->name, "slot:value") == 0)
                {
                    val = dom_tree_to_kvp_value(mark2);
                }
                else
                {
                    /* FIXME: should put some error here. 
                     *        But ignore text type! */
                }
            }

            if(key)
            {
                if(val)
                {
                    kvp_frame_set_slot_nc(frame, key, val);
                }
                else
                {
                    /* FIXME: should put some error here */
                }
                g_free(key);
            }
        }
    }

    return TRUE;
}


kvp_frame*
dom_tree_to_kvp_frame(xmlNodePtr node)
{
    kvp_frame *ret;
    
    g_return_val_if_fail(node, NULL);

    ret = kvp_frame_new();

    if (dom_tree_to_kvp_frame_given(node, ret))
        return ret;

    kvp_frame_delete(ret);

    return NULL;
}


gchar *
dom_tree_to_text(xmlNodePtr tree)
{
  /* Expect *only* text and comment sibling nodes in the given tree --
     which actually may only be a "list".  i.e. if you're trying to
     extract bar from <foo>bar</foo>, pass in <foo>->xmlChildrenNode
     to this function.  This expectation is different from the rest of
     the dom_tree_to_* converters...

     Ignores comment nodes and collapse text nodes into one string.
     Returns NULL if expectations are unsatisfied.
  */
  gchar *result;
  gchar *temp;

  g_return_val_if_fail(tree, NULL);

  /* no nodes means it's an empty string text */
  if(!tree->xmlChildrenNode)
  {
      DEBUG("No children");
      return g_strdup("");
  }

  temp = (char*)xmlNodeListGetString (NULL, tree->xmlChildrenNode, TRUE);
  if (!temp) 
  {
    DEBUG("Null string");
    return NULL;
  }

  DEBUG("node string [%s]", (temp == NULL ? "(null)" : temp));
  result = g_strdup (temp);
  xmlFree (temp);
  return result;
}

gnc_numeric*
dom_tree_to_gnc_numeric(xmlNodePtr node)
{
    gchar *content = dom_tree_to_text(node);
    gnc_numeric *ret;
    if(!content)
        return NULL;

    ret = g_new(gnc_numeric, 1);

    if(string_to_gnc_numeric(content, ret))
    {
        g_free(content);
        return ret;
    }
    else
    {
        g_free(content);
        g_free(ret);
        return NULL;
    }
}

static inline Timespec
timespec_failure(Timespec ts)
{
    ts.tv_sec = 0;
    ts.tv_nsec = 0;
    return ts;
}


Timespec
dom_tree_to_timespec(xmlNodePtr node)
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
  for(n = node->xmlChildrenNode; n; n = n->next) {
    switch(n->type) {
    case XML_COMMENT_NODE:
    case XML_TEXT_NODE:
      break;
    case XML_ELEMENT_NODE:
      if(safe_strcmp("ts:date", (char*)n->name) == 0) {
        if(seen_s)
        {
            return timespec_failure(ret);
        }
        else
        {
          gchar *content = dom_tree_to_text(n);
          if(!content)
          {
              return timespec_failure(ret);
          }
          
          if(!string_to_timespec_secs(content, &ret)) {
              g_free(content);
              return timespec_failure(ret);
          }
          g_free(content);
          seen_s = TRUE;
        }
      }
      else if(safe_strcmp("ts:ns", (char*)n->name) == 0) {
        if(seen_ns)
        {
            return timespec_failure(ret);
        }
        else
        {
          gchar *content = dom_tree_to_text(n);
          if(!content)
          {
              return timespec_failure(ret);
          }
          
          if(!string_to_timespec_nsecs(content, &ret)) {
              g_free(content);
              return timespec_failure(ret);
          }
          g_free(content);
          seen_ns = TRUE;
        }
      }
      break;
    default:
      PERR("unexpected sub-node.");
      return timespec_failure(ret);
      break;
    }
  }

  if(!seen_s)
  {
      PERR("no ts:date node found.");
      return timespec_failure(ret);
  }
  
  return ret;
}

GDate*
dom_tree_to_gdate(xmlNodePtr node)
{
  /* Turn something like this

     <sx:startdate>
         <gdate>2001-04-03</gdate>
     </sx:startdate>

     into a GDate.  If the xml is invalid, returns NULL. */

  GDate *ret;
  gboolean seen_date = FALSE;
  xmlNodePtr n;

  /* creates an invalid date */
  ret = g_date_new();

  for(n = node->xmlChildrenNode; n; n = n->next) {
    switch(n->type) {
    case XML_COMMENT_NODE:
    case XML_TEXT_NODE:
      break;
    case XML_ELEMENT_NODE:
      if(safe_strcmp("gdate", (char*)n->name) == 0) {
        if(seen_date) {
            goto failure;
        } else {
          gchar *content = dom_tree_to_text(n);
          gint year, month, day;
          if(!content) {
              goto failure;
          }

          if(sscanf(content, "%d-%d-%d", &year, &month, &day ) != 3) {
            g_free(content);
            goto failure;
          }
          g_free(content);
          seen_date = TRUE;
          g_date_set_dmy( ret, day, month, year );
          if( !g_date_valid( ret ) ) {
            PWARN("invalid date");
            goto failure;
          }
        }
      }
      break;
    default:
      PERR("unexpected sub-node.");
      goto failure;
    }
  }

  if(!seen_date) {
      PWARN("no gdate node found.");
      goto failure;
  }

  return ret;
failure:
  g_date_free( ret );
  return NULL;
}


gnc_commodity *
dom_tree_to_commodity_ref_no_engine(xmlNodePtr node, QofBook *book)
{
  /* Turn something like this
     
     <currency>
       <cmdty:space>NASDAQ</cmdty:space>
       <cmdty:id>LNUX</cmdty:space>
     </currency>
     
     into a gnc_commodity*, returning NULL on failure.  Both sub-nodes
     are required, though for now, order is irrelevant. */

  gnc_commodity *c = NULL;
  gchar *space_str = NULL;
  gchar *id_str = NULL;
  xmlNodePtr n;

  if(!node) return NULL;
  if(!node->xmlChildrenNode) return NULL;
  
  for(n = node->xmlChildrenNode; n; n = n->next) {
    switch(n->type) {
    case XML_COMMENT_NODE:
    case XML_TEXT_NODE:
      break;
    case XML_ELEMENT_NODE:
      if(safe_strcmp("cmdty:space", (char*)n->name) == 0) {
        if(space_str) {
          return NULL;
        } else {
          gchar *content = dom_tree_to_text(n);
          if(!content) return NULL;
          space_str = content;
        }
      } else if(safe_strcmp("cmdty:id", (char*)n->name) == 0) {
        if(id_str) {
          return NULL;
        } else {
          gchar *content = dom_tree_to_text(n);
          if(!content) return NULL;
          id_str = content;
        }
      }
      break;
    default:
      PERR("unexpected sub-node.");
      return NULL;
      break;
    }
  }
  if(!(space_str && id_str)) {
    c = NULL;
  } else {
    g_strstrip(space_str);
    g_strstrip(id_str);
    c = gnc_commodity_new(book, NULL, space_str, id_str, NULL, 0);
  }

  g_free(space_str);
  g_free(id_str);

  return c;
}

gnc_commodity*
dom_tree_to_commodity_ref(xmlNodePtr node, QofBook *book)
{
    gnc_commodity *daref;
    gnc_commodity *ret;
    gnc_commodity_table *table;

    daref = dom_tree_to_commodity_ref_no_engine(node, book);

    table = gnc_commodity_table_get_table (book);

    g_return_val_if_fail (table != NULL, NULL);

    ret =  gnc_commodity_table_lookup (table,
                                       gnc_commodity_get_namespace(daref),
                                       gnc_commodity_get_mnemonic(daref));

    gnc_commodity_destroy(daref);

    g_return_val_if_fail (ret != NULL, NULL);

    return ret;
}

/***********************************************************************/
/* generic parser */

static inline void
dom_tree_handlers_reset(struct dom_tree_handler *handlers)
{
    for(;handlers->tag != NULL; handlers++)
    {
        handlers->gotten = 0;
    }
}

static inline gboolean
dom_tree_handlers_all_gotten_p(struct dom_tree_handler *handlers)
{
    gboolean ret = TRUE;
    for(;handlers->tag != NULL;handlers++)
    {
        if(handlers->required && ! handlers->gotten)
        {
            PERR("Not defined and it should be: %s",
                 handlers->tag ? handlers->tag : "(null)");
            ret = FALSE;
        }
    }
    return ret;
}


static inline gboolean
gnc_xml_set_data(const gchar* tag, xmlNodePtr node, gpointer item,
                 struct dom_tree_handler *handlers)
{
    for(;handlers->tag != NULL; handlers++)
    {
        if(safe_strcmp(tag, handlers->tag) == 0)
        {
            (handlers->handler)(node, item);
            handlers->gotten = TRUE;
            break;
        }
    }

    if(!handlers->tag) 
    {
        PERR("Unhandled tag: %s",
             tag ? tag : "(null)");
        return FALSE;
    }

    return TRUE;
}

gboolean
dom_tree_generic_parse(xmlNodePtr node, struct dom_tree_handler *handlers,
                       gpointer data)
{
    xmlNodePtr achild;
    gboolean successful = TRUE;

    dom_tree_handlers_reset(handlers);

    for(achild = node->xmlChildrenNode; achild; achild = achild->next)
    {
        /* ignore stray text nodes */
        if (safe_strcmp ((char*)achild->name, "text") == 0)
          continue;

        if(!gnc_xml_set_data((char*)achild->name, achild, data, handlers))
        {
            PERR("gnc_xml_set_data failed");
            successful = FALSE;
            continue;
        }
    }

    if(!dom_tree_handlers_all_gotten_p(handlers))
    {
        PERR("didn't find all of the expected tags in the input");
        successful = FALSE;
    }

    return successful;
}

gboolean
dom_tree_valid_timespec (Timespec *ts, const xmlChar *name)
{

  if (ts->tv_sec || ts->tv_nsec)
    return TRUE;

  g_warning("Invalid timestamp in data file.  Look for a '%s' entry "
	    "with a date of 1969-12-31 or 1970-01-01.", name);
  return FALSE;
}
