/********************************************************************
 * sixtp-dom-parsers.h                                              *
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
 ********************************************************************/

#include "config.h"

#include <glib.h>
#include <string.h>

#include "gnc-xml-helper.h"
#include "gnc-engine-util.h"
#include "gnc-commodity.h"
#include "gnc-engine.h"

#include "sixtp-utils.h"
#include "sixtp-dom-parsers.h"
#include "GNCId.h"

static short module = MOD_IO;

GUID*
dom_tree_to_guid(xmlNodePtr node)
{
    if(!node->properties)
    {
        return NULL;
    }

    if(strcmp(node->properties->name, "type") != 0)
    {
        PERR("Unknown attribute for id tag: %s\n",
             node->properties->name ?
             (char *) node->properties->name : "(null)");
        return NULL;
    }

    {
        char *type = node->properties->xmlAttrPropertyValue->content;
        /* handle new and guid the same for the moment */
        if((safe_strcmp("guid", type) == 0) || (safe_strcmp("new", type) == 0))
        {
            GUID *gid = g_new(GUID, 1);
            string_to_guid(node->xmlChildrenNode->content, gid);
            return gid;
        }
        else 
        {
            PERR("Unknown type %s for attribute type for tag %s",
                 type ? type : "(null)",
                 node->properties->name ?
                 (char *) node->properties->name : "(null)");
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
    
    text = dom_tree_to_text(node);

    if(string_to_gint64(text, daint))
    {
        return TRUE;
    }
    else 
    {
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

gboolean
string_to_binary(const gchar *str,  void **v, guint64 *data_len)
{
  guint64 str_len;
  guchar *data;
  int i, j;
  
  str_len = strlen(str);

  /* Since no whitespace is allowed and hex encoding is 2 text chars
     per binary char, the result must be half the input size and the
     input size must be even. */
  if((str_len % 2) != 0)
      return(FALSE);
  *data_len = str_len / 2;
  data = g_new0(guchar, *data_len);
  
  g_return_val_if_fail(*v, FALSE);

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
        new_val = dom_tree_to_kvp_value(mark);
        if(new_val)
        {
            list = g_list_append(list, (gpointer)new_val);
        }
    }

    if(list)
    {
        ret = kvp_value_new_glist_nc(list);
    }
    
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
    { "binary", dom_tree_to_binary_kvp_value },
    { "list", dom_tree_to_list_kvp_value },
    { "frame", dom_tree_to_frame_kvp_value },
    { 0, 0 },
};
    
kvp_value*
dom_tree_to_kvp_value(xmlNodePtr node)
{
    gchar *type;
    struct kvp_val_converter *mark;
    kvp_value *ret = NULL;
    
    type = xmlGetProp(node, "type");
    if(!type)
    {
        type = g_strdup_printf("string");
    }

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

kvp_frame*
dom_tree_to_kvp_frame(xmlNodePtr node)
{
    kvp_frame *ret;
    xmlNodePtr mark;
    
    g_return_val_if_fail(node, NULL);

    ret = kvp_frame_new();

    for(mark = node->xmlChildrenNode; mark; mark = mark->next)
    {
        if(safe_strcmp(mark->name, "slot") == 0)
        {
            xmlNodePtr mark2;
            gchar *key = NULL;
            kvp_value *val = NULL;

            for(mark2 = mark->xmlChildrenNode; mark2; mark2 = mark2->next)
            {
                if(safe_strcmp(mark2->name, "slot:key") == 0)
                {
                    key = dom_tree_to_text(mark2);
                }
                else if(safe_strcmp(mark2->name, "slot:value") == 0)
                {
                    val = dom_tree_to_kvp_value(mark2);
                }
                else
                {
                    /* FIXME: should put some error here */
                }
            }

            if(key)
            {
                if(val)
                {
                    kvp_frame_set_slot_nc(ret, key, val);
                }
                else
                {
                    /* FIXME: should put some error here */
                }
                g_free(key);
            }
        }
    }

    return ret;
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

     If there are a lot of text sub-nodes, this algorithm may be
     inefficient as it will reallocate a lot.  */

  gboolean ok = TRUE;
  xmlNodePtr current;
  gchar *result;
  gchar *temp;

  g_return_val_if_fail(tree, NULL);
  /* no nodes means it's an empty string text */
  if(!tree->xmlChildrenNode)
  {
      return g_strdup("");
  }

  result = g_strdup("");
  
  for(current = tree->xmlChildrenNode; current; current = current->next) {
    switch(current->type) {
    case XML_TEXT_NODE:
      temp = g_strconcat(result, (gchar *) current->content, NULL);
      g_free(result);
      result = temp;
      break;
    case XML_COMMENT_NODE:
      break;
    default:
      PERR("dom_tree_to_text: hit illegal node while extracting text.");
      PERR("  (name %s) (type %d) (content %s)", 
           current->name ? (char *) current->name : "(null)",
           current->type,
           current->content ? (char *) current->content : "(null)");
      current = NULL;
      ok = FALSE;
      break;
    };    
  }

  if(!ok) {
    if(result) g_free(result);
    result = NULL;
  }
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

    if(string_to_gnc_numeric(content, ret) != NULL)
    {
        g_free(content);
        return ret;
    }
    else
    {
        g_free(ret);
        return NULL;
    }
}

static Timespec*
timespec_failure(Timespec *would_have_been)
{
    if(would_have_been)
    {
        g_free(would_have_been);
    }
    return NULL;
}


Timespec*
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

  Timespec *ret;
  gboolean seen_s = FALSE;
  gboolean seen_ns = FALSE;
  xmlNodePtr n;

  ret = g_new(Timespec, 1);
  
  ret->tv_sec = 0;
  ret->tv_nsec = 0;
  for(n = node->xmlChildrenNode; n; n = n->next) {
    switch(n->type) {
    case XML_COMMENT_NODE:
      break;
    case XML_ELEMENT_NODE:
      if(safe_strcmp("ts:date", n->name) == 0) {
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
          
          if(!string_to_timespec_secs(content, ret)) {
              g_free(content);
              return timespec_failure(ret);
          }
          g_free(content);
          seen_s = TRUE;
        }
      }
      else if(safe_strcmp("ts:ns", n->name) == 0) {
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
          
          if(!string_to_timespec_nsecs(content, ret)) {
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

gnc_commodity *
dom_tree_to_commodity_ref_no_engine(xmlNodePtr node)
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
      break;
    case XML_ELEMENT_NODE:
      if(safe_strcmp("cmdty:space", n->name) == 0) {
        if(space_str) {
          return NULL;
        } else {
          gchar *content = dom_tree_to_text(n);
          if(!content) return NULL;
          space_str = content;
        }
      } else if(safe_strcmp("cmdty:id", n->name) == 0) {
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
      PERR("dom_tree_to_timespec: unexpected sub-node.");
      return NULL;
      break;
    }
  }
  if(!(space_str && id_str)) {
    c = NULL;
  } else {
    g_strstrip(space_str);
    g_strstrip(id_str);
    c = gnc_commodity_new(NULL, space_str, id_str, NULL, 0);
  }

  g_free(space_str);
  g_free(id_str);

  return c;
}

gnc_commodity*
dom_tree_to_commodity_ref(xmlNodePtr node)
{
    gnc_commodity *daref;
    gnc_commodity *ret;
    
    daref = dom_tree_to_commodity_ref_no_engine(node);
    ret = associate_commodity_ref_with_engine_commodity(daref);

    gnc_commodity_destroy(daref);

    return ret;
}

gnc_commodity *
associate_commodity_ref_with_engine_commodity(gnc_commodity *com)
{
    return gnc_commodity_table_lookup(gnc_engine_commodities(),
                                      gnc_commodity_get_namespace(com),
                                      gnc_commodity_get_mnemonic(com));
}

/***********************************************************************/
/* generic parser */

static void
dom_tree_handlers_reset(struct dom_tree_handler *handlers)
{
    for(;handlers->tag != NULL; handlers++)
    {
        handlers->gotten = 0;
    }
}

static gboolean
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

        
static gboolean
gnc_xml_set_data(const gchar* tag, xmlNodePtr node, gboolean *item,
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
        PERR("Unhandled tag: %s\n",
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
        if(!gnc_xml_set_data(achild->name, achild, data, handlers))
        {
            PERR("gnc_xml_set_data failed");
            successful = FALSE;
            break;
        }
    }

    if(!dom_tree_handlers_all_gotten_p(handlers))
    {
        PERR("missing tag in input");
        successful = FALSE;
    }

    return successful;
}

gboolean
string_to_integer(const char *content, gint64 *to)
{
    long long int to_in;

    if(sscanf(content, "%lld", &to_in) == 1)
    {
        if (to)
          *to = to_in;

        return TRUE;
    }
    else
    {
        return FALSE;
    }
}
