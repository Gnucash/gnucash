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
        g_warning("Unknown attribute for id tag: %s\n",
                  node->properties->name);
        return NULL;
    }
    
    {
        char *type = node->properties->val->content;
        if(strcmp("guid", type) == 0)
        {
            GUID *gid = g_new(GUID, 1);
            string_to_guid(node->xmlChildrenNode->content, gid);
            return gid;
        }
        else if(strcmp("new", type) == 0)
        {
            /* FIXME: handle this case */
            return NULL;
        }
        else 
        {
            g_warning("Unknown type %s for attribute type for tag %s",
                      type, node->properties->name);
            return NULL;
        }
    }
}

kvp_frame*
dom_tree_handle_kvp(xmlNodePtr node)
{

    return FALSE;
}


gchar *
dom_tree_to_text(xmlNodePtr tree)
{
  /* Expect *only* text and comment sibling nodes.  Ignore comment
     nodes and collapse text nodes into one string.  Return NULL if
     expectations are unsatisfied.

     If there are a lot of text sub-nodes, this algorithm may be
     inefficient because it'll reallocate a lot.

  */

  gboolean ok = TRUE;
  xmlNodePtr current;
  gchar *result = g_strdup("");
  gchar *temp;

  for(current = tree; current; current = current->next) {
    switch(current->type) {
    case XML_TEXT_NODE:
      temp = g_strconcat(result, (gchar *) current->content, NULL);
      g_free(result);
      result = temp;
      break;
    case XML_COMMENT_NODE:
      break;
    default:
      PERR("dom_tree_to_text: hit illegal node type while extracting text.");
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
    gchar *content = dom_tree_to_text(node->xmlChildrenNode);
    gnc_numeric *ret;
    if(!content)
        return NULL;

    ret = g_new(gnc_numeric, 1);

    if(string_to_gnc_numeric(content, ret) != NULL)
    {
        free(content);
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
          gchar *content = dom_tree_to_text(n->xmlChildrenNode);
          if(!content)
          {
              return timespec_failure(ret);
          }
          
          if(!string_to_timespec_secs(content, ret)) {
            free(content);
            return timespec_failure(ret);
          }
          free(content);
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
          gchar *content = dom_tree_to_text(n->xmlChildrenNode);
          if(!content)
          {
              return timespec_failure(ret);
          }
          
          if(!string_to_timespec_nsecs(content, ret)) {
            free(content);
            return timespec_failure(ret);
          }
          free(content);
          seen_ns = TRUE;
        }
      }
      break;
    default:
      PERR("dom_tree_to_timespec: unexpected sub-node.");
      return timespec_failure(ret);
      break;
    }
  }

  if(!seen_s)
  {
      g_warning("dom_tree_to_timespec: no ts:date node found.");
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
          gchar *content = dom_tree_to_text(n->xmlChildrenNode);
          if(!content) return NULL;
          space_str = content;
        }
      } else if(safe_strcmp("cmdty:id", n->name) == 0) {
        if(id_str) {
          return NULL;
        } else {
          gchar *content = dom_tree_to_text(n->xmlChildrenNode);
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

  if(space_str) g_free(space_str);
  if(id_str) g_free(id_str);
  return c;
}

gnc_commodity*
dom_tree_to_commodity_ref(xmlNodePtr node)
{
    gnc_commodity* daref;
    gnc_commodity *ret;
    
    daref = dom_tree_to_commodity_ref_no_engine(node);
    ret = associate_commodity_ref_with_engine_commodity(daref);

    g_free(daref);

    return ret;
}

gnc_commodity *
associate_commodity_ref_with_engine_commodity(gnc_commodity *com)
{
    return gnc_commodity_table_lookup(gnc_engine_commodities(),
                                      gnc_commodity_get_namespace(com),
                                      gnc_commodity_get_exchange_code(com));
}
