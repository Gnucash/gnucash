#include "config.h"

#include <glib.h>
#include <string.h>

#include "gnc-xml-helper.h"

#include "sixtp-dom-parsers.h"
#include "GNCId.h"

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

gnc_commodity*
dom_tree_to_gnc_commodity(xmlNodePtr node)
{

    return NULL;
}

gboolean
dom_tree_handle_kvp(kvp_frame* frame, xmlNodePtr node)
{

    return FALSE;
}

