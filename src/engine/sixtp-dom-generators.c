#include "config.h"

#include <glib.h>

#include "gnc-xml-helper.h"

#include "sixtp-dom-generators.h"
#include "GNCId.h"


xmlNodePtr
guid_to_dom_tree(GUID* gid)
{
    char *guid_str;
    xmlNodePtr ret;
    
    ret = xmlNewNode(NULL, "test-guid");

    xmlSetProp(ret, "type", "guid");

    guid_str = guid_to_string(gid);
    if (!guid_str)
    {
        printf("FAILURE: guid_to_string failed\n");
        return NULL;
    }
        
    xmlNodeAddContent(ret, guid_str);

    return ret;
}
