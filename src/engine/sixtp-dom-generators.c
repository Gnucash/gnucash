#include "config.h"

#include <glib.h>

#include "gnc-xml-helper.h"

#include "sixtp-dom-generators.h"
#include "sixtp-utils.h"

#include "GNCId.h"


xmlNodePtr
guid_to_dom_tree(const char *tag, GUID* gid)
{
    char *guid_str;
    xmlNodePtr ret;
    
    ret = xmlNewNode(NULL, tag);

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

xmlNodePtr
commodity_ref_to_dom_tree(const char *tag, const gnc_commodity *c)
{
    xmlNodePtr ret;

    g_return_val_if_fail(c, NULL);
    
    ret = xmlNewNode(NULL, tag);

    xmlNewTextChild(ret, NULL, "cmdty:space", gnc_commodity_get_namespace(c));
    xmlNewTextChild(ret, NULL, "cmdty:id", gnc_commodity_get_mnemonic(c));

    return ret;
}

gchar *
timespec_sec_to_string(const Timespec *ts)
{
    gchar *ret;
    struct tm parsed_time;
    time_t tmp_time;
    
    ret = g_new(gchar, 512);

    tmp_time = ts->tv_sec;
    
    if(!localtime_r(&tmp_time, &parsed_time))
    {
        g_free(ret);
        return NULL;
    }
    
    if(strftime(ret, 512, TIMESPEC_TIME_FORMAT, &parsed_time) == 0)
    {
        g_free(ret);
        return NULL;
    }
    
    return ret;
}

gchar *
timespec_nsec_to_string(const Timespec *ts)
{
    gchar *ret;

    ret = g_new(gchar, 22);

    g_snprintf(ret, 22, "%ld", ts->tv_nsec);

    return ret;
}

xmlNodePtr
timespec_to_dom_tree(const char *tag, const Timespec *spec)
{
    xmlNodePtr ret;
    gchar *date_str;
    gchar *ns_str;
    
    g_return_val_if_fail(spec, NULL);
    

    date_str = timespec_sec_to_string(spec);
    ns_str = timespec_nsec_to_string(spec);

    if((!date_str && !ns_str) || !date_str)
    {
        if(ns_str)
        {
            g_free(ns_str);
        }
        return NULL;
    }
    
    ret = xmlNewNode(NULL, tag);
    
    xmlNewTextChild(ret, NULL, "ts:date", date_str);

    if(ns_str)
    {
        xmlNewTextChild(ret, NULL, "ts:ns", ns_str);
    }

    g_free(date_str);
    g_free(ns_str);
    
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

    ret = xmlNewNode(NULL, tag);

    xmlNodeAddContent(ret, numstr);

    g_free(numstr);

    return ret;
}
