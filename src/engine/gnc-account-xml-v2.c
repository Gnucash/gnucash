#include "config.h"

#include <glib.h>
#include <string.h>

#include "gnc-xml-helper.h"

#include "sixtp.h"
#include "sixtp-utils.h"
#include "sixtp-parsers.h"
#include "sixtp-utils.h"
#include "sixtp-dom-parsers.h"
#include "sixtp-dom-generators.h"

#include "gnc-xml.h"

#include "sixtp-dom-parsers.h"
#include "AccountP.h"
#include "Account.h"
#include "Group.h"

const gchar *account_version_string = "2.0.0";

xmlNodePtr
gnc_account_dom_tree_create(Account *act)
{
    xmlNodePtr ret;

    ret = xmlNewNode(NULL, "gnc:account");
    xmlSetProp(ret, "version", account_version_string);

    xmlNewChild(ret, NULL, "act:name", xaccAccountGetName(act));
    
    xmlAddChild(ret, guid_to_dom_tree("act:id", xaccAccountGetGUID(act)));
    
    xmlNewChild(ret, NULL, "act:type",
                xaccAccountTypeEnumAsString(xaccAccountGetType(act)));

    xmlAddChild(ret, commodity_ref_to_dom_tree("act:currency",
                                            xaccAccountGetCurrency(act)));

    if(xaccAccountGetCode(act))
    {
        xmlNewChild(ret, NULL, "act:code", xaccAccountGetCode(act));
    }

    if(xaccAccountGetDescription(act))
    {
        xmlNewChild(ret, NULL, "act:description",
                    xaccAccountGetDescription(act));
    }
       
    if(xaccAccountGetSecurity(act))
    {
        xmlAddChild(ret, commodity_ref_to_dom_tree("act:security",
                                                xaccAccountGetSecurity(act)));
    }

    if(xaccAccountGetSlots(act))
    {
        xmlAddChild(ret, kvp_frame_to_dom_tree("act:slots",
                                            xaccAccountGetSlots(act)));
    }

    if(xaccAccountGetParentAccount(act))
    {
        xmlAddChild(ret, guid_to_dom_tree(
                     "act:parent",
                     xaccAccountGetGUID(xaccAccountGetParentAccount(act))));
    }
    
    return ret;
}

/***********************************************************************/
static gboolean
account_name_handler (xmlNodePtr node, Account* act)
{
    if(node->content != NULL)
    {
        xaccAccountSetName(act, node->xmlChildrenNode->content);
        return TRUE;
    }
    else 
    {
        return FALSE;
    }
}

static gboolean
account_id_handler (xmlNodePtr node, Account* act)
{
    xaccAccountSetGUID(act, dom_tree_to_guid(node));
    return TRUE;
}

static gboolean
account_type_handler (xmlNodePtr node, Account* act)
{
    int type;

    xaccAccountStringToType(node->xmlChildrenNode->content, &type);
    xaccAccountSetType(act, type);
    return TRUE;
}

static gboolean
account_currency_handler (xmlNodePtr node, Account* act)
{
    gnc_commodity *ref;

    ref = dom_tree_to_commodity_ref(node);
    xaccAccountSetCurrency(
        act, associate_commodity_ref_with_engine_commodity(ref));
    gnc_commodity_destroy(ref);
    return TRUE;
}

static gboolean
account_security_handler (xmlNodePtr node, Account* act)
{
    gnc_commodity *ref;
    ref = dom_tree_to_commodity_ref(node);
    xaccAccountSetSecurity(
        act, associate_commodity_ref_with_engine_commodity(ref));
    gnc_commodity_destroy(ref);
    return TRUE;
}

static gboolean
account_slots_handler (xmlNodePtr node, Account* act)
{
    /* return dom_tree_handle_kvp(act->kvp_data, node); */
    return TRUE;
}

static gboolean
account_parent_handler (xmlNodePtr node, Account* act)
{
    Account *parent;
    GUID *gid = dom_tree_to_guid(node);

    parent = xaccAccountLookup(gid);

    xaccAccountInsertSubAccount(parent, act);

    xaccGUIDFree(gid);
    return TRUE;
}

struct dom_handlers
{
    char *tag;

    gboolean (*handler) (xmlNodePtr, Account*);

    int required;
    int gotten;
};

static struct dom_handlers account_handlers_v2[] = {
    { "act:name", account_name_handler, 1, 0 },
    { "act:id", account_id_handler, 1, 0 },
    { "act:type", account_type_handler, 1, 0 },
    { "act:currency", account_currency_handler, 1, 0 },
    { "act:security", account_security_handler, 0, 0 },
    { "act:slots", account_slots_handler, 0, 0 },
    { "act:parent", account_parent_handler, 0, 0 },
    { NULL, 0, 0, 0 }
};

static void
set_handlers(struct dom_handlers *handler_ptr)
{
    while(handler_ptr->tag != NULL)
    {
        handler_ptr->gotten = 0;

        handler_ptr++;
    }
}

static gboolean
all_required_gotten_p(struct dom_handlers *handler_ptr)
{
    while(handler_ptr->tag != NULL)
    {
        if(handler_ptr->required && ! handler_ptr->gotten)
        {
            return FALSE;
        }
        handler_ptr++;
    }
    return TRUE;
}
        
static gboolean
gnc_xml_set_account_data(const gchar* tag, xmlNodePtr node, Account *acc,
                         struct dom_handlers *handler_ptr)
{
    while(handler_ptr->tag)
    {
        if(strcmp(tag, handler_ptr->tag) == 0)
        {
            (handler_ptr->handler)(node, acc);
            break;
        }

        handler_ptr++;
    }
    
    if(!handler_ptr->tag) 
    {
        g_warning("Unhandled account tag: %s\n", tag);
        return FALSE;
    }

    return TRUE;
}

static gboolean
gnc_account_end_handler(gpointer data_for_children,
                        GSList* data_from_children, GSList* sibling_data,
                        gpointer parent_data, gpointer global_data,
                        gpointer *result, const gchar *tag)
{
    int successful;
    Account *acc;
    xmlNodePtr achild;
    xmlNodePtr tree = (xmlNodePtr)data_for_children;

    successful = TRUE;

    if(parent_data)
    {
        return successful;
    }
    
    acc = xaccMallocAccount();
    g_return_val_if_fail(acc, FALSE);
    xaccAccountBeginEdit(acc);

    set_handlers(account_handlers_v2);

    for(achild = tree->xmlChildrenNode; achild; achild = achild->next)
    {
        if(!gnc_xml_set_account_data(achild->name, achild, acc,
                                     account_handlers_v2))
        {
            successful = FALSE;
            break;
        }
    }

    xaccAccountCommitEdit(acc);
    
    if(!all_required_gotten_p(account_handlers_v2))
    {
        successful = FALSE;
    }

    if(!successful)
    {
        xaccFreeAccount(acc);
    }
    else
    {
        if(!xaccAccountGetParent(acc))
        {
            /* FIXME: something like this */
            /* xaccGroupInsertAccount(global_data, acc); */
        }
    }
    
    xmlFreeNode((xmlNodePtr) result);

    return successful;
}

sixtp*
gnc_account_sixtp_parser_create(void)
{
    return sixtp_dom_parser_new(gnc_account_end_handler, NULL, NULL);
}
