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
#include "io-gncxml-v2.h"

#include "sixtp-dom-parsers.h"
#include "AccountP.h"
#include "Account.h"
#include "Group.h"

const gchar *account_version_string = "2.0.0";

/* ids */
const char *gnc_account_string = "gnc:account";
const char *act_name_string = "act:name";
const char *act_id_string = "act:id";
const char *act_type_string = "act:type";
const char *act_currency_string = "act:currency";
const char *act_code_string = "act:code";
const char *act_description_string = "act:description";
const char *act_security_string = "act:security";
const char *act_slots_string = "act:slots";
const char *act_parent_string = "act:parent";

xmlNodePtr
gnc_account_dom_tree_create(Account *act)
{
    xmlNodePtr ret;

    ret = xmlNewNode(NULL, gnc_account_string);
    xmlSetProp(ret, "version", account_version_string);

    xmlNewChild(ret, NULL, act_name_string, xaccAccountGetName(act));
    
    xmlAddChild(ret, guid_to_dom_tree(act_id_string, xaccAccountGetGUID(act)));
    
    xmlNewChild(ret, NULL, act_type_string,
                xaccAccountTypeEnumAsString(xaccAccountGetType(act)));

    xmlAddChild(ret, commodity_ref_to_dom_tree(act_currency_string,
                                            xaccAccountGetCurrency(act)));

    if(xaccAccountGetCode(act))
    {
        xmlNewChild(ret, NULL, act_code_string, xaccAccountGetCode(act));
    }

    if(xaccAccountGetDescription(act))
    {
        xmlNewChild(ret, NULL, act_description_string,
                    xaccAccountGetDescription(act));
    }
       
    if(xaccAccountGetSecurity(act))
    {
        xmlAddChild(ret, commodity_ref_to_dom_tree(act_security_string,
                                                xaccAccountGetSecurity(act)));
    }

    if(xaccAccountGetSlots(act))
    {
        xmlAddChild(ret, kvp_frame_to_dom_tree(act_slots_string,
                                            xaccAccountGetSlots(act)));
    }

    if(xaccAccountGetParentAccount(act))
    {
        xmlAddChild(ret, guid_to_dom_tree(
                     act_parent_string,
                     xaccAccountGetGUID(xaccAccountGetParentAccount(act))));
    }
    
    return ret;
}

/***********************************************************************/
static gboolean
account_name_handler (xmlNodePtr node, Account* act)
{
    gchar* txt;
    
    txt = dom_tree_to_text(node);
    g_return_val_if_fail(txt, FALSE);
    
    xaccAccountSetName(act, txt);

    g_free(txt);
    
    return TRUE;
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

    ref = dom_tree_to_commodity_ref_no_engine(node);
    xaccAccountSetCurrency(act, ref);

    return TRUE;
}

static gboolean
account_security_handler (xmlNodePtr node, Account* act)
{
    gnc_commodity *ref;
    ref = dom_tree_to_commodity_ref_no_engine(node);
    xaccAccountSetSecurity(act, ref);

    return TRUE;
}

static gboolean
account_slots_handler (xmlNodePtr node, Account* act)
{
    kvp_frame *frm  = dom_tree_to_kvp_frame(node);
    g_return_val_if_fail(frm, FALSE);
    
    xaccAccountSetSlots_nc(act, frm);
    
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

static gboolean
account_code_handler(xmlNodePtr node, Account* act)
{
    gchar* txt;
    
    txt = dom_tree_to_text(node);
    g_return_val_if_fail(txt, FALSE);
    
    xaccAccountSetCode(act, txt);

    g_free(txt);
    
    return TRUE;
    
}

static gboolean
account_description_handler(xmlNodePtr node, Account *act)
{
    gchar* txt;
    
    txt = dom_tree_to_text(node);
    g_return_val_if_fail(txt, FALSE);
    
    xaccAccountSetDescription(act, txt);

    g_free(txt);
    
    return TRUE;
    
}

struct dom_handlers
{
    const char *tag;

    gboolean (*handler) (xmlNodePtr, Account*);

    int required;
    int gotten;
};

static struct dom_handlers account_handlers_v2[] = {
    { "act:name", account_name_handler, 1, 0 },
    { "act:id", account_id_handler, 1, 0 },
    { "act:type", account_type_handler, 1, 0 },
    { "act:currency", account_currency_handler, 1, 0 },
    { "act:code", account_code_handler, 1, 0 },
    { "act:description", account_description_handler, 1, 0},
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
            g_warning("Not defined and it should be: %s", handler_ptr->tag);
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
            handler_ptr->gotten = TRUE;
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
    sixtp_gdv2 *gdata = (sixtp_gdv2*)global_data;
    
    successful = TRUE;

    if(parent_data)
    {
        return TRUE;
    }

    /* OK.  For some messed up reason this is getting called again with a
       NULL tag.  So we ignore those cases */
    if(!tag)
    {
        return TRUE;
    }
    
    g_return_val_if_fail(tree, FALSE);
    
    acc = xaccMallocAccount();
    g_return_val_if_fail(acc, FALSE);
    xaccAccountBeginEdit(acc);

    set_handlers(account_handlers_v2);

    for(achild = tree->xmlChildrenNode; achild; achild = achild->next)
    {
        if(!gnc_xml_set_account_data(achild->name, achild, acc,
                                     account_handlers_v2))
        {
            g_warning("gnc_xml_set_account_data failed");
            successful = FALSE;
            break;
        }
    }

    xaccAccountCommitEdit(acc);
    
    if(!all_required_gotten_p(account_handlers_v2))
    {
        g_warning("all_required_gotten_p failed");
        successful = FALSE;
    }

    if(!successful)
    {
        xaccFreeAccount(acc);
    }
    else
    {
        gdata->addAccountFunc(global_data, acc);
    }
    
    xmlFreeNode(tree);

    return successful;
}

sixtp*
gnc_account_sixtp_parser_create(void)
{
    return sixtp_dom_parser_new(gnc_account_end_handler, NULL, NULL);
}
