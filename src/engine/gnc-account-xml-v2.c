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

    xmlAddChild(ret, text_to_dom_tree(act_name_string, xaccAccountGetName(act)));
    
    xmlAddChild(ret, guid_to_dom_tree(act_id_string, xaccAccountGetGUID(act)));
    
    xmlAddChild(ret, text_to_dom_tree(
                    act_type_string,
                    xaccAccountTypeEnumAsString(xaccAccountGetType(act))));

    xmlAddChild(ret, commodity_ref_to_dom_tree(act_currency_string,
                                            xaccAccountGetCurrency(act)));

    if(xaccAccountGetCode(act) &&
        strlen(xaccAccountGetCode(act)) > 0)
    {
        xmlAddChild(ret, text_to_dom_tree(act_code_string,
                                          xaccAccountGetCode(act)));
    }

    if(xaccAccountGetDescription(act) &&
       strlen(xaccAccountGetDescription(act)) > 0)
    {
        xmlAddChild(ret, text_to_dom_tree(act_description_string,
                                          xaccAccountGetDescription(act)));
    }
       
    if(xaccAccountGetSecurity(act))
    {
        xmlAddChild(ret, commodity_ref_to_dom_tree(act_security_string,
                                                xaccAccountGetSecurity(act)));
    }

    if(xaccAccountGetSlots(act))
    {
        xmlNodePtr kvpnode = kvp_frame_to_dom_tree(act_slots_string,
                                                   xaccAccountGetSlots(act));
        if(kvpnode)
        {
            xmlAddChild(ret, kvpnode);
        }
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
set_string(xmlNodePtr node, Account* act,
           void (*func)(Account *act, const gchar *txt))
{
    gchar* txt = dom_tree_to_text(node);
    g_return_val_if_fail(txt, FALSE);
    
    func(act, txt);

    g_free(txt);
    
    return TRUE;
}

static gboolean
account_name_handler (xmlNodePtr node, gpointer act)
{
    return set_string(node, (Account*)act, xaccAccountSetName);
}

static gboolean
account_id_handler (xmlNodePtr node, gpointer act)
{
    GUID *guid;

    guid = dom_tree_to_guid(node);
    xaccAccountSetGUID((Account*)act, guid);

    g_free(guid);
    
    return TRUE;
}

static gboolean
account_type_handler (xmlNodePtr node, gpointer act)
{
    int type;

    xaccAccountStringToType(node->xmlChildrenNode->content, &type);
    xaccAccountSetType((Account*)act, type);
    return TRUE;
}

static gboolean
account_currency_handler (xmlNodePtr node, gpointer act)
{
    gnc_commodity *ref;

    ref = dom_tree_to_commodity_ref_no_engine(node);
    xaccAccountSetCurrency((Account*)act, ref);

    return TRUE;
}

static gboolean
account_security_handler (xmlNodePtr node, gpointer act)
{
    gnc_commodity *ref;
    ref = dom_tree_to_commodity_ref_no_engine(node);
    xaccAccountSetSecurity((Account*)act, ref);

    return TRUE;
}

static gboolean
account_slots_handler (xmlNodePtr node, gpointer act)
{
    kvp_frame *frm  = dom_tree_to_kvp_frame(node);
    g_return_val_if_fail(frm, FALSE);
    
    xaccAccountSetSlots_nc((Account*)act, frm);
    
    return TRUE;
}

static gboolean
account_parent_handler (xmlNodePtr node, gpointer act)
{
    Account *parent;
    GUID *gid = dom_tree_to_guid(node);

    parent = xaccAccountLookup(gid);

    xaccAccountInsertSubAccount(parent, (Account*)act);

    xaccGUIDFree(gid);
    return TRUE;
}

static gboolean
account_code_handler(xmlNodePtr node, gpointer act)
{
    return set_string(node, (Account*)act, xaccAccountSetCode);
}

static gboolean
account_description_handler(xmlNodePtr node, gpointer act)
{
    return set_string(node, (Account*)act, xaccAccountSetDescription);
}

static struct dom_tree_handler account_handlers_v2[] = {
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

    successful = dom_tree_generic_parse(tree, account_handlers_v2, acc);
    xaccAccountCommitEdit(acc);
    
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
