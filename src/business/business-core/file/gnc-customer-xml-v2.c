/********************************************************************\
 * gnc-customer-xml-v2.c -- customer xml i/o implementation         *
 *                                                                  *
 * Copyright (C) 2002 Derek Atkins <warlord@MIT.EDU>                *
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
\********************************************************************/

#include "config.h"

#include <glib.h>
#include <stdlib.h>
#include <string.h>

#include "gnc-xml-helper.h"

#include "sixtp.h"
#include "sixtp-utils.h"
#include "sixtp-parsers.h"
#include "sixtp-utils.h"
#include "sixtp-dom-parsers.h"
#include "sixtp-dom-generators.h"

#include "gnc-xml.h"
#include "io-gncxml-gen.h"
#include "io-gncxml-v2.h"

#include "gncCustomerP.h"
#include "gnc-customer-xml-v2.h"
#include "gnc-address-xml-v2.h"
#include "gnc-engine-util.h"

#include "gncObject.h"

#define _GNC_MOD_NAME	GNC_CUSTOMER_MODULE_NAME

static short module = MOD_IO;

const gchar *customer_version_string = "2.0.0";

/* ids */
#define gnc_customer_string "gnc:GncCustomer"
#define cust_name_string "cust:name"
#define cust_guid_string "cust:guid"
#define cust_id_string "cust:id"
#define cust_addr_string "cust:addr"
#define cust_shipaddr_string "cust:shipaddr"
#define cust_notes_string "cust:notes"
#define cust_terms_string "cust:terms"
#define cust_taxincluded_string "cust:taxincluded"
#define cust_active_string "cust:active"
#define cust_discount_string "cust:discount"
#define cust_credit_string "cust:credit"
#define cust_commodity_string "cust:commodity"

static void
maybe_add_string (xmlNodePtr ptr, const char *tag, const char *str)
{
  if (str && strlen(str) > 0)
    xmlAddChild (ptr, text_to_dom_tree (tag, str));
}

static xmlNodePtr
customer_dom_tree_create (GncCustomer *cust)
{
    xmlNodePtr ret;
    gnc_numeric num;

    ret = xmlNewNode(NULL, gnc_customer_string);
    xmlSetProp(ret, "version", customer_version_string);

    xmlAddChild(ret, guid_to_dom_tree(cust_guid_string,
				      gncCustomerGetGUID (cust)));

    xmlAddChild(ret, text_to_dom_tree(cust_name_string,
                                      gncCustomerGetName (cust)));
    
    xmlAddChild(ret, text_to_dom_tree(cust_id_string,
                                      gncCustomerGetID (cust)));

    xmlAddChild(ret, gnc_address_to_dom_tree(cust_addr_string,
					     gncCustomerGetAddr (cust)));
    
    xmlAddChild(ret, gnc_address_to_dom_tree(cust_shipaddr_string,
					     gncCustomerGetShipAddr (cust)));
    
    maybe_add_string (ret, cust_notes_string, gncCustomerGetNotes (cust));
    maybe_add_string (ret, cust_terms_string, gncCustomerGetTerms (cust));

    xmlAddChild(ret, int_to_dom_tree(cust_taxincluded_string,
				     gncCustomerGetTaxIncluded (cust)));

    xmlAddChild(ret, int_to_dom_tree(cust_active_string,
				     gncCustomerGetActive (cust)));

    num = gncCustomerGetDiscount (cust);
    xmlAddChild(ret, gnc_numeric_to_dom_tree(cust_discount_string, &num));
					     
    num = gncCustomerGetCredit (cust);
    xmlAddChild(ret, gnc_numeric_to_dom_tree(cust_credit_string, &num));

    xmlAddChild
      (ret,
       commodity_ref_to_dom_tree(cust_commodity_string,
				 gncCustomerGetCommodity (cust)));

    return ret;
}

/***********************************************************************/

struct customer_pdata
{
  GncCustomer *customer;
  GNCBook *book;
};

static gboolean
set_string(xmlNodePtr node, GncCustomer* cust,
           void (*func)(GncCustomer *cust, const char *txt))
{
  char* txt = dom_tree_to_text(node);
  g_return_val_if_fail(txt, FALSE);
    
  func(cust, txt);
  
  g_free(txt);
    
  return TRUE;
}

static gboolean
customer_name_handler (xmlNodePtr node, gpointer cust_pdata)
{
    struct customer_pdata *pdata = cust_pdata;

    return set_string(node, pdata->customer, gncCustomerSetName);
}

static gboolean
customer_guid_handler (xmlNodePtr node, gpointer cust_pdata)
{
    struct customer_pdata *pdata = cust_pdata;
    GUID *guid;
    GncCustomer *cust;

    guid = dom_tree_to_guid(node);
    g_return_val_if_fail(guid, FALSE);
    cust = gncCustomerLookup (pdata->book, guid);
    if (cust) {
      gncCustomerDestroy (pdata->customer);
      pdata->customer = cust;
    } else {
      gncCustomerSetGUID(pdata->customer, guid);
    }

    g_free(guid);
    
    return TRUE;
}

static gboolean
customer_id_handler (xmlNodePtr node, gpointer cust_pdata)
{
    struct customer_pdata *pdata = cust_pdata;

    return set_string(node, pdata->customer, gncCustomerSetID);
}

static gboolean
customer_notes_handler (xmlNodePtr node, gpointer cust_pdata)
{
    struct customer_pdata *pdata = cust_pdata;

    return set_string(node, pdata->customer, gncCustomerSetNotes);
}

static gboolean
customer_terms_handler (xmlNodePtr node, gpointer cust_pdata)
{
    struct customer_pdata *pdata = cust_pdata;

    return set_string(node, pdata->customer, gncCustomerSetTerms);
}

static gboolean
customer_addr_handler (xmlNodePtr node, gpointer cust_pdata)
{
    struct customer_pdata *pdata = cust_pdata;

    return gnc_dom_tree_to_address (node, gncCustomerGetAddr(pdata->customer));
}

static gboolean
customer_shipaddr_handler (xmlNodePtr node, gpointer cust_pdata)
{
    struct customer_pdata *pdata = cust_pdata;

    return gnc_dom_tree_to_address (node,
				    gncCustomerGetShipAddr(pdata->customer));
}


static gboolean
customer_taxincluded_handler (xmlNodePtr node, gpointer cust_pdata)
{
    struct customer_pdata *pdata = cust_pdata;
    gint64 val;
    gboolean ret;

    ret = dom_tree_to_integer(node, &val);
    if (ret)
      gncCustomerSetTaxIncluded(pdata->customer, (gboolean)val);

    return ret;
}

static gboolean
customer_active_handler (xmlNodePtr node, gpointer cust_pdata)
{
    struct customer_pdata *pdata = cust_pdata;
    gint64 val;
    gboolean ret;

    ret = dom_tree_to_integer(node, &val);
    if (ret)
      gncCustomerSetActive(pdata->customer, (gboolean)val);

    return ret;
}

static gboolean
customer_discount_handler (xmlNodePtr node, gpointer cust_pdata)
{
    struct customer_pdata *pdata = cust_pdata;
    gnc_numeric *val;

    val = dom_tree_to_gnc_numeric(node);
    g_return_val_if_fail(val, FALSE);

    gncCustomerSetDiscount(pdata->customer, *val);
    g_free (val);

    return TRUE;
}

static gboolean
customer_credit_handler (xmlNodePtr node, gpointer cust_pdata)
{
    struct customer_pdata *pdata = cust_pdata;
    gnc_numeric *val;

    val = dom_tree_to_gnc_numeric(node);
    g_return_val_if_fail(val, FALSE);

    gncCustomerSetCredit(pdata->customer, *val);
    g_free (val);

    return TRUE;
}

static gboolean
customer_commodity_handler (xmlNodePtr node, gpointer customer_pdata)
{
    struct customer_pdata *pdata = customer_pdata;
    gnc_commodity *com;

    com = dom_tree_to_commodity_ref_no_engine(node, pdata->book);
    g_return_val_if_fail (com, FALSE);

    gncCustomerSetCommodity (pdata->customer, com);

    return TRUE;
}

static struct dom_tree_handler customer_handlers_v2[] = {
    { cust_name_string, customer_name_handler, 1, 0 },
    { cust_guid_string, customer_guid_handler, 1, 0 },
    { cust_id_string, customer_id_handler, 1, 0 },
    { cust_addr_string, customer_addr_handler, 1, 0 },
    { cust_shipaddr_string, customer_shipaddr_handler, 1, 0 },
    { cust_notes_string, customer_notes_handler, 0, 0 },
    { cust_terms_string, customer_terms_handler, 0, 0 },
    { cust_taxincluded_string, customer_taxincluded_handler, 1, 0 },
    { cust_active_string, customer_active_handler, 1, 0 },
    { cust_discount_string, customer_discount_handler, 1, 0 },
    { cust_credit_string, customer_credit_handler, 1, 0 },
    { cust_commodity_string, customer_commodity_handler, 1, 0 },
    { NULL, 0, 0, 0 }
};

static GncCustomer*
dom_tree_to_customer (xmlNodePtr node, GNCBook *book)
{
    struct customer_pdata cust_pdata;
    GncCustomer *custToRet;
    gboolean successful;

    custToRet = gncCustomerCreate(book);

    cust_pdata.customer = custToRet;
    cust_pdata.book = book;

    successful = dom_tree_generic_parse (node, customer_handlers_v2,
                                         &cust_pdata);
    gncCustomerCommitEdit (custToRet);

    if (!successful)
    {
        PERR ("failed to parse customer tree");
        gncCustomerDestroy (custToRet);
        custToRet = NULL;
    }

    return custToRet;
}

static gboolean
gnc_customer_end_handler(gpointer data_for_children,
			 GSList* data_from_children, GSList* sibling_data,
			 gpointer parent_data, gpointer global_data,
			 gpointer *result, const gchar *tag)
{
    int successful;
    GncCustomer *cust;
    xmlNodePtr tree = (xmlNodePtr)data_for_children;
    gxpf_data *gdata = (gxpf_data*)global_data;
    GNCBook *book = gdata->bookdata;

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

    cust = dom_tree_to_customer(tree, book);
    if(cust != NULL)
    {
        gdata->cb(tag, gdata->parsedata, cust);
    }

    xmlFreeNode(tree);

    return cust != NULL;
}

static sixtp *
customer_sixtp_parser_create(void)
{
  return sixtp_dom_parser_new(gnc_customer_end_handler, NULL, NULL);
}

static void
do_count (gpointer cust_p, gpointer count_p)
{
  int *count = count_p;
  (*count)++;
}

static int
customer_get_count (GNCBook *book)
{
  int count = 0;
  gncObjectForeach (_GNC_MOD_NAME, book, do_count, (gpointer) &count);
  return count;
}

static void
xml_add_customer (gpointer cust_p, gpointer out_p)
{
  xmlNodePtr node;
  GncCustomer *cust = cust_p;
  FILE *out = out_p;

  node = customer_dom_tree_create (cust);
  xmlElemDump(out, NULL, node);
  fprintf(out, "\n");
  xmlFreeNode (node);
}

static void
customer_write (FILE *out, GNCBook *book)
{
  gncObjectForeach (_GNC_MOD_NAME, book, xml_add_customer, (gpointer) out);
}

void
gnc_customer_xml_initialize (void)
{
  static GncXmlDataType_t be_data = {
    GNC_FILE_BACKEND_VERS,
    gnc_customer_string,
    customer_sixtp_parser_create,
    NULL,			/* add_item */
    customer_get_count,
    customer_write,
  };

  gncObjectRegisterBackend (_GNC_MOD_NAME,
			    GNC_FILE_BACKEND,
			    &be_data);
}
