/********************************************************************\
 * gnc-invoice-xml-v2.c -- invoice xml i/o implementation         *
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

#include "gncBillTermP.h"
#include "gncInvoiceP.h"
#include "gnc-invoice-xml-v2.h"
#include "gnc-owner-xml-v2.h"
#include "gnc-engine-util.h"

#include "gncObject.h"

#define _GNC_MOD_NAME	GNC_INVOICE_MODULE_NAME

static short module = MOD_IO;

const gchar *invoice_version_string = "2.0.0";

/* ids */
#define gnc_invoice_string "gnc:GncInvoice"
#define invoice_guid_string "invoice:guid"
#define invoice_id_string "invoice:id"
#define invoice_owner_string "invoice:owner"
#define invoice_opened_string "invoice:opened"
#define invoice_posted_string "invoice:posted"
#define invoice_terms_string "invoice:terms"
#define invoice_billing_id_string "invoice:billing_id"
#define invoice_notes_string "invoice:notes"
#define invoice_active_string "invoice:active"
#define invoice_posttxn_string "invoice:posttxn"
#define invoice_postlot_string "invoice:postlot"
#define invoice_postacc_string "invoice:postacc"
#define invoice_commodity_string "invoice:commodity"
#define invoice_billto_string "invoice:billto"

static void
maybe_add_string (xmlNodePtr ptr, const char *tag, const char *str)
{
  if (str && strlen(str) > 0)
    xmlAddChild (ptr, text_to_dom_tree (tag, str));
}

static void
maybe_add_timespec (xmlNodePtr ptr, const char *tag, Timespec ts)
{
  if (ts.tv_sec || ts.tv_nsec)
    xmlAddChild (ptr, timespec_to_dom_tree (tag, &ts));
}

static xmlNodePtr
invoice_dom_tree_create (GncInvoice *invoice)
{
    xmlNodePtr ret;
    Timespec ts;
    Transaction *txn;
    GNCLot *lot;
    Account *acc;
    GncBillTerm *term;
    GncOwner *billto;

    ret = xmlNewNode(NULL, gnc_invoice_string);
    xmlSetProp(ret, "version", invoice_version_string);

    xmlAddChild(ret, guid_to_dom_tree(invoice_guid_string,
				      gncInvoiceGetGUID (invoice)));

    xmlAddChild(ret, text_to_dom_tree(invoice_id_string,
                                      gncInvoiceGetID (invoice)));

    xmlAddChild(ret, gnc_owner_to_dom_tree (invoice_owner_string,
					    gncInvoiceGetOwner (invoice)));

    ts = gncInvoiceGetDateOpened (invoice);
    xmlAddChild(ret, timespec_to_dom_tree (invoice_opened_string, &ts));

    maybe_add_timespec (ret, invoice_posted_string,
			gncInvoiceGetDatePosted (invoice));
    
    term = gncInvoiceGetTerms (invoice);
    if (term)
      xmlAddChild(ret, guid_to_dom_tree(invoice_terms_string,
					gncBillTermGetGUID (term)));
      
    maybe_add_string (ret, invoice_billing_id_string,
		      gncInvoiceGetBillingID (invoice));
    maybe_add_string (ret, invoice_notes_string, gncInvoiceGetNotes (invoice));

    xmlAddChild(ret, int_to_dom_tree(invoice_active_string,
				     gncInvoiceGetActive (invoice)));

    txn = gncInvoiceGetPostedTxn (invoice);
    if (txn)
      xmlAddChild (ret, guid_to_dom_tree (invoice_posttxn_string,
					  xaccTransGetGUID (txn)));

    lot = gncInvoiceGetPostedLot (invoice);
    if (lot)
      xmlAddChild (ret, guid_to_dom_tree (invoice_postlot_string,
					  gnc_lot_get_guid (lot)));

    acc = gncInvoiceGetPostedAcc (invoice);
    if (acc)
      xmlAddChild (ret, guid_to_dom_tree (invoice_postacc_string,
					  xaccAccountGetGUID (acc)));

    xmlAddChild
      (ret,
       commodity_ref_to_dom_tree(invoice_commodity_string,
				 gncInvoiceGetCommonCommodity (invoice)));

    billto = gncInvoiceGetBillTo (invoice);
    if (billto && billto->owner.undefined != NULL)
      xmlAddChild (ret, gnc_owner_to_dom_tree (invoice_billto_string, billto));

    return ret;
}

/***********************************************************************/

struct invoice_pdata
{
  GncInvoice *invoice;
  GNCBook *book;
};

static gboolean
set_string(xmlNodePtr node, GncInvoice* invoice,
           void (*func)(GncInvoice *invoice, const char *txt))
{
  char* txt = dom_tree_to_text(node);
  g_return_val_if_fail(txt, FALSE);
    
  func(invoice, txt);
  
  g_free(txt);
  return TRUE;
}

static gboolean
set_timespec(xmlNodePtr node, GncInvoice* invoice,
           void (*func)(GncInvoice *invoice, Timespec ts))
{
  Timespec* ts = dom_tree_to_timespec(node);
  g_return_val_if_fail(ts, FALSE);
    
  func(invoice, *ts);

  g_free(ts);
  return TRUE;
}

static gboolean
invoice_guid_handler (xmlNodePtr node, gpointer invoice_pdata)
{
    struct invoice_pdata *pdata = invoice_pdata;
    GUID *guid;
    GncInvoice *invoice;

    guid = dom_tree_to_guid(node);
    g_return_val_if_fail (guid, FALSE);
    invoice = gncInvoiceLookup (pdata->book, guid);
    if (invoice) {
      gncInvoiceDestroy (pdata->invoice);
      pdata->invoice = invoice;
    } else {
      gncInvoiceSetGUID(pdata->invoice, guid);
    }

    g_free(guid);
    
    return TRUE;
}

static gboolean
invoice_id_handler (xmlNodePtr node, gpointer invoice_pdata)
{
    struct invoice_pdata *pdata = invoice_pdata;

    return set_string(node, pdata->invoice, gncInvoiceSetID);
}

static gboolean
invoice_owner_handler (xmlNodePtr node, gpointer invoice_pdata)
{
  struct invoice_pdata *pdata = invoice_pdata;
  GncOwner owner;
  gboolean ret;

  ret = gnc_dom_tree_to_owner (node, &owner, pdata->book);
  if (ret)
    gncInvoiceSetOwner (pdata->invoice, &owner);

  return ret;
}

static gboolean
invoice_opened_handler (xmlNodePtr node, gpointer invoice_pdata)
{
    struct invoice_pdata *pdata = invoice_pdata;

    return set_timespec (node, pdata->invoice, gncInvoiceSetDateOpened);
}

static gboolean
invoice_posted_handler (xmlNodePtr node, gpointer invoice_pdata)
{
    struct invoice_pdata *pdata = invoice_pdata;

    return set_timespec (node, pdata->invoice, gncInvoiceSetDatePosted);
}

static gboolean
invoice_billing_id_handler (xmlNodePtr node, gpointer invoice_pdata)
{
    struct invoice_pdata *pdata = invoice_pdata;

    return set_string(node, pdata->invoice, gncInvoiceSetBillingID);
}

static gboolean
invoice_notes_handler (xmlNodePtr node, gpointer invoice_pdata)
{
    struct invoice_pdata *pdata = invoice_pdata;

    return set_string(node, pdata->invoice, gncInvoiceSetNotes);
}

static gboolean
invoice_active_handler (xmlNodePtr node, gpointer invoice_pdata)
{
    struct invoice_pdata *pdata = invoice_pdata;
    gint64 val;
    gboolean ret;

    ret = dom_tree_to_integer(node, &val);
    if (ret)
      gncInvoiceSetActive(pdata->invoice, (gboolean)val);

    return ret;
}

static gboolean
invoice_terms_handler (xmlNodePtr node, gpointer invoice_pdata)
{
    struct invoice_pdata *pdata = invoice_pdata;
    GUID *guid;
    GncBillTerm *term;

    guid = dom_tree_to_guid(node);
    g_return_val_if_fail (guid, FALSE);
    term = gncBillTermLookup (pdata->book, guid);
    if (!term) {
      term = gncBillTermCreate (pdata->book);
      gncBillTermSetGUID (term, guid);
    } else
      gncBillTermDecRef (term);

    g_free (guid);
    gncInvoiceSetTerms (pdata->invoice, term);
    
    return TRUE;
}

static gboolean
invoice_posttxn_handler (xmlNodePtr node, gpointer invoice_pdata)
{
    struct invoice_pdata *pdata = invoice_pdata;
    GUID *guid;
    Transaction *txn;

    guid = dom_tree_to_guid(node);
    g_return_val_if_fail (guid, FALSE);
    txn = xaccTransLookup (guid, pdata->book);
    g_free (guid);
    g_return_val_if_fail (txn, FALSE);

    gncInvoiceSetPostedTxn (pdata->invoice, txn);
    return TRUE;
}

static gboolean
invoice_postlot_handler (xmlNodePtr node, gpointer invoice_pdata)
{
    struct invoice_pdata *pdata = invoice_pdata;
    GUID *guid;
    GNCLot *lot;

    guid = dom_tree_to_guid(node);
    g_return_val_if_fail (guid, FALSE);
    lot = gnc_lot_lookup (guid, pdata->book);
    g_free (guid);
    g_return_val_if_fail (lot, FALSE);

    gncInvoiceSetPostedLot (pdata->invoice, lot);
    return TRUE;
}

static gboolean
invoice_postacc_handler (xmlNodePtr node, gpointer invoice_pdata)
{
    struct invoice_pdata *pdata = invoice_pdata;
    GUID *guid;
    Account *acc;

    guid = dom_tree_to_guid(node);
    g_return_val_if_fail (guid, FALSE);
    acc = xaccAccountLookup (guid, pdata->book);
    g_free (guid);
    g_return_val_if_fail (acc, FALSE);

    gncInvoiceSetPostedAcc (pdata->invoice, acc);
    return TRUE;
}

static gboolean
invoice_commodity_handler (xmlNodePtr node, gpointer invoice_pdata)
{
    struct invoice_pdata *pdata = invoice_pdata;
    gnc_commodity *com;

    com = dom_tree_to_commodity_ref(node, pdata->book);
    g_return_val_if_fail (com, FALSE);

    gncInvoiceSetCommonCommodity (pdata->invoice, com);

    return TRUE;
}

static gboolean
invoice_billto_handler (xmlNodePtr node, gpointer invoice_pdata)
{
  struct invoice_pdata *pdata = invoice_pdata;
  GncOwner owner;
  gboolean ret;

  ret = gnc_dom_tree_to_owner (node, &owner, pdata->book);
  if (ret)
    gncInvoiceSetBillTo (pdata->invoice, &owner);

  return ret;
}

static struct dom_tree_handler invoice_handlers_v2[] = {
    { invoice_guid_string, invoice_guid_handler, 1, 0 },
    { invoice_id_string, invoice_id_handler, 1, 0 },
    { invoice_owner_string, invoice_owner_handler, 1, 0 },
    { invoice_opened_string, invoice_opened_handler, 1, 0 },
    { invoice_posted_string, invoice_posted_handler, 0, 0 },
    { invoice_billing_id_string, invoice_billing_id_handler, 0, 0 },
    { invoice_notes_string, invoice_notes_handler, 0, 0 },
    { invoice_active_string, invoice_active_handler, 1, 0 },
    { invoice_terms_string, invoice_terms_handler, 0, 0 },
    { invoice_posttxn_string, invoice_posttxn_handler, 0, 0 },
    { invoice_postlot_string, invoice_postlot_handler, 0, 0 },
    { invoice_postacc_string, invoice_postacc_handler, 0, 0 },
    { invoice_commodity_string, invoice_commodity_handler, 1, 0 },
    { invoice_billto_string, invoice_billto_handler, 0, 0 },
    { NULL, 0, 0, 0 }
};

static GncInvoice*
dom_tree_to_invoice (xmlNodePtr node, GNCBook *book)
{
    struct invoice_pdata invoice_pdata;
    gboolean successful;

    invoice_pdata.invoice = gncInvoiceCreate(book);
    invoice_pdata.book = book;

    successful = dom_tree_generic_parse (node, invoice_handlers_v2,
                                         &invoice_pdata);
    gncInvoiceCommitEdit (invoice_pdata.invoice);

    if (!successful)
    {
        PERR ("failed to parse invoice tree");
        gncInvoiceDestroy (invoice_pdata.invoice);
        invoice_pdata.invoice = NULL;
    }

    return invoice_pdata.invoice;
}

static gboolean
gnc_invoice_end_handler(gpointer data_for_children,
			 GSList* data_from_children, GSList* sibling_data,
			 gpointer parent_data, gpointer global_data,
			 gpointer *result, const gchar *tag)
{
    int successful;
    GncInvoice *invoice;
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

    invoice = dom_tree_to_invoice(tree, book);
    if(invoice != NULL)
    {
        gdata->cb(tag, gdata->parsedata, invoice);
    }

    xmlFreeNode(tree);

    return invoice != NULL;
}

static sixtp *
invoice_sixtp_parser_create(void)
{
  return sixtp_dom_parser_new(gnc_invoice_end_handler, NULL, NULL);
}

static void
do_count (gpointer invoice_p, gpointer count_p)
{
  int *count = count_p;
  (*count)++;
}

static int
invoice_get_count (GNCBook *book)
{
  int count = 0;
  gncObjectForeach (_GNC_MOD_NAME, book, do_count, (gpointer) &count);
  return count;
}

static void
xml_add_invoice (gpointer invoice_p, gpointer out_p)
{
  xmlNodePtr node;
  GncInvoice *invoice = invoice_p;
  FILE *out = out_p;

  node = invoice_dom_tree_create (invoice);
  xmlElemDump(out, NULL, node);
  fprintf(out, "\n");
  xmlFreeNode (node);
}

static void
invoice_write (FILE *out, GNCBook *book)
{
  gncObjectForeach (_GNC_MOD_NAME, book, xml_add_invoice, (gpointer) out);
}

void
gnc_invoice_xml_initialize (void)
{
  static GncXmlDataType_t be_data = {
    GNC_FILE_BACKEND_VERS,
    gnc_invoice_string,
    invoice_sixtp_parser_create,
    NULL,			/* add_item */
    invoice_get_count,
    invoice_write,
  };

  gncObjectRegisterBackend (_GNC_MOD_NAME,
			    GNC_FILE_BACKEND,
			    &be_data);
}
