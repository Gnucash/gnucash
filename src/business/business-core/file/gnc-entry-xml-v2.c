/********************************************************************\
 * gnc-entry-xml-v2.c -- entry xml i/o implementation         *
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

#include "gncEntryP.h"
#include "gncOrderP.h"
#include "gncInvoiceP.h"
#include "gncTaxTableP.h"
#include "gnc-entry-xml-v2.h"
#include "gnc-owner-xml-v2.h"
#include "gnc-engine-util.h"

#include "gncObject.h"

#define _GNC_MOD_NAME	GNC_ENTRY_MODULE_NAME

static short module = MOD_IO;

const gchar *entry_version_string = "2.0.0";

/* ids */
#define gnc_entry_string "gnc:GncEntry"
#define entry_guid_string "entry:guid"
#define entry_date_string "entry:date"
#define entry_dateentered_string "entry:entered"
#define entry_description_string "entry:description"
#define entry_action_string "entry:action"
#define entry_notes_string "entry:notes"
#define entry_qty_string "entry:qty"
#define entry_price_string "entry:price"
#define entry_discount_string "entry:discount"
#define entry_disctype_string "entry:disc-type"
#define entry_dischow_string "entry:disc-how"
#define entry_acct_string "entry:acct"
#define entry_taxable_string "entry:taxable"
#define entry_taxincluded_string "entry:taxincluded"
#define entry_taxtable_string "entry:taxtable"
#define entry_order_string "entry:order"
#define entry_invoice_string "entry:invoice"

static void
maybe_add_string (xmlNodePtr ptr, const char *tag, const char *str)
{
  if (str && strlen(str) > 0)
    xmlAddChild (ptr, text_to_dom_tree (tag, str));
}

static void
maybe_add_numeric (xmlNodePtr ptr, const char *tag, gnc_numeric num)
{
  if (!gnc_numeric_zero_p (num))
    xmlAddChild (ptr, gnc_numeric_to_dom_tree (tag, &num));
}

static xmlNodePtr
entry_dom_tree_create (GncEntry *entry)
{
    xmlNodePtr ret;
    Timespec ts;
    Account *acc;
    GncTaxTable *taxtable;
    GncOrder *order;
    GncInvoice *invoice;

    ret = xmlNewNode(NULL, gnc_entry_string);
    xmlSetProp(ret, "version", entry_version_string);

    xmlAddChild(ret, guid_to_dom_tree(entry_guid_string,
				      gncEntryGetGUID (entry)));

    ts = gncEntryGetDate (entry);
    xmlAddChild(ret, timespec_to_dom_tree (entry_date_string, &ts));

    ts = gncEntryGetDateEntered (entry);
    xmlAddChild(ret, timespec_to_dom_tree (entry_dateentered_string, &ts));
    
    maybe_add_string (ret, entry_description_string,
		      gncEntryGetDescription (entry));
    maybe_add_string (ret, entry_action_string, gncEntryGetAction (entry));
    maybe_add_string (ret, entry_notes_string, gncEntryGetNotes (entry));

    maybe_add_numeric (ret, entry_qty_string, gncEntryGetQuantity (entry));
    maybe_add_numeric (ret, entry_price_string, gncEntryGetPrice (entry));

    maybe_add_numeric (ret, entry_discount_string, gncEntryGetDiscount (entry));
    xmlAddChild(ret, int_to_dom_tree(entry_disctype_string,
				     gncEntryGetDiscountType (entry)));
    xmlAddChild(ret, int_to_dom_tree(entry_dischow_string,
				     gncEntryGetDiscountHow (entry)));

    acc = gncEntryGetAccount (entry);
    if (acc)
      xmlAddChild (ret, guid_to_dom_tree (entry_acct_string,
					  xaccAccountGetGUID (acc)));

    xmlAddChild(ret, int_to_dom_tree(entry_taxable_string,
				     gncEntryGetTaxable (entry)));
    xmlAddChild(ret, int_to_dom_tree(entry_taxincluded_string,
				     gncEntryGetTaxIncluded (entry)));
    taxtable = gncEntryGetTaxTable (entry);
    if (taxtable)
      xmlAddChild (ret, guid_to_dom_tree (entry_taxtable_string,
					  gncTaxTableGetGUID (taxtable)));

    order = gncEntryGetOrder (entry);
    if (order)
      xmlAddChild (ret, guid_to_dom_tree (entry_order_string,
					  gncOrderGetGUID (order)));

    invoice = gncEntryGetInvoice (entry);
    if (invoice)
      xmlAddChild (ret, guid_to_dom_tree (entry_invoice_string,
					  gncInvoiceGetGUID (invoice)));

    return ret;
}

/***********************************************************************/

struct entry_pdata
{
  GncEntry *entry;
  GNCBook *book;
};

static gboolean
set_string(xmlNodePtr node, GncEntry* entry,
           void (*func)(GncEntry *entry, const char *txt))
{
  char* txt = dom_tree_to_text(node);
  g_return_val_if_fail(txt, FALSE);
    
  func(entry, txt);
  g_free(txt);
  return TRUE;
}

static gboolean
set_timespec(xmlNodePtr node, GncEntry* entry,
           void (*func)(GncEntry *entry, Timespec ts))
{
  Timespec *ts = dom_tree_to_timespec (node);
  g_return_val_if_fail(ts, FALSE);
    
  func(entry, *ts);
  g_free(ts);
  return TRUE;
}

static gboolean
set_numeric(xmlNodePtr node, GncEntry* entry,
           void (*func)(GncEntry *entry, gnc_numeric num))
{
  gnc_numeric* num = dom_tree_to_gnc_numeric(node);
  g_return_val_if_fail(num, FALSE);
    
  func(entry, *num);
  g_free(num);
  return TRUE;
}

static gboolean
entry_guid_handler (xmlNodePtr node, gpointer entry_pdata)
{
    struct entry_pdata *pdata = entry_pdata;
    GUID *guid;
    GncEntry *entry;

    guid = dom_tree_to_guid(node);
    g_return_val_if_fail (guid, FALSE);
    entry = gncEntryLookup (pdata->book, guid);
    if (entry) {
      gncEntryDestroy (pdata->entry);
      pdata->entry = entry;
    } else {
      gncEntrySetGUID(pdata->entry, guid);
    }

    g_free(guid);
    
    return TRUE;
}

static gboolean
entry_date_handler (xmlNodePtr node, gpointer entry_pdata)
{
    struct entry_pdata *pdata = entry_pdata;

    return set_timespec(node, pdata->entry, gncEntrySetDate);
}

static gboolean
entry_dateentered_handler (xmlNodePtr node, gpointer entry_pdata)
{
    struct entry_pdata *pdata = entry_pdata;

    return set_timespec(node, pdata->entry, gncEntrySetDateEntered);
}

static gboolean
entry_description_handler (xmlNodePtr node, gpointer entry_pdata)
{
    struct entry_pdata *pdata = entry_pdata;

    return set_string(node, pdata->entry, gncEntrySetDescription);
}

static gboolean
entry_action_handler (xmlNodePtr node, gpointer entry_pdata)
{
    struct entry_pdata *pdata = entry_pdata;

    return set_string(node, pdata->entry, gncEntrySetAction);
}

static gboolean
entry_notes_handler (xmlNodePtr node, gpointer entry_pdata)
{
    struct entry_pdata *pdata = entry_pdata;

    return set_string(node, pdata->entry, gncEntrySetNotes);
}

static gboolean
entry_qty_handler (xmlNodePtr node, gpointer entry_pdata)
{
    struct entry_pdata *pdata = entry_pdata;

    return set_numeric(node, pdata->entry, gncEntrySetQuantity);
}

static gboolean
entry_price_handler (xmlNodePtr node, gpointer entry_pdata)
{
    struct entry_pdata *pdata = entry_pdata;

    return set_numeric(node, pdata->entry, gncEntrySetPrice);
}

static gboolean
entry_discount_handler (xmlNodePtr node, gpointer entry_pdata)
{
    struct entry_pdata *pdata = entry_pdata;

    return set_numeric(node, pdata->entry, gncEntrySetDiscount);
}

static gboolean
entry_disctype_handler (xmlNodePtr node, gpointer entry_pdata)
{
    struct entry_pdata *pdata = entry_pdata;
    gint64 val;

    dom_tree_to_integer(node, &val);
    gncEntrySetDiscountType(pdata->entry, (gint)val);

    return TRUE;
}

static gboolean
entry_dischow_handler (xmlNodePtr node, gpointer entry_pdata)
{
    struct entry_pdata *pdata = entry_pdata;
    gint64 val;

    dom_tree_to_integer(node, &val);
    gncEntrySetDiscountHow(pdata->entry, (gint)val);

    return TRUE;
}

static gboolean
entry_acct_handler (xmlNodePtr node, gpointer entry_pdata)
{
    struct entry_pdata *pdata = entry_pdata;
    GUID *guid;
    Account * acc;

    guid = dom_tree_to_guid (node);
    g_return_val_if_fail (guid, FALSE);
    acc = xaccAccountLookup (guid, pdata->book);
    g_free (guid);
    g_return_val_if_fail (acc, FALSE);

    gncEntrySetAccount (pdata->entry, acc);
    return TRUE;
}

static gboolean
entry_taxable_handler (xmlNodePtr node, gpointer entry_pdata)
{
    struct entry_pdata *pdata = entry_pdata;
    gint64 val;

    dom_tree_to_integer(node, &val);
    gncEntrySetTaxable(pdata->entry, (gint)val);

    return TRUE;
}

static gboolean
entry_taxincluded_handler (xmlNodePtr node, gpointer entry_pdata)
{
    struct entry_pdata *pdata = entry_pdata;
    gint64 val;

    dom_tree_to_integer(node, &val);
    gncEntrySetTaxIncluded(pdata->entry, (gint)val);

    return TRUE;
}

static gboolean
entry_taxtable_handler (xmlNodePtr node, gpointer entry_pdata)
{
    struct entry_pdata *pdata = entry_pdata;
    GUID *guid;
    GncTaxTable *taxtable;

    guid = dom_tree_to_guid (node);
    g_return_val_if_fail (guid, FALSE);
    taxtable = gncTaxTableLookup (pdata->book, guid);
    if (!taxtable) {
      taxtable = gncTaxTableCreate (pdata->book);
      gncTaxTableSetGUID (taxtable, guid);
    } else
      gncTaxTableDecRef (taxtable);
    gncEntrySetTaxTable (pdata->entry, taxtable);

    g_free(guid);
    return TRUE;
}

static gboolean
entry_order_handler (xmlNodePtr node, gpointer entry_pdata)
{
    struct entry_pdata *pdata = entry_pdata;
    GUID *guid;
    GncOrder *order;

    guid = dom_tree_to_guid (node);
    g_return_val_if_fail (guid, FALSE);
    order = gncOrderLookup (pdata->book, guid);
    if (!order) {
      order = gncOrderCreate (pdata->book);
      gncOrderSetGUID (order, guid);
    }
    gncOrderAddEntry (order, pdata->entry);

    g_free(guid);
    return TRUE;
}

static gboolean
entry_invoice_handler (xmlNodePtr node, gpointer entry_pdata)
{
    struct entry_pdata *pdata = entry_pdata;
    GUID *guid;
    GncInvoice *invoice;

    guid = dom_tree_to_guid (node);
    g_return_val_if_fail (guid, FALSE);
    invoice = gncInvoiceLookup (pdata->book, guid);
    if (!invoice) {
      invoice = gncInvoiceCreate (pdata->book);
      gncInvoiceSetGUID (invoice, guid);
    }
    gncInvoiceAddEntry (invoice, pdata->entry);

    g_free(guid);
    return TRUE;
}

static struct dom_tree_handler entry_handlers_v2[] = {
    { entry_guid_string, entry_guid_handler, 1, 0 },
    { entry_date_string, entry_date_handler, 1, 0 },
    { entry_dateentered_string, entry_dateentered_handler, 1, 0 },
    { entry_description_string, entry_description_handler, 0, 0 },
    { entry_action_string, entry_action_handler, 0, 0 },
    { entry_notes_string, entry_notes_handler, 0, 0 },
    { entry_qty_string, entry_qty_handler, 0, 0 },
    { entry_price_string, entry_price_handler, 0, 0 },
    { entry_discount_string, entry_discount_handler, 0, 0 },
    { entry_disctype_string, entry_disctype_handler, 0, 0 },
    { entry_dischow_string, entry_dischow_handler, 0, 0 },
    { entry_acct_string, entry_acct_handler, 0, 0 },
    { entry_taxable_string, entry_taxable_handler, 0, 0 },
    { entry_taxincluded_string, entry_taxincluded_handler, 0, 0 },
    { entry_taxtable_string, entry_taxtable_handler, 0, 0 },
    { entry_order_string, entry_order_handler, 0, 0 },
    { entry_invoice_string, entry_invoice_handler, 0, 0 },
    { NULL, 0, 0, 0 }
};

static GncEntry*
dom_tree_to_entry (xmlNodePtr node, GNCBook *book)
{
    struct entry_pdata entry_pdata;
    gboolean successful;

    entry_pdata.entry = gncEntryCreate(book);
    entry_pdata.book = book;

    successful = dom_tree_generic_parse (node, entry_handlers_v2,
                                         &entry_pdata);
    gncEntryCommitEdit (entry_pdata.entry);

    if (!successful)
    {
        PERR ("failed to parse entry tree");
        gncEntryDestroy (entry_pdata.entry);
        entry_pdata.entry = NULL;
    }

    return entry_pdata.entry;
}

static gboolean
gnc_entry_end_handler(gpointer data_for_children,
			 GSList* data_from_children, GSList* sibling_data,
			 gpointer parent_data, gpointer global_data,
			 gpointer *result, const gchar *tag)
{
    int successful;
    GncEntry *entry;
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

    entry = dom_tree_to_entry(tree, book);
    if(entry != NULL)
    {
        gdata->cb(tag, gdata->parsedata, entry);
    }

    xmlFreeNode(tree);

    return entry != NULL;
}

static sixtp *
entry_sixtp_parser_create(void)
{
  return sixtp_dom_parser_new(gnc_entry_end_handler, NULL, NULL);
}

static void
do_count (gpointer entry_p, gpointer count_p)
{
  int *count = count_p;
  (*count)++;
}

static int
entry_get_count (GNCBook *book)
{
  int count = 0;
  gncObjectForeach (_GNC_MOD_NAME, book, do_count, (gpointer) &count);
  return count;
}

static void
xml_add_entry (gpointer entry_p, gpointer out_p)
{
  xmlNodePtr node;
  GncEntry *entry = entry_p;
  FILE *out = out_p;

  /* Don't save non-attached entries! */
  if (!(gncEntryGetOrder (entry) || gncEntryGetInvoice (entry)))
    return;

  node = entry_dom_tree_create (entry);
  xmlElemDump(out, NULL, node);
  fprintf(out, "\n");
  xmlFreeNode (node);
}

static void
entry_write (FILE *out, GNCBook *book)
{
  gncObjectForeach (_GNC_MOD_NAME, book, xml_add_entry, (gpointer) out);
}

void
gnc_entry_xml_initialize (void)
{
  static GncXmlDataType_t be_data = {
    GNC_FILE_BACKEND_VERS,
    gnc_entry_string,
    entry_sixtp_parser_create,
    NULL,			/* add_item */
    entry_get_count,
    entry_write,
  };

  gncObjectRegisterBackend (_GNC_MOD_NAME,
			    GNC_FILE_BACKEND,
			    &be_data);
}
