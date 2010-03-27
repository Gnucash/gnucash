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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
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

#define _GNC_MOD_NAME	GNC_ID_ENTRY

static QofLogModule log_module = GNC_MOD_IO;

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

/* cust inv */
#define entry_invacct_string "entry:i-acct"
#define entry_iprice_string "entry:i-price"
#define entry_idiscount_string "entry:i-discount"
#define entry_idisctype_string "entry:i-disc-type"
#define entry_idischow_string "entry:i-disc-how"
#define entry_itaxable_string "entry:i-taxable"
#define entry_itaxincluded_string "entry:i-taxincluded"
#define entry_itaxtable_string "entry:i-taxtable"

/* vend bill */
#define entry_billacct_string "entry:b-acct"
#define entry_bprice_string "entry:b-price"
#define entry_btaxable_string "entry:b-taxable"
#define entry_btaxincluded_string "entry:b-taxincluded"
#define entry_btaxtable_string "entry:b-taxtable"
#define entry_billable_string "entry:billable"
#define entry_billto_string "entry:billto"

/* emp bill */
#define entry_billpayment_string "entry:b-pay"

/* other stuff */
#define entry_order_string "entry:order"
#define entry_invoice_string "entry:invoice"
#define entry_bill_string "entry:bill"
#define entry_slots_string "entry:slots"

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

    ret = xmlNewNode(NULL, BAD_CAST gnc_entry_string);
    xmlSetProp(ret, BAD_CAST "version", BAD_CAST entry_version_string);

    xmlAddChild(ret, guid_to_dom_tree(entry_guid_string,
                                      qof_instance_get_guid(QOF_INSTANCE(entry))));

    ts = gncEntryGetDate (entry);
    xmlAddChild(ret, timespec_to_dom_tree (entry_date_string, &ts));

    ts = gncEntryGetDateEntered (entry);
    xmlAddChild(ret, timespec_to_dom_tree (entry_dateentered_string, &ts));

    maybe_add_string (ret, entry_description_string,
                      gncEntryGetDescription (entry));
    maybe_add_string (ret, entry_action_string, gncEntryGetAction (entry));
    maybe_add_string (ret, entry_notes_string, gncEntryGetNotes (entry));

    maybe_add_numeric (ret, entry_qty_string, gncEntryGetQuantity (entry));

    /* cust invoice */

    acc = gncEntryGetInvAccount (entry);
    if (acc)
        xmlAddChild (ret, guid_to_dom_tree (entry_invacct_string,
                                            qof_instance_get_guid(QOF_INSTANCE(acc))));

    maybe_add_numeric (ret, entry_iprice_string, gncEntryGetInvPrice (entry));

    maybe_add_numeric (ret, entry_idiscount_string, gncEntryGetInvDiscount (entry));

    invoice = gncEntryGetInvoice (entry);
    if (invoice)
    {
        xmlAddChild (ret, guid_to_dom_tree (entry_invoice_string,
                                            qof_instance_get_guid(QOF_INSTANCE(invoice))));

        xmlAddChild(ret, text_to_dom_tree(entry_idisctype_string,
                                          gncAmountTypeToString (
                                              gncEntryGetInvDiscountType (entry))));
        xmlAddChild(ret, text_to_dom_tree(entry_idischow_string,
                                          gncEntryDiscountHowToString (
                                              gncEntryGetInvDiscountHow (entry))));

        xmlAddChild(ret, int_to_dom_tree(entry_itaxable_string,
                                         gncEntryGetInvTaxable (entry)));
        xmlAddChild(ret, int_to_dom_tree(entry_itaxincluded_string,
                                         gncEntryGetInvTaxIncluded (entry)));
    }

    taxtable = gncEntryGetInvTaxTable (entry);
    if (taxtable)
        xmlAddChild (ret, guid_to_dom_tree (entry_itaxtable_string,
                                            qof_instance_get_guid (QOF_INSTANCE(taxtable))));

    /* vendor bills */

    acc = gncEntryGetBillAccount (entry);
    if (acc)
        xmlAddChild (ret, guid_to_dom_tree (entry_billacct_string,
                                            qof_instance_get_guid (QOF_INSTANCE(acc))));

    maybe_add_numeric (ret, entry_bprice_string, gncEntryGetBillPrice (entry));

    invoice = gncEntryGetBill (entry);
    if (invoice)
    {
        GncOwner *owner;
        xmlAddChild (ret, guid_to_dom_tree (entry_bill_string,
                                            qof_instance_get_guid(QOF_INSTANCE(invoice))));
        xmlAddChild(ret, int_to_dom_tree(entry_billable_string,
                                         gncEntryGetBillable (entry)));
        owner = gncEntryGetBillTo (entry);
        if (owner && owner->owner.undefined != NULL)
            xmlAddChild (ret, gnc_owner_to_dom_tree (entry_billto_string, owner));

        xmlAddChild(ret, int_to_dom_tree(entry_btaxable_string,
                                         gncEntryGetBillTaxable (entry)));
        xmlAddChild(ret, int_to_dom_tree(entry_btaxincluded_string,
                                         gncEntryGetBillTaxIncluded (entry)));
        maybe_add_string (ret, entry_billpayment_string,
                          gncEntryPaymentTypeToString (gncEntryGetBillPayment (entry)));
    }

    taxtable = gncEntryGetBillTaxTable (entry);
    if (taxtable)
        xmlAddChild (ret, guid_to_dom_tree (entry_btaxtable_string,
                                            qof_instance_get_guid (QOF_INSTANCE(taxtable))));

    /* Other stuff */

    order = gncEntryGetOrder (entry);
    if (order)
        xmlAddChild (ret, guid_to_dom_tree (entry_order_string,
                                            qof_instance_get_guid(QOF_INSTANCE (order))));

    return ret;
}

/***********************************************************************/

struct entry_pdata
{
    GncEntry *entry;
    QofBook *book;
    Account *acc;
};

static inline gboolean
set_string(xmlNodePtr node, GncEntry* entry,
           void (*func)(GncEntry *entry, const char *txt))
{
    char* txt = dom_tree_to_text(node);
    g_return_val_if_fail(txt, FALSE);

    func(entry, txt);
    g_free(txt);
    return TRUE;
}

static inline gboolean
set_timespec(xmlNodePtr node, GncEntry* entry,
             void (*func)(GncEntry *entry, Timespec ts))
{
    Timespec ts = dom_tree_to_timespec (node);
    if (!dom_tree_valid_timespec(&ts, node->name)) return FALSE;

    func(entry, ts);
    return TRUE;
}

static inline gboolean
set_numeric(xmlNodePtr node, GncEntry* entry,
            void (*func)(GncEntry *entry, gnc_numeric num))
{
    gnc_numeric* num = dom_tree_to_gnc_numeric(node);
    g_return_val_if_fail(num, FALSE);

    func(entry, *num);
    g_free(num);
    return TRUE;
}

static inline gboolean
set_boolean(xmlNodePtr node, GncEntry* entry,
            void (*func)(GncEntry *entry, gboolean val))
{
    gint64 val;

    if (!dom_tree_to_integer(node, &val))
        return FALSE;
    func (entry, (gboolean)val);
    return TRUE;
}

static inline gboolean
set_account(xmlNodePtr node, struct entry_pdata *pdata,
            void (*func)(GncEntry *entry, Account *acc))
{
    GncGUID *guid;
    Account * acc;

    guid = dom_tree_to_guid (node);
    g_return_val_if_fail (guid, FALSE);
    acc = xaccAccountLookup (guid, pdata->book);
    g_free (guid);
    g_return_val_if_fail (acc, FALSE);

    if (func)
        func (pdata->entry, acc);
    else
        pdata->acc = acc;
    return TRUE;
}

static inline gboolean
set_taxtable (xmlNodePtr node, struct entry_pdata *pdata,
              void (*func)(GncEntry *entry, GncTaxTable *taxtable))
{
    GncGUID *guid;
    GncTaxTable *taxtable;

    guid = dom_tree_to_guid (node);
    g_return_val_if_fail (guid, FALSE);
    taxtable = gncTaxTableLookup (pdata->book, guid);
    if (!taxtable)
    {
        taxtable = gncTaxTableCreate (pdata->book);
        gncTaxTableBeginEdit (taxtable);
        gncTaxTableSetGUID (taxtable, guid);
        gncTaxTableCommitEdit (taxtable);
    }
    else
        gncTaxTableDecRef (taxtable);

    func (pdata->entry, taxtable);
    g_free(guid);
    return TRUE;
}

static gboolean
entry_guid_handler (xmlNodePtr node, gpointer entry_pdata)
{
    struct entry_pdata *pdata = entry_pdata;
    GncGUID *guid;
    GncEntry *entry;

    guid = dom_tree_to_guid(node);
    g_return_val_if_fail (guid, FALSE);
    entry = gncEntryLookup (pdata->book, guid);
    if (entry)
    {
        gncEntryDestroy (pdata->entry);
        pdata->entry = entry;
        gncEntryBeginEdit (entry);
    }
    else
    {
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

/* Cust invoice */

static gboolean
entry_invacct_handler (xmlNodePtr node, gpointer entry_pdata)
{
    struct entry_pdata *pdata = entry_pdata;
    return set_account (node, pdata, gncEntrySetInvAccount);
}

static gboolean
entry_iprice_handler (xmlNodePtr node, gpointer entry_pdata)
{
    struct entry_pdata *pdata = entry_pdata;

    return set_numeric(node, pdata->entry, gncEntrySetInvPrice);
}

static gboolean
entry_idiscount_handler (xmlNodePtr node, gpointer entry_pdata)
{
    struct entry_pdata *pdata = entry_pdata;

    return set_numeric(node, pdata->entry, gncEntrySetInvDiscount);
}

static gboolean
entry_idisctype_handler (xmlNodePtr node, gpointer entry_pdata)
{
    struct entry_pdata *pdata = entry_pdata;
    GncAmountType type;
    char *str;
    gboolean ret;

    str = dom_tree_to_text (node);
    g_return_val_if_fail (str, FALSE);

    ret = gncAmountStringToType (str, &type);
    g_free (str);

    if (ret)
        gncEntrySetInvDiscountType(pdata->entry, type);

    return ret;
}

static gboolean
entry_idischow_handler (xmlNodePtr node, gpointer entry_pdata)
{
    struct entry_pdata *pdata = entry_pdata;
    GncDiscountHow how;
    char *str;
    gboolean ret;

    str = dom_tree_to_text (node);
    g_return_val_if_fail (str, FALSE);

    ret = gncEntryDiscountStringToHow (str, &how);
    g_free (str);

    if (ret)
        gncEntrySetInvDiscountHow(pdata->entry, how);

    return ret;
}

static gboolean
entry_itaxable_handler (xmlNodePtr node, gpointer entry_pdata)
{
    struct entry_pdata *pdata = entry_pdata;
    return set_boolean (node, pdata->entry, gncEntrySetInvTaxable);
}

static gboolean
entry_itaxincluded_handler (xmlNodePtr node, gpointer entry_pdata)
{
    struct entry_pdata *pdata = entry_pdata;
    return set_boolean (node, pdata->entry, gncEntrySetInvTaxIncluded);
}

static gboolean
entry_itaxtable_handler (xmlNodePtr node, gpointer entry_pdata)
{
    struct entry_pdata *pdata = entry_pdata;
    return set_taxtable (node, pdata, gncEntrySetInvTaxTable);
}

/* vendor bills */

static gboolean
entry_billacct_handler (xmlNodePtr node, gpointer entry_pdata)
{
    struct entry_pdata *pdata = entry_pdata;
    return set_account (node, pdata, gncEntrySetBillAccount);
}

static gboolean
entry_bprice_handler (xmlNodePtr node, gpointer entry_pdata)
{
    struct entry_pdata *pdata = entry_pdata;

    return set_numeric(node, pdata->entry, gncEntrySetBillPrice);
}

static gboolean
entry_btaxable_handler (xmlNodePtr node, gpointer entry_pdata)
{
    struct entry_pdata *pdata = entry_pdata;
    return set_boolean (node, pdata->entry, gncEntrySetBillTaxable);
}

static gboolean
entry_btaxincluded_handler (xmlNodePtr node, gpointer entry_pdata)
{
    struct entry_pdata *pdata = entry_pdata;
    return set_boolean (node, pdata->entry, gncEntrySetBillTaxIncluded);
}

static gboolean
entry_btaxtable_handler (xmlNodePtr node, gpointer entry_pdata)
{
    struct entry_pdata *pdata = entry_pdata;
    return set_taxtable (node, pdata, gncEntrySetBillTaxTable);
}

static gboolean
entry_billable_handler (xmlNodePtr node, gpointer entry_pdata)
{
    struct entry_pdata *pdata = entry_pdata;
    return set_boolean (node, pdata->entry, gncEntrySetBillable);
}

static gboolean
entry_billto_handler (xmlNodePtr node, gpointer entry_pdata)
{
    struct entry_pdata *pdata = entry_pdata;
    GncOwner billto;
    gboolean ret;

    ret = gnc_dom_tree_to_owner (node, &billto, pdata->book);
    if (ret)
        gncEntrySetBillTo (pdata->entry, &billto);

    return ret;
}

/* employee bills */
static gboolean
entry_billpayment_handler (xmlNodePtr node, gpointer entry_pdata)
{
    struct entry_pdata *pdata = entry_pdata;
    GncEntryPaymentType type;
    char *str;
    gboolean ret;

    str = dom_tree_to_text (node);
    g_return_val_if_fail (str, FALSE);

    ret = gncEntryPaymentStringToType (str, &type);
    g_free (str);

    if (ret)
        gncEntrySetBillPayment(pdata->entry, type);

    return ret;
}

/* The rest of the stuff */

static gboolean
entry_order_handler (xmlNodePtr node, gpointer entry_pdata)
{
    struct entry_pdata *pdata = entry_pdata;
    GncGUID *guid;
    GncOrder *order;

    guid = dom_tree_to_guid (node);
    g_return_val_if_fail (guid, FALSE);
    order = gncOrderLookup (pdata->book, guid);
    if (!order)
    {
        order = gncOrderCreate (pdata->book);
        gncOrderBeginEdit (order);
        gncOrderSetGUID (order, guid);
        gncOrderCommitEdit (order);
    }
    gncOrderBeginEdit (order);
    gncOrderAddEntry (order, pdata->entry);
    gncOrderCommitEdit (order);

    g_free(guid);
    return TRUE;
}

static gboolean
entry_invoice_handler (xmlNodePtr node, gpointer entry_pdata)
{
    struct entry_pdata *pdata = entry_pdata;
    GncGUID *guid;
    GncInvoice *invoice;

    guid = dom_tree_to_guid (node);
    g_return_val_if_fail (guid, FALSE);
    invoice = gncInvoiceLookup (pdata->book, guid);
    if (!invoice)
    {
        invoice = gncInvoiceCreate (pdata->book);
        gncInvoiceBeginEdit (invoice);
        gncInvoiceSetGUID (invoice, guid);
        gncInvoiceCommitEdit (invoice);
    }
    gncInvoiceBeginEdit (invoice);
    gncInvoiceAddEntry (invoice, pdata->entry);
    gncInvoiceCommitEdit (invoice);

    g_free(guid);
    return TRUE;
}

static gboolean
entry_bill_handler (xmlNodePtr node, gpointer entry_pdata)
{
    struct entry_pdata *pdata = entry_pdata;
    GncGUID *guid;
    GncInvoice *invoice;

    guid = dom_tree_to_guid (node);
    g_return_val_if_fail (guid, FALSE);
    invoice = gncInvoiceLookup (pdata->book, guid);
    if (!invoice)
    {
        invoice = gncInvoiceCreate (pdata->book);
        gncInvoiceBeginEdit (invoice);
        gncInvoiceSetGUID (invoice, guid);
        gncInvoiceCommitEdit (invoice);
    }
    gncInvoiceBeginEdit (invoice);
    gncBillAddEntry (invoice, pdata->entry);
    gncInvoiceCommitEdit (invoice);

    g_free(guid);
    return TRUE;
}

/* Support for older XML versions */

static gboolean
entry_acct_handler (xmlNodePtr node, gpointer entry_pdata)
{
    struct entry_pdata *pdata = entry_pdata;
    /* XXX: try to figure out if this is an 'invoice' or a 'bill' --
     * we have to wait until the end!
     */

    return set_account (node, pdata, NULL);
}

static gboolean
entry_price_handler (xmlNodePtr node, gpointer entry_pdata)
{
    struct entry_pdata *pdata = entry_pdata;
    gboolean res;

    /* just set both.. Don't worry about extra data if it's wrong */
    res = set_numeric(node, pdata->entry, gncEntrySetInvPrice);
    if (res)
        gncEntrySetBillPrice (pdata->entry, gncEntryGetInvPrice (pdata->entry));
    return res;
}

static gboolean
entry_slots_handler (xmlNodePtr node, gpointer entry_pdata)
{
    return TRUE;
}

static struct dom_tree_handler entry_handlers_v2[] =
{
    { entry_guid_string, entry_guid_handler, 1, 0 },
    { entry_date_string, entry_date_handler, 1, 0 },
    { entry_dateentered_string, entry_dateentered_handler, 1, 0 },
    { entry_description_string, entry_description_handler, 0, 0 },
    { entry_action_string, entry_action_handler, 0, 0 },
    { entry_notes_string, entry_notes_handler, 0, 0 },
    { entry_qty_string, entry_qty_handler, 0, 0 },

    /* cust invoice */
    { entry_invacct_string, entry_invacct_handler, 0, 0 },
    { entry_iprice_string, entry_iprice_handler, 0, 0 },
    { entry_idiscount_string, entry_idiscount_handler, 0, 0 },
    { entry_idisctype_string, entry_idisctype_handler, 0, 0 },
    { entry_idischow_string, entry_idischow_handler, 0, 0 },
    { entry_itaxable_string, entry_itaxable_handler, 0, 0 },
    { entry_itaxincluded_string, entry_itaxincluded_handler, 0, 0 },
    { entry_itaxtable_string, entry_itaxtable_handler, 0, 0 },

    /* vendor invoice */
    { entry_billacct_string, entry_billacct_handler, 0, 0 },
    { entry_bprice_string, entry_bprice_handler, 0, 0 },
    { entry_btaxable_string, entry_btaxable_handler, 0, 0 },
    { entry_btaxincluded_string, entry_btaxincluded_handler, 0, 0 },
    { entry_btaxtable_string, entry_btaxtable_handler, 0, 0 },
    { entry_billable_string, entry_billable_handler, 0, 0 },
    { entry_billto_string, entry_billto_handler, 0, 0 },

    /* employee stuff */
    { entry_billpayment_string, entry_billpayment_handler, 0, 0 },

    /* Other stuff */
    { entry_order_string, entry_order_handler, 0, 0 },
    { entry_invoice_string, entry_invoice_handler, 0, 0 },
    { entry_bill_string, entry_bill_handler, 0, 0 },
    { entry_slots_string, entry_slots_handler, 0, 0 },

    /* Old XML support */
    { "entry:acct", entry_acct_handler, 0, 0 },
    { "entry:price", entry_price_handler, 0, 0 },
    { "entry:discount", entry_idiscount_handler, 0, 0 },
    { "entry:disc-type", entry_idisctype_handler, 0, 0 },
    { "entry:disc-how", entry_idischow_handler, 0, 0 },
    { "entry:taxable", entry_itaxable_handler, 0, 0 },
    { "entry:taxincluded", entry_itaxincluded_handler, 0, 0 },
    { "entry:taxtable", entry_itaxtable_handler, 0, 0 },
    { NULL, 0, 0, 0 }
};

static GncEntry*
dom_tree_to_entry (xmlNodePtr node, QofBook *book)
{
    struct entry_pdata entry_pdata;
    gboolean successful;

    entry_pdata.entry = gncEntryCreate(book);
    entry_pdata.book = book;
    entry_pdata.acc = NULL;
    gncEntryBeginEdit (entry_pdata.entry);

    successful = dom_tree_generic_parse (node, entry_handlers_v2,
                                         &entry_pdata);
    if (entry_pdata.acc != NULL)
    {
        if (gncEntryGetBill (entry_pdata.entry))
            gncEntrySetBillAccount (entry_pdata.entry, entry_pdata.acc);
        else
            gncEntrySetInvAccount (entry_pdata.entry, entry_pdata.acc);
    }

    if (successful)
        gncEntryCommitEdit (entry_pdata.entry);
    else
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
    QofBook *book = gdata->bookdata;

    successful = TRUE;

    if (parent_data)
    {
        return TRUE;
    }

    /* OK.  For some messed up reason this is getting called again with a
       NULL tag.  So we ignore those cases */
    if (!tag)
    {
        return TRUE;
    }

    g_return_val_if_fail(tree, FALSE);

    entry = dom_tree_to_entry(tree, book);
    if (entry != NULL)
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
do_count (QofInstance * entry_p, gpointer count_p)
{
    int *count = count_p;
    (*count)++;
}

static int
entry_get_count (QofBook *book)
{
    int count = 0;
    qof_object_foreach (_GNC_MOD_NAME, book, do_count, (gpointer) &count);
    return count;
}

static void
xml_add_entry (QofInstance * entry_p, gpointer out_p)
{
    xmlNodePtr node;
    GncEntry *entry = (GncEntry *) entry_p;
    FILE *out = out_p;

    if (ferror(out))
        return;

    /* Don't save non-attached entries! */
    if (!(gncEntryGetOrder (entry) || gncEntryGetInvoice (entry) ||
            gncEntryGetBill (entry)))
        return;

    node = entry_dom_tree_create (entry);
    xmlElemDump(out, NULL, node);
    xmlFreeNode (node);
    if (ferror(out) || fprintf(out, "\n") < 0)
        return;
}

static gboolean
entry_write (FILE *out, QofBook *book)
{
    qof_object_foreach (_GNC_MOD_NAME, book, xml_add_entry, (gpointer) out);
    return ferror(out) == 0;
}

static gboolean
entry_ns(FILE *out)
{
    g_return_val_if_fail(out, FALSE);
    return gnc_xml2_write_namespace_decl(out, "entry");
}

void
gnc_entry_xml_initialize (void)
{
    static GncXmlDataType_t be_data =
    {
        GNC_FILE_BACKEND_VERS,
        gnc_entry_string,
        entry_sixtp_parser_create,
        NULL,			/* add_item */
        entry_get_count,
        entry_write,
        NULL,			/* scrub */
        entry_ns,
    };

    qof_object_register_backend (_GNC_MOD_NAME,
                                 GNC_FILE_BACKEND,
                                 &be_data);
}
