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

#include "gncBillTermP.h"
#include "gncInvoiceP.h"
#include "gnc-invoice-xml-v2.h"
#include "gnc-owner-xml-v2.h"
#include "gnc-bill-term-xml-v2.h"

#define _GNC_MOD_NAME	GNC_ID_INVOICE

static QofLogModule log_module = GNC_MOD_IO;

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
#define invoice_currency_string "invoice:currency"
#define invoice_billto_string "invoice:billto"
#define invoice_tochargeamt_string "invoice:charge-amt"
#define invoice_slots_string "invoice:slots"

/* EFFECTIVE FRIEND FUNCTION */
extern KvpFrame *qof_instance_get_slots (const QofInstance *);

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
    KvpFrame *kf;
    Timespec ts;
    Transaction *txn;
    GNCLot *lot;
    Account *acc;
    GncBillTerm *term;
    GncOwner *billto;
    gnc_numeric amt;

    ret = xmlNewNode(NULL, BAD_CAST gnc_invoice_string);
    xmlSetProp(ret, BAD_CAST "version", BAD_CAST invoice_version_string);

    xmlAddChild(ret, guid_to_dom_tree(invoice_guid_string,
                                      qof_instance_get_guid(QOF_INSTANCE(invoice))));

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
                                          qof_instance_get_guid (QOF_INSTANCE(term))));

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
                                            qof_instance_get_guid(QOF_INSTANCE(acc))));

    xmlAddChild
    (ret,
     commodity_ref_to_dom_tree(invoice_currency_string,
                               gncInvoiceGetCurrency (invoice)));

    billto = gncInvoiceGetBillTo (invoice);
    if (billto && billto->owner.undefined != NULL)
        xmlAddChild (ret, gnc_owner_to_dom_tree (invoice_billto_string, billto));

    amt = gncInvoiceGetToChargeAmount (invoice);
    if (! gnc_numeric_zero_p (amt))
        xmlAddChild (ret, gnc_numeric_to_dom_tree (invoice_tochargeamt_string, &amt));

    kf = qof_instance_get_slots (QOF_INSTANCE(invoice));
    if (kf)
    {
        xmlNodePtr kvpnode = kvp_frame_to_dom_tree(invoice_slots_string, kf);
        if (kvpnode)
        {
            xmlAddChild(ret, kvpnode);
        }
    }

    return ret;
}

/***********************************************************************/

struct invoice_pdata
{
    GncInvoice *invoice;
    QofBook *book;
};

static inline gboolean
set_string(xmlNodePtr node, GncInvoice* invoice,
           void (*func)(GncInvoice *invoice, const char *txt))
{
    char* txt = dom_tree_to_text(node);
    g_return_val_if_fail(txt, FALSE);

    func(invoice, txt);

    g_free(txt);
    return TRUE;
}

static inline gboolean
set_timespec(xmlNodePtr node, GncInvoice* invoice,
             void (*func)(GncInvoice *invoice, Timespec ts))
{
    Timespec ts = dom_tree_to_timespec(node);
    if (!dom_tree_valid_timespec(&ts, node->name)) return FALSE;

    func(invoice, ts);
    return TRUE;
}

static gboolean
invoice_guid_handler (xmlNodePtr node, gpointer invoice_pdata)
{
    struct invoice_pdata *pdata = invoice_pdata;
    GncGUID *guid;
    GncInvoice *invoice;

    guid = dom_tree_to_guid(node);
    g_return_val_if_fail (guid, FALSE);
    invoice = gncInvoiceLookup (pdata->book, guid);
    if (invoice)
    {
        gncInvoiceDestroy (pdata->invoice);
        pdata->invoice = invoice;
        gncInvoiceBeginEdit (invoice);
    }
    else
    {
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
    GncGUID *guid;
    GncBillTerm *term;

    guid = dom_tree_to_guid(node);
    g_return_val_if_fail (guid, FALSE);
    term = gnc_billterm_xml_find_or_create(pdata->book, guid);
    g_assert(term);
    g_free (guid);
    gncInvoiceSetTerms (pdata->invoice, term);

    return TRUE;
}

static gboolean
invoice_posttxn_handler (xmlNodePtr node, gpointer invoice_pdata)
{
    struct invoice_pdata *pdata = invoice_pdata;
    GncGUID *guid;
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
    GncGUID *guid;
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
    GncGUID *guid;
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
invoice_currency_handler (xmlNodePtr node, gpointer invoice_pdata)
{
    struct invoice_pdata *pdata = invoice_pdata;
    gnc_commodity *com;

    com = dom_tree_to_commodity_ref(node, pdata->book);
    g_return_val_if_fail (com, FALSE);

    gncInvoiceSetCurrency (pdata->invoice, com);

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

static gboolean
invoice_tochargeamt_handler (xmlNodePtr node, gpointer invoice_pdata)
{
    struct invoice_pdata *pdata = invoice_pdata;
    gnc_numeric* num = dom_tree_to_gnc_numeric(node);
    g_return_val_if_fail(num, FALSE);

    gncInvoiceSetToChargeAmount (pdata->invoice, *num);
    g_free(num);
    return TRUE;
}

static gboolean
invoice_slots_handler (xmlNodePtr node, gpointer invoice_pdata)
{
    struct invoice_pdata *pdata = invoice_pdata;

    return dom_tree_to_kvp_frame_given
           (node, qof_instance_get_slots (QOF_INSTANCE (pdata->invoice)));
}

static struct dom_tree_handler invoice_handlers_v2[] =
{
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
    { invoice_currency_string, invoice_currency_handler, 0, 0 },
    { "invoice:commodity", invoice_currency_handler, 0, 0 },
    { invoice_billto_string, invoice_billto_handler, 0, 0 },
    { invoice_tochargeamt_string, invoice_tochargeamt_handler, 0, 0},
    { invoice_slots_string, invoice_slots_handler, 0, 0 },
    { NULL, 0, 0, 0 }
};

static GncInvoice*
dom_tree_to_invoice (xmlNodePtr node, QofBook *book)
{
    struct invoice_pdata invoice_pdata;
    gboolean successful;

    invoice_pdata.invoice = gncInvoiceCreate(book);
    invoice_pdata.book = book;
    gncInvoiceBeginEdit (invoice_pdata.invoice);

    successful = dom_tree_generic_parse (node, invoice_handlers_v2,
                                         &invoice_pdata);

    if (successful)
        gncInvoiceCommitEdit (invoice_pdata.invoice);
    else
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
    GncInvoice *invoice;
    xmlNodePtr tree = (xmlNodePtr)data_for_children;
    gxpf_data *gdata = (gxpf_data*)global_data;
    QofBook *book = gdata->bookdata;

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

    invoice = dom_tree_to_invoice(tree, book);
    if (invoice != NULL)
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

static gboolean
invoice_should_be_saved (GncInvoice *invoice)
{
    const char *id;

    /* make sure this is a valid invoice before we save it -- should have an ID */
    id = gncInvoiceGetID (invoice);
    if (id == NULL || *id == '\0')
        return FALSE;

    return TRUE;
}

static void
do_count (QofInstance * invoice_p, gpointer count_p)
{
    int *count = count_p;
    if (invoice_should_be_saved ((GncInvoice *)invoice_p))
        (*count)++;
}

static int
invoice_get_count (QofBook *book)
{
    int count = 0;
    qof_object_foreach (_GNC_MOD_NAME, book, do_count, (gpointer) &count);
    return count;
}

static void
xml_add_invoice (QofInstance * invoice_p, gpointer out_p)
{
    xmlNodePtr node;
    GncInvoice *invoice = (GncInvoice *) invoice_p;
    FILE *out = out_p;

    if (ferror(out))
        return;
    if (!invoice_should_be_saved (invoice))
        return;

    node = invoice_dom_tree_create (invoice);
    xmlElemDump(out, NULL, node);
    xmlFreeNode (node);
    if (ferror(out) || fprintf(out, "\n") < 0)
        return;
}

static gboolean
invoice_write (FILE *out, QofBook *book)
{
    qof_object_foreach_sorted (_GNC_MOD_NAME, book, xml_add_invoice, (gpointer) out);
    return ferror(out) == 0;
}

static gboolean
invoice_ns(FILE *out)
{
    g_return_val_if_fail(out, FALSE);
    return gnc_xml2_write_namespace_decl(out, "invoice");
}

void
gnc_invoice_xml_initialize (void)
{
    static GncXmlDataType_t be_data =
    {
        GNC_FILE_BACKEND_VERS,
        gnc_invoice_string,
        invoice_sixtp_parser_create,
        NULL,			/* add_item */
        invoice_get_count,
        invoice_write,
        NULL,			/* scrub */
        invoice_ns,
    };

    qof_object_register_backend (_GNC_MOD_NAME,
                                 GNC_FILE_BACKEND,
                                 &be_data);
}
