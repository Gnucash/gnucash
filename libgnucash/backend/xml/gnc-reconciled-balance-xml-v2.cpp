/*
 * gnc-reconciled-balance-xml-v2.cpp -- reconciled balance xml i/o
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */
#include <glib.h>

#include <config.h>
#include <stdlib.h>
#include <string.h>

#include "gnc-xml-helper.h"
#include "sixtp.h"
#include "sixtp-utils.h"
#include "sixtp-parsers.h"
#include "sixtp-dom-parsers.h"
#include "sixtp-dom-generators.h"

#include "gnc-xml.h"
#include "io-gncxml-gen.h"
#include "io-gncxml-v2.h"

static QofLogModule log_module = GNC_MOD_IO;

const gchar* reconciled_balance_version_string = "2.0.0";

/* ids */
#define gnc_reconciled_balance_string  "gnc:reconciled-balance"
#define rb_id_string                  "recbal:id"
#define rb_account_string             "recbal:account"
#define rb_date_string                "recbal:date"
#define rb_amount_string              "recbal:amount"
#define rb_notes_string               "recbal:notes"
#define rb_slots_string               "recbal:slots"

xmlNodePtr
gnc_reconciled_balance_dom_tree_create (GncReconciledBalance* ba)
{
    xmlNodePtr ret;
    Account* acc;
    const char* notes;

    ENTER ("(reconciled balance=%p)", ba);

    ret = xmlNewNode (NULL, BAD_CAST gnc_reconciled_balance_string);
    xmlSetProp (ret, BAD_CAST "version",
                BAD_CAST reconciled_balance_version_string);

    xmlAddChild (ret, guid_to_dom_tree (rb_id_string,
                                        gnc_reconciled_balance_get_guid (ba)));

    acc = gnc_reconciled_balance_get_account (ba);
    if (acc)
        xmlAddChild (ret, guid_to_dom_tree (rb_account_string,
                                            xaccAccountGetGUID (acc)));

    xmlAddChild (ret, time64_to_dom_tree (rb_date_string,
                                          gnc_reconciled_balance_get_date (ba)));

    gnc_numeric amount = gnc_reconciled_balance_get_amount (ba);
    xmlAddChild (ret, gnc_numeric_to_dom_tree (rb_amount_string, &amount));

    notes = gnc_reconciled_balance_get_notes (ba);
    if (notes && *notes)
        xmlAddChild (ret, text_to_dom_tree (rb_notes_string, notes));

    /* xmlAddChild won't do anything with a NULL, so tests are superfluous. */
    xmlAddChild (ret, qof_instance_slots_to_dom_tree (rb_slots_string,
                                                      QOF_INSTANCE (ba)));

    LEAVE (" ");
    return ret;
}

/***********************************************************************/

static gboolean
rb_id_handler (xmlNodePtr node, gpointer ba)
{
    auto guid = dom_tree_to_guid (node);
    g_return_val_if_fail (guid, FALSE);
    qof_instance_set_guid (QOF_INSTANCE (ba), &*guid);
    return TRUE;
}

static gboolean
rb_account_handler (xmlNodePtr node, gpointer p)
{
    auto ba = GNC_RECONCILED_BALANCE (p);
    auto guid = dom_tree_to_guid (node);
    g_return_val_if_fail (guid, FALSE);

    auto acc = xaccAccountLookup (&*guid,
                                  qof_instance_get_book (QOF_INSTANCE (ba)));
    /* Accounts are written before reconciled balances, so a missing
     * account means a damaged file rather than an ordering problem. Keep
     * the assertion -- it will simply report as unevaluatable -- but say
     * so in the log. */
    if (!acc)
    {
        PWARN ("reconciled balance refers to an unknown account");
        return TRUE;
    }

    gnc_reconciled_balance_set_account (ba, acc);
    return TRUE;
}

static gboolean
rb_date_handler (xmlNodePtr node, gpointer ba)
{
    time64 date = dom_tree_to_time64 (node);

    if (!dom_tree_valid_time64 (date, node->name))
        return FALSE;

    gnc_reconciled_balance_set_date (GNC_RECONCILED_BALANCE (ba), date);
    return TRUE;
}

static gboolean
rb_amount_handler (xmlNodePtr node, gpointer ba)
{
    gnc_reconciled_balance_set_amount (GNC_RECONCILED_BALANCE (ba),
                                      dom_tree_to_gnc_numeric (node));
    return TRUE;
}

static gboolean
rb_notes_handler (xmlNodePtr node, gpointer ba)
{
    return apply_xmlnode_text (gnc_reconciled_balance_set_notes,
                               GNC_RECONCILED_BALANCE (ba), node);
}

static gboolean
rb_slots_handler (xmlNodePtr node, gpointer ba)
{
    return dom_tree_create_instance_slots (node, QOF_INSTANCE (ba));
}

static struct dom_tree_handler reconciled_balance_handlers[] =
{
    { rb_id_string, rb_id_handler, 1, 0 },
    { rb_account_string, rb_account_handler, 0, 0 },
    { rb_date_string, rb_date_handler, 1, 0 },
    { rb_amount_string, rb_amount_handler, 1, 0 },
    { rb_notes_string, rb_notes_handler, 0, 0 },
    { rb_slots_string, rb_slots_handler, 0, 0 },
    { NULL, 0, 0, 0 }
};

static gboolean
gnc_reconciled_balance_end_handler (gpointer data_for_children,
                                   GSList* data_from_children,
                                   GSList* sibling_data,
                                   gpointer parent_data, gpointer global_data,
                                   gpointer* result, const gchar* tag)
{
    GncReconciledBalance* ba;
    xmlNodePtr tree = (xmlNodePtr)data_for_children;
    gxpf_data* gdata = (gxpf_data*)global_data;
    QofBook* book = static_cast<decltype (book)> (gdata->bookdata);

    if (parent_data)
        return TRUE;

    /* the parser calls us again with a NULL tag; ignore those */
    if (!tag)
        return TRUE;

    g_return_val_if_fail (tree, FALSE);

    ba = dom_tree_to_reconciled_balance (tree, book);
    xmlFreeNode (tree);
    if (ba != NULL)
        gdata->cb (tag, gdata->parsedata, ba);

    return ba != NULL;
}

GncReconciledBalance*
dom_tree_to_reconciled_balance (xmlNodePtr node, QofBook* book)
{
    GncReconciledBalance* ba;

    ba = gnc_reconciled_balance_new (book);
    if (!dom_tree_generic_parse (node, reconciled_balance_handlers, ba))
    {
        PERR ("failed to parse reconciled balance tree");
        gnc_reconciled_balance_destroy (ba);
        ba = NULL;
    }
    return ba;
}

sixtp*
gnc_reconciled_balance_sixtp_parser_create (void)
{
    return sixtp_dom_parser_new (gnc_reconciled_balance_end_handler, NULL, NULL);
}
/* ======================  END OF FILE ===================*/
