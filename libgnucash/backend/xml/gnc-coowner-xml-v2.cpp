/********************************************************************\
 * gnc-coowner-xml-v2.c -- coowner xml i/o implementation         *
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
#include <glib.h>

extern "C"
{
#include <config.h>
#include <stdlib.h>
#include <string.h>

#include "gncBillTermP.h"
#include "gncCoOwnerP.h"
#include "gncTaxTableP.h"
}

#include "gnc-xml-helper.h"
#include "gnc-coowner-xml-v2.h"
#include "gnc-address-xml-v2.h"
#include "gnc-bill-term-xml-v2.h"
#include "sixtp.h"
#include "sixtp-utils.h"
#include "sixtp-parsers.h"
#include "sixtp-utils.h"
#include "sixtp-dom-parsers.h"
#include "sixtp-dom-generators.h"

#include "gnc-xml.h"
#include "io-gncxml-gen.h"
#include "io-gncxml-v2.h"

#include "xml-helpers.h"

#define _GNC_MOD_NAME   GNC_ID_COOWNER

static QofLogModule log_module = GNC_MOD_IO;

const gchar* coowner_version_string = "2.0.0";

/* ids */
#define gnc_coowner_string "gnc:GncCoOwner"
#define coowner_guid_string "coowner:guid"
#define coowner_id_string "coowner:id"
#define coowner_acl_string "coowner:acl"
#define coowner_active_string "coowner:active"
#define coowner_addr_string "coowner:addr"
#define coowner_apt_share_string "coowner:apt_share"
#define coowner_apt_unit_string "coowner:apt_unit"
#define coowner_credit_string "coowner:credit"
#define coowner_currency_string "coowner:currency"
#define coowner_discount_string "coowner:discount"
#define coowner_distribution_key_string "coowner:distribution_key"
#define coowner_language_string "coowner:language"
#define coowner_notes_string "coowner:notes"
#define coowner_slots_string "coowner:slots"
#define coowner_taxincluded_string "coowner:tax_included"
#define coowner_taxtable_string "coowner:taxtable"
#define coowner_taxtableoverride_string "coowner:use-tt"
#define coowner_terms_string "coowner:terms"
#define coowner_username_string "coowner:username"

static xmlNodePtr
coowner_dom_tree_create (GncCoOwner* coowner)
{
    xmlNodePtr ret;
    gnc_numeric num;
    GncBillTerm* term;
    GncTaxTable* taxtable;

    ret = xmlNewNode (NULL, BAD_CAST gnc_coowner_string);
    xmlSetProp (ret, BAD_CAST "version", BAD_CAST coowner_version_string);

    xmlAddChild (ret, guid_to_dom_tree (coowner_guid_string,
qof_instance_get_guid (QOF_INSTANCE (coowner))));

    xmlAddChild (ret, text_to_dom_tree (coowner_id_string,
gncCoOwnerGetID (coowner)));

    maybe_add_string (ret, coowner_acl_string, gncCoOwnerGetAcl (coowner));

    xmlAddChild (ret, int_to_dom_tree (coowner_active_string,
gncCoOwnerGetActive (coowner)));

    xmlAddChild (ret, gnc_address_to_dom_tree (coowner_addr_string,
gncCoOwnerGetAddr (coowner)));

    num = gncCoOwnerGetAptShare (coowner);
    xmlAddChild (ret, gnc_numeric_to_dom_tree
(coowner_apt_share_string, &num));

    num = gncCoOwnerGetAptUnit (coowner);
    xmlAddChild (ret, gnc_numeric_to_dom_tree
(coowner_apt_unit_string, &num));

    num = gncCoOwnerGetCredit (coowner);
    xmlAddChild (ret, gnc_numeric_to_dom_tree (coowner_credit_string, &num));

    xmlAddChild (ret, text_to_dom_tree (coowner_distribution_key_string,
gncCoOwnerGetDistributionKey (coowner)));

    num = gncCoOwnerGetDiscount (coowner);
    xmlAddChild (ret, gnc_numeric_to_dom_tree (coowner_discount_string, &num));

    xmlAddChild (ret,
commodity_ref_to_dom_tree (coowner_currency_string,
gncCoOwnerGetCurrency (coowner)));

    maybe_add_string (ret, coowner_language_string,
gncCoOwnerGetLanguage (coowner));

    maybe_add_string (ret, coowner_notes_string, gncCoOwnerGetNotes (coowner));

    /* xmlAddChild won't do anything with a NULL, so tests are superfluous. */
    xmlAddChild (ret, qof_instance_slots_to_dom_tree (coowner_slots_string,
QOF_INSTANCE (coowner)));

    xmlAddChild (ret, int_to_dom_tree (coowner_taxtableoverride_string,
gncCoOwnerGetTaxTableOverride (coowner)));
    taxtable = gncCoOwnerGetTaxTable (coowner);
    if (taxtable)
xmlAddChild (ret, guid_to_dom_tree (coowner_taxtable_string,
qof_instance_get_guid (QOF_INSTANCE (taxtable))));

    xmlAddChild (ret, text_to_dom_tree (coowner_taxincluded_string,
gncTaxIncludedTypeToString (
gncCoOwnerGetTaxIncluded (coowner))));

    term = gncCoOwnerGetTerms (coowner);
    if (term)
xmlAddChild (ret, guid_to_dom_tree (coowner_terms_string,
qof_instance_get_guid (QOF_INSTANCE (term))));

    xmlAddChild (ret, text_to_dom_tree (coowner_username_string,
gncCoOwnerGetUsername (coowner)));

    return ret;
}

struct coowner_pdata
{
    GncCoOwner* coowner;
    QofBook* book;
};

/***********************************************************************/

static gboolean
set_boolean (xmlNodePtr node, GncCoOwner* coowner,
             void (*func) (GncCoOwner* coowner, gboolean b))
{
    gint64 val;
    gboolean ret;

    ret = dom_tree_to_integer (node, &val);
    if (ret)
func (coowner, (gboolean)val);

    return ret;
}

static gboolean
set_string (xmlNodePtr node, GncCoOwner* coowner,
            void (*func) (GncCoOwner* coowner, const char* txt))
{
    char* txt = dom_tree_to_text (node);
    g_return_val_if_fail (txt, FALSE);

    func (coowner, txt);

    g_free (txt);

    return TRUE;
}

/* Co-Onwer functions */
static gboolean
coowner_id_handler (xmlNodePtr node, gpointer coowner_pdata)
{
    struct coowner_pdata* pdata = static_cast<decltype (pdata)> (coowner_pdata);

    return set_string (node, pdata->coowner, gncCoOwnerSetID);
}

static gboolean
coowner_guid_handler (xmlNodePtr node, gpointer coowner_pdata)
{
    struct coowner_pdata* pdata = static_cast<decltype (pdata)> (coowner_pdata);
    GncGUID* guid;
    GncCoOwner* coowner;

    guid = dom_tree_to_guid (node);
    g_return_val_if_fail (guid, FALSE);
    coowner = gncCoOwnerLookup (pdata->book, guid);
    if (coowner)
    {
gncCoOwnerDestroy (pdata->coowner);
pdata->coowner = coowner;
gncCoOwnerBeginEdit (coowner);
    }
    else
    {
gncCoOwnerSetGUID (pdata->coowner, guid);
    }

    guid_free (guid);

    return TRUE;
}

static gboolean
coowner_active_handler (xmlNodePtr node, gpointer coowner_pdata)
{
    struct coowner_pdata* pdata = static_cast<decltype (pdata)> (coowner_pdata);
    return set_boolean (node, pdata->coowner, gncCoOwnerSetActive);
}

static gboolean
coowner_addr_handler (xmlNodePtr node, gpointer coowner_pdata)
{
    struct coowner_pdata* pdata = static_cast<decltype (pdata)> (coowner_pdata);

    return gnc_dom_tree_to_address (node, gncCoOwnerGetAddr (pdata->coowner));
}

static gboolean
coowner_apt_share_handler (xmlNodePtr node, gpointer coowner_pdata)
{
    struct coowner_pdata* pdata = static_cast<decltype (pdata)> (coowner_pdata);
    gnc_numeric* val;

    val = dom_tree_to_gnc_numeric (node);
    g_return_val_if_fail (val, FALSE);

    gncCoOwnerSetAptShare (pdata->coowner, *val);
    g_free (val);

    return TRUE;
}

static gboolean
coowner_apt_unit_handler (xmlNodePtr node, gpointer coowner_pdata)
{
    struct coowner_pdata* pdata = static_cast<decltype (pdata)> (coowner_pdata);
    gnc_numeric* val;

    val = dom_tree_to_gnc_numeric (node);
    g_return_val_if_fail (val, FALSE);

    gncCoOwnerSetAptUnit (pdata->coowner, *val);
    g_free (val);

    return TRUE;
}

static gboolean
coowner_credit_handler (xmlNodePtr node, gpointer coowner_pdata)
{
    struct coowner_pdata* pdata = static_cast<decltype (pdata)> (coowner_pdata);
    gnc_numeric* val;

    val = dom_tree_to_gnc_numeric (node);
    g_return_val_if_fail (val, FALSE);

    gncCoOwnerSetCredit (pdata->coowner, *val);
    g_free (val);

    return TRUE;
}

static gboolean
coowner_currency_handler (xmlNodePtr node, gpointer coowner_pdata)
{
    struct coowner_pdata* pdata = static_cast<decltype (pdata)> (coowner_pdata);
    gnc_commodity* com;

    com = dom_tree_to_commodity_ref (node, pdata->book);
    g_return_val_if_fail (com, FALSE);

    gncCoOwnerSetCurrency (pdata->coowner, com);

    return TRUE;
}

static gboolean
coowner_distribution_key_handler (xmlNodePtr node, gpointer coowner_pdata)
{
    struct coowner_pdata* pdata = static_cast<decltype (pdata)> (coowner_pdata);

    return set_string (node, pdata->coowner, gncCoOwnerSetDistributionKey);
}

static gboolean
coowner_discount_handler (xmlNodePtr node, gpointer coowner_pdata)
{
    struct coowner_pdata* pdata = static_cast<decltype (pdata)> (coowner_pdata);
    gnc_numeric* val;

    val = dom_tree_to_gnc_numeric (node);
    g_return_val_if_fail (val, FALSE);

    gncCoOwnerSetDiscount (pdata->coowner, *val);
    g_free (val);

    return TRUE;
}

static gboolean
coowner_notes_handler (xmlNodePtr node, gpointer coowner_pdata)
{
    struct coowner_pdata* pdata = static_cast<decltype (pdata)> (coowner_pdata);

    return set_string (node, pdata->coowner, gncCoOwnerSetNotes);
}

static gboolean
coowner_slots_handler (xmlNodePtr node, gpointer coowner_pdata)
{
    struct coowner_pdata* pdata = static_cast<decltype (pdata)> (coowner_pdata);
    return dom_tree_create_instance_slots (node, QOF_INSTANCE (pdata->coowner));
}

static gboolean
coowner_terms_handler (xmlNodePtr node, gpointer coowner_pdata)
{
    struct coowner_pdata* pdata = static_cast<decltype (pdata)> (coowner_pdata);
    GncGUID* guid;
    GncBillTerm* term;

    guid = dom_tree_to_guid (node);
    g_return_val_if_fail (guid, FALSE);
    term = gnc_billterm_xml_find_or_create (pdata->book, guid);
    g_assert (term);
    guid_free (guid);
    gncCoOwnerSetTerms (pdata->coowner, term);

    return TRUE;
}

static gboolean
coowner_taxincluded_handler (xmlNodePtr node, gpointer coowner_pdata)
{
    struct coowner_pdata* pdata = static_cast<decltype (pdata)> (coowner_pdata);
    GncTaxIncluded type;
    char* str;
    gboolean ret;

    str = dom_tree_to_text (node);
    g_return_val_if_fail (str, FALSE);

    ret = gncTaxIncludedStringToType (str, &type);
    g_free (str);

    if (ret)
gncCoOwnerSetTaxIncluded (pdata->coowner, type);

    return ret;
}

static gboolean
coowner_taxtable_handler (xmlNodePtr node, gpointer coowner_pdata)
{
    struct coowner_pdata* pdata = static_cast<decltype (pdata)> (coowner_pdata);
    GncGUID* guid;
    GncTaxTable* taxtable;

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

    gncCoOwnerSetTaxTable (pdata->coowner, taxtable);
    guid_free (guid);
    return TRUE;
}

static gboolean
coowner_taxtableoverride_handler (xmlNodePtr node, gpointer coowner_pdata)
{
    struct coowner_pdata* pdata = static_cast<decltype (pdata)> (coowner_pdata);
    return set_boolean (node, pdata->coowner, gncCoOwnerSetTaxTableOverride);
}

static gboolean
coowner_username_handler (xmlNodePtr node, gpointer coowner_pdata)
{
    struct coowner_pdata* pdata = static_cast<decltype (pdata)> (coowner_pdata);

    return set_string (node, pdata->coowner, gncCoOwnerSetUsername);
}

/* Helper functions */

static struct dom_tree_handler coowner_handlers_v2[] =
{
    { coowner_id_string, coowner_id_handler, 1, 0 },
    { coowner_guid_string, coowner_guid_handler, 1, 0 },
    { coowner_active_string, coowner_active_handler, 1, 0 },
    { coowner_addr_string, coowner_addr_handler, 1, 0 },
    { coowner_apt_share_string, coowner_apt_share_handler, 1, 0 },
    { coowner_apt_unit_string, coowner_apt_unit_handler, 1, 0 },
    { "coowner:commodity", coowner_currency_handler, 0, 0 },
    { coowner_credit_string, coowner_credit_handler, 0, 0 },
    { coowner_currency_string, coowner_currency_handler, 0, 0 },
    { coowner_discount_string, coowner_discount_handler, 0, 0 },
    { coowner_distribution_key_string, coowner_distribution_key_handler, 0, 0 },
    { coowner_notes_string, coowner_notes_handler, 0, 0 },
    { coowner_slots_string, coowner_slots_handler, 0, 0 },
    { coowner_terms_string, coowner_terms_handler, 0, 0 },
    { coowner_taxincluded_string, coowner_taxincluded_handler, 1, 0 },
    { coowner_taxtable_string, coowner_taxtable_handler, 0, 0 },
    { coowner_taxtableoverride_string, coowner_taxtableoverride_handler, 0, 0 },
    { coowner_username_string, coowner_username_handler, 1, 0 },
    { NULL, 0, 0, 0 }
};

static GncCoOwner*
dom_tree_to_coowner (xmlNodePtr node, QofBook* book)
{
    struct coowner_pdata coowner_pdata;
    gboolean successful;

    coowner_pdata.coowner = gncCoOwnerCreate (book);
    coowner_pdata.book = book;
    gncCoOwnerBeginEdit (coowner_pdata.coowner);

    successful = dom_tree_generic_parse (node, coowner_handlers_v2,
                                         &coowner_pdata);

    if (successful)
        gncCoOwnerCommitEdit (coowner_pdata.coowner);
    else
    {
        PERR ("failed to parse coowner tree");
        gncCoOwnerDestroy (coowner_pdata.coowner);
        coowner_pdata.coowner = NULL;
    }

    return coowner_pdata.coowner;
}

static gboolean
gnc_coowner_end_handler (gpointer data_for_children,
                          GSList* data_from_children, GSList* sibling_data,
                          gpointer parent_data, gpointer global_data,
                          gpointer* result, const gchar* tag)
{
    GncCoOwner* coowner;
    xmlNodePtr tree = (xmlNodePtr)data_for_children;
    gxpf_data* gdata = (gxpf_data*)global_data;
    QofBook* book = static_cast<decltype (book)> (gdata->bookdata);


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

    g_return_val_if_fail (tree, FALSE);

    coowner = dom_tree_to_coowner (tree, book);
    if (coowner != NULL)
    {
gdata->cb (tag, gdata->parsedata, coowner);
    }

    xmlFreeNode (tree);

    return coowner != NULL;
}

static sixtp*
coowner_sixtp_parser_create (void)
{
    return sixtp_dom_parser_new (gnc_coowner_end_handler, NULL, NULL);
}

static gboolean
coowner_should_be_saved (GncCoOwner* coowner)
{
    const char* id;

    /* make sure this is a valid coowner before we save it -- should have an ID */
    id = gncCoOwnerGetID (coowner);
    if (id == NULL || *id == '\0')
return FALSE;

    return TRUE;
}

static void
do_count (QofInstance* coowner_p, gpointer count_p)
{
    int* count = static_cast<decltype (count)> (count_p);
    if (coowner_should_be_saved ((GncCoOwner*)coowner_p))
(*count)++;
}

static int
coowner_get_count (QofBook* book)
{
    int count = 0;
    qof_object_foreach (_GNC_MOD_NAME, book, do_count, (gpointer) &count);
    return count;
}

static void
xml_add_coowner (QofInstance* coowner_p, gpointer out_p)
{
    xmlNodePtr node;
    GncCoOwner* coowner = (GncCoOwner*) coowner_p;
    FILE* out = static_cast<decltype (out)> (out_p);

    if (ferror (out))
return;
    if (!coowner_should_be_saved (coowner))
return;

    node = coowner_dom_tree_create (coowner);
    xmlElemDump (out, NULL, node);
    xmlFreeNode (node);
    if (ferror (out) || fprintf (out, "\n") < 0)
return;
}

static gboolean
coowner_write (FILE* out, QofBook* book)
{
    qof_object_foreach_sorted (_GNC_MOD_NAME, book, xml_add_coowner,
(gpointer) out);
    return ferror (out) == 0;
}

static gboolean
coowner_ns (FILE* out)
{
    g_return_val_if_fail (out, FALSE);
    return gnc_xml2_write_namespace_decl (out, "coowner");
}

void
gnc_coowner_xml_initialize (void)
{
    static GncXmlDataType_t be_data =
    {
GNC_FILE_BACKEND_VERS,
gnc_coowner_string,
coowner_sixtp_parser_create,
NULL,           /* add_item */
coowner_get_count,
coowner_write,
NULL,           /* scrub */
coowner_ns,
    };

    gnc_xml_register_backend (be_data);
}
