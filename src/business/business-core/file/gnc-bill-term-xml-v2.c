/********************************************************************\
 * gnc-bill-term-xml-v2.c -- billing term xml i/o implementation    *
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
#include "gnc-bill-term-xml-v2.h"
#include "gnc-engine-util.h"

#include "gncObject.h"

#define _GNC_MOD_NAME	GNC_BILLTERM_MODULE_NAME

static short module = MOD_IO;

const gchar *billterm_version_string = "2.0.0";

/* ids */
#define gnc_billterm_string "gnc:GncBillTerm"
#define billterm_guid_string "billterm:guid"
#define billterm_name_string "billterm:name"
#define billterm_desc_string "billterm:desc"
#define billterm_refcount_string "billterm:refcount"
#define billterm_invisible_string "billterm:invisible"
#define billterm_parent_string "billterm:parent"
#define billterm_child_string "billterm:child"

#define gnc_daystype_string "billterm:days"
#define days_duedays_string "bt-days:due-days"
#define days_discdays_string "bt-days:disc-days"
#define days_discount_string "bt-days:discount"

#define gnc_proximotype_string "billterm:proximo"
#define prox_dueday_string "bt-prox:due-day"
#define prox_discday_string "bt-prox:disc-day"
#define prox_discount_string "bt-prox:discount"
#define prox_cutoff_string "bt-prox:cutoff-day"

static void
maybe_add_guid (xmlNodePtr ptr, const char *tag, GncBillTerm *term)
{
  if (term)
    xmlAddChild (ptr, guid_to_dom_tree (tag, gncBillTermGetGUID (term)));
}

static void
maybe_add_int (xmlNodePtr ptr, const char *tag, gint val)
{
  if (val)
    xmlAddChild (ptr, int_to_dom_tree (tag, val));
}

static void
maybe_add_numeric (xmlNodePtr ptr, const char *tag, gnc_numeric val)
{
  if (!gnc_numeric_zero_p (val))
    xmlAddChild (ptr, gnc_numeric_to_dom_tree (tag, &val));
}

static xmlNodePtr
billterm_dom_tree_create (GncBillTerm *term)
{
    xmlNodePtr ret, data;

    ret = xmlNewNode(NULL, gnc_billterm_string);
    xmlSetProp(ret, "version", billterm_version_string);

    maybe_add_guid(ret, billterm_guid_string, term);
    xmlAddChild(ret, text_to_dom_tree (billterm_name_string,
				       gncBillTermGetName (term)));
    xmlAddChild(ret, text_to_dom_tree (billterm_desc_string,
				       gncBillTermGetDescription (term)));

    xmlAddChild(ret, int_to_dom_tree (billterm_refcount_string,
				      gncBillTermGetRefcount (term)));
    xmlAddChild(ret, int_to_dom_tree (billterm_invisible_string,
				      gncBillTermGetInvisible (term)));

    maybe_add_guid(ret, billterm_parent_string, gncBillTermGetParent (term));
    maybe_add_guid(ret, billterm_child_string, gncBillTermGetChild (term));

    switch (gncBillTermGetType (term)) {
    case GNC_TERM_TYPE_DAYS:
      data = xmlNewChild (ret, NULL, gnc_daystype_string, NULL);
      maybe_add_int (data, days_duedays_string, gncBillTermGetDueDays (term));
      maybe_add_int (data, days_discdays_string,
		     gncBillTermGetDiscountDays (term));
      maybe_add_numeric (data, days_discount_string,
			 gncBillTermGetDiscount (term));
      break;

    case GNC_TERM_TYPE_PROXIMO:
      data = xmlNewChild (ret, NULL, gnc_proximotype_string, NULL);
      maybe_add_int (data, prox_dueday_string, gncBillTermGetDueDays (term));
      maybe_add_int (data, prox_discday_string,
		     gncBillTermGetDiscountDays (term));
      maybe_add_numeric (data, prox_discount_string,
			 gncBillTermGetDiscount (term));
      maybe_add_int (data, prox_cutoff_string, gncBillTermGetCutoff (term));
      break;
    }

    return ret;
}

/***********************************************************************/

struct billterm_pdata
{
  GncBillTerm *term;
  GNCBook *book;
};

static gboolean
set_int (xmlNodePtr node, GncBillTerm *term,
	 void (*func)(GncBillTerm *, gint))
{
  gint64 val;
  dom_tree_to_integer (node, &val);
  func (term, val);
  return TRUE;
}

static gboolean
set_numeric (xmlNodePtr node, GncBillTerm *term,
	     void (*func)(GncBillTerm *, gnc_numeric))
{
  gnc_numeric* num = dom_tree_to_gnc_numeric (node);
  g_return_val_if_fail (num, FALSE);
    
  func (term, *num);
  g_free (num);
  return TRUE;
}

/***********************************************************************/

static gboolean
days_duedays_handler (xmlNodePtr node, gpointer billterm_pdata)
{
  struct billterm_pdata *pdata = billterm_pdata;
  return set_int (node, pdata->term, gncBillTermSetDueDays);
}

static gboolean
days_discdays_handler (xmlNodePtr node, gpointer billterm_pdata)
{
  struct billterm_pdata *pdata = billterm_pdata;
  return set_int (node, pdata->term, gncBillTermSetDiscountDays);
}

static gboolean
days_discount_handler (xmlNodePtr node, gpointer billterm_pdata)
{
  struct billterm_pdata *pdata = billterm_pdata;
  return set_numeric (node, pdata->term, gncBillTermSetDiscount);
}

static struct dom_tree_handler days_data_handlers_v2[] = {
  { days_duedays_string, days_duedays_handler, 0, 0 },
  { days_discdays_string, days_discdays_handler, 0, 0 },
  { days_discount_string, days_discount_handler, 0, 0 },
  { NULL, 0, 0, 0 }
};

static gboolean
dom_tree_to_days_data (xmlNodePtr node, struct billterm_pdata *pdata)
{
  gboolean successful;
  
  successful = dom_tree_generic_parse (node, days_data_handlers_v2, pdata);

  if (!successful)
    PERR ("failed to parse billing term days data");

  return successful;
}

/***********************************************************************/

static gboolean
prox_dueday_handler (xmlNodePtr node, gpointer billterm_pdata)
{
  struct billterm_pdata *pdata = billterm_pdata;
  return set_int (node, pdata->term, gncBillTermSetDueDays);
}

static gboolean
prox_discday_handler (xmlNodePtr node, gpointer billterm_pdata)
{
  struct billterm_pdata *pdata = billterm_pdata;
  return set_int (node, pdata->term, gncBillTermSetDiscountDays);
}

static gboolean
prox_discount_handler (xmlNodePtr node, gpointer billterm_pdata)
{
  struct billterm_pdata *pdata = billterm_pdata;
  return set_numeric (node, pdata->term, gncBillTermSetDiscount);
}

static gboolean
prox_cutoff_handler (xmlNodePtr node, gpointer billterm_pdata)
{
  struct billterm_pdata *pdata = billterm_pdata;
  return set_int (node, pdata->term, gncBillTermSetCutoff);
}

static struct dom_tree_handler prox_data_handlers_v2[] = {
  { prox_dueday_string, prox_dueday_handler, 0, 0 },
  { prox_discday_string, prox_discday_handler, 0, 0 },
  { prox_discount_string, prox_discount_handler, 0, 0 },
  { prox_cutoff_string, prox_cutoff_handler, 0, 0 },
  { NULL, 0, 0, 0 }
};

static gboolean
dom_tree_to_prox_data (xmlNodePtr node, struct billterm_pdata *pdata)
{
  gboolean successful;
  
  successful = dom_tree_generic_parse (node, prox_data_handlers_v2, pdata);

  if (!successful)
    PERR ("failed to parse billing term prox data");

  return successful;
}

/***********************************************************************/

static gboolean
set_parent_child (xmlNodePtr node, struct billterm_pdata *pdata,
		  void (*func)(GncBillTerm *, GncBillTerm *))
{
  GUID *guid;
  GncBillTerm *term;

  guid = dom_tree_to_guid(node);
  g_return_val_if_fail (guid, FALSE);
  term = gncBillTermLookup (pdata->book, guid);
  if (!term) {
    term = gncBillTermCreate (pdata->book);
    gncBillTermSetGUID (term, guid);
  }
  g_free (guid);
  g_return_val_if_fail (term, FALSE);
  func (pdata->term, term);

  return TRUE;
}

static gboolean
set_string (xmlNodePtr node, GncBillTerm *term,
	    void (*func)(GncBillTerm *, const char *))
{
  char* txt = dom_tree_to_text(node);
  g_return_val_if_fail(txt, FALSE);
  func (term, txt);
  g_free(txt);
  return TRUE;
}

static gboolean
billterm_guid_handler (xmlNodePtr node, gpointer billterm_pdata)
{
    struct billterm_pdata *pdata = billterm_pdata;
    GUID *guid;
    GncBillTerm *term;

    guid = dom_tree_to_guid(node);
    g_return_val_if_fail (guid, FALSE);
    term = gncBillTermLookup (pdata->book, guid);
    if (term) {
      gncBillTermDestroy (pdata->term);
      pdata->term = term;
    } else {
      gncBillTermSetGUID(pdata->term, guid);
    }

    g_free(guid);
    
    return TRUE;
}

static gboolean
billterm_name_handler (xmlNodePtr node, gpointer billterm_pdata)
{
  struct billterm_pdata *pdata = billterm_pdata;
  return set_string (node, pdata->term, gncBillTermSetName);
}

static gboolean
billterm_desc_handler (xmlNodePtr node, gpointer billterm_pdata)
{
  struct billterm_pdata *pdata = billterm_pdata;
  return set_string (node, pdata->term, gncBillTermSetDescription);
}

static gboolean
billterm_refcount_handler (xmlNodePtr node, gpointer billterm_pdata)
{
  struct billterm_pdata *pdata = billterm_pdata;
  gint64 val;

  dom_tree_to_integer(node, &val);
  gncBillTermSetRefcount (pdata->term, val);
  return TRUE;
}

static gboolean
billterm_invisible_handler (xmlNodePtr node, gpointer billterm_pdata)
{
  struct billterm_pdata *pdata = billterm_pdata;
  gint64 val;

  dom_tree_to_integer(node, &val);
  if (val)
    gncBillTermMakeInvisible (pdata->term);
  return TRUE;
}

static gboolean
billterm_parent_handler (xmlNodePtr node, gpointer billterm_pdata)
{
  struct billterm_pdata *pdata = billterm_pdata;
  return set_parent_child (node, pdata, gncBillTermSetParent);
}

static gboolean
billterm_child_handler (xmlNodePtr node, gpointer billterm_pdata)
{
  struct billterm_pdata *pdata = billterm_pdata;
  return set_parent_child (node, pdata, gncBillTermSetChild);
}

static gboolean
billterm_days_data_handler (xmlNodePtr node, gpointer billterm_pdata)
{
  struct billterm_pdata *pdata = billterm_pdata;

  g_return_val_if_fail (node, FALSE);
  g_return_val_if_fail (gncBillTermGetType (pdata->term) == 0, FALSE);

  gncBillTermSetType (pdata->term, GNC_TERM_TYPE_DAYS);
  return dom_tree_to_days_data (node, pdata);
}

static gboolean
billterm_prox_data_handler (xmlNodePtr node, gpointer billterm_pdata)
{
  struct billterm_pdata *pdata = billterm_pdata;

  g_return_val_if_fail (node, FALSE);
  g_return_val_if_fail (gncBillTermGetType (pdata->term) == 0, FALSE);

  gncBillTermSetType (pdata->term, GNC_TERM_TYPE_PROXIMO);
  return dom_tree_to_prox_data (node, pdata);
}

static struct dom_tree_handler billterm_handlers_v2[] = {
    { billterm_guid_string, billterm_guid_handler, 1, 0 },
    { billterm_name_string, billterm_name_handler, 1, 0 },
    { billterm_desc_string, billterm_desc_handler, 1, 0 },
    { billterm_refcount_string, billterm_refcount_handler, 1, 0 },
    { billterm_invisible_string, billterm_invisible_handler, 1, 0 },
    { billterm_parent_string, billterm_parent_handler, 0, 0 },
    { billterm_child_string, billterm_child_handler, 0, 0 },
    { gnc_daystype_string, billterm_days_data_handler, 0, 0 },
    { gnc_proximotype_string, billterm_prox_data_handler, 0, 0 },
    { NULL, 0, 0, 0 }
};

static GncBillTerm*
dom_tree_to_billterm (xmlNodePtr node, GNCBook *book)
{
  struct billterm_pdata billterm_pdata;
  gboolean successful;
  
  billterm_pdata.term = gncBillTermCreate (book);
  billterm_pdata.book = book;

  successful = dom_tree_generic_parse (node, billterm_handlers_v2,
				       &billterm_pdata);
  gncBillTermCommitEdit (billterm_pdata.term);

  if (!successful) {
    PERR ("failed to parse billing term tree");
    gncBillTermDestroy (billterm_pdata.term);
    billterm_pdata.term = NULL;
  }

  return billterm_pdata.term;
}

static gboolean
gnc_billterm_end_handler(gpointer data_for_children,
			 GSList* data_from_children, GSList* sibling_data,
			 gpointer parent_data, gpointer global_data,
			 gpointer *result, const gchar *tag)
{
    int successful;
    GncBillTerm *term;
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

    term = dom_tree_to_billterm (tree, book);
    if(term != NULL)
    {
        gdata->cb(tag, gdata->parsedata, term);
    }

    xmlFreeNode(tree);

    return term != NULL;
}

static sixtp *
billterm_sixtp_parser_create(void)
{
  return sixtp_dom_parser_new(gnc_billterm_end_handler, NULL, NULL);
}

static void
do_count (gpointer term_p, gpointer count_p)
{
  int *count = count_p;
  (*count)++;
}

static int
billterm_get_count (GNCBook *book)
{
  int count = 0;
  gncObjectForeach (_GNC_MOD_NAME, book, do_count, (gpointer) &count);
  return count;
}

static void
xml_add_billterm (gpointer term_p, gpointer out_p)
{
  xmlNodePtr node;
  GncBillTerm *term = term_p;
  FILE *out = out_p;

  node = billterm_dom_tree_create (term);
  xmlElemDump(out, NULL, node);
  fprintf(out, "\n");
  xmlFreeNode (node);
}

static void
billterm_write (FILE *out, GNCBook *book)
{
  gncObjectForeach (_GNC_MOD_NAME, book, xml_add_billterm, (gpointer) out);
}

void
gnc_billterm_xml_initialize (void)
{
  static GncXmlDataType_t be_data = {
    GNC_FILE_BACKEND_VERS,
    gnc_billterm_string,
    billterm_sixtp_parser_create,
    NULL,			/* add_item */
    billterm_get_count,
    billterm_write,
  };

  gncObjectRegisterBackend (_GNC_MOD_NAME,
			    GNC_FILE_BACKEND,
			    &be_data);
}
