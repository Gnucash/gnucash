/********************************************************************\
 * gnc-tax-table-xml-v2.c -- tax table xml i/o implementation       *
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

#include "gncTaxTableP.h"
#include "gnc-tax-table-xml-v2.h"
#include "gnc-engine-util.h"

#include "gncObject.h"

#define _GNC_MOD_NAME	GNC_TAXTABLE_MODULE_NAME

static short module = MOD_IO;

const gchar *taxtable_version_string = "2.0.0";

/* ids */
#define gnc_taxtable_string "gnc:GncTaxTable"
#define taxtable_guid_string "taxtable:guid"
#define taxtable_name_string "taxtable:name"
#define taxtable_refcount_string "taxtable:refcount"
#define taxtable_invisible_string "taxtable:invisible"
#define taxtable_parent_string "taxtable:parent"
#define taxtable_child_string "taxtable:child"
#define taxtable_entries_string "taxtable:entries"

#define gnc_taxtableentry_string "gnc:GncTaxTableEntry"
#define ttentry_account_string "tte:acct"
#define ttentry_type_string "tte:type"
#define ttentry_amount_string "tte:amount"

static void
maybe_add_guid (xmlNodePtr ptr, const char *tag, GncTaxTable *table)
{
  if (table)
    xmlAddChild (ptr, guid_to_dom_tree (tag, gncTaxTableGetGUID (table)));
}

static xmlNodePtr
ttentry_dom_tree_create (GncTaxTableEntry *entry)
{
  xmlNodePtr ret;
  Account *account;
  gnc_numeric amount;

  ret = xmlNewNode(NULL, gnc_taxtableentry_string);

  account = gncTaxTableEntryGetAccount (entry);
  if (account)
    xmlAddChild(ret, guid_to_dom_tree (ttentry_account_string,
				       xaccAccountGetGUID (account)));

  amount = gncTaxTableEntryGetAmount (entry);
  xmlAddChild (ret, gnc_numeric_to_dom_tree (ttentry_amount_string, &amount));

  xmlAddChild(ret, text_to_dom_tree (ttentry_type_string,
				     gncAmountTypeToString (
				    gncTaxTableEntryGetType (entry))));

  return ret;
}

static xmlNodePtr
taxtable_dom_tree_create (GncTaxTable *table)
{
    xmlNodePtr ret, entries;
    GList *list;

    ret = xmlNewNode(NULL, gnc_taxtable_string);
    xmlSetProp(ret, "version", taxtable_version_string);

    maybe_add_guid(ret, taxtable_guid_string, table);
    xmlAddChild(ret, text_to_dom_tree (taxtable_name_string,
				       gncTaxTableGetName (table)));

    xmlAddChild(ret, int_to_dom_tree (taxtable_refcount_string,
				      gncTaxTableGetRefcount (table)));
    xmlAddChild(ret, int_to_dom_tree (taxtable_invisible_string,
				      gncTaxTableGetInvisible (table)));

    maybe_add_guid(ret, taxtable_child_string, gncTaxTableGetChild (table));
    maybe_add_guid(ret, taxtable_parent_string, gncTaxTableGetParent (table));

    entries = xmlNewChild (ret, NULL, taxtable_entries_string, NULL);
    for (list = gncTaxTableGetEntries (table); list; list = list->next) {
      GncTaxTableEntry *entry = list->data;
      xmlAddChild(entries, ttentry_dom_tree_create (entry));
    }

    return ret;
}

/***********************************************************************/

struct ttentry_pdata
{
  GncTaxTableEntry *ttentry;
  GNCBook *book;
};

static gboolean
ttentry_acct_handler (xmlNodePtr node, gpointer ttentry_pdata)
{
  struct ttentry_pdata *pdata = ttentry_pdata;
  GUID *guid;
  Account * acc;

  guid = dom_tree_to_guid (node);
  g_return_val_if_fail (guid, FALSE);
  acc = xaccAccountLookup (guid, pdata->book);
  g_free (guid);
  g_return_val_if_fail (acc, FALSE);

  gncTaxTableEntrySetAccount (pdata->ttentry, acc);
  return TRUE;
}

static gboolean
ttentry_type_handler (xmlNodePtr node, gpointer taxtable_pdata)
{
  struct ttentry_pdata *pdata = taxtable_pdata;
  GncAmountType type;
  char *str;
  gboolean ret;

  str = dom_tree_to_text (node);
  g_return_val_if_fail (str, FALSE);

  ret = gncAmountStringToType (str, &type);
  g_free (str);

  if (ret)
    gncTaxTableEntrySetType (pdata->ttentry, type);

  return ret;
}

static gboolean
ttentry_amount_handler (xmlNodePtr node, gpointer ttentry_pdata)
{
  struct ttentry_pdata *pdata = ttentry_pdata;
  gnc_numeric* num = dom_tree_to_gnc_numeric(node);
  g_return_val_if_fail(num, FALSE);
    
  gncTaxTableEntrySetAmount (pdata->ttentry, *num);
  g_free(num);
  return TRUE;
}

static struct dom_tree_handler ttentry_handlers_v2[] = {
  { ttentry_account_string, ttentry_acct_handler, 0, 0 },
  { ttentry_type_string, ttentry_type_handler, 1, 0 },
  { ttentry_amount_string, ttentry_amount_handler, 1, 0 },
  { NULL, 0, 0, 0 }
};

static GncTaxTableEntry*
dom_tree_to_ttentry (xmlNodePtr node, GNCBook *book)
{
  struct ttentry_pdata ttentry_pdata;
  gboolean successful;
  
  ttentry_pdata.ttentry = gncTaxTableEntryCreate ();
  ttentry_pdata.book = book;

  successful = dom_tree_generic_parse (node, ttentry_handlers_v2,
				       &ttentry_pdata);

  if (!successful) {
    PERR ("failed to parse tax table entry tree");
    gncTaxTableEntryDestroy (ttentry_pdata.ttentry);
    ttentry_pdata.ttentry = NULL;
  }

  return ttentry_pdata.ttentry;
}

/***********************************************************************/

struct taxtable_pdata
{
  GncTaxTable *table;
  GNCBook *book;
};

static gboolean
set_parent_child (xmlNodePtr node, struct taxtable_pdata *pdata,
		  void (*func)(GncTaxTable *, GncTaxTable *))
{
  GUID *guid;
  GncTaxTable *table;

  guid = dom_tree_to_guid(node);
  g_return_val_if_fail (guid, FALSE);
  table = gncTaxTableLookup (pdata->book, guid);
  if (!table) {
    table = gncTaxTableCreate (pdata->book);
    gncTaxTableSetGUID (table, guid);
  }
  g_free (guid);
  g_return_val_if_fail (table, FALSE);
  func (pdata->table, table);

  return TRUE;
}

static gboolean
taxtable_guid_handler (xmlNodePtr node, gpointer taxtable_pdata)
{
    struct taxtable_pdata *pdata = taxtable_pdata;
    GUID *guid;
    GncTaxTable *table;

    guid = dom_tree_to_guid(node);
    g_return_val_if_fail (guid, FALSE);
    table = gncTaxTableLookup (pdata->book, guid);
    if (table) {
      gncTaxTableDestroy (pdata->table);
      pdata->table = table;
    } else {
      gncTaxTableSetGUID(pdata->table, guid);
    }

    g_free(guid);
    
    return TRUE;
}

static gboolean
taxtable_name_handler (xmlNodePtr node, gpointer taxtable_pdata)
{
  struct taxtable_pdata *pdata = taxtable_pdata;
  char* txt = dom_tree_to_text(node);
  g_return_val_if_fail(txt, FALSE);
    
  gncTaxTableSetName (pdata->table, txt);
  g_free(txt);
  return TRUE;
}

static gboolean
taxtable_refcount_handler (xmlNodePtr node, gpointer taxtable_pdata)
{
  struct taxtable_pdata *pdata = taxtable_pdata;
  gint64 val;

  dom_tree_to_integer(node, &val);
  gncTaxTableSetRefcount (pdata->table, val);
  return TRUE;
}

static gboolean
taxtable_invisible_handler (xmlNodePtr node, gpointer taxtable_pdata)
{
  struct taxtable_pdata *pdata = taxtable_pdata;
  gint64 val;

  dom_tree_to_integer(node, &val);
  if (val)
    gncTaxTableMakeInvisible (pdata->table);
  return TRUE;
}

static gboolean
taxtable_parent_handler (xmlNodePtr node, gpointer taxtable_pdata)
{
  struct taxtable_pdata *pdata = taxtable_pdata;
  return set_parent_child (node, pdata, gncTaxTableSetParent);
}

static gboolean
taxtable_child_handler (xmlNodePtr node, gpointer taxtable_pdata)
{
  struct taxtable_pdata *pdata = taxtable_pdata;
  return set_parent_child (node, pdata, gncTaxTableSetChild);
}

static gboolean
taxtable_entries_handler (xmlNodePtr node, gpointer taxtable_pdata)
{
  struct taxtable_pdata *pdata = taxtable_pdata;
  xmlNodePtr mark;

  g_return_val_if_fail (node, FALSE);
  g_return_val_if_fail (node->xmlChildrenNode, FALSE);

  for (mark = node->xmlChildrenNode; mark; mark = mark->next) {
    GncTaxTableEntry *entry;
        
    if (safe_strcmp ("text", mark->name) == 0)
      continue;

    if (safe_strcmp (gnc_taxtableentry_string, mark->name))
      return FALSE;

    entry = dom_tree_to_ttentry (mark, pdata->book);

    if (entry)
      gncTaxTableAddEntry (pdata->table, entry);
    else
      return FALSE;

  }
  return TRUE;
}

static struct dom_tree_handler taxtable_handlers_v2[] = {
    { taxtable_guid_string, taxtable_guid_handler, 1, 0 },
    { taxtable_name_string, taxtable_name_handler, 1, 0 },
    { taxtable_refcount_string, taxtable_refcount_handler, 1, 0 },
    { taxtable_invisible_string, taxtable_invisible_handler, 1, 0 },
    { taxtable_parent_string, taxtable_parent_handler, 0, 0 },
    { taxtable_child_string, taxtable_child_handler, 0, 0 },
    { taxtable_entries_string, taxtable_entries_handler, 1, 0 },
    { NULL, 0, 0, 0 }
};

static GncTaxTable*
dom_tree_to_taxtable (xmlNodePtr node, GNCBook *book)
{
  struct taxtable_pdata taxtable_pdata;
  gboolean successful;
  
  taxtable_pdata.table = gncTaxTableCreate (book);
  taxtable_pdata.book = book;

  successful = dom_tree_generic_parse (node, taxtable_handlers_v2,
				       &taxtable_pdata);
  gncTaxTableCommitEdit (taxtable_pdata.table);

  if (!successful) {
    PERR ("failed to parse tax table tree");
    gncTaxTableDestroy (taxtable_pdata.table);
    taxtable_pdata.table = NULL;
  }

  return taxtable_pdata.table;
}

static gboolean
gnc_taxtable_end_handler(gpointer data_for_children,
			 GSList* data_from_children, GSList* sibling_data,
			 gpointer parent_data, gpointer global_data,
			 gpointer *result, const gchar *tag)
{
    int successful;
    GncTaxTable *table;
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

    table = dom_tree_to_taxtable (tree, book);
    if(table != NULL)
    {
        gdata->cb(tag, gdata->parsedata, table);
    }

    xmlFreeNode(tree);

    return table != NULL;
}

static sixtp *
taxtable_sixtp_parser_create(void)
{
  return sixtp_dom_parser_new(gnc_taxtable_end_handler, NULL, NULL);
}

static void
do_count (gpointer table_p, gpointer count_p)
{
  int *count = count_p;
  (*count)++;
}

static int
taxtable_get_count (GNCBook *book)
{
  int count = 0;
  gncObjectForeach (_GNC_MOD_NAME, book, do_count, (gpointer) &count);
  return count;
}

static void
xml_add_taxtable (gpointer table_p, gpointer out_p)
{
  xmlNodePtr node;
  GncTaxTable *table = table_p;
  FILE *out = out_p;

  node = taxtable_dom_tree_create (table);
  xmlElemDump(out, NULL, node);
  fprintf(out, "\n");
  xmlFreeNode (node);
}

static void
taxtable_write (FILE *out, GNCBook *book)
{
  gncObjectForeach (_GNC_MOD_NAME, book, xml_add_taxtable, (gpointer) out);
}

void
gnc_taxtable_xml_initialize (void)
{
  static GncXmlDataType_t be_data = {
    GNC_FILE_BACKEND_VERS,
    gnc_taxtable_string,
    taxtable_sixtp_parser_create,
    NULL,			/* add_item */
    taxtable_get_count,
    taxtable_write,
  };

  gncObjectRegisterBackend (_GNC_MOD_NAME,
			    GNC_FILE_BACKEND,
			    &be_data);
}
