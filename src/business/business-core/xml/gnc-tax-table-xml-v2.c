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

#include "gncEntry.h"
#include "gncTaxTableP.h"
#include "gnc-tax-table-xml-v2.h"

#define _GNC_MOD_NAME	GNC_ID_TAXTABLE

static QofLogModule log_module = GNC_MOD_IO;

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
#define taxtable_slots_string "taxtable:slots"

#define gnc_taxtableentry_string "gnc:GncTaxTableEntry"
#define ttentry_account_string "tte:acct"
#define ttentry_type_string "tte:type"
#define ttentry_amount_string "tte:amount"

static void
maybe_add_guid (xmlNodePtr ptr, const char *tag, GncTaxTable *table)
{
  if (table)
    xmlAddChild (ptr, guid_to_dom_tree (tag, 
            qof_instance_get_guid(QOF_INSTANCE(table))));
}

static xmlNodePtr
ttentry_dom_tree_create (GncTaxTableEntry *entry)
{
  xmlNodePtr ret;
  Account *account;
  gnc_numeric amount;

  ret = xmlNewNode(NULL, BAD_CAST gnc_taxtableentry_string);

  account = gncTaxTableEntryGetAccount (entry);
  if (account)
    xmlAddChild(ret, guid_to_dom_tree (ttentry_account_string,
				       qof_instance_get_guid (QOF_INSTANCE(account))));

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

    ret = xmlNewNode(NULL, BAD_CAST gnc_taxtable_string);
    xmlSetProp(ret, BAD_CAST "version", BAD_CAST taxtable_version_string);

    maybe_add_guid(ret, taxtable_guid_string, table);
    xmlAddChild(ret, text_to_dom_tree (taxtable_name_string,
				       gncTaxTableGetName (table)));

    xmlAddChild(ret, int_to_dom_tree (taxtable_refcount_string,
				      gncTaxTableGetRefcount (table)));
    xmlAddChild(ret, int_to_dom_tree (taxtable_invisible_string,
				      gncTaxTableGetInvisible (table)));

    /* We should not be our own child */
    if (gncTaxTableGetChild(table) != table)
      maybe_add_guid(ret, taxtable_child_string, gncTaxTableGetChild (table));

    maybe_add_guid(ret, taxtable_parent_string, gncTaxTableGetParent (table));

    entries = xmlNewChild (ret, NULL, BAD_CAST taxtable_entries_string, NULL);
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
  QofBook *book;
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
dom_tree_to_ttentry (xmlNodePtr node, QofBook *book)
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
  QofBook *book;
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

  /* Ignore pointers to self */
  if (table == pdata->table) {
    PINFO ("found a self-referential parent/child; ignoring.\n");
    return TRUE;
  }

  if (!table) {
    table = gncTaxTableCreate (pdata->book);
    gncTaxTableBeginEdit (table);
    gncTaxTableSetGUID (table, guid);
    gncTaxTableCommitEdit (table);
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
      gncTaxTableBeginEdit (table);
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
        
    if (safe_strcmp ("text", (char*)mark->name) == 0)
      continue;

    if (safe_strcmp (gnc_taxtableentry_string, (char*)mark->name))
      return FALSE;

    entry = dom_tree_to_ttentry (mark, pdata->book);

    if (entry)
      gncTaxTableAddEntry (pdata->table, entry);
    else
      return FALSE;

  }
  return TRUE;
}

static gboolean
taxtable_slots_handler (xmlNodePtr node, gpointer taxtable_pdata)
{
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
    { taxtable_slots_string, taxtable_slots_handler, 0, 0 },
    { NULL, 0, 0, 0 }
};

static GncTaxTable*
dom_tree_to_taxtable (xmlNodePtr node, QofBook *book)
{
  struct taxtable_pdata taxtable_pdata;
  gboolean successful;
  
  taxtable_pdata.table = gncTaxTableCreate (book);
  taxtable_pdata.book = book;
  gncTaxTableBeginEdit (taxtable_pdata.table);

  successful = dom_tree_generic_parse (node, taxtable_handlers_v2,
				       &taxtable_pdata);

  if (successful)
    gncTaxTableCommitEdit (taxtable_pdata.table);
  else
  {
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
    QofBook *book = gdata->bookdata;

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
do_count (QofInstance * table_p, gpointer count_p)
{
  int *count = count_p;
  (*count)++;
}

static int
taxtable_get_count (QofBook *book)
{
  int count = 0;
  qof_object_foreach (_GNC_MOD_NAME, book, do_count, (gpointer) &count);
  return count;
}

static void
xml_add_taxtable (QofInstance * table_p, gpointer out_p)
{
  xmlNodePtr node;
  GncTaxTable *table = (GncTaxTable *) table_p;
  FILE *out = out_p;

  node = taxtable_dom_tree_create (table);
  xmlElemDump(out, NULL, node);
  fprintf(out, "\n");
  xmlFreeNode (node);
}

static void
taxtable_write (FILE *out, QofBook *book)
{
  qof_object_foreach (_GNC_MOD_NAME, book, xml_add_taxtable, (gpointer) out);
}


static gboolean
taxtable_is_grandchild (GncTaxTable *table)
{
  return (gncTaxTableGetParent(gncTaxTableGetParent(table)) != NULL);
}

static GncTaxTable *
taxtable_find_senior (GncTaxTable *table)
{
  GncTaxTable *temp, *parent, *gp = NULL;

  temp = table;
  do {
    /* See if "temp" is a grandchild */
    parent = gncTaxTableGetParent(temp);
    if (!parent)
      break;
    gp = gncTaxTableGetParent(parent);
    if (!gp)
      break;

    /* Yep, this is a grandchild.  Move up one generation and try again */
    temp = parent;
  } while (TRUE);

  /* Ok, at this point temp points to the most senior child and parent
   * should point to the top taxtable (and gp should be NULL).  If
   * parent is NULL then we are the most senior child (and have no
   * children), so do nothing.  If temp == table then there is no
   * grandparent, so do nothing.
   *
   * Do something if parent != NULL && temp != table
   */
  g_assert (gp == NULL);

  /* return the most senior table */
  return temp;
}

/* build a list of tax tables that are grandchildren or bogus (empty entry list). */
static void
taxtable_scrub_cb (QofInstance * table_p, gpointer list_p)
{
  GncTaxTable *table = GNC_TAXTABLE(table_p);
  GList **list = list_p;

  if (taxtable_is_grandchild(table) || gncTaxTableGetEntries(table) == NULL)
    *list = g_list_prepend(*list, table);
}

/* for each entry, check the tax tables.  If the tax tables are
 * grandchildren, then fix them to point to the most senior child
 */
static void
taxtable_scrub_entries (QofInstance * entry_p, gpointer ht_p)
{
  GHashTable *ht = ht_p;
  GncEntry *entry = GNC_ENTRY(entry_p);
  GncTaxTable *table, *new_tt;
  gint32 count;

  table = gncEntryGetInvTaxTable(entry);
  if (table) {
    if (taxtable_is_grandchild(table)) {
      PINFO("Fixing i-taxtable on entry %s\n",
	     guid_to_string(qof_instance_get_guid(QOF_INSTANCE(entry))));
      new_tt = taxtable_find_senior(table);
      gncEntryBeginEdit(entry);
      gncEntrySetInvTaxTable(entry, new_tt);
      gncEntryCommitEdit(entry);
      table = new_tt;
    }
    if (table) {
      count = GPOINTER_TO_INT(g_hash_table_lookup(ht, table));
      count++;
      g_hash_table_insert(ht, table, GINT_TO_POINTER(count));
    }
  }

  table = gncEntryGetBillTaxTable(entry);
  if (table) {
    if (taxtable_is_grandchild(table)) {
      PINFO("Fixing b-taxtable on entry %s\n",
	     guid_to_string(qof_instance_get_guid(QOF_INSTANCE(entry))));
      new_tt = taxtable_find_senior(table);
      gncEntryBeginEdit(entry);
      gncEntrySetBillTaxTable(entry, new_tt);
      gncEntryCommitEdit(entry);
      table = new_tt;
    }
    if (table) {
      count = GPOINTER_TO_INT(g_hash_table_lookup(ht, table));
      count++;
      g_hash_table_insert(ht, table, GINT_TO_POINTER(count));
    }
  }
}

static void
taxtable_scrub_cust (QofInstance * cust_p, gpointer ht_p)
{
  GHashTable *ht = ht_p;
  GncCustomer *cust = GNC_CUSTOMER(cust_p);
  GncTaxTable *table;
  gint32 count;
  
  table = gncCustomerGetTaxTable(cust);
  if (table) {
    count = GPOINTER_TO_INT(g_hash_table_lookup(ht, table));
    count++;
    g_hash_table_insert(ht, table, GINT_TO_POINTER(count));
  }
}

static void
taxtable_scrub_vendor (QofInstance * vendor_p, gpointer ht_p)
{
  GHashTable *ht = ht_p;
  GncVendor *vendor = GNC_VENDOR(vendor_p);
  GncTaxTable *table;
  gint32 count;

  table = gncVendorGetTaxTable(vendor);
  if (table) {
    count = GPOINTER_TO_INT(g_hash_table_lookup(ht, table));
    count++;
    g_hash_table_insert(ht, table, GINT_TO_POINTER(count));
  }
}

static void
taxtable_reset_refcount (gpointer key, gpointer value, gpointer notused)
{
  GncTaxTable *table = key;
  gint32 count = GPOINTER_TO_INT(value);

  if (count != gncTaxTableGetRefcount(table) && !gncTaxTableGetInvisible(table)) {
    PWARN("Fixing refcount on taxtable %s (%" G_GINT64_FORMAT " -> %d)\n",
	  guid_to_string(qof_instance_get_guid(QOF_INSTANCE(table))),
	  gncTaxTableGetRefcount(table), count);
      gncTaxTableSetRefcount(table, count);
  }
}

static void
taxtable_scrub (QofBook *book)
{
  GList *list = NULL;
  GList *node;
  GncTaxTable *parent, *table;
  GHashTable *ht = g_hash_table_new(g_direct_hash, g_direct_equal);

  qof_object_foreach (GNC_ID_ENTRY, book, taxtable_scrub_entries, ht);
  qof_object_foreach (GNC_ID_CUSTOMER, book, taxtable_scrub_cust, ht);
  qof_object_foreach (GNC_ID_VENDOR, book, taxtable_scrub_vendor, ht);
  qof_object_foreach (GNC_ID_TAXTABLE, book, taxtable_scrub_cb, &list);

  /* destroy the list of "grandchildren" tax tables */
  for (node = list; node; node = node->next) {
    table = node->data;

    PINFO ("deleting grandchild taxtable: %s\n",
	   guid_to_string(qof_instance_get_guid(QOF_INSTANCE(table))));

    /* Make sure the parent has no children */
    parent = gncTaxTableGetParent(table);
    gncTaxTableSetChild(parent, NULL);

    /* Destroy this tax table */
    gncTaxTableBeginEdit(table);
    gncTaxTableDestroy(table);
  }

  /* reset the refcounts as necessary */
  g_hash_table_foreach(ht, taxtable_reset_refcount, NULL);

  g_list_free(list);
  g_hash_table_destroy(ht);
}

static void
taxtable_ns(FILE *out)
{
  g_return_if_fail(out);
  gnc_xml2_write_namespace_decl(out, "taxtable");
  gnc_xml2_write_namespace_decl(out, "tte");
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
    taxtable_scrub,
    taxtable_ns,
  };

  qof_object_register_backend (_GNC_MOD_NAME,
			    GNC_FILE_BACKEND,
			    &be_data);
}
