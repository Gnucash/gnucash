/********************************************************************\
 * druid-hierarchy.c -- account hierarchy creation functionality    *
 * Copyright (C) 2001 Gnumatic, Inc.                                *
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
\********************************************************************/

#include "config.h"

#include <gnome.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "Group.h"
#include "dialog-new-user.h"
#include "dialog-utils.h"
#include "druid-hierarchy.h"
#include "druid-utils.h"
#include "gnc-amount-edit.h"
#include "gnc-commodity-edit.h"
#include "gnc-component-manager.h"
#include "gnc-dir.h"
#include "gnc-gui-query.h"
#include "gnc-ui-util.h"
#include "io-example-account.h"
#include "top-level.h"

static GtkWidget *hierarchy_window = NULL;
static AccountGroup *our_final_group = NULL;


static void on_balance_changed (GNCAmountEdit *gae);


static GtkWidget*
hierarchy_get_widget (const char *name)
{
  if (!hierarchy_window) return NULL;

  return gnc_glade_lookup_widget (hierarchy_window, name);
}

static GtkCTree *
hierarchy_get_final_account_tree (void)
{
    return GTK_CTREE (hierarchy_get_widget ("final_account_ctree"));
}

static void
delete_hierarchy_window (void)
{
  if (!hierarchy_window) return;

  gtk_widget_destroy (hierarchy_window);
  hierarchy_window = NULL;
}

static void
destroy_hash_helper (gpointer key, gpointer value, gpointer user_data)
{
  char *fullname = key;
  gnc_numeric *balance = value;

  g_free (fullname);
  g_free (balance);
}

static GNCAmountEdit *
get_balance_editor (void)
{
  if (!hierarchy_window) return NULL;

  return gtk_object_get_data (GTK_OBJECT (hierarchy_window), "balance_editor");
}

static GtkCList*
get_account_types_clist (void)
{
  return GTK_CLIST(hierarchy_get_widget ("account_types_clist"));
}

static GNCCommodityEdit *
get_commodity_editor(void)
{
  GtkWidget *tmp_wid = gtk_object_get_data (GTK_OBJECT (hierarchy_window),
                                            "commod_editor");

  if(!tmp_wid)
  {
    GNCCommodityEdit *cur_editor;

    cur_editor = GNC_COMMODITY_EDIT(gnc_commodity_edit_new());
    gtk_widget_show (GTK_WIDGET(cur_editor));
    gnc_commodity_edit_set_commodity (cur_editor,
                                      gnc_locale_default_currency());
    gtk_object_set_data(GTK_OBJECT(hierarchy_window),
                        "commod_editor", cur_editor);
    return cur_editor;
  }
  else
  {
    return GNC_COMMODITY_EDIT(tmp_wid);
  }
}

static void
gnc_hierarchy_destroy_cb (GtkObject *obj, gpointer user_data)
{
  GHashTable *hash;

  hash = gtk_object_get_data (obj, "balance_hash");
  if (hash)
  {
    g_hash_table_foreach (hash, destroy_hash_helper, NULL);
    g_hash_table_destroy (hash);
    gtk_object_set_data (obj, "balance_hash", NULL);
  }
}

static void
block_amount_changed (void)
{
  GNCAmountEdit *balance_edit;

  balance_edit = get_balance_editor ();
  if (!balance_edit) return;

  gtk_signal_handler_block_by_func
    (GTK_OBJECT (balance_edit),
     GTK_SIGNAL_FUNC(on_balance_changed), NULL);
}

static void
unblock_amount_changed (void)
{
  GNCAmountEdit *balance_edit;

  balance_edit = get_balance_editor ();
  if (!balance_edit) return;

  gtk_signal_handler_unblock_by_func
    (GTK_OBJECT (balance_edit),
     GTK_SIGNAL_FUNC(on_balance_changed), NULL);
}

static gnc_numeric
get_final_balance (Account *account)
{
  GHashTable *hash;
  gnc_numeric *balance;
  char *fullname;

  if (!account || !hierarchy_window) return gnc_numeric_zero ();

  hash = gtk_object_get_data (GTK_OBJECT (hierarchy_window), "balance_hash");
  if (!hash) return gnc_numeric_zero ();

  fullname = xaccAccountGetFullName (account, ':');

  balance = g_hash_table_lookup (hash, fullname);

  g_free (fullname);

  if (balance)
    return *balance;

  return gnc_numeric_zero ();
}

static void
set_final_balance (Account *account, gnc_numeric in_balance)
{
  GHashTable *hash;
  gnc_numeric *balance;
  char *fullname;

  if (!account || !hierarchy_window) return;

  hash = gtk_object_get_data (GTK_OBJECT (hierarchy_window), "balance_hash");
  if (!hash) return;

  fullname = xaccAccountGetFullName (account, ':');

  balance = g_hash_table_lookup (hash, fullname);
  if (balance)
  {
    *balance = in_balance;
    g_free (fullname);
  }
  else
  {
    balance = g_new (gnc_numeric, 1);
    *balance = in_balance;

    g_hash_table_insert (hash, fullname, balance);
  }
}

static void
update_account_balance (GtkCTree *ctree, GtkCTreeNode *node)
{
  Account *account;
  GNCAmountEdit *balance_edit;
  gboolean result;

  balance_edit = get_balance_editor ();

  account = gtk_ctree_node_get_row_data (ctree, node);
  if (!account)
    return;

  block_amount_changed ();
  result = gnc_amount_edit_evaluate (balance_edit);
  unblock_amount_changed ();

  if (result)
  {
    gnc_numeric balance;
    GNCPrintAmountInfo print_info;
    const char *string;

    balance = gnc_amount_edit_get_amount (balance_edit);

    print_info = gnc_account_print_info (account, FALSE);
    string = xaccPrintAmount (balance, print_info);

    if (gnc_numeric_zero_p (balance))
      string = "";

    gtk_ctree_node_set_text (ctree, GTK_CTREE_NODE (node), 2, string);

    if (gnc_reverse_balance (account))
      balance = gnc_numeric_neg (balance);

    set_final_balance (account, balance);
  }
}

static void
on_balance_changed (GNCAmountEdit *gae)
{
  GtkCTree *ctree;
  GtkCTreeNode *node;

  if (!GTK_WIDGET_SENSITIVE (GTK_WIDGET (gae)))
    return;

  ctree = hierarchy_get_final_account_tree ();
  if (!ctree)
    return;

  node = gtk_ctree_node_nth (ctree, GTK_CLIST(ctree)->focus_row);
  if (!node)
    return;

  update_account_balance (ctree, node);
}

static void
on_choose_currency_prepare (GnomeDruidPage  *gnomedruidpage,
                            gpointer         arg1,
                            gpointer         user_data)
{
  if(!GPOINTER_TO_INT (gtk_object_get_data
                       (GTK_OBJECT(hierarchy_window), "commod_added")))
  {
    gtk_object_set_data (GTK_OBJECT(hierarchy_window),
                         "commod_added", GINT_TO_POINTER (1));

    gtk_box_pack_start(GTK_BOX(gnc_glade_lookup_widget
                               (hierarchy_window, "currency_chooser_vbox")),
                       GTK_WIDGET(get_commodity_editor()), FALSE, FALSE, 0);
  }
}

static gchar*
gnc_get_ea_locale_dir(const char *top_dir)
{
    static gchar *default_locale = "C";
    gchar *ret;
    gchar *locale;
    struct stat buf;

    locale = g_strdup(setlocale(LC_MESSAGES, NULL));

    ret = g_strdup_printf("%s/%s", top_dir, locale);

    if(stat(ret, &buf) != 0 && (strlen (locale) > 2))
    {
        g_free (ret);
        locale[2] = '\0';
        ret = g_strdup_printf("%s/%s", top_dir, locale);
    }

    if(stat(ret, &buf) != 0)
    {
        g_free (ret);
        ret = g_strdup_printf("%s/%s", top_dir, default_locale);
    }

    g_free(locale);

    return ret;
}

static void
add_each_gea_to_clist (gpointer data, gpointer user_data)
{
  GncExampleAccount *gea = (GncExampleAccount*)data;
  GtkCList *clist = GTK_CLIST (user_data);
  gchar *rowdata[2];
  int row = 0;

  rowdata[0] = gea->title;
  rowdata[1] = gea->short_description;

  row = gtk_clist_insert(clist, row, rowdata);
  gtk_clist_set_row_data(clist, row, gea);
}

static void
on_choose_account_types_prepare (GnomeDruidPage  *gnomedruidpage,
                                 gpointer         arg1,
                                 gpointer         user_data)
{
  gpointer added_ptr;

  added_ptr = gtk_object_get_data (GTK_OBJECT(hierarchy_window),
                                   "account_list_added");

  if (!GPOINTER_TO_INT(added_ptr))
  {
    GSList *list;
    GtkCList *clist;
    gchar *locale_dir = gnc_get_ea_locale_dir (GNC_ACCOUNTS_DIR);

    gnc_suspend_gui_refresh ();
    list = gnc_load_example_account_list (locale_dir);
    gnc_resume_gui_refresh ();

    clist = get_account_types_clist ();

    gtk_clist_freeze (clist);

    g_slist_foreach (list, add_each_gea_to_clist, (gpointer)clist);

    gtk_clist_set_sort_column (clist, 0);
    gtk_clist_sort (clist);

    gtk_clist_thaw (clist);

    g_slist_free (list);
    g_free (locale_dir);

    gtk_object_set_data (GTK_OBJECT(hierarchy_window),
                         "account_list_added",
                         GINT_TO_POINTER(1));
  }
}

static gpointer
add_to_tree_account (Account* toadd, gpointer data)
{
  GtkWidget *item;
  GtkTree *tree = GTK_TREE (data);

  if (!toadd)
    return NULL;

  item = gtk_tree_item_new_with_label (xaccAccountGetName(toadd));
  gtk_tree_insert (tree, item, 0);
  gtk_widget_show (item);

  if (xaccGroupGetNumSubAccounts (xaccAccountGetChildren (toadd)) > 0)
  {
    GtkWidget *subtree = gtk_tree_new ();

    gtk_tree_item_set_subtree (GTK_TREE_ITEM(item), subtree);
    gtk_tree_item_expand (GTK_TREE_ITEM(item));
    xaccGroupForEachAccount (xaccAccountGetChildren(toadd),
                             add_to_tree_account, subtree, FALSE);
  }

  return NULL;
}

static void
add_to_tree (GtkTree *tree, AccountGroup *grp)
{
  xaccGroupForEachAccount(grp, add_to_tree_account, tree, FALSE);
}

static void
on_account_types_list_select_row (GtkCList        *clist,
                                  gint             row,
                                  gint             column,
                                  GdkEvent        *event,
                                  gpointer         user_data)
{
  GtkLabel *datext = GTK_LABEL (hierarchy_get_widget
                                ("account_types_description_entry"));
  GtkTree *datree = GTK_TREE (hierarchy_get_widget ("account_type_tree"));
  GncExampleAccount *gea = gtk_clist_get_row_data (clist, row);

  if(gea->long_description != NULL)
    gtk_label_set_text (datext, gea->long_description);

  gtk_tree_clear_items (datree, 0, g_list_length (datree->children));
  add_to_tree (datree, gea->group);
}

static void
on_account_types_list_unselect_row (GtkCList        *clist,
                                    gint             row,
                                    gint             column,
                                    GdkEvent        *event,
                                    gpointer         user_data)
{
  GtkLabel *datext = GTK_LABEL (hierarchy_get_widget
                                ("account_types_description_entry"));
  GtkTree *datree = GTK_TREE (hierarchy_get_widget ("account_type_tree"));

  gtk_label_set_text (datext, "");

  gtk_tree_clear_items (datree, 0, g_list_length (datree->children));
}

static void
select_all_clicked (GtkButton       *button,
                    gpointer         user_data)
{
  gtk_clist_select_all (get_account_types_clist ());
}

static void
clear_all_clicked (GtkButton       *button,
                   gpointer         user_data)
{
  gtk_clist_unselect_all (get_account_types_clist ());
}

typedef struct FinalInsertData_struct
{
  GtkCTree *tree;
  GtkCTreeNode *node;
  GtkCTreeNode *sibling;
} FinalInsertData;

static gchar**
generate_account_titles (Account *act)
{
  gchar **ret;

  ret = g_new (gchar *, 3);

  ret[0] = (gchar*)xaccAccountGetName(act);
  ret[1] = (gchar*)xaccAccountGetTypeStr(xaccAccountGetType(act));

  {
    gnc_numeric balance;
    const char *string;

    balance = get_final_balance (act);

    if (gnc_numeric_zero_p (balance))
      string = "";
    else
    {
      GNCPrintAmountInfo print_info;

      print_info = gnc_account_print_info (act, FALSE);
      string = xaccPrintAmount (balance, print_info);
    }

    ret[2] = (gchar*)string;
  }

  return ret;
}

static void
free_account_titles (gchar **tofree)
{
  g_free (tofree);
}

static gpointer
add_to_ctree_final_account (Account* toadd, gpointer data)
{
  FinalInsertData *topdata = (FinalInsertData*)data;
  GtkCTreeNode *node;
  gchar **titles;

  titles = generate_account_titles (toadd);

  node = gtk_ctree_insert_node (topdata->tree, topdata->node,
                                topdata->sibling, 
                                titles, 0,
                                NULL, NULL, NULL, NULL,
                                FALSE, TRUE);

  free_account_titles (titles);

  gtk_ctree_node_set_row_data (topdata->tree, node, toadd);

  if (xaccGroupGetNumAccounts (xaccAccountGetChildren (toadd)) > 0)
  {
    FinalInsertData nextdata;
    nextdata.tree = topdata->tree;
    nextdata.node = node;
    nextdata.sibling = NULL;

    xaccGroupForEachAccount (xaccAccountGetChildren(toadd),
                             add_to_ctree_final_account, &nextdata, FALSE);
  }

  topdata->sibling = node;

  return NULL;
}

static void
insert_final_accounts (GtkCTree *tree, AccountGroup *group)
{
  FinalInsertData data;
  data.tree = tree;
  data.node = NULL;
  data.sibling = NULL;

  xaccGroupForEachAccount(group, add_to_ctree_final_account, &data, FALSE);
}

static void
delete_our_final_group (void)
{
  if (our_final_group != NULL)
  {
    xaccFreeAccountGroup (our_final_group);
    our_final_group = NULL;
  }
}

static Account*
clone_account (const Account* from, gnc_commodity *com)
{
  Account *ret;

  ret = xaccCloneAccountSimple (from);

  xaccAccountSetCommodity (ret, com);

  return ret;
}

struct add_group_data_struct
{
  AccountGroup *to;
  gnc_commodity *com;
};

static gpointer
add_groups_for_each (Account *toadd, gpointer data)
{
  struct add_group_data_struct *dadata = data;
  Account *foundact;
    
  foundact = xaccGetAccountFromName (dadata->to, xaccAccountGetName(toadd));

  if (!foundact)
  {
    foundact = clone_account (toadd, dadata->com);
    xaccGroupInsertAccount (dadata->to, foundact);
  }

  {
    AccountGroup *addgrp = xaccAccountGetChildren (toadd);

    if (xaccGroupGetNumAccounts(addgrp) > 0)
    {
      struct add_group_data_struct downdata;

      downdata.to = xaccAccountGetChildren(foundact);
      downdata.com = dadata->com;

      xaccGroupForEachAccount (addgrp, add_groups_for_each,
                               &downdata, FALSE);
    }
  }

  return NULL;
}

static void
add_groups_to_with_random_guids (AccountGroup *into, AccountGroup *from,
                                 gnc_commodity *com)
{
  struct add_group_data_struct data;
  data.to = into;
  data.com = com;
    
  xaccGroupForEachAccount (from, add_groups_for_each, &data, FALSE);
}

static AccountGroup *
hierarchy_merge_groups (GSList *dalist)
{
  GSList *mark;
  gnc_commodity *com;
  AccountGroup *ret = xaccMallocAccountGroup ();

  com = gnc_commodity_edit_get_commodity (get_commodity_editor ());

  for (mark = dalist; mark; mark = mark->next)
  {
    GncExampleAccount *xea = mark->data;

    add_groups_to_with_random_guids (ret, xea->group, com);
  }

  return ret;
}

static void
on_final_account_prepare (GnomeDruidPage  *gnomedruidpage,
                          gpointer         arg1,
                          gpointer         user_data)
{
  GtkCList *clist;
  GtkWidget *ctree;
  GSList *actlist;
  GList *dalist;

  clist = get_account_types_clist ();
  ctree = GTK_WIDGET (hierarchy_get_final_account_tree ());

  gtk_clist_clear (GTK_CLIST(ctree));

  actlist = NULL;
  for (dalist = clist->selection; dalist; dalist = dalist->next)
  {
    int row = GPOINTER_TO_INT(dalist->data);
    actlist = g_slist_append (actlist, gtk_clist_get_row_data(clist, row));
  }

  gnc_suspend_gui_refresh ();
  delete_our_final_group ();
  our_final_group = hierarchy_merge_groups (actlist);
  gnc_resume_gui_refresh ();

  insert_final_accounts (GTK_CTREE(ctree), our_final_group);

  gnc_clist_columns_autosize (GTK_CLIST(ctree));

  {
    GNCAmountEdit *balance_edit;
    GtkWidget *entry;

    block_amount_changed ();

    balance_edit = get_balance_editor ();
    gnc_amount_edit_set_amount (balance_edit, gnc_numeric_zero ());

    entry = gnc_amount_edit_gtk_entry (balance_edit);
    gtk_entry_set_text (GTK_ENTRY (entry), "");

    unblock_amount_changed ();

    gtk_widget_set_sensitive (GTK_WIDGET (balance_edit), FALSE);
  }
}

static void
on_final_account_tree_select_row (GtkCTree        *ctree,
                                  GList           *node,
                                  gint             column,
                                  gpointer         user_data)
{
  Account *account;
  GNCAmountEdit *balance_edit;
  GNCPrintAmountInfo print_info;
  gnc_numeric balance;

  balance_edit = get_balance_editor ();

  account = gtk_ctree_node_get_row_data (ctree, GTK_CTREE_NODE (node));
  if (!account || xaccAccountGetType (account) == EQUITY)
  {
    GtkWidget *entry;

    entry = gnc_amount_edit_gtk_entry (balance_edit);
    gtk_entry_set_text (GTK_ENTRY (entry), "");

    gtk_widget_set_sensitive (GTK_WIDGET (balance_edit), FALSE);

    return;
  }

  gtk_widget_set_sensitive (GTK_WIDGET (balance_edit), TRUE);

  balance = get_final_balance (account);

  if (gnc_reverse_balance (account))
    balance = gnc_numeric_neg (balance);

  print_info = gnc_account_print_info (account, FALSE);
  gnc_amount_edit_set_print_info (balance_edit, print_info);
  gnc_amount_edit_set_fraction (balance_edit,
                                xaccAccountGetCommoditySCU (account));

  block_amount_changed ();

  gnc_amount_edit_set_amount (balance_edit, balance);
  if (gnc_numeric_zero_p (balance))
  {
    GtkWidget *entry;

    entry = gnc_amount_edit_gtk_entry (balance_edit);
    gtk_entry_set_text (GTK_ENTRY (entry), "");
  }

  unblock_amount_changed ();
}

static void
on_final_account_tree_unselect_row (GtkCTree        *ctree,
                                    GList           *node,
                                    gint             column,
                                    gpointer         user_data)
{
  update_account_balance (ctree, GTK_CTREE_NODE (node));

  {
    GNCAmountEdit *balance_edit;
    GtkWidget *entry;

    balance_edit = get_balance_editor ();

    entry = gnc_amount_edit_gtk_entry (balance_edit);
    gtk_entry_set_text (GTK_ENTRY (entry), "");

    gtk_widget_set_sensitive (GTK_WIDGET (balance_edit), FALSE);
  }
}

static gboolean
on_final_account_next (GnomeDruidPage  *gnomedruidpage,
                       gpointer         arg1,
                       gpointer         user_data)
{
  GNCAmountEdit *balance_edit;

  balance_edit = get_balance_editor ();

  if (!gnc_amount_edit_evaluate (balance_edit))
  {
    GtkWidget *top;
    const char *message = _("You must enter a valid balance.");

    top = gtk_widget_get_toplevel (GTK_WIDGET (gnomedruidpage));
    gnc_error_dialog_parented (GTK_WINDOW(top), message);

    return TRUE;
  }

  return FALSE;
}

static void
cancel_everything_out(void)
{
  delete_our_final_group ();
  delete_hierarchy_window ();
  gncp_new_user_finish ();
}

static void
on_cancel (GnomeDruid      *gnomedruid,
                 gpointer         user_data)
{
  cancel_everything_out ();
}

static gpointer
starting_balance_helper (Account *account, gpointer data)
{
  gnc_numeric balance;

  balance = get_final_balance (account);
  if (!gnc_numeric_zero_p (balance))
    gnc_account_create_opening_balance (account, balance, time (NULL));

  return NULL;
}

static void
on_finish (GnomeDruidPage  *gnomedruidpage,
           gpointer         arg1,
           gpointer         user_data)
{
  gnc_suspend_gui_refresh ();

  if (our_final_group)
    xaccGroupForEachAccount (our_final_group, starting_balance_helper,
                             NULL, TRUE);

  delete_hierarchy_window ();

  gncp_new_user_finish ();

  gnc_set_first_startup (FALSE);

  if (our_final_group)
    xaccGroupConcatGroup (gnc_get_current_group (), our_final_group);

  gnc_resume_gui_refresh ();
}

static GtkWidget *
gnc_create_hierarchy_druid (void)
{
  GtkWidget *balance_edit;
  GtkWidget *dialog;
  GtkWidget *druid;
  GtkWidget *clist;
  GtkWidget *box;
  GHashTable *hash;
  GladeXML *xml;

  xml = gnc_glade_xml_new ("account.glade", "Hierarchy Druid");

  glade_xml_signal_connect
    (xml, "on_choose_currency_prepare",
     GTK_SIGNAL_FUNC (on_choose_currency_prepare));

  glade_xml_signal_connect
    (xml, "on_choose_account_types_prepare",
     GTK_SIGNAL_FUNC (on_choose_account_types_prepare));

  glade_xml_signal_connect
    (xml, "on_account_types_list_select_row",
     GTK_SIGNAL_FUNC (on_account_types_list_select_row));

  glade_xml_signal_connect
    (xml, "on_account_types_list_unselect_row",
     GTK_SIGNAL_FUNC (on_account_types_list_unselect_row));

  glade_xml_signal_connect
    (xml, "on_final_account_prepare",
     GTK_SIGNAL_FUNC (on_final_account_prepare));

  glade_xml_signal_connect
    (xml, "on_final_account_tree_select_row",
     GTK_SIGNAL_FUNC (on_final_account_tree_select_row));

  glade_xml_signal_connect
    (xml, "on_final_account_tree_unselect_row",
     GTK_SIGNAL_FUNC (on_final_account_tree_unselect_row));

  glade_xml_signal_connect
    (xml, "on_final_account_next",
     GTK_SIGNAL_FUNC (on_final_account_next));

  glade_xml_signal_connect
    (xml, "select_all_clicked", GTK_SIGNAL_FUNC (select_all_clicked));

  glade_xml_signal_connect
    (xml, "clear_all_clicked", GTK_SIGNAL_FUNC (clear_all_clicked));

  glade_xml_signal_connect (xml, "on_finish", GTK_SIGNAL_FUNC (on_finish));

  glade_xml_signal_connect (xml, "on_cancel", GTK_SIGNAL_FUNC (on_cancel));

  dialog = glade_xml_get_widget (xml, "Hierarchy Druid");

  druid = glade_xml_get_widget (xml, "hierarchy_druid");
  gnc_druid_set_colors (GNOME_DRUID (druid));

  balance_edit = gnc_amount_edit_new ();
  gnc_amount_edit_set_evaluate_on_enter (GNC_AMOUNT_EDIT (balance_edit), TRUE);
  gtk_widget_show (balance_edit);

  gtk_signal_connect (GTK_OBJECT (balance_edit), "amount_changed",
                      GTK_SIGNAL_FUNC(on_balance_changed), NULL);

  clist = glade_xml_get_widget (xml, "account_types_clist");
  gtk_clist_column_titles_passive (GTK_CLIST (clist));

  box = glade_xml_get_widget (xml, "start_balance_box");
  gtk_box_pack_start (GTK_BOX (box), balance_edit, TRUE, TRUE, 0);

  gtk_object_set_data (GTK_OBJECT(dialog), "balance_editor", balance_edit);

  hash = g_hash_table_new (g_str_hash, g_str_equal);

  gtk_object_set_data (GTK_OBJECT(dialog), "balance_hash", hash);

  gtk_signal_connect (GTK_OBJECT(dialog), "destroy",
                      GTK_SIGNAL_FUNC(gnc_hierarchy_destroy_cb), NULL);

  return dialog;
}

void
gnc_ui_hierarchy_druid (void)
{
  if (hierarchy_window) return;

  hierarchy_window = gnc_create_hierarchy_druid ();

  return;
}
