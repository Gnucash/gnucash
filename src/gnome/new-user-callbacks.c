/********************************************************************\
 * new-user-callbacks.c - new user functionality for GnuCash        *
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

#include <glib.h>
#include <gnome.h>
#include <guile/gh.h>
#include <time.h>

#include "FileDialog.h"
#include "dialog-utils.h"
#include "glade-support.h"
#include "gnc-book.h"
#include "gnc-commodity-edit.h"
#include "gnc-dir.h"
#include "gnc-ui-util.h"
#include "io-example-account.h"
#include "new-user-callbacks.h"
#include "new-user-funs.h"
#include "new-user-interface.h"
#include "query-user.h"

static int commodEditAdded = 0;

static AccountGroup *our_final_group = NULL;

static void
delete_our_final_group(void)
{
    if(our_final_group != NULL)
    {
        xaccFreeAccountGroup(our_final_group);
        our_final_group = NULL;
    }
}

static void
set_first_startup(int first_startup)
{
    gchar *todo;

    todo = g_strdup_printf("((gnc:option-setter "
                           " (gnc:lookup-global-option \"__new_user\" "
                           "                           \"first_startup\"))"
                           " %d)", first_startup);
    gh_eval_str(todo);
    g_free(todo);
}


gboolean
on_newUserStartPage_next               (GnomeDruidPage  *gnomedruidpage,
                                        gpointer         arg1,
                                        gpointer         user_data)
{
    return FALSE;
}


gboolean
on_chooseAccountTypesPage_next         (GnomeDruidPage  *gnomedruidpage,
                                        gpointer         arg1,
                                        gpointer         user_data)
{
    /* must collect the file list here. */
    return FALSE;
}


static gpointer
starting_balance_helper (Account *account, gpointer data)
{
  gnc_numeric balance;

  balance = gnc_new_user_get_balance (account);
  if (!gnc_numeric_zero_p (balance))
    gnc_account_create_opening_balance (account, balance, time (NULL));

  return NULL;
}

void
on_newUserDruidFinishPage_finish       (GnomeDruidPage  *gnomedruidpage,
                                        gpointer         arg1,
                                        gpointer         user_data)
{
    if (our_final_group)
        xaccGroupForEachAccount (our_final_group, starting_balance_helper,
                                 NULL, TRUE);

    gnc_ui_delete_new_user_window();

    gh_eval_str("(gnc:default-ui-start)");
    gh_eval_str("(gnc:show-main-window)");
    gh_eval_str("(gnc:hook-run-danglers gnc:*book-opened-hook* #f)");

    set_first_startup(0);

    if(our_final_group)
    {
        xaccGroupConcatGroup(gnc_book_get_group(gncGetCurrentBook()),
                             our_final_group);
    }
}


void
on_accountChooseDruidPage_cancel       (GnomeDruid      *gnomedruid,
                                        gpointer         user_data)
{
    gnc_ui_show_nu_cancel_dialog();
}


void
on_newAccountCancelDialog_OKButton_clicked
                                        (GtkButton       *button,
                                        gpointer         user_data)
{
    gboolean keepshowing = TRUE;

    keepshowing = gtk_toggle_button_get_active(
        GTK_TOGGLE_BUTTON(lookup_widget(
                              GTK_WIDGET(button),
                              "newAccountCancelDialog_RunAgainToggle")));

    set_first_startup(keepshowing);

    delete_our_final_group();
    
    gnc_ui_delete_new_user_window();
    gnc_ui_delete_nu_cancel_dialog();
    
    gh_eval_str("(gnc:default-ui-start)");
    gh_eval_str("(gnc:show-main-window)");
    gh_eval_str("(gnc:hook-run-danglers gnc:*book-opened-hook* #f)");
}

void
on_newAccountCurrencyChoosePage_prepare (GnomeDruidPage  *gnomedruidpage,
                                         gpointer         arg1,
                                         gpointer         user_data)
{
    /* need to load currency info here.  In fact drop a
       gnc-commodity-edit widget here */
    if(!commodEditAdded)
    {
        commodEditAdded = 1;
        
        gtk_box_pack_start(
            GTK_BOX(lookup_widget(GTK_WIDGET(gnomedruidpage),
                                  "newAccountCurrencyChooser_vbox")),
            GTK_WIDGET(gnc_get_new_user_commodity_editor()), FALSE, FALSE, 0);
    }
    
}

static void
add_each_gea_to_clist(gpointer data, gpointer user_data)
{
    GncExampleAccount *gea = (GncExampleAccount*)data;
    GtkCList *clist = (GtkCList*)user_data;
    int row = 0;
    gchar **rowdata;

    rowdata = g_new(gchar*, 2);
    rowdata[0] = gea->title;
    rowdata[1] = gea->short_description;

    row = gtk_clist_insert(clist, row, rowdata);
    gtk_clist_set_row_data(clist, row, gea);
    row++;
}

void
on_chooseAccountTypesPage_prepare      (GnomeDruidPage  *gnomedruidpage,
                                        gpointer         arg1,
                                        gpointer         user_data)
{
    static gboolean addedAccountLists = 0;
    GSList *list;
    GtkCList *clist;
    
    /* Need to load the account type lists here */

    list = gnc_load_example_account_list(GNC_ACCOUNTS_DIR "/C");

    clist = gnc_new_user_get_clist();
    
    gtk_clist_freeze(clist);

    gtk_clist_set_sort_column(clist, 0);

    if(!addedAccountLists)
    {
        g_slist_foreach(list, add_each_gea_to_clist, (gpointer)clist);
        addedAccountLists = 1;
    }

    gtk_clist_sort(clist);
    gtk_clist_thaw(clist);

    g_slist_free (list);
}


static gpointer
add_to_tree_account(Account* toadd, gpointer data)
{
    GtkWidget *item;
    GtkTree *tree = GTK_TREE(data);
    
    if(!toadd)
    {
        return NULL;
    }
    
    item = gtk_tree_item_new_with_label(xaccAccountGetName(toadd));
    gtk_tree_insert(tree, item, 0);
    gtk_widget_show(item);

    if(xaccGroupGetNumSubAccounts(xaccAccountGetChildren(toadd)) > 0)
    {
        GtkWidget *subtree = gtk_tree_new();
        gtk_tree_item_set_subtree(GTK_TREE_ITEM(item), subtree);
        gtk_tree_item_expand(GTK_TREE_ITEM(item));
        xaccGroupForEachAccount(xaccAccountGetChildren(toadd),
                                add_to_tree_account, subtree, FALSE);
    }

    return NULL;
}

static void
add_to_tree(GtkTree *tree, AccountGroup *grp)
{
    xaccGroupForEachAccount(grp, add_to_tree_account, tree, FALSE);
}

void
on_newAccountTypesList_select_row      (GtkCList        *clist,
                                        gint             row,
                                        gint             column,
                                        GdkEvent        *event,
                                        gpointer         user_data)
{
    GtkText *datext =
    GTK_TEXT(gnc_new_user_get_widget("newAccountTypesDescription"));
    GtkTree *datree =
    GTK_TREE(gnc_new_user_get_widget("newAccountListTree"));
    GncExampleAccount *gea =
    (GncExampleAccount*)gtk_clist_get_row_data(clist, row);

    gtk_text_freeze(datext);
    gtk_text_set_point(datext, 0);
    gtk_text_forward_delete(datext, gtk_text_get_length(datext));
    if(gea->long_description != NULL)
    {
        gtk_text_insert(datext, NULL, NULL, NULL, gea->long_description, -1);
    }
    gtk_text_thaw(datext);

    gtk_tree_clear_items(datree, 0, g_list_length (datree->children) - 1);
    add_to_tree(datree, gea->group);
}


void
on_newAccountTypesList_unselect_row    (GtkCList        *clist,
                                        gint             row,
                                        gint             column,
                                        GdkEvent        *event,
                                        gpointer         user_data)
{
    GtkText *datext =
    GTK_TEXT(gnc_new_user_get_widget("newAccountTypesDescription"));
    GtkTree *datree =
    GTK_TREE(gnc_new_user_get_widget("newAccountListTree"));

    gtk_text_freeze(datext);
    gtk_text_set_point(datext, 0);
    gtk_text_forward_delete(datext, gtk_text_get_length(datext));
    gtk_text_thaw(datext);

    gtk_tree_clear_items(datree, 0, g_list_length (datree->children) - 1);
}


void
on_newAccountTree_select_row           (GtkCList        *clist,
                                        gint             row,
                                        gint             column,
                                        GdkEvent        *event,
                                        gpointer         user_data)
{
    /* need to put info in the box and account name here */
}


void
on_newAccountSelectAllButton_clicked   (GtkButton       *button,
                                        gpointer         user_data)
{
}


void
on_newAccountOKButton_clicked          (GtkButton       *button,
                                        gpointer         user_data)
{
}


gboolean
on_newAccountCurrencyChoosePage_next   (GnomeDruidPage  *gnomedruidpage,
                                        gpointer         arg1,
                                        gpointer         user_data)
{
  return FALSE;
}


void
on_newAccountsTypeList_SelectAllButton_clicked
                                        (GtkButton       *button,
                                        gpointer         user_data)
{
    gtk_clist_select_all(gnc_new_user_get_clist());
}

void
on_newAccountsTypeList_ClearAllButton_clicked
                                        (GtkButton       *button,
                                        gpointer         user_data)
{
    gtk_clist_unselect_all(gnc_new_user_get_clist());
}

struct FinalInsertData_struct
{
    GtkCTree *tree;
    GtkCTreeNode *node;
    GtkCTreeNode *sibling;
};
typedef struct FinalInsertData_struct FinalInsertData;

static gchar**
generate_account_titles(Account *act)
{
    gchar **ret;

    ret = g_new(gchar *, 3);

    ret[0] = (gchar*)xaccAccountGetName(act);
    ret[1] = (gchar*)xaccAccountGetTypeStr(xaccAccountGetType(act));

    {
      gnc_numeric balance;
      const char *string;

      balance = gnc_new_user_get_balance (act);

      if (gnc_numeric_zero_p (balance))
        string = "";
      else
      {
        GNCPrintAmountInfo print_info;

        print_info = gnc_account_value_print_info (act, FALSE);
        string = xaccPrintAmount (balance, print_info);
      }

      ret[2] = (gchar*)string;
    }

    return ret;
}

static void
free_account_titles(gchar **tofree)
{
    g_free(tofree);
}

static gpointer
add_to_ctree_final_account(Account* toadd, gpointer data)
{
    FinalInsertData *topdata = (FinalInsertData*)data;
    GtkCTreeNode *node;
    gchar **titles;

    titles = generate_account_titles (toadd);

    node = gtk_ctree_insert_node(topdata->tree, topdata->node,
                                 topdata->sibling, 
                                 titles, 0,
                                 NULL, NULL, NULL, NULL,
                                 FALSE, TRUE);

    free_account_titles(titles);

    gtk_ctree_node_set_row_data (topdata->tree, node, toadd);

    if(xaccGroupGetNumAccounts(xaccAccountGetChildren(toadd)) > 0)
    {
        FinalInsertData nextdata;
        nextdata.tree = topdata->tree;
        nextdata.node = node;
        nextdata.sibling = NULL;

        xaccGroupForEachAccount(xaccAccountGetChildren(toadd),
                                add_to_ctree_final_account, &nextdata, FALSE);
    }

    topdata->sibling = node;

    return NULL;
}

static void
gnc_new_user_insert_final_accounts(GtkCTree *tree, AccountGroup *group)
{
    FinalInsertData data;
    data.tree = tree;
    data.node = NULL;
    data.sibling = NULL;

    xaccGroupForEachAccount(group, add_to_ctree_final_account, &data, FALSE);
}

void
on_finalAccountDruidPage_prepare       (GnomeDruidPage  *gnomedruidpage,
                                        gpointer         arg1,
                                        gpointer         user_data)
{
    GList *dalist;
    GSList *actlist = NULL;
    GtkCList *clist = gnc_new_user_get_clist();
    GtkWidget *ctree;

    ctree = gnc_new_user_get_widget("finalAccountCTree");

    gtk_clist_clear(GTK_CLIST(ctree));
    
    for(dalist = clist->selection; dalist; dalist = dalist->next)
    {
        int row = GPOINTER_TO_INT(dalist->data);
        actlist = g_slist_append(actlist, gtk_clist_get_row_data(clist, row));
    }

    delete_our_final_group();
    our_final_group = gnc_new_user_merge_groups(actlist);

    gnc_new_user_insert_final_accounts(GTK_CTREE(ctree), our_final_group);

    gnc_clist_columns_autosize (GTK_CLIST(ctree));

    {
      GNCAmountEdit *balance_edit;
      GtkWidget *entry;

      gnc_new_user_block_amount_changed ();

      balance_edit = gnc_new_user_get_balance_editor ();
      gnc_amount_edit_set_amount (balance_edit, gnc_numeric_zero ());

      entry = gnc_amount_edit_gtk_entry (balance_edit);
      gtk_entry_set_text (GTK_ENTRY (entry), "");

      gnc_new_user_unblock_amount_changed ();

      gtk_widget_set_sensitive (GTK_WIDGET (balance_edit), FALSE);
    }
}


void
on_finalAccountCTree_tree_select_row   (GtkCTree        *ctree,
                                        GList           *node,
                                        gint             column,
                                        gpointer         user_data)
{
  Account *account;
  GNCAmountEdit *balance_edit;
  GNCPrintAmountInfo print_info;
  gnc_numeric balance;

  balance_edit = gnc_new_user_get_balance_editor ();

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

  balance = gnc_new_user_get_balance (account);

  if (gnc_reverse_balance (account))
    balance = gnc_numeric_neg (balance);

  print_info = gnc_account_value_print_info (account, FALSE);
  gnc_amount_edit_set_print_info (balance_edit, print_info);
  gnc_amount_edit_set_fraction (balance_edit,
                                xaccAccountGetCurrencySCU (account));

  gnc_new_user_block_amount_changed ();

  gnc_amount_edit_set_amount (balance_edit, balance);
  if (gnc_numeric_zero_p (balance))
  {
    GtkWidget *entry;

    entry = gnc_amount_edit_gtk_entry (balance_edit);
    gtk_entry_set_text (GTK_ENTRY (entry), "");
  }

  gnc_new_user_unblock_amount_changed ();
}


static void
update_account_balance (GtkCTree *ctree, GtkCTreeNode *node)
{
  Account *account;
  GNCAmountEdit *balance_edit;
  gboolean result;

  balance_edit = gnc_new_user_get_balance_editor ();

  account = gtk_ctree_node_get_row_data (ctree, node);
  if (!account)
    return;

  gnc_new_user_block_amount_changed ();
  result = gnc_amount_edit_evaluate (balance_edit);
  gnc_new_user_unblock_amount_changed ();

  if (result)
  {
    gnc_numeric balance;
    GNCPrintAmountInfo print_info;
    const char *string;

    balance = gnc_amount_edit_get_amount (balance_edit);

    print_info = gnc_account_value_print_info (account, FALSE);
    string = xaccPrintAmount (balance, print_info);

    if (gnc_numeric_zero_p (balance))
      string = "";

    gtk_ctree_node_set_text (ctree, GTK_CTREE_NODE (node), 2, string);

    if (gnc_reverse_balance (account))
      balance = gnc_numeric_neg (balance);

    gnc_new_user_set_balance (account, balance);
  }
}

void
on_finalAccountCTree_tree_unselect_row (GtkCTree        *ctree,
                                        GList           *node,
                                        gint             column,
                                        gpointer         user_data)
{
  update_account_balance (ctree, GTK_CTREE_NODE (node));

  {
    GNCAmountEdit *balance_edit;
    GtkWidget *entry;

    balance_edit = gnc_new_user_get_balance_editor ();

    entry = gnc_amount_edit_gtk_entry (balance_edit);
    gtk_entry_set_text (GTK_ENTRY (entry), "");

    gtk_widget_set_sensitive (GTK_WIDGET (balance_edit), FALSE);
  }
}

void
on_finalAccountBalanceEdit_changed (GNCAmountEdit *gae)
{
  GtkCTree *ctree;
  GtkCTreeNode *node;

  if (!GTK_WIDGET_SENSITIVE (GTK_WIDGET (gae)))
    return;

  ctree = gnc_new_user_get_final_account_tree ();
  if (!ctree)
    return;

  node = gtk_ctree_node_nth (ctree, GTK_CLIST(ctree)->focus_row);
  if (!node)
    return;

  update_account_balance (ctree, node);
}

gboolean
on_finalAccountDruidPage_next          (GnomeDruidPage  *gnomedruidpage,
                                        gpointer         arg1,
                                        gpointer         user_data)
{
  GNCAmountEdit *balance_edit;

  balance_edit = gnc_new_user_get_balance_editor ();

  if (!gnc_amount_edit_evaluate (balance_edit))
  {
    GtkWidget *top;
    const char *message = _("You must enter a valid balance.");

    top = gtk_widget_get_toplevel (GTK_WIDGET (gnomedruidpage));
    gnc_error_dialog_parented(GTK_WINDOW(top), message);

    return TRUE;
  }

  return FALSE;
}

