/********************************************************************\
 * generic-import.c -- Functions and utilities to help writing      * 
 * import modules.   See file generic-import-design.txt for         *
 * description                                                      *
 *                        (GnuCash)                                 *
 * Copyright (C) 2002 Benoit Grégoire <bock@step.polymtl.ca>        *
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

#define _GNU_SOURCE

#include "config.h"


#include <glib.h>
#include <gmodule.h>
//#include <gtk/gtk.h>
#include <glade/glade.h>

#include "gnc-generic-import.h"
#include "Account.h"
#include "Transaction.h"
#include "dialog-commodity.h"
#include "dialog-utils.h"
#include "AccWindow.h"
//#include "druid-utils.h"
//#include "global-options.h"
//#include "gnc-component-manager.h"
//#include "gnc-engine-util.h"
//#include "gnc-file-dialog.h"
//#include "gnc-gui-query.h"
#include "gnc-ui-util.h"
//#include "gnc-ui.h"
//#include "messages.h"
//#include "window-help.h"

//#include <g-wrap-wct.h>

struct _accountpickerdialog {
  GtkWidget       * dialog;
  GtkWidget       * treeview;
  AccountGroup * acct_group;
  Account * selected_acct;
};

 /* static gint
  test_str_cmp(gconstpointer a, gconstpointer b) {
  return strcmp(a, b);
  }*/
/********************************************************************\
 * Functions needed by gnc_import_select_account
 * 
\********************************************************************/

static void acct_tree_add_accts(AccountGroup * accts, GtkCTree * tree, GtkCTreeNode * parent)
{
  GtkCTreeNode * node;
  Account *current_acct;
  guint i;
  gchar * acctinfo[3];
  for(i=0;i<xaccGroupGetNumAccounts(accts);i++)
    {
      current_acct = xaccGroupGetAccount(accts, i);
      acctinfo[0]=(gchar *)xaccAccountGetName(current_acct);
      acctinfo[1]=g_strdup(xaccAccountGetTypeStr(xaccAccountGetType(current_acct)));
      acctinfo[2]=gnc_import_get_acc_online_id(current_acct);
      //printf("acct_tree_add_acct(): %s%s",xaccAccountGetName(current_acct),"\n");
      node = gtk_ctree_insert_node         (tree,
				     parent,
				     NULL,
				     acctinfo,
				     2,
				     NULL,NULL,NULL,NULL,
				     FALSE,//isleaf
				     FALSE);
      gtk_ctree_node_set_row_data     (tree,
				       node,
                                       current_acct);
      acct_tree_add_accts(xaccAccountGetChildren(current_acct), tree, node);
    }
}

static void
build_acct_tree(struct _accountpickerdialog * picker) {
  GtkCTreeNode * new_sel;
   //printf("build_acct_tree(): Start\n");
  
  if(picker->acct_group==NULL)
    {
      printf("build_acct_tree():Error: acct_group is NULL\n");
    }
  gtk_clist_clear(GTK_CLIST(picker->treeview));
  gtk_clist_set_column_justification (GTK_CLIST(picker->treeview),
                                      1, GTK_JUSTIFY_CENTER);
  acct_tree_add_accts(picker->acct_group,  GTK_CTREE(picker->treeview), NULL);
  
  if(picker->selected_acct!=NULL) {
    new_sel = gtk_ctree_find_by_row_data(GTK_CTREE(picker->treeview),
					 NULL,
					 picker->selected_acct);
    
    gtk_ctree_select(GTK_CTREE(picker->treeview), new_sel);
    gtk_ctree_node_moveto(GTK_CTREE(picker->treeview), new_sel, 0,
                          0.5, 0.0);
  }
  gtk_clist_columns_autosize (GTK_CLIST (picker->treeview));
  gtk_clist_column_titles_passive (GTK_CLIST (picker->treeview));
}

/*static void
  new_child_string_cb(char * string, gpointer data) {
  printf("new_child_string_cb(char * string, gpointer data)\n");
  if(string) {
  strncpy(data, string, 250);
  }
  else {
  *(char *)data = 0;
  }
  }*/

/* When user clicks to create a new account */
static void
gnc_ui_generic_account_picker_new_cb(GtkButton * w, gpointer user_data) {
  struct _accountpickerdialog * picker = user_data;  
  // printf(" gnc_ui_generic_account_picker_new_cb():Start\n");  
  printf("WRITEME: gnc_ui_generic_account_picker_new_cb() Write a more flexible function in dialog-account.c and AccWindow.h to fill in defaults\n");
  picker->selected_acct = gnc_ui_new_accounts_from_name_window_with_types("New OFX account", NULL);
  printf("WRITEME: gnc_ui_generic_account_picker_new_cb() Here we should check if account type is compatible, currency matches, etc.\n");
  build_acct_tree(picker);
}

static void
gnc_ui_generic_account_picker_select_cb(GtkCTree   * tree,
                                    GtkCTreeNode  * node,
                                    gint column,
                                    gpointer  user_data) {
  struct _accountpickerdialog * picker = user_data;
  //printf("gnc_ui_generic_account_picker_select_cb()\n");
  gtk_ctree_node_get_row_data(tree, node);
  picker->selected_acct = gtk_ctree_node_get_row_data(tree, node);
}

/*Will be called when unselection an account, or when the user clicks cancel*/
static void
gnc_ui_generic_account_picker_unselect_cb(GtkCTree   * tree,
                                      GtkCTreeNode  * node,
                                      gint column,
                                      gpointer  user_data) {
 struct _accountpickerdialog * picker = user_data;
 //printf("gnc_ui_generic_account_picker_unselect_cb()\n");
 picker->selected_acct = NULL;
}

static int
gnc_ui_generic_account_picker_map_cb(GtkWidget * w, gpointer user_data) {
  printf("gnc_ui_generic_account_picker_map_cb()\n");
  /* update the tree display */
  build_acct_tree(user_data);
  return FALSE;
}

static gpointer test_acct_online_id_match(Account *acct, gpointer param_online_id)
{
  gchar * current_online_id = gnc_import_get_acc_online_id(acct);
  if((current_online_id!=NULL&&param_online_id!=NULL)&&strcmp(current_online_id, param_online_id)==0)
    {
      return (gpointer *) acct;
    }
  else
    {
      return NULL;
    }
}

Account * gnc_import_select_account(char * account_online_id_value)
{
  struct _accountpickerdialog * picker = g_new0(struct _accountpickerdialog, 1);
  gint ui_retval;
  Account * retval = NULL;
  GladeXML *xml;
  GtkWidget * online_id_label;
  picker->acct_group = gnc_get_current_group();
  if(picker->acct_group == NULL)
    {
      printf("WARNING:gnc_import_select_account(): The account group is NULL\n");
    }
  //printf("gnc_import_select_account(): Looking for account with online_id: %s%s", account_online_id_value ,"\n");
  retval = xaccGroupForEachAccount(picker->acct_group,
				   test_acct_online_id_match,
				   account_online_id_value,
				   TRUE);
  if(retval==NULL)
    {
      /* load the interface */
      xml = gnc_glade_xml_new ("generic-import.glade", "Generic Import Account Picker");
      /* connect the signals in the interface */
      if(xml==NULL)
	{
	  printf("gnc_import_select_account(): Error opening the glade interface\n");
	}
      
      glade_xml_signal_connect_data(xml, "gnc_ui_generic_account_picker_new_cb", GTK_SIGNAL_FUNC (gnc_ui_generic_account_picker_new_cb), picker);
      glade_xml_signal_connect_data(xml, "gnc_ui_generic_account_picker_select_cb", GTK_SIGNAL_FUNC(gnc_ui_generic_account_picker_select_cb), picker);
      glade_xml_signal_connect_data(xml, "gnc_ui_generic_account_picker_unselect_cb", GTK_SIGNAL_FUNC(gnc_ui_generic_account_picker_unselect_cb), picker);
      glade_xml_signal_connect_data(xml, "gnc_ui_generic_account_picker_map_cb", GTK_SIGNAL_FUNC(gnc_ui_generic_account_picker_map_cb), picker);
      
      picker->dialog     = glade_xml_get_widget (xml, "Generic Import Account Picker");
      picker->treeview   = glade_xml_get_widget (xml, "account_tree");
      online_id_label = glade_xml_get_widget (xml, "online_id_label");
      
      //printf("gnc_import_select_account(): Fin get widget\n");
      gtk_label_set_text((GtkLabel*)online_id_label, account_online_id_value);

      build_acct_tree(picker);

      ui_retval = gnome_dialog_run_and_close(GNOME_DIALOG(picker->dialog));  

      if(ui_retval == 0) {
	gnc_import_set_acc_online_id(picker->selected_acct, account_online_id_value);
	retval=picker->selected_acct;
      }
      else {
	retval=NULL;
      }
    }      
  g_free(picker);
//  printf("gnc_import_select_account(): Return value: %p%s%s%s",retval,", account name:",xaccAccountGetName(retval),"\n");
  return retval;
}

/********************************************************************\
 * Setter and getter functions for the online_id kvp frame in
 * Account and Transaction 
\********************************************************************/

gchar * gnc_import_get_acc_online_id(Account * account)
{
  gchar * string = NULL;
  kvp_frame * frame;
  kvp_value * value;
  frame = xaccAccountGetSlots(account);
  value = kvp_frame_get_slot(frame, "online_id");
  string = kvp_value_get_string(value);  
  return string;
}

void gnc_import_set_acc_online_id(Account * account, gchar * string_value)
{
  kvp_frame * frame;
  kvp_value * value;
  frame = xaccAccountGetSlots(account);
  if(frame==NULL)
    {
      printf("gnc_import_set_acc_online_id():The kvp_frame was NULL, allocating new one\n");
      frame = kvp_frame_new();
    }
  value = kvp_frame_get_slot(frame, "online_id");
  //kvp_value_delete(value);
  value = kvp_value_new_string(string_value);
  kvp_frame_set_slot(frame,"online_id",value);  
  xaccAccountSetSlots_nc(account,frame);
  return;
}

gchar * gnc_import_get_trans_online_id(Transaction * transaction)
{
  gchar * string = NULL;
  kvp_frame * frame;
  kvp_value * value;
  frame = xaccTransGetSlots(transaction);
  value = kvp_frame_get_slot(frame, "online_id");
  string = kvp_value_get_string(value);  
  return string;
}

void gnc_import_set_trans_online_id(Transaction * transaction, gchar * string_value)
{
  kvp_frame * frame;
  kvp_value * value;
  printf("gnc_import_set_acc_online_id(): Start\n");
  frame = xaccTransGetSlots(transaction);
  if(frame==NULL)
    {
      printf("gnc_import_set_trans_online_id():The kvp_frame was NULL, allocating new one\n");
      frame = kvp_frame_new();
    }
  value = kvp_frame_get_slot(frame, "online_id");
  if(value != NULL)
    {
      kvp_value_delete(value);
    }
  value = kvp_value_new_string(string_value);
  kvp_frame_set_slot(frame,"online_id",value);  
  xaccTransSetSlots_nc(transaction,frame);
  return;
}

/********************************************************************\
 * Functions used by gnc_import_add_trans(Transaction *trans) 
\********************************************************************/

/********************************************************************\
 * check_trans_online_id() Weird function, to be used by 
 * xaccAccountForEachTransaction.  Takes to pointers to two 
 * and return TRUE if their online_id kvp_frame do NOT match or
 * if both pointers point to the same transaction 
\********************************************************************/
static gboolean check_trans_online_id(Transaction *trans1, void *trans2)
{
  gchar * online_id1 = gnc_import_get_trans_online_id(trans1);
  gchar * online_id2 = gnc_import_get_trans_online_id((Transaction *)trans2);

  if(trans1==(Transaction *)trans2||strcmp(online_id1, online_id2)!=0)
    {
      return TRUE;
    }
  else
    {
      //printf("test_trans_online_id(): Duplicate found\n");
      return FALSE;
    }
}

void gnc_import_add_trans(Transaction *trans)
{
  gint i;
  Split * source_split;
  Account * dest_acct;
  gboolean trans_not_found=TRUE;
  
  /*For each split in the transaction, check if the parent account contains a transaction with the same online id */
  for(i=0;(source_split=xaccTransGetSplit(trans,i))!=NULL&&(trans_not_found==TRUE);i++)
    {
    printf("gnc_import_add_trans():Checking split %d%s",i," for duplicates\n");
    dest_acct=xaccSplitGetAccount(source_split);
    trans_not_found = xaccAccountForEachTransaction(dest_acct,
						    check_trans_online_id,
						    trans);
  }
  if(trans_not_found==FALSE)
    {
      printf("gnc_import_add_trans(): Transaction with same online ID exists, destroying current transaction\n");
      xaccTransBeginEdit(trans);
      xaccTransDestroy(trans);
      xaccTransCommitEdit(trans);
  }
  return;
}
