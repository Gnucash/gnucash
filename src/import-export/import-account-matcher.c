/********************************************************************\
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
/** @addtogroup Import_Export
    @{ */
/**@internal
	@file import-account-matcher.c
 * \brief A very generic and flexible account matcher/picker
 \author Copyright (C) 2002 Benoit Grégoire <bock@step.polymtl.ca>
 */
#define _GNU_SOURCE

#include "config.h"


#include <glib.h>
#include <gmodule.h>

#include <glade/glade.h>

#include <stdlib.h>
#include "import-account-matcher.h"
#include "import-utilities.h"
#include "dialog-utils.h"
#include "AccWindow.h"

#include "gnc-engine-util.h"

#include "gnc-ui-util.h"


static short module = MOD_IMPORT;

/********************************************************************\
 *   Constants   *
\********************************************************************/

#define NUM_COLUMNS_CLIST 4
static const int CLIST_NAME= 0;
static const int CLIST_TYPE = 1;
static const int CLIST_DESCRIPTION = 2;
static const int CLIST_ONLINE_ID = 3;

/********************************************************************\
 *   Structs   *
\********************************************************************/

struct _accountpickerdialog {
  GtkWidget       * dialog;
  GtkWidget       * treeview;
  AccountGroup * acct_group;
  Account * selected_acct;
  gchar * account_human_description;
  gnc_commodity * new_account_default_commodity;
  GNCAccountType new_account_default_type;

};

/* static gint
   test_str_cmp(gconstpointer a, gconstpointer b) {
   return strcmp(a, b);
   }*/
/********************************************************************\
 * Functions needed by gnc_import_select_account
 * 
\********************************************************************/

static void acct_tree_add_accts(struct _accountpickerdialog * picker, AccountGroup * accts, GtkCTree * tree, GtkCTreeNode * parent)
{
  GtkCTreeNode * node;
  Account *current_acct;
  guint i;
  gchar * acctinfo[NUM_COLUMNS_CLIST];

  for(i=0;i<xaccGroupGetNumAccounts(accts);i++)
    {
      current_acct = xaccGroupGetAccount(accts, i);
      acctinfo[CLIST_NAME]=(gchar *)xaccAccountGetName(current_acct);
      acctinfo[CLIST_TYPE]=g_strdup(xaccAccountGetTypeStr(xaccAccountGetType(current_acct)));
      acctinfo[CLIST_DESCRIPTION]=(gchar *)xaccAccountGetDescription(current_acct);
      acctinfo[CLIST_ONLINE_ID]=g_strdup(gnc_import_get_acc_online_id(current_acct));
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
      if(current_acct==picker->selected_acct)
	{
	  gtk_ctree_select(tree,
			   node);
	}
      acct_tree_add_accts(picker, xaccAccountGetChildren(current_acct), tree, node);
    }
}

static void
build_acct_tree(struct _accountpickerdialog * picker) {
  GtkCTreeNode * new_sel;
  TRACE("Begin");
  
  if(picker->acct_group==NULL)
    {
      PERR("acct_group is NULL");
    }
  gtk_clist_clear(GTK_CLIST(picker->treeview));
  gtk_clist_set_column_justification (GTK_CLIST(picker->treeview),
                                      1, GTK_JUSTIFY_CENTER);
  acct_tree_add_accts(picker, picker->acct_group,  GTK_CTREE(picker->treeview), NULL);
  
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
  GList * valid_types = NULL;
  DEBUG("Begin");  
  
  if(picker->new_account_default_type!=NO_TYPE)
    {
      /*Yes, this is weird, but we really DO want to pass the value instead of the pointer...*/
     valid_types = g_list_prepend(valid_types, (gpointer)picker->new_account_default_type);
    }
  picker->selected_acct = gnc_ui_new_accounts_from_name_with_defaults ( picker->account_human_description,
									valid_types,
									picker->new_account_default_commodity,
									picker->selected_acct);
  build_acct_tree(picker);
}

static void
gnc_ui_generic_account_picker_select_cb(GtkCTree   * tree,
					GtkCTreeNode  * node,
					gint column,
					gpointer  user_data) {
  struct _accountpickerdialog * picker = user_data;
  TRACE("Begin");
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
  TRACE("Begin");
  picker->selected_acct = NULL;
}

static gpointer test_acct_online_id_match(Account *acct, gpointer param_online_id)
{
  const gchar * current_online_id = gnc_import_get_acc_online_id(acct);
  if( (current_online_id != NULL
       && param_online_id != NULL )
      && strcmp( current_online_id, param_online_id ) == 0 )
    {
      return (gpointer *) acct;
    }
  else
    {
      return NULL;
    }
}

Account * gnc_import_select_account(char * account_online_id_value,
				    char auto_create,
				    char * account_human_description,
				    gnc_commodity * new_account_default_commodity,
				    GNCAccountType new_account_default_type,
				    Account * default_selection)
{
  #define ACCOUNT_DESCRIPTION_MAX_SIZE 255
  struct _accountpickerdialog * picker;
  gint ui_retval;
  Account * retval = NULL;
  GladeXML *xml;
  GtkWidget * online_id_label;
  gchar account_description_text[ACCOUNT_DESCRIPTION_MAX_SIZE] = "";

  DEBUG("Default commodity received: %s",gnc_commodity_get_fullname( new_account_default_commodity));
  DEBUG("Default account type received: %s",xaccAccountGetTypeStr( new_account_default_type));
  picker = g_new0(struct _accountpickerdialog, 1);
  picker->acct_group = gnc_get_current_group();
  if(picker->acct_group == NULL)
    {
      PWARN("The account group is NULL");
    }
  picker->account_human_description =  account_human_description;
  picker->new_account_default_commodity = new_account_default_commodity;
  picker->new_account_default_type = new_account_default_type;
  picker->selected_acct=default_selection;

  DEBUG("Looking for account with online_id: %s", account_online_id_value);
  if(account_online_id_value!=NULL)
    {
      retval = xaccGroupForEachAccount(picker->acct_group,
				       test_acct_online_id_match,
				       account_online_id_value,
				       TRUE);
    }
  if(retval==NULL && auto_create != 0)
    {
      /* load the interface */
      xml = gnc_glade_xml_new ("generic-import.glade", "Generic Import Account Picker");
      /* connect the signals in the interface */
      if(xml==NULL)
	{
	  PERR("Error opening the glade interface");
	}
      
      glade_xml_signal_connect_data(xml, "gnc_ui_generic_account_picker_new_cb", GTK_SIGNAL_FUNC (gnc_ui_generic_account_picker_new_cb), picker);
      glade_xml_signal_connect_data(xml, "gnc_ui_generic_account_picker_select_cb", GTK_SIGNAL_FUNC(gnc_ui_generic_account_picker_select_cb), picker);
      glade_xml_signal_connect_data(xml, "gnc_ui_generic_account_picker_unselect_cb", GTK_SIGNAL_FUNC(gnc_ui_generic_account_picker_unselect_cb), picker);      
      picker->dialog     = glade_xml_get_widget (xml, "Generic Import Account Picker");
      picker->treeview   = glade_xml_get_widget (xml, "account_tree");
      online_id_label = glade_xml_get_widget (xml, "online_id_label");
      
      //printf("gnc_import_select_account(): Fin get widget\n");

      if(account_human_description!=NULL)
	{
	  strncat(account_description_text, account_human_description, ACCOUNT_DESCRIPTION_MAX_SIZE-strlen(account_description_text));
	  strncat(account_description_text, "\n", ACCOUNT_DESCRIPTION_MAX_SIZE-strlen(account_description_text));
	}
      if(account_online_id_value!=NULL)
	{
	  strncat(account_description_text,_("(Full account ID: "), ACCOUNT_DESCRIPTION_MAX_SIZE-strlen(account_description_text));
	  strncat(account_description_text, account_online_id_value, ACCOUNT_DESCRIPTION_MAX_SIZE-strlen(account_description_text));
	  strncat(account_description_text, ")", ACCOUNT_DESCRIPTION_MAX_SIZE-strlen(account_description_text));
	}
      else
	{
	  gtk_clist_set_column_visibility (GTK_CLIST (picker->treeview),
					   CLIST_ONLINE_ID,
					   FALSE);
	}
      gtk_label_set_text((GtkLabel*)online_id_label, account_description_text);
      build_acct_tree(picker);

      gtk_clist_columns_autosize (GTK_CLIST (picker->treeview));
      gtk_clist_column_titles_passive (GTK_CLIST (picker->treeview));

      ui_retval = gnome_dialog_run_and_close(GNOME_DIALOG(picker->dialog));  

      if(ui_retval == 0) {
	if( account_online_id_value != NULL)
	  {
	    gnc_import_set_acc_online_id(picker->selected_acct, account_online_id_value);
	  }
	retval=picker->selected_acct;
      }
      else {
	retval=NULL;
      }
    }     
  printf("WRITEME: gnc_import_select_account() Here we should check if account type is compatible, currency matches, etc.\n"); 
  g_free(picker);
  DEBUG("Return value: %p%s%s%s",retval,", account name:",xaccAccountGetName(retval),"\n");
  return retval;
}
/**@}*/
