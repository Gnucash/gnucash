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

#include "import-account-matcher.h"
#include "import-utilities.h"
#include "dialog-account.h"
#include "dialog-utils.h"

#include "gnc-commodity.h"
#include "gnc-engine.h"
#include "gnc-tree-view-account.h"
#include "gnc-ui.h"


static QofLogModule log_module = GNC_MOD_IMPORT;

/*-******************************************************************\
 *   Structs   *
\********************************************************************/

struct _accountpickerdialog {
  GtkWidget       * dialog;
  GncTreeViewAccount *account_tree;
  GtkWidget       * account_tree_box;
  const gchar * account_human_description;
  gchar * account_online_id_value;
  gnc_commodity * new_account_default_commodity;
  GNCAccountType new_account_default_type;
};

/*-******************************************************************\
 * Functions needed by gnc_import_select_account
 * 
\********************************************************************/

static void
build_acct_tree(struct _accountpickerdialog * picker)
{
  GtkTreeView *account_tree;

  /* Build a new account tree */
  TRACE("Begin");
  account_tree = gnc_tree_view_account_new(FALSE);
  picker->account_tree = GNC_TREE_VIEW_ACCOUNT(account_tree);
  gtk_tree_view_set_headers_visible (account_tree, TRUE);

  /* Add our custom column. */
  gnc_tree_view_account_add_kvp_column (picker->account_tree,
					_("Account ID"), "online_id");

  gtk_container_add(GTK_CONTAINER(picker->account_tree_box),
		    GTK_WIDGET(picker->account_tree));

  /* Configure the columns */
  gnc_tree_view_configure_columns (GNC_TREE_VIEW(picker->account_tree),
				   "type", "description", "online_id", NULL);
}

/* When user clicks to create a new account */
static void
gnc_import_add_account(struct _accountpickerdialog * picker)
{
  Account *selected_account, *new_account;
  GList * valid_types = NULL;
  /*DEBUG("Begin");  */
  
  if(picker->new_account_default_type!=NO_TYPE)
    {
      /*Yes, this is weird, but we really DO want to pass the value instead of the pointer...*/
     valid_types = g_list_prepend(valid_types, (gpointer)picker->new_account_default_type);
    }
  selected_account = gnc_tree_view_account_get_selected_account(picker->account_tree);
  new_account = gnc_ui_new_accounts_from_name_with_defaults ( picker->account_human_description,
							      valid_types,
							      picker->new_account_default_commodity,
							      selected_account);
  gnc_tree_view_account_set_selected_account(picker->account_tree, new_account);
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
				    gboolean auto_create,
				    const char * account_human_description,
				    gnc_commodity * new_account_default_commodity,
				    GNCAccountType new_account_default_type,
				    Account * default_selection,
				    gboolean * ok_pressed)
{
  #define ACCOUNT_DESCRIPTION_MAX_SIZE 255
  struct _accountpickerdialog * picker;
  gint response;
  Account * retval = NULL;
  GladeXML *xml;
  GtkWidget * online_id_label, *button;
  gchar account_description_text[ACCOUNT_DESCRIPTION_MAX_SIZE] = "";
  gboolean ok_pressed_retval = FALSE;

  ENTER("Default commodity received: %s",gnc_commodity_get_fullname( new_account_default_commodity));
  DEBUG("Default account type received: %s",xaccAccountGetTypeStr( new_account_default_type));
  picker = g_new0(struct _accountpickerdialog, 1);

  picker->account_online_id_value = account_online_id_value;
  picker->account_human_description =  account_human_description;
  picker->new_account_default_commodity = new_account_default_commodity;
  picker->new_account_default_type = new_account_default_type;

  /*DEBUG("Looking for account with online_id: %s", account_online_id_value);*/
  if(account_online_id_value!=NULL)
    {
      retval = xaccGroupForEachAccount(gnc_get_current_group (),
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
      
      picker->dialog     = glade_xml_get_widget (xml, "Generic Import Account Picker");
      picker->account_tree_box   = glade_xml_get_widget (xml, "account_tree_box");
      online_id_label = glade_xml_get_widget (xml, "online_id_label");
      button = glade_xml_get_widget (xml, "newbutton");
      gtk_button_set_use_stock (GTK_BUTTON(button), TRUE);

      //printf("gnc_import_select_account(): Fin get widget\n");

      if(account_human_description!=NULL)
	{
	  strncat(account_description_text, account_human_description,
		  ACCOUNT_DESCRIPTION_MAX_SIZE-strlen(account_description_text));
	  strncat(account_description_text, "\n",
		  ACCOUNT_DESCRIPTION_MAX_SIZE-strlen(account_description_text));
	}
      if(account_online_id_value!=NULL)
	{
	  strncat(account_description_text,_("(Full account ID: "),
		  ACCOUNT_DESCRIPTION_MAX_SIZE-strlen(account_description_text));
	  strncat(account_description_text, account_online_id_value,
		  ACCOUNT_DESCRIPTION_MAX_SIZE-strlen(account_description_text));
	  strncat(account_description_text, ")",
		  ACCOUNT_DESCRIPTION_MAX_SIZE-strlen(account_description_text));
	}
      gtk_label_set_text((GtkLabel*)online_id_label, account_description_text);
      build_acct_tree(picker);
      gnc_tree_view_account_set_selected_account(picker->account_tree, default_selection);

      gtk_window_set_modal(GTK_WINDOW(picker->dialog), TRUE);
      do {
	response = gtk_dialog_run(GTK_DIALOG(picker->dialog));
	switch (response) {
	 case GTK_RESPONSE_OK:
	  retval = gnc_tree_view_account_get_selected_account(picker->account_tree);
	  DEBUG("Selected account %p, %s", retval, xaccAccountGetName(retval));
	  if( account_online_id_value != NULL)
	    {
	      gnc_import_set_acc_online_id(retval, account_online_id_value);
	    }
	  ok_pressed_retval=TRUE;
	  break;
	 case GNC_RESPONSE_NEW:
	  gnc_import_add_account(picker);
	  break;
	 default:
	  ok_pressed_retval=FALSE;
	  break;
	}
      } while (response == GNC_RESPONSE_NEW);
      gtk_widget_destroy(picker->dialog);
    }
  else
    {
      ok_pressed_retval=TRUE; /* There was no dialog involved, so the computer "pressed" ok */
    }   
  /*FIXME: DEBUG("WRITEME: gnc_import_select_account() Here we should check if account type is compatible, currency matches, etc.\n"); */
  g_free(picker);
  /*DEBUG("Return value: %p%s%s%s",retval,", account name:",xaccAccountGetName(retval),"\n");*/
  if(ok_pressed!=NULL)
    {
      *ok_pressed=ok_pressed_retval;
    }
  LEAVE("Selected account %p, %s", retval, xaccAccountGetName(retval));
  return retval;
}
/**@}*/
