/********************************************************************\
 * dialog-add.c -- window for creating new accounts for GnuCash     *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1997, 1998, 1999 Linas Vepstas                     *
 * Copyright (C) 1999 Jeremy Collins                                *
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
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
 *                                                                  *
 *   Author: Rob Clark                                              *
 * Internet: rclark@cs.hmc.edu                                      *
 *  Address: 609 8th Street                                         *
 *           Huntington Beach, CA 92648-4632                        *
\********************************************************************/

/* TODO:
 * -- tooltips for the widgets in the window
 */

#include "top-level.h"

#include <gnome.h>

#include "AccWindow.h"
#include "MainWindow.h"
#include "FileDialog.h"
#include "Refresh.h"
#include "window-main.h"
#include "dialog-utils.h"
#include "account-tree.h"
#include "query-user.h"
#include "messages.h"
#include "util.h"


/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_GUI;

static int _accWindow_last_used_account_type = BANK;

static gchar * default_currency = "USD";
static gboolean default_currency_dynamically_allocated = FALSE;


struct _accwindow
{
  GtkCList  *type_list;

  AccountEditInfo edit_info;

  GNCAccountTree *tree;

  Account *parentAccount;
  Account *newAccount;
  gint	  type;
};


/********************************************************************\
 * Function: gnc_ui_accWindow_list_select_cb -  a callback for the  *
 *           list of account types window                           *
 *                                                                  *
 * Args:   type_list - the list widget                              *
 *         row       - the list row selected                        *
 *         column    - the list column selected (always 0)          *
 *         data      - gtk user data pointer, will point to the     *
 *                     accWindow structure                          *
 * Return: none                                                     *
 \********************************************************************/
static void 
gnc_ui_accWindow_list_select_cb(GtkCList * type_list, gint row, gint column,
				GdkEventButton * event, gpointer data)
{
  gboolean sensitive;
  AccWindow * accData = (AccWindow *) data;

  if(accData == NULL)
    return;

  if (!gtk_clist_get_selectable(type_list, row))
  {
    gtk_clist_unselect_row(type_list, row, 0);
    return;
  }

  accData->type = row;

  _accWindow_last_used_account_type = row;

  sensitive = (accData->type == STOCK    ||
	       accData->type == MUTUAL   ||
	       accData->type == CURRENCY);

  gtk_widget_set_sensitive(GTK_WIDGET(accData->edit_info.security_entry),
			   sensitive);
  gtk_widget_set_sensitive(GTK_WIDGET(accData->edit_info.source_menu),
			   sensitive);
}


/********************************************************************\
 * Function: gnc_ui_accWindow_list_unselect_cb -  a callback for the*
 *           list of account types window                           *
 *                                                                  *
 * Args:   type_list - the list widget                              *
 *         row       - the list row unselected                      *
 *         column    - the list column unselected (always 0)        *
 *         data      - gtk user data pointer, will point to the     *
 *                     accWindow structure                          *
 * Return: none                                                     *
 \********************************************************************/
static void 
gnc_ui_accWindow_list_unselect_cb(GtkCList * type_list, gint row, gint column,
				  GdkEventButton * event, gpointer data)
{
  AccWindow * accData = (AccWindow *) data;

  accData->type = BAD_TYPE;

  gtk_widget_set_sensitive(GTK_WIDGET(accData->edit_info.security_entry),
			   FALSE);
  gtk_widget_set_sensitive(GTK_WIDGET(accData->edit_info.source_menu),
			   FALSE);
}


/********************************************************************\
 * Function: gnc_ui_accWindow_list_fill - fils the list of account  *
 *           types widget with the account types                    *
 *                                                                  * 
 * Args:   type_list - the list to be filled                        *
 * Return: void                                                     *
\********************************************************************/
static void
gnc_ui_accWindow_list_fill(GtkCList *type_list)
{
  gint row;
  gchar *text[2] = { NULL, NULL };

  gtk_clist_clear(type_list);

  for (row = 0; row < NUM_ACCOUNT_TYPES; row++) 
  {
    text[0] = xaccAccountGetTypeStr(row);
    gtk_clist_append(type_list, text);
  }
}


/********************************************************************\
 * Function: gnc_ui_accWindow_list_box_create - creates the widget  *
 *           holding the list of account types.                     *
 *                                                                  * 
 * Args:   accData   - the associated accWindow                     *
 * Return: the box with the list                                    *
\********************************************************************/
static GtkWidget *
gnc_ui_accWindow_list_box_create(AccWindow * accData)
{
  GtkWidget *frame, *scroll_win;

  frame = gtk_frame_new(ACC_TYPE_STR);

  accData->type_list = GTK_CLIST(gtk_clist_new(1));
  gtk_container_border_width(GTK_CONTAINER(accData->type_list), 3);

  gnc_ui_accWindow_list_fill(accData->type_list);

  gtk_clist_columns_autosize(GTK_CLIST(accData->type_list));

  gtk_signal_connect(GTK_OBJECT(accData->type_list), "select-row",
		     GTK_SIGNAL_FUNC(gnc_ui_accWindow_list_select_cb),
		     accData);

  gtk_signal_connect(GTK_OBJECT(accData->type_list), "unselect-row",
		     GTK_SIGNAL_FUNC(gnc_ui_accWindow_list_unselect_cb),
		     accData);

  gtk_clist_select_row(accData->type_list,
		       _accWindow_last_used_account_type, 0);

  scroll_win = gtk_scrolled_window_new(NULL, NULL);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scroll_win),
                                 GTK_POLICY_NEVER, 
                                 GTK_POLICY_AUTOMATIC);

  gtk_container_add(GTK_CONTAINER(frame), scroll_win);
  gtk_container_border_width(GTK_CONTAINER(scroll_win), 5);
  gtk_container_add(GTK_CONTAINER(scroll_win), GTK_WIDGET(accData->type_list));

  return frame;
}


/********************************************************************\
 * Function: gnc_ui_accWindow_list_fill - fils the list of account  *
 *           types widget with the account types                    *
 *                                                                  * 
 * Args:   listOfTypes - the widget to be filled                    * 
 * Return: none                                                     *
\********************************************************************/
static void
gnc_ui_accWindow_list_row_set_active(GtkCList *type_list, gint row,
				     gboolean state)
{
  GtkStyle *style = gtk_widget_get_style(GTK_WIDGET(type_list));

  if (state)
  {
    gtk_clist_set_selectable(type_list, row, TRUE);
    gtk_clist_set_background(type_list, row, &style->white);
  }
  else
  {
    gtk_clist_unselect_row(type_list, row, 0);
    gtk_clist_set_selectable(type_list, row, FALSE);
    gtk_clist_set_background(type_list, row, &style->dark[GTK_STATE_NORMAL]);
  }
}

/********************************************************************\
 * Function: gnc_ui_accWindow_tree_select - the account selection   *
 *           callback                                               *
 *                                                                  * 
 * Args:   widget - the tree widget, child - the selected child,    *
 *         data - tells whether is a select or a deselect callack   * 
 * Return: none                                                     *
 * Notes: data == 0 unselect, data == 1 select                      *
 \********************************************************************/
static void
gnc_ui_accWindow_tree_select(GNCAccountTree *tree,
			     Account * account, 
			     gpointer data)
{
  AccWindow *accData = (AccWindow *) data;
  int       parentAccType;
  gboolean  compatible;
  gint      type;

  account = gnc_account_tree_get_current_account(tree);

  /* Deleselect any or select top account */
  if (account == NULL || account == accData->newAccount)
  {
    accData->parentAccount = NULL;

    /* sel all list widgets to be selectable */
    for (type = 0; type < NUM_ACCOUNT_TYPES; type++)
      gtk_clist_set_selectable(accData->type_list, type, TRUE);

    for (type = 0; type < NUM_ACCOUNT_TYPES; type++)
      gnc_ui_accWindow_list_row_set_active(accData->type_list, type, GNC_T);
  }
  else /* Some other account was selected */
  {
    accData->parentAccount = account;

    parentAccType = xaccAccountGetType(account);

    /* set the allowable account types for this parent */
    for (type = 0; type < NUM_ACCOUNT_TYPES; type++)
    {
      compatible = xaccAccountTypesCompatible(parentAccType, type);
      gnc_ui_accWindow_list_row_set_active(accData->type_list, type,
					   compatible);
    }

    /* now select a new account type if the account class has changed */
    compatible = xaccAccountTypesCompatible(parentAccType, accData->type);
    if (!compatible)
    {
      accData->type = parentAccType;
      gtk_clist_select_row(accData->type_list, parentAccType, 0);
      gtk_clist_moveto(accData->type_list, parentAccType, 0, 0.5, 0);
    }
  }
}


/********************************************************************\
 * Function: gnc_ui_accWindow_account_tree_box_create - creates the *
 *           widget holding the account tree.                       *
 *                                                                  * 
 * Args:   accData - the associated accWindow                       *
 * Return: the box with the list                                    *
\********************************************************************/
static GtkWidget *
gnc_ui_accWindow_account_tree_box_create(AccWindow * accData)
{
  GtkWidget *frame, *scrollWin, *accountTree;
    
  frame = gtk_frame_new(PARENT_ACC_STR);
    
  accountTree = gnc_account_tree_new_with_root(accData->newAccount);
  gtk_clist_column_titles_hide(GTK_CLIST(accountTree));
  gnc_account_tree_hide_all_but_name(GNC_ACCOUNT_TREE(accountTree));
  gnc_account_tree_refresh(GNC_ACCOUNT_TREE(accountTree));

  accData->tree = GNC_ACCOUNT_TREE(accountTree);

  gtk_signal_connect(GTK_OBJECT (accountTree), "select_account",
		     GTK_SIGNAL_FUNC(gnc_ui_accWindow_tree_select),
		     accData);
  gtk_signal_connect(GTK_OBJECT (accountTree), "unselect_account",
		     GTK_SIGNAL_FUNC(gnc_ui_accWindow_tree_select),
		     accData);

  scrollWin = gtk_scrolled_window_new (NULL, NULL);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrollWin),
				  GTK_POLICY_AUTOMATIC, 
				  GTK_POLICY_AUTOMATIC);

  gtk_container_add(GTK_CONTAINER(frame), scrollWin);
  gtk_container_border_width (GTK_CONTAINER (scrollWin), 5);
  gtk_container_add(GTK_CONTAINER(scrollWin), accountTree);

  return frame;
}


/********************************************************************\
 * gnc_ui_accWindow_create_account                                  *
 *    This function does the actually creating of the new account.  *
 *    The strings are assumed to be validated.                      *
 *                                                                  * 
 * Args:   account - account structure to use                       * 
 *         parent  - parent account or NULL                         *
 *         type    - the type of account                            *
 *         strings - strings to install in account                  *
 * Return: none                                                     *
\********************************************************************/
static void 
gnc_ui_accWindow_create_account(Account * account, Account * parent,
				gint type, AccountFieldStrings * strings)
{
  xaccAccountBeginEdit(account, 0);

  if (parent != NULL)
    xaccInsertSubAccount (parent, account);
  else
    xaccGroupInsertAccount(gncGetCurrentGroup(), account);

  xaccAccountSetType (account, type);

  gnc_ui_install_field_strings(account, strings, TRUE);

  { /* make a default transaction */
    Transaction  *trans = xaccMallocTransaction();
 
    xaccTransBeginEdit(trans, 1);
    xaccTransSetDateToday (trans);
    xaccTransSetDescription (trans, OPEN_BALN_STR);
    xaccTransCommitEdit(trans);
            
    xaccAccountInsertSplit (account, xaccTransGetSplit (trans, 0) );
  }

  xaccAccountCommitEdit (account);

  gnc_account_tree_insert_account(gnc_get_current_account_tree(), account);

  /* Refresh register so they have this account in their lists */
  gnc_group_ui_refresh(gncGetCurrentGroup());
}


/********************************************************************\
 * gnc_accWindow_create                                             *
 *   creates a window to create a new account.                      *
 *                                                                  * 
 * Args: accData - the information structure for this window        *
 * Return: the created window                                       *
 \*******************************************************************/
static GtkWidget *
gnc_accWindow_create(AccWindow *accData) 
{
  GtkWidget *vbox, *hbox, *dialog, *widget, *source_box;
  gchar     *title = SETUP_ACCT_STR;

  dialog = gnome_dialog_new(title, 
			    GNOME_STOCK_BUTTON_OK,
			    GNOME_STOCK_BUTTON_CANCEL,
			    NULL);

  vbox = GNOME_DIALOG(dialog)->vbox;

  /* Make this dialog modal */
  gtk_window_set_modal(GTK_WINDOW(dialog), TRUE);

  /* parent */
  gnome_dialog_set_parent(GNOME_DIALOG(dialog),
			  GTK_WINDOW(gnc_get_ui_data()));

  /* default to ok */
  gnome_dialog_set_default(GNOME_DIALOG(dialog), 0);

  /* don't close on buttons */
  gnome_dialog_set_close(GNOME_DIALOG(dialog), FALSE);

  /* allow grow and shrink, no auto-shrink */
  gtk_window_set_policy(GTK_WINDOW(dialog), TRUE, TRUE, FALSE);

  /* Account field edit box */
  widget = gnc_ui_account_field_box_create(&accData->edit_info, FALSE);
  gtk_box_pack_start(GTK_BOX(vbox), widget, FALSE, TRUE, 0);

  gtk_entry_set_text(GTK_ENTRY(accData->edit_info.currency_entry),
		     default_currency);

  gnome_dialog_editable_enters(GNOME_DIALOG(dialog),
			       accData->edit_info.name_entry);
  gnome_dialog_editable_enters(GNOME_DIALOG(dialog),
			       accData->edit_info.description_entry);
  gnome_dialog_editable_enters(GNOME_DIALOG(dialog),
			       accData->edit_info.currency_entry);
  gnome_dialog_editable_enters(GNOME_DIALOG(dialog),
			       accData->edit_info.security_entry);
  gnome_dialog_editable_enters(GNOME_DIALOG(dialog),
			       accData->edit_info.code_entry);

  /* Box for types and tree */
  hbox = gtk_hbox_new (FALSE, 5);
  gtk_box_pack_start (GTK_BOX (vbox), hbox, TRUE, TRUE, 0);
  gtk_container_border_width (GTK_CONTAINER (hbox), 5);

  /* Make source box now, so callbacks work */
  source_box = gnc_ui_account_source_box_create(&accData->edit_info);

  /* List of account types */
  widget = gnc_ui_accWindow_list_box_create(accData);
  gtk_box_pack_start(GTK_BOX(hbox), widget, FALSE, FALSE, 0);

  /* Account tree */
  widget = gnc_ui_accWindow_account_tree_box_create(accData);
  gtk_box_pack_start(GTK_BOX(hbox), widget, TRUE, TRUE, 0);

  /* Add source box */
  gtk_box_pack_start(GTK_BOX(vbox), source_box, FALSE, FALSE, 0);

  /* Notes entry */
  widget = gnc_ui_notes_frame_create(&accData->edit_info.notes_entry);
  gtk_box_pack_start(GTK_BOX(vbox), widget, FALSE, FALSE, 0);

  gnc_account_tree_select_account(accData->tree, accData->parentAccount, TRUE);

  return dialog;
}


/********************************************************************\
 * accWindow                                                        *
 *   opens up a window to create a new account.                     *
 *                                                                  * 
 * Args:   grp - not used                                           *
 * Return: null                                                     *
 \*******************************************************************/
AccWindow *
accWindow (AccountGroup *this_is_not_used) 
{
  static gint last_width = 0;
  static gint last_height = 0;

  AccWindow *accData = g_new0(AccWindow, 1);
  AccountFieldStrings strings;
  GtkWidget *dialog;
  gint result;

  accData->parentAccount = gnc_get_current_account();
  accData->newAccount    = xaccMallocAccount();
  accData->type          = _accWindow_last_used_account_type;

  xaccAccountSetName(accData->newAccount, NEW_TOP_ACCT_STR);

  dialog = gnc_accWindow_create(accData);
  if (last_width > 0)
    gtk_window_set_default_size(GTK_WINDOW(dialog), last_width, last_height);

  gtk_widget_show_all(dialog);

  while (1)
  {
    result = gnome_dialog_run(GNOME_DIALOG(dialog));

    if (result != 0) /* cancel or delete */
    {
      xaccFreeAccount(accData->newAccount);
      break;
    }

    gnc_ui_extract_field_strings(&strings, &accData->edit_info);

    /* check for valid name */
    if (safe_strcmp(strings.name, "") == 0)
    {
      gnc_error_dialog_parented(GTK_WINDOW(dialog), ACC_NO_NAME_MSG);
      gnc_ui_free_field_strings(&strings);
      continue;
    }

    /* check for valid type */
    if (accData->type == BAD_TYPE)
    {
      gnc_error_dialog_parented(GTK_WINDOW(dialog), ACC_TYPE_MSG);
      gnc_ui_free_field_strings(&strings);
      continue;
    }

    /* fixme check for unique code, if entered */

    gnc_ui_accWindow_create_account(accData->newAccount,
				    accData->parentAccount,
				    accData->type, &strings);

    gnc_ui_free_field_strings(&strings);

    break;
  }

  DEBUG("destroying account add window\n");

  gdk_window_get_geometry(dialog->window, NULL, NULL,
                          &last_width, &last_height, NULL);

  gtk_widget_destroy(dialog);
  g_free(accData);

  return NULL;
}


/*  This is a no-op for the gnome code */
void
xaccDestroyEditNotesWindow (Account *acc)
{
  return;
}


/*********************************************************************\
 * xaccSetDefaultNewaccountCurrency                                  *
 *   Set the default currency for new accounts                       *
 *   intended to be called by option handling code                   *
 *                                                                   *
 * Args:    new default_currency                                     *
 * Globals: default_currency, default_currency_dynamically_allocated *
 * Return value: none                                                *
\*********************************************************************/
void 
xaccSetDefaultNewaccountCurrency(char *new_default_currency)
{
  if (default_currency_dynamically_allocated)
    g_free(default_currency);

  default_currency = g_strdup(new_default_currency);
  default_currency_dynamically_allocated = TRUE;
}

/********************** END OF FILE *********************************\
\********************************************************************/
