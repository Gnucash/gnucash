/********************************************************************\
 * dialog-edit.c -- window for editing account information          *
 *                  (GnuCash)                                       *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1997, 1998, 1999 Linas Vepstas                     *
 * Copyright (C) 2000 Dave Peticolas <peticola@cs.ucdavis.edu>      *
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

#include "top-level.h"

#include <gnome.h>
#include <stdio.h>

#include "AccWindow.h"
#include "MainWindow.h"
#include "Refresh.h"
#include "FileDialog.h"
#include "MultiLedger.h"
#include "window-reconcile.h"
#include "dialog-utils.h"
#include "account-tree.h"
#include "window-help.h"
#include "query-user.h"
#include "messages.h"
#include "util.h"


/* From Account.c. One day, maybe this will be configurable. */
extern int unsafe_ops;

/* List of Open edit windows */
static EditAccWindow ** editAccList = NULL;

static gint last_width = 0;
static gint last_height = 0;


struct _editaccwindow
{
  GtkWidget * dialog;

  Account * account;

  AccountEditInfo edit_info;

  GtkWidget * parent_tree;
  GtkWidget * type_list;

  Account * current_parent;
  Account * top_level_account;

  gint type;
};


static int
gnc_ui_EditAccWindow_close_cb(GnomeDialog *dialog, gpointer user_data)
{
  EditAccWindow * editAccData = (EditAccWindow *) user_data;
  Account *acc = editAccData->account;

  REMOVE_FROM_LIST (EditAccWindow,editAccList,acc,account); 

  xaccFreeAccount(editAccData->top_level_account);
  editAccData->top_level_account = NULL;

  free(editAccData);

  gdk_window_get_geometry(GTK_WIDGET(dialog)->window, NULL, NULL,
                          &last_width, &last_height, NULL);

  gnc_save_window_size("account_edit_win", last_width, last_height);

  /* really close */
  return FALSE;
}

static void
gnc_ui_EditAccWindow_cancel_cb(GtkWidget * widget,
			       gpointer data)
{
  EditAccWindow *editAccData = (EditAccWindow *) data; 

  gnome_dialog_close(GNOME_DIALOG(editAccData->dialog));
}

static gboolean
gnc_filter_parent_accounts(Account *account, gpointer data)
{
  EditAccWindow *editAccData = data;

  if (account == NULL)
    return FALSE;

  if (account == editAccData->top_level_account)
    return TRUE;

  if (account == editAccData->account)
    return FALSE;

  if (xaccAccountHasAncestor(account, editAccData->account))
    return FALSE;

  return TRUE;
}

static void 
gnc_ui_EditAccWindow_help_cb(GtkWidget *widget, gpointer data)
{
  helpWindow(NULL, HELP_STR, HH_ACCEDIT);
}

static void
gnc_edit_change_account_types(Account *account, Account *except, int type)
{
  AccountGroup *children;
  int i, num_children;

  if (account == NULL)
    return;

  if (account == except)
    return;

  if (xaccAccountGetType(account) != type)
  {
    /* Just refreshing won't work. */
    xaccDestroyLedgerDisplay(account);

    xaccAccountBeginEdit(account, GNC_F);
    xaccAccountSetType(account, type);
    xaccAccountCommitEdit(account);
  }

  children = xaccAccountGetChildren(account);
  if (children == NULL)
    return;

  num_children = xaccGetNumAccounts(children);
  for (i = 0; i < num_children; i++)
  {
    account = xaccGroupGetAccount(children, i);
    gnc_edit_change_account_types(account, except, type);
  }
}

static void
gnc_ui_EditAccWindow_ok_cb(GtkWidget * widget, gpointer data)
{
  EditAccWindow *editAccData = (EditAccWindow *) data; 
  AccountFieldStrings strings;

  gboolean change_children;
  gboolean has_children;
  gboolean change_all;

  GNCAccountTree *tree;

  Account *new_parent;
  Account *account;
  AccountGroup *children;

  int current_type;

  char *old;

  gnc_ui_extract_field_strings(&strings, &editAccData->edit_info);

  /* check for valid name */
  if (safe_strcmp(strings.name, "") == 0)
  {
    gnc_error_dialog_parented(GTK_WINDOW(editAccData->dialog),
                              ACC_NO_NAME_MSG);
    gnc_ui_free_field_strings(&strings);
    return;
  }

  /* check for valid type */
  if (editAccData->type == BAD_TYPE)
  {
    gnc_error_dialog_parented(GTK_WINDOW(editAccData->dialog), ACC_TYPE_MSG);
    gnc_ui_free_field_strings(&strings);
    return;
  }

  tree = GNC_ACCOUNT_TREE(editAccData->parent_tree);
  new_parent = gnc_account_tree_get_current_account(tree);

  /* Parent check, probably not needed, but be safe */
  if (!gnc_filter_parent_accounts(new_parent, editAccData))
  {
    gnc_error_dialog_parented(GTK_WINDOW(editAccData->dialog),
                              ACC_BAD_PARENT_MSG);
    gnc_ui_free_field_strings(&strings);
    return;
  }

  account = editAccData->account;

  children = xaccAccountGetChildren(account);
  if (children == NULL)
    has_children = FALSE;
  else if (xaccGetNumAccounts(children) == 0)
    has_children = FALSE;
  else
    has_children = TRUE;

  current_type = xaccAccountGetType(account);

  /* currency check */
  old = xaccAccountGetCurrency(account);
  if (old == NULL)
    old = "";
  if ((safe_strcmp(old, strings.currency) != 0) &&
      (safe_strcmp(old, "") != 0))
  {
    gchar * s;
    gboolean result;

    s = g_strdup_printf(EDIT_CURRENCY_MSG, old, strings.currency);
    result = gnc_verify_dialog_parented(GTK_WINDOW(editAccData->dialog),
                                        s, GNC_T);
    g_free(s);

    if (!result)
    {
      gnc_ui_free_field_strings(&strings);
      return;
    }
  }

  /* security check */
  old = xaccAccountGetSecurity(account);
  if (old == NULL)
    old = "";
  if ((safe_strcmp(old, strings.security) != 0) &&
      (safe_strcmp(old, "") != 0))
  {
    gchar * s;
    gboolean result;

    s = g_strdup_printf(EDIT_SECURITY_MSG, old, strings.security);
    result = gnc_verify_dialog_parented(GTK_WINDOW(editAccData->dialog),
                                        s, GNC_T);
    g_free(s);

    if (!result)
    {
      gnc_ui_free_field_strings(&strings);
      return;
    }
  }

  /* If the account has children and the new type isn't compatible
   * with the old type, the children's types must be changed. */
  change_children = (has_children &&
                     !xaccAccountTypesCompatible(current_type,
                                                 editAccData->type));

  /* If the new parent's type is not compatible with the new type,
   * the whole sub-tree containing the account must be re-typed. */
  if (new_parent != editAccData->top_level_account)
  {
    int parent_type;

    parent_type = xaccAccountGetType(new_parent);

    if (!xaccAccountTypesCompatible(parent_type, editAccData->type))
      change_all = TRUE;
    else
      change_all = FALSE;
  }
  else
    change_all = FALSE;

  if (change_children || change_all)
  {
    gchar *format_str;
    gchar *warning_str;
    gchar *type_str;
    gboolean result;

    if (change_all)
      format_str = TYPE_WARN1_MSG;
    else
      format_str = TYPE_WARN2_MSG;

    type_str = xaccAccountGetTypeStr(editAccData->type);

    warning_str = g_strdup_printf(format_str, type_str);

    result = gnc_verify_dialog_parented(GTK_WINDOW(editAccData->dialog),
                                        warning_str, GNC_T);

    g_free(warning_str);

    if (!result)
    {
      gnc_ui_free_field_strings(&strings);
      return;
    }
  }


  /* Everything checked out, perform the changes */
  xaccAccountBeginEdit(account, GNC_F);

  if (xaccAccountGetType(account) != editAccData->type)
  {
    /* Just refreshing won't work. */
    xaccDestroyLedgerDisplay(account);

    xaccAccountSetType(account, editAccData->type);
  }

  gnc_ui_install_field_strings(account, &strings, FALSE);
  if (new_parent != editAccData->current_parent)
  {
    if (new_parent == editAccData->top_level_account)
      xaccGroupInsertAccount(gncGetCurrentGroup(), account);
    else
      xaccInsertSubAccount(new_parent, account);
  }

  xaccAccountCommitEdit(account);

  if (change_children)
    gnc_edit_change_account_types(account, NULL, editAccData->type);

  if (change_all)
  {
    Account *ancestor;
    Account *temp;

    temp = xaccAccountGetParentAccount(account);
    do
    {
      ancestor = temp;
      temp = xaccAccountGetParentAccount(ancestor);
    } while (temp != NULL);

    gnc_edit_change_account_types(ancestor, account, editAccData->type);
  }

  gnc_ui_free_field_strings(&strings);

  gnc_refresh_main_window();
  gnc_group_ui_refresh(gncGetCurrentGroup());

  gnome_dialog_close(GNOME_DIALOG(editAccData->dialog));
}

static void 
gnc_type_select_cb(GtkCList * type_list, gint row, gint column,
                   GdkEventButton * event, gpointer data)
{
  gboolean sensitive;
  EditAccWindow * editAccData = data;

  if(editAccData == NULL)
    return;

  if (!gtk_clist_get_selectable(type_list, row))
  {
    gtk_clist_unselect_row(type_list, row, 0);
    return;
  }

  editAccData->type = row;

  if (!unsafe_ops)
    return;

  sensitive = (editAccData->type == STOCK    ||
	       editAccData->type == MUTUAL   ||
	       editAccData->type == CURRENCY);

  gtk_widget_set_sensitive(GTK_WIDGET(editAccData->edit_info.security_entry),
			   sensitive);
  gtk_widget_set_sensitive(GTK_WIDGET(editAccData->edit_info.source_menu),
			   sensitive);
}

static void 
gnc_type_unselect_cb(GtkCList * type_list, gint row, gint column,
                     GdkEventButton * event, gpointer data)
{
  EditAccWindow * editAccData = data;

  editAccData->type = BAD_TYPE;

  gtk_widget_set_sensitive(GTK_WIDGET(editAccData->edit_info.security_entry),
			   FALSE);
  gtk_widget_set_sensitive(GTK_WIDGET(editAccData->edit_info.source_menu),
			   FALSE);
}

static void
gnc_fill_type_list(GtkCList *type_list)
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

static GtkWidget *
gnc_account_type_list_create(EditAccWindow * editAccData)
{
  GtkWidget *frame, *scroll_win;

  frame = gtk_frame_new(ACC_TYPE_STR);

  editAccData->type_list = gtk_clist_new(1);
  gtk_clist_set_selection_mode(GTK_CLIST(editAccData->type_list),
                               GTK_SELECTION_BROWSE);
  gtk_container_border_width(GTK_CONTAINER(editAccData->type_list), 3);

  gnc_fill_type_list(GTK_CLIST(editAccData->type_list));

  gtk_clist_columns_autosize(GTK_CLIST(editAccData->type_list));

  gtk_signal_connect(GTK_OBJECT(editAccData->type_list), "select-row",
		     GTK_SIGNAL_FUNC(gnc_type_select_cb), editAccData);

  gtk_signal_connect(GTK_OBJECT(editAccData->type_list), "unselect-row",
		     GTK_SIGNAL_FUNC(gnc_type_unselect_cb), editAccData);

  editAccData->type = xaccAccountGetType(editAccData->account);
  gtk_clist_select_row(GTK_CLIST(editAccData->type_list),
                       editAccData->type, 0);

  scroll_win = gtk_scrolled_window_new(NULL, NULL);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scroll_win),
                                 GTK_POLICY_NEVER, 
                                 GTK_POLICY_AUTOMATIC);

  gtk_container_add(GTK_CONTAINER(frame), scroll_win);
  gtk_container_border_width(GTK_CONTAINER(scroll_win), 5);
  gtk_container_add(GTK_CONTAINER(scroll_win), editAccData->type_list);

  return frame;
}

static GtkWidget *
gnc_ui_create_parent_acc_frame(EditAccWindow *editAccData)
{
  Account *current_parent;
  GtkWidget *scroll_win;
  GtkWidget *frame;
  GtkWidget *tree;
  
  editAccData->top_level_account = xaccMallocAccount();
  xaccAccountSetName(editAccData->top_level_account, TOP_ACCT_STR);
  frame = gtk_frame_new(PARENT_ACC_STR);

  scroll_win = gtk_scrolled_window_new(NULL, NULL);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scroll_win), 
				 GTK_POLICY_AUTOMATIC,  
				 GTK_POLICY_AUTOMATIC);   
  gtk_container_border_width(GTK_CONTAINER(frame), 5);    

  tree = gnc_account_tree_new_with_root(editAccData->top_level_account); 
  gtk_clist_set_selection_mode(GTK_CLIST(tree), GTK_SELECTION_BROWSE);
  gnc_account_tree_hide_all_but_name(GNC_ACCOUNT_TREE(tree)); 
  gtk_clist_column_titles_hide(GTK_CLIST(tree));
  gnc_account_tree_set_filter(GNC_ACCOUNT_TREE(tree),
                              gnc_filter_parent_accounts, editAccData);
  gnc_account_tree_refresh(GNC_ACCOUNT_TREE(tree));
  gnc_account_tree_expand_account(GNC_ACCOUNT_TREE(tree),
                                  editAccData->top_level_account);
  editAccData->parent_tree = tree;

  /* the initial setting should be the *parent* of the current account */
  current_parent = xaccAccountGetParentAccount(editAccData->account);
  if (current_parent == NULL)
    current_parent = editAccData->top_level_account;
  editAccData->current_parent = current_parent;

  gtk_container_add(GTK_CONTAINER(scroll_win), tree); 
  gtk_container_add(GTK_CONTAINER(frame), scroll_win);

  return frame;
}

static char *
gnc_edit_make_window_name(Account *account)
{
  char *fullname;
  char *title;

  fullname = xaccAccountGetFullName(account, gnc_get_account_separator());
  title = g_strconcat(fullname, " - ", EDIT_ACCT_STR, NULL);

  free(fullname);

  return title;
}

static void
gnc_edit_set_window_name(EditAccWindow *editAccData)
{
  char *title;

  title = gnc_edit_make_window_name(editAccData->account);

  gtk_window_set_title(GTK_WINDOW(editAccData->dialog), title);

  g_free(title);
}


/********************************************************************\
 * editAccountRefresh                                               *
 *   refreshes the edit window                                      *
 *                                                                  *
 * Args:   account - the account of the window to refresh           *
 * Return: none                                                     *
\********************************************************************/
void
editAccountRefresh(Account *account)
{
  EditAccWindow *editAccData; 

  FIND_IN_LIST (EditAccWindow, editAccList, account, account, editAccData);
  if (editAccData == NULL)
    return;

  gnc_edit_set_window_name(editAccData);
}


/********************************************************************\
 * editAccWindow                                                    *
 *   opens up a window to edit an account                           * 
 *                                                                  * 
 * Args:   acc - the account to edit                                * 
 * Return: null                                                     *
\********************************************************************/
EditAccWindow *
editAccWindow(Account *account)
{
  EditAccWindow * editAccData;
  GtkWidget *vbox, *hbox, *widget, *dialog, *source_menu;
  char *title;
  
  FETCH_FROM_LIST (EditAccWindow, editAccList, account, account, editAccData);

  title = gnc_edit_make_window_name(account);

  dialog = gnome_dialog_new(title,
			    GNOME_STOCK_BUTTON_OK,
			    GNOME_STOCK_BUTTON_CANCEL,
			    GNOME_STOCK_BUTTON_HELP,
			    NULL);

  g_free(title);

  editAccData->dialog  = dialog;
  editAccData->account = account;
  
  if (last_width == 0)
    gnc_get_window_size("account_edit_win", &last_width, &last_height);

  gtk_window_set_default_size(GTK_WINDOW(dialog), last_width, last_height);

  /* default to ok */
  gnome_dialog_set_default(GNOME_DIALOG(dialog), 0);

  /* destroy, don't hide */
  gnome_dialog_close_hides(GNOME_DIALOG(dialog), FALSE);

  /* allow grow and shrink, no auto-shrink */
  gtk_window_set_policy(GTK_WINDOW(dialog), TRUE, TRUE, FALSE);

  vbox = GNOME_DIALOG(editAccData->dialog)->vbox;

  /* Account field edit box */
  widget = gnc_ui_account_field_box_create_from_account
    (account, &editAccData->edit_info);

  gnome_dialog_editable_enters(GNOME_DIALOG(dialog),
			       editAccData->edit_info.name_entry);
  gnome_dialog_editable_enters(GNOME_DIALOG(dialog),
			       editAccData->edit_info.description_entry);
  gnome_dialog_editable_enters(GNOME_DIALOG(dialog),
			       editAccData->edit_info.currency_entry);
  gnome_dialog_editable_enters(GNOME_DIALOG(dialog),
			       editAccData->edit_info.security_entry);
  gnome_dialog_editable_enters(GNOME_DIALOG(dialog),
			       editAccData->edit_info.code_entry);

  if (!unsafe_ops)
  {
    gtk_widget_set_sensitive
      (GTK_WIDGET(editAccData->edit_info.currency_entry), FALSE);
    gtk_widget_set_sensitive
      (GTK_WIDGET(editAccData->edit_info.security_entry), FALSE);
  }

  gtk_box_pack_start(GTK_BOX(vbox), widget, FALSE, FALSE, 0);

  /* Box for types and tree */
  hbox = gtk_hbox_new (FALSE, 5);
  gtk_box_pack_start (GTK_BOX (vbox), hbox, TRUE, TRUE, 0);
  gtk_container_border_width (GTK_CONTAINER (hbox), 5);

  /* source menu */
  source_menu = gnc_ui_account_source_box_create_from_account
    (account, &editAccData->edit_info);

  /* List of account types */
  widget = gnc_account_type_list_create(editAccData);
  gtk_box_pack_start(GTK_BOX(hbox), widget, FALSE, FALSE, 0);

  /* Parent Account entry */
  widget = gnc_ui_create_parent_acc_frame(editAccData);
  gtk_box_pack_start(GTK_BOX(hbox), widget, TRUE, TRUE, 0);

  gtk_box_pack_start(GTK_BOX(vbox), source_menu, FALSE, FALSE, 0);

  { /* Notes entry */
    gchar * notes;

    widget = gnc_ui_notes_frame_create(&editAccData->edit_info.notes_entry);
    gtk_box_pack_start(GTK_BOX(vbox), widget, TRUE, TRUE, 0);
    notes = xaccAccountGetNotes(account);
    gtk_text_insert(GTK_TEXT(editAccData->edit_info.notes_entry),
		    NULL, NULL, NULL, notes, -1);
  }

  gnome_dialog_button_connect
    (GNOME_DIALOG(dialog), 0,
     GTK_SIGNAL_FUNC(gnc_ui_EditAccWindow_ok_cb), editAccData);

  gnome_dialog_button_connect
    (GNOME_DIALOG(dialog), 1,
     GTK_SIGNAL_FUNC(gnc_ui_EditAccWindow_cancel_cb), editAccData);

  gnome_dialog_button_connect
    (GNOME_DIALOG(dialog), 2,
     GTK_SIGNAL_FUNC(gnc_ui_EditAccWindow_help_cb), editAccData);

  gtk_signal_connect(GTK_OBJECT(dialog), "close",
		     GTK_SIGNAL_FUNC (gnc_ui_EditAccWindow_close_cb),
		     editAccData);

  gtk_widget_grab_focus(GTK_WIDGET(editAccData->edit_info.name_entry));

  gtk_widget_show_all(dialog);

  gnc_account_tree_select_account(GNC_ACCOUNT_TREE(editAccData->parent_tree),
                                  editAccData->current_parent, TRUE);

  return editAccData;
}


/********************************************************************\
 * gnc_ui_edit_acc_window_raise                                     *
 *   shows and raises an account editing window                     * 
 *                                                                  * 
 * Args:   editAccData - the edit window structure                  * 
\********************************************************************/
void
gnc_ui_edit_account_window_raise(EditAccWindow * editAccData)
{
  if (editAccData == NULL)
    return;

  if (editAccData->dialog == NULL)
    return;

  gtk_widget_show(editAccData->dialog);

  if (editAccData->dialog->window == NULL)
    return;

  gdk_window_raise(editAccData->dialog->window);
}


/********************************************************************\
 * Don't delete any structures -- the close callback will do this   *
\********************************************************************/

void
xaccDestroyEditAccWindow (Account * acc) 
{
  EditAccWindow *editAccData;

  FIND_IN_LIST (EditAccWindow,editAccList,acc,account,editAccData); 

  if (editAccData == NULL)
    return;
 
  gnome_dialog_close(GNOME_DIALOG(editAccData->dialog));
}
