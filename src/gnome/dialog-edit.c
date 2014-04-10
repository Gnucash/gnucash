/********************************************************************\
 * dialog-edit.c -- window for editing account information          *
 *                  (GnuCash)                                       *
 * Copyright (C) 1997 Robin D. Clark <rclark@cs.hmc.edu>            *
 * Copyright (C) 1997, 1998, 1999 Linas Vepstas <linas@linas.org>   *
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
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
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
  EditAccWindow * editAccData = user_data;
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
  EditAccWindow *editAccData = data; 

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
gnc_edit_change_account_types(GHashTable *change_type, Account *account,
                              Account *except, int type)
{
  AccountGroup *children;
  int i, num_children;

  if ((change_type == NULL) || (account == NULL))
    return;

  if (account == except)
    return;

  g_hash_table_insert(change_type, account, GINT_TO_POINTER(type));

  children = xaccAccountGetChildren(account);
  if (children == NULL)
    return;

  num_children = xaccGetNumAccounts(children);
  for (i = 0; i < num_children; i++)
  {
    account = xaccGroupGetAccount(children, i);
    gnc_edit_change_account_types(change_type, account, except, type);
  }
}

static void
change_func(gpointer key, gpointer value, gpointer field_code)
{
  Account *account = key;
  AccountFieldCode field = GPOINTER_TO_INT(field_code);

  if (account == NULL)
    return;

  xaccAccountBeginEdit(account, GNC_T);

  switch (field)
  {
    case ACCOUNT_CURRENCY:
      {
        char * string = value;

        xaccAccountSetCurrency(account, string);
      }
    break;
    case ACCOUNT_SECURITY:
      {
        char * string = value;

        xaccAccountSetSecurity(account, string);
      }
      break;
    case ACCOUNT_TYPE:
      {
        int type = GPOINTER_TO_INT(value);

        if (type == xaccAccountGetType(account))
          break;

        /* Just refreshing won't work. */
        xaccDestroyLedgerDisplay(account);

        xaccAccountSetType(account, type);
      }
      break;
    default:
      g_warning("unexpected account field code");
      break;
  }

  xaccAccountCommitEdit(account);
}

static void
make_account_changes(GHashTable *change_currency,
                     GHashTable *change_security,
                     GHashTable *change_type)
{
  if (change_currency != NULL)
    g_hash_table_foreach(change_currency, change_func,
                         GINT_TO_POINTER(ACCOUNT_CURRENCY));

  if (change_security != NULL)
    g_hash_table_foreach(change_security, change_func,
                         GINT_TO_POINTER(ACCOUNT_SECURITY));

  if (change_type != NULL)
    g_hash_table_foreach(change_type, change_func,
                         GINT_TO_POINTER(ACCOUNT_TYPE));
}

static void
gnc_account_change_currency_security(Account *account,
                                     GHashTable *change_currency,
                                     GHashTable *change_security,
                                     const char *currency,
                                     const char *security)
{
  const char *old_currency;
  const char *old_security;
  gboolean new_currency;
  gboolean new_security;
  GSList *stack;

  if ((account == NULL) || (currency == NULL) || (security == NULL))
    return;

  old_currency = xaccAccountGetCurrency(account);
  old_security = xaccAccountGetSecurity(account);

  if ((safe_strcmp(currency, old_currency) == 0) &&
      (safe_strcmp(security, old_security) == 0))
    return;

  if (safe_strcmp(currency, old_currency) != 0)
  {
    g_hash_table_insert(change_currency, account, (char *) currency);
    new_currency = TRUE;
  }
  else
    new_currency = FALSE;

  if (safe_strcmp(security, old_security) != 0)
  {
    g_hash_table_insert(change_security, account, (char *) security);
    new_security = TRUE;
  }
  else
    new_security = FALSE;

  stack = g_slist_prepend(NULL, account);

  while (stack != NULL)
  {
    Split *split;
    GSList *pop;
    gint i;

    pop = stack;
    account = pop->data;
    stack = g_slist_remove_link(stack, pop);
    g_slist_free_1(pop);

    i = 0;
    while ((split = xaccAccountGetSplit(account, i++)) != NULL)
    {
      Transaction *trans;
      Split *s;
      gint j;

      trans = xaccSplitGetParent(split);
      if (trans == NULL)
        continue;

      if (xaccTransIsCommonCurrency(trans, currency))
        continue;

      if (xaccTransIsCommonCurrency(trans, security))
        continue;

      j = 0;
      while ((s = xaccTransGetSplit(trans, j++)) != NULL)
      {
        gboolean add_it = FALSE;
        Account *a;

        a = xaccSplitGetAccount(s);

        if ((a == NULL) || (a == account))
          continue;

        if (g_hash_table_lookup(change_currency, a) != NULL)
          continue;

        if (g_hash_table_lookup(change_security, a) != NULL)
          continue;

        if (new_currency &&
            (safe_strcmp(old_currency, xaccAccountGetCurrency(a)) == 0))
        {
          g_hash_table_insert(change_currency, a, (char *) currency);
          add_it = TRUE;
        }

        if (new_security &&
            (safe_strcmp(old_security, xaccAccountGetSecurity(a)) == 0))
        {
          g_hash_table_insert(change_security, a, (char *) security);
          add_it = TRUE;
        }

        if (add_it)
          stack = g_slist_prepend(stack, a);
      }
    }
  }
}

typedef struct
{
  Account *account;
  AccountFieldCode field;
  GtkCList *list;
  guint count;
} FillStruct;

static void
fill_helper(gpointer key, gpointer value, gpointer data)
{
  Account *account = key;
  FillStruct *fs = data;
  gchar *strings[5];

  if (fs == NULL)
    return;

  if (fs->account == account)
    return;

  strings[0] = xaccAccountGetFullName(account, gnc_get_account_separator());
  strings[1] = (gchar *) gnc_ui_get_account_field_name(fs->field);
  strings[2] = (gchar *) gnc_ui_get_account_field_value_string(account,
                                                               fs->field);
  strings[4] = NULL;

  switch (fs->field)
  {
    case ACCOUNT_CURRENCY:
    case ACCOUNT_SECURITY:
      strings[3] = value;
      break;
    case ACCOUNT_TYPE:
      strings[3] = xaccAccountGetTypeStr(GPOINTER_TO_INT(value));
      break;
    default:
      g_warning("unexpected field type");
      free(strings[0]);
      return;
  }

  gtk_clist_append(fs->list, strings);
  free(strings[0]);
  fs->count++;
}

static guint
fill_list(Account *account, GtkCList *list,
          GHashTable *change, AccountFieldCode field)
{
  FillStruct fs;

  if (change == NULL)
    return 0;

  fs.account = account;
  fs.field = field;
  fs.list = list;
  fs.count = 0;

  g_hash_table_foreach(change, fill_helper, &fs);

  return fs.count;
}

static gboolean
extra_change_verify(EditAccWindow *editAccData,
                    GHashTable *change_currency,
                    GHashTable *change_security,
                    GHashTable *change_type)
{
  Account *account;
  GtkCList *list;
  gchar *titles[5];
  gboolean result;
  guint size;

  if (editAccData == NULL)
    return FALSE;

  account = editAccData->account;

  titles[0] = ACCOUNT_STR;
  titles[1] = FIELD_STR;
  titles[2] = OLD_VALUE_STR;
  titles[3] = NEW_VALUE_STR;
  titles[4] = NULL;

  list = GTK_CLIST(gtk_clist_new_with_titles(4, titles));

  size = 0;
  size += fill_list(account, list, change_currency, ACCOUNT_CURRENCY);
  size += fill_list(account, list, change_security, ACCOUNT_SECURITY);
  size += fill_list(account, list, change_type, ACCOUNT_TYPE);

  if (size == 0)
  {
    gtk_widget_destroy(GTK_WIDGET(list));
    return TRUE;
  }

  gtk_clist_column_titles_passive(list);
  gtk_clist_set_sort_column(list, 0);
  gtk_clist_sort(list);
  gtk_clist_columns_autosize(list);

  {
    GtkWidget *dialog;
    GtkWidget *scroll;
    GtkWidget *label;
    GtkWidget *frame;
    GtkWidget *vbox;

    dialog = gnome_dialog_new(VERIFY_CHANGES_STR,
                              GNOME_STOCK_BUTTON_OK,
                              GNOME_STOCK_BUTTON_CANCEL,
                              NULL);

    gnome_dialog_set_default(GNOME_DIALOG(dialog), 0);
    gnome_dialog_set_close(GNOME_DIALOG(dialog), FALSE);
    gnome_dialog_close_hides(GNOME_DIALOG(dialog), FALSE);
    gnome_dialog_set_parent(GNOME_DIALOG(dialog),
                            GTK_WINDOW(editAccData->dialog));
    gtk_window_set_policy(GTK_WINDOW(dialog), TRUE, TRUE, TRUE);
    gtk_window_set_default_size(GTK_WINDOW(dialog), 0, 300);

    vbox = GNOME_DIALOG(dialog)->vbox;

    label = gtk_label_new(VERIFY_CHANGE_MSG);
    gtk_box_pack_start(GTK_BOX(vbox), label, FALSE, FALSE, 0);

    frame = gtk_frame_new(NULL);
    gtk_box_pack_start(GTK_BOX(vbox), frame, TRUE, TRUE, 0);

    scroll = gtk_scrolled_window_new(NULL, NULL);
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scroll),
                                   GTK_POLICY_NEVER, 
                                   GTK_POLICY_AUTOMATIC);

    gtk_container_add(GTK_CONTAINER(frame), scroll);
    gtk_container_border_width(GTK_CONTAINER(scroll), 5);
    gtk_container_add(GTK_CONTAINER(scroll), GTK_WIDGET(list));

    gtk_widget_show_all(vbox);

    result = (gnome_dialog_run(GNOME_DIALOG(dialog)) == 0);

    gtk_widget_destroy(dialog);
  }

  return result;
}

static void
gnc_ui_EditAccWindow_ok_cb(GtkWidget * widget, gpointer data)
{
  EditAccWindow *editAccData = (EditAccWindow *) data; 
  AccountFieldStrings strings;

  GHashTable *change_currency;
  GHashTable *change_security;
  GHashTable *change_type;

  gboolean change_children;
  gboolean has_children;
  gboolean change_all;

  GNCAccountTree *tree;

  Account *new_parent;
  Account *account;
  AccountGroup *children;

  int current_type;

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

  change_currency = g_hash_table_new(NULL, NULL);
  change_security = g_hash_table_new(NULL, NULL);
  change_type     = g_hash_table_new(NULL, NULL);

  gnc_account_change_currency_security(account,
                                       change_currency,
                                       change_security,
                                       strings.currency,
                                       strings.security);

  children = xaccAccountGetChildren(account);
  if (children == NULL)
    has_children = FALSE;
  else if (xaccGetNumAccounts(children) == 0)
    has_children = FALSE;
  else
    has_children = TRUE;

  current_type = xaccAccountGetType(account);

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

  if (change_children)
    gnc_edit_change_account_types(change_type, account,
                                  NULL, editAccData->type);

  if (change_all)
  {
    Account *ancestor;
    Account *temp;

    temp = new_parent;

    do
    {
      ancestor = temp;
      temp = xaccAccountGetParentAccount(ancestor);
    } while (temp != NULL);

    gnc_edit_change_account_types(change_type, ancestor,
                                  account, editAccData->type);
  }

  if (!extra_change_verify(editAccData, change_currency,
                           change_security, change_type))
  {
    gnc_ui_free_field_strings(&strings);
    g_hash_table_destroy(change_currency);
    g_hash_table_destroy(change_security);
    g_hash_table_destroy(change_type);
    return;
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

  make_account_changes(change_currency, change_security, change_type);

  gnc_refresh_main_window();
  gnc_group_ui_refresh(gncGetCurrentGroup());

  gnome_dialog_close(GNOME_DIALOG(editAccData->dialog));

  gnc_ui_free_field_strings(&strings);
  g_hash_table_destroy(change_currency);
  g_hash_table_destroy(change_security);
  g_hash_table_destroy(change_type);
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
    const gchar * notes;

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
