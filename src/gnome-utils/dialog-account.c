/********************************************************************\
 * dialog-account.c -- window for creating and editing accounts for *
 *                     GnuCash                                      *
 * Copyright (C) 2000 Dave Peticolas <dave@krondo.com>              *
 * Copyright (C) 2003,2005 David Hampton <hampton@employees.org>    *
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
\********************************************************************/

#include "config.h"

#include <gnome.h>
#include <glib/gi18n.h>
#include <math.h>
#include <string.h>

#include "Transaction.h"
#include "dialog-account.h"
#include "dialog-commodity.h"
#include "dialog-utils.h"
#include "gnc-amount-edit.h"
#include "gnc-general-select.h"
#include "gnc-commodity.h"
#include "gnc-commodity-edit.h"
#include "gnc-component-manager.h"
#include "gnc-engine.h"
#include "gnc-gui-query.h"
#include "gnc-session.h"
#include "gnc-tree-view-account.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"


#define DIALOG_NEW_ACCOUNT_CM_CLASS "dialog-new-account"
#define DIALOG_EDIT_ACCOUNT_CM_CLASS "dialog-edit-account"
#define GCONF_SECTION "dialogs/account"

typedef enum
{
  NEW_ACCOUNT,
  EDIT_ACCOUNT
} AccountDialogType;


typedef struct _AccountWindow
{
  gboolean modal;
  GtkWidget *dialog;

  AccountDialogType dialog_type;

  GUID    account;
  Account *top_level_account; /* owned by the model */
  Account *created_account;

  GList *subaccount_names;

  GNCAccountType type;

  GtkWidget * notebook;

  GtkWidget * name_entry;
  GtkWidget * description_entry;
  GtkWidget * code_entry;
  GtkTextBuffer * notes_text_buffer;

  GtkWidget * commodity_edit;
  dialog_commodity_mode commodity_mode;
  GtkWidget * account_scu;
  
  GList * valid_types;
  GtkWidget * type_list;
  GtkTreeView * parent_tree;

  GtkWidget * opening_balance_edit;
  GtkWidget * opening_balance_date_edit;
  GtkWidget * opening_balance_page;

  GtkWidget * opening_equity_radio;
  GtkWidget * transfer_account_scroll;
  GtkWidget * transfer_tree;

  GtkWidget * tax_related_button;
  GtkWidget * placeholder_button;

  gint component_id;
} AccountWindow;


/** Static Globals *******************************************************/
static QofLogModule log_module = GNC_MOD_GUI;

static int last_used_account_type = BANK;

static GList *ac_destroy_cb_list = NULL;

/** Declarations *********************************************************/
static void gnc_account_window_set_name (AccountWindow *aw);
static void make_account_changes(GHashTable *change_type);


/** Implementation *******************************************************/

static void
aw_call_destroy_callbacks (Account* acc)
{
  GList *node;
  void (*cb)(Account*);

  for (node = ac_destroy_cb_list; node; node = node->next) {
    cb = node->data;
    (cb)(acc);
  }
}

static Account *
aw_get_account (AccountWindow *aw)
{
  if (!aw)
    return NULL;

  return xaccAccountLookup (&aw->account, gnc_get_current_book ());
}

static void
gnc_account_commodity_from_type (AccountWindow * aw, gboolean update)
{
  dialog_commodity_mode new_mode;

  if ((aw->type == STOCK) || (aw->type == MUTUAL))
    new_mode = DIAG_COMM_NON_CURRENCY;
  else
    new_mode = DIAG_COMM_CURRENCY;

  if (update && (new_mode != aw->commodity_mode)) {
    gnc_general_select_set_selected(GNC_GENERAL_SELECT (aw->commodity_edit),
				    NULL);
  }

  aw->commodity_mode = new_mode;
}

/* Copy the account values to the GUI widgets */
static void
gnc_account_to_ui(AccountWindow *aw)
{
  Account *account;
  gnc_commodity * commodity;
  const char *string;
  gboolean tax_related, placeholder, nonstd_scu;
  gint index;

  ENTER("%p", aw);
  account = aw_get_account (aw);
  if (!account) {
    LEAVE("no account");
    return;
  }

  string = xaccAccountGetName (account);
  if (string == NULL) string = "";
  gtk_entry_set_text(GTK_ENTRY(aw->name_entry), string);

  string = xaccAccountGetDescription (account);
  if (string == NULL) string = "";
  gtk_entry_set_text(GTK_ENTRY(aw->description_entry), string);

  commodity = xaccAccountGetCommodity (account);
  gnc_general_select_set_selected (GNC_GENERAL_SELECT (aw->commodity_edit),
                                    commodity);
  gnc_account_commodity_from_type (aw, FALSE);

  nonstd_scu = xaccAccountGetNonStdSCU (account);
  if (nonstd_scu) {
    index = xaccAccountGetCommoditySCUi(account);
    index = log10(index) + 1;
  } else {
    index = 0;
  }
  gtk_option_menu_set_history(GTK_OPTION_MENU(aw->account_scu), index);

  string = xaccAccountGetCode (account);
  if (string == NULL) string = "";
  gtk_entry_set_text(GTK_ENTRY(aw->code_entry), string);

  string = xaccAccountGetNotes (account);
  if (string == NULL) string = "";

  gtk_text_buffer_set_text (aw->notes_text_buffer, string, strlen(string));

  tax_related = xaccAccountGetTaxRelated (account);
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (aw->tax_related_button),
                                tax_related);

  placeholder = xaccAccountGetPlaceholder (account);
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (aw->placeholder_button),
                                placeholder);

  gtk_tree_view_collapse_all (aw->parent_tree);
  gnc_tree_view_account_set_selected_account (GNC_TREE_VIEW_ACCOUNT(aw->parent_tree), account);
  LEAVE(" ");
}


static gboolean
gnc_account_create_transfer_balance (Account *account,
                                     Account *transfer,
                                     gnc_numeric balance,
                                     time_t date)
{
  Transaction *trans;
  Split *split;

  if (gnc_numeric_zero_p (balance))
    return TRUE;

  g_return_val_if_fail (account != NULL, FALSE);
  g_return_val_if_fail (transfer != NULL, FALSE);

  xaccAccountBeginEdit (account);
  xaccAccountBeginEdit (transfer);

  trans = xaccMallocTransaction (gnc_get_current_book ());

  xaccTransBeginEdit (trans);

  xaccTransSetCurrency (trans, xaccAccountGetCommodity (account));
  xaccTransSetDateSecs (trans, date);
  xaccTransSetDescription (trans, _("Opening Balance"));

  split = xaccMallocSplit (gnc_get_current_book ());

  xaccTransAppendSplit (trans, split);
  xaccAccountInsertSplit (account, split);

  xaccSplitSetAmount (split, balance);
  xaccSplitSetValue (split, balance);

  balance = gnc_numeric_neg (balance);

  split = xaccMallocSplit (gnc_get_current_book ());

  xaccTransAppendSplit (trans, split);
  xaccAccountInsertSplit (transfer, split);

  xaccSplitSetAmount (split, balance);
  xaccSplitSetValue (split, balance);

  xaccTransCommitEdit (trans);
  xaccAccountCommitEdit (transfer);
  xaccAccountCommitEdit (account);

  return TRUE;
}

/* Record the GUI values into the Account structure */
static void
gnc_ui_to_account(AccountWindow *aw)
{
  Account *account;
  gnc_commodity *commodity;
  Account *parent_account;
  const char *old_string;
  const char *string;
  gboolean tax_related, placeholder;
  gnc_numeric balance;
  gboolean use_equity, nonstd;
  time_t date;
  gint index, old_scu, new_scu;
  GtkTextIter start, end;

  account = aw_get_account (aw);
  if (!account) {
    LEAVE("no account");
    return;
  }

  xaccAccountBeginEdit (account);

  if (aw->type != xaccAccountGetType (account))
    xaccAccountSetType (account, aw->type);

  string = gtk_entry_get_text (GTK_ENTRY(aw->name_entry));
  old_string = xaccAccountGetName (account);
  if (safe_strcmp (string, old_string) != 0)
    xaccAccountSetName (account, string);

  string = gtk_entry_get_text (GTK_ENTRY(aw->description_entry));
  old_string = xaccAccountGetDescription (account);
  if (safe_strcmp (string, old_string) != 0)
    xaccAccountSetDescription (account, string);

  commodity = (gnc_commodity *)
    gnc_general_select_get_selected (GNC_GENERAL_SELECT (aw->commodity_edit));
  if (commodity &&
      !gnc_commodity_equiv(commodity, xaccAccountGetCommodity (account))) {
    xaccAccountSetCommodity (account, commodity);
    old_scu = 0;
  } else {
    old_scu = xaccAccountGetCommoditySCU(account);
  }

  index = gnc_option_menu_get_active(aw->account_scu);
  nonstd = (index != 0);
  if (nonstd != xaccAccountGetNonStdSCU(account))
    xaccAccountSetNonStdSCU(account, nonstd);
  new_scu = (nonstd ? pow(10,index-1) : gnc_commodity_get_fraction(commodity));
  if (old_scu != new_scu)
    xaccAccountSetCommoditySCU(account, new_scu);

  string = gtk_entry_get_text (GTK_ENTRY(aw->code_entry));
  old_string = xaccAccountGetCode (account);
  if (safe_strcmp (string, old_string) != 0)
    xaccAccountSetCode (account, string);

  gtk_text_buffer_get_start_iter (aw->notes_text_buffer, &start);
  gtk_text_buffer_get_end_iter (aw->notes_text_buffer, &end);
  string = gtk_text_buffer_get_text (aw->notes_text_buffer, &start, &end, FALSE);
  old_string = xaccAccountGetNotes (account);
  if (safe_strcmp (string, old_string) != 0)
    xaccAccountSetNotes (account, string);

  tax_related =
    gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (aw->tax_related_button));
  xaccAccountSetTaxRelated (account, tax_related);

  placeholder =
    gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (aw->placeholder_button));
  xaccAccountSetPlaceholder (account, placeholder);

  parent_account = gnc_tree_view_account_get_selected_account (GNC_TREE_VIEW_ACCOUNT (aw->parent_tree));
  if (parent_account == aw->top_level_account)
    parent_account = NULL;

  xaccAccountBeginEdit (parent_account);

  if (parent_account != NULL)
  {
    if (parent_account != xaccAccountGetParentAccount (account))
      xaccAccountInsertSubAccount (parent_account, account);
  }
  else
    xaccGroupInsertAccount (gnc_get_current_group (), account);

  xaccAccountCommitEdit (parent_account);
  xaccAccountCommitEdit (account);

  balance = gnc_amount_edit_get_amount
    (GNC_AMOUNT_EDIT (aw->opening_balance_edit));

  if (gnc_numeric_zero_p (balance)) {
    LEAVE("zero balance");
    return;
  }

  if (gnc_reverse_balance (account))
    balance = gnc_numeric_neg (balance);

  date = gnome_date_edit_get_time (
		  GNOME_DATE_EDIT (aw->opening_balance_date_edit));

  use_equity = gtk_toggle_button_get_active
    (GTK_TOGGLE_BUTTON (aw->opening_equity_radio));

  if (use_equity)
  {
    if (!gnc_account_create_opening_balance (account, balance, date,
                                             gnc_get_current_book ()))
    {
      const char *message = _("Could not create opening balance.");
      gnc_error_dialog(aw->dialog, message);
    }
  }
  else
  {
    Account *transfer = NULL;

    transfer = gnc_tree_view_account_get_selected_account (GNC_TREE_VIEW_ACCOUNT (aw->transfer_tree));
    if (!transfer) {
      LEAVE("no transfer account");
      return;
    }

    gnc_account_create_transfer_balance (account, transfer, balance, date);
  }
    LEAVE(" ");
}


static void 
gnc_finish_ok (AccountWindow *aw,
               GHashTable *change_type)
{
  ENTER("aw %p, hash table %p", aw, change_type);
  gnc_suspend_gui_refresh ();

  /* make the account changes */
  make_account_changes (change_type);
  gnc_ui_to_account (aw);

  gnc_resume_gui_refresh ();

  /* do it all again, if needed */
  if (aw->dialog_type == NEW_ACCOUNT && aw->subaccount_names)
  {
    gnc_commodity *commodity;
    Account *parent;
    Account *account;
    GList *node;

    gnc_suspend_gui_refresh ();

    parent = aw_get_account (aw);
    account = xaccMallocAccount (gnc_get_current_book ());
    aw->account = *xaccAccountGetGUID (account);
    aw->type = xaccAccountGetType (parent);

    xaccAccountSetName (account, aw->subaccount_names->data);

    node = aw->subaccount_names;
    aw->subaccount_names = g_list_remove_link (aw->subaccount_names, node);
    g_free (node->data);
    g_list_free_1 (node);

    gnc_account_to_ui (aw);

    gnc_account_window_set_name (aw);

    commodity = xaccAccountGetCommodity (parent);
    gnc_general_select_set_selected (GNC_GENERAL_SELECT (aw->commodity_edit),
                                      commodity);
    gnc_account_commodity_from_type (aw, FALSE);

    /*gnc_account_tree_select_account (GNC_ACCOUNT_TREE(aw->parent_tree),
                                     parent, TRUE);*/

    gnc_resume_gui_refresh ();
    LEAVE("1");
    return;
  }

  /* save for posterity */
  aw->created_account = aw_get_account (aw);

  /* so it doesn't get freed on close */
  aw->account = *xaccGUIDNULL ();

  gnc_close_gui_component (aw->component_id);
  LEAVE("2");
}


/* Record all of the children of the given account as needing their
 * type changed to the one specified. */
static void
gnc_edit_change_account_types(GHashTable *change_type, Account *account,
                              Account *except, GNCAccountType type)
{
  AccountGroup *children;
  GList *list;
  GList *node;

  if ((change_type == NULL) || (account == NULL))
    return;

  if (account == except)
    return;

  g_hash_table_insert(change_type, account, GINT_TO_POINTER(type));

  children = xaccAccountGetChildren(account);
  if (children == NULL)
    return;

  list = xaccGroupGetAccountList (children);

  for (node= list; node; node = node->next)
  {
    account = node->data;
    gnc_edit_change_account_types(change_type, account, except, type);
  }
}


/* helper function to perform changes to accounts */
static void
change_func (gpointer key, gpointer value, gpointer unused)
{
  Account *account = key;
  int type;
 
  if (account == NULL)
    return;

  xaccAccountBeginEdit(account);

  type = GPOINTER_TO_INT(value);

  if (type == xaccAccountGetType(account))
    return;

  /* Just refreshing won't work. */
  aw_call_destroy_callbacks (account);

  xaccAccountSetType(account, type);

  xaccAccountCommitEdit(account);
}


/* Perform the changes to accounts dictated by the hash tables */
static void
make_account_changes(GHashTable *change_type)
{
  if (change_type != NULL)
    g_hash_table_foreach(change_type, change_func, NULL);
}


typedef struct
{
  Account *account;
  GtkCList *list;
  guint count;
} FillStruct;

static void
fill_helper(gpointer key, gpointer value, gpointer data)
{
  Account *account = key;
  FillStruct *fs = data;
  gchar *full_name;
  gchar *account_field_name;
  gchar *account_field_value;
  gchar *value_str;

  if (fs == NULL) return;
  if (fs->account == account) return;

  full_name = xaccAccountGetFullName(account, gnc_get_account_separator());
  if(!full_name)
    full_name = g_strdup("");

  account_field_name = g_strdup("Type");
  if (!account_field_name)
    account_field_name = g_strdup("");

  account_field_value =
    g_strdup (xaccAccountGetTypeStr(xaccAccountGetType(account)));
  if (!account_field_value)
    account_field_value = g_strdup("");

  value_str = g_strdup(xaccAccountGetTypeStr(GPOINTER_TO_INT(value)));

  {  
    gchar *strings[5];

    strings[0] = full_name;
    strings[1] = account_field_name;
    strings[2] = account_field_value;
    strings[3] = value_str;
    strings[4] = NULL; 

    gtk_clist_append(fs->list, strings);
  }

  g_free(full_name);
  g_free(account_field_name);
  g_free(account_field_value);
  g_free(value_str);
  fs->count++;
}

static guint
fill_list(Account *account, GtkCList *list,
          GHashTable *change)
{
  FillStruct fs;

  if (change == NULL)
    return 0;

  fs.account = account;
  fs.list = list;
  fs.count = 0;

  g_hash_table_foreach(change, fill_helper, &fs);

  return fs.count;
}


/* Present a dialog of proposed account changes for the user's ok */
static gboolean
extra_change_verify (AccountWindow *aw,
                     GHashTable *change_type)
{
  Account *account;
  GtkCList *list;
  gchar *titles[5];
  gboolean result;
  guint size;

  if (aw == NULL)
    return FALSE;

  account = aw_get_account (aw);
  if (!account)
    return FALSE;

  titles[0] = _("Account");
  titles[1] = _("Field");
  titles[2] = _("Old Value");
  titles[3] = _("New Value");
  titles[4] = NULL;

  list = GTK_CLIST(gtk_clist_new_with_titles(4, titles));

  size = 0;
  size += fill_list(account, list, change_type);

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

    dialog = gtk_dialog_new_with_buttons (_("Verify Changes"),
		    			  GTK_WINDOW(aw->dialog),
					  GTK_DIALOG_DESTROY_WITH_PARENT |
					  GTK_DIALOG_MODAL,
					  GTK_STOCK_OK, GTK_RESPONSE_OK,
					  GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
					  NULL);

    gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    gtk_window_set_default_size (GTK_WINDOW (dialog), 0, 300);

    vbox = GTK_DIALOG (dialog)->vbox;

    label = gtk_label_new(_("The following changes must be made. Continue?"));
    gtk_box_pack_start(GTK_BOX(vbox), label, FALSE, FALSE, 0);

    frame = gtk_frame_new(NULL);
    gtk_box_pack_start(GTK_BOX(vbox), frame, TRUE, TRUE, 0);

    scroll = gtk_scrolled_window_new(NULL, NULL);
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scroll),
                                   GTK_POLICY_NEVER, 
                                   GTK_POLICY_AUTOMATIC);

    gtk_container_add(GTK_CONTAINER(frame), scroll);
    gtk_container_set_border_width(GTK_CONTAINER(scroll), 5);
    gtk_container_add(GTK_CONTAINER(scroll), GTK_WIDGET(list));

    gtk_widget_show_all(vbox);

    result = (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_OK);

    gtk_widget_destroy(dialog);
  }

  return result;
}


static gboolean
gnc_filter_parent_accounts (Account *account, gpointer data)
{
  AccountWindow *aw = data;
  Account *aw_account = aw_get_account (aw);

  if (account == NULL)
    return FALSE;

  if (aw_account == NULL)
    return FALSE;

  if (account == aw->top_level_account)
    return TRUE;

  if (account == aw_account)
    return FALSE;

  if (xaccAccountHasAncestor(account, aw_account))
    return FALSE;

  return TRUE;
}


static void
gnc_edit_account_ok(AccountWindow *aw)
{
  GHashTable *change_type;

  gboolean change_children;
  gboolean has_children;
  gboolean change_all;

  Account *new_parent;
  Account *account;
  AccountGroup *children;

  GNCAccountType current_type;

  const char *name;
  gnc_commodity * commodity;

  /* check for valid name */
  ENTER("aw %p", aw);
  name = gtk_entry_get_text(GTK_ENTRY(aw->name_entry));
  if (safe_strcmp(name, "") == 0)
  {
    const char *message = _("The account must be given a name.");
    gnc_error_dialog(aw->dialog, message);
    LEAVE(" ");
    return;
  }

  /* check for valid type */
  if (aw->type == BAD_TYPE)
  {
    const char *message = _("You must select an account type.");
    gnc_error_dialog(aw->dialog, message);
    LEAVE(" ");
    return;
  }

  new_parent = gnc_tree_view_account_get_selected_account (GNC_TREE_VIEW_ACCOUNT (aw->parent_tree));

  /* Parent check, probably not needed, but be safe */
  if (!gnc_filter_parent_accounts(new_parent, aw))
  {
    const char *message = _("You must choose a valid parent account.");
    gnc_error_dialog(aw->dialog, message);
    LEAVE(" ");
    return;
  }

  commodity = (gnc_commodity *)
    gnc_general_select_get_selected (GNC_GENERAL_SELECT (aw->commodity_edit));

  if (!commodity)
  {
    const char *message = _("You must choose a commodity.");
    gnc_error_dialog(aw->dialog, message);
    LEAVE(" ");
    return;
  }

  account = aw_get_account (aw);
  if (!account) {
    LEAVE(" ");
    return;
  }

  change_type = g_hash_table_new (NULL, NULL);

  children = xaccAccountGetChildren(account);
  if (children == NULL)
    has_children = FALSE;
  else if (xaccGroupGetNumAccounts(children) == 0)
    has_children = FALSE;
  else
    has_children = TRUE;

  current_type = xaccAccountGetType(account);

  /* If the account has children and the new type isn't compatible
   * with the old type, the children's types must be changed. */
  change_children = (has_children &&
                     !xaccAccountTypesCompatible(current_type, aw->type));

  /* If the new parent's type is not compatible with the new type,
   * the whole sub-tree containing the account must be re-typed. */
  if (new_parent != aw->top_level_account)
  {
    int parent_type;

    parent_type = xaccAccountGetType(new_parent);

    if (!xaccAccountTypesCompatible(parent_type, aw->type))
      change_all = TRUE;
    else
      change_all = FALSE;
  }
  else
    change_all = FALSE;

  if (change_children)
    gnc_edit_change_account_types(change_type, account, NULL, aw->type);

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

    gnc_edit_change_account_types(change_type, ancestor, account, aw->type);
  }

  if (!extra_change_verify(aw, change_type))
  {
    g_hash_table_destroy(change_type);
    LEAVE(" ");
    return;
  }

  if (current_type != aw->type)
    /* Just refreshing won't work. */
    aw_call_destroy_callbacks (account);

  gnc_finish_ok (aw, change_type);

  g_hash_table_destroy (change_type);
    LEAVE(" ");
}


static void
gnc_new_account_ok (AccountWindow *aw)
{
  const gnc_commodity * commodity;
  Account *parent_account;
  gnc_numeric balance;
  const gchar *name;

  /* check for valid name */
  ENTER("aw %p", aw);
  name = gtk_entry_get_text(GTK_ENTRY(aw->name_entry));
  if (safe_strcmp(name, "") == 0)
  {
    const char *message = _("The account must be given a name.");
    gnc_error_dialog(aw->dialog, message);
    LEAVE(" ");
    return;
  }

  parent_account = gnc_tree_view_account_get_selected_account (GNC_TREE_VIEW_ACCOUNT (aw->parent_tree));

  if (parent_account == aw->top_level_account)
    parent_account = NULL;

  /* check for a duplicate name */
  {
    Account *account;
    AccountGroup *group;
    char separator;

    group = gnc_get_current_group ();

    separator = gnc_get_account_separator();

    if (parent_account == NULL)
      account = xaccGetAccountFromFullName(group, name, separator);
    else
    {
      char *fullname_parent;
      char *fullname;
      char sep_string[2];

      sep_string[0] = separator;
      sep_string[1] = '\0';

      fullname_parent = xaccAccountGetFullName(parent_account, separator);
      fullname = g_strconcat(fullname_parent, sep_string, name, NULL);

      account = xaccGetAccountFromFullName(group, fullname, separator);

      g_free(fullname_parent);
      g_free(fullname);
    }

    if (account != NULL)
    {
      const char *message = _("There is already an account with that name.");
      gnc_error_dialog(aw->dialog, message);
      LEAVE(" ");
      return;
    }
  }

  /* check for valid type */
  if (aw->type == BAD_TYPE)
  {
    const char *message = _("You must select an account type.");
    gnc_error_dialog(aw->dialog, message);
    LEAVE(" ");
    return;
  }

  /* check for commodity */
  commodity = (gnc_commodity *)
    gnc_general_select_get_selected (GNC_GENERAL_SELECT (aw->commodity_edit));

  if (!commodity)
  {
    const char *message = _("You must choose a commodity.");
    gnc_error_dialog(aw->dialog, message);
    LEAVE(" ");
    return;
  }

  if (!gnc_amount_edit_evaluate (GNC_AMOUNT_EDIT (aw->opening_balance_edit)))
  {
    const char *message = _("You must enter a valid opening balance "
                            "or leave it blank.");
    gnc_error_dialog(aw->dialog, message);
    LEAVE(" ");
    return;
  }

  balance = gnc_amount_edit_get_amount
    (GNC_AMOUNT_EDIT (aw->opening_balance_edit));

  if (!gnc_numeric_zero_p (balance))
  {
    gboolean use_equity;

    use_equity = gtk_toggle_button_get_active
      (GTK_TOGGLE_BUTTON (aw->opening_equity_radio));

    if (!use_equity)
    {
      Account *transfer = NULL;

      transfer = gnc_tree_view_account_get_selected_account (GNC_TREE_VIEW_ACCOUNT (aw->transfer_tree));
      if (!transfer)
      {
        const char *message = _("You must select a transfer account or choose"
                                " the opening balances equity account.");
        gnc_error_dialog(aw->dialog, message);
	LEAVE(" ");
        return;
      }
    }
  }

  gnc_finish_ok (aw, NULL);
  LEAVE(" ");
}

static void
gnc_account_window_response_cb (GtkDialog *dialog,
	       			gint response,
			       	gpointer data)
{
	AccountWindow *aw = data; 

	ENTER("dialog %p, response %d, aw %p", dialog, response, aw);
	switch (response) {
		case GTK_RESPONSE_OK:
			switch (aw->dialog_type) {
				case NEW_ACCOUNT:
					DEBUG("new acct dialog, OK");
					gnc_new_account_ok (aw);
					break;
				case EDIT_ACCOUNT:
					DEBUG("edit acct dialog, OK");
					gnc_edit_account_ok (aw);
					break;
				default:
					g_assert_not_reached ();
					return;
			}
			break;
		case GTK_RESPONSE_HELP:
			switch (aw->dialog_type) {
				case NEW_ACCOUNT:
					DEBUG("new acct dialog, HELP");
					gnc_gnome_help(HF_USAGE, HL_ACC);
					break;
				case EDIT_ACCOUNT:
					DEBUG("edit acct dialog, HELP");
					gnc_gnome_help(HF_USAGE, HL_ACCEDIT);
					break;
				default:
					g_assert_not_reached ();
					return;
			}
			break;
		case GTK_RESPONSE_CANCEL:
		default:
			DEBUG("CANCEL");
			gnc_close_gui_component (aw->component_id);
			break;
	}
	LEAVE(" ");
}

static void
gnc_account_window_destroy_cb (GtkObject *object, gpointer data)
{
  AccountWindow *aw = data;
  Account *account;

  ENTER("object %p, aw %p", object, aw);
  account = aw_get_account (aw);

  gnc_suspend_gui_refresh ();

  switch (aw->dialog_type)
  {
    case NEW_ACCOUNT:
      if (account != NULL)
      {
        xaccAccountBeginEdit (account);
        xaccAccountDestroy (account);
        aw->account = *xaccGUIDNULL ();
      }

      DEBUG ("account add window destroyed\n");
      break;

    case EDIT_ACCOUNT:
      break;

    default:
      PERR ("unexpected dialog type\n");
      gnc_resume_gui_refresh ();
      LEAVE(" ");
      return;
  }

  gnc_unregister_gui_component (aw->component_id);

  aw->top_level_account = NULL;

  gnc_resume_gui_refresh ();

  if (aw->subaccount_names)
  {
    GList *node;
    for (node = aw->subaccount_names; node; node = node->next)
      g_free (node->data);
    g_list_free (aw->subaccount_names);
    aw->subaccount_names = NULL;
  }

  g_list_free (aw->valid_types);
  g_free (aw);
  LEAVE(" ");
}


static void 
gnc_type_list_select_cb(GtkCList * type_list, gint row, gint column,
                        GdkEventButton * event, gpointer data)
{
  AccountWindow * aw = data;
  gboolean sensitive;

  if (aw == NULL)
    return;

  if (!gtk_clist_get_selectable(type_list, row))
  {
    gtk_clist_unselect_row(type_list, row, 0);
    return;
  }

  aw->type = (GNCAccountType)gtk_clist_get_row_data(type_list, row);

  last_used_account_type = aw->type;

  sensitive = (aw->type != EQUITY &&
               aw->type != CURRENCY &&
               aw->type != STOCK &&
               aw->type != MUTUAL);

  gnc_account_commodity_from_type (aw, TRUE);
  gtk_widget_set_sensitive(aw->opening_balance_page, sensitive);
  if (!sensitive)
  {
    gtk_notebook_set_current_page (GTK_NOTEBOOK (aw->notebook), 0);
    gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (aw->opening_balance_edit),
                                gnc_numeric_zero ());
  }
}


static void 
gnc_type_list_unselect_cb(GtkCList * type_list, gint row, gint column,
                          GdkEventButton * event, gpointer data)
{
  AccountWindow * aw = data;

  aw->type = BAD_TYPE;
}


static void
gnc_account_list_fill(GtkCList *type_list, GList *types)
{
  gint row, acct_type;
  gchar *text[2] = { NULL, NULL };

  gtk_clist_freeze(type_list);
  gtk_clist_clear(type_list);

  if (types == NULL)
  {
    for (acct_type = 0; acct_type < NUM_ACCOUNT_TYPES; acct_type++) 
    {
      text[0] = (gchar *) xaccAccountGetTypeStr(acct_type);
      row = gtk_clist_append(type_list, text);
      gtk_clist_set_row_data(type_list, row, GINT_TO_POINTER(acct_type));
    }
  }
  else
  {
    for ( ; types != NULL; types = types->next )
    {
      text[0] = (gchar *) xaccAccountGetTypeStr
	((GNCAccountType) (types->data));
      row = gtk_clist_append (type_list, text);
      gtk_clist_set_row_data(type_list, row, (gpointer)types->data);
    }
  }
  gtk_clist_thaw(type_list);
}

static GNCAccountType
gnc_account_choose_new_acct_type (AccountWindow *aw)
{
  
  if (aw->valid_types == NULL)
    return last_used_account_type;

  if (g_list_index (aw->valid_types, GINT_TO_POINTER(last_used_account_type)) != -1)
    return last_used_account_type;

  return ((GNCAccountType)(aw->valid_types->data));
}

static void
gnc_account_type_list_create(AccountWindow *aw)
{
  int row;

  gnc_account_list_fill(GTK_CLIST(aw->type_list), aw->valid_types);

  gtk_clist_columns_autosize(GTK_CLIST(aw->type_list));

  gtk_clist_sort(GTK_CLIST(aw->type_list));

  g_signal_connect(GTK_OBJECT(aw->type_list), "select-row",
		   G_CALLBACK(gnc_type_list_select_cb), aw);

  g_signal_connect(GTK_OBJECT(aw->type_list), "unselect-row",
		   G_CALLBACK(gnc_type_list_unselect_cb), aw);

  switch (aw->dialog_type)
  {
    case NEW_ACCOUNT:
      aw->type = gnc_account_choose_new_acct_type (aw);
      break;
    case EDIT_ACCOUNT:
      aw->type = xaccAccountGetType (aw_get_account (aw));
      break;
  }

  row = gtk_clist_find_row_from_data(GTK_CLIST(aw->type_list),
				     (gpointer)aw->type);
  gtk_clist_select_row(GTK_CLIST(aw->type_list), row, 0);
  gtk_clist_moveto(GTK_CLIST(aw->type_list), row, 0, 0.5, 0);
}

static void
gnc_account_name_changed_cb(GtkWidget *widget, gpointer data)
{
  AccountWindow *aw = data;

  gnc_account_window_set_name (aw);
}

static void
commodity_changed_cb (GNCGeneralSelect *gsl, gpointer data)
{
  AccountWindow *aw = data;
  gnc_commodity *currency;
  GtkTreeSelection *selection;

  currency = (gnc_commodity *) gnc_general_select_get_selected (gsl);
  if (!currency)
    return;

  gnc_amount_edit_set_fraction (GNC_AMOUNT_EDIT (aw->opening_balance_edit),
                                gnc_commodity_get_fraction (currency));
  gnc_amount_edit_set_print_info (GNC_AMOUNT_EDIT (aw->opening_balance_edit),
                                  gnc_commodity_print_info (currency, FALSE));

  selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (aw->transfer_tree));
  gtk_tree_selection_unselect_all (selection);
}

static gboolean
account_commodity_filter (GtkTreeSelection *selection,
			  GtkTreeModel *unused_model,
			  GtkTreePath *s_path,
			  gboolean path_currently_selected,
			  gpointer user_data)
{
  gnc_commodity *commodity;
  AccountWindow *aw;
  Account *account;

  g_return_val_if_fail (GTK_IS_TREE_SELECTION (selection), FALSE);

  aw = user_data;

  if (path_currently_selected) {
    /* already selected, don't waste time. */
    return TRUE;
  }

  account = gnc_tree_view_account_get_account_from_path (GNC_TREE_VIEW_ACCOUNT (aw->transfer_tree), s_path);
  if (!account) {
    return FALSE;
  }

  commodity = (gnc_commodity *)
    gnc_general_select_get_selected (GNC_GENERAL_SELECT (aw->commodity_edit));

  return gnc_commodity_equiv (xaccAccountGetCommodity (account), commodity);
}

static void
opening_equity_cb (GtkWidget *w, gpointer data)
{
  AccountWindow *aw = data;
  gboolean use_equity;

  use_equity = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (w));

  gtk_widget_set_sensitive (aw->transfer_account_scroll, !use_equity);
}

/********************************************************************\
 * gnc_account_window_create                                        *
 *   creates a window to create a new account.                      *
 *                                                                  * 
 * Args:   aw - the information structure for this window           *
 * Return: the created window                                       *
 \*******************************************************************/
static void
gnc_account_window_create(AccountWindow *aw)
{
  GtkDialog *awd;
  GtkWidget *amount;
  GObject *awo;
  GtkWidget *box;
  GtkWidget *label;
  GladeXML  *xml;
  GtkTreeSelection *selection;

  ENTER("aw %p, modal %d", aw, aw->modal);
  xml = gnc_glade_xml_new ("account.glade", "Account Dialog");

  aw->dialog = glade_xml_get_widget (xml, "Account Dialog");
  awo = G_OBJECT (aw->dialog);
  awd = GTK_DIALOG (awo);

  g_object_set_data (awo, "dialog_info", aw);

  g_signal_connect (awo, "destroy",
                    G_CALLBACK (gnc_account_window_destroy_cb), aw);

  if (!aw->modal)
    g_signal_connect (awo, "response",
		      G_CALLBACK (gnc_account_window_response_cb), aw);
  else 
    gtk_window_set_modal (GTK_WINDOW (aw->dialog), TRUE);

  aw->notebook = glade_xml_get_widget (xml, "account_notebook");

  aw->name_entry = glade_xml_get_widget (xml, "name_entry");
  g_signal_connect (G_OBJECT (aw->name_entry), "changed",
		    G_CALLBACK (gnc_account_name_changed_cb), aw);

  aw->description_entry = glade_xml_get_widget (xml, "description_entry");
  aw->code_entry =        glade_xml_get_widget (xml, "code_entry");
  aw->notes_text_buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (glade_xml_get_widget (xml, "notes_text")));

  box = glade_xml_get_widget (xml, "commodity_hbox");
  aw->commodity_edit = gnc_general_select_new (GNC_GENERAL_SELECT_TYPE_SELECT,
					       gnc_commodity_edit_get_string,
					       gnc_commodity_edit_new_select,
					       &aw->commodity_mode);
  gtk_box_pack_start(GTK_BOX(box), aw->commodity_edit, TRUE, TRUE, 0);
  gtk_widget_show (aw->commodity_edit);

  label = glade_xml_get_widget (xml, "commodity_label");
  gnc_general_select_make_mnemonic_target (GNC_GENERAL_SELECT(aw->commodity_edit), label);

  g_signal_connect (G_OBJECT (aw->commodity_edit), "changed",
                    G_CALLBACK (commodity_changed_cb), aw);

  aw->account_scu = glade_xml_get_widget (xml, "account_scu");
  gnc_option_menu_init(aw->account_scu);

  box = glade_xml_get_widget (xml, "parent_scroll");

  //  group = gnc_book_get_group (gnc_get_current_book ());
  aw->parent_tree = gnc_tree_view_account_new(TRUE);
  gtk_container_add(GTK_CONTAINER(box), GTK_WIDGET(aw->parent_tree));
  gtk_widget_show(GTK_WIDGET(aw->parent_tree));
  aw->top_level_account =
    gnc_tree_view_account_get_top_level (GNC_TREE_VIEW_ACCOUNT(aw->parent_tree));

  aw->tax_related_button = glade_xml_get_widget (xml, "tax_related_button");
  aw->placeholder_button = glade_xml_get_widget (xml, "placeholder_button");

  box = glade_xml_get_widget (xml, "opening_balance_box");
  amount = gnc_amount_edit_new ();
  aw->opening_balance_edit = amount;
  gtk_box_pack_start(GTK_BOX(box), amount, TRUE, TRUE, 0);
  gnc_amount_edit_set_evaluate_on_enter (GNC_AMOUNT_EDIT (amount), TRUE);
  gtk_widget_show (amount);

  label = glade_xml_get_widget (xml, "balance_label");
  gtk_label_set_mnemonic_widget (GTK_LABEL(label), amount);

  box = glade_xml_get_widget (xml, "opening_balance_date_box");
  aw->opening_balance_date_edit = glade_xml_get_widget (xml, "opening_balance_date_edit");

  aw->opening_balance_page =
    gtk_notebook_get_nth_page (GTK_NOTEBOOK (aw->notebook), 1);

  aw->opening_equity_radio = glade_xml_get_widget (xml,
                                                   "opening_equity_radio");
  g_signal_connect (G_OBJECT (aw->opening_equity_radio), "toggled",
                    G_CALLBACK (opening_equity_cb), aw);

  box = glade_xml_get_widget (xml, "transfer_account_scroll");
  aw->transfer_account_scroll = box;

  aw->transfer_tree = GTK_WIDGET(gnc_tree_view_account_new(FALSE));
  selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(aw->transfer_tree));
  gtk_tree_selection_set_select_function(selection, account_commodity_filter, aw, NULL);

  gtk_container_add(GTK_CONTAINER(box), GTK_WIDGET(aw->transfer_tree));
  gtk_widget_show (GTK_WIDGET(aw->transfer_tree));

  label = glade_xml_get_widget (xml, "parent_label");
  gtk_label_set_mnemonic_widget (GTK_LABEL(label), aw->transfer_tree);

  /* This goes at the end so the select callback has good data. */
  aw->type_list = glade_xml_get_widget (xml, "type_list");
  gnc_account_type_list_create (aw);

  gnc_restore_window_size (GCONF_SECTION, GTK_WINDOW(aw->dialog));

  gtk_widget_grab_focus(GTK_WIDGET(aw->name_entry));
  LEAVE(" ");
}


static char *
get_ui_fullname (AccountWindow *aw)
{
  Account *parent_account;
  char *fullname;
  const gchar *name;

  name = gtk_entry_get_text (GTK_ENTRY(aw->name_entry));
  if (!name || *name == '\0')
    name = _("<No name>");

  parent_account = NULL;

  parent_account = gnc_tree_view_account_get_selected_account (GNC_TREE_VIEW_ACCOUNT (aw->parent_tree));
  if (parent_account == aw->top_level_account)
    parent_account = NULL;

  if (parent_account)
  {
    char *parent_name;
    char sep_string[2];

    parent_name = xaccAccountGetFullName (parent_account,
                                          gnc_get_account_separator());

    sep_string[0] = gnc_get_account_separator ();
    sep_string[1] = '\0';

    fullname = g_strconcat (parent_name, sep_string, name, NULL);

    g_free (parent_name);
  }
  else 
    fullname = g_strdup (name);

  return fullname;
}

static void
gnc_account_window_set_name (AccountWindow *aw)
{
  char *fullname;
  char *title;

  if (!aw || !aw->parent_tree)
    return;

  fullname = get_ui_fullname (aw);

  if (aw->dialog_type == EDIT_ACCOUNT)
    title = g_strconcat(_("Edit Account"), " - ", fullname, NULL);
  else if (g_list_length (aw->subaccount_names) > 0)
  {
    const char *format = _("(%d) New Accounts");
    char *prefix;

    prefix = g_strdup_printf (format,
                              g_list_length (aw->subaccount_names) + 1);

    title = g_strconcat (prefix, " - ", fullname, " ...", NULL);

    g_free (prefix);
  }
  else
    title = g_strconcat (_("New Account"), " - ", fullname, NULL);

  gtk_window_set_title (GTK_WINDOW(aw->dialog), title);

  g_free (fullname);
  g_free (title);
}


static void
close_handler (gpointer user_data)
{
  AccountWindow *aw = user_data;

  ENTER("aw %p, modal %d", aw, aw->modal);
  gnc_save_window_size (GCONF_SECTION, GTK_WINDOW(aw->dialog));

  gtk_widget_destroy (GTK_WIDGET (aw->dialog));
  LEAVE(" ");
}


/********************************************************************\
 * gnc_ui_refresh_account_window                                    *
 *   refreshes the edit window                                      *
 *                                                                  *
 * Args:   aw - the account window to refresh                       *
 * Return: none                                                     *
\********************************************************************/
static void
gnc_ui_refresh_account_window (AccountWindow *aw)
{
  if (aw == NULL)
    return;

/*  gnc_account_tree_refresh (GNC_ACCOUNT_TREE(aw->parent_tree));*/

  gnc_account_window_set_name (aw);
}


static void
refresh_handler (GHashTable *changes, gpointer user_data)
{
  AccountWindow *aw = user_data;
  const EventInfo *info;
  Account *account;

  account = aw_get_account (aw);
  if (!account)
  {
    gnc_close_gui_component (aw->component_id);
    return;
  }

  if (changes)
  {
    info = gnc_gui_get_entity_events (changes, &aw->account);
    if (info && (info->event_mask & GNC_EVENT_DESTROY))
    {
      gnc_close_gui_component (aw->component_id);
      return;
    }
  }

  gnc_ui_refresh_account_window (aw);
}


static AccountWindow *
gnc_ui_new_account_window_internal (Account *base_account,
                                    GList *subaccount_names,
				    GList *valid_types,
				    gnc_commodity * default_commodity,
				    gboolean modal)
{
  gnc_commodity *commodity;
  AccountWindow *aw;
  Account *account;

  aw = g_new0 (AccountWindow, 1);

  aw->modal = modal;
  aw->dialog_type = NEW_ACCOUNT;
  aw->valid_types = g_list_copy (valid_types);

  account = xaccMallocAccount (gnc_get_current_book ());
  aw->account = *xaccAccountGetGUID (account);

  if (base_account)
    aw->type = xaccAccountGetType (base_account);
  else
    aw->type = last_used_account_type;

  gnc_suspend_gui_refresh ();

  if (subaccount_names)
  {
    GList *node;

    xaccAccountSetName (account, subaccount_names->data);

    aw->subaccount_names = g_list_copy (subaccount_names->next);
    for (node = aw->subaccount_names; node; node = node->next)
      node->data = g_strdup (node->data);
  }

  gnc_account_window_create (aw);
  gnc_account_to_ui (aw);

  gnc_resume_gui_refresh ();

  if (default_commodity != NULL) {
    commodity = default_commodity;
  } else if ((aw->type != STOCK) && (aw->type != MUTUAL)) {
    commodity = gnc_default_currency ();
  } else {
    commodity = NULL;
  }
  gnc_general_select_set_selected (GNC_GENERAL_SELECT (aw->commodity_edit),
                                    commodity);
  gnc_account_commodity_from_type (aw, FALSE);

  gtk_widget_show (aw->dialog);

  if (base_account == NULL) {
	  base_account = aw->top_level_account;
  }

  gtk_tree_view_collapse_all (aw->parent_tree);
  gnc_tree_view_account_set_selected_account (GNC_TREE_VIEW_ACCOUNT (aw->parent_tree),
					      base_account);

  gnc_window_adjust_for_screen (GTK_WINDOW(aw->dialog));

  gnc_account_window_set_name (aw);

  aw->component_id = gnc_register_gui_component (DIALOG_NEW_ACCOUNT_CM_CLASS,
                                                 refresh_handler,
                                                 modal ? NULL : close_handler,
						 aw);

  gnc_gui_component_set_session (aw->component_id, gnc_get_current_session());
  gnc_gui_component_watch_entity_type (aw->component_id,
                                       GNC_ID_ACCOUNT,
                                       GNC_EVENT_MODIFY | GNC_EVENT_DESTROY);
  return aw;
}


static GList *
gnc_split_account_name (const char *in_name, Account **base_account)
{
  AccountGroup *group;
  GList *names;
  char separator;
  char *name;

  names = NULL;
  name = g_strdup (in_name);
  *base_account = NULL;
  group = gnc_get_current_group ();

  separator = gnc_get_account_separator ();

  while (name && *name != '\0')
  {
    Account *account;
    char *p;

    account = xaccGetAccountFromFullName (group, name, separator);
    if (account)
    {
      *base_account = account;
      break;
    }

    p = strrchr (name, separator);
    if (p)
    {
      *p++ = '\0';

      if (*p == '\0')
      {
        GList *node;
        for (node = names; node; node = node->next)
          g_free (node->data);
        g_list_free (names);
        return NULL;
      }

      names = g_list_prepend (names, g_strdup (p));
    }
    else
    {
      names = g_list_prepend (names, g_strdup (name));
      break;
    }
  }

  g_free (name);

  return names;
}


/************************************************************
 *              Entry points for a Modal Dialog             *
 ************************************************************/

Account *
gnc_ui_new_accounts_from_name_window (const char *name)
{
  return  gnc_ui_new_accounts_from_name_with_defaults (name, NULL, NULL, NULL);
}

Account *
gnc_ui_new_accounts_from_name_window_with_types (const char *name,
						 GList *valid_types)
{
  return gnc_ui_new_accounts_from_name_with_defaults(name, valid_types, NULL, NULL);
}

Account * gnc_ui_new_accounts_from_name_with_defaults (const char *name,
						       GList *valid_types,
						       gnc_commodity * default_commodity,
						       Account * parent)
{
  AccountWindow *aw;
  Account *base_account;
  Account *created_account = NULL;
  GList * subaccount_names;
  GList * node;
  gint response;

  ENTER("name %s, valid %p, commodity %p, account %p",
	name, valid_types, default_commodity, parent);
  if (!name || *name == '\0')
  {
    subaccount_names = NULL;
    base_account = NULL;
  }
  else
    subaccount_names = gnc_split_account_name (name, &base_account);

  if (parent != NULL)
    {
      base_account=parent;
    }
  aw = gnc_ui_new_account_window_internal (base_account, subaccount_names, 
					   valid_types, default_commodity,
					   TRUE);

  for (node = subaccount_names; node; node = node->next)
    g_free (node->data);
  g_list_free (subaccount_names);

  do {
    response = gtk_dialog_run (GTK_DIALOG(aw->dialog));

    /* This can destroy the dialog */
    gnc_account_window_response_cb (GTK_DIALOG(aw->dialog), response, (gpointer)aw);

    if (response == GTK_RESPONSE_OK)
      created_account = aw->created_account;
  } while ((response == GTK_RESPONSE_HELP) || (response == GNC_RESPONSE_NEW));

  close_handler(aw);
  LEAVE("created %s (%p)", xaccAccountGetName(created_account), created_account);
  return created_account;
}

/************************************************************
 *            Entry points for a non-Modal Dialog           *
 ************************************************************/

static gboolean
find_by_account (gpointer find_data, gpointer user_data)
{
  Account *account = find_data;
  AccountWindow *aw = user_data;

  if (!aw)
    return FALSE;

  return guid_equal (&aw->account, xaccAccountGetGUID (account));
}

/*
 * opens up a window to edit an account
 * 
 * Args:   account - the account to edit
 * Return: EditAccountWindow object
 */
void
gnc_ui_edit_account_window(Account *account)
{
  AccountWindow * aw;
  Account *parent;

  if (account == NULL)
    return;

  aw = gnc_find_first_gui_component (DIALOG_EDIT_ACCOUNT_CM_CLASS,
                                     find_by_account, account);
  if (aw) {
    gtk_window_present(GTK_WINDOW(aw->dialog));
    return;
  }

  aw = g_new0 (AccountWindow, 1);

  aw->modal = FALSE;
  aw->dialog_type = EDIT_ACCOUNT;
  aw->account = *xaccAccountGetGUID (account);
  aw->subaccount_names = NULL;

  gnc_suspend_gui_refresh ();

  gnc_account_window_create (aw);
  gnc_account_to_ui (aw);

  gnc_resume_gui_refresh ();

  gtk_widget_show_all (aw->dialog);
  gtk_widget_hide (aw->opening_balance_page);

  parent = xaccAccountGetParentAccount (account);
  if (parent == NULL)
    parent = aw->top_level_account;

  gnc_tree_view_account_set_selected_account (GNC_TREE_VIEW_ACCOUNT(aw->parent_tree), parent);

  gnc_account_window_set_name (aw);

  gnc_window_adjust_for_screen(GTK_WINDOW(aw->dialog));

  aw->component_id = gnc_register_gui_component (DIALOG_EDIT_ACCOUNT_CM_CLASS,
                                                 refresh_handler,
                                                 close_handler, aw);

  gnc_gui_component_set_session (aw->component_id, gnc_get_current_session());
  gnc_gui_component_watch_entity_type (aw->component_id,
                                       GNC_ID_ACCOUNT,
                                       GNC_EVENT_MODIFY | GNC_EVENT_DESTROY);

  gtk_window_present(GTK_WINDOW(aw->dialog));
}


/*
 * opens up a window to create a new account
 * 
 * Args:   group - not used
 */
void
gnc_ui_new_account_window (AccountGroup *this_is_not_used) 
{
  /* FIXME get_current_account went away. */
  gnc_ui_new_account_window_internal (NULL, NULL, NULL, NULL, FALSE);
}

/*
 * opens up a window to create a new account
 * 
 * Args:   group - not used
 *        parent - The initial parent for the new account
 */
void
gnc_ui_new_account_window_with_default(AccountGroup *this_is_not_used,
                                       Account * parent)
{
  gnc_ui_new_account_window_internal (parent, NULL, NULL, NULL, FALSE);
}

void
gnc_ui_new_account_with_types( AccountGroup *unused,
                               GList *valid_types )
{
  GList *validTypesCopy = g_list_copy( valid_types );
  AccountWindow *aw;

  aw = gnc_ui_new_account_window_internal( NULL, NULL, validTypesCopy, NULL, FALSE );
  if ( validTypesCopy != NULL ) {
    /* Attach it with "[...]_full" so we can set the appropriate
     * GtkDestroyNotify func. */
    g_object_set_data_full( G_OBJECT(aw->dialog), "validTypesListCopy",
			    validTypesCopy, (GDestroyNotify)g_list_free );
  }
}

/************************************************************
 *             Callbacks for a non-Modal Dialog             *
 ************************************************************/

/*
 * register a callback that get's called when the account has changed
 * so significantly that you need to destroy yourself.  In particular
 * this is used by the ledger display to destroy ledgers when the
 * account type has changed.
 */
void
gnc_ui_register_account_destroy_callback (void (*cb)(Account *))
{
  if (!cb)
    return;

  if (g_list_index (ac_destroy_cb_list, cb) == -1)
    ac_destroy_cb_list = g_list_append (ac_destroy_cb_list, cb);

  return;
}
