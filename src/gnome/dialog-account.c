/********************************************************************\
 * dialog-account.c -- window for creating and editing accounts for *
 *                     GnuCash                                      *
 * Copyright (C) 2000 Dave Peticolas <dave@krondo.com>              *
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
#include <string.h>

#include "AccWindow.h"
#include "FileDialog.h"
#include "MainWindow.h"
#include "MultiLedger.h"
#include "account-tree.h"
#include "dialog-account.h"
#include "dialog-utils.h"
#include "glade-gnc-dialogs.h"
#include "global-options.h"
#include "gnc-amount-edit.h"
#include "gnc-commodity-edit.h"
#include "gnc-commodity.h"
#include "gnc-component-manager.h"
#include "gnc-dateedit.h"
#include "gnc-engine-util.h"
#include "gnc-engine.h"
#include "gnc-ui.h"
#include "messages.h"
#include "query-user.h"
#include "window-help.h"
#include "window-main.h"


#define DIALOG_NEW_ACCOUNT_CM_CLASS "dialog-new-account"
#define DIALOG_EDIT_ACCOUNT_CM_CLASS "dialog-edit-account"

typedef enum
{
  NEW_ACCOUNT,
  EDIT_ACCOUNT
} AccountDialogType;


struct _AccountWindow
{
  GtkWidget *dialog;

  AccountDialogType dialog_type;

  GUID    account;
  Account *top_level_account;
  Account *created_account;

  GList *subaccount_names;

  GNCAccountType type;

  GtkWidget * name_entry;
  GtkWidget * description_entry;
  GtkWidget * code_entry;
  GtkWidget * notes_text;

  GtkWidget * currency_edit;
  GtkWidget * security_edit;

  GtkWidget * type_list;
  GtkWidget * parent_tree;

  GtkWidget * opening_balance_edit;
  GtkWidget * opening_balance_date_edit;
  GtkWidget * opening_balance_frame;

  /* These probably don't belong here anymore, but until we figure out
     what we want, we'll leave them alone. */
  GtkWidget * source_menu;
  GtkWidget * quote_tz_menu;
  GtkWidget * quote_frame;

  GtkWidget * tax_related_button;

  gint component_id;
};


/** Static Globals *******************************************************/
static short module = MOD_GUI;

static gint last_width = 0;
static gint last_height = 0;

static int last_used_account_type = BANK;


/** Declarations *********************************************************/
static void gnc_account_window_set_name (AccountWindow *aw);
static AccountWindow *
gnc_ui_new_account_window_internal (Account *base_account,
                                    GList *subaccount_names);
static void make_account_changes(GHashTable *change_currency,
                                 GHashTable *change_security,
                                 GHashTable *change_type);
static void gnc_ui_refresh_account_window (AccountWindow *aw);


/** Implementation *******************************************************/

static Account *
aw_get_account (AccountWindow *aw)
{
  if (!aw)
    return NULL;

  return xaccAccountLookup (&aw->account);
}

/* Copy the account values to the GUI widgets */
static void
gnc_account_to_ui(AccountWindow *aw)
{
  Account *account = aw_get_account (aw);
  gnc_commodity * commodity;
  const char *string;
  gboolean tax_related;
  gint pos = 0;

  if (!account)
    return;

  string = xaccAccountGetName (account);
  gtk_entry_set_text(GTK_ENTRY(aw->name_entry), string);

  string = xaccAccountGetDescription (account);
  gtk_entry_set_text(GTK_ENTRY(aw->description_entry), string);

  commodity = xaccAccountGetCurrency (account);
  gnc_commodity_edit_set_commodity (GNC_COMMODITY_EDIT (aw->currency_edit),
                                    commodity);

  commodity = xaccAccountGetSecurity (account);
  gnc_commodity_edit_set_commodity (GNC_COMMODITY_EDIT (aw->security_edit),
                                    commodity);

  string = xaccAccountGetCode (account);
  gtk_entry_set_text(GTK_ENTRY(aw->code_entry), string);

  string = xaccAccountGetNotes (account);
  if (string == NULL) string = "";

  gtk_editable_delete_text (GTK_EDITABLE (aw->notes_text), 0, -1);
  gtk_editable_insert_text (GTK_EDITABLE (aw->notes_text), string,
                            strlen(string), &pos);

  tax_related = xaccAccountGetTaxRelated (account);
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (aw->tax_related_button),
                                tax_related);

  if ((STOCK != aw->type) && (MUTUAL != aw->type) && (CURRENCY != aw->type))
    return;

  {
    /* we'll let GetPriceSrc handle the account type checking... */
    const char* price_src = xaccAccountGetPriceSrc (account);
    if (price_src)
      gtk_option_menu_set_history (GTK_OPTION_MENU (aw->source_menu),
                                   gnc_get_source_code (price_src));
  }

  {
    const char* quote_tz = xaccAccountGetQuoteTZ (account);
    guint pos = 0;

    if (quote_tz)
    {
      pos = gnc_find_timezone_menu_position(quote_tz);
      if(pos == 0)
      {
        PWARN("Unknown price quote timezone (%s), resetting to default.",
              quote_tz);
        xaccAccountSetQuoteTZ (account, NULL);
      }
    }

    gtk_option_menu_set_history (GTK_OPTION_MENU (aw->quote_tz_menu), pos);
  }
}


/* Record the GUI values into the Account structure */
static void
gnc_ui_to_account(AccountWindow *aw)
{
  Account *account = aw_get_account (aw);
  gnc_commodity *commodity;
  Account *parent_account;
  const char *old_string;
  const char *string;
  gboolean tax_related;
  gnc_numeric balance;
  time_t date;

  if (!account)
    return;

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

  commodity =
    gnc_commodity_edit_get_commodity (GNC_COMMODITY_EDIT (aw->currency_edit));
  if (commodity &&
      !gnc_commodity_equiv(commodity, xaccAccountGetCurrency(account)))
    xaccAccountSetCurrency (account, commodity);

  string = gtk_entry_get_text (GTK_ENTRY(aw->code_entry));
  old_string = xaccAccountGetCode (account);
  if (safe_strcmp (string, old_string) != 0)
    xaccAccountSetCode (account, string);

  if ((STOCK == aw->type) || (MUTUAL == aw->type) || (CURRENCY == aw->type))
  {
    commodity = gnc_commodity_edit_get_commodity
      (GNC_COMMODITY_EDIT (aw->security_edit));
    if (commodity &&
        !gnc_commodity_equiv (commodity, xaccAccountGetSecurity(account)))
      xaccAccountSetSecurity (account, commodity);

    if ((STOCK == aw->type) || (MUTUAL == aw->type))
    {
      gint code;

      code = gnc_option_menu_get_active (aw->source_menu);
      string = gnc_get_source_code_name (code);
      old_string = xaccAccountGetPriceSrc (account);
      if (safe_strcmp (string, old_string) != 0)
        xaccAccountSetPriceSrc (account, string);

      code = gnc_option_menu_get_active (aw->quote_tz_menu);
      string = gnc_timezone_menu_position_to_string(code);
      old_string = xaccAccountGetQuoteTZ (account);
      if (safe_strcmp (string, old_string) != 0)
        xaccAccountSetQuoteTZ (account, string);
    }
  }

  string = gtk_editable_get_chars (GTK_EDITABLE(aw->notes_text), 0, -1);
  old_string = xaccAccountGetNotes (account);
  if (safe_strcmp (string, old_string) != 0)
    xaccAccountSetNotes (account, string);

  tax_related =
    gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (aw->tax_related_button));
  xaccAccountSetTaxRelated (account, tax_related);

  parent_account =
    gnc_account_tree_get_current_account (GNC_ACCOUNT_TREE(aw->parent_tree));
  if (parent_account == aw->top_level_account)
    parent_account = NULL;

  xaccAccountBeginEdit (parent_account);

  if (parent_account != NULL)
  {
    if (parent_account != xaccAccountGetParentAccount (account))
      xaccAccountInsertSubAccount (parent_account, account);
  }
  else
    xaccGroupInsertAccount (gncGetCurrentGroup(), account);

  xaccAccountCommitEdit (parent_account);
  xaccAccountCommitEdit (account);

  balance = gnc_amount_edit_get_amount
    (GNC_AMOUNT_EDIT (aw->opening_balance_edit));
  date = gnc_date_edit_get_date
    (GNC_DATE_EDIT (aw->opening_balance_date_edit));

  if (!gnc_account_create_opening_balance (account, balance, date))
  {
    const char *message = _("Could not create opening balance.");
    gnc_error_dialog_parented(GTK_WINDOW(aw->dialog), message);
  }
}


static void 
gnc_finish_ok (AccountWindow *aw,
               GHashTable *change_currency,
               GHashTable *change_security,
               GHashTable *change_type)
{
  gnc_suspend_gui_refresh ();

  /* make the account changes */
  make_account_changes (change_currency, change_security, change_type);
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
    account = xaccMallocAccount ();
    aw->account = *xaccAccountGetGUID (account);
    aw->type = xaccAccountGetType (parent);

    xaccAccountSetName (account, aw->subaccount_names->data);

    node = aw->subaccount_names;
    aw->subaccount_names = g_list_remove_link (aw->subaccount_names, node);
    g_free (node->data);
    g_list_free_1 (node);

    gnc_account_to_ui (aw);

    gnc_account_window_set_name (aw);

    commodity = xaccAccountGetCurrency (parent);
    gnc_commodity_edit_set_commodity (GNC_COMMODITY_EDIT (aw->currency_edit),
                                      commodity);

    commodity = xaccAccountGetSecurity (parent);
    gnc_commodity_edit_set_commodity (GNC_COMMODITY_EDIT (aw->security_edit),
                                      commodity);

    gnc_account_tree_select_account (GNC_ACCOUNT_TREE(aw->parent_tree),
                                     parent, TRUE);

    gnc_resume_gui_refresh ();

    return;
  }

  /* save for posterity */
  aw->created_account = aw_get_account (aw);

  /* so it doesn't get freed on close */
  aw->account = *xaccGUIDNULL ();

  gnc_close_gui_component (aw->component_id);
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
change_func (gpointer key, gpointer value, gpointer field_code)
{
  Account *account = key;
  AccountFieldCode field = GPOINTER_TO_INT(field_code);

  if (account == NULL)
    return;

  xaccAccountBeginEdit(account);

  switch (field)
  {
    case ACCOUNT_CURRENCY:
      {
        gnc_commodity * currency = value;

        xaccAccountSetCurrency(account, currency);
      }
      break;
    case ACCOUNT_SECURITY:
      {
        gnc_commodity * security = value;

        xaccAccountSetSecurity(account, security);
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


/* Perform the changes to accounts dictated by the hash tables */
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


/* Determine which accounts must have their currency and/or
 * security changed in order to keep things kosher when the
 * given account is changed to have the given currency and
 * security. The changes needed are recorded in the hash
 * tables. */
static void
gnc_account_change_currency_security(Account *account,
                                     GHashTable *change_currency,
                                     GHashTable *change_security,
                                     gnc_commodity * currency,
                                     gnc_commodity * security)
{
  gnc_commodity * old_currency;
  gnc_commodity * old_security;
  gboolean new_currency;
  gboolean new_security;
  GSList *stack;

  if (account == NULL)
    return;

  old_currency = xaccAccountGetCurrency(account);
  old_security = xaccAccountGetSecurity(account);

  if ((gnc_commodity_equiv(currency, old_currency)) &&
      (gnc_commodity_equiv(security, old_security)))
    return;

  if (!gnc_commodity_equiv(currency, old_currency))
  {
    g_hash_table_insert(change_currency, account, (gpointer) currency);
    new_currency = TRUE;
  }
  else
    new_currency = FALSE;

  if (security && !gnc_commodity_equiv(security, old_security)) 
  {
    g_hash_table_insert(change_security, account, (gpointer) security);
    new_security = TRUE;
  }
  else
    new_security = FALSE;

  stack = g_slist_prepend(NULL, account);

  while (stack != NULL)
  {
    GSList *pop;
    GList *node;

    pop = stack;
    account = pop->data;
    stack = g_slist_remove_link(stack, pop);
    g_slist_free_1(pop);

    for (node = xaccAccountGetSplitList (account); node; node = node->next)
    {
      Split *split = node->data;
      Transaction *trans;
      GList *n;

      trans = xaccSplitGetParent(split);
      if (trans == NULL)
        continue;

      if (xaccTransIsCommonExclSCurrency(trans, currency, split))
        continue;

      if (xaccTransIsCommonExclSCurrency(trans, security, split))
        continue;

      for (n = xaccTransGetSplitList (trans); n; n = n->next)
      {
        Split *s = n->data;
        gboolean add_it = FALSE;
        const gnc_commodity * commodity;
        Account * a;

        a = xaccSplitGetAccount (s);

        if ((a == NULL) || (a == account))
          continue;

        if (g_hash_table_lookup(change_currency, a) != NULL)
          continue;

        if (g_hash_table_lookup(change_security, a) != NULL)
          continue;

        commodity = xaccAccountGetCurrency(a);

        if (new_currency && (gnc_commodity_equiv(old_currency, commodity)))
        {
          g_hash_table_insert(change_currency, a, (gpointer) currency);
          add_it = TRUE;
        }

        if (new_security && (gnc_commodity_equiv(old_security, commodity)))
        {
          g_hash_table_insert(change_currency, a, (gpointer) security);
          add_it = TRUE;
        }

        commodity = xaccAccountGetSecurity(a);

        if (new_security && (gnc_commodity_equiv(old_security, commodity)))
        {
          g_hash_table_insert(change_security, a, (gpointer) security);
          add_it = TRUE;
        }

        if (new_currency && (gnc_commodity_equiv(old_currency, commodity)))
        {
          g_hash_table_insert(change_security, a, (gpointer) currency);
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
  gchar *full_name;
  gchar *account_field_name;
  gchar *account_field_value;
  gchar *value_str;

  if (fs == NULL) return;
  if (fs->account == account) return;

  full_name = xaccAccountGetFullName(account, gnc_get_account_separator());
  if(!full_name)
    full_name = g_strdup("");

  account_field_name = g_strdup(gnc_ui_get_account_field_name(fs->field));
  if (!account_field_name)
    account_field_name = g_strdup("");

  account_field_value = 
    g_strdup(gnc_ui_get_account_field_value_string(account, fs->field));
  if (!account_field_value)
    account_field_value = g_strdup("");

  switch (fs->field)
  {
    case ACCOUNT_CURRENCY:
    case ACCOUNT_SECURITY:
      value_str = g_strdup(gnc_commodity_get_printname(value));
      break;
    case ACCOUNT_TYPE:
      value_str = g_strdup(xaccAccountGetTypeStr(GPOINTER_TO_INT(value)));
      break;
    default:
      g_warning("unexpected field type");
      g_free(full_name);
      g_free(account_field_name);
      g_free(account_field_value);
      return;
  }

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


/* Present a dialog of proposed account changes for the user's ok */
static gboolean
extra_change_verify(AccountWindow *aw,
                    GHashTable *change_currency,
                    GHashTable *change_security,
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

    dialog = gnome_dialog_new(_("Verify Changes"),
                              GNOME_STOCK_BUTTON_OK,
                              GNOME_STOCK_BUTTON_CANCEL,
                              NULL);

    gnome_dialog_set_default(GNOME_DIALOG(dialog), 0);
    gnome_dialog_set_close(GNOME_DIALOG(dialog), FALSE);
    gnome_dialog_close_hides(GNOME_DIALOG(dialog), FALSE);
    gnome_dialog_set_parent(GNOME_DIALOG(dialog), GTK_WINDOW(aw->dialog));
    gtk_window_set_policy(GTK_WINDOW(dialog), TRUE, TRUE, TRUE);
    gtk_window_set_default_size(GTK_WINDOW(dialog), 0, 300);

    vbox = GNOME_DIALOG(dialog)->vbox;

    label = gtk_label_new(_("The following changes must be made. Continue?"));
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

  GNCAccountType current_type;

  const char *name;
  gnc_commodity * currency;
  gnc_commodity * security;
  gnc_numeric balance;


  /* check for valid name */
  name = gtk_entry_get_text(GTK_ENTRY(aw->name_entry));
  if (safe_strcmp(name, "") == 0)
  {
    const char *message = _("The account must be given a name.");
    gnc_error_dialog_parented(GTK_WINDOW(aw->dialog), message);
    return;
  }

  /* check for valid type */
  if (aw->type == BAD_TYPE)
  {
    const char *message = _("You must select an account type.");
    gnc_error_dialog_parented(GTK_WINDOW(aw->dialog), message);
    return;
  }

  tree = GNC_ACCOUNT_TREE(aw->parent_tree);
  new_parent = gnc_account_tree_get_current_account(tree);

  /* Parent check, probably not needed, but be safe */
  if (!gnc_filter_parent_accounts(new_parent, aw))
  {
    const char *message = _("You must choose a valid parent account.");
    gnc_error_dialog_parented(GTK_WINDOW(aw->dialog), message);
    return;
  }

  currency =
    gnc_commodity_edit_get_commodity (GNC_COMMODITY_EDIT (aw->currency_edit));
  security =
    gnc_commodity_edit_get_commodity (GNC_COMMODITY_EDIT (aw->security_edit));

  if (!currency)
  {
    const char *message = _("You must choose a currency.");
    gnc_error_dialog_parented(GTK_WINDOW(aw->dialog), message);
    return;
  }

  if (!security && ((aw->type == STOCK)  ||
                    (aw->type == MUTUAL) ||
                    (aw->type == CURRENCY)))
  {
    const char *message = _("You must choose a security.");
    gnc_error_dialog_parented(GTK_WINDOW(aw->dialog), message);
    return;
  }


  account = aw_get_account (aw);
  if (!account)
    return;

  change_currency = g_hash_table_new(NULL, NULL);
  change_security = g_hash_table_new(NULL, NULL);
  change_type     = g_hash_table_new(NULL, NULL);

  gnc_account_change_currency_security(account,
                                       change_currency,
                                       change_security,
                                       currency,
                                       security);

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

  if (!extra_change_verify(aw, change_currency, change_security, change_type))
  {
    g_hash_table_destroy(change_currency);
    g_hash_table_destroy(change_security);
    g_hash_table_destroy(change_type);
    return;
  }

  if (current_type != aw->type)
    /* Just refreshing won't work. */
    xaccDestroyLedgerDisplay (account);

  gnc_finish_ok (aw, change_currency, change_security, change_type);

  g_hash_table_destroy (change_currency);
  g_hash_table_destroy (change_security);
  g_hash_table_destroy (change_type);
}


static void
gnc_new_account_ok (AccountWindow *aw)
{
  const gnc_commodity * currency;
  const gnc_commodity * security;
  Account *parent_account;
  char *name;

  /* check for valid name */
  name = gtk_entry_get_text(GTK_ENTRY(aw->name_entry));
  if (safe_strcmp(name, "") == 0)
  {
    const char *message = _("The account must be given a name.");
    gnc_error_dialog_parented(GTK_WINDOW(aw->dialog), message);
    return;
  }

  parent_account =
    gnc_account_tree_get_current_account(GNC_ACCOUNT_TREE(aw->parent_tree));
  if (parent_account == aw->top_level_account)
    parent_account = NULL;

  /* check for a duplicate name */
  {
    Account *account;
    AccountGroup *group;
    char separator;

    group = gncGetCurrentGroup();

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
      gnc_error_dialog_parented(GTK_WINDOW(aw->dialog), message);
      return;
    }
  }

  /* check for valid type */
  if (aw->type == BAD_TYPE)
  {
    const char *message = _("You must select an account type.");
    gnc_error_dialog_parented(GTK_WINDOW(aw->dialog), message);
    return;
  }

  /* check for currency & security */
  currency =
    gnc_commodity_edit_get_commodity (GNC_COMMODITY_EDIT (aw->currency_edit));
  security =
    gnc_commodity_edit_get_commodity (GNC_COMMODITY_EDIT (aw->security_edit));

  if (!currency)
  {
    const char *message = _("You must choose a currency.");
    gnc_error_dialog_parented(GTK_WINDOW(aw->dialog), message);
    return;
  }

  if (!security && ((aw->type == STOCK)  ||
                    (aw->type == MUTUAL) ||
                    (aw->type == CURRENCY)))
  {
    const char *message = _("You must choose a security.");
    gnc_error_dialog_parented(GTK_WINDOW(aw->dialog), message);
    return;
  }

  if (!gnc_amount_edit_evaluate (GNC_AMOUNT_EDIT (aw->opening_balance_edit)))
  {
    const char *message = _("You must enter a valid opening balance "
                            "or leave it blank.");
    gnc_error_dialog_parented(GTK_WINDOW(aw->dialog), message);
    return;
  }

  gnc_finish_ok (aw, NULL, NULL, NULL);
}


static void
gnc_account_window_ok_cb(GtkWidget * widget, gpointer data)
{
  AccountWindow *aw = data; 

  switch (aw->dialog_type)
  {
    case NEW_ACCOUNT:
      gnc_new_account_ok (aw);
      break;
    case EDIT_ACCOUNT:
      gnc_edit_account_ok (aw);
      break;
    default:
      return;
  }
}


static void
gnc_account_window_cancel_cb(GtkWidget * widget, gpointer data)
{
  AccountWindow *aw = data; 

  gnc_close_gui_component (aw->component_id);
}


static void 
gnc_account_window_help_cb(GtkWidget *widget, gpointer data)
{
  AccountWindow *aw = data;
  char *help_file;

  switch (aw->dialog_type)
  {
    case NEW_ACCOUNT:
      help_file = HH_ACC;
      break;
    case EDIT_ACCOUNT:
      help_file = HH_ACCEDIT;
      break;
    default:
      return;
  }

  helpWindow(NULL, NULL, help_file);
}


static int
gnc_account_window_destroy_cb (GtkObject *object, gpointer data)
{
  AccountWindow *aw = data;
  Account *account;

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
      return FALSE;
  }

  gnc_unregister_gui_component (aw->component_id);

  xaccAccountBeginEdit (aw->top_level_account);
  xaccAccountDestroy (aw->top_level_account);
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

  g_free (aw);

  return FALSE;
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

  aw->type = row;

  last_used_account_type = row;

  sensitive = (aw->type == STOCK    ||
	       aw->type == MUTUAL   ||
	       aw->type == CURRENCY);

  gtk_widget_set_sensitive(aw->security_edit, sensitive);
  gtk_widget_set_sensitive(aw->quote_frame, sensitive);

  sensitive = (aw->type != EQUITY &&
               aw->type != CURRENCY &&
               aw->type != STOCK &&
               aw->type != MUTUAL);

  gtk_widget_set_sensitive(aw->opening_balance_frame, sensitive);
}


static void 
gnc_type_list_unselect_cb(GtkCList * type_list, gint row, gint column,
                          GdkEventButton * event, gpointer data)
{
  AccountWindow * aw = data;

  aw->type = BAD_TYPE;

  gtk_widget_set_sensitive(aw->security_edit, FALSE);
  gtk_widget_set_sensitive(aw->source_menu, FALSE);
}


static void
gnc_account_list_fill(GtkCList *type_list)
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

static void
gnc_account_type_list_create(AccountWindow *aw)
{
  gnc_account_list_fill(GTK_CLIST(aw->type_list));

  gtk_clist_columns_autosize(GTK_CLIST(aw->type_list));

  gtk_signal_connect(GTK_OBJECT(aw->type_list), "select-row",
		     GTK_SIGNAL_FUNC(gnc_type_list_select_cb), aw);

  gtk_signal_connect(GTK_OBJECT(aw->type_list), "unselect-row",
		     GTK_SIGNAL_FUNC(gnc_type_list_unselect_cb), aw);

  switch (aw->dialog_type)
  {
    case NEW_ACCOUNT:
      aw->type = last_used_account_type;
      break;
    case EDIT_ACCOUNT:
      aw->type = xaccAccountGetType (aw_get_account (aw));
      break;
  }

  gtk_clist_select_row(GTK_CLIST(aw->type_list), aw->type, 0);
}


static void
gnc_type_list_row_set_active(GtkCList *type_list, gint row, gboolean state)
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


static void
gnc_parent_tree_select(GNCAccountTree *tree,
                       Account * account, 
                       gpointer data)
{
  AccountWindow *aw = data;
  GNCAccountType parent_type;
  gboolean  compatible;
  gint      type;

  gnc_account_window_set_name (aw);

  if (aw->dialog_type == EDIT_ACCOUNT)
    return;

  account = gnc_account_tree_get_current_account(tree);

  /* Deleselect any or select top account */
  if (account == NULL || account == aw->top_level_account)
    for (type = 0; type < NUM_ACCOUNT_TYPES; type++)
      gnc_type_list_row_set_active(GTK_CLIST(aw->type_list), type, TRUE);
  else /* Some other account was selected */
  {
    parent_type = xaccAccountGetType(account);

    /* set the allowable account types for this parent */
    for (type = 0; type < NUM_ACCOUNT_TYPES; type++)
    {
      compatible = xaccAccountTypesCompatible(parent_type, type);
      gnc_type_list_row_set_active(GTK_CLIST(aw->type_list), type, compatible);
    }

    /* now select a new account type if the account class has changed */
    compatible = xaccAccountTypesCompatible(parent_type, aw->type);
    if (!compatible)
    {
      aw->type = parent_type;
      gtk_clist_select_row(GTK_CLIST(aw->type_list), parent_type, 0);
      gtk_clist_moveto(GTK_CLIST(aw->type_list), parent_type, 0, 0.5, 0);
    }
  }
}

static void
gnc_account_name_changed_cb(GtkWidget *widget, gpointer data)
{
  AccountWindow *aw = data;

  gnc_account_window_set_name (aw);
}

static void
currency_changed_cb (GNCCommodityEdit *gce, gpointer data)
{
  AccountWindow *aw = data;
  gnc_commodity *currency;

  currency = gnc_commodity_edit_get_commodity (gce);
  if (!currency)
    return;

  gnc_amount_edit_set_fraction (GNC_AMOUNT_EDIT (aw->opening_balance_edit),
                                gnc_commodity_get_fraction (currency));
  gnc_amount_edit_set_print_info (GNC_AMOUNT_EDIT (aw->opening_balance_edit),
                                  gnc_commodity_print_info (currency, FALSE));
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
  GnomeDialog *awd;
  GtkWidget *amount;
  GtkWidget *date;
  GtkObject *awo;
  GtkWidget *box;

  aw->dialog = create_Account_Dialog ();
  awo = GTK_OBJECT (aw->dialog);
  awd = GNOME_DIALOG (awo);

  gtk_object_set_data (awo, "dialog_info", aw);

  /* default to ok */
  gnome_dialog_set_default(awd, 0);

  gtk_signal_connect(awo, "destroy",
                     GTK_SIGNAL_FUNC(gnc_account_window_destroy_cb), aw);

  gnome_dialog_button_connect
    (awd, 0, GTK_SIGNAL_FUNC(gnc_account_window_ok_cb), aw);
  gnome_dialog_button_connect
    (awd, 1, GTK_SIGNAL_FUNC(gnc_account_window_cancel_cb), aw);
  gnome_dialog_button_connect
    (awd, 2, GTK_SIGNAL_FUNC(gnc_account_window_help_cb), aw);

  aw->name_entry =        gtk_object_get_data(awo, "name_entry");
  gtk_signal_connect(GTK_OBJECT (aw->name_entry), "changed",
		     GTK_SIGNAL_FUNC(gnc_account_name_changed_cb), aw);

  aw->description_entry = gtk_object_get_data(awo, "description_entry");
  aw->code_entry =        gtk_object_get_data(awo, "code_entry");
  aw->notes_text =        gtk_object_get_data(awo, "notes_text");

  gtk_object_set_data(awo, "account_window_struct", aw);

  gnome_dialog_editable_enters(awd, GTK_EDITABLE(aw->name_entry));
  gnome_dialog_editable_enters(awd, GTK_EDITABLE(aw->description_entry));
  gnome_dialog_editable_enters(awd, GTK_EDITABLE(aw->code_entry));

  box = gtk_object_get_data(awo, "currency_hbox");
  aw->currency_edit = gnc_commodity_edit_new ();
  gtk_box_pack_start(GTK_BOX(box), aw->currency_edit, TRUE, TRUE, 0);

  gtk_signal_connect (GTK_OBJECT (aw->currency_edit), "changed",
                      GTK_SIGNAL_FUNC (currency_changed_cb), aw);

  box = gtk_object_get_data(awo, "security_hbox");
  aw->security_edit = gnc_commodity_edit_new ();
  gtk_box_pack_start(GTK_BOX(box), aw->security_edit, TRUE, TRUE, 0);

  box = gtk_object_get_data(awo, "source_box");
  aw->source_menu = gnc_ui_source_menu_create(aw_get_account (aw));
  gtk_box_pack_start(GTK_BOX(box), aw->source_menu, TRUE, TRUE, 0);

  box = gtk_object_get_data(awo, "quote_tz_box");
  aw->quote_tz_menu = gnc_ui_quote_tz_menu_create(aw_get_account (aw));
  gtk_box_pack_start(GTK_BOX(box), aw->quote_tz_menu, TRUE, TRUE, 0);

  aw->quote_frame = gtk_object_get_data (awo, "price_quote_frame");

  box = gtk_object_get_data(awo, "opening_balance_box");
  amount = gnc_amount_edit_new ();
  aw->opening_balance_edit = amount;
  gtk_box_pack_start(GTK_BOX(box), amount, TRUE, TRUE, 0);
  gnc_amount_edit_set_evaluate_on_enter (GNC_AMOUNT_EDIT (amount), TRUE);

  box = gtk_object_get_data (awo, "opening_balance_date_box");
  date = gnc_date_edit_new(time(NULL), FALSE, FALSE);
  aw->opening_balance_date_edit = date;
  gtk_box_pack_start(GTK_BOX(box), date, TRUE, TRUE, 0);

  aw->opening_balance_frame =
    gtk_object_get_data (awo, "opening_balance_frame");

  aw->type_list = gtk_object_get_data(awo, "type_list");
  gnc_account_type_list_create (aw);

  box = gtk_object_get_data(awo, "parent_scroll");

  aw->top_level_account = xaccMallocAccount();
  xaccAccountSetName(aw->top_level_account, _("New top level account"));

  aw->parent_tree = gnc_account_tree_new_with_root(aw->top_level_account);
  gtk_clist_column_titles_hide(GTK_CLIST(aw->parent_tree));
  gnc_account_tree_hide_all_but_name(GNC_ACCOUNT_TREE(aw->parent_tree));
  gnc_account_tree_refresh(GNC_ACCOUNT_TREE(aw->parent_tree));
  gnc_account_tree_expand_account(GNC_ACCOUNT_TREE(aw->parent_tree),
                                  aw->top_level_account);
  gtk_container_add(GTK_CONTAINER(box), GTK_WIDGET(aw->parent_tree));

  gtk_signal_connect(GTK_OBJECT (aw->parent_tree), "select_account",
		     GTK_SIGNAL_FUNC(gnc_parent_tree_select), aw);
  gtk_signal_connect(GTK_OBJECT (aw->parent_tree), "unselect_account",
		     GTK_SIGNAL_FUNC(gnc_parent_tree_select), aw);

  aw->tax_related_button = gtk_object_get_data (awo, "tax_related_button");

  if (last_width == 0)
    gnc_get_window_size("account_win", &last_width, &last_height);

  gtk_window_set_default_size(GTK_WINDOW(aw->dialog),
                              last_width, last_height);

  gtk_widget_grab_focus(GTK_WIDGET(aw->name_entry));
}


static char *
get_ui_fullname (AccountWindow *aw)
{
  Account *parent_account;
  char *fullname;
  char *name;

  name = gtk_entry_get_text (GTK_ENTRY(aw->name_entry));
  if (!name || *name == '\0')
    name = _("<No name>");

  parent_account =
    gnc_account_tree_get_current_account (GNC_ACCOUNT_TREE(aw->parent_tree));
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

  gdk_window_get_geometry (GTK_WIDGET(aw->dialog)->window, NULL, NULL,
                           &last_width, &last_height, NULL);

  gnc_save_window_size ("account_win", last_width, last_height);

  gnome_dialog_close (GNOME_DIALOG (aw->dialog));
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
                                    GList *subaccount_names)
{
  gnc_commodity *commodity;
  AccountWindow *aw;
  Account *account;

  aw = g_new0 (AccountWindow, 1);

  aw->dialog_type = NEW_ACCOUNT;

  account = xaccMallocAccount ();
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

  commodity = gnc_lookup_currency_option ("International",
                                          "Default Currency",
                                          gnc_locale_default_currency ());

  gnc_commodity_edit_set_commodity (GNC_COMMODITY_EDIT (aw->currency_edit),
                                    commodity);

  gtk_widget_show_all (aw->dialog);

  gnc_account_tree_select_account (GNC_ACCOUNT_TREE(aw->parent_tree),
                                   base_account, TRUE);

  gnc_window_adjust_for_screen (GTK_WINDOW(aw->dialog));

  gnc_account_window_set_name (aw);

  aw->component_id = gnc_register_gui_component (DIALOG_NEW_ACCOUNT_CM_CLASS,
                                                 refresh_handler,
                                                 close_handler, aw);

  gnc_gui_component_watch_entity_type (aw->component_id,
                                       GNC_ID_ACCOUNT,
                                       GNC_EVENT_MODIFY | GNC_EVENT_DESTROY);
  return aw;
}


/********************************************************************\
 * gnc_ui_new_account_window                                        *
 *   opens up a window to create a new account.                     *
 *                                                                  * 
 * Args:   group - not used                                         *
 * Return: AccountWindow object                                     *
 \*******************************************************************/
AccountWindow *
gnc_ui_new_account_window (AccountGroup *this_is_not_used) 
{
  return gnc_ui_new_account_window_internal (gnc_get_current_account (), NULL);
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
  group = gncGetCurrentGroup ();

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


static int
from_name_close_cb (GnomeDialog *dialog, gpointer data)
{
  AccountWindow *aw;
  Account **created_account = data;

  aw = gtk_object_get_data (GTK_OBJECT (dialog), "dialog_info");

  *created_account = aw->created_account;

  gtk_main_quit ();

  return FALSE;
}

Account *
gnc_ui_new_accounts_from_name_window (const char *name)
{
  AccountWindow *aw;
  Account *base_account;
  Account *created_account;
  GList * subaccount_names;
  GList * node;

  if (!name || *name == '\0')
  {
    subaccount_names = NULL;
    base_account = NULL;
  }
  else
    subaccount_names = gnc_split_account_name (name, &base_account);

  aw = gnc_ui_new_account_window_internal (base_account, subaccount_names);

  for (node = subaccount_names; node; node = node->next)
    g_free (node->data);
  g_list_free (subaccount_names);

  gtk_signal_connect(GTK_OBJECT (aw->dialog), "close",
                     GTK_SIGNAL_FUNC (from_name_close_cb), &created_account);

  gtk_window_set_modal (GTK_WINDOW (aw->dialog), TRUE);

  gtk_main ();

  return created_account;
}


static gboolean
find_by_account (gpointer find_data, gpointer user_data)
{
  Account *account = find_data;
  AccountWindow *aw = user_data;

  if (!aw)
    return FALSE;

  return guid_equal (&aw->account, xaccAccountGetGUID (account));
}

/********************************************************************\
 * gnc_ui_edit_account_window                                       *
 *   opens up a window to edit an account                           * 
 *                                                                  * 
 * Args:   account - the account to edit                            * 
 * Return: EditAccountWindow object                                 *
\********************************************************************/
AccountWindow *
gnc_ui_edit_account_window(Account *account)
{
  AccountWindow * aw;
  Account *parent;

  if (account == NULL)
    return NULL;

  aw = gnc_find_first_gui_component (DIALOG_EDIT_ACCOUNT_CM_CLASS,
                                     find_by_account, account);
  if (aw)
    return aw;

  aw = g_new0 (AccountWindow, 1);

  aw->dialog_type = EDIT_ACCOUNT;
  aw->account = *xaccAccountGetGUID (account);
  aw->subaccount_names = NULL;

  gnc_suspend_gui_refresh ();

  gnc_account_window_create (aw);
  gnc_account_to_ui (aw);

  gnc_resume_gui_refresh ();

  gtk_widget_show_all (aw->dialog);
  gtk_widget_hide (aw->opening_balance_frame);

  parent = xaccAccountGetParentAccount (account);
  if (parent == NULL)
    parent = aw->top_level_account;
  gnc_account_tree_select_account(GNC_ACCOUNT_TREE(aw->parent_tree),
                                  parent, TRUE);

  gnc_account_window_set_name (aw);

  gnc_window_adjust_for_screen(GTK_WINDOW(aw->dialog));

  aw->component_id = gnc_register_gui_component (DIALOG_EDIT_ACCOUNT_CM_CLASS,
                                                 refresh_handler,
                                                 close_handler, aw);

  gnc_gui_component_watch_entity_type (aw->component_id,
                                       GNC_ID_ACCOUNT,
                                       GNC_EVENT_MODIFY | GNC_EVENT_DESTROY);

  return aw;
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

  gnc_account_tree_refresh (GNC_ACCOUNT_TREE(aw->parent_tree));

  gnc_account_window_set_name (aw);
}


/********************************************************************\
 * gnc_ui_edit_account_window_raise                                 *
 *   shows and raises an account editing window                     *
 *                                                                  *
 * Args:   aw - the edit window structure                           *
\********************************************************************/
void
gnc_ui_edit_account_window_raise(AccountWindow * aw)
{
  if (aw == NULL)
    return;

  if (aw->dialog == NULL)
    return;

  gtk_widget_show (aw->dialog);

  if (aw->dialog->window == NULL)
    return;

  gdk_window_raise (aw->dialog->window);
}
