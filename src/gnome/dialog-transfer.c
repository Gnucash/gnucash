/********************************************************************\
 * dialog-transfer.c -- transfer dialog for GnuCash                 *
 * Copyright (C) 1999 Linas Vepstas                                 *
 * Copyright (C) 2000 Dave Peticolas                                *
 * Copyright (C) 2000 Herbert Thoma                                 *
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

#include "dialog-transfer.h"
#include "dialog-utils.h"
#include "MultiLedger.h"
#include "FileDialog.h"
#include "Refresh.h"
#include "window-reconcile.h"
#include "query-user.h"
#include "account-tree.h"
#include "glade-gnc-dialogs.h"
#include "gnc-amount-edit.h"
#include "gnc-dateedit.h"
#include "gnc-exp-parser.h"
#include "messages.h"
#include "gnc-ui.h"
#include "global-options.h"
#include "gnc-engine-util.h"


typedef enum
{
  XFER_DIALOG_FROM,
  XFER_DIALOG_TO
} XferDirection;


/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_GUI;

static GList *xfer_dialogs = NULL;

struct _xferDialog
{
  GtkWidget * dialog;

  GtkWidget * amount_edit;
  GtkWidget * date_entry;
  GtkWidget * num_entry;
  GtkWidget * description_entry;
  GtkWidget * memo_entry;

  GNCAccountTree * from;
  GNCAccountTree * to;

  GtkWidget * from_currency_label;
  GtkWidget * to_currency_label;

  GtkWidget * from_show_button;
  GtkWidget * to_show_button;

  GtkWidget * curr_transfer_frame;
  GtkWidget * curr_acct_label;

  GtkWidget * curr_acct_combo;
  GList * curr_accts_list;
  GtkWidget * curr_acct_combo_entry;
  GtkWidget * price_edit;
  GtkWidget * to_amount_edit;

  GtkWidget * price_radio;
  GtkWidget * amount_radio;
};

struct _acct_list_item
{
  char *acct_full_name;
  Account *acct;
};
typedef struct _acct_list_item acct_list_item;


static void
gnc_xfer_dialog_toggle_cb(GtkToggleButton *button, gpointer data)
{
  GNCAccountTree *tree = GNC_ACCOUNT_TREE(data);

  if (gtk_toggle_button_get_active(button))
    gnc_account_tree_show_income_expense(tree);
  else
    gnc_account_tree_hide_income_expense(tree);
}


static void
gnc_xfer_dialog_list_curr_accts(AccountGroup *group,
				GList **list,
				const gnc_commodity *from_curr,
				const gnc_commodity *to_curr)
{
  Account *acct;
  char *name;
  char separator_char;
  acct_list_item *list_element;
  int n;

  if (!group) return;

  separator_char = gnc_get_account_separator();

  n = 0;
  acct = xaccGroupGetAccount(group, n);
  while (acct)
  {
    if (xaccAccountGetType(acct) == CURRENCY)
    {
      const gnc_commodity *curr, *secu;

      curr = xaccAccountGetCurrency(acct);
      secu = xaccAccountGetSecurity(acct);

      if ((gnc_commodity_equiv(from_curr, curr) && 
           gnc_commodity_equiv(to_curr, secu)) ||
          (gnc_commodity_equiv(to_curr, curr) && 
           gnc_commodity_equiv(from_curr, secu)))
      {
	name = xaccAccountGetFullName(acct, separator_char);
	/* xaccAccountGetFullName returns a string, which must be
	   freed (inconsistent with the rest of the engine API).
	   The string is freed in gnc_xfer_dialog_free_curr_accts_list. */
	list_element = g_new(acct_list_item, 1);
	list_element->acct_full_name = name;
	list_element->acct = acct;
	*list = g_list_append(*list, list_element);
      }
    }
    gnc_xfer_dialog_list_curr_accts(xaccAccountGetChildren(acct),
				    list, from_curr, to_curr);
    n++;
    acct = xaccGroupGetAccount(group, n);
  }
}


static void
gnc_xfer_dialog_free_curr_accts_list(gpointer data, gpointer user_data)
{
  acct_list_item *list_element = data;

  g_free(list_element->acct_full_name);
  g_free(list_element);
}


static Account *
gnc_xfer_dialog_get_selected_curr_acct(XferDialog *xferData)
{
  Account *curr;
  char *curr_acct_name;
  GList *help;

  curr = NULL;
  curr_acct_name = gtk_editable_get_chars
    (GTK_EDITABLE(xferData->curr_acct_combo_entry), 0, -1);
  help = xferData->curr_accts_list;
  while(help)
  {
    if(!strcmp(curr_acct_name,
	       ((acct_list_item *)(help->data))->acct_full_name))
    {
      curr = ((acct_list_item *)(help->data))->acct;
      break;
    }
    help = g_list_next(help);
  }
  g_free(curr_acct_name);

  return curr;
}


static void
gnc_xfer_dialog_curr_acct_activate(XferDialog *xferData)
{
  Account *from_account;
  const gnc_commodity *from_currency = NULL;
  Account *to_account;
  const gnc_commodity *to_currency = NULL;
  gboolean curr_active;

  GList *curr_accts_name_list = NULL;
  GList *help;

  if(xferData->curr_accts_list)
  {
    g_list_foreach(xferData->curr_accts_list,
		   gnc_xfer_dialog_free_curr_accts_list, NULL);
    g_list_free(xferData->curr_accts_list);
    xferData->curr_accts_list = NULL;
  }

  from_account = 
    gnc_account_tree_get_current_account(GNC_ACCOUNT_TREE(xferData->from));
  if(from_account != NULL)
    from_currency = xaccAccountGetCurrency(from_account);

  to_account = 
    gnc_account_tree_get_current_account(GNC_ACCOUNT_TREE(xferData->to));
  if(to_account != NULL)
    to_currency = xaccAccountGetCurrency(to_account);

  curr_active = (from_account != NULL) && (to_account != NULL)
    && !gnc_commodity_equiv(from_currency, to_currency);

  gtk_widget_set_sensitive(xferData->curr_transfer_frame, curr_active);
  gtk_widget_set_sensitive(xferData->curr_acct_label, curr_active);
  gtk_widget_set_sensitive(xferData->curr_acct_combo, curr_active);
  gtk_widget_set_sensitive(xferData->price_edit,
			   curr_active && gtk_toggle_button_get_active
			   (GTK_TOGGLE_BUTTON(xferData->price_radio)));
  gtk_widget_set_sensitive(xferData->to_amount_edit, 
			   curr_active && gtk_toggle_button_get_active
			   (GTK_TOGGLE_BUTTON(xferData->amount_radio)));
  gtk_widget_set_sensitive(xferData->price_radio, curr_active);
  gtk_widget_set_sensitive(xferData->amount_radio, curr_active);

  if(curr_active)
  {
    gnc_xfer_dialog_list_curr_accts(gncGetCurrentGroup(),
				    &(xferData->curr_accts_list),
				    from_currency, to_currency);
    help = xferData->curr_accts_list;
    while(help)
    {
      curr_accts_name_list = 
	g_list_append(curr_accts_name_list,
		      ((acct_list_item *)(help->data))->acct_full_name);
      help = g_list_next(help);
    }
    if(!curr_accts_name_list)
      curr_accts_name_list = g_list_append(curr_accts_name_list, "");
    gtk_combo_set_popdown_strings(GTK_COMBO(xferData->curr_acct_combo),
				  curr_accts_name_list);
  }
  else
  {
    GtkEntry *entry;

    curr_accts_name_list = g_list_append(curr_accts_name_list, "");
    gtk_combo_set_popdown_strings(GTK_COMBO(xferData->curr_acct_combo),
				  curr_accts_name_list);

    gnc_amount_edit_set_amount(GNC_AMOUNT_EDIT(xferData->price_edit),
                               gnc_numeric_zero ());
    entry = GTK_ENTRY(gnc_amount_edit_gtk_entry
		      (GNC_AMOUNT_EDIT(xferData->price_edit)));
    gtk_entry_set_text(entry, "");

    gnc_amount_edit_set_amount(GNC_AMOUNT_EDIT(xferData->to_amount_edit),
                               gnc_numeric_zero ());
    entry = GTK_ENTRY(gnc_amount_edit_gtk_entry
		      (GNC_AMOUNT_EDIT(xferData->to_amount_edit)));
    gtk_entry_set_text(entry, "");
  }
  g_list_free(curr_accts_name_list);
}


static void
price_amount_radio_toggled_cb(GtkToggleButton *togglebutton, gpointer data)
{
  XferDialog *xferData = data;

  gtk_widget_set_sensitive(xferData->price_edit, gtk_toggle_button_get_active
			   (GTK_TOGGLE_BUTTON(xferData->price_radio)));
  gtk_widget_set_sensitive(xferData->to_amount_edit,
			   gtk_toggle_button_get_active
			   (GTK_TOGGLE_BUTTON(xferData->amount_radio)));
}


static void
gnc_xfer_dialog_from_tree_select_cb(GNCAccountTree *tree,
				    Account *account, gpointer data)
{
  XferDialog *xferData = data;
  const gnc_commodity *currency;

  account = gnc_account_tree_get_current_account(tree);
  currency = xaccAccountGetCurrency(account);
  gtk_label_set_text(GTK_LABEL(xferData->from_currency_label), 
		     gnc_commodity_get_printname(currency));

  gnc_xfer_dialog_curr_acct_activate(xferData);
}


static void
gnc_xfer_dialog_to_tree_select_cb(GNCAccountTree *tree,
				  Account *account, gpointer data)
{
  XferDialog *xferData = data;
  const gnc_commodity *currency;

  account = gnc_account_tree_get_current_account(tree);
  currency = xaccAccountGetCurrency(account);
  gtk_label_set_text(GTK_LABEL(xferData->to_currency_label),
		     gnc_commodity_get_printname(currency));

  gnc_xfer_dialog_curr_acct_activate(xferData);
}


static void
gnc_xfer_dialog_fill_tree_frame(XferDialog *xferData,
                                XferDirection direction,
                                GtkTooltips *tooltips)
{
  const char *show_inc_exp_message = _("Show the income and expense accounts");
  GNCAccountTree *atree;
  GtkWidget *scroll_win;
  GtkWidget *button;
  GtkWidget *tree;
  GtkObject *tdo;

  tdo = GTK_OBJECT (xferData->dialog);

  tree = gnc_account_tree_new();
  atree = GNC_ACCOUNT_TREE (tree);

  if (direction == XFER_DIALOG_TO)
    xferData->to = atree;
  else
    xferData->from = atree;
  gtk_clist_column_titles_hide(GTK_CLIST(tree));
  gnc_account_tree_hide_all_but_name(GNC_ACCOUNT_TREE(tree));
  gnc_account_tree_hide_income_expense(GNC_ACCOUNT_TREE(tree));
  gnc_account_tree_refresh(GNC_ACCOUNT_TREE(tree));

  scroll_win = gtk_object_get_data (tdo,
                                    (direction == XFER_DIALOG_TO) ?
                                    "to_window" : "from_window");

  gtk_container_add(GTK_CONTAINER(scroll_win), tree);

  {
    GtkStyle *st = gtk_widget_get_style(tree);
    GdkFont *font = NULL;
    gint height;

    if (st != NULL)
      font = st->font;

    if (font != NULL)
    {
      height = gdk_char_height(font, 'X');
      gtk_widget_set_usize(scroll_win, 0, (height + 6) * 10);
    }
  }

  button = gtk_object_get_data (tdo,
                                (direction == XFER_DIALOG_TO) ?
                                "to_show_button" : "from_show_button");

  if (direction == XFER_DIALOG_TO)
    xferData->to_show_button = button;
  else
    xferData->from_show_button = button;

  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(button), FALSE);
  gtk_tooltips_set_tip(tooltips, button, show_inc_exp_message, NULL);

  gtk_signal_connect(GTK_OBJECT(button), "toggled",
		     GTK_SIGNAL_FUNC(gnc_xfer_dialog_toggle_cb), tree);
}


static void
gnc_parse_error_dialog (XferDialog *xferData, const char *amount_edit)
{
  const char *error_string;
  char * error_phrase;

  error_string = gnc_exp_parser_error_string ();
  if (error_string == NULL)
    error_string = "";

  error_phrase = g_strdup_printf(_("You must enter a valid %s.\n\n"
                                   "Error: %s."), amount_edit,  error_string);

  gnc_error_dialog_parented(GTK_WINDOW(xferData->dialog), error_phrase);

  g_free(error_phrase);
}


static gboolean
gnc_xfer_amount_update_cb(GtkWidget *widget, GdkEventFocus *event,
			  gpointer data)
{
  XferDialog * xferData = data;
  Account *from, *to, *account;
  gnc_numeric amount, price, to_amount;
  const gnc_commodity * currency;

  from = gnc_account_tree_get_current_account(xferData->from);
  to = gnc_account_tree_get_current_account(xferData->to);
  account = from;
  if (account == NULL)
    account = to;

  currency = xaccAccountGetCurrency(account);

  gnc_amount_edit_evaluate (GNC_AMOUNT_EDIT (xferData->amount_edit));

  if ((from != NULL) && (to != NULL))
  {
    if (!xaccAccountsHaveCommonCurrency(from, to))
    {
      amount = gnc_amount_edit_get_amount
	(GNC_AMOUNT_EDIT(xferData->amount_edit));
      price = gnc_amount_edit_get_amount
	(GNC_AMOUNT_EDIT(xferData->price_edit));
      to_amount = gnc_numeric_div (amount, price,
                                   xaccAccountGetCurrencySCU (to),
                                   GNC_RND_ROUND);;
      gnc_amount_edit_set_amount(GNC_AMOUNT_EDIT(xferData->to_amount_edit),
                                 to_amount);
    }
  }

  return FALSE;
}


static gboolean
gnc_xfer_price_update_cb(GtkWidget *widget, GdkEventFocus *event,
			 gpointer data)
{
  XferDialog *xferData = data;
  const gnc_commodity *currency;
  gnc_numeric amount, price, to_amount;
  Account *account;

  account = gnc_account_tree_get_current_account(xferData->to);
  if (account == NULL)
    account = gnc_account_tree_get_current_account(xferData->from);

  currency = xaccAccountGetCurrency(account);

  gnc_amount_edit_evaluate (GNC_AMOUNT_EDIT (xferData->price_edit));

  amount = gnc_amount_edit_get_amount(GNC_AMOUNT_EDIT(xferData->amount_edit));
  price = gnc_amount_edit_get_amount(GNC_AMOUNT_EDIT(xferData->price_edit));
  to_amount = gnc_numeric_div (amount, price,
                               xaccAccountGetCurrencySCU (account),
                               GNC_RND_ROUND);
  gnc_amount_edit_set_amount(GNC_AMOUNT_EDIT(xferData->to_amount_edit),
                             to_amount);

  return FALSE;
}


static gboolean
gnc_xfer_to_amount_update_cb(GtkWidget *widget, GdkEventFocus *event,
                            gpointer data)
{
  XferDialog *xferData = data;
  const gnc_commodity *currency;
  gnc_numeric amount, price, to_amount;
  Account *account;

  account = gnc_account_tree_get_current_account(xferData->to);
  if (account == NULL)
    account = gnc_account_tree_get_current_account(xferData->from);

  currency = xaccAccountGetCurrency(account);

  gnc_amount_edit_evaluate (GNC_AMOUNT_EDIT (xferData->to_amount_edit));

  amount = gnc_amount_edit_get_amount(GNC_AMOUNT_EDIT(xferData->amount_edit));
  to_amount = gnc_amount_edit_get_amount
    (GNC_AMOUNT_EDIT(xferData->to_amount_edit));
  price = gnc_numeric_div (amount, to_amount, 100000, GNC_RND_ROUND);
  gnc_amount_edit_set_amount(GNC_AMOUNT_EDIT(xferData->price_edit), price);

  return FALSE;
}


static void
gnc_xfer_dialog_select_account(XferDialog *xferData, Account *account,
                               XferDirection direction)
{
  GNCAccountTree *tree;
  GtkWidget *show_button;
  gboolean is_income_expense;
  GNCAccountType type;

  if (xferData == NULL)
    return;
  if (account == NULL)
    return;

  switch (direction)
  {
    case XFER_DIALOG_FROM:
      tree = xferData->from;
      show_button = xferData->from_show_button;
      break;
    case XFER_DIALOG_TO:
      tree = xferData->to;
      show_button = xferData->to_show_button;
      break;
    default:
      return;
  }

  type = xaccAccountGetType(account);
  is_income_expense = (type == EXPENSE) || (type == INCOME);

  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(show_button),
                               is_income_expense);

  gnc_account_tree_select_account(tree, account, TRUE);
}


/********************************************************************\
 * gnc_xfer_dialog_select_from_account                              *
 *   select the from account in a xfer dialog                       *
 *                                                                  *
 * Args:   xferData - xfer dialog structure                         *
 *         account  - account to select                             *
 * Return: none                                                     *
\********************************************************************/
void
gnc_xfer_dialog_select_from_account(XferDialog *xferData, Account *account)
{
  gnc_xfer_dialog_select_account(xferData, account, XFER_DIALOG_FROM);
}


/********************************************************************\
 * gnc_xfer_dialog_select_to_account                                *
 *   select the to account in a xfer dialog                         *
 *                                                                  *
 * Args:   xferData - xfer dialog structure                         *
 *         account  - account to select                             *
 * Return: none                                                     *
\********************************************************************/
void
gnc_xfer_dialog_select_to_account(XferDialog *xferData, Account *account)
{
  gnc_xfer_dialog_select_account(xferData, account, XFER_DIALOG_TO);
}


/********************************************************************\
 * gnc_xfer_dialog_set_amount                                       *
 *   set the amount in the given xfer dialog                        *
 *                                                                  *
 * Args:   xferData - xfer dialog structure                         *
 *         amount   - the amount to set                             *
 * Return: none                                                     *
\********************************************************************/
void
gnc_xfer_dialog_set_amount(XferDialog *xferData, gnc_numeric amount)
{
  Account * account;
  const gnc_commodity * currency;

  if (xferData == NULL)
    return;

  account = gnc_account_tree_get_current_account(xferData->from);
  if (account == NULL)
    account = gnc_account_tree_get_current_account(xferData->to);
  
  currency = xaccAccountGetCurrency(account);

  gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (xferData->amount_edit), amount);
}


/********************************************************************\
 * gnc_xfer_dialog_set_description                                  *
 *   set the description in the given xfer dialog                   *
 *                                                                  *
 * Args:   xferData    - xfer dialog structure                      *
 *         description - the description to set                     *
 * Return: none                                                     *
\********************************************************************/
void
gnc_xfer_dialog_set_description(XferDialog *xferData, const char *description)
{
  if (xferData == NULL)
    return;

  gtk_entry_set_text(GTK_ENTRY(xferData->description_entry), description);
}


static void
gnc_xfer_dialog_ok_cb(GtkWidget * widget, gpointer data)
{
  XferDialog *xferData = data;
  Account *from, *to, *curr;
  const gnc_commodity *from_currency, *to_currency;
  gnc_numeric amount, to_amount;
  char * string;
  time_t time;

  gboolean curr_trans;

  Transaction *trans;
  Split *from_split;
  Split *to_split;
  Split *curr_split;


  from = gnc_account_tree_get_current_account(xferData->from);
  to   = gnc_account_tree_get_current_account(xferData->to);

  if ((from == NULL) || (to == NULL))
  {
    const char *message = _("You must specify an account to transfer from,\n"
                            "or to, or both, for this transaction.\n"
                            "Otherwise, it will not be recorded.");
    gnc_error_dialog_parented(GTK_WINDOW(xferData->dialog), message);
    return;
  }

  if (from == to)
  {
    const char *message = _("You can't transfer from and to the same "
                            "account!");
    gnc_error_dialog_parented(GTK_WINDOW(xferData->dialog), message);
    return;
  }

  if (!gnc_amount_edit_evaluate (GNC_AMOUNT_EDIT (xferData->amount_edit)))
  {
    gnc_parse_error_dialog (xferData, "amount");
    return;
  }

  from_currency = xaccAccountGetCurrency(from);
  to_currency = xaccAccountGetCurrency(to);

  curr_trans = !gnc_commodity_equiv(from_currency, to_currency);

  amount = gnc_amount_edit_get_amount(GNC_AMOUNT_EDIT(xferData->amount_edit));

  time = gnc_date_edit_get_date(GNC_DATE_EDIT(xferData->date_entry));

  if (curr_trans)
  {
    if (!gnc_amount_edit_evaluate (GNC_AMOUNT_EDIT (xferData->price_edit)))
    {
      if (gtk_toggle_button_get_active
          (GTK_TOGGLE_BUTTON(xferData->price_radio)))
      {
	gnc_parse_error_dialog (xferData, "price");
	return;
      }
    }

    if (!gnc_amount_edit_evaluate (GNC_AMOUNT_EDIT (xferData->to_amount_edit)))
    {
      if (gtk_toggle_button_get_active
          (GTK_TOGGLE_BUTTON(xferData->amount_radio)))
      {
	gnc_parse_error_dialog (xferData, "to_amount");
	return;
      }
    }

    curr = gnc_xfer_dialog_get_selected_curr_acct(xferData);
    if (curr == NULL)
    {
      char *message = 
	g_strdup_printf(_("No matching currency account!\n"
			  "Please create a currency account\n"
			  "with currency %s\n"
			  "and security %s\n"
			  "(or vice versa) to transfer funds\n"
			  "between the selected accounts."),
			gnc_commodity_get_printname(from_currency),
			gnc_commodity_get_printname(to_currency));

      gnc_error_dialog_parented(GTK_WINDOW(xferData->dialog), message);

      g_free(message);

      return;
    }
 
    to_amount = gnc_amount_edit_get_amount
      (GNC_AMOUNT_EDIT(xferData->to_amount_edit));
 
    /* from -> curr transaction */
    /* Create the transaction */
    trans = xaccMallocTransaction();

    xaccTransBeginEdit(trans, TRUE);
    xaccTransSetDateSecs(trans, time);

    string = gtk_entry_get_text(GTK_ENTRY(xferData->num_entry));
    xaccTransSetNum(trans, string);

    string = gtk_entry_get_text(GTK_ENTRY(xferData->description_entry));
    xaccTransSetDescription(trans, string);

    /* create first split */
    curr_split = xaccMallocSplit();
    xaccTransAppendSplit(trans, curr_split); 

    /* create second split */
    from_split = xaccMallocSplit();
    xaccTransAppendSplit(trans, from_split); 

    xaccAccountBeginEdit(curr);
    xaccAccountInsertSplit(curr, curr_split);
    xaccAccountBeginEdit(from);
    xaccAccountInsertSplit(from, from_split);

    xaccSplitSetShareAmount(from_split, gnc_numeric_neg (amount));
    xaccSplitSetBaseValue(curr_split, amount, from_currency);
    xaccSplitSetBaseValue(curr_split, to_amount, to_currency);

    /* Set the memo fields */
    string = gtk_entry_get_text(GTK_ENTRY(xferData->memo_entry));
    xaccSplitSetMemo(curr_split, string);
    xaccSplitSetMemo(from_split, string);

    /* finish transaction */
    xaccAccountCommitEdit(from);
    xaccAccountCommitEdit(curr);
    xaccTransCommitEdit(trans);

    /* curr -> to transaction */
    /* Create the transaction */
    trans = xaccMallocTransaction();

    xaccTransBeginEdit(trans, TRUE);
    xaccTransSetDateSecs(trans, time);

    string = gtk_entry_get_text(GTK_ENTRY(xferData->num_entry));
    xaccTransSetNum(trans, string);

    string = gtk_entry_get_text(GTK_ENTRY(xferData->description_entry));
    xaccTransSetDescription(trans, string);

    /* create first split */
    curr_split = xaccMallocSplit();
    xaccTransAppendSplit(trans, curr_split); 

    /* create second split */
    to_split = xaccMallocSplit();
    xaccTransAppendSplit(trans, to_split); 

    xaccAccountBeginEdit(to);
    xaccAccountInsertSplit(to, to_split);
    xaccAccountBeginEdit(curr);
    xaccAccountInsertSplit(curr, curr_split);

    xaccSplitSetShareAmount(to_split, to_amount);
    xaccSplitSetBaseValue(curr_split, gnc_numeric_neg(amount), from_currency);
    xaccSplitSetBaseValue(curr_split, gnc_numeric_neg(to_amount), to_currency);

    /* set the memo fields */
    string = gtk_entry_get_text(GTK_ENTRY(xferData->memo_entry));
    xaccSplitSetMemo(curr_split, string);
    xaccSplitSetMemo(to_split, string);

    /* finish transaction */
    xaccAccountCommitEdit(curr);
    xaccAccountCommitEdit(to);
    xaccTransCommitEdit(trans);

    /* Refresh everything */
    gnc_account_ui_refresh(to);
    gnc_account_ui_refresh(from);
    gnc_account_ui_refresh(curr);
  }
  else
  {
    /* Create the transaction */
    trans = xaccMallocTransaction();

    xaccTransBeginEdit(trans, TRUE);
    xaccTransSetDateSecs(trans, time);

    string = gtk_entry_get_text(GTK_ENTRY(xferData->num_entry));
    xaccTransSetNum(trans, string);

    string = gtk_entry_get_text(GTK_ENTRY(xferData->description_entry));
    xaccTransSetDescription(trans, string);

    /* create first split */
    to_split = xaccMallocSplit();
    xaccTransAppendSplit(trans, to_split); 
    xaccSplitSetShareAmount(to_split, amount);

    /* create second split */
    from_split = xaccMallocSplit();
    xaccSplitSetShareAmount(from_split, gnc_numeric_neg (amount));
    xaccTransAppendSplit(trans, from_split); 

    /* set the memo fields */
    string = gtk_entry_get_text(GTK_ENTRY(xferData->memo_entry));
    xaccSplitSetMemo(to_split, string);
    xaccSplitSetMemo(from_split, string);

    /* Now do the 'to' account */
    xaccAccountBeginEdit(to);
    xaccAccountInsertSplit(to, to_split);
    xaccAccountCommitEdit(to);

    /* Now do the 'from' account */
    xaccAccountBeginEdit(from);
    xaccAccountInsertSplit(from, from_split);
    xaccAccountCommitEdit(from);

    /* finish transaction */
    xaccTransCommitEdit(trans);

    /* Refresh everything */
    gnc_account_ui_refresh(to);
    gnc_account_ui_refresh(from);
  }

  gnc_refresh_main_window();

  gnome_dialog_close(GNOME_DIALOG(xferData->dialog));
}


static void
gnc_xfer_dialog_cancel_cb(GtkWidget * widget, gpointer data)
{
  XferDialog *xferData = data; 

  gnome_dialog_close(GNOME_DIALOG(xferData->dialog));
}


static int
gnc_xfer_dialog_close_cb(GnomeDialog *dialog, gpointer data)
{
  XferDialog * xferData = data;
  GtkWidget *entry;

  entry = gnc_amount_edit_gtk_entry(GNC_AMOUNT_EDIT(xferData->amount_edit));
  gtk_signal_disconnect_by_data(GTK_OBJECT(entry), xferData);

  entry = gnc_amount_edit_gtk_entry(GNC_AMOUNT_EDIT(xferData->price_edit));
  gtk_signal_disconnect_by_data(GTK_OBJECT(entry), xferData);

  entry = gnc_amount_edit_gtk_entry(GNC_AMOUNT_EDIT(xferData->to_amount_edit));
  gtk_signal_disconnect_by_data(GTK_OBJECT(entry), xferData);

  if(xferData->curr_accts_list)
  {
    g_list_foreach(xferData->curr_accts_list,
                   gnc_xfer_dialog_free_curr_accts_list, NULL);
    g_list_free(xferData->curr_accts_list);
    xferData->curr_accts_list = NULL;
  }

  xfer_dialogs = g_list_remove(xfer_dialogs, dialog);

  g_free(xferData);

  DEBUG("xfer dialog destroyed\n");

  return FALSE;
}


static void
gnc_xfer_dialog_create(GtkWidget * parent, XferDialog *xferData)
{
  GtkWidget *dialog;
  GtkTooltips *tooltips;
  GtkObject *tdo;

  dialog = create_Transfer_Dialog();
  xferData->dialog = dialog;
  tdo = GTK_OBJECT (dialog);

  /* parent */
  if (parent != NULL)
    gnome_dialog_set_parent(GNOME_DIALOG(dialog), GTK_WINDOW(parent));

  /* default to ok */
  gnome_dialog_set_default(GNOME_DIALOG(dialog), 0);

  gnome_dialog_button_connect(GNOME_DIALOG(dialog), 0,
                              GTK_SIGNAL_FUNC(gnc_xfer_dialog_ok_cb),
                              xferData);

  gnome_dialog_button_connect(GNOME_DIALOG(dialog), 1,
                              GTK_SIGNAL_FUNC(gnc_xfer_dialog_cancel_cb),
                              xferData);

  gtk_signal_connect(GTK_OBJECT(dialog), "close",
                     GTK_SIGNAL_FUNC(gnc_xfer_dialog_close_cb), xferData);

  tooltips = gtk_tooltips_new();

  /* amount & date widgets */
  {
    GtkWidget *amount;
    GtkWidget *entry;
    GtkWidget *date;
    GtkWidget *hbox;

    amount = gnc_amount_edit_new();
    hbox = gtk_object_get_data(tdo, "amount_hbox");
    gtk_box_pack_end(GTK_BOX(hbox), amount, TRUE, TRUE, 0);
    xferData->amount_edit = amount;

    entry = gnc_amount_edit_gtk_entry (GNC_AMOUNT_EDIT (amount));
    gtk_signal_connect(GTK_OBJECT(entry), "focus-out-event",
                       GTK_SIGNAL_FUNC(gnc_xfer_amount_update_cb), xferData);
    gnome_dialog_editable_enters(GNOME_DIALOG(dialog), GTK_EDITABLE(entry));

    date = gnc_date_edit_new(time(NULL), FALSE, FALSE);
    hbox = gtk_object_get_data(tdo, "date_hbox");

    gtk_box_pack_end(GTK_BOX(hbox), date, TRUE, TRUE, 0);
    xferData->date_entry = date;
  }

  {
    GtkWidget *entry;

    entry = gtk_object_get_data(tdo, "num_entry");
    xferData->num_entry = entry;
    gnome_dialog_editable_enters(GNOME_DIALOG(dialog), GTK_EDITABLE(entry));

    entry = gtk_object_get_data(tdo, "description_entry");
    xferData->description_entry = entry;
    gnome_dialog_editable_enters(GNOME_DIALOG(dialog), GTK_EDITABLE(entry));

    entry = gtk_object_get_data(tdo, "memo_entry");
    xferData->memo_entry = entry;
    gnome_dialog_editable_enters(GNOME_DIALOG(dialog), GTK_EDITABLE(entry));
  }

  /* from and to */
  {
    GtkWidget *label;

    gnc_xfer_dialog_fill_tree_frame(xferData, XFER_DIALOG_TO, tooltips);
    gnc_xfer_dialog_fill_tree_frame(xferData, XFER_DIALOG_FROM, tooltips);

    gtk_signal_connect(GTK_OBJECT(xferData->from), "select_account",
		       GTK_SIGNAL_FUNC(gnc_xfer_dialog_from_tree_select_cb),
		       xferData);
    gtk_signal_connect(GTK_OBJECT(xferData->to), "select_account",
		       GTK_SIGNAL_FUNC(gnc_xfer_dialog_to_tree_select_cb),
		       xferData);

    label = gtk_object_get_data(tdo, "from_currency_label");
    xferData->from_currency_label = label;

    label = gtk_object_get_data(tdo, "to_currency_label");
    xferData->to_currency_label = label;
  }

  /* optional intermediate currency account */
  {
    GtkWidget *frame;
    GtkWidget *label;
    GtkWidget *combo;
    GtkWidget *entry;
    GtkWidget *edit;
    GtkWidget *hbox;
    GtkWidget *button;

    frame = gtk_object_get_data(tdo, "curr_transfer_frame");
    xferData->curr_transfer_frame = frame;

    label = gtk_object_get_data(tdo, "curr_acct_label");
    xferData->curr_acct_label = label;

    combo = gtk_object_get_data(tdo, "curr_acct_combo");
    xferData->curr_acct_combo = combo;

    xferData->curr_accts_list = NULL;

    entry = gtk_object_get_data(tdo, "curr_acct_combo_entry");
    xferData->curr_acct_combo_entry = entry;

    edit = gnc_amount_edit_new();
    gnc_amount_edit_set_print_info(GNC_AMOUNT_EDIT(edit),
                                   gnc_default_price_print_info ());
    hbox = gtk_object_get_data(tdo, "price_hbox");
    gtk_box_pack_start(GTK_BOX(hbox), edit, TRUE, TRUE, 0);
    xferData->price_edit = edit;
    entry = gnc_amount_edit_gtk_entry (GNC_AMOUNT_EDIT (edit));
    gtk_signal_connect(GTK_OBJECT(entry), "focus-out-event",
                       GTK_SIGNAL_FUNC(gnc_xfer_price_update_cb), xferData);
    gnome_dialog_editable_enters(GNOME_DIALOG(dialog), GTK_EDITABLE(entry));

    edit = gnc_amount_edit_new();
    hbox = gtk_object_get_data(tdo, "to_amount_hbox");
    gtk_box_pack_start(GTK_BOX(hbox), edit, TRUE, TRUE, 0);
    xferData->to_amount_edit = edit;
    entry = gnc_amount_edit_gtk_entry (GNC_AMOUNT_EDIT (edit));
    gtk_signal_connect(GTK_OBJECT(entry), "focus-out-event",
                       GTK_SIGNAL_FUNC(gnc_xfer_to_amount_update_cb),
		       xferData);
    gnome_dialog_editable_enters(GNOME_DIALOG(dialog), GTK_EDITABLE(entry));

    button = gtk_object_get_data(tdo, "price_radio");
    xferData->price_radio = button;
    gtk_signal_connect(GTK_OBJECT(xferData->price_radio), "toggled",
		       GTK_SIGNAL_FUNC(price_amount_radio_toggled_cb),
		       xferData);

    button = gtk_object_get_data(tdo, "amount_radio");
    xferData->amount_radio = button;
    gtk_signal_connect(GTK_OBJECT(xferData->amount_radio), "toggled",
		       GTK_SIGNAL_FUNC(price_amount_radio_toggled_cb),
		       xferData);
  }
}


/********************************************************************\
 * gnc_xfer_dialog                                                  *
 *   opens up a window to do an automatic transfer between accounts *
 *                                                                  * 
 * Args:   parent  - the parent of the window to be created         *
 *         initial - the initial account in the from/to fields      *
 * Return: XferDialog structure                                     *
\********************************************************************/
XferDialog *
gnc_xfer_dialog(GtkWidget * parent, Account * initial)
{
  XferDialog *xferData;
  GNCAmountEdit *gae;
  GtkWidget *amount_entry;

  xferData = g_new0(XferDialog, 1);

  gnc_xfer_dialog_create(parent, xferData);

  xfer_dialogs = g_list_prepend(xfer_dialogs, xferData->dialog);

  gae = GNC_AMOUNT_EDIT(xferData->amount_edit);
  amount_entry = gnc_amount_edit_gtk_entry (gae);

  gtk_widget_grab_focus(amount_entry);

  gnc_xfer_dialog_select_from_account(xferData, initial);
  gnc_xfer_dialog_select_to_account(xferData, initial);

  if (initial == NULL)
    gnc_xfer_dialog_curr_acct_activate(xferData);

  gtk_widget_show_all(xferData->dialog);

  gnc_window_adjust_for_screen(GTK_WINDOW(xferData->dialog));

  return xferData;
}


/********************************************************************\
 * gnc_ui_destroy_xfer_windows                                      *
 *   destroy all open transfer dialogs                              *
 *                                                                  *
 * Args:   none                                                     *
 * Return: none                                                     *
\********************************************************************/
void
gnc_ui_destroy_xfer_windows(void)
{
  GnomeDialog *dialog;

  while (xfer_dialogs != NULL)
  {
    dialog = GNOME_DIALOG(xfer_dialogs->data);

    gnome_dialog_close(dialog);
  }
}
