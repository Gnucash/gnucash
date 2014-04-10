/********************************************************************\
 * dialog-transfer.c -- transfer dialog for GnuCash                 *
 * Copyright (C) 1999 Linas Vepstas                                 *
 * Copyright (C) 2000 Dave Peticolas                                *
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
#include "ui-callbacks.h"
#include "util.h"


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

  GtkWidget * from_show_button;
  GtkWidget * to_show_button;
};


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
gnc_xfer_dialog_fill_tree_frame(XferDialog *xferData,
                                XferDirection direction,
                                GtkTooltips *tooltips)
{
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
  gtk_tooltips_set_tip(tooltips, button, SHOW_INC_EXP_MSG, NULL);

  gtk_signal_connect(GTK_OBJECT(button), "toggled",
		     GTK_SIGNAL_FUNC(gnc_xfer_dialog_toggle_cb), tree);
}


static void
gnc_parse_error_dialog (XferDialog *xferData)
{
  const char *error_string;
  char * error_phrase;

  error_string = gnc_exp_parser_error_string ();
  if (error_string == NULL)
    error_string = "";

  error_phrase = g_strdup_printf(ERROR_IN_AMOUNT, error_string);

  gnc_error_dialog_parented(GTK_WINDOW(xferData->dialog), error_phrase);

  g_free(error_phrase);
}


static gboolean
gnc_xfer_update_cb(GtkWidget *widget, GdkEventFocus *event, gpointer data)
{
  XferDialog * xferData = data;
  Account    * account;
  const gnc_commodity * currency;

  account = gnc_account_tree_get_current_account(xferData->from);
  if (account == NULL)
    account = gnc_account_tree_get_current_account(xferData->to);

  currency = xaccAccountGetCurrency(account);
  
  gnc_amount_edit_set_currency (GNC_AMOUNT_EDIT (xferData->amount_edit),
                                gnc_commodity_get_printname(currency));
  gnc_amount_edit_evaluate (GNC_AMOUNT_EDIT (xferData->amount_edit));
  
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
gnc_xfer_dialog_set_amount(XferDialog *xferData, double amount)
{
  Account * account;
  const gnc_commodity * currency;

  if (xferData == NULL)
    return;

  account = gnc_account_tree_get_current_account(xferData->from);
  if (account == NULL)
    account = gnc_account_tree_get_current_account(xferData->to);
  
  currency = xaccAccountGetCurrency(account);

  gnc_amount_edit_set_currency (GNC_AMOUNT_EDIT (xferData->amount_edit),
                                gnc_commodity_get_printname(currency));
  
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
  Account *from, *to;
  char * string;
  double amount;
  time_t time;

  Transaction *trans;
  Split *from_split;
  Split *to_split;


  from = gnc_account_tree_get_current_account(xferData->from);
  to   = gnc_account_tree_get_current_account(xferData->to);

  if ((from == NULL) || (to == NULL))
  {
    gnc_error_dialog_parented(GTK_WINDOW(xferData->dialog), XFER_NO_ACC_MSG);
    return;
  }

  if (from == to)
  {
    gnc_error_dialog_parented(GTK_WINDOW(xferData->dialog), XFER_SAME_MSG);
    return;
  }

  if (!xaccAccountsHaveCommonCurrency(from, to))
  {
    gnc_error_dialog_parented(GTK_WINDOW(xferData->dialog), XFER_CURR_MSG);
    return;
  }

  if (!gnc_amount_edit_evaluate (GNC_AMOUNT_EDIT (xferData->amount_edit)))
  {
    gnc_parse_error_dialog (xferData);
    return;
  }

  amount = gnc_amount_edit_get_amount(GNC_AMOUNT_EDIT(xferData->amount_edit));

  time = gnc_date_edit_get_date(GNC_DATE_EDIT(xferData->date_entry));

  /* Create the transaction */
  trans = xaccMallocTransaction();

  xaccTransBeginEdit(trans, TRUE);
  xaccTransSetDateSecs(trans, time);

  string = gtk_entry_get_text(GTK_ENTRY(xferData->num_entry));
  xaccTransSetNum(trans, string);

  string = gtk_entry_get_text(GTK_ENTRY(xferData->description_entry));
  xaccTransSetDescription(trans, string);

  /* first split is already there */
  to_split = xaccTransGetSplit(trans, 0);
  DxaccSplitSetShareAmount(to_split, amount);

  /* second split must be created */
  from_split = xaccMallocSplit();
  DxaccSplitSetShareAmount(from_split, -amount);
  xaccTransAppendSplit(trans, from_split); 

  /* TransSetMemo will set the memo for both splits */
  string = gtk_entry_get_text(GTK_ENTRY(xferData->memo_entry));
  xaccTransSetMemo(trans, string);

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

  entry = gnc_amount_edit_gtk_entry (GNC_AMOUNT_EDIT (xferData->amount_edit));

  gtk_signal_disconnect_by_data(GTK_OBJECT(entry), xferData);

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
    gnc_amount_edit_set_print_flags (GNC_AMOUNT_EDIT(amount), PRTSEP);

    hbox = gtk_object_get_data(tdo, "amount_hbox");
    gtk_box_pack_end(GTK_BOX(hbox), amount, TRUE, TRUE, 0);
    xferData->amount_edit = amount;

    entry = gnc_amount_edit_gtk_entry (GNC_AMOUNT_EDIT (amount));
    gtk_signal_connect(GTK_OBJECT(entry), "focus-out-event",
                       GTK_SIGNAL_FUNC(gnc_xfer_update_cb), xferData);
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
  gnc_xfer_dialog_fill_tree_frame(xferData, XFER_DIALOG_TO, tooltips);
  gnc_xfer_dialog_fill_tree_frame(xferData, XFER_DIALOG_FROM, tooltips);
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
