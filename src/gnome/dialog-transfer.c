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
#include "gnc-dateedit.h"
#include "enriched-messages.h"
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

  GtkWidget * amount_entry;
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


static GtkWidget *
gnc_xfer_dialog_create_tree_frame(gchar *title,
                                  GNCAccountTree **set_tree,
                                  GtkWidget **set_show_button,
                                  GtkTooltips *tooltips)
{
  GtkWidget *frame, *scrollWin, *accountTree, *vbox, *button;

  frame = gtk_frame_new(title);

  vbox = gtk_vbox_new(FALSE, 5);
  gtk_container_add(GTK_CONTAINER(frame), vbox);
  gtk_container_set_border_width(GTK_CONTAINER(vbox), 10);

  accountTree = gnc_account_tree_new();
  *set_tree = GNC_ACCOUNT_TREE(accountTree);
  gtk_clist_column_titles_hide(GTK_CLIST(accountTree));
  gnc_account_tree_hide_all_but_name(GNC_ACCOUNT_TREE(accountTree));
  gnc_account_tree_hide_income_expense(GNC_ACCOUNT_TREE(accountTree));
  gnc_account_tree_refresh(GNC_ACCOUNT_TREE(accountTree));

  scrollWin = gtk_scrolled_window_new (NULL, NULL);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW (scrollWin),
				 GTK_POLICY_AUTOMATIC, 
				 GTK_POLICY_AUTOMATIC);

  gtk_box_pack_start(GTK_BOX(vbox), scrollWin, TRUE, TRUE, 0);
  gtk_container_add(GTK_CONTAINER(scrollWin), accountTree);

  {
    GtkStyle *st = gtk_widget_get_style(accountTree);
    GdkFont *font = NULL;
    gint height;

    if (st != NULL)
      font = st->font;

    if (font != NULL)
    {
      height = gdk_char_height(font, 'X');
      gtk_widget_set_usize(scrollWin, 0, (height + 6) * 10);
    }
  }

  button = gtk_check_button_new_with_label(SHOW_INC_EXP_STR);
  *set_show_button = button;
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(button), FALSE);
  gtk_box_pack_start(GTK_BOX(vbox), button, FALSE, FALSE, 0);
  gtk_tooltips_set_tip(tooltips, button, SHOW_INC_EXP_MSG, NULL);

  gtk_signal_connect(GTK_OBJECT(button), "toggled",
		     GTK_SIGNAL_FUNC(gnc_xfer_dialog_toggle_cb), accountTree);

  return frame;
}


static gboolean
gnc_xfer_update_cb(GtkWidget *widget, GdkEventFocus *event, gpointer data)
{
  GtkEntry *entry = GTK_ENTRY(widget);
  XferDialog *xferData = data;
  Account *account;
  const char *new_string;
  const char *currency;
  const char *string;
  double value;

  account = gnc_account_tree_get_current_account(xferData->from);
  if (account == NULL)
    account = gnc_account_tree_get_current_account(xferData->to);

  string = gtk_entry_get_text(entry);

  if ((string == NULL) || (*string == 0))
    return FALSE;

  value = 0.0;
  xaccParseAmount(string, TRUE, &value, NULL);

  currency = xaccAccountGetCurrency(account);

  new_string = xaccPrintAmount(value, PRTSEP, currency);

  if (safe_strcmp(string, new_string) == 0)
    return FALSE;

  gtk_entry_set_text(entry, new_string);

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
  Account *account;
  const char *currency;
  const char *string;

  if (xferData == NULL)
    return;

  account = gnc_account_tree_get_current_account(xferData->from);
  if (account == NULL)
    account = gnc_account_tree_get_current_account(xferData->to);

  currency = xaccAccountGetCurrency(account);

  string = xaccPrintAmount(amount, PRTSEP, currency);

  gtk_entry_set_text(GTK_ENTRY(xferData->amount_entry), string);
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

  string = gtk_entry_get_text(GTK_ENTRY(xferData->amount_entry));
  amount = 0.0;
  xaccParseAmount(string, TRUE, &amount, NULL);

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
  xaccSplitSetShareAmount(to_split, amount);

  /* second split must be created */
  from_split = xaccMallocSplit();
  xaccSplitSetShareAmount(from_split, -amount);
  xaccTransAppendSplit(trans, from_split); 

  /* TransSetMemo will set the memo for both splits */
  string = gtk_entry_get_text(GTK_ENTRY(xferData->memo_entry));
  xaccTransSetMemo(trans, string);

  /* Now do the 'to' account */
  xaccAccountBeginEdit(to, FALSE);
  xaccAccountInsertSplit(to, to_split);
  xaccAccountCommitEdit(to);

  /* Now do the 'from' account */
  xaccAccountBeginEdit(from, FALSE);
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
  
  dialog = gnome_dialog_new(TRANSFER_STR,
			    GNOME_STOCK_BUTTON_OK,
			    GNOME_STOCK_BUTTON_CANCEL,
			    NULL);

  xferData->dialog = dialog;

  /* parent */
  if (parent != NULL)
    gnome_dialog_set_parent(GNOME_DIALOG(dialog), GTK_WINDOW(parent));

  /* default to ok */
  gnome_dialog_set_default(GNOME_DIALOG(dialog), 0);

  /* destroy on close */
  gnome_dialog_close_hides(GNOME_DIALOG(dialog), FALSE);

  /* allow grow and shrink, no auto-shrink */
  gtk_window_set_policy(GTK_WINDOW(dialog), TRUE, TRUE, FALSE);

  gnome_dialog_button_connect(GNOME_DIALOG(dialog), 0,
                              GTK_SIGNAL_FUNC(gnc_xfer_dialog_ok_cb),
                              xferData);

  gnome_dialog_button_connect(GNOME_DIALOG(dialog), 1,
                              GTK_SIGNAL_FUNC(gnc_xfer_dialog_cancel_cb),
                              xferData);

  gtk_signal_connect(GTK_OBJECT(dialog), "close",
                     GTK_SIGNAL_FUNC(gnc_xfer_dialog_close_cb), xferData);

  tooltips = gtk_tooltips_new();

  /* contains amount, date, num, description, and memo */
  {
    GtkWidget *frame, *vbox, *hbox, *label;

    frame = gtk_frame_new(XFER_INFO);
    gtk_container_set_border_width(GTK_CONTAINER(frame), 5);

    gtk_box_pack_start(GTK_BOX(GNOME_DIALOG(dialog)->vbox),
		       frame, TRUE, TRUE, 0);

    vbox = gtk_vbox_new(FALSE, 6);
    gtk_container_add(GTK_CONTAINER(frame), vbox);
    gtk_container_set_border_width(GTK_CONTAINER(vbox), 10);

    /* Contains amount and date */
    hbox = gtk_hbox_new(FALSE, 5);
    gtk_box_pack_start(GTK_BOX(vbox), hbox, TRUE, TRUE, 0);

    {
      GtkWidget *amount;

      label = gtk_label_new(AMOUNT_C_STR);
      gtk_misc_set_alignment(GTK_MISC(label), 1.0, 0.5);
      gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 0);

      amount = gtk_entry_new();
      gtk_box_pack_start(GTK_BOX(hbox), amount, TRUE, TRUE, 0);
      xferData->amount_entry = amount;

      gtk_signal_connect(GTK_OBJECT(amount), "focus-out-event",
                         GTK_SIGNAL_FUNC(gnc_xfer_update_cb), xferData);

      gnome_dialog_editable_enters(GNOME_DIALOG(dialog), GTK_EDITABLE(amount));
    }

    {
      GtkWidget *date;

      label = gtk_label_new(DATE_C_STR);
      gtk_misc_set_alignment(GTK_MISC(label), 1.0, 0.5);
      gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 0);

      date = gnc_date_edit_new(time(NULL), FALSE, FALSE);
      gtk_box_pack_start(GTK_BOX(hbox), date, TRUE, TRUE, 0);
      xferData->date_entry = date;
    }

    {
      GtkWidget *sep;

      sep = gtk_hseparator_new();
      gtk_box_pack_start(GTK_BOX(vbox), sep, TRUE, TRUE, 3);
    }

    /* Contains num, description, and memo */
    hbox = gtk_hbox_new(FALSE, 5);
    gtk_box_pack_start(GTK_BOX(vbox), hbox, TRUE, TRUE, 0);

    {
      GtkWidget *vbox;
      gchar *string;

      vbox = gtk_vbox_new(TRUE, 5);
      gtk_box_pack_start(GTK_BOX(hbox), vbox, FALSE, FALSE, 0);

      string = g_strconcat(NUM_STR, ":", NULL);
      label = gtk_label_new(string);
      g_free(string);
      gtk_misc_set_alignment(GTK_MISC(label), 1.0, 0.5);
      gtk_box_pack_start(GTK_BOX(vbox), label, FALSE, FALSE, 0);

      label = gtk_label_new(DESC_C_STR);
      gtk_misc_set_alignment(GTK_MISC(label), 1.0, 0.5);
      gtk_box_pack_start(GTK_BOX(vbox), label, FALSE, FALSE, 0);

      label = gtk_label_new(MEMO_C_STR);
      gtk_misc_set_alignment(GTK_MISC(label), 1.0, 0.5);
      gtk_box_pack_start(GTK_BOX(vbox), label, FALSE, FALSE, 0);
    }

    {
      GtkWidget *vbox, *entry;

      vbox = gtk_vbox_new(TRUE, 5);
      gtk_box_pack_start(GTK_BOX(hbox), vbox, TRUE, TRUE, 0);

      entry = gtk_entry_new();
      gtk_box_pack_start(GTK_BOX(vbox), entry, TRUE, TRUE, 0);
      xferData->num_entry = entry;
      gnome_dialog_editable_enters(GNOME_DIALOG(dialog), GTK_EDITABLE(entry));

      entry = gtk_entry_new();
      gtk_box_pack_start(GTK_BOX(vbox), entry, TRUE, TRUE, 0);
      xferData->description_entry = entry;
      gnome_dialog_editable_enters(GNOME_DIALOG(dialog), GTK_EDITABLE(entry));

      entry = gtk_entry_new();
      gtk_box_pack_start(GTK_BOX(vbox), entry, TRUE, TRUE, 0);
      xferData->memo_entry = entry;
      gnome_dialog_editable_enters(GNOME_DIALOG(dialog), GTK_EDITABLE(entry));
    }
  }

  /* Contains from and to */
  {
    GtkWidget *hbox, *tree;

    hbox = gtk_hbox_new(TRUE, 5);
    gtk_box_pack_start(GTK_BOX(GNOME_DIALOG(dialog)->vbox),
		       hbox, TRUE, TRUE, 0);

    tree = gnc_xfer_dialog_create_tree_frame(XFRM_STR,
                                             &xferData->from,
                                             &xferData->from_show_button,
                                             tooltips);
    gtk_box_pack_start(GTK_BOX(hbox), tree, TRUE, TRUE, 0);

    tree = gnc_xfer_dialog_create_tree_frame(XFTO_STR,
                                             &xferData->to,
                                             &xferData->to_show_button,
                                             tooltips);
    gtk_box_pack_start(GTK_BOX(hbox), tree, TRUE, TRUE, 0);
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

  xferData = g_new0(XferDialog, 1);

  gnc_xfer_dialog_create(parent, xferData);

  xfer_dialogs = g_list_prepend(xfer_dialogs, xferData->dialog);

  gtk_widget_grab_focus(xferData->amount_entry);

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
gnc_ui_destroy_xfer_windows()
{
  GnomeDialog *dialog;

  while (xfer_dialogs != NULL)
  {
    dialog = GNOME_DIALOG(xfer_dialogs->data);

    gnome_dialog_close(dialog);
  }
}
