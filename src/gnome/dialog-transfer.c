/********************************************************************\
 * dialog-transfer.c -- transfer dialog for GnuCash                 *
 * Copyright (C) 1999 Linas Vepstas                                 *
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


/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_GUI;

typedef struct _xferDialog XferDialog;
struct _xferDialog
{
  GtkWidget * dialog;
  GtkWidget * amount_entry;
  GtkWidget * date_entry;
  GtkWidget * description_entry;
  GtkWidget * memo_entry;

  GNCAccountTree * from;
  GNCAccountTree * to;
};


static void
gnc_xfer_dialog_toggle_cb(GtkToggleButton *button, gpointer data)
{
  GNCAccountTree *tree = GNC_ACCOUNT_TREE(data);

  if (gtk_toggle_button_get_active(button))
    gnc_account_tree_show_categories(tree);
  else
    gnc_account_tree_hide_categories(tree);
}


static GtkWidget *
gnc_xfer_dialog_create_tree_frame(Account *initial, gchar *title,
				  GNCAccountTree **set_tree,
                                  GtkTooltips *tooltips)
{
  GtkWidget *frame, *scrollWin, *accountTree, *vbox, *button;
  gboolean is_category;
  int type;

  frame = gtk_frame_new(title);

  vbox = gtk_vbox_new(FALSE, 5);
  gtk_container_add(GTK_CONTAINER(frame), vbox);
  gtk_container_set_border_width(GTK_CONTAINER(vbox), 10);

  type = xaccAccountGetType(initial);
  is_category = (type == EXPENSE) || (type == INCOME);

  accountTree = gnc_account_tree_new();
  *set_tree = GNC_ACCOUNT_TREE(accountTree);
  gtk_clist_column_titles_hide(GTK_CLIST(accountTree));
  gnc_account_tree_hide_all_but_name(GNC_ACCOUNT_TREE(accountTree));
  if (!is_category)
    gnc_account_tree_hide_categories(GNC_ACCOUNT_TREE(accountTree));
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

  button = gtk_check_button_new_with_label(SHOW_CATEGORIES_STR);
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(button), is_category);
  gtk_box_pack_start(GTK_BOX(vbox), button, FALSE, FALSE, 0);
  gtk_tooltips_set_tip(tooltips, button, SHOW_CAT_MSG, NULL);

  gtk_signal_connect(GTK_OBJECT(button), "toggled",
		     GTK_SIGNAL_FUNC(gnc_xfer_dialog_toggle_cb),
		     (gpointer) accountTree);

  return frame;
}


static gboolean
gnc_xfer_update_cb(GtkWidget *widget, GdkEventFocus *event, gpointer data)
{
  GtkEntry *entry = GTK_ENTRY(widget);
  XferDialog *xferData = data;
  Account *account;
  gchar *new_string;
  gchar *currency;
  gchar *string;
  double value;

  account = gnc_account_tree_get_current_account(xferData->from);
  if (account == NULL)
    account = gnc_account_tree_get_current_account(xferData->to);

  string = gtk_entry_get_text(entry);

  if ((string == NULL) || (*string == 0))
    return FALSE;

  value = xaccParseAmount(string, GNC_T);

  currency = xaccAccountGetCurrency(account);

  new_string = xaccPrintAmount(value, PRTSEP, currency);

  if (safe_strcmp(string, new_string) == 0)
    return FALSE;

  gtk_entry_set_text(entry, new_string);

  return FALSE;
}


static GtkWidget *
gnc_xfer_dialog_create(GtkWidget * parent, Account * initial,
		       XferDialog *xferData)
{
  GtkWidget *dialog;
  GtkTooltips *tooltips;
  
  dialog = gnome_dialog_new(TRANSFER_STR,
			    GNOME_STOCK_BUTTON_OK,
			    GNOME_STOCK_BUTTON_CANCEL,
			    NULL);

  xferData->dialog = dialog;

  /* Make this dialog modal */
  gtk_window_set_modal(GTK_WINDOW(dialog), TRUE);

  /* parent */
  gnome_dialog_set_parent(GNOME_DIALOG(dialog), GTK_WINDOW(parent));

  /* default to ok */
  gnome_dialog_set_default(GNOME_DIALOG(dialog), 0);

  /* don't close on buttons */
  gnome_dialog_set_close(GNOME_DIALOG(dialog), FALSE);

  tooltips = gtk_tooltips_new();

  /* contains amount, date, description, and notes */
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

    /* Contains description and memo */
    hbox = gtk_hbox_new(FALSE, 5);
    gtk_box_pack_start(GTK_BOX(vbox), hbox, TRUE, TRUE, 0);

    {
      GtkWidget *vbox;

      vbox = gtk_vbox_new(TRUE, 5);
      gtk_box_pack_start(GTK_BOX(hbox), vbox, FALSE, FALSE, 0);

      label = gtk_label_new(DESC_C_STR);
      gtk_misc_set_alignment(GTK_MISC(label), 1.0, 0.5);
      gtk_box_pack_start(GTK_BOX(vbox), label, FALSE, FALSE, 0);

      label = gtk_label_new(MEMO_C_STR);
      gtk_misc_set_alignment(GTK_MISC(label), 1.0, 0.5);
      gtk_box_pack_start(GTK_BOX(vbox), label, FALSE, FALSE, 0);
    }

    {
      GtkWidget *vbox, *desc, *memo;

      vbox = gtk_vbox_new(TRUE, 5);
      gtk_box_pack_start(GTK_BOX(hbox), vbox, TRUE, TRUE, 0);

      desc = gtk_entry_new();
      gtk_box_pack_start(GTK_BOX(vbox), desc, TRUE, TRUE, 0);
      xferData->description_entry = desc;
      gnome_dialog_editable_enters(GNOME_DIALOG(dialog), GTK_EDITABLE(desc));

      memo = gtk_entry_new();
      gtk_box_pack_start(GTK_BOX(vbox), memo, TRUE, TRUE, 0);
      xferData->memo_entry = memo;
      gnome_dialog_editable_enters(GNOME_DIALOG(dialog), GTK_EDITABLE(memo));
    }
  }

  /* Contains from and to */
  {
    GtkWidget *hbox, *tree;

    hbox = gtk_hbox_new(TRUE, 5);
    gtk_box_pack_start(GTK_BOX(GNOME_DIALOG(dialog)->vbox),
		       hbox, TRUE, TRUE, 0);

    tree = gnc_xfer_dialog_create_tree_frame(initial, XFRM_STR,
                                             &xferData->from, tooltips);
    gtk_box_pack_start(GTK_BOX(hbox), tree, TRUE, TRUE, 0);

    tree = gnc_xfer_dialog_create_tree_frame(initial, XFTO_STR,
					     &xferData->to, tooltips);
    gtk_box_pack_start(GTK_BOX(hbox), tree, TRUE, TRUE, 0);
  }

  /* allow grow and shrink, no auto-shrink */
  gtk_window_set_policy(GTK_WINDOW(dialog), TRUE, TRUE, FALSE);

  return dialog;
}


/********************************************************************\
 * xferWindow                                                       *
 *   opens up a window to do an automatic transfer between accounts *
 *                                                                  * 
 * Args:   parent  - the parent of the window to be created         *
 *         initial - the initial account in the from/to fields      *
 *         group   - the group from which the accounts are taken    *
 * Return: none                                                     *
\********************************************************************/
void 
gnc_xfer_dialog(GtkWidget * parent, Account * initial)
{
  Account *from, *to;
  GtkWidget *dialog;
  XferDialog xferData;
  char * string;
  double amount;
  time_t time;
  gint result;

  if (xaccGroupGetNumAccounts(gncGetCurrentGroup()) < 2)
  {
    gnc_error_dialog(XFER_NSF_MSG);
    return;
  }

  dialog = gnc_xfer_dialog_create(parent, initial, &xferData);

  gtk_widget_show_all(dialog);

  gtk_widget_grab_focus(xferData.amount_entry);

  gnc_account_tree_select_account(xferData.from, initial, TRUE);
  gnc_account_tree_select_account(xferData.to, initial, TRUE);

  while (1)
  {
    result = gnome_dialog_run(GNOME_DIALOG(dialog));

    if (result != 0)
      break;

    from = gnc_account_tree_get_current_account(xferData.from);
    to   = gnc_account_tree_get_current_account(xferData.to);

    if ((from == NULL) || (to == NULL))
    {
      gnc_error_dialog_parented(GTK_WINDOW(dialog), XFER_NO_ACC_MSG);
      continue;
    }

    if (from == to)
    {
      gnc_error_dialog_parented(GTK_WINDOW(dialog), XFER_SAME_MSG);
      continue;
    }

    if (!xaccAccountsHaveCommonCurrency(from, to))
    {
      gnc_error_dialog_parented(GTK_WINDOW(dialog), XFER_CURR_MSG);
      continue;
    }

    string = gtk_entry_get_text(GTK_ENTRY(xferData.amount_entry));
    amount = xaccParseAmount(string, GNC_T);

    time = gnc_date_edit_get_date(GNC_DATE_EDIT(xferData.date_entry));

    {
      Transaction *trans;
      Split *to_split, *from_split;

      /* Create the transaction */
      trans = xaccMallocTransaction();

      xaccTransBeginEdit(trans, GNC_T);
      xaccTransSetDateSecs(trans, time);

      string = gtk_entry_get_text(GTK_ENTRY(xferData.description_entry));
      xaccTransSetDescription(trans, string);

      /* first split is already there */
      to_split = xaccTransGetSplit(trans, 0);
      xaccSplitSetShareAmount(to_split, amount);

      /* second split must be created */
      from_split = xaccMallocSplit();
      xaccSplitSetShareAmount(from_split, -amount);
      xaccTransAppendSplit(trans, from_split); 

      /* TransSetMemo will set the memo for both splits */
      string = gtk_entry_get_text(GTK_ENTRY(xferData.memo_entry));
      xaccTransSetMemo(trans, string);

      /* Now do the 'to' account */
      xaccAccountBeginEdit(to, GNC_F);
      xaccAccountInsertSplit(to, to_split);
      xaccAccountCommitEdit(to);

      /* Now do the 'from' account */
      xaccAccountBeginEdit(from, GNC_F);
      xaccAccountInsertSplit(from, from_split);
      xaccAccountCommitEdit(from);

      /* finish transaction */
      xaccTransCommitEdit(trans);

      /* Refresh everything */
      gnc_account_ui_refresh(to);
      gnc_account_ui_refresh(from);
      gnc_refresh_main_window();

      break;
    }
  }

  DEBUG("destroying transfer dialog\n");

  gtk_widget_destroy(dialog);
}
