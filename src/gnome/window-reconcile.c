/********************************************************************\
 * window-reconcile.c -- the reconcile window                       *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1998-2000 Linas Vepstas                            *
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
 *                                                                  *
 *   Author: Rob Clark                                              *
 * Internet: rclark@cs.hmc.edu                                      *
 *  Address: 609 8th Street                                         *
 *           Huntington Beach, CA 92648-4632                        *
\********************************************************************/

#define _GNU_SOURCE

#include "config.h"

#include <gnome.h>
#include <stdio.h>

#include "AccWindow.h"
#include "MainWindow.h"
#include "MultiLedger.h"
#include "Refresh.h"
#include "RegWindow.h"
#include "Scrub.h"
#include "date.h"
#include "dialog-transfer.h"
#include "dialog-utils.h"
#include "global-options.h"
#include "gnc-amount-edit.h"
#include "gnc-component-manager.h"
#include "gnc-dateedit.h"
#include "gnc-engine-util.h"
#include "gnc-ui-util.h"
#include "gnc-ui.h"
#include "guile-util.h"
#include "messages.h"
#include "query-user.h"
#include "reconcile-list.h"
#include "window-help.h"
#include "window-reconcile.h"
#include "window-register.h"


#define WINDOW_RECONCILE_CM_CLASS "window-reconcile"

/** STRUCTS *********************************************************/
struct _RecnWindow
{
  GUID account;             /* The account that we are reconciling  */
  gnc_numeric new_ending;   /* The new ending balance               */
  time_t statement_date;    /* The statement date                   */
  gboolean use_shares;      /* Use share balances                   */

  sort_type_t debit_sort;   /* Sorting style of the debit list      */
  sort_type_t credit_sort;  /* Sorting style of the credit list     */

  GtkWidget *window;        /* The reconcile window                 */

  GtkWidget *toolbar;       /* Toolbar widget                       */
  SCM toolbar_change_cb_id; /* id for toolbar preference change cb  */

  GtkWidget *starting;      /* The starting balance                 */
  GtkWidget *ending;        /* The ending balance                   */
  GtkWidget *reconciled;    /* The reconciled balance               */
  GtkWidget *difference;    /* Text field, amount left to reconcile */

  GtkWidget *total_debit;   /* Text field, total debit reconciled   */
  GtkWidget *total_credit;  /* Text field, total credit reconciled  */

  GtkWidget *debit;         /* Debit matrix show unreconciled debit */
  GtkWidget *credit;        /* Credit matrix, shows credits...      */

  GtkWidget *debit_frame;   /* Frame around debit matrix            */
  GtkWidget *credit_frame;  /* Frame around credit matrix           */

  SCM title_change_cb_id;   /* id for label preference cb           */

  GtkWidget *edit_item;     /* Edit transaction menu item           */
  GtkWidget *delete_item;   /* Delete transaction menu item         */

  GtkWidget *sort_debits_formal;    /* Sort debits menu formal      */
  GtkWidget *sort_credits_formal;   /* Sort credits menu formal     */

  GtkWidget *sort_debits_informal;  /* Sort debits menu informal    */
  GtkWidget *sort_credits_informal; /* Sort credits menu informal   */

  GtkWidget *edit_popup;    /* Edit transaction popup menu item     */
  GtkWidget *delete_popup;  /* Delete transaction popup menu item   */

  GtkWidget *edit_button;   /* Edit transaction button              */
  GtkWidget *delete_button; /* Delete transaction button            */
  GtkWidget *finish_button; /* Finish reconciliation button         */

  gboolean delete_refresh;  /* do a refresh upon a window deletion  */
};


/** PROTOTYPES ******************************************************/
static gnc_numeric recnRecalculateBalance (RecnWindow *recnData);

static void   recn_destroy_cb (GtkWidget *w, gpointer data);
static void   recnFinishCB(GtkWidget *w, gpointer data);
static void   recnCancelCB(GtkWidget *w, gpointer data);

static void   gnc_reconcile_window_set_sensitivity(RecnWindow *recnData);
static char * gnc_recn_make_window_name(Account *account);
static void   gnc_recn_set_window_name(RecnWindow *recnData);
static gboolean find_by_account (gpointer find_data, gpointer user_data);


/** GLOBALS *********************************************************/
/* This static indicates the debugging module that this .o belongs to. */
/* static short module = MOD_GUI; */


/** IMPLEMENTATIONS *************************************************/


/********************************************************************\
 * recnRefresh                                                      *
 *   refreshes the transactions in the reconcile window             *
 *                                                                  *
 * Args:   account - the account of the reconcile window to refresh *
 * Return: none                                                     *
\********************************************************************/
static void
recnRefresh (RecnWindow *recnData)
{
  if (recnData == NULL)
    return;

  gnc_reconcile_list_refresh(GNC_RECONCILE_LIST(recnData->debit));
  gnc_reconcile_list_refresh(GNC_RECONCILE_LIST(recnData->credit));

  gnc_reconcile_window_set_sensitivity(recnData);

  gnc_recn_set_window_name(recnData);

  recnRecalculateBalance(recnData);

  gtk_widget_queue_resize(recnData->window);
}


static Account *
recn_get_account (RecnWindow *recnData)
{
  if (!recnData)
    return NULL;

  return xaccAccountLookup (&recnData->account);
}

/********************************************************************\
 * recnRecalculateBalance                                           *
 *   refreshes the balances in the reconcile window                 *
 *                                                                  *
 * Args:   recnData -- the reconcile window to refresh              *
 * Return: the difference between the nominal ending balance        *
 *         and the 'effective' ending balance.                      *
\********************************************************************/
static gnc_numeric
recnRecalculateBalance(RecnWindow *recnData)
{
  Account *account;
  const char *amount;
  gnc_numeric debit;
  gnc_numeric credit;
  gnc_numeric starting;
  gnc_numeric ending;
  gnc_numeric reconciled;
  gnc_numeric diff;
  GNCPrintAmountInfo print_info;
  gboolean reverse_balance;

  account = recn_get_account (recnData);
  if (!account)
    return gnc_numeric_zero ();

  reverse_balance = gnc_reverse_balance(account);

  /* update the starting balance */
  if (recnData->use_shares)
  {
    starting = xaccAccountGetShareReconciledBalance(account);
    print_info = gnc_account_quantity_print_info (account, TRUE);
  }
  else
  {
    starting = xaccAccountGetReconciledBalance(account);
    print_info = gnc_account_value_print_info (account, TRUE);
  }

  if (reverse_balance)
    starting = gnc_numeric_neg (starting);

  amount = xaccPrintAmount(starting, print_info);
  gnc_set_label_color(recnData->starting, starting);
  gtk_label_set_text(GTK_LABEL(recnData->starting), amount);
  if (reverse_balance)
    starting = gnc_numeric_neg (starting);

  /* update the ending balance */
  ending = recnData->new_ending;
  if (reverse_balance)
    ending = gnc_numeric_neg (ending);
  amount = xaccPrintAmount(ending, print_info);
  gnc_set_label_color(recnData->ending, ending);
  gtk_label_set_text(GTK_LABEL(recnData->ending), amount);
  if (reverse_balance)
    ending = gnc_numeric_neg (ending);

  debit = gnc_reconcile_list_reconciled_balance
    (GNC_RECONCILE_LIST(recnData->debit));

  credit = gnc_reconcile_list_reconciled_balance
    (GNC_RECONCILE_LIST(recnData->credit));

  /* Update the total debit and credit fields */
  amount = xaccPrintAmount(debit, print_info);
  gtk_label_set_text(GTK_LABEL(recnData->total_debit), amount);

  amount = xaccPrintAmount(credit, print_info);

  gtk_label_set_text(GTK_LABEL(recnData->total_credit), amount);

  /* update the reconciled balance */
  reconciled = gnc_numeric_add_fixed (starting,
                                      gnc_numeric_sub_fixed (debit, credit));
  if (reverse_balance)
    reconciled = gnc_numeric_neg (reconciled);
  amount = xaccPrintAmount(reconciled, print_info);
  gnc_set_label_color(recnData->reconciled, reconciled);
  gtk_label_set_text(GTK_LABEL(recnData->reconciled), amount);
  if (reverse_balance)
    reconciled = gnc_numeric_neg (reconciled);

  /* update the difference */
  diff = gnc_numeric_sub_fixed (ending, reconciled);
  if (reverse_balance)
    diff = gnc_numeric_neg (diff);
  amount = xaccPrintAmount(diff, print_info);
  gnc_set_label_color(recnData->difference, diff);
  gtk_label_set_text(GTK_LABEL(recnData->difference), amount);
  if (reverse_balance)
    diff = gnc_numeric_neg (diff);

  gtk_widget_set_sensitive(recnData->finish_button, gnc_numeric_zero_p (diff));

  return diff;
}

static gboolean
gnc_start_recn_update_cb(GtkWidget *widget, GdkEventFocus *event,
                         gpointer data)
{
  GNCAmountEdit *edit = data;

  gnc_amount_edit_evaluate (edit);

  return FALSE;
}

/********************************************************************\
 * startRecnWindow                                                  *
 *   opens up the window to prompt the user to enter the ending     *
 *   balance from bank statement                                    *
 *                                                                  *
 * NOTE: This function does not return until the user presses "Ok"  *
 *       or "Cancel"                                                *
 *                                                                  *
 * Args:   parent         - the parent of this window               *
 *         account        - the account to reconcile                *
 *         new_ending     - returns the amount for ending balance   *
 *         statement_date - returns date of the statement :)        *
 * Return: True, if the user presses "Ok", else False               *
\********************************************************************/
static gboolean
startRecnWindow(GtkWidget *parent, Account *account,
                gnc_numeric *new_ending, time_t *statement_date)
{
  GtkWidget *dialog, *end_value, *date_value;
  GNCAccountType account_type;
  GNCPrintAmountInfo print_info;
  gnc_numeric ending;
  char *title;
  int result;

  account_type = xaccAccountGetType(account);

  if ((account_type == STOCK) || (account_type == MUTUAL) ||
      (account_type == CURRENCY))
  {
    ending = xaccAccountGetShareReconciledBalance(account);
    print_info = gnc_account_quantity_print_info (account, TRUE);
  }
  else
  {
    ending = xaccAccountGetReconciledBalance(account);
    print_info = gnc_account_value_print_info (account, TRUE);
  }

  if (gnc_reverse_balance(account))
  {
    ending = gnc_numeric_neg (ending);
    *new_ending = gnc_numeric_neg (*new_ending);
  }

  /* Create the dialog box... */
  title = gnc_recn_make_window_name(account);

  dialog = gnome_dialog_new(title,
                            GNOME_STOCK_BUTTON_OK,
                            GNOME_STOCK_BUTTON_CANCEL,
                            NULL);
  g_free(title);

  gnome_dialog_set_default(GNOME_DIALOG(dialog), 0);
  gnome_dialog_set_close(GNOME_DIALOG(dialog), TRUE);
  gnome_dialog_close_hides(GNOME_DIALOG(dialog), TRUE);
  gnome_dialog_set_parent(GNOME_DIALOG(dialog), GTK_WINDOW(parent));

  {
    GtkWidget *frame = gtk_frame_new(_("Reconcile Information"));
    GtkWidget *main_area = gtk_hbox_new(FALSE, 5);
    GtkWidget *left_column = gtk_vbox_new(TRUE, 0);
    GtkWidget *right_column = gtk_vbox_new(TRUE, 0);
    GtkWidget *date_title = gtk_label_new(_("Statement Date:"));
    GtkWidget *start_title = gtk_label_new(_("Starting Balance:"));
    GtkWidget *end_title = gtk_label_new(_("Ending Balance:"));
    GtkWidget *start_value =
      gtk_label_new(xaccPrintAmount (ending, print_info));
    GtkWidget *vbox = GNOME_DIALOG(dialog)->vbox;
    GtkWidget *entry;

    date_value = gnc_date_edit_new(*statement_date, FALSE, FALSE);

    end_value = gnc_amount_edit_new ();
    print_info.use_symbol = 0;
    gnc_amount_edit_set_print_info (GNC_AMOUNT_EDIT (end_value), print_info);
    if ((account_type == STOCK) || (account_type == MUTUAL) ||
        (account_type == CURRENCY))
      gnc_amount_edit_set_fraction (GNC_AMOUNT_EDIT (end_value),
                                    xaccAccountGetSecuritySCU (account));
    else
      gnc_amount_edit_set_fraction (GNC_AMOUNT_EDIT (end_value),
                                    xaccAccountGetCurrencySCU (account));

    gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (end_value), *new_ending);

    entry = gnc_amount_edit_gtk_entry (GNC_AMOUNT_EDIT (end_value));
    gtk_editable_select_region (GTK_EDITABLE (entry), 0, -1);

    gtk_signal_connect(GTK_OBJECT(entry), "focus-out-event",
                       GTK_SIGNAL_FUNC(gnc_start_recn_update_cb), end_value);

    gnome_dialog_editable_enters(GNOME_DIALOG(dialog), GTK_EDITABLE(entry));

    gtk_misc_set_alignment(GTK_MISC(date_title), 1.0, 0.5);
    gtk_misc_set_alignment(GTK_MISC(start_title), 1.0, 0.5);
    gtk_misc_set_alignment(GTK_MISC(start_value), 0.0, 0.5);
    gtk_misc_set_alignment(GTK_MISC(end_title), 1.0, 0.5);

    gtk_container_set_border_width(GTK_CONTAINER(main_area), 10);
    gtk_container_set_border_width(GTK_CONTAINER(frame), 5);
    gtk_container_add(GTK_CONTAINER(frame), main_area);

    gtk_box_pack_start(GTK_BOX(vbox), frame, TRUE, TRUE, 0);

    gtk_box_pack_start(GTK_BOX(main_area), left_column, FALSE, FALSE, 0);
    gtk_box_pack_end(GTK_BOX(main_area), right_column, TRUE, TRUE, 0);

    gtk_box_pack_start(GTK_BOX(left_column), date_title, TRUE, TRUE, 3);
    gtk_box_pack_start(GTK_BOX(left_column), start_title, TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(left_column), end_title, TRUE, TRUE, 0);

    gtk_box_pack_start(GTK_BOX(right_column), date_value, TRUE, TRUE, 3);
    gtk_box_pack_start(GTK_BOX(right_column), start_value, TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(right_column), end_value, TRUE, TRUE, 0);

    gtk_widget_show_all(dialog);

    gtk_widget_grab_focus(end_value);
  }

  while (TRUE)
  {
    result = gnome_dialog_run(GNOME_DIALOG(dialog));

    if (result == 0) /* ok button */
    {
      *new_ending = gnc_amount_edit_get_amount (GNC_AMOUNT_EDIT (end_value));
      *statement_date = gnc_date_edit_get_date(GNC_DATE_EDIT(date_value));

      if (gnc_reverse_balance(account))
        *new_ending = gnc_numeric_neg (*new_ending);
    }

    /* cancel or delete */
    break;
  }

  gtk_widget_destroy (dialog);

  return (result == 0);
}


static void
gnc_reconcile_window_set_sensitivity(RecnWindow *recnData)
{
  gboolean sensitive = FALSE;
  GNCReconcileList *list;

  list = GNC_RECONCILE_LIST(recnData->debit);
  if (gnc_reconcile_list_get_current_split(list) != NULL)
    sensitive = TRUE;

  list = GNC_RECONCILE_LIST(recnData->credit);
  if (gnc_reconcile_list_get_current_split(list) != NULL)
    sensitive = TRUE;

  gtk_widget_set_sensitive(recnData->edit_item, sensitive);
  gtk_widget_set_sensitive(recnData->delete_item, sensitive);

  gtk_widget_set_sensitive(recnData->edit_popup, sensitive);
  gtk_widget_set_sensitive(recnData->delete_popup, sensitive);

  gtk_widget_set_sensitive(recnData->edit_button, sensitive);
  gtk_widget_set_sensitive(recnData->delete_button, sensitive);
}

static void
gnc_reconcile_window_list_cb(GNCReconcileList *list, Split *split,
                             gpointer data)
{
  RecnWindow *recnData = data;

  gnc_reconcile_window_set_sensitivity(recnData);
  recnRecalculateBalance(recnData);
}

static void
gnc_reconcile_window_double_click_cb(GNCReconcileList *list, Split *split,
                                     gpointer data)
{
  RecnWindow *recnData = data;
  RegWindow *regData;

  /* This should never be true, but be paranoid */
  if (split == NULL)
    return;

  regData = regWindowSimple (recn_get_account (recnData));
  if (regData == NULL)
    return;

  gnc_register_raise (regData);
  gnc_register_jump_to_split_amount (regData, split);
}

static void
gnc_reconcile_window_focus_cb(GtkWidget *widget, GdkEventFocus *event,
                              gpointer data)
{
  RecnWindow *recnData = data;
  GNCReconcileList *this_list, *other_list;
  GNCReconcileList *debit, *credit;

  this_list = GNC_RECONCILE_LIST(widget);

  debit  = GNC_RECONCILE_LIST(recnData->debit);
  credit = GNC_RECONCILE_LIST(recnData->credit);

  other_list = GNC_RECONCILE_LIST(this_list == debit ? credit : debit);

  /* clear the *other* list so we always have no more than one selection */
  gnc_reconcile_list_unselect_all(other_list);
}

static gboolean
gnc_reconcile_key_press_cb (GtkWidget *widget, GdkEventKey *event,
                            gpointer data)
{
  RecnWindow *recnData = data;
  GtkWidget *this_list, *other_list;
  GtkWidget *debit, *credit;

  switch (event->keyval)
  {
    case GDK_Tab:
    case GDK_ISO_Left_Tab:
      break;

    default:
      return FALSE;
  }

  gtk_signal_emit_stop_by_name (GTK_OBJECT (widget), "key_press_event");

  this_list = widget;

  debit  = recnData->debit;
  credit = recnData->credit;

  other_list = (this_list == debit ? credit : debit);

  gtk_widget_grab_focus (other_list);

  return TRUE;
}

static void
gnc_reconcile_window_set_titles(RecnWindow *recnData)
{
  gboolean formal;
  gchar *title;

  formal = gnc_lookup_boolean_option("General",
                                     "Use accounting labels", FALSE);

  if (formal)
    title = _("Debits");
  else
    title = gnc_get_debit_string(NO_TYPE);

  gtk_frame_set_label(GTK_FRAME(recnData->debit_frame), title);

  if (!formal)
    g_free(title);

  if (formal)
    title = _("Credits");
  else
    title = gnc_get_credit_string(NO_TYPE);

  gtk_frame_set_label(GTK_FRAME(recnData->credit_frame), title);

  if (!formal)
    g_free(title);

  if (formal)
  {
    gtk_widget_show(recnData->sort_debits_formal);
    gtk_widget_show(recnData->sort_credits_formal);
    gtk_widget_hide(recnData->sort_debits_informal);
    gtk_widget_hide(recnData->sort_credits_informal);
  }
  else
  {
    gtk_widget_hide(recnData->sort_debits_formal);
    gtk_widget_hide(recnData->sort_credits_formal);
    gtk_widget_show(recnData->sort_debits_informal);
    gtk_widget_show(recnData->sort_credits_informal);
  }
}

static void
set_titles_cb(void *data)
{
  gnc_reconcile_window_set_titles(data);
}

static GtkWidget *
gnc_reconcile_window_create_list_box(Account *account,
                                     GNCReconcileListType type,
                                     RecnWindow *recnData,
                                     GtkWidget **list_save,
                                     GtkWidget **total_save)
{
  GtkWidget *frame, *scrollWin, *list, *vbox, *label, *hbox;

  frame = gtk_frame_new(NULL);

  if (type == RECLIST_DEBIT)
    recnData->debit_frame = frame;
  else
    recnData->credit_frame = frame;

  vbox = gtk_vbox_new(FALSE, 5);

  list = gnc_reconcile_list_new(account, type);
  *list_save = list;

  gtk_signal_connect(GTK_OBJECT(list), "toggle_reconciled",
                     GTK_SIGNAL_FUNC(gnc_reconcile_window_list_cb),
                     recnData);
  gtk_signal_connect(GTK_OBJECT(list), "double_click_split",
                     GTK_SIGNAL_FUNC(gnc_reconcile_window_double_click_cb),
                     recnData);
  gtk_signal_connect(GTK_OBJECT(list), "focus_in_event",
                     GTK_SIGNAL_FUNC(gnc_reconcile_window_focus_cb),
                     recnData);
  gtk_signal_connect(GTK_OBJECT(list), "key_press_event",
                     GTK_SIGNAL_FUNC(gnc_reconcile_key_press_cb),
                     recnData);

  scrollWin = gtk_scrolled_window_new (NULL, NULL);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW (scrollWin),
				 GTK_POLICY_NEVER, 
				 GTK_POLICY_ALWAYS);
  gtk_container_set_border_width(GTK_CONTAINER(scrollWin), 5);

  gtk_container_add(GTK_CONTAINER(frame), scrollWin);
  gtk_container_add(GTK_CONTAINER(scrollWin), list);
  gtk_box_pack_start(GTK_BOX(vbox), frame, TRUE, TRUE, 0);

  hbox = gtk_hbox_new(FALSE, 5);
  gtk_box_pack_start(GTK_BOX(vbox), hbox, FALSE, FALSE, 0);

  label = gtk_label_new(_("Total:"));
  gtk_misc_set_alignment(GTK_MISC(label), 1.0, 0.5);
  gtk_box_pack_start(GTK_BOX(hbox), label, TRUE, TRUE, 0);

  label = gtk_label_new("");
  gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 0);
  *total_save = label;

  return vbox;
}


static GtkWidget *
gnc_recn_create_status_bar(RecnWindow *recnData)
{
  GtkWidget *statusbar;

  statusbar = gnome_appbar_new(FALSE, /* no progress bar */
			       TRUE,  /* has status area */
			       GNOME_PREFERENCES_USER);

  return statusbar;
}


static Split *
gnc_reconcile_window_get_current_split(RecnWindow *recnData)
{
  GNCReconcileList *list;
  Split *split;

  list = GNC_RECONCILE_LIST(recnData->debit);
  split = gnc_reconcile_list_get_current_split(list);
  if (split != NULL)
    return split;

  list = GNC_RECONCILE_LIST(recnData->credit);
  split = gnc_reconcile_list_get_current_split(list);

  return split;
}

static void
gnc_ui_reconcile_window_help_cb(GtkWidget *widget, gpointer data)
{
  helpWindow(NULL, NULL, HH_RECNWIN);
}

static void
gnc_ui_reconcile_window_change_cb(GtkButton *button, gpointer data)
{
  RecnWindow *recnData = data;
  gnc_numeric new_ending = recnData->new_ending;
  time_t statement_date = recnData->statement_date;

  if (startRecnWindow(recnData->window, recn_get_account (recnData),
                      &new_ending, &statement_date))
  {
    recnData->new_ending = new_ending;
    recnData->statement_date = statement_date;
    recnRecalculateBalance (recnData);
  }
}

static void
gnc_ui_reconcile_window_new_cb(GtkButton *button, gpointer data)
{
  RecnWindow *recnData = data;
  RegWindow *regData;

  regData = regWindowSimple (recn_get_account (recnData));
  if (regData == NULL)
    return;

  gnc_register_raise (regData);
  gnc_register_jump_to_blank (regData);
}

static void
gnc_ui_reconcile_window_delete_cb(GtkButton *button, gpointer data)
{
  RecnWindow *recnData = data;
  GList *affected_accounts = NULL;
  Transaction *trans;
  Split *split;
  int i, num_splits;

  split = gnc_reconcile_window_get_current_split(recnData);
  /* This should never be true, but be paranoid */
  if (split == NULL)
    return;

  {
    const char *message = _("Are you sure you want to delete the current "
                            "transaction?");
    gboolean result;

    result = gnc_verify_dialog_parented(recnData->window, message, FALSE);

    if (!result)
      return;
  }

  /* make a copy of all of the accounts that will be  
   * affected by this deletion, so that we can update
   * their register windows after the deletion. */
  trans = xaccSplitGetParent(split);
  num_splits = xaccTransCountSplits(trans);

  for (i = 0; i < num_splits; i++) 
  {
    Account *a;
    Split *s;

    s = xaccTransGetSplit(trans, i);
    a = xaccSplitGetAccount(s);
    if (a != NULL)
      affected_accounts = g_list_prepend(affected_accounts, a);
  }

  xaccTransBeginEdit(trans);
  xaccTransDestroy(trans);
  xaccTransCommitEdit(trans);

  gnc_account_glist_ui_refresh(affected_accounts);

  g_list_free(affected_accounts);

  gnc_refresh_main_window ();
}

static void
gnc_ui_reconcile_window_edit_cb(GtkButton *button, gpointer data)
{
  RecnWindow *recnData = data;
  RegWindow *regData;
  Split *split;

  split = gnc_reconcile_window_get_current_split (recnData);
  /* This should never be true, but be paranoid */
  if (split == NULL)
    return;

  regData = regWindowSimple (recn_get_account (recnData));
  if (regData == NULL)
    return;

  gnc_register_raise (regData);
  gnc_register_jump_to_split_amount (regData, split);
}


static char *
gnc_recn_make_window_name(Account *account)
{
  char *fullname;
  char *title;

  fullname = xaccAccountGetFullName(account, gnc_get_account_separator());
  title = g_strconcat(fullname, " - ", _("Reconcile"), NULL);

  g_free(fullname);

  return title;
}

static void
gnc_recn_set_window_name(RecnWindow *recnData)
{
  char *title;

  title = gnc_recn_make_window_name (recn_get_account (recnData));

  gtk_window_set_title (GTK_WINDOW (recnData->window), title);

  g_free (title);
}

static void 
gnc_recn_edit_account_cb(GtkWidget * w, gpointer data)
{
  RecnWindow *recnData = data;
  Account *account = recn_get_account (recnData);

  if (account == NULL)
    return;

  gnc_ui_edit_account_window (account);
}

static void 
gnc_recn_xfer_cb(GtkWidget * w, gpointer data)
{
  RecnWindow *recnData = data;
  Account *account = recn_get_account (recnData);

  if (account == NULL)
    return;

  gnc_xfer_dialog (recnData->window, account);
}

static void
gnc_recn_scrub_cb(GtkWidget *widget, gpointer data)
{
  RecnWindow *recnData = data;
  Account *account = recn_get_account (recnData);

  if (account == NULL)
    return;

  xaccAccountTreeScrubOrphans (account);
  xaccAccountTreeScrubImbalance (account);

  gnc_account_ui_refresh (account);
  gnc_refresh_main_window ();
}

static void
gnc_recn_open_cb(GtkWidget *widget, gpointer data)
{
  RecnWindow *recnData = data;
  Account *account = recn_get_account (recnData);
  RegWindow *regData;

  if (!account)
    return;

  regData = regWindowSimple (account);
  gnc_register_raise (regData);
}

static void
gnc_reconcile_sort(RecnWindow *recnData, GNCReconcileListType list_type,
                   sort_type_t sort_code)
{
  GNCReconcileList *list;
  sort_type_t *old_type_p;

  if (list_type == RECLIST_DEBIT)
  {
    list = GNC_RECONCILE_LIST(recnData->debit);
    old_type_p = &recnData->debit_sort;
  }
  else
  {
    list = GNC_RECONCILE_LIST(recnData->credit);
    old_type_p = &recnData->credit_sort;
  }

  if (sort_code == *old_type_p)
    return;

  switch(sort_code)
  {
    default:
    case BY_STANDARD:
      gnc_reconcile_list_set_sort_order(list, BY_STANDARD);
      break;
    case BY_NUM:
      gnc_reconcile_list_set_sort_order(list, BY_NUM);
      break;
    case BY_AMOUNT:
      gnc_reconcile_list_set_sort_order(list, BY_AMOUNT);
      break;
    case BY_DESC:
      gnc_reconcile_list_set_sort_order(list, BY_DESC);
      break;
  }

  *old_type_p = sort_code;

  gnc_reconcile_list_refresh(GNC_RECONCILE_LIST(recnData->debit));
  gnc_reconcile_list_refresh(GNC_RECONCILE_LIST(recnData->credit));
}

static void
sort_debit_standard_cb(GtkWidget *w, gpointer data)
{
  RecnWindow *recnData = data;

  gnc_reconcile_sort(recnData, RECLIST_DEBIT, BY_STANDARD);
}

static void
sort_debit_num_cb(GtkWidget *w, gpointer data)
{
  RecnWindow *recnData = data;

  gnc_reconcile_sort(recnData, RECLIST_DEBIT, BY_NUM);
}

static void
sort_debit_desc_cb(GtkWidget *w, gpointer data)
{
  RecnWindow *recnData = data;

  gnc_reconcile_sort(recnData, RECLIST_DEBIT, BY_DESC);
}

static void
sort_debit_amount_cb(GtkWidget *w, gpointer data)
{
  RecnWindow *recnData = data;

  gnc_reconcile_sort(recnData, RECLIST_DEBIT, BY_AMOUNT);
}

static void
sort_credit_standard_cb(GtkWidget *w, gpointer data)
{
  RecnWindow *recnData = data;

  gnc_reconcile_sort(recnData, RECLIST_CREDIT, BY_STANDARD);
}

static void
sort_credit_num_cb(GtkWidget *w, gpointer data)
{
  RecnWindow *recnData = data;

  gnc_reconcile_sort(recnData, RECLIST_CREDIT, BY_NUM);
}

static void
sort_credit_desc_cb(GtkWidget *w, gpointer data)
{
  RecnWindow *recnData = data;

  gnc_reconcile_sort(recnData, RECLIST_CREDIT, BY_DESC);
}

static void
sort_credit_amount_cb(GtkWidget *w, gpointer data)
{
  RecnWindow *recnData = data;

  gnc_reconcile_sort(recnData, RECLIST_CREDIT, BY_AMOUNT);
}

static GtkWidget *
gnc_recn_create_menu_bar(RecnWindow *recnData, GtkWidget *statusbar)
{
  GtkWidget *menubar;
  GtkAccelGroup *accel_group;

  static GnomeUIInfo reconcile_menu[] =
  {
    {
      GNOME_APP_UI_ITEM,
      N_("_Reconcile Information..."),
      N_("Change the reconcile information "
         "including statement date and ending balance."),
      gnc_ui_reconcile_window_change_cb, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    {
      GNOME_APP_UI_ITEM,
      N_("_Finish"),
      N_("Finish the reconciliation of this account"),
      recnFinishCB, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      'f', GDK_CONTROL_MASK, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      N_("_Cancel"),
      N_("Cancel the reconciliation of this account"),
      recnCancelCB, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    GNOMEUIINFO_END
  };

  static GnomeUIInfo sort_debit_list[] =
  {
    GNOMEUIINFO_RADIOITEM_DATA(N_("Standard order"),
                               N_("Keep normal account order"),
                               sort_debit_standard_cb, NULL, NULL),
    GNOMEUIINFO_RADIOITEM_DATA(N_("Sort by Num"),
                               N_("Sort by Num"),
                               sort_debit_num_cb, NULL, NULL),
    GNOMEUIINFO_RADIOITEM_DATA(N_("Sort by Description"),
                               N_("Sort by Description"),
                               sort_debit_desc_cb, NULL, NULL),
    GNOMEUIINFO_RADIOITEM_DATA(N_("Sort by Amount"),
                               N_("Sort by Amount"),
                               sort_debit_amount_cb, NULL, NULL),
    GNOMEUIINFO_END
  };

  static GnomeUIInfo sort_debit_menu[] =
  {
    GNOMEUIINFO_RADIOLIST(sort_debit_list),
    GNOMEUIINFO_END
  };

  static GnomeUIInfo sort_credit_list[] =
  {
    GNOMEUIINFO_RADIOITEM_DATA(N_("Standard order"),
                               N_("Keep normal account order"),
                               sort_credit_standard_cb, NULL, NULL),
    GNOMEUIINFO_RADIOITEM_DATA(N_("Sort by Num"),
                               N_("Sort by Num"),
                               sort_credit_num_cb, NULL, NULL),
    GNOMEUIINFO_RADIOITEM_DATA(N_("Sort by Description"),
                               N_("Sort by Description"),
                               sort_credit_desc_cb, NULL, NULL),
    GNOMEUIINFO_RADIOITEM_DATA(N_("Sort by Amount"),
                               N_("Sort by Amount"),
                               sort_credit_amount_cb, NULL, NULL),
    GNOMEUIINFO_END
  };

  static GnomeUIInfo sort_credit_menu[] =
  {
    GNOMEUIINFO_RADIOLIST(sort_credit_list),
    GNOMEUIINFO_END
  };

  static GnomeUIInfo sort_menu[] =
  {
    GNOMEUIINFO_SUBTREE(N_("Debits"), sort_debit_menu),
    GNOMEUIINFO_SUBTREE(N_("Credits"), sort_credit_menu),
    GNOMEUIINFO_SUBTREE(NULL, sort_debit_menu),
    GNOMEUIINFO_SUBTREE(NULL, sort_credit_menu),
    GNOMEUIINFO_END
  };

  static GnomeUIInfo account_menu[] =
  {
    {
      GNOME_APP_UI_ITEM,
      N_("_Open Account"), N_("Open the account"),
      gnc_recn_open_cb, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      N_("_Edit Account"), N_("Edit the main account for this register"),
      gnc_recn_edit_account_cb, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    {
      GNOME_APP_UI_ITEM,
      N_("_Transfer..."), N_("Transfer funds from one account to another"),
      gnc_recn_xfer_cb, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    {
      GNOME_APP_UI_ITEM,
      N_("_Scrub"), N_("Identify and fix problems in the account"),
      gnc_recn_scrub_cb, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    GNOMEUIINFO_END
  };

  static GnomeUIInfo transaction_menu[] =
  {
    {
      GNOME_APP_UI_ITEM,
      N_("_New"), N_("Add a new transaction to the account"),
      gnc_ui_reconcile_window_new_cb, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      'n', GDK_CONTROL_MASK, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      N_("_Edit"), N_("Edit the current transaction"),
      gnc_ui_reconcile_window_edit_cb, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      'e', GDK_CONTROL_MASK, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      N_("_Delete"), N_("Delete the current transaction"),
      gnc_ui_reconcile_window_delete_cb, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      'd', GDK_CONTROL_MASK, NULL
    },
    GNOMEUIINFO_END
  };

  static GnomeUIInfo help_menu[] =
  {
    {
      GNOME_APP_UI_ITEM,
      N_("_Help"), N_("Open the GnuCash help window"),
      gnc_ui_reconcile_window_help_cb, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    GNOMEUIINFO_END
  };

  static GnomeUIInfo reconcile_window_menu[] =
  {
    GNOMEUIINFO_SUBTREE(N_("_Reconcile"), reconcile_menu),
    GNOMEUIINFO_SUBTREE(N_("Sort _Order"), sort_menu),
    GNOMEUIINFO_SUBTREE(N_("_Account"), account_menu),
    GNOMEUIINFO_SUBTREE(N_("_Transaction"), transaction_menu),
    GNOMEUIINFO_MENU_HELP_TREE(help_menu),
    GNOMEUIINFO_END
  };

  gnc_fill_menu_with_data(reconcile_window_menu, recnData);

  sort_menu[2].label = gnc_get_debit_string(NO_TYPE);
  sort_menu[3].label = gnc_get_credit_string(NO_TYPE);

  menubar = gtk_menu_bar_new();

  accel_group = gtk_accel_group_new();
  gtk_accel_group_attach(accel_group, GTK_OBJECT(recnData->window));

  gnome_app_fill_menu(GTK_MENU_SHELL(menubar), reconcile_window_menu,
  		      accel_group, TRUE, 0);

  gnome_app_install_appbar_menu_hints(GNOME_APPBAR(statusbar),
                                      reconcile_window_menu);

  recnData->edit_item = transaction_menu[1].widget;
  recnData->delete_item = transaction_menu[2].widget;

  recnData->sort_debits_formal = sort_menu[0].widget;
  recnData->sort_credits_formal = sort_menu[1].widget;
  recnData->sort_debits_informal = sort_menu[2].widget;
  recnData->sort_credits_informal = sort_menu[3].widget;

  g_free(sort_menu[2].label);
  g_free(sort_menu[3].label);

  return menubar;
}


static GtkWidget *
gnc_recn_create_popup_menu(RecnWindow *recnData)
{
  GtkWidget *popup;

  GnomeUIInfo transaction_menu[] =
  {
    {
      GNOME_APP_UI_ITEM,
      N_("_New"), N_("Add a new transaction to the account"),
      gnc_ui_reconcile_window_new_cb, recnData, NULL,
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_NEW,
      'n', GDK_CONTROL_MASK, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      N_("_Edit"), N_("Edit the current transaction"),
      gnc_ui_reconcile_window_edit_cb, recnData, NULL,
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_PROPERTIES,
      'e', GDK_CONTROL_MASK, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      N_("_Delete"), N_("Delete the current transaction"),
      gnc_ui_reconcile_window_delete_cb, recnData, NULL,
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_TRASH,
      'd', GDK_CONTROL_MASK, NULL
    },
    GNOMEUIINFO_END
  };

  popup = gnome_popup_menu_new(transaction_menu);

  recnData->edit_popup = transaction_menu[1].widget;
  recnData->delete_popup = transaction_menu[2].widget;

  return popup;
}


static void
gnc_recn_refresh_toolbar(RecnWindow *recnData)
{
  GtkToolbarStyle tbstyle;

  if ((recnData == NULL) || (recnData->toolbar == NULL))
    return;

  tbstyle = gnc_get_toolbar_style();

  gtk_toolbar_set_style(GTK_TOOLBAR(recnData->toolbar), tbstyle);
}

static void
gnc_toolbar_change_cb(void *data)
{
  RecnWindow *recnData = data;

  gnc_recn_refresh_toolbar(recnData);
}

static GtkWidget *
gnc_recn_create_tool_bar(RecnWindow *recnData)
{
  GtkWidget *toolbar;
  GnomeUIInfo toolbar_info[] =
  {
    {
      GNOME_APP_UI_ITEM,
      N_("New"), N_("Add a new transaction to the account"),
      gnc_ui_reconcile_window_new_cb, NULL, NULL,
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_NEW,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      N_("Edit"), N_("Edit the current transaction"),
      gnc_ui_reconcile_window_edit_cb, NULL, NULL,
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_PROPERTIES,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      N_("Delete"), N_("Delete the current transaction"),
      gnc_ui_reconcile_window_delete_cb, NULL, NULL,
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_TRASH,
      0, 0, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    {
      GNOME_APP_UI_ITEM,
      N_("Open"), N_("Open the account"),
      gnc_recn_open_cb, NULL, NULL,
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_JUMP_TO,
      0, 0, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    {
      GNOME_APP_UI_ITEM,
      N_("Finish"), N_("Finish the reconciliation of this account"),
      recnFinishCB, NULL, NULL,
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_DOWN,
      0, 0, NULL
    },
    GNOMEUIINFO_END
  };

  toolbar = gtk_toolbar_new(GTK_ORIENTATION_HORIZONTAL, GTK_TOOLBAR_BOTH);

  gnome_app_fill_toolbar_with_data(GTK_TOOLBAR(toolbar), toolbar_info,
                                   NULL, recnData);

  recnData->toolbar = toolbar;

  recnData->edit_button = toolbar_info[1].widget;
  recnData->delete_button = toolbar_info[2].widget;
  recnData->finish_button = toolbar_info[6].widget;

  return toolbar;
}

static gboolean
find_by_account (gpointer find_data, gpointer user_data)
{
  Account *account = find_data;
  RecnWindow *recnData = user_data;

  if (!recnData)
    return FALSE;

  return guid_equal (&recnData->account, xaccAccountGetGUID (account));
}

static void
refresh_handler (GHashTable *changes, gpointer user_data)
{
  RecnWindow *recnData = user_data;
  const EventInfo *info;
  Account *account;

  account = recn_get_account (recnData);
  if (!account)
  {
    gnc_close_gui_component_by_data (WINDOW_RECONCILE_CM_CLASS, recnData);
    return;
  }

  if (changes)
  {
    info = gnc_gui_get_entity_events (changes, &recnData->account);
    if (info && (info->event_mask & GNC_EVENT_DESTROY))
    {
      gnc_close_gui_component_by_data (WINDOW_RECONCILE_CM_CLASS, recnData);
      return;
    }
  }

  recnRefresh (recnData);
}

static void
close_handler (gpointer user_data)
{
  RecnWindow *recnData = user_data;

  gtk_widget_destroy (recnData->window);
}

/********************************************************************\
 * recnWindow                                                       *
 *   opens up the window to reconcile an account                    *
 *                                                                  *
 * Args:   parent  - the parent of this window                      *
 *         account - the account to reconcile                       *
 * Return: recnData - the instance of this RecnWindow               *
\********************************************************************/
RecnWindow *
recnWindow (GtkWidget *parent, Account *account)
{
  static time_t last_statement_date = 0;

  RecnWindow *recnData;
  GtkWidget *statusbar;
  GtkWidget *vbox;
  GtkWidget *dock;
  gnc_numeric new_ending;
  time_t statement_date;
  GNCAccountType type;
  gint component_id;

  if (account == NULL)
    return NULL;

  recnData = gnc_find_first_gui_component (WINDOW_RECONCILE_CM_CLASS,
                                           find_by_account, account);
  if (recnData)
    return recnData;

  recnData = g_new0 (RecnWindow, 1);

  recnData->account = *xaccAccountGetGUID (account);

  component_id = gnc_register_gui_component (WINDOW_RECONCILE_CM_CLASS,
                                             refresh_handler, close_handler,
                                             recnData);

  gnc_gui_component_watch_entity (component_id,
                                  xaccAccountGetGUID (account),
                                  GNC_EVENT_MODIFY | GNC_EVENT_DESTROY);

  type = xaccAccountGetType(account);
  recnData->use_shares = ((type == STOCK) || (type == MUTUAL) ||
                          (type == CURRENCY));

  if (recnData->use_shares)
    new_ending = xaccAccountGetShareBalance(account);
  else
    new_ending = xaccAccountGetBalance(account);

  /* The last time reconciliation was attempted during the current
   * execution of gnucash, the date was stored.  Use that date if 
   * possible.  This helps with balancing multiple accounts for
   * which statements are issued at the same time, like multiple
   * bank accounts on a single statement.
   */
  if (!last_statement_date)
     statement_date = time(NULL);
  else
     statement_date = last_statement_date;

  /* Popup a little window to prompt the user to enter the
   * ending balance for his/her bank statement */
  if (!startRecnWindow(parent, account, &new_ending, &statement_date))
  {
    gnc_unregister_gui_component_by_data (WINDOW_RECONCILE_CM_CLASS, recnData);
    g_free (recnData);
    return NULL;
  }

  last_statement_date = statement_date;

  recnData->new_ending = new_ending;
  recnData->statement_date = statement_date;
  recnData->window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  recnData->delete_refresh = FALSE;
  recnData->debit_sort = BY_STANDARD;
  recnData->credit_sort = BY_STANDARD;

  gnc_recn_set_window_name(recnData);

  vbox = gtk_vbox_new(FALSE, 0);
  gtk_container_add(GTK_CONTAINER(recnData->window), vbox);

  dock = gnome_dock_new();
  gtk_box_pack_start(GTK_BOX(vbox), dock, TRUE, TRUE, 0);

  statusbar = gnc_recn_create_status_bar(recnData);
  gtk_box_pack_start(GTK_BOX(vbox), statusbar, FALSE, FALSE, 0);

  gtk_signal_connect (GTK_OBJECT (recnData->window), "destroy",
                      GTK_SIGNAL_FUNC(recn_destroy_cb), recnData);

  /* The menu bar */
  {
    GtkWidget *dock_item;
    GtkWidget *menubar;

    dock_item = gnome_dock_item_new("menu", GNOME_DOCK_ITEM_BEH_EXCLUSIVE);

    menubar = gnc_recn_create_menu_bar(recnData, statusbar);
    gtk_container_set_border_width(GTK_CONTAINER(menubar), 2);
    gtk_container_add(GTK_CONTAINER(dock_item), menubar);

    gnome_dock_add_item (GNOME_DOCK(dock), GNOME_DOCK_ITEM(dock_item),
                         GNOME_DOCK_TOP, 0, 0, 0, TRUE);
  }

  /* The tool bar */
  {
    GtkWidget *dock_item;
    GtkWidget *toolbar;
    SCM id;

    dock_item = gnome_dock_item_new("toolbar", GNOME_DOCK_ITEM_BEH_EXCLUSIVE);

    toolbar = gnc_recn_create_tool_bar(recnData);
    gtk_container_set_border_width(GTK_CONTAINER(toolbar), 2);
    gtk_container_add(GTK_CONTAINER(dock_item), toolbar);

    id = gnc_register_option_change_callback(gnc_toolbar_change_cb, recnData,
                                             "General", "Toolbar Buttons");
    recnData->toolbar_change_cb_id = id;

    gnome_dock_add_item (GNOME_DOCK(dock), GNOME_DOCK_ITEM(dock_item),
                         GNOME_DOCK_TOP, 1, 0, 0, TRUE);
  }

  /* The main area */
  {
    GtkWidget *frame = gtk_frame_new(NULL);
    GtkWidget *main_area = gtk_vbox_new(FALSE, 10);
    GtkWidget *debcred_area = gtk_hbox_new(FALSE, 15);
    GtkWidget *debits_box;
    GtkWidget *credits_box;
    GtkWidget *popup;

    gnome_dock_set_client_area(GNOME_DOCK(dock), frame);

    gtk_container_add(GTK_CONTAINER(frame), main_area);
    gtk_container_set_border_width(GTK_CONTAINER(main_area), 10);

    debits_box = gnc_reconcile_window_create_list_box
      (account, RECLIST_DEBIT, recnData,
       &recnData->debit, &recnData->total_debit);

    credits_box = gnc_reconcile_window_create_list_box
      (account, RECLIST_CREDIT, recnData,
       &recnData->credit, &recnData->total_credit);

    popup = gnc_recn_create_popup_menu(recnData);
    gnome_popup_menu_attach(popup, recnData->debit, recnData);
    gnome_popup_menu_attach(popup, recnData->credit, recnData);

    gtk_box_pack_start(GTK_BOX(main_area), debcred_area, TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(debcred_area), debits_box, TRUE, FALSE, 0);
    gtk_box_pack_end(GTK_BOX(debcred_area), credits_box, TRUE, FALSE, 0);

    {
      GtkWidget *hbox, *title_vbox, *value_vbox;
      GtkWidget *totals_hbox, *frame, *title, *value;

      /* lower horizontal bar below reconcile lists */
      hbox = gtk_hbox_new(FALSE, 5);
      gtk_box_pack_start(GTK_BOX(main_area), hbox, FALSE, FALSE, 0);

      /* frame to hold totals */
      frame = gtk_frame_new(NULL);
      gtk_box_pack_end(GTK_BOX(hbox), frame, FALSE, FALSE, 0);

      /* hbox to hold title/value vboxes */
      totals_hbox = gtk_hbox_new(FALSE, 3);
      gtk_container_add(GTK_CONTAINER(frame), totals_hbox);
      gtk_container_set_border_width(GTK_CONTAINER(totals_hbox), 5);

      /* vbox to hold titles */
      title_vbox = gtk_vbox_new(FALSE, 3);
      gtk_box_pack_start(GTK_BOX(totals_hbox), title_vbox, FALSE, FALSE, 0);

      /* vbox to hold values */
      value_vbox = gtk_vbox_new(FALSE, 3);
      gtk_box_pack_start(GTK_BOX(totals_hbox), value_vbox, TRUE, TRUE, 0);

      /* starting balance title/value */
      title = gtk_label_new(_("Starting Balance:"));
      gtk_misc_set_alignment(GTK_MISC(title), 1.0, 0.5);
      gtk_box_pack_start(GTK_BOX(title_vbox), title, FALSE, FALSE, 3);

      value = gtk_label_new("");
      recnData->starting = value;
      gtk_misc_set_alignment(GTK_MISC(value), 1.0, 0.5);
      gtk_box_pack_start(GTK_BOX(value_vbox), value, FALSE, FALSE, 3);

      /* ending balance title/value */
      title = gtk_label_new(_("Ending Balance:"));
      gtk_misc_set_alignment(GTK_MISC(title), 1.0, 0.5);
      gtk_box_pack_start(GTK_BOX(title_vbox), title, FALSE, FALSE, 0);

      value = gtk_label_new("");
      recnData->ending = value;
      gtk_misc_set_alignment(GTK_MISC(value), 1.0, 0.5);
      gtk_box_pack_start(GTK_BOX(value_vbox), value, FALSE, FALSE, 0);

      /* reconciled balance title/value */
      title = gtk_label_new(_("Reconciled Balance:"));
      gtk_misc_set_alignment(GTK_MISC(title), 1.0, 0.5);
      gtk_box_pack_start(GTK_BOX(title_vbox), title, FALSE, FALSE, 0);

      value = gtk_label_new("");
      recnData->reconciled = value;
      gtk_misc_set_alignment(GTK_MISC(value), 1.0, 0.5);
      gtk_box_pack_start(GTK_BOX(value_vbox), value, FALSE, FALSE, 0);

      /* difference title/value */
      title = gtk_label_new(_("Difference:"));
      gtk_misc_set_alignment(GTK_MISC(title), 1.0, 0.5);
      gtk_box_pack_start(GTK_BOX(title_vbox), title, FALSE, FALSE, 0);

      value = gtk_label_new("");
      recnData->difference = value;
      gtk_misc_set_alignment(GTK_MISC(value), 1.0, 0.5);
      gtk_box_pack_start(GTK_BOX(value_vbox), value, FALSE, FALSE, 0);
    }

    /* Set up the data */
    recnRefresh (recnData);

    /* Clamp down on the size */
    {
      GNCReconcileList *rlist;
      gint height, num_debits, num_credits, num_show;

      num_credits = gnc_reconcile_list_get_num_splits
        (GNC_RECONCILE_LIST(recnData->credit));
      num_debits = gnc_reconcile_list_get_num_splits
        (GNC_RECONCILE_LIST(recnData->debit));

      num_show = MAX(num_debits, num_credits);
      num_show = MIN(num_show, 15);
      num_show = MAX(num_show, 8);

      gtk_widget_realize(recnData->credit);
      rlist = GNC_RECONCILE_LIST(recnData->credit);
      height = gnc_reconcile_list_get_needed_height(rlist, num_show);

      gtk_widget_set_usize(recnData->credit, 0, height);
      gtk_widget_set_usize(recnData->debit, 0, height);
    }
  }

  /* Allow grow, allow shrink, auto-shrink */
  gtk_window_set_policy(GTK_WINDOW(recnData->window), TRUE, TRUE, TRUE);

  gtk_widget_show_all(recnData->window);

  gnc_reconcile_window_set_titles(recnData);

  recnData->title_change_cb_id =
    gnc_register_option_change_callback(set_titles_cb, recnData,
                                        "General", "Use accounting labels");

  recnRecalculateBalance(recnData);

  gnc_recn_refresh_toolbar(recnData);

  gnc_window_adjust_for_screen(GTK_WINDOW(recnData->window));

  gtk_widget_grab_focus (recnData->debit);

  return recnData;
}


/********************************************************************\
 * gnc_ui_reconile_window_raise                                     *
 *   shows and raises an account editing window                     * 
 *                                                                  * 
 * Args:   editAccData - the edit window structure                  * 
\********************************************************************/
void
gnc_ui_reconcile_window_raise(RecnWindow * recnData)
{
  if (recnData == NULL)
    return;

  if (recnData->window == NULL)
    return;

  gtk_widget_show(recnData->window);

  if (recnData->window->window == NULL)
    return;

  gdk_window_raise(recnData->window->window);
}


/********************************************************************\
 * recn_destroy_cb                                                  *
 *   frees memory allocated for an recnWindow, and other cleanup    *
 *   stuff                                                          *
 *                                                                  *
 * Args:   w    - the widget that called us                         *
 *         data - the data struct for this window                   *
 * Return: none                                                     *
\********************************************************************/
static void 
recn_destroy_cb (GtkWidget *w, gpointer data)
{
  RecnWindow *recnData = data;
  SCM id;

  gnc_unregister_gui_component_by_data (WINDOW_RECONCILE_CM_CLASS, recnData);

  id = recnData->toolbar_change_cb_id;
  gnc_unregister_option_change_callback_id (id);

  id = recnData->title_change_cb_id;
  gnc_unregister_option_change_callback_id (id);

  if (recnData->delete_refresh)
    gnc_account_ui_refresh (recn_get_account (recnData));

  g_free (recnData);
}


/********************************************************************\
 * find_payment_account                                             *
 *   find an account that 'looks like' a payment account for the    *
 *   given account. This really only makes sense for credit card    *
 *   accounts.                                                      *
 *                                                                  *
 * Args:   account - the account to look in                         *
 * Return: a candidate payment account or NULL if none was found    *
\********************************************************************/
static Account *
find_payment_account(Account *account)
{
  int i;

  if (account == NULL)
    return NULL;

  i = xaccAccountGetNumSplits(account);
  /* Search backwards to find the latest payment */
  for (i -= 1; i >= 0; i--)
  {
    Transaction *trans;
    Split *split;
    int num_splits;
    int j;

    split = xaccAccountGetSplit(account, i);
    if (split == NULL)
      continue;

    /* ignore 'purchases' */
    if (!gnc_numeric_positive_p (xaccSplitGetShareAmount(split)))
      continue;

    trans = xaccSplitGetParent(split);
    if (trans == NULL)
      continue;

    num_splits = xaccTransCountSplits(trans);
    for (j = 0; j < num_splits; j++)
    {
      GNCAccountType type;
      Account *a;
      Split *s;

      s = xaccTransGetSplit(trans, j);
      if ((s == NULL) || (s == split))
        continue;

      a = xaccSplitGetAccount(s);
      if ((a == NULL) || (a == account))
        continue;

      type = xaccAccountGetType(a);
      if ((type == BANK) || (type == CASH) || (type == ASSET))
        return a;
    }
  }

  return NULL;
}


/********************************************************************\
 * recnFinishCB                                                     *
 *   saves reconcile information                                    *
 *                                                                  *
 * Args:   w    - the widget that called us                         *
 *         data - the data struct for this window                   *
 * Return: none                                                     *
\********************************************************************/
static void 
recnFinishCB (GtkWidget *w, gpointer data)
{
  RecnWindow *recnData = data;
  gboolean auto_payment;
  Account *account;
  time_t date;

  if (!gnc_numeric_zero_p (recnRecalculateBalance(recnData)))
  {
    const char *message = _("The account is not balanced.\n"
                            "Are you sure you want to finish?");
    if (!gnc_verify_dialog_parented(recnData->window, message, FALSE))
      return;
  }

  date = recnData->statement_date;

  gnc_reconcile_list_commit(GNC_RECONCILE_LIST(recnData->credit), date);
  gnc_reconcile_list_commit(GNC_RECONCILE_LIST(recnData->debit), date);

  recnData->delete_refresh = TRUE;

  auto_payment = gnc_lookup_boolean_option ("Reconcile",
                                            "Automatic credit card payments",
                                            TRUE);

  account = recn_get_account (recnData);

  if (auto_payment &&
      (xaccAccountGetType (account) == CREDIT) &&
      (gnc_numeric_negative_p (recnData->new_ending)))
  {
    Account *payment_account;
    XferDialog *xfer;

    xfer = gnc_xfer_dialog(NULL, account);

    gnc_xfer_dialog_set_amount(xfer, gnc_numeric_neg (recnData->new_ending));

    payment_account = find_payment_account (account);
    if (payment_account != NULL)
      gnc_xfer_dialog_select_from_account (xfer, payment_account);
  }

  gnc_close_gui_component_by_data (WINDOW_RECONCILE_CM_CLASS, recnData);
}

static void 
recnCancelCB (GtkWidget *w, gpointer data)
{
  RecnWindow *recnData = data;
  gboolean changed = FALSE;

  if (gnc_reconcile_list_changed(GNC_RECONCILE_LIST(recnData->credit)))
    changed = TRUE;
  if (gnc_reconcile_list_changed(GNC_RECONCILE_LIST(recnData->debit)))
    changed = TRUE;

  if (changed)
  {
    const char *message = _("You have made changes to this reconcile "
                            "window.\nAre you sure you want to cancel?");
    if (!gnc_verify_dialog_parented(recnData->window, message, FALSE))
      return;
  }

  gnc_close_gui_component_by_data (WINDOW_RECONCILE_CM_CLASS, recnData);
}
