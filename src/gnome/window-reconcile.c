/********************************************************************\
 * window-reconcile.c -- the reconcile window                       *
 * Copyright (C) 1997 Robin D. Clark                                *
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

#define _GNU_SOURCE
#include <stdio.h>
#include <gnome.h>

#include "top-level.h"

#include "date.h"
#include "MultiLedger.h"
#include "MainWindow.h"
#include "RegWindow.h"
#include "window-reconcile.h"
#include "reconcile-list.h"
#include "query-user.h"
#include "window-help.h"
#include "messages.h"
#include "util.h"


/** STRUCTS *********************************************************/
struct _RecnWindow
{
  Account *account;         /* The account that we are reconciling  */
  double  ddiff;            /* The amount to reconcile              */

  GtkWidget *dialog;        /* The reconcile window                 */

  GtkWidget *starting;      /* The starting balance                 */
  GtkWidget *ending;        /* The ending balance                   */
  GtkWidget *difference;    /* Text field, amount left to reconcile */

  GtkWidget *total_debit;   /* Text field, total debit reconciled   */
  GtkWidget *total_credit;  /* Text field, total credit reconciled  */

  GtkWidget *debit;         /* Debit matrix show unreconciled debit */
  GtkWidget *credit;        /* Credit matrix, shows credits...      */

  char * symbol;            /* Currency symbol or 's' for shares    */
};

/** PROTOTYPES ******************************************************/
static double recnRecalculateBalance( RecnWindow *recnData );

static void recnClose(GtkWidget *w, gpointer data);
static void recnOkCB(GtkWidget *w, gpointer data);
static void recnCancelCB(GtkWidget *w, gpointer data);

/** GLOBALS *********************************************************/

static RecnWindow **recnList = NULL;

/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_GUI;

/********************************************************************/


/********************************************************************\
 * recnRefresh                                                      *
 *   refreshes the transactions in the reconcile window             *
 *                                                                  *
 * Args:   account - the account of the reconcile window to refresh *
 * Return: none                                                     *
\********************************************************************/
void
recnRefresh(Account *account)
{
  RecnWindow *recnData; 
  
  FIND_IN_LIST (RecnWindow, recnList, account, account, recnData);
  if (recnData == NULL)
    return;

  gnc_reconcile_list_refresh(GNC_RECONCILE_LIST(recnData->debit));
  gnc_reconcile_list_refresh(GNC_RECONCILE_LIST(recnData->credit));

  recnRecalculateBalance(recnData);
}


/********************************************************************\
 * recnRecalculateBalance                                           *
 *   refreshes the balances in the reconcile window                 *
 *                                                                  *
 * Args:   recnData -- the reconcile window to refresh              *
 * Return: the reconciled balance                                   *
\********************************************************************/
static double
recnRecalculateBalance(RecnWindow *recnData)
{
  char *amount;
  double value;
  double ddebit  = 0.0;
  double dcredit = 0.0;
  double ddiff   = 0.0;
  short shares = PRTSYM;
  int account_type;
  
  account_type = xaccAccountGetType(recnData->account);
  if ((account_type == STOCK ) || (account_type == MUTUAL))
    shares |= PRTSHR;

  value = xaccAccountGetReconciledBalance(recnData->account);
  amount = xaccPrintAmount(value, shares);
  gtk_label_set_text(GTK_LABEL(recnData->starting), amount);

  amount = xaccPrintAmount(value - recnData->ddiff, shares);
  gtk_label_set_text(GTK_LABEL(recnData->ending), amount);
  
  ddebit = gnc_reconcile_list_reconciled_balance
    (GNC_RECONCILE_LIST(recnData->debit));
  dcredit = gnc_reconcile_list_reconciled_balance
    (GNC_RECONCILE_LIST(recnData->credit));

  /* Update the difference field, and the total fields */
  amount = xaccPrintAmount(DABS(ddebit), shares);
  gtk_label_set_text(GTK_LABEL(recnData->total_debit), amount);

  amount = xaccPrintAmount(dcredit, shares);
  gtk_label_set_text(GTK_LABEL(recnData->total_credit), amount);

  ddiff = recnData->ddiff + dcredit - ddebit;
  amount = xaccPrintAmount(ddiff, shares);
  gtk_label_set_text(GTK_LABEL(recnData->difference), amount);

  return ddiff;
}


/********************************************************************\
 * startRecnWindow                                                  *
 *   opens up the window to prompt the user to enter the ending     *
 *   balance from bank statement                                    *
 *                                                                  *
 * NOTE: This dialog does not return until the user presses "Ok"    *
 *       or "Cancel"                                                *
 *                                                                  *
 * Args:   parent  - the parent of this window                      *
 *         acc     - the account to reconcile                       *
 *         diff    - returns the amount from ending balance field   *
 * Return: True, if the user presses "Ok", else False               *
\********************************************************************/
gboolean
startRecnWindow(GtkWidget *parent, Account *account, double *diff)
{
  GtkWidget *dialog, *end_value;
  char *amount, *title;
  double dendBalance, value;
  int result;
  short shares = PRTSYM;
  int account_type;
  gchar *string;

  /* Get the previous ending balance.  Use the published
   * account interface for this, since the ending balance
   * may have to be adjusted for stock price fluctuations.
   */
  dendBalance = xaccAccountGetReconciledBalance(account);

  account_type = xaccAccountGetType(account);
  if ((account_type == STOCK) || (account_type == MUTUAL))
    shares |= PRTSHR;

  amount = xaccPrintAmount(dendBalance, shares);

  /* Create the dialog box... */
  asprintf(&title, "%s: %s", xaccAccountGetName(account), RECONCILE_STR);

  dialog = gnome_dialog_new(title,
                            GNOME_STOCK_BUTTON_OK,
                            GNOME_STOCK_BUTTON_CANCEL,
                            NULL);
  free(title);
  
  gnome_dialog_set_default(GNOME_DIALOG(dialog), 0);
  gnome_dialog_set_close(GNOME_DIALOG(dialog), TRUE);
  gnome_dialog_close_hides(GNOME_DIALOG(dialog), TRUE);
  gnome_dialog_set_parent(GNOME_DIALOG(dialog), GTK_WINDOW(parent));

  {
    GtkWidget *frame = gtk_frame_new("Reconcile Information");
    GtkWidget *main_area = gtk_hbox_new(FALSE, 5);
    GtkWidget *left_column = gtk_vbox_new(TRUE, 0);
    GtkWidget *right_column = gtk_vbox_new(TRUE, 0);
    GtkWidget *prev_title = gtk_label_new(PREV_BALN_C_STR);
    GtkWidget *end_title = gtk_label_new(END_BALN_C_STR);
    GtkWidget *prev_value = gtk_label_new(amount);
    GtkWidget *vbox = GNOME_DIALOG(dialog)->vbox;
    end_value = gtk_entry_new();

    amount = xaccPrintAmount(dendBalance - *diff, shares & PRTSHR);
    gtk_entry_set_text(GTK_ENTRY(end_value), amount);
    gtk_editable_select_region(GTK_EDITABLE(end_value), 0, -1);

    gnome_dialog_editable_enters(GNOME_DIALOG(dialog),
                                 GTK_EDITABLE(end_value));

    gtk_misc_set_alignment(GTK_MISC(end_title), 0.95, 0.5);
    gtk_misc_set_alignment(GTK_MISC(prev_title), 0.95, 0.5);
    gtk_misc_set_alignment(GTK_MISC(prev_value), 0, 0.5);

    gtk_container_set_border_width(GTK_CONTAINER(main_area), 10);
    gtk_container_set_border_width(GTK_CONTAINER(frame), 5);
    gtk_container_add(GTK_CONTAINER(frame), main_area);

    gtk_box_pack_start(GTK_BOX(vbox), frame, TRUE, TRUE, 0);

    gtk_box_pack_start(GTK_BOX(main_area), left_column, FALSE, FALSE, 0);
    gtk_box_pack_end(GTK_BOX(main_area), right_column, TRUE, TRUE, 0);
    
    gtk_box_pack_start(GTK_BOX(left_column), prev_title, TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(left_column), end_title, TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(right_column), prev_value, TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(right_column), end_value, TRUE, TRUE, 0);

    gtk_widget_show_all(dialog);

    gtk_widget_grab_focus(end_value);
  }
    
  while (1)
  {
    result = gnome_dialog_run(GNOME_DIALOG(dialog));

    if (result == 0) /* ok button */
    {
      string = gtk_entry_get_text(GTK_ENTRY(end_value));

      if(sscanf(string, "%lf", &value) == 1)
      {
        *diff = dendBalance - value;
        break;
      }
      else
      {
        gnc_error_dialog_parented(GTK_WINDOW(parent),
                                  "Ending balance must be a number.");
        continue;
      }
    }

    /* cancel or delete */
    break;
  }

  gtk_widget_destroy(dialog);

  return (result == 0);
}


static void
gnc_reconcile_window_list_cb(GNCReconcileList *list, Split *split,
                             gpointer data)
{
  RecnWindow *recnData = (RecnWindow *) data;

  recnRecalculateBalance(recnData);
}

static GtkWidget *
gnc_reconcile_window_create_list_frame(Account *account,
                                       GNCReconcileListType type,
                                       RecnWindow *recnData,
                                       GtkWidget **list_save,
                                       GtkWidget **total_save)
{
  GtkWidget *frame, *scrollWin, *list, *vbox, *label, *hbox;
  gchar * title;

  if (type == RECLIST_DEBIT)
    title = DEBITS_STR;
  else
    title = CREDITS_STR;

  vbox = gtk_vbox_new(FALSE, 5);

  frame = gtk_frame_new(title);

  list = gnc_reconcile_list_new(account, type);
  *list_save = list;

  gtk_signal_connect(GTK_OBJECT(list), "toggle_reconciled",
                     GTK_SIGNAL_FUNC(gnc_reconcile_window_list_cb), recnData);

  scrollWin = gtk_scrolled_window_new (NULL, NULL);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW (scrollWin),
				 GTK_POLICY_NEVER, 
				 GTK_POLICY_AUTOMATIC);
  gtk_container_set_border_width(GTK_CONTAINER(scrollWin), 5);
    
  gtk_container_add(GTK_CONTAINER(frame), scrollWin);
  gtk_container_add(GTK_CONTAINER(scrollWin), list);
  gtk_box_pack_start(GTK_BOX(vbox), frame, TRUE, TRUE, 0);

  hbox = gtk_hbox_new(FALSE, 5);
  gtk_box_pack_start(GTK_BOX(vbox), hbox, FALSE, FALSE, 0);

  label = gtk_label_new(TOTAL_C_STR);
  gtk_misc_set_alignment(GTK_MISC(label), 0.95, 0.5);
  gtk_box_pack_start(GTK_BOX(hbox), label, TRUE, TRUE, 0);

  label = gtk_label_new("");
  gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 0);
  *total_save = label;

  return vbox;
}


static void
gnc_ui_reconcile_window_help_cb(GtkWidget *widget, gpointer data)
{
  helpWindow(NULL, HELP_STR, HH_RECNWIN);
}

static void
gnc_ui_reconcile_window_change_cb(GtkButton *button, gpointer data)
{
  RecnWindow *recnData = (RecnWindow *) data;
  double ddiff = recnData->ddiff;
  
  if (startRecnWindow(recnData->dialog, recnData->account, &ddiff))
  {
    recnData->ddiff = ddiff;
    recnRecalculateBalance(recnData);
  }
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
recnWindow(GtkWidget *parent, Account *account)
{
  RecnWindow *recnData;
  double ddiff = 0.0;
  gchar *title = NULL;
  
  FETCH_FROM_LIST(RecnWindow, recnList, account, account, recnData);

  /* Popup a little window to prompt the user to enter the
   * ending balance for his/her bank statement */
  if (!startRecnWindow(parent, account, &ddiff))
  {
    REMOVE_FROM_LIST(RecnWindow, recnList, account, account);
    free(recnData);
    return NULL;
  }

  recnData->ddiff = ddiff;
  
  asprintf(&title, "%s: %s", xaccAccountGetName(account), RECONCILE_STR);
  
  recnData->dialog = gnome_dialog_new(title,
                                      GNOME_STOCK_BUTTON_OK,
                                      GNOME_STOCK_BUTTON_CANCEL,
                                      GNOME_STOCK_BUTTON_HELP,
                                      NULL);
  free(title);

  /* here we connect the "destroy" event to a signal handler.  
   * This event occurs when we call gtk_widget_destroy() on the window,
   * or if we return 'FALSE' in the "delete_event" callback.
   * Eventually executed by gnome_dialog_close() */
  gtk_signal_connect (GTK_OBJECT (recnData->dialog), "destroy",
                      GTK_SIGNAL_FUNC(recnClose), (gpointer) recnData);

  /* Ok is default */
  gnome_dialog_set_default(GNOME_DIALOG(recnData->dialog), 0);

  /* Buttons don't close automatically */
  gnome_dialog_set_close(GNOME_DIALOG(recnData->dialog), FALSE);

  /* destroy, don't hide */
  gnome_dialog_close_hides(GNOME_DIALOG(recnData->dialog), FALSE);
  
  gnome_dialog_button_connect(GNOME_DIALOG(recnData->dialog), 0,
                              GTK_SIGNAL_FUNC(recnOkCB),
                              (gpointer) recnData);

  gnome_dialog_button_connect(GNOME_DIALOG(recnData->dialog), 1,
                              GTK_SIGNAL_FUNC(recnCancelCB),
                              (gpointer) recnData);

  gnome_dialog_button_connect(GNOME_DIALOG(recnData->dialog), 2,
                              GTK_SIGNAL_FUNC(gnc_ui_reconcile_window_help_cb),
                              NULL);

  {
    GtkWidget *main_area = gtk_vbox_new(FALSE, 10);
    GtkWidget *debcred_area = gtk_hbox_new(FALSE, 15);
    GtkWidget *debits_frame;
    GtkWidget *credits_frame;

    debits_frame = gnc_reconcile_window_create_list_frame
      (account, RECLIST_DEBIT, recnData,
       &recnData->debit, &recnData->total_debit);
    credits_frame = gnc_reconcile_window_create_list_frame
      (account, RECLIST_CREDIT, recnData,
       &recnData->credit, &recnData->total_credit);

    gtk_container_set_border_width(GTK_CONTAINER(main_area), 5);
    gtk_box_pack_start(GTK_BOX(GNOME_DIALOG(recnData->dialog)->vbox),
                       main_area, TRUE, TRUE, 0);

    gtk_box_pack_start(GTK_BOX(main_area), debcred_area, TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(debcred_area), debits_frame, TRUE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(debcred_area), credits_frame, TRUE, FALSE, 0);

  {
    GtkWidget *hbox     = gtk_hbox_new(FALSE, 5);

    GtkWidget *prev_title = gtk_label_new(PREV_BALN_C_STR);
    GtkWidget *end_title  = gtk_label_new(END_BALN_C_STR);
    GtkWidget *space      = gtk_label_new("");
    GtkWidget *prev_value = gtk_label_new("");
    GtkWidget *end_value  = gtk_label_new("");

    GtkWidget *change_end = gtk_button_new();
    GtkWidget *difference_frame = gtk_frame_new(NULL);
    GtkWidget *difference_box = gtk_hbox_new(FALSE, 5);
    GtkWidget *difference_label = gtk_label_new(DIFF_C_STR);
    GtkWidget *difference_value = gtk_label_new("");

    gtk_container_add(GTK_CONTAINER(change_end), end_value);
    gtk_container_set_border_width(GTK_CONTAINER(change_end), 4);
    gtk_button_set_relief(GTK_BUTTON(change_end), GTK_RELIEF_HALF);

    gtk_signal_connect(GTK_OBJECT(change_end), "clicked",
                       GTK_SIGNAL_FUNC(gnc_ui_reconcile_window_change_cb),
                       recnData);

    gtk_misc_set_alignment(GTK_MISC(prev_title), 0.95, 0.5);
    gtk_misc_set_alignment(GTK_MISC(end_title), 0.95, 0.5);
    gtk_misc_set_alignment(GTK_MISC(difference_label), 0.95, 0.5);

    recnData->starting = prev_value;
    recnData->ending = end_value;
    recnData->difference = difference_value;

    gtk_box_pack_start(GTK_BOX(main_area), hbox, FALSE, FALSE, 0);

    gtk_box_pack_start(GTK_BOX(hbox), prev_title, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(hbox), prev_value, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(hbox), space, FALSE, FALSE, 3);
    gtk_box_pack_start(GTK_BOX(hbox), end_title, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(hbox), change_end, FALSE, FALSE, 0);
    gtk_box_pack_end(GTK_BOX(hbox), difference_frame, FALSE, FALSE, 0);

    gtk_frame_set_shadow_type(GTK_FRAME(difference_frame), GTK_SHADOW_IN);
    gtk_container_add(GTK_CONTAINER(difference_frame), difference_box); 

    gtk_container_set_border_width(GTK_CONTAINER(difference_box), 5);
    gtk_box_pack_start(GTK_BOX(difference_box), difference_label,
                       TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(difference_box), difference_value,
                       FALSE, FALSE, 0);
  }

    /* Set up the data */
    recnRefresh(account);

    /* Clamp down on the size */
    {
      gint row_height, num_debits, num_credits, num_show;

      gtk_widget_realize(recnData->credit);
      row_height = gnc_reconcile_list_get_row_height
        (GNC_RECONCILE_LIST(recnData->credit));

      num_credits = gnc_reconcile_list_get_num_splits
        (GNC_RECONCILE_LIST(recnData->credit));
      num_debits = gnc_reconcile_list_get_num_splits
        (GNC_RECONCILE_LIST(recnData->debit));

      num_show = MAX(num_debits, num_credits);
      num_show = MIN(num_show, 15) + 2;

      gtk_widget_set_usize(recnData->credit, 0, row_height * num_show);
      gtk_widget_set_usize(recnData->debit, 0, row_height * num_show);
    }
  }

  gtk_widget_show_all(recnData->dialog);
    
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

  if (recnData->dialog == NULL)
    return;

  gtk_widget_show(recnData->dialog);

  if (recnData->dialog->window == NULL)
    return;

  gdk_window_raise(recnData->dialog->window);
}


/********************************************************************\
 * Don't delete any structures -- the close callback will handle this *
\********************************************************************/

void 
xaccDestroyRecnWindow(Account *account)
{
  RecnWindow *recnData = NULL;

  DEBUG("Destroying reconcile window");

  FIND_IN_LIST(RecnWindow, recnList, account, account, recnData);
  if (recnData == NULL)
    return;

  gnome_dialog_close(GNOME_DIALOG(recnData->dialog));
}


/********************************************************************\
 * recnClose                                                        *
 *   frees memory allocated for an recnWindow, and other cleanup    *
 *   stuff                                                          *
 *                                                                  *
 * Args:   w    - the widget that called us                         *
 *         data - the data struct for this window                   *
 * Return: none                                                     *
\********************************************************************/
static void 
recnClose(GtkWidget *w, gpointer data)
{
  RecnWindow *recnData = (RecnWindow *) data;
  Account *account = recnData->account;
  
  REMOVE_FROM_LIST(RecnWindow, recnList, account, account);
  free(recnData);
}


/********************************************************************\
 * recnOkCB                                                         *
 *   saves account stuff, when the user clicks "Ok"                 *
 *                                                                  *
 * Args:   w    - the widget that called us                         *
 *         data - the data struct for this window                   *
 * Return: none                                                     *
\********************************************************************/
static void 
recnOkCB(GtkWidget *w, gpointer data)
{
  RecnWindow  *recnData = (RecnWindow *) data;

  if (!DEQ(recnRecalculateBalance(recnData), 0.0))
    if (!gnc_verify_dialog_parented(GTK_WINDOW(recnData->dialog),
                                    "The account is not balanced.\n" \
                                    "Are you sure you want to finish?",
                                    GNC_F))
      return;

  gnc_reconcile_list_commit(GNC_RECONCILE_LIST(recnData->credit));
  gnc_reconcile_list_commit(GNC_RECONCILE_LIST(recnData->debit));

  /* refresh the register window */
  xaccAccountDisplayRefresh(recnData->account);
  
  gnome_dialog_close(GNOME_DIALOG(recnData->dialog));
}

static void 
recnCancelCB(GtkWidget *w, gpointer data)
{
  RecnWindow  *recnData = (RecnWindow *) data;

  gnome_dialog_close(GNOME_DIALOG(recnData->dialog));
}
