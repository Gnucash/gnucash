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

#include "config.h"

#include "Account.h"
#include "date.h"
#include "Group.h"
#include "MultiLedger.h"
#include "top-level.h"
#include "MainWindow.h"
#include "RegWindow.h"
#include "window-reconcile.h"
#include "messages.h"
#include "util.h"


/** STRUCTS *********************************************************/
struct _RecnWindow
{
  Account *acc;             /* The account that we are reconciling  */
  double  ddiff;            /* The amount to reconcile              */
  GtkWidget  *dialog;       /* The reconcile window dialog          */
  GtkWidget  *difference;   /* Text field, amount left to reconcile */
  GtkWidget  *totDebit;     /* Text field, total debit reconciled   */
  GtkWidget  *totCredit;    /* Text field, total credit reconciled  */
  GtkWidget  *debit;        /* Debit matrix show unreconciled debit */
  GtkWidget  *credit;       /* Credit matrix, shows credits...      */
  char *  symbol;           /* Currency symbol or 's' for shares    */
};

/** PROTOTYPES ******************************************************/
static void recnRecalculateBalance( RecnWindow *recnData );

static void recnClose(GtkWidget *w, gpointer data);
static void recnOkCB(GtkWidget *w, gpointer data);
static void recnCancelCB(GtkWidget *w, gpointer data);
static void recnCB(GtkWidget *w, gpointer data);

/** GLOBALS *********************************************************/

static RecnWindow **recnList = NULL;

/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_GUI;

/********************************************************************/


/********************************************************************\
 * recnRefresh                                                      *
 *   refreshes the transactions in the reconcile window             *
 *                                                                  *
 * Args:   recnData -- the reconcile window to refresh              *
 * Return: none                                                     *
\********************************************************************/
void
recnRefresh(Account *acc)
{
  int   i;
  Split *split;
  RecnWindow *recnData; 
  GList *debit_items = NULL;
  GList *credit_items = NULL;
  
  FIND_IN_LIST (RecnWindow, recnList, acc, acc, recnData);
  if(!recnData)
    return;
  
  /* Build lists of the non-reconciled transactions */
  i=0;
  split = xaccAccountGetSplit(acc, i);
  while(split)
  {
    char *item_str = NULL;
    Transaction *trans = xaccSplitGetParent (split);
    const char split_recn = xaccSplitGetReconcile(split);
    
    if( YREC != split_recn)
    {
      double themount;
      int acc_type;
      
      /* for stock accounts, show share quantity, 
       * not currency amount */
      acc_type = xaccAccountGetType (acc);
      if((STOCK == acc_type) || (MUTUAL == acc_type)) {
        themount = xaccSplitGetShareAmount (split);
      } else {
        themount = xaccSplitGetValue (split);
      }

      asprintf(&item_str, "%c %s %s %s %.2f",
               split_recn,
               xaccTransGetNum(trans),
               xaccTransGetDateStr(trans),
               xaccTransGetDescription(trans),
               DABS(themount));

      {
        GtkWidget *list_item = gtk_list_item_new();
        GtkWidget *check_button = gtk_check_button_new_with_label(item_str);

        gtk_signal_connect(GTK_OBJECT(check_button), "toggled",
                           GTK_SIGNAL_FUNC(recnCB), (gpointer) recnData);

        gtk_container_add(GTK_CONTAINER(list_item), check_button);
        gtk_widget_show(list_item);
        gtk_widget_show(check_button);
        
        gtk_object_set_user_data(GTK_OBJECT(check_button), (gpointer) split);

        if(themount < 0)
        {
          PINFO ("Adding debit: %s\n", item_str);
          debit_items = g_list_append(debit_items, (gpointer) list_item);
        } else {
          PINFO("Adding credit: %s\n", item_str);
          credit_items = g_list_append(credit_items, (gpointer) list_item);
        }

        free(item_str);
      }
    }
    i++;
    split = xaccAccountGetSplit (acc, i);
  }

  /* NOTE: an improvement of the current design would be to use the
   *       user-data in the rows to detect where transactions need
   *       to be inserted/delete, instead of deleting and re-inserting
   *       all the transactions! */
  
  /* Delete all the entries in the debit and credit matrices */
  gtk_list_clear_items(GTK_LIST(recnData->debit), 0, -1);
  gtk_list_clear_items(GTK_LIST(recnData->credit), 0, -1);

  /* the gtk list becomes the owner of the item lists, so don't free */
  gtk_list_append_items(GTK_LIST(recnData->debit), debit_items);
  gtk_list_append_items(GTK_LIST(recnData->credit), credit_items);

  recnRecalculateBalance(recnData);
}

static void
recn_recalc_share_balance_helper(gpointer item, gpointer data) {
  double *total = (double *) data;
  GtkListItem *li = GTK_LIST_ITEM(item);
  GtkCheckButton *checkbutton = GTK_CHECK_BUTTON(li->item.bin.child);
  Split *split = gtk_object_get_user_data(GTK_OBJECT(checkbutton));
  // const char recn = xaccSplitGetReconcile(split);

  if(GTK_TOGGLE_BUTTON(checkbutton)->active) {
    *total += xaccSplitGetShareAmount(split);
  }
}
  
static void
recn_recalc_non_share_balance_helper(gpointer item, gpointer data) {
  double *total = (double *) data;
  GtkListItem *li = GTK_LIST_ITEM(item);
  GtkCheckButton *checkbutton = GTK_CHECK_BUTTON(li->item.bin.child);
  Split *split = gtk_object_get_user_data(GTK_OBJECT(checkbutton));
  // const char recn = xaccSplitGetReconcile(split);
  
  if(GTK_TOGGLE_BUTTON(checkbutton)->active) {
    *total += xaccSplitGetValue(split);
  }
}

/********************************************************************\
 * recnRecalculateBalance                                           *
 *   refreshes the balances in the reconcile window                 *
 *                                                                  *
 * Args:   recnData -- the reconcile window to refresh              *
 * Return: none                                                     *
\********************************************************************/
static void
recnRecalculateBalance(RecnWindow *recnData)
{
  Account *acc = recnData ->acc;
  char *amt;
  double ddebit  = 0.0;
  double dcredit = 0.0;
  double ddiff   = 0.0;
  short shrs = 0;
  int acc_type;
  
  acc_type = xaccAccountGetType (acc);
  if ((STOCK == acc_type) || (MUTUAL == acc_type)) shrs = PRTSHR;
  
  /* Calculate the total debit: */
  ddebit = 0.0;

  {
    const GFunc func = (shrs) ? 
      (GFunc) recn_recalc_share_balance_helper :
      (GFunc) recn_recalc_non_share_balance_helper;
    
    /* Calculate the total debit and credit */
    g_list_foreach(GTK_LIST(recnData->debit)->children, func, &ddebit);
    g_list_foreach(GTK_LIST(recnData->credit)->children, func, &dcredit);
  }
  
  shrs |= PRTSYM;

  /* Update the difference field, and the total fields */
  amt = xaccPrintAmount(DABS(ddebit), shrs);
  {
    char *str = NULL;
    asprintf(&str, "%s %s", DEBITS_C_STR, amt);
    gtk_frame_set_label(GTK_FRAME(recnData->totDebit), str);
    free(str);
  }

  amt = xaccPrintAmount(dcredit, shrs);
  {
    char *str = NULL;
    asprintf(&str, "%s %s", CREDITS_C_STR, amt);
    gtk_frame_set_label(GTK_FRAME(recnData->totCredit), str);
    free(str);
  }

  ddiff = recnData->ddiff + dcredit + ddebit;
  amt = xaccPrintAmount(ddiff, shrs);
  gtk_label_set(GTK_LABEL(recnData->difference), amt);
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
startRecnWindow(GtkWidget *parent, Account *acc, double *diff)
{
  GtkWidget *dialog, *end_value;
  char *amt, *title;
  double dendBalance, val;
  int result;
  short shrs = 0;
  int acc_type;
  gchar *string;

  /* Get the previous ending balance.  Use the published
   * account interface for this, since the ending balance
   * may have to be adjusted for stock price fluctuations.
   */
  dendBalance = xaccAccountGetReconciledBalance(acc);

  acc_type = xaccAccountGetType(acc);
  if ((STOCK == acc_type) || (MUTUAL == acc_type))
    shrs = 1;

  shrs *= PRTSHR;
  shrs |= PRTSYM;
  amt = xaccPrintAmount (dendBalance, shrs);

  /* Create the dialog box... */
  asprintf(&title, "%s: %s", xaccAccountGetName(acc), RECONCILE_STR);

  dialog = gnome_dialog_new(title,
                            GNOME_STOCK_BUTTON_OK,
                            GNOME_STOCK_BUTTON_CANCEL,
                            NULL);
  free(title);
  
  gnome_dialog_set_default(GNOME_DIALOG(dialog), 0);
  gnome_dialog_set_close(GNOME_DIALOG(dialog), FALSE);
  gnome_dialog_close_hides(GNOME_DIALOG(dialog), TRUE);
  gnome_dialog_set_parent(GNOME_DIALOG(dialog), GTK_WINDOW(parent));

  {
    GtkWidget *frame = gtk_frame_new("Reconcile Information");
    GtkWidget *main_area = gtk_hbox_new(FALSE, 5);
    GtkWidget *left_column = gtk_vbox_new(TRUE, 0);
    GtkWidget *right_column = gtk_vbox_new(TRUE, 0);
    GtkWidget *prev_title = gtk_label_new(PREV_BALN_C_STR);
    GtkWidget *end_title = gtk_label_new(END_BALN_C_STR);
    GtkWidget *prev_value = gtk_label_new(amt);
    GtkWidget *vbox = GNOME_DIALOG(dialog)->vbox;
    end_value = gtk_entry_new();

    gnome_dialog_editable_enters(GNOME_DIALOG(dialog),
                                 GTK_EDITABLE(end_value));

    gtk_misc_set_alignment (GTK_MISC(end_title), 0.95, 0.5);
    gtk_misc_set_alignment (GTK_MISC(prev_title), 0.95, 0.5);
    gtk_misc_set_alignment (GTK_MISC(prev_value), 0, 0.5);

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

    gtk_widget_show(frame);
    gtk_widget_show(main_area);
    gtk_widget_show(left_column);
    gtk_widget_show(right_column);
    gtk_widget_show(prev_title);
    gtk_widget_show(end_title);
    gtk_widget_show(prev_value);
    gtk_widget_show(end_value); 
  }
    
  while (1)
  {
    result = gnome_dialog_run(GNOME_DIALOG(dialog));

    if (result == 0) /* ok button */
    {
      string = gtk_entry_get_text(GTK_ENTRY(end_value));

      if(sscanf(string, "%lf", &val ) == 1)
      {
        *diff = dendBalance - val;
        break;
      }
      else
      {
        gnc_error_dialog(_("Ending balance must be a number."));
        continue;
      }
    }

    /* cancel or delete */
    break;
  }

  gtk_widget_destroy(dialog);

  return (result == 0);
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
recnWindow(GtkWidget *parent, Account *acc)
{
  RecnWindow *recnData;
  double ddiff;
  gchar *title = NULL;
  
  FETCH_FROM_LIST(RecnWindow, recnList, acc, acc, recnData);

  /* Popup a little window to prompt the user to enter the
   * ending balance for his/her bank statement */
  if (!startRecnWindow(parent, acc, &ddiff))
    return NULL;

  recnData->ddiff = ddiff;
  
  asprintf(&title, "%s: %s", xaccAccountGetName (acc), RECONCILE_STR);
  
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

  PERR ("recnWindow(): Not implemented: helpMenubarCB\n");

  {
    GtkWidget *main_area = gtk_vbox_new(FALSE, 0);
    GtkWidget *debcred_area = gtk_hbox_new(TRUE, 0);
    GtkWidget *debits_frame = gtk_frame_new(DEBITS_C_STR);
    GtkWidget *credits_frame = gtk_frame_new(CREDITS_C_STR);
    GtkWidget *debits_list = gtk_list_new();
    GtkWidget *credits_list = gtk_list_new();
    
    GtkWidget *difference_align = gtk_alignment_new(0.5, 0.5, 0, 0);
    GtkWidget *difference_box = gtk_hbox_new(FALSE, 0);
    GtkWidget *difference_label = gtk_label_new(DIFF_C_STR);
    GtkWidget *difference_value = gtk_label_new("");
    
    gtk_box_pack_start(GTK_BOX(GNOME_DIALOG(recnData->dialog)->vbox),
                       main_area, TRUE, TRUE, 0);

    gtk_box_pack_start(GTK_BOX(main_area), debcred_area, TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(debcred_area), debits_frame, TRUE, TRUE, 0);
    gtk_box_pack_end(GTK_BOX(debcred_area), credits_frame, TRUE, TRUE, 0);
    gtk_container_add(GTK_CONTAINER(debits_frame), debits_list); 
    gtk_container_add(GTK_CONTAINER(credits_frame), credits_list); 
    
    gtk_box_pack_end(GTK_BOX(main_area), difference_align, TRUE, TRUE, 0);
    gtk_container_add(GTK_CONTAINER(difference_align), difference_box); 
    gtk_box_pack_start(GTK_BOX(difference_box), difference_label,
                       TRUE, TRUE, 0);
    gtk_box_pack_end(GTK_BOX(difference_box), difference_value,
                     TRUE, TRUE, 0);
    
    recnData->debit = debits_list;
    recnData->totDebit = debits_frame;
    recnData->credit = credits_list;
    recnData->totCredit = credits_frame;
    recnData->difference = difference_value;

    gtk_widget_show(main_area);
    gtk_widget_show(debcred_area);
    gtk_widget_show(debits_frame);
    gtk_widget_show(credits_frame);
    gtk_widget_show(debits_list);
    gtk_widget_show(credits_list);
    gtk_widget_show(difference_align);
    gtk_widget_show(difference_box);
    gtk_widget_show(difference_label);
    gtk_widget_show(difference_value);    
    gtk_widget_show(recnData->dialog);
  }
    
  /* now that the matrices are set up, fill 'em in with transactions: */
  recnRefresh (acc);

  /* and then refresh the total/difference balance fields: */
  recnRecalculateBalance(recnData);
  
  return recnData;
}


/********************************************************************\
 * Don't delete any structures -- the close callback will handle this *
\********************************************************************/

void 
xaccDestroyRecnWindow(Account *acc)
{
  RecnWindow *recnData = NULL;
  
  FIND_IN_LIST(RecnWindow, recnList, acc, acc, recnData);
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
  Account *acc = recnData->acc;
  
  REMOVE_FROM_LIST (RecnWindow, recnList, acc, acc);
  free(recnData);
  
  DEBUG("closed RecnWindow");
}

static void
recn_ok_cb_set_reconciled_helper(gpointer item, gpointer data) {
  // double *total = (double *) data;
  GtkListItem *li = GTK_LIST_ITEM(item);
  GtkCheckButton *checkbutton = GTK_CHECK_BUTTON(li->item.bin.child);
  Split *split = gtk_object_get_user_data(GTK_OBJECT(checkbutton));
  /*const char recn = xaccSplitGetReconcile(split);*/
  
  if(GTK_TOGGLE_BUTTON(checkbutton)->active) {
    xaccSplitSetReconcile (split, YREC);
  }
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

  /* Update the debit and credit transactions reconciled state */
  g_list_foreach(GTK_LIST(recnData->debit)->children,
                 recn_ok_cb_set_reconciled_helper, NULL);
  g_list_foreach(GTK_LIST(recnData->credit)->children,
                 recn_ok_cb_set_reconciled_helper, NULL);

  /* refresh the register window */
  xaccAccountDisplayRefresh (recnData->acc);
  
  gnome_dialog_close(GNOME_DIALOG(recnData->dialog));
}

static void 
recnCancelCB(GtkWidget *w, gpointer data)
{
  RecnWindow  *recnData = (RecnWindow *) data;
  PINFO ("X\n");
  gnome_dialog_close(GNOME_DIALOG(recnData->dialog));
  PINFO ("Y\n");
}


/********************************************************************\
 * recnCB                                                           *
 *   called whenever the users does anything in the debit/credit    *
 *   matrices                                                       *
 *                                                                  *
 * Args:   w    - the matrix widget that called us                  *
 *         data - the data struct for this window                   *
 * Return: none                                                     *
\********************************************************************/
static void
recnCB(GtkWidget *w, gpointer data)
{
  RecnWindow *recnData = (RecnWindow *) data;
  
  /* recalculate the total/difference balance fields: */
  recnRecalculateBalance(recnData);
}

/*
  Local Variables:
  tab-width: 2
  indent-tabs-mode: nil
  eval: (c-set-style "gnu")
  End:
*/
