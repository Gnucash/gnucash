/********************************************************************\
 * window-adjust.c -- the adjust balance window                     *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1998 Linas Vepstas                                 *
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

#include <gnome.h>
#include <time.h>

#include "top-level.h"

#include "ui-callbacks.h"
#include "MultiLedger.h"
#include "AdjBWindow.h"
#include "Refresh.h"
#include "window-reconcile.h"
#include "dialog-utils.h"
#include "query-user.h"
#include "messages.h"
#include "util.h"


/** STRUCTS *********************************************************/
struct _AdjBWindow
{
  Account * account;         /* The account that we are adjusting    */
  GtkWidget * dialog;        /* The adjust balance dialog            */
  GtkWidget * balance_entry; /* Text field, the new balance          */
  GtkWidget * date_entry;    /* Date field, the date for the balance */
};

/** GLOBALS *********************************************************/
static AdjBWindow **adjBList = NULL;

/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_GUI;


static int
gnc_ui_adjBWindow_close_cb(GnomeDialog *dialog, gpointer user_data)
{
  AdjBWindow * adjBData = (AdjBWindow *) user_data;
  Account *account = adjBData->account;

  DEBUG("Closing adjust balance window");

  REMOVE_FROM_LIST (AdjBWindow, adjBList, account, account); 

  free(adjBData);

  /* really close */
  return FALSE;
}


static void
gnc_ui_AdjBWindow_cancel_cb(GtkWidget * widget, gpointer data)
{
  AdjBWindow *adjBData = (AdjBWindow *) data;

  gnome_dialog_close(GNOME_DIALOG(adjBData->dialog));
}


static void
gnc_ui_AdjBWindow_ok_cb(GtkWidget * widget, gpointer data)
{
  AdjBWindow *adjBData = (AdjBWindow *) data;
  Transaction *trans;
  Split *source_split;
  time_t time;
  double new_balance, current_balance;
  gchar * string;

  string = gtk_entry_get_text(GTK_ENTRY(adjBData->balance_entry));

  if(sscanf(string, "%lf", &new_balance) != 1)
  {
    gnc_error_dialog_parented(GTK_WINDOW(adjBData->dialog),
                              "Balance must be a number.");
    return;
  }

  time = gnome_date_edit_get_date(GNOME_DATE_EDIT(adjBData->date_entry));

  trans = xaccMallocTransaction();
  
  xaccTransBeginEdit(trans, 0);

  xaccTransSetDateSecs(trans, time);
  xaccTransSetDescription(trans, ADJ_BALN_STR);

  source_split = xaccTransGetSplit(trans, 0);

  xaccAccountBeginEdit(adjBData->account, 0);
  xaccAccountInsertSplit(adjBData->account, source_split);

  /* Compute the dollar amount this transaction should have.
   * It is the difference between the current balance, and
   * the desired balance. */
  current_balance = xaccSplitGetBalance(source_split);
  xaccSplitSetValue(source_split, new_balance - current_balance);

  xaccAccountCommitEdit(adjBData->account);
  xaccTransCommitEdit(trans);
  
  gnc_account_ui_refresh(adjBData->account);
  gnc_refresh_main_window();

  gnome_dialog_close(GNOME_DIALOG(adjBData->dialog));
}


/********************************************************************\
 * adjBWindow                                                       *
 *   opens up the window to adjust the balance                      *
 *                                                                  *
 * Args:   account - the account to adjust                          *
 * Return: adjBData - the instance of this AdjBWindow               *
\********************************************************************/
AdjBWindow *
adjBWindow(Account *account)
{
  GtkWidget *dialog, *frame, *vbox;
  AdjBWindow *adjBData;
  gchar *title, *name;
   
  FETCH_FROM_LIST(AdjBWindow, adjBList, account, account, adjBData);

  name = gnc_ui_get_account_full_name(account, ":");
  title = g_strconcat(name, " - ", ADJ_BALN_STR, NULL);

  dialog = gnome_dialog_new(title,
			    GNOME_STOCK_BUTTON_OK,
			    GNOME_STOCK_BUTTON_CANCEL,
			    NULL);

  g_free(name);
  g_free(title);

  adjBData->account = account;
  adjBData->dialog = dialog;

  /* default to ok */
  gnome_dialog_set_default(GNOME_DIALOG(dialog), 0);

  /* destroy, don't hide */
  gnome_dialog_close_hides(GNOME_DIALOG(dialog), FALSE);

  vbox = GNOME_DIALOG(dialog)->vbox;

  frame = gtk_frame_new(NULL);
  gtk_container_set_border_width(GTK_CONTAINER(frame), 5);
  gtk_box_pack_start(GTK_BOX(vbox), frame, TRUE, TRUE, 0);
  gtk_widget_show(frame);

  {
    GtkWidget *hbox, *vbox;
    GtkWidget *amount, *date;
    GtkWidget *label;
    gchar *string;

    hbox = gtk_hbox_new(FALSE, 5);
    gtk_container_set_border_width(GTK_CONTAINER(hbox), 10);
    gtk_container_add(GTK_CONTAINER(frame), hbox);
    gtk_widget_show(hbox);

    /* Label box */
    vbox = gtk_vbox_new(TRUE, 3);
    gtk_widget_show(vbox);

    /* Date label */
    label = gtk_label_new(DATE_STR);
    gtk_misc_set_alignment(GTK_MISC(label), 0.95, 0.5);
    gtk_widget_show(label);
    gtk_box_pack_start(GTK_BOX(vbox), label, TRUE, TRUE, 0);

    /* new balance label */
    string = g_strconcat(NEW_BALN_STR, " ", CURRENCY_SYMBOL, NULL);
    label = gtk_label_new(string);
    g_free(string);
    gtk_misc_set_alignment(GTK_MISC(label), 0.95, 0.5);
    gtk_widget_show(label);
    gtk_box_pack_start(GTK_BOX(vbox), label, TRUE, TRUE, 0);

    gtk_box_pack_start(GTK_BOX(hbox), vbox, FALSE, FALSE, 0);

    /* Edit widget box */
    vbox = gtk_vbox_new(TRUE, 3);
    gtk_widget_show(vbox);

    date = gnome_date_edit_new(time(NULL), FALSE, FALSE);
    gtk_widget_show(date);
    gtk_box_pack_start(GTK_BOX(vbox), date, TRUE, TRUE, 0);
    adjBData->date_entry = date;

    amount = gtk_entry_new();
    gtk_widget_show(amount);
    gtk_box_pack_start(GTK_BOX(vbox), amount, TRUE, TRUE, 0);
    adjBData->balance_entry = amount;
    gnome_dialog_editable_enters(GNOME_DIALOG(dialog), GTK_EDITABLE(amount));

    gtk_box_pack_start(GTK_BOX(hbox), vbox, TRUE, TRUE, 0);
  }

  gnome_dialog_button_connect
    (GNOME_DIALOG(dialog), 0,
     GTK_SIGNAL_FUNC(gnc_ui_AdjBWindow_ok_cb), adjBData);

  gnome_dialog_button_connect
    (GNOME_DIALOG(dialog), 1,
     GTK_SIGNAL_FUNC(gnc_ui_AdjBWindow_cancel_cb), adjBData);

  gtk_signal_connect(GTK_OBJECT(dialog), "close",
		     GTK_SIGNAL_FUNC (gnc_ui_adjBWindow_close_cb),
		     adjBData);

  gtk_widget_show(dialog);

  return adjBData;
}

/********************************************************************\
 * Don't delete any structures, the close callback will do this     *
\********************************************************************/

void
xaccDestroyAdjBWindow (Account *account)
{
  AdjBWindow *adjBData;

  FIND_IN_LIST(AdjBWindow, adjBList, account, account, adjBData); 

  if (adjBData == NULL)
    return;
 
  gnome_dialog_close(GNOME_DIALOG(adjBData->dialog));
}


/******************** END OF FILE ***********************************\
\********************************************************************/
