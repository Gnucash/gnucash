/********************************************************************\
 * window-adjust.c -- the adjust balance window                     *
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
\********************************************************************/

#include "top-level.h"

#include <gnome.h>
#include <time.h>

#include "gnome-top-level.h"
#include "ui-callbacks.h"
#include "MultiLedger.h"
#include "AdjBWindow.h"
#include "Refresh.h"
#include "window-reconcile.h"
#include "dialog-utils.h"
#include "query-user.h"
#include "gnc-dateedit.h"
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


/** Prototypes ******************************************************/
static void gnc_adjb_set_window_name(AdjBWindow *adjBData);


/********************************************************************\
 * adjBRefresh                                                      *
 *   refreshes the adjust balance window                            *
 *                                                                  *
 * Args:   account - the account of the window to refresh           *
 * Return: none                                                     *
\********************************************************************/
void
adjBRefresh(Account *account)
{
  AdjBWindow *adjBData; 

  FIND_IN_LIST (AdjBWindow, adjBList, account, account, adjBData);
  if (adjBData == NULL)
    return;

  gnc_adjb_set_window_name(adjBData);
}


static int
gnc_ui_adjBWindow_close_cb(GnomeDialog *dialog, gpointer user_data)
{
  AdjBWindow * adjBData = (AdjBWindow *) user_data;
  Account *account = adjBData->account;

  DEBUG("Closing adjust balance window\n");

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
  new_balance = xaccParseAmount(string, GNC_T);
  if (gnc_reverse_balance(adjBData->account))
    new_balance = -new_balance;

  time = gnc_date_edit_get_date(GNC_DATE_EDIT(adjBData->date_entry));

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


static gboolean
gnc_adjust_update_cb(GtkWidget *widget, GdkEventFocus *event, gpointer data)
{
  GtkEntry *entry = GTK_ENTRY(widget);
  Account *account = data;
  gchar *new_string;
  const char *currency;
  gchar *string;
  double value;

  string = gtk_entry_get_text(entry);

  value = xaccParseAmount(string, GNC_T);

  currency = xaccAccountGetCurrency(account);

  new_string = xaccPrintAmount(value, PRTSEP, currency);

  if (safe_strcmp(string, new_string) == 0)
    return FALSE;

  gtk_entry_set_text(entry, new_string);

  return FALSE;
}


static char *
gnc_adjb_make_window_name(Account *account)
{
  char *fullname;
  char *title;

  fullname = xaccAccountGetFullName(account, gnc_get_account_separator());
  title = g_strconcat(fullname, " - ", ADJ_BALN_STR, NULL);

  free(fullname);

  return title;
}

static void
gnc_adjb_set_window_name(AdjBWindow *adjBData)
{
  char *title;

  title = gnc_adjb_make_window_name(adjBData->account);

  gtk_window_set_title(GTK_WINDOW(adjBData->dialog), title);

  g_free(title);
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
  gchar *title;

  FETCH_FROM_LIST(AdjBWindow, adjBList, account, account, adjBData);

  title = gnc_adjb_make_window_name(account);

  dialog = gnome_dialog_new(title,
			    GNOME_STOCK_BUTTON_OK,
			    GNOME_STOCK_BUTTON_CANCEL,
			    NULL);

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

  {
    GtkTooltips *tooltips;
    GtkWidget *hbox, *vbox;
    GtkWidget *amount, *date;
    GtkWidget *label, *entry;
    const char *currency;
    gchar *string;

    tooltips = gtk_tooltips_new();

    hbox = gtk_hbox_new(FALSE, 5);
    gtk_container_set_border_width(GTK_CONTAINER(hbox), 10);
    gtk_container_add(GTK_CONTAINER(frame), hbox);

    /* Label box */
    vbox = gtk_vbox_new(TRUE, 3);

    /* Date label */
    string = g_strconcat(DATE_STR, ":", NULL);
    label = gtk_label_new(string);
    g_free(string);
    gtk_misc_set_alignment(GTK_MISC(label), 1.0, 0.5);
    gtk_box_pack_start(GTK_BOX(vbox), label, TRUE, TRUE, 0);

    /* new balance label */
    string = g_strconcat(NEW_BALN_STR, ":", NULL);
    label = gtk_label_new(string);
    g_free(string);
    gtk_misc_set_alignment(GTK_MISC(label), 1.0, 0.5);
    gtk_box_pack_start(GTK_BOX(vbox), label, TRUE, TRUE, 0);

    gtk_box_pack_start(GTK_BOX(hbox), vbox, FALSE, FALSE, 0);

    /* Edit widget box */
    vbox = gtk_vbox_new(TRUE, 3);

    date = gnc_date_edit_new(time(NULL), FALSE, FALSE);
    gtk_box_pack_start(GTK_BOX(vbox), date, TRUE, TRUE, 0);
    adjBData->date_entry = date;

    entry = GNC_DATE_EDIT(date)->date_entry;
    gtk_tooltips_set_tip(tooltips, entry, TOOLTIP_ADJUST_DATE, NULL);

    amount = gtk_entry_new();
    gtk_box_pack_start(GTK_BOX(vbox), amount, TRUE, TRUE, 0);
    adjBData->balance_entry = amount;

    gtk_tooltips_set_tip(tooltips, amount, TOOLTIP_ADJUST_AMOUNT, NULL);

    currency = xaccAccountGetCurrency(account);
    string = xaccPrintAmount(0.0, PRTSEP, currency);
    gtk_entry_set_text(GTK_ENTRY(amount), string);
    gtk_entry_select_region(GTK_ENTRY(amount), 0, -1);

    gtk_signal_connect(GTK_OBJECT(amount), "focus-out-event",
                       GTK_SIGNAL_FUNC(gnc_adjust_update_cb), account);

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

  gtk_widget_grab_focus(adjBData->balance_entry);

  gtk_widget_show_all(dialog);

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
