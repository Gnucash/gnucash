/********************************************************************\
 * dialog-edit.c -- window for editing account information          *
 *                  (GnuCash)                                       *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1997, 1998, 1999 Linas Vepstas                     *
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

#include "top-level.h"

#include <gnome.h>
#include <stdio.h>

#include "AccWindow.h"
#include "MainWindow.h"
#include "Refresh.h"
#include "FileDialog.h"
#include "dialog-utils.h"
#include "messages.h"
#include "util.h"


/* From Account.c. One day, maybe this will be configurable. */
extern int unsafe_ops;

/* List of Open edit windows */
static EditAccWindow ** editAccList   = NULL;


struct _editaccwindow
{
  GtkWidget * dialog;

  Account * account;

  AccountEditInfo edit_info;
};


static int
gnc_ui_EditAccWindow_close_cb(GnomeDialog *dialog, gpointer user_data)
{
  EditAccWindow * editAccData = (EditAccWindow *) user_data;
  Account *acc = editAccData->account;

  REMOVE_FROM_LIST (EditAccWindow,editAccList,acc,account); 

  free(editAccData);

  /* really close */
  return FALSE;
}

static void
gnc_ui_EditAccWindow_cancel_cb(GtkWidget * widget,
			       gpointer data)
{
  EditAccWindow *editAccData = (EditAccWindow *) data; 

  gnome_dialog_close(GNOME_DIALOG(editAccData->dialog));
}

static void
gnc_ui_EditAccWindow_ok_cb(GtkWidget * widget,
			   gpointer data)
{
  EditAccWindow *editAccData = (EditAccWindow *) data; 
  AccountFieldStrings strings;
  Account * acc;
  char * old;

  gnc_ui_extract_field_strings(&strings, &editAccData->edit_info);

  /* check for valid name */
  if (safe_strcmp(strings.name, "") == 0)
  {
    gnc_error_dialog(ACC_NO_NAME_MSG);
    gnc_ui_free_field_strings(&strings);
    return;
  }

  /* fixme check for unique code, if entered */

  acc = editAccData->account;

  /* currency check */
  old = xaccAccountGetCurrency(acc);
  if (old == NULL)
    old = "";
  if ((safe_strcmp(old, strings.currency) != 0) &&
      (safe_strcmp(old, "") != 0))
  {
    gchar * s;
    gboolean result;

    s = g_strdup_printf(EDIT_CURRENCY_MSG, old, strings.currency);
    result = gnc_verify_dialog(s, GNC_T);
    g_free(s);

    if (!result)
    {
      gnc_ui_free_field_strings(&strings);
      return;
    }
  }

  /* security check */
  old = xaccAccountGetSecurity(acc);
  if (old == NULL)
    old = "";
  if ((safe_strcmp(old, strings.security) != 0) &&
      (safe_strcmp(old, "") != 0))
  {
    gchar * s;
    gboolean result;

    s = g_strdup_printf(EDIT_SECURITY_MSG, old, strings.security);
    result = gnc_verify_dialog(s, GNC_T);
    g_free(s);

    if (!result)
    {
      gnc_ui_free_field_strings(&strings);
      return;
    }
  }

  xaccAccountBeginEdit(acc, 0);
  gnc_ui_install_field_strings(acc, &strings, FALSE);
  xaccAccountCommitEdit (acc);

  gnc_ui_free_field_strings(&strings);

  gnc_refresh_main_window();
  gnc_group_ui_refresh(gncGetCurrentGroup());

  gnome_dialog_close(GNOME_DIALOG(editAccData->dialog));
}


/********************************************************************\
 * editAccWindow                                                    *
 *   opens up a window to edit an account                           * 
 *                                                                  * 
 * Args:   acc - the account to edit                                * 
 * Return: null                                                     *
\********************************************************************/
EditAccWindow *
editAccWindow(Account *acc)
{
  EditAccWindow * editAccData;
  GtkWidget *vbox, *widget, *dialog;
  char *name, *title;
  
  FETCH_FROM_LIST (EditAccWindow, editAccList, acc, account, editAccData);

  name = gnc_ui_get_account_full_name(acc, ":");
  title = g_strconcat(name, " - ", EDIT_ACCT_STR, NULL);

  dialog = gnome_dialog_new(title,
			    GNOME_STOCK_BUTTON_OK,
			    GNOME_STOCK_BUTTON_CANCEL,
			    NULL);

  g_free(name);
  g_free(title);

  editAccData->dialog  = dialog;
  editAccData->account = acc;
  
  /* default to ok */
  gnome_dialog_set_default(GNOME_DIALOG(dialog), 0);

  /* destroy, don't hide */
  gnome_dialog_close_hides(GNOME_DIALOG(dialog), FALSE);

  vbox = GNOME_DIALOG(editAccData->dialog)->vbox;
  gtk_widget_show (vbox);

  /* Account field edit box */
  widget = gnc_ui_account_field_box_create_from_account
    (acc, &editAccData->edit_info);

  gnome_dialog_editable_enters(GNOME_DIALOG(dialog),
			       editAccData->edit_info.name_entry);
  gnome_dialog_editable_enters(GNOME_DIALOG(dialog),
			       editAccData->edit_info.description_entry);
  gnome_dialog_editable_enters(GNOME_DIALOG(dialog),
			       editAccData->edit_info.currency_entry);
  gnome_dialog_editable_enters(GNOME_DIALOG(dialog),
			       editAccData->edit_info.security_entry);
  gnome_dialog_editable_enters(GNOME_DIALOG(dialog),
			       editAccData->edit_info.code_entry);

  if (!unsafe_ops)
  {
    gtk_widget_set_sensitive
      (GTK_WIDGET(editAccData->edit_info.currency_entry), FALSE);
    gtk_widget_set_sensitive
      (GTK_WIDGET(editAccData->edit_info.security_entry), FALSE);
  }

  gtk_box_pack_start(GTK_BOX(vbox), widget, FALSE, FALSE, 0);

  /* source menu */
  widget = gnc_ui_account_source_box_create_from_account
    (acc, &editAccData->edit_info);
  gtk_box_pack_start(GTK_BOX(vbox), widget, FALSE, FALSE, 0);
  {
    int type = xaccAccountGetType(acc);
    if ((type != STOCK) && (type != MUTUAL) && (type != CURRENCY))
      gtk_widget_hide(widget);
  }

  { /* Notes entry */
    gchar * notes;

    widget = gnc_ui_notes_frame_create(&editAccData->edit_info.notes_entry);
    gtk_box_pack_start(GTK_BOX(vbox), widget, TRUE, TRUE, 0);
    notes = xaccAccountGetNotes(acc);
    gtk_text_insert(GTK_TEXT(editAccData->edit_info.notes_entry),
		    NULL, NULL, NULL, notes, -1);
  }

  gnome_dialog_button_connect
    (GNOME_DIALOG(dialog), 0,
     GTK_SIGNAL_FUNC(gnc_ui_EditAccWindow_ok_cb), editAccData);

  gnome_dialog_button_connect
    (GNOME_DIALOG(dialog), 1,
     GTK_SIGNAL_FUNC(gnc_ui_EditAccWindow_cancel_cb), editAccData);

  gtk_signal_connect(GTK_OBJECT(dialog), "close",
		     GTK_SIGNAL_FUNC (gnc_ui_EditAccWindow_close_cb),
		     editAccData);

  gtk_widget_show(dialog);

  return editAccData;
}


/********************************************************************\
 * gnc_ui_edit_acc_window_raise                                     *
 *   shows and raises an account editing window                     * 
 *                                                                  * 
 * Args:   editAccData - the edit window structure                  * 
\********************************************************************/
void
gnc_ui_edit_account_window_raise(EditAccWindow * editAccData)
{
  if (editAccData == NULL)
    return;

  if (editAccData->dialog == NULL)
    return;

  gtk_widget_show(editAccData->dialog);

  if (editAccData->dialog->window == NULL)
    return;

  gdk_window_raise(editAccData->dialog->window);
}


/********************************************************************\
 * Don't delete any structures -- the close callback will do this   *
\********************************************************************/

void
xaccDestroyEditAccWindow (Account * acc) 
{
  EditAccWindow *editAccData;

  FIND_IN_LIST (EditAccWindow,editAccList,acc,account,editAccData); 

  if (editAccData == NULL)
    return;
 
  gnome_dialog_close(GNOME_DIALOG(editAccData->dialog));
}
