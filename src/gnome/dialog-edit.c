/********************************************************************\
 * dialog-edit.c -- window for editing account information          *
 *                (X-Accountant)                                    *
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


#include <gnome.h>
#include <stdio.h>

#include "config.h"

#include "top-level.h"
#include "AccWindow.h"
#include "MainWindow.h"
#include "AccInfo.h"
#include "Account.h"
#include "messages.h"
#include "util.h"

int unsafe_edits = 1;

struct _editaccwindow
{
  GnomeDialog *dialog;
  
  Account     *account;
};

static EditAccWindow   ** editAccList   = NULL;

static GtkWidget*
gnc_ui_get_widget (GtkWidget *widget, gchar *widget_name)
{
  GtkWidget *found_widget;
  
  if (widget->parent)
    widget = gtk_widget_get_toplevel (widget);
  found_widget = (GtkWidget*) gtk_object_get_data (GTK_OBJECT (widget),
                                                   widget_name);
  
  /* entrySecurity widget will not always be found! */
#if 0
  if (!found_widget)
    g_warning ("Widget not found: %s", widget_name);
#endif
  
  return found_widget;
}


static void
gnc_ui_EditAccWindow_close_callback(GtkWidget *dialog, gpointer data)
{
  EditAccWindow *editAccData = (EditAccWindow *)data;
  Account *acc = editAccData->account;
  
  REMOVE_FROM_LIST (EditAccWindow,editAccList,acc,account); 
  free(editAccData);
  gnome_dialog_close(GNOME_DIALOG(gtk_widget_get_toplevel(dialog))); 
}

static void
gnc_ui_EditAccWindow_finished_callback(GtkWidget *dialog, gpointer data)
{
  Account      		*acc;
  EditAccWindow         *editAccData = data; 
  GtkWidget             *entryAccountName;
  GtkWidget             *entryDescription;
  GtkWidget             *entryCurrency;
  GtkWidget             *entrySecurity;
  GtkWidget	        *entryAccountCode;
  GtkWidget             *notesWidget;
  gchar                 *name, *tmp;
  char                  *oldval;
  
  entryAccountName = gnc_ui_get_widget(GTK_WIDGET(dialog), "entryAccountName");
  entryDescription = gnc_ui_get_widget(GTK_WIDGET(dialog), "entryDescription");
  entryCurrency    = gnc_ui_get_widget(GTK_WIDGET(dialog), "entryCurrency"   );
  entrySecurity    = gnc_ui_get_widget(GTK_WIDGET(dialog), "entrySecurity"   );
  entryAccountCode = gnc_ui_get_widget(GTK_WIDGET(dialog), "entryAccountCode");
  notesWidget      = gnc_ui_get_widget(GTK_WIDGET(dialog), "notesText");
  
  acc = editAccData->account;
  
  /* The account has to have a name! */
  name = gtk_entry_get_text(GTK_ENTRY(entryAccountName));
  if(0 != strcmp( name, "" ))
  {
    xaccAccountSetName (acc, name);
  }
  else
  {
    GtkWidget *msgbox;
    msgbox = gnome_message_box_new ( "You must enter a name for the account",
                                     GNOME_MESSAGE_BOX_ERROR, "Ok", NULL );
    gtk_window_set_modal(GTK_WINDOW(msgbox) ,TRUE );
    gtk_widget_show ( msgbox );
    return;
  }
  
  xaccAccountBeginEdit(acc, 0);
  
  tmp = gtk_entry_get_text(GTK_ENTRY(entryDescription));
  xaccAccountSetDescription (acc, tmp);
  
  tmp = gtk_entry_get_text(GTK_ENTRY(entryAccountCode));
  xaccAccountSetCode (acc, tmp);
  
  oldval = xaccAccountGetCurrency (acc);
  if (!oldval) oldval = "";
  if ((0x0 == oldval[0]) || unsafe_edits) {
    char * oldval = xaccAccountGetCurrency (acc);
    tmp = gtk_entry_get_text(GTK_ENTRY(entryCurrency));
    if (0x0 == oldval[0]) {
      xaccAccountSetCurrency (acc, tmp);
    } else 
      if (strcmp (oldval, tmp)) {
        char buff[1000];
        sprintf (buff, EDIT_CURRENCY_MSG, oldval, tmp);
        if (verifyBox (buff)) {
          xaccAccountSetCurrency (acc, tmp);
        }
      }
  }
  
  /* if this field wasn't displayed, then don't try to deal with it */
  if (entrySecurity) {
    oldval = xaccAccountGetSecurity (acc);
    if (!oldval) oldval = "";
    if ((0x0 == oldval[0]) || unsafe_edits) {
      char * oldval = xaccAccountGetSecurity (acc);
      tmp = gtk_entry_get_text(GTK_ENTRY(entrySecurity));
      if (0x0 == oldval[0]) {
        xaccAccountSetSecurity (acc, tmp);
      } else 
        if (strcmp (oldval, tmp)) {
          char buff[1000];
          sprintf (buff, EDIT_SECURITY_MSG, oldval, tmp);
          if (verifyBox (buff)) {
            xaccAccountSetSecurity (acc, tmp);
          }
        }
      
    }
  }
  
  {
    char *notes = gtk_editable_get_chars(GTK_EDITABLE(notesWidget), 0, -1);
    xaccAccountSetNotes(acc, notes);
  }
  
  //  gnome_dialog_close(GNOME_DIALOG(gtk_widget_get_toplevel(dialog)));
  
  xaccAccountCommitEdit (acc);
  
  refreshMainWindow();
  
}


/********************************************************************\
 * editAccWindow                                                    *
 *   opens up a window to edit an account                           * 
 *                                                                  * 
 * Args:   parent   - the parent of the window to be created        * 
 *         account  - the account to edit                           * 
 * Return: none                                                     *
\********************************************************************/
EditAccWindow *
editAccWindow( Account *acc ) {
  EditAccWindow *editAccData;
  GtkWidget *vbox1;
  gchar     *title = EDIT_ACCT_STR;
  
  FETCH_FROM_LIST (EditAccWindow, editAccList, acc, account, editAccData);
  
  editAccData = (EditAccWindow *)g_malloc(sizeof(EditAccWindow));  
  
  editAccData->dialog = GNOME_DIALOG( gnome_dialog_new ( title, 
                                                         OK_STR,
                                                         CANCEL_STR,
                                                         NULL));
  
  editAccData->account = acc;
  
  vbox1 = GNOME_DIALOG(editAccData->dialog)->vbox;
  gtk_widget_show (vbox1);
  gtk_container_border_width (GTK_CONTAINER (vbox1), 5);
  
  
  {
    /* account name, description, curency, (security), code */
    GtkWidget *frame, *table, *item;
    
    frame = gtk_frame_new("Account Info");
    gtk_container_border_width(GTK_CONTAINER(frame), 5);
    gtk_box_pack_start(GTK_BOX(vbox1), frame, FALSE, TRUE, 0);
    
    table = gtk_table_new(6, 2, TRUE);
    gtk_container_border_width(GTK_CONTAINER(table), 5);
    gtk_table_set_row_spacings(GTK_TABLE(table), 3);
    gtk_table_set_col_spacings(GTK_TABLE(table), 5);
    gtk_widget_show(table);
    gtk_container_add(GTK_CONTAINER(frame), table);
    
    /* fill the table */
    
    item = gtk_label_new("Name:");
    gtk_widget_show(item);
    gtk_misc_set_alignment (GTK_MISC (item), 0.95, 0.5);
    gtk_table_attach_defaults(GTK_TABLE(table), item,
                              0, 1, 0, 1);
    
    item = gtk_entry_new();
    gtk_entry_set_text(GTK_ENTRY(item), xaccAccountGetName(acc));
    gtk_widget_show(item);
    gtk_object_set_data(GTK_OBJECT(editAccData->dialog), "entryAccountName", item);
    gtk_table_attach_defaults(GTK_TABLE(table), item, 1, 2, 0, 1);
    
    item = gtk_label_new("Type");
    gtk_widget_show(item);
    gtk_misc_set_alignment (GTK_MISC (item), 0.95, 0.5);
    gtk_table_attach_defaults(GTK_TABLE(table), item, 0, 1, 1, 2);
    
    item = gtk_entry_new();
    gtk_entry_set_text(GTK_ENTRY(item), xaccAccountGetTypeStr(xaccAccountGetType(acc)));
    gtk_widget_set_sensitive(item , FALSE );
    gtk_widget_show(item);
    gtk_table_attach_defaults(GTK_TABLE(table), item, 1, 2, 1, 2);
    
    item = gtk_label_new("Code:");
    gtk_widget_show(item);
    gtk_misc_set_alignment (GTK_MISC (item), 0.95, 0.5);
    gtk_table_attach_defaults(GTK_TABLE(table), item, 0, 1, 2, 3);
    
    item = gtk_entry_new();
    gtk_entry_set_text(GTK_ENTRY(item), xaccAccountGetCode(acc));
    gtk_widget_show(item);
    gtk_table_attach_defaults(GTK_TABLE(table), item, 1, 2, 2, 3);
    gtk_object_set_data (GTK_OBJECT (editAccData->dialog), "entryAccountCode", item);
    
    item = gtk_label_new("Currency:");
    gtk_widget_show(item);
    gtk_misc_set_alignment (GTK_MISC (item), 0.95, 0.5);
    gtk_table_attach_defaults(GTK_TABLE(table), item, 0, 1, 3, 4);
    
    item = gtk_entry_new();
    gtk_entry_set_text(GTK_ENTRY(item), xaccAccountGetCurrency(acc));
    gtk_widget_show(item);
    gtk_table_attach_defaults(GTK_TABLE(table), item, 1, 2, 3, 4);
    gtk_object_set_data (GTK_OBJECT (editAccData->dialog), "entryCurrency", item);
    
    item = gtk_label_new("Security:");
    gtk_widget_show(item);
    gtk_misc_set_alignment (GTK_MISC (item), 0.95, 0.5);
    gtk_table_attach_defaults(GTK_TABLE(table), item, 0, 1, 4, 5);
    
    item = gtk_entry_new();
    
    {
      int accType = xaccAccountGetType(acc);
      if ((STOCK == accType) || (MUTUAL == accType) || (CURRENCY== accType)) 
      {
        gtk_entry_set_text(GTK_ENTRY(item), xaccAccountGetSecurity(acc));
        gtk_widget_set_sensitive(item, TRUE);
        gtk_object_set_data(GTK_OBJECT(editAccData->dialog), 
                            "entrySecurity", item);
      }
      else
      {
        gtk_widget_set_sensitive(item, FALSE);
      }
    }
    
    gtk_widget_show(item);
    gtk_table_attach_defaults(GTK_TABLE(table), item, 1, 2, 4, 5);
    
    
    item = gtk_label_new("Description:");
    gtk_widget_show(item);
    gtk_misc_set_alignment (GTK_MISC (item), 0.95, 0.5);
    gtk_table_attach_defaults(GTK_TABLE(table), item, 0, 1, 5, 6);
    
    item = gtk_entry_new();
    gtk_entry_set_text(GTK_ENTRY(item), xaccAccountGetDescription(acc));
    gtk_widget_show(item);
    gtk_object_set_data(GTK_OBJECT(editAccData->dialog), "entryDescription", item);
    gtk_table_attach_defaults(GTK_TABLE(table), item, 1, 2, 5, 6);    
    
    
    gtk_widget_show(frame);
  }
  
  {
    /* the notes box */
    GtkWidget *frame, *text, *box;
    char *notes;
    
    frame = gtk_frame_new("Notes");
    gtk_container_border_width(GTK_CONTAINER(frame), 5);
    gtk_widget_show(frame);
    gtk_box_pack_start(GTK_BOX(vbox1), frame, FALSE, TRUE, 0);
    
    box = gtk_hbox_new(TRUE, 5);
    gtk_widget_show(box);
    gtk_container_add(GTK_CONTAINER(frame), box);
    gtk_container_border_width (GTK_CONTAINER (box), 5);
    
    text = gtk_text_new(NULL, NULL);
    gtk_object_set_data (GTK_OBJECT(editAccData->dialog), "notesText", text);
    gtk_widget_set_usize(text, 300, 50);
    
    notes = xaccAccountGetNotes(acc);
    gtk_text_insert(GTK_TEXT(text), NULL, NULL, NULL, notes, -1 );
    gtk_text_set_editable(GTK_TEXT(text), TRUE);
    
    gtk_widget_show(text);    
    
    gtk_box_pack_start(GTK_BOX(box), text, TRUE, TRUE, 0);
  }
  
  gnome_dialog_button_connect (GNOME_DIALOG (editAccData->dialog), 0,
                               GTK_SIGNAL_FUNC (gnc_ui_EditAccWindow_finished_callback), 
                               editAccData);
  
  gnome_dialog_button_connect (GNOME_DIALOG (editAccData->dialog), 0,
                               GTK_SIGNAL_FUNC (gnc_ui_EditAccWindow_close_callback), 
                               editAccData);                               
  
  gnome_dialog_button_connect (GNOME_DIALOG (editAccData->dialog), 1,
                               GTK_SIGNAL_FUNC (gnc_ui_EditAccWindow_close_callback), 
                               editAccData);
  
  gtk_widget_show(GTK_WIDGET(editAccData->dialog));
  
  return editAccData;
}

/********************************************************************\
 * Don't delete any structures -- the close callback wil do this    *
\********************************************************************/

void
xaccDestroyEditAccWindow (Account * acc) 
{
  EditAccWindow *editAccData;

  FIND_IN_LIST (EditAccWindow,editAccList,acc,account,editAccData); 
  if (!editAccData) return;
 
  gnome_dialog_close(GNOME_DIALOG(editAccData->dialog));
 
}

