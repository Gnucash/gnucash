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

#include "AccWindow.h"
#include "dialog-editnotes.h"
#include "AccInfo.h"
#include "Account.h"
#include "top-level.h"
#include "MainWindow.h"
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
  if (!found_widget)
    g_warning ("Widget not found: %s", widget_name);
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
  gchar                 *name, *tmp;
  char                  *oldval;
  
  entryAccountName = gnc_ui_get_widget(GTK_WIDGET(dialog), "entryAccountName");
  entryDescription = gnc_ui_get_widget(GTK_WIDGET(dialog), "entryDescription");
  entryCurrency    = gnc_ui_get_widget(GTK_WIDGET(dialog), "entryCurrency"   );
  entrySecurity    = gnc_ui_get_widget(GTK_WIDGET(dialog), "entrySecurity"   );
  entryAccountCode = gnc_ui_get_widget(GTK_WIDGET(dialog), "entryAccountCode");
 
  acc = editAccData->account;

  xaccAccountBeginEdit (acc, 0);

  /* The account has to have a name! */
  name = gtk_entry_get_text(GTK_ENTRY(entryAccountName));
  if( strcmp( name, "" ) != 0 )
    {
    xaccAccountSetName (acc, name);
    }

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
  GtkWidget *hboxAccountName;
  GtkWidget *labelAccountName;
  GtkWidget *entryAccountName;
  GtkWidget *hboxlabelDescription;
  GtkWidget *labelDescription;
  GtkWidget *entryDescription;
  GtkWidget *hboxCurrency;
  GtkWidget *labelCurrency;
  GtkWidget *entryCurrency;
  GtkWidget *hboxAccountCode;
  GtkWidget *labelAccountCode;
  GtkWidget *entryAccountCode;
  GtkWidget *entrySecurity;
  GtkWidget *labelSecurity;
  GtkWidget *hboxSecurity;
  gchar     *title = EDIT_ACCT_STR;
  gint      acctype;

  FETCH_FROM_LIST (EditAccWindow, editAccList, acc, account, editAccData);

  editAccData = (EditAccWindow *)g_malloc(sizeof(EditAccWindow));  

  editAccData->dialog = GNOME_DIALOG( gnome_dialog_new ( title, 
                                      NOTES_STR,
                                      OK_STR,
                                      CANCEL_STR,
                                      NULL));

  editAccData->account = acc;

  vbox1 = GNOME_DIALOG(editAccData->dialog)->vbox;
  gtk_object_set_data (GTK_OBJECT (editAccData->dialog), "vbox1", vbox1);
  gtk_widget_show (vbox1);
  gtk_container_border_width (GTK_CONTAINER (vbox1), 10);

  hboxAccountName = gtk_hbox_new (TRUE, 0);
  gtk_object_set_data (GTK_OBJECT (editAccData->dialog), "hboxAccountName", hboxAccountName);
  gtk_widget_show (hboxAccountName);
  gtk_box_pack_start (GTK_BOX (vbox1), hboxAccountName, FALSE, FALSE, 0);

  labelAccountName = gtk_label_new ("Account Name:");
  gtk_object_set_data (GTK_OBJECT (editAccData->dialog), "labelAccountName", labelAccountName);
  gtk_widget_show (labelAccountName);
  gtk_box_pack_start (GTK_BOX (hboxAccountName), labelAccountName, TRUE, TRUE, 0);
  gtk_label_set_justify (GTK_LABEL (labelAccountName), GTK_JUSTIFY_RIGHT);
  gtk_misc_set_alignment (GTK_MISC (labelAccountName), 0.9, 0.5);

  entryAccountName = gtk_entry_new ();
  gtk_entry_set_text(GTK_ENTRY(entryAccountName), xaccAccountGetName(acc));
  gtk_object_set_data (GTK_OBJECT (editAccData->dialog), "entryAccountName", entryAccountName);
  gtk_widget_show (entryAccountName);
  gtk_box_pack_start (GTK_BOX (hboxAccountName), entryAccountName, TRUE, FALSE, 0);

  hboxlabelDescription = gtk_hbox_new (TRUE, 0);
  gtk_object_set_data (GTK_OBJECT (editAccData->dialog), "hboxlabelDescription", hboxlabelDescription);
  gtk_widget_show (hboxlabelDescription);
  gtk_box_pack_start (GTK_BOX (vbox1), hboxlabelDescription, FALSE, FALSE, 0);

  labelDescription = gtk_label_new ("Description:");
  gtk_object_set_data (GTK_OBJECT (editAccData->dialog), "labelDescription", labelDescription);
  gtk_widget_show (labelDescription);
  gtk_box_pack_start (GTK_BOX (hboxlabelDescription), labelDescription, TRUE, TRUE, 0);
  gtk_label_set_justify (GTK_LABEL (labelDescription), GTK_JUSTIFY_RIGHT);
  gtk_misc_set_alignment (GTK_MISC (labelDescription), 0.91, 0.5);

  entryDescription = gtk_entry_new ();
  gtk_entry_set_text(GTK_ENTRY(entryDescription), xaccAccountGetDescription(acc));
  gtk_object_set_data (GTK_OBJECT (editAccData->dialog), "entryDescription", entryDescription);
  gtk_widget_show (entryDescription);
  gtk_box_pack_start (GTK_BOX (hboxlabelDescription), entryDescription, TRUE, FALSE, 0);

  hboxCurrency = gtk_hbox_new (TRUE, 0);
  gtk_object_set_data (GTK_OBJECT (editAccData->dialog), "hboxCurrency", hboxCurrency);
  gtk_widget_show (hboxCurrency);
  gtk_box_pack_start (GTK_BOX (vbox1), hboxCurrency, FALSE, FALSE, 0);

  labelCurrency = gtk_label_new ("Currency:");
  gtk_object_set_data (GTK_OBJECT (editAccData->dialog), "labelCurrency", labelCurrency);
  gtk_widget_show (labelCurrency);
  gtk_box_pack_start (GTK_BOX (hboxCurrency), labelCurrency, TRUE, TRUE, 0);
  gtk_label_set_justify (GTK_LABEL (labelCurrency), GTK_JUSTIFY_RIGHT);
  gtk_misc_set_alignment (GTK_MISC (labelCurrency), 0.91, 0.5);

  entryCurrency = gtk_entry_new ();
  gtk_entry_set_text(GTK_ENTRY(entryCurrency), xaccAccountGetCurrency(acc));    
  gtk_object_set_data (GTK_OBJECT (editAccData->dialog), "entryCurrency", entryCurrency);
  gtk_widget_show (entryCurrency);
  gtk_box_pack_start (GTK_BOX (hboxCurrency), entryCurrency, TRUE, FALSE, 0);

  /* Security field should only be shown if this account is one of the following:
   *    STOCK
   *    MUTUAL
   *    CURRENCY
   */
  acctype = xaccAccountGetType (acc);
  if ((STOCK == acctype) || (MUTUAL == acctype) || (CURRENCY== acctype)) 
  {
    hboxSecurity = gtk_hbox_new (TRUE, 0);
    gtk_object_set_data (GTK_OBJECT (editAccData->dialog), "hboxSecurity", hboxSecurity);
    gtk_widget_show (hboxSecurity);
    gtk_box_pack_start (GTK_BOX (vbox1), hboxSecurity, FALSE, FALSE, 0);

    labelSecurity = gtk_label_new ("Security:");
    gtk_object_set_data (GTK_OBJECT (editAccData->dialog), "labelSecurity", labelSecurity);
    gtk_widget_show (labelSecurity);
    gtk_box_pack_start (GTK_BOX (hboxSecurity), labelSecurity, TRUE, TRUE, 0);
    gtk_label_set_justify (GTK_LABEL (labelSecurity), GTK_JUSTIFY_RIGHT);
    gtk_misc_set_alignment (GTK_MISC (labelSecurity), 0.91, 0.5);

    entrySecurity = gtk_entry_new ();
    gtk_entry_set_text(GTK_ENTRY(entrySecurity), xaccAccountGetSecurity(acc));    
    gtk_object_set_data (GTK_OBJECT (editAccData->dialog), "entrySecurity", entrySecurity);
    gtk_widget_show (entrySecurity);
    gtk_box_pack_start (GTK_BOX (hboxSecurity), entrySecurity, TRUE, FALSE, 0);
  }

  hboxAccountCode = gtk_hbox_new (TRUE, 0);
  gtk_object_set_data (GTK_OBJECT (editAccData->dialog), "hboxAccountCode", hboxAccountCode);
  gtk_widget_show (hboxAccountCode);
  gtk_box_pack_start (GTK_BOX (vbox1), hboxAccountCode, FALSE, FALSE, 0);

  labelAccountCode = gtk_label_new ("Account Code:");
  gtk_object_set_data (GTK_OBJECT (editAccData->dialog), "labelAccountCode", labelAccountCode);
  gtk_widget_show (labelAccountCode);
  gtk_box_pack_start (GTK_BOX (hboxAccountCode), labelAccountCode, TRUE, TRUE, 0);
  gtk_label_set_justify (GTK_LABEL (labelAccountCode), GTK_JUSTIFY_RIGHT);
  gtk_misc_set_alignment (GTK_MISC (labelAccountCode), 0.88, 0.5);

  entryAccountCode = gtk_entry_new ();
  gtk_entry_set_text(GTK_ENTRY(entryAccountCode), xaccAccountGetCode(acc));  
  gtk_object_set_data (GTK_OBJECT (editAccData->dialog), "entryAccountCode", entryAccountCode);
  gtk_widget_show (entryAccountCode);
  gtk_box_pack_start (GTK_BOX (hboxAccountCode), entryAccountCode, TRUE, FALSE, 0);

  gnome_dialog_button_connect (GNOME_DIALOG (editAccData->dialog), 0,
                               GTK_SIGNAL_FUNC (gnc_ui_editnotes_callback), 
                               editAccData->account);

  gnome_dialog_button_connect (GNOME_DIALOG (editAccData->dialog), 1,
                               GTK_SIGNAL_FUNC (gnc_ui_EditAccWindow_finished_callback), 
                               editAccData);
                               
  gnome_dialog_button_connect (GNOME_DIALOG (editAccData->dialog), 1,
                               GTK_SIGNAL_FUNC (gnc_ui_EditAccWindow_close_callback), 
                               editAccData);                               
                               
  gnome_dialog_button_connect (GNOME_DIALOG (editAccData->dialog), 2,
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

