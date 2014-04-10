/********************************************************************\
 * editnotes.c -- window for editing account notes                  *
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
#include "AccInfo.h"
#include "Account.h"
#include "top-level.h"
#include "MainWindow.h"
#include "messages.h"
#include "util.h"

struct _editnoteswindow
{
  GnomeDialog *dialog;
  
  Account     *account; 
};

static EditNotesWindow ** editNotesList = NULL;

void
gnc_ui_editnotes_callback(GtkWidget *widget, gpointer data)
{
  Account *acc = (Account *)data;

  editNotesWindow(acc);  
     
}

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
gnc_ui_editNotesWindow_commit_callback(GtkWidget *dialog, gpointer data)
{
  EditNotesWindow *enw = (EditNotesWindow *) data;
  Account         * acc = enw->account;
  char            *newnotes;
  char            *notes;
  GtkWidget       *text;

  text = gnc_ui_get_widget(GTK_WIDGET(enw->dialog), "text1");  

  newnotes = gtk_editable_get_chars(GTK_EDITABLE(text),0,-1);

  /* don't bother updating the database if the value of the field
   * literally did not change.  Note that this callback is called 
   * for commit and cancel buttons ... 
   */
  notes = xaccAccountGetNotes (acc);
  if (safe_strcmp (notes, newnotes)) {
    xaccAccountBeginEdit (acc, 1);
    xaccAccountSetNotes (acc, newnotes);
    xaccAccountCommitEdit (acc);
  }

  REMOVE_FROM_LIST (EditNotesWindow,editNotesList,acc,account) 

  gnome_dialog_close(GNOME_DIALOG(gtk_widget_get_toplevel(dialog)));
  free (enw);
 
}

static void
gnc_ui_editNotesWindow_cancel_callback(GtkWidget *dialog, gpointer data)
{
  Account *acc = (Account *)data;
  
  REMOVE_FROM_LIST (EditNotesWindow,editNotesList,acc,account) 
  
  gnome_dialog_close(GNOME_DIALOG(gtk_widget_get_toplevel(dialog)));
}

EditNotesWindow *
editNotesWindow (Account *acc) 
{
  EditNotesWindow *enw;
  GtkWidget *text1;
  gchar     *title = NOTES_STR;
  gchar     *notes;

  FETCH_FROM_LIST (EditNotesWindow, editNotesList, acc, account, enw);

  enw->dialog = GNOME_DIALOG( gnome_dialog_new ( 
                                      title, 
                                      COMMIT_STR,
                                      CANCEL_STR,
                                      NULL));
   
  text1 = gtk_text_new (NULL, NULL);
  gtk_object_set_data (GTK_OBJECT (enw->dialog), "text1", text1);
  gtk_widget_show (text1);
  gtk_container_add (GTK_CONTAINER (GNOME_DIALOG(enw->dialog)->vbox), text1);
  gtk_text_set_editable (GTK_TEXT (text1), TRUE);

  notes = xaccAccountGetNotes (acc);

  gtk_text_insert( GTK_TEXT(text1),
                   NULL,
                   NULL,
                   NULL,
                   notes,
                   -1 );

  gnome_dialog_button_connect (GNOME_DIALOG (enw->dialog), 0,
                               GTK_SIGNAL_FUNC (gnc_ui_editNotesWindow_commit_callback), 
                               enw);

  gnome_dialog_button_connect (GNOME_DIALOG (enw->dialog), 1,
                               GTK_SIGNAL_FUNC (gnc_ui_editNotesWindow_cancel_callback), 
                               acc);

  gtk_widget_show(GTK_WIDGET(enw->dialog));
  
  return enw;
}

/********************************************************************\
 * don't delete any structures; the close callack will do this       *
\********************************************************************/

void 
xaccDestroyEditNotesWindow (Account *acc) {
  
  EditNotesWindow *edwin;

  FIND_IN_LIST (EditNotesWindow,editNotesList,acc,account,edwin) 
  if (!edwin) return;

  gnome_dialog_close(GNOME_DIALOG(edwin->dialog));

}


