/********************************************************************\
 * AccWindow.c -- window for creating new accounts for xacc         *
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
#include <nana.h>
#include <stdio.h>

#include "config.h"

#include "AccWindow.h"
#include "top-level.h"
#include "MainWindow.h"
#include "messages.h"

/* Please look at ../motif/AccWindow.c for info on what should be
   going on in these functions */

struct _accwindow
{
  GnomeDialog 	*dialog;
  GtkWidget 	*main_vbox;
  GtkWidget 	*box2;
  GtkWidget	*box3;
  GtkWidget	*box4;
  GtkWidget 	*frame;
  
  GSList 	*group;

  GtkWidget 	*label;
  GtkWidget 	*textbox_name;
  GtkWidget	*textbox_description;

  GtkWidget	*separator;

  Account	*new_account;
  AccountGroup	*parent_account;
  gint		 type;
};

/** Callback Functions **********************************************/

static void
gnc_ui_accWindow_toggle_callback (GtkWidget *widget, gpointer data)
{
  int *val;

  if (GTK_TOGGLE_BUTTON (widget)->active)
  {
    val = data;
    *val = (long) gtk_object_get_user_data (GTK_OBJECT (widget));
  }
}

static void
gnc_ui_accWindow_cancelled_callback( GtkWidget *ignore, gpointer data )
{
  AccWindow  *info = data;
  
  gnome_dialog_close( GNOME_DIALOG(info->dialog));
}

static void 
gnc_ui_accWindow_create_callback(GtkWidget * dialog, gpointer data)
{
  Transaction 		*trans;
  Account      		*account;
  AccWindow             *info = data; 
  
  /* Check to make sure something was entered */
  
  if( strcmp( gtk_entry_get_text(GTK_ENTRY(info->textbox_name)), "" ) == 0 ) 
  {
    GtkWidget *msgbox;
    msgbox = gnome_message_box_new ( "You must enter a filename",
                                     GNOME_MESSAGE_BOX_ERROR, "Ok", NULL );
    gtk_widget_show ( msgbox );
    return;
  }
  
  account = xaccMallocAccount();
            
  xaccAccountBeginEdit (account, 0);
 
  xaccAccountSetName (account, gtk_entry_get_text(GTK_ENTRY(info->textbox_name)));
  xaccAccountSetDescription (account, gtk_entry_get_text(GTK_ENTRY(info->textbox_description)));
   
  xaccAccountSetType ( account, info->type );

  /* Add an opening balance transaction (as the first transaction) */
  trans = xaccMallocTransaction();
 
  xaccTransBeginEdit(trans, 1);
  xaccTransSetDateToday (trans);
  xaccTransSetDescription (trans, OPEN_BALN_STR);
  xaccTransCommitEdit(trans);
            
  /* add the new transaction to the account */
  xaccAccountInsertSplit (account, xaccTransGetSplit (trans, 0) );
  
  /* once the account is set up, add it to account group 
   * If the user indicated a parent acccount, make it a 
   * sub account of that */
       
  /* The g_print statements should be removed after we are sure 
   * everything is a ok with this callback 
   */       
       
  if ((Account *)info->parent_account) {
    xaccInsertSubAccount ((Account *)info->parent_account, account);
  } else {
    xaccGroupInsertAccount(xaccSessionGetGroup(current_session), account );
  }
  xaccAccountCommitEdit (account);

  refreshMainWindow();

  gnome_dialog_close ( GNOME_DIALOG(info->dialog) );

}

/********************************************************************\
 * accWindow                                                        *
 *   opens up a window to create a new account... the account is    * 
 *   actually created in the "create" callback                      * 
 *                                                                  * 
 * Args:   parent   - the parent of the window to be created        * 
 * Return: none                                                     *
\********************************************************************/
AccWindow *
accWindow (AccountGroup *grp) {

  AccWindow *accData;
  //GtkWidget parent = gnc_get_ui_data();
  GtkWidget *button;  

  gchar *title = SETUP_ACCT_STR;

  accData = (AccWindow *)g_malloc(sizeof(AccWindow));

  accData->parent_account = grp;
  
  accData->dialog = 
         GNOME_DIALOG( gnome_dialog_new ( title, 
                       GNOME_STOCK_BUTTON_OK,
                       GNOME_STOCK_BUTTON_CANCEL,
                       NULL));

  gtk_window_set_title ( GTK_WINDOW ( accData->dialog ), title );
    
  gtk_window_position ( GTK_WINDOW ( accData->dialog ), 
                        GTK_WIN_POS_CENTER );

  accData->main_vbox = gtk_vbox_new ( FALSE, 2 );
  gtk_container_add ( GTK_CONTAINER ( (accData->dialog)->vbox ), accData->main_vbox );  
 
  accData->frame = gtk_frame_new ( "Account Type" );
  gtk_container_border_width (GTK_CONTAINER (accData->frame), 10);
  gtk_box_pack_start ( GTK_BOX( accData->main_vbox ), accData->frame, FALSE, FALSE, 0 );
  gtk_widget_show ( accData->frame );

  accData->box2 = gtk_hbox_new ( FALSE, 2 );
  gtk_container_add ( GTK_CONTAINER( accData->frame ), accData->box2 );
  gtk_widget_show ( accData->box2 );
  
  accData->box3 = gtk_vbox_new ( FALSE, 2 );
  gtk_box_pack_start ( GTK_BOX( accData->box2 ), accData->box3, FALSE, FALSE, 0 );
  gtk_widget_show ( accData->box3 );
  
  accData->box4 = gtk_vbox_new ( FALSE, 2 );
  gtk_box_pack_start ( GTK_BOX( accData->box2 ), accData->box4, FALSE, FALSE, 0 );
  gtk_widget_show ( accData->box4 );     
  
  /* Create radio button group for Account Type **********************/
    
  button = gtk_radio_button_new_with_label (NULL, "Bank");
  gtk_box_pack_start (GTK_BOX (accData->box3), button, TRUE, TRUE, 0 );
  gtk_object_set_user_data (GTK_OBJECT (button), (gpointer) BANK);
  gtk_signal_connect (GTK_OBJECT (button), "toggled",
		      (GtkSignalFunc) gnc_ui_accWindow_toggle_callback,
		      &accData->type);
  if (accData->type == BANK)
    gtk_toggle_button_set_state (GTK_TOGGLE_BUTTON (button), TRUE);
  gtk_widget_show (button);
      
  accData->group = gtk_radio_button_group (GTK_RADIO_BUTTON (button));
  button = gtk_radio_button_new_with_label(accData->group, "Cash");
  gtk_box_pack_start (GTK_BOX (accData->box3), button, TRUE, TRUE, 0 );
  gtk_object_set_user_data (GTK_OBJECT (button), (gpointer) CASH);
  gtk_signal_connect (GTK_OBJECT (button), "toggled",
		      (GtkSignalFunc) gnc_ui_accWindow_toggle_callback,
		      &accData->type);
  if (accData->type == CASH)
    gtk_toggle_button_set_state (GTK_TOGGLE_BUTTON (button), TRUE);
  gtk_widget_show (button);
  
  accData->group = gtk_radio_button_group (GTK_RADIO_BUTTON (button));
  button = gtk_radio_button_new_with_label(accData->group, "Asset");
  gtk_box_pack_start (GTK_BOX (accData->box3), button, TRUE, TRUE, 0 );
  gtk_object_set_user_data (GTK_OBJECT (button), (gpointer) ASSET);
  gtk_signal_connect (GTK_OBJECT (button), "toggled",
		      (GtkSignalFunc) gnc_ui_accWindow_toggle_callback,
		      &accData->type);
  if (accData->type == ASSET)
    gtk_toggle_button_set_state (GTK_TOGGLE_BUTTON (button), TRUE);
  gtk_widget_show (button);

  accData->group = gtk_radio_button_group (GTK_RADIO_BUTTON (button));
  button = gtk_radio_button_new_with_label(accData->group, "Credit Card");
  gtk_box_pack_start (GTK_BOX (accData->box3), button, TRUE, TRUE, 0 );
  gtk_object_set_user_data (GTK_OBJECT (button), (gpointer) CREDIT);
  gtk_signal_connect (GTK_OBJECT (button), "toggled",
		      (GtkSignalFunc) gnc_ui_accWindow_toggle_callback,
		      &accData->type);
  if (accData->type == CREDIT)
    gtk_toggle_button_set_state (GTK_TOGGLE_BUTTON (button), TRUE);
  gtk_widget_show (button);
  
  accData->group = gtk_radio_button_group (GTK_RADIO_BUTTON (button));
  button = gtk_radio_button_new_with_label(accData->group, "Liability");
  gtk_box_pack_start (GTK_BOX (accData->box3), button, TRUE, TRUE, 0 );
  gtk_object_set_user_data (GTK_OBJECT (button), (gpointer) LIABILITY);
  gtk_signal_connect (GTK_OBJECT (button), "toggled",
		      (GtkSignalFunc) gnc_ui_accWindow_toggle_callback,
		      &accData->type);
  if (accData->type == LIABILITY)
    gtk_toggle_button_set_state (GTK_TOGGLE_BUTTON (button), TRUE);		      
  gtk_widget_show (button);
  
  accData->group = gtk_radio_button_group (GTK_RADIO_BUTTON (button));
  button = gtk_radio_button_new_with_label(accData->group, "Stock");
  gtk_box_pack_start (GTK_BOX (accData->box4), button, TRUE, TRUE, 0 );
  gtk_object_set_user_data (GTK_OBJECT (button), (gpointer) STOCK);
  gtk_signal_connect (GTK_OBJECT (button), "toggled",
		      (GtkSignalFunc) gnc_ui_accWindow_toggle_callback,
		      &accData->type);
  if (accData->type == STOCK)
    gtk_toggle_button_set_state (GTK_TOGGLE_BUTTON (button), TRUE);		     
  gtk_widget_show (button);

  accData->group = gtk_radio_button_group (GTK_RADIO_BUTTON (button));
  button = gtk_radio_button_new_with_label(accData->group, "Mutual Fund");
  gtk_box_pack_start (GTK_BOX (accData->box4), button, TRUE, TRUE, 0 );
  gtk_object_set_user_data (GTK_OBJECT (button), (gpointer) MUTUAL);
  gtk_signal_connect (GTK_OBJECT (button), "toggled",
		      (GtkSignalFunc) gnc_ui_accWindow_toggle_callback,
		      &accData->type);
  if (accData->type == MUTUAL)
    gtk_toggle_button_set_state (GTK_TOGGLE_BUTTON (button), TRUE);		      
  gtk_widget_show (button);
    
  accData->group = gtk_radio_button_group (GTK_RADIO_BUTTON (button));
  button = gtk_radio_button_new_with_label(accData->group, "Income");
  gtk_box_pack_start (GTK_BOX (accData->box4), button, TRUE, TRUE, 0 );
  gtk_object_set_user_data (GTK_OBJECT (button), (gpointer) INCOME);
  gtk_signal_connect (GTK_OBJECT (button), "toggled",
		      (GtkSignalFunc) gnc_ui_accWindow_toggle_callback,
		      &accData->type);
  if (accData->type == INCOME)
    gtk_toggle_button_set_state (GTK_TOGGLE_BUTTON (button), TRUE);		      
  gtk_widget_show (button);
  
  accData->group = gtk_radio_button_group (GTK_RADIO_BUTTON (button));
  button = gtk_radio_button_new_with_label(accData->group, "Expense");
  gtk_box_pack_start (GTK_BOX (accData->box4), button, TRUE, TRUE, 0 );
  gtk_object_set_user_data (GTK_OBJECT (button), (gpointer) EXPENSE);
  gtk_signal_connect (GTK_OBJECT (button), "toggled",
		      (GtkSignalFunc) gnc_ui_accWindow_toggle_callback,
		      &accData->type);
  if (accData->type == EXPENSE)
    gtk_toggle_button_set_state (GTK_TOGGLE_BUTTON (button), TRUE);		      
  gtk_widget_show (button);
  
  accData->group = gtk_radio_button_group (GTK_RADIO_BUTTON (button));
  button = gtk_radio_button_new_with_label(accData->group, "Equity");
  gtk_box_pack_start (GTK_BOX (accData->box4), button, TRUE, TRUE, 0 );
  gtk_object_set_user_data (GTK_OBJECT (button), (gpointer) EQUITY);
  gtk_signal_connect (GTK_OBJECT (button), "toggled",
		      (GtkSignalFunc) gnc_ui_accWindow_toggle_callback,
		      &accData->type);
  if (accData->type == EQUITY)
    gtk_toggle_button_set_state (GTK_TOGGLE_BUTTON (button), TRUE);		      
  gtk_widget_show (button);

  /************************ END OF RADIOBUTTONS ***********************/
  
  accData->box2 = gtk_hbox_new ( FALSE, 2 );
  gtk_box_pack_start ( GTK_BOX( accData->main_vbox ), accData->box2, FALSE, FALSE, 0 );
  gtk_widget_show ( accData->box2 );
  
  accData->textbox_name = gtk_entry_new_with_max_length ( 25 );
  gtk_box_pack_end ( GTK_BOX(accData->box2), accData->textbox_name, FALSE, FALSE, 10 );
  gtk_widget_show ( accData->textbox_name );
  
  {
    accData->label = gtk_label_new ( "Account Name: " );
    gtk_box_pack_end ( GTK_BOX(accData->box2), accData->label, FALSE, FALSE, 0 );
    gtk_widget_show ( accData->label );
  }

  accData->box2 = gtk_hbox_new ( FALSE, 2 );
  gtk_box_pack_start ( GTK_BOX( accData->main_vbox ), accData->box2, FALSE, FALSE, 0 );
  gtk_widget_show ( accData->box2 );
  
  accData->textbox_description = gtk_entry_new_with_max_length ( 25 );
  gtk_box_pack_end ( GTK_BOX(accData->box2), accData->textbox_description, FALSE, FALSE, 10 );
  gtk_widget_show ( accData->textbox_description );

  {
    accData->label = gtk_label_new ( "Account Description: " );
    gtk_box_pack_end ( GTK_BOX(accData->box2), accData->label, FALSE, FALSE, 0 );
    gtk_widget_show ( accData->label );
  }
     
  accData->box2 = gtk_hbox_new ( FALSE, 2 );
  gtk_box_pack_start ( GTK_BOX( accData->main_vbox ), accData->box2, FALSE, FALSE, 0 );
  gtk_widget_show ( accData->box2 );

  /*** Callbacks ****************************************************/
   
  gnome_dialog_button_connect (GNOME_DIALOG (accData->dialog), 0,
                               GTK_SIGNAL_FUNC (gnc_ui_accWindow_create_callback), 
                               accData);
  
  gnome_dialog_button_connect (GNOME_DIALOG (accData->dialog), 1,
                               GTK_SIGNAL_FUNC (gnc_ui_accWindow_cancelled_callback), 
                               accData);
  
                       
  /*** End of Callbacks *********************************************/  

  accData->box2 = gtk_hbox_new ( FALSE, 2 );
  gtk_box_pack_start ( GTK_BOX ( accData->main_vbox ), accData->box2, FALSE, FALSE, 0 );
  gtk_widget_show ( accData->box2 );

  gtk_widget_show ( accData->main_vbox );
  gtk_widget_show ( GTK_WIDGET(accData->dialog) );

  return accData;
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
  EditAccWindow *editAccData = NULL;

  L("STUB: editAccWindow needs to be written for GNOME.\n");

  return editAccData;
}

/********************************************************************\
 * Don't delete any structures -- the close callback wil do this    *
\********************************************************************/

void
xaccDestroyEditAccWindow (Account * acc) {

  L("STUB: xaccDestroyEditAccWindow needs to be written for GNOME.\n");
  
}

/********************************************************************\
 *                                                                  * 
\********************************************************************/

EditNotesWindow *
editNotesWindow (Account *acc) {
  EditNotesWindow *enw = NULL;

  L("STUB: editNotesWindow needs to be written for GNOME.\n");

  return enw;
}

/********************************************************************\
 * don't delete any structures; the close callack will do this       *
\********************************************************************/

void 
xaccDestroyEditNotesWindow (Account *acc) {
  
  L("STUB: xaccDestroyEditNotesWindow needs to be written for GNOME.\n");

}


/********************** END OF FILE *********************************\
\********************************************************************/
