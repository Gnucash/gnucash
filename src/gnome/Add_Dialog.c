/********************************************************************\
 * Add_Dialog.c --  functions for creating add_account_dialog       * 
 *                  and callback functions for GnuCash              *
 * Copyright (C) 1998 Jeremy Collins				    *
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
\********************************************************************/

#include <gnome.h>
#include "Add_Dialog.h"
#include "MenuBar.h"
#include "messages.h"
#include "AccInfo.h"
#include <string.h>

/** Globals *********************************************************/


add_account_dialog 
*add_account_dialog_init (  )
{
  return ( g_malloc ( sizeof ( add_account_dialog ) ) );
}


/** Callback Functions **********************************************/

static void
add_account_toggle_callback (GtkWidget *widget, gpointer data)
{
  int *val;

  if (GTK_TOGGLE_BUTTON (widget)->active)
  {
    val = data;
    *val = (long) gtk_object_get_user_data (GTK_OBJECT (widget));
  }
}

static void
add_account_omenu_update ( GtkWidget *widget, gpointer data )
{
  add_account_dialog *info = data;
  Account *account = gtk_object_get_user_data(GTK_OBJECT(widget));

  if (account) {
    info->parent_account = account;
  }
   
}

static void 
add_account_dialog_okclicked_cb(GtkWidget * dialog, gpointer data)
{
  Transaction 		*trans;
  Account      		*account;
  add_account_dialog 	*info = data; 
  
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
       
  if (info->parent_account) {
    g_print( "Return Code (SUB): %d\n", xaccInsertSubAccount (info->parent_account, account));
  } else {
    g_print( "Return Code (TOPGROUP): %d\n", insertAccount( topgroup, account ));
  }
  xaccAccountCommitEdit (account);

  refresh_tree();

  gnome_dialog_close ( GNOME_DIALOG(info->dialog) );

}

static void
add_account_canceled_cb ( GtkWidget *ignore, gpointer data )
{
  add_account_dialog *info = data;
  
  gnome_dialog_close( GNOME_DIALOG(info->dialog));
}

void
add_account_dialog_destroy ( GtkWidget *ignore, GnomeDialog *dialog )
{
  //gtk_widget_destroy ( GTK_WIDGET(dialog) );
}

static void
build_omenu ( GtkMenu *menu, add_account_dialog *accWindow, Account *acct,
              int is_submenu )
{
  /* Create an popup menu to select which account to add this new account too */
  /* needs to create submenus for each Parent account that has subaccounts    */
  
  GtkWidget *omenu;
  GtkWidget *submenu;
  GtkWidget *menuitem;
  int i;
  int count = xaccGroupGetNumAccounts(acct);
     
  GSList *omenu_group;
 
  if ( is_submenu == -1 )
  {
    omenu = gtk_option_menu_new ();
    menu = gtk_menu_new ();
    submenu = NULL;
    omenu_group = NULL;
  }
  
  menuitem = gtk_radio_menu_item_new_with_label (omenu_group, "New TopLevel Account" );
  gtk_object_set_user_data(GTK_OBJECT(menuitem), NULL); 
  omenu_group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
  gtk_signal_connect (GTK_OBJECT (menuitem), "activate",
		      GTK_SIGNAL_FUNC (add_account_omenu_update), accWindow);
  gtk_menu_append (GTK_MENU (menu), menuitem);
  gtk_widget_show (menuitem);
  
  for ( i=0; i<count; i++ )
  {
    Account *account = xaccGroupGetAccount (acct, i );
    gchar *account_name = xaccAccountGetName ( account );
           
    menuitem = gtk_radio_menu_item_new_with_label (omenu_group, account_name);
    gtk_object_set_user_data(GTK_OBJECT(menuitem), account); 
    gtk_signal_connect (GTK_OBJECT (menuitem), "activate",
		        GTK_SIGNAL_FUNC (add_account_omenu_update), accWindow);
    omenu_group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
    gtk_menu_append (GTK_MENU (menu), menuitem);
    gtk_widget_show (menuitem);
      
  }
         
  gtk_option_menu_set_menu (GTK_OPTION_MENU (omenu), menu);
  gtk_box_pack_end ( GTK_BOX(accWindow->box2), omenu, FALSE, FALSE, 10 );
  gtk_widget_show ( omenu );
}

void
create_add_account_dialog (AccountGroup *acct)
{
  GtkWidget *button;
  add_account_dialog *accWindow;

  /* This should be in messages.h */  
  gchar *title = "Setup Account";

  accWindow = add_account_dialog_init();
  accWindow->parent_account = acct;
  
  accWindow->dialog = 
         GNOME_DIALOG( gnome_dialog_new ( title, 
                       GNOME_STOCK_BUTTON_OK,
                       GNOME_STOCK_BUTTON_CANCEL,
                       NULL));

  gtk_window_set_title ( GTK_WINDOW ( accWindow->dialog ), title );
    
  gtk_window_position ( GTK_WINDOW ( accWindow->dialog ), 
                        GTK_WIN_POS_CENTER );

  accWindow->main_vbox = gtk_vbox_new ( FALSE, 2 );
  gtk_container_add ( GTK_CONTAINER ( (accWindow->dialog)->vbox ), accWindow->main_vbox );  
 
  accWindow->frame = gtk_frame_new ( "Account Type" );
  gtk_container_border_width (GTK_CONTAINER (accWindow->frame), 10);
  gtk_box_pack_start ( GTK_BOX( accWindow->main_vbox ), accWindow->frame, FALSE, FALSE, 0 );
  gtk_widget_show ( accWindow->frame );

  accWindow->box2 = gtk_hbox_new ( FALSE, 2 );
  gtk_container_add ( GTK_CONTAINER( accWindow->frame ), accWindow->box2 );
  gtk_widget_show ( accWindow->box2 );
  
  accWindow->box3 = gtk_vbox_new ( FALSE, 2 );
  gtk_box_pack_start ( GTK_BOX( accWindow->box2 ), accWindow->box3, FALSE, FALSE, 0 );
  gtk_widget_show ( accWindow->box3 );
  
  accWindow->box4 = gtk_vbox_new ( FALSE, 2 );
  gtk_box_pack_start ( GTK_BOX( accWindow->box2 ), accWindow->box4, FALSE, FALSE, 0 );
  gtk_widget_show ( accWindow->box4 );     
  
  /* Create radio button group for Account Type **********************/
    
  button = gtk_radio_button_new_with_label (NULL, "Bank");
  gtk_box_pack_start (GTK_BOX (accWindow->box3), button, TRUE, TRUE, 0 );
  gtk_object_set_user_data (GTK_OBJECT (button), (gpointer) BANK);
  gtk_signal_connect (GTK_OBJECT (button), "toggled",
		      (GtkSignalFunc) add_account_toggle_callback,
		      &accWindow->type);
  if (accWindow->type == BANK)
    gtk_toggle_button_set_state (GTK_TOGGLE_BUTTON (button), TRUE);
  gtk_widget_show (button);
      
  accWindow->group = gtk_radio_button_group (GTK_RADIO_BUTTON (button));
  button = gtk_radio_button_new_with_label(accWindow->group, "Cash");
  gtk_box_pack_start (GTK_BOX (accWindow->box3), button, TRUE, TRUE, 0 );
  gtk_object_set_user_data (GTK_OBJECT (button), (gpointer) CASH);
  gtk_signal_connect (GTK_OBJECT (button), "toggled",
		      (GtkSignalFunc) add_account_toggle_callback,
		      &accWindow->type);
  if (accWindow->type == CASH)
    gtk_toggle_button_set_state (GTK_TOGGLE_BUTTON (button), TRUE);
  gtk_widget_show (button);
  
  accWindow->group = gtk_radio_button_group (GTK_RADIO_BUTTON (button));
  button = gtk_radio_button_new_with_label(accWindow->group, "Asset");
  gtk_box_pack_start (GTK_BOX (accWindow->box3), button, TRUE, TRUE, 0 );
  gtk_object_set_user_data (GTK_OBJECT (button), (gpointer) ASSET);
  gtk_signal_connect (GTK_OBJECT (button), "toggled",
		      (GtkSignalFunc) add_account_toggle_callback,
		      &accWindow->type);
  if (accWindow->type == ASSET)
    gtk_toggle_button_set_state (GTK_TOGGLE_BUTTON (button), TRUE);
  gtk_widget_show (button);

  accWindow->group = gtk_radio_button_group (GTK_RADIO_BUTTON (button));
  button = gtk_radio_button_new_with_label(accWindow->group, "Credit Card");
  gtk_box_pack_start (GTK_BOX (accWindow->box3), button, TRUE, TRUE, 0 );
  gtk_object_set_user_data (GTK_OBJECT (button), (gpointer) CREDIT);
  gtk_signal_connect (GTK_OBJECT (button), "toggled",
		      (GtkSignalFunc) add_account_toggle_callback,
		      &accWindow->type);
  if (accWindow->type == CREDIT)
    gtk_toggle_button_set_state (GTK_TOGGLE_BUTTON (button), TRUE);
  gtk_widget_show (button);
  
  accWindow->group = gtk_radio_button_group (GTK_RADIO_BUTTON (button));
  button = gtk_radio_button_new_with_label(accWindow->group, "Liability");
  gtk_box_pack_start (GTK_BOX (accWindow->box3), button, TRUE, TRUE, 0 );
  gtk_object_set_user_data (GTK_OBJECT (button), (gpointer) LIABILITY);
  gtk_signal_connect (GTK_OBJECT (button), "toggled",
		      (GtkSignalFunc) add_account_toggle_callback,
		      &accWindow->type);
  if (accWindow->type == LIABILITY)
    gtk_toggle_button_set_state (GTK_TOGGLE_BUTTON (button), TRUE);		      
  gtk_widget_show (button);
  
  accWindow->group = gtk_radio_button_group (GTK_RADIO_BUTTON (button));
  button = gtk_radio_button_new_with_label(accWindow->group, "Stock");
  gtk_box_pack_start (GTK_BOX (accWindow->box4), button, TRUE, TRUE, 0 );
  gtk_object_set_user_data (GTK_OBJECT (button), (gpointer) STOCK);
  gtk_signal_connect (GTK_OBJECT (button), "toggled",
		      (GtkSignalFunc) add_account_toggle_callback,
		      &accWindow->type);
  if (accWindow->type == STOCK)
    gtk_toggle_button_set_state (GTK_TOGGLE_BUTTON (button), TRUE);		     
  gtk_widget_show (button);

  accWindow->group = gtk_radio_button_group (GTK_RADIO_BUTTON (button));
  button = gtk_radio_button_new_with_label(accWindow->group, "Mutual Fund");
  gtk_box_pack_start (GTK_BOX (accWindow->box4), button, TRUE, TRUE, 0 );
  gtk_object_set_user_data (GTK_OBJECT (button), (gpointer) MUTUAL);
  gtk_signal_connect (GTK_OBJECT (button), "toggled",
		      (GtkSignalFunc) add_account_toggle_callback,
		      &accWindow->type);
  if (accWindow->type == MUTUAL)
    gtk_toggle_button_set_state (GTK_TOGGLE_BUTTON (button), TRUE);		      
  gtk_widget_show (button);
    
  accWindow->group = gtk_radio_button_group (GTK_RADIO_BUTTON (button));
  button = gtk_radio_button_new_with_label(accWindow->group, "Income");
  gtk_box_pack_start (GTK_BOX (accWindow->box4), button, TRUE, TRUE, 0 );
  gtk_object_set_user_data (GTK_OBJECT (button), (gpointer) INCOME);
  gtk_signal_connect (GTK_OBJECT (button), "toggled",
		      (GtkSignalFunc) add_account_toggle_callback,
		      &accWindow->type);
  if (accWindow->type == INCOME)
    gtk_toggle_button_set_state (GTK_TOGGLE_BUTTON (button), TRUE);		      
  gtk_widget_show (button);
  
  accWindow->group = gtk_radio_button_group (GTK_RADIO_BUTTON (button));
  button = gtk_radio_button_new_with_label(accWindow->group, "Expense");
  gtk_box_pack_start (GTK_BOX (accWindow->box4), button, TRUE, TRUE, 0 );
  gtk_object_set_user_data (GTK_OBJECT (button), (gpointer) EXPENSE);
  gtk_signal_connect (GTK_OBJECT (button), "toggled",
		      (GtkSignalFunc) add_account_toggle_callback,
		      &accWindow->type);
  if (accWindow->type == EXPENSE)
    gtk_toggle_button_set_state (GTK_TOGGLE_BUTTON (button), TRUE);		      
  gtk_widget_show (button);
  
  accWindow->group = gtk_radio_button_group (GTK_RADIO_BUTTON (button));
  button = gtk_radio_button_new_with_label(accWindow->group, "Equity");
  gtk_box_pack_start (GTK_BOX (accWindow->box4), button, TRUE, TRUE, 0 );
  gtk_object_set_user_data (GTK_OBJECT (button), (gpointer) EQUITY);
  gtk_signal_connect (GTK_OBJECT (button), "toggled",
		      (GtkSignalFunc) add_account_toggle_callback,
		      &accWindow->type);
  if (accWindow->type == EQUITY)
    gtk_toggle_button_set_state (GTK_TOGGLE_BUTTON (button), TRUE);		      
  gtk_widget_show (button);

  /************************ END OF RADIOBUTTONS ***********************/
  
  accWindow->box2 = gtk_hbox_new ( FALSE, 2 );
  gtk_box_pack_start ( GTK_BOX( accWindow->main_vbox ), accWindow->box2, FALSE, FALSE, 0 );
  gtk_widget_show ( accWindow->box2 );
  
  accWindow->textbox_name = gtk_entry_new_with_max_length ( 25 );
  gtk_box_pack_end ( GTK_BOX(accWindow->box2), accWindow->textbox_name, FALSE, FALSE, 10 );
  gtk_widget_show ( accWindow->textbox_name );
  
  {
    accWindow->label = gtk_label_new ( "Account Name: " );
    gtk_box_pack_end ( GTK_BOX(accWindow->box2), accWindow->label, FALSE, FALSE, 0 );
    gtk_widget_show ( accWindow->label );
  }

  accWindow->box2 = gtk_hbox_new ( FALSE, 2 );
  gtk_box_pack_start ( GTK_BOX( accWindow->main_vbox ), accWindow->box2, FALSE, FALSE, 0 );
  gtk_widget_show ( accWindow->box2 );
  
  accWindow->textbox_description = gtk_entry_new_with_max_length ( 25 );
  gtk_box_pack_end ( GTK_BOX(accWindow->box2), accWindow->textbox_description, FALSE, FALSE, 10 );
  gtk_widget_show ( accWindow->textbox_description );

  {
    accWindow->label = gtk_label_new ( "Account Description: " );
    gtk_box_pack_end ( GTK_BOX(accWindow->box2), accWindow->label, FALSE, FALSE, 0 );
    gtk_widget_show ( accWindow->label );
  }
     
  accWindow->box2 = gtk_hbox_new ( FALSE, 2 );
  gtk_box_pack_start ( GTK_BOX( accWindow->main_vbox ), accWindow->box2, FALSE, FALSE, 0 );
  gtk_widget_show ( accWindow->box2 );

  /* Call build menu here */

/*  {
    GtkWidget *menu;  
    build_omenu ( menu, accWindow, acct, -1 );
  }

  {
    accWindow->label = gtk_label_new ( "Parent Account:   " );
    gtk_box_pack_end ( GTK_BOX(accWindow->box2), accWindow->label, FALSE, FALSE, 0 );
    gtk_widget_show ( accWindow->label );
  }
*/ 
  
  /*** Callbacks ****************************************************/
   
  gnome_dialog_button_connect (GNOME_DIALOG (accWindow->dialog), 0,
                               GTK_SIGNAL_FUNC (add_account_dialog_okclicked_cb), 
                               accWindow);
  
  gnome_dialog_button_connect (GNOME_DIALOG (accWindow->dialog), 1,
                               GTK_SIGNAL_FUNC (add_account_canceled_cb), 
                               accWindow->dialog);
  
                       
  /*** End of Callbacks *********************************************/  

  accWindow->box2 = gtk_hbox_new ( FALSE, 2 );
  gtk_box_pack_start ( GTK_BOX ( accWindow->main_vbox ), accWindow->box2, FALSE, FALSE, 0 );
  gtk_widget_show ( accWindow->box2 );

  gtk_widget_show ( accWindow->main_vbox );
  gtk_widget_show ( GTK_WIDGET(accWindow->dialog) );

/*gnome_dialog_set_close ( accWindow->dialog, TRUE );*/
  
}



