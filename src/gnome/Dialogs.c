// Dialogs.c

#include <gnome.h>
#include "Dialogs.h"

gpointer *add_account_dialog_init (  )
{
  return ( g_malloc ( sizeof ( add_account_dialog ) ) );
}

GtkWidget *create_add_account_dialog ()
{
  //GnomeDialog 	*dialog;
  GnomeDialog 	*add_account_dialog;
  GtkWidget 	*main_vbox;
  GtkWidget 	*box2;
  GtkWidget	*box3;
  GtkWidget	*box4;
  GtkWidget 	*frame;
  
  GSList 	*group;
  
//  GList		*parent_accounts;
  
  GtkWidget 	*label;
  GtkWidget 	*textbox_name;
  GtkWidget	*textbox_description;
  GtkWidget	*button;


  gchar *title = "Setup Account";
  
  add_account_dialog = GNOME_DIALOG(
                       gnome_dialog_new ( title, "Ok", "Cancel", NULL));

  gtk_window_set_title ( GTK_WINDOW ( add_account_dialog ), title );
    
  gtk_window_position ( GTK_WINDOW ( add_account_dialog ), 
                        GTK_WIN_POS_CENTER );

  main_vbox = gtk_vbox_new ( FALSE, 2 );
  gtk_container_add ( GTK_CONTAINER ( add_account_dialog->vbox ), main_vbox );  
 
  frame = gtk_frame_new ( "Account Type" );
  gtk_container_border_width (GTK_CONTAINER (frame), 10);
  gtk_box_pack_start ( GTK_BOX( main_vbox ), frame, FALSE, FALSE, 0 );
  gtk_widget_show ( frame );

  box2 = gtk_hbox_new ( FALSE, 2 );
  gtk_container_add ( GTK_CONTAINER( frame ), box2 );
  gtk_widget_show ( box2 );
  
  box3 = gtk_vbox_new ( FALSE, 2 );
  gtk_box_pack_start ( GTK_BOX( box2 ), box3, FALSE, FALSE, 0 );
  gtk_widget_show ( box3 );
  
  box4 = gtk_vbox_new ( FALSE, 2 );
  gtk_box_pack_start ( GTK_BOX( box2 ), box4, FALSE, FALSE, 0 );
  gtk_widget_show ( box4 );     
  
  /* Create radio button group for Account Type */
    
  button = gtk_radio_button_new_with_label (NULL, "Bank");
  gtk_box_pack_start (GTK_BOX (box3), button, TRUE, TRUE, 0 );
  gtk_widget_show (button);

  group = gtk_radio_button_group (GTK_RADIO_BUTTON (button));
  button = gtk_radio_button_new_with_label(group, "Cash");
  gtk_toggle_button_set_state (GTK_TOGGLE_BUTTON (button), TRUE);
  gtk_box_pack_start (GTK_BOX (box3), button, TRUE, TRUE, 0 );
  gtk_widget_show (button);

  group = gtk_radio_button_group (GTK_RADIO_BUTTON (button));
  button = gtk_radio_button_new_with_label(group, "Asset");
  gtk_box_pack_start (GTK_BOX (box3), button, TRUE, TRUE, 0 );
  gtk_widget_show (button);

  group = gtk_radio_button_group (GTK_RADIO_BUTTON (button));
  button = gtk_radio_button_new_with_label(group, "Credit Card");
  gtk_box_pack_start (GTK_BOX (box3), button, TRUE, TRUE, 0 );
  gtk_widget_show (button);  
  
  group = gtk_radio_button_group (GTK_RADIO_BUTTON (button));
  button = gtk_radio_button_new_with_label(group, "Liability");
  gtk_box_pack_start (GTK_BOX (box3), button, TRUE, TRUE, 0 );
  gtk_widget_show (button);
  
  group = gtk_radio_button_group (GTK_RADIO_BUTTON (button));
  button = gtk_radio_button_new_with_label(group, "Stock");
  gtk_box_pack_start (GTK_BOX (box4), button, TRUE, TRUE, 0 );
  gtk_widget_show (button);

  group = gtk_radio_button_group (GTK_RADIO_BUTTON (button));
  button = gtk_radio_button_new_with_label(group, "Mutual Fund");
  gtk_box_pack_start (GTK_BOX (box4), button, TRUE, TRUE, 0 );
  gtk_widget_show (button);
    
  group = gtk_radio_button_group (GTK_RADIO_BUTTON (button));
  button = gtk_radio_button_new_with_label(group, "Income");
  gtk_box_pack_start (GTK_BOX (box4), button, TRUE, TRUE, 0 );
  gtk_widget_show (button);
  
  group = gtk_radio_button_group (GTK_RADIO_BUTTON (button));
  button = gtk_radio_button_new_with_label(group, "Expense");
  gtk_box_pack_start (GTK_BOX (box4), button, TRUE, TRUE, 0 );
  gtk_widget_show (button);
  
  group = gtk_radio_button_group (GTK_RADIO_BUTTON (button));
  button = gtk_radio_button_new_with_label(group, "Equity");
  gtk_box_pack_start (GTK_BOX (box4), button, TRUE, TRUE, 0 );
  gtk_widget_show (button);  

  box2 = gtk_hbox_new ( FALSE, 2 );
  gtk_box_pack_start ( GTK_BOX( main_vbox ), box2, FALSE, FALSE, 0 );
  gtk_widget_show ( box2 );
  
  textbox_name = gtk_entry_new_with_max_length ( 25 );
  gtk_box_pack_end ( GTK_BOX(box2), textbox_name, FALSE, FALSE, 10 );
  gtk_widget_show ( textbox_name );
  
  {
    label = gtk_label_new ( "Account Name: " );
    gtk_box_pack_end ( GTK_BOX(box2), label, FALSE, FALSE, 0 );
    gtk_widget_show ( label );
  }

  box2 = gtk_hbox_new ( FALSE, 2 );
  gtk_box_pack_start ( GTK_BOX( main_vbox ), box2, FALSE, FALSE, 0 );
  gtk_widget_show ( box2 );
  
  textbox_description = gtk_entry_new_with_max_length ( 25 );
  gtk_box_pack_end ( GTK_BOX(box2), textbox_description, FALSE, FALSE, 10 );
  gtk_widget_show ( textbox_description );

  {
    label = gtk_label_new ( "Account Description: " );
    gtk_box_pack_end ( GTK_BOX(box2), label, FALSE, FALSE, 0 );
    gtk_widget_show ( label );
  }
     
  box2 = gtk_hbox_new ( FALSE, 2 );
  gtk_box_pack_start ( GTK_BOX( main_vbox ), box2, FALSE, FALSE, 0 );
  gtk_widget_show ( box2 );

  {
    label = gtk_label_new ( "Parent Account: " );
    gtk_box_pack_end ( GTK_BOX(box2), label, FALSE, FALSE, 0 );
    gtk_widget_show ( label );
  }
   

  
  box2 = gtk_hbox_new ( FALSE, 2 );
  gtk_box_pack_start ( GTK_BOX ( main_vbox ), box2, FALSE, FALSE, 0 );
  gtk_widget_show ( box2 );

  gtk_widget_show ( main_vbox );
  gtk_widget_show ( add_account_dialog );

  return add_account_dialog;

}
