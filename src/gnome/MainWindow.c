/********************************************************************\
 * MainWindow.c -- the main window, and associated helper functions * 
 *                 and callback functions for xacc (X-Accountant)   *
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

#include "MainWindow.h"
#include "MenuBar.h"
#include "Dialogs.h"
#include "messages.h"
#include "version.h"
#include "RegWindow.h"

#include "main.h"

gchar *clist_titles[] =
{
  ACC_NAME_STR,
  ACC_TYPE_STR,
  BALN_STR 
};

struct main_window {

};

static void
acct_tree_open_selected(GtkWidget *child) {  
  Account *acct = gtk_object_get_user_data(GTK_OBJECT(child));
  fprintf(stderr, "calling regWindowSimple(%p)\n", acct);
  regWindowSimple(acct);
}

static void
acct_tree_select(GtkTree *tree, GtkWidget *child) {
  //if(bevent && bevent->type == GDK_2BUTTON_PRESS) {
  acct_tree_open_selected(child);
  //}
}

static void
cram_accts_into_tree(GtkTree *maintree, AccountGroup *accts) {
  int count = xaccGetNumAccounts(accts);
  int i;
  
  for(i=0; i<count; i++) 
  {
    Account *acc = xaccGroupGetAccount(accts, i);
    
    gchar *rowstrs[3]; 

    GtkWidget *tree_item;

    rowstrs[0] = xaccAccountGetName (acc);
    rowstrs[1] = xaccAccountGetDescription (acc);
    rowstrs[2] = xaccAccountGetNotes (acc);

    tree_item = gtk_tree_item_new_with_label( rowstrs[0] );
    /* Set the tree item to point to the actual account so we can reach it
       trivially when the user selects the row.  (Should we use
       gtk_*_data_full and have a destroy notify?) */
    gtk_object_set_user_data(GTK_OBJECT(tree_item), acc); 

    gtk_tree_append(GTK_TREE(maintree), tree_item ); 
    gtk_widget_show ( tree_item );
  }
  gtk_signal_connect (GTK_OBJECT (maintree), 
                      "select_child",
                      (GtkSignalFunc) acct_tree_select, 
                      NULL);
}

void
main_window_init(AccountGroup *accts)
{

  GtkWidget *scrolled_win;
  GtkWidget *toolBar[5];
  GtkTooltips *tooltip; 
  GtkWidget *main_vbox;
  GtkWidget *clist_vbox;
  GtkWidget *button_bar;
  GtkWidget *menubar;
  GtkWidget *maintree;
  GtkWidget *clist;
  GtkWidget *notebook;
  
  GtkAcceleratorTable *accel;

  /* this is the GtkMenuEntry structure used to create new menus.  The
     first member is the menu definition string.  The second, the default
     accelerator key used to access this menu function with the keyboard.
     The third is the callback function to call when this menu item is
     selected (by the accelerator key, or with the mouse.) The last
     member is the data to pass to your callback function.  */
  
  GtkMenuEntry menu_items[] =
  {
    {"<Main>/File/New", "<control>N", NULL, NULL},
    {"<Main>/File/Open", "<control>O", file_cmd_open, NULL},
    {"<Main>/File/Save", "<control>S", file_cmd_save, NULL},
    {"<Main>/File/Save as", NULL, NULL, NULL},
    {"<Main>/File/<separator>", NULL, NULL, NULL},
    {"<Main>/File/Quit", "<control>Q", file_cmd_quit, NULL },
    {"<Main>/Options/Preferences..", "<control>A", options_cb, NULL},
    {"<Main>/Help/Help", NULL, help_cb, NULL},
    {"<Main>/Help/<separator>", NULL, NULL, NULL},
    {"<Main>/Help/About..", NULL, about_cb, NULL}
  };
  
  /* calculate the number of menu_item's */
  int nmenu_items = sizeof(menu_items) / sizeof(menu_items[0]);

  MenuBar *main_menu_bar;

  maintree = gtk_tree_new ( );

  /* Cram accounts into the tree widget */
  cram_accts_into_tree(GTK_TREE(maintree), accts);

  /* Create the notebook */
  notebook = gtk_notebook_new ();
  gtk_notebook_set_tab_pos ( GTK_NOTEBOOK( notebook ), GTK_POS_TOP );
  gtk_widget_show ( notebook );
  
  /* Create main vbox */
  main_vbox = gtk_vbox_new( FALSE, 1 );
  gtk_container_border_width( GTK_CONTAINER( main_vbox ), 2 );
  gnome_app_set_contents ( GNOME_APP ( app ), main_vbox );

  /* Create main hbox which will hold the clist widget */
  clist_vbox = gtk_vbox_new( FALSE, 1 );

  /* Now create clist, and pack it in the hbox we just created */
  clist = gtk_clist_new_with_titles ( 3, clist_titles );
  gtk_box_pack_start ( GTK_BOX( clist_vbox ), clist, TRUE, TRUE, 1 );
  
  /* Fix the column widths */
  gtk_clist_set_column_width ( GTK_CLIST(clist), 1, 85 );
  gtk_clist_set_column_width ( GTK_CLIST(clist), 0, 85 );
  
  gtk_widget_show ( clist );
      
  /* create scrolled window */
  scrolled_win = gtk_scrolled_window_new (NULL, NULL);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_win),
				  GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_widget_show (scrolled_win);

  {
    MenuBarGroup *mbg = menuBarGroupCreate();
    main_menu_bar = menuBarCreate(mbg, "<Main>");
    menuBarGroupAddItems(mbg, menu_items, nmenu_items);
  }

  menubar = main_menu_bar->widget;
  accel = main_menu_bar->table;

#if 0

  {
    /* Here's how to use the new MenuBar stuff to create multiple menu
       bars */

    GtkWidget *test_win = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    GtkWidget *test_vbox = gtk_vbox_new(FALSE, 1);
    MenuBarGroup *mbg = menuBarGroupCreate();
    MenuBar *mb1;
    MenuBar *mb2;
    GtkMenuEntry items[] =
    {
      {"<Stumpy>/Eat/Grubs", "<control>N", NULL, NULL},
      {"<Stumpy>/Eat/Gravel", "<control>O", NULL, NULL},
      {"<Stumpy>/So/Scary", "<control>S", NULL, NULL},
      {"<Stumpy2>/Eat-2/Grubs", "<control>N", NULL, NULL},
      {"<Stumpy2>/Eat-2/Gravel", "<control>O", NULL, NULL},
      {"<Stumpy2>/So-2/Scary/Err/Umm", "<control>S", NULL, NULL}
    };
    int nitems = sizeof(items) / sizeof(items[0]);
    
    mb1 = menuBarCreate(mbg, "<Stumpy>");
    mb2 = menuBarCreate(mbg, "<Stumpy2>");
    
    menuBarGroupAddItems(mbg, items, nitems);

    gtk_window_set_title(GTK_WINDOW(test_win), "Stumpy");
    gtk_container_add(GTK_CONTAINER(test_win), test_vbox);
    gtk_box_pack_start(GTK_BOX(test_vbox), menuBarGetWidget(mb1),
                       FALSE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(test_vbox), menuBarGetWidget(mb2),
                       FALSE, TRUE, 0);

    gtk_widget_show(menuBarGetWidget(mb1));
    gtk_widget_show(menuBarGetWidget(mb2));
    gtk_widget_show(test_vbox);
    gtk_widget_show(test_win);

  }
#endif

//gtk_window_add_accelerator_table(GTK_WINDOW(window), accel);
  gtk_box_pack_start(GTK_BOX(main_vbox), menubar, FALSE, TRUE, 0);     
  gtk_container_add( GTK_CONTAINER( main_vbox ), notebook );
//  gtk_box_pack_start (GTK_BOX (main_vbox), scrolled_win, TRUE, TRUE, 0);
  gtk_container_add(GTK_CONTAINER(scrolled_win), maintree);
//  gtk_container_add(GTK_CONTAINER(notebook), clist_vbox );

  /* Append some pages to notebook */
  {
    GtkWidget *label;
    label = gtk_label_new ( " All Accounts " );
    gtk_notebook_append_page (GTK_NOTEBOOK(notebook), clist_vbox, label);
    
    label = gtk_label_new ( " Bank Accounts " );
    gtk_notebook_append_page (GTK_NOTEBOOK(notebook), scrolled_win, label);
  }
  
  gtk_widget_show(clist_vbox);
  gtk_widget_show(menubar);

  /* create a bunch of buttons */
    
  button_bar = gtk_hbox_new(FALSE, 1);
  toolBar[open] = gtk_button_new_with_label ( OPEN_STR );
  toolBar[close] = gtk_button_new_with_label ( NEW_STR );
  toolBar[button3] = gtk_button_new_with_label ( EDIT_STR );
  toolBar[button4] = gtk_button_new_with_label ( DELETE_STR );
  toolBar[exit] = gtk_button_new_with_label (" Exit ");

  /* Initialize callbacks */
  gtk_signal_connect(GTK_OBJECT(toolBar[exit]), "clicked",
                     GTK_SIGNAL_FUNC (file_cmd_quit), NULL);

  /* Initilize ToolTips */

  tooltip = gtk_tooltips_new ();
  gtk_tooltips_set_tip (tooltip, toolBar[open], TOOLTIP_OPEN, NULL);
  gtk_tooltips_set_tip (tooltip, toolBar[close], TOOLTIP_NEW , NULL);
  gtk_tooltips_set_tip (tooltip, toolBar[button3], TOOLTIP_EDIT, NULL);
  gtk_tooltips_set_tip (tooltip, toolBar[button4], TOOLTIP_DELETE, NULL); 
  gtk_tooltips_set_tip (tooltip, toolBar[exit], TOOLTIP_EDIT, NULL);  

  /* Pack the buttons into the toolbar */ 
  gtk_box_pack_start(GTK_BOX(button_bar), toolBar[0], FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(button_bar), toolBar[1], FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(button_bar), toolBar[2], FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(button_bar), toolBar[3], FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(button_bar), toolBar[4], FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(main_vbox), button_bar, FALSE, TRUE, 1);

  gtk_widget_show(toolBar[open]);
  gtk_widget_show(toolBar[close]);
  gtk_widget_show(toolBar[button3]);
  gtk_widget_show(toolBar[button4]);
  gtk_widget_show(toolBar[exit]);
  gtk_widget_show(button_bar);
  gtk_widget_show(maintree);

  /* Setup some callbacks */
	
  gtk_signal_connect (GTK_OBJECT (toolBar[1]), "clicked",
		      GTK_SIGNAL_FUNC (add_account), accts);

  //gtk_signal_connect (GTK_OBJECT(window), "destroy",
  //                    GTK_SIGNAL_FUNC (gnucash_shutdown), NULL);
  //gtk_signal_connect (GTK_OBJECT (window), "delete_event",
  //                    GTK_SIGNAL_FUNC (gnucash_shutdown), NULL);		       

  gtk_widget_set_usize ( app, 400, 400 );		      
		      
  /* Show everything now that it is created */

  gtk_widget_show(main_vbox);
  gtk_widget_show ( app );

} 

/* Standard Gnome About Dialog, need I say more? */
void
about_cb (GtkWidget *widget, void *data)
{
  GtkWidget *about;
  gchar *authors[] = {
  /* Here should be your names */
          "Rob Clark",
          "Linas Vepsta",
          "Jeremy Collins",
          "Rob Browning",
          "For more see http://gnucash.ml.org/developers.html",
          NULL
          };

  about = gnome_about_new ( "GnuCash", VERSION,
                            "(C) 1998 The GnuCash Project",
                            authors,
                            "GnuCash: The GNU way to manage your money!",
                            NULL);
  gtk_widget_show (about);

}                          

/* Help system callback */
void
help_cb ( GtkWidget *widget, void *data )
{
  /* We need some config menus to setup were the docs are located */
  /* for now I just set it to be $HOME/xacc-docs/                 */
  
  gchar *docs_path = "xacc-docs/xacc-main.html";
  
  docs_path = gnome_util_prepend_user_home( docs_path );

  gnome_help_goto( NULL, docs_path );

  g_free( docs_path );

}

/* Some dialog stubs to be worked on */
/* We might want to move these to there own file =\ */

void
file_new_cb ( GtkWidget *widget, void *data )
{

}

/* Options dialog... this should house all of the config options     */
/* like where the docs reside, and whatever else is deemed necessary */
void
options_cb ( GtkWidget *widget, void *data )
{
  GnomePropertyBox *box;
  GtkWidget *w, *label, *box2;

  box = GNOME_PROPERTY_BOX(gnome_property_box_new());
  w = gtk_button_new_with_label("Click me (Page #1)");

  box2 = gtk_vbox_new ( FALSE, 1 );
  gtk_box_pack_start(GTK_BOX(box2), w, FALSE, FALSE, 0);    
  
  gtk_widget_show ( box2 );
  
  gtk_widget_show(w);
  label = gtk_label_new("Config Box 1");
  gtk_widget_show(label);
  
  gnome_property_box_append_page(box, box2, label);
  w = gtk_button_new_with_label("Click me (Page #2)");
  gtk_widget_show(w);
  
  label = gtk_label_new("Config Box 2");
  gtk_widget_show(label);
  
  gnome_property_box_append_page(box, w, label);
  
  gtk_widget_set_usize ( box, 500, 400 );
  gtk_widget_set_usize ( box2, 225, 225 ); 
  
  gtk_widget_show(GTK_WIDGET(box));
}

void
add_account ( AccountGroup *acct )
{
  GtkWidget *add_dialog;
  
  add_dialog = create_add_account_dialog();
  
  /* Callbacks */
  
//  gtk_signal_connect (GTK_OBJECT (add_dialog->Ok), "clicked",
//		      GTK_SIGNAL_FUNC (add_account_finish), accts);
  
  g_print ("New Account\n");
}



/********************* END OF FILE **********************************/

/*
  Local Variables:
  tab-width: 2
  indent-tabs-mode: nil
  mode: c-mode
  c-indentation-style: gnu
  eval: (c-set-offset 'block-open '-)
  End:
*/
