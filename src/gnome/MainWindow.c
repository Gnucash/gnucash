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
 *                                                                  *
 *   Author: Rob Clark                                              *
 * Internet: rclark@cs.hmc.edu                                      *
 *  Address: 609 8th Street                                         *
 *           Huntington Beach, CA 92648-4632                        *
\********************************************************************/

#include "MainWindow.h"
#include "MenuBar.h"
#include "messages.h"

#include "main.h"

gchar *clist_titles[] =
{
  ACC_NAME_STR,
  ACC_TYPE_STR,
  BALN_STR 
};

struct main_window {

};

/* This should most likely be rewritting to use a Tree, not a Clist so
   we can represent the heirarchical account structure */
static void
cram_accts_into_clist(GtkCList *list, AccountGroup *accts) {
  int count = xaccGetNumAccounts(accts);
  int i;
  GtkWidget *item;
  
  for(i=0; i<count; i++) {
    Account *acc = getAccount(accts, i);
    
    gchar *rowstrs[] = { acc->accountName,
                         acc->description,
                         acc->notes };
    
    gint new_row = gtk_clist_append(GTK_CLIST(list), rowstrs); 

    /* Set the row to point to the actual account so we can reach it
    trivially when the user selects the row.  (Should we use
    gtk_*_data_full and have a destroy notify?) */
    gtk_clist_set_row_data(GTK_CLIST(list), new_row, acc); 
  }
}

void
main_window_init(AccountGroup *accts)
{

  GtkWidget *window;
  GtkWidget *toolBar[5];
    
  GtkTooltips *tooltip; 

  GtkWidget *label;
  GtkWidget *main_vbox;
  GtkWidget *button_bar;
    
  GtkWidget *menubar;

  GtkWidget *clist;
  GtkWidget *list_item;

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
    {"<Main>/File/Save", "<control>S", NULL, NULL},
    {"<Main>/File/Save as", NULL, NULL, NULL},
    {"<Main>/File/<separator>", NULL, NULL, NULL},
    {"<Main>/File/Quit", "<control>Q", gtk_main_quit, "OK, I'll quit"},
    {"<Main>/Options/General..", "<control>A", NULL, NULL},
    {"<Main>/Help/About..", NULL, NULL, NULL}
  };
  
  /* calculate the number of menu_item's */
  int nmenu_items = sizeof(menu_items) / sizeof(menu_items[0]);

  MenuBar *main_menu_bar;

  clist = gtk_clist_new_with_titles(3, clist_titles);    

  /* Fix the column widths */
  gtk_clist_set_column_width ( GTK_CLIST(clist), 1, 85 );
  gtk_clist_set_column_width ( GTK_CLIST(clist), 0, 85 );

  cram_accts_into_clist(GTK_CLIST(clist), accts);

  window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(GTK_WINDOW(window), "GnoMoney");

  main_vbox = gtk_vbox_new(FALSE, 1);
  gtk_container_border_width(GTK_CONTAINER(main_vbox), 1);
  gtk_container_add(GTK_CONTAINER(window), main_vbox);
  
  {
    MenuBarGroup *mbg = menuBarGroupCreate();
    main_menu_bar = menuBarCreate(mbg, "<Main>");
    menuBarGroupAddItems(mbg, menu_items, nmenu_items);
  }

  menubar = main_menu_bar->widget;
  accel = main_menu_bar->table;

#if 0

  {
    /* Here's how to use the new MenBar stuff to create multiple menu
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

  gtk_window_add_accelerator_table(GTK_WINDOW(window), accel);
  gtk_box_pack_start(GTK_BOX(main_vbox), menubar, FALSE, TRUE, 0);     
  gtk_widget_show(menubar);
   
  /* create a bunch of buttons */
    
  button_bar = gtk_hbox_new(FALSE, 1);
  toolBar[0] = gtk_button_new_with_label ( OPEN_STR );
  toolBar[1] = gtk_button_new_with_label ( NEW_STR );
  toolBar[2] = gtk_button_new_with_label ( EDIT_STR );
  toolBar[3] = gtk_button_new_with_label ( DELETE_STR );
  toolBar[4] = gtk_button_new_with_label (" Exit ");

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
  gtk_box_pack_start(GTK_BOX(main_vbox), clist, FALSE, FALSE, 1);
  gtk_box_pack_start(GTK_BOX(main_vbox), button_bar, FALSE, TRUE, 1);
    
    
  gtk_widget_show(toolBar[open]);
  gtk_widget_show(toolBar[close]);
  gtk_widget_show(toolBar[button3]);
  gtk_widget_show(toolBar[button4]);
  gtk_widget_show(toolBar[exit]);
  gtk_widget_show(button_bar);
  gtk_widget_show(clist);

  gtk_widget_set_usize (window, 400, 400 );
  gtk_widget_set_usize ( clist, 400, 300 );
    
  /* Setup some callbacks */
	
  gtk_signal_connect (GTK_OBJECT(window), "destroy",
                      GTK_SIGNAL_FUNC (destroy), NULL);

  //gtk_signal_connect(GTK_OBJECT(window), "destroy",
  //                   GTK_SIGNAL_FUNC(file_quit_cmd_callback),
  //                   "WM destroy");
    
  gtk_signal_connect (GTK_OBJECT (window), "delete_event",
                      GTK_SIGNAL_FUNC (gtk_exit), NULL);		       

  /* Show everything now that it is created */
  gtk_widget_show(main_vbox);
  gtk_container_border_width (GTK_CONTAINER (window), 2);
  gtk_widget_show(window);
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
