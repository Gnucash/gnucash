/********************************************************************\
 * MainWindow.c -- the main window, and associated helper functions * 
 *                 and callback functions for xacc (X-Accountant)   *
 * Copyright (C) 1997 Robin D. Clark                                *
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

//#include "main_window.h"
#include "main.h"

gchar *clist_titles[] =
{
	"Account Name",
	"Type",
	"Balance"
};

struct main_window {

};

void main_window_init()
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

    clist = gtk_clist_new_with_titles(3, clist_titles);    

    window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title(GTK_WINDOW(window), "GnoMoney");

    main_vbox = gtk_vbox_new(FALSE, 1);
    gtk_container_border_width(GTK_CONTAINER(main_vbox), 1);
    gtk_container_add(GTK_CONTAINER(window), main_vbox);

    get_main_menu(&menubar, &accel);
    gtk_window_add_accelerator_table(GTK_WINDOW(window), accel);
    gtk_box_pack_start(GTK_BOX(main_vbox), menubar, FALSE, TRUE, 0);     
    gtk_widget_show(menubar);
   
   /* create a bunch of buttons */
    
    button_bar = gtk_hbox_new(FALSE, 1);
    toolBar[0] = gtk_button_new_with_label (" Open ");
    toolBar[1] = gtk_button_new_with_label (" New ");
    toolBar[2] = gtk_button_new_with_label (" Edit ");
    toolBar[3] = gtk_button_new_with_label (" Delete ");
    toolBar[4] = gtk_button_new_with_label (" Exit ");

    /* Initilize ToolTips */
	
    /*
    tooltip = gtk_tooltips_new ();
    gtk_tooltips_set_tips (tooltip, toolBar[open], "Open an account.");
    gtk_tooltips_set_tips (tooltip, toolBar[close], "Create a new account.");
    gtk_tooltips_set_tips (tooltip, toolBar[button3], "Edit selected account");
    gtk_tooltips_set_tips (tooltip, toolBar[button4], "Delete selected account"); 
    gtk_tooltips_set_tips (tooltip, toolBar[exit], "Exit GnoMoney.");  
    */
    
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
