/********************************************************************\
 * main.c -- main for xacc (X-Accountant)                           *
 * Copyright (C) 1997 Robin D. Clark                                *
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

#include <config.h>
#include <gnome.h>

#include <stdlib.h>

#include "config.h"
#include "main.h"
#include "FileIO.h"
#include "Group.h"
#include "util.h"
#include "MainWindow.h"

/** PROTOTYPES ******************************************************/

/** GLOBALS *********************************************************/
char        *datafile = NULL;
char        *helpPath = NULL;
GtkWidget   *toplevel;
GtkWidget   *filebox;

GtkWidget   *app;


/* The names of the different types of accounts.  For resource
 * specification. Must match the enums in Account.h */

gchar *accRes[] ={
  "bank",
  "cash",
  "asset",
  "credit",
  "liability",
  "portfolio",
  "mutual",
  "income",
  "expense",
  "equity"
};


void file_ok_sel (GtkWidget *w, GtkFileSelection *fs)
{
  datafile = gtk_file_selection_get_filename (GTK_FILE_SELECTION (fs));

  /* Check to see if this is a valid datafile */
  if ( datafile != NULL )
  {
    topgroup = xaccReadAccountGroup (datafile);   
    gtk_widget_destroy ( filebox );
  }
 
  if( NULL == topgroup )
  {
    /* load the accounts data from datafile*/
    topgroup = xaccMallocAccountGroup(); 
  } 

  main_window_init(topgroup);
}

void destroy (GtkWidget *widget, gpointer *data)
{
  gtk_main_quit ();
}

void file_cmd_open (GtkWidget *widget, gpointer data)
{
  g_print ( "Menu Command: file open\n" );
}

void file_cmd_quit (GtkWidget *widget, gpointer data)
{
  gtk_main_quit();
}

/********************************************************************\
 * main                                                             *
 *  the entry point for the program... sets up the top level widget * 
 *  and calls the mainWindow() function which creates the main      * 
 *  window.                                                         * 
 *                                                                  * 
 * Args:   argc, the number of command line arguments, and argv,    * 
 *         the array of command line args                           * 
 * Return:                                                          * 
 * Global: topgroup - the data from the datafile                    *
 *         datafile - the name of the user's datafile               *
\********************************************************************/
int 
main( int argc, char *argv[] )
{   
 // gtk_init ( &argc, &argv );
 
  gnome_init ("GnuCash", NULL, argc, argv,
              0, NULL);
  
  prepare_app(); 

  {
    /* Use a nicer font IMO, if available */
    char font[] = "-adobe-helvetica-medium-r-normal--*-100-*-*-*-*-*-*";
    GtkStyle *st = gtk_widget_get_default_style();
    GdkFont *f = gdk_font_load(font);
    if(st && f) {
      st->font = f;
    }
  }

  filebox = gtk_file_selection_new ( "Open..." );
  
  /* read in the filename (should be the first arg after all
   * the X11 stuff */
  if( argc > 1 )
  {
    datafile = argv[1];

    if( datafile != NULL ) 
    {
   
      /* load the accounts data from datafile*/
      topgroup = xaccReadAccountGroup (datafile); 

      if ( topgroup == NULL )
      {
      	GtkWidget *dialog;
      	GtkWidget *button;
      	GtkWidget *label;
      	
      	dialog = gtk_dialog_new ();
      	
      	button = gtk_button_new_with_label ( "Ok" );
        gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->action_area), button,
                            TRUE, TRUE, 0);
        gtk_widget_show ( button );

        label = gtk_label_new (" \nInvalid filename \nNew file started.\n ");
        gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox), label, TRUE,
                            TRUE, 0);
        gtk_widget_show ( label );
        gtk_widget_show ( dialog );
        
	topgroup = xaccMallocAccountGroup(); 
      }
    }
    /* Create main window */
    main_window_init(topgroup);
  }
  else
  {
    /* Filebox code here */
    gtk_widget_show ( filebox );
  }
  
  /* Check to see if this is a valid datafile */
  if ( datafile != NULL )
    topgroup = xaccReadAccountGroup (datafile);   
  
  
  /* Callbacks for File Box and Stuff */
  
  gtk_signal_connect (GTK_OBJECT (filebox), "delete_event",
                      (GtkSignalFunc) gtk_widget_destroy, 
                      GTK_OBJECT(filebox));
  
  /* Connect the ok_button to file_ok_sel function */
  gtk_signal_connect (GTK_OBJECT (GTK_FILE_SELECTION (filebox)->ok_button),
                      "clicked", (GtkSignalFunc) file_ok_sel, filebox );
  
  /* Connect the cancel_button to also kill the app */
  gtk_signal_connect_object ( GTK_OBJECT (GTK_FILE_SELECTION (filebox)->cancel_button),
                              "clicked", (GtkSignalFunc) gtk_exit, NULL );
  

  
  /* Enter event loop */
  gtk_main();

  return 0;
}

void
prepare_app()
{
  app = gnome_app_new ( "gnucash", "GnuCash" );
  gtk_widget_realize (app);
/*  gtk_signal_connect (GTK_OBJECT (app), "delete_event",
                      GTK_SIGNAL_FUNC (quit_cb),
                      NULL); */

}

/****************** END OF FILE **********************/

/*
  Local Variables:
  tab-width: 2
  indent-tabs-mode: nil
  mode: c-mode
  c-indentation-style: gnu
  eval: (c-set-offset 'block-open '-)
  End:
*/
