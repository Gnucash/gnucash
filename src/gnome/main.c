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

#include <gtk/gtk.h>

#include <stdlib.h>

#include "config.h"
#include "main.h"
#include "FileIO.h"
#include "Group.h"
#include "util.h"
#include "MainWindow.h"

/* Motif GUI includes 
#include "FileBox.h"

*/

/** PROTOTYPES ******************************************************/

/** GLOBALS *********************************************************/
char    *datafile = NULL;
char    *helpPath = NULL;
GtkWidget   *toplevel;
GtkWidget *filebox;

/* The names of the different types of accounts.  For resource
 * specification. Must match the enums in Account.h */
/*
String accRes[] ={
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
*/


/* Get the selected filename and print it to the console */
/* This function should eventually get the filename, and */
/* close the filebox. 				         */
void file_ok_sel (GtkWidget *w, GtkFileSelection *fs)
{
  datafile = gtk_file_selection_get_filename (GTK_FILE_SELECTION (fs));
  if( datafile != NULL ) 
  {
   /* load the accounts data from datafile*/
    topgroup = xaccReadAccountGroup (datafile); 
   /* Close the filebox */
    gtk_widget_destroy(filebox);
  }
  
  if( NULL == topgroup )
    {
    topgroup = xaccMallocAccountGroup(); 
    topgroup->new = TRUE;
    }
}

void destroy (GtkWidget *widget, gpointer *data)
{
  gtk_main_quit ();
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
 *         toplevel - the toplevel widget, for creating new windows *
 *         app      - the XtAppContext                              *
\********************************************************************/
int 
main( int argc, char *argv[] )
  {
#if DEBUG_MEMORY
  char *blk;
  DEBUG("Initializing memory");
  blk = (char *)_malloc(8192);
  _free(blk);
  printf(" coresize = %d\n",_coresize());
  DEBUG("Done initializing memory");
#endif
  


  gtk_init (&argc, &argv);
  
  filebox = gtk_file_selection_new ("File selection");

  /* read in the filename (should be the first arg after all
   * the X11 stuff */
  if( argc > 1 )
    datafile = argv[1];
  else
  {
    gtk_file_selection_set_filename (GTK_FILE_SELECTION(filebox), "*");
    gtk_widget_show(filebox);
  }
  
 
  /* Callbacks for File Box and Stuff */

  gtk_signal_connect (GTK_OBJECT (filebox), "delete_event",
                     (GtkSignalFunc) gtk_widget_destroy, 
                     GTK_OBJECT(filebox));
 
  /* Connect the ok_button to file_ok_sel function */
  gtk_signal_connect (GTK_OBJECT (GTK_FILE_SELECTION (filebox)->ok_button),
                     "clicked", (GtkSignalFunc) file_ok_sel, filebox );
         
  /* Connect the cancel_button to destroy the widget */
  gtk_signal_connect_object (GTK_OBJECT (GTK_FILE_SELECTION(filebox)->cancel_button),                                     
                            "clicked", (GtkSignalFunc) gtk_widget_destroy,                                    
                            GTK_OBJECT (filebox));

  /* Make main window */
  main_window_init();
                            
  

  /* Enter event loop */
  gtk_main();

  return 0;
  }

