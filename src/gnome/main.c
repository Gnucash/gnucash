/*-*-gnucash-c-*-****************************************************\
 * main.c -- main for xacc (X-Accountant)                           *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1998 Linas Vepstas                                 *
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
\********************************************************************/

#include <config.h>
#include <gnome.h>
#include <gnucash.h>

#include <stdlib.h>
#include <assert.h>

#include "config.h"
#include "main.h"
#include "FileIO.h"
#include "Group.h"
#include "util.h"
#include "MainWindow.h"
#include "messages.h"
#include "TransLog.h"

/** PROTOTYPES ******************************************************/

/** GLOBALS *********************************************************/

AccountGroup *topgroup = 0x0;
char        *helpPath = NULL;
GtkWidget   *toplevel;
GtkWidget   *filebox;
gint	     filebox_quit;
GtkWidget   *import_filebox;
char        *datafile = NULL;
GtkWidget   *app;

/* utility routine do deal with opening a new file.  cleans up the 
 * mess left behind with the old one.
 */
static void 
open_new_file (const char * newfile)
{
   int io_error;
   AccountGroup *newgrp; 

   if (!newfile) return;

  /* disable logging while we move over to the new set of accounts to
   * edit; the mass deletetion of accounts and transactions during
   * switchover is not something we want to keep in a journal.  */
  xaccLogDisable ();

  newgrp = xaccReadAccountGroup (newfile);   
 
  /* check for i/o error, put up appropriate error message */
  io_error = xaccGetFileIOError();
  SHOW_IO_ERR_MSG(io_error);
 
  if (newgrp) {
    if (datafile) free (datafile);
    datafile = strdup (newfile);

     xaccLogSetBase (newfile);
     if (topgroup) {
        xaccLogDisable ();
        xaccFreeAccountGroup (topgroup);
     }
    topgroup = newgrp;
  }

   xaccAccountGroupMarkSaved (topgroup);
   xaccLogEnable ();
}

/* serious hack alert -- the design here needs fixin, it completely fails to 
 * warn user to save any uncommited work before blowing them out of the water.
 * to fix this, should do something like
 *     if( xaccAccountGroupNotSaved (topgrp) ) {
 *       if( verifyBox(toplevel, FMB_SAVE_MSG) ) {
 *         file_ok_sel( ...);
 *         }
 *       }
 */

void
file_ok_sel (GtkWidget *w, GtkFileSelection *fs)
{
  char *newfile = gtk_file_selection_get_filename (GTK_FILE_SELECTION (fs));

  /* Check to see if this is a valid datafile */
  if ( newfile == NULL )
    return;

  if (filebox_quit)
    {
      gtk_signal_disconnect (GTK_OBJECT (filebox), filebox_quit);
      filebox_quit = 0;
    }
  gtk_widget_hide (filebox);

  oepn_new_file (newfile);
  main_window_init (topgroup);
}

void
import_ok_sel (GtkWidget *w, GtkFileSelection *fs)
{
  char *newfile = gtk_file_selection_get_filename (GTK_FILE_SELECTION (fs));
  int io_error;
  AccountGroup *newgrp;

  if (newfile) {
    gtk_widget_hide (import_filebox);

    /* Load the accounts from the users datafile */
    newgrp = xaccReadQIFAccountGroup (newfile);
    
    /* Check for i/o error, put up appropriate error message */
    io_error = xaccGetQIFIOError();
    if (io_error)
      {
        /* hack alert -- this should be a pop-up dialog, not a print statement */
	printf ("I/O Error: %d", io_error);
	return;
      }
    
    if( NULL == topgroup )
      {
	/* no topgroup exists */
	topgroup = xaccMallocAccountGroup();
      }

    /* Since Quicken will not export all accounts into one file, we
       must merge them in one by one */
    xaccConcatGroups (topgroup, newgrp);
    xaccMergeAccounts (topgroup);
    xaccConsolidateGrpTransactions (topgroup);
  }
}

void
gnucash_shutdown (GtkWidget *widget, gpointer *data)
{
  if ( xaccAccountGroupNotSaved(topgroup) )
  {
    GtkWidget *msgbox;
    msgbox = gnome_message_box_new ( FMB_SAVE_MSG,
                                     GNOME_MESSAGE_BOX_ERROR, 
                                     GNOME_STOCK_BUTTON_OK,
                                     GNOME_STOCK_BUTTON_CANCEL, NULL );
    gnome_dialog_button_connect (GNOME_DIALOG (msgbox), 0,
                                 GTK_SIGNAL_FUNC (file_cmd_save), 
                                 NULL);
    gnome_dialog_button_connect (GNOME_DIALOG (msgbox), 0,
                                 GTK_SIGNAL_FUNC (file_cmd_quit), 
                                 NULL);                                 
    gnome_dialog_button_connect (GNOME_DIALOG (msgbox), 1,
                                 GTK_SIGNAL_FUNC (file_cmd_quit), 
                                 NULL);                                                    
    gtk_widget_show ( msgbox );   
  }
  else
  {
    gtk_main_quit ();
  }

}

void
file_cmd_open (GtkWidget *widget, gpointer data)
{
    gtk_widget_show ( filebox );
}

void
file_cmd_import (GtkWidget *widget, gpointer data)
{
    gtk_widget_show (import_filebox);
}

void
file_cmd_save(GtkWidget *widget, gpointer data)
{
  /* hack alert -- Somehow make sure all in-progress edits get committed! */
  if (NULL == datafile) {
    fprintf(stderr, "Can't save file.  No open file\n");
    return;
  }
  xaccWriteAccountGroup(datafile, topgroup);
  xaccAccountGroupMarkSaved(topgroup);
}

void file_cmd_quit (GtkWidget *widget, gpointer data)
{
  //gnucash_shutdown(NULL, NULL);
  gtk_main_quit();
}

/* hack alert -- what is the difference between this ui_open
 * and the file_ok_sel() routine?  methinks the two should be merged,
 */

int
gnucash_ui_open_file(const char name[]) {

  if( name == NULL ) return 0;

  open_new_file (name);
    
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
  
  /* Create main window */
  main_window_init(topgroup);

  return 0;
}

int
gnucash_ui_select_file() {

  /* Connect the cancel_button to kill the app */
  filebox_quit =
    gtk_signal_connect_object
    (GTK_OBJECT (GTK_FILE_SELECTION (filebox)->cancel_button),
     "clicked", (GtkSignalFunc)gtk_exit, NULL );
  
  gtk_widget_show ( filebox );

  return 0;
}

int
gnucash_lowlev_app_init()
{
  int fake_argc = 1;
  char *fake_argv[] = {"gnucash"};

  gnome_init("GnuCash", NULL, fake_argc, fake_argv, 0, NULL);  
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
  /* Callbacks for File Box and Stuff */
  
  gtk_signal_connect (GTK_OBJECT (filebox), "delete_event",
                      (GtkSignalFunc) gtk_widget_destroy, 
                      GTK_OBJECT(filebox));
  
  /* Connect the ok_button to file_ok_sel function */
  gtk_signal_connect (GTK_OBJECT (GTK_FILE_SELECTION (filebox)->ok_button),
                      "clicked", (GtkSignalFunc) file_ok_sel, filebox );
  
  /* Filebox widget for importing QIF files. */
  import_filebox = gtk_file_selection_new ( "Import..." );

  gtk_signal_connect (GTK_OBJECT (import_filebox), "delete_event",
                      (GtkSignalFunc) gtk_widget_destroy, 
                      GTK_OBJECT(import_filebox));
  
  /* Connect the ok_button to file_ok_sel function */
  gtk_signal_connect
    (GTK_OBJECT (GTK_FILE_SELECTION (import_filebox)->ok_button),
     "clicked", (GtkSignalFunc) import_ok_sel, import_filebox );

  return 0;
}

/********************************************************************\
 * gnome_main                                                       *
 *  called after the guile engine is up and running                 *
 *  sets up the top level widget                                    * 
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
gnucash_lowlev_app_main()
{  
  /* Enter gnome event loop */
  gtk_main();

  return 0;
}

/********************************************************************\
 * main                                                             *
 *  the entry point for the program                                 *
 *  call gnucash_main to startup guile and then run gnome_main      *
 *  and calls the mainWindow() function which creates the main      * 
 *  window.                                                         * 
 *                                                                  * 
 * Args:   argc, the number of command line arguments, and argv,    * 
 *         the array of command line args                           * 
\********************************************************************/
int 
main( int argc, char *argv[] )
{
  gnucash_main(argc, argv);
  /* This will never return. */

  assert(0);  /* Just to be anal */
  return 0;   /* Just to shut gcc up. */
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
