/********************************************************************\
 * FileBox.c -- the file dialog box                                 *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1998 Rob Browning <rlb@cs.utexas.edu>              *
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

#include "config.h"

#include "FileBox.h"
#include "messages.h"
#include "util.h"
#include "xtutil.h"

/** GLOBALS *********************************************************/
//extern XtAppContext app;
//Boolean done=True;

static GtkWidget *filebox = NULL;
static gint       filebox_quit;

/* This static indicates the debugging module that this .o belongs to.   */
static short module  MOD_GUI;

/** PROTOTYPES ******************************************************/
void fileBoxCB( GtkWidget mw, gpointer *data );
void closeBoxCB( GtkWidget mw, gpointer *data );

/********************************************************************\
 * fileBox                                                          * 
 *   pops up a file selection dialog (either a "Save As" or an      * 
 *   "Open"), and returns the name of the file the users selected.  * 
 *   (This function does not return until the user selects a file   * 
 *   or pressed "Cancel")                                           * 
 *                                                                  * 
 *   NOTE: fileBox is not re-entrant... if an instance of fileBox   * 
 *         already exists, the latter call will return NULL         * 
 *                                                                  * 
 * Args:   parent  - the parent of this window                      *
 *         type    - either OPEN or SAVE                            * 
 * Return: none                                                     * 
 * Global: app     - the XtAppContext                               * 
 *         done    - whether fileBox should return                  * 
\********************************************************************/
char *
fileBox( GtkWidget parent, char * title, char * filter)
  {
  GtkWidget   *dialog;
  char*    fileName = NULL;

//  if( !done )
//    return NULL;                   /* Don't open if there already is
//				    * an instance of fileBox */

//  done = False;
  
  ENTER("fileBox");

  fileName = gtk_file_selection_get_filename (GTK_FILE_SELECTION (dialog));

  gtk_widget_show(GTK_FILE_SELECTION(dialog));

  /* Check to see if this is a valid datafile */
  if ( fileName == NULL )
    return;

//  done = True;
 
  LEAVE("fileBox");
  return fileName;
  }

/********************************************************************\
 * fileBoxCB                                                        * 
 *   callback that saves the name of the file so that fileBox       * 
 *   can return                                                     * 
 *                                                                  * 
 * Args:   mw - the widget that called us                           * 
 *         data - fileName                                            * 
 *         cb -                                                     * 
 * Return: none                                                     * 
 * Global: done    - whether fileBox should return                  * 
\********************************************************************/
void
fileBoxCB( GtkWidget mw, gpointer *data )
  {
  char **fileName = (char **)data;
  ENTER("fileBoxCB");

  /* NEED CALLBACK CODE DARNIT */
  
//  done = True;
  LEAVE("fileBoxCB");
  }

void
closeBoxCB( GtkWidget mw, gpointer *data )
  {
  char **fileName = (char **)data;
  ENTER("closeBoxCB");
  *fileName = NULL;
//  done = True;
  LEAVE("closeBoxCB");
  }

/* ======================== END OF FILE ======================== */


#if 0

/* OLD_GNOME_CODE */

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

#endif
