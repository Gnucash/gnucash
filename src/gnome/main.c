/*-*-gnucash-c-*-****************************************************\
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
#include <gnucash.h>

#include <stdlib.h>
#include <assert.h>
#include <guile/gh.h> /* This will go away once "datafile" is handled better. */

#include "config.h"
#include "main.h"
#include "FileIO.h"
#include "Group.h"
#include "util.h"
#include "MainWindow.h"
#include "messages.h"

/** PROTOTYPES ******************************************************/

/** GLOBALS *********************************************************/

char        *helpPath = NULL;
GtkWidget   *toplevel;
GtkWidget   *filebox;
gint	     filebox_quit;
GtkWidget   *import_filebox;
char        *datafile = NULL;
GtkWidget   *app;


/* The names of the different types of accounts.  For resource
 * specification. Must match the enums in Account.h */

gchar *accRes[] ={
  "bank",
  "cash",
  "asset",
  "credit",
  "liability",
  "stock",
  "mutual",
  "income",
  "expense",
  "equity",
  "checking",
  "savings",
  "creditline",
};

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

  datafile = newfile;
  topgroup = xaccReadAccountGroup (datafile);   
  gtk_widget_hide (filebox);
 
  if( NULL == topgroup )
    {
      /* Load the accounts data from datafile*/
      topgroup = xaccMallocAccountGroup(); 
    } 
  
  xaccAccountGroupMarkSaved (topgroup);
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

#if 0
static void
foreach_split_in_group(AccountGroup *g, void (*f)(Split *)) {
  const int num_accts = xaccGroupGetNumAccounts(g);
  Account **acc_list = (Account **) _malloc((num_accts  + 1) *
                                            sizeof(Account *));
  Account *acct;
  int i, pos;
  Split **splits;
  Split **split_cursor;
    
  for(i = 0, pos = 0; i < num_accts; i++) {
    acct = xaccGetAccountFromID(g, i);
    if(acct) {
      acc_list[pos++] = acct;
    }
  }
  acc_list[pos] = NULL;
  
  splits = accListGetSortedSplits(acc_list);
  
  split_cursor = splits;
  while(*split_cursor) {
    f(*split_cursor);
    split_cursor++;
  }
  _free(splits);
  _free(acc_list);
}
#endif


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
static int
gnome_main(int argc, char *argv[])
{
  SCM datafile_scm = gh_lookup("gnucash:datafile-tmp_");
  if(datafile_scm != SCM_BOOL_F)
  {
    datafile = gh_scm2newstr(datafile_scm, NULL);
  }

  /* argc and argv have been cleared by guile at this point */
  gnome_init("GnuCash", NULL, argc, argv, 0, &argc);
  
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
  
  /* read in the filename (should be the first arg after all
   * the X11 stuff */
  if( datafile != NULL ) {
    
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
    /* Create main window */
    xaccAccountGroupMarkSaved(topgroup);
    main_window_init(topgroup);
  } else {
    /* Filebox code here */
    /* Connect the cancel_button to also kill the app */
    filebox_quit =
      gtk_signal_connect_object
	(GTK_OBJECT (GTK_FILE_SELECTION (filebox)->cancel_button),
	 "clicked", (GtkSignalFunc)gtk_exit, NULL );

    gtk_widget_show ( filebox );
  }
  
  /* Enter event loop */
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
  gnucash_main(argc, argv, gnome_main);
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

/*
  Local Variables:
  tab-width: 2
  indent-tabs-mode: nil
  mode: c
  c-indentation-style: gnu
  eval: (c-set-offset 'substatement-open 0)
  End:
*/
