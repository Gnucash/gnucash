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
#include "gtkfilesel2.h"

/** GLOBALS *********************************************************/
#define CLOSED    1
#define DESTROYED 2
gint    done = CLOSED;

/** GLOBALS FOR FILEBOX FILTERS *************************************/
gchar *xac_suffs[] = {".xac", ".gnc", NULL};
gchar *qif_suffs[] = {".qif", NULL};
gchar *all_suffs[] = {"", NULL};

GtkFileSelection2FileType 
  xfiles = {"Gnucash files (*.xac; *.gnc)", xac_suffs},
  qfiles = {"QIF files (*.qif)", qif_suffs},
  allfiles = {"All files (*)", all_suffs};

GtkFileSelection2FileType 
  *gnucash_types[] = {&xfiles, &qfiles, &allfiles, NULL},
  *qif_types[] = {&qfiles, &allfiles, NULL};

/* This static indicates the debugging module that this .o belongs to.   */
static short module = MOD_GUI;

/** PROTOTYPES ******************************************************/
void fileBoxCB ( GtkWidget *mw, gpointer data );
void closeBoxCB( GtkWidget *mw, gpointer data );

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
fileBox(const char * title, const char * filter) 
{
  GtkWidget *fileBox;
  gchar     *fileName = NULL;

  if ( !done )
    return NULL;

  /* Set a default title if nothing was passed in */  
  if (title == NULL)
  {
    title = OPEN_STR;
  }

  done = 0;
                          
  ENTER("filebox");

  fileBox = gtk_file_selection2_new (title);

  /* hack alert -- this is hard coding the filter... BAD very BAD */
  gtk_file_selection2_show_file_types(GTK_FILE_SELECTION2(fileBox));
  gtk_file_selection2_set_file_types (GTK_FILE_SELECTION2(fileBox), gnucash_types);
  
  gtk_object_set_data(GTK_OBJECT(fileBox), "done", &done);

  /* Connect the dialog to the destroy even */  
  gtk_signal_connect (GTK_OBJECT (fileBox), "destroy",
                      (GtkSignalFunc) closeBoxCB, fileBox);
                             
  /* Connect the ok_button to file_ok_sel function */
  gtk_signal_connect (GTK_OBJECT (GTK_FILE_SELECTION2(fileBox)->ok_button),
                      "clicked", (GtkSignalFunc) fileBoxCB, fileBox );
         
  /* Connect the cancel_button to destroy the widget */
  gtk_signal_connect (GTK_OBJECT (GTK_FILE_SELECTION2(fileBox)->cancel_button),
                      "clicked", (GtkSignalFunc) closeBoxCB,
                       fileBox);

  gtk_widget_show(GTK_WIDGET(fileBox));

  while ( !done )
  {
    gtk_main_iteration(); 
  }

  LEAVE("fileBox");

  if ( done == CLOSED )
  {
    fileName = gtk_file_selection2_get_filename(GTK_FILE_SELECTION2(fileBox));  
  }
  if ( done == DESTROYED )
  {
    fileName = NULL;
  }

  /* Check validity of file here */

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
fileBoxCB( GtkWidget *mw, gpointer data )
{
  gint      *done;
  GtkWidget *fileBox;
    
  ENTER("fileBoxCB");

  fileBox = data;
  done    = gtk_object_get_data(GTK_OBJECT(fileBox), "done");
  *done   = CLOSED;

  gtk_widget_hide(fileBox);

  LEAVE("fileBoxCB");
}

void
closeBoxCB( GtkWidget *mw, gpointer data )
{
  gint      *done;
  GtkWidget *fileBox;
    
  ENTER("fileBoxCB");

  fileBox = data;
  done    = gtk_object_get_data(GTK_OBJECT(fileBox), "done");
  *done   = DESTROYED;

  gtk_widget_destroy(fileBox);

  LEAVE("fileBoxCB");
}

/* ======================== END OF FILE ======================== */

