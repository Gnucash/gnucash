/********************************************************************\
 * FileBox.c -- the file dialog box                                 *
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

#include <Xm/Xm.h>
#include <Xm/FileSB.h>

#include "config.h"

#include "FileBox.h"
#include "main.h"
#include "util.h"

/** GLOBALS *********************************************************/
extern XtAppContext app;
Boolean done=True;

/** PROTOTYPES ******************************************************/
void fileBoxCB( Widget mw, XtPointer cd, XtPointer cb );

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
fileBox( Widget parent, int type, char * filter)
  {
  Widget   dialog=0;
  char*    fileName = NULL;
  XmString filterpattern, dialogname=NULL;

  if( !done )
    return NULL;                   /* Don't open if there already is
				    * an instance of fileBox */
  if (!filter) {
    filterpattern = XmStringCreateSimple( "*.xac" );
  } else {
    filterpattern = XmStringCreateSimple( filter );
  }
  
  done = False;
  
  ENTER("fileBox");

  /* setBusyCursor( parent ); */
  
  if( type == OPEN )
    {
    dialogname = XmStringCreateSimple( OPEN_STR );
    dialog = XmCreateFileSelectionDialog( parent, OPEN_STR, NULL, 0 );
    }
  else if( type == SAVE )
    {
    dialogname = XmStringCreateSimple( SAVE_STR );
    dialog = XmCreateFileSelectionDialog( parent, SAVE_STR, NULL, 0 );
    }
  
  XtVaSetValues( dialog,
		 XmNpattern,      filterpattern,
		 XmNdialogTitle,  dialogname,
		 XmNminWidth,     350,
		 XmNminHeight,    350,
		 NULL );
  
  XtUnmanageChild( XmFileSelectionBoxGetChild(dialog,XmDIALOG_HELP_BUTTON) );
  
  /* Add the "ok" button callbacks... first save the name, then
   * destroy the dialog */
  XtAddCallback( dialog, XmNokCallback, 
		 fileBoxCB, (XtPointer)&fileName );
  XtAddCallback( dialog, XmNokCallback, 
		 destroyShellCB, (XtPointer)dialog );
  
  /* Add the cancel button callback */
  XtAddCallback( dialog, XmNcancelCallback, 
		 fileBoxCB, (XtPointer)NULL );
  XtAddCallback( dialog, XmNcancelCallback, 
		 destroyShellCB, (XtPointer)dialog );
  
  XmStringFree( filterpattern );
  
  XtManageChild( dialog );

  /* unsetBusyCursor( parent ); */
  /* while the user hasn't pushed "Ok", simulate XtMainLoop.*/
  while( !done || XtAppPending(app) )
    XtAppProcessEvent( app, XtIMAll );
  
  LEAVE("fileBox");
  return fileName;
  }

/********************************************************************\
 * fileBoxCB                                                        * 
 *   callback that saves the name of the file so that fileBox       * 
 *   can return                                                     * 
 *                                                                  * 
 * Args:   mw - the widget that called us                           * 
 *         cd - fileName                                            * 
 *         cb -                                                     * 
 * Return: none                                                     * 
 * Global: done    - whether fileBox should return                  * 
\********************************************************************/
void
fileBoxCB( Widget mw, XtPointer cd, XtPointer cb )
  {
  char **fileName = (char **)cd;
  ENTER("fileBoxCB");
  
  if( cd != NULL )
    {
    XmFileSelectionBoxCallbackStruct *cbs = 
    (XmFileSelectionBoxCallbackStruct *)cb;
    
    if( !XmStringGetLtoR(cbs->value,charset,fileName) )
      *fileName = NULL;
    }
  done = True;
  LEAVE("fileBoxCB");
  }
