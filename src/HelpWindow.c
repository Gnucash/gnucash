/********************************************************************\
 * HelpWindow.c -- a help window for hypertext help.                *
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

#define USEDEBUG

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/PanedW.h>
#include <Xm/Frame.h>
#include <Xm/Form.h>
#include <Xm/PushB.h>
#include <X11/xpm.h>

#include "main.h"
#include "util.h"
#include "HTML.h"

/********************************************************************\
 *     HTML History functions                                       * 
\********************************************************************/
typedef struct _HTMLHistory {
  char   *htmlfile;
  struct _HTMLHistory *last;
  struct _HTMLHistory *next;
} HTMLHistory;

/* insert an htmlfile into history.  Return TRUE if this
 * is the first element in the history.  If not last element
 * in history, all next pages are deleted */
int
historyInsert( HTMLHistory **history, char *htmlfile )
  {
  if( (*history) != NULL )
    {
    HTMLHistory *temp;
    
    /* delete all next pages: */
    while( (*history)->next != NULL )
      {
      temp = (*history)->next;
      
      (*history)->next = temp->next;
      _free(temp->htmlfile);
      _free(temp);
      }
    
    /* Add new node to history: */
    temp = (HTMLHistory *)_malloc(sizeof(HTMLHistory));
    temp->htmlfile = (char *)_malloc((strlen(htmlfile)+1)*sizeof(char));
    sprintf(temp->htmlfile,"%s",htmlfile);
    temp->next = NULL;
    temp->last = (*history);
    (*history) = temp;
    
    return FALSE;
    }
  else
    {
    /* This must be the first node in the history... */
    (*history) = (HTMLHistory *)_malloc(sizeof(HTMLHistory));
    (*history)->htmlfile = (char *)_malloc((strlen(htmlfile)+1)*sizeof(char));
    sprintf((*history)->htmlfile,"%s",htmlfile);
    (*history)->last = NULL;
    (*history)->next = NULL;
    
    /* ...so return TRUE */
    return TRUE;
    }
  }

/* Move forward in history, and return current htmlfile */
char *
historyFwd( HTMLHistory **history )
  {
  if( (*history) != NULL )
    {
    if( (*history)->next != NULL )
      (*history) = (*history)->next;
    return (*history)->htmlfile;
    }
  else
    return NULL;
  }

/* Move back in history, and return current htmlfile */
char *
historyBack( HTMLHistory **history )
  {
  if( (*history) != NULL )
    {
    if( (*history)->last != NULL )
      (*history) = (*history)->last;
    return (*history)->htmlfile;
    }
  else
    return NULL;
  }

/* Return current htmlfile */
char *
historyCurrent( HTMLHistory **history )
  {
  if( (*history) != NULL )
    return (*history)->htmlfile;
  else
    return NULL;
  }

/* Remove all entries from history: */
void
historyClear( HTMLHistory **history )
  {
  /* move to beginning of history: */
  while( (*history)->last != NULL )
    (*history) = (*history)->last;
  
  /* delete all next pages: */
  while( (*history)->next != NULL )
    {
    HTMLHistory *temp = (*history)->next;
    
    (*history)->next = temp->next;
    _free(temp->htmlfile);
    _free(temp);
    }
  
  /* delete current page: */
  _free((*history)->htmlfile);
  _free(*history);
  (*history) = NULL;
  }


/********************************************************************\
 *     Help Window stuff...                                         * 
\********************************************************************/

/** GLOBALS *********************************************************/
Widget helpwidget;
HTMLHistory *helpHistory = NULL;

/** PROTOTYPES ******************************************************/
static void   closeHelpWin( Widget mw, XtPointer cd, XtPointer cb );
static void   helpBackCB( Widget mw, XtPointer cd, XtPointer cb );
static void   helpFwdCB( Widget mw, XtPointer cd, XtPointer cb );
static void   helpAnchorCB( Widget mw, XtPointer cd, XtPointer cb );

char      *htmlRead( char *file );
ImageInfo *htmlResolveImage( Widget wm, char *file, int nl );

/********************************************************************\
 * helpWindow                                                       * 
 *   opens up a help window, and displays html                      * 
 *                                                                  * 
 * Args:   parent   - the parent widget                             * 
 *         title    - the title of the window                       * 
 *         htmlfile - the file name of the help file to display     * 
 * Return: none                                                     * 
\********************************************************************/
void
helpWindow( Widget parent, char *title, char *htmlfile )
  {
  Widget dialog,
         pane,
         controlform,
         actionform,
         widget;
  int position=0;
  
  setBusyCursor( parent );
  
  /* if the help window isn't open, then open it... otherwise, load
   * new help page into current help window */
  if( historyInsert(&helpHistory,htmlfile) )
    {
    /* Create the dialog box... XmNdeleteResponse is set to
     * XmDESTROY so the dialog's memory is freed when it is closed */
    dialog = XtVaCreatePopupShell( "dialog", 
				   xmDialogShellWidgetClass, parent,
				   XmNdialogStyle,  XmDIALOG_APPLICATION_MODAL,
				   XmNtitle,        title,
				   XmNdeleteResponse, XmDESTROY,
				   XmNminWidth,     500,
				   XmNminHeight,    400,
				   NULL );
    
    XtAddCallback( dialog, XmNdestroyCallback, closeHelpWin, NULL );
    
    /* Create a PanedWindow Manager for the dialog box... the child 
     * of optiondialog the paned window is the parent of the two 
     * forms which comprise the two areas of the dialog box...
     * The sash is set to minimun size to make it invisible */
    pane = XtVaCreateWidget( "pane", 
			     xmPanedWindowWidgetClass, dialog,
			     XmNsashWidth,     1,
			     XmNsashHeight,    1,
			     XmNtraversalOn,   False,
			     NULL );
    
    /** CONTROLFORM ****************************************
     * Create a controlform for control area of dialog box */
    controlform = XtVaCreateWidget( "frame", 
				    xmFrameWidgetClass, pane,
				    NULL );
    
    helpwidget =
      XtVaCreateManagedWidget( "help",
			       htmlWidgetClass,         controlform,
			       WbNresolveImageFunction, htmlResolveImage,
			       WbNdelayImageLoads,      False,
			       XmNtopAttachment,        XmATTACH_FORM,
			       XmNbottomAttachment,     XmATTACH_FORM,
			       XmNleftAttachment,       XmATTACH_FORM,
			       XmNrightAttachment,      XmATTACH_FORM,
			       NULL );
    
    XtAddCallback( helpwidget, WbNanchorCallback, helpAnchorCB, NULL );
    
    XtManageChild( controlform );
    
    /* we have to load the page after the widget is realized, so
     * the pictures can be drawn */
    XtVaSetValues( helpwidget, WbNtext, htmlRead(htmlfile), NULL );
    
    /** ACTIONFORM ********************************************
     * Create a Form actionform for action area of dialog box */
    actionform = XtVaCreateWidget( "actionform", 
				   xmFormWidgetClass, pane,
				   XmNfractionBase,   7,
				   NULL );
    position=1;
    
    /* The "Back" button */
    widget = XtVaCreateManagedWidget( "Back",
				      xmPushButtonWidgetClass, actionform,
				      XmNtopAttachment,      XmATTACH_FORM,
				      XmNbottomAttachment,   XmATTACH_FORM,
				      XmNleftAttachment,     XmATTACH_POSITION,
				      XmNleftPosition,       position,
				      XmNrightAttachment,    XmATTACH_POSITION,
				      XmNrightPosition,      position+1,
				      XmNshowAsDefault,      True,
				      NULL );
    
    XtAddCallback( widget, XmNactivateCallback, helpBackCB, NULL );
    
    /* The "Forward" button */
    position +=2;
    widget = XtVaCreateManagedWidget( "Forward",
				      xmPushButtonWidgetClass, actionform,
				      XmNtopAttachment,      XmATTACH_FORM,
				      XmNbottomAttachment,   XmATTACH_FORM,
				      XmNleftAttachment,     XmATTACH_POSITION,
				      XmNleftPosition,       position,
				      XmNrightAttachment,    XmATTACH_POSITION,
				      XmNrightPosition,      position+1,
				      XmNshowAsDefault,      True,
				      NULL );
    
    XtAddCallback( widget, XmNactivateCallback, helpFwdCB, NULL );
    
    /* The "Close" button */
    position +=2;
    widget = XtVaCreateManagedWidget( "Close",
				      xmPushButtonWidgetClass, actionform,
				      XmNtopAttachment,      XmATTACH_FORM,
				      XmNbottomAttachment,   XmATTACH_FORM,
				      XmNleftAttachment,     XmATTACH_POSITION,
				      XmNleftPosition,       position,
				      XmNrightAttachment,    XmATTACH_POSITION,
				      XmNrightPosition,      position+1,
				      XmNshowAsDefault,      True,
				      NULL );
    
    XtAddCallback( widget, XmNactivateCallback, destroyShellCB, dialog );
    
    /* Fix action area of the pane to its current size, and not let it
     *  resize. */
    XtManageChild( actionform );
    
    {
    Dimension h;
    XtVaGetValues( widget, XmNheight, &h, NULL );
    XtVaSetValues( actionform, XmNpaneMaximum, h, XmNpaneMinimum, h, NULL );
    }
    
    XtManageChild( pane );
    XtPopup( dialog, XtGrabNone );
    }
  /* if help window is already open, just load new page */
  else
    XtVaSetValues( helpwidget, WbNtext, htmlRead(htmlfile), NULL );
  
  unsetBusyCursor( parent );
  }

/********************************************************************\
 *     callback functions...                                        * 
\********************************************************************/

/********************************************************************\
 * helpBackCB - called when user clicks "Back" button... shows last * 
 *   help page in history                                           * 
 *                                                                  * 
 * Args:   mw -                                                     * 
 *         cd -                                                     * 
 *         cb -                                                     * 
 * Return: none                                                     * 
\********************************************************************/
static void
helpBackCB( Widget mw, XtPointer cd, XtPointer cb )
  {
  char *file = historyBack(&helpHistory);
  if( file != NULL )
    {
    XtVaSetValues( helpwidget,
		   WbNtext, htmlRead(file),
		   NULL );
    }
  }

/********************************************************************\
 * helpFwdCB - called when user clicks "Forward" button... shows    * 
 *   next help page in the history                                  * 
 *                                                                  * 
 * Args:   mw -                                                     * 
 *         cd -                                                     * 
 *         cb -                                                     * 
 * Return: none                                                     * 
\********************************************************************/
static void
helpFwdCB( Widget mw, XtPointer cd, XtPointer cb )
  {
  char *file = historyFwd(&helpHistory);
  if( file != NULL )
    {
    XtVaSetValues( helpwidget,
		   WbNtext, htmlRead(file),
		   NULL );
    }
  }

/********************************************************************\
 * closeHelpWin - called when the help window is closed             * 
 *                                                                  * 
 * Args:   mw -                                                     * 
 *         cd -                                                     * 
 *         cb -                                                     * 
 * Return: none                                                     * 
\********************************************************************/
static void
closeHelpWin( Widget mw, XtPointer cd, XtPointer cb )
  {
  /* Delete the history: */
  historyClear(&helpHistory);
  }

/********************************************************************\
 * helpAnchorCB - called when user clicks on html anchor tag        * 
 *                                                                  * 
 * Args:   mw - the html widget that called us                      * 
 *         cd -                                                     * 
 *         cb - the anchor call-back struct                         * 
 * Return: none                                                     * 
\********************************************************************/
static void
helpAnchorCB( Widget mw, XtPointer cd, XtPointer cb )
  {
  WbAnchorCallbackData *acbs = (WbAnchorCallbackData *)cb;

  fprintf(stderr,"%d %s\n",acbs->element_id, acbs->text);
  
  if( historyInsert(&helpHistory,acbs->href) )
    {ERROR();}     /* CB shouldn't be called if there is not history!!! */
  else
    XtVaSetValues( mw, WbNtext, htmlRead(acbs->href), NULL );
  }

/********************************************************************\
 *     HTML functions...                                            * 
\********************************************************************/

/********************************************************************\
 * htmlRead                                                         * 
 *                                                                  * 
 * Args:   file - the name of the html file to read                 * 
 * Return: none                                                     * 
\********************************************************************/
char *
htmlRead( char *file )
  {
  char *buf=NULL;
  char  filename[BUFSIZE];
  int   size=0;
  int   fd;

  sprintf( (char *)&filename, "%s/%s", HELP_ROOT, file );
  /* Open file: */
  fd = open( filename, O_RDONLY );
  if( fd == -1 )
    {
    ERROR();
    return NULL;
    }
  
  /* Find size: */
  size = lseek( fd, 0, SEEK_END );
  lseek( fd, 0, SEEK_SET );

  /* Allocate memory... don't use _malloc(), `cause it is
   * freed elsewhere */
  buf = (char *)malloc(size*sizeof(char));

  /* read in file */
  if( read(fd,buf,size) == -1 )
    {
    free(buf);
    buf=NULL;
    }
  
  close(fd);
  return buf;
  }

/********************************************************************\
 * htmlResolveImage                                                 * 
 *                                                                  * 
 * Args:   mw   - the html widge                                    *
 *         file - the name of the html file to read                 * 
 *         nl   - ???                                               * 
 * Return: none                                                     * 
\********************************************************************/
extern Widget toplevel;

ImageInfo *
htmlResolveImage( Widget mw, char *file, int nl )
  {
  ImageInfo *img = (ImageInfo *)malloc(sizeof(ImageInfo));
  XpmImage  xpm;
  XpmAttributes attr;
  int err,i;
  char filename[BUFSIZE];
  
  sprintf( (char *)&filename, "%s/%s", HELP_ROOT, file );
  
  /* initialize stuff: */
  memset( img, 0, sizeof(ImageInfo) );
  memset( &attr, 0, sizeof(XpmAttributes) );
  memset( &xpm, 0, sizeof(XpmImage) );
  
  err = XpmReadFileToXpmImage( filename, &xpm, NULL );
  img->width  = xpm.width;
  img->height = xpm.height;
  img->num_colors = xpm.ncolors;
  img->image_data = None;
  
  err = XpmCreatePixmapFromXpmImage( XtDisplay(mw), XtWindow(mw), &xpm,
				     &(img->image), NULL, NULL );
  if( err != 0 )
    ERROR();
  
  return img;
  }
