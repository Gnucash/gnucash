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

#include "config.h"

#if HAVE_XPM
#  include <X11/xpm.h>
#endif

#include "main.h"
#include "util.h"

#if USE_HTMLW
#include "HTML.h"
#endif 

#if USE_XMHTML
#include "XmHTML.h"
#endif 


/********************************************************************\
 *     HTML History functions                                       * 
\********************************************************************/
typedef struct _HTMLHistory {
  struct _HTMLHistory *next;
  struct _HTMLHistory *last;
  char   *htmlfile;
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
    temp = (*history)->next;
    while( temp ) 
      {
      (*history)->next = temp->next;
      _free(temp->htmlfile);
      _free(temp);

      temp = (*history)->next;
      }
    
    /* Add new node to history: */
    temp = (HTMLHistory *)_malloc(sizeof(HTMLHistory));
    temp->htmlfile = (char *)_malloc((strlen(htmlfile)+1)*sizeof(char));
    strcpy (temp->htmlfile,htmlfile);
    temp->next = NULL;
    temp->last = (*history);
    (*history)->next = temp;
    (*history) = temp;
    
    return FALSE;
    }
  else
    {
    /* This must be the first node in the history... */
    (*history) = (HTMLHistory *)_malloc(sizeof(HTMLHistory));
    (*history)->htmlfile = (char *)_malloc((strlen(htmlfile)+1)*sizeof(char));
    strcpy ((*history)->htmlfile,htmlfile);
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
  if( (*history) != NULL ) {
    if( (*history)->next != NULL ) { 
      (*history) = (*history)->next; 
       }
    return (*history)->htmlfile;
    }
  else
    return NULL;
  }

/* Move back in history, and return current htmlfile */
char *
historyBack( HTMLHistory **history )
  {
  if( (*history) != NULL ) {
    if( (*history)->last != NULL ) {
      (*history) = (*history)->last;
      }
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

#if USE_XMHTML
void xaccJumpToLabel (Widget mw, char *jumpfile);
XmImageInfo * htmlReadImageProc (Widget w, String file);
#endif 

#if HAVE_XPM
struct _image_info {
  int width;
  int height;
  int num_colors;
  Pixmap pixmap;
};

typedef struct _image_info ImageInfo;
  
ImageInfo *htmlResolveImage( Widget wm, char *file, int nl );
#endif

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
         buttonform,
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
				   XmNwidth,        500,
				   XmNheight,       400,
                                   XmNtransient,    FALSE,  /* allow window to be repositioned */

				   XmNminWidth,     500,
				   XmNminHeight,    200,
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
#if USE_HTMLW
			       htmlWidgetClass,         controlform,
#if HAVE_XPM
			       WbNresolveImageFunction, htmlResolveImage,
#endif
			       WbNdelayImageLoads,      False,
#endif 

#if USE_XMHTML
                               xmHTMLWidgetClass,       controlform,
                               XmNanchorButtons,        False, 
                               XmNimageProc,            htmlReadImageProc,
#endif 
			       XmNtopAttachment,        XmATTACH_FORM,
			       XmNbottomAttachment,     XmATTACH_FORM,
			       XmNleftAttachment,       XmATTACH_FORM,
			       XmNrightAttachment,      XmATTACH_FORM,
			       XmNwidth,                500,
			       XmNheight,               400,
			       NULL );
    
#if USE_HTMLW
    XtAddCallback( helpwidget, WbNanchorCallback, helpAnchorCB, NULL );
#endif 
#if USE_XMHTML
    XtAddCallback( helpwidget, XmNactivateCallback, helpAnchorCB, NULL );
#endif
    
    
    /** ACTIONFORM ********************************************
     * Create a Form buttonform for action area of dialog box */
    buttonform = XtVaCreateWidget( "buttonform", 
				   xmFormWidgetClass, pane,
				   XmNfractionBase,   7,
				   NULL );
    position=1;
    
    /* The "Back" button */
    widget = XtVaCreateManagedWidget( "Back",
				      xmPushButtonWidgetClass, buttonform,
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
				      xmPushButtonWidgetClass, buttonform,
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
				      xmPushButtonWidgetClass, buttonform,
				      XmNtopAttachment,      XmATTACH_FORM,
				      XmNbottomAttachment,   XmATTACH_FORM,
				      XmNleftAttachment,     XmATTACH_POSITION,
				      XmNleftPosition,       position,
				      XmNrightAttachment,    XmATTACH_POSITION,
				      XmNrightPosition,      position+1,
				      XmNshowAsDefault,      True,
				      NULL );
    
    XtAddCallback( widget, XmNactivateCallback, destroyShellCB, dialog );
    
    XtManageChild( helpwidget );
    
    /* we have to load the page after the widget is realized, so
     * the pictures can be drawn  ?? but its not realized yet! */
#if USE_HTMLW
    XtVaSetValues( helpwidget, WbNtext, htmlRead(htmlfile), NULL );
#endif
#if USE_XMHTML
    xaccJumpToLabel( helpwidget, htmlfile );
#endif

    /* Fix action area of the pane to its current size, and not let it
     *  resize. */
    XtManageChild( buttonform );
    
    {
    Dimension h;
    XtVaGetValues( widget, XmNheight, &h, NULL );
    XtVaSetValues( buttonform, XmNpaneMaximum, h, XmNpaneMinimum, h, NULL );
    }
    
    XtManageChild( controlform );
    XtManageChild( pane );
    XtPopup( dialog, XtGrabNone );
    }
  /* if help window is already open, just load new page */
  else {
#if USE_HTMLW
    XtVaSetValues( helpwidget, WbNtext, htmlRead(htmlfile), NULL );
#endif
#if USE_XMHTML
    xaccJumpToLabel( helpwidget, htmlfile );
#endif
    }
  
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
#if USE_HTMLW
    XtVaSetValues( helpwidget, WbNtext, htmlRead(file), NULL );
#endif
#if USE_XMHTML
    xaccJumpToLabel( helpwidget, file );
#endif
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
#if USE_HTMLW
    XtVaSetValues( helpwidget, WbNtext, htmlRead(file), NULL );
#endif
#if USE_XMHTML
    xaccJumpToLabel( helpwidget, file );
#endif
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
#if USE_HTMLW
  WbAnchorCallbackData *acbs = (WbAnchorCallbackData *)cb;
  fprintf(stderr,"%d %s\n",acbs->element_id, acbs->text);

  
  if( historyInsert(&helpHistory,acbs->href) )
    {ERROR();}     /* CB shouldn't be called if there is not history!!! */
  else {
    XtVaSetValues( mw, WbNtext, htmlRead(acbs->href), NULL );
    }
#endif

#if USE_XMHTML
   XmHTMLAnchorCallbackStruct *acbs = (XmHTMLAnchorCallbackStruct *) cb;

   if(acbs->reason != XmCR_ACTIVATE) return;

   switch(acbs->url_type) {

      /* a named anchor on a page that is already displayed */
      case ANCHOR_JUMP: {
         XmHTMLAnchorScrollToName(mw, acbs->href);
      }
      break;

      /* a local file with a possible jump to label */
      case ANCHOR_FILE_LOCAL: {
         historyInsert(&helpHistory, acbs->href);
         xaccJumpToLabel (mw, acbs->href);
      }
      break;

      /*  other types are unsupported, but it would be fun if they were ... */
      case ANCHOR_FTP:
         fprintf(stderr, "Error: this help window doesn't support ftp: %s\n", acbs->href);
         break;
      case ANCHOR_HTTP:
         fprintf(stderr, "Error: this help window doesn't support http: %s\n", acbs->href);
         break;
      case ANCHOR_MAILTO:
         fprintf(stderr, "Error: this help window doesn't support email: %s\n", acbs->href);
         break;
      case ANCHOR_UNKNOWN:
      default:
         fprintf(stderr, "Error: don't know this type of url: %s\n", acbs->href);
         break;
   }

#endif
  }

/********************************************************************\
 *     utility functions...                                         * 
\********************************************************************/

#if USE_XMHTML
void
xaccJumpToLabel (Widget mw, char *jumpfile)
{
   char *label, *file, *text;

   file = strdup (jumpfile);

   /*  see if this anchor contains a jump */
   label = strchr (jumpfile, '#');
   if (label) {
      file [label - jumpfile] = 0x0;  /* truncate # from name */
      label = strdup (label);
   }

   text = htmlRead (file);
   XmHTMLTextSetString(mw, text);
   if (label) {
      XmHTMLAnchorScrollToName(mw, label);
   } else {
      XmHTMLTextScrollToLine(mw, 0);
   }
   free (file);
   if (label) free (label);
}
#endif

/********************************************************************\
 *     HTML functions...                                            * 
\********************************************************************/

/********************************************************************\
 * htmlRead                                                         * 
 *                                                                  * 
 * Args:   file - the name of the html file to read                 * 
 * Return: none                                                     * 
 * Global: helpPath - the path to the help files                    * 
\********************************************************************/
char *
htmlRead( char *file )
  {
  char *buf=NULL;
  char  filename[BUFSIZE];
  int   size=0;
  int   fd;

  /* construct absolute path */
  strcpy (filename, helpPath);
  strcat (filename, "/");
  strcat (filename, file);

  /* Open file: */
  fd = open( filename, O_RDONLY );
  if( fd == -1 )
    {
    ERROR();
    fprintf (stderr, "file was %s \n", file);
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
 * htmlReadImageProc                                                * 
 *                                                                  * 
 * Args:   file - the name of the html file to read                 * 
 * Return: none                                                     * 
 * Global: helpPath - the path to the help files                    * 
\********************************************************************/
#if USE_XMHTML
XmImageInfo *
htmlReadImageProc (Widget w, String file) 
  {
  char  filename[BUFSIZE];
  XmImageInfo *retval;

  /* construct absolute path -- twiddle teh relative path we recieved */
  strcpy (filename, helpPath);
  strcat (filename, "/");
  strcat (filename, file);

  /* use the default proc for the hard work */
  retval = XmHTMLImageDefaultProc(w, filename, NULL, 0);
  return retval;
}
#endif /* USE_XMHTML */

#if HAVE_XPM
/********************************************************************\
 * htmlResolveImage                                                 * 
 *                                                                  * 
 * Args:   mw   - the html widge                                    *
 *         file - the name of the html file to read                 * 
 *         nl   - ???                                               * 
 * Return: none                                                     * 
 * Global: helpPath - the path to the help files                    * 
\********************************************************************/
extern Widget toplevel;

ImageInfo *
htmlResolveImage( Widget mw, char *file, int nl )
  {
  ImageInfo *img = (ImageInfo *)malloc(sizeof(ImageInfo));
  XpmImage  xpm;
  XpmAttributes attr;
  int err;
  char filename[BUFSIZE];
  
  sprintf( (char *)&filename, "%s/%s", helpPath, file );
  
  /* initialize stuff: */
  memset( img, 0, sizeof(ImageInfo) );
  memset( &attr, 0, sizeof(XpmAttributes) );
  memset( &xpm, 0, sizeof(XpmImage) );
  
  err = XpmReadFileToXpmImage( filename, &xpm, NULL );
  img->width  = xpm.width;
  img->height = xpm.height;
  img->num_colors = xpm.ncolors;
  img->pixmap = None;
  
  err = XpmCreatePixmapFromXpmImage( XtDisplay(mw), XtWindow(mw), &xpm,
				     &(img->pixmap), NULL, NULL );
  if( err != 0 )
    ERROR();
  
  return img;
  }

#endif
