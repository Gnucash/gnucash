/********************************************************************\
 * util.c -- utility functions that are used everywhere else for    *
 *           xacc (X-Accountant)                                    *
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

#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/cursorfont.h>
#include <Xm/Xm.h>
#include <Xm/Text.h>
#include <Xm/PanedW.h>
#include <Xm/Form.h>
#include <Xm/PushB.h>
#include <Xm/DialogS.h>
#include <Xm/RowColumn.h>
#include <Xm/MessageB.h>
#include <Xbae/Matrix.h>

#include "config.h"
#include "main.h"
#include "util.h"
#include "xtutil.h"

/** GLOBALS *********************************************************/
extern XtAppContext app;
extern int realized;


/********************************************************************\
 * dateCB -- ensures the data the user enters in the date field     * 
 *   is in a valid format.                                          * 
 *                                                                  * 
 * Args:   mw - the widget that called us                           * 
 *         cd -                                                     *
 *         cb - the callback struct                                 * 
 * Return: none                                                     * 
\********************************************************************/
void
dateCB( Widget mw, XtPointer cd, XtPointer cb )
  {
  XmTextVerifyCallbackStruct *cbs = (XmTextVerifyCallbackStruct *)cb;
  char input;

  /* TODO: ??? add support for date field accelerator keys!! */
  if( cbs->text->ptr != NULL )
    {
    input = (cbs->text->ptr)[0];
  
    switch( input )
      {
      case '/':
        /* Make sure that there is at most two '/' */
        {
        String str = XmTextGetString(mw);
        int i,count=0;
        
        for( i=0; str[i] != '\0'; i++ )
          if( str[i] == '/' )
            count++;
        if( count >= 2 )
          cbs->doit = False;
        }
        break;

      case 0x0:
        /* if delete key (for example) is hit, then input string */
        /* will be an empty string. In such a case, allow the input */
        cbs->doit = True;
        break;

      default:
        /* only accept the input if it is a number */
        cbs->doit = isNum(input);
      }
    }
  }

/********************************************************************\
 * amountCB -- ensures the data entered in the amount field is in   * 
 *   a valid format.                                                * 
 *                                                                  * 
 * Args:   mw - the widget that called us                           * 
 *         cd -                                                     *
 *         cb - the callback struct                                 * 
 * Return: none                                                     * 
\********************************************************************/
void
amountCB( Widget mw, XtPointer cd, XtPointer cb )
  {
  XmTextVerifyCallbackStruct *cbs = (XmTextVerifyCallbackStruct *)cb;
  char input;
  
  if( cbs->text->ptr != NULL )
    {
    input = (cbs->text->ptr)[0];
  
    switch( input )
      {
      case '.':
        /* Make sure that there is only one '.' */
      {
      String str = XmTextGetString(mw);
      int i,count=0;
	
      for( i=0; str[i] != '\0'; i++ )
        if( str[i] == '.' )
          count++;
      if( count >= 1 )
        cbs->doit = False;
      }
      break;
      default:
        /* only accept the input if it is a number */
        cbs->doit = isNum(input);
      }
    }
  }

/********************************************************************\
 * noeditCB                                                         * 
 *   makes an Xbae matrix non-editable                              * 
 *                                                                  * 
 * Args:   mw - the widget that called us                           * 
 *         cd -                                                     * 
 *         cb -                                                     * 
 * Return: none                                                     * 
\********************************************************************/
void
noeditCB( Widget mw, XtPointer cd, XtPointer cb )
  {
  XbaeMatrixEnterCellCallbackStruct *cbs = 
      (XbaeMatrixEnterCellCallbackStruct * )cb;
  
  cbs->doit = False;
  }

/********************************************************************\
 * destroyShellCB                                                   * 
 *   a callback to destroy a widget (cd, not w, because we want to  * 
 *   destroy a window, not a button!)                               * 
 *                                                                  * 
 * Args:   mw - the widget that called us                           * 
 *         cd - the widget to destroy                               * 
 *         cb -                                                     * 
 * Return: none                                                     * 
\********************************************************************/
void 
destroyShellCB( Widget w, XtPointer cd, XtPointer cb )
  {
  Widget window = (Widget)cd;
  
  XtDestroyWidget(window);
  }

/********************************************************************\
 * setBusyCursor                                                    * 
 *   sets the cursor to the busy watch                              * 
 *                                                                  * 
 * Args:   w - the widget over which to make cursor busy            * 
 * Return: none                                                     * 
\********************************************************************/
void 
setBusyCursor( Widget w )
  {
  if( realized )
    {
    static Cursor watch = 0;
    
    if( watch == 0 )
      watch = XCreateFontCursor(XtDisplay(w),XC_watch);
    
    XDefineCursor(XtDisplay(w),XtWindow(w),watch);
    XmUpdateDisplay(w);
    }
  }

/********************************************************************\
 * unsetBusyCursor                                                  * 
 *   sets the cursor to the default cursor                          * 
 *                                                                  * 
 * Args:   w - the widget over which to make cursor normal          * 
 * Return: none                                                     * 
\********************************************************************/
void 
unsetBusyCursor( Widget w )
  {
  if( realized )
    {
    XUndefineCursor(XtDisplay(w),XtWindow(w));
    XmUpdateDisplay(w);
    }
  }

/********************************************************************\
 **************** VERIFYBOX STUFF ***********************************
\********************************************************************/
typedef struct _verifybox {
  Boolean done;
  Boolean answer;
} VerifyBox;

void verifyBoxCB( Widget mw, XtPointer cd, XtPointer cb );

/********************************************************************\
 * verifyBox                                                        *
 *   display a message, and asks the user to press "Ok" or "Cancel" *
 *                                                                  *
 * NOTE: This function does not return until the dialog is closed   *
 *                                                                  *
 * Args:   parent   - the parent widget                             *
 *         title    - the title of the window                       *
 *         text     - the message to display                        *
 * Return: none                                                     *
\********************************************************************/
Boolean
verifyBox( Widget parent, char *text )
  {
  Widget    dialog,msgbox;
  /* XmString  message     = XmStringCreateSimple(text); */
  XmString  message     = XmStringCreateLtoR( text, charset );
  XmString  yes,no;
  VerifyBox verifyData;
  
  verifyData.done   = False;
  verifyData.answer = False;
  
  setBusyCursor( parent );
  
  /* Create the dialog box... XmNdeleteResponse is set to
   * XmDESTROY so the dialog's memory is freed when it is closed */
  dialog = XtVaCreatePopupShell( "dialog", 
                                 xmDialogShellWidgetClass,	parent,
                                 XmNdialogStyle,    XmDIALOG_APPLICATION_MODAL,
                                 XmNtitle,          "",
                                 XmNwidth,          300,
                                 XmNdeleteResponse, XmDESTROY,
                                 NULL );
  
  yes = XmStringCreateSimple(YES_STR);
  no  = XmStringCreateSimple(NO_STR);
  
  /* Create a messagebox.... has message and "Ok","Cancel" buttons */
  msgbox = 
    XtVaCreateManagedWidget( "dialog", 
                             xmMessageBoxWidgetClass, dialog,
                             XmNdialogType,       XmDIALOG_QUESTION,
                             XmNdefaultButtonType,XmDIALOG_CANCEL_BUTTON,
                             XmNmessageString,    message,
                             XmNcancelLabelString,no,
                             XmNokLabelString,    yes,
                             NULL );
  
  /* Get rid of the "Help" Button!! */
  XtUnmanageChild( XmMessageBoxGetChild(msgbox, XmDIALOG_HELP_BUTTON) );
  
  /* We can make verifyData an automatic variable, and pass it by 
   * reference because this function doesn't return until the
   * dialog is destroyed, which means that the callbacks are done */
  XtAddCallback( msgbox, XmNokCallback,     verifyBoxCB, &verifyData );
  XtAddCallback( msgbox, XmNcancelCallback, verifyBoxCB, &verifyData );
  
  XtPopup( dialog, XtGrabNone ); 
  
  unsetBusyCursor( parent );
  
  /* while the user hasn't pushed "Ok" or "Cancel", simulate XtMainLoop.
   * When verifyData.done changes from False, it means the user has 
   * pressed a button.  Don't break loop until XtAppPending() also
   * returns False to assure widget destruction. */
  while( !(verifyData.done) || XtAppPending(app) )
    XtAppProcessEvent( app, XtIMAll );
  
  XmStringFree(message);
  XmStringFree(yes);
  XmStringFree(no);
  
  return verifyData.answer;
  }

/********************************************************************\
 * verifyBoxCB                                                      * 
 *                                                                  * 
 * Args:   mw - the widget that called us                           * 
 *         cd - verifyData                                          * 
 *         cb -                                                     * 
 * Return: none                                                     * 
\********************************************************************/
void
verifyBoxCB( Widget mw, XtPointer cd, XtPointer cb )
  {
  XmAnyCallbackStruct *cbs = (XmAnyCallbackStruct *)cb;
  VerifyBox    *verifyData = (VerifyBox *)cd;
  
  switch( cbs->reason )
    {
    case XmCR_OK:
      verifyData->answer = True;
      break;
    case XmCR_CANCEL:
    default:
      verifyData->answer = False;
    }
  
  verifyData->done = True;
  }

/********************************************************************\
*********************************************************************
\********************************************************************/

/********************************************************************\
 * errorBox                                                         * 
 *   displays an error dialog box                                   * 
 *                                                                  * 
 * Args:   w       - the parent widget                              * 
 *         message - the error message to display                   * 
 * Return: none                                                     * 
\********************************************************************/
void 
errorBox( Widget parent, char *message )
  {
  Widget   dialog;

  if( message != NULL )
    {
    XmString warning_msg,
             dialogname;
    
    setBusyCursor( parent );
    
    /* Create a warning dialog */
    dialog = XmCreateWarningDialog( parent, "warning", NULL, 0 );
    
    /* Create the warning XmString */
    warning_msg = XmStringCreateLtoR( message, charset );
    dialogname = XmStringCreateSimple( WARN_STR );
    XtVaSetValues( dialog,
		   XmNdialogTitle,     dialogname,
		   XmNmessageString,   warning_msg,
		   NULL );
    
    /* Get rid of the "Help" and "Cancel" buttons that would normally 
     * be in the warning dialog dialog by unmanaging them */
    XtUnmanageChild( XmMessageBoxGetChild(dialog, XmDIALOG_HELP_BUTTON) );
    XtUnmanageChild( XmMessageBoxGetChild(dialog, XmDIALOG_CANCEL_BUTTON) );
    
    /* Register the callback for the "Ok" button */
    XtAddCallback( dialog, XmNokCallback, destroyShellCB, dialog );

    /* Free up the allocated XmStrings */
    XmStringFree( warning_msg );
    XmStringFree( dialogname );
    
    XtManageChild( dialog );

    unsetBusyCursor( parent );
    }
  }

/************************* END OF FILE ******************************\
\********************************************************************/
