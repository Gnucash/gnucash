/********************************************************************\
 * TextBox.c -- Text Box utility function                           *
 *           xacc (X-Accountant)                                    *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1997 Linas Vepstas                                 *
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
#include "messages.h"
#include "util.h"
#include "xtutil.h"

/********************************************************************\
 **************** TEXTBOX STUFF *************************************
\********************************************************************/

typedef struct _textbox {
  Widget dialog;
  Widget textfield;
  char   **pmodifytext;
} TextBox;

static void textBoxCB      ( Widget mw, XtPointer cd, XtPointer cb );

/********************************************************************\
\********************************************************************/

void
xaccDestroyTextBox (TextBox *tb)
{
   if (!tb) return;
   XtDestroyWidget (tb->dialog);
}

/********************************************************************\
 * textBox                                                          * 
 *   opens up a text box, and displays text                         * 
 *                                                                  * 
 * NOTE: This function does not return until the textBox is closed  * 
 *                                                                  * 
 * Args:   parent   - the parent widget                             * 
 *         title    - the title of the window                       * 
 *         text     - the initial text to display                   * 
 *         editable - can this text be edited by the user?          * 
 * Return: none                                                     * 
\********************************************************************/
TextBox *
textBox( Widget parent, char *title, char **pmodifytext, 
         XtCallbackProc userCB, XtPointer userData)
  {
  Boolean editable = True;
  Widget   dialog,
           pane,
           controlform,
           actionform,
           widget;
  Arg      args[20];
  TextBox  *textData = (TextBox *)_malloc(sizeof(TextBox));

  textData->pmodifytext = pmodifytext;
  
  setBusyCursor( parent );
  
  /* Create the dialog box... XmNdeleteResponse is set to
   * XmDESTROY so the dialog's memory is freed when it is closed */
  dialog = XtVaCreatePopupShell( "dialog", 
                                 xmDialogShellWidgetClass,	parent,
                                 XmNdialogStyle,    XmDIALOG_APPLICATION_MODAL,
                                 XmNtitle,          title,
                                 XmNdeleteResponse, XmDESTROY,
                                 XmNminWidth,       150,
                                 XmNminHeight,      200,
                                 XmNtransient,      FALSE,  /* allow window to be repositioned */
                                 NULL );

  textData->dialog = dialog;
  
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
  controlform = XtVaCreateWidget( "controlform", 
                                  xmFormWidgetClass, pane,
                                  NULL );
  
  /* Create a text widget as child of controlform */
  XtSetArg( args[0], XmNeditMode,  XmMULTI_LINE_EDIT );
  XtSetArg( args[1], XmNwordWrap,  True );
  XtSetArg( args[2], XmNrows,      12 );
  XtSetArg( args[3], XmNcolumns,   70 );
  XtSetArg( args[4], XmNeditable,  editable );
  XtSetArg( args[5], XmNscrollHorizontal,        False );
  XtSetArg( args[6], XmNtopAttachment,           XmATTACH_FORM );
  XtSetArg( args[7], XmNbottomAttachment,        XmATTACH_FORM );
  XtSetArg( args[8], XmNleftAttachment,          XmATTACH_FORM );
  XtSetArg( args[9], XmNrightAttachment,         XmATTACH_FORM );
  
  textData->textfield = 
    XmCreateScrolledText( controlform, "text", args, 10 );
  
  XtManageChild( textData->textfield );

  XmTextSetString( textData->textfield, *pmodifytext );
  
  XtManageChild( controlform );

  /** ACTIONFORM ********************************************
   * Create a Form actionform for action area of dialog box */
  {
  int fb = editable ? 5 : 3;
  actionform = XtVaCreateWidget( "actionform", 
                                 xmFormWidgetClass, pane,
                                 XmNfractionBase,   fb,
                                 NULL );
  }
  /* The OK button is anchored to the form, between divider 1 & 2
   * (in the fraction base) */
  widget = XtVaCreateManagedWidget( OK_STR,
                                    xmPushButtonWidgetClass, actionform,
                                    XmNtopAttachment,      XmATTACH_FORM,
                                    XmNbottomAttachment,   XmATTACH_FORM,
                                    XmNleftAttachment,     XmATTACH_POSITION,
                                    XmNleftPosition,       1,
                                    XmNrightAttachment,    XmATTACH_POSITION,
                                    XmNrightPosition,      2,
                                    XmNshowAsDefault,      True,
                                    NULL );
  
  /* Add callback function to Ok.. calls textBoxCB to save the text,
   * and destroyOptionDialog to kill option dialog box */
  XtAddCallback( widget, XmNactivateCallback, textBoxCB, textData );
  if (userCB) {
    XtAddCallback( widget, XmNactivateCallback, userCB, userData );
  }
  XtAddCallback( widget, XmNactivateCallback, destroyShellCB, dialog );
  
  if( editable )
    {
    /* If it is editable, provide a cancel button too! */
    widget = XtVaCreateManagedWidget( CANCEL_STR, 
                                      xmPushButtonWidgetClass, actionform,
                                      XmNtopAttachment,      XmATTACH_FORM,
                                      XmNbottomAttachment,   XmATTACH_FORM,
                                      XmNleftAttachment,     XmATTACH_POSITION,
                                      XmNleftPosition,       3,
                                      XmNrightAttachment,    XmATTACH_POSITION,
                                      XmNrightPosition,      4,
                                      XmNshowAsDefault,      True,
                                      NULL );
  
    /* Add callback function to Cancel.. calls destroyOptionDialog to
     * kill option dialog box */
    XtAddCallback( widget, XmNactivateCallback, destroyShellCB, dialog );
    if (userCB) {
      XtAddCallback( widget, XmNactivateCallback, userCB, userData );
      }
    }
  
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

  unsetBusyCursor( parent );
  
#ifdef __EMULATE_MAIN_LOOP
  /* while the user hasn't pushed "Ok", simulate XtMainLoop.
   * When textData->text changes from NULL, it means the user
   * has pressed "Ok".  Don't break loop until XtAppPending() 
   * also returns False to assure widget destruction. */

  /* DANGER WARNING:
   * While emulating the main loop may be OK for the verify
   * boxes as long as thier use is limited, it can become
   * *extremely dangerous* for more complex uses, such as text
   * boxes.  The basic problem is that this resembles
   * multi-threaded programming: while we are spinning in this
   * loop, all sorts of other things can be created and
   * destroyed.  In particular, the memory location which is
   * supposed to contain the results of the TextBox might get
   * deleted, resulting in a core dump when text box returns!
   * Owwwww!!.
   * 
   * In fact, the same can happen for the verify box as well,
   * although as long as the user respondsto the verify box
   * immediately, and desn't monkey around with anything else,
   * its safe.  Although I don't like it.  Some sort of grab
   * should probably be done, blocking the user from doing
   * anything else until they've ansered the question.
   */

  while( textData->newtext == NULL || XtAppPending(app) )
    XtAppProcessEvent( app, XtIMAll );
  return textData->newtext;

#endif /* __EMULATE_MAIN_LOOP */
  
  return textData;
  }

/********************************************************************\
 * textBoxCB                                                        * 
 *   callback that saves the data in the the buffer before textBox  * 
 *   can return                                                     * 
 *                                                                  * 
 * Args:   mw - the widget that called us                           * 
 *         cd - textData                                            * 
 *         cb -                                                     * 
 * Return: none                                                     * 
\********************************************************************/
static void
textBoxCB( Widget mw, XtPointer cd, XtPointer cb )
  {
  TextBox  *textData = (TextBox *)cd;
  
  *(textData->pmodifytext) = XmTextGetString( textData->textfield );
  }

/*********************** END OF FILE ********************************\
\********************************************************************/
