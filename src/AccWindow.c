/********************************************************************\
 * AccWindow.c -- window for creating new accounts for xacc         *
 *                (X-Accountant)                                    *
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
#include <Xm/DialogS.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/RowColumn.h>
#include <Xm/LabelGP.h>
#include <Xm/ToggleB.h>
#include <Xm/PushB.h>
#include <Xm/Text.h>
#include <string.h>

#include "Account.h"
#include "Data.h"
#include "main.h"
#include "util.h"

/* NOTE: notes has to be at the beginning of the struct!  Order is 
 *       important */
typedef struct _accwindow {
  String notes;          /* The text from the "Notes" window        */
                         /* The account type buttons:               */
  Widget dialog;
  Widget bank;
  Widget cash;
  Widget asset;
  Widget credit;
  Widget liability;
  Widget portfolio;
  Widget mutual;
                         /* The text fields:                        */
  Widget name;           /* The account name text field             */
  Widget desc;           /* Account description text field          */
} AccWindow;

/* NOTE: notes has to be at the beginning of the struct!  Order is 
 *       important */
typedef struct _editaccwindow {
  String notes;          /* The text from the "Notes" window        */
                         /* The text fields:                        */
  Widget  name;          /* The account name text field             */
  Widget  desc;          /* Account description text field          */
  
  Account *account;      /* The account to edit                     */
} EditAccWindow;

/** GLOBALS *********************************************************/
extern Data   *data;
extern Widget toplevel;

/** PROTOTYPES ******************************************************/
void closeAccWindow( Widget mw, XtPointer cd, XtPointer cb );
void closeEditAccWindow( Widget mw, XtPointer cd, XtPointer cb );
void notesCB( Widget mw, XtPointer cd, XtPointer cb );
void createCB( Widget mw, XtPointer cd, XtPointer cb );
void editCB( Widget mw, XtPointer cd, XtPointer cb );

/********************************************************************\
 * accWindow                                                        *
 *   opens up a window to create a new account... the account is    * 
 *   actually created in the "create" callback                      * 
 *                                                                  * 
 * Args:   parent   - the parent of the window to be created        * 
 * Return: none                                                     *
\********************************************************************/
void 
accWindow( Widget parent )
  {
  Widget    dialog, form, frame, rc, widget, 
            label, buttonform;
  AccWindow *accData;
  
  setBusyCursor( parent );
  
  accData = (AccWindow *)_malloc(sizeof(AccWindow));
  accData->notes = XtNewString("");
  
  /* force the size of the dialog so it is not resizable */
  dialog = XtVaCreatePopupShell( "dialog", 
				 xmDialogShellWidgetClass, parent,
				 XmNtitle,            "Set Up Account",
				 XmNdeleteResponse,   XmDESTROY,
				 XmNwidth,     350,
				 XmNminWidth,  350,
				 XmNmaxWidth,  350,
				 XmNheight,    300,
				 XmNminHeight, 300,
				 XmNmaxHeight, 300,
                                 XmNresizable, FALSE,
                                 XmNallowShellResize, FALSE,
                                 /* XmNtransient, FALSE,  /* allow window to be repositioned */
				 NULL );
  
  XtAddCallback( dialog, XmNdestroyCallback, 
		 closeAccWindow, (XtPointer)accData );
  accData->dialog = dialog;
  
  /* The form to put everything in the dialog in */
  form = XtVaCreateWidget( "form", xmFormWidgetClass, dialog, 
                                 XmNrubberPositioning, TRUE,
                                 NULL );
  

  /******************************************************************\
   * The account type area                                          *
  \******************************************************************/
  
  /* Label tells the user what this area is */
  widget = XtVaCreateManagedWidget( "Account Type",
				    xmLabelGadgetClass, form,
				    XmNtopAttachment,   XmATTACH_FORM,
				    XmNtopOffset,       10,
				    XmNleftAttachment,  XmATTACH_FORM,
				    XmNleftOffset,      20,
				    NULL );
  
  /* Makes a nice looking frame around the radio buttons */
  frame = XtVaCreateManagedWidget( "frame", 
				   xmFrameWidgetClass, form,
				   XmNtopAttachment,   XmATTACH_WIDGET,
				   XmNtopWidget,       widget,
				   XmNleftAttachment,  XmATTACH_FORM,
				   XmNleftOffset,      20,
				   XmNrightAttachment, XmATTACH_FORM,
				   XmNrightOffset,     20,
				   NULL);
  
  /* A RowCol goes in the frame, to place the buttons */ 
  rc = XtVaCreateManagedWidget( "rowcol", 
				xmRowColumnWidgetClass, frame, 
				/*XmNentryAlignment,      XmALIGNMENT_CENTER,*/
				XmNorientation,         XmVERTICAL,
				XmNmarginHeight,        10,
				XmNmarginWidth,         10,
				/*XmNpacking,             XmPACK_TIGHT,*/
				XmNradioBehavior,       True,
				XmNnumColumns,          2,
				NULL );
  
  /* Create the buttons */
  accData->bank = 
    XtVaCreateManagedWidget( "Bank",
			     xmToggleButtonWidgetClass, rc,
			     XmNindicatorType,   XmONE_OF_MANY,
			     XmNset,             True,
			     NULL);
  
  accData->cash = 
    XtVaCreateManagedWidget( "Cash",
			     xmToggleButtonWidgetClass, rc,
			     XmNindicatorType,   XmONE_OF_MANY,
			     NULL);
  
  accData->asset =
    XtVaCreateManagedWidget( "Asset",
			     xmToggleButtonWidgetClass, rc,
			     XmNindicatorType,   XmONE_OF_MANY,
			     NULL);
  
  accData->credit = 
    XtVaCreateManagedWidget( "Credit Card",
			     xmToggleButtonWidgetClass, rc,
			     XmNindicatorType,   XmONE_OF_MANY,
			     NULL);
  
  accData->liability = 
    XtVaCreateManagedWidget( "Liability",
			     xmToggleButtonWidgetClass, rc,
			     XmNindicatorType,   XmONE_OF_MANY,
			     NULL);
  
  accData->portfolio = 
    XtVaCreateManagedWidget( "Portfolio",
			     xmToggleButtonWidgetClass, rc,
			     XmNindicatorType,   XmONE_OF_MANY,
			     NULL);
  /* Portfolio account not supported yet, so grey it out: */
/* hack alert 
  XtSetSensitive( accData->portfolio, False );
*/
  
  accData->mutual =
    XtVaCreateManagedWidget( "Mutual Fund",
			     xmToggleButtonWidgetClass, rc,
			     XmNindicatorType,   XmONE_OF_MANY,
			     NULL);
  /* Mutual Fund account not supported yet, so grey it out: */
/* hack alert
  XtSetSensitive( accData->mutual, False );
*/
  
  /******************************************************************\
   * Text fields....                                                *
  \******************************************************************/
  
  label = 
    XtVaCreateManagedWidget( "Account Name:",
			     xmLabelGadgetClass, form,
			     XmNtopAttachment,   XmATTACH_WIDGET,
			     XmNtopWidget,       frame,
			     XmNtopOffset,       10, 
			     XmNrightAttachment, XmATTACH_POSITION,
			     XmNrightPosition,   35,        /* 35% */
			     NULL );
  
  accData->name = 
    XtVaCreateManagedWidget( "text",
			     xmTextWidgetClass,  form,
			     XmNmaxLength,       40,
			     XmNcolumns,         25,
			     XmNtopAttachment,   XmATTACH_WIDGET,
			     XmNtopWidget,       frame,
			     XmNtopOffset,       10, 
			     XmNleftAttachment,  XmATTACH_POSITION,
			     XmNleftPosition,    35,        /* 35% */
			     NULL );
  
  label = 
    XtVaCreateManagedWidget( "Description:",
			     xmLabelGadgetClass, form,
			     XmNtopAttachment,   XmATTACH_WIDGET,
			     XmNtopWidget,       accData->name, 
			     XmNtopOffset,       10,
			     XmNrightAttachment, XmATTACH_POSITION,
			     XmNrightPosition,   35,        /* 35% */
			     NULL );
  
  accData->desc = 
    XtVaCreateManagedWidget( "text",
			     xmTextWidgetClass,  form,
			     XmNmaxLength,       40,
			     XmNcolumns,         30,
			     XmNtopAttachment,   XmATTACH_WIDGET,
			     XmNtopWidget,       accData->name,
			     XmNtopOffset,       10, 
			     XmNleftAttachment,  XmATTACH_POSITION,
			     XmNleftPosition,    35,        /* 35% */
			     NULL );
  
  /******************************************************************\
   * The buttons at the bottom...                                   *
  \******************************************************************/
  
  buttonform = XtVaCreateWidget( "form", 
				 xmFormWidgetClass,   form,
				 XmNfractionBase,     5,
				 XmNtopAttachment,    XmATTACH_WIDGET,
				 XmNtopWidget,        accData->desc, 
				 XmNtopOffset,        10, 
				 XmNbottomAttachment, XmATTACH_FORM,
				 XmNbottomOffset,     10,
				 XmNleftAttachment,   XmATTACH_FORM,
				 XmNrightAttachment,  XmATTACH_FORM,
				 NULL );
  
  
  /* The "Notes" button opens a window to a few lines of notes about
   * the account */
  widget = XtVaCreateManagedWidget( "Notes", 
				    xmPushButtonWidgetClass, buttonform,
				    XmNtopAttachment,      XmATTACH_FORM,
				    XmNleftAttachment,     XmATTACH_POSITION,
				    XmNleftPosition,       1,
				    XmNrightAttachment,    XmATTACH_POSITION,
				    XmNrightPosition,      2,  
				    XmNshowAsDefault,      True,
				    NULL );
  XtAddCallback( widget, XmNactivateCallback, 
		 notesCB, (XtPointer)accData );  
  
  /* The "Cancel" button */
  widget = XtVaCreateManagedWidget( "Cancel", 
				    xmPushButtonWidgetClass, buttonform,
				    XmNtopAttachment,      XmATTACH_FORM,
				    XmNleftAttachment,     XmATTACH_POSITION,
				    XmNleftPosition,       2, 
				    XmNrightAttachment,    XmATTACH_POSITION,
				    XmNrightPosition,      3,  
				    XmNshowAsDefault,      True,
				    NULL );
  
  /* We need to do something to clean up memory too! */
  XtAddCallback( widget, XmNactivateCallback, 
		 destroyShellCB, (XtPointer)dialog );  
  
  /* The "Create" button creates the new account with the data 
   * that the user entered */
  widget = XtVaCreateManagedWidget( "Create", 
				    xmPushButtonWidgetClass, buttonform,
				    XmNtopAttachment,      XmATTACH_FORM,
				    XmNleftAttachment,     XmATTACH_POSITION,
				    XmNleftPosition,       3, 
				    XmNrightAttachment,    XmATTACH_POSITION,
				    XmNrightPosition,      4,  
				    XmNshowAsDefault,      True,
				    NULL );
  
  XtAddCallback( widget, XmNactivateCallback, 
		 createCB, (XtPointer)accData );
  /* We need to do something to clean up memory too! */
/* this is done at endo fo dialog.
  XtAddCallback( widget, XmNactivateCallback, 
		 destroyShellCB, (XtPointer)dialog );  
*/
    
  XtManageChild(buttonform);
  
  /******************************************************************/
  XtManageChild(form);
  
  XtPopup( dialog, XtGrabNone );
  
  unsetBusyCursor( parent );
  }

/********************************************************************\
 * closeAccWindow                                                   *
 *   frees memory allocated for an accWindow, and other cleanup     * 
 *   stuff                                                          * 
 *                                                                  * 
 * Args:   mw -                                                     * 
 *         cd - accData - the struct for the accWindow that is      * 
 *              being closed                                        * 
 *         cb -                                                     * 
 * Return: none                                                     *
\********************************************************************/
void 
closeAccWindow( Widget mw, XtPointer cd, XtPointer cb )
  {
  AccWindow *accData = (AccWindow *)cd;
  
  _free(accData);
  DEBUG("close AccWindow");
  }

/********************************************************************\
 * editAccWindow                                                    *
 *   opens up a window to edit an account                           * 
 *                                                                  * 
 * Args:   parent   - the parent of the window to be created        * 
 *         account  - the account to edit                           * 
 * Return: none                                                     *
\********************************************************************/
void 
editAccWindow( Widget parent, Account *account )
  {
  Widget dialog, form, widget, label, buttonform;
  EditAccWindow *editAccData;
  
  /* hack alert -- if no account selected for editing,
   * put up a popup and tell the user to pick something.
   * for the moment, we just no-op. */
  if (0x0 == account) return;

  setBusyCursor( parent );
  
  editAccData = (EditAccWindow *)_malloc(sizeof(EditAccWindow));
  editAccData->notes   = account->notes;
  editAccData->account = account;
  
  /* force the size of the dialog so it is not resizable */
  dialog = XtVaCreatePopupShell( "dialog", 
				 xmDialogShellWidgetClass, parent,
				 XmNtitle,            "Edit Account",
				 XmNdeleteResponse,   XmDESTROY,
				 XmNwidth,     350,
				 XmNminWidth,  350,
				 XmNmaxWidth,  350,
				 XmNheight,    150,
				 XmNminHeight, 150,
				 XmNmaxHeight, 150,
                                 XmNresizable, FALSE,
                                 XmNallowShellResize, FALSE,
                                 XmNtransient, FALSE,  /* allow window to be repositioned */
				 NULL );
  
  XtAddCallback( dialog, XmNdestroyCallback, 
		 closeEditAccWindow, (XtPointer)editAccData );
  
  /* The form to put everything in the dialog in */
  form = XtVaCreateWidget( "form", xmFormWidgetClass, dialog, 
                                 XmNrubberPositioning, TRUE,
                                 NULL );
  /******************************************************************\
   * Text fields....                                                *
  \******************************************************************/
  
  label = 
    XtVaCreateManagedWidget( "Account Name:",
			     xmLabelGadgetClass, form,
			     XmNtopAttachment,   XmATTACH_FORM,
			     XmNtopOffset,       10,
			     XmNrightAttachment, XmATTACH_POSITION,
			     XmNrightPosition,   35,        /* 35% */
			     NULL );
  
  editAccData->name = 
    XtVaCreateManagedWidget( "text",
			     xmTextWidgetClass,  form,
			     XmNmaxLength,       40,
			     XmNcolumns,         25,
			     XmNvalue,           account->accountName,
			     XmNeditable,        True,
			     XmNtopAttachment,   XmATTACH_FORM,
			     XmNtopOffset,       10,
			     XmNleftAttachment,  XmATTACH_POSITION,
			     XmNleftPosition,    35,        /* 35% */
			     NULL );
  
  label = 
    XtVaCreateManagedWidget( "Description:",
			     xmLabelGadgetClass, form,
			     XmNtopAttachment,   XmATTACH_WIDGET,
			     XmNtopWidget,       editAccData->name,
			     XmNtopOffset,       10,
			     XmNrightAttachment, XmATTACH_POSITION,
			     XmNrightPosition,   35,        /* 35% */
			     NULL );
  
  editAccData->desc = 
    XtVaCreateManagedWidget( "text",
			     xmTextWidgetClass,  form,
			     XmNmaxLength,       40,
			     XmNcolumns,         30,
			     XmNvalue,           account->description,
			     XmNeditable,        True,
			     XmNtopAttachment,   XmATTACH_WIDGET,
			     XmNtopWidget,       editAccData->name,
			     XmNtopOffset,       10,
			     XmNleftAttachment,  XmATTACH_POSITION,
			     XmNleftPosition,    35,        /* 35% */
			     NULL );
  
  /******************************************************************\
   * The buttons at the bottom...                                   *
  \******************************************************************/
  
  buttonform = 
    XtVaCreateWidget( "form", 
		      xmFormWidgetClass,   form,
		      XmNfractionBase,     5,
		      XmNtopAttachment,    XmATTACH_WIDGET,
		      XmNtopWidget,        editAccData->desc,
		      XmNtopOffset,        10,
		      XmNbottomAttachment, XmATTACH_FORM,
		      XmNbottomOffset,     10,
		      XmNleftAttachment,   XmATTACH_FORM,
		      XmNrightAttachment,  XmATTACH_FORM,
		      NULL );
  
  /* The "Notes" button opens a window to a few lines of notes about
   * the account */
  widget = 
    XtVaCreateManagedWidget( "Notes", 
			     xmPushButtonWidgetClass, buttonform,
			     XmNtopAttachment,      XmATTACH_FORM,
			     XmNleftAttachment,     XmATTACH_POSITION,
			     XmNleftPosition,       1,
			     XmNrightAttachment,    XmATTACH_POSITION,
			     XmNrightPosition,      2,
			     XmNshowAsDefault,      True,
			     NULL );
  
  XtAddCallback( widget, XmNactivateCallback, 
		 notesCB, (XtPointer)editAccData );  
  
  /* The "Cancel" button */
  widget = 
    XtVaCreateManagedWidget( "Cancel", 
			     xmPushButtonWidgetClass, buttonform,
			     XmNtopAttachment,      XmATTACH_FORM,
			     XmNleftAttachment,     XmATTACH_POSITION,
			     XmNleftPosition,       2,
			     XmNrightAttachment,    XmATTACH_POSITION,
			     XmNrightPosition,      3,
			     XmNshowAsDefault,      True,
			     NULL );
  
  /* We need to do something to clean up memory too! */
  XtAddCallback( widget, XmNactivateCallback, 
		 destroyShellCB, (XtPointer)dialog );  
  
  /* The "Create" button creates the new account with the data 
   * that the user entered */
  widget = 
    XtVaCreateManagedWidget( "Ok",
			     xmPushButtonWidgetClass, buttonform,
			     XmNtopAttachment,      XmATTACH_FORM,
			     XmNleftAttachment,     XmATTACH_POSITION,
			     XmNleftPosition,       3,
			     XmNrightAttachment,    XmATTACH_POSITION,
			     XmNrightPosition,      4,
			     XmNshowAsDefault,      True,
			     NULL );
  
  XtAddCallback( widget, XmNactivateCallback, 
		 editCB, (XtPointer)editAccData );
  /* We need to do something to clean up memory too! */
  XtAddCallback( widget, XmNactivateCallback, 
		 destroyShellCB, (XtPointer)dialog );  
    
  XtManageChild(buttonform);
  
  /******************************************************************/
  XtManageChild(form);
  
  XtPopup( dialog, XtGrabNone );
  
  unsetBusyCursor( parent );
  }

/********************************************************************\
 * closeEditAccWindow                                               *
 *   frees memory allocated for an editAccWindow, and other cleanup * 
 *   stuff                                                          * 
 *                                                                  * 
 * Args:   mw -                                                     * 
 *         cd - editAccData - the struct for the editAccWindow      * 
 *              that is being closed                                * 
 *         cb -                                                     * 
 * Return: none                                                     *
\********************************************************************/
void 
closeEditAccWindow( Widget mw, XtPointer cd, XtPointer cb )
  {
  EditAccWindow *editAccData = (EditAccWindow *)cd;
  
  _free(editAccData);
  DEBUG("close EditAccWindow");
  }

/********************************************************************\
 * notesCB -- called when the user presses the "Notes" Button       * 
 *                                                                  * 
 * Args:   mw - the widget that called us                           * 
 *         cd - accData - the struct that has the notes text in it  * 
 *         cb -                                                     * 
 * Return: none                                                     * 
 * Global: toplevel    - the toplevel widget                        *
\********************************************************************/
void
notesCB( Widget mw, XtPointer cd, XtPointer cb )
  {
  AccWindow *accData = (AccWindow *)cd;
  
  accData->notes = textBox( toplevel, "Notes", accData->notes, True );
  }

/********************************************************************\
 * createCB -- creates the new account from data in the newaccount  * 
 *   dialog window                                                  * 
 *                                                                  * 
 * Args:   mw - the widget that called us                           * 
 *         cd - accData - the struct of data associated with this   * 
 *              accWindow.                                          * 
 *         cb -                                                     * 
 * Return: none                                                     * 
 * Global: data        - the data from the datafile                 *
 *         toplevel    - the toplevel widget                        *
\********************************************************************/
void
createCB( Widget mw, XtPointer cd, XtPointer cb )
  {
  int i,num;
  Transaction *trans;
  Account     *acc;
  AccWindow   *accData = (AccWindow *)cd;
  Boolean set = False;

  String name = XmTextGetString(accData->name);
  String desc = XmTextGetString(accData->desc);
  
#ifdef SHOULDNT_BE_BROKEN_ANYMORE
  {
    /* since portfolio & mutual not fully implemented, provide warning */
    int warn = 0;
    XtVaGetValues( accData->portfolio, XmNset, &set, NULL );
    if(set) warn = 1;
    
    XtVaGetValues( accData->mutual, XmNset, &set, NULL );
    if(set) warn = 1;

    if (warn) {
     int do_it_anyway;
     do_it_anyway = verifyBox (toplevel, 
"Warning: Portfolio and Mutual Fund \n\
Account types are not fully implemented. \n\
You can play with the interface here, \n\
but doing so may damage your data. \n\
You have been warned! \n\
Do you want to continue anyway?\n");
    if (!do_it_anyway) return;
    }
  }
#endif /* SHOULDNT_BE_BROKEN_ANYMORE */

  /* The account has to have a name! */
  if( strcmp( name, "" ) == 0 ) {
    errorBox (toplevel, "The account must be given a name! \n");
    return;
  }
  
  acc = mallocAccount();
  acc->flags     = 0;
  acc->accountName = name;
  acc->description = desc;
  acc->notes       = accData->notes;
  
  /* figure out account type */
    
  XtVaGetValues( accData->bank, XmNset, &set, NULL );
  if(set)
    acc->type = BANK;
  
  XtVaGetValues( accData->cash, XmNset, &set, NULL );
  if(set)
    acc->type = CASH;
  
  XtVaGetValues( accData->asset, XmNset, &set, NULL );
  if(set)
    acc->type = ASSET;
  
  XtVaGetValues( accData->credit, XmNset, &set, NULL );
  if(set)
    acc->type = CREDIT;
  
  XtVaGetValues( accData->liability, XmNset, &set, NULL );
  if(set)
    acc->type = LIABILITY;
  
  XtVaGetValues( accData->portfolio, XmNset, &set, NULL );
  if(set)
    acc->type = PORTFOLIO;
  
  XtVaGetValues( accData->mutual, XmNset, &set, NULL );
  if(set)
    acc->type = MUTUAL;
  
  /* Add an opening balance transaction (as the first transaction) */
  trans = mallocTransaction();
  
  todaysDate( &(trans->date) );
  trans->num         = XtNewString("");
  trans->description = XtNewString("Opening Balance\0");
  trans->memo        = XtNewString("");
  
  /* add the new transaction to the account */
  insertTransaction( acc, trans );
  
  /* once the account is set up, add it to data */
  insertAccount( data, acc );
  
  /* make sure the accountlist is updated to reflect the new account */
  refreshMainWindow();
  /* open up the account window for the user */
  regWindow( toplevel, acc );

  /* if we got to here, tear down the dialog window */
  XtDestroyWidget (accData->dialog);
  }

/********************************************************************\
 * editCB -- records the edits made by in the editAccWindow         * 
 *                                                                  * 
 * Args:   mw - the widget that called us                           * 
 *         cd - editAccData - the struct of data associated with    *
 *              the EditAccWindow                                   * 
 *         cb -                                                     * 
 * Return: none                                                     * 
 * Global: data        - the data from the datafile                 *
\********************************************************************/
void
editCB( Widget mw, XtPointer cd, XtPointer cb )
  {
  EditAccWindow *editAccData = (EditAccWindow *)cd;
  String name = XmTextGetString(editAccData->name);
  String desc = XmTextGetString(editAccData->desc);
  
  /* The account has to have a name! */
  if( strcmp( name, "" ) != 0 )
    {
    XtFree(editAccData->account->accountName);
    editAccData->account->accountName = name;
    }
  
  XtFree(editAccData->account->description);
  editAccData->account->description = name;
  
  refreshMainWindow();
  }
