/********************************************************************\
 * AccWindow.c -- window for creating new accounts for xacc         *
 *                (X-Accountant)                                    *
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

#include <string.h>

#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/RowColumn.h>
#include <Xm/LabelGP.h>
#include <Xm/ToggleB.h>
#include <Xm/PushB.h>
#include <Xm/Text.h>

#include "config.h"

#include "Account.h"
#include "AccountMenu.h"
#include "Data.h"
#include "messages.h"
#include "MainWindow.h"
#include "RegWindow.h"
#include "TextBox.h"
#include "util.h"
#include "xtutil.h"

typedef struct _accwindow {
                         /* The account type buttons:               */
  Widget dialog;
  Widget type_widgets[NUM_ACCOUNT_TYPES];

                         /* The text fields:                        */
  Widget name;           /* The account name text field             */
  Widget desc;           /* Account description text field          */

  AccountMenu *accMenu;

  Account *newacc;       /* tmp account for editing */

} AccWindow;

typedef struct _editaccwindow {
  Widget dialog;
                         /* The text fields:                        */
  Widget  name;          /* The account name text field             */
  Widget  desc;          /* Account description text field          */
  
  Account *account;      /* The account to edit                     */

} EditAccWindow;

typedef struct _editnoteswindow {
  TextBox *tb;
  Account *account;      /* The account to edit                     */
} EditNotesWindow;


/** GLOBALS *********************************************************/
extern Widget toplevel;

static EditAccWindow   ** editAccList   = NULL;
static EditNotesWindow ** editNotesList = NULL;

/** PROTOTYPES ******************************************************/
static void closeAccWindow      ( Widget mw, XtPointer cd, XtPointer cb );
static void closeEditAccWindow  ( Widget mw, XtPointer cd, XtPointer cb );
static void notesCB             ( Widget mw, XtPointer cd, XtPointer cb );
static void editNotesCB         ( Widget mw, XtPointer cd, XtPointer cb );
static void createCB            ( Widget mw, XtPointer cd, XtPointer cb );
static void finishEditCB        ( Widget mw, XtPointer cd, XtPointer cb );
static void selectAccountCB     ( Widget mw, XtPointer cd, XtPointer cb );
static void closeEditNotesWindow( Widget mw, XtPointer cd, XtPointer cb );

EditNotesWindow * editNotesWindow (Account *acc);

/********************************************************************\
 * accWindow                                                        *
 *   opens up a window to create a new account... the account is    * 
 *   actually created in the "create" callback                      * 
 *                                                                  * 
 * Args:   parent   - the parent of the window to be created        * 
 * Return: none                                                     *
\********************************************************************/
AccWindow *
accWindow( Widget parent )
  {
  int i;
  Widget    dialog, form, frame, rc, widget, 
            label, buttonform, group_menu, topwid;
  AccWindow *accData;
  AccountGroup *grp = topgroup;  /* hack alert -- should be passed as argument */
  
  setBusyCursor( parent );
  
  accData = (AccWindow *)_malloc(sizeof(AccWindow));

  accData->newacc = xaccMallocAccount();
  
  /* force the size of the dialog so it is not resizable */
  dialog = XtVaCreatePopupShell( "dialog", 
				 xmDialogShellWidgetClass, parent,
				 XmNtitle,            SETUP_ACCT_STR,
				 XmNdeleteResponse,   XmDESTROY,
                                 /*
                                  * Let the window find it's own size,
                                  * based on the font size.
				  * XmNwidth,     350,
				  * XmNminWidth,  350,
				  * XmNmaxWidth,  350,
				  * XmNheight,    340,
				  * XmNminHeight, 340,
				  * XmNmaxHeight, 340,
                                  */
                                 XmNresizable, FALSE,
                                 XmNallowShellResize, FALSE,
                                 /* allow window to be repositioned */
                                 /* XmNtransient, FALSE, */ 
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
  widget = XtVaCreateManagedWidget( ACC_TYPE_STR,
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
  for (i=0; i<NUM_ACCOUNT_TYPES; i++) {
    accData->type_widgets[i] = 
    XtVaCreateManagedWidget( account_type_name[i],
			     xmToggleButtonWidgetClass, rc,
			     XmNindicatorType,   XmONE_OF_MANY,
			     XmNset,             False,
			     NULL);
  }
  XtVaSetValues (accData->type_widgets[BANK],
                             XmNset,             True,
                             NULL);

  
  /******************************************************************\
   * Text fields....                                                *
  \******************************************************************/
  
    
  label = 
    XtVaCreateManagedWidget( ACC_NAME_C_STR,
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
    XtVaCreateManagedWidget( DESC_C_STR,
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
  
  topwid = accData->desc;
  label = 
    XtVaCreateManagedWidget( PARENT_ACC_C_STR,
			     xmLabelGadgetClass, form,
			     XmNtopAttachment,   XmATTACH_WIDGET,
			     XmNtopWidget,       topwid,
			     XmNtopOffset,       10,
			     XmNrightAttachment, XmATTACH_POSITION,
			     XmNrightPosition,   35,        /* 35% */
			     NULL );
  
  /* put up a pulldown menu to let user choose an account */
  accData->accMenu = xaccBuildAccountMenu (grp, form, PICK_ONE_STR);
  group_menu = xaccGetAccountMenuWidget (accData->accMenu);
  xaccAccountMenuAddCallback (accData->accMenu, selectAccountCB, (XtPointer) accData);

  XtVaSetValues( group_menu,
                             XmNtopAttachment,  XmATTACH_WIDGET,
			     XmNtopWidget,       accData->desc,
			     XmNtopOffset,       10, 
			     XmNleftAttachment,  XmATTACH_POSITION,
			     XmNleftPosition,    35,        /* 35% */
                             NULL );
  
  XtManageChild (group_menu); 

  /******************************************************************\
   * The buttons at the bottom...                                   *
  \******************************************************************/

  topwid = group_menu;
  
  buttonform = XtVaCreateWidget( "form", 
				 xmFormWidgetClass,   form,
				 XmNfractionBase,     5,
				 XmNtopAttachment,    XmATTACH_WIDGET,
				 XmNtopWidget,        topwid, 
				 XmNtopOffset,        10, 
				 XmNbottomAttachment, XmATTACH_FORM,
				 XmNbottomOffset,     10,
				 XmNleftAttachment,   XmATTACH_FORM,
				 XmNrightAttachment,  XmATTACH_FORM,
				 NULL );
  
  
  /* The "Notes" button opens a window to a few lines of notes about
   * the account */
  widget = XtVaCreateManagedWidget( NOTES_STR,
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
  widget = XtVaCreateManagedWidget( CANCEL_STR,
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
  widget = XtVaCreateManagedWidget( CREATE_STR,
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
  /* this is done at end of dialog.
   * XtAddCallback( widget, XmNactivateCallback, 
   * 		 destroyShellCB, (XtPointer)dialog );  
   */
    
  XtManageChild(buttonform);
  
  /******************************************************************/
  XtManageChild(form);
  
  XtPopup( dialog, XtGrabNone );
  
  unsetBusyCursor( parent );

  return accData;
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
static void 
closeAccWindow( Widget mw, XtPointer cd, XtPointer cb )
  {
  AccWindow *accData = (AccWindow *)cd;

  if(accData->newacc) xaccFreeAccount (accData->newacc);  

  xaccFreeAccountMenu (accData->accMenu);
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
EditAccWindow *
editAccWindow( Widget parent, Account *acc )
{

  Widget dialog, form, widget, label, buttonform;
  EditAccWindow *editAccData;
  
  FETCH_FROM_LIST (EditAccWindow, editAccList, acc, account, editAccData);

  setBusyCursor( parent );
  
  /* force the size of the dialog so it is not resizable */
  dialog = XtVaCreatePopupShell( "dialog", 
				 xmDialogShellWidgetClass, parent,
				 XmNtitle,            EDIT_ACCT_STR,
				 XmNdeleteResponse,   XmDESTROY,
                                 /*
                                  * Let the window find it's own size, 
                                  * based on the size of the font.
				  * XmNwidth,     350,
				  * XmNminWidth,  350,
				  * XmNmaxWidth,  350,
				  * XmNheight,    150,
				  * XmNminHeight, 150,
				  * XmNmaxHeight, 150,
                                  */
                                 XmNresizable, FALSE,
                                 XmNallowShellResize, FALSE,
                                 XmNtransient, FALSE,  /* allow window to be repositioned */
				 NULL );
  editAccData->dialog = dialog;
  
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
    XtVaCreateManagedWidget( ACC_NAME_C_STR,
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
			     XmNvalue,           acc->accountName,
			     XmNeditable,        True,
			     XmNtopAttachment,   XmATTACH_FORM,
			     XmNtopOffset,       10,
			     XmNleftAttachment,  XmATTACH_POSITION,
			     XmNleftPosition,    35,        /* 35% */
			     NULL );
  
  label = 
    XtVaCreateManagedWidget( DESC_C_STR,
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
			     XmNvalue,           acc->description,
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
    XtVaCreateManagedWidget( NOTES_STR, 
			     xmPushButtonWidgetClass, buttonform,
			     XmNtopAttachment,      XmATTACH_FORM,
			     XmNleftAttachment,     XmATTACH_POSITION,
			     XmNleftPosition,       1,
			     XmNrightAttachment,    XmATTACH_POSITION,
			     XmNrightPosition,      2,
			     XmNshowAsDefault,      True,
			     NULL );
  
  XtAddCallback( widget, XmNactivateCallback, 
		 editNotesCB, (XtPointer)editAccData );  
  
  /* The "Cancel" button */
  widget = 
    XtVaCreateManagedWidget( CANCEL_STR, 
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
    XtVaCreateManagedWidget( OK_STR,
			     xmPushButtonWidgetClass, buttonform,
			     XmNtopAttachment,      XmATTACH_FORM,
			     XmNleftAttachment,     XmATTACH_POSITION,
			     XmNleftPosition,       3,
			     XmNrightAttachment,    XmATTACH_POSITION,
			     XmNrightPosition,      4,
			     XmNshowAsDefault,      True,
			     NULL );
  
  XtAddCallback( widget, XmNactivateCallback, 
		 finishEditCB, (XtPointer)editAccData );
  /* We need to do something to clean up memory too! */
  XtAddCallback( widget, XmNactivateCallback, 
		 destroyShellCB, (XtPointer)dialog );  
    
  XtManageChild(buttonform);
  
  /******************************************************************/
  XtManageChild(form);
  
  XtPopup( dialog, XtGrabNone );
  
  unsetBusyCursor( parent );

  return editAccData;
}

/********************************************************************\
 * Don't delete any structures -- the close callback wil do this    *
\********************************************************************/

void xaccDestroyEditAccWindow (Account * acc)
{
  EditAccWindow *editAccData;

  FIND_IN_LIST (EditAccWindow,editAccList,acc,account,editAccData); 
  if (!editAccData) return;
  XtDestroyWidget (editAccData->dialog);
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
static void 
closeEditAccWindow( Widget mw, XtPointer cd, XtPointer cb )
  {
  EditAccWindow *editAccData = (EditAccWindow *)cd;
  Account *acc = editAccData->account;

  REMOVE_FROM_LIST (EditAccWindow,editAccList,acc,account); 
  free(editAccData);
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
static void
notesCB( Widget mw, XtPointer cd, XtPointer cb )
{
  AccWindow *accData = (AccWindow *)cd;
  Account *acc = accData -> newacc;
  editNotesWindow (acc);
  /* hack alert -- should raise window to the top */
}

/********************************************************************\
 * editNotesCB -- called when the user presses the "Notes" Button   * 
 *                                                                  * 
 * Args:   mw - the widget that called us                           * 
 *         cd - accData - the struct that has the notes text in it  * 
 *         cb -                                                     * 
 * Return: none                                                     * 
 * Global: toplevel    - the toplevel widget                        *
\********************************************************************/
static void
editNotesCB( Widget mw, XtPointer cd, XtPointer cb )
{
  EditAccWindow *editAccData = (EditAccWindow *)cd;
  Account *acc = editAccData -> account;
  editNotesWindow (acc);
  /* hack alert -- should raise window to the top */
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
static void
createCB( Widget mw, XtPointer cd, XtPointer cb )
  {
  int i;
  Transaction *trans;
  Account     *acc, *parent_acc;
  AccWindow   *accData = (AccWindow *)cd;
  Boolean set = False;

  String name = XmTextGetString(accData->name);
  String desc = XmTextGetString(accData->desc);
  
  /* The account has to have a name! */
  if( strcmp( name, "" ) == 0 ) {
    errorBox (toplevel, ACC_NO_NAME_MSG);
    return;
  }
  
  acc = accData->newacc;
  accData->newacc = NULL;

  acc->flags     = 0;
  acc->accountName = name;
  acc->description = desc;
  
  /* figure out account type */
  for (i=0; i<NUM_ACCOUNT_TYPES; i++) {
    XtVaGetValues( accData->type_widgets[i], XmNset, &set, NULL );
    if(set) acc->type = i;
  }
  
  /* Add an opening balance transaction (as the first transaction) */
  trans = xaccMallocTransaction();
 
  todaysDate( &(trans->date) );
  xaccTransSetDescription (trans, OPEN_BALN_STR);

  /* add the new transaction to the account */
  xaccInsertSplit (acc, &(trans->credit_split) );
  
  /* once the account is set up, add it to account group 
   * If the user indicated a parent acccount, make it a 
   * sub account of that */
  parent_acc = xaccGetAccountMenuSelection (accData->accMenu);
  if (parent_acc) {
    xaccInsertSubAccount (parent_acc, acc);
  } else {
    insertAccount( topgroup, acc );
  }
  
  /* make sure the accountlist is updated to reflect the new account */
  refreshMainWindow();

  /* open up the account window for the user */
  regWindowSimple ( toplevel, acc );

  /* if we got to here, tear down the dialog window */
  XtDestroyWidget (accData->dialog);
  }

/********************************************************************\
 * finishEditCB -- records the edits made in the editAccWindow      * 
 *                                                                  * 
 * Args:   mw - the widget that called us                           * 
 *         cd - editAccData - the struct of data associated with    *
 *              the EditAccWindow                                   * 
 *         cb -                                                     * 
 * Return: none                                                     * 
 * Global: data        - the data from the datafile                 *
\********************************************************************/
static void
finishEditCB( Widget mw, XtPointer cd, XtPointer cb )
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
  editAccData->account->description = desc;
  
  refreshMainWindow();
  }

/********************************************************************\
 * selectAccountCB -- checks the use account selection              * 
 * 
 * Basically, sub-account *must* be of the same category as thier 
 * parent accounts, otherwise chaos will errupt.  The five basic 
 * categories are asset, liability, income,. expense, and equity.
 *
 * Currently, there are four subcategories for asset accounts:
 * banks, cash, stocks, bonds, mutual funds.
 *
 *                                                                  * 
\********************************************************************/
static void
selectAccountCB( Widget mw, XtPointer cd, XtPointer cb )
{
  int i, but=0;
  Boolean set;
  AccWindow *menu = (AccWindow *) cd;
  Account *acc = (Account *) cb;

  /* unset any pressed radio buttons in preparation for 
   * setting insensitive of some of them. 
   */

  /* figure out which radio button might be set */
  for (i=0; i<NUM_ACCOUNT_TYPES; i++) {
    XtVaGetValues( menu->type_widgets[i], XmNset, &set, NULL );
    if(set) but = i;
  }

  if (acc) {
    switch (acc->type) {
       case BANK:
       case CASH:
       case ASSET:
       case STOCK:
       case MUTUAL:
          XtSetSensitive (menu->type_widgets[BANK],      True);
          XtSetSensitive (menu->type_widgets[CASH],      True);
          XtSetSensitive (menu->type_widgets[ASSET],     True);
          XtSetSensitive (menu->type_widgets[STOCK],     True);
          XtSetSensitive (menu->type_widgets[MUTUAL],    True);
          XtSetSensitive (menu->type_widgets[LIABILITY], False);
          XtSetSensitive (menu->type_widgets[CREDIT],    False);
          XtSetSensitive (menu->type_widgets[INCOME],    False);
          XtSetSensitive (menu->type_widgets[EXPENSE],   False);
          XtSetSensitive (menu->type_widgets[EQUITY],    False);

          /* unset unavailable buttons */
          XtVaSetValues (menu->type_widgets[LIABILITY], XmNset, False, NULL);
          XtVaSetValues (menu->type_widgets[CREDIT],    XmNset, False, NULL);
          XtVaSetValues (menu->type_widgets[INCOME],    XmNset, False, NULL);
          XtVaSetValues (menu->type_widgets[EXPENSE],   XmNset, False, NULL);
          XtVaSetValues (menu->type_widgets[EQUITY],    XmNset, False, NULL);

          /* set a default, if an inapporpriate button is pushed */
          if ((BANK   != but) && (CASH      != but) &&
              (ASSET  != but) && (STOCK != but) &&
              (MUTUAL != but) ) {
             XtVaSetValues (menu->type_widgets[acc->type], XmNset, True, NULL);
          }
          break;

       case LIABILITY:
       case CREDIT:
          XtSetSensitive (menu->type_widgets[BANK],      False);
          XtSetSensitive (menu->type_widgets[CASH],      False);
          XtSetSensitive (menu->type_widgets[ASSET],     False);
          XtSetSensitive (menu->type_widgets[STOCK],     False);
          XtSetSensitive (menu->type_widgets[MUTUAL],    False);
          XtSetSensitive (menu->type_widgets[LIABILITY], True);
          XtSetSensitive (menu->type_widgets[CREDIT],    True);
          XtSetSensitive (menu->type_widgets[INCOME],    False);
          XtSetSensitive (menu->type_widgets[EXPENSE],   False);
          XtSetSensitive (menu->type_widgets[EQUITY],    False);

          /* unset unavailable buttons */
          XtVaSetValues (menu->type_widgets[BANK],      XmNset, False, NULL);
          XtVaSetValues (menu->type_widgets[CASH],      XmNset, False, NULL);
          XtVaSetValues (menu->type_widgets[ASSET],     XmNset, False, NULL);
          XtVaSetValues (menu->type_widgets[STOCK],     XmNset, False, NULL);
          XtVaSetValues (menu->type_widgets[MUTUAL],    XmNset, False, NULL);
          XtVaSetValues (menu->type_widgets[INCOME],    XmNset, False, NULL);
          XtVaSetValues (menu->type_widgets[EXPENSE],   XmNset, False, NULL);
          XtVaSetValues (menu->type_widgets[EQUITY],    XmNset, False, NULL);

          /* set a default, if an inapporpriate button is pushed */
          if ((LIABILITY != but) && (CREDIT != but)) {
             XtVaSetValues (menu->type_widgets[acc->type], XmNset, True, NULL);
          }
          break;

       case INCOME:
          XtSetSensitive (menu->type_widgets[BANK],      False);
          XtSetSensitive (menu->type_widgets[CASH],      False);
          XtSetSensitive (menu->type_widgets[ASSET],     False);
          XtSetSensitive (menu->type_widgets[STOCK],     False);
          XtSetSensitive (menu->type_widgets[MUTUAL],    False);
          XtSetSensitive (menu->type_widgets[LIABILITY], False);
          XtSetSensitive (menu->type_widgets[CREDIT],    False);
          XtSetSensitive (menu->type_widgets[INCOME],    True);
          XtSetSensitive (menu->type_widgets[EXPENSE],   False);
          XtSetSensitive (menu->type_widgets[EQUITY],    False);

          /* unset unavailable buttons */
          for (i=0; i<NUM_ACCOUNT_TYPES; i++) {
             XtVaSetValues (menu->type_widgets[i],      XmNset, False, NULL);
          }

          /* set a default, if an inapporpriate button is pushed */
          XtVaSetValues (menu->type_widgets[acc->type], XmNset, True, NULL);
          break;

       case EXPENSE:
          XtSetSensitive (menu->type_widgets[BANK],      False);
          XtSetSensitive (menu->type_widgets[CASH],      False);
          XtSetSensitive (menu->type_widgets[ASSET],     False);
          XtSetSensitive (menu->type_widgets[STOCK],     False);
          XtSetSensitive (menu->type_widgets[MUTUAL],    False);
          XtSetSensitive (menu->type_widgets[LIABILITY], False);
          XtSetSensitive (menu->type_widgets[CREDIT],    False);
          XtSetSensitive (menu->type_widgets[INCOME],    False);
          XtSetSensitive (menu->type_widgets[EXPENSE],   True);
          XtSetSensitive (menu->type_widgets[EQUITY],    False);

          /* unset unavailable buttons */
          for (i=0; i<NUM_ACCOUNT_TYPES; i++) {
             XtVaSetValues (menu->type_widgets[i],      XmNset, False, NULL);
          }

          /* set a default, if an inapporpriate button is pushed */
          XtVaSetValues (menu->type_widgets[acc->type], XmNset, True, NULL);
          break;

       case EQUITY:
          XtSetSensitive (menu->type_widgets[BANK],      False);
          XtSetSensitive (menu->type_widgets[CASH],      False);
          XtSetSensitive (menu->type_widgets[ASSET],     False);
          XtSetSensitive (menu->type_widgets[STOCK],     False);
          XtSetSensitive (menu->type_widgets[MUTUAL],    False);
          XtSetSensitive (menu->type_widgets[LIABILITY], False);
          XtSetSensitive (menu->type_widgets[CREDIT],    False);
          XtSetSensitive (menu->type_widgets[INCOME],    False);
          XtSetSensitive (menu->type_widgets[EXPENSE],   False);
          XtSetSensitive (menu->type_widgets[EQUITY],    True);

          /* unset unavailable buttons */
          for (i=0; i<NUM_ACCOUNT_TYPES; i++) {
             XtVaSetValues (menu->type_widgets[i],      XmNset, False, NULL);
          }

          /* set a default, if an inapporpriate button is pushed */
          XtVaSetValues (menu->type_widgets[acc->type], XmNset, True, NULL);
          break;

    }
  } else {
     XtSetSensitive (menu->type_widgets[BANK],      True);
     XtSetSensitive (menu->type_widgets[CASH],      True);
     XtSetSensitive (menu->type_widgets[ASSET],     True);
     XtSetSensitive (menu->type_widgets[CREDIT],    True);
     XtSetSensitive (menu->type_widgets[LIABILITY], True);
     XtSetSensitive (menu->type_widgets[STOCK],     True);
     XtSetSensitive (menu->type_widgets[MUTUAL],    True);
     XtSetSensitive (menu->type_widgets[INCOME],    True);
     XtSetSensitive (menu->type_widgets[EXPENSE],   True);
     XtSetSensitive (menu->type_widgets[EQUITY],    True);
  }
}

/********************************************************************\
 *                                                                  * 
\********************************************************************/

EditNotesWindow *
editNotesWindow (Account *acc)
{
  EditNotesWindow *enw;

  FETCH_FROM_LIST (EditNotesWindow, editNotesList, acc, account, enw);
  
  enw->tb = textBox( toplevel, NOTES_STR, 
                     &(acc->notes),
                     closeEditNotesWindow, enw);
  return enw;
}

/********************************************************************\
 * don't delete any structures; te close callack will do this       *
\********************************************************************/

void 
xaccDestroyEditNotesWindow (Account *acc)
{
  EditNotesWindow *edwin;

  FIND_IN_LIST (EditNotesWindow,editNotesList,acc,account,edwin) 
  if (!edwin) return;

  xaccDestroyTextBox (edwin->tb);
}

/********************************************************************\
\********************************************************************/

static void 
closeEditNotesWindow( Widget mw, XtPointer cd, XtPointer cb )
{
  EditNotesWindow *enw = (EditNotesWindow *) cd;
  Account * acc = enw->account;

  REMOVE_FROM_LIST (EditNotesWindow,editNotesList,acc,account) 

  xaccDestroyTextBox (enw->tb);
  free (enw);

  DEBUG("close EditNotesWindow");
}

/********************** END OF FILE *********************************\
\********************************************************************/
